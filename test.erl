#!/usr/bin/env ERL_LIBS=../erlyvideo/apps:.. escript

-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("mpegts/include/mpegts.hrl").


main([]) ->
  code:add_path("ebin"),
  application:start(log4erl),
  log4erl:error_logger_handler(), %% to get all error_logger
  log4erl:add_logger(default_logger),
  log4erl:add_console_appender(default_logger, app1, {debug, "%l%n"}),
  
  

  Path = "../erlyvideo/zvezda.ts",
  
  {ok, File} = file:open(Path, [read,binary,{read_ahead,131072},raw]),
  {ok, Reader} = mpegts_reader:init([[{program,104}]]),
  {ok, Writer} = flv_writer:start_link("out.flv"),
  dump_frames(File, Reader, Writer, undefined).
  

dump_frames(File, Reader, Writer, Transcoder) ->
  case file:read(File, 188) of
    {ok, <<16#47, Bin/binary>>} ->
      case mpegts_reader:decode_ts(Bin, Reader) of
        {ok, Reader1, undefined} ->
          dump_frames(File, Reader1, Writer, Transcoder);
        {ok, Reader1, #pes_packet{pts = PTS, dts = DTS, body = Body} = PES} ->
          {TC1, Frame} = ems_video:mpeg2_h264(Transcoder, #video_frame{codec = mpeg2video, pts = PTS, dts = DTS, body = Body, content = video}),
          case Frame of
            undefined -> ok;
            #video_frame{} -> flv_writer:write_frame(Frame, Writer)
          end,
          
          dump_frames(File, Reader1, Writer, TC1)
      end;
    eof -> ok
  end.     

unpack_config(NALS) ->
  unpack_config(NALS, h264:init()).

unpack_config(<<Len:32, NAL:Len/binary, NALS/binary>>, H264) ->
  {H264_1, _} = h264:decode_nal(NAL, H264),
  unpack_config(NALS, H264_1);

unpack_config(<<>>, H264) ->
  h264:decoder_config(H264).

encode_frame(_, X264, #video_frame{body = undefined}, _) ->
  X264;

encode_frame(LibMPEG2, undefined, #video_frame{dts = DTS} = YUV, Writer) ->
  io:format("Initing X264 (~px~p)~n", [ems_video:mpeg2_getopt(LibMPEG2, width), ems_video:mpeg2_getopt(LibMPEG2, height)]),
  {ok, Enc, NALS} = ems_video:init_x264([{width, ems_video:mpeg2_getopt(LibMPEG2, width)}, {height, ems_video:mpeg2_getopt(LibMPEG2, height)}]),
  
  Config = unpack_config(NALS),
  
  flv_writer:write_frame(#video_frame{
    content = video,
    flavor = config,
    codec = h264,
    pts = DTS, 
    dts = DTS,
    body = Config
  }, Writer),
  encode_frame(LibMPEG2, Enc, YUV, Writer);
  
encode_frame(_, Encoder, #video_frame{pts = PTS, dts = DTS, codec = yuv} = YUV, Writer) ->
  case ems_video:yuv_x264(Encoder, YUV) of
    ok -> ok;
    {ok, Flavor, DTS1, PTS1, H264} ->
      flv_writer:write_frame(#video_frame{
        content = video,
        flavor = Flavor,
        codec = h264,
        pts = PTS1, 
        dts = DTS1,
        body = H264
      }, Writer)
  end,
  Encoder.

  
decode_pes(Reader, PES, LibMPEG2) ->
  {ok, Reader1, Frames} = mpegts_reader:decode_pes(Reader, PES),
  Reply = case Frames of
    [#video_frame{codec = mpeg2video, body = Body, dts = DTS}] ->
      % io:format("Mpeg2: ~p ~p ~p~n", [round(DTS), size(Body), erlang:crc32(Body)]),
      case ems_video:mpeg2_raw(LibMPEG2, Body) of
        {yuv, YUV} -> YUV;
        _ -> undefined
      end;
    _ -> undefined
  end,
  % io:format("Read ~p MPEG2 frames~n", [length(Frames)]),
  {Reader1, Reply}.