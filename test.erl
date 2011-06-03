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
  % Path = "zz.ts",
  % Path = "zvezda-bkp.ts",
  
  {ok, File} = file:open(Path, [read,binary,{read_ahead,131072},raw]),
  {ok, Reader} = mpegts_reader:init([[{program,104}]]),
  % {ok, Reader} = mpegts_reader:init([[]]),
  {ok, Writer} = flv_writer:start_link("out.flv"),
  put(last_dts, 0),
  dump_frames(File, Reader, Writer, undefined).
  

dump_frames(File, Reader, Writer, Transcoder) ->
  case file:read(File, 188) of
    {ok, <<16#47, Bin/binary>>} ->
      LastDTS = get(last_dts),
      case mpegts_reader:decode_ts(Bin, Reader) of
        {ok, Reader1, #pes_packet{pts = PTS, dts = DTS, body = Body} = PES} ->
          put(last_dts, DTS),
          % io:format("PES ~p ~p ~p ~p~n", [round(DTS), round(PTS - DTS), size(Body), erlang:crc32(Body)]),
          % io:format("PES ~p ~p~n", [size(Body), erlang:crc32(Body)]),
          {TC1, Frame} = ems_video:mpeg2_h264(Transcoder, #video_frame{codec = mpeg2video, pts = PTS, dts = DTS, body = Body, content = video}),
          case Frame of
            undefined -> ok;
            #video_frame{} -> flv_writer:write_frame(Frame, Writer)
          end,
          
          dump_frames(File, Reader1, Writer, TC1);
        {ok, Reader1, _} ->
          dump_frames(File, Reader1, Writer, Transcoder)
      end;
    eof -> ok
  end.     

