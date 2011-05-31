#!/usr/bin/env ERL_LIBS=../erlyvideo/apps:.. escript

-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("mpegts/include/mpegts.hrl").


main([]) ->
  code:add_path("ebin"),
  application:start(log4erl),
  log4erl:error_logger_handler(), %% to get all error_logger
  log4erl:add_logger(default_logger),
  log4erl:add_console_appender(default_logger, app1, {debug, "%l%n"}),
  

  X264 = ems_video:init_x264([]),
  {ok, Out} = file:open("out.264", [binary, write]),

  LibMPEG2 = ems_video:init_mpeg2(),
  Path = "zvezda.ts",
  
  {ok, File} = file:open(Path, [read,binary,{read_ahead,131072},raw]),
  {ok, Reader} = mpegts_reader:init([[{program,104}]]),
  dump_frames(File, Reader, LibMPEG2, X264, undefined).
  

dump_frames(File, Reader, LibMPEG2, X264, Writer) ->
  case file:read(File, 188) of
    {ok, <<16#47, Bin/binary>>} ->
      case mpegts_reader:decode_ts(Bin, Reader) of
        {ok, Reader1, undefined} ->
          dump_frames(File, Reader1, LibMPEG2, X264, Writer);
        {ok, Reader1, PES} ->
          {Reader2, YUV} = decode_pes(Reader1, PES, LibMPEG2),
          dump_frames(File, Reader2, LibMPEG2, X264, Writer)
      end;
    eof -> ok
  end.            
  
decode_pes(Reader, PES, LibMPEG2) ->
  {ok, Reader1, Frames} = mpegts_reader:decode_pes(Reader, PES),
  case Frames of
    [#video_frame{codec = mpeg2video, body = Body, dts = DTS}] ->
      % io:format("Mpeg2: ~p ~p ~p~n", [round(DTS), size(Body), erlang:crc32(Body)]),
      ems_video:mpeg2_raw(LibMPEG2, Body),
      
      % case ems_video:mpeg2_raw(LibMPEG2, Body) of
      %   {ok, YUV} -> io:format("YUV: ~p~n", [size(YUV)]);
      %   _ -> ok
      % end,
      ok;
    _ -> ok
  end,  
  % io:format("Read ~p MPEG2 frames~n", [length(Frames)]),
  {Reader1, undefined}.