#!/usr/bin/env ERL_LIBS=../erlyvideo/apps:.. escript

-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("mpegts/include/mpegts.hrl").


main([]) ->
  code:add_path("ebin"),
  application:start(log4erl),
  log4erl:error_logger_handler(), %% to get all error_logger
  log4erl:add_logger(default_logger),
  log4erl:add_console_appender(default_logger, app1, {debug, "%l%n"}),
  application:start(iconv),
  application:start(mpegts),
  
  

  Path = "../erlyvideo/test/files/mpeg2.ts",
  % Path = "zz.ts",
  % Path = "zvezda-bkp.ts",
  
  {ok, File} = file:open(Path, [read,binary,{read_ahead,131072},raw]),
  {ok, Reader} = mpegts_reader:init([[{program,104}]]),
  % {ok, Reader} = mpegts_reader:init([[]]),
  {ok, Writer} = flv_writer:start_link("out.flv"),
  put(last_dts, 0),
  Reply = (catch dump_frames(File, Reader, Writer)),
  io:format("~p:~p~n", [Reply, erlang:get_stacktrace()]).

dump_frames(File, Reader, Writer) ->
  case file:read(File, 188) of
    {ok, <<16#47, Bin/binary>>} ->
      LastDTS = get(last_dts),
      case mpegts_reader:decode_ts(Bin, Reader) of
        {ok, Reader1, #pes_packet{pts = PTS, dts = DTS, body = Body} = PES} ->
          put(last_dts, DTS),
          % io:format("PES ~p ~p ~p ~p~n", [round(DTS), round(PTS - DTS), size(Body), erlang:crc32(Body)]),
          % io:format("PES ~p ~p~n", [size(Body), erlang:crc32(Body)]),
          case mpegts_reader:decode_pes(Reader1, PES) of
            {ok, Reader2, Frames} ->
              % io:format("~p~n", [[{Codec,DTS} || #video_frame{codec = Codec, dts = DTS} <- Frames]]),
              (catch [flv_writer:write_frame(Frame, Writer) || Frame <- Frames]),
              dump_frames(File, Reader2, Writer);
            {ok, Reader2} ->
              dump_frames(File, Reader2, Writer)                
          end;
        {ok, Reader1, _} ->
          dump_frames(File, Reader1, Writer)
      end;
    eof -> ok
  end.     

