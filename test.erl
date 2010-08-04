#!/usr/bin/env escript

main([]) ->
  code:add_path("ebin"),
  code:add_path(".."),
  code:add_path("../erlyvideo/ebin"),
  code:add_pathz("../erlyvideo/deps/erlmedia/ebin"),
  code:add_pathz("../erlyvideo/deps/amf/ebin"),

  X264 = ems_video:init_x264(),
  {ok, Out} = file:open("out.264", [binary, write]),
  convert(X264, Out, 1).
  
convert(X264, Out, N) when N < 10 ->
  Path = io_lib:format("fixtures/images/images0000~p.jpg", [N]),
  dump(X264, Out, Path, N);

convert(X264, Out, N) when N < 100 ->
  Path = io_lib:format("fixtures/images/images000~p.jpg", [N]),
  dump(X264, Out, Path, N);

convert(X264, Out, N) ->
  file:close(Out),
  ok.
  
dump(X264, Out, Path, N) ->  
  {ok, JPEG} = file:read_file(lists:flatten(Path)),
  {ok, RGB} = ems_video:jpeg_rgb(JPEG),
  {ok, H264} = ems_video:rgb_x264(X264, RGB),
  file:write(Out, H264),
  convert(X264, Out, N+1).
