-module(ems_video).
-author('Max Lapshin <max@maxidoors.ru>').

-on_load(on_load/0).
-export([on_load/0]).
-include_lib("erlmedia/include/video_frame.hrl").

-export([init_xvid/0, mpeg4_raw/2]).
-export([init_x264/0, rgb_x264/2]).
-export([jpeg_rgb/1]).

-define(NIF_STUB, erlang:error(nif_not_loaded_ems_sound)).

on_load() ->
  load_nif(erlang:system_info(otp_release) >= "R14A").
  
load_nif(true) ->
  Load = erlang:load_nif(code:lib_dir(h264,ebin) ++ "/ems_video", 0),
  io:format("Load ems_video: ~p~n", [Load]),
  ok;

load_nif(false) ->
  ok.


init_xvid() ->
  ?NIF_STUB.
  
mpeg4_raw(_Xvid, _Mpeg4) ->
  ?NIF_STUB.

init_x264() ->
  ?NIF_STUB.

rgb_x264(_X264, _RGB) ->
  ?NIF_STUB.


jpeg_rgb(_JPEG) ->
  ?NIF_STUB.