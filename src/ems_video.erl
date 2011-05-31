-module(ems_video).
-author('Max Lapshin <max@maxidoors.ru>').

-on_load(on_load/0).
-export([on_load/0]).
-include_lib("erlmedia/include/video_frame.hrl").

-export([init_xvid/0, mpeg4_raw/2]).
-export([init_x264/1, yuv_x264/2]).
-export([jpeg_rgb/1]).

-export([init_mpeg2/0, mpeg2_raw/2, mpeg2_getopt/2]).

-define(NIF_STUB, erlang:error(nif_not_loaded_ems_video)).

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

init_x264(_Options) ->
  ?NIF_STUB.

yuv_x264(X264, #video_frame{dts = DTS, pts = PTS, codec = yuv, body = YUV}) ->
  real_yuv_x264(X264, YUV, DTS, PTS).

real_yuv_x264(_X264, _YUV, _DTS, _PTS) ->
  ?NIF_STUB.

jpeg_rgb(_JPEG) ->
  ?NIF_STUB.

init_mpeg2() ->
  ?NIF_STUB.
  
mpeg2_getopt(_Mpeg2, _Option) ->
  ?NIF_STUB.

mpeg2_raw(Mpeg2, Pes) ->
  % real_mpeg2_raw(Mpeg2, <<Pes/binary, 0,0,1,16#FF>>).
  real_mpeg2_raw(Mpeg2, Pes).

real_mpeg2_raw(_Mpeg2, _Pes) ->
  ?NIF_STUB.

