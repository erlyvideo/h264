-module(x264).
-author('Max Lapshin <max@maxidoors.ru>').

-on_load(on_load/0).
-export([on_load/0]).
-include_lib("erlmedia/include/video_frame.hrl").

-export([init_x264/1, yuv_x264/2]).

-define(NIF_STUB, erlang:error(nif_not_loaded_ems_video)).

on_load() ->
  Path = case code:lib_dir(h264,priv) of
    {error, _} -> "priv";
    Else -> Else
  end,
  Load = erlang:load_nif(Path ++ "/ems_video", 0),
  io:format("Load ems_video: ~p~n", [Load]),
  ok.


init_x264(Options) ->
  {ok, X264, NALS} = real_init_x264(Options),
  {ok, X264, unpack_config(NALS)}.

real_init_x264(_Options) ->
  ?NIF_STUB.

yuv_x264(X264, #video_frame{pts = PTS, codec = yuv, body = YUV}) ->
  real_yuv_x264(X264, YUV, round(PTS)).

real_yuv_x264(_X264, _YUV, _PTS) ->
  ?NIF_STUB.

unpack_config(NALS) ->
  unpack_config(NALS, h264:init()).

unpack_config(<<Len:32, NAL:Len/binary, NALS/binary>>, H264) ->
  {H264_1, _} = h264:decode_nal(NAL, H264),
  unpack_config(NALS, H264_1);

unpack_config(<<>>, H264) ->
  h264:decoder_config(H264).



