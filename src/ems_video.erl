-module(ems_video).
-author('Max Lapshin <max@maxidoors.ru>').

-on_load(on_load/0).
-export([on_load/0]).
-include_lib("erlmedia/include/video_frame.hrl").

-export([init_xvid/0, mpeg4_raw/2]).
-export([init_x264/1, yuv_x264/2]).
-export([jpeg_rgb/1]).

-export([init_mpeg2/0, mpeg2_raw/2, mpeg2_getopt/2]).
-export([mpeg2_h264/2]).

-define(NIF_STUB, erlang:error(nif_not_loaded_ems_video)).

-record(mpeg2_h264, {
  mpeg2,
  x264,
  buffer = <<>>,
  config_frame
}).

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

yuv_x264(X264, #video_frame{pts = PTS, codec = yuv, body = YUV}) ->
  real_yuv_x264(X264, YUV, round(PTS)).

real_yuv_x264(_X264, _YUV, _PTS) ->
  ?NIF_STUB.

jpeg_rgb(_JPEG) ->
  ?NIF_STUB.

init_mpeg2() ->
  ?NIF_STUB.
  
  
mpeg2_h264(undefined, #video_frame{} = Frame) ->
  mpeg2_h264(#mpeg2_h264{}, Frame);

mpeg2_h264(#mpeg2_h264{mpeg2 = undefined} = State, #video_frame{} = Frame) ->
  mpeg2_h264(State#mpeg2_h264{mpeg2 = ems_video:init_mpeg2()}, Frame);

mpeg2_h264(#mpeg2_h264{mpeg2 = Mpeg2, buffer = Buffer} = State, #video_frame{codec = mpeg2video, body = Body, dts = DTS} = Frame) ->
  case mpeg2_raw(Mpeg2, <<Buffer/binary, Body/binary>>) of
    {yuv, YUV, Rest} ->
      % io:format("Got YUV~n"),
      encode_h264(State#mpeg2_h264{buffer = Rest}, Frame#video_frame{codec = yuv, body = YUV, dts = DTS, pts = DTS});
    {more, Rest} ->
      io:format("Buffering ~p~n", [size(Rest)]),
      {State#mpeg2_h264{buffer = Rest}, []};
    more ->
      {State#mpeg2_h264{buffer = <<>>}, []};
    {error,invalid_stream} ->  
      {State#mpeg2_h264{buffer = <<>>}, []}
    % _Else ->
    %   % io:format("Buffering MPEG-TS ~p~n", [_Else]),
    %   {State, undefined}
  end.

unpack_config(NALS) ->
  unpack_config(NALS, h264:init()).

unpack_config(<<Len:32, NAL:Len/binary, NALS/binary>>, H264) ->
  {H264_1, _} = h264:decode_nal(NAL, H264),
  unpack_config(NALS, H264_1);

unpack_config(<<>>, H264) ->
  h264:decoder_config(H264).


encode_h264(#mpeg2_h264{x264 = undefined, mpeg2 = Mpeg2} = State, #video_frame{dts = DTS, codec = yuv} = YUV) ->
  {ok, EncoderConfig} = file:read_file(code:lib_dir(h264,priv)++"/encoder.preset"),
  {ok, Encoder, NALS} = init_x264([{width, mpeg2_getopt(Mpeg2, width)}, {height, mpeg2_getopt(Mpeg2, height)}, {config, EncoderConfig}, {preset, "fast"}]),
  Config = unpack_config(NALS),
  
  Frame = #video_frame{
    content = video,
    flavor = config,
    codec = h264,
    pts = DTS, 
    dts = DTS,
    body = Config
  },
  ems_video:yuv_x264(Encoder, YUV),
  {State#mpeg2_h264{x264 = Encoder, config_frame = Frame}, []};

encode_h264(#mpeg2_h264{x264 = Encoder, config_frame = Config} = State, #video_frame{codec = yuv} = YUV) ->
  case ems_video:yuv_x264(Encoder, YUV) of
    ok -> {State, []};
    {ok, Flavor, DTS, PTS, H264} ->
      Frame = #video_frame{
        content = video,
        flavor = Flavor,
        codec = h264,
        dts = DTS,
        pts = PTS,
        body = H264
      },
      % io:format("H264: ~p ~p~n", [DTS, PTS - DTS]),
      case Config of
        undefined -> 
          {State, [Frame]};
        _ ->
          {State#mpeg2_h264{config_frame = undefined}, [Config#video_frame{dts = DTS, pts = DTS}, Frame]}
      end    
  end.
  
  
  
mpeg2_getopt(_Mpeg2, _Option) ->
  ?NIF_STUB.

mpeg2_raw(Mpeg2, Pes) ->
  % real_mpeg2_raw(Mpeg2, <<Pes/binary, 0,0,1,16#FF>>).
  real_mpeg2_raw(Mpeg2, Pes).

real_mpeg2_raw(_Mpeg2, _Pes) ->
  ?NIF_STUB.

