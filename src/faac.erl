-module(faac).
-author('Max Lapshin <max@maxidoors.ru>').

-on_load(on_load/0).
-export([on_load/0]).
-export([init/1, encode/2, pcm_aac/2]).
-include_lib("erlmedia/include/video_frame.hrl").

-define(NIF_STUB(X), erlang:error({nif_not_implemented, X})).

on_load() ->
  Path = case code:lib_dir(h264,priv) of
    {error, _} -> "priv";
    Else -> Else
  end,
  Load = erlang:load_nif(Path ++ "/faac", 0),
  io:format("Load faac: ~p~n", [Load]),
  ok.


encode(Encoder, PCM) ->
  case pcm_aac(Encoder, PCM) of
    undefined -> undefined;
    AAC ->
      #video_frame{
        content = audio,
        flavor = frame,
        codec = aac,
        sound = {stereo, bit16, rate44},
        body = AAC
      }
  end.

init(Options) ->
  SampleRate = proplists:get_value(sample_rate, Options),
  Channels = proplists:get_value(channels, Options),
  {ok, AAC, Config} = init_faac(<<SampleRate:32/little, Channels:32/little>>),
  AConfig = #video_frame{
    content = audio,
    flavor = config,
    codec = aac,
    sound = {stereo, bit16, rate44},
    pts = 0, 
    dts = 0,
    body = Config
  },
  {ok, AAC, AConfig}.

pcm_aac(_Decoder, _Body) ->
  ?NIF_STUB(pcm_aac).

init_faac(_Options) ->
  ?NIF_STUB(init_faac).
