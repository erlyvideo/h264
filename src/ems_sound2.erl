-module(ems_sound2).
-author('Max Lapshin <max@maxidoors.ru>').

-on_load(on_load/0).
-export([on_load/0]).
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/mp3.hrl").

-define(NIF_STUB, erlang:error(nif_not_loaded_ems_sound2)).

-export([mp2_aac/2]).

on_load() ->
  load_nif(erlang:system_info(otp_release) >= "R14A").
  
load_nif(true) ->
  Load = erlang:load_nif(code:lib_dir(h264,ebin) ++ "/ems_sound2", 0),
  io:format("Load ems_sound2: ~p~n", [Load]),
  ok;

load_nif(false) ->
  ok.


-record(transcoder, {
  decoder,
  encoder,
  options,
  decoder_samples,
  encoder_samples,
  buffer = <<>>,
  writer
}).

mp2_aac(undefined, #video_frame{body = Body} = Frame) ->
  {ok, #mp3_frame{samples = RawSamples, sample_rate = SampleRate, channels = Channels}, _} = mp3:read(Body),
  io:format("Hm, initing mpg123: ~p ~p ~p~n", [RawSamples, SampleRate, Channels]),
  {ok, Decoder} = init_mpg123(<<RawSamples:32/little, SampleRate:32/little, Channels:32/little>>),
  % {Encoder, Config, SamplesPerFrame} = init_faac(Options),
  Encoder = undefined,
  Config = <<18,5>>,
  SamplesPerFrame = 1024,
  {ok, F} = file:open("out.mp3", [write,binary]),
  Transcoder = #transcoder{decoder = Decoder, encoder = Encoder, decoder_samples = RawSamples, encoder_samples = SamplesPerFrame, writer = F},
  _ConfigFrame = Frame#video_frame{codec = aac, flavor = config, body = Config},
  file:write(F, Body),
  % io:format("mp2: ~p~n", [Body]),
  {Transcoder, []};
  
  
  

mp2_aac(#transcoder{writer = F} = Transcoder, #video_frame{codec = mpeg2audio, body = Body} = Frame) ->
  file:write(F, Body),
  io:format("Transcoding sound~n"),
  {Transcoder, [Frame#video_frame{codec = pcm, sound = {stereo, bit16, rate44}}]}.



init_mpg123(_Body) ->
  ?NIF_STUB.


init_faac(_Options) ->
  ?NIF_STUB.
