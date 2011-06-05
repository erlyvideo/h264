-module(ems_sound2).
-author('Max Lapshin <max@maxidoors.ru>').

-on_load(on_load/0).
-export([on_load/0]).
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/mp3.hrl").

-define(NIF_STUB(X), erlang:error({nif_not_implemented, X})).

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
  encoder_bytes,
  encoder_samples,
  buffer = <<>>,
  writer,
  start_dts,
  counter = 0,
  sample_rate,
  sound,
  decoder_config
}).

mp2_aac(undefined, #video_frame{body = Body, dts = DTS} = Frame) ->
  {ok, #mp3_frame{samples = RawSamples, sample_rate = SampleRate, channels = Channels}, _} = mp3:read(Body),
  io:format("Inited MP3: ~p ~p ~p~n", [RawSamples, SampleRate, Channels]),
  {ok, Decoder} = init_mpg123(<<RawSamples:32/little, SampleRate:32/little, Channels:32/little>>),
  {ok, Encoder, Config} = init_faac(<<SampleRate:32/little, Channels:32/little>>),
  SamplesPerFrame = 1024,
  Sound = {
    case Channels of
      1 -> mono;
      _ -> stereo
    end, bit16, rate44
  },
  Transcoder = #transcoder{decoder = Decoder, encoder = Encoder, encoder_bytes = SamplesPerFrame*2*Channels, encoder_samples = SamplesPerFrame, 
                           sound = Sound, start_dts = DTS, sample_rate = SampleRate},
  ConfigFrame = Frame#video_frame{codec = aac, flavor = config, body = Config, sound = Sound},
  {TC1, Frames} = mp2_aac(Transcoder, Frame),
  case Frames of
    [] ->
      {TC1#transcoder{decoder_config = ConfigFrame}, []};
    _ ->  
      {TC1, [ConfigFrame|Frames]}
  end;

  

mp2_aac(#transcoder{} = Transcoder, #video_frame{codec = mpeg2audio, body = Body}) ->
  mp2_aac(Transcoder, Body, []).


mp2_aac(#transcoder{decoder_config = ConfigFrame} = TC, <<>>, [#video_frame{dts = DTS}|_] = Acc) when ConfigFrame =/= undefined ->
  {TC#transcoder{decoder_config = undefined}, [ConfigFrame#video_frame{dts = DTS, pts = DTS}|Acc]};

mp2_aac(#transcoder{} = TC, <<>>, Acc) ->
  {TC, Acc};

mp2_aac(#transcoder{decoder = Decoder, buffer = Buf} = TC, Body, Acc) ->
  {ok, #mp3_frame{body = MP3}, Rest} = mp3:read(Body),
  PCM = mp3_pcm(Decoder, MP3),
  Bin = case Buf of
    <<>> -> PCM;
    _ -> <<Buf/binary, PCM/binary>>
  end,
  {TC1, Frames} = encode(TC, Bin, []),
  mp2_aac(TC1, Rest, Acc++Frames).


encode(#transcoder{encoder_bytes = Length, encoder = Encoder} = TC, Bin, Acc) ->
  case Bin of
    <<PCM:Length/binary, Rest/binary>> ->
      case pcm_aac(Encoder, PCM) of
        undefined ->
          encode(TC, Rest, Acc);
        AAC ->
          {TC1, Frame} = output_frame(TC, AAC),
          encode(TC1, Rest, [Frame|Acc])
      end;
    _ ->
      {TC#transcoder{buffer = Bin}, lists:reverse(Acc)}
  end.

output_frame(#transcoder{encoder_samples = Samples, counter = Counter, sample_rate = SampleRate, start_dts = StartDTS, sound = Sound} = TC, AAC) ->
  DTS = StartDTS + Counter*1000/SampleRate,
  Frame = #video_frame{
    content = audio,
    flavor = frame,
    sound = Sound,
    codec = aac,
    dts = DTS,
    pts = DTS,
    body = AAC
  },
  {TC#transcoder{counter = Counter + Samples}, Frame}.



init_mpg123(_Body) ->
  ?NIF_STUB(init_mpg123).

mp3_pcm(_Decoder, _Body) ->
  ?NIF_STUB(mp3_pcm).

pcm_aac(_Decoder, _Body) ->
  ?NIF_STUB(pcm_aac).

init_faac(_Options) ->
  ?NIF_STUB(init_faac).
