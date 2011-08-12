-module(faac).
-author('Max Lapshin <max@maxidoors.ru>').

-on_load(on_load/0).
-export([on_load/0]).
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

-record(transcoder, {
  decoder,
  encoder,
  options,
  encoder_bytes,
  encoder_samples,
  in_buffer = <<>>,
  pcm_buffer = <<>>,
  writer,
  start_dts,
  counter = 0,
  sample_rate,
  sound,
  decoder_config
}).

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
      {TC#transcoder{pcm_buffer = Bin}, lists:reverse(Acc)}
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



pcm_aac(_Decoder, _Body) ->
  ?NIF_STUB(pcm_aac).

init_faac(_Options) ->
  ?NIF_STUB(init_faac).
