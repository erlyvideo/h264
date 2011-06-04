#include <faac.h>


ErlNifResourceType* faac_resource;

typedef struct {
  faacEncHandle faac;
  unsigned long samples;
  uint32_t sample_rate;
  uint32_t channels;
  unsigned long max_bytes;
} faacState;


static void
faac_destructor(ErlNifEnv* env, void* obj)
{
  faacState *faac = obj;
  faacEncClose(faac->faac);
  free(obj);
}


static ERL_NIF_TERM
init_faac(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  faacEncHandle faac;
  faacState *res;
  faacEncConfigurationPtr faacConfig;
  ErlNifBinary config;

  uint32_t *cfg;
  uint8_t *decoder_ptr;
  unsigned long decoder_length;
  
  if(!enif_inspect_binary(env, argv[0], &config)) {
    return enif_make_badarg(env);
  }
  cfg = (uint32_t *)config.data;

  res = (faacState *)enif_alloc_resource(faac_resource, sizeof(faacState));
  res->sample_rate = cfg[0];
  res->channels = cfg[1];
  
  faac = faacEncOpen(res->sample_rate, res->channels, &res->samples, &res->max_bytes);
  res->faac = faac;
  
  // fprintf(stderr, "FAAC init: %d, %d -> %d, %d\r\n", opt.rate, opt.channels, res->samples, res->max_bytes);
  
  faacConfig = faacEncGetCurrentConfiguration(faac);
  
  faacConfig->mpegVersion   = MPEG4;
  faacConfig->aacObjectType = MAIN;
  faacConfig->allowMidside  = 1;
  faacConfig->useTns        = 0;
  faacConfig->quantqual     = 100;
  // faacConfig->outputFormat  = opt.format;
  faacConfig->inputFormat   = FAAC_INPUT_16BIT;
  faacConfig->bandWidth     = 0;
  
  if(!faacEncSetConfiguration(faac, faacConfig)) {
    fprintf(stderr, "Couldn't set config\r\n");
    faacEncClose(faac);
    enif_release_resource(res);
    return enif_make_badarg(env);
  }

  {
    if( faacEncGetDecoderSpecificInfo( faac, &decoder_ptr, &decoder_length)) {
      fprintf(stderr, "Failed to get decoder info\r\n");
      faacEncClose(faac);
      enif_release_resource(res);
      return enif_make_badarg(env);
    }
  }
  ErlNifBinary decoder_config;
  enif_alloc_binary(decoder_length, &decoder_config);
  memcpy(decoder_config.data, decoder_ptr, decoder_length);

  return enif_make_tuple3(env, enif_make_atom(env, "ok"), enif_make_resource(env, res), enif_make_binary(env, &decoder_config));
}


static ERL_NIF_TERM
pcm_faac(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  faacState *res;
  int out_bytes;
  ErlNifBinary pcm,aac;

  if(!enif_get_resource(env, argv[0], faac_resource, (void **)&res)) {
    return enif_make_badarg(env);
  }
  
  if(!enif_inspect_binary(env, argv[1], &pcm)) {
    return enif_make_badarg(env);
  }
  
  enif_alloc_binary(res->max_bytes, &aac);
  
  out_bytes = faacEncEncode(res->faac, (int32_t *)pcm.data, pcm.size/2, aac.data, aac.size);
  enif_realloc_binary(&aac, out_bytes);
  return enif_make_binary(env, &aac);
}

