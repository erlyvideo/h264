#include <mpg123.h>


typedef struct {
  mpg123_handle *mh;
  uint32_t sample_rate;
  uint32_t channels;
  uint32_t samples;
  int enc;
} Mpg123;


ErlNifResourceType* mpg123_resource;


const int FRAME_SIZE = 1024*2*2;

static void
mpg123_destructor(ErlNifEnv* env, void* obj)
{
  Mpg123 *mpg123 = (Mpg123 *)obj;
  mpg123_handle *mh = mpg123->mh;
  mpg123_close(mh);
  mpg123_delete(mh);
}


static ERL_NIF_TERM
init_mpg123(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  mpg123_handle *mh;
  ErlNifBinary config, raw;
  int ret;
  uint32_t *cfg;
  
  if(!enif_inspect_binary(env, argv[0], &config)) {
    return enif_make_badarg(env);
  }
  cfg = (uint32_t *)config.data;
  
  mh = mpg123_new(NULL, &ret);
  if(!mh) {
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "cant_open_mpg123"));
  }
  ret = 0;
  
  
  
  Mpg123 *mpg123 = (Mpg123 *)enif_alloc_resource(mpg123_resource, sizeof(Mpg123));

  mpg123->mh = mh;
  
  mpg123_open_feed(mh);
  mpg123_format_none(mh);
  mpg123_param(mh, MPG123_VERBOSE, 4, 0);
  mpg123->samples = cfg[0];
  mpg123->sample_rate = cfg[1];
  mpg123->channels = cfg[2];
  mpg123_format(mh, mpg123->sample_rate, mpg123->channels == 2 ? MPG123_STEREO : MPG123_MONO, MPG123_ENC_SIGNED_16);

  return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_resource(env, mpg123));
}


static ERL_NIF_TERM
mp3_pcm(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  mpg123_handle *mh;
  size_t converted = 0;
  int res;
  Mpg123 *mpg123;
  ErlNifBinary mp3, pcm;
  
  if(!enif_get_resource(env, argv[0], mpg123_resource, (void **)&mpg123)) {
    return enif_make_badarg(env);
  }
  
  if(!enif_inspect_binary(env, argv[1], &mp3)) {
    return enif_make_badarg(env);
  }
  
  mh = mpg123->mh;

  enif_alloc_binary(mpg123->samples*2*mpg123->channels, &pcm);
  
  res = mpg123_decode(mh, mp3.data, mp3.size, pcm.data, pcm.size, &converted);
  enif_realloc_binary(&pcm, converted);

  switch(res) {
    case MPG123_NEED_MORE:
    case MPG123_OK:
    case MPG123_NEW_FORMAT:
    case MPG123_DONE:
    return enif_make_binary(env, &pcm);
  }
  return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "mpg123"));
}

