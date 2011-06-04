#include <mpg123.h>


typedef struct {
  mpg123_handle *mh;
  uint32_t sample_rate;
  uint32_t channels;
  uint32_t samples;
  uint8_t *buffer;
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

// int mp3_pcm(void *decoder, bin in, bin *out)
// {
//   mpg123_handle *mh;
//   size_t converted = 0;
//   int res;
//   
//   mh = (mpg123_handle *)decoder;
// 
//   assert(out->size >= FRAME_SIZE);
// 
//   
//   res = mpg123_decode(mh, in.body, in.len, out->body, out->size, &converted);
//   out->len = converted;
//   // fprintf(stderr, "res(%d): %lu -> %lu\r\n", res, in.len, converted); 
//   switch(res) {
//     case MPG123_NEED_MORE: return 0;
//     case MPG123_OK: return converted;
//     case MPG123_NEW_FORMAT: fprintf(stderr, "New format\r\n"); return converted;
//     case MPG123_DONE: exit(0);
//   }
//   fprintf(stderr, "mpg123_decoder: %d\r\n", res);
//   abort();
// }
// 
