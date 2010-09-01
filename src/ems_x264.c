#include "encoder.h"
#include "readjpeg.h"
ErlNifResourceType* x264_resource;

typedef struct {
  Encoder encoder;
} X264;



static void
x264_destructor(ErlNifEnv* env, void* obj)
{
  X264 *x264;
  x264 = obj;
  encoder_close(x264->encoder);
}



static ERL_NIF_TERM
init_x264(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  X264 *x264;
  
  x264 = (X264 *)enif_alloc_resource(x264_resource, sizeof(X264));
  memset(x264, 0, sizeof(X264));
  return enif_make_resource(env, x264);
}

static ERL_NIF_TERM
rgb_x264(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  X264 *x264;
  Data h264;
  ErlNifBinary rgb, out;
  Image *image;
  
  if(!enif_get_resource(env, argv[0], x264_resource, (void **)&x264)) {
    return enif_make_badarg(env);
  }

  if(!enif_inspect_binary(env, argv[1], &rgb)) {
    return enif_make_badarg(env);
  }
  
  image = (Image *)rgb.data;
  
  if(!x264->encoder) {
    x264->encoder = encoder_init(image->width, image->height, NULL);
    printf("Initializing encoder %dx%d\r\n", image->width, image->height);
  }
  
  h264 = encoder_encode(x264->encoder, image->data);

  enif_alloc_binary(h264.size, &out);
  memcpy(out.data, h264.data, h264.size);
  free(h264.data);
  return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_binary(env, &out));
}

