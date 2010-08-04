#include "readjpeg.h"


static ERL_NIF_TERM
jpeg_rgb(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary jpeg,rgb;
  Image image, *img1;
  
  if(!enif_inspect_binary(env, argv[0], &jpeg)) {
    return enif_make_badarg(env);
  }
  
  image = readjpeg_info(jpeg.data, jpeg.size);
  enif_alloc_binary(sizeof(image) + image.width*image.height*3, &rgb);
  memcpy(rgb.data, &image, sizeof(Image));
  ((Image *)rgb.data)->data = rgb.data + sizeof(Image);
  
  img1 = readjpeg(jpeg.data, jpeg.size, (Image *)rgb.data);

  return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_binary(env, &rgb));
}

