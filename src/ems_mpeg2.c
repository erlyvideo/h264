#ifdef ATTRIBUTE_ALIGNED_MAX
#define ATTR_ALIGN(align) __attribute__ ((__aligned__ ((ATTRIBUTE_ALIGNED_MAX < align) ? ATTRIBUTE_ALIGNED_MAX : align)))
#else
#define ATTR_ALIGN(align)
#endif
#include "../libmpeg2/mpeg2.h"
// #include <mpeg2dec/mpeg2.h>
// #include "mpeg2_internal.h"

ErlNifResourceType* mpeg2_resource;

typedef struct {
  mpeg2dec_t *dec;
  void *user_data;
  const mpeg2_info_t * info;
  uint32_t user_data_len;
  int width;
  int height;
} Mpeg2;



static void
mpeg2_destructor(ErlNifEnv* env, void* obj)
{
  Mpeg2 *mpeg2;
  mpeg2 = obj;
  mpeg2_close(mpeg2->dec);
}



static ERL_NIF_TERM
init_mpeg2(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  
  int debug_level = 4, ret;
  Mpeg2 *mpeg2;
  mpeg2dec_t *mpeg2dec;
  
  mpeg2 = (Mpeg2 *)enif_alloc_resource(mpeg2_resource, sizeof(Mpeg2));
  
  // mpeg2_accel(MPEG2_ACCEL_DETECT);
  mpeg2->dec = mpeg2_init();
  mpeg2->info = mpeg2_info(mpeg2->dec);
  // mpeg2_custom_fbuf(mpeg2dec, 1); // enable DR1
  
  return enif_make_resource(env, mpeg2);
  
}

static ERL_NIF_TERM mpeg2_getopt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  Mpeg2 *mpeg2_state;
  if(!enif_get_resource(env, argv[0], mpeg2_resource, (void **)&mpeg2_state)) {
    return enif_make_badarg(env);
  }
  
  if(!enif_compare(argv[1], enif_make_atom(env, "width"))) {
    return enif_make_int(env, mpeg2_state->width);
  }

  if(!enif_compare(argv[1], enif_make_atom(env, "height"))) {
    return enif_make_int(env, mpeg2_state->height);
  }
  
  return enif_make_atom(env, "undefined");
}

static ERL_NIF_TERM
mpeg2_raw(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  Mpeg2 *mpeg2_state;
  mpeg2dec_t *mpeg2dec;
  ErlNifBinary mpeg2;
  ErlNifBinary *raw = NULL;
  int flushed_buffer = 0;
  int has_raw = 0;
  int used_bytes;
  
  
  if(!enif_get_resource(env, argv[0], mpeg2_resource, (void **)&mpeg2_state)) {
    return enif_make_badarg(env);
  }
  
  if(!enif_inspect_binary(env, argv[1], &mpeg2)) {
    return enif_make_badarg(env);
  }
  
  // static int started = 0;
  // if(!started && mpeg2.size < 80000) return enif_make_atom(env, "more");
  // started = 1;
  
  mpeg2dec = mpeg2_state->dec;
  // int i;
  // fprintf(stderr, "PES(%lu): <<", mpeg2.size);
  // for(i = 0; i < 30; i++) {
  //   fprintf(stderr, "%s%d", i == 0 ? "" : ",", mpeg2.data[i]);
  // }
  // fprintf(stderr, ">>\r\n");
  
  mpeg2_buffer(mpeg2dec, mpeg2.data, mpeg2.data + mpeg2.size);
  
  
  
  while(1) {
    // uint8_t *s, *e;
    // s = mpeg2_getbuffer(mpeg2dec);
    // e = mpeg2_getbuffer(mpeg2dec) + mpeg2_getpos(mpeg2dec);
    // fprintf(stderr, "Before parse: %d\r\n", mpeg2_getpos(mpeg2dec));
    int state = mpeg2_parse (mpeg2dec);
    // fprintf(stderr, "After parse: %d\r\n", mpeg2_getpos(mpeg2dec));
    switch(state) {
      case STATE_BUFFER:
      {
        // fprintf(stderr, "STATE_BUFFER: %d\r\n", mpeg2_getpos(mpeg2dec));
        return enif_make_atom(env, "more");
      }
      
      case STATE_SEQUENCE:
      mpeg2_state->width = mpeg2_state->info->sequence->width;
      mpeg2_state->height = mpeg2_state->info->sequence->height;
      // mpeg2_custom_fbuf(mpeg2dec, 1);
      // fprintf(stderr, "Seq %dx%d\r\n", mpeg2_state->width, mpeg2_state->height);
      break;

      case STATE_PICTURE:
      {
        // fprintf(stderr, "Pic\r\n");
        // uint32_t stride_size = mpeg2_state->width*mpeg2_state->height;
        // raw = (ErlNifBinary *)malloc(sizeof(ErlNifBinary));
        // enif_alloc_binary(stride_size*3/2, raw);
        // uint8_t *planes[3] = {raw->data, raw->data + stride_size, raw->data + stride_size*5/4};
        // has_raw = 1;
        // mpeg2_set_buf(mpeg2dec, planes, raw);
        break;
      }

      case STATE_GOP:
      {
        // if(mpeg2_state->info->user_data_len > 2) {
        //   if(mpeg2_state->user_data) free(mpeg2_state->user_data);
        //   mpeg2_state->user_data = calloc(1, mpeg2_state->info->user_data_len);
        //   mpeg2_state->user_data_len = mpeg2_state->info->user_data_len;
        //   memcpy(mpeg2_state->user_data, mpeg2_state->user_data, mpeg2_state->info->user_data_len);
        // }
        // fprintf(stderr, "GOP\r\n");

        break;

      }

    	case STATE_SLICE:
    	case STATE_END:
    	case STATE_INVALID_END:
    	{
        char *name = state == STATE_SLICE ? "slice" : state == STATE_END ? "end" : "invalid_end";
        
        if(mpeg2_state->info->display_fbuf) {
          // return enif_make_atom(env, "yuv");
          uint32_t stride_size = mpeg2_state->width*mpeg2_state->height;
          ErlNifBinary raw;
          enif_alloc_binary(stride_size*3/2, &raw);
          memcpy(raw.data, mpeg2_state->info->display_fbuf->buf[0], stride_size);
          memcpy(raw.data+stride_size, mpeg2_state->info->display_fbuf->buf[1], stride_size/4);
          memcpy(raw.data+stride_size*5/4, mpeg2_state->info->display_fbuf->buf[2], stride_size/4);
          // fprintf(stderr, "STATE_SLICE %d, %d\r\n", state, mpeg2_getpos(mpeg2dec));
          return enif_make_tuple3(env, enif_make_atom(env, "yuv"), 
            enif_make_binary(env, &raw), enif_make_sub_binary(env, argv[1], mpeg2_getbuffer(mpeg2dec) - mpeg2.data, mpeg2_getpos(mpeg2dec)));
          
          // return enif_make_tuple2(env, enif_make_atom(env, "yuv"), enif_make_binary(env, mpeg2_state->info->display_fbuf->id));
        }
        // fprintf(stderr, "Fuck, no buf\r\n");
        return enif_make_tuple2(env, enif_make_atom(env, "more"), enif_make_sub_binary(env, argv[1], mpeg2_getbuffer(mpeg2dec) - mpeg2.data, mpeg2_getpos(mpeg2dec)));
    	}

      case STATE_INVALID:
      {
        fprintf(stderr, "Invalid stream\r\n");
  // int i;
  // fprintf(stderr, "PES(%lu): <<", mpeg2.size);
  // for(i = 0; i < 30; i++) {
  //   fprintf(stderr, "%s%d", i == 0 ? "" : ",", mpeg2.data[i]);
  // }
  // fprintf(stderr, ">>\r\n");
        mpeg2_close(mpeg2dec);
        mpeg2dec = mpeg2_init();
        mpeg2_state->info = mpeg2_info(mpeg2dec);
        mpeg2_state->dec = mpeg2dec;
        
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "invalid_stream"));
        break;
      }

    }
    
  }
  return enif_make_atom(env, "ok");
}

