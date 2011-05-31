#include <mpeg2dec/mpeg2.h>
#define ATTR_ALIGN(X)
#include "mpeg2_internal.h"

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
  // mpeg2_decore(mpeg2->dec_handle, XVID_DEC_DESTROY, NULL, NULL);
}



static ERL_NIF_TERM
init_mpeg2(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int debug_level = 4, ret;
  Mpeg2 *mpeg2;
  mpeg2dec_t *mpeg2dec;
  
  mpeg2 = (Mpeg2 *)enif_alloc_resource(mpeg2_resource, sizeof(Mpeg2));
  
  // mpeg2_accel(MPEG2_ACCEL_DETECT);
  mpeg2_accel(0);
  mpeg2dec = mpeg2_init();
  mpeg2->info = mpeg2_info(mpeg2dec);
  
  mpeg2_custom_fbuf(mpeg2dec, 1); // enable DR1
  mpeg2->dec = mpeg2dec;
  
  return enif_make_resource(env, mpeg2);
  
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
  
  
  mpeg2dec = mpeg2_state->dec;
  int i;
  fprintf(stderr, "PES(%lu): <<", mpeg2.size);
  for(i = 0; i < 30; i++) {
    fprintf(stderr, "%s%d", i == 0 ? "" : ",", mpeg2.data[i]);
  }
  fprintf(stderr, ">>\r\n");
  
  while(1) {
    int state = mpeg2_parse (mpeg2dec);
    // fprintf(stderr, "State: %d (%d)\r\n", state, mpeg2dec->buf_end - mpeg2dec->buf_start);
    switch(state) {
      case STATE_BUFFER:
      {
        if(flushed_buffer) {
          if(has_raw) {
            // enif_free_binary(&raw);
          }
          return enif_make_atom(env, "more");
        }
        flushed_buffer = 1;
        mpeg2_buffer(mpeg2dec, mpeg2.data, mpeg2.data + mpeg2.size);
        break;
      }
      
      case STATE_SEQUENCE:
      mpeg2_state->width = mpeg2_state->info->sequence->width;
      mpeg2_state->height = mpeg2_state->info->sequence->height;
      // mpeg2_custom_fbuf(mpeg2dec, 1);
      fprintf(stderr, "Seq %dx%d\r\n", mpeg2_state->width, mpeg2_state->height);
      break;

      case STATE_PICTURE:
      {
        fprintf(stderr, "Pic\r\n");
        uint32_t stride_size = mpeg2_state->width*mpeg2_state->height;
        raw = (ErlNifBinary *)malloc(sizeof(ErlNifBinary));
        enif_alloc_binary(stride_size*3/2, raw);
        uint8_t *planes[3] = {raw->data, raw->data + stride_size, raw->data + stride_size*5/4};
        has_raw = 1;
        mpeg2_set_buf(mpeg2dec, planes, raw);
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
        fprintf(stderr, "GOP\r\n");

        break;

      }

    	case STATE_SLICE:
    	case STATE_END:
    	case STATE_INVALID_END:
    	{
        char *name = state == STATE_SLICE ? "slice" : state == STATE_END ? "end" : "invalid_end";
        fprintf(stderr, "Slice: %d %s, %d\r\n", state, name, mpeg2_state->info->display_fbuf != NULL);
        if(mpeg2_state->info->display_fbuf) {
          return enif_make_tuple2(env, enif_make_atom(env, "yuv"), enif_make_binary(env, mpeg2_state->info->display_fbuf->id));
        }
        break;
    	}

      case STATE_INVALID:
      {
        fprintf(stderr, "Invalid stream\r\n");
        if(has_raw) {
          // enif_free_binary(&raw);
        }
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "invalid_stream"));
        break;
      }

    }
    
  }
  return enif_make_atom(env, "ok");
}

