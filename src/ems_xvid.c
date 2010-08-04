#include <xvid.h>
ErlNifResourceType* xvid_resource;

typedef struct {
  void *dec_handle;
  int csp;
  int xdim;
  int ydim;
  int bpp;
} Xvid;



static void
xvid_destructor(ErlNifEnv* env, void* obj)
{
  Xvid *xvid;
  xvid = obj;
	xvid_decore(xvid->dec_handle, XVID_DEC_DESTROY, NULL, NULL);
}



static ERL_NIF_TERM
init_xvid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int debug_level = 4, ret;
  Xvid *xvid;
  
  xvid_gbl_init_t   xvid_gbl_init;
	xvid_dec_create_t xvid_dec_create;
	
	/* Reset the structure with zeros */
	memset(&xvid_gbl_init, 0, sizeof(xvid_gbl_init_t));
	memset(&xvid_dec_create, 0, sizeof(xvid_dec_create_t));
	
	/*------------------------------------------------------------------------
	 * Xvid core initialization
	 *----------------------------------------------------------------------*/

	/* Version */
	xvid_gbl_init.version = XVID_VERSION;

	/* Assembly setting */
	xvid_gbl_init.cpu_flags = 0; /* XVID_CPU_FORCE */

	xvid_gbl_init.debug = debug_level;

	xvid_global(NULL, 0, &xvid_gbl_init, NULL);

	/*------------------------------------------------------------------------
	 * Xvid decoder initialization
	 *----------------------------------------------------------------------*/

	/* Version */
	xvid_dec_create.version = XVID_VERSION;

	/*
	 * Image dimensions -- set to 0, xvidcore will resize when ever it is
	 * needed
	 */
	xvid_dec_create.width = 0;
	xvid_dec_create.height = 0;

	ret = xvid_decore(NULL, XVID_DEC_CREATE, &xvid_dec_create, NULL);

  xvid = (Xvid *)enif_alloc_resource(xvid_resource, sizeof(Xvid));
  xvid->dec_handle = xvid_dec_create.handle;
  xvid->xdim = 0;
  xvid->ydim = 0;
  xvid->bpp = 1;
  xvid->csp = XVID_CSP_I420;
  printf("Inited xivd: %p\n", xvid->dec_handle);
  
  return enif_make_resource(env, xvid);
  
}

static ERL_NIF_TERM
mpeg4_raw(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  Xvid *xvid;
  ErlNifBinary mpeg4,raw;
  int used_bytes;
  xvid_dec_stats_t xvid_dec_stats;
  xvid_dec_frame_t xvid_dec_frame;
	
  
  if(!enif_get_resource(env, argv[0], xvid_resource, (void **)&xvid)) {
    printf("Need Xvid\r\n");
    return enif_make_badarg(env);
  }
  
  if(!enif_inspect_binary(env, argv[1], &mpeg4)) {
    printf("Need MPEG4\r\n");
    return enif_make_badarg(env);
  }
  
	/* Reset all structures */
	memset(&xvid_dec_frame, 0, sizeof(xvid_dec_frame_t));
	memset(&xvid_dec_stats, 0, sizeof(xvid_dec_stats_t));
	
	/* Set version */
	xvid_dec_frame.version = XVID_VERSION;
	xvid_dec_stats.version = XVID_VERSION;
	xvid_dec_frame.general          = XVID_DEBLOCKY | XVID_DEBLOCKUV;
	
	xvid_dec_frame.bitstream        = mpeg4.data;
	xvid_dec_frame.length           = mpeg4.size;

  if(xvid->xdim && xvid->ydim) {
    enif_alloc_binary(xvid->xdim*xvid->ydim*4, &raw);
  	xvid_dec_frame.output.plane[0]  = raw.data;
  	xvid_dec_frame.output.stride[0] = xvid->xdim*xvid->bpp;
  } else {
    enif_alloc_binary(0, &raw);
  	xvid_dec_frame.output.plane[0]  = NULL;
  	xvid_dec_frame.output.stride[0] = 0;
  }

	/* Output frame structure */
	xvid_dec_frame.output.csp = xvid->csp;

	used_bytes = xvid_decore(xvid->dec_handle, XVID_DEC_DECODE, &xvid_dec_frame, &xvid_dec_stats);
  
  if(xvid_dec_stats.type == XVID_TYPE_VOL) {
    xvid->xdim = xvid_dec_stats.data.vol.width;
    xvid->ydim = xvid_dec_stats.data.vol.height;
	}
  printf("Used: %d/%lu\n", used_bytes, mpeg4.size);
  return enif_make_binary(env, &raw);
}

