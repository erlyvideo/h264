#ifndef _READJPEG_H_
#define _READJPEG_H_

#include <stdio.h>
#include <stdint.h>
#include <jpeglib.h>
#include <setjmp.h>

extern uint8_t *readjpeg(char *filename);

#endif /* _READJPEG_H_ */
