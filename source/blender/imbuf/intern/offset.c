/*
 * ***** BEGIN GPL LICENSE BLOCK *****
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 * The Original Code is Copyright (C) 2001-2002 by NaN Holding BV.
 * All rights reserved.
 *
 * The Original Code is: all of this file.
 *
 * Contributor(s): Fabio Russo.
 *
 * ***** END GPL LICENSE BLOCK *****
 * offset.c
 *
 */

/** \file blender/imbuf/intern/offset.c
 *  \ingroup imbuf
 */

#include "BLI_blenlib.h"
#include "BLI_utildefines.h"

#include "BKE_image.h"

#include "MEM_guardedalloc.h"

#include "DNA_image_types.h"

#include "BLI_math.h"
#include "imbuf.h"
#include "DNA_imbuf_types.h"
#include "IMB_imbuf.h"

#include "IMB_allocimbuf.h"

struct ImBuf *IMB_offset(struct ImBuf *ibuf, float x, float y, int half, int wrap, float default_color[4])
{
	int i, j, x1, y1;
	struct ImBuf *ibuf_2;
	unsigned char ccol[4];
	unsigned char *col_d, *col_s;
	float *f_col_d, *f_col_s;

	if (ibuf == NULL) return NULL;

	ibuf_2 = IMB_dupImBuf(ibuf);

	if (ibuf_2 == NULL) return NULL;

	if (half) {
		y = (int)(ibuf->y / 2); 
		x = (int)(ibuf->x / 2);
	}

	if ((x != 0) || (y != 0)) {
		
		if (ibuf->rect) {
			if (!wrap)
				rgba_float_to_uchar(ccol, default_color);
		}

		for (j = 0; j < ibuf->y; j++) {
			for (i = 0; i < ibuf->x; i++) {

				if (x != 0) {
					if (x > 0) {
						x1 = i + (x - 1);
						if (x1 >= ibuf_2->x)
							x1 = x1 - ibuf->x;
					}
					else {
						x1 = i + (x + 1);
						if (x1 < 0)
							x1 = x1 + ibuf->x;
					}
				}
				else
					x1 = i;

				if (y != 0) {
					if (y > 0) {
						y1 = j + (y - 1);
						if (y1 >= ibuf_2->y)
							y1 = y1 - ibuf->y;
					}
					else {
						y1 = j + (y + 1);
						if (y1 < 0)
							y1 = y1 + ibuf->y;

					}
				}
				else
					y1 = j;
				
				if (ibuf->rect) {
					col_d = NULL;
					col_s = NULL;
					col_d = (unsigned char *)ibuf_2->rect + ibuf_2->x * y1 * 4 + 4 * x1;
					col_s = (unsigned char *)ibuf->rect + ibuf->x * j * 4 + 4 * i;
				}

				if (ibuf->rect_float) {
					f_col_d = NULL;
					f_col_s = NULL;
					f_col_d = ibuf_2->rect_float + ibuf_2->x * y1 * 4 + 4 * x1;
					f_col_s = ibuf->rect_float + ibuf->x * j * 4 + 4 * i;
				}

				if (wrap) {
					if (ibuf->rect) {
						col_d[0] = col_s[0];
						col_d[1] = col_s[1];
						col_d[2] = col_s[2];
						col_d[3] = col_s[3];
					}
					if (ibuf->rect_float)
						copy_v4_v4(f_col_d, f_col_s);
				}
				else {
					if (((i + (x - 1)) < ibuf->x) && ((j + (y - 1)) < ibuf->y)) {
						if (ibuf->rect) {
							col_d[0] = col_s[0];
							col_d[1] = col_s[1];
							col_d[2] = col_s[2];
							col_d[3] = col_s[3];
						}
						if (ibuf->rect_float)
							copy_v4_v4(f_col_d, f_col_s);
						}
					else {
						if (ibuf->rect) {
							col_d[0] = ccol[0];
							col_d[1] = ccol[1];
							col_d[2] = ccol[2];
							col_d[3] = ccol[3];
						}
						if (ibuf->rect_float)
							copy_v4_v4(f_col_d, default_color);
					}
				}
			}
		}
	}

	IMB_freeImBuf(ibuf);
	return ibuf_2;
}

struct ImBuf *IMB_size(struct ImBuf *ibuf, int width, int height, int off_x, int off_y, int centre, float default_color[4])
{
	struct ImBuf *ibuf_2;
	int off_x_n = 0, off_y_n = 0;

	if (ibuf == NULL) return NULL;

	if (ibuf->rect_float)
		ibuf_2 = IMB_allocImBuf(width, height, ibuf->planes, IB_rectfloat);
	else
		ibuf_2 = IMB_allocImBuf(width, height, ibuf->planes, IB_rect);

	if (ibuf_2 == NULL) return NULL;

	if ((width != 0) || (height != 0)) {
		if ((width > ibuf->x) || (height > ibuf->y))
			BKE_image_buf_fill_color((unsigned char*)ibuf_2->rect, ibuf_2->rect_float, width, height, default_color);
	
		if (centre) {
			printf("1\n");
			off_x = (width - ibuf->x) / 2;
			off_y = (height - ibuf->y) / 2;
		}

		IMB_rectcpy(ibuf_2, ibuf, off_x, off_y, 0, 0, ibuf->x, ibuf->y);
	}

	IMB_freeImBuf(ibuf);
	return ibuf_2;
}