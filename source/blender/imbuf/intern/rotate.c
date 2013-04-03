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
 * Contributor(s): none yet.
 *
 * ***** END GPL LICENSE BLOCK *****
 * rotate.c
 *
 */

/** \file blender/imbuf/intern/rotate.c
 *  \ingroup imbuf
 */

#include "BLI_utildefines.h"

#include "MEM_guardedalloc.h"

#include "BLI_math.h"
#include "imbuf.h"
#include "DNA_imbuf_types.h"
#include "IMB_imbuf.h"

#include "IMB_allocimbuf.h"

void IMB_flipy(struct ImBuf *ibuf)
{
	int x, y;

	if (ibuf == NULL) return;

	if (ibuf->rect) {
		unsigned int *top, *bottom, *line;

		x = ibuf->x;
		y = ibuf->y;

		top = ibuf->rect;
		bottom = top + ((y - 1) * x);
		line = MEM_mallocN(x * sizeof(int), "linebuf");
	
		y >>= 1;

		for (; y > 0; y--) {
			memcpy(line, top, x * sizeof(int));
			memcpy(top, bottom, x * sizeof(int));
			memcpy(bottom, line, x * sizeof(int));
			bottom -= x;
			top += x;
		}

		MEM_freeN(line);
	}

	if (ibuf->rect_float) {
		float *topf = NULL, *bottomf = NULL, *linef = NULL;

		x = ibuf->x;
		y = ibuf->y;

		topf = ibuf->rect_float;
		bottomf = topf + 4 * ((y - 1) * x);
		linef = MEM_mallocN(4 * x * sizeof(float), "linebuff");

		y >>= 1;

		for (; y > 0; y--) {
			memcpy(linef, topf, 4 * x * sizeof(float));
			memcpy(topf, bottomf, 4 * x * sizeof(float));
			memcpy(bottomf, linef, 4 * x * sizeof(float));
			bottomf -= 4 * x;
			topf += 4 * x;
		}

		MEM_freeN(linef);
	}
}

void IMB_flipx(struct ImBuf *ibuf)
{
	int x, y, xr, xl, yi;
	float px_f[4];
	
	if (ibuf == NULL) return;

	x = ibuf->x;
	y = ibuf->y;

	if (ibuf->rect) {
		for (yi = y - 1; yi >= 0; yi--) {
			for (xr = x - 1, xl = 0; xr >= xl; xr--, xl++) {
				SWAP(unsigned int, ibuf->rect[(x * yi) + xr], ibuf->rect[(x * yi) + xl]);
			}
		}
	}
	
	if (ibuf->rect_float) {
		for (yi = y - 1; yi >= 0; yi--) {
			for (xr = x - 1, xl = 0; xr >= xl; xr--, xl++) {
				memcpy(&px_f, &ibuf->rect_float[((x * yi) + xr) * 4], 4 * sizeof(float));
				memcpy(&ibuf->rect_float[((x * yi) + xr) * 4], &ibuf->rect_float[((x * yi) + xl) * 4], 4 * sizeof(float));
				memcpy(&ibuf->rect_float[((x * yi) + xl) * 4], &px_f, 4 * sizeof(float));
			}
		}
	}
}

/*float bilinear_interpolation(unsigned char *outI, float *outF, float x, float y, int width, int height)
{
    int x0 = int(x);
    int y0 = int(y);
    int x1 = x0 + 1;
    int y1 = y1 + 1;
    if (x1 >= width)
		x1 = x0;

    if (y1 >= height)
	    y1 = y0;

    float v0 = input[x0][y0];
    float v1 = input[x1][y0];
    float v2 = input[x0][y1];
    float v3 = input[x1][y1];
    float dx = x - float(x0);
    float dx1 = 1 - dx;
    float vx0 = v1 * dx + v0 * dx1;
    float vx1 = v3 * dx + v2 * dx1;
    float dy = y - float(y0);
    return vx1 * dy + vx0 * (1 - dy);
}*/

struct ImBuf *IMB_rotation(struct ImBuf *ibuf, float x, float y, float angle, int filter_type, float default_color[4])
{
	ImBuf *ibuf2;
	int w, h, flags = 0;
	int offset_d, offset_s, Src_x, Src_y;
	float cosine, sine, p1x, p1y, p2x, p2y, p3x, p3y, minx, miny, maxx, maxy;
	unsigned char *outI_d, *outI_s;
	unsigned char ccol[4];
	float *outF_d, *outF_s;

	if (ibuf) {
		int i, j;
		
		w = ibuf->x;
		h = ibuf->y;

		cosine = (float)cos(angle);
		sine = (float)sin(angle);

		p1x = (-ibuf->y * sine);
		p1y = (ibuf->y * cosine);
		p2x = (ibuf->x * cosine - ibuf->y * sine);
		p2y = (ibuf->y * cosine + ibuf->x * sine);
		p3x = (ibuf->x * cosine);
		p3y = (ibuf->x * sine);

		minx = MIN2(0, MIN2(p1x, MIN2(p2x, p3x)));
		miny = MIN2(0, MIN2(p1y, MIN2(p2y, p3y)));
		maxx = MAX2(p1x, MAX2(p2x, p3x));
		maxy = MAX2(p1y, MAX2(p2y, p3y));

		h = (int)floor(fabs(maxy) - miny); 
		w = (int)floor(fabs(maxx) - minx);

		if (ibuf->rect) flags |= IB_rect;
		if (ibuf->rect_float) flags |= IB_rectfloat;
		
		ibuf2 = IMB_allocImBuf(w, h, ibuf->planes, flags);
		
		if (ibuf->rect)
			rgba_float_to_uchar(ccol, default_color);

		for (j = 0; j < ibuf2->y; j++) {
			for (i = 0; i < ibuf2->x; i++) {

				Src_x = (int)((i + minx) * cosine + (j + miny) * sine); 
				Src_y = (int)((j + miny) * cosine - (i + minx) * sine); 

				if ((Src_x >= 0) && (Src_x < ibuf->x) && (Src_y >=0) && 
					(Src_y < ibuf->y)) {
						offset_d = ibuf2->x * j * 4 + 4 * i;
						offset_s = ibuf->x * Src_y * 4 + 4 * Src_x;
						
						if (ibuf->rect_float) {
							outF_d = NULL;
							outF_s = NULL;
							outF_d = ibuf2->rect_float + offset_d;
							outF_s = ibuf->rect_float + offset_s;

							copy_v4_v4(outF_d, outF_s);
						}
						
						if (ibuf->rect) {
							outI_d = NULL;
							outI_s = NULL;
							outI_d = (unsigned char *)ibuf2->rect + offset_d;
							outI_s = (unsigned char *)ibuf->rect + offset_s;

							outI_d[0] = outI_s[0];
							outI_d[1] = outI_s[1];
							outI_d[2] = outI_s[2];
							outI_d[3] = outI_s[3];
						}

						/*switch (filter_type) {
							case 0:
								neareast_interpolation(ibuf, ibuf2, vec[0], vec[1], i, j);
								break;
							case 1:
								bilinear_interpolation(ibuf, ibuf2, vec[0], vec[1], i, j);
								break;
							case 2: {
								//bicubic_interpolation(ibuf, ibuf2, vec[0], vec[1], i, j);

								bicubic_interpolation_color(ibuf, outI, outF, vec[0], vec[1]);
								break;
						}*/
						//bicubic_interpolation(ibuf, ibuf2, Src_x, Src_y, i, j);
						//bicubic_interpolation_color(ibuf2, outI_d, outF_d, Src_x, Src_y);
				}
				else {
					if (ibuf->rect_float) {
						float *col;
						col = ibuf2->rect_float + ibuf2->x * j * 4 + 4 * i;
						copy_v4_v4(col, default_color);
					}
					if (ibuf->rect) {
						unsigned char *col;
						
						col = (unsigned char *)ibuf2->rect + ibuf2->x * j * 4 + 4 * i;
						col[0] = ccol[0];
						col[1] = ccol[1];
						col[2] = ccol[2];
						col[3] = ccol[3];
					}
				}
				//bicubic_interpolation(ibuf, ibuf2, Src_x, Src_y, i, y);
			}
		}
	}
	IMB_freeImBuf(ibuf);
	return ibuf2;
}
