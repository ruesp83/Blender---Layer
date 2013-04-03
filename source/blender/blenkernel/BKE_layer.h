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
 * Contributor(s): ruesp83.
 *
 * ***** END GPL LICENSE BLOCK *****
 */
 #ifndef __BKE_LAYER_H__
#define __BKE_LAYER_H__

/** \file BKE_layer.h
 *  \ingroup bke
 *  \since March 2012
 *  \author ruesp83
 */

#ifdef __cplusplus
extern "C" {
#endif

struct Image;
struct ImageLayer;
struct ImBuf;

/* call from library */

struct ImageLayer *layer_alloc(struct Image *ima, const char *name);

/* Removes all image layers from the image "ima" */
void image_free_image_layers(struct Image *ima);
 
/* Frees an image layer and associated memory */
void free_image_layer(struct ImageLayer *layer);
 
/* Removes the currently selected image layer */
int image_remove_layer(struct Image *ima, const int action);

/* Removes the currently selected image layer */
struct ImageLayer *image_duplicate_current_image_layer(struct Image *ima);
struct ImageLayer *image_duplicate_layer(struct Image *ima, struct ImageLayer *layer);

/* Adds another image layer and selects it */
struct ImageLayer *image_add_image_layer(struct Image *ima, const char *name, int depth, float color[4], int order);

/* Adds the base layer of images that points Image->ibufs.first */
void image_add_image_layer_base(struct Image *ima);
 
/* Returns the index of the currently selected image layer */
short imalayer_get_current_act(struct Image *ima);
short imalayer_get_index_layer(struct Image *ima, struct ImageLayer *iml);

short imalayer_get_count(struct Image *ima);
 
/* Selects the image layer with the number specified in "value" */
void imalayer_set_current_act(struct Image *ima, short value);
 
/* Returns the image layer that is currently selected */
struct ImageLayer *imalayer_get_layer_index(struct Image *ima, short value);
struct ImageLayer *imalayer_get_current(struct Image *ima);

int imalayer_is_locked(struct Image *ima);
 
/* Fills the current selected image layer with the color given */
void imalayer_fill_color(struct Image *ima, float color[4]);

void imalayer_unique_name(struct ImageLayer *iml, struct Image *ima);
//void imalayer_unique_name(const struct ListBase *imlayers, struct ImageLayer *newlayer);

struct ImBuf *imalayer_blend(struct ImBuf *base, struct ImBuf *layer, float opacity, short mode);

struct ImageLayer *merge_layers(struct Image *ima, struct ImageLayer *iml, struct ImageLayer *iml_next);

void merge_layers_visible_nd(struct Image *ima);

unsigned int IML_blend_color(unsigned int src1, unsigned int src2, int opacity, short mode);
void IML_blend_color_float(float *dst, float *src1, float *src2, float opacity, short mode);

void get_color_background_layer(float col[4], struct ImageLayer *layer);

#ifdef __cplusplus
}
#endif

#endif

