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
 */

/** \file DNA_image_types.h
 *  \ingroup DNA
 */

#ifndef __DNA_IMAGE_TYPES_H__
#define __DNA_IMAGE_TYPES_H__

#include "DNA_defs.h"
#include "DNA_ID.h"
#include "DNA_color_types.h"  /* for color management */

#ifndef MAX_LIMA
#define MAX_LIMA	18
#endif

struct PackedFile;
struct Scene;
struct anim;
struct ImBuf;
struct RenderResult;
struct GPUTexture;


/* ImageUser is in Texture, in Nodes, Background Image, Image Window, .... */
/* should be used in conjunction with an ID * to Image. */
typedef struct ImageUser {
	struct Scene *scene;		/* to retrieve render result */

	int framenr;				/* movies, sequences: current to display */
	int frames;					/* total amount of frames to use */
	int offset, sfra;			/* offset within movie, start frame in global time */
	char fie_ima, cycl;		/* fields/image in movie, cyclic flag */
	char ok, pad;

	short multi_index, layer, pass;	 /* listbase indices, for menu browsing or retrieve buffer */

	short flag;
	char use_layer_ima, pad1;
	short pad2;
} ImageUser;

/* iuser->flag */
#define	IMA_ANIM_ALWAYS		1
#define IMA_ANIM_REFRESHED	2
/* #define IMA_DO_PREMUL	4 */
#define IMA_NEED_FRAME_RECALC	8


typedef struct ImageLayer {
	struct ImageLayer *next, *prev;
	//ID id;
	//struct PreviewImage * preview;
	char name[64];
	char file_path[1024];
	float opacity;
	short background;
	short mode;
	short type;
	short visible;
	short select;
	short locked;
	int pad1;
	//int icon_id;
	float default_color[4];
	int pad2;
	ListBase ibufs;
	struct ImBuf *preview_ibuf;
}ImageLayer;

/* **************** IMAGE LAYER********************* */
#define IMA_LAYER_MAX_LEN	64

/* ImageLayer.background */
#define IMA_LAYER_BG_RGB		(1<<0)
#define IMA_LAYER_BG_WHITE		(1<<1)
#define IMA_LAYER_BG_ALPHA		(1<<2)
#define IMA_LAYER_BG_IMAGE		(1<<3)

/* ImageLayer.mode */
typedef enum ImageLayerMode {
	IMA_LAYER_NORMAL = 0,

	IMA_LAYER_MULTIPLY = 1,
	IMA_LAYER_SCREEN = 2,
	IMA_LAYER_OVERLAY = 3,
	IMA_LAYER_SOFT_LIGHT = 4,
	IMA_LAYER_HARD_LIGHT = 5,

	IMA_LAYER_COLOR_DODGE = 6,
	IMA_LAYER_LINEAR_DODGE = 7,
	IMA_LAYER_COLOR_BURN = 8,
	IMA_LAYER_LINEAR_BURN = 9,

	IMA_LAYER_AVERAGE = 10,
	IMA_LAYER_ADD = 11,
	IMA_LAYER_SUBTRACT = 12,
	IMA_LAYER_DIFFERENCE = 13,
	IMA_LAYER_LIGHTEN = 14,
	IMA_LAYER_DARKEN = 15,

	IMA_LAYER_NEGATION = 16,
	IMA_LAYER_EXCLUSION = 17,
	
	IMA_LAYER_LINEAR_LIGHT = 18,
	IMA_LAYER_VIVID_LIGHT = 19,
	IMA_LAYER_PIN_LIGHT = 20,
	IMA_LAYER_HARD_MIX = 21,
	IMA_LAYER_INVERSE_COLOR_BURN = 22,
	IMA_LAYER_SOFT_BURN = 23
}ImageLayerMode;

/*#define ChannelBlend_Reflect(A,B)    ((uint8)((B == 255) ? B:min(255, (A * A / (255 - B)))))
#define ChannelBlend_Glow(A,B)       (ChannelBlend_Reflect(B,A))
#define ChannelBlend_Phoenix(A,B)    ((uint8)(min(A,B) - max(A,B) + 255))
#define ChannelBlend_Alpha(A,B,O)    ((uint8)(O * A + (1 - O) * B))
#define ChannelBlend_AlphaF(A,B,F,O) (ChannelBlend_Alpha(F(A,B),A,O))

#define ColorBlend_Hue(T,A,B)            ColorBlend_Hls(T,A,B,HueB,LuminationA,SaturationA)
#define ColorBlend_Saturation(T,A,B)     ColorBlend_Hls(T,A,B,HueA,LuminationA,SaturationB)
#define ColorBlend_Color(T,A,B)          ColorBlend_Hls(T,A,B,HueB,LuminationA,SaturationB)
#define ColorBlend_Luminosity(T,A,B)     ColorBlend_Hls(T,A,B,HueA,LuminationB,SaturationA)

#define ColorBlend_Hls(T,A,B,O1,O2,O3) {
    float64 HueA, LuminationA, SaturationA;
    float64 HueB, LuminationB, SaturationL;
    Color_RgbToHls((A)[2],(A)[1],(A)[0], &HueA, &LuminationA, &SaturationA);
    Color_RgbToHls((B)[2],(B)[1],(B)[0], &HueB, &LuminationB, &SaturationB);
    Color_HlsToRgb(O1,O2,O3,&(T)[2],&(T)[1],&(T)[0]);
    }

int32 Color_HueToRgb(float64 M1, float64 M2, float64 Hue, float64 *Channel)
{
    if (Hue < 0.0)
        Hue += 1.0;
    else if (Hue > 1.0)
        Hue -= 1.0;

    if ((6.0 * Hue) < 1.0)
        *Channel = (M1 + (M2 - M1) * Hue * 6.0);
    else if ((2.0 * Hue) < 1.0)
        *Channel = (M2);
    else if ((3.0 * Hue) < 2.0)
        *Channel = (M1 + (M2 - M1) * ((2.0F / 3.0F) - Hue) * 6.0);
    else
        *Channel = (M1);

    return TRUE;
}

int32 Color_RgbToHls(uint8 Red, uint8 Green, uint8 Blue, float64 *Hue, float64 *Lumination, float64 *Saturation)
{
    float64 Delta;
    float64 Max, Min;
    float64 Redf, Greenf, Bluef;

    Redf    = ((float64)Red   / 255.0F);
    Greenf  = ((float64)Green / 255.0F);
    Bluef   = ((float64)Blue  / 255.0F); 

    Max     = max(max(Redf, Greenf), Bluef);
    Min     = min(min(Redf, Greenf), Bluef);

    *Hue        = 0;
    *Lumination = (Max + Min) / 2.0F;
    *Saturation = 0;

    if (Max == Min)
        return TRUE;

    Delta = (Max - Min);

    if (*Lumination < 0.5)
        *Saturation = Delta / (Max + Min);
    else
        *Saturation = Delta / (2.0 - Max - Min);

    if (Redf == Max)
        *Hue = (Greenf - Bluef) / Delta;
    else if (Greenf == Max)
        *Hue = 2.0 + (Bluef - Redf) / Delta;
    else
        *Hue = 4.0 + (Redf - Greenf) / Delta;

    *Hue /= 6.0; 

    if (*Hue < 0.0)
        *Hue += 1.0;       

    return TRUE;
}

int32 Color_HlsToRgb(float64 Hue, float64 Lumination, float64 Saturation, uint8 *Red, uint8 *Green, uint8 *Blue)
{
    float64 M1, M2;
    float64 Redf, Greenf, Bluef;

    if (Saturation == 0)
        {
        Redf    = Lumination;
        Greenf  = Lumination;
        Bluef   = Lumination;
        }
    else
        {
        if (Lumination <= 0.5)
            M2 = Lumination * (1.0 + Saturation);
        else
            M2 = Lumination + Saturation - Lumination * Saturation;

        M1 = (2.0 * Lumination - M2);

        Color_HueToRgb(M1, M2, Hue + (1.0F / 3.0F), &Redf);
        Color_HueToRgb(M1, M2, Hue, &Greenf);
        Color_HueToRgb(M1, M2, Hue - (1.0F / 3.0F), &Bluef);
        }

    *Red    = (uint8)(Redf * 255);
    *Blue   = (uint8)(Bluef * 255);
    *Green  = (uint8)(Greenf * 255);

    return TRUE;
}

*/

/* ImageLayer.type */
#define IMA_LAYER_BASE		(1<<0)
#define IMA_LAYER_LAYER		(1<<1)

/* ImageLayer.visible */
#define IMA_LAYER_VISIBLE	(1<<0)

/* ImageLayer.select */
#define IMA_LAYER_SEL_CURRENT	(1<<0)
#define	IMA_LAYER_SEL_PREVIOUS	(1<<1)
#define	IMA_LAYER_SEL_NEXT		(1<<2)
#define IMA_LAYER_SEL_TOP		(1<<3)
#define IMA_LAYER_SEL_BOTTOM	(1<<4)

/* ImageLayer.locked */
#define IMA_LAYER_LOCK			1
#define IMA_LAYER_LOCK_ALPHA	2

/* Option for delete the layer*/
#define IMA_LAYER_DEL_SELECTED	(1<<0)
#define IMA_LAYER_DEL_HIDDEN	(1<<1)

/* Option for open a image*/
#define IMA_LAYER_OPEN_IMAGE	(1<<0)
#define IMA_LAYER_OPEN_LAYER	(1<<1)

/* Option for Image Node */
#define IMA_USE_LAYER	(1<<0)

typedef struct Image {
	ID id;
	
	char name[1024];			/* file path, 1024 = FILE_MAX */
	
	ListBase ibufs;					/* not written in file */
	struct ImBuf *preview_ibuf;
	struct GPUTexture *gputexture;	/* not written in file */
	
	/* sources from: */
	struct anim *anim;
	struct RenderResult *rr;

	struct RenderResult *renders[8]; /* IMA_MAX_RENDER_SLOT */
	short render_slot, last_render_slot;
	
	short ok, flag;
	short source, type;
	int lastframe;

	/* texture page */
	short tpageflag, totbind;
	short xrep, yrep;
	short twsta, twend;
	unsigned int bindcode;	/* only for current image... */
	unsigned int *repbind;	/* for repeat of parts of images */
	
	struct PackedFile *packedfile;
	struct PreviewImage *preview;

	/* game engine tile animation */
	float lastupdate;
	int lastused;
	short animspeed;
	short pad2;
	
	/* for generated images */
	int gen_x, gen_y;
	char gen_type, gen_flag;
	short gen_depth;
	
	/* display aspect - for UV editing images resized for faster openGL display */
	float aspx, aspy;
	/* color management */
	ColorManagedColorspaceSettings colorspace_settings;
	char alpha_mode;

	char pad[7];
	int Act_Layers;
	int Count_Layers;
	short use_layers;
	short color_space;
	int pad4;
	struct ListBase imlayers;
} Image;


/* **************** IMAGE ********************* */

/* Image.flag */
enum {
	IMA_FIELDS              = (1 << 0),
	IMA_STD_FIELD           = (1 << 1),
	IMA_DO_PREMUL           = (1 << 2),  /* deprecated, should not be used */
	IMA_REFLECT             = (1 << 4),
	IMA_NOCOLLECT           = (1 << 5),
	//IMA_DONE_TAG          = (1 << 6),  // UNUSED
	IMA_OLD_PREMUL          = (1 << 7),
	// IMA_CM_PREDIVIDE     = (1 << 8),  /* deprecated, should not be used */
	IMA_USED_FOR_RENDER     = (1 << 9),
	IMA_USER_FRAME_IN_RANGE = (1 << 10), /* for image user, but these flags are mixed */
	IMA_VIEW_AS_RENDER      = (1 << 11),
	IMA_IGNORE_ALPHA        = (1 << 12),
};

#if (DNA_DEPRECATED_GCC_POISON == 1)
#pragma GCC poison IMA_DO_PREMUL
#endif

/* Image.tpageflag */
#define IMA_TILES			1
#define IMA_TWINANIM		2
#define IMA_COLCYCLE		4	/* Depreciated */
#define IMA_MIPMAP_COMPLETE 8   /* all mipmap levels in OpenGL texture set? */
#define IMA_CLAMP_U			16 
#define IMA_CLAMP_V			32
#define IMA_TPAGE_REFRESH	64
#define IMA_GLBIND_IS_DATA	128 /* opengl image texture bound as non-color data */

/* ima->type and ima->source moved to BKE_image.h, for API */

/* render */
#define IMA_MAX_RENDER_TEXT		512
#define IMA_MAX_RENDER_SLOT		8

/* gen_flag */
#define IMA_GEN_FLOAT		1

/* alpha_mode */
enum {
	IMA_ALPHA_STRAIGHT = 0,
	IMA_ALPHA_PREMUL = 1,
};

/* color_space */
#define IMA_COL_RGB		(1<<0)
#define IMA_COL_GRAY	(1<<1)

#endif
