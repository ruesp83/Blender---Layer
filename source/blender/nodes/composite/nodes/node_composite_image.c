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
 * The Original Code is Copyright (C) 2006 Blender Foundation.
 * All rights reserved.
 *
 * The Original Code is: all of this file.
 *
 * Contributor(s): none yet.
 *
 * ***** END GPL LICENSE BLOCK *****
 */

/** \file blender/nodes/composite/nodes/node_composite_image.c
 *  \ingroup cmpnodes
 */

#include "node_composite_util.h"

#include "BKE_global.h"
#include "BKE_main.h"

/* **************** IMAGE (and RenderResult, multilayer image) ******************** */

static bNodeSocketTemplate cmp_node_rlayers_out[] = {
	{	SOCK_RGBA, 0, N_("Image"),					0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f},
	{	SOCK_FLOAT, 0, N_("Alpha"),					1.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f},
	{	SOCK_FLOAT, 0, N_("Z"),						1.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f},
	{	SOCK_VECTOR, 0, N_("Normal"),				0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f},
	{	SOCK_VECTOR, 0, N_("UV"),					1.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f},
	{	SOCK_VECTOR, 0, N_("Speed"),				1.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f},
	{	SOCK_RGBA, 0, N_("Color"),					0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f},
	{	SOCK_RGBA, 0, N_("Diffuse"),				0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f},
	{	SOCK_RGBA, 0, N_("Specular"),				0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f},
	{	SOCK_RGBA, 0, N_("Shadow"),					0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f},
	{	SOCK_RGBA, 0, N_("AO"),						0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f},
	{	SOCK_RGBA, 0, N_("Reflect"),				0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f},
	{	SOCK_RGBA, 0, N_("Refract"),				0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f},
	{	SOCK_RGBA, 0, N_("Indirect"),				0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f},
	{	SOCK_FLOAT, 0, N_("IndexOB"),				0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f},
	{	SOCK_FLOAT, 0, N_("IndexMA"),				0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f},
	{	SOCK_FLOAT, 0, N_("Mist"),					0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f},
	{	SOCK_RGBA, 0, N_("Emit"),					0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f},
	{	SOCK_RGBA, 0, N_("Environment"),			0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f},
	{	SOCK_RGBA, 0, N_("Diffuse Direct"),			0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f},
	{	SOCK_RGBA, 0, N_("Diffuse Indirect"),		0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f},
	{	SOCK_RGBA, 0, N_("Diffuse Color"),			0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f},
	{	SOCK_RGBA, 0, N_("Glossy Direct"),			0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f},
	{	SOCK_RGBA, 0, N_("Glossy Indirect"),		0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f},
	{	SOCK_RGBA, 0, N_("Glossy Color"),			0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f},
	{	SOCK_RGBA, 0, N_("Transmission Direct"),	0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f},
	{	SOCK_RGBA, 0, N_("Transmission Indirect"),	0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f},
	{	SOCK_RGBA, 0, N_("Transmission Color"),		0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f},
	{	-1, 0, ""	}
};

static bNodeSocket *cmp_node_image_add_render_pass_output(bNodeTree *ntree, bNode *node, int pass, int rres_index)
{
	bNodeSocket *sock;
	NodeImageLayer *sockdata;
	
	sock = node_add_socket_from_template(ntree, node, &cmp_node_rlayers_out[rres_index], SOCK_OUT);
	/* extra socket info */
	sockdata = MEM_callocN(sizeof(NodeImageLayer), "node image layer");
	sock->storage = sockdata;
	
	sockdata->pass_flag = pass;
	
	return sock;
}

static void cmp_node_image_add_render_pass_outputs(bNodeTree *ntree, bNode *node, int passflag)
{
	if (passflag & SCE_PASS_COMBINED) {
		cmp_node_image_add_render_pass_output(ntree, node, SCE_PASS_COMBINED, RRES_OUT_IMAGE);
		cmp_node_image_add_render_pass_output(ntree, node, SCE_PASS_COMBINED, RRES_OUT_ALPHA);
	}
	
	if (passflag & SCE_PASS_Z)
		cmp_node_image_add_render_pass_output(ntree, node, SCE_PASS_Z, RRES_OUT_Z);
	if (passflag & SCE_PASS_NORMAL)
		cmp_node_image_add_render_pass_output(ntree, node, SCE_PASS_NORMAL, RRES_OUT_NORMAL);
	if (passflag & SCE_PASS_VECTOR)
		cmp_node_image_add_render_pass_output(ntree, node, SCE_PASS_VECTOR, RRES_OUT_VEC);
	if (passflag & SCE_PASS_UV)
		cmp_node_image_add_render_pass_output(ntree, node, SCE_PASS_UV, RRES_OUT_UV);
	if (passflag & SCE_PASS_RGBA)
		cmp_node_image_add_render_pass_output(ntree, node, SCE_PASS_RGBA, RRES_OUT_RGBA);
	if (passflag & SCE_PASS_DIFFUSE)
		cmp_node_image_add_render_pass_output(ntree, node, SCE_PASS_DIFFUSE, RRES_OUT_DIFF);
	if (passflag & SCE_PASS_SPEC)
		cmp_node_image_add_render_pass_output(ntree, node, SCE_PASS_SPEC, RRES_OUT_SPEC);
	if (passflag & SCE_PASS_SHADOW)
		cmp_node_image_add_render_pass_output(ntree, node, SCE_PASS_SHADOW, RRES_OUT_SHADOW);
	if (passflag & SCE_PASS_AO)
		cmp_node_image_add_render_pass_output(ntree, node, SCE_PASS_AO, RRES_OUT_AO);
	if (passflag & SCE_PASS_REFLECT)
		cmp_node_image_add_render_pass_output(ntree, node, SCE_PASS_REFLECT, RRES_OUT_REFLECT);
	if (passflag & SCE_PASS_REFRACT)
		cmp_node_image_add_render_pass_output(ntree, node, SCE_PASS_REFRACT, RRES_OUT_REFRACT);
	if (passflag & SCE_PASS_INDIRECT)
		cmp_node_image_add_render_pass_output(ntree, node, SCE_PASS_INDIRECT, RRES_OUT_INDIRECT);
	if (passflag & SCE_PASS_INDEXOB)
		cmp_node_image_add_render_pass_output(ntree, node, SCE_PASS_INDEXOB, RRES_OUT_INDEXOB);
	if (passflag & SCE_PASS_INDEXMA)
		cmp_node_image_add_render_pass_output(ntree, node, SCE_PASS_INDEXMA, RRES_OUT_INDEXMA);
	if (passflag & SCE_PASS_MIST)
		cmp_node_image_add_render_pass_output(ntree, node, SCE_PASS_MIST, RRES_OUT_MIST);
	if (passflag & SCE_PASS_EMIT)
		cmp_node_image_add_render_pass_output(ntree, node, SCE_PASS_EMIT, RRES_OUT_EMIT);
	if (passflag & SCE_PASS_ENVIRONMENT)
		cmp_node_image_add_render_pass_output(ntree, node, SCE_PASS_ENVIRONMENT, RRES_OUT_ENV);
	
	if (passflag & SCE_PASS_DIFFUSE_DIRECT)
		cmp_node_image_add_render_pass_output(ntree, node, SCE_PASS_DIFFUSE_DIRECT, RRES_OUT_DIFF_DIRECT);
	if (passflag & SCE_PASS_DIFFUSE_INDIRECT)
		cmp_node_image_add_render_pass_output(ntree, node, SCE_PASS_DIFFUSE_INDIRECT, RRES_OUT_DIFF_INDIRECT);
	if (passflag & SCE_PASS_DIFFUSE_COLOR)
		cmp_node_image_add_render_pass_output(ntree, node, SCE_PASS_DIFFUSE_COLOR, RRES_OUT_DIFF_COLOR);
	
	if (passflag & SCE_PASS_GLOSSY_DIRECT)
		cmp_node_image_add_render_pass_output(ntree, node, SCE_PASS_GLOSSY_DIRECT, RRES_OUT_GLOSSY_DIRECT);
	if (passflag & SCE_PASS_GLOSSY_INDIRECT)
		cmp_node_image_add_render_pass_output(ntree, node, SCE_PASS_GLOSSY_INDIRECT, RRES_OUT_GLOSSY_INDIRECT);
	if (passflag & SCE_PASS_GLOSSY_COLOR)
		cmp_node_image_add_render_pass_output(ntree, node, SCE_PASS_GLOSSY_COLOR, RRES_OUT_GLOSSY_COLOR);
	
	if (passflag & SCE_PASS_TRANSM_DIRECT)
		cmp_node_image_add_render_pass_output(ntree, node, SCE_PASS_TRANSM_DIRECT, RRES_OUT_TRANSM_DIRECT);
	if (passflag & SCE_PASS_TRANSM_INDIRECT)
		cmp_node_image_add_render_pass_output(ntree, node, SCE_PASS_TRANSM_INDIRECT, RRES_OUT_TRANSM_INDIRECT);
	if (passflag & SCE_PASS_TRANSM_COLOR)
		cmp_node_image_add_render_pass_output(ntree, node, SCE_PASS_TRANSM_COLOR, RRES_OUT_TRANSM_COLOR);
}

static void cmp_node_image_add_multilayer_outputs(bNodeTree *ntree, bNode *node, RenderLayer *rl)
{
	bNodeSocket *sock;
	NodeImageLayer *sockdata;
	RenderPass *rpass;
	int index;
	for (rpass=rl->passes.first, index=0; rpass; rpass=rpass->next, ++index) {
		int type;
		if (rpass->channels == 1)
			type = SOCK_FLOAT;
		else
			type = SOCK_RGBA;
		
		sock = nodeAddStaticSocket(ntree, node, SOCK_OUT, type, PROP_NONE, rpass->name, rpass->name);
		/* extra socket info */
		sockdata = MEM_callocN(sizeof(NodeImageLayer), "node image layer");
		sock->storage = sockdata;
		
		sockdata->pass_index = index;
		sockdata->pass_flag = rpass->passtype;
	}
}

static void cmp_node_image_create_outputs(bNodeTree *ntree, bNode *node)
{
	Image *ima= (Image *)node->id;
	if (ima) {
		ImageUser *iuser = node->storage;
		ImageUser load_iuser = {NULL};
		ImBuf *ibuf;
		int offset = BKE_image_sequence_guess_offset(ima);

		/* It is possible that image user in this node is not
		 * properly updated yet. In this case loading image will
		 * fail and sockets detection will go wrong.
		 *
		 * So we manually construct image user to be sure first
		 * image from sequence (that one which is set as fileanme
		 * for image datablock) is used for sockets detection
		 */
		load_iuser.ok = 1;
		load_iuser.framenr = offset;

		/* make sure ima->type is correct */
<<<<<<< .mine
		BKE_image_get_ibuf(ima, iuser, IMA_IBUF_IMA);
=======
		ibuf = BKE_image_acquire_ibuf(ima, &load_iuser, NULL);
>>>>>>> .r55757
		
		if (ima->rr) {
			RenderLayer *rl= BLI_findlink(&ima->rr->layers, iuser->layer);
			
			if (rl) {
				if (ima->type!=IMA_TYPE_MULTILAYER)
					cmp_node_image_add_render_pass_outputs(ntree, node, rl->passflag);
				else
					cmp_node_image_add_multilayer_outputs(ntree, node, rl);
			}
			else
				cmp_node_image_add_render_pass_outputs(ntree, node, RRES_OUT_IMAGE|RRES_OUT_ALPHA);
		}
		else
			cmp_node_image_add_render_pass_outputs(ntree, node, RRES_OUT_IMAGE|RRES_OUT_ALPHA|RRES_OUT_Z);
		
		BKE_image_release_ibuf(ima, ibuf, NULL);
	}
	else
		cmp_node_image_add_render_pass_outputs(ntree, node, RRES_OUT_IMAGE|RRES_OUT_ALPHA);
}

static bNodeSocket *cmp_node_image_output_find_match(bNode *UNUSED(node), bNodeSocket *newsock, ListBase *oldsocklist)
{
	bNodeSocket *sock;
	
	for (sock=oldsocklist->first; sock; sock = sock->next)
		if (STREQ(sock->name, newsock->name))
			return sock;
	return NULL;
}

static bNodeSocket *cmp_node_image_output_relink(bNode *node, bNodeSocket *oldsock, int oldindex)
{
	bNodeSocket *sock;
	
	/* first try to find matching socket name */
	for (sock = node->outputs.first; sock; sock = sock->next)
		if (STREQ(sock->name, oldsock->name))
			return sock;
	
	/* no matching name, simply link to same index */
	return BLI_findlink(&node->outputs, oldindex);
}

static void cmp_node_image_sync_output(bNode *UNUSED(node), bNodeSocket *UNUSED(newsock), bNodeSocket *UNUSED(oldsock))
{
	/* pass */
}

/* XXX make this into a generic socket verification function for dynamic socket replacement (multilayer, groups, static templates) */
static void cmp_node_image_verify_outputs(bNodeTree *ntree, bNode *node)
{
	bNodeSocket *newsock, *oldsock, *oldsock_next;
	ListBase oldsocklist;
	int oldindex;
	bNodeLink *link;
	
	/* store current nodes in oldsocklist, then clear socket list */
	oldsocklist = node->outputs;
	node->outputs.first = node->outputs.last = NULL;
	
	/* XXX make callback */
	cmp_node_image_create_outputs(ntree, node);
	
	for (newsock = node->outputs.first; newsock; newsock=newsock->next) {
		/* XXX make callback */
		oldsock = cmp_node_image_output_find_match(node, newsock, &oldsocklist);
		if (oldsock) {
			/* XXX make callback */
			cmp_node_image_sync_output(node, newsock, oldsock);
		}
	}
	
	/* move links to new socket */
	for (oldsock=oldsocklist.first, oldindex=0; oldsock; oldsock=oldsock->next, ++oldindex) {
		newsock = cmp_node_image_output_relink(node, oldsock, oldindex);
		
		if (newsock) {
			for (link=ntree->links.first; link; link=link->next) {
				if (link->fromsock == oldsock)
					link->fromsock = newsock;
			}
		}
	}
	
	/* delete old sockets
	 * XXX oldsock is not actually in the node->outputs list any more,
	 * but the nodeRemoveSocket function works anyway. In future this
	 * should become part of the core code, so can take care of this behavior.
	 */
	for (oldsock=oldsocklist.first; oldsock; oldsock=oldsock_next) {
		oldsock_next = oldsock->next;
		MEM_freeN(oldsock->storage);
		nodeRemoveSocket(ntree, node, oldsock);
	}
}

static void cmp_node_image_update(bNodeTree *ntree, bNode *node)
{
	/* avoid unnecessary updates, only changes to the image/image user data are of interest */
	if (node->update & NODE_UPDATE_ID)
		cmp_node_image_verify_outputs(ntree, node);
}

static void node_composit_init_image(bNodeTree *ntree, bNode *node)
{
<<<<<<< .mine
	float *rect;
	int predivide= (ibuf->flags & IB_cm_predivide);

	*alloc= FALSE;

	/* OCIO_TODO: this is a part of legacy compositor system, don't bother with porting this code
	 *            to new color management system since this code would likely be simply removed soon
	 */
	if (rd->color_mgt_flag & R_COLOR_MANAGEMENT) {
		rect= ibuf->rect_float;
	}
	else {
		rect= MEM_mapallocN(sizeof(float) * 4 * ibuf->x * ibuf->y, "node_composit_get_image");

		IMB_buffer_float_from_float(rect, ibuf->rect_float,
			4, IB_PROFILE_SRGB, IB_PROFILE_LINEAR_RGB, predivide,
			ibuf->x, ibuf->y, ibuf->x, ibuf->x);

			*alloc= TRUE;
	}

	return rect;
}

/* note: this function is used for multilayer too, to ensure uniform 
 * handling with BKE_image_get_ibuf() */
static CompBuf *node_composit_get_image(RenderData *rd, Image *ima, ImageUser *iuser)
{
	ImBuf *ibuf;
	CompBuf *stackbuf;
	int type;

	float *rect;
	int alloc= FALSE;

	if (iuser->use_layer_ima) {
		imalayer_set_current_act(ima, iuser->layer_ima);
		ibuf = BKE_image_get_ibuf(ima, iuser, IMA_IBUF_LAYER);
	}
	else
		ibuf = BKE_image_get_ibuf(ima, iuser, IMA_IBUF_IMA);

	if (ibuf==NULL || (ibuf->rect==NULL && ibuf->rect_float==NULL)) {
		return NULL;
	}

	if (ibuf->rect_float == NULL) {
		IMB_float_from_rect(ibuf);
	}

	/* now we need a float buffer from the image with matching color management */
	/* XXX weak code, multilayer is excluded from this */
	if (ibuf->channels == 4 && ima->rr==NULL) {
		rect = node_composit_get_float_buffer(rd, ibuf, &alloc);
	}
	else {
		/* non-rgba passes can't use color profiles */
		rect = ibuf->rect_float;
	}
	/* done coercing into the correct color management */


	type = ibuf->channels;
	
	if (rd->scemode & R_COMP_CROP) {
		stackbuf = get_cropped_compbuf(&rd->disprect, rect, ibuf->x, ibuf->y, type);
		if (alloc)
			MEM_freeN(rect);
	}
	else {
		/* we put imbuf copy on stack, cbuf knows rect is from other ibuf when freed! */
		stackbuf = alloc_compbuf(ibuf->x, ibuf->y, type, FALSE);
		stackbuf->rect = rect;
		stackbuf->malloc = alloc;
	}
	
	/* code to respect the premul flag of images; I'm
	 * not sure if this is a good idea for multilayer images,
	 * since it never worked before for them.
	 */
#if 0
	if (type==CB_RGBA && ima->flag & IMA_DO_PREMUL) {
		//premul the image
		int i;
		float *pixel = stackbuf->rect;
		
		for (i=0; i<stackbuf->x*stackbuf->y; i++, pixel += 4) {
			pixel[0] *= pixel[3];
			pixel[1] *= pixel[3];
			pixel[2] *= pixel[3];
		}
	}
#endif
	return stackbuf;
}

static CompBuf *node_composit_get_zimage(bNode *node, RenderData *rd)
{
	ImBuf *ibuf= BKE_image_get_ibuf((Image *)node->id, node->storage, IMA_IBUF_IMA);
	CompBuf *zbuf= NULL;
	
	if (ibuf && ibuf->zbuf_float) {
		if (rd->scemode & R_COMP_CROP) {
			zbuf= get_cropped_compbuf(&rd->disprect, ibuf->zbuf_float, ibuf->x, ibuf->y, CB_VAL);
		}
		else {
			zbuf= alloc_compbuf(ibuf->x, ibuf->y, CB_VAL, 0);
			zbuf->rect= ibuf->zbuf_float;
		}
	}
	return zbuf;
}

/* check if layer is available, returns pass buffer */
static CompBuf *compbuf_multilayer_get(RenderData *rd, RenderLayer *rl, Image *ima, ImageUser *iuser, int passindex)
{
	RenderPass *rpass = BLI_findlink(&rl->passes, passindex);
	if (rpass) {
		CompBuf *cbuf;
		
		iuser->pass = passindex;
		BKE_render_multilayer_index(ima->rr, iuser);
		cbuf = node_composit_get_image(rd, ima, iuser);
		
		return cbuf;
	}
	return NULL;
}

static void node_composit_exec_image(void *data, bNode *node, bNodeStack **UNUSED(in), bNodeStack **out)
{
	/* image assigned to output */
	/* stack order input sockets: col, alpha */
	if (node->id) {
		RenderData *rd= data;
		Image *ima= (Image *)node->id;
		ImageUser *iuser= (ImageUser *)node->storage;
		
		/* first set the right frame number in iuser */
		BKE_image_user_frame_calc(iuser, rd->cfra, 0);
		
		/* force a load, we assume iuser index will be set OK anyway */
		if (ima->type==IMA_TYPE_MULTILAYER)
			BKE_image_get_ibuf(ima, iuser, IMA_IBUF_IMA);
		
		if (ima->type==IMA_TYPE_MULTILAYER && ima->rr) {
			RenderLayer *rl = BLI_findlink(&ima->rr->layers, iuser->layer);
			
			if (rl) {
				bNodeSocket *sock;
				NodeImageLayer *sockdata;
				int out_index;
				CompBuf *combinedbuf= NULL, *firstbuf= NULL;
				
				for (sock=node->outputs.first, out_index=0; sock; sock=sock->next, ++out_index) {
					sockdata = sock->storage;
					if (out[out_index]->hasoutput) {
						CompBuf *stackbuf = out[out_index]->data = compbuf_multilayer_get(rd, rl, ima, iuser, sockdata->pass_index);
						if (stackbuf) {
							/* preview policy: take first 'Combined' pass if available,
							 * otherwise just use the first layer.
							 */
							if (!firstbuf) {
								firstbuf = stackbuf;
							}
							if (!combinedbuf &&
							    (strcmp(sock->name, "Combined") == 0 || strcmp(sock->name, "Image") == 0))
							{
								combinedbuf = stackbuf;
							}
						}
					}
				}
				
				/* preview */
				if (combinedbuf)
					generate_preview(data, node, combinedbuf);
				else if (firstbuf)
					generate_preview(data, node, firstbuf);
			}
		}
		else {
			CompBuf *stackbuf = node_composit_get_image(rd, ima, iuser);
			if (stackbuf) {
				int num_outputs = BLI_countlist(&node->outputs);
				
				/*respect image premul option*/
				if (stackbuf->type==CB_RGBA && ima->flag & IMA_DO_PREMUL) {
					int i;
					float *pixel;
			
					/* first duplicate stackbuf->rect, since it's just a pointer
					 * to the source imbuf, and we don't want to change that.*/
					stackbuf->rect = MEM_dupallocN(stackbuf->rect);
					
					/* since stackbuf now has allocated memory, rather than just a pointer,
					 * mark it as allocated so it can be freed properly */
					stackbuf->malloc=1;
					
					/*premul the image*/
					pixel = stackbuf->rect;
					for (i=0; i<stackbuf->x*stackbuf->y; i++, pixel += 4) {
						pixel[0] *= pixel[3];
						pixel[1] *= pixel[3];
						pixel[2] *= pixel[3];
					}
				}
			
				/* put image on stack */
				if (num_outputs > 0)
					out[0]->data= stackbuf;
				
				/* alpha output */
				if (num_outputs > 1 && out[1]->hasoutput)
					out[1]->data= valbuf_from_rgbabuf(stackbuf, CHAN_A);
				
				/* Z output */
				if (num_outputs > 2 && out[2]->hasoutput)
					out[2]->data= node_composit_get_zimage(node, rd);
				
				/* preview */
				generate_preview(data, node, stackbuf);
			}
		}
	}
}

#endif  /* WITH_COMPOSITOR_LEGACY */

static void node_composit_init_image(bNodeTree *ntree, bNode *node, bNodeTemplate *UNUSED(ntemp))
{
=======
>>>>>>> .r55757
	ImageUser *iuser= MEM_callocN(sizeof(ImageUser), "node image user");
	node->storage= iuser;
	iuser->frames= 1;
	iuser->sfra= 1;
	iuser->fie_ima= 2;
	iuser->ok= 1;
	
	/* setup initial outputs */
	cmp_node_image_verify_outputs(ntree, node);
}

static void node_composit_free_image(bNode *node)
{
	bNodeSocket *sock;
	
	/* free extra socket info */
	for (sock = node->outputs.first; sock; sock = sock->next)
		MEM_freeN(sock->storage);
	
	MEM_freeN(node->storage);
}

static void node_composit_copy_image(bNodeTree *UNUSED(dest_ntree), bNode *dest_node, bNode *src_node)
{
	bNodeSocket *sock;
	
	dest_node->storage= MEM_dupallocN(src_node->storage);
	
	/* copy extra socket info */
	for (sock=src_node->outputs.first; sock; sock = sock->next)
		sock->new_sock->storage = MEM_dupallocN(sock->storage);
}

void register_node_type_cmp_image(void)
{
	static bNodeType ntype;

	cmp_node_type_base(&ntype, CMP_NODE_IMAGE, "Image", NODE_CLASS_INPUT, NODE_PREVIEW|NODE_OPTIONS);
	node_type_init(&ntype, node_composit_init_image);
	node_type_storage(&ntype, "ImageUser", node_composit_free_image, node_composit_copy_image);
	node_type_update(&ntype, cmp_node_image_update, NULL);

	nodeRegisterType(&ntype);
}


/* **************** RENDER RESULT ******************** */

static int node_composit_poll_rlayers(bNodeType *UNUSED(ntype), bNodeTree *ntree)
{
	if (strcmp(ntree->idname, "CompositorNodeTree")==0) {
		Scene *scene;
		
		/* XXX ugly: check if ntree is a local scene node tree.
		 * Render layers node can only be used in local scene->nodetree,
		 * since it directly links to the scene.
		 */
		for (scene = G.main->scene.first; scene; scene = scene->id.next)
			if (scene->nodetree == ntree)
				break;
		
		return (scene != NULL);
	}
	return false;
}

void register_node_type_cmp_rlayers(void)
{
	static bNodeType ntype;

	cmp_node_type_base(&ntype, CMP_NODE_R_LAYERS, "Render Layers", NODE_CLASS_INPUT, NODE_PREVIEW|NODE_OPTIONS);
	node_type_socket_templates(&ntype, NULL, cmp_node_rlayers_out);
	ntype.poll = node_composit_poll_rlayers;

	nodeRegisterType(&ntype);
}
