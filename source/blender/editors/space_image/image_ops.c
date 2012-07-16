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
 * Contributor(s): Blender Foundation, 2002-2009, Xavier Thomas
 *
 * ***** END GPL LICENSE BLOCK *****
 */

/** \file blender/editors/space_image/image_ops.c
 *  \ingroup spimage
 */


#include <stddef.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>

#include "MEM_guardedalloc.h"

#include "BLI_math.h"
#include "BLI_blenlib.h"
#include "BLI_utildefines.h"

#include "DNA_object_types.h"
#include "DNA_node_types.h"
#include "DNA_packedFile_types.h"
#include "DNA_scene_types.h"

#include "BKE_colortools.h"
#include "BKE_context.h"
#include "BKE_image.h"
#include "BKE_layer.h"
#include "BKE_global.h"
#include "BKE_library.h"
#include "BKE_main.h"
#include "BKE_node.h"
#include "BKE_packedFile.h"
#include "BKE_report.h"
#include "BKE_screen.h"

#include "IMB_imbuf.h"
#include "IMB_imbuf_types.h"

#include "RE_pipeline.h"

#include "RNA_access.h"
#include "RNA_define.h"
#include "RNA_enum_types.h"

#include "ED_image.h"
#include "ED_render.h"
#include "ED_screen.h"
#include "ED_space_api.h"
#include "ED_uvedit.h"
#include "ED_util.h"

#include "UI_interface.h"
#include "UI_resources.h"
#include "UI_view2d.h"

#include "WM_api.h"
#include "WM_types.h"

#include "image_intern.h"

/******************** view navigation utilities *********************/

static void sima_zoom_set(SpaceImage *sima, ARegion *ar, float zoom, float location[2])
{
	float oldzoom = sima->zoom;
	int width, height;

	sima->zoom = zoom;

	if (sima->zoom < 0.1f || sima->zoom > 4.0f) {
		/* check zoom limits */
		ED_space_image_size(sima, &width, &height);

		width *= sima->zoom;
		height *= sima->zoom;

		if ((width < 4) && (height < 4))
			sima->zoom = oldzoom;
		else if ((ar->winrct.xmax - ar->winrct.xmin) <= sima->zoom)
			sima->zoom = oldzoom;
		else if ((ar->winrct.ymax - ar->winrct.ymin) <= sima->zoom)
			sima->zoom = oldzoom;
	}

	if ((U.uiflag & USER_ZOOM_TO_MOUSEPOS) && location) {
		float aspx, aspy, w, h;

		ED_space_image_size(sima, &width, &height);
		ED_space_image_aspect(sima, &aspx, &aspy);

		w = width * aspx;
		h = height * aspy;

		sima->xof += ((location[0] - 0.5f) * w - sima->xof) * (sima->zoom - oldzoom) / sima->zoom;
		sima->yof += ((location[1] - 0.5f) * h - sima->yof) * (sima->zoom - oldzoom) / sima->zoom;
	}
}

static void sima_zoom_set_factor(SpaceImage *sima, ARegion *ar, float zoomfac, float location[2])
{
	sima_zoom_set(sima, ar, sima->zoom * zoomfac, location);
}

#if 0 // currently unused
static int image_poll(bContext *C)
{
	return (CTX_data_edit_image(C) != NULL);
}
#endif

static int space_image_buffer_exists_poll(bContext *C)
{
	SpaceImage *sima = CTX_wm_space_image(C);
	if (sima && sima->spacetype == SPACE_IMAGE)
		if (ED_space_image_has_buffer(sima))
			return 1;
	return 0;
}

static int space_image_file_exists_poll(bContext *C)
{
	if (space_image_buffer_exists_poll(C)) {
		Main *bmain = CTX_data_main(C);
		SpaceImage *sima = CTX_wm_space_image(C);
		ImBuf *ibuf;
		void *lock;
		int ret = FALSE;
		char name[FILE_MAX];

		ibuf = ED_space_image_acquire_buffer(sima, &lock);
		if (ibuf) {
			BLI_strncpy(name, ibuf->name, FILE_MAX);
			BLI_path_abs(name, bmain->name);

			if (BLI_exists(name) == FALSE) {
				CTX_wm_operator_poll_msg_set(C, "image file not found");
			}
			else if (BLI_file_is_writable(name) == FALSE) {
				CTX_wm_operator_poll_msg_set(C, "image path can't be written to");
			}
			else {
				ret = TRUE;
			}
		}
		ED_space_image_release_buffer(sima, lock);

		return ret;
	}
	return 0;
}

static int space_image_poll(bContext *C)
{
	SpaceImage *sima = CTX_wm_space_image(C);
	if (sima && sima->spacetype == SPACE_IMAGE && sima->image)
		return 1;
	return 0;
}

int space_image_main_area_poll(bContext *C)
{
	SpaceImage *sima = CTX_wm_space_image(C);
	// XXX ARegion *ar= CTX_wm_region(C);

	if (sima)
		return 1;  // XXX (ar && ar->type->regionid == RGN_TYPE_WINDOW);
	
	return 0;
}

/* For IMAGE_OT_curves_point_set to avoid sampling when in uv smooth mode or editmode */
int space_image_main_area_not_uv_brush_poll(bContext *C)
{
	SpaceImage *sima = CTX_wm_space_image(C);
	Scene *scene = CTX_data_scene(C);
	ToolSettings *toolsettings = scene->toolsettings;

	if (sima && !toolsettings->uvsculpt && !scene->obedit)
		return 1;

	return 0;
}

static int space_image_image_sample_poll(bContext *C)
{
	SpaceImage *sima = CTX_wm_space_image(C);
	Object *obedit = CTX_data_edit_object(C);
	ToolSettings *toolsettings = CTX_data_scene(C)->toolsettings;

	if (obedit) {
		if (ED_space_image_show_uvedit(sima, obedit) && (toolsettings->use_uv_sculpt))
			return 0;
	}
	return space_image_main_area_poll(C);
}
/********************** view pan operator *********************/

typedef struct ViewPanData {
	float x, y;
	float xof, yof;
	int event_type;
} ViewPanData;

static void image_view_pan_init(bContext *C, wmOperator *op, wmEvent *event)
{
	SpaceImage *sima = CTX_wm_space_image(C);
	ViewPanData *vpd;

	op->customdata = vpd = MEM_callocN(sizeof(ViewPanData), "ImageViewPanData");
	WM_cursor_modal(CTX_wm_window(C), BC_NSEW_SCROLLCURSOR);

	vpd->x = event->x;
	vpd->y = event->y;
	vpd->xof = sima->xof;
	vpd->yof = sima->yof;
	vpd->event_type = event->type;

	WM_event_add_modal_handler(C, op);
}

static void image_view_pan_exit(bContext *C, wmOperator *op, int cancel)
{
	SpaceImage *sima = CTX_wm_space_image(C);
	ViewPanData *vpd = op->customdata;

	if (cancel) {
		sima->xof = vpd->xof;
		sima->yof = vpd->yof;
		ED_region_tag_redraw(CTX_wm_region(C));
	}

	WM_cursor_restore(CTX_wm_window(C));
	MEM_freeN(op->customdata);
}

static int image_view_pan_exec(bContext *C, wmOperator *op)
{
	SpaceImage *sima = CTX_wm_space_image(C);
	float offset[2];

	RNA_float_get_array(op->ptr, "offset", offset);
	sima->xof += offset[0];
	sima->yof += offset[1];

	ED_region_tag_redraw(CTX_wm_region(C));

	/* XXX notifier? */
#if 0
	if (image_preview_active(curarea, NULL, NULL)) {
		/* recalculates new preview rect */
		scrarea_do_windraw(curarea);
		image_preview_event(2);
	}
#endif
	
	return OPERATOR_FINISHED;
}

static int image_view_pan_invoke(bContext *C, wmOperator *op, wmEvent *event)
{
	if (event->type == MOUSEPAN) {
		SpaceImage *sima = CTX_wm_space_image(C);
		float offset[2];
		
		offset[0] = (event->x - event->prevx) / sima->zoom;
		offset[1] = (event->y - event->prevy) / sima->zoom;
		RNA_float_set_array(op->ptr, "offset", offset);

		image_view_pan_exec(C, op);
		return OPERATOR_FINISHED;
	}
	else {
		image_view_pan_init(C, op, event);
		return OPERATOR_RUNNING_MODAL;
	}
}

static int image_view_pan_modal(bContext *C, wmOperator *op, wmEvent *event)
{
	SpaceImage *sima = CTX_wm_space_image(C);
	ViewPanData *vpd = op->customdata;
	float offset[2];

	switch (event->type) {
		case MOUSEMOVE:
			sima->xof = vpd->xof;
			sima->yof = vpd->yof;
			offset[0] = (vpd->x - event->x) / sima->zoom;
			offset[1] = (vpd->y - event->y) / sima->zoom;
			RNA_float_set_array(op->ptr, "offset", offset);
			image_view_pan_exec(C, op);
			break;
		default:
			if (event->type == vpd->event_type && event->val == KM_RELEASE) {
				image_view_pan_exit(C, op, 0);
				return OPERATOR_FINISHED;
			}
			break;
	}

	return OPERATOR_RUNNING_MODAL;
}

static int image_view_pan_cancel(bContext *C, wmOperator *op)
{
	image_view_pan_exit(C, op, 1);
	return OPERATOR_CANCELLED;
}

void IMAGE_OT_view_pan(wmOperatorType *ot)
{
	/* identifiers */
	ot->name = "View Pan";
	ot->idname = "IMAGE_OT_view_pan";
	ot->description = "Pan the view";
	
	/* api callbacks */
	ot->exec = image_view_pan_exec;
	ot->invoke = image_view_pan_invoke;
	ot->modal = image_view_pan_modal;
	ot->cancel = image_view_pan_cancel;
	ot->poll = space_image_main_area_poll;

	/* flags */
	ot->flag = OPTYPE_BLOCKING | OPTYPE_GRAB_POINTER;
	
	/* properties */
	RNA_def_float_vector(ot->srna, "offset", 2, NULL, -FLT_MAX, FLT_MAX,
	                     "Offset", "Offset in floating point units, 1.0 is the width and height of the image", -FLT_MAX, FLT_MAX);
}

/********************** view zoom operator *********************/

typedef struct ViewZoomData {
	float x, y;
	float zoom;
	int event_type;
	float location[2];
} ViewZoomData;

static void image_view_zoom_init(bContext *C, wmOperator *op, wmEvent *event)
{
	SpaceImage *sima = CTX_wm_space_image(C);
	ARegion *ar = CTX_wm_region(C);
	ViewZoomData *vpd;

	op->customdata = vpd = MEM_callocN(sizeof(ViewZoomData), "ImageViewZoomData");
	WM_cursor_modal(CTX_wm_window(C), BC_NSEW_SCROLLCURSOR);

	vpd->x = event->x;
	vpd->y = event->y;
	vpd->zoom = sima->zoom;
	vpd->event_type = event->type;

	UI_view2d_region_to_view(&ar->v2d, event->mval[0], event->mval[1], &vpd->location[0], &vpd->location[1]);

	WM_event_add_modal_handler(C, op);
}

static void image_view_zoom_exit(bContext *C, wmOperator *op, int cancel)
{
	SpaceImage *sima = CTX_wm_space_image(C);
	ViewZoomData *vpd = op->customdata;

	if (cancel) {
		sima->zoom = vpd->zoom;
		ED_region_tag_redraw(CTX_wm_region(C));
	}

	WM_cursor_restore(CTX_wm_window(C));
	MEM_freeN(op->customdata);
}

static int image_view_zoom_exec(bContext *C, wmOperator *op)
{
	SpaceImage *sima = CTX_wm_space_image(C);
	ARegion *ar = CTX_wm_region(C);

	sima_zoom_set_factor(sima, ar, RNA_float_get(op->ptr, "factor"), NULL);

	ED_region_tag_redraw(CTX_wm_region(C));

	/* XXX notifier? */
#if 0
	if (image_preview_active(curarea, NULL, NULL)) {
		/* recalculates new preview rect */
		scrarea_do_windraw(curarea);
		image_preview_event(2);
	}
#endif
	
	return OPERATOR_FINISHED;
}

static int image_view_zoom_invoke(bContext *C, wmOperator *op, wmEvent *event)
{
	if (event->type == MOUSEZOOM) {
		SpaceImage *sima = CTX_wm_space_image(C);
		ARegion *ar = CTX_wm_region(C);
		float factor, location[2];

		UI_view2d_region_to_view(&ar->v2d, event->mval[0], event->mval[1], &location[0], &location[1]);

		factor = 1.0f + (event->x - event->prevx + event->y - event->prevy) / 300.0f;
		RNA_float_set(op->ptr, "factor", factor);
		sima_zoom_set(sima, ar, sima->zoom * factor, location);
		ED_region_tag_redraw(CTX_wm_region(C));
		
		return OPERATOR_FINISHED;
	}
	else {
		image_view_zoom_init(C, op, event);
		return OPERATOR_RUNNING_MODAL;
	}
}

static int image_view_zoom_modal(bContext *C, wmOperator *op, wmEvent *event)
{
	SpaceImage *sima = CTX_wm_space_image(C);
	ARegion *ar = CTX_wm_region(C);
	ViewZoomData *vpd = op->customdata;
	float factor;

	switch (event->type) {
		case MOUSEMOVE:
			factor = 1.0f + (vpd->x - event->x + vpd->y - event->y) / 300.0f;
			RNA_float_set(op->ptr, "factor", factor);
			sima_zoom_set(sima, ar, vpd->zoom * factor, vpd->location);
			ED_region_tag_redraw(CTX_wm_region(C));
			break;
		default:
			if (event->type == vpd->event_type && event->val == KM_RELEASE) {
				image_view_zoom_exit(C, op, 0);
				return OPERATOR_FINISHED;
			}
			break;
	}

	return OPERATOR_RUNNING_MODAL;
}

static int image_view_zoom_cancel(bContext *C, wmOperator *op)
{
	image_view_zoom_exit(C, op, 1);
	return OPERATOR_CANCELLED;
}

void IMAGE_OT_view_zoom(wmOperatorType *ot)
{
	/* identifiers */
	ot->name = "View Zoom";
	ot->idname = "IMAGE_OT_view_zoom";
	ot->description = "Zoom in/out the image";
	
	/* api callbacks */
	ot->exec = image_view_zoom_exec;
	ot->invoke = image_view_zoom_invoke;
	ot->modal = image_view_zoom_modal;
	ot->cancel = image_view_zoom_cancel;
	ot->poll = space_image_main_area_poll;

	/* flags */
	ot->flag = OPTYPE_BLOCKING;
	
	/* properties */
	RNA_def_float(ot->srna, "factor", 0.0f, 0.0f, FLT_MAX,
	              "Factor", "Zoom factor, values higher than 1.0 zoom in, lower values zoom out", -FLT_MAX, FLT_MAX);
}

/********************** NDOF operator *********************/

/* Combined pan/zoom from a 3D mouse device.
 * Z zooms, XY pans
 * "view" (not "paper") control -- user moves the viewpoint, not the image being viewed
 * that explains the negative signs in the code below
 */

static int image_view_ndof_invoke(bContext *C, wmOperator *UNUSED(op), wmEvent *event)
{
	if (event->type != NDOF_MOTION)
		return OPERATOR_CANCELLED;
	else {
		SpaceImage *sima = CTX_wm_space_image(C);
		ARegion *ar = CTX_wm_region(C);

		wmNDOFMotionData *ndof = (wmNDOFMotionData *) event->customdata;

		float dt = ndof->dt;
		/* tune these until it feels right */
		const float zoom_sensitivity = 0.5f; // 50% per second (I think)
		const float pan_sensitivity = 300.f; // screen pixels per second

		float pan_x = pan_sensitivity * dt * ndof->tvec[0] / sima->zoom;
		float pan_y = pan_sensitivity * dt * ndof->tvec[1] / sima->zoom;

		/* "mouse zoom" factor = 1 + (dx + dy) / 300
		 * what about "ndof zoom" factor? should behave like this:
		 * at rest -> factor = 1
		 * move forward -> factor > 1
		 * move backward -> factor < 1
		 */
		float zoom_factor = 1.f + zoom_sensitivity * dt * -ndof->tvec[2];

		if (U.ndof_flag & NDOF_ZOOM_INVERT)
			zoom_factor = -zoom_factor;

		sima_zoom_set_factor(sima, ar, zoom_factor, NULL);
		sima->xof += pan_x;
		sima->yof += pan_y;

		ED_region_tag_redraw(ar);	

		return OPERATOR_FINISHED;
	}
}

void IMAGE_OT_view_ndof(wmOperatorType *ot)
{
	/* identifiers */
	ot->name = "NDOF Pan/Zoom";
	ot->idname = "IMAGE_OT_view_ndof";
	ot->description = "Use a 3D mouse device to pan/zoom the view";
	
	/* api callbacks */
	ot->invoke = image_view_ndof_invoke;
}

/********************** view all operator *********************/

/* Updates the fields of the View2D member of the SpaceImage struct.
 * Default behavior is to reset the position of the image and set the zoom to 1
 * If the image will not fit within the window rectangle, the zoom is adjusted */

static int image_view_all_exec(bContext *C, wmOperator *UNUSED(op))
{
	SpaceImage *sima;
	ARegion *ar;
	float aspx, aspy, zoomx, zoomy, w, h;
	int width, height;

	/* retrieve state */
	sima = CTX_wm_space_image(C);
	ar = CTX_wm_region(C);

	ED_space_image_size(sima, &width, &height);
	ED_space_image_aspect(sima, &aspx, &aspy);

	w = width * aspx;
	h = height * aspy;
	
	/* check if the image will fit in the image with zoom==1 */
	width = ar->winrct.xmax - ar->winrct.xmin + 1;
	height = ar->winrct.ymax - ar->winrct.ymin + 1;

	if ((w >= width || h >= height) && (width > 0 && height > 0)) {
		/* find the zoom value that will fit the image in the image space */
		zoomx = width / w;
		zoomy = height / h;
		sima_zoom_set(sima, ar, 1.0f / power_of_2(1 / MIN2(zoomx, zoomy)), NULL);
	}
	else
		sima_zoom_set(sima, ar, 1.0f, NULL);

	sima->xof = sima->yof = 0.0f;

	ED_region_tag_redraw(CTX_wm_region(C));
	
	return OPERATOR_FINISHED;
}

void IMAGE_OT_view_all(wmOperatorType *ot)
{
	/* identifiers */
	ot->name = "View All";
	ot->idname = "IMAGE_OT_view_all";
	ot->description = "View the whole picture";
	
	/* api callbacks */
	ot->exec = image_view_all_exec;
	ot->poll = space_image_main_area_poll;
}

/********************** view selected operator *********************/

static int image_view_selected_exec(bContext *C, wmOperator *UNUSED(op))
{
	SpaceImage *sima;
	ARegion *ar;
	Scene *scene;
	Object *obedit;
	Image *ima;
	float size, min[2], max[2], d[2], aspx, aspy;
	int width, height;

	/* retrieve state */
	sima = CTX_wm_space_image(C);
	ar = CTX_wm_region(C);
	scene = CTX_data_scene(C);
	obedit = CTX_data_edit_object(C);

	ima = ED_space_image(sima);
	ED_space_image_size(sima, &width, &height);
	ED_image_aspect(ima, &aspx, &aspy);

	width = width * aspx;
	height = height * aspy;

	/* get bounds */
	if (!ED_uvedit_minmax(scene, ima, obedit, min, max))
		return OPERATOR_CANCELLED;

	/* adjust offset and zoom */
	sima->xof = (int)(((min[0] + max[0]) * 0.5f - 0.5f) * width);
	sima->yof = (int)(((min[1] + max[1]) * 0.5f - 0.5f) * height);

	d[0] = max[0] - min[0];
	d[1] = max[1] - min[1];
	size = 0.5f * MAX2(d[0], d[1]) * MAX2(width, height) / 256.0f;
	
	if (size <= 0.01f) size = 0.01f;
	sima_zoom_set(sima, ar, 0.7f / size, NULL);

	ED_region_tag_redraw(CTX_wm_region(C));
	
	return OPERATOR_FINISHED;
}

static int image_view_selected_poll(bContext *C)
{
	return (space_image_main_area_poll(C) && ED_operator_uvedit(C));
}

void IMAGE_OT_view_selected(wmOperatorType *ot)
{
	/* identifiers */
	ot->name = "View Center";
	ot->idname = "IMAGE_OT_view_selected";
	ot->description = "View all selected UVs";
	
	/* api callbacks */
	ot->exec = image_view_selected_exec;
	ot->poll = image_view_selected_poll;
}

/********************** view zoom in/out operator *********************/

static int image_view_zoom_in_exec(bContext *C, wmOperator *op)
{
	SpaceImage *sima = CTX_wm_space_image(C);
	ARegion *ar = CTX_wm_region(C);
	float location[2];

	RNA_float_get_array(op->ptr, "location", location);

	sima_zoom_set_factor(sima, ar, 1.25f, location);

	ED_region_tag_redraw(CTX_wm_region(C));
	
	return OPERATOR_FINISHED;
}

static int image_view_zoom_in_invoke(bContext *C, wmOperator *op, wmEvent *event)
{
	ARegion *ar = CTX_wm_region(C);
	float location[2];

	UI_view2d_region_to_view(&ar->v2d, event->mval[0], event->mval[1], &location[0], &location[1]);
	RNA_float_set_array(op->ptr, "location", location);

	return image_view_zoom_in_exec(C, op);
}

void IMAGE_OT_view_zoom_in(wmOperatorType *ot)
{
	/* identifiers */
	ot->name = "View Zoom In";
	ot->idname = "IMAGE_OT_view_zoom_in";
	ot->description = "Zoom in the image (centered around 2D cursor)";
	
	/* api callbacks */
	ot->invoke = image_view_zoom_in_invoke;
	ot->exec = image_view_zoom_in_exec;
	ot->poll = space_image_main_area_poll;

	/* properties */
	RNA_def_float_vector(ot->srna, "location", 2, NULL, -FLT_MAX, FLT_MAX, "Location", "Cursor location in screen coordinates", -10.0f, 10.0f);
}

static int image_view_zoom_out_exec(bContext *C, wmOperator *op)
{
	SpaceImage *sima = CTX_wm_space_image(C);
	ARegion *ar = CTX_wm_region(C);
	float location[2];

	RNA_float_get_array(op->ptr, "location", location);

	sima_zoom_set_factor(sima, ar, 0.8f, location);

	ED_region_tag_redraw(CTX_wm_region(C));
	
	return OPERATOR_FINISHED;
}

static int image_view_zoom_out_invoke(bContext *C, wmOperator *op, wmEvent *event)
{
	ARegion *ar = CTX_wm_region(C);
	float location[2];

	UI_view2d_region_to_view(&ar->v2d, event->mval[0], event->mval[1], &location[0], &location[1]);
	RNA_float_set_array(op->ptr, "location", location);

	return image_view_zoom_out_exec(C, op);
}

void IMAGE_OT_view_zoom_out(wmOperatorType *ot)
{
	/* identifiers */
	ot->name = "View Zoom Out";
	ot->idname = "IMAGE_OT_view_zoom_out";
	ot->description = "Zoom out the image (centered around 2D cursor)";
	
	/* api callbacks */
	ot->invoke = image_view_zoom_out_invoke;
	ot->exec = image_view_zoom_out_exec;
	ot->poll = space_image_main_area_poll;

	/* properties */
	RNA_def_float_vector(ot->srna, "location", 2, NULL, -FLT_MAX, FLT_MAX, "Location", "Cursor location in screen coordinates", -10.0f, 10.0f);
}

/********************** view zoom ratio operator *********************/

static int image_view_zoom_ratio_exec(bContext *C, wmOperator *op)
{
	SpaceImage *sima = CTX_wm_space_image(C);
	ARegion *ar = CTX_wm_region(C);

	sima_zoom_set(sima, ar, RNA_float_get(op->ptr, "ratio"), NULL);
	
	/* ensure pixel exact locations for draw */
	sima->xof = (int)sima->xof;
	sima->yof = (int)sima->yof;

	/* XXX notifier? */
#if 0
	if (image_preview_active(curarea, NULL, NULL)) {
		/* recalculates new preview rect */
		scrarea_do_windraw(curarea);
		image_preview_event(2);
	}
#endif

	ED_region_tag_redraw(CTX_wm_region(C));
	
	return OPERATOR_FINISHED;
}

void IMAGE_OT_view_zoom_ratio(wmOperatorType *ot)
{
	/* identifiers */
	ot->name = "View Zoom Ratio";
	ot->idname = "IMAGE_OT_view_zoom_ratio";
	ot->description = "Set zoom ration of the view";
	
	/* api callbacks */
	ot->exec = image_view_zoom_ratio_exec;
	ot->poll = space_image_main_area_poll;
	
	/* properties */
	RNA_def_float(ot->srna, "ratio", 0.0f, 0.0f, FLT_MAX,
	              "Ratio", "Zoom ratio, 1.0 is 1:1, higher is zoomed in, lower is zoomed out", -FLT_MAX, FLT_MAX);
}

/**************** load/replace/save callbacks ******************/
static void image_filesel(bContext *C, wmOperator *op, const char *path)
{
	RNA_string_set(op->ptr, "filepath", path);
	WM_event_add_fileselect(C, op); 
}

/******************** open image operator ********************/

static void image_open_init(bContext *C, wmOperator *op)
{
	PropertyPointerRNA *pprop;

	op->customdata = pprop = MEM_callocN(sizeof(PropertyPointerRNA), "OpenPropertyPointerRNA");
	uiIDContextProperty(C, &pprop->ptr, &pprop->prop);
}

static int image_open_cancel(bContext *UNUSED(C), wmOperator *op)
{
	MEM_freeN(op->customdata);
	op->customdata = NULL;
	return OPERATOR_CANCELLED;
}

static int image_open_exec(bContext *C, wmOperator *op)
{
	SpaceImage *sima = CTX_wm_space_image(C); /* XXX other space types can call */
	Scene *scene = CTX_data_scene(C);
	Object *obedit = CTX_data_edit_object(C);
	ImageUser *iuser = NULL;
	PropertyPointerRNA *pprop;
	PointerRNA idptr;
	Image *ima = NULL;
	char str[FILE_MAX];
	int entry = 0;
	int action = RNA_enum_get(op->ptr, "action");

	RNA_string_get(op->ptr, "filepath", str);

	if (sima) {
		if (!sima->image)
			entry = 1;
	}
	else
		entry = 1;
	/* default to frame 1 if there's no scene in context */
	//sima->image == NULL
	if ((entry) || (action & IMA_LAYER_OPEN_IMAGE)) {
		errno = 0;

		ima = BKE_image_load_exists(str);
		if (!ima) {
			if (op->customdata) MEM_freeN(op->customdata);
			BKE_reportf(op->reports, RPT_ERROR, "Can't read: \"%s\", %s", str, errno ? strerror(errno) : "Unsupported image format");
			return OPERATOR_CANCELLED;
		}

		/* hook into UI */
		pprop = op->customdata;

		if (pprop->prop) {
			/* when creating new ID blocks, use is already 1, but RNA
			 * pointer se also increases user, so this compensates it */
			ima->id.us--;

			RNA_id_pointer_create(&ima->id, &idptr);
			RNA_property_pointer_set(&pprop->ptr, pprop->prop, idptr);
			RNA_property_update(C, &pprop->ptr, pprop->prop);
		}
		else if (sima) {
			ED_space_image_set(sima, scene, obedit, ima);
			iuser = &sima->iuser;
		}
		else {
			Tex *tex = CTX_data_pointer_get_type(C, "texture", &RNA_Texture).data;
			if (tex && tex->type == TEX_IMAGE)
				iuser = &tex->iuser;
		
		}
		
		/* initialize because of new image */
		if (iuser) {
			iuser->sfra = 1;
			iuser->offset = 0;
			iuser->fie_ima = 2;
		}

		/* XXX unpackImage frees image buffers */
		ED_preview_kill_jobs(C);
	
		BKE_image_signal(ima, iuser, IMA_SIGNAL_RELOAD);
		WM_event_add_notifier(C, NC_IMAGE | NA_EDITED | ND_DRAW, ima);

		MEM_freeN(op->customdata);
	} else if ((action & IMA_LAYER_OPEN_LAYER)) {
		if (sima) {
			ima = sima->image;
		}

		if (ima) {
			ImageLayer *iml;
			struct ImBuf *ibuf;
			int flag;

			iml = BKE_add_image_file_as_layer(ima, str);
			iml->background = IMA_LAYER_BG_IMAGE;
			strcpy(iml->file_path, str);
			flag= IB_rect|IB_multilayer|IB_metadata;
			if (ima->flag & IMA_DO_PREMUL)
				flag |= IB_premul;
		
			/* read ibuf */
			ibuf = IMB_loadiffname(str, flag);
			BLI_addtail(&iml->ibufs, ibuf);
			if(!iml)
				return OPERATOR_CANCELLED;

			WM_event_add_notifier(C, NC_IMAGE | ND_DRAW, ima);
			MEM_freeN(op->customdata);
		}
	}
	return OPERATOR_FINISHED;
}

static int image_open_invoke(bContext *C, wmOperator *op, wmEvent *UNUSED(event))
{
	SpaceImage *sima = CTX_wm_space_image(C); /* XXX other space types can call */
	char *path = U.textudir;
	Image *ima = NULL;

	if (sima) {
		ima = sima->image;
	}

	if (ima == NULL) {
		Tex *tex = CTX_data_pointer_get_type(C, "texture", &RNA_Texture).data;
		if (tex && tex->type == TEX_IMAGE)
			ima = tex->ima;
	}

	if (ima)
		path = ima->name;

	if (RNA_struct_property_is_set(op->ptr, "filepath"))
		return image_open_exec(C, op);
	
	image_open_init(C, op);

	image_filesel(C, op, path);

	return OPERATOR_RUNNING_MODAL;
}

/* called by other space types too */
void IMAGE_OT_open(wmOperatorType *ot)
{
	PropertyRNA *prop;
	static EnumPropertyItem open_actions[] = {
			{IMA_LAYER_OPEN_IMAGE, "IMAGE", 0, "Image", "Open Image"},
			{IMA_LAYER_OPEN_LAYER, "LAYER", 0, "Layer", "Open image as layer"},
			{0, NULL, 0, NULL, NULL}
	};

	/* identifiers */
	ot->name = "Open Image";
	ot->description = "Open image";
	ot->idname = "IMAGE_OT_open";
	
	/* api callbacks */
	ot->exec = image_open_exec;
	ot->invoke = image_open_invoke;
	ot->cancel = image_open_cancel;

	/* flags */
	ot->flag = OPTYPE_REGISTER | OPTYPE_UNDO;

	prop = RNA_def_enum(ot->srna, "action", open_actions, IMA_LAYER_DEL_SELECTED, "Action", "Selection action to execute");
	RNA_def_property_flag(prop, PROP_HIDDEN);
	/* properties */
	WM_operator_properties_filesel(ot, FOLDERFILE | IMAGEFILE | MOVIEFILE, FILE_SPECIAL, FILE_OPENFILE, WM_FILESEL_FILEPATH | WM_FILESEL_RELPATH, FILE_DEFAULTDISPLAY);
}

/******************** Match movie length operator ********************/
static int image_match_len_exec(bContext *C, wmOperator *UNUSED(op))
{
	Scene *scene = CTX_data_scene(C);
	Image *ima = CTX_data_pointer_get_type(C, "edit_image", &RNA_Image).data;
	ImageUser *iuser = CTX_data_pointer_get_type(C, "edit_image_user", &RNA_ImageUser).data;

	if (!ima || !iuser) {
		/* Try to get a Texture, or a SpaceImage from context... */
		SpaceImage *sima = CTX_wm_space_image(C);
		Tex *tex = CTX_data_pointer_get_type(C, "texture", &RNA_Texture).data;
		if (tex && tex->type == TEX_IMAGE) {
			ima = tex->ima;
			iuser = &tex->iuser;
		}
		else if (sima) {
			ima = sima->image;
			iuser = &sima->iuser;
		}
		
	}

	if (!ima || !iuser || !ima->anim)
		return OPERATOR_CANCELLED;

	iuser->frames = IMB_anim_get_duration(ima->anim, IMB_TC_RECORD_RUN);
	BKE_image_user_frame_calc(iuser, scene->r.cfra, 0);

	return OPERATOR_FINISHED;
}

/* called by other space types too */
void IMAGE_OT_match_movie_length(wmOperatorType *ot)
{
	/* identifiers */
	ot->name = "Match Movie Length";
	ot->description = "Set image's users length to the one of this video";
	ot->idname = "IMAGE_OT_match_movie_length";
	
	/* api callbacks */
	ot->exec = image_match_len_exec;

	/* flags */
	ot->flag = OPTYPE_REGISTER /* | OPTYPE_UNDO */; /* Don't think we need undo for that. */
}

/******************** replace image operator ********************/

static int image_replace_exec(bContext *C, wmOperator *op)
{
	SpaceImage *sima = CTX_wm_space_image(C);
	char str[FILE_MAX];

	if (!sima->image)
		return OPERATOR_CANCELLED;
	
	RNA_string_get(op->ptr, "filepath", str);

	/* we cant do much if the str is longer then FILE_MAX :/ */
	BLI_strncpy(sima->image->name, str, sizeof(sima->image->name));

	if (BLI_testextensie_array(str, imb_ext_movie))
		sima->image->source = IMA_SRC_MOVIE;
	else
		sima->image->source = IMA_SRC_FILE;

	/* XXX unpackImage frees image buffers */
	ED_preview_kill_jobs(C);
	
	BKE_image_signal(sima->image, &sima->iuser, IMA_SIGNAL_RELOAD);
	WM_event_add_notifier(C, NC_IMAGE | NA_EDITED, sima->image);

	return OPERATOR_FINISHED;
}

static int image_replace_invoke(bContext *C, wmOperator *op, wmEvent *UNUSED(event))
{
	SpaceImage *sima = CTX_wm_space_image(C);

	if (!sima->image)
		return OPERATOR_CANCELLED;

	if (RNA_struct_property_is_set(op->ptr, "filepath"))
		return image_replace_exec(C, op);

	if (!RNA_struct_property_is_set(op->ptr, "relative_path"))
		RNA_boolean_set(op->ptr, "relative_path", (strncmp(sima->image->name, "//", 2)) == 0);

	image_filesel(C, op, sima->image->name);

	return OPERATOR_RUNNING_MODAL;
}

void IMAGE_OT_replace(wmOperatorType *ot)
{
	/* identifiers */
	ot->name = "Replace Image";
	ot->idname = "IMAGE_OT_replace";
	ot->description = "Replace current image by another one from disk";
	
	/* api callbacks */
	ot->exec = image_replace_exec;
	ot->invoke = image_replace_invoke;
	ot->poll = space_image_poll;

	/* flags */
	ot->flag = OPTYPE_REGISTER | OPTYPE_UNDO;

	/* properties */
	WM_operator_properties_filesel(ot, FOLDERFILE | IMAGEFILE | MOVIEFILE, FILE_SPECIAL, FILE_OPENFILE, WM_FILESEL_FILEPATH | WM_FILESEL_RELPATH, FILE_DEFAULTDISPLAY);
}

/******************** save image as operator ********************/

typedef struct {
	/* matching scene->r settings */
	//short planes, imtype, subimtype, quality;
	ImageFormatData im_format;
	char filepath[FILE_MAX]; /* keep absolute */
} SaveImageOptions;

static void save_image_options_defaults(SaveImageOptions *simopts)
{
	BKE_imformat_defaults(&simopts->im_format);
	simopts->filepath[0] = '\0';
}

static char imtype_best_depth(ImBuf *ibuf, const char imtype)
{
	const char depth_ok = BKE_imtype_valid_depths(imtype);

	if (ibuf->rect_float) {
		if (depth_ok & R_IMF_CHAN_DEPTH_32) return R_IMF_CHAN_DEPTH_32;
		if (depth_ok & R_IMF_CHAN_DEPTH_24) return R_IMF_CHAN_DEPTH_24;
		if (depth_ok & R_IMF_CHAN_DEPTH_16) return R_IMF_CHAN_DEPTH_16;
		if (depth_ok & R_IMF_CHAN_DEPTH_12) return R_IMF_CHAN_DEPTH_12;
		return R_IMF_CHAN_DEPTH_8;
	}
	else {
		if (depth_ok & R_IMF_CHAN_DEPTH_8) return R_IMF_CHAN_DEPTH_8;
		if (depth_ok & R_IMF_CHAN_DEPTH_12) return R_IMF_CHAN_DEPTH_12;
		if (depth_ok & R_IMF_CHAN_DEPTH_16) return R_IMF_CHAN_DEPTH_16;
		if (depth_ok & R_IMF_CHAN_DEPTH_24) return R_IMF_CHAN_DEPTH_24;
		if (depth_ok & R_IMF_CHAN_DEPTH_32) return R_IMF_CHAN_DEPTH_32;
		return R_IMF_CHAN_DEPTH_8; /* fallback, should not get here */
	}
}

static int save_image_options_init(SaveImageOptions *simopts, SpaceImage *sima, Scene *scene, const short guess_path)
{
	void *lock;
	ImBuf *ibuf = NULL;
	//ImBuf *ibuf = ED_space_image_acquire_buffer(sima, &lock);

	if (sima && sima->image)
		ibuf = BKE_image_acquire_ibuf(sima->image, &sima->iuser, &lock, IMA_IBUF_IMA);

	if (ibuf && (ibuf->rect || ibuf->rect_float)) {
		Image *ima = sima->image;
		short is_depth_set = FALSE;

		simopts->im_format.planes = ibuf->planes;

		if (ELEM(ima->type, IMA_TYPE_R_RESULT, IMA_TYPE_COMPOSITE)) {
			/* imtype */
			simopts->im_format = scene->r.im_format;
			is_depth_set = TRUE;
		}
		else if (ima->source == IMA_SRC_GENERATED) {
			simopts->im_format.imtype = R_IMF_IMTYPE_PNG;
		}
		else {
			simopts->im_format.imtype = BKE_ftype_to_imtype(ibuf->ftype);
		}
		//simopts->subimtype= scene->r.subimtype; /* XXX - this is lame, we need to make these available too! */
		simopts->im_format.quality = ibuf->ftype & 0xff;

		BLI_strncpy(simopts->filepath, ibuf->name, sizeof(simopts->filepath));

		/* sanitize all settings */

		/* unlikely but just in case */
		if (ELEM3(simopts->im_format.planes, R_IMF_PLANES_BW, R_IMF_PLANES_RGB, R_IMF_PLANES_RGBA) == 0) {
			simopts->im_format.planes = R_IMF_PLANES_RGBA;
		}

		/* depth, account for float buffer and format support */
		if (is_depth_set == FALSE) {
			simopts->im_format.depth = imtype_best_depth(ibuf, simopts->im_format.imtype);
		}

		/* some formats don't use quality so fallback to scenes quality */
		if (simopts->im_format.quality == 0) {
			simopts->im_format.quality = scene->r.im_format.quality;
		}

		/* check for empty path */
		if (guess_path && simopts->filepath[0] == 0) {
			if ( (G.ima[0] == '/') && (G.ima[1] == '/') && (G.ima[2] == '\0') ) {
				BLI_strncpy(simopts->filepath, "//untitled", FILE_MAX);
			}
			else {
				BLI_strncpy(simopts->filepath, G.ima, FILE_MAX);
			}
			BLI_path_abs(simopts->filepath, G.main->name);
		}
	}

	ED_space_image_release_buffer(sima, lock);

	return (ibuf != NULL);
}

static void save_image_options_from_op(SaveImageOptions *simopts, wmOperator *op)
{
	if (op->customdata) {
		simopts->im_format = *(ImageFormatData *)op->customdata;
	}

	if (RNA_struct_property_is_set(op->ptr, "filepath")) {
		RNA_string_get(op->ptr, "filepath", simopts->filepath);
		BLI_path_abs(simopts->filepath, G.main->name);
	}
}

static void save_image_options_to_op(SaveImageOptions *simopts, wmOperator *op)
{
	if (op->customdata) {
		*(ImageFormatData *)op->customdata = simopts->im_format;
	}

	RNA_string_set(op->ptr, "filepath", simopts->filepath);
}

/* assumes name is FILE_MAX */
/* ima->name and ibuf->name should end up the same */
static void save_image_doit(bContext *C, SpaceImage *sima, wmOperator *op, SaveImageOptions *simopts, int do_newpath)
{
	Image *ima = ED_space_image(sima);
	void *lock;
	ImBuf *ibuf = NULL;
	//ImBuf *ibuf = ED_space_image_acquire_buffer(sima, &lock);
	if (sima && sima->image)
		ibuf = BKE_image_acquire_ibuf(sima->image, &sima->iuser, &lock, IMA_IBUF_IMA);

	if (ibuf && (ibuf->rect || ibuf->rect_float)) {
		const char *relbase = ID_BLEND_PATH(CTX_data_main(C), &ima->id);
		const short relative = (RNA_struct_find_property(op->ptr, "relative_path") && RNA_boolean_get(op->ptr, "relative_path"));
		const short save_copy = (RNA_struct_find_property(op->ptr, "copy") && RNA_boolean_get(op->ptr, "copy"));
		short ok = FALSE;

		/* old global to ensure a 2nd save goes to same dir */
		BLI_strncpy(G.ima, simopts->filepath, sizeof(G.ima));

		WM_cursor_wait(1);

		if (ima->type == IMA_TYPE_R_RESULT) {
			/* enforce user setting for RGB or RGBA, but skip BW */
			if (simopts->im_format.planes == R_IMF_PLANES_RGBA) {
				ibuf->planes = R_IMF_PLANES_RGBA;
			}
			else if (simopts->im_format.planes == R_IMF_PLANES_RGB) {
				ibuf->planes = R_IMF_PLANES_RGB;
			}
		}
		else {
			/* TODO, better solution, if a 24bit image is painted onto it may contain alpha */
			if (ibuf->userflags & IB_BITMAPDIRTY) { /* it has been painted onto */
				/* checks each pixel, not ideal */
				ibuf->planes = BKE_imbuf_alpha_test(ibuf) ? 32 : 24;
			}
		}
		
		if (simopts->im_format.imtype == R_IMF_IMTYPE_MULTILAYER) {
			Scene *scene = CTX_data_scene(C);
			RenderResult *rr = BKE_image_acquire_renderresult(scene, ima);
			if (rr) {
				RE_WriteRenderResult(op->reports, rr, simopts->filepath, simopts->im_format.quality);
				ok = TRUE;
			}
			else {
				BKE_report(op->reports, RPT_ERROR, "Did not write, no Multilayer Image");
			}
			BKE_image_release_renderresult(scene, ima);
		}
		else {
			if (BKE_imbuf_write_as(ibuf, simopts->filepath, &simopts->im_format, save_copy)) {
				ok = TRUE;
			}
		}

		if (ok) {
			if (!save_copy) {
				if (do_newpath) {
					BLI_strncpy(ibuf->name, simopts->filepath, sizeof(ibuf->name));
					BLI_strncpy(ima->name, simopts->filepath, sizeof(ima->name));
				}

				ibuf->userflags &= ~IB_BITMAPDIRTY;

				/* change type? */
				if (ima->type == IMA_TYPE_R_RESULT) {
					ima->type = IMA_TYPE_IMAGE;

					/* workaround to ensure the render result buffer is no longer used
					 * by this image, otherwise can crash when a new render result is
					 * created. */
					if (ibuf->rect && !(ibuf->mall & IB_rect))
						imb_freerectImBuf(ibuf);
					if (ibuf->rect_float && !(ibuf->mall & IB_rectfloat))
						imb_freerectfloatImBuf(ibuf);
					if (ibuf->zbuf && !(ibuf->mall & IB_zbuf))
						IMB_freezbufImBuf(ibuf);
					if (ibuf->zbuf_float && !(ibuf->mall & IB_zbuffloat))
						IMB_freezbuffloatImBuf(ibuf);
				}
				if (ELEM(ima->source, IMA_SRC_GENERATED, IMA_SRC_VIEWER)) {
					ima->source = IMA_SRC_FILE;
					ima->type = IMA_TYPE_IMAGE;
				}

				/* only image path, never ibuf */
				if (relative) {
					BLI_path_rel(ima->name, relbase); /* only after saving */
				}
			}
		}
		else {
			BKE_reportf(op->reports, RPT_ERROR, "Couldn't write image: %s", simopts->filepath);
		}


		WM_event_add_notifier(C, NC_IMAGE | NA_EDITED, sima->image);

		WM_cursor_wait(0);
	}

	ED_space_image_release_buffer(sima, lock);
}

static void image_save_as_free(wmOperator *op)
{
	if (op->customdata) {
		MEM_freeN(op->customdata);
		op->customdata = NULL;
	}
}

static int image_save_as_exec(bContext *C, wmOperator *op)
{
	SpaceImage *sima = CTX_wm_space_image(C);
	SaveImageOptions simopts;

	save_image_options_defaults(&simopts);

	/* just in case to initialize values,
	 * these should be set on invoke or by the caller. */
	save_image_options_init(&simopts, sima, CTX_data_scene(C), 0);

	save_image_options_from_op(&simopts, op);

	save_image_doit(C, sima, op, &simopts, TRUE);

	image_save_as_free(op);
	return OPERATOR_FINISHED;
}


static int image_save_as_check(bContext *UNUSED(C), wmOperator *op)
{
	ImageFormatData *imf = op->customdata;
	return WM_operator_filesel_ensure_ext_imtype(op, imf->imtype);
}

static int image_save_as_invoke(bContext *C, wmOperator *op, wmEvent *UNUSED(event))
{
	SpaceImage *sima = CTX_wm_space_image(C);
	Image *ima = ED_space_image(sima);
	Scene *scene = CTX_data_scene(C);
	SaveImageOptions simopts;

	if (RNA_struct_property_is_set(op->ptr, "filepath"))
		return image_save_as_exec(C, op);

	save_image_options_defaults(&simopts);

	if (save_image_options_init(&simopts, sima, scene, TRUE) == 0)
		return OPERATOR_CANCELLED;
	save_image_options_to_op(&simopts, op);

	/* enable save_copy by default for render results */
	if (ELEM(ima->type, IMA_TYPE_R_RESULT, IMA_TYPE_COMPOSITE) && !RNA_struct_property_is_set(op->ptr, "copy")) {
		RNA_boolean_set(op->ptr, "copy", TRUE);
	}

	op->customdata = MEM_mallocN(sizeof(simopts.im_format), __func__);
	memcpy(op->customdata, &simopts.im_format, sizeof(simopts.im_format));

	image_filesel(C, op, simopts.filepath);

	return OPERATOR_RUNNING_MODAL;
}

static int image_save_as_cancel(bContext *UNUSED(C), wmOperator *op)
{
	image_save_as_free(op);

	return OPERATOR_CANCELLED;
}

static int image_save_as_draw_check_prop(PointerRNA *ptr, PropertyRNA *prop)
{
	const char *prop_id = RNA_property_identifier(prop);

	return !(strcmp(prop_id, "filepath") == 0 ||
	         strcmp(prop_id, "directory") == 0 ||
	         strcmp(prop_id, "filename") == 0 ||
	         /* when saving a copy, relative path has no effect */
	         ((strcmp(prop_id, "relative_path") == 0) && RNA_boolean_get(ptr, "copy"))
	         );
}

static void image_save_as_draw(bContext *UNUSED(C), wmOperator *op)
{
	uiLayout *layout = op->layout;
	ImageFormatData *imf = op->customdata;
	PointerRNA ptr;

	/* image template */
	RNA_pointer_create(NULL, &RNA_ImageFormatSettings, imf, &ptr);
	uiTemplateImageSettings(layout, &ptr);

	/* main draw call */
	RNA_pointer_create(NULL, op->type->srna, op->properties, &ptr);
	uiDefAutoButsRNA(layout, &ptr, image_save_as_draw_check_prop, '\0');
}

void IMAGE_OT_save_as(wmOperatorType *ot)
{
//	PropertyRNA *prop;

	/* identifiers */
	ot->name = "Save As Image";
	ot->idname = "IMAGE_OT_save_as";
	ot->description = "Save the image with another name and/or settings";
	
	/* api callbacks */
	ot->exec = image_save_as_exec;
	ot->check = image_save_as_check;
	ot->invoke = image_save_as_invoke;
	ot->cancel = image_save_as_cancel;
	ot->ui = image_save_as_draw;
	ot->poll = space_image_buffer_exists_poll;

	/* flags */
	ot->flag = OPTYPE_REGISTER | OPTYPE_UNDO;

	/* properties */
	RNA_def_boolean(ot->srna, "copy", 0, "Copy", "Create a new image file without modifying the current image in blender");

	WM_operator_properties_filesel(ot, FOLDERFILE | IMAGEFILE | MOVIEFILE, FILE_SPECIAL, FILE_SAVE, WM_FILESEL_FILEPATH | WM_FILESEL_RELPATH, FILE_DEFAULTDISPLAY);
}

/******************** save image operator ********************/

static int image_save_exec(bContext *C, wmOperator *op)
{
	SpaceImage *sima = CTX_wm_space_image(C);
	Scene *scene = CTX_data_scene(C);
	SaveImageOptions simopts;

	if (save_image_options_init(&simopts, sima, scene, FALSE) == 0)
		return OPERATOR_CANCELLED;
	save_image_options_from_op(&simopts, op);

	if (BLI_exists(simopts.filepath) && BLI_file_is_writable(simopts.filepath)) {
		save_image_doit(C, sima, op, &simopts, FALSE);
	}
	else {
		BKE_reportf(op->reports, RPT_ERROR, "Can not save image, path '%s' is not writable", simopts.filepath);
		return OPERATOR_CANCELLED;
	}

	return OPERATOR_FINISHED;
}

void IMAGE_OT_save(wmOperatorType *ot)
{
	/* identifiers */
	ot->name = "Save Image";
	ot->idname = "IMAGE_OT_save";
	ot->description = "Save the image with current name and settings";
	
	/* api callbacks */
	ot->exec = image_save_exec;
	ot->poll = space_image_file_exists_poll;

	/* flags */
	ot->flag = OPTYPE_REGISTER | OPTYPE_UNDO;
}

/******************* save sequence operator ********************/

static int image_save_sequence_exec(bContext *C, wmOperator *op)
{
	Main *bmain = CTX_data_main(C);
	SpaceImage *sima = CTX_wm_space_image(C);
	ImBuf *ibuf;
	int tot = 0;
	char di[FILE_MAX], fi[FILE_MAX];
	
	if (sima->image == NULL)
		return OPERATOR_CANCELLED;

	if (sima->image->source != IMA_SRC_SEQUENCE) {
		BKE_report(op->reports, RPT_ERROR, "Can only save sequence on image sequences");
		return OPERATOR_CANCELLED;
	}

	if (sima->image->type == IMA_TYPE_MULTILAYER) {
		BKE_report(op->reports, RPT_ERROR, "Can't save multilayer sequences");
		return OPERATOR_CANCELLED;
	}
	
	/* get total */
	for (ibuf = sima->image->ibufs.first; ibuf; ibuf = ibuf->next)
		if (ibuf->userflags & IB_BITMAPDIRTY)
			tot++;
	
	if (tot == 0) {
		BKE_report(op->reports, RPT_WARNING, "No images have been changed");
		return OPERATOR_CANCELLED;
	}

	/* get a filename for menu */
	for (ibuf = sima->image->ibufs.first; ibuf; ibuf = ibuf->next)
		if (ibuf->userflags & IB_BITMAPDIRTY)
			break;
	
	BLI_strncpy(di, ibuf->name, FILE_MAX);
	BLI_splitdirstring(di, fi);
	
	BKE_reportf(op->reports, RPT_INFO, "%d Image(s) will be saved in %s", tot, di);

	for (ibuf = sima->image->ibufs.first; ibuf; ibuf = ibuf->next) {
		if (ibuf->userflags & IB_BITMAPDIRTY) {
			char name[FILE_MAX];
			BLI_strncpy(name, ibuf->name, sizeof(name));
			
			BLI_path_abs(name, bmain->name);

			if (0 == IMB_saveiff(ibuf, name, IB_rect | IB_zbuf | IB_zbuffloat)) {
				BKE_reportf(op->reports, RPT_ERROR, "Could not write image %s", name);
				break;
			}

			BKE_reportf(op->reports, RPT_INFO, "Saved: %s\n", ibuf->name);
			ibuf->userflags &= ~IB_BITMAPDIRTY;
		}
	}

	return OPERATOR_FINISHED;
}

void IMAGE_OT_save_sequence(wmOperatorType *ot)
{
	/* identifiers */
	ot->name = "Save Sequence";
	ot->idname = "IMAGE_OT_save_sequence";
	ot->description = "Save a sequence of images";
	
	/* api callbacks */
	ot->exec = image_save_sequence_exec;
	ot->poll = space_image_buffer_exists_poll;

	/* flags */
	ot->flag = OPTYPE_REGISTER | OPTYPE_UNDO;
}

/******************** reload image operator ********************/

static int image_reload_exec(bContext *C, wmOperator *UNUSED(op))
{
	Image *ima = CTX_data_edit_image(C);
	SpaceImage *sima = CTX_wm_space_image(C);

	if (!ima)
		return OPERATOR_CANCELLED;

	/* XXX unpackImage frees image buffers */
	ED_preview_kill_jobs(C);
	
	// XXX other users?
	BKE_image_signal(ima, (sima) ? &sima->iuser : NULL, IMA_SIGNAL_RELOAD);

	WM_event_add_notifier(C, NC_IMAGE | NA_EDITED, ima);
	
	return OPERATOR_FINISHED;
}

void IMAGE_OT_reload(wmOperatorType *ot)
{
	/* identifiers */
	ot->name = "Reload Image";
	ot->idname = "IMAGE_OT_reload";
	ot->description = "Reload current image from disk";
	
	/* api callbacks */
	ot->exec = image_reload_exec;

	/* flags */
	ot->flag = OPTYPE_REGISTER; /* no undo, image buffer is not handled by undo */
}

/********************** new image operator *********************/

static int image_new_exec(bContext *C, wmOperator *op)
{
	SpaceImage *sima;
	Scene *scene;
	Object *obedit;
	Image *ima;
	PointerRNA ptr, idptr;
	PropertyRNA *prop;
	char name[MAX_ID_NAME - 2];
	float color[4];
	int width, height, floatbuf, uvtestgrid, alpha;
	short background;

	/* retrieve state */
	sima = CTX_wm_space_image(C);
	scene = CTX_data_scene(C);
	obedit = CTX_data_edit_object(C);

	RNA_string_get(op->ptr, "name", name);
	width = RNA_int_get(op->ptr, "width");
	height = RNA_int_get(op->ptr, "height");
	floatbuf = RNA_boolean_get(op->ptr, "float");
	uvtestgrid = RNA_boolean_get(op->ptr, "uv_test_grid");
	RNA_float_get_array(op->ptr, "color", color);
	alpha = RNA_boolean_get(op->ptr, "alpha");
	background = RNA_enum_get(op->ptr, "background");

	if (background & IMA_LAYER_BG_WHITE) {
		color[0] = 1.0f;
		color[1] = 1.0f;
		color[2] = 1.0f;
		color[3] = 1.0f;
	}
	else if (background & IMA_LAYER_BG_ALPHA) {
		color[0] = 0.0f;
		color[1] = 0.0f;
		color[2] = 0.0f;
		color[3] = 0.0f;
	}

	if (!floatbuf && scene->r.color_mgt_flag & R_COLOR_MANAGEMENT)
		linearrgb_to_srgb_v3_v3(color, color);

	if (!alpha)
		color[3] = 1.0f;

	ima = BKE_image_add_generated(width, height, name, alpha ? 32 : 24, floatbuf, uvtestgrid, color);
	((ImageLayer *)ima->imlayers.last)->background = background;
	copy_v4_v4(((ImageLayer *)ima->imlayers.first)->default_color, color);
	if (!ima)
		return OPERATOR_CANCELLED;

	/* hook into UI */
	uiIDContextProperty(C, &ptr, &prop);

	if (prop) {
		/* when creating new ID blocks, use is already 1, but RNA
		 * pointer se also increases user, so this compensates it */
		ima->id.us--;

		RNA_id_pointer_create(&ima->id, &idptr);
		RNA_property_pointer_set(&ptr, prop, idptr);
		RNA_property_update(C, &ptr, prop);
	}
	else if (sima)
		ED_space_image_set(sima, scene, obedit, ima);

	// XXX other users?
	BKE_image_signal(ima, (sima) ? &sima->iuser : NULL, IMA_SIGNAL_USER_NEW_IMAGE);
	
	return OPERATOR_FINISHED;
}

/* XXX, Ton is not a fan of OK buttons but using this function to avoid undo/redo bug while in mesh-editmode, - campbell */
static int image_new_invoke(bContext *C, wmOperator *op, wmEvent *UNUSED(event))
{
	return WM_operator_props_dialog_popup(C, op, 300, 100);

}

static int image_offset_invoke(bContext *C, wmOperator *op, wmEvent *UNUSED(event))
{
	return WM_operator_props_dialog_popup(C, op, 250, 100);;
}

void IMAGE_OT_new(wmOperatorType *ot)
{
	PropertyRNA *prop;
	short background;
	
	static float default_color[4]= {0.0f, 0.0f, 0.0f, 1.0f};

	static EnumPropertyItem prop_background_items[] = {
		{IMA_LAYER_BG_RGB, "RGB", 0, "RGB", ""},
		{IMA_LAYER_BG_WHITE, "WHITE", 0, "White", ""},
		{IMA_LAYER_BG_ALPHA, "ALPHA", 0, "Transparent", ""},
		{0, NULL, 0, NULL, NULL}};

	
	/* identifiers */
	ot->name = "New Image";
	ot->description = "Create a new image";
	ot->idname = "IMAGE_OT_new";
	
	/* api callbacks */
	ot->exec = image_new_exec;
	ot->invoke = image_new_invoke;
		
	/* flags */
	ot->flag = OPTYPE_UNDO;

	/* properties */
	RNA_def_string(ot->srna, "name", "untitled", MAX_ID_NAME - 2, "Name", "Image datablock name");
	RNA_def_int(ot->srna, "width", 1024, 1, INT_MAX, "Width", "Image width", 1, 16384);
	RNA_def_int(ot->srna, "height", 1024, 1, INT_MAX, "Height", "Image height", 1, 16384);
	RNA_def_enum(ot->srna, "background", prop_background_items, 0, "Background", "");

	prop= RNA_def_float_color(ot->srna, "color", 4, NULL, 0.0f, FLT_MAX, "Color", "Default fill color", 0.0f, 1.0f);
	RNA_def_property_float_array_default(prop, default_color);
	RNA_def_boolean(ot->srna, "alpha", 1, "Alpha", "Create an image with an alpha channel");
	RNA_def_boolean(ot->srna, "float", 0, "32 bit Float", "Create image with 32 bit floating point bit depth");
	RNA_def_boolean(ot->srna, "uv_test_grid", 0, "UV Test Grid", "Fill the image with a grid for UV map testing");
	
}

/********************* invert operators *********************/

static int image_invert_poll(bContext *C)
{
	Image *ima = CTX_data_edit_image(C);
	ImBuf *ibuf = BKE_image_get_ibuf(ima, NULL, IMA_IBUF_IMA);
	
	if (ibuf != NULL)
		return 1;
	return 0;
}

static int image_invert_exec(bContext *C, wmOperator *op)
{
	Image *ima = CTX_data_edit_image(C);
	ImBuf *ibuf = BKE_image_get_ibuf(ima, NULL, IMA_IBUF_IMA);

	/* flags indicate if this channel should be inverted */
	const short r = RNA_boolean_get(op->ptr, "invert_r");
	const short g = RNA_boolean_get(op->ptr, "invert_g");
	const short b = RNA_boolean_get(op->ptr, "invert_b");
	const short a = RNA_boolean_get(op->ptr, "invert_a");

	int i;

	if (ibuf == NULL)  /* TODO: this should actually never happen, but does for render-results -> cleanup */
		return OPERATOR_CANCELLED;

	/* TODO: make this into an IMB_invert_channels(ibuf,r,g,b,a) method!? */
	if (ibuf->rect_float) {
		
		float *fp = (float *) ibuf->rect_float;
		for (i = ibuf->x * ibuf->y; i > 0; i--, fp += 4) {
			if (r) fp[0] = 1.0f - fp[0];
			if (g) fp[1] = 1.0f - fp[1];
			if (b) fp[2] = 1.0f - fp[2];
			if (a) fp[3] = 1.0f - fp[3];
		}

		if (ibuf->rect) {
			IMB_rect_from_float(ibuf);
		}
	}
	else if (ibuf->rect) {
		
		char *cp = (char *) ibuf->rect;
		for (i = ibuf->x * ibuf->y; i > 0; i--, cp += 4) {
			if (r) cp[0] = 255 - cp[0];
			if (g) cp[1] = 255 - cp[1];
			if (b) cp[2] = 255 - cp[2];
			if (a) cp[3] = 255 - cp[3];
		}
	}
	else {
		return OPERATOR_CANCELLED;
	}

	ibuf->userflags |= IB_BITMAPDIRTY;
	if (ibuf->mipmap[0])
		ibuf->userflags |= IB_MIPMAP_INVALID;

	WM_event_add_notifier(C, NC_IMAGE | NA_EDITED, ima);
	return OPERATOR_FINISHED;
}

void IMAGE_OT_invert(wmOperatorType *ot)
{
	/* identifiers */
	ot->name = "Invert Channels";
	ot->idname = "IMAGE_OT_invert";
	ot->description = "Invert image's channels";
	
	/* api callbacks */
	ot->exec = image_invert_exec;
	ot->poll = image_invert_poll;
	
	/* properties */
	RNA_def_boolean(ot->srna, "invert_r", 0, "Red", "Invert Red Channel");
	RNA_def_boolean(ot->srna, "invert_g", 0, "Green", "Invert Green Channel");
	RNA_def_boolean(ot->srna, "invert_b", 0, "Blue", "Invert Blue Channel");
	RNA_def_boolean(ot->srna, "invert_a", 0, "Alpha", "Invert Alpha Channel");
	
	/* flags */
	ot->flag = OPTYPE_REGISTER | OPTYPE_UNDO;
}

/********************** new image layer operators *********************/

int image_layer_poll(bContext *C)
{	
	SpaceImage *sima= CTX_wm_space_image(C);
	return ED_space_image_show_paint(sima);
}
 
static int image_layer_add_exec(bContext *C, wmOperator *op)
{	
	char name[22];
	float color[4];
	int alpha, order;
	Scene *scene;
	Image *ima = CTX_data_edit_image(C);
	ImageLayer *iml;
	
	scene= (Scene*)CTX_data_scene(C);

	RNA_string_get(op->ptr, "name", name);
	RNA_float_get_array(op->ptr, "color", color);
	alpha = RNA_boolean_get(op->ptr, "alpha");

	order = 2;
	if (strcmp(op->idname, "IMAGE_OT_image_layer_add_above") == 0)
		order = 1;
	else if (strcmp(op->idname, "IMAGE_OT_image_layer_add_below") == 0)
		order = -1;

	if (scene->r.color_mgt_flag & R_COLOR_MANAGEMENT)
		linearrgb_to_srgb_v3_v3(color, color);

	if(!alpha) 
		color[3] = 1.0f;
	
	iml = image_add_image_layer(ima, name, alpha ? 32 : 24, color, order);
	if(!iml)
		return OPERATOR_CANCELLED;

	WM_event_add_notifier(C, NC_IMAGE|ND_DRAW, ima);
 
	return OPERATOR_FINISHED;
}

void IMAGE_OT_image_layer_add(wmOperatorType *ot)
{
	PropertyRNA *prop;
	static float default_color[4]= {0.0f, 0.0f, 0.0f, 0.0f};

	/* identifiers */
	ot->name= "New Layer";
	ot->idname= "IMAGE_OT_image_layer_add";
	ot->description="Add a new image layer";
 
	/* api callbacks */
	ot->exec = image_layer_add_exec;
	ot->poll = image_layer_poll;
	ot->invoke = image_new_invoke;
	
	/* flags */
	ot->flag= OPTYPE_REGISTER|OPTYPE_UNDO;

	/* properties */
	RNA_def_string(ot->srna, "name", "Layer", 21, "Name", "Layer name.");
	prop= RNA_def_float_color(ot->srna, "color", 4, NULL, 0.0f, FLT_MAX, "Fill Color", "Color used to fill the layer.", 0.0f, 1.0f);
	RNA_def_property_float_array_default(prop, default_color);
	RNA_def_boolean(ot->srna, "alpha", 1, "Alpha", "Create an image with an alpha channel.");
}

void IMAGE_OT_image_layer_add_above(wmOperatorType *ot)
{
	PropertyRNA *prop;
	static float default_color[4]= {0.0f, 0.0f, 0.0f, 0.0f};

	/* identifiers */
	ot->name= "Above active layer";
	ot->idname= "IMAGE_OT_image_layer_add_above";
	ot->description="Add a new image layer";
 
	/* api callbacks */
	ot->exec= image_layer_add_exec;
	ot->poll = image_layer_poll;
	ot->invoke= image_new_invoke;
	
	/* flags */
	ot->flag= OPTYPE_REGISTER|OPTYPE_UNDO;

	/* properties */
	RNA_def_string(ot->srna, "name", "Layer", 21, "Name", "Layer name.");
	prop= RNA_def_float_color(ot->srna, "color", 4, NULL, 0.0f, FLT_MAX, "Fill Color", "Color used to fill the layer.", 0.0f, 1.0f);
	RNA_def_property_float_array_default(prop, default_color);
	RNA_def_boolean(ot->srna, "alpha", 1, "Alpha", "Create an image with an alpha channel.");
}

void IMAGE_OT_image_layer_add_below(wmOperatorType *ot)
{
	PropertyRNA *prop;
	static float default_color[4]= {0.0f, 0.0f, 0.0f, 0.0f};

	/* identifiers */
	ot->name= "Below active layer";
	ot->idname= "IMAGE_OT_image_layer_add_below";
	ot->description="Add a new image layer";
 
	/* api callbacks */
	ot->exec= image_layer_add_exec;
	ot->poll = image_layer_poll;
	ot->invoke= image_new_invoke;
	
	/* flags */
	ot->flag= OPTYPE_REGISTER|OPTYPE_UNDO;

	/* properties */
	RNA_def_string(ot->srna, "name", "Layer", 21, "Name", "Layer name.");
	prop= RNA_def_float_color(ot->srna, "color", 4, NULL, 0.0f, FLT_MAX, "Fill Color", "Color used to fill the layer.", 0.0f, 1.0f);
	RNA_def_property_float_array_default(prop, default_color);
	RNA_def_boolean(ot->srna, "alpha", 1, "Alpha", "Create an image with an alpha channel.");
}

static int image_layer_duplicate_exec(bContext *C, wmOperator *op)
{
	Image *ima= CTX_data_edit_image(C);
	ImageLayer *iml;

	if(!ima)
		return OPERATOR_CANCELLED;
 
	iml = image_duplicate_current_image_layer(ima);
	if(!iml)
		return OPERATOR_CANCELLED;

	WM_event_add_notifier(C, NC_IMAGE|ND_DRAW, ima);
 
	return OPERATOR_FINISHED;
}
 
void IMAGE_OT_image_layer_duplicate(wmOperatorType *ot)
{
	/* identifiers */
	ot->name= "Duplicate Layer";
	ot->idname= "IMAGE_OT_image_layer_duplicate";
	ot->description="Duplicate the selected image layer";
 
	/* api callbacks */
	ot->exec= image_layer_duplicate_exec;
	ot->poll = image_layer_poll;
 
	/* flags */
	ot->flag= OPTYPE_REGISTER|OPTYPE_UNDO;
}


static int image_layer_remove_exec(bContext *C, wmOperator *op)
{
	Image *ima= CTX_data_edit_image(C);
	int action = RNA_enum_get(op->ptr, "action");
 
	if(!ima)
		return OPERATOR_CANCELLED;
 
	if (image_remove_layer(ima, action) == -1)
		BKE_report(op->reports, RPT_INFO, "Impossible to remove only one layer");
		
	WM_event_add_notifier(C, NC_IMAGE|ND_DRAW, ima);
 
	return OPERATOR_FINISHED;
}
 
void IMAGE_OT_image_layer_remove(wmOperatorType *ot)
{
	static EnumPropertyItem select_all_actions[] = {
			{IMA_LAYER_DEL_SELECTED, "SELECTED", 0, "Selected", "Remove the selected layer"},
			{IMA_LAYER_DEL_HIDDEN, "HIDDEN", 0, "Hidden", "Removes the hidden layers"},
			{0, NULL, 0, NULL, NULL}
	};

	/* identifiers */
	ot->name= "Remove Layer";
	ot->idname= "IMAGE_OT_image_layer_remove";
	ot->description="Remove the selected image layer";
 
	/* api callbacks */
	ot->exec= image_layer_remove_exec;
	ot->poll = image_layer_poll;
 
	/* flags */
	ot->flag= OPTYPE_REGISTER|OPTYPE_UNDO;

	/* properties */
	RNA_def_enum(ot->srna, "action", select_all_actions, IMA_LAYER_DEL_SELECTED, "Action", "Selection action to execute");
}

static int image_layer_move_exec(bContext *C, wmOperator *op)
{
	Image *ima= CTX_data_edit_image(C);
	ImageLayer *layer, *tmp;
	int type, layerID;
 
	if(!ima)
		return OPERATOR_CANCELLED;
 
	layer = imalayer_get_current(ima);
 
	if (!layer)
		return OPERATOR_CANCELLED;
 
	type = RNA_enum_get(op->ptr, "type");
	layerID = imalayer_get_current_act(ima);
	
	if (!(layer->type & IMA_LAYER_BASE)) {
		if (type == -1) { /* Move direction: Up */
			if (layerID > 0) {
				tmp = layer->prev;
				BLI_remlink(&ima->imlayers, layer);
				layer->next = layer->prev = NULL;
				if (tmp) {
					BLI_insertlinkbefore(&ima->imlayers, tmp, layer);
				}
				else {
					BLI_addhead(&ima->imlayers, layer);
				}
				imalayer_set_current_act(ima, layerID-1);
			}
		}
		else if (type == 1){ /* Move direction: Down */
			if (layerID < (ima->Count_Layers - 1)) {
				tmp = layer->next;
				if (!(tmp->type & IMA_LAYER_BASE)) {
					BLI_remlink(&ima->imlayers, layer);
					layer->next = layer->prev = NULL;
					if (tmp) {
						BLI_insertlinkafter(&ima->imlayers, tmp, layer);
					}
					else {
						BLI_addtail(&ima->imlayers, layer);
					}	
					imalayer_set_current_act(ima, layerID+1);
				}
			}
		}
		else if (type == -2) {  /* Move direction: Top */
			BLI_remlink(&ima->imlayers, layer);
			layer->next = layer->prev = NULL;
			BLI_addhead(&ima->imlayers, layer);
			ima->Act_Layers = 0;
		}
		else if (type == 2) {  /* Move direction: Bottom */
			BLI_remlink(&ima->imlayers, layer);
			layer->next = layer->prev = NULL;
			if (((ImageLayer *)ima->imlayers.last)->type & IMA_LAYER_BASE) {
				BLI_insertlink(&ima->imlayers,((ImageLayer *)ima->imlayers.last)->prev, layer);
				ima->Act_Layers = ima->Count_Layers - 2;
			}
			else {
				BLI_addtail(&ima->imlayers, layer);
				ima->Act_Layers = ima->Count_Layers - 1;
			}
		}
		else if (type == 3) {  /* Move direction: Invert */
			int i = 0, lim;
			ImageLayer *tmp1, *tmp2, *next, *prev, *tmp_up, *tmp_down;
			if (ima->Count_Layers > 2) {
				if (ima->Count_Layers % 2 == 0)
					lim = (ima->Count_Layers / 2);
				else
					lim = (ima->Count_Layers / 2) + 1;

				tmp_up = (ImageLayer *)ima->imlayers.first;
				tmp_down = ((ImageLayer *)ima->imlayers.last)->prev;
				while ((i<lim) && (tmp_up != tmp_down)) {
					tmp1 = tmp_down;
					tmp2 = tmp_up;

					next = tmp_down->next;
					prev = tmp_down->prev;
					
					tmp_down->next = tmp2->next;
					tmp_down->prev = tmp2->prev;
					if (tmp2->prev)
						tmp2->prev->next = tmp_down;
					else
						ima->imlayers.first = tmp_down;
					tmp2->next->prev = tmp_down;

					tmp_up->next = next;
					tmp_up->prev = prev;
					if (prev)
						prev->next = tmp_up;
					next->prev = tmp_up;

					prev = tmp_up->prev;
					tmp_up = tmp_down->next;
					tmp_down = prev;
					i++;
				}
			}
		}
	}
	WM_event_add_notifier(C, NC_IMAGE|ND_DRAW, NULL);
 
	return OPERATOR_FINISHED;
}
 
void IMAGE_OT_image_layer_move(wmOperatorType *ot)
{
	static EnumPropertyItem slot_move[] = {
		{-2, "TOP", 0, "Top", ""},
		{-1, "UP", 0, "Up", ""},
		{1, "DOWN", 0, "Down", ""},
		{2, "BOTTOM", 0, "Bottom", ""},
		{3, "INVERT", 0, "Invert", ""},
		{0, NULL, 0, NULL, NULL}
	};
 
	/* identifiers */
	ot->name= "Move Layer";
	ot->idname= "IMAGE_OT_image_layer_move";
	ot->description="Move image layers up and down";
 
	/* api callbacks */
	ot->exec= image_layer_move_exec;
	ot->poll = image_layer_poll;
 
	/* flags */
	ot->flag= OPTYPE_REGISTER|OPTYPE_UNDO;
 
	/* properties */
	RNA_def_enum(ot->srna, "type", slot_move, 0, "Type", "");
}

static int image_layer_select_exec(bContext *C, wmOperator *op)
{
	Image *ima= CTX_data_edit_image(C);
	ImageLayer *layer;
	int action = RNA_enum_get(op->ptr, "action");

	layer = imalayer_get_current(ima);
	
	switch (action) {
		case IMA_LAYER_SEL_PREVIOUS:
			if (ima->Act_Layers >= 1) {
				layer->select = !IMA_LAYER_SEL_CURRENT;
				layer->prev->select = IMA_LAYER_SEL_CURRENT;
				ima->Act_Layers--;
			}
			//else
			//	layer->select = IMA_LAYER_SEL_CURRENT;
			break;
		case IMA_LAYER_SEL_NEXT:
			if (ima->Act_Layers < (ima->Count_Layers-1)) {
				layer->select = !IMA_LAYER_SEL_CURRENT;
				layer->next->select = IMA_LAYER_SEL_CURRENT;
				ima->Act_Layers++;
			}
			//else
			//	layer->select = IMA_LAYER_SEL_CURRENT;
			break;
		case IMA_LAYER_SEL_TOP:
			((ImageLayer *)ima->imlayers.first)->select = IMA_LAYER_SEL_CURRENT;
			ima->Act_Layers = 0;
			break;
		case IMA_LAYER_SEL_BOTTOM:
			((ImageLayer *)ima->imlayers.last)->select = IMA_LAYER_SEL_CURRENT;
			ima->Act_Layers = ima->Count_Layers - 1;
			break;
	}
	WM_event_add_notifier(C, NC_IMAGE|ND_DRAW, NULL);
	return OPERATOR_FINISHED;
}

void IMAGE_OT_image_layer_select(wmOperatorType *ot)
{
	static EnumPropertyItem select_all_actions[] = {
			{IMA_LAYER_SEL_PREVIOUS, "PREVIOUS", 0, "Previous", "Select the previous layer"},
			{IMA_LAYER_SEL_NEXT, "NEXT", 0, "Next", "Select the next layer"},
			{IMA_LAYER_SEL_TOP, "TOP", 0, "Top", "Select the top layer"},
			{IMA_LAYER_SEL_BOTTOM, "BOTTOM", 0, "Select the bottom layer"},
			{0, NULL, 0, NULL, NULL}
	};
 
	/* identifiers */
	ot->name= "Select Layers";
	ot->idname= "IMAGE_OT_image_layer_select";
	ot->description="Select layers";
 
	/* api callbacks */
	ot->exec= image_layer_select_exec;
	ot->poll = image_layer_poll;
 
	/* flags */
	ot->flag= OPTYPE_REGISTER|OPTYPE_UNDO;
 
	/* properties */
	RNA_def_enum(ot->srna, "action", select_all_actions, IMA_LAYER_SEL_NEXT, "Action", "Selection action to execute");
}

static int image_layer_merge_exec(bContext *C, wmOperator *op)
{
	Image *ima= CTX_data_edit_image(C);
	ImageLayer *layer;
	int type;
 
	if(!ima)
		return OPERATOR_CANCELLED;
  
	type = RNA_enum_get(op->ptr, "type");
	
	if (type == 1) { /* Merge Layers */
		layer = imalayer_get_current(ima);
		if (!layer)
				return OPERATOR_CANCELLED;
		
		if (!(layer->type & IMA_LAYER_BASE)) {
			ImageLayer *next;
			
			next = layer->next;
			if ((next->visible & IMA_LAYER_VISIBLE) && (!(next->lock & IMA_LAYER_LOCK))) {
				merge_layers(ima, layer, next);

				imalayer_set_current_act(ima, imalayer_get_current_act(ima));
				ima->Count_Layers--;
			}
			else
				if (!(next->visible & IMA_LAYER_VISIBLE))
					BKE_report(op->reports, RPT_INFO, "It can not merge the layers, because the next layer is hidden");
				else
					BKE_report(op->reports, RPT_INFO, "It can not merge the layers, because the next layer is locked");
		}
	}
	else if (type == 2) { /* Merge Visible */
		int i=0;
		ImageLayer *next;
		for (layer = (ImageLayer *)ima->imlayers.first; layer; layer = layer->next) {
			if (layer->visible & IMA_LAYER_VISIBLE) {
				i = 1;
				break;
			}
		}
		if (i == 1) {
			next = layer;
			while ((next != NULL) && (layer->type != IMA_LAYER_BASE)) {
				next = layer->next;
				while ((next != NULL) && (!(next->visible & IMA_LAYER_VISIBLE)))
					next = next->next;

				if (next) {
					layer = merge_layers(ima, layer, next);
					ima->Count_Layers--;
				}
			}
			imalayer_set_current_act(ima,imalayer_get_current_act(ima));
		}
		else
			BKE_report(op->reports, RPT_INFO, "It can not merge the layers, because the layers are hidden");
	}
	else if (type == 3) {  /* Merge All */
		ImageLayer *next, *app;
		static float white_color[4] = {1.0f, 1.0f, 1.0f, 1.0f};

		for (layer = (ImageLayer *)ima->imlayers.first; layer; layer = layer->next) {
			if (layer->visible & IMA_LAYER_VISIBLE) {
				break;
			}
			else {
				BLI_remlink(&ima->imlayers, layer);
				free_image_layer(layer);
				ima->Count_Layers--;
			}
		}
		if (ima->imlayers.first) {
			next = layer;
			while ((next != NULL) && (layer->type != IMA_LAYER_BASE)) {
				next = layer->next;
				while ((next != NULL) && (!(next->visible & IMA_LAYER_VISIBLE))) {
					app = next;
					next = next->next;

					BLI_remlink(&ima->imlayers, app);
					free_image_layer(app);
					ima->Count_Layers--;
				}
				if (next) {
					layer = merge_layers(ima, layer, next);
					ima->Count_Layers--;
				}
			}

			imalayer_set_current_act(ima, imalayer_get_current_act(ima));
			layer = (ImageLayer *)ima->imlayers.last;
			if (!(layer->type & IMA_LAYER_BASE)) {
				ImBuf *base;
				int i;
				
				strcpy(layer->name, "Background");
				layer->background = IMA_LAYER_BG_WHITE;
				copy_v4_v4(layer->default_color, white_color);
				base = (ImBuf *)layer->ibufs.first;
				if (base->rect_float) {
					float *fp_b = (float *) base->rect_float;
					for( i = base->x * base->y; i > 0; i--, fp_b+=4) {
						if (fp_b[3] != 1.0f) {
							if (fp_b[3] == 0.0f) {
								fp_b[0] = 1.0f;
								fp_b[1] = 1.0f;
								fp_b[2] = 1.0f;
							}
							fp_b[3] = 1.0f;
						}
					}
				} else if(base->rect) {
					char *cp_b = (char *) base->rect;
					for( i = base->x * base->y; i > 0; i--, cp_b+=4) {
						if (cp_b[3] != 255) {
							if (cp_b[3] == 0) {
								cp_b[0] = 255;
								cp_b[1] = 255;
								cp_b[2] = 255;
							}
							cp_b[3] = 255;
						}
					}
				}
			}
		}
		else {
			image_add_image_layer_base(ima);
			layer = (ImageLayer *)ima->imlayers.last;
			layer->background = IMA_LAYER_BG_WHITE;
			copy_v4_v4(layer->default_color, white_color);
			imalayer_fill_color(ima, white_color);
		}
	}
	
	WM_event_add_notifier(C, NC_IMAGE|ND_DRAW, NULL);
 
	return OPERATOR_FINISHED;
}

void IMAGE_OT_image_layer_merge(wmOperatorType *ot)
{
	static EnumPropertyItem slot_merge[] = {
		{1, "DOWN", 0, "Down", ""},
		{2, "VISIBLE", 0, "Visible", ""},
		{3, "ONE", 0, "One", ""},
		{0, NULL, 0, NULL, NULL}
	};
 
	/* identifiers */
	ot->name= "Merge Layer";
	ot->idname= "IMAGE_OT_image_layer_merge";
	ot->description="Layers merge into one";
 
	/* api callbacks */
	ot->exec= image_layer_merge_exec;
	ot->poll = image_layer_poll;
 
	/* flags */
	ot->flag= OPTYPE_REGISTER|OPTYPE_UNDO;
 
	/* properties */
	RNA_def_enum(ot->srna, "type", slot_merge, 0, "Type", "");
}

static int image_layer_clean_exec(bContext *C, wmOperator *op)
{
	Image *ima= CTX_data_edit_image(C);
	ImageLayer *layer;
	static float alpha_color[4] = {0.0f, 0.0f, 0.0f, 0.0f};
	static float white_color[4] = {1.0f, 1.0f, 1.0f, 1.0f};

	layer = imalayer_get_current(ima);

	if (layer->background & IMA_LAYER_BG_IMAGE) {
		int flag;
		struct ImBuf *ibuf;

		ibuf = layer->ibufs.first;
		BLI_remlink(&layer->ibufs, ibuf);
		IMB_freeImBuf(ibuf);

		flag= IB_rect|IB_multilayer|IB_metadata;
		if (ima->flag & IMA_DO_PREMUL)
			flag |= IB_premul;

		ibuf = IMB_loadiffname(layer->file_path, flag);
		
		BLI_addtail(&layer->ibufs, ibuf);
	}
	else if (layer->background & IMA_LAYER_BG_WHITE)
		imalayer_fill_color(ima, white_color);
	else if (layer->background & IMA_LAYER_BG_ALPHA)
		imalayer_fill_color(ima, alpha_color);
	else {
		if (layer->default_color[0] != -1)
			imalayer_fill_color(ima, layer->default_color);
	}

	WM_event_add_notifier(C, NC_IMAGE|ND_DRAW, ima);
	return OPERATOR_FINISHED;
}

void IMAGE_OT_image_layer_clean(wmOperatorType *ot)
{
 
	/* identifiers */
	ot->name= "Clean Layer";
	ot->idname= "IMAGE_OT_image_layer_clean";
	ot->description="Clean image layers";
 
	/* api callbacks */
	ot->exec= image_layer_clean_exec;
	ot->poll = image_layer_poll;
 
	/* flags */
	ot->flag= OPTYPE_REGISTER|OPTYPE_UNDO;
}

static int image_layer_flip_exec(bContext *C, wmOperator *op)
{
	Image *ima= CTX_data_edit_image(C);
	ImageLayer *layer;
	int type;
 
	if(!ima)
		return OPERATOR_CANCELLED;
  
	type = RNA_enum_get(op->ptr, "type");
	
	
	layer = imalayer_get_current(ima);
	if (!layer)
			return OPERATOR_CANCELLED;
	
	if (type == 1) /* Flip Horizontally */
		IMB_flipx((ImBuf *)layer->ibufs.first);
	else if (type == 2) /* Flip Vertically */
		IMB_flipy((ImBuf *)layer->ibufs.first);
	
	WM_event_add_notifier(C, NC_IMAGE|ND_DRAW, NULL);
 
	return OPERATOR_FINISHED;
}

void IMAGE_OT_image_layer_flip(wmOperatorType *ot)
{
	static EnumPropertyItem slot_flip[] = {
		{1, "FLIP_H", 0, "Horizontally", ""},
		{2, "FLIP_V", 0, "Vertically", ""},
		{0, NULL, 0, NULL, NULL}
	};
 
	/* identifiers */
	ot->name= "Flip Layer";
	ot->idname= "IMAGE_OT_image_layer_flip";
	ot->description="Flip the Layer";
 
	/* api callbacks */
	ot->exec= image_layer_flip_exec;
	ot->poll = image_layer_poll;
 
	/* flags */
	ot->flag= OPTYPE_REGISTER|OPTYPE_UNDO;
 
	/* properties */
	RNA_def_enum(ot->srna, "type", slot_flip, 0, "Type", "");
}

static int image_layer_rotate_exec(bContext *C, wmOperator *op)
{
	Image *ima= CTX_data_edit_image(C);
	ImageLayer *layer;
	ImBuf *ibuf;
	int type;
	static float alpha_color[4] = {0.0f, 0.0f, 0.0f, 0.0f};
	static float white_color[4] = {1.0f, 1.0f, 1.0f, 1.0f};
	float col[4];
 
	if(!ima)
		return OPERATOR_CANCELLED;
  
	type = RNA_enum_get(op->ptr, "type");

	layer = imalayer_get_current(ima);
	if (!layer)
			return OPERATOR_CANCELLED;
	
	if (layer->background & IMA_LAYER_BG_WHITE)
		copy_v4_v4(col, white_color);
	else if (layer->background & IMA_LAYER_BG_ALPHA)
		copy_v4_v4(col, alpha_color);
	else {
		if (layer->default_color[0] != -1)
			copy_v4_v4(col, layer->default_color);
	}

	ibuf = (ImBuf*)((ImageLayer*)layer->ibufs.first);
	if (type == 1) /* ROT_90 */
		layer->ibufs.first = IMB_rotation(ibuf, 0.0, 0.0, -90.0, 1.0, 2, col);
	else if (type == 2) /* ROT_90A */
		layer->ibufs.first = IMB_rotation(ibuf, 0.0, 0.0, 90.0, 1.0, 2, col);
	else if (type == 3) /* ROT_180 */
		layer->ibufs.first = IMB_rotation(ibuf, 0.0, 0.0, 180.0, 1.0, 2, col);

	WM_event_add_notifier(C, NC_IMAGE|ND_DRAW, NULL);
 
	return OPERATOR_FINISHED;
}

void IMAGE_OT_image_layer_rotate(wmOperatorType *ot)
{
	static EnumPropertyItem slot_rot[] = {
		{1, "ROT_90", 0, "Rotate 90 clockwise", ""},
		{2, "ROT_90A", 0, "Rotate 90 anti-clockwise", ""},
		{3, "ROT_180", 0, "Rotate 180", ""},
		{0, NULL, 0, NULL, NULL}
	};
 
	/* identifiers */
	ot->name= "Rotating Layer";
	ot->idname= "IMAGE_OT_image_layer_rotate";
	ot->description="Rotate the Layer";
 
	/* api callbacks */
	ot->exec= image_layer_rotate_exec;
	ot->poll = image_layer_poll;
 
	/* flags */
	ot->flag= OPTYPE_REGISTER|OPTYPE_UNDO;
 
	/* properties */
	RNA_def_enum(ot->srna, "type", slot_rot, 0, "Type", "");
}

static int image_layer_arbitrary_rot_exec(bContext *C, wmOperator *op)
{
	Image *ima= CTX_data_edit_image(C);
	ImageLayer *layer;
	float angle;
	static float alpha_color[4] = {0.0f, 0.0f, 0.0f, 0.0f};
	static float white_color[4] = {1.0f, 1.0f, 1.0f, 1.0f};
	float col[4];
	
	if(!ima)
		return OPERATOR_CANCELLED;
	
	layer = imalayer_get_current(ima);
	if (!layer)
			return OPERATOR_CANCELLED;

	//type = RNA_enum_get(op->ptr, "type");
	angle = RNA_float_get(op->ptr, "angle");
	
	if (layer->background & IMA_LAYER_BG_WHITE)
		copy_v4_v4(col, white_color);
	else if (layer->background & IMA_LAYER_BG_ALPHA)
		copy_v4_v4(col, alpha_color);
	else {
		if (layer->default_color[0] != -1)
			copy_v4_v4(col, layer->default_color);
	}

	angle = angle * (-1);
	
	layer->ibufs.first = IMB_rotation((ImBuf *)layer->ibufs.first, 0.0, 0.0, angle, 2, col);

	WM_event_add_notifier(C, NC_IMAGE|ND_DRAW, NULL);
 
	return OPERATOR_FINISHED;
}

void IMAGE_OT_image_layer_arbitrary_rot(wmOperatorType *ot)
{
	PropertyRNA *prop;
	
	static EnumPropertyItem rotate_items[] = {
		{0, "NEAREST",   0, "Nearest",   ""},
		{1, "BILINEAR",   0, "Bilinear",   ""},
		{2, "BICUBIC", 0, "Bicubic", ""},
		{0, NULL, 0, NULL, NULL}
	};

	/* identifiers */
	ot->name= "Arbitrary Rotating Layer";
	ot->idname= "IMAGE_OT_image_layer_arbitrary_rot";
	ot->description="Arbitrary Rotate the Layer";
 
	/* api callbacks */
	ot->exec= image_layer_arbitrary_rot_exec;
	ot->poll = image_layer_poll;
	ot->invoke = image_offset_invoke;
 
	/* flags */
	ot->flag= OPTYPE_REGISTER|OPTYPE_UNDO;
 
	/* properties */
	//RNA_def_enum(ot->srna, "type", rotate_items, 0, "Type", "");
	prop = RNA_def_float_rotation(ot->srna, "angle", 0, NULL, DEG2RADF(-180.0f), DEG2RADF(180.0f),
	                              "Angle", "Angle of rotation", DEG2RADF(-180.0f), DEG2RADF(180.0f));
	RNA_def_property_float_default(prop, DEG2RADF(0.0f));
}

static int image_layer_offset_exec(bContext *C, wmOperator *op)
{
	Image *ima= CTX_data_edit_image(C);
	ImageLayer *layer;
	struct ImBuf *ibuf;
	int x, y, half, wrap, background;
	static float alpha_color[4] = {0.0f, 0.0f, 0.0f, 0.0f};
	static float white_color[4] = {1.0f, 1.0f, 1.0f, 1.0f};
	float col[4];
	
	if(!ima)
		return OPERATOR_CANCELLED;
	
	layer = imalayer_get_current(ima);
	if (!layer)
			return OPERATOR_CANCELLED;

	ibuf = (ImBuf *)layer->ibufs.first;
	if (!ibuf)
			return OPERATOR_CANCELLED;
	
	x = RNA_int_get(op->ptr, "off_x");
	y = RNA_int_get(op->ptr, "off_y");
	half = RNA_boolean_get(op->ptr, "half");
	wrap = RNA_boolean_get(op->ptr, "wrap");
		
	if (abs(x) > ibuf->x) {
		BKE_report(op->reports, RPT_WARNING, "The offset can not be larger than the image.");
		return OPERATOR_CANCELLED;
	}

	if (abs(y) > ibuf->y) {
		BKE_report(op->reports, RPT_WARNING, "The offset can not be larger than the image.");
		return OPERATOR_CANCELLED;
	}

	if (!wrap) {
		if (layer->background & IMA_LAYER_BG_WHITE)
			copy_v4_v4(col, white_color);
		else if (layer->background & IMA_LAYER_BG_ALPHA)
			copy_v4_v4(col, alpha_color);
		else {
			if (layer->default_color[0] != -1)
				copy_v4_v4(col, layer->default_color);
		}
	}

	layer->ibufs.first = IMB_offset((ImBuf *)layer->ibufs.first, x, y, half, wrap, col);

	WM_event_add_notifier(C, NC_IMAGE|ND_DRAW, NULL);
 
	return OPERATOR_FINISHED;
}

void IMAGE_OT_image_layer_offset(wmOperatorType *ot)
{

	/* identifiers */
	ot->name = "Offset Layer";
	ot->idname = "IMAGE_OT_image_layer_offset";
	ot->description ="Shift the pixels";
 
	/* api callbacks */
	ot->exec= image_layer_offset_exec;
	ot->poll = image_layer_poll;
	ot->invoke = image_offset_invoke;

	/* flags */
	ot->flag = OPTYPE_REGISTER | OPTYPE_UNDO;

	/* properties */
	RNA_def_int(ot->srna, "off_x", 0, INT_MIN, INT_MAX, "X", "Offset X", -16384, 16384);
	RNA_def_int(ot->srna, "off_y", 0, INT_MIN, INT_MAX, "Y", "Offset Y", -16384, 16384);
	RNA_def_boolean(ot->srna, "half", 0, "Offset by x/2 y/2", "Offset by x/2 y/2.");
	RNA_def_boolean(ot->srna, "wrap", 1, "Wrap around", "Wrap around.");
}
/********************* pack operator *********************/

static int image_pack_test(bContext *C, wmOperator *op)
{
	Image *ima = CTX_data_edit_image(C);
	int as_png = RNA_boolean_get(op->ptr, "as_png");

	if (!ima)
		return 0;
	if (!as_png && ima->packedfile)
		return 0;

	if (ima->source == IMA_SRC_SEQUENCE || ima->source == IMA_SRC_MOVIE) {
		BKE_report(op->reports, RPT_ERROR, "Packing movies or image sequences not supported");
		return 0;
	}

	return 1;
}

static int image_pack_exec(bContext *C, wmOperator *op)
{
	struct Main *bmain = CTX_data_main(C);
	Image *ima = CTX_data_edit_image(C);
	ImBuf *ibuf = BKE_image_get_ibuf(ima, NULL, IMA_IBUF_IMA);
	int as_png = RNA_boolean_get(op->ptr, "as_png");

	if (!image_pack_test(C, op))
		return OPERATOR_CANCELLED;
	
	if (!as_png && (ibuf && (ibuf->userflags & IB_BITMAPDIRTY))) {
		BKE_report(op->reports, RPT_ERROR, "Can't pack edited image from disk, only as internal PNG");
		return OPERATOR_CANCELLED;
	}

	if (as_png)
		BKE_image_memorypack(ima);
	else
		ima->packedfile = newPackedFile(op->reports, ima->name, ID_BLEND_PATH(bmain, &ima->id));

	WM_event_add_notifier(C, NC_IMAGE | NA_EDITED, ima);
	
	return OPERATOR_FINISHED;
}

static int image_pack_invoke(bContext *C, wmOperator *op, wmEvent *UNUSED(event))
{
	Image *ima = CTX_data_edit_image(C);
	ImBuf *ibuf = BKE_image_get_ibuf(ima, NULL, IMA_IBUF_IMA);
	uiPopupMenu *pup;
	uiLayout *layout;
	int as_png = RNA_boolean_get(op->ptr, "as_png");

	if (!image_pack_test(C, op))
		return OPERATOR_CANCELLED;
	
	if (!as_png && (ibuf && (ibuf->userflags & IB_BITMAPDIRTY))) {
		pup = uiPupMenuBegin(C, "OK", ICON_QUESTION);
		layout = uiPupMenuLayout(pup);
		uiItemBooleanO(layout, "Can't pack edited image from disk. Pack as internal PNG?", ICON_NONE, op->idname, "as_png", 1);
		uiPupMenuEnd(C, pup);

		return OPERATOR_CANCELLED;
	}

	return image_pack_exec(C, op);
}

void IMAGE_OT_pack(wmOperatorType *ot)
{
	/* identifiers */
	ot->name = "Pack Image";
	ot->description = "Pack an image as embedded data into the .blend file"; 
	ot->idname = "IMAGE_OT_pack";
	
	/* api callbacks */
	ot->exec = image_pack_exec;
	ot->invoke = image_pack_invoke;

	/* flags */
	ot->flag = OPTYPE_REGISTER | OPTYPE_UNDO;

	/* properties */
	RNA_def_boolean(ot->srna, "as_png", 0, "Pack As PNG", "Pack image as lossless PNG");
}

/********************* unpack operator *********************/

static int image_unpack_exec(bContext *C, wmOperator *op)
{
	Image *ima = CTX_data_edit_image(C);
	int method = RNA_enum_get(op->ptr, "method");

	/* find the suppplied image by name */
	if (RNA_struct_property_is_set(op->ptr, "id")) {
		char imaname[MAX_ID_NAME - 2];
		RNA_string_get(op->ptr, "id", imaname);
		ima = BLI_findstring(&CTX_data_main(C)->image, imaname, offsetof(ID, name) + 2);
		if (!ima) ima = CTX_data_edit_image(C);
	}
	
	if (!ima || !ima->packedfile)
		return OPERATOR_CANCELLED;

	if (ima->source == IMA_SRC_SEQUENCE || ima->source == IMA_SRC_MOVIE) {
		BKE_report(op->reports, RPT_ERROR, "Unpacking movies or image sequences not supported");
		return OPERATOR_CANCELLED;
	}

	if (G.fileflags & G_AUTOPACK)
		BKE_report(op->reports, RPT_WARNING, "AutoPack is enabled, so image will be packed again on file save");
	
	/* XXX unpackImage frees image buffers */
	ED_preview_kill_jobs(C);
	
	unpackImage(op->reports, ima, method);
	
	WM_event_add_notifier(C, NC_IMAGE | NA_EDITED, ima);

	return OPERATOR_FINISHED;
}

static int image_unpack_invoke(bContext *C, wmOperator *op, wmEvent *UNUSED(event))
{
	Image *ima = CTX_data_edit_image(C);

	if (RNA_struct_property_is_set(op->ptr, "id"))
		return image_unpack_exec(C, op);
		
	if (!ima || !ima->packedfile)
		return OPERATOR_CANCELLED;

	if (ima->source == IMA_SRC_SEQUENCE || ima->source == IMA_SRC_MOVIE) {
		BKE_report(op->reports, RPT_ERROR, "Unpacking movies or image sequences not supported");
		return OPERATOR_CANCELLED;
	}

	if (G.fileflags & G_AUTOPACK)
		BKE_report(op->reports, RPT_WARNING, "AutoPack is enabled, so image will be packed again on file save");

	unpack_menu(C, "IMAGE_OT_unpack", ima->id.name + 2, ima->name, "textures", ima->packedfile);

	return OPERATOR_FINISHED;
}

void IMAGE_OT_unpack(wmOperatorType *ot)
{
	/* identifiers */
	ot->name = "Unpack Image";
	ot->description = "Save an image packed in the .blend file to disk"; 
	ot->idname = "IMAGE_OT_unpack";
	
	/* api callbacks */
	ot->exec = image_unpack_exec;
	ot->invoke = image_unpack_invoke;

	/* flags */
	ot->flag = OPTYPE_REGISTER | OPTYPE_UNDO;
	
	/* properties */
	RNA_def_enum(ot->srna, "method", unpack_method_items, PF_USE_LOCAL, "Method", "How to unpack");
	RNA_def_string(ot->srna, "id", "", MAX_ID_NAME - 2, "Image Name", "Image datablock name to unpack"); /* XXX, weark!, will fail with library, name collisions */
}

/******************** sample image operator ********************/

typedef struct ImageSampleInfo {
	ARegionType *art;
	void *draw_handle;
	int x, y;
	int channels;

	unsigned char col[4];
	float colf[4];
	int z;
	float zf;

	unsigned char *colp;
	float *colfp;
	int *zp;
	float *zfp;

	int draw;
} ImageSampleInfo;

static void image_sample_draw(const bContext *UNUSED(C), ARegion *ar, void *arg_info)
{
	ImageSampleInfo *info = arg_info;
	if (info->draw) {
		/* no color management needed for images (color_manage=0) */
		ED_image_draw_info(ar, 0, info->channels, info->x, info->y, info->colp, info->colfp, info->zp, info->zfp, 1);
	}
}

static void image_sample_apply(bContext *C, wmOperator *op, wmEvent *event)
{
	SpaceImage *sima = CTX_wm_space_image(C);
	ARegion *ar = CTX_wm_region(C);
	void *lock;
	ImBuf *ibuf = ED_space_image_acquire_buffer(sima, &lock);
	ImageSampleInfo *info = op->customdata;
	float fx, fy;
	//wmWindow *win= CTX_wm_window(C);
	
	if (ibuf == NULL) {
		ED_space_image_release_buffer(sima, lock);
		info->draw = 0;
		return;
	}

	//WM_cursor_modal(win, BC_EYEDROPPER_CURSOR);
	UI_view2d_region_to_view(&ar->v2d, event->mval[0], event->mval[1], &fx, &fy);

	if (fx >= 0.0f && fy >= 0.0f && fx < 1.0f && fy < 1.0f) {
		float *fp;
		unsigned char *cp;
		int x = (int)(fx * ibuf->x), y = (int)(fy * ibuf->y);

		CLAMP(x, 0, ibuf->x - 1);
		CLAMP(y, 0, ibuf->y - 1);

		info->x = x;
		info->y = y;
		info->draw = 1;
		info->channels = ibuf->channels;

		info->colp = NULL;
		info->colfp = NULL;
		info->zp = NULL;
		info->zfp = NULL;
		
		if (ibuf->rect) {
			cp = (unsigned char *)(ibuf->rect + y * ibuf->x + x);

			info->col[0] = cp[0];
			info->col[1] = cp[1];
			info->col[2] = cp[2];
			info->col[3] = cp[3];
			info->colp = info->col;

			info->colf[0] = (float)cp[0] / 255.0f;
			info->colf[1] = (float)cp[1] / 255.0f;
			info->colf[2] = (float)cp[2] / 255.0f;
			info->colf[3] = (float)cp[3] / 255.0f;
			info->colfp = info->colf;
		}
		if (ibuf->rect_float) {
			fp = (ibuf->rect_float + (ibuf->channels) * (y * ibuf->x + x));

			info->colf[0] = fp[0];
			info->colf[1] = fp[1];
			info->colf[2] = fp[2];
			info->colf[3] = fp[3];
			info->colfp = info->colf;
		}

		if (ibuf->zbuf) {
			info->z = ibuf->zbuf[y * ibuf->x + x];
			info->zp = &info->z;
		}
		if (ibuf->zbuf_float) {
			info->zf = ibuf->zbuf_float[y * ibuf->x + x];
			info->zfp = &info->zf;
		}
		
		if (sima->cumap && ibuf->channels == 4) {
			/* we reuse this callback for set curves point operators */
			if (RNA_struct_find_property(op->ptr, "point")) {
				int point = RNA_enum_get(op->ptr, "point");

				if (point == 1) {
					curvemapping_set_black_white(sima->cumap, NULL, info->colfp);
					if (ibuf->rect_float)
						curvemapping_do_ibuf(sima->cumap, ibuf);
				}
				else if (point == 0) {
					curvemapping_set_black_white(sima->cumap, info->colfp, NULL);
					if (ibuf->rect_float)
						curvemapping_do_ibuf(sima->cumap, ibuf);
				}
			}
		}
				
		// XXX node curve integration ..
#if 0
		{
			ScrArea *sa, *cur = curarea;
			
			node_curvemap_sample(fp);   /* sends global to node editor */
			for (sa = G.curscreen->areabase.first; sa; sa = sa->next) {
				if (sa->spacetype == SPACE_NODE) {
					areawinset(sa->win);
					scrarea_do_windraw(sa);
				}
			}
			node_curvemap_sample(NULL);     /* clears global in node editor */
			curarea = cur;
		}
#endif
	}
	else {
		info->draw = 0;
	}

	ED_space_image_release_buffer(sima, lock);
	ED_area_tag_redraw(CTX_wm_area(C));
}

static void image_sample_exit(bContext *C, wmOperator *op)
{
	ImageSampleInfo *info = op->customdata;

	ED_region_draw_cb_exit(info->art, info->draw_handle);
	ED_area_tag_redraw(CTX_wm_area(C));
	MEM_freeN(info);
}

static int image_sample_invoke(bContext *C, wmOperator *op, wmEvent *event)
{
	SpaceImage *sima = CTX_wm_space_image(C);
	ARegion *ar = CTX_wm_region(C);
	ImageSampleInfo *info;

	if (!ED_space_image_has_buffer(sima))
		return OPERATOR_CANCELLED;

	info = MEM_callocN(sizeof(ImageSampleInfo), "ImageSampleInfo");
	info->art = ar->type;
	info->draw_handle = ED_region_draw_cb_activate(ar->type, image_sample_draw, info, REGION_DRAW_POST_PIXEL);
	op->customdata = info;

	image_sample_apply(C, op, event);

	WM_event_add_modal_handler(C, op);

	return OPERATOR_RUNNING_MODAL;
}

static int image_sample_modal(bContext *C, wmOperator *op, wmEvent *event)
{	
	switch (event->type) {
		case LEFTMOUSE:
		case RIGHTMOUSE: // XXX hardcoded
			image_sample_exit(C, op);
			return OPERATOR_CANCELLED;
		case MOUSEMOVE:
			image_sample_apply(C, op, event);
			break;
	}

	return OPERATOR_RUNNING_MODAL;
}

static int image_sample_cancel(bContext *C, wmOperator *op)
{
	image_sample_exit(C, op);
	return OPERATOR_CANCELLED;
}

void IMAGE_OT_sample(wmOperatorType *ot)
{
	/* identifiers */
	ot->name = "Sample Color";
	ot->idname = "IMAGE_OT_sample";
	ot->description = "Use mouse to sample a color in current image";
	
	/* api callbacks */
	ot->invoke = image_sample_invoke;
	ot->modal = image_sample_modal;
	ot->cancel = image_sample_cancel;
	ot->poll = space_image_image_sample_poll;

	/* flags */
	ot->flag = OPTYPE_BLOCKING;
}

/******************** sample line operator ********************/
static int image_sample_line_exec(bContext *C, wmOperator *op)
{
	SpaceImage *sima = CTX_wm_space_image(C);
	ARegion *ar = CTX_wm_region(C);
	Scene *scene = CTX_data_scene(C);

	int x_start = RNA_int_get(op->ptr, "xstart");
	int y_start = RNA_int_get(op->ptr, "ystart");
	int x_end = RNA_int_get(op->ptr, "xend");
	int y_end = RNA_int_get(op->ptr, "yend");
	
	void *lock;
	ImBuf *ibuf = ED_space_image_acquire_buffer(sima, &lock);
	Histogram *hist = &sima->sample_line_hist;
	
	float x1f, y1f, x2f, y2f;
	
	if (ibuf == NULL) {
		ED_space_image_release_buffer(sima, lock);
		return OPERATOR_CANCELLED;
	}
	/* hmmmm */
	if (ibuf->channels < 3) {
		ED_space_image_release_buffer(sima, lock);
		return OPERATOR_CANCELLED;
	}
	
	UI_view2d_region_to_view(&ar->v2d, x_start, y_start, &x1f, &y1f);
	UI_view2d_region_to_view(&ar->v2d, x_end, y_end, &x2f, &y2f);

	hist->co[0][0] = x1f;
	hist->co[0][1] = y1f;
	hist->co[1][0] = x2f;
	hist->co[1][1] = y2f;

	BKE_histogram_update_sample_line(hist, ibuf, (scene->r.color_mgt_flag & R_COLOR_MANAGEMENT) != 0);
	
	ED_space_image_release_buffer(sima, lock);
	
	ED_area_tag_redraw(CTX_wm_area(C));
	
	return OPERATOR_FINISHED;
}

static int image_sample_line_invoke(bContext *C, wmOperator *op, wmEvent *event)
{
	SpaceImage *sima = CTX_wm_space_image(C);

	Histogram *hist = &sima->sample_line_hist;
	hist->flag &= ~HISTO_FLAG_SAMPLELINE;

	if (!ED_space_image_has_buffer(sima))
		return OPERATOR_CANCELLED;
	
	return WM_gesture_straightline_invoke(C, op, event);
}

void IMAGE_OT_sample_line(wmOperatorType *ot)
{
	/* identifiers */
	ot->name = "Sample Line";
	ot->idname = "IMAGE_OT_sample_line";
	ot->description = "Sample a line and show it in Scope panels";
	
	/* api callbacks */
	ot->invoke = image_sample_line_invoke;
	ot->modal = WM_gesture_straightline_modal;
	ot->exec = image_sample_line_exec;
	ot->poll = space_image_main_area_poll;
	ot->cancel = WM_gesture_straightline_cancel;
	
	/* flags */
	ot->flag = 0; /* no undo/register since this operates on the space */
	
	WM_operator_properties_gesture_straightline(ot, CURSOR_EDIT);
}

/******************** set curve point operator ********************/

void IMAGE_OT_curves_point_set(wmOperatorType *ot)
{
	static EnumPropertyItem point_items[] = {
		{0, "BLACK_POINT", 0, "Black Point", ""},
		{1, "WHITE_POINT", 0, "White Point", ""},
		{0, NULL, 0, NULL, NULL}
	};

	/* identifiers */
	ot->name = "Set Curves Point";
	ot->idname = "IMAGE_OT_curves_point_set";
	ot->description = "Set black point or white point for curves";

	/* flags */
	ot->flag = OPTYPE_REGISTER | OPTYPE_UNDO;
	
	/* api callbacks */
	ot->invoke = image_sample_invoke;
	ot->modal = image_sample_modal;
	ot->cancel = image_sample_cancel;
	ot->poll = space_image_main_area_not_uv_brush_poll;

	/* properties */
	RNA_def_enum(ot->srna, "point", point_items, 0, "Point", "Set black point or white point for curves");
}

/******************** record composite operator *********************/

typedef struct RecordCompositeData {
	wmTimer *timer;
	int old_cfra;
	int sfra, efra;
} RecordCompositeData;

static int image_record_composite_apply(bContext *C, wmOperator *op)
{
	SpaceImage *sima = CTX_wm_space_image(C);
	RecordCompositeData *rcd = op->customdata;
	Scene *scene = CTX_data_scene(C);
	ImBuf *ibuf;
	
	WM_timecursor(CTX_wm_window(C), scene->r.cfra);

	// XXX scene->nodetree->test_break= blender_test_break;
	// XXX scene->nodetree->test_break= NULL;
	
	BKE_image_all_free_anim_ibufs(scene->r.cfra);
	ntreeCompositTagAnimated(scene->nodetree);
	ntreeCompositExecTree(scene->nodetree, &scene->r, 0, scene->r.cfra != rcd->old_cfra);  /* 1 is no previews */

	ED_area_tag_redraw(CTX_wm_area(C));
	
	ibuf = BKE_image_get_ibuf(sima->image, &sima->iuser, IMA_IBUF_IMA);
	/* save memory in flipbooks */
	if (ibuf)
		imb_freerectfloatImBuf(ibuf);
	
	scene->r.cfra++;

	return (scene->r.cfra <= rcd->efra);
}

static int image_record_composite_init(bContext *C, wmOperator *op)
{
	SpaceImage *sima = CTX_wm_space_image(C);
	Scene *scene = CTX_data_scene(C);
	RecordCompositeData *rcd;

	if (sima->iuser.frames < 2)
		return 0;
	if (scene->nodetree == NULL)
		return 0;
	
	op->customdata = rcd = MEM_callocN(sizeof(RecordCompositeData), "ImageRecordCompositeData");

	rcd->old_cfra = scene->r.cfra;
	rcd->sfra = sima->iuser.sfra;
	rcd->efra = sima->iuser.sfra + sima->iuser.frames - 1;
	scene->r.cfra = rcd->sfra;

	return 1;
}

static void image_record_composite_exit(bContext *C, wmOperator *op)
{
	Scene *scene = CTX_data_scene(C);
	SpaceImage *sima = CTX_wm_space_image(C);
	RecordCompositeData *rcd = op->customdata;

	scene->r.cfra = rcd->old_cfra;

	WM_cursor_restore(CTX_wm_window(C));

	if (rcd->timer)
		WM_event_remove_timer(CTX_wm_manager(C), CTX_wm_window(C), rcd->timer);

	WM_event_add_notifier(C, NC_IMAGE | NA_EDITED, sima->image);

	// XXX play_anim(0);
	// XXX allqueue(REDRAWNODE, 1);

	MEM_freeN(rcd);
}

static int image_record_composite_exec(bContext *C, wmOperator *op)
{
	if (!image_record_composite_init(C, op))
		return OPERATOR_CANCELLED;
	
	while (image_record_composite_apply(C, op))
		;
	
	image_record_composite_exit(C, op);
	
	return OPERATOR_FINISHED;
}

static int image_record_composite_invoke(bContext *C, wmOperator *op, wmEvent *UNUSED(event))
{
	RecordCompositeData *rcd;
	
	if (!image_record_composite_init(C, op))
		return OPERATOR_CANCELLED;

	rcd = op->customdata;
	rcd->timer = WM_event_add_timer(CTX_wm_manager(C), CTX_wm_window(C), TIMER, 0.0f);
	WM_event_add_modal_handler(C, op);

	if (!image_record_composite_apply(C, op))
		return OPERATOR_FINISHED;

	return OPERATOR_RUNNING_MODAL;
}

static int image_record_composite_modal(bContext *C, wmOperator *op, wmEvent *event)
{
	RecordCompositeData *rcd = op->customdata;

	switch (event->type) {
		case TIMER:
			if (rcd->timer == event->customdata) {
				if (!image_record_composite_apply(C, op)) {
					image_record_composite_exit(C, op);
					return OPERATOR_FINISHED;
				}
			}
			break;
		case ESCKEY:
			image_record_composite_exit(C, op);
			return OPERATOR_FINISHED;
	}

	return OPERATOR_RUNNING_MODAL;
}

static int image_record_composite_cancel(bContext *C, wmOperator *op)
{
	image_record_composite_exit(C, op);
	return OPERATOR_CANCELLED;
}

void IMAGE_OT_record_composite(wmOperatorType *ot)
{
	/* identifiers */
	ot->name = "Record Composite";
	ot->idname = "IMAGE_OT_record_composite";
	
	/* api callbacks */
	ot->exec = image_record_composite_exec;
	ot->invoke = image_record_composite_invoke;
	ot->modal = image_record_composite_modal;
	ot->cancel = image_record_composite_cancel;
	ot->poll = space_image_buffer_exists_poll;
}

/********************* cycle render slot operator *********************/

static int image_cycle_render_slot_poll(bContext *C)
{
	Image *ima = CTX_data_edit_image(C);

	return (ima && ima->type == IMA_TYPE_R_RESULT);
}

static int image_cycle_render_slot_exec(bContext *C, wmOperator *op)
{
	Image *ima = CTX_data_edit_image(C);
	int a, slot, cur = ima->render_slot;
	const short use_reverse = RNA_boolean_get(op->ptr, "reverse");

	for (a = 1; a < IMA_MAX_RENDER_SLOT; a++) {
		slot = (cur + (use_reverse ? -a : a)) % IMA_MAX_RENDER_SLOT;
		if (slot < 0) slot += IMA_MAX_RENDER_SLOT;

		if (ima->renders[slot] || slot == ima->last_render_slot) {
			ima->render_slot = slot;
			break;
		}
	}

	if (a == IMA_MAX_RENDER_SLOT)
		ima->render_slot = ((cur == 1) ? 0 : 1);
	
	WM_event_add_notifier(C, NC_IMAGE | ND_DRAW, NULL);

	/* no undo push for browsing existing */
	if (ima->renders[ima->render_slot] || ima->render_slot == ima->last_render_slot)
		return OPERATOR_CANCELLED;
	
	return OPERATOR_FINISHED;
}

void IMAGE_OT_cycle_render_slot(wmOperatorType *ot)
{
	/* identifiers */
	ot->name = "Cycle Render Slot";
	ot->idname = "IMAGE_OT_cycle_render_slot";
	ot->description = "Cycle through all non-void render slots";
	
	/* api callbacks */
	ot->exec = image_cycle_render_slot_exec;
	ot->poll = image_cycle_render_slot_poll;

	/* flags */
	ot->flag = OPTYPE_REGISTER | OPTYPE_UNDO;

	RNA_def_boolean(ot->srna, "reverse", 0, "Cycle in Reverse", "");
}

/******************** TODO ********************/

/* XXX notifier? */

/* goes over all ImageUsers, and sets frame numbers if auto-refresh is set */

static void image_update_frame(struct Image *UNUSED(ima), struct ImageUser *iuser, void *customdata)
{
	int cfra = *(int*)customdata;

	BKE_image_user_check_frame_calc(iuser, cfra, 0);
}

void ED_image_update_frame(const Main *mainp, int cfra)
{
	BKE_image_walk_all_users(mainp, &cfra, image_update_frame);
}

