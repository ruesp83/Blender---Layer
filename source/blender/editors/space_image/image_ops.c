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

#include "BLF_translation.h"

#include "DNA_object_types.h"
#include "DNA_node_types.h"
#include "DNA_packedFile_types.h"
#include "DNA_scene_types.h"

#include "BKE_colortools.h"
#include "BKE_context.h"
#include "BKE_icons.h"
#include "BKE_image.h"
#include "BKE_layer.h"
#include "BKE_global.h"
#include "BKE_library.h"
#include "BKE_main.h"
#include "BKE_node.h"
#include "BKE_packedFile.h"
#include "BKE_report.h"
#include "BKE_screen.h"

#include "GPU_draw.h"

#include "IMB_colormanagement.h"
#include "IMB_imbuf.h"
#include "DNA_imbuf_types.h"

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

#include "PIL_time.h"

#include "image_intern.h"
#include "ED_sculpt.h"

/******************** view navigation utilities *********************/

static void sima_zoom_set(SpaceImage *sima, ARegion *ar, float zoom, const float location[2])
{
	float oldzoom = sima->zoom;
	int width, height;

	sima->zoom = zoom;

	if (sima->zoom < 0.1f || sima->zoom > 4.0f) {
		/* check zoom limits */
		ED_space_image_get_size(sima, &width, &height);

		width *= sima->zoom;
		height *= sima->zoom;

		if ((width < 4) && (height < 4))
			sima->zoom = oldzoom;
		else if (BLI_rcti_size_x(&ar->winrct) <= sima->zoom)
			sima->zoom = oldzoom;
		else if (BLI_rcti_size_y(&ar->winrct) <= sima->zoom)
			sima->zoom = oldzoom;
	}

	if ((U.uiflag & USER_ZOOM_TO_MOUSEPOS) && location) {
		float aspx, aspy, w, h;

		ED_space_image_get_size(sima, &width, &height);
		ED_space_image_get_aspect(sima, &aspx, &aspy);

		w = width * aspx;
		h = height * aspy;

		sima->xof += ((location[0] - 0.5f) * w - sima->xof) * (sima->zoom - oldzoom) / sima->zoom;
		sima->yof += ((location[1] - 0.5f) * h - sima->yof) * (sima->zoom - oldzoom) / sima->zoom;
	}
}

static void sima_zoom_set_factor(SpaceImage *sima, ARegion *ar, float zoomfac, const float location[2])
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
			else if (!BLI_file_is_writable(name)) {
				CTX_wm_operator_poll_msg_set(C, "image path can't be written to");
			}
			else {
				ret = TRUE;
			}
		}
		ED_space_image_release_buffer(sima, ibuf, lock);

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
	// XXX ARegion *ar = CTX_wm_region(C);

	if (sima)
		return 1;  // XXX (ar && ar->type->regionid == RGN_TYPE_WINDOW);
	
	return 0;
}

/* For IMAGE_OT_curves_point_set to avoid sampling when in uv smooth mode or editmode */
static int space_image_main_area_not_uv_brush_poll(bContext *C)
{
	SpaceImage *sima = CTX_wm_space_image(C);
	Scene *scene = CTX_data_scene(C);
	ToolSettings *toolsettings = scene->toolsettings;

	if (sima && !toolsettings->uvsculpt && !scene->obedit)
		return 1;

	return 0;
}

static int image_sample_poll(bContext *C)
{
	SpaceImage *sima = CTX_wm_space_image(C);
	if (sima) {
		Scene *scene = CTX_data_scene(C);
		Object *obedit = CTX_data_edit_object(C);
		ToolSettings *toolsettings = scene->toolsettings;

		if (obedit) {
			if (ED_space_image_show_uvedit(sima, obedit) && (toolsettings->use_uv_sculpt))
				return FALSE;
		}
		else if (sima->mode != SI_MODE_VIEW) {
			return FALSE;
		}

		return space_image_main_area_poll(C);
	}
	else {
		return FALSE;
	}
}
/********************** view pan operator *********************/

typedef struct ViewPanData {
	float x, y;
	float xof, yof;
	int event_type;
} ViewPanData;

static void image_view_pan_init(bContext *C, wmOperator *op, const wmEvent *event)
{
	SpaceImage *sima = CTX_wm_space_image(C);
	ViewPanData *vpd;

	op->customdata = vpd = MEM_callocN(sizeof(ViewPanData), "ImageViewPanData");
	WM_cursor_modal_set(CTX_wm_window(C), BC_NSEW_SCROLLCURSOR);

	vpd->x = event->x;
	vpd->y = event->y;
	vpd->xof = sima->xof;
	vpd->yof = sima->yof;
	vpd->event_type = event->type;

	WM_event_add_modal_handler(C, op);
}

static void image_view_pan_exit(bContext *C, wmOperator *op, bool cancel)
{
	SpaceImage *sima = CTX_wm_space_image(C);
	ViewPanData *vpd = op->customdata;

	if (cancel) {
		sima->xof = vpd->xof;
		sima->yof = vpd->yof;
		ED_region_tag_redraw(CTX_wm_region(C));
	}

	WM_cursor_modal_restore(CTX_wm_window(C));
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

static int image_view_pan_invoke(bContext *C, wmOperator *op, const wmEvent *event)
{
	if (event->type == MOUSEPAN) {
		SpaceImage *sima = CTX_wm_space_image(C);
		float offset[2];
		
		offset[0] = (event->prevx - event->x) / sima->zoom;
		offset[1] = (event->prevy - event->y) / sima->zoom;
		RNA_float_set_array(op->ptr, "offset", offset);

		image_view_pan_exec(C, op);
		return OPERATOR_FINISHED;
	}
	else {
		image_view_pan_init(C, op, event);
		return OPERATOR_RUNNING_MODAL;
	}
}

static int image_view_pan_modal(bContext *C, wmOperator *op, const wmEvent *event)
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
				image_view_pan_exit(C, op, false);
				return OPERATOR_FINISHED;
			}
			break;
	}

	return OPERATOR_RUNNING_MODAL;
}

static void image_view_pan_cancel(bContext *C, wmOperator *op)
{
	image_view_pan_exit(C, op, true);
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
	float origx, origy;
	float zoom;
	int event_type;
	float location[2];

	/* needed for continuous zoom */
	wmTimer *timer;
	double timer_lastdraw;

	/* */
	SpaceImage *sima;
	ARegion *ar;
} ViewZoomData;

static void image_view_zoom_init(bContext *C, wmOperator *op, const wmEvent *event)
{
	SpaceImage *sima = CTX_wm_space_image(C);
	ARegion *ar = CTX_wm_region(C);
	ViewZoomData *vpd;

	op->customdata = vpd = MEM_callocN(sizeof(ViewZoomData), "ImageViewZoomData");
	WM_cursor_modal_set(CTX_wm_window(C), BC_NSEW_SCROLLCURSOR);

	vpd->origx = event->x;
	vpd->origy = event->y;
	vpd->zoom = sima->zoom;
	vpd->event_type = event->type;

	UI_view2d_region_to_view(&ar->v2d, event->mval[0], event->mval[1], &vpd->location[0], &vpd->location[1]);

	if (U.viewzoom == USER_ZOOM_CONT) {
		/* needs a timer to continue redrawing */
		vpd->timer = WM_event_add_timer(CTX_wm_manager(C), CTX_wm_window(C), TIMER, 0.01f);
		vpd->timer_lastdraw = PIL_check_seconds_timer();
	}

	vpd->sima = sima;
	vpd->ar = ar;

	WM_event_add_modal_handler(C, op);
}

static void image_view_zoom_exit(bContext *C, wmOperator *op, bool cancel)
{
	SpaceImage *sima = CTX_wm_space_image(C);
	ViewZoomData *vpd = op->customdata;

	if (cancel) {
		sima->zoom = vpd->zoom;
		ED_region_tag_redraw(CTX_wm_region(C));
	}

	if (vpd->timer)
		WM_event_remove_timer(CTX_wm_manager(C), vpd->timer->win, vpd->timer);

	WM_cursor_modal_restore(CTX_wm_window(C));
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

enum {
	VIEW_PASS = 0,
	VIEW_APPLY,
	VIEW_CONFIRM
};

static int image_view_zoom_invoke(bContext *C, wmOperator *op, const wmEvent *event)
{
	if (event->type == MOUSEZOOM || event->type == MOUSEPAN) {
		SpaceImage *sima = CTX_wm_space_image(C);
		ARegion *ar = CTX_wm_region(C);
		float delta, factor, location[2];

		UI_view2d_region_to_view(&ar->v2d, event->mval[0], event->mval[1], &location[0], &location[1]);

		delta = event->prevx - event->x + event->prevy - event->y;

		if (U.uiflag & USER_ZOOM_INVERT)
			delta *= -1;

		factor = 1.0f + delta / 300.0f;
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

static void image_zoom_apply(ViewZoomData *vpd, wmOperator *op, const int x, const int y, const short viewzoom, const short zoom_invert)
{
	float factor;

	if (viewzoom == USER_ZOOM_CONT) {
		double time = PIL_check_seconds_timer();
		float time_step = (float)(time - vpd->timer_lastdraw);
		float fac;
		float zfac;

		if (U.uiflag & USER_ZOOM_HORIZ) {
			fac = (float)(x - vpd->origx);
		}
		else {
			fac = (float)(y - vpd->origy);
		}

		if (zoom_invert) {
			fac = -fac;
		}

		/* oldstyle zoom */
		zfac = 1.0f + ((fac / 20.0f) * time_step);
		vpd->timer_lastdraw = time;
		/* this is the final zoom, but instead make it into a factor */
		//zoom = vpd->sima->zoom * zfac;
		factor = (vpd->sima->zoom * zfac) / vpd->zoom;
	}
	else {
		/* for now do the same things for scale and dolly */
		float delta = x - vpd->origx + y - vpd->origy;

		if (zoom_invert)
			delta *= -1.0f;

		factor = 1.0f + delta / 300.0f;
	}

	RNA_float_set(op->ptr, "factor", factor);
	sima_zoom_set(vpd->sima, vpd->ar, vpd->zoom * factor, vpd->location);
	ED_region_tag_redraw(vpd->ar);
}

static int image_view_zoom_modal(bContext *C, wmOperator *op, const wmEvent *event)
{
	ViewZoomData *vpd = op->customdata;
	short event_code = VIEW_PASS;

	/* execute the events */
	if (event->type == TIMER && event->customdata == vpd->timer) {
		/* continuous zoom */
		event_code = VIEW_APPLY;
	}
	else if (event->type == MOUSEMOVE) {
		event_code = VIEW_APPLY;
	}
	else if (event->type == vpd->event_type && event->val == KM_RELEASE) {
		event_code = VIEW_CONFIRM;
	}

	if (event_code == VIEW_APPLY) {
		image_zoom_apply(vpd, op, event->x, event->y, U.viewzoom, (U.uiflag & USER_ZOOM_INVERT) != 0);
	}
	else if (event_code == VIEW_CONFIRM) {
		image_view_zoom_exit(C, op, false);
		return OPERATOR_FINISHED;
	}

	return OPERATOR_RUNNING_MODAL;
}

static void image_view_zoom_cancel(bContext *C, wmOperator *op)
{
	image_view_zoom_exit(C, op, true);
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
	RNA_def_float(ot->srna, "factor", 0.0f, -FLT_MAX, FLT_MAX,
	              "Factor", "Zoom factor, values higher than 1.0 zoom in, lower values zoom out", -FLT_MAX, FLT_MAX);
}

/********************** NDOF operator *********************/

/* Combined pan/zoom from a 3D mouse device.
 * Z zooms, XY pans
 * "view" (not "paper") control -- user moves the viewpoint, not the image being viewed
 * that explains the negative signs in the code below
 */

static int image_view_ndof_invoke(bContext *C, wmOperator *UNUSED(op), const wmEvent *event)
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

static int image_view_all_exec(bContext *C, wmOperator *op)
{
	SpaceImage *sima;
	ARegion *ar;
	float aspx, aspy, zoomx, zoomy, w, h;
	int width, height;
	int fit_view = RNA_boolean_get(op->ptr, "fit_view");

	/* retrieve state */
	sima = CTX_wm_space_image(C);
	ar = CTX_wm_region(C);

	ED_space_image_get_size(sima, &width, &height);
	ED_space_image_get_aspect(sima, &aspx, &aspy);

	w = width * aspx;
	h = height * aspy;
	
	/* check if the image will fit in the image with (zoom == 1) */
	width  = BLI_rcti_size_x(&ar->winrct) + 1;
	height = BLI_rcti_size_y(&ar->winrct) + 1;

	if (fit_view) {
		const int margin = 5; /* margin from border */

		zoomx = (float) width / (w + 2 * margin);
		zoomy = (float) height / (h + 2 * margin);

		sima_zoom_set(sima, ar, min_ff(zoomx, zoomy), NULL);
	}
	else {
		if ((w >= width || h >= height) && (width > 0 && height > 0)) {
			zoomx = (float) width / w;
			zoomy = (float) height / h;

			/* find the zoom value that will fit the image in the image space */
			sima_zoom_set(sima, ar, 1.0f / power_of_2(1.0f / min_ff(zoomx, zoomy)), NULL);
		}
		else
			sima_zoom_set(sima, ar, 1.0f, NULL);
	}

	sima->xof = sima->yof = 0.0f;

	ED_region_tag_redraw(CTX_wm_region(C));
	
	return OPERATOR_FINISHED;
}

void IMAGE_OT_view_all(wmOperatorType *ot)
{
	PropertyRNA *prop;

	/* identifiers */
	ot->name = "View All";
	ot->idname = "IMAGE_OT_view_all";
	ot->description = "View the entire image";
	
	/* api callbacks */
	ot->exec = image_view_all_exec;
	ot->poll = space_image_main_area_poll;

	/* properties */
	prop = RNA_def_boolean(ot->srna, "fit_view", 0, "Fit View", "Fit frame to the viewport");
	RNA_def_property_flag(prop, PROP_SKIP_SAVE);
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
	ED_space_image_get_size(sima, &width, &height);
	ED_space_image_get_aspect(sima, &aspx, &aspy);

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

static int image_view_zoom_in_invoke(bContext *C, wmOperator *op, const wmEvent *event)
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

static int image_view_zoom_out_invoke(bContext *C, wmOperator *op, const wmEvent *event)
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
	ot->description = "Set zoom ratio of the view";
	
	/* api callbacks */
	ot->exec = image_view_zoom_ratio_exec;
	ot->poll = space_image_main_area_poll;
	
	/* properties */
	RNA_def_float(ot->srna, "ratio", 0.0f, -FLT_MAX, FLT_MAX,
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

static void image_open_cancel(bContext *UNUSED(C), wmOperator *op)
{
	MEM_freeN(op->customdata);
	op->customdata = NULL;
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
	if ((entry) || (action & IMA_LAYER_OPEN_IMAGE)) {
		errno = 0;

		ima = BKE_image_load_exists(str);
		if (!ima) {
			if (op->customdata) MEM_freeN(op->customdata);
			BKE_reportf(op->reports, RPT_ERROR, "Can't read: \"%s\", %s",
			            str, errno ? strerror(errno) : TIP_("Unsupported image format"));
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

		ima->color_space = IMA_COL_RGB;
	} else if (action & IMA_LAYER_OPEN_LAYER) {
		if (sima)
			ima = sima->image;

		if (ima) {
			ImageLayer *iml;
			struct ImBuf *ibuf;
			int flag;

			iml = BKE_add_image_file_as_layer(ima, str);
			iml->background = IMA_LAYER_BG_IMAGE;
			strcpy(iml->file_path, str);
			flag = IB_rect | IB_multilayer | IB_metadata;
		
			/* read ibuf */
			ibuf = IMB_loadiffname(str, flag, ima->colorspace_settings.name);
			iml->ibufs.first = NULL;
			iml->ibufs.last = NULL;
			BLI_addtail(&iml->ibufs, ibuf);
			if(!iml)
				return OPERATOR_CANCELLED;
		}
	}

	/* XXX unpackImage frees image buffers */
	ED_preview_kill_jobs(C);
	if (!(action & IMA_LAYER_OPEN_LAYER))
		BKE_image_signal(ima, iuser, IMA_SIGNAL_RELOAD);
	WM_event_add_notifier(C, NC_IMAGE | NA_EDITED | ND_DRAW, ima);

	MEM_freeN(op->customdata);

	return OPERATOR_FINISHED;
}

static int image_open_invoke(bContext *C, wmOperator *op, const wmEvent *UNUSED(event))
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

	if (ima == NULL) {
		PointerRNA ptr;
		PropertyRNA *prop;

		/* hook into UI */
		uiIDContextProperty(C, &ptr, &prop);

		if (prop) {
			PointerRNA oldptr;
			Image *oldima;

			oldptr = RNA_property_pointer_get(&ptr, prop);
			oldima = (Image *)oldptr.id.data;
			/* unlikely to fail but better avoid strange crash */
			if (oldima && GS(oldima->id.name) == ID_IM) {
				ima = oldima;
			}
		}
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
	WM_operator_properties_filesel(ot, FOLDERFILE | IMAGEFILE | MOVIEFILE, FILE_SPECIAL, FILE_OPENFILE,
	                               WM_FILESEL_FILEPATH | WM_FILESEL_RELPATH, FILE_DEFAULTDISPLAY);
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
	ot->description = "Set image's user's length to the one of this video";
	ot->idname = "IMAGE_OT_match_movie_length";
	
	/* api callbacks */
	ot->exec = image_match_len_exec;

	/* flags */
	ot->flag = OPTYPE_REGISTER | OPTYPE_INTERNAL/* | OPTYPE_UNDO */; /* Don't think we need undo for that. */
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

	if (sima->image->source == IMA_SRC_GENERATED) {
		sima->image->source = IMA_SRC_FILE;
		BKE_image_signal(sima->image, &sima->iuser, IMA_SIGNAL_SRC_CHANGE);
	}
	
	if (BLI_testextensie_array(str, imb_ext_movie))
		sima->image->source = IMA_SRC_MOVIE;
	else
		sima->image->source = IMA_SRC_FILE;

	/* XXX unpackImage frees image buffers */
	ED_preview_kill_jobs(C);
	
	BKE_icon_changed(BKE_icon_getid(&sima->image->id));
	BKE_image_signal(sima->image, &sima->iuser, IMA_SIGNAL_RELOAD);
	WM_event_add_notifier(C, NC_IMAGE | NA_EDITED, sima->image);

	return OPERATOR_FINISHED;
}

static int image_replace_invoke(bContext *C, wmOperator *op, const wmEvent *UNUSED(event))
{
	SpaceImage *sima = CTX_wm_space_image(C);

	if (!sima->image)
		return OPERATOR_CANCELLED;

	if (RNA_struct_property_is_set(op->ptr, "filepath"))
		return image_replace_exec(C, op);

	if (!RNA_struct_property_is_set(op->ptr, "relative_path"))
		RNA_boolean_set(op->ptr, "relative_path", BLI_path_is_rel(sima->image->name));

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
	WM_operator_properties_filesel(ot, FOLDERFILE | IMAGEFILE | MOVIEFILE, FILE_SPECIAL, FILE_OPENFILE,
	                               WM_FILESEL_FILEPATH | WM_FILESEL_RELPATH, FILE_DEFAULTDISPLAY);
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

static int save_image_options_init(SaveImageOptions *simopts, SpaceImage *sima, Scene *scene,
                                   const bool guess_path, const bool save_as_render)
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
		else {
			if (ima->source == IMA_SRC_GENERATED) {
				simopts->im_format.imtype = R_IMF_IMTYPE_PNG;
			}
			else {
				BKE_imbuf_to_image_format(&simopts->im_format, ibuf);
				simopts->im_format.quality = ibuf->ftype & 0xff;
			}
			simopts->im_format.quality = ibuf->ftype & 0xff;
		}
		//simopts->subimtype = scene->r.subimtype; /* XXX - this is lame, we need to make these available too! */

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
			const bool is_prev_save = !STREQ(G.ima, "//");
			if (save_as_render) {
				if (is_prev_save) {
					BLI_strncpy(simopts->filepath, G.ima, sizeof(simopts->filepath));
				}
				else {
					BLI_strncpy(simopts->filepath, "//untitled", sizeof(simopts->filepath));
					BLI_path_abs(simopts->filepath, G.main->name);
				}
			}
			else {
				BLI_snprintf(simopts->filepath, sizeof(simopts->filepath), "//%s", ima->id.name + 2);
				BLI_path_abs(simopts->filepath, is_prev_save ? G.ima : G.main->name);
			}
		}

		/* color management */
		BKE_color_managed_display_settings_copy(&simopts->im_format.display_settings, &scene->display_settings);
		BKE_color_managed_view_settings_copy(&simopts->im_format.view_settings, &scene->view_settings);
	}

	ED_space_image_release_buffer(sima, ibuf, lock);

	return (ibuf != NULL);
}

static void save_image_options_from_op(SaveImageOptions *simopts, wmOperator *op)
{
	if (op->customdata) {
		BKE_color_managed_view_settings_free(&simopts->im_format.view_settings);

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
		BKE_color_managed_view_settings_free(&((ImageFormatData *)op->customdata)->view_settings);

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
		ImBuf *colormanaged_ibuf;
		const char *relbase = ID_BLEND_PATH(CTX_data_main(C), &ima->id);
		const short relative = (RNA_struct_find_property(op->ptr, "relative_path") && RNA_boolean_get(op->ptr, "relative_path"));
		const short save_copy = (RNA_struct_find_property(op->ptr, "copy") && RNA_boolean_get(op->ptr, "copy"));
		const bool save_as_render = (RNA_struct_find_property(op->ptr, "save_as_render") && RNA_boolean_get(op->ptr, "save_as_render"));
		ImageFormatData *imf = &simopts->im_format;
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

		colormanaged_ibuf = IMB_colormanagement_imbuf_for_write(ibuf, save_as_render, true, &imf->view_settings, &imf->display_settings, imf);

		if (simopts->im_format.imtype == R_IMF_IMTYPE_MULTILAYER) {
			Scene *scene = CTX_data_scene(C);
			RenderResult *rr = BKE_image_acquire_renderresult(scene, ima);
			if (rr) {
				RE_WriteRenderResult(op->reports, rr, simopts->filepath, simopts->im_format.exr_codec);
				ok = TRUE;
			}
			else {
				BKE_report(op->reports, RPT_ERROR, "Did not write, no Multilayer Image");
			}
			BKE_image_release_renderresult(scene, ima);
		}
		else {
			if (BKE_imbuf_write_as(colormanaged_ibuf, simopts->filepath, &simopts->im_format, save_copy)) {
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

				IMB_colormanagment_colorspace_from_ibuf_ftype(&ima->colorspace_settings, ibuf);
			}
		}
		else {
			BKE_reportf(op->reports, RPT_ERROR, "Could not write image %s", simopts->filepath);
		}


		WM_event_add_notifier(C, NC_IMAGE | NA_EDITED, sima->image);

		WM_cursor_wait(0);

		if (colormanaged_ibuf != ibuf)
			IMB_freeImBuf(colormanaged_ibuf);
	}

	ED_space_image_release_buffer(sima, ibuf, lock);
}

static void image_save_as_free(wmOperator *op)
{
	if (op->customdata) {
		ImageFormatData *im_format = (ImageFormatData *)op->customdata;
		BKE_color_managed_view_settings_free(&im_format->view_settings);

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
	save_image_options_init(&simopts, sima, CTX_data_scene(C), false, false);

	save_image_options_from_op(&simopts, op);

	save_image_doit(C, sima, op, &simopts, TRUE);

	image_save_as_free(op);
	return OPERATOR_FINISHED;
}


static bool image_save_as_check(bContext *UNUSED(C), wmOperator *op)
{
	ImageFormatData *imf = op->customdata;
	return WM_operator_filesel_ensure_ext_imtype(op, imf);
}

static int image_save_as_invoke(bContext *C, wmOperator *op, const wmEvent *UNUSED(event))
{
	SpaceImage *sima = CTX_wm_space_image(C);
	Image *ima = ED_space_image(sima);
	Scene *scene = CTX_data_scene(C);
	SaveImageOptions simopts;
	const bool save_as_render = ((ima->source == IMA_SRC_VIEWER) || (ima->flag & IMA_VIEW_AS_RENDER));

	if (RNA_struct_property_is_set(op->ptr, "filepath"))
		return image_save_as_exec(C, op);

	save_image_options_defaults(&simopts);

	if (save_image_options_init(&simopts, sima, scene, true, save_as_render) == 0)
		return OPERATOR_CANCELLED;
	save_image_options_to_op(&simopts, op);

	/* enable save_copy by default for render results */
	if (ELEM(ima->type, IMA_TYPE_R_RESULT, IMA_TYPE_COMPOSITE) && !RNA_struct_property_is_set(op->ptr, "copy")) {
		RNA_boolean_set(op->ptr, "copy", TRUE);
	}

	RNA_boolean_set(op->ptr, "save_as_render", save_as_render);

	op->customdata = MEM_mallocN(sizeof(simopts.im_format), __func__);
	memcpy(op->customdata, &simopts.im_format, sizeof(simopts.im_format));

	image_filesel(C, op, simopts.filepath);

	return OPERATOR_RUNNING_MODAL;
}

static void image_save_as_cancel(bContext *UNUSED(C), wmOperator *op)
{
	image_save_as_free(op);
}

static bool image_save_as_draw_check_prop(PointerRNA *ptr, PropertyRNA *prop)
{
	const char *prop_id = RNA_property_identifier(prop);

	return !(STREQ(prop_id, "filepath") ||
	         STREQ(prop_id, "directory") ||
	         STREQ(prop_id, "filename") ||
	         /* when saving a copy, relative path has no effect */
	         ((STREQ(prop_id, "relative_path")) && RNA_boolean_get(ptr, "copy"))
	         );
}

static void image_save_as_draw(bContext *UNUSED(C), wmOperator *op)
{
	uiLayout *layout = op->layout;
	ImageFormatData *imf = op->customdata;
	PointerRNA ptr;

	/* image template */
	RNA_pointer_create(NULL, &RNA_ImageFormatSettings, imf, &ptr);
	uiTemplateImageSettings(layout, &ptr, FALSE);

	/* main draw call */
	RNA_pointer_create(NULL, op->type->srna, op->properties, &ptr);
	uiDefAutoButsRNA(layout, &ptr, image_save_as_draw_check_prop, '\0');
}

static int image_save_as_poll(bContext *C)
{
	if (space_image_buffer_exists_poll(C)) {
		if (G.is_rendering) {
			/* no need to NULL check here */
			SpaceImage *sima = CTX_wm_space_image(C);
			Image *ima = ED_space_image(sima);

			if (ima->source == IMA_SRC_VIEWER) {
				CTX_wm_operator_poll_msg_set(C, "can't save image while rendering");
				return FALSE;
			}
		}
		return TRUE;
	}
	return FALSE;
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
	ot->poll = image_save_as_poll;

	/* flags */
	ot->flag = OPTYPE_REGISTER | OPTYPE_UNDO;

	/* properties */
	RNA_def_boolean(ot->srna, "save_as_render", 0, "Save As Render", "Apply render part of display transform when saving byte image");
	RNA_def_boolean(ot->srna, "copy", 0, "Copy", "Create a new image file without modifying the current image in blender");

	WM_operator_properties_filesel(ot, FOLDERFILE | IMAGEFILE | MOVIEFILE, FILE_SPECIAL, FILE_SAVE,
	                               WM_FILESEL_FILEPATH | WM_FILESEL_RELPATH, FILE_DEFAULTDISPLAY);
}

/******************** save image operator ********************/

static int image_save_exec(bContext *C, wmOperator *op)
{
	SpaceImage *sima = CTX_wm_space_image(C);
	Scene *scene = CTX_data_scene(C);
	SaveImageOptions simopts;

	save_image_options_defaults(&simopts);
	if (save_image_options_init(&simopts, sima, scene, false, false) == 0)
		return OPERATOR_CANCELLED;
	save_image_options_from_op(&simopts, op);

	if (BLI_exists(simopts.filepath) && BLI_file_is_writable(simopts.filepath)) {
		save_image_doit(C, sima, op, &simopts, FALSE);
	}
	else {
		BKE_reportf(op->reports, RPT_ERROR, "Cannot save image, path '%s' is not writable", simopts.filepath);
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
	char di[FILE_MAX];
	
	if (sima->image == NULL)
		return OPERATOR_CANCELLED;

	if (sima->image->source != IMA_SRC_SEQUENCE) {
		BKE_report(op->reports, RPT_ERROR, "Can only save sequence on image sequences");
		return OPERATOR_CANCELLED;
	}

	if (sima->image->type == IMA_TYPE_MULTILAYER) {
		BKE_report(op->reports, RPT_ERROR, "Cannot save multilayer sequences");
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

	BLI_split_dir_part(ibuf->name, di, sizeof(di));
	BKE_reportf(op->reports, RPT_INFO, "%d image(s) will be saved in %s", tot, di);

	for (ibuf = sima->image->ibufs.first; ibuf; ibuf = ibuf->next) {
		if (ibuf->userflags & IB_BITMAPDIRTY) {
			char name[FILE_MAX];
			BLI_strncpy(name, ibuf->name, sizeof(name));
			
			BLI_path_abs(name, bmain->name);

			if (0 == IMB_saveiff(ibuf, name, IB_rect | IB_zbuf | IB_zbuffloat)) {
				BKE_reportf(op->reports, RPT_ERROR, "Could not write image %s", name);
				break;
			}

			BKE_reportf(op->reports, RPT_INFO, "Saved %s", ibuf->name);
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
#define IMA_DEF_NAME N_("Untitled")

static int image_new_exec(bContext *C, wmOperator *op)
{
	SpaceImage *sima;
	Scene *scene;
	Object *obedit;
	Image *ima;
	Main *bmain;
	PointerRNA ptr, idptr;
	PropertyRNA *prop;
	char _name[MAX_ID_NAME - 2];
	char *name = _name;
	float color[4];
	int width, height, floatbuf, gen_type, alpha;
	short background, color_space;

	/* retrieve state */
	sima = CTX_wm_space_image(C);
	scene = CTX_data_scene(C);
	obedit = CTX_data_edit_object(C);
	bmain = CTX_data_main(C);

	prop = RNA_struct_find_property(op->ptr, "name");
	RNA_property_string_get(op->ptr, prop, name);
	if (!RNA_property_is_set(op->ptr, prop)) {
		/* Default value, we can translate! */
		name = (char *)DATA_(name);
	}
	width = RNA_int_get(op->ptr, "width");
	height = RNA_int_get(op->ptr, "height");
	floatbuf = RNA_boolean_get(op->ptr, "float");
	gen_type = RNA_enum_get(op->ptr, "generated_type");
	RNA_float_get_array(op->ptr, "color", color);
	alpha = RNA_boolean_get(op->ptr, "alpha");
	background = RNA_enum_get(op->ptr, "background");
	color_space = RNA_enum_get(op->ptr, "color_space");

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

	if (!alpha)
		color[3] = 1.0f;

	ima = BKE_image_add_generated(bmain, width, height, name, alpha ? 32 : 24, floatbuf, gen_type, color);

	if (color_space & IMA_COL_RGB)
		ima->color_space = IMA_COL_RGB;
	else
		ima->color_space = IMA_COL_GRAY;

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
	else if (sima) {
		ED_space_image_set(sima, scene, obedit, ima);
	}
	else {
		Tex *tex = CTX_data_pointer_get_type(C, "texture", &RNA_Texture).data;
		if (tex && tex->type == TEX_IMAGE) {
			if (tex->ima)
				id_us_min(&tex->ima->id);
			tex->ima = ima;
			ED_area_tag_redraw(CTX_wm_area(C));
		}
	}

	BKE_image_signal(ima, (sima) ? &sima->iuser : NULL, IMA_SIGNAL_USER_NEW_IMAGE);

	WM_event_add_notifier(C, NC_IMAGE | NA_ADDED, ima);
	
	return OPERATOR_FINISHED;
}

/* XXX, Ton is not a fan of OK buttons but using this function to avoid undo/redo bug while in mesh-editmode, - campbell */
/* XXX Note: the WM_operator_props_dialog_popup() doesn't work for uiIDContextProperty(), image is not being that way */
static int image_new_invoke(bContext *C, wmOperator *op, const wmEvent *UNUSED(event))
{
	/* Better for user feedback. */
	RNA_string_set(op->ptr, "name", DATA_(IMA_DEF_NAME));
	return WM_operator_props_dialog_popup(C, op, 15 * UI_UNIT_X, 5 * UI_UNIT_Y);
}

static int image_op_layer_invoke(bContext *C, wmOperator *op, const wmEvent *UNUSED(event))
{
	return WM_operator_props_dialog_popup(C, op, 250, 100);;
}

void IMAGE_OT_new(wmOperatorType *ot)
{
	PropertyRNA *prop;
	
	static float default_color[4]= {0.0f, 0.0f, 0.0f, 1.0f};

	static EnumPropertyItem prop_background_items[] = {
		{IMA_LAYER_BG_RGB, "RGB", 0, "RGB", ""},
		{IMA_LAYER_BG_WHITE, "WHITE", 0, "White", ""},
		{IMA_LAYER_BG_ALPHA, "ALPHA", 0, "Transparent", ""},
		{0, NULL, 0, NULL, NULL}};

	static EnumPropertyItem prop_color_space_items[] = {
		{IMA_COL_RGB, "RGB", 0, "RGB", ""},
		{IMA_COL_GRAY, "GRAY", 0, "GrayScale", ""},
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
	RNA_def_string(ot->srna, "name", IMA_DEF_NAME, MAX_ID_NAME - 2, "Name", "Image datablock name");
	prop = RNA_def_int(ot->srna, "width", 1024, 1, INT_MAX, "Width", "Image width", 1, 16384);
	RNA_def_property_subtype(prop, PROP_PIXEL);
	prop = RNA_def_int(ot->srna, "height", 1024, 1, INT_MAX, "Height", "Image height", 1, 16384);
	RNA_def_property_subtype(prop, PROP_PIXEL);
	RNA_def_enum(ot->srna, "background", prop_background_items, 0, "Background", "");

	prop= RNA_def_float_color(ot->srna, "color", 4, NULL, 0.0f, FLT_MAX, "Color", "Default fill color", 0.0f, 1.0f);
	RNA_def_property_subtype(prop, PROP_COLOR_GAMMA);
	RNA_def_property_float_array_default(prop, default_color);
	RNA_def_boolean(ot->srna, "alpha", 1, "Alpha", "Create an image with an alpha channel");
	RNA_def_enum(ot->srna, "color_space", prop_color_space_items, 0, "Color Space", "");
	RNA_def_enum(ot->srna, "generated_type", image_generated_type_items, IMA_GENTYPE_BLANK,
		             "Generated Type", "Fill the image with a grid for UV map testing");
	RNA_def_boolean(ot->srna, "float", 0, "32 bit Float", "Create image with 32 bit floating point bit depth");	
}

#undef IMA_DEF_NAME

/************************* New Color Operators ************************/

static int image_operator_poll(bContext *C)
{
	Image *ima = CTX_data_edit_image(C);

	return BKE_image_has_ibuf(ima, NULL, IMA_IBUF_IMA);
}

static void image_preview_cancel(bContext *C, wmOperator *UNUSED(op))
{
	Image *ima = CTX_data_edit_image(C);

	if (ima) {
		if (ima->preview_ibuf) {
			IMB_freeImBuf(ima->preview_ibuf);
			ima->preview_ibuf = NULL;
			WM_event_add_notifier(C, NC_IMAGE | ND_DRAW, NULL);
		}
	}
}

static int image_invert_exec(bContext *C, wmOperator *op)
{
	SpaceImage *sima = CTX_wm_space_image(C);
	Image *ima = CTX_data_edit_image(C);
	ImBuf *ibuf;
	short r, g, b, a;

	/* undo is supported only on image paint mode currently */
	bool support_undo = ((sima != NULL) && (sima->mode == SI_MODE_PAINT));
	
	if (!ima)
		return OPERATOR_CANCELLED;

	ima->use_layers = FALSE;
	if (sima->mode == SI_MODE_PAINT)
		ibuf = BKE_image_acquire_ibuf(ima, NULL, NULL, IMA_IBUF_LAYER);
	else 
		ibuf = BKE_image_acquire_ibuf(ima, NULL, NULL, IMA_IBUF_IMA);
	ima->use_layers = TRUE;

	/* flags indicate if this channel should be inverted */
	r = RNA_boolean_get(op->ptr, "invert_r");
	g = RNA_boolean_get(op->ptr, "invert_g");
	b = RNA_boolean_get(op->ptr, "invert_b");
	a = RNA_boolean_get(op->ptr, "invert_a");

	if (ibuf == NULL)  /* TODO: this should actually never happen, but does for render-results -> cleanup */
		return OPERATOR_CANCELLED;

	if (support_undo) {
		ED_undo_paint_push_begin(UNDO_PAINT_IMAGE, op->type->name,
		                         ED_image_undo_restore, ED_image_undo_free);
		/* not strictly needed, because we only imapaint_dirty_region to invalidate all tiles
		 * but better do this right in case someone copies this for a tool that uses partial redraw better */
		ED_imapaint_clear_partial_redraw();
		ED_imapaint_dirty_region(ima, ibuf, 0, 0, ibuf->x, ibuf->y);
	}

	IMB_invert_channels(ibuf, r, g, b, a);
	if (sima->mode != SI_MODE_PAINT) {
		ImageLayer *layer;
		ImBuf *ibuf_l;
	
		for (layer = ima->imlayers.first; layer; layer = layer->next) {
			ibuf_l = (ImBuf *)layer->ibufs.first;
			IMB_invert_channels(ibuf_l, r, g, b, a);
		}
	}

	ibuf->userflags |= IB_BITMAPDIRTY | IB_DISPLAY_BUFFER_INVALID;

	if (ibuf->mipmap[0])
		ibuf->userflags |= IB_MIPMAP_INVALID;

	if (support_undo)
		ED_undo_paint_push_end(UNDO_PAINT_IMAGE);

	/* force GPU reupload, all image is invalid */
	GPU_free_image(ima);

	WM_event_add_notifier(C, NC_IMAGE | NA_EDITED, ima);

	BKE_image_release_ibuf(ima, ibuf, NULL);

	return OPERATOR_FINISHED;
}

void IMAGE_OT_invert(wmOperatorType *ot)
{
	PropertyRNA *prop;

	/* identifiers */
	ot->name = "Invert Channels";
	ot->idname = "IMAGE_OT_invert";
	ot->description = "Invert image's channels";
	
	/* api callbacks */
	ot->exec = image_invert_exec;
	ot->poll = image_operator_poll;
	
	/* properties */
	prop = RNA_def_boolean(ot->srna, "invert_r", 0, "Red", "Invert Red Channel");
	RNA_def_property_flag(prop, PROP_SKIP_SAVE);
	prop = RNA_def_boolean(ot->srna, "invert_g", 0, "Green", "Invert Green Channel");
	RNA_def_property_flag(prop, PROP_SKIP_SAVE);
	prop = RNA_def_boolean(ot->srna, "invert_b", 0, "Blue", "Invert Blue Channel");
	RNA_def_property_flag(prop, PROP_SKIP_SAVE);
	prop = RNA_def_boolean(ot->srna, "invert_a", 0, "Alpha", "Invert Alpha Channel");
	RNA_def_property_flag(prop, PROP_SKIP_SAVE);
	
	/* flags */
	ot->flag = OPTYPE_REGISTER | OPTYPE_UNDO;
}

static int image_invert_value_exec(bContext *C, wmOperator *UNUSED(op))
{
	SpaceImage *sima = CTX_wm_space_image(C);
	Image *ima = CTX_data_edit_image(C);
	ImBuf *ibuf;
	
	if (!ima)
		return OPERATOR_CANCELLED;

	ima->use_layers = FALSE;
	if (sima->mode == SI_MODE_PAINT)
		ibuf = BKE_image_acquire_ibuf(ima, NULL, NULL, IMA_IBUF_LAYER);
	else 
		ibuf = BKE_image_acquire_ibuf(ima, NULL, NULL, IMA_IBUF_IMA);
	ima->use_layers = TRUE;

	if (ibuf == NULL)  /* TODO: this should actually never happen, but does for render-results -> cleanup */
		return OPERATOR_CANCELLED;

	IMB_invert_value(ibuf);
	if (sima->mode != SI_MODE_PAINT) {
		ImageLayer *layer;
		ImBuf *ibuf_l;

		for (layer = ima->imlayers.first; layer; layer = layer->next) {
			ibuf_l = (ImBuf *)layer->ibufs.first;
			IMB_invert_value(ibuf_l);
		}
	}

	ibuf->userflags |= IB_BITMAPDIRTY;

	WM_event_add_notifier(C, NC_IMAGE | NA_EDITED, ima);

	BKE_image_release_ibuf(ima, ibuf, NULL);

	return OPERATOR_FINISHED;
}

void IMAGE_OT_invert_value(wmOperatorType *ot)
{
	/* identifiers */
	ot->name = "Invert Value";
	ot->idname = "IMAGE_OT_invert_value";
	ot->description = "Invert the brightness of each pixel";
	
	/* api callbacks */
	ot->exec = image_invert_value_exec;
	ot->poll = image_operator_poll;
	
	/* flags */
	ot->flag = OPTYPE_REGISTER | OPTYPE_UNDO;
}

static int image_bright_contrast_exec(bContext *C, wmOperator *op)
{
	SpaceImage *sima = CTX_wm_space_image(C);
	Image *ima = CTX_data_edit_image(C);
	ImBuf *ibuf;
	float bright, contrast;

	if (!ima)
		return OPERATOR_CANCELLED;

	ima->use_layers = FALSE;
	if (sima->mode == SI_MODE_PAINT)
		ibuf = BKE_image_acquire_ibuf(ima, NULL, NULL, IMA_IBUF_LAYER);
	else 
		ibuf = BKE_image_acquire_ibuf(ima, NULL, NULL, IMA_IBUF_IMA);
	ima->use_layers = TRUE;

	if (ima->preview_ibuf) {
		IMB_freeImBuf(ima->preview_ibuf);
		ima->preview_ibuf = NULL;
	}

	bright = RNA_float_get(op->ptr, "bright");
	contrast = RNA_float_get(op->ptr, "contrast");

	if ((bright == 0.0f) && (contrast == 0.0f)) {
		BKE_image_release_ibuf(ima, ibuf, NULL);
		return OPERATOR_CANCELLED;
	}

	IMB_bright_contrast(ibuf, bright, contrast);
	if (sima->mode != SI_MODE_PAINT) {
		ImageLayer *layer;
		ImBuf *ibuf_l;

		for (layer = ima->imlayers.first; layer; layer = layer->next) {
			ibuf_l = (ImBuf *)layer->ibufs.first;
			IMB_bright_contrast(ibuf_l, bright, contrast);
		}
	}

	ibuf->userflags |= IB_BITMAPDIRTY;

	WM_event_add_notifier(C, NC_IMAGE | ND_DRAW, ima);

	BKE_image_release_ibuf(ima, ibuf, NULL);

	return OPERATOR_FINISHED;
}

static bool image_bright_contrast_check(bContext *C, wmOperator *op)
{
	Image *ima = CTX_data_edit_image(C);
	ImBuf *ibuf;
	float bright, contrast;

	if (!ima)
		return FALSE;

	ima->use_layers = FALSE;
	ibuf = BKE_image_acquire_ibuf(ima, NULL, NULL, IMA_IBUF_IMA);
	ima->use_layers = TRUE;

	bright = RNA_float_get(op->ptr, "bright");
	contrast = RNA_float_get(op->ptr, "contrast");

	if (ima->preview_ibuf) {
		IMB_freeImBuf(ima->preview_ibuf);
		ima->preview_ibuf = NULL;
	}

	ima->preview_ibuf = IMB_dupImBuf(ibuf);

	IMB_bright_contrast(ima->preview_ibuf, bright, contrast);

	BKE_image_release_ibuf(ima, ibuf, NULL);
	return TRUE;
}

void IMAGE_OT_bright_contrast(wmOperatorType *ot)
{
	/* identifiers */
	ot->name = "Bright Contrast";
	ot->idname = "IMAGE_OT_bright_contrast";
	ot->description = "Adjust brightness and contrast";
 
	/* api callbacks */
	ot->exec = image_bright_contrast_exec;
	ot->poll = image_operator_poll;
	ot->invoke = image_op_layer_invoke;
	ot->check = image_bright_contrast_check;
	ot->cancel = image_preview_cancel;
 
	/* flags */
	ot->flag = OPTYPE_REGISTER | OPTYPE_UNDO;

	RNA_def_float(ot->srna, "bright", 0.0f, -FLT_MAX, FLT_MAX, "Bright", "Bright", -100, 100);
	RNA_def_float(ot->srna, "contrast", 0.0f, -FLT_MAX, FLT_MAX, "Contrast", "Contrast", -100, 100);
}

static int image_desaturate_exec(bContext *C, wmOperator *op)
{
	SpaceImage *sima = CTX_wm_space_image(C);
	Image *ima = CTX_data_edit_image(C);
	ImBuf *ibuf;
	int type;
 
	if (!ima)
		return OPERATOR_CANCELLED;

	type = RNA_enum_get(op->ptr, "type");

	ima->use_layers = FALSE;
	if (sima->mode == SI_MODE_PAINT)
		ibuf = BKE_image_acquire_ibuf(ima, NULL, NULL, IMA_IBUF_LAYER);
	else 
		ibuf = BKE_image_acquire_ibuf(ima, NULL, NULL, IMA_IBUF_IMA);
	ima->use_layers = TRUE;

	IMB_desaturate(ibuf, type);
	if (sima->mode != SI_MODE_PAINT) {
		ImageLayer *layer;
		ImBuf *ibuf_l;

		for (layer = ima->imlayers.first; layer; layer = layer->next) {
			ibuf_l = (ImBuf *)layer->ibufs.first;
			IMB_desaturate(ibuf_l, type);
		}
	}

	ibuf->userflags |= IB_BITMAPDIRTY;

	WM_event_add_notifier(C, NC_IMAGE | ND_DRAW, NULL);

	BKE_image_release_ibuf(ima, ibuf, NULL);
 
	return OPERATOR_FINISHED;
}

void IMAGE_OT_desaturate(wmOperatorType *ot)
{
	static EnumPropertyItem slot_des[] = {
		{1, "LIGHT", 0, "Lightness", ""},
		{2, "LUM", 0, "Luminosity", ""},
		{3, "AVG", 0, "Average", ""},
		{0, NULL, 0, NULL, NULL}
	};
 
	/* identifiers */
	ot->name = "Desaturate Image";
	ot->idname = "IMAGE_OT_desaturate";
	ot->description = "Desaturate the Image";
 
	/* api callbacks */
	ot->exec = image_desaturate_exec;
	ot->poll = image_operator_poll;
 
	/* flags */
	ot->flag = OPTYPE_REGISTER | OPTYPE_UNDO;
 
	/* properties */
	RNA_def_enum(ot->srna, "type", slot_des, 0, "Type", "");
}

static int image_posterize_exec(bContext *C, wmOperator *op)
{
	SpaceImage *sima = CTX_wm_space_image(C);
	Image *ima = CTX_data_edit_image(C);
	ImBuf *ibuf;
	int levels;
 
	if (!ima)
		return OPERATOR_CANCELLED;

	levels = RNA_int_get(op->ptr, "levels");

	ima->use_layers = FALSE;
	if (sima->mode == SI_MODE_PAINT)
		ibuf = BKE_image_acquire_ibuf(ima, NULL, NULL, IMA_IBUF_LAYER);
	else 
		ibuf = BKE_image_acquire_ibuf(ima, NULL, NULL, IMA_IBUF_IMA);
	ima->use_layers = TRUE;

	if (ima->preview_ibuf) {
		IMB_freeImBuf(ima->preview_ibuf);
		ima->preview_ibuf = NULL;
	}

	IMB_posterize(ibuf, levels);
	if (sima->mode != SI_MODE_PAINT) {
		ImageLayer *layer;
		ImBuf *ibuf_l;

		for (layer = ima->imlayers.first; layer; layer = layer->next) {
			ibuf_l = (ImBuf *)layer->ibufs.first;
			IMB_posterize(ibuf_l, levels);
		}
	}

	ibuf->userflags |= IB_BITMAPDIRTY;

	WM_event_add_notifier(C, NC_IMAGE | ND_DRAW, NULL);

	BKE_image_release_ibuf(ima, ibuf, NULL);
 
	return OPERATOR_FINISHED;
}

static bool image_posterize_check(bContext *C, wmOperator *op)
{
	Image *ima = CTX_data_edit_image(C);
	ImBuf *ibuf;
	int levels;

	if (!ima)
		return FALSE;

	ima->use_layers = FALSE;
	ibuf = BKE_image_acquire_ibuf(ima, NULL, NULL, IMA_IBUF_IMA);
	ima->use_layers = TRUE;

	levels = RNA_int_get(op->ptr, "levels");

	if (ima->preview_ibuf) {
		IMB_freeImBuf(ima->preview_ibuf);
		ima->preview_ibuf = NULL;
	}

	ima->preview_ibuf = IMB_dupImBuf(ibuf);

	IMB_posterize(ima->preview_ibuf, levels);

	BKE_image_release_ibuf(ima, ibuf, NULL);
	return TRUE;
}

void IMAGE_OT_posterize(wmOperatorType *ot)
{
	/* identifiers */
	ot->name = "Posterize Image";
	ot->idname = "IMAGE_OT_posterize";
	ot->description = "Posterize the Image";
 
	/* api callbacks */
	ot->exec = image_posterize_exec;
	ot->poll = image_operator_poll;
	ot->invoke = image_op_layer_invoke;
	ot->check = image_posterize_check;
	ot->cancel = image_preview_cancel;
 
	/* flags */
	ot->flag = OPTYPE_REGISTER | OPTYPE_UNDO;
 
	/* properties */
	RNA_def_int(ot->srna, "levels", 256, 2, 256, "Levels", "Posterize Levels", 2, 256);
}

static int image_threshold_exec(bContext *C, wmOperator *op)
{
	SpaceImage *sima = CTX_wm_space_image(C);
	Image *ima = CTX_data_edit_image(C);
	ImBuf *ibuf;
	int low, high;
 
	if (!ima)
		return OPERATOR_CANCELLED;

	low = RNA_int_get(op->ptr, "low");
	high = RNA_int_get(op->ptr, "high");

	ima->use_layers = FALSE;
	if (sima->mode == SI_MODE_PAINT)
		ibuf = BKE_image_acquire_ibuf(ima, NULL, NULL, IMA_IBUF_LAYER);
	else 
		ibuf = BKE_image_acquire_ibuf(ima, NULL, NULL, IMA_IBUF_IMA);
	ima->use_layers = TRUE;

	if (ima->preview_ibuf) {
		IMB_freeImBuf(ima->preview_ibuf);
		ima->preview_ibuf = NULL;
	}

	IMB_threshold(ibuf, low, high);
	if (sima->mode != SI_MODE_PAINT) {
		ImageLayer *layer;
		ImBuf *ibuf_l;

		for (layer = ima->imlayers.first; layer; layer = layer->next) {
			ibuf_l = (ImBuf *)layer->ibufs.first;
			IMB_threshold(ibuf_l, low, high);
		}
	}

	ibuf->userflags |= IB_BITMAPDIRTY;

	WM_event_add_notifier(C, NC_IMAGE | ND_DRAW, NULL);

	BKE_image_release_ibuf(ima, ibuf, NULL);
 
	return OPERATOR_FINISHED;
}

static bool image_threshold_check(bContext *C, wmOperator *op)
{
	Image *ima = CTX_data_edit_image(C);
	ImBuf *ibuf;
	int low, high;

	if (!ima)
		return FALSE;

	ima->use_layers = FALSE;
	ibuf = BKE_image_acquire_ibuf(ima, NULL, NULL, IMA_IBUF_IMA);
	ima->use_layers = TRUE;

	low = RNA_int_get(op->ptr, "low");
	high = RNA_int_get(op->ptr, "high");

	if (ima->preview_ibuf) {
		IMB_freeImBuf(ima->preview_ibuf);
		ima->preview_ibuf = NULL;
	}

	ima->preview_ibuf = IMB_dupImBuf(ibuf);

	IMB_threshold(ima->preview_ibuf, low, high);

	BKE_image_release_ibuf(ima, ibuf, NULL);
	return TRUE;
}

void IMAGE_OT_threshold(wmOperatorType *ot)
{
/* identifiers */
	ot->name = "Threshold Image";
	ot->idname = "IMAGE_OT_threshold";
	ot->description = "Reduce Image to two colours using a threshold";
 
	/* api callbacks */
	ot->exec = image_threshold_exec;
	ot->poll = image_operator_poll;
	ot->invoke = image_op_layer_invoke;
	ot->check = image_threshold_check;
	ot->cancel = image_preview_cancel;
 
	/* flags */
	ot->flag = OPTYPE_REGISTER | OPTYPE_UNDO;
 
	/* properties */
	RNA_def_int(ot->srna, "low", 127, 0, 255, "Low", "Threshold", 0, 255);
	RNA_def_int(ot->srna, "high", 255, 0, 255, "High", "Threshold", 0, 255);
}

static int image_exposure_exec(bContext *C, wmOperator *op)
{
	SpaceImage *sima = CTX_wm_space_image(C);
	Image *ima = CTX_data_edit_image(C);
	ImBuf *ibuf;
	float exposure, offset, gamma;

	if (!ima)
		return OPERATOR_CANCELLED;

	ima->use_layers = FALSE;
	if (sima->mode == SI_MODE_PAINT)
		ibuf = BKE_image_acquire_ibuf(ima, NULL, NULL, IMA_IBUF_LAYER);
	else 
		ibuf = BKE_image_acquire_ibuf(ima, NULL, NULL, IMA_IBUF_IMA);
	ima->use_layers = TRUE;

	if (ima->preview_ibuf) {
		IMB_freeImBuf(ima->preview_ibuf);
		ima->preview_ibuf = NULL;
	}

	exposure = RNA_float_get(op->ptr, "exposure");
	offset = RNA_float_get(op->ptr, "offset");
	gamma = RNA_float_get(op->ptr, "gamma");

	if ((exposure == 0.0f) && (offset == 0.0f) && (gamma == 1.0f)) {
		BKE_image_release_ibuf(ima, ibuf, NULL);
		return OPERATOR_CANCELLED;
	}

	IMB_exposure(ibuf, exposure, offset, gamma);
	if (sima->mode != SI_MODE_PAINT) {
		ImageLayer *layer;
		ImBuf *ibuf_l;

		for (layer = ima->imlayers.first; layer; layer = layer->next) {
			ibuf_l = (ImBuf *)layer->ibufs.first;
			IMB_exposure(ibuf_l, exposure, offset, gamma);
		}
	}

	ibuf->userflags |= IB_BITMAPDIRTY;

	WM_event_add_notifier(C, NC_IMAGE | ND_DRAW, ima);

	BKE_image_release_ibuf(ima, ibuf, NULL);

	return OPERATOR_FINISHED;
}

static bool image_exposure_check(bContext *C, wmOperator *op)
{
	Image *ima = CTX_data_edit_image(C);
	ImBuf *ibuf;
	float exposure, offset, gamma;

	if (!ima)
		return FALSE;

	ima->use_layers = FALSE;
	ibuf = BKE_image_acquire_ibuf(ima, NULL, NULL, IMA_IBUF_IMA);
	ima->use_layers = TRUE;

	exposure = RNA_float_get(op->ptr, "exposure");
	offset = RNA_float_get(op->ptr, "offset");
	gamma = RNA_float_get(op->ptr, "gamma");

	if (ima->preview_ibuf) {
		IMB_freeImBuf(ima->preview_ibuf);
		ima->preview_ibuf = NULL;
	}

	ima->preview_ibuf = IMB_dupImBuf(ibuf);

	IMB_exposure(ima->preview_ibuf, exposure, offset, gamma);

	BKE_image_release_ibuf(ima, ibuf, NULL);
	return TRUE;
}

void IMAGE_OT_exposure(wmOperatorType *ot)
{
	/* identifiers */
	ot->name = "Exposure";
	ot->idname = "IMAGE_OT_exposure";
	ot->description = "Exposure the Image";
 
	/* api callbacks */
	ot->exec = image_exposure_exec;
	ot->poll = image_operator_poll;
	ot->invoke = image_op_layer_invoke;
	ot->check = image_exposure_check;
	ot->cancel = image_preview_cancel;
 
	/* flags */
	ot->flag = OPTYPE_REGISTER | OPTYPE_UNDO;

	RNA_def_float(ot->srna, "exposure", 0.0f, -20.0f, 20.0f, "Exposure", "Exposure", -20.0f, 20.0f);
	RNA_def_float(ot->srna, "offset", 0.0f, -0.5f, 0.5f, "Offset", "Offset", -0.5f, 0.5f);
	RNA_def_float(ot->srna, "gamma", 1.0f, 0.01f, 9.99f, "Gamma", "Gamma", 0.01f, 9.99f);
}

static int image_colorize_exec(bContext *C, wmOperator *op)
{
	SpaceImage *sima = CTX_wm_space_image(C);
	Image *ima = CTX_data_edit_image(C);
	ImBuf *ibuf;
	int hue, saturation, lightness;
 
	if (!ima)
		return OPERATOR_CANCELLED;

	hue = RNA_int_get(op->ptr, "hue");
	saturation = RNA_int_get(op->ptr, "saturation");
	lightness = RNA_int_get(op->ptr, "lightness");

	ima->use_layers = FALSE;
	if (sima->mode == SI_MODE_PAINT)
		ibuf = BKE_image_acquire_ibuf(ima, NULL, NULL, IMA_IBUF_LAYER);
	else 
		ibuf = BKE_image_acquire_ibuf(ima, NULL, NULL, IMA_IBUF_IMA);
	ima->use_layers = TRUE;

	if (ima->preview_ibuf) {
		IMB_freeImBuf(ima->preview_ibuf);
		ima->preview_ibuf = NULL;
	}

	IMB_colorize(ibuf, hue, saturation, lightness);
	if (sima->mode != SI_MODE_PAINT) {
		ImageLayer *layer;
		ImBuf *ibuf_l;

		for (layer = ima->imlayers.first; layer; layer = layer->next) {
			ibuf_l = (ImBuf *)layer->ibufs.first;
			IMB_colorize(ibuf_l, hue, saturation, lightness);
		}
	}

	ibuf->userflags |= IB_BITMAPDIRTY;

	WM_event_add_notifier(C, NC_IMAGE | ND_DRAW, NULL);

	BKE_image_release_ibuf(ima, ibuf, NULL);
 
	return OPERATOR_FINISHED;
}

static bool image_colorize_check(bContext *C, wmOperator *op)
{
	Image *ima = CTX_data_edit_image(C);
	ImBuf *ibuf;
	int hue, saturation, lightness;

	if (!ima)
		return FALSE;

	ima->use_layers = FALSE;
	ibuf = BKE_image_acquire_ibuf(ima, NULL, NULL, IMA_IBUF_IMA);
	ima->use_layers = TRUE;

	hue = RNA_int_get(op->ptr, "hue");
	saturation = RNA_int_get(op->ptr, "saturation");
	lightness = RNA_int_get(op->ptr, "lightness");

	if (ima->preview_ibuf) {
		IMB_freeImBuf(ima->preview_ibuf);
		ima->preview_ibuf = NULL;
	}

	ima->preview_ibuf = IMB_dupImBuf(ibuf);

	IMB_colorize(ima->preview_ibuf, hue, saturation, lightness);

	BKE_image_release_ibuf(ima, ibuf, NULL);
	return TRUE;
}

void IMAGE_OT_colorize(wmOperatorType *ot)
{
/* identifiers */
	ot->name = "Colorize Image";
	ot->idname = "IMAGE_OT_colorize";
	ot->description = "Colorize the Image";
 
	/* api callbacks */
	ot->exec = image_colorize_exec;
	ot->poll = image_operator_poll;
	ot->invoke = image_op_layer_invoke;
	ot->check = image_colorize_check;
	ot->cancel = image_preview_cancel;
 
	/* flags */
	ot->flag = OPTYPE_REGISTER | OPTYPE_UNDO;
 
	/* properties */
	RNA_def_int(ot->srna, "hue", 180, 0, 360, "Hue", "Hue", 0, 360);
	RNA_def_int(ot->srna, "saturation", 50, 0, 100, "Saturation", "Saturation", 0, 100);
	RNA_def_int(ot->srna, "lightness", 0, -100, 100, "Lightness", "Lightness", -100, 100);
}

/********************* Color Space Operators *********************/

static int image_grayscale_exec(bContext *C, wmOperator *UNUSED(op))
{
	ImageLayer *layer;
	Image *ima = CTX_data_edit_image(C);
	ImBuf *ibuf = BKE_image_acquire_ibuf(ima, NULL, NULL, IMA_IBUF_IMA);

	if (ibuf == NULL)
		return OPERATOR_CANCELLED;

	if ((!ibuf->rect_float) && (!ibuf->rect)) {
		BKE_image_release_ibuf(ima, ibuf, NULL);
		return OPERATOR_CANCELLED;
	}

	ima->color_space = IMA_COL_GRAY;
	IMB_color_to_bw(ibuf);

	for (layer = (ImageLayer *)ima->imlayers.first; layer; layer = layer->next) {
		IMB_color_to_bw((ImBuf *)layer->ibufs.first);
	}

	ibuf->userflags |= IB_BITMAPDIRTY;
	if (ibuf->mipmap[0])
		ibuf->userflags |= IB_MIPMAP_INVALID;

	WM_event_add_notifier(C, NC_IMAGE | NA_EDITED, ima);

	BKE_image_release_ibuf(ima, ibuf, NULL);
	return OPERATOR_FINISHED;
}

void IMAGE_OT_color_space_grayscale(wmOperatorType *ot)
{
	/* identifiers */
	ot->name = "GrayScale";
	ot->idname = "IMAGE_OT_color_space_grayscale";
	ot->description = "Convert the image to GrayScale";
	
	/* api callbacks */
	ot->exec = image_grayscale_exec;
	ot->poll = image_operator_poll;
	
	/* flags */
	ot->flag = OPTYPE_REGISTER;
}

static int image_rgb_exec(bContext *C, wmOperator *UNUSED(op))
{
	Image *ima = CTX_data_edit_image(C);

	ima->color_space = IMA_COL_RGB;

	return OPERATOR_FINISHED;
}

void IMAGE_OT_color_space_rgb(wmOperatorType *ot)
{
	/* identifiers */
	ot->name = "RGB";
	ot->idname = "IMAGE_OT_color_space_rgb";
	ot->description = "Convert the image to the RGB";
	
	/* api callbacks */
	ot->exec = image_rgb_exec;
	ot->poll = image_operator_poll;
	
	/* flags */
	ot->flag = OPTYPE_REGISTER;
}

/************************* New Image Operators ************************/

static int image_duplicate_exec(bContext *C, wmOperator *op)
{
	Image *ima = CTX_data_edit_image(C);
	Image *new_ima;
	ImageLayer *layer, *dup;
	ImBuf *ibuf;

	if (!ima)
		return OPERATOR_CANCELLED;

	new_ima = BKE_image_copy(G.main, ima);

	ibuf = BKE_image_acquire_ibuf(ima, NULL, NULL, IMA_IBUF_IMA);
	BLI_addtail(&new_ima->ibufs, IMB_dupImBuf(ibuf));
	new_ima->Act_Layers = ima->Act_Layers;
	new_ima->Count_Layers = ima->Count_Layers;
	new_ima->use_layers = ima->use_layers;
	new_ima->color_space = ima->color_space;

	layer = (ImageLayer *)ima->imlayers.first;
	new_ima->imlayers.first = new_ima->imlayers.last = NULL;

	while (layer) {
		dup = image_duplicate_layer(ima, layer);
		BLI_addtail(&new_ima->imlayers, dup);

		layer = layer->next;
	}

	BKE_report(op->reports, RPT_INFO, "Duplicate Image");

	WM_event_add_notifier(C, NC_IMAGE | ND_DRAW, ima);

	BKE_image_release_ibuf(ima, ibuf, NULL);

	return OPERATOR_FINISHED;
}
 
void IMAGE_OT_duplicate(wmOperatorType *ot)
{
	/* identifiers */
	ot->name = "Duplicate";
	ot->idname = "IMAGE_OT_duplicate";
	ot->description = "Duplicate the Image";
 
	/* api callbacks */
	ot->exec = image_duplicate_exec;
	ot->poll = image_operator_poll;
 
	/* flags */
	ot->flag = OPTYPE_REGISTER | OPTYPE_UNDO;
}

static int image_flip_exec(bContext *C, wmOperator *op)
{
	Image *ima = CTX_data_edit_image(C);
	ImBuf *ibuf;
	ImageLayer *layer;
	int type;

	if (!ima)
		return OPERATOR_CANCELLED;

	ima->use_layers = FALSE;
	ibuf = BKE_image_acquire_ibuf(ima, NULL, NULL, IMA_IBUF_IMA);
	ima->use_layers = TRUE;

	type = RNA_enum_get(op->ptr, "type");

	if (type == 1) { /* Flip Horizontally */
		IMB_flipx(ibuf);
		for (layer = ima->imlayers.first; layer; layer = layer->next)
			IMB_flipx((ImBuf *)layer->ibufs.first);
	}
	else if (type == 2) { /* Flip Vertically */
		IMB_flipy(ibuf);
		for (layer = ima->imlayers.first; layer; layer = layer->next)
			IMB_flipy((ImBuf *)layer->ibufs.first);
	}
	
	WM_event_add_notifier(C, NC_IMAGE | ND_DRAW, NULL);

	BKE_image_release_ibuf(ima, ibuf, NULL);
 
	return OPERATOR_FINISHED;
}

void IMAGE_OT_flip(wmOperatorType *ot)
{
	static EnumPropertyItem slot_flip[] = {
		{1, "FLIP_H", 0, "Horizontally", ""},
		{2, "FLIP_V", 0, "Vertically", ""},
		{0, NULL, 0, NULL, NULL}
	};
 
	/* identifiers */
	ot->name = "Flip";
	ot->idname = "IMAGE_OT_flip";
	ot->description = "Flip the Image";
 
	/* api callbacks */
	ot->exec = image_flip_exec;
	ot->poll = image_operator_poll;
 
	/* flags */
	ot->flag = OPTYPE_REGISTER | OPTYPE_UNDO;
 
	/* properties */
	RNA_def_enum(ot->srna, "type", slot_flip, 0, "Type", "");
}

static int image_rotate_exec(bContext *C, wmOperator *op)
{
	Image *ima = CTX_data_edit_image(C);
	ImageLayer *layer;
	ImBuf *ibuf, *ibuf_l;
	int type;
	static float col[4] = {0.0f, 0.0f, 0.0f, 0.0f};
 
	if (!ima)
		return OPERATOR_CANCELLED;

	type = RNA_enum_get(op->ptr, "type");

	ima->use_layers = FALSE;
	ibuf = BKE_image_acquire_ibuf(ima, NULL, NULL, IMA_IBUF_IMA);
	ima->use_layers = TRUE;
	
	if (type == 1) { /* ROT_90 */
		ibuf = IMB_rotation(ibuf, 0.0, 0.0, DEG2RADF(-90.0), 2, 0, col);
		for (layer = ima->imlayers.first; layer; layer = layer->next) {
			ibuf_l = (ImBuf *)layer->ibufs.first;
			layer->ibufs.first = NULL;
			layer->ibufs.last = NULL;
			BLI_addtail(&layer->ibufs, IMB_rotation(ibuf_l, 0.0, 0.0, DEG2RADF(-90.0), 2, 0, col));
		}
	}
	else if (type == 2) { /* ROT_90A */
		ibuf = IMB_rotation(ibuf, 0.0, 0.0, DEG2RADF(90.0), 2, 0, col);
		for (layer = ima->imlayers.first; layer; layer = layer->next) {
			ibuf_l = (ImBuf *)layer->ibufs.first;
			layer->ibufs.first = NULL;
			layer->ibufs.last = NULL;
			BLI_addtail(&layer->ibufs, IMB_rotation(ibuf_l, 0.0, 0.0, DEG2RADF(90.0), 2, 0, col));
		}
	}
	else if (type == 3) { /* ROT_180 */
		ibuf = IMB_rotation(ibuf, 0.0, 0.0, DEG2RADF(180.0), 2, 0, col);
		for (layer = ima->imlayers.first; layer; layer = layer->next) {
			ibuf_l = (ImBuf *)layer->ibufs.first;
			layer->ibufs.first = NULL;
			layer->ibufs.last = NULL;
			BLI_addtail(&layer->ibufs, IMB_rotation(ibuf_l, 0.0, 0.0, DEG2RADF(180.0), 2, 0, col));
		}
	}

	WM_event_add_notifier(C, NC_IMAGE | ND_DRAW, NULL);

	BKE_image_release_ibuf(ima, ibuf, NULL);
 
	return OPERATOR_FINISHED;
}

void IMAGE_OT_rotate(wmOperatorType *ot)
{
	static EnumPropertyItem slot_rot[] = {
		{1, "ROT_90", 0, "Rotate 90 clockwise", ""},
		{2, "ROT_90A", 0, "Rotate 90 anti-clockwise", ""},
		{3, "ROT_180", 0, "Rotate 180", ""},
		{0, NULL, 0, NULL, NULL}
	};
 
	/* identifiers */
	ot->name = "Rotating Image";
	ot->idname = "IMAGE_OT_rotate";
	ot->description = "Rotate the Image";
 
	/* api callbacks */
	ot->exec = image_rotate_exec;
	ot->poll = image_operator_poll;
 
	/* flags */
	ot->flag = OPTYPE_REGISTER | OPTYPE_UNDO;
 
	/* properties */
	RNA_def_enum(ot->srna, "type", slot_rot, 0, "Type", "");
}

static int image_arbitrary_rot_exec(bContext *C, wmOperator *op)
{
	Image *ima = CTX_data_edit_image(C);
	ImageLayer *layer;
	ImBuf *ibuf, *ibuf_l;
	float angle;
	float col[4];
	short type;
	int lock;
	
	if (!ima)
		return OPERATOR_CANCELLED;

	ima->use_layers = FALSE;
	ibuf = BKE_image_acquire_ibuf(ima, NULL, NULL, IMA_IBUF_IMA);
	ima->use_layers = TRUE;

	if (ima->preview_ibuf) {
		IMB_freeImBuf(ima->preview_ibuf);
		ima->preview_ibuf = NULL;
	}

	type = RNA_enum_get(op->ptr, "type");
	angle = RNA_float_get(op->ptr, "angle");
	lock = RNA_boolean_get(op->ptr, "lock_size");

	get_color_background_layer(col, (ImageLayer*)ima->imlayers.last);
	
	angle = angle * (-1);

	ibuf = IMB_rotation(ibuf, 0.0, 0.0, angle, type, lock, col);
	for (layer = ima->imlayers.first; layer; layer = layer->next) {
		ibuf_l = (ImBuf *)layer->ibufs.first;
		layer->ibufs.first = NULL;
		layer->ibufs.last = NULL;
		BLI_addtail(&layer->ibufs, IMB_rotation(ibuf_l, 0.0, 0.0, angle, type, lock, col));
	}

	WM_event_add_notifier(C, NC_IMAGE | ND_DRAW, NULL);

	BKE_image_release_ibuf(ima, ibuf, NULL);

	return OPERATOR_FINISHED;
}

static bool image_arbitrary_rot_check(bContext *C, wmOperator *op)
{
	Image *ima = CTX_data_edit_image(C);
	ImBuf *ibuf;
	float col[4];
	float angle;
	short type;
	int lock;

	if (!ima)
		return FALSE;

	ima->use_layers = FALSE;
	ibuf = BKE_image_acquire_ibuf(ima, NULL, NULL, IMA_IBUF_IMA);
	ima->use_layers = TRUE;

	type = RNA_enum_get(op->ptr, "type");
	angle = RNA_float_get(op->ptr, "angle");
	lock = RNA_boolean_get(op->ptr, "lock_size");
	get_color_background_layer(col, (ImageLayer*)ima->imlayers.last);

	angle = angle * (-1);

	if (ima->preview_ibuf) {
		IMB_freeImBuf(ima->preview_ibuf);
		ima->preview_ibuf = NULL;
	}

	ima->preview_ibuf = IMB_dupImBuf(ibuf);

	ima->preview_ibuf = IMB_rotation(ima->preview_ibuf, 0.0, 0.0, angle, type, lock, col);

	BKE_image_release_ibuf(ima, ibuf, NULL);
	return TRUE;
}

void IMAGE_OT_arbitrary_rot(wmOperatorType *ot)
{
	PropertyRNA *prop;
	
	static EnumPropertyItem rotate_items[] = {
		{0, "NEAREST", 0, "Nearest", ""},
		{1, "BILINEAR", 0, "Bilinear", ""},
		{2, "BICUBIC", 0, "Bicubic", ""},
		{0, NULL, 0, NULL, NULL}
	};

	/* identifiers */
	ot->name = "Arbitrary Rotating Image";
	ot->idname = "IMAGE_OT_arbitrary_rot";
	ot->description = "Arbitrary Rotate the Imager";
 
	/* api callbacks */
	ot->exec = image_arbitrary_rot_exec;
	ot->poll = image_operator_poll;
	ot->invoke = image_op_layer_invoke;
	ot->check = image_arbitrary_rot_check;
	ot->cancel = image_preview_cancel;
 
	/* flags */
	ot->flag = OPTYPE_REGISTER | OPTYPE_UNDO;
 
	/* properties */
	RNA_def_enum(ot->srna, "type", rotate_items, 0, "Type", "");
	RNA_def_boolean(ot->srna, "lock_size", 1, "Lock size", "Lock size");
	prop = RNA_def_float_rotation(ot->srna, "angle", 0, NULL, DEG2RADF(-180.0f), DEG2RADF(180.0f),
	                              "Angle", "Angle of rotation", DEG2RADF(-180.0f), DEG2RADF(180.0f));
	RNA_def_property_float_default(prop, DEG2RADF(0.0f));
}

static int image_offset_exec(bContext *C, wmOperator *op)
{
	Image *ima = CTX_data_edit_image(C);
	ImageLayer *layer;
	struct ImBuf *ibuf, *ibuf_l;
	int x, y, half, wrap;
	float col[4];
	
	if (!ima)
		return OPERATOR_CANCELLED;

	ima->use_layers = FALSE;
	ibuf = BKE_image_acquire_ibuf(ima, NULL, NULL, IMA_IBUF_IMA);
	ima->use_layers = TRUE;

	if (ima->preview_ibuf) {
		IMB_freeImBuf(ima->preview_ibuf);
		ima->preview_ibuf = NULL;
	}

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
		get_color_background_layer(col, (ImageLayer*)ima->imlayers.last);
	}

	ibuf = IMB_offset(ibuf, x, y, half, wrap, col);
	for (layer = ima->imlayers.first; layer; layer = layer->next) {
		ibuf_l = (ImBuf *)layer->ibufs.first;
		layer->ibufs.first = NULL;
		layer->ibufs.last = NULL;
		BLI_addtail(&layer->ibufs, IMB_offset(ibuf_l, x, y, half, wrap, col));
	}

	WM_event_add_notifier(C, NC_IMAGE | ND_DRAW, NULL);

	BKE_image_release_ibuf(ima, ibuf, NULL);
 
	return OPERATOR_FINISHED;
}

static bool image_offset_check(bContext *C, wmOperator *op)
{
	Image *ima = CTX_data_edit_image(C);
	ImBuf *ibuf;
	float col[4];
	int x, y, half, wrap;

	if (!ima)
		return FALSE;

	ima->use_layers = FALSE;
	ibuf = BKE_image_acquire_ibuf(ima, NULL, NULL, IMA_IBUF_IMA);
	ima->use_layers = TRUE;

	x = RNA_int_get(op->ptr, "off_x");
	y = RNA_int_get(op->ptr, "off_y");
	half = RNA_boolean_get(op->ptr, "half");
	wrap = RNA_boolean_get(op->ptr, "wrap");

	if (abs(x) > ibuf->x) {
		if (x > 0)
			x = ibuf->x;
		else
			x = - ibuf->x;
	}

	if (abs(y) > ibuf->y) {
		if (y > 0)
			y = ibuf->y;
		else
			y = - ibuf->y;
	}

	if (!wrap) {
		get_color_background_layer(col, (ImageLayer*)ima->imlayers.last);
	}

	if (ima->preview_ibuf) {
		IMB_freeImBuf(ima->preview_ibuf);
		ima->preview_ibuf = NULL;
	}

	ima->preview_ibuf = IMB_dupImBuf(ibuf);

	ima->preview_ibuf = IMB_offset(ima->preview_ibuf, x, y, half, wrap, col);

	BKE_image_release_ibuf(ima, ibuf, NULL);
	return TRUE;
}

void IMAGE_OT_offset(wmOperatorType *ot)
{
	/* identifiers */
	ot->name = "Offset";
	ot->idname = "IMAGE_OT_offset";
	ot->description = "Shift the pixels";
 
	/* api callbacks */
	ot->exec = image_offset_exec;
	ot->poll = image_operator_poll;
	ot->invoke = image_op_layer_invoke;
	ot->check = image_offset_check;
	ot->cancel = image_preview_cancel;

	/* flags */
	ot->flag = OPTYPE_REGISTER | OPTYPE_UNDO;

	/* properties */
	RNA_def_int(ot->srna, "off_x", 0, INT_MIN, INT_MAX, "X", "Offset X", -16384, 16384);
	RNA_def_int(ot->srna, "off_y", 0, INT_MIN, INT_MAX, "Y", "Offset Y", -16384, 16384);
	RNA_def_boolean(ot->srna, "half", 0, "Offset by x/2 y/2", "Offset by x/2 y/2");
	RNA_def_boolean(ot->srna, "wrap", 1, "Wrap around", "Wrap around");
}

static int image_scale_exec(bContext *C, wmOperator *op)
{
	Image *ima = CTX_data_edit_image(C);
	ImageLayer *layer;
	struct ImBuf *ibuf, *ibuf_l;
	int width, height, proportions;
	float props;
	
	if (!ima)
		return OPERATOR_CANCELLED;

	ima->use_layers = FALSE;
	ibuf = BKE_image_acquire_ibuf(ima, NULL, NULL, IMA_IBUF_IMA);
	ima->use_layers = TRUE;
	
	width = RNA_int_get(op->ptr, "width");
	height = RNA_int_get(op->ptr, "height");
	proportions = RNA_boolean_get(op->ptr, "proportions");

	if ((width == 0) && (height == 0))
		return OPERATOR_CANCELLED;

	if (proportions) {
		if ((width != 0) && (height != 0)) {
			BKE_report(op->reports, RPT_WARNING, "If you want to keep the proportions, the height or width must be 0.");
			return OPERATOR_CANCELLED;
		}

		if ((width == 0) || (height == 0)) {
			if (width == 0) {
				props = (float)ibuf->y / ibuf->x;
				width = (int)floor((float)height / props);
			}
			else {
				props = (float)ibuf->x / ibuf->y;
				height = (int)floor((float)width / props);
			}
		}
	}

	IMB_scaleImBuf(ibuf, width, height);
	for (layer = ima->imlayers.first; layer; layer = layer->next) {
		ibuf_l = (ImBuf *)layer->ibufs.first;
		layer->ibufs.first = NULL;
		layer->ibufs.last = NULL;
		BLI_addtail(&layer->ibufs, IMB_scaleImBuf(ibuf_l, width, height));
	}

	WM_event_add_notifier(C, NC_IMAGE | ND_DRAW, NULL);

	BKE_image_release_ibuf(ima, ibuf, NULL);
 
	return OPERATOR_FINISHED;
}

void IMAGE_OT_scale(wmOperatorType *ot)
{
	/* identifiers */
	ot->name = "Scale";
	ot->idname = "IMAGE_OT_scale";
	ot->description = "Scale the Image";
 
	/* api callbacks */
	ot->exec = image_scale_exec;
	ot->poll = image_operator_poll;
	ot->invoke = image_op_layer_invoke;

	/* flags */
	ot->flag = OPTYPE_REGISTER | OPTYPE_UNDO;

	/* properties */
	RNA_def_int(ot->srna, "width", 0, 0, INT_MAX, "Width", "Width", 0, 16384);
	RNA_def_int(ot->srna, "height", 0, 0, INT_MAX, "Height", "Height", 0, 16384);
	RNA_def_boolean(ot->srna, "proportions", 1, "Keep Proportions", "Keep proportions");
}

static int image_merge_exec(bContext *C, wmOperator *op)
{
	Image *ima = CTX_data_edit_image(C);
	ImageLayer *layer, *prec;
	int flag = 0;
	int discard;
 
	if (!ima)
		return OPERATOR_CANCELLED;

	discard = RNA_boolean_get(op->ptr, "discard");

	for (layer = (ImageLayer *)ima->imlayers.first; layer; layer = layer->next) {
		if (layer->visible & IMA_LAYER_VISIBLE) {
			flag = 1;
			break;
		}
	}
	if (flag == 1) {
		prec = NULL;
		for (layer = (ImageLayer *)ima->imlayers.first; layer; layer = layer->next) {
			if (layer->visible & IMA_LAYER_VISIBLE) {
				if (prec == NULL) {
					prec = layer;
				}
				else {
					layer = merge_layers(ima, prec, layer);
					ima->Count_Layers--;
					prec = layer;
				}
				
			}
			else {
				if (discard) {
					BLI_remlink(&ima->imlayers, layer);
					free_image_layer(layer);
					if (ima->imlayers.first) {
						if (imalayer_get_current_act(ima) != 1)
							imalayer_set_current_act(ima, imalayer_get_current_act(ima));
						else
							imalayer_set_current_act(ima, imalayer_get_current_act(ima) - 1);
					}
					ima->Count_Layers -= 1;
				}
			}
		}
		if (discard)
			ima->Act_Layers = 0;
		else
			ima->Act_Layers = ima->Count_Layers -1;
		imalayer_set_current_act(ima, ima->Act_Layers);
	}
	else
		BKE_report(op->reports, RPT_INFO, "It can not merge the layers, because the layers are hidden");

	WM_event_add_notifier(C, NC_IMAGE | ND_DRAW, NULL);
 
	return OPERATOR_FINISHED;
}

void IMAGE_OT_merge(wmOperatorType *ot)
{
	/* identifiers */
	ot->name = "Merge Layer";
	ot->idname = "IMAGE_OT_merge";
	ot->description = "";
 
	/* api callbacks */
	ot->exec = image_merge_exec;
	ot->poll = image_operator_poll;
	ot->invoke = image_op_layer_invoke;
 
	/* flags */
	ot->flag = OPTYPE_REGISTER | OPTYPE_UNDO;
 
	/* properties */
	RNA_def_boolean(ot->srna, "discard", 0, "Del invisible layers", "Discard invisible layers");
}

static int image_flatten_exec(bContext *C, wmOperator *op)
{
	SpaceImage *sima = CTX_wm_space_image(C);
	Image *ima = CTX_data_edit_image(C);
	ImageLayer *layer, *prec;
	ImBuf *ibuf;
	float col[4];
	int flag = 0;

	if (!ima)
		return OPERATOR_CANCELLED;

	for (layer = (ImageLayer *)ima->imlayers.first; layer; layer = layer->next) {
		if (layer->visible & IMA_LAYER_VISIBLE) {
			flag = 1;
			break;
		}
	}
	if (flag == 1) {
		prec = NULL;
		for (layer = (ImageLayer *)ima->imlayers.first; layer; layer = layer->next) {
			if (layer->visible & IMA_LAYER_VISIBLE) {
				if (prec == NULL) {
					prec = layer;
				}
				else {
					layer = merge_layers(ima, prec, layer);
					ima->Count_Layers--;
					prec = layer;
				}
				
			}
			else {
				BLI_remlink(&ima->imlayers, layer);
				free_image_layer(layer);
				if (ima->imlayers.first) {
					if (imalayer_get_current_act(ima) != 1)
						imalayer_set_current_act(ima, imalayer_get_current_act(ima));
					else
						imalayer_set_current_act(ima, imalayer_get_current_act(ima) - 1);
				}
				ima->Count_Layers -= 1;
			}
		}

		ima->Act_Layers = 0;
		imalayer_set_current_act(ima, ima->Act_Layers);
		
		get_color_background_layer(col, prec);

		ima->use_layers = FALSE;
		if (sima->mode == SI_MODE_PAINT)
			ibuf = BKE_image_acquire_ibuf(ima, NULL, NULL, IMA_IBUF_LAYER);
		else 
			ibuf = BKE_image_acquire_ibuf(ima, NULL, NULL, IMA_IBUF_IMA);
		ima->use_layers = TRUE;

		if (ibuf->rect)
			IMB_alpha_under_color_byte((unsigned char *)ibuf->rect, ibuf->x, ibuf->y, col);
		else if (ibuf->rect_float)
			IMB_alpha_under_color_float(ibuf->rect_float, ibuf->x, ibuf->y, col);
		if (sima->mode != SI_MODE_PAINT) {
			ImBuf *ibuf_l;

			ibuf_l = (ImBuf *)prec->ibufs.first;
			if (ibuf_l->rect)
				IMB_alpha_under_color_byte((unsigned char *)ibuf_l->rect, ibuf_l->x, ibuf_l->y, col);
			else if (ibuf_l->rect_float)
				IMB_alpha_under_color_float(ibuf_l->rect_float, ibuf_l->x, ibuf_l->y, col);
		}

		ibuf->userflags |= IB_BITMAPDIRTY;
		BKE_image_release_ibuf(ima, ibuf, NULL);
	}
	else
		BKE_report(op->reports, RPT_INFO, "It can not merge the layers, because the layers are hidden");

	WM_event_add_notifier(C, NC_IMAGE | ND_DRAW, ima);
	return OPERATOR_FINISHED;
}

void IMAGE_OT_flatten(wmOperatorType *ot)
{
 
	/* identifiers */
	ot->name = "Flatten Image";
	ot->idname = "IMAGE_OT_flatten";
	ot->description = "";
 
	/* api callbacks */
	ot->exec = image_flatten_exec;
	ot->poll = image_operator_poll;
 
	/* flags */
	ot->flag = OPTYPE_REGISTER | OPTYPE_UNDO;
}

/********************** New Image Layer Operators *********************/

static int image_layer_poll(bContext *C)
{
	SpaceImage *sima = CTX_wm_space_image(C);
	if (sima)
		return ED_space_image_show_paint(sima);
	else
		return 0;
}
 
static void image_layer_preview_cancel(bContext *C, wmOperator *UNUSED(op))
{
	Image *ima = CTX_data_edit_image(C);
	ImageLayer *layer;

	layer = imalayer_get_current(ima);
	if (layer) {
		if (layer->preview_ibuf) {
			IMB_freeImBuf(layer->preview_ibuf);
			layer->preview_ibuf = NULL;
			WM_event_add_notifier(C, NC_IMAGE | ND_DRAW, NULL);
		}
	}
}

static int image_layer_add_exec(bContext *C, wmOperator *op)
{	
	char name[22];
	float color[4];
	int alpha, order;
	Scene *scene;
	Image *ima = CTX_data_edit_image(C);
	ImageLayer *iml;

	scene = (Scene*)CTX_data_scene(C);

	if (strcmp(op->idname, "IMAGE_OT_layer_add_default") != 0) {
		RNA_string_get(op->ptr, "name", name);
		RNA_float_get_array(op->ptr, "color", color);
		alpha = RNA_boolean_get(op->ptr, "alpha");
	}
	else {
		strcpy(name, "Layer");
		color[0] = 0.0f;
		color[1] = 0.0f;
		color[2] = 0.0f;
		color[3] = 0.0f;
		alpha = 1;
	}

	order = 2;
	if (strcmp(op->idname, "IMAGE_OT_layer_add_above") == 0)
		order = 1;
	else if (strcmp(op->idname, "IMAGE_OT_layer_add_below") == 0)
		order = -1;

	if (scene->r.color_mgt_flag & R_COLOR_MANAGEMENT)
		linearrgb_to_srgb_v3_v3(color, color);

	if (!alpha) 
		color[3] = 1.0f;
	
	iml = image_add_image_layer(ima, name, alpha ? 32 : 24, color, order);

	if (!iml)
		return OPERATOR_CANCELLED;

	WM_event_add_notifier(C, NC_IMAGE | ND_DRAW, ima);
 
	return OPERATOR_FINISHED;
}

void IMAGE_OT_layer_add(wmOperatorType *ot)
{
	PropertyRNA *prop;
	static float default_color[4] = {0.0f, 0.0f, 0.0f, 0.0f};

	/* identifiers */
	ot->name = "New Layer";
	ot->idname = "IMAGE_OT_layer_add";
	ot->description = "Add a new image layer";
 
	/* api callbacks */
	ot->exec = image_layer_add_exec;
	ot->poll = image_layer_poll;
	ot->invoke = image_new_invoke;
	
	/* flags */
	ot->flag = OPTYPE_REGISTER | OPTYPE_UNDO;

	/* properties */
	RNA_def_string(ot->srna, "name", "Layer", 21, "Name", "Layer name");
	prop = RNA_def_float_color(ot->srna, "color", 4, NULL, 0.0f, FLT_MAX, "Fill Color", "Color used to fill the layer", 0.0f, 1.0f);
	RNA_def_property_float_array_default(prop, default_color);
	RNA_def_boolean(ot->srna, "alpha", 1, "Alpha", "Create an image with an alpha channel");
}

void IMAGE_OT_layer_add_above(wmOperatorType *ot)
{
	PropertyRNA *prop;
	static float default_color[4] = {0.0f, 0.0f, 0.0f, 0.0f};

	/* identifiers */
	ot->name = "Above active layer";
	ot->idname = "IMAGE_OT_layer_add_above";
	ot->description = "Add a new image layer";
 
	/* api callbacks */
	ot->exec = image_layer_add_exec;
	ot->poll = image_layer_poll;
	ot->invoke = image_new_invoke;
	
	/* flags */
	ot->flag = OPTYPE_REGISTER|OPTYPE_UNDO;

	/* properties */
	RNA_def_string(ot->srna, "name", "Layer", 21, "Name", "Layer name");
	prop = RNA_def_float_color(ot->srna, "color", 4, NULL, 0.0f, FLT_MAX, "Fill Color", "Color used to fill the layer", 0.0f, 1.0f);
	RNA_def_property_float_array_default(prop, default_color);
	RNA_def_boolean(ot->srna, "alpha", 1, "Alpha", "Create an image with an alpha channel");
}

void IMAGE_OT_layer_add_below(wmOperatorType *ot)
{
	PropertyRNA *prop;
	static float default_color[4] = {0.0f, 0.0f, 0.0f, 0.0f};

	/* identifiers */
	ot->name = "Below active layer";
	ot->idname = "IMAGE_OT_layer_add_below";
	ot->description = "Add a new image layer";
 
	/* api callbacks */
	ot->exec = image_layer_add_exec;
	ot->poll = image_layer_poll;
	ot->invoke = image_new_invoke;
	
	/* flags */
	ot->flag = OPTYPE_REGISTER | OPTYPE_UNDO;

	/* properties */
	RNA_def_string(ot->srna, "name", "Layer", 21, "Name", "Layer name");
	prop = RNA_def_float_color(ot->srna, "color", 4, NULL, 0.0f, FLT_MAX, "Fill Color", "Color used to fill the layer", 0.0f, 1.0f);
	RNA_def_property_float_array_default(prop, default_color);
	RNA_def_boolean(ot->srna, "alpha", 1, "Alpha", "Create an image with an alpha channel");
}

void IMAGE_OT_layer_add_default(wmOperatorType *ot)
{
	/* identifiers */
	ot->name = "Default layer";
	ot->idname = "IMAGE_OT_layer_add_default";
	ot->description = "Add a new image layer";
 
	/* api callbacks */
	ot->exec = image_layer_add_exec;
	ot->poll = image_layer_poll;
	
	/* flags */
	ot->flag = OPTYPE_REGISTER | OPTYPE_UNDO;
}

static int image_layer_duplicate_exec(bContext *C, wmOperator *UNUSED(op))
{
	Image *ima = CTX_data_edit_image(C);
	ImageLayer *iml;

	if (!ima)
		return OPERATOR_CANCELLED;
 
	iml = image_duplicate_current_image_layer(ima);
	
	if (!iml)
		return OPERATOR_CANCELLED;

	WM_event_add_notifier(C, NC_IMAGE | ND_DRAW, ima);
 
	return OPERATOR_FINISHED;
}
 
void IMAGE_OT_layer_duplicate(wmOperatorType *ot)
{
	/* identifiers */
	ot->name = "Duplicate Layer";
	ot->idname = "IMAGE_OT_layer_duplicate";
	ot->description = "Duplicate the selected image layer";
 
	/* api callbacks */
	ot->exec = image_layer_duplicate_exec;
	ot->poll = image_layer_poll;
 
	/* flags */
	ot->flag = OPTYPE_REGISTER | OPTYPE_UNDO;
}

static int image_layer_remove_exec(bContext *C, wmOperator *op)
{
	Image *ima = CTX_data_edit_image(C);
	int action = RNA_enum_get(op->ptr, "action");
 
	if (!ima)
		return OPERATOR_CANCELLED;
 
	if (image_remove_layer(ima, action) == -1)
		BKE_report(op->reports, RPT_INFO, "Impossible to remove only one layer");
		
	WM_event_add_notifier(C, NC_IMAGE | ND_DRAW, ima);
 
	return OPERATOR_FINISHED;
}
 
void IMAGE_OT_layer_remove(wmOperatorType *ot)
{
	static EnumPropertyItem select_all_actions[] = {
			{IMA_LAYER_DEL_SELECTED, "SELECTED", 0, "Selected", "Remove the selected layer"},
			{IMA_LAYER_DEL_HIDDEN, "HIDDEN", 0, "Hidden", "Removes the hidden layers"},
			{0, NULL, 0, NULL, NULL}
	};

	/* identifiers */
	ot->name = "Remove Layer";
	ot->idname = "IMAGE_OT_layer_remove";
	ot->description = "Remove the selected image layer";
 
	/* api callbacks */
	ot->exec = image_layer_remove_exec;
	ot->poll = image_layer_poll;
 
	/* flags */
	ot->flag = OPTYPE_REGISTER | OPTYPE_UNDO;

	/* properties */
	RNA_def_enum(ot->srna, "action", select_all_actions, IMA_LAYER_DEL_SELECTED, "Action", "Selection action to execute");
}

static int image_layer_move_exec(bContext *C, wmOperator *op)
{
	Image *ima = CTX_data_edit_image(C);
	ImageLayer *layer, *tmp;
	int type, layerID;
 
	if (!ima)
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

				if (tmp)
					BLI_insertlinkbefore(&ima->imlayers, tmp, layer);
				else
					BLI_addhead(&ima->imlayers, layer);

				imalayer_set_current_act(ima, layerID - 1);
			}
		}
		else if (type == 1){ /* Move direction: Down */
			if (layerID < (ima->Count_Layers - 1)) {
				tmp = layer->next;
				if (!(tmp->type & IMA_LAYER_BASE)) {
					BLI_remlink(&ima->imlayers, layer);
					layer->next = layer->prev = NULL;

					if (tmp)
						BLI_insertlinkafter(&ima->imlayers, tmp, layer);
					else
						BLI_addtail(&ima->imlayers, layer);

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
				BLI_insertlinkafter(&ima->imlayers,((ImageLayer *)ima->imlayers.last)->prev, layer);
				ima->Act_Layers = ima->Count_Layers - 2;
			}
			else {
				BLI_addtail(&ima->imlayers, layer);
				ima->Act_Layers = ima->Count_Layers - 1;
			}
		}
		else if (type == 3) {  /* Move direction: Invert */
			int i = 0, lim;
			ImageLayer *tmp, *next, *prev, *tmp_up, *tmp_down;
			if (ima->Count_Layers > 2) {
				if (ima->Count_Layers % 2 == 0)
					lim = (ima->Count_Layers / 2);
				else
					lim = (ima->Count_Layers / 2) + 1;

				tmp_up = (ImageLayer *)ima->imlayers.first;
				tmp_down = ((ImageLayer *)ima->imlayers.last)->prev;
				while ((i<lim) && (tmp_up != tmp_down)) {
					tmp = tmp_up;

					next = tmp_down->next;
					prev = tmp_down->prev;
					
					tmp_down->next = tmp->next;
					tmp_down->prev = tmp->prev;
					if (tmp->prev)
						tmp->prev->next = tmp_down;
					else
						ima->imlayers.first = tmp_down;
					tmp->next->prev = tmp_down;

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
	WM_event_add_notifier(C, NC_IMAGE | ND_DRAW, NULL);
 
	return OPERATOR_FINISHED;
}
 
void IMAGE_OT_layer_move(wmOperatorType *ot)
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
	ot->name = "Move Layer";
	ot->idname = "IMAGE_OT_layer_move";
	ot->description = "Move image layers up and down";
 
	/* api callbacks */
	ot->exec = image_layer_move_exec;
	ot->poll = image_layer_poll;
 
	/* flags */
	ot->flag = OPTYPE_REGISTER | OPTYPE_UNDO;
 
	/* properties */
	RNA_def_enum(ot->srna, "type", slot_move, 0, "Type", "");
}

static int image_layer_select_exec(bContext *C, wmOperator *op)
{
	Image *ima = CTX_data_edit_image(C);
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
	WM_event_add_notifier(C, NC_IMAGE | ND_DRAW, NULL);
	return OPERATOR_FINISHED;
}

void IMAGE_OT_layer_select(wmOperatorType *ot)
{
	static EnumPropertyItem select_all_actions[] = {
			{IMA_LAYER_SEL_PREVIOUS, "PREVIOUS", 0, "Previous", "Select the previous layer"},
			{IMA_LAYER_SEL_NEXT, "NEXT", 0, "Next", "Select the next layer"},
			{IMA_LAYER_SEL_TOP, "TOP", 0, "Top", "Select the top layer"},
			{IMA_LAYER_SEL_BOTTOM, "BOTTOM", 0, "Select the bottom layer"},
			{0, NULL, 0, NULL, NULL}
	};
 
	/* identifiers */
	ot->name = "Select Layers";
	ot->idname = "IMAGE_OT_layer_select";
	ot->description = "Select layers";
 
	/* api callbacks */
	ot->exec = image_layer_select_exec;
	ot->poll = image_layer_poll;
 
	/* flags */
	ot->flag = OPTYPE_REGISTER | OPTYPE_UNDO;
 
	/* properties */
	RNA_def_enum(ot->srna, "action", select_all_actions, IMA_LAYER_SEL_NEXT, "Action", "Selection action to execute");
}

static int image_layer_merge_exec(bContext *C, wmOperator *op)
{
	Image *ima = CTX_data_edit_image(C);
	ImageLayer *layer;
	int type;
 
	if (!ima)
		return OPERATOR_CANCELLED;
  
	type = RNA_enum_get(op->ptr, "type");
	
	if (type == 1) { /* Merge Layers */
		layer = imalayer_get_current(ima);
		if (!layer)
				return OPERATOR_CANCELLED;
		
		if (!(layer->type & IMA_LAYER_BASE)) {
			ImageLayer *next;
			
			next = layer->next;
			if ((next->visible & IMA_LAYER_VISIBLE) && (!(next->locked & IMA_LAYER_LOCK))) {
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
		int i = 0;
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
					float *fp_b = (float *)base->rect_float;
					for (i = base->x * base->y; i > 0; i--, fp_b += 4) {
						if (fp_b[3] != 1.0f) {
							if (fp_b[3] == 0.0f) {
								fp_b[0] = 1.0f;
								fp_b[1] = 1.0f;
								fp_b[2] = 1.0f;
							}
							fp_b[3] = 1.0f;
						}
					}
				} else if (base->rect) {
					char *cp_b = (char *) base->rect;
					for (i = base->x * base->y; i > 0; i--, cp_b += 4) {
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
	
	WM_event_add_notifier(C, NC_IMAGE | ND_DRAW, NULL);
 
	return OPERATOR_FINISHED;
}

void IMAGE_OT_layer_merge(wmOperatorType *ot)
{
	static EnumPropertyItem slot_merge[] = {
		{1, "DOWN", 0, "Down", ""},
		{2, "VISIBLE", 0, "Visible", ""},
		{3, "ONE", 0, "One", ""},
		{0, NULL, 0, NULL, NULL}
	};
 
	/* identifiers */
	ot->name = "Merge Layer";
	ot->idname = "IMAGE_OT_layer_merge";
	ot->description = "Layers merge into one";
 
	/* api callbacks */
	ot->exec = image_layer_merge_exec;
	ot->poll = image_layer_poll;
 
	/* flags */
	ot->flag = OPTYPE_REGISTER | OPTYPE_UNDO;
 
	/* properties */
	RNA_def_enum(ot->srna, "type", slot_merge, 0, "Type", "");
}

static int image_layer_clean_exec(bContext *C, wmOperator *UNUSED(op))
{
	Image *ima = CTX_data_edit_image(C);
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

		flag = IB_rect | IB_multilayer | IB_metadata;

		ibuf = IMB_loadiffname(layer->file_path, flag, ima->colorspace_settings.name);
		
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

	WM_event_add_notifier(C, NC_IMAGE | ND_DRAW, ima);
	return OPERATOR_FINISHED;
}

void IMAGE_OT_layer_clean(wmOperatorType *ot)
{
 
	/* identifiers */
	ot->name = "Clean Layer";
	ot->idname = "IMAGE_OT_layer_clean";
	ot->description = "Clean image layers";
 
	/* api callbacks */
	ot->exec = image_layer_clean_exec;
	ot->poll = image_layer_poll;
 
	/* flags */
	ot->flag = OPTYPE_REGISTER | OPTYPE_UNDO;
}

static int image_layer_flip_exec(bContext *C, wmOperator *op)
{
	Image *ima = CTX_data_edit_image(C);
	ImageLayer *layer;
	int type;
 
	if (!ima)
		return OPERATOR_CANCELLED;
  
	type = RNA_enum_get(op->ptr, "type");
	
	layer = imalayer_get_current(ima);
	if (!layer)
			return OPERATOR_CANCELLED;
	
	if (type == 1) /* Flip Horizontally */
		IMB_flipx((ImBuf *)layer->ibufs.first);
	else if (type == 2) /* Flip Vertically */
		IMB_flipy((ImBuf *)layer->ibufs.first);
	
	WM_event_add_notifier(C, NC_IMAGE | ND_DRAW, NULL);
 
	return OPERATOR_FINISHED;
}

void IMAGE_OT_layer_flip(wmOperatorType *ot)
{
	static EnumPropertyItem slot_flip[] = {
		{1, "FLIP_H", 0, "Horizontally", ""},
		{2, "FLIP_V", 0, "Vertically", ""},
		{0, NULL, 0, NULL, NULL}
	};
 
	/* identifiers */
	ot->name = "Flip Layer";
	ot->idname = "IMAGE_OT_layer_flip";
	ot->description = "Flip the Layer";
 
	/* api callbacks */
	ot->exec = image_layer_flip_exec;
	ot->poll = image_layer_poll;
 
	/* flags */
	ot->flag = OPTYPE_REGISTER | OPTYPE_UNDO;
 
	/* properties */
	RNA_def_enum(ot->srna, "type", slot_flip, 0, "Type", "");
}

static int image_layer_rotate_exec(bContext *C, wmOperator *op)
{
	Image *ima = CTX_data_edit_image(C);
	ImageLayer *layer;
	ImBuf *ibuf;
	int type;
	float col[4];
 
	if (!ima)
		return OPERATOR_CANCELLED;
  
	type = RNA_enum_get(op->ptr, "type");

	layer = imalayer_get_current(ima);
	if (!layer)
			return OPERATOR_CANCELLED;
	
	get_color_background_layer(col, layer);

	ibuf = (ImBuf*)((ImageLayer*)layer->ibufs.first);
	if (type == 1) { /* ROT_90 */
		layer->ibufs.first = NULL;
		layer->ibufs.last = NULL;
		BLI_addtail(&layer->ibufs, IMB_rotation(ibuf, 0.0, 0.0, DEG2RADF(-90.0), 2, 0, col));
	}
	else if (type == 2) { /* ROT_90A */
		layer->ibufs.first = NULL;
		layer->ibufs.last = NULL;
		BLI_addtail(&layer->ibufs, IMB_rotation(ibuf, 0.0, 0.0, DEG2RADF(90.0), 2, 0, col));
	}
	else if (type == 3) { /* ROT_180 */
		layer->ibufs.first = NULL;
		layer->ibufs.last = NULL;
		BLI_addtail(&layer->ibufs, IMB_rotation(ibuf, 0.0, 0.0, DEG2RADF(180.0), 2, 0, col));
	}

	WM_event_add_notifier(C, NC_IMAGE | ND_DRAW, NULL);
 
	return OPERATOR_FINISHED;
}

void IMAGE_OT_layer_rotate(wmOperatorType *ot)
{
	static EnumPropertyItem slot_rot[] = {
		{1, "ROT_90", 0, "Rotate 90 clockwise", ""},
		{2, "ROT_90A", 0, "Rotate 90 anti-clockwise", ""},
		{3, "ROT_180", 0, "Rotate 180", ""},
		{0, NULL, 0, NULL, NULL}
	};
 
	/* identifiers */
	ot->name = "Rotating Layer";
	ot->idname = "IMAGE_OT_layer_rotate";
	ot->description = "Rotate the Layer";
 
	/* api callbacks */
	ot->exec = image_layer_rotate_exec;
	ot->poll = image_layer_poll;
 
	/* flags */
	ot->flag = OPTYPE_REGISTER | OPTYPE_UNDO;
 
	/* properties */
	RNA_def_enum(ot->srna, "type", slot_rot, 0, "Type", "");
}

static int image_layer_arbitrary_rot_exec(bContext *C, wmOperator *op)
{
	Image *ima = CTX_data_edit_image(C);
	ImageLayer *layer;
	ImBuf *ibuf;
	float angle;
	float col[4];
	short type;
	int lock;
	
	if (!ima)
		return OPERATOR_CANCELLED;
	
	ima->use_layers = FALSE;
	ibuf = BKE_image_acquire_ibuf(ima, NULL, NULL, IMA_IBUF_LAYER);
	ima->use_layers = TRUE;

	layer = imalayer_get_current(ima);
	if (!layer)
			return OPERATOR_CANCELLED;

	if (layer->preview_ibuf) {
		IMB_freeImBuf(layer->preview_ibuf);
		layer->preview_ibuf = NULL;
	}

	type = RNA_enum_get(op->ptr, "type");
	angle = RNA_float_get(op->ptr, "angle");
	lock = RNA_boolean_get(op->ptr, "lock_size");
	get_color_background_layer(col, layer);

	angle = angle * (-1);

	ibuf = (ImBuf *)layer->ibufs.first;
	layer->ibufs.first = NULL;
	layer->ibufs.last = NULL;
	BLI_addtail(&layer->ibufs, IMB_rotation(ibuf, 0.0, 0.0, angle, type, lock, col));

	BKE_image_release_ibuf(ima, ibuf, NULL);

	WM_event_add_notifier(C, NC_IMAGE | ND_DRAW, NULL);

	return OPERATOR_FINISHED;
}

static bool image_layer_arbitrary_rot_check(bContext *C, wmOperator *op)
{
	Image *ima = CTX_data_edit_image(C);
	ImageLayer *layer;
	ImBuf *ibuf;
	float angle;
	float col[4];
	short type;
	int lock;

	if (!ima)
		return FALSE;
	
	ima->use_layers = FALSE;
	ibuf = BKE_image_acquire_ibuf(ima, NULL, NULL, IMA_IBUF_LAYER);
	ima->use_layers = TRUE;

	layer = imalayer_get_current(ima);
	type = RNA_enum_get(op->ptr, "type");
	angle = RNA_float_get(op->ptr, "angle");
	lock = RNA_boolean_get(op->ptr, "lock_size");

	get_color_background_layer(col, layer);

	angle = angle * (-1);

	if (layer->preview_ibuf) {
		IMB_freeImBuf(layer->preview_ibuf);
		layer->preview_ibuf = NULL;
	}

	layer->preview_ibuf = IMB_dupImBuf(ibuf);

	layer->preview_ibuf = IMB_rotation(layer->preview_ibuf, 0.0, 0.0, angle, type, lock, col);
	
	BKE_image_release_ibuf(ima, ibuf, NULL);
	return TRUE;
}

void IMAGE_OT_layer_arbitrary_rot(wmOperatorType *ot)
{
	PropertyRNA *prop;
	
	static EnumPropertyItem rotate_items[] = {
		{0, "NEAREST", 0, "Nearest", ""},
		{1, "BILINEAR", 0, "Bilinear", ""},
		{2, "BICUBIC", 0, "Bicubic", ""},
		{0, NULL, 0, NULL, NULL}
	};

	/* identifiers */
	ot->name = "Arbitrary Rotating Layer";
	ot->idname = "IMAGE_OT_layer_arbitrary_rot";
	ot->description = "Arbitrary Rotate the Layer";
 
	/* api callbacks */
	ot->exec = image_layer_arbitrary_rot_exec;
	ot->poll = image_layer_poll;
	ot->invoke = image_op_layer_invoke;
	ot->check = image_layer_arbitrary_rot_check;
	ot->cancel = image_layer_preview_cancel;
 
	/* flags */
	ot->flag = OPTYPE_REGISTER | OPTYPE_UNDO;
 
	/* properties */
	RNA_def_enum(ot->srna, "type", rotate_items, 0, "Type", "");
	RNA_def_boolean(ot->srna, "lock_size", 1, "Lock size", "Lock size");
	prop = RNA_def_float_rotation(ot->srna, "angle", 0, NULL, DEG2RADF(-180.0f), DEG2RADF(180.0f),
	                              "Angle", "Angle of rotation", DEG2RADF(-180.0f), DEG2RADF(180.0f));
	RNA_def_property_float_default(prop, DEG2RADF(0.0f));
}

static int image_layer_offset_exec(bContext *C, wmOperator *op)
{
	Image *ima = CTX_data_edit_image(C);
	ImageLayer *layer;
	struct ImBuf *ibuf;
	int x, y, half, wrap;
	float col[4];
	
	if (!ima)
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

	if (!wrap)
		get_color_background_layer(col, layer);

	layer->ibufs.first = NULL;
	layer->ibufs.last = NULL;
	BLI_addtail(&layer->ibufs, IMB_offset(ibuf, x, y, half, wrap, col));

	WM_event_add_notifier(C, NC_IMAGE | ND_DRAW, NULL);
 
	return OPERATOR_FINISHED;
}

void IMAGE_OT_layer_offset(wmOperatorType *ot)
{
	/* identifiers */
	ot->name = "Offset Layer";
	ot->idname = "IMAGE_OT_layer_offset";
	ot->description = "Shift the pixels";
 
	/* api callbacks */
	ot->exec = image_layer_offset_exec;
	ot->poll = image_layer_poll;
	ot->invoke = image_op_layer_invoke;

	/* flags */
	ot->flag = OPTYPE_REGISTER | OPTYPE_UNDO;

	/* properties */
	RNA_def_int(ot->srna, "off_x", 0, INT_MIN, INT_MAX, "X", "Offset X", -16384, 16384);
	RNA_def_int(ot->srna, "off_y", 0, INT_MIN, INT_MAX, "Y", "Offset Y", -16384, 16384);
	RNA_def_boolean(ot->srna, "half", 0, "Offset by x/2 y/2", "Offset by x/2 y/2");
	RNA_def_boolean(ot->srna, "wrap", 1, "Wrap around", "Wrap around");
}

static int image_layer_scale_exec(bContext *C, wmOperator *op)
{
	Image *ima = CTX_data_edit_image(C);
	ImageLayer *layer;
	struct ImBuf *ibuf;
	int width, height, proportions;
	float props;
	
	if (!ima)
		return OPERATOR_CANCELLED;
	
	layer = imalayer_get_current(ima);
	if (!layer)
			return OPERATOR_CANCELLED;

	ibuf = (ImBuf *)layer->ibufs.first;
	if (!ibuf)
			return OPERATOR_CANCELLED;
	
	width = RNA_int_get(op->ptr, "width");
	height = RNA_int_get(op->ptr, "height");
	proportions = RNA_boolean_get(op->ptr, "proportions");

	if ((width == 0) && (height == 0))
		return OPERATOR_CANCELLED;

	if (proportions) {
		if ((width != 0) && (height != 0)) {
			BKE_report(op->reports, RPT_WARNING, "If you want to keep the proportions, the height or width must be 0.");
			return OPERATOR_CANCELLED;
		}

		if ((width == 0) || (height == 0)) {
			if (width == 0) {
				props = (float)ibuf->y / ibuf->x;
				width = (int)floor((float)height / props);
			}
			else {
				props = (float)ibuf->x / ibuf->y;
				height = (int)floor((float)width / props);
			}
		}
	}

	IMB_scaleImBuf(ibuf, width, height);

	WM_event_add_notifier(C, NC_IMAGE | ND_DRAW, NULL);
 
	return OPERATOR_FINISHED;
}

void IMAGE_OT_layer_scale(wmOperatorType *ot)
{
	/* identifiers */
	ot->name = "Scale Layer";
	ot->idname = "IMAGE_OT_layer_scale";
	ot->description = "Scale the layer";
 
	/* api callbacks */
	ot->exec = image_layer_scale_exec;
	ot->poll = image_layer_poll;
	ot->invoke = image_op_layer_invoke;

	/* flags */
	ot->flag = OPTYPE_REGISTER | OPTYPE_UNDO;

	/* properties */
	RNA_def_int(ot->srna, "width", 0, 0, INT_MAX, "Width", "Width", 0, 16384);
	RNA_def_int(ot->srna, "height", 0, 0, INT_MAX, "Height", "Height", 0, 16384);
	RNA_def_boolean(ot->srna, "proportions", 1, "Keep Proportions", "Keep proportions");
}

static int image_layer_size_exec(bContext *C, wmOperator *op)
{
	Image *ima = CTX_data_edit_image(C);
	ImageLayer *layer;
	struct ImBuf *ibuf;
	int width, height, off_x, off_y, proportions, centre;
	float props;
	float col[4];
	
	if (!ima)
		return OPERATOR_CANCELLED;
	
	layer = imalayer_get_current(ima);
	if (!layer)
			return OPERATOR_CANCELLED;

	ibuf = (ImBuf *)layer->ibufs.first;
	if (!ibuf)
			return OPERATOR_CANCELLED;
	
	width = RNA_int_get(op->ptr, "width");
	height = RNA_int_get(op->ptr, "height");
	proportions = RNA_boolean_get(op->ptr, "proportions");
	
	off_x = RNA_int_get(op->ptr, "off_x");
	off_y = RNA_int_get(op->ptr, "off_y");
	centre = RNA_boolean_get(op->ptr, "centre");

	if ((width == 0) && (height == 0))
		return OPERATOR_CANCELLED;

	if (proportions) {
		if ((width != 0) && (height != 0)) {
			BKE_report(op->reports, RPT_WARNING, "If you want to keep the proportions, the height or width must be 0.");
			return OPERATOR_CANCELLED;
		}

		if ((width == 0) || (height == 0)) {
			if (width == 0) {
				props = (float)ibuf->y / ibuf->x;
				width = (int)floor((float)height / props);
			}
			else {
				props = (float)ibuf->x / ibuf->y;
				height = (int)floor((float)width / props);
			}
		}
	}

	if (width > ibuf->x) {
		if (off_x < 0) {
			BKE_report(op->reports, RPT_WARNING, "The offset must be greater than 0.");
			return OPERATOR_CANCELLED;
		}
	}
	else {
		if (off_x > 0) {
			BKE_report(op->reports, RPT_WARNING, "The offset should be less than 0.");
			return OPERATOR_CANCELLED;
		}
	}

	if (height > ibuf->y) {
		if (off_y < 0) {
			BKE_report(op->reports, RPT_WARNING, "The offset must be greater than 0.");
			return OPERATOR_CANCELLED;
		}
	}
	else {
		if (off_y > 0) {
			BKE_report(op->reports, RPT_WARNING, "The offset should be less than 0.");
			return OPERATOR_CANCELLED;
		}
	}

	get_color_background_layer(col, layer);
	layer->ibufs.first = NULL;
	layer->ibufs.last = NULL;
	BLI_addtail(&layer->ibufs, IMB_size(ibuf, width, height, off_x, off_y, centre, col));

	WM_event_add_notifier(C, NC_IMAGE|ND_DRAW, NULL);
 
	return OPERATOR_FINISHED;
}

void IMAGE_OT_layer_size(wmOperatorType *ot)
{
	/* identifiers */
	ot->name = "Layer Boundary Size";
	ot->idname = "IMAGE_OT_layer_size";
	ot->description = "Adjust the layer dimensions";
 
	/* api callbacks */
	ot->exec = image_layer_size_exec;
	ot->poll = image_layer_poll;
	ot->invoke = image_op_layer_invoke;

	/* flags */
	ot->flag = OPTYPE_REGISTER | OPTYPE_UNDO;

	/* properties */
	RNA_def_int(ot->srna, "width", 0, 0, INT_MAX, "Width", "Width", 0, 16384);
	RNA_def_int(ot->srna, "height", 0, 0, INT_MAX, "Height", "Height", 0, 16384);
	RNA_def_boolean(ot->srna, "proportions", 1, "Keep Proportions", "Keep proportions");
	RNA_def_int(ot->srna, "off_x", 0, INT_MIN, INT_MAX, "Offset X", "Offset X", -16384, 16384);
	RNA_def_int(ot->srna, "off_y", 0, INT_MIN, INT_MAX, "Offset Y", "Offset Y", -16384, 16384);
	RNA_def_boolean(ot->srna, "centre", 0, "Centre", "Centre");
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
	ImBuf *ibuf = BKE_image_acquire_ibuf(ima, NULL, NULL, IMA_IBUF_IMA);
	int as_png = RNA_boolean_get(op->ptr, "as_png");

	if (!image_pack_test(C, op))
		return OPERATOR_CANCELLED;
	
	if (!as_png && (ibuf && (ibuf->userflags & IB_BITMAPDIRTY))) {
		BKE_report(op->reports, RPT_ERROR, "Cannot pack edited image from disk, only as internal PNG");
		return OPERATOR_CANCELLED;
	}

	if (as_png)
		BKE_image_memorypack(ima);
	else
		ima->packedfile = newPackedFile(op->reports, ima->name, ID_BLEND_PATH(bmain, &ima->id));

	WM_event_add_notifier(C, NC_IMAGE | NA_EDITED, ima);

	BKE_image_release_ibuf(ima, ibuf, NULL);

	return OPERATOR_FINISHED;
}

static int image_pack_invoke(bContext *C, wmOperator *op, const wmEvent *UNUSED(event))
{
	Image *ima = CTX_data_edit_image(C);
	ImBuf *ibuf;
	uiPopupMenu *pup;
	uiLayout *layout;
	int as_png = RNA_boolean_get(op->ptr, "as_png");

	if (!image_pack_test(C, op))
		return OPERATOR_CANCELLED;

	ibuf = BKE_image_acquire_ibuf(ima, NULL, NULL, IMA_IBUF_IMA);

	if (!as_png && (ibuf && (ibuf->userflags & IB_BITMAPDIRTY))) {
		pup = uiPupMenuBegin(C, IFACE_("OK"), ICON_QUESTION);
		layout = uiPupMenuLayout(pup);
		uiItemBooleanO(layout, IFACE_("Can't pack edited image from disk, pack as internal PNG?"), ICON_NONE,
		               op->idname, "as_png", 1);
		uiPupMenuEnd(C, pup);

		BKE_image_release_ibuf(ima, ibuf, NULL);

		return OPERATOR_CANCELLED;
	}

	BKE_image_release_ibuf(ima, ibuf, NULL);

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

static int image_unpack_invoke(bContext *C, wmOperator *op, const wmEvent *UNUSED(event))
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
	float linearcol[4];
	int z;
	float zf;

	unsigned char *colp;
	float *colfp;
	int *zp;
	float *zfp;

	int draw;
	int color_manage;
	int use_default_view;
} ImageSampleInfo;

static void image_sample_draw(const bContext *C, ARegion *ar, void *arg_info)
{
	ImageSampleInfo *info = arg_info;
	if (info->draw) {
		Scene *scene = CTX_data_scene(C);

		ED_image_draw_info(scene, ar, info->color_manage, info->use_default_view, info->channels,
		                   info->x, info->y, info->colp, info->colfp, info->linearcol, info->zp, info->zfp, 1);
	}
}

/* returns color in SRGB */
/* matching ED_space_node_color_sample() */
int ED_space_image_color_sample(SpaceImage *sima, ARegion *ar, int mval[2], float r_col[3])
{
	void *lock;
	ImBuf *ibuf = ED_space_image_acquire_buffer(sima, &lock);
	float fx, fy;
	int ret = FALSE;

	if (ibuf == NULL) {
		ED_space_image_release_buffer(sima, ibuf, lock);
		return FALSE;
	}

	UI_view2d_region_to_view(&ar->v2d, mval[0], mval[1], &fx, &fy);

	if (fx >= 0.0f && fy >= 0.0f && fx < 1.0f && fy < 1.0f) {
		float *fp;
		unsigned char *cp;
		int x = (int)(fx * ibuf->x), y = (int)(fy * ibuf->y);

		CLAMP(x, 0, ibuf->x - 1);
		CLAMP(y, 0, ibuf->y - 1);

		if (ibuf->rect_float) {
			fp = (ibuf->rect_float + (ibuf->channels) * (y * ibuf->x + x));
			linearrgb_to_srgb_v3_v3(r_col, fp);
			ret = TRUE;
		}
		else if (ibuf->rect) {
			cp = (unsigned char *)(ibuf->rect + y * ibuf->x + x);
			rgb_uchar_to_float(r_col, cp);
			ret = TRUE;
		}
	}

	ED_space_image_release_buffer(sima, ibuf, lock);
	return ret;
}

static void image_sample_apply(bContext *C, wmOperator *op, const wmEvent *event)
{
	SpaceImage *sima = CTX_wm_space_image(C);
	ARegion *ar = CTX_wm_region(C);
	void *lock;
	ImBuf *ibuf = ED_space_image_acquire_buffer(sima, &lock);
	ImageSampleInfo *info = op->customdata;
	float fx, fy;
	Scene *scene = CTX_data_scene(C);
	CurveMapping *curve_mapping = scene->view_settings.curve_mapping;

	if (ibuf == NULL) {
		ED_space_image_release_buffer(sima, ibuf, lock);
		info->draw = 0;
		return;
	}

	//WM_cursor_modal(win, BC_EYEDROPPER_CURSOR);
	UI_view2d_region_to_view(&ar->v2d, event->mval[0], event->mval[1], &fx, &fy);

	if (fx >= 0.0f && fy >= 0.0f && fx < 1.0f && fy < 1.0f) {
		float *fp;
		unsigned char *cp;
		int x = (int)(fx * ibuf->x), y = (int)(fy * ibuf->y);
		Image *image = ED_space_image(sima);

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

		info->use_default_view = (image->flag & IMA_VIEW_AS_RENDER) ? FALSE : TRUE;

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

			copy_v4_v4(info->linearcol, info->colf);
			IMB_colormanagement_colorspace_to_scene_linear_v4(info->linearcol, false, ibuf->rect_colorspace);

			info->color_manage = TRUE;
		}
		if (ibuf->rect_float) {
			fp = (ibuf->rect_float + (ibuf->channels) * (y * ibuf->x + x));

			info->colf[0] = fp[0];
			info->colf[1] = fp[1];
			info->colf[2] = fp[2];
			info->colf[3] = fp[3];
			info->colfp = info->colf;

			copy_v4_v4(info->linearcol, info->colf);

			info->color_manage = TRUE;
		}

		if (ibuf->zbuf) {
			info->z = ibuf->zbuf[y * ibuf->x + x];
			info->zp = &info->z;
		}
		if (ibuf->zbuf_float) {
			info->zf = ibuf->zbuf_float[y * ibuf->x + x];
			info->zfp = &info->zf;
		}

		if (curve_mapping && ibuf->channels == 4) {
			/* we reuse this callback for set curves point operators */
			if (RNA_struct_find_property(op->ptr, "point")) {
				int point = RNA_enum_get(op->ptr, "point");

				if (point == 1) {
					curvemapping_set_black_white(curve_mapping, NULL, info->colfp);
				}
				else if (point == 0) {
					curvemapping_set_black_white(curve_mapping, info->colfp, NULL);
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

	ED_space_image_release_buffer(sima, ibuf, lock);
	ED_area_tag_redraw(CTX_wm_area(C));
}

static void image_sample_exit(bContext *C, wmOperator *op)
{
	ImageSampleInfo *info = op->customdata;

	ED_region_draw_cb_exit(info->art, info->draw_handle);
	ED_area_tag_redraw(CTX_wm_area(C));
	MEM_freeN(info);
}

static int image_sample_invoke(bContext *C, wmOperator *op, const wmEvent *event)
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

static int image_sample_modal(bContext *C, wmOperator *op, const wmEvent *event)
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

static void image_sample_cancel(bContext *C, wmOperator *op)
{
	image_sample_exit(C, op);
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
	ot->poll = image_sample_poll;

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
		ED_space_image_release_buffer(sima, ibuf, lock);
		return OPERATOR_CANCELLED;
	}
	/* hmmmm */
	if (ibuf->channels < 3) {
		ED_space_image_release_buffer(sima, ibuf, lock);
		return OPERATOR_CANCELLED;
	}
	
	UI_view2d_region_to_view(&ar->v2d, x_start, y_start, &x1f, &y1f);
	UI_view2d_region_to_view(&ar->v2d, x_end, y_end, &x2f, &y2f);

	hist->co[0][0] = x1f;
	hist->co[0][1] = y1f;
	hist->co[1][0] = x2f;
	hist->co[1][1] = y2f;

	/* enable line drawing */
	hist->flag |= HISTO_FLAG_SAMPLELINE;

	BKE_histogram_update_sample_line(hist, ibuf, &scene->view_settings, &scene->display_settings);
	
	/* reset y zoom */
	hist->ymax = 1.0f;

	ED_space_image_release_buffer(sima, ibuf, lock);
	
	ED_area_tag_redraw(CTX_wm_area(C));
	
	return OPERATOR_FINISHED;
}

static int image_sample_line_invoke(bContext *C, wmOperator *op, const wmEvent *event)
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

#if 0 /* Not ported to 2.5x yet */
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
	
	WM_cursor_time(CTX_wm_window(C), scene->r.cfra);

	// XXX scene->nodetree->test_break = blender_test_break;
	// XXX scene->nodetree->test_break = NULL;
	
	BKE_image_all_free_anim_ibufs(scene->r.cfra);
	ntreeCompositTagAnimated(scene->nodetree);
	ntreeCompositExecTree(scene->nodetree, &scene->r, 0, scene->r.cfra != rcd->old_cfra,
	                      &scene->view_settings, &scene->display_settings);  /* 1 is no previews */

	ED_area_tag_redraw(CTX_wm_area(C));
	
	ibuf = BKE_image_acquire_ibuf(sima->image, &sima->iuser, NULL, IMA_IBUF_IMA);
	/* save memory in flipbooks */
	if (ibuf)
		imb_freerectfloatImBuf(ibuf);

	BKE_image_release_ibuf(sima->image, ibuf, NULL);

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

	WM_cursor_modal_restore(CTX_wm_window(C));

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
	
	while (image_record_composite_apply(C, op)) {}
	
	image_record_composite_exit(C, op);
	
	return OPERATOR_FINISHED;
}

static int image_record_composite_invoke(bContext *C, wmOperator *op, const wmEvent *UNUSED(event))
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

static int image_record_composite_modal(bContext *C, wmOperator *op, const wmEvent *event)
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

static void image_record_composite_cancel(bContext *C, wmOperator *op)
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

#endif

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
