# BEGIN GPL LICENSE BLOCK #####
#
#  This program is free software; you can redistribute it and/or
#  modify it under the terms of the GNU General Public License
#  as published by the Free Software Foundation; either version 2
#  of the License, or (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software Foundation,
#  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
# END GPL LICENSE BLOCK #####


bl_info = {
    "name": "Texture Atlas",
    "author": "Andreas Esau, Paul Geraskin, Campbell Barton",
    "version": (0, 2, 0),
    "blender": (2, 6, 7),
    "location": "Properties > Render",
    "description": "A simple Texture Atlas for unwrapping many objects. It creates additional UV",
    "wiki_url": "http://wiki.blender.org/index.php/Extensions:2.6/Py/Scripts/UV/TextureAtlas",
    "tracker_url": "http://projects.blender.org/scm/viewvc.php/trunk/py/scripts/addons/uv_texture_atlas.py?view=log&root=bf-extensions",
    "category": "UV"}

import bpy
from bpy.types import (Operator,
                       Panel,
                       PropertyGroup,
                       )

from bpy.props import (BoolProperty,
                       CollectionProperty,
                       EnumProperty,
                       FloatProperty,
                       IntProperty,
                       StringProperty,
                       )
import mathutils


def check_all_objects_visible(self, context):
    scene = context.scene
    group = scene.ms_lightmap_groups[scene.ms_lightmap_groups_index]
    isAllObjectsVisible = True
    bpy.ops.object.select_all(action='DESELECT')
    for thisObject in bpy.data.groups[group.name].objects:
        isThisObjectVisible = False
        # scene.objects.active = thisObject
        for thisLayerNumb in range(20):
            if thisObject.layers[thisLayerNumb] is True and scene.layers[thisLayerNumb] is True:
                isThisObjectVisible = True
                break
        # If Object is on an invisible Layer
        if isThisObjectVisible is False:
            isAllObjectsVisible = False
    return isAllObjectsVisible


def check_group_exist(self, context, use_report=True):
    scene = context.scene
    group = scene.ms_lightmap_groups[scene.ms_lightmap_groups_index]

    if group.name in bpy.data.groups:
        return True
    else:
        if use_report:
            self.report({'INFO'}, "No Such Group %r!" % group.name)
        return False


class TextureAtlas(Panel):
    bl_label = "Texture Atlas"
    bl_space_type = 'PROPERTIES'
    bl_region_type = 'WINDOW'
    bl_context = "render"
    COMPAT_ENGINES = {'BLENDER_RENDER'}

    def draw(self, context):
        scene = context.scene
        ob = context.object

        col = self.layout.column()
        row = self.layout.row()
        split = self.layout.split()

        row.template_list("UI_UL_list", "template_list_controls", scene,
                          "ms_lightmap_groups", scene, "ms_lightmap_groups_index", rows=2, maxrows=5)
        col = row.column(align=True)
        col.operator("scene.ms_add_lightmap_group", icon='ZOOMIN', text="")
        col.operator("scene.ms_del_lightmap_group", icon='ZOOMOUT', text="")

        row = self.layout.row(align=True)

        # Resolution and Unwrap types (only if Lightmap group is added)
        if context.scene.ms_lightmap_groups:
            group = scene.ms_lightmap_groups[scene.ms_lightmap_groups_index]
            row.prop(group, 'resolution', text='Resolution', expand=True)
            row = self.layout.row()
            row.prop(group, 'unwrap_type', text='Lightmap', expand=True)
            row = self.layout.row()

            row = self.layout.row()
            row.operator("scene.ms_remove_other_uv",
                         text="RemoveOtherUVs", icon="GROUP")
            row.operator("scene.ms_remove_selected",
                         text="RemoveSelected", icon="GROUP")
            row = self.layout.row()
            row = self.layout.row()
            row = self.layout.row()
            row.operator("scene.ms_add_selected_to_group",
                         text="AddSelected", icon="GROUP")
            row.operator("scene.ms_select_group",
                         text="SelectGroup", icon="GROUP")

            row = self.layout.row()
            row.operator(
                "object.ms_auto", text="Auto Unwrap", icon="LAMP_SPOT")
            row = self.layout.row()
            row.operator(
                "object.ms_run", text="StartManualUnwrap", icon="LAMP_SPOT")
            row.operator(
                "object.ms_run_remove", text="FinshManualUnwrap", icon="LAMP_SPOT")


class RunAuto(Operator):
    bl_idname = "object.ms_auto"
    bl_label = "Auto Unwrapping"
    bl_description = "Auto Unwrapping"

    def execute(self, context):
        scene = context.scene
        old_context = context.area.type

        # Check if group exists
        if check_group_exist(self, context) is False:
            return {'CANCELLED'}

        group = scene.ms_lightmap_groups[scene.ms_lightmap_groups_index]
        context.area.type = 'VIEW_3D'
        if scene.objects.active is not None:
            bpy.ops.object.mode_set(mode='OBJECT', toggle=False)

        if group.bake is True and bpy.data.groups[group.name].objects:

            # Check if objects are all on the visible Layers.
            isAllObjVisible = check_all_objects_visible(self, context)

            if isAllObjVisible is True:
                res = int(group.resolution)
                bpy.ops.object.ms_create_lightmap(
                    group_name=group.name, resolution=res)
                bpy.ops.object.ms_merge_objects(
                    group_name=group.name, unwrap=True)
                bpy.ops.object.ms_separate_objects(group_name=group.name)
            else:
                self.report({'INFO'}, "Not All Objects Are Visible!!!")

        context.area.type = old_context

        return{'FINISHED'}


class RunStart(Operator):
    bl_idname = "object.ms_run"
    bl_label = "Make Manual Unwrapping Object"
    bl_description = "Makes Manual Unwrapping Object"

    def execute(self, context):
        scene = context.scene
        # old_context = context.area.type

        # Check if group exists
        if check_group_exist(self, context) is False:
            return {'CANCELLED'}

        group = scene.ms_lightmap_groups[scene.ms_lightmap_groups_index]

        if scene.objects.active is not None:
            bpy.ops.object.mode_set(mode='OBJECT', toggle=False)

        if group.bake is True and bpy.data.groups[group.name].objects and bpy.data.objects.get(group.name + "_mergedObject") is None:

            # Check if objects are all on the visible Layers.
            isAllObjVisible = check_all_objects_visible(self, context)

            if isAllObjVisible is True:
                res = int(group.resolution)
                bpy.ops.object.ms_create_lightmap(
                    group_name=group.name, resolution=res)
                bpy.ops.object.ms_merge_objects(
                    group_name=group.name, unwrap=False)
            else:
                self.report({'INFO'}, "Not All Objects Are Visible!!!")

        return{'FINISHED'}


class RunFinish(Operator):
    bl_idname = "object.ms_run_remove"
    bl_label = "Remove Manual Unwrapping Object"
    bl_description = "Removes Manual Unwrapping Object"

    def execute(self, context):
        scene = context.scene
        old_context = context.area.type

        # Check if group exists
        if check_group_exist(self, context) is False:
            return {'CANCELLED'}

        group = scene.ms_lightmap_groups[scene.ms_lightmap_groups_index]
        context.area.type = 'VIEW_3D'

        if scene.objects.active is not None:
            bpy.ops.object.mode_set(mode='OBJECT', toggle=False)

        if group.bake is True and bpy.data.groups[group.name].objects:

            # Check if objects are all on the visible Layers.
            isAllObjVisible = check_all_objects_visible(self, context)

            if isAllObjVisible is True:
                bpy.ops.object.ms_separate_objects(group_name=group.name)
            else:
                self.report({'INFO'}, "Not All Objects Are Visible!!!")

        context.area.type = old_context
        return{'FINISHED'}


class MSUVLayers(PropertyGroup):
    name = StringProperty(default="")


class MSVertexGroups(PropertyGroup):
    name = StringProperty(default="")


class MSGroups(PropertyGroup):
    name = StringProperty(default="")


class MSLightmapGroups(PropertyGroup):

    name = StringProperty(default="")
    bake = BoolProperty(default=True)

    unwrap_type = EnumProperty(
        name="unwrap_type",
        items=(('0', 'Smart_Unwrap', 'Smart_Unwrap'),
               ('1', 'Lightmap', 'Lightmap'),
               ('2', 'No_Unwrap', 'No_Unwrap'),
               ),
    )
    resolution = EnumProperty(
        name="resolution",
        items=(('256', '256', ''),
               ('512', '512', ''),
               ('1024', '1024', ''),
               ('2048', '2048', ''),
               ('4096', '4096', ''),
               ('8192', '8192', ''),
               ('16384', '16384', ''),
               ),
    )
    template_list_controls = StringProperty(
        default="bake",
        options={"HIDDEN"},
    )


class MSMergedObjects(PropertyGroup):
    name = StringProperty()
    vertex_groups = CollectionProperty(
        type=MSVertexGroups,
    )
    groups = CollectionProperty(type=MSGroups)
    uv_layers = CollectionProperty(type=MSUVLayers)


class AddSelectedToGroup(Operator):
    bl_idname = "scene.ms_add_selected_to_group"
    bl_label = "Add to Group"
    bl_description = "Adds selected Objects to current Group"

    def execute(self, context):
        scene = context.scene
        group_name = scene.ms_lightmap_groups[
            scene.ms_lightmap_groups_index].name

        # Create a New Group if it was deleted.
        obj_group = bpy.data.groups.get(group_name)
        if obj_group is None:
            obj_group = bpy.data.groups.new(group_name)

        # Add objects to  a group
        if scene.objects.active is not None:
            bpy.ops.object.mode_set(mode='OBJECT', toggle=False)

        for object in context.selected_objects:
            if object.type == 'MESH' and object.name not in obj_group.objects:
                obj_group.objects.link(object)

        return {'FINISHED'}


class SelectGroup(Operator):
    bl_idname = "scene.ms_select_group"
    bl_label = "sel Group"
    bl_description = "Selected Objects of current Group"

    def execute(self, context):
        scene = context.scene
        group_name = scene.ms_lightmap_groups[
            scene.ms_lightmap_groups_index].name

        # Check if group exists
        if check_group_exist(self, context) is False:
            return {'CANCELLED'}

        if scene.objects.active is not None:
            bpy.ops.object.mode_set(mode='OBJECT', toggle=False)
        bpy.ops.object.select_all(action='DESELECT')
        obj_group = bpy.data.groups[group_name]
        for object in obj_group.objects:
            object.select = True
        return {'FINISHED'}


class RemoveFromGroup(Operator):
    bl_idname = "scene.ms_remove_selected"
    bl_label = "del Selected"
    bl_description = "Remove Selected Group and UVs"

        # remove all modifiers
        # for m in mesh.modifiers:
            # bpy.ops.object.modifier_remove(modifier=m.name)

    def execute(self, context):
        scene = context.scene

        # Check if group exists
        if check_group_exist(self, context) is False:
            return {'CANCELLED'}

        if scene.objects.active is not None:
            bpy.ops.object.mode_set(mode='OBJECT', toggle=False)

        for group in scene.ms_lightmap_groups:
            group_name = group.name

            obj_group = bpy.data.groups[group_name]
            for object in context.selected_objects:
                scene.objects.active = object

                if object.type == 'MESH' and object.name in obj_group.objects:

                    # remove UV
                    tex = object.data.uv_textures.get(group_name)
                    if tex is not None:
                        object.data.uv_textures.remove(tex)

                    # remove from group
                    obj_group.objects.unlink(object)
                    object.hide_render = False

        return {'FINISHED'}


class RemoveOtherUVs(Operator):
    bl_idname = "scene.ms_remove_other_uv"
    bl_label = "remOther"
    bl_description = "Remove Other UVs from Selected"

    def execute(self, context):
        scene = context.scene
        group_name = scene.ms_lightmap_groups[
            scene.ms_lightmap_groups_index].name

        # Check if group exists
        if check_group_exist(self, context) is False:
            return {'CANCELLED'}

        if scene.objects.active is not None:
            bpy.ops.object.mode_set(mode='OBJECT', toggle=False)
        # bpy.ops.object.select_all(action='DESELECT')

        obj_group = bpy.data.groups[group_name]

        # Remove other UVs of selected objects
        for object in context.selected_objects:
            scene.objects.active = object
            if object.type == 'MESH' and object.name in obj_group.objects:

                # remove UVs
                UVLIST = []
                for uv in object.data.uv_textures:
                    if uv.name != group_name:
                        UVLIST.append(uv.name)

                for uvName in UVLIST:
                    tex = object.data.uv_textures[uvName]
                    object.data.uv_textures.remove(tex)

                UVLIST.clear()  # clear array

        return {'FINISHED'}


class AddLightmapGroup(Operator):
    bl_idname = "scene.ms_add_lightmap_group"
    bl_label = "add Lightmap"
    bl_description = "Adds a new Lightmap Group"

    name = StringProperty(name="Group Name", default='TextureAtlas')

    def execute(self, context):
        scene = context.scene
        obj_group = bpy.data.groups.new(self.name)

        item = scene.ms_lightmap_groups.add()
        item.name = obj_group.name
        item.resolution = '1024'
        scene.ms_lightmap_groups_index = len(scene.ms_lightmap_groups) - 1

        # Add selested objects to group
        for object in context.selected_objects:
            if object.type == 'MESH':
                obj_group.objects.link(object)

        return {'FINISHED'}

    def invoke(self, context, event):
        wm = context.window_manager
        return wm.invoke_props_dialog(self)


class DelLightmapGroup(Operator):
    bl_idname = "scene.ms_del_lightmap_group"
    bl_label = "delete Lightmap"
    bl_description = "Deletes active Lightmap Group"

    def execute(self, context):
        scene = context.scene
        if len(scene.ms_lightmap_groups) > 0:
            idx = scene.ms_lightmap_groups_index
            group_name = scene.ms_lightmap_groups[idx].name

            # Remove Group
            group = bpy.data.groups.get(group_name)
            if group is not None:

                # Unhide Objects if they are hidden
                for obj in group.objects:
                    obj.hide_render = False
                    obj.hide = False

                bpy.data.groups.remove(group)

            # Remove Lightmap Group
            scene.ms_lightmap_groups.remove(scene.ms_lightmap_groups_index)
            scene.ms_lightmap_groups_index -= 1
            if scene.ms_lightmap_groups_index < 0:
                scene.ms_lightmap_groups_index = 0

        return {'FINISHED'}


class CreateLightmap(Operator):
    bl_idname = "object.ms_create_lightmap"
    bl_label = "TextureAtlas - Generate Lightmap"
    bl_description = "Generates a Lightmap"

    group_name = StringProperty(default='')
    resolution = IntProperty(default=1024)

    def execute(self, context):
        scene = context.scene

        # Create/Update Image
        image = bpy.data.images.get(self.group_name)
        if image is None:
            image = bpy.data.images.new(
                name=self.group_name, width=self.resolution, height=self.resolution)

        image.generated_type = 'COLOR_GRID'
        image.generated_width = self.resolution
        image.generated_height = self.resolution
        obj_group = bpy.data.groups[self.group_name]

        # non MESH objects for removal list
        NON_MESH_LIST = []

        for object in obj_group.objects:
            # Remove non MESH objects
            if object.type != 'MESH':
                NON_MESH_LIST.append(object)

            else:
                # Add Image to faces
                if object.data.uv_textures.active is None:
                    tex = object.data.uv_textures.new()
                    tex.name = self.group_name
                else:
                    if self.group_name not in object.data.uv_textures:
                        tex = object.data.uv_textures.new()
                        tex.name = self.group_name
                        tex.active = True
                        tex.active_render = True
                    else:
                        tex = object.data.uv_textures[self.group_name]
                        tex.active = True
                        tex.active_render = True

                for face_tex in tex.data:
                    face_tex.image = image

        # remove non NESH objects
        for object in NON_MESH_LIST:
            obj_group.objects.unlink(object)

        NON_MESH_LIST.clear()  # clear array

        return{'FINISHED'}


class MergeObjects(Operator):
    bl_idname = "object.ms_merge_objects"
    bl_label = "TextureAtlas - MergeObjects"
    bl_description = "Merges Objects and stores Origins"

    group_name = StringProperty(default='')
    unwrap = BoolProperty(default=False)

    def execute(self, context):
        scene = context.scene

        # objToDelete = None
        bpy.ops.object.select_all(action='DESELECT')
        for obj in scene.objects:
            if obj.name == self.group_name + "_mergedObject":
                obj.select = True
                scene.objects.active = obj
                bpy.ops.object.delete(use_global=False)

        me = bpy.data.meshes.new(self.group_name + '_mergedObject')
        ob_merge = bpy.data.objects.new(self.group_name + '_mergedObject', me)
        ob_merge.location = scene.cursor_location   # position object at 3d-cursor
        scene.objects.link(ob_merge)                # Link object to scene
        me.update()
        ob_merge.select = False

        activeNowObject = bpy.data.groups[self.group_name].objects[0]
        bpy.ops.object.select_all(action='DESELECT')

        OBJECTLIST = bpy.data.groups[self.group_name].objects[:]
        for obj in OBJECTLIST:
            obj.select = True
        scene.objects.active = activeNowObject

        # Make Object Single User
        # bpy.ops.object.make_single_user(type='SELECTED_OBJECTS', object=True,
        # obdata=True, material=False, texture=False, animation=False)
        for object in OBJECTLIST:

            bpy.ops.object.select_all(action='DESELECT')
            object.select = True

            # activate lightmap uv if existant
            for uv in object.data.uv_textures:
                if uv.name == self.group_name:
                    uv.active = True
                    scene.objects.active = object

            # generate temp Duplicate Objects with copied modifier,properties
            # and logic bricks
            bpy.ops.object.select_all(action='DESELECT')
            object.select = True
            scene.objects.active = object
            bpy.ops.object.duplicate(linked=False, mode='TRANSLATION')
            activeNowObject = scene.objects.active
            activeNowObject.select = True

            # hide render of original mesh
            object.hide_render = True
            object.hide = True
            object.select = False

            # remove unused UV
            # remove UVs
            UVLIST = []
            for uv in activeNowObject.data.uv_textures:
                if uv.name != self.group_name:
                    UVLIST.append(uv.name)

            for uvName in UVLIST:
                tex = activeNowObject.data.uv_textures[uvName]
                activeNowObject.data.uv_textures.remove(tex)

            UVLIST.clear()  # clear array

            # create vertex groups for each selected object
            scene.objects.active = activeNowObject
            vgroup = activeNowObject.vertex_groups.new(name=object.name)
            vgroup.add(
                list(range(len(activeNowObject.data.vertices))), weight=1.0, type='ADD')

            # save object name and object location in merged object
            item = ob_merge.ms_merged_objects.add()
            item.name = object.name

            # Add material to a tempObject if there are no materialSlots on the object
            if not activeNowObject.data.materials:
                matName = "zz_TextureAtlas_NO_Material"
                mat = bpy.data.materials.get(matName)

                if mat is None:
                    mat = bpy.data.materials.new(matName)

                activeNowObject.data.materials.append(mat)

            # merge objects together
            bpy.ops.object.select_all(action='DESELECT')
            activeNowObject.select = True
            ob_merge.select = True
            scene.objects.active = ob_merge
            bpy.ops.object.join()

        OBJECTLIST.clear()  # clear array

        # make Unwrap
        bpy.ops.object.select_all(action='DESELECT')
        ob_merge.select = True
        scene.objects.active = ob_merge

        if self.unwrap is True:
            unwrapType = scene.ms_lightmap_groups[self.group_name].unwrap_type

            if unwrapType == '0' or unwrapType == '1':
                bpy.ops.object.mode_set(mode='EDIT')
                bpy.ops.mesh.select_all(action='SELECT')

            if unwrapType == '0':
                bpy.ops.uv.smart_project(
                    angle_limit=72.0, island_margin=0.2, user_area_weight=0.0)
            elif unwrapType == '1':
                bpy.ops.uv.lightmap_pack(
                    PREF_CONTEXT='ALL_FACES', PREF_PACK_IN_ONE=True, PREF_NEW_UVLAYER=False,
                    PREF_APPLY_IMAGE=False, PREF_IMG_PX_SIZE=1024, PREF_BOX_DIV=48, PREF_MARGIN_DIV=0.2)
            bpy.ops.object.mode_set(mode='OBJECT', toggle=False)

        return{'FINISHED'}


class SeparateObjects(Operator):
    bl_idname = "object.ms_separate_objects"
    bl_label = "TextureAtlas - Separate Objects"
    bl_description = "Separates Objects and restores Origin"

    group_name = StringProperty(default='')

    def execute(self, context):
        scene = context.scene

        for obj in scene.objects:
            if obj.name == self.group_name + "_mergedObject":

                # if scene.objects.active is not None:
                    # bpy.ops.object.mode_set(mode='OBJECT', toggle=False)
                bpy.ops.object.select_all(action='DESELECT')
                ob_merged = obj
                obj.hide = False
                ob_merged.select = True
                groupSeparate = bpy.data.groups.new(ob_merged.name)
                groupSeparate.objects.link(ob_merged)
                ob_merged.select = False

                for ms_obj in ob_merged.ms_merged_objects:
                    # select vertex groups and separate group from merged
                    # object
                    bpy.ops.object.select_all(action='DESELECT')
                    ob_merged.select = True
                    scene.objects.active = ob_merged

                    bpy.ops.object.mode_set(mode='EDIT')
                    bpy.ops.mesh.select_all(action='DESELECT')
                    ob_merged.vertex_groups.active_index = ob_merged.vertex_groups[
                        ms_obj.name].index
                    bpy.ops.object.vertex_group_select()
                    bpy.ops.mesh.separate(type='SELECTED')
                    bpy.ops.object.mode_set(mode='OBJECT', toggle=False)
                    # scene.objects.active.select = False

                    # find separeted object
                    ob_separeted = None
                    for obj in groupSeparate.objects:
                        if obj != ob_merged:
                            ob_separeted = obj
                            break

                    # Copy UV Coordinates to the original mesh
                    if ms_obj.name in scene.objects:
                        ob_merged.select = False
                        ob_original = scene.objects[ms_obj.name]
                        ob_original.hide = False
                        ob_original.select = True
                        scene.objects.active = ob_separeted
                        bpy.ops.object.join_uvs()
                        ob_original.hide_render = False

                    # delete separeted object
                    bpy.ops.object.select_all(action='DESELECT')
                    ob_separeted.select = True
                    bpy.ops.object.delete(use_global=False)

                # delete duplicated object
                bpy.ops.object.select_all(action='DESELECT')
                ob_merged.select = True
                bpy.ops.object.delete(use_global=False)

        return{'FINISHED'}


def register():
    bpy.utils.register_class(TextureAtlas)

    bpy.utils.register_class(AddLightmapGroup)
    bpy.utils.register_class(DelLightmapGroup)
    bpy.utils.register_class(AddSelectedToGroup)
    bpy.utils.register_class(SelectGroup)
    bpy.utils.register_class(RemoveFromGroup)
    bpy.utils.register_class(RemoveOtherUVs)

    bpy.utils.register_class(RunAuto)
    bpy.utils.register_class(RunStart)
    bpy.utils.register_class(RunFinish)
    bpy.utils.register_class(MergeObjects)
    bpy.utils.register_class(SeparateObjects)
    bpy.utils.register_class(CreateLightmap)

    # types
    bpy.utils.register_class(MSUVLayers)
    bpy.utils.register_class(MSVertexGroups)
    bpy.utils.register_class(MSGroups)

    bpy.utils.register_class(MSMergedObjects)
    bpy.types.Object.ms_merged_objects = CollectionProperty(
        type=MSMergedObjects)

    bpy.utils.register_class(MSLightmapGroups)
    bpy.types.Scene.ms_lightmap_groups = CollectionProperty(
        type=MSLightmapGroups)
    bpy.types.Scene.ms_lightmap_groups_index = IntProperty()


def unregister():
    bpy.utils.unregister_class(TextureAtlas)

    bpy.utils.unregister_class(AddLightmapGroup)
    bpy.utils.unregister_class(DelLightmapGroup)
    bpy.utils.unregister_class(AddSelectedToGroup)
    bpy.utils.unregister_class(SelectGroup)
    bpy.utils.unregister_class(RemoveFromGroup)
    bpy.utils.unregister_class(RemoveOtherUVs)

    bpy.utils.unregister_class(RunAuto)
    bpy.utils.unregister_class(RunStart)
    bpy.utils.unregister_class(RunFinish)
    bpy.utils.unregister_class(MergeObjects)
    bpy.utils.unregister_class(SeparateObjects)
    bpy.utils.unregister_class(CreateLightmap)

    # types
    bpy.utils.unregister_class(MSUVLayers)
    bpy.utils.unregister_class(MSVertexGroups)
    bpy.utils.unregister_class(MSGroups)

    bpy.utils.unregister_class(MSMergedObjects)

    bpy.utils.unregister_class(MSLightmapGroups)


if __name__ == "__main__":
    register()