#include "clisp.h"

extern object module__ffiogre__object_tab[];

subr_t module__ffiogre__subr_tab[1];
uintC module__ffiogre__subr_tab_size = 0;
subr_initdata_t module__ffiogre__subr_tab_initdata[1];

object module__ffiogre__object_tab[1];
object_initdata_t module__ffiogre__object_tab_initdata[1];
uintC module__ffiogre__object_tab_size = 0;


void module__ffiogre__init_function_1 (module_t* module);

void module__ffiogre__init_function_2 (module_t* module);

void module__ffiogre__fini_function (module_t* module);


void module__ffiogre__init_function_1 (module_t* module)
{
}

void module__ffiogre__init_function_2 (module_t* module)
{
  register_foreign_function((void*)&get_scene_manager,"get_scene_manager",1024);
  register_foreign_function((void*)&create_scene_manager,"create_scene_manager",1024);
  register_foreign_function((void*)&get_light,"get_light",1024);
  register_foreign_function((void*)&get_root_scene_node,"get_root_scene_node",1024);
  register_foreign_function((void*)&create_child_scene_node,"create_child_scene_node",1024);
  register_foreign_function((void*)&attach_object,"attach_object",1024);
  register_foreign_function((void*)&create_camera,"create_camera",1024);
  register_foreign_function((void*)&set_ambient_light,"set_ambient_light",1024);
  register_foreign_function((void*)&set_position_vec,"set_position_vec",1024);
  register_foreign_function((void*)&move_cam_f,"move_cam_f",1024);
  register_foreign_function((void*)&set_cam_position,"set_cam_position",1024);
  register_foreign_function((void*)&look_at,"look_at",1024);
  register_foreign_function((void*)&add_viewport,"add_viewport",1024);
  register_foreign_function((void*)&set_background_colour,"set_background_colour",1024);
  register_foreign_function((void*)&set_aspect_to_viewport,"set_aspect_to_viewport",1024);
  register_foreign_function((void*)&create_entity,"create_entity",1024);
  register_foreign_function((void*)&set_material_name,"set_material_name",1024);
  register_foreign_function((void*)&set_cast_shadows,"set_cast_shadows",1024);
  register_foreign_function((void*)&get_animation_state,"get_animation_state",1024);
  register_foreign_function((void*)&create_light,"create_light",1024);
  register_foreign_function((void*)&set_lt_type,"set_lt_type",1024);
  register_foreign_function((void*)&set_lt_position,"set_lt_position",1024);
  register_foreign_function((void*)&set_lt_diffuse_colour,"set_lt_diffuse_colour",1024);
  register_foreign_function((void*)&set_lt_specular_colour,"set_lt_specular_colour",1024);
  register_foreign_function((void*)&set_lt_direction,"set_lt_direction",1024);
}

void module__ffiogre__fini_function (module_t* module)
{
}
