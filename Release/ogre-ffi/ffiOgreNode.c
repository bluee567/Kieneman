#include "clisp.h"

extern object module__ffiOgreNode__object_tab[];

subr_t module__ffiOgreNode__subr_tab[1];
uintC module__ffiOgreNode__subr_tab_size = 0;
subr_initdata_t module__ffiOgreNode__subr_tab_initdata[1];

object module__ffiOgreNode__object_tab[1];
object_initdata_t module__ffiOgreNode__object_tab_initdata[1];
uintC module__ffiOgreNode__object_tab_size = 0;


void module__ffiOgreNode__init_function_1 (module_t* module);

void module__ffiOgreNode__init_function_2 (module_t* module);

void module__ffiOgreNode__fini_function (module_t* module);


void module__ffiOgreNode__init_function_1 (module_t* module)
{
}

void module__ffiOgreNode__init_function_2 (module_t* module)
{
  register_foreign_function((void*)&set_scale,"set_scale",1024);
  register_foreign_function((void*)&get_name,"get_name",1024);
  register_foreign_function((void*)&get_parent,"get_parent",1024);
  register_foreign_function((void*)&get_orientation,"get_orientation",1024);
  register_foreign_function((void*)&set_orientation_q,"set_orientation_q",1024);
  register_foreign_function((void*)&set_orientation_f,"set_orientation_f",1024);
  register_foreign_function((void*)&reset_orientation,"reset_orientation",1024);
  register_foreign_function((void*)&set_position_v,"set_position_v",1024);
  register_foreign_function((void*)&set_position_f,"set_position_f",1024);
  register_foreign_function((void*)&get_position,"get_position",1024);
  register_foreign_function((void*)&set_scale_v,"set_scale_v",1024);
  register_foreign_function((void*)&set_scale_f,"set_scale_f",1024);
  register_foreign_function((void*)&get_scale,"get_scale",1024);
  register_foreign_function((void*)&set_inherit_orientation,"set_inherit_orientation",1024);
  register_foreign_function((void*)&get_inherit_orientation,"get_inherit_orientation",1024);
  register_foreign_function((void*)&set_inherit_scale,"set_inherit_scale",1024);
  register_foreign_function((void*)&get_inherit_scale,"get_inherit_scale",1024);
  register_foreign_function((void*)&scale_v,"scale_v",1024);
  register_foreign_function((void*)&scale_f,"scale_f",1024);
  register_foreign_function((void*)&translate_v,"translate_v",1024);
  register_foreign_function((void*)&translate_f,"translate_f",1024);
  register_foreign_function((void*)&roll,"roll",1024);
  register_foreign_function((void*)&pitch,"pitch",1024);
  register_foreign_function((void*)&yaw,"yaw",1024);
  register_foreign_function((void*)&rotate_v,"rotate_v",1024);
  register_foreign_function((void*)&rotate_q,"rotate_q",1024);
  register_foreign_function((void*)&rotate_f,"rotate_f",1024);
  register_foreign_function((void*)&create_child,"create_child",1024);
  register_foreign_function((void*)&create_child_named,"create_child_named",1024);
}

void module__ffiOgreNode__fini_function (module_t* module)
{
}
