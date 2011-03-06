#include "clisp.h"

extern object module__ffiOgreMath__object_tab[];

subr_t module__ffiOgreMath__subr_tab[1];
uintC module__ffiOgreMath__subr_tab_size = 0;
subr_initdata_t module__ffiOgreMath__subr_tab_initdata[1];

object module__ffiOgreMath__object_tab[1];
object_initdata_t module__ffiOgreMath__object_tab_initdata[1];
uintC module__ffiOgreMath__object_tab_size = 0;


void module__ffiOgreMath__init_function_1 (module_t* module);

void module__ffiOgreMath__init_function_2 (module_t* module);

void module__ffiOgreMath__fini_function (module_t* module);


void module__ffiOgreMath__init_function_1 (module_t* module)
{
}

void module__ffiOgreMath__init_function_2 (module_t* module)
{
  register_foreign_function((void*)&degree,"degree",1024);
  register_foreign_function((void*)&radian,"radian",1024);
  register_foreign_function((void*)&vector3,"vector3",1024);
  register_foreign_function((void*)&vector3_plus,"vector3_plus",1024);
  register_foreign_function((void*)&vector3_add_x,"vector3_add_x",1024);
  register_foreign_function((void*)&vector3_add_y,"vector3_add_y",1024);
  register_foreign_function((void*)&vector3_add_z,"vector3_add_z",1024);
  register_foreign_function((void*)&quaternion,"quaternion",1024);
  register_foreign_function((void*)&copy_quaternion,"copy_quaternion",1024);
}

void module__ffiOgreMath__fini_function (module_t* module)
{
}
