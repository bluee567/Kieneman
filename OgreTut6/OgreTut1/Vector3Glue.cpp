#include "GlueDeclarations.h"

extern "C"
{
	//Functional
	DECLDIR Vector3* vector3(Real x, Real y, Real z)
	{
		return new Vector3(x,y,z);
	}

	//Functional
	DECLDIR Real vector3_x(Vector3* v)
	{
		return v->x;
	}

	//Functional
	DECLDIR Real vector3_y(Vector3* v)
	{
		return v->y;
	}

	//Functional
	DECLDIR Real vector3_z(Vector3* v)
	{
		return v->z;
	}

	//Functional
	DECLDIR Vector3* vector3_plus(Vector3* a, Vector3* b)
	{
		return new Vector3(*a+*b);
	}

	//Modifies
	DECLDIR void vector3_destroy(Vector3* v)
	{
		delete v;
	}

	//Modifies
	DECLDIR void vector3_add_x(Vector3* a, Real b)
	{
		a->x += b;
	}

	//Modifies
	DECLDIR void vector3_add_y(Vector3* a, Real b)
	{
		a->y += b;
	}

	//Modifies
	DECLDIR void vector3_add_z(Vector3* a, Real b)
	{
		a->z += b;
	}
}