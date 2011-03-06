#include "GlueDeclarations.h"

extern "C"
{
	DECLDIR Quaternion* quaternion(Real w, Real x, Real y, Real z)
	{
		return new Quaternion(w,x,y,z);
	}

	DECLDIR Quaternion* copy_quaternion(Quaternion* rkq)
	{
		return new Quaternion(*rkq);
	}
}