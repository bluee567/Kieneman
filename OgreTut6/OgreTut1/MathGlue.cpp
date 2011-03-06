#include "GlueDeclarations.h"

extern "C"
{
	DECLDIR   Radian*  radian(Real rad)
	{
		return new Radian(rad);
	}

	DECLDIR   Degree*  degree(Real deg)
	{
		return new Degree(deg);
	}
}