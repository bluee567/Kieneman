#ifndef _GLUE_DECLARATIONS_
#define _GLUE_DECLARATIONS_

#include <Ogre.h>

using namespace Ogre;

//Deletes a single object that has been 'new'ed
#ifndef DESTROY
#define DESTROY(obj) delete (obj); obj = NULL
#endif

//DLL exporting

typedef const char* str;

#define DLL_EXPORT 

//This preposessor if should be moved into a header file which is then optionally included
//under a DLL_EXPORT defenition depending on if the translation unit is defining or using the DLL.
#if defined DLL_EXPORT
#define DECLDIR __declspec(dllexport)
#else
#define DECLDIR __declspec(dllimport)
#endif

//template<class T> inline void destroy(T*& p) { delete p; p = 0; }



/*

		NOTES ABOUT CLISP FFI GLUE CODE CONVENTIONS

*/
#endif