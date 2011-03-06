#include <iostream>
#include <windows.h>

typedef void (*FunctionFunc)();


class A
{
public:
	virtual int take()=0;
	int give(){return 'A';}
};

class B
{
public:
	int take(){return 'B';}
	virtual int give()=0;
};

class AB : public A, public B
{
};


int main()
{
   FunctionFunc create_root;
   HINSTANCE hInstLibrary = LoadLibraryA("OgreTut1.dll");

   if (hInstLibrary)
   {
	   create_root = (FunctionFunc)GetProcAddress(hInstLibrary,
         "createRoot");

	   if (create_root)
      {
         create_root();
      }

	   printf("It DLL works!");

      FreeLibrary(hInstLibrary);
   }
   else
   {
      std::cout << "DLL Failed To Load!" << std::endl;
   }

   std::cin.get();

   return 0;
}