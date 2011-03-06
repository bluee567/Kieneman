#include "GlueDeclarations.h"

extern "C"
{
	DECLDIR Entity* makeHitRectangle(Real x, Real y, Ogre::Real width, Ogre::Real height, const char *material, SceneManager *mgr)
	{
		try
		{
		static int recNo = 0;
		
		char name[64];

		sprintf(name, "rect%d", recNo);
		
		Entity *ent = mgr->createEntity(name, "hitbox");
		SceneNode *sn = mgr->getRootSceneNode()->createChildSceneNode();
		sn->attachObject(ent);
		sn->setPosition(x, y, 0.0);
		sn->setScale(width, height, 1.0);

		//ent->setMaterialName("Examples/Rockwall");
		ent->setCastShadows(false);
		ent->setMaterialName(material);

		recNo++;
		return ent;
		}
		catch(Exception& e)
	    {
	#if OGRE_PLATFORM == PLATFORM_WIN32 || OGRE_PLATFORM == OGRE_PLATFORM_WIN32
		  MessageBoxA(NULL, e.getFullDescription().c_str(), "An exception has occurred!", MB_OK | MB_ICONERROR | MB_TASKMODAL);
	#else
		  fprintf(stderr, "An exception has occurred: %s\n",
			e.getFullDescription().c_str());
	#endif
		  return NULL;
	    }
	}
}