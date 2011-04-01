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

	DECLDIR ManualObject* makeHitTriangle(Real x1, Real y1, Real x2, Real y2, Real x3, Real y3, const char *material, SceneManager *mgr)
	{
		try
		{
		static int triNo = 0;
		
		char name[64];

		sprintf(name, "tri%d", triNo);

		// Create a manual object for 2D
		ManualObject* manual = mgr->createManualObject(name);
		 
		manual->begin(material, RenderOperation::OT_TRIANGLE_LIST);
		 
		manual->position(x1, y1, 0.0);
		manual->position(x2, y2, 0.0);
		manual->position(x3, y3, 0.0);
		 
		manual->index(0);
		manual->index(1);
		manual->index(2);
		 
		manual->end();
		 
		// Use infinite AAB to always stay visible
		AxisAlignedBox aabInf;
		aabInf.setInfinite();
		manual->setBoundingBox(aabInf);
		 
		// Render just before overlays
		manual->setRenderQueueGroup(RENDER_QUEUE_OVERLAY - 1);
		 
		// Attach to scene
		mgr->getRootSceneNode()->createChildSceneNode()->attachObject(manual);
		manual->setCastShadows(false);

		triNo++;
		return manual;
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

	
	DECLDIR void updateHitTriangle(Real x1, Real y1, Real x2, Real y2, Real x3, Real y3, ManualObject* manual)
	{
		try
		{
		manual->beginUpdate(0);
		 
		manual->position(x1, y1, 0.0);
		manual->position(x2, y2, 0.0);
		manual->position(x3, y3, 0.0);
		 
		manual->index(0);
		manual->index(1);
		manual->index(2);
		 
		manual->end();
		
		}
		catch(Exception& e)
	    {
	#if OGRE_PLATFORM == PLATFORM_WIN32 || OGRE_PLATFORM == OGRE_PLATFORM_WIN32
		  MessageBoxA(NULL, e.getFullDescription().c_str(), "An exception has occurred!", MB_OK | MB_ICONERROR | MB_TASKMODAL);
	#else
		  fprintf(stderr, "An exception has occurred: %s\n",
			e.getFullDescription().c_str());
	#endif
	    }
	}
}