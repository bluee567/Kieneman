#include <Ogre.h>
#include <OIS/OIS.h>
#include <CEGUI/CEGUI.h>
#include <OgreCEGUIRenderer.h>
#include <OgreTextAreaOverlayElement.h>
#include <OgreFontManager.h>

#include "GlueDeclarations.h"
#include "OverlayGlue.h"

class InputListener : public FrameListener
{
public:
    InputListener(OIS::Keyboard *keyboard, OIS::Mouse* mouse)
        : mKeyboard(keyboard), mMouse(mouse)
    {
    }

    bool frameStarted(const FrameEvent& evt)
    {
        //mKeyboard->capture();
        return !mKeyboard->isKeyDown(OIS::KC_ESCAPE);
    }

private:
    OIS::Keyboard *mKeyboard;
    OIS::Mouse* mMouse;
    //OIS::JoyStick* mJoystick;
};

//class Application
//{
//public:

	Root *mRoot;
    OIS::Keyboard *mKeyboard = NULL;
    OIS::Mouse* mMouse = NULL;
    std::vector<OIS::JoyStick*> mJoysticks;
    OIS::InputManager *mInputManager = NULL;
    CEGUI::OgreCEGUIRenderer *mRenderer = NULL;
    CEGUI::System *mSystem = NULL;
    InputListener *mListener = NULL;
    HWND renderWindowHWnd = 0;

    /*
    A class which represents a channel of input which is toggled (such as a button).
    The function 'held' returns true if the input channel is 'pressed'. It is important to note that
    this class does not need to represent a physical button, but can be hooked into anything.
    Yes, this is basicly a custom lambda... only you need to subclass it to use it...
    */
    class InputToggle
    {
    public:
	    virtual ~InputToggle(){};

	    virtual bool held()=0;
    };

    /*
    An input toggle which corresponds to a keyboard key.
    Precondition: Keyboard.capture() must be called every
    frame in order for keybord input to work properly.
    */
    class InputToggleKey : public InputToggle
    {
	    OIS::KeyCode kc;
    public:

	    InputToggleKey(OIS::KeyCode _kc)
		    : kc(_kc)
	    {}

	    bool held()
	    {
			return mKeyboard->isKeyDown(kc);
	    }
    };

    /*
    An input toggle which corresponds to a pad/stick.
    Precondition: joy.capture() must be called every
    frame in order for the joystick to work properly.
    */
    class InputToggleJoy : public InputToggle
    {
	    int buttonNo;
	    int joystickNo;
    public:

	    InputToggleJoy(int _bn, int _jn)
		    : buttonNo(_bn), joystickNo(_jn)
	    {}

	    bool held()
	    {
		    return static_cast<bool>(mJoysticks[joystickNo]->getJoyStickState().buttonDown(buttonNo));
	    }
    };

extern "C"
{
	DECLDIR bool toggleHeld(InputToggle * toggle)
	{
		return toggle->held();
	}

	//NEWS MEMORY, CALLER MUST DEALLOCATE.
	DECLDIR InputToggle* getIToggle(OIS::KeyCode kc)
	{
		return new InputToggleKey(kc);
	}

	//NEWS MEMORY, CALLER MUST DEALLOCATE.
	DECLDIR InputToggle* getIToggleJoy(int buttonNo, int joystickNo)
	{
		return new InputToggleJoy(buttonNo, joystickNo);
	}

	DECLDIR void deleteIToggle(InputToggle* it)
	{
		delete it;
	}

	DECLDIR bool keyHeld(OIS::KeyCode kc)
	{
		return mKeyboard->isKeyDown(kc);
	}

	DECLDIR bool buttonHeld(int buttonNo, int joystickNo)
	{
		return mJoysticks[joystickNo]->getJoyStickState().buttonDown(buttonNo);
	}

	DECLDIR bool directionHeld(int dir, int joystickNo)
	{
		//if (mJoysticks[joystickNo]->hats() >= hat)
			return (mJoysticks[joystickNo]->getJoyStickState().mPOV[0].direction) == dir;
		//else
		//	return OIS::Pov::Centered;
	}

	DECLDIR int returnDir(int joystickNo)
	{return (mJoysticks[joystickNo]->getJoyStickState().mPOV[0].direction);}

	const int NUM_SLIDERS = 4;
	const int NUM_SLIDER_AXIS = 2;

	DECLDIR int sliderVal(int slider, int joystickNo)
	{
		if (slider >= NUM_SLIDER_AXIS*NUM_SLIDER_AXIS)
			return 0;
		else
		{
			int sliderNo = slider/NUM_SLIDER_AXIS;
			int xy = slider%NUM_SLIDER_AXIS;
			if (xy == 0)
				return mJoysticks[joystickNo]->getJoyStickState().mSliders[sliderNo].abX;
			else
				return mJoysticks[joystickNo]->getJoyStickState().mSliders[sliderNo].abY;
		}
	}

	//Returns a value between -1.0 and 1.0
	DECLDIR float axisVal(int axis, int joystickNo)
	{
		using namespace OIS;
	
		const float diff = ((float)(JoyStick::MAX_AXIS) - (float)(JoyStick::MIN_AXIS));
		return ((float)(mJoysticks[joystickNo]->getJoyStickState().mAxes[axis].abs)) / diff;
	}

	DECLDIR int noOfJoysticks()
	{
		return mJoysticks.size();
	}

	DECLDIR short noOfHats(int joystickNo)
	{
		return mJoysticks[joystickNo]->hats();
	}

	DECLDIR short noOfAxes(int joystickNo)
	{
		return mJoysticks[joystickNo]->axes();
	}

	DECLDIR short noOfButtons(int joystickNo)
	{
		return mJoysticks[joystickNo]->buttons();
	}

	DECLDIR void capture_input()
	{
		mKeyboard->capture();
		//mJoy->capture();

		for (std::vector<OIS::JoyStick*>::iterator itJoystick  = mJoysticks.begin();
			     itJoystick != mJoysticks.end();
			     ++itJoystick)
		{(*itJoystick)->capture();}
	}
}

    

//private:
    

    
    
    void defineResources()
    {
	    String secName, typeName, archName;
       ConfigFile cf;
       cf.load("resources.cfg");

	 ConfigFile::SectionIterator seci = cf.getSectionIterator();
	while (seci.hasMoreElements())
	{
		secName = seci.peekNextKey();
		ConfigFile::SettingsMultiMap *settings = seci.getNext();
		ConfigFile::SettingsMultiMap::iterator i;

		for (i = settings->begin(); i != settings->end(); ++i)
	     {
		   typeName = i->first;
		   archName = i->second;
		   ResourceGroupManager::getSingleton().addResourceLocation(archName, typeName, secName);
	     }
	}

	#if defined(WIN32)
	//Adds windows fonts.
	Ogre::ResourceGroupManager::getSingleton().addResourceLocation(
		"c:\\windows\\fonts", "FileSystem", "GUI");
	#endif
    }
    
    void setupRenderSystem()
    {
	    if (!mRoot->restoreConfig() && !mRoot->showConfigDialog())
           throw Exception(52, "User canceled the config dialog!", "Application::setupRenderSystem()");
    }
    
    void createRenderWindow()
    {
	mRoot->initialise(true, "Tutorial Render Window");

	#if OGRE_PLATFORM == PLATFORM_WIN32 || OGRE_PLATFORM == OGRE_PLATFORM_WIN32
	//Get the handle to the main window and set it as the active window.
	//HWND renderWindowHWnd = 0;
	mRoot->getAutoCreatedWindow()->getCustomAttribute("WINDOW", (void*)&renderWindowHWnd);

	if(IsWindow(renderWindowHWnd))
		SetActiveWindow(renderWindowHWnd);
	else
		throw Exception(52,
			"Handle to the render window could not be obtained!\nPossible custom atribute failure.", "Application::createRenderWindow()");

	#endif
    }

    void initializeResourceGroups()
    {
	    TextureManager::getSingleton().setDefaultNumMipmaps(5);
		ResourceGroupManager::getSingleton().initialiseAllResourceGroups();
    }

    void setupScene()
    {
	    SceneManager *mgr = mRoot->createSceneManager(ST_GENERIC, "Default SceneManager");
       Camera *cam = mgr->createCamera("Camera");
	 cam->setPosition(Vector3(0, 10, 500));
	    cam->lookAt(Vector3(0,0,0));
	    cam->setNearClipDistance(5);
	    cam->setFarClipDistance(500);

       Viewport *vp = mRoot->getAutoCreatedWindow()->addViewport(cam);
	vp->setBackgroundColour(ColourValue(0,0,0));

	 // Alter the camera aspect ratio to match the viewport
	    cam->setAspectRatio(Real(vp->getActualWidth()) / Real(vp->getActualHeight()));

	    Entity *ent;
        Light *light;

	  mgr->setAmbientLight(ColourValue(0.05, 0.05, 0.05));
        mgr->setShadowTechnique(SHADOWTYPE_STENCIL_ADDITIVE);

	  ent = mgr->createEntity("Ninja", "ninja.mesh");
	  ent->setCastShadows(true);
	  mgr->getRootSceneNode()->createChildSceneNode()->attachObject(ent);

	  Plane plane(Vector3::UNIT_Y, -2);
	  Plane leftPlane(Vector3::UNIT_X, -50.0);
	  Plane rightPlane(Vector3::UNIT_X, 50.0);
	  MeshManager::getSingleton().createPlane("ground", ResourceGroupManager::DEFAULT_RESOURCE_GROUP_NAME, plane,
		  1500,1500, 20, 20, true, 1,5,5, Vector3::UNIT_Z);
	  MeshManager::getSingleton().createPlane("leftWall", ResourceGroupManager::DEFAULT_RESOURCE_GROUP_NAME, leftPlane,
		  1500,1500, 20, 20, true, 1,5,5, Vector3::UNIT_X);
	  MeshManager::getSingleton().createPlane("ground", ResourceGroupManager::DEFAULT_RESOURCE_GROUP_NAME, rightPlane,
		  1500,1500, 20, 20, true, 1,5,5, Vector3::UNIT_X);
	  ent = mgr->createEntity("GroundEntity", "ground");
	  mgr->getRootSceneNode()->createChildSceneNode()->attachObject(ent);

	  ent->setMaterialName("Examples/Rockwall");
	  ent->setCastShadows(false);

	  light = mgr->createLight("Light1");
	  light->setType(Light::LT_POINT);
	  light->setPosition(Vector3(0, 150, 250));
	  light->setDiffuseColour(1.0f, 0.0f, 0.0f);
	  light->setSpecularColour(1.0f, 0.0f, 0.0f);

	  light = mgr->createLight("Light2");
       light->setType(Light::LT_DIRECTIONAL);
       light->setDiffuseColour(ColourValue(.25, .25, 0));
       light->setSpecularColour(ColourValue(.25, .25, 0));
	 light->setDirection(Vector3( 0, -1, 1 ));

	 light = mgr->createLight("Light3");
       light->setType(Light::LT_SPOTLIGHT);
       light->setDiffuseColour(0, 0, 1.0);
       light->setSpecularColour(0, 0, 1.0);
	 light->setDirection(-1, -1, 0);
       light->setPosition(Vector3(300, 300, 0));
	 light->setSpotlightRange(Degree(35), Degree(50));
    }

    void setupInputSystem()
    {
	    size_t windowHnd = 0;
	    std::ostringstream windowHndStr;
	    OIS::ParamList pl;
	    RenderWindow *win = mRoot->getAutoCreatedWindow();
	    win->getCustomAttribute("WINDOW", &windowHnd);
       windowHndStr << windowHnd;
       pl.insert(std::make_pair(std::string("WINDOW"), windowHndStr.str()));
       mInputManager = OIS::InputManager::createInputSystem(pl);

	 try
       {
           mKeyboard = static_cast<OIS::Keyboard*>(mInputManager->createInputObject(OIS::OISKeyboard, false));
           mMouse = static_cast<OIS::Mouse*>(mInputManager->createInputObject(OIS::OISMouse, false));
           //mJoy = static_cast<OIS::JoyStick*>(mInputManager->createInputObject(OIS::OISJoyStick, false));

	     if (mInputManager->numJoySticks() > 0)
	     {
		     mJoysticks.resize(mInputManager->numJoySticks());

		     for(std::vector<OIS::JoyStick*>::iterator itJoystick  = mJoysticks.begin();
			     itJoystick != mJoysticks.end();
			     ++itJoystick )
		     {
			     (*itJoystick) = static_cast<OIS::JoyStick*>( mInputManager->createInputObject( OIS::OISJoyStick, false ));
		     }
	     }

       }
       catch (const OIS::Exception &e)
       {
           throw Exception(42, e.eText, "Application::setupInputSystem");
       }
    }

    void setupCEGUI()
    {
	    SceneManager *mgr = mRoot->getSceneManager("Default SceneManager");
       RenderWindow *win = mRoot->getAutoCreatedWindow();

       // CEGUI setup
       mRenderer = new CEGUI::OgreCEGUIRenderer(win, Ogre::RENDER_QUEUE_OVERLAY, false, 3000, mgr);
       mSystem = new CEGUI::System(mRenderer);
    }

    void createFrameListener()
    {
		mListener = new InputListener(mKeyboard, mMouse);
		mRoot->addFrameListener(mListener);
    }

    void startRenderLoop()
    {
	    //mRoot->startRendering();
	while (mRoot->renderOneFrame())
	{
	}
    }
//};


#if OGRE_PLATFORM == PLATFORM_WIN32 || OGRE_PLATFORM == OGRE_PLATFORM_WIN32
#define WIN32_LEAN_AND_MEAN
#include "windows.h"

INT WINAPI WinMain(HINSTANCE hInst, HINSTANCE, LPSTR strCmdLine, INT)
#else
int main(int argc, char **argv)
#endif
{
    try
    {
	  //Application app;
	  //app.go();
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

    return 0;
}

extern "C"
{
	DECLDIR int test1()
	{
		static int ii = 0;
		++ii;
		return ii; 
	}

	DECLDIR void reloadEntitySkeleton(Entity * ent)
	{
		ent->getSkeleton()->reload();
	}

	DECLDIR void createRoot()
    {
	    
		try
	    {
		  mRoot = new Root();
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

	/*********************************************************************
	*** FFI Wrapper Functions
	*********************************************************************/

	//Scene Manager
	DECLDIR SceneManager * get_scene_manager(str name)
	{return mRoot->getSceneManager(name);}

	DECLDIR SceneManager * create_scene_manager(SceneTypeMask smask,  str name)
	{return mRoot->createSceneManager(smask ,name);}

	DECLDIR Light*  get_light(SceneManager *mgr , str name)
{return  mgr->getLight(name);}

	DECLDIR   Camera*  get_camera(SceneManager *mgr , str name)
{return  mgr->getCamera(name);}

	DECLDIR   bool  has_camera(SceneManager *mgr , str name)
{return  mgr->hasCamera(name);}

	DECLDIR   void  destroy_camera(SceneManager *mgr , str name)
{ mgr->destroyCamera(name);}

	DECLDIR   void  destroy_all_cameras(SceneManager *mgr  )
{ mgr->destroyAllCameras();}

	DECLDIR   void  destroy_light(SceneManager *mgr , str name)
{ mgr->destroyLight(name);}

	DECLDIR   void  destroy_all_lights(SceneManager *mgr  )
{ mgr->destroyAllLights();}

	//Node
	DECLDIR  void  set_scale(Node *node , Real x, Real y, Real z)
{ node->setScale(x, y, z);}

	//Scene Nodes
	DECLDIR SceneNode * get_root_scene_node(SceneManager * mgr)
	{return mgr->getRootSceneNode();}

	DECLDIR SceneNode * create_child_scene_node(SceneNode * node)
	{return node->createChildSceneNode();}

	/*DECLDIR void destroy_scene_node(SceneManager *mgr, SceneNode * node)
	{mgr->destroyS;}*/

	DECLDIR void attach_object(SceneNode * node, Entity *ent)
	{node->attachObject(ent);}

	//Cameras
	DECLDIR Camera * create_camera(SceneManager * scenemgr, str name)
	{return scenemgr->createCamera(name);}

	DECLDIR void set_ambient_light(SceneManager * mgr, float r, float g, float b, float a)
	{mgr->setAmbientLight(ColourValue(r, g, b, a));}

	DECLDIR void set_position_vec(Camera * cam, Vector3 * vec)
	{cam->setPosition(*vec);}

	DECLDIR void set_cam_position(Camera * cam, Real x, Real y, Real z)
	{cam->setPosition(x,y,z);}

	DECLDIR void move_cam_f(Camera* cam, Real x, Real y, Real z)
	{cam->move(Vector3(x,y,z));}

	DECLDIR void look_at(Camera * cam, Real x, Real y, Real z)
	{cam->lookAt(x,y,z);}

	//Viewports

	DECLDIR Viewport * add_viewport(Camera *cam)
	{return mRoot->getAutoCreatedWindow()->addViewport(cam);}

	DECLDIR void set_background_colour(Viewport *vp, float r, float g, float b, float a)
	{vp->setBackgroundColour(ColourValue(r,g,b,a));}

	 DECLDIR void set_aspect_to_viewport(Camera *cam, Viewport *vp)
	 {cam->setAspectRatio(Real(vp->getActualWidth()) / Real(vp->getActualHeight()));}

	 //Entities
	 DECLDIR Entity * create_entity(SceneManager *mgr, str name, str mesh)
	 {return mgr->createEntity(name, mesh);}

	 DECLDIR void destroy_entity(SceneManager *mgr, Entity *ent)
	 {mgr->destroyEntity(ent);}

	 DECLDIR Node* get_parent_node(Entity *ent)
	 {
		 return ent->getParentNode();
	 }

	 DECLDIR void set_material_name(Entity *ent, str material)
	 {ent->setMaterialName(material);}

	 DECLDIR void set_cast_shadows(Entity *ent, bool tf)
	 {ent->setCastShadows(tf);}

	 DECLDIR void set_visible(Entity *ent, bool vis)
	 {
		 ent->setVisible(vis);
	 }

	 //Manual Objects
	DECLDIR void destroy_manual_object(SceneManager *mgr, ManualObject *mo)
	{mgr->destroyManualObject(mo);}

	 //NO COUNTERPART
	DECLDIR SkeletonInstance* get_skeleton_inst(Entity *ent)
	{
		return ent->getSkeleton();
	}

	 //NO COUNTERPART
	DECLDIR Vector3* get_bone_derived_position(Bone* bone)
	{
		return new Vector3(bone->_getDerivedPosition());
	}

	//NO COUNTERPART
	DECLDIR Bone* get_bone(SkeletonInstance *skelInst, str name)
	{
		return skelInst->getBone(name);
	}

	//NO COUNTERPART
	DECLDIR Bone* get_bone_global_pos(SkeletonInstance *skelInst, str name)
	{
		return skelInst->getBone(name);
	}

	 DECLDIR AnimationState* get_animation_state(Entity* ent, str name)
	 {
		 try{
		 return ent->getAnimationState(name);
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

	 //Lights
	 DECLDIR Light * create_light(SceneManager * mgr, str name)
	 {return mgr->createLight(name);}

	 DECLDIR  void  set_lt_type(Light *_lt , Ogre::Light::LightTypes type){ _lt->setType(type);}

	 DECLDIR  void  set_lt_position(Light *_lt , Real x, Real y, Real z){ _lt->setPosition(x, y, z);}

	 DECLDIR  void  set_lt_position_v(Light *_lt , const Vector3* vec){ _lt->setPosition(*vec);}

	 DECLDIR  void  set_lt_diffuse_colour(Light *_lt , Real red, Real green, Real blue){ _lt->setDiffuseColour(red, green, blue);}

	 DECLDIR  void  set_lt_specular_colour(Light *_lt , Real red, Real green, Real blue){ _lt->setSpecularColour(red, green, blue);}

	 DECLDIR  void  set_lt_direction(Light *_lt , Real x, Real y, Real z)
	{ _lt->setDirection(x, y, z);}

	DECLDIR void endApplication()
    {
		 mInputManager->destroyInputObject(mKeyboard);
		 mInputManager->destroyInputObject(mMouse);
		 //mInputManager->destroyInputObject(mJoy);
		for (std::vector<OIS::JoyStick*>::iterator itJoystick  = mJoysticks.begin();
			     itJoystick != mJoysticks.end();
			     ++itJoystick)
		{mInputManager->destroyInputObject( *itJoystick );}
		mJoysticks.clear();

		OIS::InputManager::destroyInputSystem(mInputManager);
		DESTROY(mRenderer);
		DESTROY(mSystem);
		DESTROY(mListener);
		DESTROY(mRoot);
    }

	DECLDIR void startSystem()
	{
		try
		{
			  createRoot();
			  defineResources();
			  setupRenderSystem();
			  createRenderWindow();
			  initializeResourceGroups();
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

	DECLDIR void setupBackground(SceneManager* mgr, Viewport* vp, Real stageWidth)
	{
		mgr->setShadowTechnique(SHADOWTYPE_STENCIL_ADDITIVE);

		Plane plane(Vector3::UNIT_Y, 0);
		Plane leftPlane(Vector3::UNIT_X, -stageWidth);
		Plane rightPlane(Vector3::NEGATIVE_UNIT_X, -stageWidth);

		const int mesh_fineness = 15;
		Real s_width = 1000;

		MeshManager::getSingleton().createPlane("ground", ResourceGroupManager::DEFAULT_RESOURCE_GROUP_NAME, plane,
			  s_width, s_width, mesh_fineness, mesh_fineness, true, 1,5,5, Vector3::UNIT_Z);
		  Entity* ent = mgr->createEntity("GroundEntity", "ground");
		  mgr->getRootSceneNode()->createChildSceneNode()->attachObject(ent);
		  ent->setMaterialName("Examples/Rockwall");
		  ent->setCastShadows(false);

		  ent = mgr->createEntity("leftWall", "ground");
		  ent->setMaterialName("Examples/Rockwall");
		  ent->setCastShadows(false);
		  SceneNode* wall = mgr->getRootSceneNode()->createChildSceneNode();
		  wall->translate(-stageWidth,0,0);
		  wall->rotate(Vector3::UNIT_Z, Ogre::Radian(Math::HALF_PI), Node::TS_WORLD);
		  wall->attachObject(ent);

		  ent = mgr->createEntity("rightWall", "ground");
		  ent->setMaterialName("Examples/Rockwall");
		  ent->setCastShadows(false);
		  wall = mgr->getRootSceneNode()->createChildSceneNode();
		  wall->translate(-stageWidth,0,0);
		  wall->rotate(Vector3::UNIT_Z, Ogre::Radian(-Math::HALF_PI));
		  wall->attachObject(ent);
		  

		  /*MeshManager::getSingleton().createPlane("leftWall", ResourceGroupManager::DEFAULT_RESOURCE_GROUP_NAME, leftPlane,
			  s_width, s_width, mesh_fineness, mesh_fineness, true, 1,5,5, Vector3::UNIT_Z);
		  ent = mgr->createEntity("wall1", "leftWall");
		  mgr->getRootSceneNode()->createChildSceneNode()->attachObject(ent);
		  ent->setMaterialName("Examples/Rockwall");
		  ent->setCastShadows(false);*/

		  MeshManager::getSingleton().createPlane("rightWall", ResourceGroupManager::DEFAULT_RESOURCE_GROUP_NAME, rightPlane,
			  s_width, s_width, mesh_fineness, mesh_fineness, true, 1,5,5, Vector3::UNIT_Z);
		  ent = mgr->createEntity("wall2", "rightWall");
		  mgr->getRootSceneNode()->createChildSceneNode()->attachObject(ent);
		  ent->setMaterialName("Examples/Rockwall");
		  ent->setCastShadows(false);

		Plane plane2(Vector3::UNIT_Z, 0);
		MeshManager::getSingleton().createPlane("hitbox", ResourceGroupManager::DEFAULT_RESOURCE_GROUP_NAME, plane2,
			1,1, 5, 5, true, 1,5,5, Vector3::UNIT_Y);

		setUpHPbars(mgr, vp);
		
		//ManualObject* manObj = mSceneMgr->createManualObject("manual");

		//// Create a manual object for 2D
		//ManualObject* manual = mgr->createManualObject("manual");
		// 
		//// Use identity view/projection matrices
		//manual->setUseIdentityProjection(true);
		//manual->setUseIdentityView(true);
		// 
		//manual->begin("BaseWhiteNoLighting", RenderOperation::OT_LINE_STRIP);
		// 
		//manual->position(-0.2, -0.2, 0.0);
		//manual->position( 0.2, -0.2, 0.0);
		//manual->position( 0.2,  0.2, 0.0);
		//manual->position(-0.2,  0.2, 0.0);
		// 
		//manual->index(0);
		//manual->index(1);
		//manual->index(2);
		//manual->index(3);
		//manual->index(0);
		// 
		//manual->end();
		// 
		//// Use infinite AAB to always stay visible
		//AxisAlignedBox aabInf;
		//aabInf.setInfinite();
		//manual->setBoundingBox(aabInf);
		// 
		//// Render just before overlays
		//manual->setRenderQueueGroup(RENDER_QUEUE_OVERLAY - 1);
		// 
		//// Attach to scene
		//mgr->getRootSceneNode()->createChildSceneNode()->attachObject(manual);
	}

	DECLDIR void startIO()
	{
		try
		{
			  setupInputSystem();
			  setupCEGUI();
			  createFrameListener();
			  //startRenderLoop();
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

		//endApplication();
	}

	/*
	This function renders a single frame of Ogre action. 
	*/
	DECLDIR bool renderFrame()//Whether or not the render window is regaining control.
	{
		try
		{
			/* On windows NT, when the render loop is paused by yielding control to the lisp process
			(this is mainly used so that developers can enter the debugger through a conditional and
			evaluate code/modify the lisp process without leaving the main loop), continuing the main loop
			will not cause the window to redraw itself for reasons unknown to me. This code should 
			'jump start' the window drawing process if control is given to the window.
			*/
			#if OGRE_PLATFORM == PLATFORM_WIN32 || OGRE_PLATFORM == OGRE_PLATFORM_WIN32
			static bool reactivate = false;

			if (GetForegroundWindow() != renderWindowHWnd)
				reactivate = true;

			if (reactivate && GetForegroundWindow() == renderWindowHWnd)
			{
				SetForegroundWindow(renderWindowHWnd);

				if (IsIconic(renderWindowHWnd))
					ShowWindow( renderWindowHWnd,SW_RESTORE );
				else
					ShowWindow( renderWindowHWnd,SW_SHOW );

				SetActiveWindow(renderWindowHWnd);
				BringWindowToTop(renderWindowHWnd);

				InvalidateRect (renderWindowHWnd, NULL, TRUE);
				UpdateWindow (renderWindowHWnd);

				reactivate = false;
			}
			#endif

			return mRoot->renderOneFrame();
		}
		catch(Exception& e)
	    {
	#if OGRE_PLATFORM == PLATFORM_WIN32 || OGRE_PLATFORM == OGRE_PLATFORM_WIN32
		  MessageBoxA(NULL, e.getFullDescription().c_str(), "An exception has occurred!", MB_OK | MB_ICONERROR | MB_TASKMODAL);
	#else
		  fprintf(stderr, "An exception has occurred: %s\n",
			e.getFullDescription().c_str());
	#endif
		  return false;
	    }
	}
}


    