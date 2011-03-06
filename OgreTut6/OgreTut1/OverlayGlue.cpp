#include "GlueDeclarations.h"
#include "OverlayGlue.h"


class HPBar
{
public:
	Real x,y;
	ManualObject* man;
	TextAreaOverlayElement* toe;
};

static HPBar* hp1 = NULL;
static HPBar* hp2 = NULL;
//static TextAreaOverlayElement* debug1 = NULL;
//static TextAreaOverlayElement* debug2 = NULL;


HPBar* createHpElement(Ogre::Real x, Ogre::Real y, const Ogre::DisplayString name, SceneManager* mgr)
{
	OverlayManager& overlayManager = OverlayManager::getSingleton();

		/*char textName[64] = "";

		sprintf(textName, "tx%s", name);*/

	// Create a text area
		TextAreaOverlayElement* hp = static_cast<TextAreaOverlayElement*>(
		    overlayManager.createOverlayElement("TextArea", name));
		hp->setMetricsMode(Ogre::GMM_PIXELS);
		hp->setPosition((x+1.0)*750, y);
		hp->setDimensions(100, 100);
		hp->setCaption("No Value!");
		hp->setCharHeight(16);
		hp->setFontName("Arial");
		hp->setColour(Ogre::ColourValue(0.9, 0.0, 0.0, 1.0));

		// Create a manual object for 2D
		ManualObject* manual = mgr->createManualObject(name);
		 
		// Use identity view/projection matrices
		manual->setUseIdentityProjection(true);
		manual->setUseIdentityView(true);
		 
		manual->begin("BaseWhiteNoLighting", RenderOperation::OT_TRIANGLE_STRIP);
		 
		manual->position(x, y, 0.0);
		manual->position( x+0.1, y, 0.0);
		manual->position( x+0.1,  y+0.1, 0.0);
		manual->position(x,  y+0.1, 0.0);
		 
		manual->index(0);
		manual->index(1);
		manual->index(2);
		manual->index(3);
		//manual->index(0);
		 
		manual->end();
		 
		// Use infinite AAB to always stay visible
		AxisAlignedBox aabInf;
		aabInf.setInfinite();
		manual->setBoundingBox(aabInf);
		 
		// Render just before overlays
		manual->setRenderQueueGroup(RENDER_QUEUE_OVERLAY - 1);
		 
		// Attach to scene
		mgr->getRootSceneNode()->createChildSceneNode()->attachObject(manual);

		HPBar* pm = new HPBar();
		pm->man = manual;
		pm->x = x;
		pm->y = y;
		pm->toe = hp;

		return pm;
}

TextAreaOverlayElement* createTextArea(Ogre::Real x, Ogre::Real y, const Ogre::DisplayString name)
{
	OverlayManager& overlayManager = OverlayManager::getSingleton();

	// Create a text area
		TextAreaOverlayElement* hp = static_cast<TextAreaOverlayElement*>(
		    overlayManager.createOverlayElement("TextArea", name));
		hp->setMetricsMode(Ogre::GMM_PIXELS);
		hp->setPosition(x, y);
		hp->setDimensions(200, 200);
		hp->setCaption("No Value!");
		hp->setCharHeight(16);
		hp->setFontName("Arial");
		hp->setColour(Ogre::ColourValue(0.0, 0.9, 0.0, 1.0));

		return hp;
}

extern "C"
{
	DECLDIR  void  set_char_height(TextAreaOverlayElement *taoe ,  Real height ){ taoe->setCharHeight( height);}

	DECLDIR  Real  get_char_height(TextAreaOverlayElement *taoe  ){return  taoe->getCharHeight();}

	DECLDIR  void  set_space_width(TextAreaOverlayElement *taoe ,  Real width ){ taoe->setSpaceWidth( width);}

	 DECLDIR  Real  get_space_width(TextAreaOverlayElement *taoe  ){return  taoe->getSpaceWidth();}

	 DECLDIR  void  set_font_name(TextAreaOverlayElement *taoe ,  const String& font ){ taoe->setFontName( font);}

	DECLDIR  void  setUpHPbars(SceneManager* mgr, Viewport* vp)
	{
		//Create Overlay
		OverlayManager& overlayManager = OverlayManager::getSingleton();

		// Create a panel
		OverlayContainer* panel = static_cast<OverlayContainer*>(
		    overlayManager.createOverlayElement("Panel", "PanelName"));
		panel->setMetricsMode(Ogre::GMM_PIXELS);
		panel->setPosition(50, 30);
		panel->setDimensions(vp->getActualWidth()-50, 200);
		//panel->setMaterialName("MaterialName"); // Optional background material

		hp1 = createHpElement(-0.8, 0.6, "hp1", mgr);
		hp2 = createHpElement(0.6, 0.6, "hp2", mgr);
		//debug1 = createTextArea(10.0, 300.0, "db1");
		//debug2 = createTextArea(-300+ vp->getActualWidth(), 300.0, "db2");

		// Create an overlay, and add the panel
		Overlay* overlay = overlayManager.create("OverlayName");
		overlay->add2D(panel);

		// Add the text areas to the panel
		panel->addChild(hp1->toe);
		panel->addChild(hp2->toe);
		//panel->addChild(debug1);
		//panel->addChild(debug2);

		// Show the overlay
		overlay->show();
	}

	DECLDIR  void  setHPbarValue(int value, int bar)
	{
		HPBar* pm;
		if (bar == 1)
			 pm = hp1;
		else
			 pm = hp2;

		ManualObject* manual = pm->man;
		Real x = pm->x;
		Real y = pm->y;

		manual->beginUpdate(0);
		 
		manual->position(x, y, 0.0);
		manual->position( x+0.0003*value, y, 0.0);
		manual->position( x+0.0003*value,  y+0.1, 0.0);
		manual->position(x,  y+0.1, 0.0);
		 
		manual->index(0);
		manual->index(1);
		manual->index(2);
		manual->index(3);
		manual->index(0);
		 
		manual->end();

		char text[64] = "";

		sprintf(text, "%d", value);

		if (bar == 1)
			hp1->toe->setCaption(text);
		else
			hp2->toe->setCaption(text);
	}

	DECLDIR  void  setTextArea(str text, int area)
	{
		/*if (area == 1)
			debug1->setCaption(text);
		else
			debug2->setCaption(text);*/
	}
}