#include "GlueDeclarations.h"
#include "OverlayGlue.h"


static TextAreaOverlayElement* hp1 = NULL;
static TextAreaOverlayElement* hp2 = NULL;


TextAreaOverlayElement* createHpElement(Ogre::Real x, Ogre::Real y, const Ogre::DisplayString name)
{
	OverlayManager& overlayManager = OverlayManager::getSingleton();

	// Create a text area
		TextAreaOverlayElement* hp = static_cast<TextAreaOverlayElement*>(
		    overlayManager.createOverlayElement("TextArea", name));
		hp->setMetricsMode(Ogre::GMM_PIXELS);
		hp->setPosition(x, y);
		hp->setDimensions(100, 100);
		hp->setCaption("No Value!");
		hp->setCharHeight(16);
		hp->setFontName("Arial");
		hp->setColour(Ogre::ColourValue(0.6, 0.0, 0.0, 1.0));

		return hp;
}

extern "C"
{
	DECLDIR  void  set_char_height(TextAreaOverlayElement *taoe ,  Real height ){ taoe->setCharHeight( height);}

	DECLDIR  Real  get_char_height(TextAreaOverlayElement *taoe  ){return  taoe->getCharHeight();}

	DECLDIR  void  set_space_width(TextAreaOverlayElement *taoe ,  Real width ){ taoe->setSpaceWidth( width);}

	 DECLDIR  Real  get_space_width(TextAreaOverlayElement *taoe  ){return  taoe->getSpaceWidth();}

	 DECLDIR  void  set_font_name(TextAreaOverlayElement *taoe ,  const String& font ){ taoe->setFontName( font);}

	DECLDIR  void  setUpHPbars(Viewport* vp)
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

		hp1 = createHpElement(10.0, 10.0, "hp1");
		hp2 = createHpElement(-200+ vp->getActualWidth(), 10.0, "hp2");

		// Create an overlay, and add the panel
		Overlay* overlay = overlayManager.create("OverlayName");
		overlay->add2D(panel);

		// Add the text areas to the panel
		panel->addChild(hp1);
		panel->addChild(hp2);

		// Show the overlay
		overlay->show();
	}

	DECLDIR  void  setHPbarValue(int value, int bar)
	{
		char text[64] = "";

		sprintf(text, "%d", value);

		if (bar == 1)
			hp1->setCaption(text);
		else
			hp2->setCaption(text);
	}
}