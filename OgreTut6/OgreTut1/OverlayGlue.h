#ifndef OVERLAY_GLUE
#define OVERLAY_GLUE

#include <OgreTextAreaOverlayElement.h>

extern "C"
{
	DECLDIR  void  set_char_height(TextAreaOverlayElement *taoe ,  Real height );

	DECLDIR  Real  get_char_height(TextAreaOverlayElement *taoe  );

	DECLDIR  void  set_space_width(TextAreaOverlayElement *taoe ,  Real width );

	 DECLDIR  Real  get_space_width(TextAreaOverlayElement *taoe  );

	 DECLDIR  void  set_font_name(TextAreaOverlayElement *taoe ,  const String& font );

	DECLDIR  void  setUpHPbars(SceneManager* mgr ,Viewport* vp);

	DECLDIR  void  setHPbarValue(int value, int bar);
}

#endif