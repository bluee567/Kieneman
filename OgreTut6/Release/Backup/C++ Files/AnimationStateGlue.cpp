#include "GlueDeclarations.h"

extern "C"
{
	DECLDIR const  str get_animation_name(AnimationState *as)
	{return (as->getAnimationName()).c_str();}

	DECLDIR  Real  get_time_position(AnimationState *as)
{return  as->getTimePosition();}

	DECLDIR  void  set_time_position(AnimationState *as , Real timePos)
{ as->setTimePosition(timePos);}

	DECLDIR  Real  get_length(AnimationState *as)
{return  as->getLength();}

	DECLDIR  void  set_length(AnimationState *as , Real len)
{ as->setLength(len);}

	DECLDIR  Real  get_weight(AnimationState *as  )
{return  as->getWeight();}

	DECLDIR  void  set_weight(AnimationState *as , Real weight)
{ as->setWeight(weight);}

	DECLDIR  void  add_time(AnimationState *as , Real offset)
{ as->addTime(offset);}

	DECLDIR  bool  has_ended(AnimationState *as  )
{return  as->hasEnded();}

	DECLDIR  bool  get_enabled(AnimationState *as  )
{return  as->getEnabled();}

	DECLDIR  void  set_enabled(AnimationState *as , bool enabled)
{ as->setEnabled(enabled);}

	DECLDIR  void  set_loop(AnimationState *as , bool loop)
{ as->setLoop(loop);}

	DECLDIR  bool  get_loop(AnimationState *as  )
{return  as->getLoop();}

	DECLDIR  void  copy_state_from(AnimationState *as , const AnimationState* const animState)
{as->copyStateFrom(*animState);}

	DECLDIR  AnimationStateSet*  get_anim_parent(AnimationState *as)
{return  as->getParent();}
}