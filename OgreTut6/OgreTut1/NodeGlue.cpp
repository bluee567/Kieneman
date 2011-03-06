#include "GlueDeclarations.h"
//#include <OgreMath.h>

using Ogre::Node;

extern "C"
{
DECLDIR const  str  get_name(Node *node  )
{return  node->getName().c_str();}

DECLDIR   Node*  get_parent(Node *node  )
{return  node->getParent();}

DECLDIR const  Quaternion *  get_orientation(Node *node  )
  {return  &(node->getOrientation());}

DECLDIR   void  set_orientation_q(Node *node ,  const Quaternion* q )
{ node->setOrientation(*q);}

DECLDIR   void  set_orientation_f(Node *node ,  Real w, Real x, Real y, Real z)
{ node->setOrientation( w, x, y, z);}

DECLDIR   void  reset_orientation(Node *node  )
{ node->resetOrientation();}

DECLDIR   void  set_position_v(Node *node , const Vector3* pos)
{ node->setPosition(*pos);}

DECLDIR   void  set_position_f(Node *node , Real x, Real y, Real z)
{ node->setPosition(x, y, z);}

DECLDIR const  Vector3*  get_position(Node *node  )
{return  &(node->getPosition());}

DECLDIR   void  set_scale_v(Node *node , const Vector3* scale)
{ node->setScale(*scale);}

DECLDIR   void  set_scale_f(Node *node , Real x, Real y, Real z)
{ node->setScale(x, y, z);}

DECLDIR const  Vector3 *  get_scale(Node *node  )
  {return  &(node->getScale());}

DECLDIR   void  set_inherit_orientation(Node *node , bool inherit)
{ node->setInheritOrientation(inherit);}

DECLDIR   bool  get_inherit_orientation(Node *node  )
{return  node->getInheritOrientation();}

DECLDIR   void  set_inherit_scale(Node *node , bool inherit)
{ node->setInheritScale(inherit);}

DECLDIR   bool  get_inherit_scale(Node *node  )
{return  node->getInheritScale();}

DECLDIR   void  scale_v(Node *node , const Vector3* scale)
{ node->scale(*scale);}

DECLDIR   void  scale_f(Node *node , Real x, Real y, Real z)
{ node->scale(x, y, z);}

DECLDIR   void  translate_v(Node *node , const Vector3* d, Ogre::Node::TransformSpace relativeTo)
{ node->translate(*d,relativeTo);}

DECLDIR   void  translate_f(Node *node , Real x, Real y, Real z, Ogre::Node::TransformSpace  relativeTo)
{ node->translate(x, y, z, relativeTo);}

DECLDIR   void  roll(Node *node , Real angle, Ogre::Node::TransformSpace relativeTo)
{ node->roll(Degree(angle), relativeTo);}

DECLDIR   void  pitch(Node *node , Real angle, Ogre::Node::TransformSpace relativeTo)
{ node->pitch(Degree(angle), relativeTo);}

DECLDIR   void  yaw(Node *node , Real angle, Ogre::Node::TransformSpace relativeTo)
{ node->yaw(Degree(angle), relativeTo);}

DECLDIR   void  rotate_v(Node *node , Vector3* axis, Radian* angle, Ogre::Node::TransformSpace relativeTo)
{ node->rotate(*axis, *angle, relativeTo);}

DECLDIR   void  rotate_q(Node *node , Quaternion* q, Ogre::Node::TransformSpace relativeTo)
{ node->rotate(*q, relativeTo);}

DECLDIR   void rotate_f(Node* node, Real w, Real x, Real y, Real z)
{node->rotate(Vector3(x,y,z), Radian(w) );}

DECLDIR   Node*  create_child(Node *node , Vector3* translate, Quaternion* rotate)
{return  node->createChild(*translate, *rotate);}

DECLDIR   Node*  create_child_named(Node *node , str name, Vector3* translate, Quaternion* rotate)
{return  node->createChild(name, *translate, *rotate);}

DECLDIR   void  add_child(Node *node , Node* child)
{ node->addChild(child);}

DECLDIR unsigned  short  num_children(Node *node  )
{return  node->numChildren();}

DECLDIR   Node*  get_child(Node *node , unsigned short index)
{return  node->getChild(index);}

DECLDIR   Node*  get_child_named(Node *node , str name)
{return  node->getChild(name);}

DECLDIR   Node*  remove_child(Node *node , Node* child)
{return  node->removeChild(child);}

DECLDIR   Node*  remove_child_index(Node *node , unsigned short index)
{return  node->removeChild(index);}

DECLDIR   Node*  remove_child_named(Node *node , const String& name)
{return  node->removeChild(name);}

DECLDIR   void  remove_all_children(Node *node  )
{ node->removeAllChildren();}

DECLDIR const  Quaternion *  _get_derived_orientation(Node *node  )
{return  &(node->_getDerivedOrientation());}

DECLDIR const  Vector3 *  _get_derived_position(Node *node  )
{return  &(node->_getDerivedPosition());}

DECLDIR const  Vector3 *  _get_derived_scale(Node *node  )
{return  &(node->_getDerivedScale());}

DECLDIR const  Matrix4*  _get_full_transform(Node *node  )
{return  &(node->_getFullTransform());}

DECLDIR const  MaterialPtr*  get_material(Node *node  )
	{return  &(node->getMaterial());}

DECLDIR  void  get_world_transforms(Node *node , Matrix4* xform)
{ node->getWorldTransforms(xform);}

//DECLDIR const  Quaternion*  get_world_orientation(Node *node  )
//{return  &(node->getWorldOrientation());}
//
//DECLDIR const  Vector3*  get_world_position(Node *node  )
//{return  &(node->getWorldPosition());}

DECLDIR   void  set_initial_state(Node *node  )
{ node->setInitialState();}

DECLDIR   void  reset_to_initial_state(Node *node  )
{ node->resetToInitialState();}

DECLDIR   void  need_update(Node *node , bool forceParentUpdate)
{ node->needUpdate(forceParentUpdate);}

DECLDIR   void  request_update(Node *node , Node* child, bool forceParentUpdate)
{ node->requestUpdate(child, forceParentUpdate);}

DECLDIR   void  cancel_update(Node *node , Node* child)
{ node->cancelUpdate(child);}

DECLDIR const  LightList*  get_lights(Node *node  )
	{return  &(node->getLights());}
}