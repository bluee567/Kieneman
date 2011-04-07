(in-package :kieneman)

(DEFAULT-FOREIGN-LANGUAGE :stdc)
(default-foreign-library "OgreTut1.dll")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Ogre Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FFI Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defmacro deffiout (fname &body plist)
;;   `(progn
;;      (def-call-out ,fname
;;        ,)
;;      (defun ,fname
;;        (,(mapcar
;; 	 (λx
;; 	  (case (car x)
;; 	    (:arguments
;; 	     (mapcar
;; 	      #'(lambda (arg)
;; 		  (case (car arg)
;; 		    (:optional (list))
;; 		    (otherwise arg)))
;; 	      (cdr x)))
;; 	    (otherwise x)))
;; 	 plist)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FFI Callout Definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;Animation
;;(load "ffiOgreAnimation.lisp")

;SceneManager
(def-call-out get-scene-manager
  (:name "get_scene_manager")
  (:arguments (name c-string))
  (:return-type scene-manager))

(def-call-out create-scene-manager
  (:name "create_scene_manager")
  (:arguments (st uint16) (name c-string))
  (:return-type scene-manager))

(def-call-out get-light
  (:name "get_light")
  (:arguments (name c-string))
  (:return-type light))

;Math
;;(load "ffiOgreMath.lisp")

;Node
;;(load "ffiOgreNode.lisp")


;;Scene Node

(def-call-out get-root-scene-node
  (:name "get_root_scene_node")
  (:arguments (sm scene-manager))
  (:return-type scene-node))

(def-call-out create-child-scene-node
  (:name "create_child_scene_node")
  (:arguments (node scene-node))
  (:return-type scene-node))

(def-call-out attach-object
  (:name "attach_object")
  (:arguments (node scene-node) (ent entity)))

(def-call-out create-camera
  (:name "create_camera")
  (:arguments (sm scene-manager) (name c-string))
  (:return-type camera))

(def-call-out set-ambient-light
  (:name "set_ambient_light")
  (:arguments (sm scene-manager) (r single-float) (g single-float) (b single-float) (a single-float))
  (:return-type ))

;;Camera

(def-call-out set-position-vec
  (:name "set_position_vec")
  (:arguments (cam camera) (vec vector3))
  (:return-type ))

(def-call-out move-cam-f
  (:name "move_cam_f")
  (:arguments (cam camera) (x single-float) (y single-float) (z single-float)))

(def-call-out set-cam-position
  (:name "set_cam_position")
  (:arguments (cam camera) (x single-float) (y single-float) (z single-float))
  (:return-type ))

(def-call-out look-at
  (:name "look_at")
  (:arguments (cam camera) (x single-float) (y single-float) (z single-float))
  (:return-type ))

(def-call-out add-viewport
  (:name "add_viewport")
  (:arguments (cam camera))
  (:return-type viewport))

(def-call-out set-background-colour
  (:name "set_background_colour")
  (:arguments (vp viewport) (r single-float) (g single-float) (b single-float) (a single-float))
  (:return-type ))

(def-call-out set-aspect-to-viewport
  (:name "set_aspect_to_viewport")
  (:arguments (cam camera) (vp viewport))
  (:return-type ))

;Entity

(def-call-out create-entity
  (:name "create_entity")
  (:arguments (mgr scene-manager) (name c-string) (mesh c-string))
  (:return-type entity))

(def-call-out destroy-entity
  (:name "destroy_entity")
  (:arguments (mgr scene-manager) (ent entity))
  (:return-type ))

(def-call-out get-parent-node
  (:name "get_parent_node")
  (:arguments (ent entity))
  (:return-type node))

(def-call-out set-material-name
  (:name "set_material_name")
  (:arguments (ent entity) (material c-string))
  (:return-type ))

(def-call-out set-visible
  (:name "set_visible")
  (:arguments (ent entity) (vis boolean))
  (:return-type ))

(def-call-out set-cast-shadows
  (:name "set_cast_shadows")
  (:arguments (ent entity) (tf boolean))
  (:return-type ))

(def-call-out get-animation-state
  (:name "get_animation_state")
  (:arguments (ent entity) (name c-string))
  (:return-type c-pointer))

(defun set-animation (entity anim-name &optional (looping t))
  "Sets the animation of the entity to the animation named
by anim-name. Returns a pointer to the animation state."
  (let ((anim (get-animation-state entity anim-name)))
    (set-enabled anim t)
    (set-loop anim t)
    (set-weight anim 1.0f0)
    anim))

(def-call-out make-hit-rectangle
  (:name "makeHitRectangle")
  (:arguments (x single-float) (y single-float) (width single-float) (height single-float) (material c-string) (sm scene-manager))
  (:return-type entity))
  
(def-call-out make-hit-triangle
  (:name "makeHitTriangle")
  (:arguments (x1 single-float) (y1 single-float) (x2 single-float) (y2 single-float) (x3 single-float) (y3 single-float) (material c-string) (sm scene-manager))
  (:return-type manual-object))
 
 (def-call-out update-hit-triangle
  (:name "updateHitTriangle")
  (:arguments (x1 single-float) (y1 single-float) (x2 single-float) (y2 single-float) (x3 single-float) (y3 single-float) (mo manual-object))
  (:return-type ))

(def-call-out reload-entity-skeleton
  (:name "reloadEntitySkeleton")
  (:arguments (ent entity))
  (:return-type ))
  
 ;Manual Object
 
(def-call-out destroy-manual-object
	(:name "destroy_manual_object")
	(:arguments (sm scene-manager) (mo manual-object)))

;Light

(def-call-out create-light
  (:name "create_light")
  (:arguments (mgr scene-manager) (name c-string))
  (:return-type light))

(def-call-out set-lt-type
  (:name "set_lt_type")
  (:arguments (lt light) (type int))
  (:return-type ))

(def-call-out set-lt-position
  (:name "set_lt_position")
  (:arguments (lt light) (x single-float) (y single-float) (z single-float))
  (:return-type ))

(def-call-out set-lt-diffuse-colour
  (:name "set_lt_diffuse_colour")
  (:arguments (lt light) (r single-float) (g single-float) (b single-float))
  (:return-type ))

(def-call-out set-lt-specular-colour
  (:name "set_lt_specular_colour")
  (:arguments (lt light) (r single-float) (g single-float) (b single-float))
  (:return-type ))

(def-call-out set-lt-direction
  (:name "set_lt_direction")
  (:arguments (lt light) (x single-float) (y single-float) (z single-float))
  (:return-type ))