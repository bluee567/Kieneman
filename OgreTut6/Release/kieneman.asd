(push "./cl-store/" asdf:*central-registry*)

(defsystem kieneman
  :name "kineman"
  :components ((:file "Kieneman")
	       (:file "KienemanMain"
		      :depends-on ("Kieneman" "misc-macros" "defclass+" "ffitypes" "input" "mortal" "hitboxes"))
	       (:file "run-kieneman"
		      :depends-on ("Kieneman" "KienemanMain" "ffiOgreNode" "CreateFighterObjs"))
	       (:file "misc-macros"
		      :depends-on ("Kieneman"))
	       
	       (:file "defclass+"
		      :depends-on ("Kieneman" "misc-macros"))
	       (:file "ffitypes"
		      :depends-on ("Kieneman" "defclass+"))
	       (:file "input"
		      :depends-on ("Kieneman" "ffiOgreNode"))

	       (:file "ffiogre"
		      :depends-on ("Kieneman" "ffitypes"))
	       (:file "ffiOgreAnimation"
		      :depends-on ("Kieneman" "ffitypes" "ffiogre" "misc-macros"))
	       (:file "ffiOgreMath"
		      :depends-on ("Kieneman" "ffitypes" "ffiOgreAnimation"))
	       (:file "ffiOgreNode"
		      :depends-on ("Kieneman" "ffitypes" "ffiOgreMath"))

	       (:file "mortal"
		      :depends-on ("Kieneman" "input"))
	       (:file "hitboxes"
		      :depends-on ("Kieneman" "mortal"))

	       
	       (:file "FighterObjs"
		      :depends-on ("Kieneman" "KienemanMain"))
	       (:file "state-init"
		      :depends-on ("Kieneman" "FighterObjs"))
	       (:file "animation-init"
		      :depends-on ("Kieneman" "state-init"))
	       (:file "states"
		      :depends-on ("Kieneman" "animation-init"))
	       (:file "state-transitions"
		      :depends-on ("Kieneman" "states"))
	       (:file "attacks"
		      :depends-on ("Kieneman" "state-init" "state-transitions" "hitboxes"))

	       (:file "CreateFighterObjs"
		      :depends-on ("Kieneman" "KienemanMain" "attacks" "input")))
  
  :depends-on ("cl-store" "premutations" "infix"))