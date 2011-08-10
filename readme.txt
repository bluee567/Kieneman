-  The Kineman Project  -

v0.51


Do you enjoy the thrill of carefully evading an attack in order to set up a devistating counter blow?
Do you find memorising the buttons and timing of combination attacks and move strings to be a tedious waste of time?
Are you unsatisfied throwing out random attacks, not because you are reacting to something skillfully, but simply out of the hope that you'll happen to beat out whatever your opponent does?
Have you ever wished that there existed a game where the action played out like a Hong Kong martial arts film, only with you in control?

If you've had any of the above thoughts, I hope Project Kieneman (working title) offers you something that satisfies.


- Controls -

The controls are currently assigned to 2 gamepads which must have analogue sticks. Gameplay without analogue controls is possible, but won't allow the degree of precision which would otherwise be possible with analogue enabled. Open the file "input-config.lisp" in a text editor and modify the 1p-input and 2p-input values under the heading 'PLAYER INPUT CONFIGURATIONS' at the bottom of the file.


Move - Hold the direction of motion and tap/hold MOVE. Aiming the direction upward will increase the movement distance/speed while aiming diaginally down will shorten it. This method of movement will allow fine control of approach and retreat given that movement requires a certain comitment. Any action which is affected in degree by the angle of the stick will be refered to as a butterfly action (notated with the symbol '%').


Dodge - Hold the direction of motion and tap/hold DODGE. Works similarly to MOVE, except it creates short bursts of movement.


Defend - Hold DEFENSE and the direction which you wish to defend. There are currently 2 diffrent defense directions, face (the default) and body (achieved by holding Downwards). Tapping DEFENCE will preform a quick parry like block, while holding DEFENCE for a longer period of time will result in a stronger block whick will take more time to recover from.


Attack-A - Preforms these actions.

	Tap: 				Jab - Will feint if cancel is also pressed (notated with the symbol '*'). Will clash will itself (notated with '#') [* #]
	Forward + Tap: 		Advancing Hook - [%]
	Dodge + Tap:		Bear Hug - Heavy/Unblockable (notated with '!') [!]
	

Attack-B - Preforms these actions.

	Tap:				Roundhouse - [* #]
	Forward + Tap:		Sidekick - Can be preformed from forward movement (notated '>'). [# >]
	UpForward + Tap&Hold:	Long Front - [* >]
	Up + Tap:			Bodykick - [#]
	Down + Tap:			Spinning Hook - [!]
	

Note that various attacks may have hidden properties which may not be apparent at first.


- Display -

Currently only white hit point bars are displayed at the top of the screen. Depleating your opponent's bar fully will result in your victory.


- Known Bugs -

* Loading the game without controllers connected may result in an error.