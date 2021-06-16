;;;; Provides all of this project's packages.

;;; Internal Package ;;;

(defpackage :dgcl0-int
  (:use :cl))

;;; Package 'dgcl0' ;;;
;;; Contains the API for controlling and running the game.
(let ((ext-syms (list

        :main       ;; Load vehicle files, then start the game.
        ;; Syntax:
        ;; main argv
        ;; argv::= (dgcl0 vehicle-file+)
        ;;
        ;; Arguments and Values:
        ;; vehicle-file---a pathname designator.
        ;;
        ;; Description:
        ;; main loads each vehicle file designated by
        ;; vehicle-file, then invokes run. Vehicle files
        ;; are assumed to be lisp source code which contains
        ;; at least one defvehicle form.

        :run        ;; Start the game.
        ;; Syntax:
        ;; run &optional worldstate
        ;;
        ;; Arguments and Values:
        ;; worldstate---a worldstate object.
        ;;
        ;; Description:
        ;; run simulates the worldstate worldstate.
        ;; After each intermediate simulation step,
        ;; the worldstate is displayed, and
        ;; execution is paused until the enter key
        ;; is pressed.

        :defvehicle ;; Create a vehicle and add it to the worldstate.
        ;; Syntax:
        ;; defvehicle name &key nodes
        ;; nodes::= (node-spec*)
        ;; node-spec::= (lambda pos no-connect-side*)
        ;; pos::= (y x)
        ;;
        ;; Arguments and Values:
        ;; lambda---a form
        ;; no-connect-side---a direction specifier, as described below; not evaluated
        ;; y---an integer
        ;; x---an integer
        ;;
        ;; Description:
        ;; defvehicle creates a new vehicle and adds it to the default worldstate.
        ;;
        ;; The vehicle is constructed by first creating a node from each node-spec.
        ;; Each node contains a function object obtained by evaluating the lambda
        ;; of the node specifier.
        ;;
        ;; Next, every pair of nodes are connected if the following criteria are met:
        ;;   - The pos's of the corresponding node-specs represent points on a
        ;;     cartesian grid that are adjacent to each other in the cardinal directions.
        ;;   - Neither node's corresponding node-spec specifies a no-connect-side that
        ;;     designates the pos of the other node's node-spec.
        ;;
        ;; A direction specifier is an integer or a case-insensitive string designator.
        ;; It corresponds with a direction vector in the manner given in the following table.
        ;;
        ;;     Direction Specifier | Direction Vector
        ;;     =================== | ================
        ;;     0                   | ( 0  1 )
        ;;     1                   | (-1  0 )
        ;;     2                   | ( 0 -1 )
        ;;     3                   | ( 1  0 )
        ;;     "right"             | ( 0  1 )
        ;;     "up"                | (-1  0 )
        ;;     "left"              | ( 0 -1 )
        ;;     "down"              | ( 1  0 )
        ;;     "east"              | ( 0  1 )
        ;;     "north"             | (-1  0 )
        ;;     "west"              | ( 0 -1 )
        ;;     "south"             | ( 1  0 )
        ;;
        ;; A direction designator designates the pos p+v, where p is the node-spec's
        ;; pos, and v is the direction designator's corresponding direction vector.
        ;;
        ;; After all nodes are connected, the full vehicle they constitute is placed into
        ;; the worldstate. The node whose node-spec came first in nodes is designated as
        ;; the vehicle's top-level node.

        )))
  (macrolet ((def-dgcl0-package (syms)
               `(defpackage :dgcl0
                  (:import-from :dgcl0-int ,@syms)
                  (:export ,@syms))))
    (def-dgcl0-package ext-syms)))

;;; Package 'driver' ;;;
;;; Contains the public interface for calls to the dgcl0 driver from loaded vehicles.
;;; Implementation in vehicle-logic.lisp.
(defpackage :dgcl0-driver
  (:export

    neighbor    ;; Get lambdas of connected nodes.
    ;; Syntax:
    ;; neighbor direction => lambda, present-p
    ;;
    ;; Arguments and Values:
    ;; direction---a direction specifier.
    ;; lambda---a function.
    ;; present-p---a generalized boolean.
    ;;
    ;; Description:
    ;; neighbor checks if the calling node is connected in direction to
    ;; another node. If so, then the lambda of that other node is
    ;; returned, and present-p is true. Otherwise, present-p is false,
    ;; and lambda is a function that has no effect and returns nil.

    sense	;; Gather information from the environment.
    ;; Syntax:
    ;; sense y x => char
    ;;
    ;; Arguments and Values:
    ;; y---an integer.
    ;; x---an integer.
    ;; char---a character.
    ;;
    ;; Description:
    ;; sense detects elements of the worldstate located along
    ;; the vector (y x) from the position of the calling node.

    shoot	;; Fire a projectile that can destroy nodes.
    ;; Syntax:
    ;; shoot y x => nil
    ;; Arguments and Values:
    ;; y---an integer.
    ;; x---an integer.
    ;;
    ;; Description:
    ;; shoots adds a bullet to the worldstate at the position
    ;; of the calling node plus (y x), that on every simulation
    ;; step travels in the direction (y x). If the bullet ever
    ;; occupies the same space as a vehicle node, then both the
    ;; bullet and the node are destroyed.

    translate   ;; Shift your vehicle in one of the four cardinal directions.
    ;; Syntax:
    ;; translate direction => nil
    ;;
    ;; Arguments and Values:
    ;; direction---a direction specifier.
    ;;
    ;; Description:
    ;; translate shifts the calling node's vehicle by one unit in
    ;; one of the four cardinal directions, designated by direction.
    ;; This vehicle motion can trigger collisions.

    rotate      ;; Pivot the entire vehicle around this node.
    ;; Syntax:
    ;; rotate turn-direction => nil
    ;;
    ;; Arguments and Values:
    ;; turn-direction---a direction specifier.
    ;;
    ;; Description:
    ;; Pivot the calling vehicle around the calling node by 90 degrees.
    ;; The direction turned is the difference between the direction north
    ;; and turn-direction. For example, (rotate "right") rotates the vehicle
    ;; 90 degrees to the right. This vehicle motion can trigger collisions.

    re-top      ;; Set the calling-node as the new top level for your entire vehicle.
    ;; Syntax:
    ;; re-top => nil
    ;;
    ;; Description:
    ;; re-top sets the calling node as the new top-level node of the
    ;; calling vehicle.

    connect     ;; Make a connection to a node nearby.
    ;; Syntax:
    ;; connect direction => nil
    ;;
    ;; Arguments and Values:
    ;; direction---a direction specifier.
    ;;
    ;; Description:
    ;; If there is a vehicle node in direction from the calling node,
    ;; then the calling node is connected to the other node. Otherwise,
    ;; an error is signaled.

    disconnect  ;; Remove a connection to a nearby node.
    ;; Syntax:
    ;; disconnect direction => nil
    ;;
    ;; Arguments and Values:
    ;; direction---a direction specifier
    ;;
    ;; Description:
    ;; disconnect disconnects the calling node from a connected node
    ;; in direction, if it exists. Otherwise, disconnect has no effect.

    explode     ;; Destroy the area around the caller.
    ;; Syntax:
    ;; explode => nil
    ;;
    ;; Description:
    ;; explode destroys the calling node and the 8 nodes surrounding
    ;; it. Control remains in the calling node, but it cannot be called
    ;; again.

    mimic       ;; Take on the appearance of another type of node
    ;; Syntax:
    ;; mimic char
    ;;
    ;; Arguments and Values:
    ;; char---a character
    ;;
    ;; Description:
    ;; mimic allows the calling node to take on the appearance of
    ;; another type of node. After a node calls mimic, then attempts
    ;; to sense the position of the calling node will return char.
    ))
