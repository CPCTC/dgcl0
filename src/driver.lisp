#|
(defpackage driver
  (:documentation "Public interface for calls to the dgcl0 driver from loaded vehicles.")
  (:export
    call	;; Call other functions, but the driver can track your location.
    sense	;; Gather information from the environment.
    shoot	;; Fire a projectile that can destroy nodes.
    rotate      ;; Reorder your four children.
    re-top      ;; Set the calling-node as the new top level for your entire vehicle.
    re-parent   ;; Pick a new parent for this node.
    translate   ;; Shift your vehicle in one of the four cardinal directions.
    children    ;; Check which children are still attached.
    release     ;; Split off a child.
    explode     ;; Destroy the area around the caller.
    mimic       ;; Take on the appearance of another type of node
    ))
|#

;;;; Containes the public interface for calls to the dgcl0 driver from loaded vehicles.
;;;; Non-trivial driver calls should call functions from vehicle-logic.lisp.

(in-package :dgcl0)

(defpackage :driver
  (:export
    neighbor    ;; get information about connected nodes (like thier lambda objects).
    sense	;; Gather information from the environment.
    shoot	;; Fire a projectile that can destroy nodes.
    translate   ;; Shift your vehicle in one of the four cardinal directions.
    rotate      ;; Pivot the entire vehicle around this node.
    re-top      ;; Set the calling-node as the new top level for your entire vehicle.
    connect     ;; Make a connection to a node nearby.
    disconnect  ;; Remove a connection to a nearby node.
    explode     ;; Destroy the area around the caller.
    mimic       ;; Take on the appearance of another type of node
    ))
