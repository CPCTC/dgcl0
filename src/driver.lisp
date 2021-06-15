;;;; Contains the public interface for calls to the dgcl0 driver from loaded vehicles.
;;;; Implementation in vehicle-logic.lisp.

(in-package :dgcl0)

(defpackage :driver
  (:export
    neighbor    ;; Get lambdas of connected nodes.
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
