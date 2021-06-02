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
