(defpackage driver
  (:documentation "Public interface for calls to the dgcl0 driver from loaded vehicles.")
  (:export
    call	;; Call other functions, but the driver can track your location
    sense	;; Gather information from environment
    shoot	;; bang bang
    shield))	;; Create defensive structures
