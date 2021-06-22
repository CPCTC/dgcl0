(defsystem "dgcl0"
  :components
  ((:module "src"
    :components
    ((:file "packages"
        :depends-on (
          ))
     (:file "main"
        :depends-on (
          "packages"
          "run"
          "env"
          ;; defvehicle at runtime
          ))
     (:file "run"
        :depends-on (
          "packages"
          "vehicle-logic"
          "vehicle"
          "env"
          "env-logic"
          "draw"
          ))
     (:file "defvehicle"
        :depends-on (
          "packages"
          "env"
          "vehicle"
          "directions"
          ))
     (:file "directions"
        :depends-on (
          "packages"
          ))
     (:file "env-logic"
        :depends-on (
          "packages"
          "env"
          "directions"
          ))
     (:file "env"
        :depends-on (
          "packages"
          "vehicle"
          "directions"
          ))
     (:file "draw"
        :depends-on (
          "packages"
          "vehicle"
          "env"
          "obj-types"
          ))
     (:file "vehicle-logic"
        :depends-on (
          "packages"
          "env"
          "vehicle"
          "directions"
          ))
     (:file "vehicle"
        :depends-on (
          "packages"
          "obj-types"
          "directions"
          ))
     (:file "obj-types"
        :depends-on (
          "packages"
          ))
     ))))
