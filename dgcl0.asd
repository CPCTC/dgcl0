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
          "vehicle"
          "env"
          ;"env-logic"
          "draw"
          ))
     (:file "defvehicle"
        :depends-on (
          "packages"
          "env"
          "vehicle"
          ))
     (:file "env"
        :depends-on (
          "packages"
          "vehicle"
          ))
     (:file "draw"
        :depends-on (
          "packages"
          "vehicle"
          "env"
          ))
     (:file "vehicle-logic"
        :depends-on (
          "packages"
          "env"
          "vehicle"
          ))
     (:file "vehicle"
        :depends-on (
          "packages"
          ))
     ))))
