(defsystem "dgcl0"
  :components
  ((:module "src"
    :components
    ((:file "package"
        :depends-on (
          ))
     (:file "main"
        :depends-on (
          "package"
          "run"
          "env"
          ;; defvehicle at runtime
          ))
     (:file "run"
        :depends-on (
          "package"
          "vehicle"
          "env"
          ;"env-logic"
          ;"draw"
          ))
     (:file "defvehicle"
        :depends-on (
          "package"
          "env"
          "vehicle"
          ))
     (:file "env"
        :depends-on (
          "package"
          "vehicle"
          ))
     (:file "driver"
        :depends-on (
          "package"
          ))
     (:file "vehicle"
        :depends-on (
          "package"
          ))
     ))))
