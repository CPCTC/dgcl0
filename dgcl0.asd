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
          ;; defvehicle at runtime
          ))
     (:file "env"
        :depends-on (
          "package"
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
