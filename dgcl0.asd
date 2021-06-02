(defsystem "dgcl0"
  :components
  ((:module "src"
    :components
    ((:file "main"
        :depends-on (
          "vehicle"
          "step"
          "draw"
          ))
     (:file "step"
        :depends-on (
          "manipulate"
          "vehicle"
          "const"
          ))
     (:file "draw"
        :depends-on (
          "manipulate"
          "vehicle"
          "const"
          ))
     (:file "driver"
        :depends-on (
          ))
     (:file "manipulate"
        :depends-on (
          "vehicle"
          "const"
          ))
     (:file "vehicle"
        :depends-on (
          "const"
          ))
     (:file "const"
        :depends-on (
          ))))))
