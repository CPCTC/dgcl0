(require 'asdf)
(asdf:make 'dgcl0)
(save-lisp-and-die "dgcl0"
  :executable t
  :toplevel
    (lambda ()
      (dgcl0:main sb-ext:*posix-argv*)))
