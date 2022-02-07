(defsystem :burke
  :depends-on ()
  :components
  ((:file "packages")
   (:file "interpret" :depends-on ("packages"))
   (:file "type" :depends-on ("packages"))
   (:file "info" :depends-on ("type" "packages"))
   (:module "ir"
    :depends-on ("info" "packages")
    :components ((:file "ir")
                 (:file "instructions" :depends-on ("ir"))
                 (:file "copy" :depends-on ("ir"))
                 (:file "assemble" :depends-on ("ir"))
                 (:file "disassemble" :depends-on ("ir"))
                 (:file "verify" :depends-on ("instructions" "ir"))))
   (:file "compile-initial" :depends-on ("ir" "interpret" "packages"))
   (:file "runtime" :depends-on ("interpret" "packages"))
   (:file "ir2cl" :depends-on ("interpret" "ir" "packages"))
   (:file "flow" :depends-on ("info" "ir" "packages"))
   (:file "optimize" :depends-on ("flow" "ir" "packages"))
   (:file "compile" :depends-on ("interpret" "runtime" "optimize" "ir2cl"
                                             "packages"))
   (:file "ground" :depends-on ("compile" "interpret" "packages"))
   (:file "repl" :depends-on ("ground" "interpret" "packages"))))
