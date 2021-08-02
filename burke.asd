(defsystem :burke
  :depends-on ()
  :components
  ((:file "packages")
   (:file "interpret" :depends-on ("packages"))
   (:file "ir" :depends-on ("packages"))
   (:file "build" :depends-on ("ir" "packages"))
   (:file "compile-initial" :depends-on ("build" "interpret" "packages"))
   (:file "runtime" :depends-on ("interpret" "packages"))
   (:file "ir2cl" :depends-on ("interpret" "ir" "packages"))
   (:file "compile" :depends-on ("interpret" "runtime" "ir2cl" "packages"))))
