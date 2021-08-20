(defsystem :burke
  :depends-on ()
  :components
  ((:file "packages" :depends-on ("ir")) ; for local nickname
   (:file "interpret" :depends-on ("packages"))
   (:module "ir"
    :components ((:file "packages")
                 (:file "ir" :depends-on ("packages"))
                 (:file "copy" :depends-on ("ir" "packages"))
                 (:file "modification" :depends-on ("packages"))
                 (:file "assemble" :depends-on ("modification" "packages"))
                 (:file "verify" :depends-on ("ir" "packages"))))
   (:file "compile-initial" :depends-on ("ir" "interpret" "packages"))
   (:file "runtime" :depends-on ("interpret" "packages"))
   (:file "ir2cl" :depends-on ("interpret" "ir" "packages"))
   #+(or)
   (:file "compile" :depends-on ("interpret" "runtime" "ir2cl" "packages"))))
