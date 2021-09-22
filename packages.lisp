(defpackage #:burke/type
  (:use #:cl)
  (:shadow #:type #:null #:cons #:list
           #:subtypep #:car #:cdr #:ignore #:symbol)
  (:export #:type #:recur
           #:environment #:inert #:ignore #:symbol
           #:null #:cons #:car #:cdr #:list
           #:applicative #:operative)
  (:export #:subtypep))

(defpackage #:burke
  (:use #:cl)
  (:shadow #:eval #:boolean #:variable #:ignore)
  (:local-nicknames (#:ir #:burke/ir)
                    (#:type #:burke/type)))
