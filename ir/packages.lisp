(defpackage #:burke/ir
  (:use #:cl)
  (:shadow #:cons #:car #:cdr #:continue #:function #:inline #:eval #:sequence)
  (:export #:datum #:name #:map-users)
  (:export #:value)
  (:export #:parameter #:continuation)
  (:export #:continuation #:function #:parent #:parameter #:start #:terminator
           #:map-instructions #:add-child #:map-children #:children
           #:make-continuation)
  (:export #:enclosed)
  (:export #:function #:module #:enclosed #:start #:rcont
           #:map-continuations)
  (:export #:module #:add-function #:remove-function #:map-functions)
  (:export #:instruction #:continuation #:map-inputs #:inputs)
  (:export #:bind #:prev #:next)
  (:export #:terminator #:map-next)
  (:export #:use #:definition #:user)
  (:export #:constant #:value)
  ;; Particular binds
  (:export #:lookup #:cons #:car #:cdr #:enclose #:augment)
  ;; Particular terminators
  (:export #:combination #:local-combination #:eval #:sequence #:continue)
  ;; Modifications
  (:export #:replace-datum #:replace-terminator)
  (:export #:inline #:outline)
  (:export #:builder #:build-before #:build-after #:build-continuation #:build)
  ;; Copying
  (:export #:copy)
  ;; Assembly
  (:export #:assemble-continuation #:assemble)
  ;; Verification
  (:export #:verify))
