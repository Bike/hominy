(defpackage #:burke/type
  (:use #:cl)
  (:shadow #:type #:cons #:list
           #:typep #:subtypep #:car #:cdr #:symbol
           #:member)
  (:export #:top #:bot #:top-p #:bot-p)
  (:export #:type
           #:environment #:member #:elements #:symbol
           #:cons #:car #:cdr #:list
           #:applicative #:operative)
  (:export #:typep #:subtypep #:conjoin #:disjoin)
  (:export #:lookup))

(defpackage #:burke/flow
  (:use #:cl)
  (:local-nicknames (#:type #:burke/type)) ; circular, may be a problem?
  (:shadow #:type)
  (:export #:info #:default-info)
  (:export #:type)
  (:export #:forward-propagate-datum))

(defpackage #:burke/ir
  (:use #:cl)
  (:local-nicknames (#:flow #:burke/flow))
  (:shadow #:cons #:car #:cdr #:continue #:function #:inline #:eval #:sequence
           #:disassemble)
  (:export #:datum #:name #:map-users #:map-uses #:unusedp)
  (:export #:value)
  (:export #:parameter #:continuation)
  (:export #:continuation #:function #:parent #:parameter #:start #:terminator
           #:map-instructions #:add-child #:map-children #:children
           #:make-continuation)
  (:export #:enclosed)
  (:export #:function #:module #:enclosed #:start #:rcont
           #:map-continuations)
  (:export #:module #:add-function #:remove-function #:map-functions)
  (:export #:instruction #:continuation #:map-inputs #:inputs #:uinputs)
  (:export #:node)
  (:export #:terminator #:map-next)
  (:export #:use #:definition #:user #:info)
  (:export #:constant #:value)
  ;; Particular binds
  (:export #:lookup #:cons #:car #:cdr #:enclose #:augment)
  ;; Particular terminators
  (:export #:combination #:local-combination #:eval #:sequence #:continue)
  ;; Copying
  (:export #:copy)
  ;; Assembly
  (:export #:assemble-continuation #:assemble)
  (:export #:disassemble)
  ;; Verification
  (:export #:verify))

(defpackage #:burke
  (:use #:cl)
  (:shadow #:eval #:boolean #:variable #:ignore)
  (:local-nicknames (#:ir #:burke/ir)
                    (#:type #:burke/type)
                    (#:flow #:burke/flow)))
