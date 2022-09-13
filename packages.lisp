(defpackage #:burke/type
  (:use #:cl)
  (:shadow #:type #:cons #:list
           #:typep #:subtypep #:car #:cdr #:symbol
           #:member)
  (:export #:top #:bot #:top-p #:bot-p)
  (:export #:type
           #:environment #:member #:elements #:symbol
           #:cons #:car #:cdr #:list #:elemtype
           #:applicative #:unwrap #:operative)
  (:export #:typep #:subtypep #:conjoin/2 #:disjoin/2 #:conjoin #:disjoin
           #:unparse)
  (:export #:lookup))

(defpackage #:burke/info
  (:use #:cl)
  (:local-nicknames (#:type #:burke/type))
  (:shadow #:type)
  (:export #:info #:default-info
           #:join/2 #:meet/2 #:subinfop)
  (:export #:operative #:dynenvp
           #:local-operative #:data
           #:known-operative #:value
           #:applicative #:wrap #:unwrap
           #:macro #:known-macro #:expander
           #:constant)
  (:export #:type))

(defpackage #:burke/cenv
  (:use #:cl)
  (:local-nicknames (#:info #:burke/info)
                    (#:i #:burke/interpreter))
  (:export #:binding #:info)
  (:export #:cenvironment #:empty-cenv #:make-cenv #:augment1 #:lookup))

(defpackage #:burke/baselib
  (:use #:cl)
  (:local-nicknames (#:i #:burke/interpreter)
                    (#:syms #:burke/interpreter/syms)
                    (#:info #:burke/info)
                    (#:cenv #:burke/cenv))
  ;; Environment definition stuff
  (:export #:defenv #:*defining-environment*
           #:defop #:defapp #:defmac #:boolify #:defpred)
  ;; Derived operative internals, for use with compilers
  (:export #:derived-operative #:ptree #:eparam #:static-environment #:body)
  ;; Macros
  (:export #:macro #:make-macro #:expander)
  ;; Ultimate products
  (:export #:*base* #:*basec*))

(defpackage #:burke/ir
  (:use #:cl)
  (:local-nicknames (#:info #:burke/info))
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
  (:export #:replace-terminator)
  ;; Verification
  (:export #:verify))

(defpackage #:burke/flow
  (:use #:cl)
  (:local-nicknames (#:info #:burke/info)
                    (#:ir #:burke/ir)
                    (#:type #:burke/type))
  (:export #:forward-propagate-datum))

(defpackage #:burke
  (:use #:cl)
  (:shadow #:read #:read-from-string)
  (:local-nicknames (#:i #:burke/interpreter)
                    (#:baselib #:burke/baselib))
  (:export #:repl)
  (:export #:read #:read-from-string))
