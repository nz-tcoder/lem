(defsystem "lem-posix-shell-mode"
  :depends-on ("lem-core"
               #+#.(cl:if (asdf:find-system :async-process cl:nil) '(and) '(or)) "lem-process")

  :serial t
  :components ((:file "posix-shell-mode")
               #+#.(cl:if (asdf:find-system :async-process cl:nil) '(and) '(or))
               (:file "run-shell-mode")))
