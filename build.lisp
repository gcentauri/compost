(ql:quickload :compost)
(clack.util:find-handler :hunchentoot)
(local-time:reread-timezone-repository )
(sb-ext:save-lisp-and-die #P"compost"
                          :toplevel #'compost::start-loop
                          :executable t)
