(ql:quickload :compost)
(clack.util:find-handler :hunchentoot)
(local-time:reread-timezone-repository )
(swank-loader:init :load-contribs t)
(swank:swank-require 'swank-io-package::swank-indentation)
(sb-ext:save-lisp-and-die #P"compost"
                          :toplevel #'compost::start-loop
                          :executable t)
