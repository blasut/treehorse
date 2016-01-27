(in-package :cl-user)
(defpackage treehorse-test-asd
  (:use :cl :asdf))
(in-package :treehorse-test-asd)

(defsystem treehorse-test
  :author "FullName"
  :license ""
  :depends-on (:treehorse
               :prove)
  :components ((:module "t"
                :components
                ((:file "treehorse"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
