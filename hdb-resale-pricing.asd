(defsystem "hdb-resale-pricing"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (#:drakma 
               #:cl-json
               #:cl-ppcre
               #:cl-json-path)
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "hdb-resale-pricing/tests"))))

(defsystem "hdb-resale-pricing/tests"
  :author ""
  :license ""
  :depends-on ("hdb-resale-pricing"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for hdb-resale-pricing"
  :perform (test-op (op c) (symbol-call :rove :run c)))
