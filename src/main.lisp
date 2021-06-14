(defpackage hdb-resale-pricing
  (:use #:cl
        #:cl-json
        #:drakma)
  (:export #:make-transactions-query
           #:make-transactions-query-string 
           #:make-transactions-curl-command
           #:fetch-transactions
           #:make-property-query
           #:make-property-query-string
           #:make-property-curl-command
           #:fetch-property))
(in-package :hdb-resale-pricing)

(defvar *endpoint* "https://data.gov.sg/api/action/datastore_search") 
(defvar *transactions-resource-id* "42ff9cfe-abe5-4b54-beda-c88f9bb438ee")
(defvar *property-resource-id* "482bfa14-2977-4035-9c61-c85f871daf4e")
(defvar *transactions-sort* "month desc")

(defun make-transactions-query (&key (town nil) (block-number nil) (flat-type nil))
  (let ((params (remove-if #'(lambda (p) (null (cadr p))) 
      (list (list "town" town) (list "block" block-number) (list "flat_type" flat-type)))))
    (format nil "{~{~{~s~^:~}~^,~}}" params)))

(defun make-transactions-query-string (&key (town nil) (block-number nil) (flat-type nil))
  (concatenate 'string "resource_id=" *transactions-resource-id* "&q=" 
    (drakma:url-encode (make-transactions-query :town town :block-number block-number :flat-type flat-type) :utf-8) 
    "&sort=" (drakma:url-encode *transactions-sort* :utf-8)))

(defun make-transactions-curl-command (&key (town nil) (block-number nil) (flat-type nil))
  (concatenate 'string "curl -v -X GET \"" *endpoint* "?" 
      (make-transactions-query-string :town town :block-number block-number :flat-type flat-type) "\""))

(defun make-property-query (&key (street nil) (block-number nil))
  (let ((params (remove-if #'(lambda (p) (null (cadr p))) 
      (list (list "street" street) (list "blk_no" block-number)))))
    (format nil "{~{~{~s~^:~}~^,~}}" params)))

(defun make-property-query-string (&key (street nil) (block-number nil))
  (concatenate 'string "resource_id=" *property-resource-id* "&q=" 
    (drakma:url-encode (make-property-query :street street :block-number block-number) :utf-8)))

(defun make-property-curl-command (&key (street nil) (block-number nil))
    (concatenate 'string "curl -v -X GET \"" *endpoint* "?" 
      (make-property-query-string :street street :block-number block-number) "\""))

(defmacro fetch-json (curl-command)
  `(let ((response (uiop:run-program ,curl-command :output :string)))
    (with-input-from-string (s response)
      (json:decode-json s))))

(defun fetch-transactions (&key (town nil) (block-number nil) (flat-type nil))
  (fetch-json (make-transactions-curl-command :town town :block-number block-number :flat-type flat-type)))

(defun fetch-property (&key (street nil) (block-number nil))
  (fetch-json (make-property-curl-command :street street :block-number block-number))) 