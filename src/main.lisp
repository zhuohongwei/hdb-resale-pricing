(defpackage hdb-resale-pricing
  (:use #:cl
        #:cl-json
        #:cl-json-path
        #:cl-ppcre
        #:drakma)
  (:export #:make-transactions-query
           #:make-transactions-query-string 
           #:make-transactions-curl-command
           #:fetch-transactions
           #:make-property-query
           #:make-property-query-string
           #:make-property-curl-command
           #:fetch-property
           #:assess-listing))
(in-package :hdb-resale-pricing)

(defvar *endpoint* "https://data.gov.sg/api/action/datastore_search") 
(defvar *transactions-resource-id* "f1765b54-a209-4718-8d38-a39237f502b3")
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

(defun get-json-value (path json)
  (cl-json-path:json-value-with-path path json))

(defun fetch-number-of-floors (&key street block-number)
  (get-json-value "result/records/0/max--floor--lvl" (fetch-property :street street :block-number block-number)))

(defun atoi (s)
  (parse-integer s :junk-allowed t))

(defun trim (s)
  (string-trim '(#\Space) s))

(defun make-range (start end)
  (do ((x start (+ x 1)) (xs nil (cons x xs))) 
      ((> x end) (nreverse xs))))

(defun extract-floors (floor-range)
  (let ((boundaries (map 'list (alexandria:compose #'atoi #'trim) (split "TO" floor-range))))
      (if (every #'integerp boundaries) 
        (make-range (car boundaries) (cadr boundaries)))))

(defun floor-to-category (floor)
  (cond ((> floor 8) :high)
        ((> floor 4) :mid)
        (t :low)))

(defun make-floor-predicate (floor-category)
  (lambda (transaction)
    (let* ((floor-range (get-json-value "storey--range" transaction))
          (categories (map 'list #'floor-to-category (extract-floors floor-range)))
          (matches (count-if #'(lambda (x) (eq x floor-category)) categories))
          (non-matches (- (length categories) matches)))
      (> matches non-matches))))

(defun format-transactions (transactions)
  (if (null transactions)
    (format t "~&No transactions yet.~%")
    (map nil #'format-transaction transactions)))

(defun format-transaction (transaction)
  (format t "~&~A~C~A~C~A~C~A~C~A~C~A~C~A" 
    (get-json-value "month" transaction) #\tab
    (get-json-value "flat--type" transaction) #\tab
    (get-json-value "storey--range" transaction) #\tab
    (get-json-value "block" transaction) #\tab
    (get-json-value "street--name" transaction) #\tab
    (get-json-value "remaining--lease" transaction) #\tab
    (format-price (get-json-value "resale--price" transaction))))

(defun format-price (string-price)
  (format nil "$~:D" (parse-integer string-price :junk-allowed t)))

(defun assess-listing (&key town block-number flat-type floor-category listing-price)
    (let* (
      (same-block-transactions (get-json-value "result/records" (fetch-transactions :town town :block-number block-number :flat-type flat-type)))
      (closest-transactions (remove-if-not (make-floor-predicate floor-category) same-block-transactions)) )
      (format-transactions closest-transactions)))