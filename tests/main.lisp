(defpackage hdb-resale-pricing/tests/main
  (:use #:cl
        #:hdb-resale-pricing
        #:rove
        #:drakma)
  (:import-from #:hdb-resale-pricing))
(in-package :hdb-resale-pricing/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :hdb-resale-pricing)' in your Lisp.

(deftest test-make-transactions-query
  (testing "when all parameters are supplied"
    (ok (string-equal (make-transactions-query :town "yishun" :block-number "798" :flat-type "5 room")
      "{\"town\":\"yishun\",\"block\":\"798\",\"flat_type\":\"5 room\"}")))
  (testing "when there are missing parameters"
    (ok (string-equal (make-transactions-query :block-number "798" :flat-type "5 room")
      "{\"block\":\"798\",\"flat_type\":\"5 room\"}"))
    (ok (string-equal (make-transactions-query :flat-type "5 room")
      "{\"flat_type\":\"5 room\"}"))
    (ok (string-equal (make-transactions-query)
      "{}"))))

(deftest test-make-transactions-query-string 
  (testing "make transactions query string"
    (ok (string-equal (make-transactions-query-string :town "yishun" :block-number "798" :flat-type "5 room")
     (concatenate 'string "resource_id=42ff9cfe-abe5-4b54-beda-c88f9bb438ee&q=" 
      (drakma:url-encode "{\"town\":\"yishun\",\"block\":\"798\",\"flat_type\":\"5 room\"}" :utf-8) 
      "&sort=" (drakma:url-encode "month desc" :utf-8))))))

(deftest test-make-transactions-curl-command 
  (testing "make transactions curl command" 
    (ok (string-equal (make-transactions-curl-command :town "yishun" :block-number "798" :flat-type "5 room") 
      (concatenate 'string "curl -v -X GET \"https://data.gov.sg/api/action/datastore_search?" 
        (make-transactions-query-string :town "yishun" :block-number "798" :flat-type "5 room") "\"")))))

(deftest test-make-property-query
  (testing "when all parameters are supplied"
    (ok (string-equal (make-property-query :street "yishun" :block-number "798")
      "{\"street\":\"yishun\",\"blk_no\":\"798\"}")))
  (testing "when there are missing parameters"
    (ok (string-equal (make-property-query :block-number "798")
      "{\"blk_no\":\"798\"}"))
    (ok (string-equal (make-property-query)
      "{}"))))

(deftest test-make-property-query-string 
  (testing "make property query string"
    (ok (string-equal (make-property-query-string :street "yishun" :block-number "798")
     (concatenate 'string "resource_id=482bfa14-2977-4035-9c61-c85f871daf4e&q=" 
      (drakma:url-encode "{\"street\":\"yishun\",\"blk_no\":\"798\"}" :utf-8))))))

(deftest test-make-property-curl-command 
  (testing "make query curl command" 
    (ok (string-equal (make-property-curl-command :street "yishun" :block-number "798") 
      (concatenate 'string "curl -v -X GET \"https://data.gov.sg/api/action/datastore_search?" 
        (make-property-query-string :street "yishun" :block-number "798") "\"")))))
