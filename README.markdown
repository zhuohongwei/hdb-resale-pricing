# Hdb-Resale-Pricing

This is an experiment using [HDB resale prices API](https://data.gov.sg/dataset/resale-flat-prices) provided by the government.

## Usage

```lisp 
(ql:quickload :hdb-resale-pricing)

;;; to look up past transactions matching a 4-room listing for block 457 in Yishun. Listing price is currently not used, but may be used in future for comparison with past transactions.
(hdb-resale-pricing::assess-listing :town "yishun" :block-number "457" :flat-type "4 room" :floor-category :high :listing-price 500000)
```

## Installation

Clone this repository into one of the well known project directories, e.g., ~/common-lisp/

## Dependencies
- cl-json
- cl-json-path
