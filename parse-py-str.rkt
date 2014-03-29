#lang plai

(require "parse-python.rkt")
(require "get-structured-python.rkt")

(define (parse-py-str str)
  (get-structured-python (parse-python/string str "/usr/bin/python3")))
