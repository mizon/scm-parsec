(import (prefix (scm-parsec combinator) parsec:)
        (srfi srfi-64 testing)
        (oop goops))

(test-begin "combinators")

(let ([context (make parsec:<parser-context> #:succeed #t #:value "fuga" #:string "hogefuga")])
  (test-assert "context-succeed?" (parsec:context-succeed? context)))

(let ([string-parser (parsec:-string "hogefuga")])
  (test-assert "accept string parser" (parsec:parser-accept? string-parser "hogefuga"))
  (test-assert "not accept string parser" (not (parsec:parser-accept? string-parser "noga")))
  (test-equal "success string parser" "hogefuga" (parsec:parser-run string-parser "hogefuga"))
  (test-equal "fail string parser" #f (parsec:parser-accept? string-parser "noga")))

(let ([regexp-parser (parsec:regexp "number: [0-9][0-9][0-9]")])
  (test-equal "success regexp parser" "number: 123" (parsec:parser-run regexp-parser "number: 123")))

(test-end)
