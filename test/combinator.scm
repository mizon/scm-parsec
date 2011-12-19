(import (prefix (scm-parsec combinator) parsec:)
        (srfi srfi-64 testing)
        (oop goops)
        (only (guile) display)
        (prefix (rnrs) rnrs:))

(test-begin "combinators")

(let ([context (make parsec:<parser-context> #:succeed #t #:value "fuga" #:string "hogefuga")])
  (test-assert "context-succeed?" (parsec:context-succeed? context)))

(let ([string-parser (parsec:string "hogefuga")])
  (display parsec:string)
  (test-assert "accept string parser" (parsec:parser-accept? string-parser "hogefuga"))
  (test-assert "not accept string parser" (not (parsec:parser-accept? string-parser "noga")))
  (test-equal "success string parser" "hogefuga" (parsec:parser-run string-parser "hogefuga"))
  (test-equal "fail string parser" #f (parsec:parser-accept? string-parser "noga")))

(let ([regexp-parser (parsec:regexp "number: [0-9][0-9][0-9]")])
  (test-equal "success regexp parser" "number: 123" (parsec:parser-run regexp-parser "number: 123")))

(let ([right-composit-parser (parsec:*> (parsec:string "hoge") (parsec:string "fuga"))]
      [left-composit-parser (parsec:<* (parsec:string "hoge") (parsec:string "fuga"))])
  (test-equal "success right-composit-parser" "fuga" (parsec:parser-run right-composit-parser "hogefuga"))
  (test-equal "success left-composit-parser" "hoge" (parsec:parser-run left-composit-parser "hogefuga")))

(let ([map-parser (parsec:map rnrs:string-upcase (parsec:string "hoge"))]
      [map2-parser (parsec:map2 string-append (parsec:string "hoge") (parsec:string "fuga"))]
      [map3-parser (parsec:map3 string-append
                                (parsec:string "hoge")
                                (parsec:string "fuga")
                                (parsec:string "homu"))])
  (test-equal "success map-parser" "HOGE" (parsec:parser-run map-parser "hoge"))
  (test-equal "success map2-parser" "hogefuga" (parsec:parser-run map2-parser "hogefuga"))
  (test-equal "success map3-parser" "hogefugahomu" (parsec:parser-run map3-parser "hogefugahomu")))

(let ([or-parser (parsec:or (parsec:string "hoge") (parsec:string "fuga") (parsec:string "nuga"))])
  (test-equal "success or left" "hoge" (parsec:parser-run or-parser "hoge"))
  (test-equal "success or middle" "fuga" (parsec:parser-run or-parser "fuga"))
  (test-equal "success or right" "nuga" (parsec:parser-run or-parser "nuga")))

(test-end)
