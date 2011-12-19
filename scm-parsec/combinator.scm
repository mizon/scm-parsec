(library (scm-parsec combinator)
  (export parser-run parser-accept? <parser-context> context-succeed?
          context-value context-string
          -string any regexp)

  (import (only (rnrs) define lambda let if =)
          (prefix (rnrs) rnrs:)
          (rnrs)
          (oop goops)
          (srfi srfi-13))

  (define (parser-run parser string)
    (context-value (parser (make <parser-context> #:suceed #t #:value #f #:string string))))

  (define (parser-accept? parser string)
    (context-succeed? (parser (make <parser-context> #:suceed #t #:value #f #:string string))))


  (define-class <parser-context> ()
    [suceed #:init-keyword #:succeed #:getter context-succeed?]
    [value #:init-keyword #:value #:getter context-value]
    [string #:init-keyword #:string #:getter context-string])

  (define-method (context-eof? [self <parser-context>])
    (= (string-length (context-string self)) 0))

  (define (parser-return value string)
    (make <parser-context> #:succeed #t #:string string #:value value))

  (define (parser-fail)
    (make <parser-context> #:succeed #f))


  ;; Combinators.

  (define (any context)
    (parser-return (substring (context-string context) 0 1)
                   (substring (context-string context) 1)))

  (define (-string str)
    (lambda (context)
      (let ([matched-len (string-prefix-length str (context-string context))])
        (if (= matched-len (string-length str))
            (parser-return str (substring (context-string context) matched-len))
            (parser-fail)))))

  (define (regexp reg)
    (lambda (context)
      (let ([matched (string-match (string-append "\\A" reg) (context-string context))])
        (if matched
            (parser-return (match:substring matched) (match:suffix matched))
            (parser-fail))))))
