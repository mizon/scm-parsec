(library (scm-parsec combinator)
  (export parser-run parser-accept? <parser-context> context-succeed?
          context-value context-string
          any string regexp <$> *> <* or)

  (import (ice-9 regex)
          (only (rnrs) define lambda let if =)
          (prefix (rnrs) rnrs:)
          (except (rnrs) string or)
          (oop goops)
          (except (srfi srfi-13) string))

  (define (parser-run parser string)
    (let ([context (parser (make <parser-context> #:suceed #t #:value #f #:string string))])
      (if (context-succeed? context)
          (context-value context)
          #f)))

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

  (define (string str)
    (lambda (context)
      (let ([matched-len (string-prefix-length str (context-string context))])
        (if (= matched-len (string-length str))
            (parser-return str (substring (context-string context) matched-len))
            (parser-fail)))))

  (define (regexp reg)
    (lambda (context)
      (let ([matched (string-match (string-append "^" reg) (context-string context))])
        (if matched
            (parser-return (match:substring matched) (match:suffix matched))
            (parser-fail)))))

  (define (<$> proc parser)
    (lambda (context)
      (let ([result (parser context)])
        (if (context-succeed? result)
            (parser-return (proc (context-value result)) (context-string result))
            (parser-fail)))))

  (define (or left right)
    (lambda (context)
      (let ([result (left context)])
        (if (context-succeed? result)
            result
            (right context)))))

  (define (*> left right)
    (lambda (context)
      (let ([result (left context)])
        (if (context-succeed? result)
            (right result)
            (parser-fail)))))

  (define (<* left right)
    (lambda (context)
      (let ([left-result (left context)])
        (if (context-succeed? left-result)
            (let ([right-result (right left-result)])
              (if (context-succeed? right-result)
                  (parser-return (context-value left-result) (context-string right-result))
                  (parser-fail)))
            (parser-fail))))))
