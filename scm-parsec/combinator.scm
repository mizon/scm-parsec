(library (scm-parsec combinator)
  (export parser-run parser-accept? <parser-context> context-succeed?
          context-value context-string
          any string regexp sequence map *> <* or many not
          end)

  (import (ice-9 regex)
          (only (rnrs) define lambda let if =)
          (prefix (rnrs) rnrs:)
          (except (rnrs) string map or not)
          (oop goops)
          (except (srfi srfi-1) map)
          (srfi srfi-26)
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
    (make <parser-context> #:succeed #f #:string "" #:value #f))


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

  (define (map proc . parsers)
    (lambda (context)
      (let ([result ((apply sequence parsers) context)])
        (if (context-succeed? result)
            (parser-return (apply proc (context-value result)) (context-string result))
            (parser-fail)))))

  (define (sequence . parsers)
    (lambda (context)
      (let loop ([values '()]
                 [context context]
                 [parsers parsers])
        (if (null? parsers)
            (parser-return (reverse values) (context-string context))
            (let ([result ((car parsers) context)])
              (if (context-succeed? result)
                  (loop (cons (context-value result) values) result (cdr parsers))
                  (parser-fail)))))))

  (define (or parser . parsers)
    (lambda (context)
      (let ([result (parser context)])
        (if (context-succeed? result)
            result
            (if (null? parsers)
                (parser-fail)
                ((apply or parsers) context))))))

  (define (*> . parsers)
    (apply map (cons (lambda (. values) (last values)) parsers)))

  (define (<* . parsers)
    (apply map (cons (lambda (. values) (first values)) parsers)))

  (define (not parser)
    (lambda (context)
      (let ([result (parser context)])
        (if (context-succeed? result)
            (parser-fail)
            (parser-return #f (context-string context))))))

  (define (many parser)
    (lambda (context)
      (let loop ([old-result context]
                 [results '()])
        (let ([result (parser old-result)])
          (if (context-succeed? result)
              (loop result (cons result results))
              (parser-return (rnrs:map context-value (reverse results))
                             (context-string old-result)))))))

  (define (end)
    (lambda (context)
      (if (string=? (context-string context) "")
          (parser-return #f "")
          (parser-fail)))))
