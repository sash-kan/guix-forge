;;; guix-forge --- Guix software forge meta-service
;;; Copyright © 2022 Arun Isaac <arunisaac@systemreboot.net>
;;;
;;; This file is part of guix-forge.
;;;
;;; guix-forge is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published
;;; by the Free Software Foundation, either version 3 of the License,
;;; or (at your option) any later version.
;;;
;;; guix-forge is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with guix-forge.  If not, see
;;; <https://www.gnu.org/licenses/>.

(define-module (doc skribilo)
  #:use-module (rnrs conditions)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-28)
  #:use-module (srfi srfi-171)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (skribilo ast)
  #:use-module (skribilo engine)
  #:use-module (skribilo lib)
  #:use-module (skribilo writer)
  #:use-module (skribilo package base)
  #:use-module (skribilo parameters)
  #:use-module (skribilo source lisp)
  #:use-module (skribilo utils keywords)
  #:export (file
            command
            scheme-source
            scheme-source-form
            source-ref
            record-documentation
            record-field))

;; Constants
(define %source-uri-base
  "https://git.systemreboot.net/guix-forge/tree/")

;; Aliases
(define file samp)
(define command code)

;; Abbreviations
(define-markup (abbr #:rest opts
		     #:key (ident #f) (class "abbr") (short #f) (long #f))
  (new container
       (markup 'abbr)
       (ident (or ident (symbol->string (gensym "abbr"))))
       (class class)
       (loc &invocation-location)
       (required-options '(#:short #:long))
       (options `((#:short ,short)
		  (#:long ,long)
		  ,@(the-options opts #:ident #:class #:short #:long)))
       (body (the-body opts))))

;; S-exp source links
(define (source-uri file start-line end-line)
  "Return a URI referring to source FILE from START-LINE to END-LINE."
  (string-append %source-uri-base
                 file
                 "#n"
                 (number->string start-line)))

(define (sexp-position str regexp)
  "Return (START . END) where START is the start of the match to
REGEXP in STR and END is the end of the sexp beginning at START. START
and END are character positions indexed from 0. If multiple matches
are found, error out."
  (cond
   ((string-match regexp str)
    => (lambda (match-struct)
         (let ((start (match:start match-struct)))
           (if (string-match regexp (substring str (1+ start)))
               (raise-exception (condition (make-message-condition
                                            (format "source-ref: regexp ~s found on multiple lines"
                                                    regexp))
                                           (make-irritants-condition regexp)))
               (cons start
                     (1- (- (string-length str)
                            (string-length
                             (call-with-input-string (substring str start)
                               (lambda (port)
                                 (read port)
                                 (get-string-all port)))))))))))
   (else
    (raise-exception (condition (make-message-condition
                                 (format "source-ref: regexp ~s not found" regexp))
                                (make-irritants-condition regexp))))))

(define (position->line-number str position)
  "Return the line number in STR corresponding to POSITION."
  (string-fold (lambda (c result)
                 (if (char=? c #\newline)
                     (1+ result)
                     result))
               1
               (substring str 0 position)))

(define (sexp-file-lines file regexp)
  "Return (START . END) where START is the start of the match to
REGEXP in STR and END is the end of the sexp beginning at START. START
and END are line numbers indexed from 1."
  (let ((str (call-with-input-file file get-string-all)))
    (match (sexp-position str regexp)
      ((start . end)
       (cons (position->line-number str start)
             (position->line-number str end))))))

(define (source-ref file regexp text)
  "Link to S-expression in FILE whose beginning matches REGEXP. TEXT
is the text of the link."
  (ref #:url (match (sexp-file-lines (search-path (*source-path*) file)
                                     regexp)
               ((start-line . end-line)
                (source-uri file start-line end-line)))
       #:text text))

;; Extract forms from scheme source
(define (scheme-source-form file regexp)
  "Extract form from scheme source FILE whose beginning matches
REGEXP. Return it enclosed in a prog form."
  (prog (match (sexp-file-lines (search-path (*source-path*) file)
                                regexp)
          ((start . stop)
           (source #:language scheme
                   #:file file
                   #:start (1- start)
                   #:stop (1- stop))))
        #:line #f))

(define-record-type <record>
  (record identifier fields)
  record?
  (identifier record-identifier)
  (fields record-fields))

(define-record-type <no-default>
  (no-default)
  no-default?)

(define-record-type <record-field>
  (make-record-field identifier getter default documentation)
  record-field?
  (identifier record-field-identifier)
  (getter record-field-getter)
  (default record-field-default)
  (documentation record-field-documentation))

(define (field-sexp->record-field sexp)
  "Return a <record-field> object describing the Guix record defined
by SEXP, an S-expression."
  (match sexp
    ((identifier getter other ...)
     (make-record-field identifier
                        getter
                        (fold (lambda (element result)
                                (match element
                                  (('default default) default)
                                  (_ result)))
                              (no-default)
                              other)
                        #f))))

(define (record-sexp->record sexp)
  "Convert SEXP defining a Guix record type to a <record> object
describing it."
  (match sexp
    (('define-record-type* identifier rest ...)
     (record identifier
             (map field-sexp->record-field
                  (drop-while symbol? rest))))))

(define (find-record-definition file identifier)
  "Find record identified by IDENTIFIER, a symbol, in FILE."
  (call-with-input-file file
    (cut port-transduce
         (tmap identity)
         (rany (lambda (sexp)
                 (match sexp
                   (('define-record-type* record-type _ ...)
                    (and (eq? record-type identifier)
                         (record-sexp->record sexp)))
                   (_ #f))))
         read
         <>)))

(define* (record-field identifier documentation #:key default)
  "Document record field identified by IDENTIFIER, a symbol, with the
DOCUMENTATION string. DEFAULT is an optional textual description of
the default value. DEFAULT, when specified, will override the default
value extracted from the source."
  (make-record-field identifier #f default documentation))

(define (expression->string exp)
  "Return EXP as a human-readable string. In particular, quote forms
are printed using the quote symbol."
  (match exp
    (('quote exp)
     (string-append "'" (expression->string exp)))
    (_ (format "~s" exp))))

(define (record-documentation file identifier . fields)
  "Document record identified by IDENTIFIER, a symbol, in FILE. FIELDS
are a list of <record-field> objects."
  (let ((record (or (find-record-definition (search-path (*source-path*) file)
                                            identifier)
                    (raise-exception (condition (make-message-condition
                                                 (format "Unknown record ~a in ~a"
                                                         identifier file)))))))
    ;; Run sanity checks.
    (let* ((documented-fields (map (compose string->symbol record-field-identifier)
                                   fields))
           (fields-in-record (map record-field-identifier
                                  (record-fields record)))
           (undocumented-fields (lset-difference eq? fields-in-record documented-fields))
           (unknown-fields (lset-difference eq? documented-fields fields-in-record)))
      (unless (null? undocumented-fields)
        (raise-exception (condition (make-message-condition
                                     (format "Undocumented fields ~a in ~a record documentation"
                                             undocumented-fields
                                             identifier))
                                    (make-irritants-condition undocumented-fields))))
      (unless (null? unknown-fields)
        (raise-exception (condition (make-message-condition
                                     (format "Unknown fields ~a in ~a record documentation"
                                             unknown-fields
                                             identifier))
                                    (make-irritants-condition unknown-fields)))))
    ;; Generate markup.
    (item #:key (let ((identifier (symbol->string identifier)))
                  (list (list "Record Type: "
                              (mark identifier)
                              (index #:note "record type" identifier)
                              (source-ref file
                                          (string-append "\\(define-record-type\\* " identifier)
                                          (code identifier)))))
          (apply description
                 (map (lambda (documented-field)
                        (let* ((identifier (record-field-identifier documented-field))
                               (record-field (find (lambda (field)
                                                     (eq? (record-field-identifier field)
                                                          (string->symbol identifier)))
                                                   (record-fields record))))
                          (item #:key
                                (cond
                                 ;; No default value
                                 ((no-default? (record-field-default record-field))
                                  (code identifier))
                                 ;; Default value in documentation
                                 ((record-field-default documented-field)
                                  => (lambda (default)
                                       (list (append (list (code identifier) " (Default: ")
                                                     default
                                                     (list ")")))))
                                 ;; Default value from the source
                                 (else (list (list (code identifier) " (Default: "
                                                   (code (expression->string
                                                          (record-field-default record-field)))
                                                   ")"))))
                                (record-field-documentation documented-field))))
                      fields)))))

;; HTML engine customizations
(let ((html-engine (find-engine 'html)))
  (engine-custom-set! html-engine 'css "/style.css")
  (engine-custom-set! html-engine 'charset "UTF-8")
  (markup-writer 'abbr html-engine
                 #:options '(#:short #:long)
                 #:action (lambda (markup engine)
                            (display (format "<abbr title=\"~a\">~a</abbr> (~a)"
                                             (markup-option markup #:long)
                                             (markup-option markup #:short)
                                             (markup-option markup #:long))
                                     (current-output-port)))))
