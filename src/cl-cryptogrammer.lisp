;; -*- mode: lisp; eval: (read-only-mode 0)  -*-
;; orginally generated from [[denote:20221225T091918][Cryptogram generator in Common Lisp]]
(defpackage #:cl-cryptogrammer
            (:use :cl))
(in-package #:cl-cryptogrammer)

;; load packages
(ql:quickload :alexandria)
(ql:quickload :cl-dbi)
(ql:quickload :dexador)
(ql:quickload :fset)
(ql:quickload :jonathan)
(ql:quickload :str)

(defvar *cryptogram-connection*
  (dbi:connect :sqlite3
               :database-name (truename "~/projects/homelab/cryptogrammer.db")))

(dbi:do-sql *cryptogram-connection*
            "create table if not exists quotations (qid string primary key, author string, quotation, string, date_generated timestamp default current_timestamp )")

(defun get-quotation (&key (minlength 40 min-supplied-p))
  "Retrieve a quotation via the Quotable API.
Return association list."
  (let ((quotation (jojo:parse (dex:get (format nil "https://api.quotable.io/random?minLength=~a" minlength)))))
    (pairlis
     (list :id
           :quote
           :author
           :length)
     (list (getf quotation :|_id|)
           (getf quotation :|content|)
           (getf quotation :|author|)
           (length (getf quotation :|content|))))))

(defun string-letter-frequency (str &optional (case-conversion t))
  "Count letter frequency of a string."
  (let*
      ((mystr (if case-conversion (string-downcase str) str))
       (mybag (fset:convert 'fset:bag mystr)))
    ;; (defparameter *gort* mybag)
    (fset:do-bag-pairs (value mult mybag)
                       ;; (format t "~a: ~a~%" value mult)
                       (format t "~a: ~a~%" mult value)
                       )))

(defun string-word-frequency (str &optional (case-conversion t))
  "Count words in a string."
  (let*
      ((mystr (if case-conversion (string-downcase str) str))
       (mybag (fset:convert 'fset:bag (uiop:split-string mystr :separator '(#\Space #\Tab #\Newline #\, #\; #\.)))))
    (fset:do-bag-pairs (value mult mybag)
                       (format t "~a: ~a~%" mult value))))


(defparameter *alphabet* "abcdefghijklmnopqrstuvwxyz" "Greg's handy ABCs" )

(defun cryptogram-key () ;; TODO assure same letter does not get used for key and value
  "Generate a simple letter substitution alist to use as an encryption key."

  (let*
      ((base-alphabet (copy-seq *alphabet*)) ;; shuffle may destructively modify its parameter
       (base-key (alexandria:shuffle (copy-seq *alphabet*))))
    (pairlis (map 'list #'identity base-key) (map 'list #'identity base-alphabet))))


(defparameter *current-cryptogram-key* (cryptogram-key))

(defun generate-cryptogram (&key (minlength 40 min-supplied-p) (refresh-all nil))
  "Generate a cryptogram from a quotation, a crytogram key.
TODO: Use global values if set, unless REFRESH-ALL is true."
  (let* ((quotation-alist (get-quotation :minlength minlength))
         (quotation       (cdr (assoc :quote quotation-alist)))
         (author          (cdr (assoc :author quotation-alist)))
         (qid             (cdr (assoc :id quotation-alist)))
         (downcase-quotation-list (coerce (string-downcase quotation) 'list))
         (cryptokey (cryptogram-key)))
    (format t "From author ~a" author) ; side effect to stdout
    ;; write to file
    ;; quotable_id|author|quotation|time_generated
    (str:to-file  "~/projects/homelab/cryptogram-log.org"
                  (format nil "|~a|~a|~a|~a ~C"
                          qid author quotation (local-time:now) #\newline)
                  :if-exists :append)
    (dbi:do-sql *cryptogram-connection*
                (format nil "insert or ignore into quotations (qid,author,quotation) values (\"~a\", \"~a\", \"~a\") "
                        qid author quotation))

    (coerce (map 'list
                 (lambda (c)
                   (if (alpha-char-p c)
                       (cdr (assoc c cryptokey))
                     c))
                 downcase-quotation-list) 'string)))

(defun %main (argv)
  "Parse CLI args."
  (when (member "-h" argv :test #'equal)
    ;; To properly parse command line arguments, use a third-party library such as
    ;; clingon, unix-opts, defmain, adopt… when needed.
    (help)
    (uiop:quit))
  (generate-cryptogram))

(defun main ()
  "Entry point for the executable.
  Reads command line arguments."
  ;; uiop:command-line-arguments returns a list of arguments (sans the script name).
  ;; We defer the work of parsing to %main because we call it also from the Roswell script.
  (%main (uiop:command-line-arguments)))
