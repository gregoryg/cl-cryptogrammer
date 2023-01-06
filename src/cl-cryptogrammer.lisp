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

(defparameter *cryptogram-connection*
  (dbi:connect :sqlite3
               :database-name (truename "~/.config/cryptogrammer/cryptogrammer.db")))

(dbi:do-sql *cryptogram-connection*
            "create table if not exists quotations (qid string primary key, author string, quotation, string, date_generated timestamp default current_timestamp )")

(defun quotable-quotation (&key (minlength 40 min-supplied-p))
  "Retrieve a quotation from the Quotable.io API."
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

(defun fortune-local-quotation (&key (minlength 40 min-supplied-p))
  "Retrieve a quotation from the local OS fortune command."

  (let* ((output (str:split "%" (with-output-to-string (stream) (uiop:run-program (format nil "fortune -c -a -n ~d -l" minlength)  :output stream))))
         (author (str:replace-all (string #\Newline) "" (first output)))
         (quotation (str:replace-first (string #\Newline) "" (second output))))
    (pairlis (list :id :author :quote :length)
             (list "fortune-cli" author quotation (length quotation )))))

(defun get-quotation (&key (quote-source 'quotable) (minlength 40 min-supplied-p))
  "Retrieve a quotation via the Quotable API.
Return association list."
  (cond ((eq 'quotable quote-source)
         (quotable-quotation :minlength minlength))
        ((eq 'fortune-cli quote-source)
          (fortune-local-quotation :minlength minlength))
        (t nil)))

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

(defun generate-cryptogram (&key (minlength 40 min-supplied-p) (quote-source 'quotable) (refresh-all nil) (quotation nil))
  "Generate a cryptogram from a quotation, a crytogram key.
TODO: Use global values if set, unless REFRESH-ALL is true."
  (let* ((quotation-alist (if (not quotation)
                              (get-quotation :minlength minlength :quote-source quote-source)
                              (pairlis (list :id :quote :length)
                                       (list "CLI" quotation (length quotation)))))
         (quotation       (cdr (assoc :quote quotation-alist)))
         (author          (cdr (assoc :author quotation-alist)))
         (qid             (cdr (assoc :id quotation-alist)))
         (downcase-quotation-list (coerce (string-downcase quotation) 'list))
         (cryptokey (cryptogram-key)))
    (format t "From author ~a" author) ; side effect to stdout
    ;; write to file
    ;; quotable_id|author|quotation|time_generated
    (str:to-file  (truename "~/.config/cryptogrammer/cryptogram-log.org")
                  (format nil "|~a|~a|~a|~a ~C"
                          qid author quotation (local-time:now) #\newline)
                  :if-exists :append)
    (dbi:do-sql *cryptogram-connection*
                (format nil "insert or ignore into quotations (qid,author,quotation) values ('~a', '~a', '~a') "
                        qid author quotation))

    (coerce (map 'list
                 (lambda (c)
                   (if (alpha-char-p c)
                       (cdr (assoc c cryptokey))
                     c))
                 downcase-quotation-list) 'string)))


;; find-executable lifted from https://gist.github.com/ryukinix/5273af4b25dec53ed9f078bd7e350d88
(defun executables ()
  (loop with path = (uiop:getenv "PATH")
        for p in (uiop:split-string path :separator ":")
        for dir = (probe-file p)
        when (uiop:directory-exists-p dir)
          append (uiop:directory-files dir)))

(defun find-executable (name)
  (find name (executables)
        :test #'equalp
        :key #'pathname-name))

(defun prepare-config-dir ()
  "Prepare configuration directory.  Return alist."

  (let* ((sqlite-found (find-executable "sqlite3"))  ;; check for existence of sqlite3
         (config-home (or (uiop:getenv "XDG_CONFIG_HOME") (truename "~/.config/")))
         (config-path )
         )
    )
  )



(defun help ()
  (format t "~&Usage:
  cl-cryptogrammer [quotation]~&"))

(defun %main (argv)
  "Parse CLI args."
  (when (member "-h" argv :test #'equal)
    ;; To properly parse command line arguments, use a third-party library such as
    ;; clingon, unix-opts, defmain, adopt… when needed.
    (help)
    (uiop:quit))
  (print (dbi:connection-database-name *cryptogram-connection*))
  (print (generate-cryptogram :quotation (first argv))))

(defun main ()
  "Entry point for the executable.
  Reads command line arguments."

  ;; uiop:command-line-arguments returns a list of arguments (sans the script name).
  ;; We defer the work of parsing to %main because we call it also from the Roswell script.
  (%main (uiop:command-line-arguments)))
