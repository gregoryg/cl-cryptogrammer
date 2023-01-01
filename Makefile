LISP ?= sbcl

all: test

run:
	rlwrap $(LISP) --load run.lisp

build:
	$(LISP)	--non-interactive \
		--load cl-cryptogrammer.asd \
		--eval '(ql:quickload :cl-cryptogrammer)' \
		--eval '(asdf:make :cl-cryptogrammer)'

test:
	$(LISP) --non-interactive \
		--load run-tests.lisp
