(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system :lisp-unit))

(defpackage :method-combinators/test
  (:use :cl :method-combinators :lisp-unit)
  (:export :test))

(in-package ::method-combinators/test)


(define-test basic
  (defcombined basic-cgf (i)
    (:method ((i number)) i)
    (:method ((i integer)) i))

  (assert-equal 5 (basic-cgf 5))
  (assert-equal 10 (call/cb :+ basic-cgf 5))
  (assert-equal 25 (call/cb :* basic-cgf 5))
  (assert-equal 10 (call/cb :+ basic-cgf 5))
  (assert-equal 5 (basic-cgf 5)))


(define-test change-main-combinator
  ;; FMAKUNBOUND needed because DEFGENERIC won't change the method combinator.
  (fmakunbound 'change-main-combinator-cgf)
  (defcombined change-main-combinator-cgf (i)
    (:method ((i number)) i)
    (:method ((i integer)) (1+ i)))

  (assert-equal 6 (change-main-combinator-cgf 5))
  (assert-equal 11 (call/cb :+ change-main-combinator-cgf 5))
  (assert-equal 30 (call/cb :* change-main-combinator-cgf 5))

  (change-method-combinator #'change-main-combinator-cgf :min)

  (assert-equal 5 (change-main-combinator-cgf 5))
  (assert-equal 11 (call/cb :+ change-main-combinator-cgf 5))
  (assert-equal 30 (call/cb :* change-main-combinator-cgf 5)))


(define-test update-main-combinator
  (define-long-method-combinator :-
      ((methods () :required t))
    `(- ,@(mapcar (lambda (method) `(call-method ,method)) methods)))

  (defcombined update-main-combinator-cgf (i)
    (:method-combinator :-)
    (:method ((i number)) i)
    (:method ((i integer)) (1+ i)))

  (assert-equal  1 (update-main-combinator-cgf 5))
  (assert-equal 11 (call/cb :+ update-main-combinator-cgf 5))
  (assert-equal 30 (call/cb :* update-main-combinator-cgf 5))

  (define-long-method-combinator :-
      ((methods () :required t))
    `(- ,@(mapcar (lambda (method) `(call-method ,method)) (reverse methods))))

  (assert-equal -1 (update-main-combinator-cgf 5))
  (assert-equal 11 (call/cb :+ update-main-combinator-cgf 5))
  (assert-equal 30 (call/cb :* update-main-combinator-cgf 5)))


(define-test update-call/cb-combinator
  (define-long-method-combinator :-
      ((methods () :required t))
    `(- ,@(mapcar (lambda (method) `(call-method ,method)) methods)))

  (defcombined update-call/cb-combinator-cgf (i)
    (:method ((i number)) i)
    (:method ((i integer)) (1+ i)))

  (assert-equal  6 (update-call/cb-combinator-cgf 5))
  (assert-equal 11 (call/cb :+ update-call/cb-combinator-cgf 5))
  (assert-equal 30 (call/cb :* update-call/cb-combinator-cgf 5))
  (assert-equal  1 (call/cb :- update-call/cb-combinator-cgf 5))

  (define-long-method-combinator :-
      ((methods () :required t))
    `(- ,@(mapcar (lambda (method) `(call-method ,method)) (reverse methods))))

  (assert-equal  6 (update-call/cb-combinator-cgf 5))
  (assert-equal 11 (call/cb :+ update-call/cb-combinator-cgf 5))
  (assert-equal 30 (call/cb :* update-call/cb-combinator-cgf 5))
  (assert-equal -1 (call/cb :- update-call/cb-combinator-cgf 5)))


(define-test add-method
  ;; FMAKUNBOUND needed because DEFGENERIC won't remove methods added
  ;; afterwards.
  (fmakunbound 'add-method-cgf)
  (defcombined add-method-cgf (i)
    (:method ((i number)) i)
    (:method ((i integer)) i))

  (assert-equal 5 (add-method-cgf 5))
  (assert-equal 10 (call/cb :+ add-method-cgf 5))
  (assert-equal 25 (call/cb :* add-method-cgf 5))

  (defmethod add-method-cgf ((i fixnum)) i)

  (assert-equal 5 (add-method-cgf 5))
  (assert-equal 15 (call/cb :+ add-method-cgf 5))
  (assert-equal 125 (call/cb :* add-method-cgf 5)))


(define-test remove-method
  (defcombined remove-method-cgf (i)
    (:method ((i number)) i)
    (:method ((i integer)) i))

  (assert-equal 5 (remove-method-cgf 5))
  (assert-equal 10 (call/cb :+ remove-method-cgf 5))
  (assert-equal 25 (call/cb :* remove-method-cgf 5))

  (remove-method #'remove-method-cgf
		 (find-method #'remove-method-cgf nil '(integer)))

  (assert-equal 5 (remove-method-cgf 5))
  (assert-equal 5 (call/cb :+ remove-method-cgf 5))
  (assert-equal 5 (call/cb :* remove-method-cgf 5)))


(defun test ()
  (let ((lisp-unit:*print-failures* t))
    (lisp-unit:run-tests :all :method-combinators/test)))
