(defpackage :method-combinators
  (:use :cl)
  (:import-from #+sbcl      :sb-mop
		#+cmu       :mop
		#+ccl       :ccl
		#+ecl       :clos
		#+clisp     :clos
		#+abcl      :mop
		#+allegro   :mop
		#+lispworks :clos
		:find-method-combination
		:funcallable-standard-class
		:finalize-inheritance
		:compute-effective-method
		:generic-function-name
		:class-prototype)
  (:import-from #+sbcl      :sb-pcl
		#+cmu       :pcl
		:short-combination-operator
		:short-combination-identity-with-one-argument
		:long-method-combination-function
		:long-method-combination-args-lambda-list)
  (:export :find-method-combinator
	   :ensure-short-method-combinator :define-short-method-combinator
	   :ensure-long-method-combinator :define-long-method-combinator
	   :define-long-short-method-combinator
	   :combined-generic-function :defcombined
	   :change-method-combinator
	   :call-with-combinator :call/cb :install-#!-reader-macro))

(in-package :method-combinators)


;; ==================
;; Method Combinators
;; ==================

;; -----------------------
;; Method combinators pool
;; -----------------------

(defvar *method-combinators* (make-hash-table)
  "The set of currently defined method combinators.")

;; This is meant to be the equivalent of FIND-CLASS.
(defun find-method-combinator (name &optional (errorp t))
  "Find a method combinator by NAME.
If ERRORP (the default), throw an error if not found. Otherwise, return NIL."
  (or (gethash name *method-combinators*)
      (and errorp (error "No method combinator named ~A." name))))

(defun (setf find-method-combinator) (combinator name)
  "Associate method COMBINATOR with NAME.
If COMBINATOR is NIL, break the association."
  (if combinator
    (setf (gethash name *method-combinators*) combinator)
    (remhash name *method-combinators*)))


;; -------------------------
;; Method combinator classes
;; -------------------------

;; Method combinations distinguish between options set when the combination is
;; defined, and options set when generic functions are defined. The
;; combination meta-objects (short ones notably) mix the two. That is because
;; there is a confusion between combination types and instances. One
;; additional proof of this is that the name of a combination is called the
;; type-name in SBCL (or just the type in CMUCL), and the CLHS speaks of the
;; method combination type, whereas methods combinations are instances.

;; A better design would have been that define-method-combination creates a
;; new class of combination, and every generic function instantiate one of the
;; existing combination classes, with particular options (slot values).

(defclass method-combinator-mixin ()
  ((clients
    :initform nil :accessor clients
    :documentation "The set of generic functions using this combinator."))
  (:documentation "A mixin for all method combinator classes."))

(defun method-combinator-p (object)
  "Return T if OBJECT is a method combinator."
  (typep object 'method-combinator-mixin))

(defclass short-method-combinator
    (#+sbcl sb-pcl::short-method-combination
     #+cmu     pcl::short-method-combination
     method-combinator-mixin)
  ((#+sbcl sb-pcl::type-name
    #+cmu     pcl::type
    :reader method-combinator-name)
   (#+sbcl sb-pcl::%documentation
    #+cmu     pcl::documentation
    :reader method-combinator-documentation))
  (:documentation "The short method combinator class."))

(defun short-method-combinator-p (object)
  "Return T if OBJECT is a short method combinator."
  (typep object 'short-method-combinator))

(defclass long-method-combinator
    (#+sbcl sb-pcl::long-method-combination
     #+cmu     pcl::long-method-combination
     method-combinator-mixin)
  ((#+sbcl sb-pcl::type-name
    #+cmu     pcl::type
    :reader method-combinator-name)
   (#+sbcl sb-pcl::%documentation
    #+cmu     pcl::documentation
    :reader method-combinator-documentation))
  (:documentation "The long method combinator class."))

(defun long-method-combinator-p (object)
  "Return T if OBJECT is a long method combinator."
  (typep object 'long-method-combinator))


;; ------------------
;; Clients management
;; ------------------

(defgeneric make-clients-obsolete (method-combinator)
  (:documentation "Make every METHOD-COMBINATOR client obsolete.")
  (:method ((method-combinator method-combinator-mixin))
    "Make every METHOD-COMBINATOR client obsolete."
    (mapc (lambda (client)
	    (update-combined-generic-function-for-redefined-method-combinator
	     client method-combinator))
      (clients method-combinator)))
  (:method ((method-combinator symbol))
    "Find METHOD-COMBINATOR and makes its clients obsolete."
    (make-clients-obsolete (find-method-combinator method-combinator))))

(defmethod reinitialize-instance :after
    ((method-combinator method-combinator-mixin) &key &allow-other-keys)
  "Update every METHOD-COMBINATOR client of a redefinition."
  (make-clients-obsolete method-combinator))

(defmethod update-instance-for-different-class :after
    ((previous method-combinator-mixin) (current method-combinator-mixin)
     &key &allow-other-keys)
  "Inform every CURRENT method combinator client of a class change."
  (make-clients-obsolete current))



;; ========================
;; Short Method Combinators
;; ========================

;; In SBCL, the short-method-combination class contains the operator name,
;; operator-related properties (identity-with-one-argument), but also the
;; call-related properties (:most-specific-first/last). Every generic function
;; gets a local method combination object with all this information. This
;; means that global changes to the method combination do not affect existing
;; generic functions at all.

;; -------------------------------------------
;; The ENSURE-SHORT-METHOD-COMBINATOR protocol
;; -------------------------------------------

(defgeneric ensure-short-method-combinator-using-class
    (existing name
     &key documentation operator identity-with-one-argument order)
  (:documentation
   "Ensure that a NAMEd short method combinator exists. Return it.
Depending on EXISTING, this means either defining or redefining it.")
  (:method ((existing null) name &key documentation
				      (operator name)
				      identity-with-one-argument
				      (order :most-specific-first))
    "Create a new NAMEd short method combinator."
    ;; #### TODO: check the sanity of the provided keys.
    ;; What we do here is create a regular short method combination, and then
    ;; upgrade it to a short method combinator by changing its class.
    ;; 1. Create a new method on FIND-METHOD-COMBINATION. Note that
    ;; options are not handled here.
    (#+sbcl sb-pcl::load-short-defcombin
     #+cmu     pcl::load-short-defcombin
     name operator identity-with-one-argument documentation
     #+sbcl (sb-c:source-location)
     #+cmu  (c::source-location))
    ;; 2. Call it and upgrade the resulting combination to a combinator. Note
    ;; that the options are handled only when calling the method created
    ;; above. That is because they are originally meant to be used only when
    ;; generic functions are created (so when their own method combination
    ;; meta-object is).
    (setf (find-method-combinator name)
	  (change-class (find-method-combination
			 #'make-instance name (list order))
			'short-method-combinator)))
  (:method ((existing method-combinator-mixin) name
	    &key (documentation nil documentation-provided-p)
		 (operator name operator-provided-p)
		 (identity-with-one-argument
		  nil identity-with-one-argument-provided-p)
		 (order :most-specific-first order-provided-p))
    "Update an EXISTING method combinator to a short one."
    ;; #### TODO: check the sanity of the provided keys.
    ;; 1. Reinitialize the previous combinator, hence preserving object
    ;; identity.
    (if (short-method-combinator-p existing)
      (apply #'reinitialize-instance existing
	     `(,@(when documentation-provided-p
		   `(:documentation ,documentation))
	       ,@(when operator-provided-p `(:operator ,operator))
	       ,@(when identity-with-one-argument-provided-p
		   `(:identity-with-one-argument ,identity-with-one-argument))
	       ,@(when order-provided-p `(:options (,order)))))
      (apply #'change-class existing 'short-method-combinator
	     `(,@(when documentation-provided-p
		   `(:documentation ,documentation))
	       :operator ,(if operator-provided-p operator name)
	       :identity-with-one-argument
	       ,(when identity-with-one-argument-provided-p
		  identity-with-one-argument)
	       :options ,(if order-provided-p
			   `(,order)
			   '(:most-specific-first)))))
    ;; 2. Recreate a method on FIND-METHOD-COMBINATION to reflect the change
    ;; at the regular combination level. Note that options are not handled
    ;; here.
    (#+sbcl sb-pcl::load-short-defcombin
     #+cmu     pcl::load-short-defcombin
     name
     (short-combination-operator existing)
     (short-combination-identity-with-one-argument existing)
     (method-combinator-documentation existing)
     #+sbcl (sb-c:source-location)
     #+cmu  (c::source-location))))

(defun ensure-short-method-combinator
    (name &rest keys
	  &key documentation operator identity-with-one-argument order)
  "Ensure that a NAMEd short method combinator exists. Return it."
  (declare (ignore documentation operator identity-with-one-argument order))
  ;; #### TODO: check the sanity of the provided keys.
  (apply #'ensure-short-method-combinator-using-class
    (find-method-combinator name nil) name keys))

(defmacro define-short-method-combinator
    (name &key (documentation nil documentation-provided-p)
	       (operator name operator-provided-p)
	       (identity-with-one-argument
		nil identity-with-one-argument-provided-p)
	       (order :most-specific-first order-provided-p))
  "Define or redefine a short method combinator. Return its name."
  ;; #### TODO: check the sanity of the provided keys.
  `(progn
     (ensure-short-method-combinator ',name
       ,@(when documentation-provided-p `(:documentation ,documentation))
       ,@(when operator-provided-p `(:operator ',operator))
       ,@(when identity-with-one-argument-provided-p
	   `(:identity-with-one-argument ,identity-with-one-argument))
       ,@(when order-provided-p `(:order ,order)))
     ',name))



;; =======================
;; Long Method Combinators
;; =======================

;; The case of long method combinations in SBCL is a bit different. The
;; long-method-combination class contains a function slot (presumably for the
;; method combination function), but it is unused because those functions are
;; stored in SB-PCL::*LONG-METHOD-COMBINATION-FUNCTIONS*, a global
;; variable. The result is that, contrary to short method combinations,
;; modifying a long one globally may affect existing generic functions. More
;; precisely, a global change alone goes unnoticed (note that the generic
;; function's method-combination meta-object is unchanged), but if the generic
;; function gets reinitialized (e.g. by redefining a method), then the new
;; method combination function will take effect. This is a mess.


;; -------------------------------------------
;; The ENSURE-LONG-METHOD-COMBINATOR protocol
;; -------------------------------------------

(defgeneric ensure-long-method-combinator-using-class
    (existing name &key documentation function arguments)
  (:documentation
   "Ensure that a NAMEd long method combinator exists. Return it.
Depending on EXISTING, this means either defining or redefining it.")
  (:method ((existing null) name &key documentation function arguments)
    "Create a new NAMEd long method combinator."
    ;; What we do here is create a regular long method combination, and then
    ;; upgrade it to a long method combinator by changing its class.
    ;; 1. Create a new method on FIND-METHOD-COMBINATION.
    (#+sbcl sb-pcl::load-long-defcombin
     #+cmu     pcl::load-long-defcombin
     name documentation function arguments #+sbcl (sb-c:source-location)
					   #+cmu  (c::source-location))
    ;; 2. Call it and upgrade the resulting combination to a combinator.
    (setf (find-method-combinator name)
	  (change-class
	   (find-method-combination #'make-instance name nil)
	   'long-method-combinator
	   ;; #### NOTE: SBCL stores the long method combination functions in
	   ;; a hash table called *LONG-METHOD-COMBINATION-FUNCTIONS*, and
	   ;; consequently doesn't set the FUNCTION slot below. CMUCL does.
	   :function function)))
  (:method ((existing method-combinator-mixin) name
	    &key (documentation nil documentation-provided-p)
		 (function nil function-provided-p)
		 (arguments nil arguments-provided-p))
    "Update an EXISTING long method combinator."
    ;; 1. Reinitialize the combinator, hence preserving object identity.
    (if (long-method-combinator-p existing)
      (apply #'reinitialize-instance existing
	     `(,@(when documentation-provided-p
		   `(:documentation ,documentation))
	       ,@(when function-provided-p `(:function ,function))
	       ,@(when arguments-provided-p `(:args-lambda-list ,arguments))))
      (apply #'change-class existing 'long-method-combinator
	     `(,@(when documentation-provided-p
		   `(:documentation ,documentation))
	       :function ,function
	       :args-lambda-list ,arguments)))
    ;; 2. Recreate a method on FIND-METHOD-COMBINATION to reflect the change
    ;; at the regular combination level.
    (#+sbcl sb-pcl::load-long-defcombin
     #+cmu     pcl::load-long-defcombin
     name
     (method-combinator-documentation existing)
     (long-method-combination-function existing)
     (long-method-combination-args-lambda-list existing)
     #+sbcl (sb-c:source-location)
     #+cmu  (c::source-location))
    ;; 3. Remember that SBCL uses the contents of
    ;; *LONG-METHOD-COMBINATION-FUNCTIONS* instead of the FUNCTION slot to
    ;; compute effective methods, so also update this variable.
    #+sbcl (setf (gethash name sb-pcl::*long-method-combination-functions*)
		 (long-method-combination-function existing))
    existing))

(defun ensure-long-method-combinator
    (name &rest keys
	  &key documentation function arguments)
  "Ensure that a NAMEd long method combinator exists. Return it."
  (declare (ignore documentation function arguments))
  (apply #'ensure-long-method-combinator-using-class
    (find-method-combinator name nil) name keys))

(defmacro define-long-method-combinator
    ;; #### NOTE: there's no lambda-list argument here because I'm forbidding
    ;; options until a proper distinction between combination types (classes)
    ;; and instances is done.
    (name #|lambda-list|# method-group-specifiers
     &body body
     &aux arguments generic-function)
  "Define or redefine a long method combinator. Return its name."
  ;; #### NOTE: the piece below is grabbed from SBCL's
  ;; EXPAND-LONG-DEFCOMBIN. It does more than EXPAND-SHORT-DEFCOMBIN because
  ;; there's a bit of parsing on the BODY, but also because it computes the
  ;; long method combination's function when the macro itself is
  ;; evaluated. This suggests that we could also work around the macro layer
  ;; and provide our own function directly.
  (when (and (consp (car body)) (eq (caar body) :arguments))
    (setq arguments (cdr (pop body))))
  (when (and (consp (car body)) (eq (caar body) :generic-function))
    (setq generic-function (cadr (pop body))))
  (multiple-value-bind (documentation function)
      (#+sbcl sb-pcl::make-long-method-combination-function
       #+cmu     pcl::make-long-method-combination-function
       name #|lambda-list|# nil method-group-specifiers
       arguments generic-function
       body)
    `(progn
       (ensure-long-method-combinator ',name
	 ;; #### NOTE: contrary to the short method combination case, the
	 ;; following arguments are always provided from this macro call.
	 :documentation ,documentation
	 :function ,function
	 :arguments ,arguments)
       ',name)))



;; ===========================
;; Built-in Method Combinators
;; ===========================

;; -------------------------------
;; The :STANDARD method combinator
;; -------------------------------

(define-long-method-combinator :standard
    ((around (:around))
     (before (:before))
     (primary () :required t)
     (after (:after)))
  (flet ((call-methods (methods)
	   (mapcar (lambda (method) `(call-method ,method)) methods)))
    (let ((form (if (or before after (rest primary))
		  `(multiple-value-prog1
		       (progn ,@(call-methods before)
			      (call-method ,(first primary) ,(rest primary)))
		     ,@(call-methods (reverse after)))
		  `(call-method ,(first primary)))))
      (if around
	`(call-method ,(first around) (,@(rest around) (make-method ,form)))
	form))))


;; -------------------------------
;; The built-in method combinators
;; -------------------------------

(defmacro define-long-short-method-combinator
    (name &key documentation (order :most-specific-first)
	       identity-with-one-argument (operator name))
  "Define NAME as a long-short method combination.
OPERATOR will be used to define a combination resembling a short method
combination, with the following differences:
- the primary methods must not be qualified,
- :before and :after methods are available."
  (let ((documentation (when documentation (list documentation)))
	(single-method-call (if identity-with-one-argument
			      '`(call-method ,(first primary))
			      ``(,',operator (call-method ,(first primary))))))
    `(define-long-method-combinator ,name
       ((around (:around))
	(before (:before))
	(primary () :order ,order :required t)
	(after (:after)))
       ,@documentation
       (flet ((call-methods (methods)
		(mapcar (lambda (method) `(call-method ,method)) methods)))
	 (let* ((primary-form (if (rest primary)
				`(,',operator ,@(call-methods primary))
				,single-method-call))
		(form (if (or before after)
			`(multiple-value-prog1
			     (progn ,@(call-methods before) ,primary-form)
			   ,@(call-methods (reverse after)))
			primary-form)))
	   (if around
	     `(call-method
	       ,(first around) (,@(rest around) (make-method ,form)))
	     form))))))

(defmacro define-built-in-method-combinators ()
  "Define all built-in method combinators, and then some.
This defines :+ :* :max :min :list :append :nconc :progn :and and :or."
  `(progn
     ,@(mapcar
	(lambda (name)
	  `(define-long-short-method-combinator ,name
	     :documentation
	     ,(format nil "The ~A built-in method combinator." name)
	     :operator ,(intern (symbol-name name) :cl)
	     :identity-with-one-argument t))
	'(:+ :* :max :min :list :append :nconc :progn :and :or))))

(define-built-in-method-combinators)



;; ==========================
;; Combined Generic Functions
;; ==========================

(defclass combined-generic-function (standard-generic-function)
  ((#+sbcl sb-pcl::%method-combination
    #+cmu     pcl::method-combination
    :initarg :method-combinator :accessor method-combinator)
   (functions :initform (make-hash-table) :reader functions))
  (:metaclass funcallable-standard-class)
  (:default-initargs :method-combinator (find-method-combinator :standard))
  (:documentation "Meta-class for generic functions using combinators."))

(defun combined-generic-function-p (object)
  "Return T if OBJECT is a combined generic function."
  (typep object 'combined-generic-function))

;; #### WARNING: DEFGENERIC cannot be extended with additional keyword
;; arguments (except by implementations themselves), so if we want to allow
;; the use of :method-combinator in DEFCOMBINED, we need to process it back to
;; :method-combination. Also, it is not possible to call
;; ENSURE-GENERIC-FUNCTION with a :method-combinator argument, because the
;; standard specifies the list of authorized keys.

(defun process-defcombined-options
    (options
     &aux (method-combinator-option (assoc :method-combinator options)))
  "Process DEFCOMBINED options before calling DEFGENERIC.
1. Replace a :method-combinator option by a :method-combination one,
2. provide a :generic-function-class option if missing."
  (when method-combinator-option
    (setf (car method-combinator-option) :method-combination))
  (unless (assoc :generic-function-class options)
    (push '(:generic-function-class combined-generic-function) options))
  options)

(defmacro defcombined (name lambda-list &body options)
  "Wrapper around DEFGENERIC for creating combined generic functions."
  `(defgeneric ,name ,lambda-list ,@(process-defcombined-options options)))

;; This is needed right now because of the specialization below on the class
;; prototype. We cannot wait until the first combined generic function is
;; created.
(finalize-inheritance (find-class 'combined-generic-function))


;; -----------------------------
;; Effective methods computation
;; -----------------------------

;; #### NOTE: the fact that COMPUTE-EFFECTIVE-METHOD takes both a generic
;; #### function and a method combination as arguments is another incentive
;; #### for more orthogonality between these two concepts.

;; #### NOTE: this method isn't actually needed in SBCL, as long as we also
;; update *LONG-METHOD-COMBINATION-FUNCTIONS*. If we don't retro-propagate
;; long combinator to long combinations, it will become needed. In CMUCL, this
;; method isn't needed at all because it does exactly what's below.
(defmethod compute-effective-method
    ((generic-function combined-generic-function)
     (combinator long-method-combinator)
     applicable-methods)
  "Use combined GENERIC-FUNCTION's method combinator function for computing."
  (funcall (#+sbcl sb-pcl::long-method-combination-function
	    #+cmu     pcl::long-method-combination-function
	    (method-combinator generic-function))
    generic-function combinator applicable-methods))


;; -----------------------------------------------
;; Method combinator initialization and management
;; -----------------------------------------------

;; In SBCL, a generic function's method combination is initialized as follows:
;; DEFGENERIC => ... => ENSURE-GENERIC-FUNCTION => ... => NORMALIZE-OPTIONS
;; => (find-method-combination (class-prototype class) type options), CLASS
;; being here the generic function's meta-class. Note (see blog post) that all
;; methods on FIND-METHOD-COMBINATION defined by SBCL when a new method
;; combination is created ignore their 1st argument. Hence, we can specialize
;; this function for calls on the class prototype, and we'll know that this is
;; for initializing the method combination slot of the generic function.
;;
;; Another note: the MOP (p. 191) is not very specific about this function: it
;; only says that this function "is called to determine the method combination
;; object used by a generic function", which does not explicitly speak of
;; initialization, and the additional remark is that the details are not
;; specified.
(defmethod find-method-combination
    ((prototype
      (eql (class-prototype (find-class 'combined-generic-function))))
     type options)
  "Return the method combinator named TYPE. Throw an error if not found."
  (when options (error "Method combinators do not support options."))
  (find-method-combinator type))

;; Now, let's sanitize the semantics of FIND-METHOD-COMBINATION for combined
;; generic functions. This deviates from what SBCL makes it do, but the CLOS
;; MOP is so vague that this may very well remain compliant.
(defmethod find-method-combination
    ((generic-function combined-generic-function) type options)
  "Return the method combinator named TYPE if GF uses it.
Throw an error otherwise."
  (when options (error "Method combinators do not support options."))
  (if (eq type (method-combinator-name (method-combinator generic-function)))
    (method-combinator generic-function)
    (error "Method combination of ~A is not ~A."
	   (generic-function-name generic-function) type)))


;; -------------------------------------
;; The CHANGE-METHOD-COMBINATOR protocol
;; -------------------------------------

;; #### NOTE: better to stay away from ENSURE-GENERIC-FUNCTION[-USING-CLASS]
;; or the other ENSURE-* altogether. You need to provide the
;; :generic-function-class option every time, even when the generic function
;; already exists. Otherwise it gets defaulted to standard-generic-function. I
;; don't understand the rationale behind this. Why not use the existing
;; generic function's class by default? In general, we can always use
;; REINITIALIZE-INSTANCE which is public. See
;; https://mailman.common-lisp.net/pipermail/pro/2018-January/001609.html
;; about this.

;; #### NOTE: this protocol is useless (and hidden from the ELS 2018
;; paper). Contrary to what I had written in a preliminary version, it's not
;; really similar to CHANGE-CLASS because CHANGE-CLASS affects the whole
;; structure of an instance, whereas CHANGE-METHOD-COMBINATOR only affects one
;; aspect of it. CHANGE-METHOD-COMBINATOR might as well call
;; REINITIALIZE-INSTANCE directly.
(defgeneric update-combined-generic-function-for-different-method-combinator
    (generic-function method-combinator)
  (:documentation "Change GENERIC-FUNCTION to new METHOD-COMBINATOR.")
  (:method ((generic-function combined-generic-function)
	    (method-combinator method-combinator-mixin))
    "Change combined GENERIC-FUNCTION to new METHOD-COMBINATOR."
    ;; #### NOTE: there's no portable way to only invalidate effective methods
    ;; (caches),so the only thing we can do to implement this is to call
    ;; REINITIALIZE-INSTANCE.
    (reinitialize-instance generic-function
      :method-combinator method-combinator)))

(defun change-method-combinator (generic-function method-combinator)
  "Set combined GENERIC-FUNCTION to a new METHOD-COMBINATOR (designator)."
  (unless (method-combinator-p method-combinator)
    (setq method-combinator (find-method-combinator method-combinator)))
  (unless (eq method-combinator (method-combinator generic-function))
    (update-combined-generic-function-for-different-method-combinator
     generic-function method-combinator)))


;; ------------------------------------
;; Method combinator clients management
;; ------------------------------------

(defmethod initialize-instance :after
    ((generic-function combined-generic-function) &key &allow-other-keys)
  "Add GENERIC-FUNCTION to it's method combinator's clients."
  (pushnew generic-function (clients (method-combinator generic-function))))

(defgeneric update-combined-generic-function-for-redefined-method-combinator
    (combined-generic-function method-combinator)
  (:documentation
   "Inform COMBINED-GENERIC-FUNCTION that METHOD-COMBINATOR was redefined.")
  (:method ((generic-function combined-generic-function)
	    (method-combinator method-combinator-mixin))
    "Update combined GENERIC-FUNCTION after redefinition of METHOD-COMBINATOR.
Either invalidate GENERIC-FUNCTION's main funcallable instance function,
or the entry corresponding to METHOD-COMBINATOR in its alternative functions
cache."
    ;; Normally, we need to invalidate every effective method (because they
    ;; are potentially cached) in generic functions using this
    ;; combinator. Unfortunately, the MOP doesn't specify anything about this
    ;; (neither caching, nor invalidation to the best of my knowledge). There
    ;; are different ways to do so. We could dig deep into every
    ;; implementation and figure out how they do it. In SBCL, there is a thing
    ;; called SB-PCL::FLUSH-EFFECTIVE-METHOD-CACHE but it doesn't seem to be
    ;; engouh. A call to SB-PCL::UPDATE-DFUN is needed as well (which boils
    ;; down to recreating the discriminating function). This is not very
    ;; satisfactory of course, and we could miss some other important
    ;; effects. A better way, slightly higher level, is to reinitialize the
    ;; generic function itself, which will invalidate the discriminating
    ;; function. Again, this is in fact more than needed in theory because
    ;; only the effective methods would need to be invalidated, but it's the
    ;; only standard thing that will work. Since the combinator's identity is
    ;; preserved, we need to trick REINITIALIZE-INSTANCE into thinking that
    ;; the combinator changed (which, indeed, it has, only internally). So we
    ;; first set the combinator to NIL. One final note: this trick works
    ;; because REINITIALIZE-INSTANCE is reasonably low level. We cannot do
    ;; that with ENSURE-GENERIC-FUNCTION, for instance, because it would
    ;; trigger CLIENTS maintenance with a null combinator.
    (cond ((eq (method-combinator generic-function) method-combinator)
	   (setf (method-combinator generic-function) nil)
	   (reinitialize-instance generic-function
	     :method-combinator method-combinator))
	  (t
	   (remhash method-combinator (functions generic-function))))))

;; #### NOTE: the Common Lisp standard lets one change generic function
;; #### classes provided that they are "compatible" (undefined; see the
;; #### documentation of ENSURE-GENERIC-FUNCTION). However, the AMOP says that
;; #### an error is signaled if the classes are different (see the
;; #### documentation of ENSURE-GENERIC-FUNCTION-USING-CLASS). This seems to
;; #### suggest that meta-classes are only compatible with themselves. In any
;; #### case, this means that we can assume that the generic function's class
;; #### remains the same below. Otherwise, CALL-NEXT-METHOD would trigger an
;; #### error. As a consequence, we can directly specialize on combined
;; #### generic-functions below, and avoid resorting to :around methods.

(defmethod reinitialize-instance
    ((generic-function combined-generic-function)
     &key &allow-other-keys
     &aux (previous-combinator (method-combinator generic-function)))
  "Update GENERIC-FUNCTION's method combinators client information."
  (call-next-method)
  ;; #### NOTE: a NULL previous method combinator is the trick for noticing a
  ;; redefinition of it, so in that case, there's actually nothing more to do
  ;; than what the standard method actually does.
  (when previous-combinator
    (let* ((new-combinator (method-combinator generic-function)))
      (unless (eq previous-combinator new-combinator)
	(setf (clients previous-combinator)
	      (delete generic-function (clients previous-combinator)))
	(remhash new-combinator (functions generic-function))
	(pushnew generic-function (clients new-combinator)))))
  generic-function)


;; -----------------------
;; Alternative combinators
;; -----------------------

(defmethod add-method :after
    ((generic-function combined-generic-function) method)
  "Invalidate all cached alternative functions."
  (clrhash (functions generic-function)))

(defmethod remove-method :after
    ((generic-function combined-generic-function) method)
  "Invalidate all cached alternative functions."
  (clrhash (functions generic-function)))

(defun call-with-combinator
    (combinator generic-function
     &rest args
     &aux (default-combinator (method-combinator generic-function)))
  "Call GENERIC-FUNCTION on ARGS with alternative method COMBINATOR."
  (if (eq combinator default-combinator)
    (apply generic-function args)
    (let ((function (gethash combinator (functions generic-function))))
      (if function
	(apply function args)
	(let (values)
	  (change-method-combinator generic-function combinator)
	  (setq values (multiple-value-list (apply generic-function args))
		function (#+sbcl sb-pcl::funcallable-instance-fun
			  #+cmu  kernel::funcallable-instance-function
			  generic-function))
	  ;; #### TODO: this second reinitialization of the generic function
	  ;; may not be actually needed. Instead, we could try to work at a
	  ;; lower level, saving the original discriminating function, and
	  ;; then just restoring it here manually, along with the original
	  ;; combinator and a manual client re-updating (because the first
	  ;; reinitialization will have removed it from the default
	  ;; combinator). The potential gain is probably not much however (for
	  ;; a function called many times), and besides, I'm not sure if this
	  ;; would work in case the function was never actually called
	  ;; before. This needs to be tested.
	  (change-method-combinator generic-function default-combinator)
	  (setf (gethash combinator (functions generic-function)) function)
	  (pushnew generic-function (clients combinator))
	  (values-list values))))))

(defmacro call/cb (combinator generic-function &rest args)
  "Call GENERIC-FUNCTION on ARGS with alternative method COMBINATOR.
COMBINATOR and GENERIC-FUNCTION are names (not evaluated)."
  `(call-with-combinator (find-method-combinator ',combinator)
			 (function ,generic-function)
     ,@args))

(defun install-#!-reader-macro ()
  "Install a #! reader-macro for CALL/CB in the current readtable.
The new syntax is #!combinator-name(function args...)."
  (set-dispatch-macro-character #\# #\!
    (lambda (stream subchar arg
	     &aux (method-combinator (read stream t nil t))
		  (function-call (read stream t nil t)))
      (declare (ignore subchar arg))
      `(call/cb ,method-combinator ,@function-call))))
