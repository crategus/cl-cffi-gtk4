;;; ----------------------------------------------------------------------------
;;; gtk4.expression.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2023 Dieter Kaiser
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;; ----------------------------------------------------------------------------
;;;
;;; GtkExpression
;;;
;;;     Expressions to values
;;;
;;; Types and Values
;;;
;;;     GtkExpression
;;;     GtkExpressionWatch
;;;     GtkParamSpecExpression
;;;
;;; Functions
;;;
;;;     gtk_expression_ref
;;;     gtk_expression_unref
;;;     gtk_expression_get_value_type
;;;     gtk_expression_is_static
;;;     gtk_expression_evaluate
;;;     gtk_expression_watch
;;;     gtk_expression_bind
;;;
;;;     GtkExpressionNotify
;;;
;;;     gtk_expression_watch_ref
;;;     gtk_expression_watch_unref
;;;     gtk_expression_watch_evaluate
;;;     gtk_expression_watch_unwatch
;;;
;;;     gtk_property_expression_new
;;;     gtk_property_expression_new_for_pspec
;;;     gtk_property_expression_get_expression
;;;     gtk_property_expression_get_pspec
;;;
;;;     gtk_constant_expression_new
;;;     gtk_constant_expression_new_for_value
;;;     gtk_constant_expression_get_value
;;;
;;;     gtk_object_expression_new
;;;     gtk_object_expression_get_object
;;;
;;;     gtk_closure_expression_new
;;;     gtk_cclosure_expression_new
;;;
;;;     GTK_VALUE_HOLDS_EXPRESSION()
;;;
;;;     gtk_value_set_expression
;;;     gtk_value_take_expression
;;;     gtk_value_get_expression
;;;     gtk_value_dup_expression
;;;
;;;     gtk_param_spec_expression
;;;
;;; Object Hierarchy
;;;
;;;     GtkExpression
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkExpressionWatch
;;; ----------------------------------------------------------------------------

(glib:define-g-boxed-opaque expression-watch "GtkExpressionWatch"
  :export t
  :type-initializer "gtk_expression_watch_get_type"
  :alloc (error "GtkExpressionWatch cannot be created from the Lisp side."))

#+liber-documentation
(setf (liber:alias-for-class 'expression-watch)
      "GBoxed"
      (documentation 'expression-watch 'type)
 "@version{2023-11-6}
  @begin{short}
    An opaque structure representing a watched @class{gtk:expression} instance.
  @end{short}
  @see-class{gtk:expression}")

;;; ----------------------------------------------------------------------------
;;; GtkExpression
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (glib-init:at-init ()
    (cffi:foreign-funcall "gtk_expression_get_type" :size)))

(cffi:define-foreign-type expression ()
  ()
  (:actual-type :pointer)
  (:simple-parser expression))

(defmethod cffi:translate-to-foreign (proxy (type expression))
  proxy)

(defmethod cffi:translate-from-foreign (native (type expression))
  native)

(defmethod gobject:parse-g-value-for-type
    (gvalue (glib:gtype (eql (glib:gtype "GtkExpression"))) kind)
  (declare (ignore kind))
  (value-expression gvalue))

(defmethod gobject:set-g-value-for-type
    (gvalue (glib:gtype (eql (glib:gtype "GtkExpression"))) value)
  (setf (value-expression gvalue) value))

#+liber-documentation
(setf (liber:alias-for-class 'expression)
      "GtkExpression"
      (documentation 'expression 'type)
 "@version{2023-11-6}
  @begin{short}
    The @type{gtk:expression} type provides a way to describe references to
    values.
  @end{short}
  An important aspect of expressions is that the value can be obtained from a
  source that is several steps away. For example, an expression may describe
  \"the value of property A of @code{object1}, which is itself the value of a
  property of @code{object2}\". And @code{object1} may not even exist yet at
  the time that the expression is created. This is contrast to @code{GObject}
  property bindings, which can only create direct connections between the
  properties of two objects that must both exist for the duration of the
  binding.

  An expression needs to be \"evaluated\" to obtain the value that it currently
  refers to. An evaluation always happens in the context of a current object
  called this (it mirrors the behavior of object-oriented languages), which may
  or may not influence the result of the evaluation. Use the
  @fun{gtk:expression-evaluate} function for evaluating an expression.

  Various methods for defining expressions exist, from simple constants via
  the @fun{gtk:constant-expression-new} function to looking up properties in a
  @code{GObject} (even recursively) via the @fun{gtk:property-expression-new}
  function or providing custom functions to transform and combine expressions
  via the @fun{gtk:closure-expression-new} function.

  Here is an example of a complex expression:
  @begin{pre}
color_expr = gtk_property_expression_new (GTK_TYPE_LIST_ITEM,
                                          NULL, \"item\");
expression = gtk_property_expression_new (GTK_TYPE_COLOR,
                                          color_expr, \"name\");
  @end{pre}
  when evaluated with @code{this} being a @class{gtk:list-item} object, it will
  obtain the @slot[gtk:list-item]{item} property from the @class{gtk:list-item}
  object, and then obtain the @code{name} property from the resulting object
  (which is assumed to be of type GTK_TYPE_COLOR).

  A more concise way to describe this would be
  @begin{pre}
this->item->name
  @end{pre}
  The most likely place where you will encounter expressions is in the context
  of list models and list widgets using them. For example, the
  @class{gtk:drop-down} widget is evaluating a @class{gtk:expression} instance
  to obtain strings from the items in its model that it can then use to match
  against the contents of its search entry. The @class{gtk:string-filter}
  object is using a @class{gtk:expression} instance for similar reasons.

  By default, expressions are not paying attention to changes and evaluation is
  just a snapshot of the current state at a given time. To get informed about
  changes, an expression needs to be \"watched\" via a
  @class{gtk:expression-watch} instance, which will cause a callback to be
  called whenever the value of the expression may have changed. The
  @fun{gtk:expression-watch} function starts watching an expression, and the
  @fun{gtk:expression-watch-unwatch} function stops.

  Watches can be created for automatically updating the property of an object,
  similar to @code{GObject}'s @code{GBinding} mechanism, by using the
  @fun{gtk:expression-bind} function.

  @subheading{GtkExpression in GObject properties}
  In order to use a @class{gtk:expression} instance as a @code{GObject}
  property, you must use the the @fun{gtk:param-spec-expression} function when
  creating a @code{GParamSpec} to install in the @code{GObject} class being
  defined; for instance:
  @begin{pre}
obj_props[PROP_EXPRESSION] =
  gtk_param_spec_expression (\"expression\",
                             \"Expression\",
                             \"The expression used by the widget\",
                             G_PARAM_READWRITE |
                             G_PARAM_STATIC_STRINGS |
                             G_PARAM_EXPLICIT_NOTIFY);
  @end{pre}
  When implementing the @code{GObjectClass.set_property()} and
  @code{GObjectClass.get_property()} virtual functions, you must use the
  @fun{gtk:value-get-expression} function, to retrieve the stored
  @class{gtk:expression} instance from the @code{GValue} container, and
  the @fun{gtk:value-set-expression}, to store the
  @class{gtk:expression} instance into the @code{GValue}; for instance:
  @begin{pre}
// in set_property()...
case PROP_EXPRESSION:
  foo_widget_set_expression (foo, gtk_value_get_expression (value));
  break;

// in get_property()...
case PROP_EXPRESSION:
  gtk_value_set_expression (value, foo->expression);
  break;
  @end{pre}
  @subheading{GtkExpression in .ui files}
  The @class{gtk:builder} class has support for creating expressions. The syntax
  here can be used where a @class{gtk:expression} instance is needed like in a
  @code{<property>} tag for an expression property, or in a @code{<binding>}
  tag to bind a property to an expression.

  To create an property expression, use the @code{<lookup>} element. It can have
  a type attribute to specify the object type, and a name attribute to specify
  the property to look up. The content of @code{<lookup>} can either be an
  element specfiying the expression to use the object, or a string that
  specifies the name of the object to use.

  @subheading{Example:}
  @begin{pre}
<lookup name='search'>string_filter</lookup>
  @end{pre}
  To create a constant expression, use the @code{<constant>} element. If the
  type attribute is specified, the element content is interpreted as a value of
  that type. Otherwise, it is assumed to be an object. For instance:
  @begin{pre}
<constant>string_filter</constant>
<constant type='gchararray'>Hello, world</constant>
  @end{pre}
  To create a closure expression, use the @code{<closure>} element. The type and
  function attributes specify what function to use for the closure, the content
  of the element contains the expressions for the parameters. For instance:
  @begin{pre}
<closure type='gchararray' function='combine_args_somehow'>
  <constant type='gchararray'>File size:</constant>
  <lookup type='GFile' name='size'>myfile</lookup>
</closure>
  @end{pre}")

(export 'expression)

;;; ----------------------------------------------------------------------------
;;; gtk_expression_ref ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_expression_ref" expression-ref) expression
 #+liber-documentation
 "@version{2023-11-6}
  @argument[expression]{a @class{gtk:expression} instance}
  @return{The @class{gtk:expression} instance with an additional reference.}
  @begin{short}
    Acquires a reference on the given @arg{expression}.
  @end{short}
  @see-class{gtk:expression}"
  (expression expression))

(export 'expression-ref)

;;; ----------------------------------------------------------------------------
;;; gtk_expression_unref ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_expression_unref" expression-unref) :void
 #+liber-documentation
 "@version{2023-11-6}
  @argument[expression]{a @class{gtk:expression} instance}
  @begin{short}
    Releases a reference on the given @class{gtk:expression} instance.
  @end{short}
  If the reference was the last, the resources associated to the
  @arg{expression} are freed.
  @see-class{gtk:expression}"
  (expression expression))

(export 'expression-unref)

;;; ----------------------------------------------------------------------------
;;; gtk_expression_get_value_type ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_expression_get_value_type" expression-value-type) g:type-t
 #+liber-documentation
 "@version{2023-11-6}
  @argument[expression]{a @class{gtk:expression} instance}
  @return{The @class{g:type-t} type returned from the
  @fun{gtk:expression-evaluate} function.}
  @begin{short}
    Gets the GType that this expression evaluates to.
  @end{short}
  This type is constant and will not change over the lifetime of the expression.
  @see-class{gtk:expression}
  @see-class{g:type-t}
  @see-function{gtk:expression-evaluate}"
  (expression expression))

(export 'expression-value-type)

;;; ----------------------------------------------------------------------------
;;; gtk_expression_is_static ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_expression_is_static" expression-is-static) :boolean
 #+liber-documentation
 "@version{2023-11-6}
  @argument[expresson]{a @class{gtk:expression} instance}
  @return{@em{True} if @arg{expression} is static.}
  @begin{short}
    Checks if the expression is static.
  @end{short}
  A static expression will never change its result when the
  @fun{gtk:expression-evaluate} function is called on it with the same
  arguments. That means a call to the @fun{gtk:expression-watch} function is
  not necessary because it will never trigger a notify.
  @see-class{gtk:expression}
  @see-function{gtk:expression-evaluate}
  @see-function{gtk:expression-watch}"
  (expression expression))

(export 'expression-is-static)

;;; ----------------------------------------------------------------------------
;;; gtk_expression_evaluate ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_expression_evaluate" %expression-evaluate) :boolean
  (expression expression)
  (object :pointer)
  (value (:pointer (:struct g:value))))

(defun expression-evaluate (expression object value)
 #+liber-documentation
 "@version{2023-11-6}
  @argument[expression]{a @class{gtk:expression} instance}
  @argument[object]{a @class{g:object} instance for the evaluation}
  @argument[gvalue]{an initialized @symbol{g:value} instance}
  @return{@em{True} if the expression could be evaluated.}
  @begin{short}
    Evaluates the given @arg{expression} and on success stores the result as
    a @symbol{g:value} instance in @arg{gvalue}.
  @end{short}
  The GType of the returned value will be the type given by the
  @fun{gtk:expression-value-type} function.

  It is possible that expressions cannot be evaluated - for example when the
  expression references objects that have been destroyed. In that case
  @em{false} will be returned.

  See the @fun{gtk:expression-evaluate-value} function for a variant that does
  not need an @symbol{g:value} instance, but returns the value.
  @see-class{gtk:expression}
  @see-symbol{g:value}
  @see-function{gtk:expression-value-type}"
  (if object
      (progn
        (assert (g:type-is-object (g:type-from-instance object)))
        (let ((object (gobject:object-pointer object)))
          (%expression-evaluate expression object value)))
      (%expression-evaluate expression (cffi:null-pointer) value)))

(export 'expression-evaluate)

;; TODO: A Lisp extension which returns the value of the g:value instance.
;; Document the function.

(defun expression-evaluate-value (expression object)
 #+liber-documentation
 "@version{#2023-11-16}
  @argument[expression]{a @class{gtk:expression} instance}
  @argument[object]{a @class{g:object} instance for the evaluation}
  @return{The evaluated value of the expression.}
  @begin{short}
    Evaluates the given @arg{expression} and on success returns the result.
  @end{short}
  This is a variant of the @fun{gtk:expression-evaluate} function that avoids
  the usage of a @symbol{g:value} instance to get the value of the expression.
  @see-class{gtk:expression}
  @see-function{gtk:expression-evaluate}"
  (cffi:with-foreign-object (gvalue '(:struct g:value))
    (g:value-init gvalue)
    (if object
        (progn
          (assert (g:type-is-object (g:type-from-instance object)))
          (let ((object (gobject:object-pointer object)))
            (when (%expression-evaluate expression object gvalue)
              (g:value-get gvalue))))
        (when (%expression-evaluate expression (cffi:null-pointer) gvalue)
          (g:value-get gvalue)))))

(export 'expression-evaluate-value)

;;; ----------------------------------------------------------------------------
;;; gtk_expression_bind ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_expression_bind" expression-bind) (g:boxed expression-watch)
 #+liber-documentation
 "@version{#2023-9-14}
  @argument[expression]{a @class{gtk:expression} instance}
  @argument[target]{a pointer to the target object to bind to}
  @argument[property]{a string with name of the property on @arg{target} to
    bind to}
  @argument[this]{a pointer to the argument for the evaluation of
    @arg{expression}}
  @return{The @class{gtk:expression-watch} instance.}
  @begin{short}
    Bind @arg{target}'s property named @arg{property} to @arg{expression}.
  @end{short}
  The value that @arg{expression} evaluates to is set via the
  @fun{g:object-set} function on @arg{target}. This is repeated whenever
  @arg{expression} changes to ensure that the object's property stays
  synchronized with @arg{expression}.

  If expression's evaluation fails, @arg{target}'s property is not updated. You
  can ensure that this does not happen by using a fallback expression.

  Note that this function takes ownership of @arg{expression}. If you want to
  keep it around, you should use the @fun{gtk:expression-ref} function it
  beforehand.
  @see-class{gtk:expression}
  @see-class{gtk:expression-watch}
  @see-function{g:object-set}"
  (expression expression)
  (target g:object)
  (property :string)
  (this g:object))

(export 'expression-bind)

;;; ----------------------------------------------------------------------------
;;; GtkExpressionNotify ()
;;; ----------------------------------------------------------------------------

(cffi:defcallback expression-notify :void
    ((data :pointer))
  (let ((ptr (glib:get-stable-pointer-value data)))
    (funcall ptr)))

#+liber-documentation
(setf (liber:alias-for-symbol 'expression-notify)
      "Callback"
      (liber:symbol-documentation 'expression-notify)
 "@version{#2023-9-14}
  @begin{short}
    Callback called by the @fun{gtk:expression-watch} function when the
    expression value changes.
  @end{short}
  @begin{pre}
lambda ()
  @end{pre}
  @begin[code]{table}
    No arguments and no return value.
  @end{table}
  @see-function{gtk:expression-watch}")

(export 'expression-notify)

;;; ----------------------------------------------------------------------------
;;; gtk_expression_watch ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_expression_watch" %expression-watch)
    (g:boxed expression-watch)
  (expression expression)
  (this :pointer)
  (data :pointer)
  (notify :pointer))

(defun expression-watch (expression this func)
 #+liber-documentation
 "@version{#2023-9-16}
  @argument[expression]{a @class{gtk:expression} intstance}
  @argument[this]{a pointer to the argument to watch}
  @argument[func]{a @symbol{expression-notify} callback function to invoke when
    the expression changes}
  @return{The newly installed watch. Note that the only reference held to the
    watch will be released when the watch is unwatched which can happen
    automatically, and not just via the @fun{gtk:expression-watch-unwatch}
    function.}
  @begin{short}
    Installs a watch for the given expression that calls the notify function
    whenever the evaluation of @arg{expression} may have changed.
  @end{short}
  GTK cannot guarantee that the evaluation did indeed change when the notify
  gets invoked, but it guarantees the opposite: When it did in fact change,
  the notify will be invoked.
  @see-class{gtk:expression-watch}
  @see-symbol{gtk:expression-notify}
  @see-function{gtk:expression-watch-unwatch}"
  (%expression-watch expression
                     this
                     (cffi:callback expression-notify)
                     (glib:allocate-stable-pointer func)))

(export 'expression-watch)

;;; ----------------------------------------------------------------------------
;;; gtk_expression_watch_ref ()
;;;
;;; GtkExpressionWatch *
;;; gtk_expression_watch_ref (GtkExpressionWatch *watch);
;;;
;;; Acquires a reference on the given GtkExpressionWatch.
;;;
;;; watch :
;;;     a GtkExpressionWatch.
;;;
;;; Returns
;;;     the GtkExpression with an additional reference.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_expression_watch_unref ()
;;;
;;; void
;;; gtk_expression_watch_unref (GtkExpressionWatch *watch);
;;;
;;; Releases a reference on the given GtkExpressionWatch.
;;;
;;; If the reference was the last, the resources associated to self are freed.
;;;
;;; watch :
;;;     a GtkExpressionWatch.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_expression_watch_evaluate ()
;;; ----------------------------------------------------------------------------

;; FIXME: This implementation is wrong.

(cffi:defcfun ("gtk_expression_watch_evaluate" %expression-watch-evaluate)
    :boolean
  (watch (g:boxed expression-watch))
  (value (:pointer (:struct g:value))))

(defun expression-watch-evaluate (watch)
 #+liber-documentation
 "@version{#2023-9-15}
  @argument[watch]{a @class{gtk:expression-watch} instance}
  @return{The @symbol{g:value} instance with the result.}
  @begin{short}
    Evaluates the watched expression and on success returns the result.
  @end{short}
  This is equivalent to calling the @fun{gtk:expression-evaluate} function with
  the expression and this pointer originally used to create @arg{watch}.
  @see-class{gtk:expression-watch}
  @see-symbol{g:value}"
  (cffi:with-foreign-object (value '(:struct g:value))
    (when (%expression-watch-evaluate watch value)
      (cffi:mem-ref value '(:struct g:value)))))

(export 'expression-watch-evaluate)

;;; ----------------------------------------------------------------------------
;;; gtk_expression_watch_unwatch ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_expression_watch_unwatch" expression-watch-unwatch) :void
 #+liber-documentation
 "@version{#2023-9-15}
  @argument[watch]{a @class{gtk:expression-watch} instance to release}
  @begin{short}
    Stops watching an expression that was established via the
    @fun{gtk:expression-watch} function.
  @end{short}
  @see-class{gtk:expression-watch}
  @see-function{gtk:expression-watch}"
  (watch (g:boxed expression-watch)))

(export 'expression-watch-unwatch)

;;; ----------------------------------------------------------------------------
;;; gtk_property_expression_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_property_expression_new" %property-expression-new)
    expression
  (gtype g:type-t)
  (expression expression)
  (property :string))

(defun property-expression-new (gtype expression property)
 #+liber-documentation
 "@version{2023-11-6}
  @argument[gtype]{a @class{g:type-t} type to expect for the @arg{this} type}
  @argument[expression]{a @class{gtk:expression} instance to evaluate to get
    the object to query or @code{nil} to query the @arg{this} object}
  @argument[property]{a string with the name of the property}
  @return{A new @class{gtk:expression} instance.}
  @begin{short}
    Creates an expression that looks up a property via the given expression or
    the @arg{this} argument when @arg{expression} is @code{nil}.
  @end{short}
  If the resulting object conforms to @arg{gtype}, its property named
  @arg{property} will be queried. Otherwise, this expression's evaluation will
  fail. The given @arg{gtype} must have a property with @arg{property}.
  @see-class{gtk:expression}"
  (let ((expression (if expression expression (cffi:null-pointer))))
    (%property-expression-new gtype expression property)))

(export 'property-expression-new)

;;; ----------------------------------------------------------------------------
;;; gtk_property_expression_new_for_pspec ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_property_expression_new_for_pspec"
               %property-expression-new-for-pspec) expression
  (expression expression)
  (pspec (:pointer (:struct g:param-spec))))

(defun property-expression-new-for-pspec (expression pspec)
 #+liber-documentation
 "@version{2023-11-6}
  @argument[expression]{a @class{gtk:expression} instance to evaluate to get
    the object to query or @code{nil} to query the @arg{this} object}
  @argument[pspec]{a @symbol{g:param-spec} instance for the property to query}
  @return{The new @class{gtk:expression} instance.}
  @begin{short}
    Creates an expression that looks up a property via the given expression or
    the @arg{this} argument when expression is @code{nil}.
  @end{short}
  If the resulting object conforms to @arg{gtype}, its property specified by
  @arg{pspec} will be queried. Otherwise, this expression's evaluation will
  fail.
  @see-class{gtk:expression}
  @see-symbol{g:param-spec}"
  (let ((expression (if expression expression (cffi:null-pointer))))
    (%property-expression-new-for-pspec expression pspec)))

(export 'property-expression-new-for-pspec)

;;; ----------------------------------------------------------------------------
;;; gtk_property_expression_get_expression ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_property_expression_get_expression"
               %property-expression-expression) expression
  (expression expression))

(defun property-expression-expression (expression)
 #+liber-documentation
 "@version{2023-11-6}
  @argument[expression]{a property @class{gtk:expression} instance}
  @return{The object @class{gtk:expression} instance.}
  @begin{short}
    Gets the expression specifying the object of a property expression.
  @end{short}
  @see-class{gtk:expression}"
  (let ((value (%property-expression-expression expression)))
    (when (not (cffi:null-pointer-p value)) value)))

(export 'property-expression-expression)

;;; ----------------------------------------------------------------------------
;;; gtk_property_expression_get_pspec ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_property_expression_get_pspec" property-expression-pspec)
    (:pointer (:struct g:param-spec))
 #+liber-documentation
 "@version{2023-11-6}
  @argument[expression]{a @class{gtk:expression} instance}
  @return{The @symbol{g:param-spec} instance.}
  @begin{short}
    Gets the @symbol{g:param-spec} instance specifying the property of a
    property expression.
  @end{short}
  @see-class{gtk:expression}
  @see-symbol{g:param-spec}"
  (expression expression))

(export 'property-expression-pspec)

;;; ----------------------------------------------------------------------------
;;; gtk_constant_expression_new ()
;;; ----------------------------------------------------------------------------

(defun constant-expression-new (gtype value)
 #+liber-documentation
 "@version{2023-11-7}
  @argument[gtype]{a @symbol{g:type-t} type}
  @argument[value]{a value corresponding to @arg{gtype}}
  @return{A new @class{gtk:expression} instance.}
  @begin{short}
    Creates an expression that evaluates to the object given by the arguments.
  @end{short}
  @begin[Example]{dictionary}
    @begin{pre}
(let ((expr (gtk:constant-expression-new \"gint\" 100)))
  (prog1
    (g:value-int (gtk:constant-expression-value expr))
    (gtk:expression-unref expr)))
=> 100
    @end{pre}
  @end{dictionary}
  @see-class{gtk:expression}"
  (gobject:with-g-value (gvalue gtype value)
    (constant-expression-new-for-value gvalue)))

(export 'constant-expression-new)

;;; ----------------------------------------------------------------------------
;;; gtk_constant_expression_new_for_value ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_constant_expression_new_for_value"
               constant-expression-new-for-value) expression
 #+liber-documentation
 "@version{2023-11-6}
  @argument[value]{a @symbol{g:value} instance}
  @return{A new @class{gtk:expression} instance.}
  @begin{short}
    Creates an expression that always evaluates to the given @arg{value}.
  @end{short}
  @see-class{gtk:expression}
  @see-symbol{g:value}"
  (value (:pointer (:struct g:value))))

(export 'constant-expression-new-for-value)

;;; ----------------------------------------------------------------------------
;;; gtk_constant_expression_get_value ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_constant_expression_get_value" constant-expression-value)
    (:pointer (:struct g:value))
 #+liber-documentation
 "@version{2023-11-6}
  @argument[expression]{a constant @class{gtk:expression} instance}
  @return{The @symbol{g:value} value.}
  @begin{short}
    Gets the value that a constant expression evaluates to.
  @end{short}
  @see-class{gtk:expression}"
  (expression expression))

(export 'constant-expression-value)

;;; ----------------------------------------------------------------------------
;;; gtk_object_expression_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_object_expression_new" object-expression-new) expression
 #+liber-documentation
 "@version{2023-11-6}
  @argument[object]{a @class{g:object} object to watch}
  @return{The new @class{gtk:expression} instance.}
  @begin{short}
    Creates an expression evaluating to the given @arg{object} with a weak
    reference.
  @end{short}
  Once the object is disposed, it will fail to evaluate. This expression is
  meant to break reference cycles. If you want to keep a reference to
  @arg{object}, use the @fun{gtk:constant-expression-new} function.
  @see-class{g:object}
  @see-class{gtk:expression}
  @see-function{gtk:constant-expression-new}"
  (object g:object))

(export 'object-expression-new)

;;; ----------------------------------------------------------------------------
;;; gtk_object_expression_get_object ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_object-expression_get_object" object-expression-object)
    g:object
 #+liber-documentation
 "@version{2023-11-6}
  @argument[expression]{a @class{gtk:expression} instance for an object}
  @return{The @class{g:object} object, or @code{nil}.}
  @short{Gets the object that the expression evaluates to.}
  @see-class{gtk:expression}"
  (expression expression))

(export 'object-expression-object)

;;; ----------------------------------------------------------------------------
;;; gtk_closure_expression_new ()
;;;
;;; GtkExpression *
;;; gtk_closure_expression_new (GType value_type,
;;;                             GClosure *closure,
;;;                             guint n_params,
;;;                             GtkExpression **params);
;;;
;;; Creates a GtkExpression that calls closure when it is evaluated. closure is
;;; called with the this object and the results of evaluating the params
;;; expressions.
;;;
;;; value_type :
;;;     the type of the value that this expression evaluates to
;;;
;;; closure :
;;;     closure to call when evaluating this expression. If closure is floating,
;;;     it is adopted
;;;
;;; n_params :
;;;     the number of params needed for evaluating closure
;;;
;;; params :
;;;     expressions for each parameter.
;;;
;;; Returns :
;;;     a new GtkExpression
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cclosure_expression_new ()
;;;
;;; GtkExpression *
;;; gtk_cclosure_expression_new (GType value_type,
;;;                              GClosureMarshal marshal,
;;;                              guint n_params,
;;;                              GtkExpression **params,
;;;                              GCallback callback_func,
;;;                              gpointer user_data,
;;;                              GClosureNotify user_destroy);
;;;
;;; This function is a variant of gtk_closure_expression_new() that creates a
;;; GClosure by calling g_cclosure_new() with the given callback_func ,
;;; user_data and user_destroy .
;;;
;;; value_type :
;;;     the type of the value that this expression evaluates to
;;;
;;; marshal :
;;;     marshaller used for creating a closure.
;;;
;;; n_params :
;;;     the number of params needed for evaluating closure
;;;
;;; params :
;;;     expressions for each parameter.
;;;
;;; callback_func :
;;;     callback used for creating a closure.
;;;
;;; user_data :
;;;     user data used for creating a closure.
;;;
;;; user_destroy :
;;;     destroy notify for user_data .
;;;
;;; Returns :
;;;     a new GtkExpression
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_VALUE_HOLDS_EXPRESSION()
;;;
;;; #define GTK_VALUE_HOLDS_EXPRESSION(value) (G_VALUE_HOLDS ((value),
;;;                                            GTK_TYPE_EXPRESSION))
;;;
;;; Evaluates to TRUE if value was initialized with GTK_TYPE_EXPRESSION.
;;;
;;; value :
;;;     a GValue
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_value_get_expression ()
;;; gtk_value_set_expression ()
;;; ----------------------------------------------------------------------------

(defun (setf value-expression) (value gvalue)
  (cffi:foreign-funcall "gtk_value_set_expression"
                        (:pointer (:struct g:value)) gvalue
                        expression value
                        :void)
  value)

(cffi:defcfun ("gtk_value_get_expression" value-expression) expression
 #+liber-documentation
 "@version{2023-11-6}
  @syntax[]{(gtk:value-expression value) => expression}
  @syntax[]{(setf (gtk:value-expression value) expression)}
  @argument[value]{a @symbol{g:value} instance initialized with the
    \"GtkExpression\" type}
  @argument[expression]{a @class{gtk:expression} instance}
  @begin{short}
    The @fun{gtk:value-expression} function retrieves the @class{gtk:expression}
    instance stored inside the given @arg{value}.
  @end{short}
  The @setf{gtk:value-expression} function stores the given
  @class{gtk:expression} instance inside @arg{value}. The @symbol{g:value}
  instance will acquire a reference to the expression.
  @see-class{gtk:expression}
  @see-symbol{g:value}"
  (value (:pointer (:struct g:value))))

(export 'value-expression)

;;; ----------------------------------------------------------------------------
;;; gtk_value_take_expression ()
;;;
;;; void
;;; gtk_value_take_expression (GValue *value,
;;;                            GtkExpression *expression);
;;;
;;; Stores the given GtkExpression inside value .
;;;
;;; This function transfers the ownership of the expression to the GValue.
;;;
;;; value :
;;;     a GValue initialized with type GTK_TYPE_EXPRESSION
;;;
;;; expression :
;;;     a GtkExpression.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_value_dup_expression ()
;;;
;;; GtkExpression *
;;; gtk_value_dup_expression (const GValue *value);
;;;
;;; Retrieves the GtkExpression stored inside the given value , and acquires a
;;; reference to it.
;;;
;;; value :
;;;     a GValue initialized with type GTK_TYPE_EXPRESSION
;;;
;;; Returns :
;;;     a GtkExpression.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkParamSpecExpression
;;;
;;; A GParamSpec for properties holding a GtkExpression.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_param_spec_expression ()
;;;
;;; GParamSpec *
;;; gtk_param_spec_expression (const char *name,
;;;                            const char *nick,
;;;                            const char *blurb,
;;;                            GParamFlags flags);
;;;
;;; Creates a new GParamSpec instance for a property holding a GtkExpression.
;;;
;;; See g_param_spec_internal() for details on the property strings.
;;;
;;; name :
;;;     canonical name of the property
;;;
;;; nick :
;;;     a user-readable name for the property
;;;
;;; blurb :
;;;     a user-readable description of the property
;;;
;;; flags :
;;;     flags for the property
;;;
;;; Returns :
;;;     a newly created property specification.
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk4.expression.lisp ---------------------------------------
