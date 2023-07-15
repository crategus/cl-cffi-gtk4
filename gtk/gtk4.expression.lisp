;;; ----------------------------------------------------------------------------
;;; gtk4.expression.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2023 Dieter Kaiser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
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
;;;     GtkExpressionNotify
;;;
;;;     gtk_expression_ref
;;;     gtk_expression_unref
;;;     gtk_expression_get_value_type
;;;     gtk_expression_is_static
;;;     gtk_expression_evaluate
;;;
;;;     gtk_expression_watch
;;;     gtk_expression_bind
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
;;; GtkExpression
;;;
;;; The GtkExpression structure contains only private data.
;;; ----------------------------------------------------------------------------

;; FIXME : GtkExpression is not derived from GObject. It is a new fundamental
;; type.

(gobject:define-g-object-class "GtkExpression" expression
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gtk_expression_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; GtkParamSpecExpression
;;;
;;; A GParamSpec for properties holding a GtkExpression.
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkParamSpecExpression" param-spec-expression
  (:superclass expression
   :export t
   :interfaces nil
   :type-initializer "gtk_param_spec_expression_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; GtkExpressionWatch
;;;
;;; An opaque structure representing a watched GtkExpression.
;;;
;;; The contents of GtkExpressionWatch should only be accessed through the
;;; provided API.
;;; ----------------------------------------------------------------------------

(glib:define-g-boxed-opaque expression-watch "GtkExpressionWatch"
  :alloc (error "GtkExpressionWatch cannot be created from the Lisp side."))

(export 'expression-watch)


;;;Description
;;;GtkExpression provides a way to describe references to values.

;;;An important aspect of expressions is that the value can be obtained from a source that is several steps away. For example, an expression may describe ‘the value of property A of object1 , which is itself the value of a property of object2 ’. And object1 may not even exist yet at the time that the expression is created. This is contrast to GObject property bindings, which can only create direct connections between the properties of two objects that must both exist for the duration of the binding.

;;;An expression needs to be "evaluated" to obtain the value that it currently refers to. An evaluation always happens in the context of a current object called this (it mirrors the behavior of object-oriented languages), which may or may not influence the result of the evaluation. Use gtk_expression_evaluate() for evaluating an expression.

;;;Various methods for defining expressions exist, from simple constants via gtk_constant_expression_new() to looking up properties in a GObject (even recursively) via gtk_property_expression_new() or providing custom functions to transform and combine expressions via gtk_closure_expression_new().

;;;Here is an example of a complex expression:

;;;when evaluated with this being a GtkListItem, it will obtain the "item" property from the GtkListItem, and then obtain the "name" property from the resulting object (which is assumed to be of type GTK_TYPE_COLOR).

;;;A more concise way to describe this would be

;;;The most likely place where you will encounter expressions is in the context of list models and list widgets using them. For example, GtkDropDown is evaluating a GtkExpression to obtain strings from the items in its model that it can then use to match against the contents of its search entry. GtkStringFilter is using a GtkExpression for similar reasons.

;;;By default, expressions are not paying attention to changes and evaluation is just a snapshot of the current state at a given time. To get informed about changes, an expression needs to be "watched" via a GtkExpressionWatch, which will cause a callback to be called whenever the value of the expression may have changed. gtk_expression_watch() starts watching an expression, and gtk_expression_watch_unwatch() stops.

;;;Watches can be created for automatically updating the property of an object, similar to GObject's GBinding mechanism, by using gtk_expression_bind().

;;;GtkExpression in GObject properties
;;;In order to use a GtkExpression as a GObject property, you must use the gtk_param_spec_expression() when creating a GParamSpec to install in the GObject class being defined; for instance:

;;;When implementing the GObjectClass.set_property() and GObjectClass.get_property() virtual functions, you must use gtk_value_get_expression(), to retrieve the stored GtkExpression from the GValue container, and gtk_value_set_expression(), to store the GtkExpression into the GValue; for instance:

;;;GtkExpression in .ui files
;;;GtkBuilder has support for creating expressions. The syntax here can be used where a GtkExpression object is needed like in a <property> tag for an expression property, or in a <binding> tag to bind a property to an expression.

;;;To create an property expression, use the <lookup> element. It can have a type attribute to specify the object type, and a name attribute to specify the property to look up. The content of <lookup> can either be an element specfiying the expression to use the object, or a string that specifies the name of the object to use.

;;;Example:

;;;To create a constant expression, use the <constant> element. If the type attribute is specified, the element content is interpreted as a value of that type. Otherwise, it is assumed to be an object.

;;;Example:

;;;To create a closure expression, use the <closure> element. The type and function attributes specify what function to use for the closure, the content of the element contains the expressions for the parameters.

;;;Example:

;;;color_expr = gtk_property_expression_new (GTK_TYPE_LIST_ITEM,
;;;                                          NULL, "item");
;;;expression = gtk_property_expression_new (GTK_TYPE_COLOR,
;;;                                          color_expr, "name");
;;;Functions
;;;GtkExpressionNotify ()
;;;void
;;;(*GtkExpressionNotify) (gpointer user_data);
;;;Callback called by gtk_expression_watch() when the expression value changes.

;;;Parameters
;;;user_data

;;;data passed to gtk_expression_watch()

;;;gtk_expression_ref ()
;;;GtkExpression *
;;;gtk_expression_ref (GtkExpression *self);
;;;Acquires a reference on the given GtkExpression.

;;;Parameters
;;;self

;;;a GtkExpression.

;;;[allow-none]
;;;Returns
;;;the GtkExpression with an additional reference.

;;;[transfer none]

;;;gtk_expression_unref ()
;;;void
;;;gtk_expression_unref (GtkExpression *self);
;;;Releases a reference on the given GtkExpression.

;;;If the reference was the last, the resources associated to the self are freed.

;;;Parameters
;;;self

;;;a GtkExpression.

;;;[allow-none]
;;;gtk_expression_get_value_type ()
;;;GType
;;;gtk_expression_get_value_type (GtkExpression *self);
;;;Gets the GType that this expression evaluates to. This type is constant and will not change over the lifetime of this expression.

;;;Parameters
;;;self

;;;a GtkExpression

;;;Returns
;;;The type returned from gtk_expression_evaluate()

;;;gtk_expression_is_static ()
;;;gboolean
;;;gtk_expression_is_static (GtkExpression *self);
;;;Checks if the expression is static.

;;;A static expression will never change its result when gtk_expression_evaluate() is called on it with the same arguments.

;;;That means a call to gtk_expression_watch() is not necessary because it will never trigger a notify.

;;;Parameters
;;;self

;;;a GtkExpression

;;;Returns
;;;TRUE if the expression is static

;;;gtk_expression_evaluate ()
;;;gboolean
;;;gtk_expression_evaluate (GtkExpression *self,
;;;                         gpointer this_,
;;;                         GValue *value);
;;;Evaluates the given expression and on success stores the result in value . The GType of value will be the type given by gtk_expression_get_value_type().

;;;It is possible that expressions cannot be evaluated - for example when the expression references objects that have been destroyed or set to NULL. In that case value will remain empty and FALSE will be returned.

;;;Parameters
;;;self

;;;a GtkExpression

;;;this_

;;;the this argument for the evaluation.

;;;[transfer none][type GObject][nullable]
;;;value

;;;an empty GValue

;;;Returns
;;;TRUE if the expression could be evaluated

;;;gtk_expression_watch ()
;;;GtkExpressionWatch *
;;;gtk_expression_watch (GtkExpression *self,
;;;                      gpointer this_,
;;;                      GtkExpressionNotify notify,
;;;                      gpointer user_data,
;;;                      GDestroyNotify user_destroy);
;;;Installs a watch for the given expression that calls the notify function whenever the evaluation of self may have changed.

;;;GTK cannot guarantee that the evaluation did indeed change when the notify gets invoked, but it guarantees the opposite: When it did in fact change, the notify will be invoked.

;;;Parameters
;;;self

;;;a GtkExpression

;;;this_

;;;the this argument to watch.

;;;[transfer none][type GObject][nullable]
;;;notify

;;;callback to invoke when the expression changes.

;;;[closure user_data]
;;;user_data

;;;user data to pass to notify callback

;;;user_destroy

;;;destroy notify for user_data

;;;Returns
;;;The newly installed watch. Note that the only reference held to the watch will be released when the watch is unwatched which can happen automatically, and not just via gtk_expression_watch_unwatch(). You should call gtk_expression_watch_ref() if you want to keep the watch around.

;;;[transfer none]

;;;gtk_expression_bind ()
;;;GtkExpressionWatch *
;;;gtk_expression_bind (GtkExpression *self,
;;;                     gpointer target,
;;;                     const char *property,
;;;                     gpointer this_);
;;;Bind target 's property named property to self .

;;;The value that self evaluates to is set via g_object_set() on target . This is repeated whenever self changes to ensure that the object's property stays synchronized with self .

;;;If self 's evaluation fails, target 's property is not updated. You can ensure that this doesn't happen by using a fallback expression.

;;;Note that this function takes ownership of self . If you want to keep it around, you should gtk_expression_ref() it beforehand.

;;;Parameters
;;;self

;;;a GtkExpression.

;;;[transfer full]
;;;target

;;;the target object to bind to.

;;;[transfer none][type GObject]
;;;property

;;;name of the property on target to bind to

;;;this_

;;;the this argument for the evaluation of self .

;;;[transfer none][type GObject][nullable]
;;;Returns
;;;a GtkExpressionWatch.

;;;[transfer none]

;;;gtk_expression_watch_ref ()
;;;GtkExpressionWatch *
;;;gtk_expression_watch_ref (GtkExpressionWatch *watch);
;;;Acquires a reference on the given GtkExpressionWatch.

;;;Parameters
;;;watch

;;;a GtkExpressionWatch.

;;;[allow-none]
;;;Returns
;;;the GtkExpression with an additional reference.

;;;[transfer none]

;;;gtk_expression_watch_unref ()
;;;void
;;;gtk_expression_watch_unref (GtkExpressionWatch *watch);
;;;Releases a reference on the given GtkExpressionWatch.

;;;If the reference was the last, the resources associated to self are freed.

;;;Parameters
;;;watch

;;;a GtkExpressionWatch.

;;;[allow-none]
;;;gtk_expression_watch_evaluate ()
;;;gboolean
;;;gtk_expression_watch_evaluate (GtkExpressionWatch *watch,
;;;                               GValue *value);
;;;Evaluates the watched expression and on success stores the result in value .

;;;This is equivalent to calling gtk_expression_evaluate() with the expression and this pointer originally used to create watch .

;;;Parameters
;;;watch

;;;a GtkExpressionWatch

;;;value

;;;an empty GValue to be set

;;;Returns
;;;TRUE if the expression could be evaluated and value was set

;;;gtk_expression_watch_unwatch ()
;;;void
;;;gtk_expression_watch_unwatch (GtkExpressionWatch *watch);
;;;Stops watching an expression that was established via gtk_expression_watch().

;;;Parameters
;;;watch

;;;watch to release.

;;;[transfer none]
;;;gtk_property_expression_new ()
;;;GtkExpression *
;;;gtk_property_expression_new (GType this_type,
;;;                             GtkExpression *expression,
;;;                             const char *property_name);
;;;Creates an expression that looks up a property via the given expression or the this argument when expression is NULL.

;;;If the resulting object conforms to this_type , its property named property_name will be queried. Otherwise, this expression's evaluation will fail.

;;;The given this_type must have a property with property_name .

;;;Parameters
;;;this_type

;;;The type to expect for the this type

;;;expression

;;;Expression to evaluate to get the object to query or NULL to query the this object.

;;;[nullable][transfer full]
;;;property_name

;;;name of the property

;;;Returns
;;;a new GtkExpression

;;;gtk_property_expression_new_for_pspec ()
;;;GtkExpression *
;;;gtk_property_expression_new_for_pspec (GtkExpression *expression,
;;;                                       GParamSpec *pspec);
;;;Creates an expression that looks up a property via the given expression or the this argument when expression is NULL.

;;;If the resulting object conforms to this_type , its property specified by pspec will be queried. Otherwise, this expression's evaluation will fail.

;;;Parameters
;;;expression

;;;Expression to evaluate to get the object to query or NULL to query the this object.

;;;[nullable][transfer full]
;;;pspec

;;;the GParamSpec for the property to query

;;;Returns
;;;a new GtkExpression

;;;gtk_property_expression_get_expression ()
;;;GtkExpression *
;;;gtk_property_expression_get_expression
;;;                               (GtkExpression *expression);
;;;Gets the expression specifying the object of a property expression.

;;;Parameters
;;;expression

;;;a property GtkExpression.

;;;[type GtkPropertyExpression]
;;;Returns
;;;the object expression.

;;;[transfer none]

;;;gtk_property_expression_get_pspec ()
;;;GParamSpec *
;;;gtk_property_expression_get_pspec (GtkExpression *expression);
;;;Gets the GParamSpec specifying the property of a property expression.

;;;Parameters
;;;expression

;;;a property GtkExpression.

;;;[type GtkPropertyExpression]
;;;Returns
;;;the GParamSpec.

;;;[transfer none]

;;;gtk_constant_expression_new ()
;;;GtkExpression *
;;;gtk_constant_expression_new (GType value_type,
;;;                             ...);
;;;Creates a GtkExpression that evaluates to the object given by the arguments.

;;;Parameters
;;;value_type

;;;The type of the object

;;;...

;;;arguments to create the object from

;;;Returns
;;;a new GtkExpression

;;;gtk_constant_expression_new_for_value ()
;;;GtkExpression *
;;;gtk_constant_expression_new_for_value (const GValue *value);
;;;Creates an expression that always evaluates to the given value .

;;;Parameters
;;;value

;;;a GValue

;;;Returns
;;;a new GtkExpression

;;;gtk_constant_expression_get_value ()
;;;const GValue *
;;;gtk_constant_expression_get_value (GtkExpression *expression);
;;;Gets the value that a constant expression evaluates to.

;;;Parameters
;;;expression

;;;a constant GtkExpression.

;;;[type GtkConstantExpression]
;;;Returns
;;;the value.

;;;[transfer none]

;;;gtk_object_expression_new ()
;;;GtkExpression *
;;;gtk_object_expression_new (GObject *object);
;;;Creates an expression evaluating to the given object with a weak reference. Once the object is disposed, it will fail to evaluate. This expression is meant to break reference cycles.

;;;If you want to keep a reference to object , use gtk_constant_expression_new().

;;;Parameters
;;;object

;;;object to watch.

;;;[transfer none]
;;;Returns
;;;a new GtkExpression

;;;gtk_object_expression_get_object ()
;;;GObject *
;;;gtk_object_expression_get_object (GtkExpression *expression);
;;;Gets the object that the expression evaluates to.

;;;Parameters
;;;expression

;;;an object GtkExpression.

;;;[type GtkObjectExpression]
;;;Returns
;;;the object, or NULL.

;;;[transfer none][nullable]

;;;gtk_closure_expression_new ()
;;;GtkExpression *
;;;gtk_closure_expression_new (GType value_type,
;;;                            GClosure *closure,
;;;                            guint n_params,
;;;                            GtkExpression **params);
;;;Creates a GtkExpression that calls closure when it is evaluated. closure is called with the this object and the results of evaluating the params expressions.

;;;Parameters
;;;value_type

;;;the type of the value that this expression evaluates to

;;;closure

;;;closure to call when evaluating this expression. If closure is floating, it is adopted

;;;n_params

;;;the number of params needed for evaluating closure

;;;params

;;;expressions for each parameter.

;;;[nullable][array length=n_params][transfer full]
;;;Returns
;;;a new GtkExpression

;;;gtk_cclosure_expression_new ()
;;;GtkExpression *
;;;gtk_cclosure_expression_new (GType value_type,
;;;                             GClosureMarshal marshal,
;;;                             guint n_params,
;;;                             GtkExpression **params,
;;;                             GCallback callback_func,
;;;                             gpointer user_data,
;;;                             GClosureNotify user_destroy);
;;;This function is a variant of gtk_closure_expression_new() that creates a GClosure by calling g_cclosure_new() with the given callback_func , user_data and user_destroy .

;;;Parameters
;;;value_type

;;;the type of the value that this expression evaluates to

;;;marshal

;;;marshaller used for creating a closure.

;;;[scope call][nullable]
;;;n_params

;;;the number of params needed for evaluating closure

;;;params

;;;expressions for each parameter.

;;;[array length=n_params][transfer full]
;;;callback_func

;;;callback used for creating a closure.

;;;[scope notified][closure user_data][destroy user_destroy]
;;;user_data

;;;user data used for creating a closure.

;;;[nullable]
;;;user_destroy

;;;destroy notify for user_data .

;;;[nullable]
;;;Returns
;;;a new GtkExpression

;;;GTK_VALUE_HOLDS_EXPRESSION()
;;;#define GTK_VALUE_HOLDS_EXPRESSION(value)       (G_VALUE_HOLDS ((value), GTK_TYPE_EXPRESSION))
;;;Evaluates to TRUE if value was initialized with GTK_TYPE_EXPRESSION.

;;;Parameters
;;;value

;;;a GValue

;;;gtk_value_set_expression ()
;;;void
;;;gtk_value_set_expression (GValue *value,
;;;                          GtkExpression *expression);
;;;Stores the given GtkExpression inside value .

;;;The GValue will acquire a reference to the expression .

;;;Parameters
;;;value

;;;a GValue initialized with type GTK_TYPE_EXPRESSION

;;;expression

;;;a GtkExpression

;;;gtk_value_take_expression ()
;;;void
;;;gtk_value_take_expression (GValue *value,
;;;                           GtkExpression *expression);
;;;Stores the given GtkExpression inside value .

;;;This function transfers the ownership of the expression to the GValue.

;;;Parameters
;;;value

;;;a GValue initialized with type GTK_TYPE_EXPRESSION

;;;expression

;;;a GtkExpression.

;;;[transfer full][nullable]
;;;gtk_value_get_expression ()
;;;GtkExpression *
;;;gtk_value_get_expression (const GValue *value);
;;;Retrieves the GtkExpression stored inside the given value .

;;;Parameters
;;;value

;;;a GValue initialized with type GTK_TYPE_EXPRESSION

;;;Returns
;;;a GtkExpression.

;;;[transfer none][nullable]

;;;gtk_value_dup_expression ()
;;;GtkExpression *
;;;gtk_value_dup_expression (const GValue *value);
;;;Retrieves the GtkExpression stored inside the given value , and acquires a reference to it.

;;;Parameters
;;;value

;;;a GValue initialized with type GTK_TYPE_EXPRESSION

;;;Returns
;;;a GtkExpression.

;;;[transfer full][nullable]

;;;gtk_param_spec_expression ()
;;;GParamSpec *
;;;gtk_param_spec_expression (const char *name,
;;;                           const char *nick,
;;;                           const char *blurb,
;;;                           GParamFlags flags);
;;;Creates a new GParamSpec instance for a property holding a GtkExpression.

;;;See g_param_spec_internal() for details on the property strings.

;;;Parameters
;;;name

;;;canonical name of the property

;;;nick

;;;a user-readable name for the property

;;;blurb

;;;a user-readable description of the property

;;;flags

;;;flags for the property

;;;Returns
;;;a newly created property specification.

;;;[transfer full]


;;; --- End of file gtk4.expression.lisp ---------------------------------------
