;;; ----------------------------------------------------------------------------
;;; gtk4.builder.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2025 Dieter Kaiser
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
;;; GtkBuilder
;;;
;;;     Build an interface from an XML UI definition
;;;
;;; Types and Values
;;;
;;;     GtkBuilderClosureFlags
;;;     GtkBuilderError                                     not exported
;;;
;;;     GtkBuilderScope
;;;     GtkBuilderClScope
;;;
;;; Virtual functions for GtkBuilderScope
;;;
;;;     gtk_builder_scope_create_closure
;;;     gtk_builder_scope_get_type_from_function
;;;     gtk_builder_scope_get_type_from_name
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Types and Values
;;;
;;;     GtkBuilder
;;;
;;; Properties
;;;
;;;     current-object
;;;     scope
;;;     translation-domain
;;;
;;; Accessors
;;;
;;;     gtk_builder_get_current_object
;;;     gtk_builder_set_current_object
;;;     gtk_builder_get_scope
;;;     gtk_builder_set_scope
;;;     gtk_builder_get_translation_domain
;;;     gtk_builder_set_translation_domain
;;;
;;; Functions
;;;
;;;     gtk_builder_new
;;;     gtk_builder_new_from_file
;;;     gtk_builder_new_from_resource
;;;     gtk_builder_new_from_string
;;;
;;;     gtk_builder_add_from_file
;;;     gtk_builder_add_from_resource
;;;     gtk_builder_add_from_string
;;;
;;;     gtk_builder_add_objects_from_file
;;;     gtk_builder_add_objects_from_resource
;;;     gtk_builder_add_objects_from_string
;;;
;;;     gtk_builder_extend_with_template                    not implemented
;;;
;;;     gtk_builder_get_object
;;;     gtk_builder_get_objects
;;;     gtk_builder_expose_object
;;;     gtk_builder_create_closure                          not exported
;;;
;;;     gtk_builder_get_type_from_name                      not exported
;;;     gtk_builder_value_from_string                       not implemented
;;;     gtk_builder_value_from_string_type                  not implemented
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkBuilder
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkBuilderClosureFlags
;;; ----------------------------------------------------------------------------

(gobject:define-gflags "GtkBuilderClosureFlags" builder-closure-flags
  (:export t
   :type-initializer "gtk_builder_closure_flags_get_type")
  (:swapped #.(ash 1 0)))

#+liber-documentation
(setf (liber:alias-for-symbol 'builder-closure-flags)
      "GFlags"
      (liber:symbol-documentation 'builder-closure-flags)
 "@version{2024-10-5}
  @begin{declaration}
(gobject:define-gflags \"GtkBuilderClosureFlags\" builder-closure-flags
  (:export t
   :type-initializer \"gtk_builder_closure_flags_get_type\")
  (:swapped #.(ash 1 0)))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:swapped]{The closure should be created swapped.}
    @end{table}
  @end{values}
  @begin{short}
    The list of flags that can be passed to the @fun{gtk:builder-create-closure}
    function.
  @end{short}
  New values may be added in the future for new features, so external
  implementations of the @class{gtk:builder-scope} interface should test the
  flags for unknown values and raise an error when they encounter one.
  @see-class{gtk:builder}
  @see-function{gtk:builder-create-closure}")

;;; ----------------------------------------------------------------------------
;;; GtkBuilderError                                         not exported
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GtkBuilderError" builder-error
  (:export nil
   :type-initializer "gtk_builder_error_get_type")
  (:invalid-type-function 0)
  (:unhandled-tag 1)
  (:missing-attribute 2)
  (:invalid-attribute 3)
  (:invalid-tag 4)
  (:missing-property-value 5)
  (:invalid-value 6)
  (:version-mismatch 7)
  (:duplicate-id 8)
  (:type-refused 9)
  (:template-mismatch 10)
  (:invalid-property 11)
  (:invalid-signal 12)
  (:invalid-id 13)
  (:invalid-function 14))

;;; ----------------------------------------------------------------------------
;;; GtkBuilderScope
;;; ----------------------------------------------------------------------------

(gobject:define-ginterface "GtkBuilderScope" builder-scope
  (:export t
   :type-initializer "gtk_builder_scope_get_type")
  nil)

#+liber-documentation
(setf (liber:alias-for-class 'builder-scope)
      "Interface"
      (documentation 'builder-scope 'type)
 "@version{2024-10-6}
  @begin{short}
    The @class{gtk:builder-scope} interface is an interface to provide language
    binding support to the @class{gtk:builder} object.
  @end{short}
  The goal of the @class{gtk:builder-scope} interface is to look up programming
  language specific values for strings that are given in a @class{gtk:builder}
  UI definition file. The primary intended audience is bindings that want to
  provide deeper integration of the @class{gtk:builder} object into the
  language.

  The @class{gtk:builder-scope} instance may be used with multiple
  @class{gtk:builder} objects, even at once. By default, GTK will use its own
  implementation of the @class{gtk:builder-scope} instance for the C language.

  If you implement the @class{gtk:builder-scope} instance for a language
  binding, you may want to (partially) derive from or fall back to a
  @code{GtkBuilderCScope}, as that class implements support for automatic
  lookups from C symbols.
  @begin[Notes]{dictionary}
    The Lisp binding implements the @class{gtk:builder-cl-scope} subclass. For
    the Lisp binding, this subclass is the default builder scope for the
    @class{gtk:builder} object.
  @end{dictionary}
  @see-class{gtk:builder}
  @see-class{gtk:builder-cl-scope}")

;;; ----------------------------------------------------------------------------
;;; GtkBuilderScope Inferface vtable
;;; ----------------------------------------------------------------------------

(gobject:define-vtable ("GtkBuilderScope" builder-scope)
  (:skip parent-instance (:struct g:type-interface))
  ;; Methods of the GtkBuilderScope interface
  (:skip get-type-from-name :pointer)
  (:skip get-type-from-function :pointer)
  (create-closure (:pointer    ;   for (:struct g:closure))
                   (scope (g:object builder-scope))
                   (builder (g:object builder))
                   (funcname :string)
                   (flags builder-closure-flags)
                   (object :pointer)
                   (err :pointer))))

;;; ----------------------------------------------------------------------------
;;; GtkBuilderClScope
;;; ----------------------------------------------------------------------------

(gobject:define-gobject-subclass "GtkBuilderClScope" builder-cl-scope
  (:superclass g:object
   :export t
   :interfaces ("GtkBuilderScope"))
  nil)

#+liber-documentation
(setf (liber:alias-for-class 'builder-cl-scope)
      "Class"
      (documentation 'builder-cl-scope 'type)
 "@version{2024-11-3}
  @begin{short}
    The @class{gtk:builder-cl-scope} class is the implementation of the
    @class{gtk:builder-scope} interface for the Lisp binding.
  @end{short}
  This class implements the @code{GtkBuilderScope::create_closure()} virtual
  function, which creates Lisp closures for binding signal handlers to objects
  in the UI definition. An instance of the @class{gtk:builder-cl-scope} object
  is the default builder scope for the @class{gtk:builder} object.
  @see-class{gtk:builder-scope}
  @see-class{gtk:builder}")

;;; --- gtk:builder-scope-create-closure-impl ----------------------------------

;; TODO: We need the OBJECT we are creating the closure for. For this
;; implementation the OBJECT is set with the 'object' attribute in the
;; UI definition. Can we improve the implementation to allow the
;; mechanism of swapping objects?

(defmethod builder-scope-create-closure-impl ((scope builder-cl-scope)
                                              builder funcname flags object err)
  (let ((func (find-symbol (setf funcname (string-upcase funcname)))))
    (if func
        (if object
            (gobject:create-closure-for-instance object func)
            (progn
              (warn "BUILDER-SCOPE-CREATE-CLOSURE: No object given")
              (cffi:null-pointer)))
        (progn
          (warn "BUILDER-SCOPE-CREATE-CLOSURE: Unknown function ~a" funcname)
          (cffi:null-pointer)))))

;;; ----------------------------------------------------------------------------
;;; GtkBuilder
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkBuilder" builder
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gtk_builder_get_type")
  ((current-object
    builder-current-object
    "current-object" "GObject" t t)
   (scope
    builder-scope
    "scope" "GtkBuilderScope" t t)
   (translation-domain
    builder-translation-domain
    "translation-domain" "gchararray" t t)))

;;; This Lisp extension is not documented
#+nil
(defmethod initialize-instance :after ((builder builder)
                                       &key from-file from-string)
  (when from-file
    (builder-add-from-file builder (namestring from-file)))
  (when from-string
    (builder-add-from-string builder from-string)))

  ;; Initialize default scope with a global GTK:BUILDER-CL-SCOPE instance
(let ((builder-cl-scope-instance nil))
  (defmethod initialize-instance :after ((obj builder) &key &allow-other-keys)
    (if builder-cl-scope-instance
        (setf (builder-scope obj) builder-cl-scope-instance)
        (setf (builder-scope obj)
              (setf builder-cl-scope-instance
                    (make-instance 'builder-cl-scope))))))

#+liber-documentation
(setf (documentation 'builder 'type)
 "@version{2025-05-04}
  @begin{short}
    The @class{gtk:builder} object reads XML descriptions of a user interface
    and instantiates the described objects.
  @end{short}
  To create a @class{gtk:builder} object from a user interface description, call
  the @fun{gtk:builder-new-from-file}, @fun{gtk:builder-new-from-resource} or
  @fun{gtk:builder-new-from-string} function.

  In the (unusual) case that you want to add user interface descriptions from
  multiple sources to the same @class{gtk:builder} object you can call the
  @fun{gtk:builder-new} function to get an empty builder and populate it by
  (multiple) calls to the @fun{gtk:builder-add-from-file},
  @fun{gtk:builder-add-from-resource} or @fun{gtk:builder-add-from-string}
  functions.

  The @class{gtk:builder} object holds a reference to all objects that it has
  constructed and drops these references when it is finalized. This finalization
  can cause the destruction of non-widget objects or widgets which are not
  contained in a toplevel window. For toplevel windows constructed by a builder,
  it is the responsibility of the user to call the @fun{gtk:window-destroy}
  function to get rid of them and all the widgets they contain.

  The @fun{gtk:builder-object} and @fun{gtk:builder-objects} functions can be
  used to access the widgets in the interface by the names assigned to them
  inside the UI description. Toplevel windows returned by these functions will
  stay around until the user explicitly destroys them with the
  @fun{gtk:window-destroy} function. Other widgets will either be part of a
  larger hierarchy constructed by the builder, in which case you should not
  have to worry about their lifecycle, or without a parent, in which case they
  have to be added to some container to make use of them. Non-widget objects
  need to be reffed with the @fun{g:object-ref} function to keep them beyond
  the lifespan of the builder.

  @subheading{GtkBuilder UI Definitions}
  The @class{gtk:builder} object parses textual descriptions of user interfaces
  which are specified in XML format. We refer to these descriptions as
  \"GtkBuilder UI definitions\" or just \"UI definitions\" if the context is
  clear.

  @subheading{Structure of UI definitions}
  UI definition files are always encoded in UTF-8. The toplevel element is
  @code{<interface>}. It optionally takes a @code{\"domain\"} attribute, which
  will make the builder look for translated strings using the @code{dgettext()}
  function in the domain specified. This can also be done by calling the
  @fun{gtk:builder-translation-domain} function on the builder. For example:
  @begin{pre}
<?xml version=\"1.0\" encoding=\"UTF-8\">
<interface domain=\"your-app\">
  ...
</interface>
  @end{pre}

  @subheading{Requirements}
  The target toolkit version(s) are described by @code{<requires>} elements, the
  @code{\"lib\"} attribute specifies the widget library in question, currently
  the only supported value is @code{\"gtk\"}, and the @code{\"version\"}
  attribute specifies the target version in the form @code{\"<major>.<minor>\"}.
  The @class{gtk:builder} object will error out if the version requirements are
  not met. For example:
  @begin{pre}
<?xml version=\"1.0\" encoding=\"UTF-8\">
<interface domain=\"your-app\">
  <requires lib=\"gtk\" version=\"4.0\" />
</interface>
  @end{pre}

  @subheading{Objects}
  Objects are defined as children of the @code{<interface>} element. Objects
  are described by @code{<object>} elements, which can contain @code{<property>}
  elements to set properties, @code{<signal>} elements which connect signals to
  handlers, and @code{<child>} elements, which describe child objects, most
  often widgets inside a container, but also, for example, actions in an action
  group, or columns in a tree model. A @code{<child>} element contains an
  @code{<object>} element which describes the child object.

  Typically, the specific kind of object represented by an @code{<object>}
  element is specified by the @code{\"class\"} attribute. If the type has not
  been loaded yet, GTK tries to find the type initializer from the class name
  by applying heuristics. This works in most cases, but if necessary, it is
  possible to specify the name of the type initializer explicitly with the
  @code{\"type-func\"} attribute. If your UI definition is referencing internal
  types, you should make sure to call the @fun{g:type-ensure} function for each
  object type before parsing the UI definition.

  Objects may be given a name with the @code{\"id\"} attribute, which allows
  the application to retrieve them from the builder with the
  @fun{gtk:builder-object} function. An ID is also necessary to use the object
  as property value in other parts of the UI definition. GTK reserves IDs
  starting and ending with @code{\"___\"} (three consecutive underscores) for
  its own purposes.

  @subheading{Properties}
  Setting properties of objects is pretty straightforward with the
  @code{<property>} element. The @code{\"name\"} attribute specifies the name
  of the property, and the content of the element specifies the value. For
  example:
  @begin{pre}
<object class=\"GtkButton\">
  <property name=\"label\">Hello, world</property>
</object>
  @end{pre}
  If the @code{\"translatable\"} attribute is set to a @code{true} value, GTK
  uses the @code{gettext()} function, or the @code{dgettext()} function if the
  builder has a translation domain set, to find a translation for the value.
  This happens before the value is parsed, so it can be used for properties of
  any type, but it is probably most useful for string properties. It is also
  possible to specify a context to disambiguate short strings, and comments
  which may help the translators. For example:
  @begin{pre}
<object class=\"GtkButton\">
  <property name=\"label\"
            translatable=\"yes\"
            context=\"button\">Hello, world</property>
</object>
  @end{pre}
  The @class{gtk:builder} object can parse textual representations for the most
  common property types:
  @begin{itemize}
    @item{characters}
    @item{strings}
    @item{integers}
    @item{floating-point numbers}
    @item{booleans, strings like @code{\"TRUE\"}, @code{\"t\"}, @code{\"yes\"},
      @code{\"y\"}, @code{\"1\"} are interpreted as true values, strings like
      @code{\"FALSE\"}, @code{\"f\"}, @code{\"no\"}, @code{\"n\"}, @code{\"0\"}
      are interpreted as false values}
    @item{enumeration types, can be specified by their full C identifier their
      short name used when registering the enumeration type, or their integer
      value}
    @item{flag types, can be specified by their C identifier, short name,
      integer value, and optionally combined with @code{\"|\"} for bitwise OR,
      for example, @code{\"GTK_INPUT_HINT_EMOJI|GTK_INPUT_HINT_LOWERCASE\"}, or
      @code{\"emoji|lowercase\"}}
    @item{colors, in a format understood by the @fun{gdk:rgba-parse} function}
    @item{@symbol{g:variant} parameters, can be specified in the format
      understood by the @fun{g:variant-parse} function}
    @item{pixbufs, can be specified as an object ID, a resource path or a
      filename of an image file to load relative to the builder file or the
      current working directory if the @fun{gtk:builder-add-from-string}
      function was used}
    @item{@class{g:file} objects like pixbufs, can be specified as an object ID,
      a URI or a filename of a file to load relative to the builder file or the
      current working directory if the @fun{gtk:builder-add-from-string}
      function was used}
  @end{itemize}
  Objects can be referred to by their name and by default refer to objects
  declared in the local XML fragment and objects exposed with the
  @fun{gtk:builder-expose-object} function. In general, the @class{gtk:builder}
  object allows forward references to objects declared in the local XML. An
  object does not have to be constructed before it can be referred to. The
  exception to this rule is that an object has to be constructed before it can
  be used as the value of a construct-only property.

  @subheading{Child objects}
  Many widgets have properties for child widgets, such as the
  @slot[gtk:expander]{child} property of the @class{gtk:expander} widget. In
  this case, the preferred way to specify the child widget in a UI definition
  file is to simply set the property:
  @begin{pre}
<object class=\"GtkExpander\">
  <property name=\"child\">
    <object class=\"GtkLabel\">
    ...
    </object>
  </property>
</object>
  @end{pre}
  Generic containers that can contain an arbitrary number of children, such as
  the @class{gtk:box} widget instead use the @code{<child>} element. A
  @code{<child>} element contains an @code{<object>} element which describes
  the child object. Most often, child objects are widgets inside a container,
  but they can also be, for example, actions in an action group, or columns in
  a tree model.

  Any object type that implements the @class{gtk:buildable} interface can
  specify how children may be added to it. Since many objects and widgets that
  are included with GTK already implement the @class{gtk:buildable} interface,
  typically child objects can be added using the @code{<child>} element without
  having to be concerned about the underlying implementation.

  See the @class{gtk:widget} documentation for many examples of using the
  @class{gtk:builder} object with widgets, including setting child objects using
  the @code{<child>} element.

  A noteworthy special case to the general rule that only objects implementing
  the @class{gtk:buildable} interface may specify how to handle the
  @code{<child>} element is that the @class{gtk:builder} object provides special
  support for adding objects to a @class{g:list-store} object by using the
  @code{<child>} element. For instance:
  @begin{pre}
<object class=\"GListStore\">
  <property name=\"item-type\">MyObject</property>
  <child>
    <object class=\"MyObject\"/>
  </child>
  ...
</object>
  @end{pre}
  @subheading{Property bindings}
  It is also possible to bind a property value to another object’s property
  value using the @code{\"bind-source\"} attribute to specify the source object
  of the binding, and optionally, @code{\"bind-property\"} and
  @code{\"bind-flags\"} attributes to specify the source property and source
  binding flags respectively. Internally, the @class{gtk:builder} class
  implements this using @class{g:binding} objects.

  For instance, in the example below the @code{\"label\"} property of the
  @code{bottom_label} widget is bound to the @code{\"label\"} property of the
  @code{top_button} widget:
  @begin{pre}
<object class=\"GtkBox\">
  <property name=\"orientation\">vertical</property>
  <child>
    <object class=\"GtkButton\" id=\"top_button\">
      <property name=\"label\">Hello, world</property>
    </object>
  </child>
  <child>
    <object class=\"GtkLabel\" id=\"bottom_label\">
      <property name=\"label\"
                bind-source=\"top_button\"
                bind-property=\"label\"
                bind-flags=\"sync-create\"/>
    </object>
  </child>
</object>
  @end{pre}
  For more information, see the documentation of the
  @fun{g:object-bind-property} method.

  Please note that another way to set up bindings between objects in
  @file{.ui} files is to use the @class{gtk:expression} methodology. See the
  @class{gtk:expression} documentation for more information.

  @subheading{Internal children}
  Sometimes it is necessary to refer to widgets which have implicitly been
  constructed by GTK as part of a composite widget, to set properties on them
  or to add further children, for example, the content area of a the
  @class{gtk:dialog} widget. This can be achieved by setting the
  @code{\"internal-child\"} property of the @code{<child>} element to a true
  value. Note that the @class{gtk:builder} object still requires an
  @code{<object>} element for the internal child, even if it has already been
  constructed.

  @subheading{Specialized children}
  Some widgets have different places where a child can be added, for example,
  tabs versus page content in notebooks. This can be reflected in a UI
  definition by specifying the @code{\"type\"} attribute on a @code{<child>}.
  The possible values for the @code{\"type\"} attribute are described in the
  sections describing the widget-specific portions of UI definitions.

  @subheading{Signal handlers and function pointers}
  Signal handlers are set up with the @code{<signal>} element. The
  @code{\"name\"} attribute specifies the name of the signal, and the
  @code{\"handler\"} attribute specifies the function to connect to the signal.
  @begin{pre}
<object class=\"GtkButton\" id=\"hello_button\">
  <signal name=\"clicked\" handler=\"hello_button__clicked\" />
</object>
  @end{pre}
  The remaining attributes, @code{\"after\"}, @code{\"swapped\"} and
  @code{\"object\"}, have the same meaning as the corresponding parameters of
  the @fun{g:signal-connect-object} or @fun{g:signal-connect-data} functions:
  @begin{itemize}
    @item{@code{\"after\"} matches the @code{:after} flag, and will ensure that
      the handler is called after the default class closure for the signal}
    @item{@code{\"swapped\"} matches the @code{:swapped} flag, and will swap
      the instance and closure arguments when invoking the signal handler}
    @item{@code{\"object\"} will bind the signal handler to the lifetime of the
      object referenced by the attribute}
  @end{itemize}
  By default @code{\"swapped\"} will be set to @code{\"yes\"} if not specified
  otherwise, in the case where @code{\"object\"} is set, for convenience. A
  @code{\"last_modification_time\"} attribute is also allowed, but it does not
  have a meaning to the builder.

  @subheading{Example UI Definition}
  @begin{pre}
<interface>
  <object class=\"GtkDialog\" id=\dialog1\">
    <child internal-child=\"content_area\">
      <object class=\"GtkBox\">
        <child internal-child=\"action_area\">
          <object class=\"GtkBox\">
            <child>
              <object class=\"GtkButton\" id=\"ok-button\">
                <property name=\"label\" translatable=\"yes\">_Ok</property>
                <property name=\"use-underline\">True</property>
                <signal name=\"clicked\" handler=\"ok-button-clicked\" object=\"ok-button\"/>
              </object>
            </child>
          </object>
        </child>
      </object>
    </child>
  </object>
</interface>
  @end{pre}
  @subheading{Using GtkBuildable for extending UI definitions}
  Objects can implement the @class{gtk:buildable} interface to add custom
  elements and attributes to the XML. Typically, any extension will be
  documented in each type that implements the interface.

  @subheading{Templates}
  When describing a @class{gtk:widget} object, you can use the
  @code{<template>} tag to describe a UI bound to a specific widget type. GTK
  will automatically load the UI definition when instantiating the type, and
  bind children and signal handlers to instance fields and function symbols.
  For more information, see the @class{gtk:widget} documentation.
  @see-constructor{gtk:builder-new}
  @see-constructor{gtk:builder-new-from-file}
  @see-constructor{gtk:builder-new-from-resource}
  @see-constructor{gtk:builder-new-from-string}
  @see-slot{gtk:builder-current-object}
  @see-slot{gtk:builder-scope}
  @see-slot{gtk:builder-translation-domain}
  @see-class{gtk:buildable}
  @see-class{gtk:builder-cl-scope}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:builder-current-object ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "current-object" 'builder) t)
 "The @code{current-object} property of type @class{g:object} (Read / Write)
  @br{}
  The object the builder is evaluating for.")

#+liber-documentation
(setf (liber:alias-for-function 'builder-current-object)
      "Accessor"
      (documentation 'builder-current-object 'function)
 "@version{2024-9-15}
  @syntax{(gtk:builder-current-object object) => current}
  @syntax{(setf (gtk:builder-current-object object) current)}
  @argument[object]{a @class{gtk:builder} object}
  @argument[current]{a @class{g:object} instance}
  @begin{short}
    Accessor of the @slot[gtk:builder]{current-object} slot of the
    @class{gtk:builder} class.
  @end{short}
  The @fun{gtk:builder-current-object} function gets the current object for the
  builder. The @setf{gtk:builder-current-object} function sets the current
  object. The current object can be thought of the object that the builder is
  working for and will often be used as the default object when an object is
  optional.

  The @fun{gtk:widget-init-template} function for example will set the current
  object to the widget the template is inited for. For functions like the
  @fun{gtk:builder-new-from-resource} function, the current object will be
  @code{nil}.
  @see-class{gtk:builder}
  @see-class{g:object}
  @see-function{gtk:widget-init-template}
  @see-function{gtk:builder-new-from-resource}")

;;; --- gtk:builder-scope ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "scope" 'builder) t)
 "The @code{scope} property of type @code{GtkBuilderScope}
  (Read / Write / Construct) @br{}
  The scope the builder is operating in. If the @slot[gtk:builder]{scope}
  property is @code{nil}, a @class{gtk:builder-cl-scope} instance will be set
  as the default value.")

#+liber-documentation
(setf (liber:alias-for-function 'builder-scope)
      "Accessor"
      (documentation 'builder-scope 'function)
 "@version{2024-11-4}
  @syntax{(gtk:builder-scope object) => scope}
  @syntax{(setf (gtk:builder-scope object) scope)}
  @argument[object]{a @class{gtk:builder} object}
  @argument[scope]{a @class{gtk:builder-cl-scope} object}
  @begin{short}
    Accessor of the @slot[gtk:builder]{scope} slot of the @class{gtk:builder}
    class.
  @end{short}
  The @fun{gtk:builder-scope} function gets the scope in use. The
  @setf{gtk:builder-scope} function sets the scope the builder should operate
  in. If the @arg{scope} argument is @code{nil}, a
  @class{gtk:builder-cl-scope} instance will be set as the default value.
  @see-class{gtk:builder}
  @see-class{gtk:builder-cl-scope}")

;;; --- gtk:builder-translation-domain -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "translation-domain" 'builder) t)
 "The @code{translation-domain} property of type @code{:string} (Read / Write)
  @br{}
  The translation domain used when translating property values that have been
  marked as translatable in interface descriptions. If the translation domain
  is @code{nil}, the @class{gtk:builder} object uses GNU gettext, otherwise
  GLIB gettext. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'builder-translation-domain)
      "Accessor"
      (documentation 'builder-translation-domain 'function)
 "@version{2024-11-4}
  @syntax{(gtk:builder-translation-domain object) => domain}
  @syntax{(setf (gtk:builder-translation-domain object) domain)}
  @argument[object]{a @class{gtk:builder} object}
  @argument[domain]{a string with the translation domain or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk:builder]{translation-domain} slot of the
    @class{gtk:builder} class.
  @end{short}
  The @fun{gtk:builder-translation-domain} function gets the translation domain
  of the builder. The @setf{gtk:builder-translation-domain} function sets the
  translation domain.

  The translation domain used when translating property values that have been
  marked as translatable in interface descriptions. If the translation domain
  is @code{nil}, the @class{gtk:builder} object uses the @code{gettext()}
  function, otherwise the @code{d_dgettext()} function.
  @see-class{gtk:builder}")

;;; ----------------------------------------------------------------------------
;;; gtk_builder_new
;;; ----------------------------------------------------------------------------

(declaim (inline builder-new))

(defun builder-new ()
 #+liber-documentation
 "@version{2024-9-15}
  @return{The new @class{gtk:builder} object.}
  @begin{short}
    Creates a new builder object.
  @end{short}
  This function is only useful if you intend to make multiple calls to the
  @fun{gtk:builder-add-from-file}, @fun{gtk:builder-add-from-resource} or
  @fun{gtk:builder-add-from-string} functions in order to merge multiple UI
  descriptions into a single builder.
  @see-class{gtk:builder}
  @see-function{gtk:builder-add-from-file}
  @see-function{gtk:builder-add-from-resource}
  @see-function{gtk:builder-add-from-string}"
  (make-instance 'builder))

(export 'builder-new)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_new_from_file
;;; ----------------------------------------------------------------------------

(defun builder-new-from-file (path)
 #+liber-documentation
 "@version{2024-10-6}
  @argument[path]{a pathname or namestring for the file}
  @return{The @class{gtk:builder} object containing the described interface.}
  @begin{short}
    Builds the @class{gtk:builder} UI definition from an user interface
    description file.
  @end{short}
  If there is an error opening the file or parsing the description then the
  program will be aborted. You should only ever attempt to parse user interface
  descriptions that are shipped as part of your program.
  @see-class{gtk:builder}"
  (let ((builder (make-instance 'builder)))
    (builder-add-from-file builder path)
    builder))

(export 'builder-new-from-file)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_new_from_resource
;;; ----------------------------------------------------------------------------

(defun builder-new-from-resource (path)
 #+liber-documentation
 "@version{2025-3-1}
  @argument[path]{a string for the path of the resource file to parse}
  @begin{return}
    The new @class{gtk:builder} object containing the described interface.
  @end{return}
  @begin{short}
    Builds the @class{gtk:builder} UI definition from a resource path.
  @end{short}
  If there is an error locating the resource or parsing the description then
  the program will be aborted.
  @see-class{gtk:builder}
  @see-class{g:resource}"
  (let ((builder (make-instance 'builder)))
    (builder-add-from-resource builder path)
    builder))

(export 'builder-new-from-resource)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_new_from_string
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_builder_new_from_string" %builder-new-from-string)
    (g:object builder :return)
  (string :string)
  (length :int))

(defun builder-new-from-string (string)
 #+liber-documentation
 "@version{2025-05-03}
  @argument[string]{a string for the user interface description}
  @begin{return}
    The new @class{gtk:builder} object containing the interface described by
    @arg{string}.
  @end{return}
  @begin{short}
    Builds the user interface described by @arg{string} in the
    @class{gtk:builder} UI definition format.
  @end{short}
  If there is an error parsing the string then the program will be aborted. You
  should not attempt to parse user interface description from untrusted sources.
  @see-class{gtk:builder}"
  (%builder-new-from-string string -1))

(export 'builder-new-from-string)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_add_from_file
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_builder_add_from_file" %builder-add-from-file) :uint
  (builder (g:object builder))
  (filename :string)
  (err :pointer))

(defun builder-add-from-file (builder path)
 #+liber-documentation
 "@version{2025-05-03}
  @argument[builder]{a @class{gtk:builder} object}
  @argument[path]{a pathname or namestring for the name of the file to parse}
  @begin{short}
    Parses a file containing a @class{gtk:builder} UI definition and merges it
    with the current contents of the builder.
  @end{short}
  This function is useful if you need to call the
  @fun{gtk:builder-current-object} function to add user data to callbacks
  before loading the @class{gtk:builder} UI definition. Otherwise, you probably
  want the @fun{gtk:builder-new-from-file} function instead.

  If there is an error opening the file or parsing the description then the
  program will be aborted.
  @see-class{gtk:builder}
  @see-function{gtk:builder-current-object}
  @see-function{gtk:builder-new-from-file}"
  (glib:with-error (err)
    (%builder-add-from-file builder (namestring path) err)))

(export 'builder-add-from-file)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_add_from_resource
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_builder_add_from_resource" %builder-add-from-resource) :uint
  (builder (g:object builder))
  (path :string)
  (err :pointer))

(defun builder-add-from-resource (builder path)
 #+liber-documentation
 "@version{2025-05-03}
  @argument[builder]{a @class{gtk:builder} object}
  @argument[path]{a string for the path of the resouce file to parse}
  @begin{short}
    Parses a resource file containing a @class{gtk:builder} UI definition and
    merges it with the current contents of the builder.
  @end{short}
  This function is useful if you need to call the
  @fun{gtk:builder-current-object} function to add user data to callbacks before
  loading the @class{gtk:builder} UI definition. Otherwise, you probably want
  the @fun{gtk:builder-new-from-resource} function instead.

  If there is an error locating the resource or parsing the description then
  the program will be aborted.
  @see-class{gtk:builder}
  @see-class{g:resource}
  @see-function{gtk:builder-current-object}
  @see-function{gtk:builder-new-from-resource}"
  (glib:with-error (err)
    (%builder-add-from-resource builder path err)))

(export 'builder-add-from-resource)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_add_from_string
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_builder_add_from_string" %builder-add-from-string) :boolean
  (builder (g:object builder))
  (string :string)
  (length :int)
  (err :pointer))

(defun builder-add-from-string (builder string)
 #+liber-documentation
 "@version{2024-10-5}
  @argument[builder]{a @class{gtk:builder} object}
  @argument[string]{a string to parse}
  @return{@em{True} on sucess, @em{false} if an error occured.}
  @begin{short}
    Parses a string containing a @class{gtk:builder} UI definition and merges
    it with the current contents of the builder.
  @end{short}
  This function is useful if you need to call the
  @fun{gtk:builder-current-object} function to add user data to callbacks before
  loading the @class{gtk:builder} UI definition. Otherwise, you probably want
  the @fun{gtk:builder-new-from-string} function instead.

  If there is an error parsing the string then the program will be aborted.
  @see-class{gtk:builder}
  @see-function{gtk:builder-current-object}
  @see-function{gtk:builder-new-from-string}"
  (glib:with-error (err)
    (%builder-add-from-string builder string -1 err)))

(export 'builder-add-from-string)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_add_objects_from_file
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_builder_add_objects_from_file"
               %builder-add-objects-from-file) :uint
  (builder (g:object builder))
  (filename :string)
  (strptr :pointer)
  (err :pointer))

;; TODO: Search a better place for this utility macro.
;; The macro is implemented for this file, but can be used generally to copy
;; a list of Lisp strings into a C array of strings.
(defmacro with-foreign-string-array ((strptr strs) &body body)
  (let ((i (gensym))
        (str (gensym)))
    `(let ((,strptr (cffi:foreign-alloc :pointer :count (1+ (length ,strs)))))
       (iter (for ,i from 0)
             (for ,str in ,strs)
             (setf (cffi:mem-aref ,strptr :pointer ,i)
                   (cffi:foreign-string-alloc ,str)))
       (setf (cffi:mem-aref ,strptr :pointer (length ,strs)) (cffi:null-pointer))
       (unwind-protect
         (progn ,@body)
         (progn
           (iter (for ,i from 0)
                 (repeat (1- (length ,strs)))
                 (cffi:foreign-string-free (cffi:mem-aref ,strptr
                                                          :pointer ,i)))
           (cffi:foreign-string-free ,strptr))))))

(defun builder-add-objects-from-file (builder path &rest args)
 #+liber-documentation
 "@version{2025-05-03}
  @argument[builder]{a @class{gtk:builder} object}
  @argument[path]{a pathname or namestring for the name of the file to parse}
  @argument[args]{strings for the object IDs to build}
  @return{The positive value on success, 0 if an error occurred.}
  @begin{short}
    Parses a file containing a @class{gtk:builder} UI definition building only
    the requested objects and merges them with the current contents of builder.
  @end{short}
  Upon errors 0 will be returned.
  @begin[Notes]{dictionary}
    If you are adding an object that depends on an object that is not its
    child, for instance a @class{gtk:tree-view} widget that depends on its
    @class{gtk:tree-model} implementation, you have to explicitely list all of
    them in @arg{args}.
  @end{dictionary}
  @see-class{gtk:builder}
  @see-function{gtk:builder-add-objects-from-string}
  @see-function{gtk:builder-add-objects-from-resource}"
  (with-foreign-string-array (strptr args)
    (glib:with-error (err)
      (%builder-add-objects-from-file builder (namestring path) strptr err))))

(export 'builder-add-objects-from-file)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_add_objects_from_resource
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_builder_add_objects_from_resource"
               %builder-add-objects-from-resource) :uint
  (builder (g:object builder))
  (path :string)
  (strptr :pointer)
  (err :pointer))

(defun builder-add-objects-from-resource (builder path &rest args)
 #+liber-documentation
 "@version{2025-05-03}
  @argument[builder]{a @class{gtk:builder} object}
  @argument[path]{a string for the path of the resource file to parse}
  @argument[args]{strings for the object IDs to build}
  @return{The positive value on success, 0 if an error occurred.}
  @begin{short}
    Parses a resource file containing a @class{gtk:builder} UI definition
    building only the requested objects and merges them with the current
    contents of builder.
  @end{short}
  @begin[Notes]{dictionary}
    If you are adding an object that depends on an object that is not its
    child, for instance a @class{gtk:tree-view} widget that depends on its
    @class{gtk:tree-model} implementation, you have to explicitely list all of
    them in @arg{args}.
  @end{dictionary}
  @see-class{gtk:builder}
  @see-function{gtk:builder-add-objects-from-file}
  @see-function{gtk:builder-add-objects-from-string}"
  (with-foreign-string-array (strptr args)
    (glib:with-error (err)
      (%builder-add-objects-from-resource builder path strptr err))))

(export 'builder-add-objects-from-resource)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_add_objects_from_string
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_builder_add_objects_from_string"
               %builder-add-objects-from-string) :uint
  (builder (g:object builder))
  (str :string)
  (len :int)
  (strptr :pointer)
  (err :pointer))

(defun builder-add-objects-from-string (builder string &rest args)
 #+liber-documentation
 "@version{2025-05-03}
  @argument[builder]{a @class{gtk:builder} object}
  @argument[string]{a string to parse}
  @argument[args]{strings for the object IDs to build}
  @return{The positive value on success, 0 if an error occurred.}
  @begin{short}
    Parses a string containing a @class{gtk:builder} UI definition building only
    the requested objects and merges them with the current contents of builder.
  @end{short}
  @begin[Notes]{dictionary}
    If you are adding an object that depends on an object that is not its child,
    for instance a @class{gtk:tree-view} widget that depends on its
    @class{gtk:tree-model} implementation, you have to explicitely list all of
    them in @arg{args}.
  @end{dictionary}
  @see-class{gtk:builder}
  @see-function{gtk:builder-add-objects-from-file}
  @see-function{gtk:builder-add-objects-from-resource}"
  (with-foreign-string-array (strptr args)
    (glib:with-error (err)
      (%builder-add-objects-from-string builder string -1 strptr err))))

(export 'builder-add-objects-from-string)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_extend_with_template
;;;
;;; guint
;;; gtk_builder_extend_with_template (GtkBuilder *builder,
;;;                                   GtkWidget *widget,
;;;                                   GType template_type,
;;;                                   const gchar *buffer,
;;;                                   gsize length,
;;;                                   GError **error)
;;;
;;; Main private entry point for building composite container components from
;;; template XML.
;;;
;;; This is exported purely to let gtk:builder-tool validate templates,
;;; applications have no need to call this function.
;;;
;;; builder :
;;;     a GtkBuilder
;;;
;;; widget :
;;;     the widget that is being extended
;;;
;;; template_type :
;;; the type that the template is for
;;;
;;; buffer :
;;;     the string to parse
;;;
;;; length :
;;;     the length of buffer (may be -1 if buffer is nul-terminated)
;;;
;;; error :
;;;     return location for an error, or NULL.
;;;
;;; Returns :
;;;     A positive value on success, 0 if an error occurred
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_builder_get_object
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_builder_get_object" builder-object) g:object
 #+liber-documentation
 "@version{2025-1-3}
  @argument[builder]{a @class{gtk:builder} object}
  @argument[name]{a string for the name of the object to get}
  @begin{return}
    The @class{g:object} instance named @arg{name} or @code{nil} if it
    could not be found in the object tree.
  @end{return}
  @begin{short}
    Gets the object named @arg{name} from the @class{gtk:builder} UI definition.
  @end{short}
  @see-class{gtk:builder}
  @see-class{g:object}
  @see-function{gtk:builder-objects}"
  (builder (g:object builder))
  (name :string))

(export 'builder-object)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_get_objects
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_builder_get_objects" builder-objects) (g:slist-t g:object)
 #+liber-documentation
 "@version{2024-9-15}
  @argument[builder]{a @class{gtk:builder} object}
  @begin{return}
    The list containing all the @class{g:object} instances constructed by the
    @class{gtk:builder} object.
  @end{return}
  @begin{short}
    Gets all objects that have been constructed by the builder.
  @end{short}
  @see-class{gtk:builder}
  @see-class{g:object}
  @see-function{gtk:builder-object}"
  (builder (g:object builder)))

(export 'builder-objects)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_expose_object
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_builder_expose_object" builder-expose-object) :void
 #+liber-documentation
 "@version{2025-05-03}
  @argument[builder]{a @class{gtk:builder} object}
  @argument[name]{a string for the name of the object exposed to the builder}
  @argument[object]{a @class{g:object} instance to expose}
  @begin{short}
    Adds an object to the builder object pool so it can be referenced just like
    any other object built by the builder.
  @end{short}
  @see-class{gtk:builder}
  @see-class{g:object}"
  (builder (g:object builder))
  (name :string)
  (object g:object))

(export 'builder-expose-object)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_create_closure                              not exported
;;;
;;; GClosure*
;;; gtk_builder_create_closure (GtkBuilder* builder,
;;;                             const char* function_name,
;;;                             GtkBuilderClosureFlags flags,
;;;                             GObject* object,
;;;                             GError** error)
;;;
;;; Creates a closure to invoke the function called function_name.
;;;
;;; This is using the create_closure() implementation of builder‘s
;;; GtkBuilderScope.
;;;
;;; If no closure could be created, NULL will be returned and error will be set.
;;;
;;; function_name :
;;;     Name of the function to look up. The data is owned by the caller of the
;;;     function. The value is a NUL terminated UTF-8 string.
;;;
;;; flags :
;;;     Closure creation flags
;;;
;;; object :
;;;     Object to create the closure with. The argument can be NULL. The data
;;;     is owned by the caller of the function.
;;;
;;; error :
;;;     The return location for a GError*, or NULL.
;;;
;;; Returns :
;;;     A new closure for invoking function_name. The caller of the method
;;;     takes ownership of the data, and is responsible for freeing it. The
;;;     return value can be NULL.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_builder_create_closure" %builder-create-closure) :pointer
  (builder (g:object builder))
  (func :string)
  (flags builder-closure-flags)
  (object g:object)
  (err :pointer))

(defun builder-create-closure (builder func flags object)
  (glib:with-error (err)
    (%builder-create-closure builder func flags object err)))

;;; ----------------------------------------------------------------------------
;;; gtk_builder_get_type_from_name                          not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_builder_get_type_from_name" builder-type-from-name) g:type-t
 #+liber-documentation
 "@version{#2025-05-03}
  @argument[builder]{a @class{gtk:builder} object}
  @argument[name]{a string for the type name to lookup}
  @return{The @class{g:type-t} type ID found for @arg{name}.}
  @begin{short}
    Looks up a type by name, using the virtual function that the
    @class{gtk:builder} class has for that purpose.
  @end{short}
  This is mainly used when implementing the @class{gtk:buildable} interface on
  a type.
  @see-class{gtk:builder}
  @see-class{gtk:buildable}
  @see-class{g:type-t}"
  (builder (g:object builder))
  (name :string))

;;; ----------------------------------------------------------------------------
;;; gtk_builder_value_from_string
;;;
;;; gboolean
;;; gtk_builder_value_from_string (GtkBuilder *builder,
;;;                                GParamSpec *pspec,
;;;                                const gchar *string,
;;;                                GValue *value,
;;;                                GError **error)
;;;
;;; This function demarshals a value from a string. This function calls
;;; g_value_init() on the value argument, so it need not be initialised
;;; beforehand.
;;;
;;; This function can handle char, uchar, boolean, int, uint, long, ulong, enum,
;;; flags, float, double, string, GdkColor, GdkRGBA and GtkAdjustment type
;;; values. Support for GtkWidget type values is still to come.
;;;
;;; Upon errors FALSE will be returned and error will be assigned a GError from
;;; the GTK_BUILDER_ERROR domain.
;;;
;;; builder :
;;;     a GtkBuilder
;;;
;;; pspec :
;;;     the GParamSpec for the property
;;;
;;; string :
;;;     the string representation of the value
;;;
;;; value :
;;;     the GValue to store the result in
;;;
;;; error :
;;;     return location for an error, or NULL
;;;
;;; Returns :
;;;     TRUE on success
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_builder_value_from_string_type
;;;
;;; gboolean
;;; gtk_builder_value_from_string_type (GtkBuilder *builder,
;;;                                     GType type,
;;;                                     const gchar *string,
;;;                                     GValue *value,
;;;                                     GError **error)
;;;
;;; Like gtk_builder_value_from_string(), this function demarshals a value from
;;; a string, but takes a GType instead of GParamSpec. This function calls
;;; g_value_init() on the value argument, so it need not be initialised
;;; beforehand.
;;;
;;; Upon errors FALSE will be returned and error will be assigned a GError from
;;; the GTK_BUILDER_ERROR domain.
;;;
;;; builder :
;;;     a GtkBuilder
;;;
;;; type :
;;;     the GType of the value
;;;
;;; string :
;;;     the string representation of the value
;;;
;;; value :
;;;     the GValue to store the result in
;;;
;;; error :
;;;     return location for an error, or NULL
;;;
;;; Returns :
;;;     TRUE on success
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk4.builder.lisp ------------------------------------------
