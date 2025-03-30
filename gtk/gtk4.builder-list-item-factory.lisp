;;; ----------------------------------------------------------------------------
;;; gtk4.builder-list-item-factory.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2023 - 2025 Dieter Kaiser
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
;;; GtkBuilderListItemFactory
;;;
;;;     A listitem factory using ui files
;;;
;;; Types and Values
;;;
;;;     GtkBuilderListItemFactory
;;;
;;; Functions
;;;
;;;     gtk_builder_list_item_factory_new_from_bytes
;;;     gtk_builder_list_item_factory_new_from_resource
;;;
;;;     gtk_builder_list_item_factory_get_bytes
;;;     gtk_builder_list_item_factory_get_resource
;;;     gtk_builder_list_item_factory_get_scope
;;;
;;; Properties
;;;
;;;     bytes
;;;     resource
;;;     scope
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkListItemFactory
;;;         ╰── GtkBuilderListItemFactory
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkBuilderListItemFactory
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkBuilderListItemFactory" builder-list-item-factory
  (:superclass list-item-factory
   :export t
   :interfaces nil
   :type-initializer "gtk_builder_list_item_factory_get_type")
  ((bytes
    builder-list-item-factory-bytes
    "bytes" "GBytes" t t)
   (resource
    builder-list-item-factory-resource
    "resource" "gchararray" t t)
   (scope
    builder-list-item-factory-scope
    "scope" "GtkBuilderScope" t t)))

#+liber-documentation
(setf (documentation 'builder-list-item-factory 'type)
 "@version{2025-3-16}
  @begin{short}
    The @class{gtk:builder-list-item-factory} object is a
    @class{gtk:list-item-factory} object that creates widgets by instantiating
    @class{gtk:builder} UI templates.
  @end{short}

  The templates must be extending the @class{gtk:list-item} object, and
  typically use the @class{gtk:expression} instances to obtain data from the
  items in the model.
  @begin[Examples]{dictionary}
    @begin{pre}
<interface>
  <template class=\"GtkListItem\">
    <property name=\"child\">
      <object class=\"GtkLabel\">
        <property name=\"xalign\">0</property>
        <binding name=\"label\">
          <lookup name=\"name\" type=\"SettingsKey\">
            <lookup name=\"item\">GtkListItem</lookup>
          </lookup>
        </binding>
      </object>
    </property>
  </template>
</interface>
    @end{pre}
  @end{dictionary}
  @see-constructor{gtk:builder-list-item-factory-new-from-bytes}
  @see-constructor{gtk:builder-list-item-factory-new-from-resource}
  @see-slot{gtk:builder-list-item-factory-bytes}
  @see-slot{gtk:builder-list-item-factory-resource}
  @see-slot{gtk:builder-list-item-factory-scope}
  @see-class{gtk:list-item-factory}
  @see-class{gtk:builder-scope}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:builder-list-item-factory-bytes ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "bytes"
                                               'builder-list-item-factory) t)
 "The @code{bytes} property of type @class{g:bytes}
  (Read / Write / Construct Only) @br{}
  The bytes instance containing the UI definition.")

#+liber-documentation
(setf (liber:alias-for-function 'builder-list-item-factory-bytes)
      "Accessor"
      (documentation 'builder-list-item-factory-bytes 'function)
 "@version{2025-3-16}
  @syntax{(gtk:builder-list-item-factory-bytes object) => bytes}
  @syntax{(setf (gtk:builder-list-item-factory-bytes object) bytes)}
  @argument[object]{a @class{gtk:builder-list-item-factory} object}
  @argument[bytes]{a @class{g:bytes} instance for the @class{gtk:builder} data}
  @begin{short}
    Accessor of the @slot[gtk:builder-list-item-factory]{bytes} slot of the
    @class{gtk:builder-list-item-factory} class.
  @end{short}
  The @fun{gtk:builder-list-item-factory-bytes} function gets the data used as
  the @class{gtk:builder} UI template for constructing list items.
  @see-class{gtk:builder-list-item-factory}
  @see-class{gtk:builder}
  @see-class{g:bytes}")

;;; --- gtk:builder-list-item-factory-resource ---------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "resource"
                                               'builder-list-item-factory) t)
 "The @code{resource} property of type @code{:string} (Read / Write / Construct
  Only) @br{}
  The resource path containing the UI definition. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'builder-list-item-factory-resource)
      "Accessor"
      (documentation 'builder-list-item-factory-resource 'function)
 "@version{2025-3-16}
  @syntax{(gtk:builder-list-item-factory-resource object) => resource}
  @syntax{(setf (gtk:builder-list-item-factory-resource object) resource)}
  @argument[object]{a @class{gtk:builder-list-item-factory} object}
  @argument[resource]{a string for the path to the resource or @code{nil}
    if none}
  @begin{short}
    Accessor of the @slot[gtk:builder-list-item-factory]{resource} slot of the
    @class{gtk:builder-list-item-factory} class.
  @end{short}
  If the data references a resource, gets the path of that resource.
  @see-class{gtk:builder-list-item-factory}
  @see-class{g:resource}")

;;; --- gtk:builder-list-item-factory-scope ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "scope"
                                               'builder-list-item-factory) t)
 "The @code{scope} property of type @class{gtk:builder-scope} (Read / Write /
  Construct Only) @br{}
  The builder scope to use when instantiating list items.")

#+liber-documentation
(setf (liber:alias-for-function 'builder-list-item-factory-scope)
      "Accessor"
      (documentation 'builder-list-item-factory-scope 'function)
 "@version{2025-3-16}
  @syntax{(gtk:builder-list-item-factory-scope object) => scope}
  @syntax{(setf (gtk:builder-list-item-factory-scope object) scope)}
  @argument[object]{a @class{gtk:builder-list-item-factory} object}
  @argument[resource]{a @class{gtk:builder-scope} object used when
    constructing list items}
  @begin{short}
    Accessor of the @slot[gtk:builder-list-item-factory]{scope} slot of the
    @class{gtk:builder-list-item-factory} class.
  @end{short}
  Gets the scope used when constructing list items.
  @see-class{gtk:builder-list-item-factory}
  @see-class{g:resource}")

;;; ----------------------------------------------------------------------------
;;; gtk_builder_list_item_factory_new_from_bytes
;;; ----------------------------------------------------------------------------

(defun builder-list-item-factory-new-from-bytes (scope bytes)
 #+liber-documentation
 "@version{2025-3-16}
  @argument[scope]{a @class{gtk:builder-scope} object to use when instantiating}
  @argument[bytes]{a @class{g:bytes} instance containing the UI file to
    instantiate}
  @return{The new @class{gtk:builder-list-item-factory} object.}
  @begin{short}
    Creates a new builder list item factory that instantiates widgets using
    @arg{bytes} as the data to pass to the @class{gtk:builder} object.
  @end{short}
  @see-class{gtk:list-item-factory}
  @see-class{gtk:builder}
  @see-class{gtk:builder-scope}"
  (make-instance 'builder-list-item-factory
                 :scope scope
                 :bytes bytes))

(export 'builder-list-item-factory-new-from-bytes)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_list_item_factory_new_from_resource
;;; ----------------------------------------------------------------------------

(defun builder-list-item-factory-new-from-resource (scope path)
 #+liber-documentation
 "@version{2025-3-16}
  @argument[scope]{a @class{gtk:builder-scope} object to use when instantiating}
  @argument[path]{a string for a valid path to a resource that contains the
    data}
  @return{The new @class{gtk:builder-list-item-factory} object.}
  @begin{short}
    Creates a new builder list item factory that instantiates widgets using
    data read from the given resource path to pass to the @class{gtk:builder}
    object.
  @end{short}
  @see-class{gtk:list-item-factory}
  @see-class{gtk:builder}
  @see-class{g:resource}
  @see-class{gtk:builder-scope}"
  (make-instance 'builder-list-item-factory
                 :scope scope
                 :resource (or path (cffi:null-pointer))))

(export 'builder-list-item-factory-new-from-resource)

;;; --- End of file gtk4.builder-list-item-factory.lisp ------------------------
