;;; ----------------------------------------------------------------------------
;;; gtk4.accessible.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2025 Dieter Kaiser
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
;;; GtkAccessible
;;;
;;;     Accessible interface
;;;
;;; Types and Values
;;;
;;;     GtkAccessible
;;;     GtkAccessibleList                                   Since 4.14
;;;
;;;     GtkAccessibleRole                                   gtk4.enumerations
;;;     GtkAccessibleState                                  gtk4.enumerations
;;;     GtkAccessibleProperty                               gtk4.enumerations
;;;     GtkAccessibleRelation                               gtk4.enumerations
;;;     GtkAccessibleTristate                               gtk4.enumerations
;;;     GtkAccessibleInvalidState                           gtk4.enumerations
;;;     GtkAccessibleAutocomplete                           gtk4.enumerations
;;;     GtkAccessibleSort                                   gtk4.enumerations
;;;
;;;     GtkAccessiblePlatformState                          Since 4.10
;;;
;;; Accessors
;;;
;;;     gtk_accessible_get_accessible_role
;;;
;;; Functions
;;;
;;;     gtk_accessible_list_new_from_array                  not implemented
;;;     gtk_accessible_list_new_from_list                   Since 4.14
;;;     gtk_accessible_list_get_objects                     Since 4.14
;;;
;;;     gtk_accessible_property_init_value
;;;     gtk_accessible_relation_init_value
;;;     gtk_accessible_state_init_value
;;;
;;;     gtk_accessible_get_accessible_parent                Since 4.10
;;;     gtk_accessible_set_accessible_parent                Since 4.10
;;;     gtk_accessible_get_at_context                       Since 4.10
;;;     gtk_accessible_get_bounds                           Since 4.10
;;;     gtk_accessible_get_first_accessible_child           Since 4.10
;;;     gtk_accessible_get_next_accessible_sibling          Since 4.10
;;;     gtk_accessible_get_platform_state                   Since 4.10
;;;
;;;     gtk_accessible_reset_property
;;;     gtk_accessible_reset_relation
;;;     gtk_accessible_reset_state
;;;
;;;     gtk_accessible_update_next_accessible_sibling       Since 4.10
;;;
;;;     gtk_accessible_update_property
;;;     gtk_accessible_update_property_value
;;;     gtk_accessible_update_relation
;;;     gtk_accessible_update_relation_value
;;;     gtk_accessible_update_state
;;;     gtk_accessible_update_state_value
;;;
;;;     gtk_accessible_announce                             Since 4.14
;;;
;;; Properties
;;;
;;;     accessible-role
;;;
;;; Hierarchy
;;;
;;;     GInterface
;;;     ╰── GtkAccessible
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkAccessiblePlatformState
;;; ----------------------------------------------------------------------------

#+gtk-4-10
(gobject:define-genum "GtkAccessiblePlatformState" accessible-platform-state
  (:export t
   :type-initializer "gtk_accessible_platform_state_get_type")
  (:focusable 0)
  (:focused 1)
  (:active 2))

#+(and gtk-4-10 liber-documentation)
(setf (liber:alias-for-symbol 'accessible-platform-state)
      "GEnum"
      (liber:symbol-documentation 'accessible-platform-state)
 "@version{2025-07-22}
  @begin{declaration}
(gobject:define-genum \"GtkAccessiblePlatformState\" accessible-platform-state
  (:export t
   :type-initializer \"gtk_accessible_platform_state_get_type\")
  (:focusable 0)
  (:focused 1)
  (:active 2))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:focusable]{Whether the accessible can be focused.}
      @entry[:focused]{Whether the accessible has focus.}
      @entry[:active]{Whether the accessible is active.}
    @end{simple-table}
  @end{values}
  @begin{short}
    The various platform states which can be queried using the
    @fun{gtk:accessible-platform-state} function.
  @end{short}

  Since 4.10
  @see-function{gtk:accessible-platform-state}")

;;; ----------------------------------------------------------------------------
;;; GtkAccessibleList
;;; ----------------------------------------------------------------------------

#+gtk-4-14
(glib:define-gboxed-opaque accessible-list "GtkAccessibleList"
  :export t
  :type-initializer "gtk_accessible_list_get_type"
  :alloc (error "GtkAccessibleList cannot be created from the Lisp side."))

#+(and gtk-4-14 liber-documentation)
(setf (liber:alias-for-class 'accessible-list)
      "GBoxed"
      (documentation 'accessible-list 'type)
 "@version{2024-11-05}
  @begin{declaration}
(glib:define-gboxed-opaque accessible-list \"GtkAccessibleList\"
  :export t
  :type-initializer \"gtk_accessible_list_get_type\"
  :alloc (error \"GtkAccessibleList cannot be created from the Lisp side.\"))
  @end{declaration}
  @begin{short}
    A boxed type which wraps a list of references to @class{gtk:accessible}
    objects.
  @end{short}

  Since 4.14
  @see-construcutor{gtk:accessible-list-new-from-list}
  @see-class{gtk:accessible}")

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_list_new_from_array                      not implemented
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_list_new_from_list
;;; ----------------------------------------------------------------------------

#+gtk-4-14
(cffi:defcfun ("gtk_accessible_list_new_from_list"
               accessible-list-new-from-list) (g:boxed accessible-list :return)
 #+liber-documentation
 "@version{2024-11-05}
  @argument[list]{a list of @class{gtk:accessible} objects}
  @return{The new @class{gtk:accessible-list} instance.}
  @begin{short}
    Allocates a new @class{gtk:accessible-list} instance, doing a shallow copy
    of the passed list of @class{gtk:accessible} objects.
  @end{short}

  Since 4.14
  @see-class{gtk:accessible-list}
  @see-class{gtk:accessible}"
  (list (g:slist-t (g:object accessible))))

#+gtk-4-14
(export 'accessible-list-new-from-list)

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_list_get_objects
;;; ----------------------------------------------------------------------------

#+gtk-4-14
(cffi:defcfun ("gtk_accessible_list_get_objects" accessible-list-objects)
    (g:slist-t (g:object accessible))
 #+liber-documentation
 "@version{2024-11-05}
  @argument[list]{a @class{gtk:accessible-list} instance}
  @return{The list of @class{g:object} instances.}
  @begin{short}
    Gets the list of objects this boxed type holds.
  @end{short}

  Since 4.14
  @see-class{gtk:accessible-list}"
  (list (g:boxed accessible-list)))

#+gtk-4-14
(export 'accessible-list-objects)

;;; ----------------------------------------------------------------------------
;;; GtkAccessible
;;; ----------------------------------------------------------------------------

(gobject:define-ginterface "GtkAccessible" accessible
  (:export t
   :type-initializer "gtk_accessible_get_type")
  ((accessible-role
    accessible-accessible-role
    "accessible-role" "GtkAccessibleRole" t t)))

#+liber-documentation
(setf (liber:alias-for-class 'accessible)
      "Interface"
      (documentation 'accessible 'type)
 "@version{2024-07-25}
  @begin{short}
    The @class{gtk:accessible} interface is an interface for describing UI
    elements for Assistive Technologies.
  @end{short}
  Every accessible implementation has:
  @begin{itemize}
    @begin{item}
      a \"role\", represented by a value of the @sym{gtk:accessible-role}
      enumeration
    @end{item}
    @begin{item}
      an \"attribute\", represented by a set of @sym{gtk:accessible-state},
      @sym{gtk:accessible-property} and @sym{gtk:accessible-relation} values
    @end{item}
  @end{itemize}
  The role cannot be changed after instantiating a @class{gtk:accessible}
  implementation.

  The attributes are updated every time a state of a UI element changes in a
  way that should be reflected by assistive technologies. For instance, if a
  widget visibility changes, the @code{:hidden} state will also change to
  reflect the @slot[gtk:widget]{visible} property of the widget.

  Every accessible implementation is part of a tree of accessible objects.
  Normally, this tree corresponds to the widget tree, but can be customized by
  reimplementing the @code{Gtk.AccessibleInterface.get_accessible_parent},
  @code{Gtk.AccessibleInterface.get_first_accessible_child} and
  @code{Gtk.AccessibleInterface.get_next_accessible_sibling} virtual functions.
  Note that you can not create a top-level accessible object as of now, which
  means that you must always have a parent accessible object. Also note that
  when an accessible object does not correspond to a widget, and it has
  children, whose implementation you do not control, it is necessary to ensure
  the correct shape of the a11y tree by calling the
  @fun{gtk:accessible-accessible-parent} function and updating the sibling by
  the @fun{gtk:accessible-update-next-accessible-sibling} function.
  @see-slot{gtk:accessible-accessible-role}
  @see-class{gtk:widget}
  @see-symbol{gtk:accessible-role}
  @see-symbol{gtk:accessible-state}
  @see-symbol{gtk:accessible-property}
  @see-symbol{gtk:accessible-relation}
  @see-function{gtk:widget-visible}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:accessible-accessible-role -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "accessible-role" 'accessible) t)
 "The @code{accessible-role} property of type @sym{gtk:accessible-role}
  (Read / Write) @br{}
  The accessible role of the given assistive implementation. The accessible
  role cannot be changed once set.")

#+liber-documentation
(setf (liber:alias-for-function 'accessible-accessible-role)
      "Accessor"
      (documentation 'accessible-accessible-role 'function)
 "@version{2025-07-25}
  @syntax{(gtk:accessible-accessible-role object) => role}
  @syntax{(setf (gtk:accessible-accessible-role object) role)}
  @argument[object]{a @class{gtk:accessible} widget}
  @argument[role]{a value for the @sym{gtk:accessible-role} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:accessible]{accessible-role} slot of the
    @class{gtk:accessible} class.
  @end{short}
  Retrieves the @sym{gtk:accessible-role} value for the given
  @class{gtk:accessible} widget.
  @see-class{gtk:accessible}
  @see-symbol{gtk:accessible-role}")

  ;;; ----------------------------------------------------------------------------
;;; gtk_accessible_property_init_value
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_accessible_property_init_value"
               accessible-property-init-value) :void
 #+liber-documentation
 "@version{2025-07-25}
  @argument[property]{a @sym{gtk:accessible-property} value}
  @argument[gvalue]{an uninitialized @sym{g:value} instance}
  @begin{short}
    Initializes @arg{gvalue} with the appropriate type for @arg{property}.
  @end{short}
  This function is mostly meant for language bindings, in conjunction with
  the @fun{gtk:accessible-update-property-value} function.
  @see-symbol{gtk:accessible-property}
  @see-function{gtk:accessible-update-property-value}"
  (property accessible-property)
  (value (:pointer (:struct g:value))))

(export 'accessible-property-init-value)

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_relation_init_value
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_accessible_relation_init_value"
               accessible-relation-init-value) :void
 #+liber-documentation
 "@version{2025-07-25}
  @argument[relation]{a @sym{gtk:accessible-relation} value}
  @argument[gvalue]{an uninitialized @sym{g:value} value}
  @begin{short}
    Initializes @arg{value} with the appropriate type for @arg{relation}.
  @end{short}
  This function is mostly meant for language bindings, in conjunction with the
  @fun{gtk:accessible-update-relation-value} function.
  @see-symbol{gtk:accessible-relation}
  @see-function{gtk:accessible-update-relation-value}"
  (relation accessible-relation)
  (value (:pointer (:struct g:value))))

(export 'accessible-relation-init-value)

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_state_init_value
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_accessible_state_init_value" accessible-state-init-value)
    :void
 #+liber-documentation
 "@version{2025-07-25}
  @argument[state]{a @sym{gtk:accessible-state} value}
  @argument[gvalue]{an uninitialized @sym{g:value} value}
  @begin{short}
    Initializes @arg{gvalue} with the appropriate type for @arg{state}.
  @end{short}
  This function is mostly meant for language bindings, in conjunction with the
  @fun{gtk:accessible-update-state-value} function.
  @see-symbol{gtk:accessible-state}
  @see-function{gtk:accessible-update-state-value}"
  (state accessible-state)
  (value (:pointer (:struct g:value))))

(export 'accessible-state-init-value)

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_get_accessible_parent
;;; gtk_accessible_set_accessible_parent
;;; ----------------------------------------------------------------------------

#+gtk-4-10
(defun (setf accessible-accessible-parent) (parent accessible &optional sibling)
  (cffi:foreign-funcall "gtk_accessible_set_accessible_parent"
                        (g:object accessible) accessible
                        (g:object accessible) parent
                        (g:object accessible) sibling
                        :void)
  parent)

#+gtk-4-10
(cffi:defcfun ("gtk_accessible_get_accessible_parent"
               accessible-accessible-parent) (g:object accessible)
 #+liber-documentation
 "@version{2024-11-05}
  @syntax{(gtk:accessible-accessible-parent accessible) => parent}
  @syntax{(setf (gtk:accessible-accessible-parent accessible) parent)}
  @syntax{(setf (gtk:accessible-accessible-parent accessible sibling) parent)}
  @argument[accessible]{a @class{gtk:accessible} object}
  @argument[parent]{a parent @class{gtk:accessible} object}
  @argument[sibling]{a sibling @class{gtk:accessible} object}
  @begin{short}
    The @fun{gtk:accessible-accessible-parent} function retrieves the accessible
    parent for an accessible object.
  @end{short}
  This function returns @code{nil} for top level widgets. The
  @setf{gtk:accessible-accessible-parent} function sets the parent and sibling
  of an accessible object.

  This function is meant to be used by accessible implementations that are not
  part of the widget hierarchy, but act as a logical bridge between widgets.
  For instance, if a widget creates an object that holds metadata for each
  child, and you want that object to implement the @class{gtk:accessible}
  interface, you will use this function to ensure that the parent of each child
  widget is the metadata object, and the parent of each metadata object is the
  container widget.

  Since 4.10
  @see-class{gtk:accessible}"
  (accessible (g:object accessible)))

#+gtk-4-10
(export 'accessible-accessible-parent)

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_get_at_context
;;; ----------------------------------------------------------------------------

#+gtk-4-10
(cffi:defcfun ("gtk_accessible_get_at_context" accessible-at-context)
    (g:object at-context :return)
 #+liber-documentation
 "@version{#2025-08-04}
  @argument[accessible]{a @class{gtk:accessible} object}
  @begin{return}
    The @class{gtk:at-context} object for the accessible implementaton object.
  @end{return}
  @begin{short}
    Retrieves the accessible implementation for the given @arg{accessible}.
  @end{short}

  Since 4.10
  @see-class{gtk:accessible}"
  (accessible (g:object accessible)))

#+gtk-4-10
(export 'accessible-at-context)

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_get_bounds
;;; ----------------------------------------------------------------------------

#+gtk-4-10
(cffi:defcfun ("gtk_accessible_get_bounds" %accessible-bounds) :boolean
  (accessible (g:object accessible))
  (x (:pointer :int))
  (y (:pointer :int))
  (width (:pointer :int))
  (height (:pointer :int)))

#+gtk-4-10
(defun accessible-bounds (accessible)
 #+liber-documentation
 "@version{#2025-07-19}
  @syntax{(gtk:accessible-bounds accessible) => x, y, width, height}
  @argument[accessible]{a @class{gtk:accessible} object}
  @argument[x]{an integer for the x coordinate of the top left corner of the
    accessible object}
  @argument[y]{an integer for the y coordinate of the top left corner of the
    accessible object}
  @argument[width]{an integer for the width of the accessible object}
  @argument[height]{an integer for the height of the accessible object}
  @begin{short}
    Queries the coordinates and dimensions of the accessible object.
  @end{short}
  This functionality can be overridden by @class{gtk:accessible}
  implementations, for example, to get the bounds from an ignored child widget.

  If the bounds are not valid, @code{nil} is returned.

  Since 4.10
  @see-class{gtk:accessible}"
  (cffi:with-foreign-objects ((x :int) (y :int) (width :int) (height :int))
    (when (%accessible-bounds accessible x y width height)
      (values (cffi:mem-ref x :int)
              (cffi:mem-ref y :int)
              (cffi:mem-ref width :int)
              (cffi:mem-ref height :int)))))

#+gtk-4-10
(export 'accessible-bounds)

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_get_first_accessible_child
;;; ----------------------------------------------------------------------------

#+gtk-4-10
(cffi:defcfun ("gtk_accessible_get_first_accessible_child"
               accessible-first-accessible-child) (g:object accessible :return)
 #+liber-documentation
 "@version{#2025-07-27}
  @argument[accessible]{a @class{gtk:accessible} object}
  @return{The @class{gtk:accessible} object for the first accessible child.}
  @begin{short}
    Retrieves the first accessible child of an accessible object.
  @end{short}

  Since 4.10
  @see-class{gtk:accessible}"
  (accessible (g:object accessible)))

#+gtk-4-10
(export 'accessible-first-accessible-child)

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_get_next_accessible_sibling
;;; ----------------------------------------------------------------------------

#+gtk-4-10
(cffi:defcfun ("gtk_accessible_get_next_accessible_sibling"
               accessible-next-accessible-sibling) (g:object accessible :return)
 #+liber-documentation
 "@version{#2025-07-27}
  @argument[accessible]{a @class{gtk:accessible} object}
  @return{The @class{gtk:accessible} object for the next accessible sibling.}
  @begin{short}
    Retrieves the next accessible sibling of an accessible object.
  @end{short}

  Since 4.10
  @see-class{gtk:accessible}"
  (accessible (g:object accessible)))

#+gtk-4-10
(export 'accessible-next-accessible-sibling)

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_get_platform_state
;;; ----------------------------------------------------------------------------

#+gtk-4-10
(cffi:defcfun ("gtk_accessible_get_platform_state" accessible-platform-state)
    :boolean
 #+liber-documentation
 "@version{#2025-07-25}
  @argument[accessible]{a @class{gtk:accessible} object}
  @argument[state]{a @sym{gtk:accessible-platform-state} value to query}
  @return{The boolean value of @arg{state} for the accessible object.}
  @begin{short}
    Query a platform state, such as focus.
  @end{short}
  See the @fun{gtk:accessible-platform-changed} function.

  This functionality can be overridden by the @class{gtk:accessible}
  implementations, for example, to get platform state from an ignored child
  widget, as is the case for @class{gtk:text} wrappers.

  Since 4.10
  @see-class{gtk:accessible}
  @see-class{gtk:text}
  @see-symbol{gtk:accessible-platform-state}
  @see-function{gtk:accessible-platform-changed}"
  (accessible (g:object accessible))
  (state accessible-platform-state))

#+gtk-4-10
(export 'accessible-platform-state)

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_reset_property
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_accessible_reset_property" accessible-reset-property) :void
 #+liber-documentation
 "@version{#2025-07-25}
  @argument[accessible]{a @class{gtk:accessible} object}
  @argument[property]{a @sym{gtk:accessible-property} value}
  @begin{short}
    Resets the accessible @arg{property} value to its default value.
  @end{short}
  @see-class{gtk:accessible}
  @see-symbol{gtk:accessible-property}"
  (accessible (g:object accessible))
  (property accessible-property))

(export 'accessible-reset-property)

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_reset_relation
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_accessible_reset_relation" accessible-reset-relation) :void
 #+liber-documentation
 "@version{#2025-07-25}
  @argument[accessible]{a @class{gtk:accessible} widget}
  @argument[relation]{a @sym{gtk:accessible-relation} value}
  @begin{short}
    Resets the accessible @arg{relation} value to its default value.
  @end{short}
  @see-class{gtk:accessible}
  @see-symbol{gtk:accessible-relation}"
  (accessible (g:object accessible))
  (relation accessible-relation))

(export 'accessible-reset-relation)

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_reset_state
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_accessible_reset_state" accessible-reset-state) :void
 "@version{#2025-07-25}
  @argument[accessible]{a @class{gtk:accessible} widget}
  @argument[state]{a @sym{gtk:accessible-state} value}
  @begin{short}
    Resets the accessible @arg{state} value to its default value.
  @end{short}
  @see-class{gtk:accessible}
  @see-symbol{gtk:accessible-state}"
  (accessible (g:object accessible))
  (state accessible-state))

(export 'accessible-reset-state)

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_update_next_accessible_sibling
;;; ----------------------------------------------------------------------------

#+gtk-4-10
(cffi:defcfun ("gtk_accessible_update_next_accessible_sibling"
               accessible-update-next-accessible-sibling) :void
 #+liber-documentation
 "@version{#2026-06-21}
  @argument[accessible]{a @class{gtk:accessible} object}
  @argument[sibling]{a @class{gtk:accessible} object for the new next
    accessible sibling to set, the argument can be @code{nil}}
  @begin{short}
    Updates the next accessible sibling of @arg{accessible}.
  @end{short}
  That might be useful when a new child of a custom @class{gtk:accessible}
  object is created, and it needs to be linked to a previous child.

  Since 4.10
  @see-class{gtk:accessible}"
  (accessible (g:object accessible))
  (sibling (g:object accessible)))

#+gtk-4-10
(export 'accessible-update-next-accessible-sibling)

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_update_property
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_accessible_update_property_value"
               %accessible-update-property-value) :void
  (accessible (g:object accessible))
  (n-properties :int)
  (properties (:pointer accessible-property))
  (values (:pointer (:struct g:value))))

(defun accessible-update-property (accessible properties values)
 #+liber-documentation
 "@version{#2025-07-25}
  @argument[accessible]{a @class{gtk:accessible} object}
  @argument[properties]{a list of @class{gtk:accessible-property} values}
  @argument[values]{a list of @sym{g:value} instances, one for each property}
  @begin{short}
    Updates a list of accessible properties.
  @end{short}
  See the @sym{gtk:accessible-property} documentation for the value types of
  accessible properties.

  This function should be called by @class{gtk:widget} types whenever an
  accessible property change must be communicated to assistive technologies.
  @see-class{gtk:accessible}
  @see-class{gtk:widget}
  @see-symbol{gtk:accessible-property}
  @see-symbol{g:value}"
  (let ((n (length values)))
    (cffi:with-foreign-objects ((value-ar '(:struct g:value) n)
                                (prop-ar 'accessible-property n))
      (iter (for i from 0 below n)
            (for value in values)
            (for property in properties)
            (cffi:with-foreign-object (gvalue '(:struct g:value))
              (setf (cffi:mem-aref prop-ar 'accessible-property i) property)
              (g:value-init gvalue)
              (accessible-property-init-value property gvalue)
              (gobject:set-gvalue (cffi:mem-aptr value-ar '(:struct g:value) i)
                                  value
                                  (gobject:value-type gvalue))))
      (%accessible-update-property-value accessible n prop-ar value-ar)
      (iter (for i from 0 below n)
            (gobject:value-unset (cffi:mem-aptr value-ar
                                                '(:struct g:value) i))))))

(export 'accessible-update-property)

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_update_relation
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_accessible_update_relation_value"
               %accessible-update-relation-value) :void
  (accessible (g:object accessible))
  (n-relations :int)
  (relations (:pointer accessible-relation))
  (values (:pointer (:struct g:value))))

(defun accessible-update-relation (accessible relations values)
 #+liber-documentation
 "@version{#2025-07-25}
  @argument[accessible]{a @class{gtk:accessible} object}
  @argument[relations]{a list of @class{gtk:accessible-relation} values}
  @argument[values]{a list of @sym{g:value} instances, one for each relation}
  @begin{short}
    Updates a list of accessible relations.
  @end{short}
  See the @sym{gtk:accessible-relation} documentation for the value types of
  accessible relations.

  This function should be called by @class{gtk:widget} types whenever an
  accessible relation change must be communicated to assistive technologies.
  @see-class{gtk:accessible}
  @see-class{gtk:widget}
  @see-symbol{gtk:accessible-relation}
  @see-symbol{g:value}"
  (let ((n (length values)))
    (cffi:with-foreign-objects ((value-ar '(:struct g:value) n)
                                (relation-ar 'accessible-relation n))
      (iter (for i from 0 below n)
            (for value in values)
            (for relation in relations)
            (cffi:with-foreign-object (gvalue '(:struct g:value))
              (setf (cffi:mem-aref relation-ar 'accessible-relation i) relation)
              (g:value-init gvalue)
              (accessible-relation-init-value relation gvalue)
              (gobject:set-gvalue (cffi:mem-aptr value-ar '(:struct g:value) i)
                                  value
                                  (gobject:value-type gvalue))))
      (%accessible-update-relation-value accessible n relation-ar value-ar)
      (iter (for i from 0 below n)
            (gobject:value-unset (cffi:mem-aptr value-ar
                                                '(:struct g:value) i))))))

(export 'accessible-update-relation)

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_update_state
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_accessible_update_state_value"
               %accessible-update-state-value) :void
  (accessible (g:object accessible))
  (n-states :int)
  (states (:pointer accessible-state))
  (values (:pointer (:struct g:value))))

(defun accessible-update-state (accessible states values)
 #+liber-documentation
 "@version{#2025-07-25}
  @argument[accessible]{a @class{gtk:accessible} object}
  @argument[states]{a list of @class{gtk:accessible-state} values}
  @argument[values]{a list of @sym{g:value} instances, one for each state}
  @begin{short}
    Updates a list of accessible states.
  @end{short}
  See the @sym{gtk:accessible-state} documentation for the value types of
  accessible states.

  This function should be called by @class{gtk:widget} types whenever an
  accessible state change must be communicated to assistive technologies.
  @see-class{gtk:accessible}
  @see-class{gtk:widget}
  @see-symbol{gtk:accessible-state}
  @see-symbol{g:value}"
  (let ((n (length values)))
    (cffi:with-foreign-objects ((value-ar '(:struct g:value) n)
                                (state-ar 'accessible-state n))
      (iter (for i from 0 below n)
            (for value in values)
            (for state in states)
            (cffi:with-foreign-object (gvalue '(:struct g:value))
              (setf (cffi:mem-aref state-ar 'accessible-state i) state)
              (g:value-init gvalue)
              (accessible-state-init-value state gvalue)
              (gobject:set-gvalue (cffi:mem-aptr value-ar '(:struct g:value) i)
                                  value
                                  (gobject:value-type gvalue))))
      (%accessible-update-state-value accessible n state-ar value-ar)
      (iter (for i from 0 below n)
            (gobject:value-unset (cffi:mem-aptr value-ar
                                                '(:struct g:value) i))))))

(export 'accessible-update-state)

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_announce
;;; ----------------------------------------------------------------------------

#+gtk-4-14
(cffi:defcfun ("gtk_accessible_announce" accessible-announce) :void
 #+liber-documentation
 "@version{#2025-07-25}
  @argument[accessible]{a @class{gtk:accessible} object}
  @argument[message]{a string for the message to announce}
  @argument[priority]{a @sym{gtk:accessible-announcement-priority} value
    for the priority of the announcement}
  @begin{short}
    Requests the user’s screen reader to announce the given message.
  @end{short}
  This kind of notification is useful for messages that either have only a
  visual representation or that are not exposed visually at all, for
  example, a notification about a successful operation.

  Also, by using this API, you can ensure that the message does not interrupts
  the user’s current screen reader output.

  Since 4.14
  @see-class{gtk:accessible}
  @see-symbol{gtk:accessible-announcement-priority}"
  (accessible (g:object accessible))
  (message :string)
  (priority accessible-announcement-priority))

#+gtk-4-14
(export 'accessible-announce)

;;; --- End of file gtk4.accessible.lisp ---------------------------------------
