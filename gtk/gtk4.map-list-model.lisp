;;; ----------------------------------------------------------------------------
;;; gtk4.map-list-model.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.16 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2023 - 2024 Dieter Kaiser
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
;;; GtkMapListModel
;;;
;;;     A list model that transforms its items
;;;
;;; Types and Values
;;;
;;;     GtkMapListModel
;;;
;;; Functions
;;;
;;;     GtkMapListModelMapFunc
;;;
;;;     gtk_map_list_model_new
;;;     gtk_map_list_model_set_map_func
;;;     gtk_map_list_model_set_model
;;;     gtk_map_list_model_get_model
;;;     gtk_map_list_model_has_map
;;;
;;; Properties
;;;
;;;     has-map
;;;     item-type                                          Since 4.8
;;;     model
;;;     n-items                                            Since 4.8
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkMapListModel
;;;
;;; Implemented Interfaces
;;;
;;;     GListModel
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkMapListModel
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkMapListModel" map-list-model
  (:superclass g:object
   :export t
   :interfaces ("GListModel"
                #+gtk-4-12 "GtkSectionModel")
   :type-initializer "gtk_map_list_model_get_type")
  ((has-map
    map-list-model-has-map
    "has-map" "gboolean" t nil)
   #+gtk-4-8
   (item-type
    %map-list-model-item-type
    "item-type" "Gtype" t nil)
   (model
    map-list-model-model
    "model" "GListModel" t t)
   #+gtk-4-8
   (n-items
    map-list-model-n-items
    "n-items" "guint" t nil)))

#+liber-documentation
(setf (documentation 'map-list-model 'type)
 "@version{#2023-9-3}
  @begin{short}
    The @class{gtk:map-list-model} object is a list model that takes a list
    model and maps the items in that model to different items according to a
    @symbol{gtk:map-list-model-map-func} callback function.
  @end{short}
  @begin[Examples]{dictionary}
    Create a list of @class{gtk:event-controller} objects.
    @begin{pre}
static gpointer
map_to_controllers (gpointer widget,
                    gpointer data)
 {
  gpointer result = gtk_widget_observe_controllers (widget);
  g_object_unref (widget);
  return result;
 @}

widgets = gtk_widget_observe_children (widget);

controllers = gtk_map_list_model_new (G_TYPE_LIST_MODEL,
                                      widgets,
                                      map_to_controllers,
                                      NULL, NULL);

model = gtk_flatten_list_model_new (GTK_TYPE_EVENT_CONTROLLER,
                                    controllers);
    @end{pre}
    The @class{gtk:map-list-model} object will attempt to discard the mapped
    objects as soon as they are no longer needed and recreate them if necessary.
  @end{dictionary}
  @see-class{g:list-model}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:map-list-model-has-map ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "has-map" 'map-list-model) t)
 "The @code{has-map} property of type @code{:boolean} (Read) @br{}
  Whether a map function is set for this model. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'map-list-model-has-map)
      "Accessor"
      (documentation 'map-list-model-has-map 'function)
 "@version{#2023-9-3}
  @syntax{(gtk:map-list-model-has-map object) => setting}
  @argument[object]{a @class{gtk:map-list-model} object}
  @argument[setting]{a boolean whether a map function is set for this model}
  @begin{short}
    Accessor of the @slot[gtk:map-list-model]{has-map} slot of the
    @class{gtk:map-list-model} class.
  @end{short}
  The @fun{gtk:map-list-model-has-map} function checks if a map function is
  currently set on @arg{object}.
  @see-class{gtk:map-list-model}")

;;; --- gtk:map-list-model-item-type -------------------------------------------

#+(and gtk-4-8 liber-documentation)
(setf (documentation (liber:slot-documentation "item-type" 'map-list-model) t)
 "The @code{item-type} property of type @class{g:type-t} (Read) @br{}
  The type of items. See the @fun{g:list-model-item-type} function. Since 4.8")

#+gtk-4-8
(declaim (inline map-list-model-item-type))

#+gtk-4-8
(defun map-list-model-item-type (object)
  (g:list-model-item-type object))

#+(and gtk-4-8 liber-documentation)
(setf (liber:alias-for-function 'map-list-model-item-type)
      "Accessor"
      (documentation 'map-list-model-item-type 'function)
 "@version{#2023-9-3}
  @syntax{(gtk:map-list-model-item-type object) => gtype}
  @argument[object]{a @class{gtk:map-list-model} object}
  @argument[gtype]{a @class{g:type-t} type ID}
  @begin{short}
    Accessor of the @slot[gtk:map-list-model]{item-type} slot of the
    @class{gtk:map-list-model} class.
  @end{short}
  The type of items contained in the list model. Items must be subclasses of
  the @class{g:object} class.
  @begin[Notes]{dictionary}
    This function is equivalent to the @fun{g:list-model-item-type} function.
  @end{dictionary}
  @see-class{gtk:map-list-model}
  @see-class{g:type-t}
  @see-class{g:object}
  @see-function{g:list-model-item-type}")

;;; --- gtk:map-list-model-model -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "model" 'map-list-model) t)
 "The @code{model} property of type @class{g:list-model}
  (Read / Write / Construct only) @br{}
  The model being mapped.")

#+liber-documentation
(setf (liber:alias-for-function 'map-list-model-model)
      "Accessor"
      (documentation 'map-list-model-model 'function)
 "@version{#2023-9-3}
  @syntax{(gtk:map-list-model-model object) => model}
  @syntax{(setf (gtk:map-list-model-model object) model)}
  @argument[object]{a @class{gtk:map-list-model} object}
  @argument[model]{a @class{g:list-model} object that gets mapped}
  @begin{short}
    Accessor of the @slot[gtk:map-list-model]{model} slot of the
    @class{gtk:map-list-model} class.
  @end{short}
  The @fun{gtk:map-list-model-model} function gets the model that is currently
  being mapped or @code{nil} if none. The @setf{gtk:map-list-model-model}
  function sets the model to be mapped.

  GTK makes no effort to ensure that the model conforms to the item type
  expected by the map function. It assumes that the caller knows what they are
  doing and have set up an appropriate map function.
  @see-class{gtk:map-list-model}")

;;; --- gtk:map-list-model-n-items ---------------------------------------------

#+(and gtk-4-8 liber-documentation)
(setf (documentation (liber:slot-documentation "n-items" 'map-list-model) t)
 "The @code{n-items} property of type @code{:uint} (Read / Write) @br{}
  The number of items. See the @fun{g:list-model-n-items} function. Since 4.8
  @br{}
  Default value: 0")

#+(and gtk-4-8 liber-documentation)
(setf (liber:alias-for-function 'map-list-model-n-items)
      "Accessor"
      (documentation 'map-list-model-n-items 'function)
 "@version{#2023-9-3}
  @syntax{(gtk:map-list-model-n-items object) => n-items}
  @argument[object]{a @class{gtk:map-list-model} object}
  @argument[n-items]{an unsigned integer with the number of items contained in
    the model}
  @begin{short}
    Accessor of the @slot[gtk:map-list-model]{n-items} slot of the
    @class{gtk:map-list-model} class.
  @end{short}
  @see-class{g:map-list-model}
  @see-function{g:list-model-n-items}")

;;; ----------------------------------------------------------------------------
;;; GtkMapListModelMapFunc
;;; ----------------------------------------------------------------------------

(cffi:defcallback map-list-model-map-func :pointer
    ((item :pointer)
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (funcall func item)))

#+liber-documentation
(setf (liber:alias-for-symbol 'map-list-model-map-func)
      "Callback"
      (liber:symbol-documentation 'map-list-model-map-func)
 "@version{#2024-5-3}
  @syntax{lambda (item) => result}
  @argument[item]{a pointer to the item to map}
  @argument[result]{a pointer to the item to map to, this function may not
    return @code{cffi:null-pointer}}
  @begin{short}
    User function that is called to map an item of the original model to an item
    expected by the map model.
  @end{short}
  The returned items must conform to the item type of the model they are used
  with.
  @see-class{gtk:map-list-model}")

(export 'map-list-model-map-func)

;;; ----------------------------------------------------------------------------
;;; gtk_map_list_model_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_map_list_model_new" %map-list-model-new)
    (g:object map-list-model)
  (model (g:object g:list-model))
  (func :pointer)
  (data :pointer)
  (notify :pointer))

(defun map-list-model-new (model func)
 #+liber-documentation
 "@version{#2023-9-15}
  @argument[model]{a @class{g:list-model} object to map or @code{nil} for none}
  @argument[func]{a @symbol{gtk:map-list-model-map-func} callback function
    to map items or @code{nil}}
  @return{The new @class{gtk:map-list-model} object.}
  @begin{short}
    Creates a new @class{gtk:map-list-model} object for the given arguments.
  @end{short}
  @see-class{gtk:map-list-model}
  @see-symbol{gtk:map-list-model-map-func}"
  (%map-list-model-new model
                       (cffi:callback map-list-model-map-func)
                       (glib:allocate-stable-pointer func)
                       (cffi:callback glib:stable-pointer-destroy-notify)))

(export 'map-list-model-new)

;;; ----------------------------------------------------------------------------
;;; gtk_map_list_model_set_map_func
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_map_list_model_set_map_func" %map-list-model-set-map-func)
    :void
  (model (g:object map-list-model))
  (func :pointer)
  (data :pointer)
  (notify :pointer))

(defun map-list-model-set-map-func (model func)
 #+liber-documentation
 "@version{#2023-9-15}
  @argument[model]{a @class{gtk:map-list-model} object}
  @argument[func]{a @symbol{gtk:map-list-model-map-func} callback function
    to map items or @code{nil}}
  @begin{short}
    Sets the function used to map items.
  @end{short}
  The function will be called whenever an item needs to be mapped and must
  return the item to use for the given input item.

  Note that the @class{gtk:map-list-model} object may call this function
  multiple times on the same item, because it may delete items it does not need
  anymore.

  GTK makes no effort to ensure that @arg{func} conforms to the item type of
  @arg{model}. It assumes that the caller knows what they are doing and the map
  function returns items of the appropriate type.
  @see-class{gtk:map-list-model}
  @see-symbol{gtk:map-list-model-map-func}"
  (%map-list-model-set-map-func
          model
          (cffi:callback map-list-model-map-func)
          (glib:allocate-stable-pointer func)
          (cffi:callback glib:stable-pointer-destroy-notify)))

(export 'map-list-model-set-map-func)

;;; --- End of file gtk4.map-list-model.lisp -----------------------------------
