;;; ----------------------------------------------------------------------------
;;; gtk4.map-list-model.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2023 Dieter Kaiser
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

(gobject:define-g-object-class "GtkMapListModel" map-list-model
  (:superclass g:object
   :export t
   :interfaces ("GListModel")
   :type-initializer "gtk_map_list_model_get_type")
  ((has-map
    map-list-model-has-map
    "has-map" "gboolean" t nil)
   #+gtk-4-8
   (item-type
    map-list-model-item-type
    "item-type" "Gtype" t nil)
   (model
    map-list-model-model
    "model" "GListModel" t t)
   #+gtk-4-8
   (n-items
    map-list-model-n-items
    "n-items" "guint" t nil)))


;;;Property Details
;;;The “has-map” property
;;;  “has-map”                  gboolean
;;;If a map is set for this model

;;;Owner: GtkMapListModel

;;;Flags: Read

;;;Default value: FALSE

;;;The “model” property
;;;  “model”                    GListModel *
;;;The model being mapped

;;;Owner: GtkMapListModel

;;;Flags: Read / Write / Construct Only

;;;See Also
;;;GListModel


;;;Description
;;;GtkMapListModel is a list model that takes a list model and maps the items in that model to different items according to a GtkMapListModelMapFunc.

;;;Example: Create a list of GtkEventControllers

;;;static gpointer
;;;map_to_controllers (gpointer widget,
;;;                    gpointer data)
;;; {
;;;  gpointer result = gtk_widget_observe_controllers (widget);
;;;  g_object_unref (widget);
;;;  return result;
;;; }

;;;widgets = gtk_widget_observe_children (widget);

;;;controllers = gtk_map_list_model_new (G_TYPE_LIST_MODEL,
;;;                                      widgets,
;;;                                      map_to_controllers,
;;;                                      NULL, NULL);

;;;model = gtk_flatten_list_model_new (GTK_TYPE_EVENT_CONTROLLER,
;;;                                    controllers);
;;;GtkMapListModel will attempt to discard the mapped objects as soon as they are no longer needed and recreate them if necessary.

;;;Functions
;;;GtkMapListModelMapFunc ()
;;;gpointer
;;;(*GtkMapListModelMapFunc) (gpointer item,
;;;                           gpointer user_data);
;;;User function that is called to map an item of the original model to an item expected by the map model.

;;;The returned items must conform to the item type of the model they are used with.

;;;Parameters
;;;item

;;;The item to map.

;;;[type GObject][transfer full]
;;;user_data

;;;user data

;;;Returns
;;;The item to map to. This function may not return NULL.

;;;[type GObject][transfer full]

;;;gtk_map_list_model_new ()
;;;GtkMapListModel *
;;;gtk_map_list_model_new (GListModel *model,
;;;                        GtkMapListModelMapFunc map_func,
;;;                        gpointer user_data,
;;;                        GDestroyNotify user_destroy);
;;;Creates a new GtkMapListModel for the given arguments.

;;;Parameters
;;;model

;;;The model to map or NULL for none.

;;;[transfer full][allow-none]
;;;map_func

;;;map function or NULL to not map items.

;;;[allow-none]
;;;user_data

;;;user data passed to map_func .

;;;[closure]
;;;user_destroy

;;;destroy notifier for user_data

;;;Returns
;;;a new GtkMapListModel

;;;gtk_map_list_model_set_map_func ()
;;;void
;;;gtk_map_list_model_set_map_func (GtkMapListModel *self,
;;;                                 GtkMapListModelMapFunc map_func,
;;;                                 gpointer user_data,
;;;                                 GDestroyNotify user_destroy);
;;;Sets the function used to map items. The function will be called whenever an item needs to be mapped and must return the item to use for the given input item.

;;;Note that GtkMapListModel may call this function multiple times on the same item, because it may delete items it doesn't need anymore.

;;;GTK makes no effort to ensure that map_func conforms to the item type of self . It assumes that the caller knows what they are doing and the map function returns items of the appropriate type.

;;;Parameters
;;;self

;;;a GtkMapListModel

;;;map_func

;;;map function or NULL to not map items.

;;;[allow-none]
;;;user_data

;;;user data passed to map_func .

;;;[closure]
;;;user_destroy

;;;destroy notifier for user_data

;;;gtk_map_list_model_set_model ()
;;;void
;;;gtk_map_list_model_set_model (GtkMapListModel *self,
;;;                              GListModel *model);
;;;Sets the model to be mapped.

;;;GTK makes no effort to ensure that model conforms to the item type expected by the map function. It assumes that the caller knows what they are doing and have set up an appropriate map function.

;;;Parameters
;;;self

;;;a GtkMapListModel

;;;model

;;;The model to be mapped.

;;;[allow-none]
;;;gtk_map_list_model_get_model ()
;;;GListModel *
;;;gtk_map_list_model_get_model (GtkMapListModel *self);
;;;Gets the model that is currently being mapped or NULL if none.

;;;Parameters
;;;self

;;;a GtkMapListModel

;;;Returns
;;;The model that gets mapped.

;;;[nullable][transfer none]

;;;gtk_map_list_model_has_map ()
;;;gboolean
;;;gtk_map_list_model_has_map (GtkMapListModel *self);
;;;Checks if a map function is currently set on self

;;;Parameters
;;;self

;;;a GtkMapListModel

;;;Returns
;;;TRUE if a map function is set


;;; --- End of file gtk4.map-list-model.lisp -----------------------------------
