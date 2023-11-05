(in-package :gtk-test)

(def-suite gtk-map-list-model :in gtk-suite)
(in-suite gtk-map-list-model)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkMapListModel

(test gtk-map-list-model-class
  ;; Type check
  (is (g:type-is-object "GtkMapListModel"))
  ;; Check the registered name
  (is (eq 'gtk:map-list-model
          (glib:symbol-for-gtype "GtkMapListModel")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkMapListModel")
          (g:gtype (cffi:foreign-funcall "gtk_map_list_model_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkMapListModel")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkMapListModel")))
  ;; Check the interfaces
  (is (equal '("GListModel" "GtkSectionModel")
             (list-interfaces "GtkMapListModel")))
  ;; Check the properties
  (is (equal '("has-map" "item-type" "model" "n-items")
             (list-properties "GtkMapListModel")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkMapListModel")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkMapListModel"
                                             GTK-MAP-LIST-MODEL
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                                ("GListModel" "GtkSectionModel")
                                :TYPE-INITIALIZER
                                "gtk_map_list_model_get_type")
                               ((HAS-MAP GTK-MAP-LIST-MODEL-HAS-MAP "has-map"
                                 "gboolean" T NIL)
                                (ITEM-TYPE GTK-MAP-LIST-MODEL-ITEM-TYPE
                                 "item-type" "GType" T NIL)
                                (MODEL GTK-MAP-LIST-MODEL-MODEL "model"
                                 "GListModel" T NIL)
                                (N-ITEMS GTK-MAP-LIST-MODEL-N-ITEMS "n-items"
                                 "guint" T NIL)))
             (gobject:get-g-type-definition "GtkMapListModel"))))

;;; --- Properties -------------------------------------------------------------

;;;     has-map
;;;     item-type                                          Since 4.8
;;;     model
;;;     n-items                                            Since 4.8

;;; --- Functions --------------------------------------------------------------

;;;     GtkMapListModelMapFunc

;;;     gtk_map_list_model_new
;;;     gtk_map_list_model_set_map_func
;;;     gtk_map_list_model_set_model
;;;     gtk_map_list_model_get_model
;;;     gtk_map_list_model_has_map

;;; --- 2023-9-3 ---------------------------------------------------------------
