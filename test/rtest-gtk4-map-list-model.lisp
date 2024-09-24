(in-package :gtk-test)

(def-suite gtk-map-list-model :in gtk-suite)
(in-suite gtk-map-list-model)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkMapListModel

(test gtk-map-list-model-class
  ;; Check type
  (is (g:type-is-object "GtkMapListModel"))
  ;; Check registered name
  (is (eq 'gtk:map-list-model
          (glib:symbol-for-gtype "GtkMapListModel")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkMapListModel")
          (g:gtype (cffi:foreign-funcall "gtk_map_list_model_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkMapListModel")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkMapListModel")))
  ;; Check interfaces
  (is (equal '("GListModel" "GtkSectionModel")
             (glib-test:list-interfaces "GtkMapListModel")))
  ;; Check properties
  (is (equal '("has-map" "item-type" "model" "n-items")
             (glib-test:list-properties "GtkMapListModel")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkMapListModel")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkMapListModel" GTK:MAP-LIST-MODEL
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES ("GListModel" "GtkSectionModel")
                        :TYPE-INITIALIZER "gtk_map_list_model_get_type")
                       ((HAS-MAP MAP-LIST-MODEL-HAS-MAP "has-map" "gboolean" T NIL)
                        (ITEM-TYPE MAP-LIST-MODEL-ITEM-TYPE
                         "item-type" "GType" T NIL)
                        (MODEL MAP-LIST-MODEL-MODEL "model" "GListModel" T NIL)
                        (N-ITEMS MAP-LIST-MODEL-N-ITEMS
                         "n-items" "guint" T NIL)))
             (gobject:get-gtype-definition "GtkMapListModel"))))

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

;;; 2024-9-19
