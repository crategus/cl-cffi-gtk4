(in-package :gtk-test)

(def-suite gtk-flatten-list-model :in gtk-suite)
(in-suite gtk-flatten-list-model)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFlattenListModel

(test gtk-flatten-list-model-class
  ;; Check type
  (is (g:type-is-object "GtkFlattenListModel"))
  ;; Check registered name
  (is (eq 'gtk:flatten-list-model
          (glib:symbol-for-gtype "GtkFlattenListModel")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkFlattenListModel")
          (g:gtype (cffi:foreign-funcall "gtk_flatten_list_model_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkFlattenListModel")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkFlattenListModel")))
  ;; Check interfaces
  (is (equal '("GListModel" "GtkSectionModel")
             (glib-test:list-interfaces "GtkFlattenListModel")))
  ;; Check properties
  (is (equal '("item-type" "model" "n-items")
             (glib-test:list-properties "GtkFlattenListModel")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkFlattenListModel")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkFlattenListModel" GTK:FLATTEN-LIST-MODEL
                       (:SUPERCLASS GOBJECT:OBJECT
                        :EXPORT T
                        :INTERFACES ("GListModel" "GtkSectionModel")
                        :TYPE-INITIALIZER "gtk_flatten_list_model_get_type")
                       ((ITEM-TYPE FLATTEN-LIST-MODEL-ITEM-TYPE
                         "item-type" "GType" T NIL)
                        (MODEL FLATTEN-LIST-MODEL-MODEL "model" "GListModel" T T)
                        (N-ITEMS FLATTEN-LIST-MODEL-N-ITEMS
                         "n-items" "guint" T NIL)))
             (gobject:get-gtype-definition "GtkFlattenListModel"))))

;;; --- Properties -------------------------------------------------------------

;;;     item-type                                          Since 4.8
;;;     model
;;;     n-items                                            Since 4.8

;;; --- Functions --------------------------------------------------------------

;;;     gtk_flatten_list_model_new
;;;     gtk_flatten_list_model_get_model_for_item

;;; 2024-9-19
