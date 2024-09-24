(in-package :gtk-test)

(def-suite gtk-slice-list-model :in gtk-suite)
(in-suite gtk-slice-list-model)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkSliceListModel

(test gtk-slice-list-model-class
  ;; Check type
  (is (g:type-is-object "GtkSliceListModel"))
  ;; Check registered name
  (is (eq 'gtk:slice-list-model
          (glib:symbol-for-gtype "GtkSliceListModel")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkSliceListModel")
          (g:gtype (cffi:foreign-funcall "gtk_slice_list_model_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkSliceListModel")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkSliceListModel")))
  ;; Check interfaces
  (is (equal '("GListModel" "GtkSectionModel")
             (glib-test:list-interfaces "GtkSliceListModel")))
  ;; Check properties
  (is (equal '("item-type" "model" "n-items" "offset" "size")
             (glib-test:list-properties "GtkSliceListModel")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkSliceListModel")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkSliceListModel" GTK:SLICE-LIST-MODEL
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES ("GListModel" "GtkSectionModel")
                        :TYPE-INITIALIZER "gtk_slice_list_model_get_type")
                       ((ITEM-TYPE SLICE-LIST-MODEL-ITEM-TYPE
                         "item-type" "GType" T NIL)
                        (MODEL SLICE-LIST-MODEL-MODEL "model" "GListModel" T T)
                        (N-ITEMS SLICE-LIST-MODEL-N-ITEMS "n-items" "guint" T NIL)
                        (OFFSET SLICE-LIST-MODEL-OFFSET "offset" "guint" T T)
                        (SIZE SLICE-LIST-MODEL-SIZE "size" "guint" T T)))
             (gobject:get-gtype-definition "GtkSliceListModel"))))

;;; --- Properties -------------------------------------------------------------

;;;     item-type                                          Since 4.8
;;;     model
;;;     n-items                                            Since 4.8
;;;     offset
;;;     size

;;; --- Functions --------------------------------------------------------------

;;;     gtk_slice_list_model_new
;;;     gtk_slice_list_model_set_model
;;;     gtk_slice_list_model_get_model
;;;     gtk_slice_list_model_set_offset
;;;     gtk_slice_list_model_get_offset
;;;     gtk_slice_list_model_set_size
;;;     gtk_slice_list_model_get_size

;;; 2024-9-19
