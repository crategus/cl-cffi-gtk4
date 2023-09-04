(in-package :gtk-test)

(def-suite gtk-slice-list-model :in gtk-suite)
(in-suite gtk-slice-list-model)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkSliceListModel

(test gtk-slice-list-model-class
  ;; Type check
  (is (g:type-is-object "GtkSliceListModel"))
  ;; Check the registered name
  (is (eq 'gtk:slice-list-model
          (glib:symbol-for-gtype "GtkSliceListModel")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkSliceListModel")
          (g:gtype (cffi:foreign-funcall "gtk_slice_list_model_get_type" 
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkSliceListModel")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkSliceListModel")))
  ;; Check the interfaces
  (is (equal '("GListModel")
             (list-interfaces "GtkSliceListModel")))
  ;; Check the properties
  (is (equal '("item-type" "model" "n-items" "offset" "size")
             (list-properties "GtkSliceListModel")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkSliceListModel")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkSliceListModel" 
                                             GTK-SLICE-LIST-MODEL
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                                ("GListModel") :TYPE-INITIALIZER
                                "gtk_slice_list_model_get_type")
                               ((ITEM-TYPE GTK-SLICE-LIST-MODEL-ITEM-TYPE
                                 "item-type" "GType" T NIL)
                                (MODEL GTK-SLICE-LIST-MODEL-MODEL "model"
                                 "GListModel" T T)
                                (N-ITEMS GTK-SLICE-LIST-MODEL-N-ITEMS "n-items"
                                 "guint" T NIL)
                                (OFFSET GTK-SLICE-LIST-MODEL-OFFSET "offset"
                                 "guint" T T)
                                (SIZE GTK-SLICE-LIST-MODEL-SIZE "size" "guint"
                                 T T)))
             (gobject:get-g-type-definition "GtkSliceListModel"))))


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

;;; --- 2023-9-3 ---------------------------------------------------------------
