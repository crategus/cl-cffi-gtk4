(in-package :gtk-test)

(def-suite gtk-flatten-list-model :in gtk-suite)
(in-suite gtk-flatten-list-model)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFlattenListModel

(test gtk-flatten-list-model-class
  ;; Type check
  (is (g:type-is-object "GtkFlattenListModel"))
  ;; Check the registered name
  (is (eq 'gtk:flatten-list-model
          (glib:symbol-for-gtype "GtkFlattenListModel")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkFlattenListModel")
          (g:gtype (cffi:foreign-funcall "gtk_flatten_list_model_get_type" 
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkFlattenListModel")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkFlattenListModel")))
  ;; Check the interfaces
  (is (equal '("GListModel")
             (list-interfaces "GtkFlattenListModel")))
  ;; Check the properties
  (is (equal '("item-type" "model" "n-items")
             (list-properties "GtkFlattenListModel")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkFlattenListModel")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkFlattenListModel" 
                                             GTK-FLATTEN-LIST-MODEL
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                                ("GListModel") :TYPE-INITIALIZER
                                "gtk_flatten_list_model_get_type")
                               ((ITEM-TYPE GTK-FLATTEN-LIST-MODEL-ITEM-TYPE
                                 "item-type" "GType" T NIL)
                                (MODEL GTK-FLATTEN-LIST-MODEL-MODEL "model"
                                 "GListModel" T T)
                                (N-ITEMS GTK-FLATTEN-LIST-MODEL-N-ITEMS
                                 "n-items" "guint" T NIL)))
             (gobject:get-g-type-definition "GtkFlattenListModel"))))

;;; --- Properties -------------------------------------------------------------

;;;     item-type                                          Since 4.8
;;;     model
;;;     n-items                                            Since 4.8

;;; --- Functions --------------------------------------------------------------

;;;     gtk_flatten_list_model_new
;;;     gtk_flatten_list_model_get_model_for_item

;;; --- 2023-9-3 ---------------------------------------------------------------
