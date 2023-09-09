(in-package :gtk-test)

(def-suite gtk-selection-filter-model :in gtk-suite)
(in-suite gtk-selection-filter-model)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkSelectionFilterModel

(test gtk-selection-filter-model-class
  ;; Type check
  (is (g:type-is-object "GtkSelectionFilterModel"))
  ;; Check the registered name
  (is (eq 'gtk:selection-filter-model
          (glib:symbol-for-gtype "GtkSelectionFilterModel")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkSelectionFilterModel")
          (g:gtype (cffi:foreign-funcall "gtk_selection_filter_model_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkSelectionFilterModel")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkSelectionFilterModel")))
  ;; Check the interfaces
  (is (equal '("GListModel")
             (list-interfaces "GtkSelectionFilterModel")))
  ;; Check the properties
  (is (equal '("item-type" "model" "n-items")
             (list-properties "GtkSelectionFilterModel")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkSelectionFilterModel")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkSelectionFilterModel"
                               GTK-SELECTION-FILTER-MODEL
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                                ("GListModel") :TYPE-INITIALIZER
                                "gtk_selection_filter_model_get_type")
                               ((ITEM-TYPE GTK-SELECTION-FILTER-MODEL-ITEM-TYPE
                                 "item-type" "GType" T NIL)
                                (MODEL GTK-SELECTION-FILTER-MODEL-MODEL "model"
                                 "GtkSelectionModel" T T)
                                (N-ITEMS GTK-SELECTION-FILTER-MODEL-N-ITEMS
                                 "n-items" "guint" T NIL)))
             (gobject:get-g-type-definition "GtkSelectionFilterModel"))))

;;; --- Properties -------------------------------------------------------------

;;;     item-type                                          Since 4.8
;;;     model
;;;     n-items                                            Since 4.8

;;; --- Functions --------------------------------------------------------------

;;;     gtk_selection_filter_model_new

;;; --- 2023-9-6 ---------------------------------------------------------------
