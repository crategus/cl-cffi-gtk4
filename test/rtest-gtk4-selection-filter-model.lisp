(in-package :gtk-test)

(def-suite gtk-selection-filter-model :in gtk-suite)
(in-suite gtk-selection-filter-model)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkSelectionFilterModel

(test gtk-selection-filter-model-class
  ;; Check type
  (is (g:type-is-object "GtkSelectionFilterModel"))
  ;; Check registered name
  (is (eq 'gtk:selection-filter-model
          (glib:symbol-for-gtype "GtkSelectionFilterModel")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkSelectionFilterModel")
          (g:gtype (cffi:foreign-funcall "gtk_selection_filter_model_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkSelectionFilterModel")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkSelectionFilterModel")))
  ;; Check interfaces
  (is (equal '("GListModel")
             (glib-test:list-interfaces "GtkSelectionFilterModel")))
  ;; Check properties
  (is (equal '("item-type" "model" "n-items")
             (glib-test:list-properties "GtkSelectionFilterModel")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkSelectionFilterModel")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkSelectionFilterModel"
                                      GTK:SELECTION-FILTER-MODEL
                       (:SUPERCLASS GOBJECT:OBJECT
                        :EXPORT T
                        :INTERFACES ("GListModel")
                        :TYPE-INITIALIZER "gtk_selection_filter_model_get_type")
                       ((ITEM-TYPE SELECTION-FILTER-MODEL-ITEM-TYPE
                         "item-type" "GType" T NIL)
                        (MODEL SELECTION-FILTER-MODEL-MODEL
                         "model" "GtkSelectionModel" T T)
                        (N-ITEMS SELECTION-FILTER-MODEL-N-ITEMS
                         "n-items" "guint" T NIL)))
             (gobject:get-gtype-definition "GtkSelectionFilterModel"))))

;;; --- Properties -------------------------------------------------------------

;;;     item-type                                          Since 4.8
;;;     model
;;;     n-items                                            Since 4.8

;;; --- Functions --------------------------------------------------------------

;;;     gtk_selection_filter_model_new

;;; 2024-9-19
