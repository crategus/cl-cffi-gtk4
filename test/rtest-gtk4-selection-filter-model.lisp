(in-package :gtk-test)

(def-suite gtk-selection-filter-model :in gtk-list-model-support)
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
          (g:gtype (cffi:foreign-funcall "gtk_selection_filter_model_get_type"
                                         :size))))
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

(test gtk-selection-filter-model-properties
  (glib-test:with-check-memory (model)
    (setf model (make-instance 'gtk:selection-filter-model))
    (is (eq (g:gtype "GObject") (gtk:selection-filter-model-item-type model)))
    (is-false (gtk:selection-filter-model-model model))
    (is (= 0 (gtk:selection-filter-model-n-items model)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_selection_filter_model_new

(test gtk-selection-filter-model-new
  (glib-test:with-check-memory (model selection)
    (setf model (gtk:single-selection-new))
    (is (typep (setf selection
                     (gtk:selection-filter-model-new model))
               'gtk:selection-filter-model))
    ;; Remove references
    (is-false (setf (gtk:selection-filter-model-model selection) nil))))

;;; 2024-12-17
