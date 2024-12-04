(in-package :gtk-test)

(def-suite gtk-multi-selection :in gtk-list-model-support)
(in-suite gtk-multi-selection)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkMultiSelection

(test gtk-multi-selection-class
  ;; Check type
  (is (g:type-is-object "GtkMultiSelection"))
  ;; Check registered name
  (is (eq 'gtk:multi-selection
          (glib:symbol-for-gtype "GtkMultiSelection")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkMultiSelection")
          (g:gtype (cffi:foreign-funcall "gtk_multi_selection_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkMultiSelection")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkMultiSelection")))
  ;; Check interfaces
  (is (equal '("GListModel" "GtkSectionModel" "GtkSelectionModel")
             (glib-test:list-interfaces "GtkMultiSelection")))
  ;; Check properties
  (is (equal '("item-type" "model" "n-items")
             (glib-test:list-properties "GtkMultiSelection")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkMultiSelection")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkMultiSelection" GTK:MULTI-SELECTION
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES
                        ("GListModel" "GtkSectionModel" "GtkSelectionModel")
                        :TYPE-INITIALIZER "gtk_multi_selection_get_type")
                       ((ITEM-TYPE MULTI-SELECTION-ITEM-TYPE
                         "item-type" "GType" T NIL)
                        (MODEL MULTI-SELECTION-MODEL "model" "GListModel" T T)
                        (N-ITEMS MULTI-SELECTION-N-ITEMS
                         "n-items" "guint" T NIL)))
             (gobject:get-gtype-definition "GtkMultiSelection"))))

;;; --- Properties -------------------------------------------------------------

;;;     item-type                                          Since 4.8
;;;     model
;;;     n-items                                            Since 4.8

(test gtk-multi-selection-properties
  (let ((selection (make-instance 'gtk:multi-selection)))
    (is (eq (g:gtype "GObject") (gtk:multi-selection-item-type selection)))
    (is-false (gtk:multi-selection-model selection))
    (is (= 0 (gtk:multi-selection-n-items selection)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_multi_selection_new

(test gtk-multi-selection-new
  (let* ((model (create-string-list-for-package "GSK"))
         (selection nil))
    (is (typep (gtk:multi-selection-new) 'gtk:multi-selection))
    (is (typep (setf selection
                     (gtk:multi-selection-new model)) 'gtk:multi-selection))
    ;; Check memory management
    (is-false (setf (gtk:multi-selection-model selection) nil))
    (is (= 1 (g:object-ref-count model)))
    (is (= 1 (g:object-ref-count selection)))))

;;; 2024-12-2
