(in-package :gtk-test)

(def-suite gtk-no-selection :in gtk-list-model-support)
(in-suite gtk-no-selection)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkNoSelection

(test gtk-no-selection-class
  ;; Check type
  (is (g:type-is-object "GtkNoSelection"))
  ;; Check registered name
  (is (eq 'gtk:no-selection
          (glib:symbol-for-gtype "GtkNoSelection")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkNoSelection")
          (g:gtype (cffi:foreign-funcall "gtk_no_selection_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkNoSelection")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkNoSelection")))
  ;; Check interfaces
  (is (equal '("GListModel" "GtkSectionModel" "GtkSelectionModel")
             (glib-test:list-interfaces "GtkNoSelection")))
  ;; Check properties
  (is (equal '("item-type" "model" "n-items")
             (glib-test:list-properties "GtkNoSelection")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkNoSelection")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkNoSelection" GTK:NO-SELECTION
                       (:SUPERCLASS GOBJECT:OBJECT
                        :EXPORT T
                        :INTERFACES
                        ("GListModel" "GtkSectionModel" "GtkSelectionModel")
                        :TYPE-INITIALIZER "gtk_no_selection_get_type")
                       ((ITEM-TYPE NO-SELECTION-ITEM-TYPE
                         "item-type" "GType" T NIL)
                        (MODEL NO-SELECTION-MODEL "model" "GListModel" T T)
                        (N-ITEMS NO-SELECTION-N-ITEMS "n-items" "guint" T NIL)))
             (gobject:get-gtype-definition "GtkNoSelection"))))

;;; --- Properties -------------------------------------------------------------

;;;     item-type                                          Since 4.8
;;;     model
;;;     n-items                                            Since 4.8

(test gtk-no-selection-properties
  (glib-test:with-check-memory (selection)
    (setf selection (make-instance 'gtk:no-selection))
    (is (eq (g:gtype "GObject") (gtk:no-selection-item-type selection)))
    (is-false (gtk:no-selection-model selection))
    (is (= 0 (gtk:no-selection-n-items selection)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_no_selection_new

(test gtk-no-selection-new
  (glib-test:with-check-memory (model selection)
    (setf model (g:list-store-new "GtkWidget"))
    (is (typep (setf selection
                     (gtk:no-selection-new)) 'gtk:no-selection))
    (is (typep (setf selection
                     (gtk:no-selection-new nil)) 'gtk:no-selection))
    (is (typep (setf selection
                     (gtk:no-selection-new model))
               'gtk:no-selection))
    ;; Remove references
    (is-false (setf (gtk:no-selection-model selection) nil))))

;;; 2024-12-17
