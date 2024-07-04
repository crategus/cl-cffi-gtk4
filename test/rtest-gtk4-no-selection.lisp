(in-package :gtk-test)

(def-suite gtk-no-selection :in gtk-suite)
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
             (gtk-test:list-children "GtkNoSelection")))
  ;; Check interfaces
  (is (equal '("GListModel" "GtkSectionModel" "GtkSelectionModel")
             (gtk-test:list-interfaces "GtkNoSelection")))
  ;; Check properties
  (is (equal '("item-type" "model" "n-items")
             (gtk-test:list-properties "GtkNoSelection")))
  ;; Check signals
  (is (equal '()
             (gtk-test:list-signals "GtkNoSelection")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkNoSelection" GTK-NO-SELECTION
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                                ("GListModel" "GtkSectionModel"
                                 "GtkSelectionModel")
                                :TYPE-INITIALIZER "gtk_no_selection_get_type")
                               ((ITEM-TYPE GTK-NO-SELECTION-ITEM-TYPE
                                 "item-type" "GType" T NIL)
                                (MODEL GTK-NO-SELECTION-MODEL "model"
                                 "GListModel" T T)
                                (N-ITEMS GTK-NO-SELECTION-N-ITEMS "n-items"
                                 "guint" T NIL)))
             (gobject:get-g-type-definition "GtkNoSelection"))))

;;; --- Properties -------------------------------------------------------------

;;;     item-type                                          Since 4.8
;;;     model
;;;     n-items                                            Since 4.8

(test gtk-no-selection-properties
  (let ((selection (make-instance 'gtk:no-selection)))
    (is (eq (g:gtype "GObject") (gtk:no-selection-item-type selection)))
    (is-false (gtk:no-selection-model selection))
    (is (= 0 (gtk:no-selection-n-items selection)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_no_selection_new

(test gtk-no-selection-new
  (is (typep (gtk:no-selection-new nil) 'gtk:no-selection))
  (is (typep (gtk:no-selection-new (g:list-store-new "GtkWidget"))
             'gtk:no-selection)))

;;; 2024-7-4
