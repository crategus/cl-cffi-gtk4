(in-package :gtk-test)

(def-suite gtk-no-selection :in gtk-suite)
(in-suite gtk-no-selection)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkNoSelection

(test gtk-no-selection-class
  ;; Type check
  (is (g:type-is-object "GtkNoSelection"))
  ;; Check the registered name
  (is (eq 'gtk:no-selection
          (glib:symbol-for-gtype "GtkNoSelection")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkNoSelection")
          (g:gtype (cffi:foreign-funcall "gtk_no_selection_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkNoSelection")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkNoSelection")))
  ;; Check the interfaces
  (is (equal '("GListModel" "GtkSelectionModel")
             (list-interfaces "GtkNoSelection")))
  ;; Check the properties
  (is (equal '("item-type" "model" "n-items")
             (list-properties "GtkNoSelection")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkNoSelection")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkNoSelection" GTK-NO-SELECTION
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                                ("GListModel" "GtkSelectionModel")
                                :TYPE-INITIALIZER "gtk_no_selection_get_type")
                               ((ITEM-TYPE GTK-NO-SELECTION-ITEM-TYPE
                                 "item-type" "GType" T NIL)
                                (MODEL GTK-NO-SELECTION-MODEL "model"
                                 "GListModel" T T)
                                (N-ITEMS GTK-NO-SELECTION-N-ITEMS "n-items"
                                 "guint" T NIL)))
             (gobject:get-g-type-definition "GtkNoSelection"))))

;;; --- Properties -------------------------------------------------------------

;;;     model

(test gtk-no-selection-properties
  (let ((selection (make-instance 'gtk:no-selection)))
    (is-false (gtk:no-selection-model selection))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_no_selection_new

(test gtk-no-selection-new
  (is (typep (gtk:no-selection-new nil) 'gtk:no-selection))
  (is (typep (gtk:no-selection-new (g:list-store-new "gint")) 
             'gtk:no-selection)))

;;; --- 2023-8-10 --------------------------------------------------------------
