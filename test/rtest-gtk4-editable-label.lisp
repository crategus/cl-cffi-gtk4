(in-package :gtk-test)

(def-suite gtk-editable-label :in gtk-suite)
(in-suite gtk-editable-label)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkEditableLabel

(test gtk-editable-label-class
  ;; Check type
  (is (g:type-is-object "GtkEditableLabel"))
  ;; Check registered name
  (is (eq 'gtk:editable-label
          (glib:symbol-for-gtype "GtkEditableLabel")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkEditableLabel")
          (g:gtype (cffi:foreign-funcall "gtk_editable_label_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkEditableLabel")))
  ;; Check children
  (is (equal '()
             (list-children "GtkEditableLabel")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkEditable")
             (list-interfaces "GtkEditableLabel")))
  ;; Check properties
  (is (equal '("cursor-position" "editable" "editing" "enable-undo"
               "max-width-chars" "selection-bound" "text" "width-chars"
               "xalign")
             (list-properties "GtkEditableLabel")))
  ;; Check signals
  (is (equal '()
             (list-signals "GtkEditableLabel")))
  ;; Check CSS name
  (is (string= "editablelabel"
               (gtk:widget-class-css-name "GtkEditableLabel")))
  ;; Check accessible role
  (is (eq :widget (gtk:widget-class-accessible-role "GtkEditableLabel")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkEditableLabel" GTK-EDITABLE-LABEL
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkBuildable"
                                 "GtkConstraintTarget" "GtkEditable")
                                :TYPE-INITIALIZER
                                "gtk_editable_label_get_type")
                               ((EDITING GTK-EDITABLE-LABEL-EDITING "editing"
                                 "gboolean" T T)))
             (gobject:get-g-type-definition "GtkEditableLabel"))))

;;; --- Properties -------------------------------------------------------------

;;;     editing

(test gtk-editable-label-properties
  (let ((label (make-instance 'gtk:editable-label)))
    (is-false (gtk:editable-label-editing label))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_editable_label_new

(test gtk-editable-label-new
  (is (typep (gtk:editable-label-new "label") 'gtk:editable-label)))

;;;     gtk_editable_label_start_editing
;;;     gtk_editable_label_stop_editing

;;; 2024-5-23
