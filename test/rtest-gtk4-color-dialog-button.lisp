(in-package :gtk-test)

(def-suite gtk-color-dialog-button :in gtk-suite)
(in-suite gtk-color-dialog-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkColorDialogButton

(test gtk-color-dialog-button-class
  ;; Check type
  (is (g:type-is-object "GtkColorDialogButton"))
  ;; Check registered name
  (is (eq 'gtk:color-dialog-button
          (glib:symbol-for-gtype "GtkColorDialogButton")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkColorDialogButton")
          (g:gtype (cffi:foreign-funcall "gtk_color_dialog_button_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkColorDialogButton")))
  ;; Check children
  (is (equal '()
             (gtk-test:list-children "GtkColorDialogButton")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (gtk-test:list-interfaces "GtkColorDialogButton")))
  ;; Check properties
  (is (equal '("dialog" "rgba")
             (gtk-test:list-properties "GtkColorDialogButton")))
  ;; Check signals
  (is (equal '("activate")
             (gtk-test:list-signals "GtkColorDialogButton")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkColorDialogButton"
                                             GTK-COLOR-DIALOG-BUTTON
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkBuildable"
                                 "GtkConstraintTarget")
                                :TYPE-INITIALIZER
                                "gtk_color_dialog_button_get_type")
                               ((DIALOG GTK-COLOR-DIALOG-BUTTON-DIALOG "dialog"
                                 "GtkColorDialog" T T)
                                (RGBA GTK-COLOR-DIALOG-BUTTON-RGBA "rgba"
                                 "GdkRGBA" T T)))
             (gobject:get-g-type-definition "GtkColorDialogButton"))))

;;; --- Properties -------------------------------------------------------------

;;;     dialog
;;;     rgba

(test gtk-color-dialog-button-properties
  (let ((button (make-instance 'gtk:color-dialog-button)))
    (is-false (gtk:color-dialog-button-dialog button))
    (is (typep (gtk:color-dialog-button-rgba button) 'gdk:rgba))))

;;; --- Signals ----------------------------------------------------------------

;;;     activate

(test gtk-color-dialog-button-activate-signal
  (let ((query (g:signal-query (g:signal-lookup "activate"
                                                "GtkColorDialogButton"))))
    (is (string= "activate" (g:signal-query-signal-name query)))
    (is (string= "GtkColorDialogButton"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:ACTION :RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_color_dialog_button_new

(test gtk-color-dialog-button-new
  (is (typep (gtk:color-dialog-button-new) 'gtk:color-dialog-button))
  (let ((dialog (make-instance 'gtk:color-dialog)))
    (is (typep (gtk:color-dialog-button-new dialog) 'gtk:color-dialog-button))))

;;; 2024-7-4
