(in-package :gtk-test)

(def-suite gtk-color-button :in gtk-deprecated)
(in-suite gtk-color-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkColorButton

(test gtk-color-button-class
  ;; Check type
  (is (g:type-is-object "GtkColorButton"))
  ;; Check registered name
  (is (eq 'gtk:color-button
          (glib:symbol-for-gtype "GtkColorButton")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkColorButton")
          (g:gtype (cffi:foreign-funcall "gtk_color_button_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkColorButton")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkColorButton")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkColorChooser")
             (glib-test:list-interfaces "GtkColorButton")))
  ;; Check properties
  ;; RGBA and USE-ALPHA are inherited from the GtkColorChooser interface
  (is (equal '("modal" "rgba" "show-editor" "title" "use-alpha")
             (glib-test:list-properties "GtkColorButton")))
  ;; Check signals
  (is (equal '("activate" "color-set")
             (glib-test:list-signals "GtkColorButton")))
  ;; Check CSS information
  (is (string= "colorbutton"
               (gtk:widget-class-css-name "GtkColorButton")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkColorButton" GTK:COLOR-BUTTON
                       (:SUPERCLASS GTK:WIDGET
                        :EXPORT T
                        :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkColorChooser"
                         "GtkConstraintTarget")
                        :TYPE-INITIALIZER "gtk_color_button_get_type")
                       ((MODAL COLOR-BUTTON-MODAL "modal" "gboolean" T T)
                        (SHOW-EDITOR COLOR-BUTTON-SHOW-EDITOR
                         "show-editor" "gboolean" T T)
                        (TITLE COLOR-BUTTON-TITLE "title" "gchararray" T T)))
             (gobject:get-gtype-definition "GtkColorButton"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-color-button-properties
  (let* ((gtk-init:*warn-deprecated* nil)
         (button (make-instance 'gtk:color-button)))
    (is-true (gtk:color-button-modal button))
    (is-false (setf (gtk:color-button-modal button) nil))
    (is-false (gtk:color-button-show-editor button))
    (is-true (setf (gtk:color-button-show-editor button) t))
    #-windows
    (is (string= "Wählen Sie eine Farbe" (gtk:color-button-title button)))
    #+windows
    (is (string= "Eine Farbe wählen" (gtk:color-button-title button)))
    (is (string= "Wähle eine Farbe"
                 (setf (gtk:color-button-title button) "Wähle eine Farbe")))))

;;; --- Signals ----------------------------------------------------------------

;;;     activate                                           Since 4.4

(test gtk-color-button-activate-signal
  (let ((query (g:signal-query (g:signal-lookup "activate" "GtkColorButton"))))
    (is (string= "activate" (g:signal-query-signal-name query)))
    (is (string= "GtkColorButton" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:ACTION :RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;;     color-set

(test gtk-color-button-color-set-signal
  (let ((query (g:signal-query (g:signal-lookup "color-set" "GtkColorButton"))))
    (is (string= "color-set" (g:signal-query-signal-name query)))
    (is (string= "GtkColorButton" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_color_button_new

(test gtk-color-button-new
  (let ((gtk-init:*warn-deprecated* nil))
    (is (typep (gtk:color-button-new) 'gtk:color-button))))

;;;     gtk_color_button_new_with_rgba

(test gtk-color-button-new-with-rgba
  (let* ((gtk-init:*warn-deprecated* nil)
         (button (gtk:color-button-new-with-rgba (gdk:rgba-parse "Blue"))))
    (is (typep button 'gtk:color-button))
    (is (string= "rgb(0,0,255)"
                 (gdk:rgba-to-string (gtk:color-chooser-rgba button))))
    (is-false (gtk:color-chooser-use-alpha button))))

;;; 2024-9-20
