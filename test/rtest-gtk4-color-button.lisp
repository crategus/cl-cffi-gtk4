(in-package :gtk-test)

(def-suite gtk-color-button :in gtk-suite)
(in-suite gtk-color-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkColorButton

(test color-button-class
  ;; Type check
  (is (g:type-is-object "GtkColorButton"))
  ;; Check the registered name
  (is (eq 'gtk:color-button
          (glib:symbol-for-gtype "GtkColorButton")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkColorButton")
          (g:gtype (cfi:foreign-funcall "gtk_color_button_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkColorButton")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkColorButton")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkColorChooser")
             (list-interfaces "GtkColorButton")))
  ;; Check the class properties
  ;; RGBA and USE-ALPHA are inherited from the GtkColorChooser interface
  (is (equal '("modal" "rgba" "show-editor" "title" "use-alpha")
             (list-properties "GtkColorButton")))
  ;; Check the list of signals
  (is (equal '("activate" "color-set")
             (list-signals "GtkColorButton")))
  ;; CSS information
  (is (string= "colorbutton"
               (gtk:widget-class-css-name "GtkColorButton")))
  (is (string=
"colorbutton:dir(ltr)
  button.color:dir(ltr)
    colorswatch:dir(ltr)
      overlay:dir(ltr)
"
               (gtk:style-context-to-string
                   (gtk:widget-style-context (make-instance 'gtk:color-button))
                   :none)))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkColorButton" GTK-COLOR-BUTTON
                       (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkColorChooser"
                         "GtkConstraintTarget")
                        :TYPE-INITIALIZER "gtk_color_button_get_type")
                       ((MODAL GTK-COLOR-BUTTON-MODAL "modal" "gboolean" T T)
                        (SHOW-EDITOR GTK-COLOR-BUTTON-SHOW-EDITOR "show-editor"
                         "gboolean" T T)
                        (TITLE GTK-COLOR-BUTTON-TITLE "title" "gchararray" T
                         T)))
             (gobject:get-g-type-definition "GtkColorButton"))))

;;; --- Properties -------------------------------------------------------------

(test color-button-properties
  (let ((button (make-instance 'gtk:color-button)))
    (is-true (gtk:color-button-modal button))
    (is-false (setf (gtk:color-button-modal button) nil))
    (is-false (gtk:color-button-show-editor button))
    (is-true (setf (gtk:color-button-show-editor button) t))
    (is (string= "Pick a Color" (gtk:color-button-title button)))
    (is (string= "Wähle eine Farbe"
                 (setf (gtk:color-button-title button) "Wähle eine Farbe")))))

;;; --- Signals ----------------------------------------------------------------

;;;     activate                                           Since 4.4

(test color-button-activate-signal
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

(test color-button-color-set-signal
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

(test color-button-new
  (is (typep (gtk:color-button-new) 'gtk:color-button)))

;;;     gtk_color_button_new_with_rgba

(test color-button-new-with-rgba
  (let ((button (gtk:color-button-new-with-rgba (gdk:rgba-parse "Blue"))))
    (is (typep button 'gtk:color-button))
    (is (string= "rgb(0,0,255)"
                 (gdk:rgba-to-string (gtk:color-chooser-rgba button))))
    (is-false (gtk:color-chooser-use-alpha button))))

;;; --- 2023-5-29 --------------------------------------------------------------
