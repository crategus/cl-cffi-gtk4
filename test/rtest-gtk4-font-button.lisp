(in-package :gtk-test)

(def-suite gtk-font-button :in gtk-suite)
(in-suite gtk-font-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFontButton

(test gtk-font-button-class
  ;; Check type
  (is (g:type-is-object "GtkFontButton"))
  ;; Check registered name
  (is (eq 'gtk:font-button
          (glib:symbol-for-gtype "GtkFontButton")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkFontButton")
          (g:gtype (cffi:foreign-funcall "gtk_font_button_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkFontButton")))
  ;; Check children
  (is (equal '()
             (list-children "GtkFontButton")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkFontChooser")
             (list-interfaces "GtkFontButton")))
  ;; Check properties
  (is (equal '("font" "font-desc" "font-features" "language" "level" "modal"
               "preview-text" "show-preview-entry" "title" "use-font"
               "use-size")
             (list-properties "GtkFontButton")))
  ;; Check signals
  (is (equal '("activate" "font-set")
             (list-signals "GtkFontButton")))
  ;; Check CSS name
  (is (string= "fontbutton"
               (gtk:widget-class-css-name "GtkFontButton")))
  ;; Check accessible role
  (is (eq :widget (gtk:widget-class-accessible-role "GtkFontButton")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkFontButton" GTK-FONT-BUTTON
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkBuildable"
                                 "GtkConstraintTarget" "GtkFontChooser")
                                :TYPE-INITIALIZER "gtk_font_button_get_type")
                               ((MODAL GTK-FONT-BUTTON-MODAL "modal" "gboolean"
                                 T T)
                                (TITLE GTK-FONT-BUTTON-TITLE "title"
                                 "gchararray" T T)
                                (USE-FONT GTK-FONT-BUTTON-USE-FONT "use-font"
                                 "gboolean" T T)
                                (USE-SIZE GTK-FONT-BUTTON-USE-SIZE "use-size"
                                 "gboolean" T T)))
             (gobject:get-g-type-definition "GtkFontButton"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-font-button-properties
  (let* ((gtk-init:*gtk-warn-deprecated* nil)
         (button (make-instance 'gtk:font-button)))
    (is-true (gtk:font-button-modal button))
    #-windows
    (is (string= "Wählen Sie eine Schrift" (gtk:font-button-title button)))
    #+windows
    (is (string= "Eine Schrift wählen" (gtk:font-button-title button)))
    (is-false (gtk:font-button-use-font button))
    (is-false (gtk:font-button-use-size button))))

;;; --- Signals ----------------------------------------------------------------

;;;     font-set

(test gtk-font-button-font-set-signal
  (let* ((name "font-set") (gtype "GtkFontButton")
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (string= gtype (g:type-name (g:signal-query-owner-type query))))
    ;; Check flags
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_font_button_new

(test gtk-font-button-new
  (let* ((gtk-init:*gtk-warn-deprecated* nil))
    (is (typep (gtk:font-button-new) 'gtk:font-button))))

;;;     gtk_font_button_new_with_font

(test gtk-font-button-new-with-font
  (let* ((gtk-init:*gtk-warn-deprecated* nil)
         (button (gtk:font-button-new-with-font "Sans Italic 12")))
    (is-true (gtk:font-button-modal button))
    #-windows
    (is (string= "Wählen Sie eine Schrift" (gtk:font-button-title button)))
    #+windows
    (is (string= "Eine Schrift wählen" (gtk:font-button-title button)))
    (is-false (gtk:font-button-use-font button))
    (is-false (gtk:font-button-use-size button))

    (is (string= "Sans 12" (gtk:font-chooser-font button)))
    (is (typep (gtk:font-chooser-font-desc button) 'pango:font-description))
    (is-false (gtk:font-chooser-font-features button))
    (is (string= "de-de" (gtk:font-chooser-language button)))
    (is (equal '(:style :size) (gtk:font-chooser-level button)))
    (is-false (gtk:font-chooser-preview-text button))
    (is-true (gtk:font-chooser-show-preview-entry button))))

;;; 2024-6-1
