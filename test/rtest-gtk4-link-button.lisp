(in-package :gtk-test)

(def-suite gtk-link-button :in gtk-suite)
(in-suite gtk-link-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkLinkButton

(test link-button-class
  ;; Type check
  (is (g:type-is-object "GtkLinkButton"))
  ;; Check the registered name
  (is (eq 'gtk:link-button
          (gobject:symbol-for-gtype "GtkLinkButton")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkLinkButton")
          (g:gtype (cffi:foreign-funcall "gtk_link_button_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkButton")
          (g:type-parent "GtkLinkButton")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkLinkButton")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkActionable")
             (list-interfaces "GtkLinkButton")))
  ;; Check the properties
  (is (equal '("uri" "visited")
             (list-properties "GtkLinkButton")))
  ;; Check the signals
  (is (equal '("activate-link")
             (list-signals "GtkLinkButton")))
  ;; CSS information
  (is (string= "button"
               (gtk:widget-class-css-name "GtkLinkButton")))
  (is (string=
"button.flat.link:dir(ltr):link
"
               (gtk:style-context-to-string
                   (gtk:widget-style-context (make-instance 'gtk:link-button))
                   :none)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkLinkButton" GTK-LINK-BUTTON
                       (:SUPERCLASS GTK-BUTTON :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkActionable" "GtkBuildable"
                         "GtkConstraintTarget")
                        :TYPE-INITIALIZER "gtk_link_button_get_type")
                       ((URI GTK-LINK-BUTTON-URI "uri" "gchararray" T T)
                        (VISITED GTK-LINK-BUTTON-VISITED "visited" "gboolean" T
                         T)))
             (gobject:get-g-type-definition "GtkLinkButton"))))

;;; --- Properties -------------------------------------------------------------

;;;     uri
;;;     visited

;;; --- Signals ----------------------------------------------------------------

;;;     activate-link

;;; --- Functions --------------------------------------------------------------

;;;     gtk_link_button_new
;;;     gtk_link_button_new_with_label

;;; --- 2023-3-18 --------------------------------------------------------------
