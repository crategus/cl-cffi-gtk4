(in-package :gtk-test)

(def-suite gtk-link-button :in gtk-suite)
(in-suite gtk-link-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkLinkButton

(test gtk-link-button-class
  ;; Type check
  (is (g:type-is-object "GtkLinkButton"))
  ;; Check the registered name
  (is (eq 'gtk:link-button
          (glib:symbol-for-gtype "GtkLinkButton")))
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
  ;; CSS name
  (is (string= "button"
               (gtk:widget-class-css-name "GtkLinkButton")))
  ;; CSS classes
  (is (equal '("flat" "link")
             (gtk:widget-css-classes (make-instance 'gtk:link-button))))
  ;; Accessible role
  (is (eq :link (gtk:widget-class-accessible-role "GtkLinkButton")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkLinkButton" GTK-LINK-BUTTON
                       (:SUPERCLASS GTK-BUTTON :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkActionable" "GtkBuildable"
                         "GtkConstraintTarget")
                        :TYPE-INITIALIZER "gtk_link_button_get_type")
                       ((URI GTK-LINK-BUTTON-URI "uri" "gchararray" T T)
                        (VISITED GTK-LINK-BUTTON-VISITED "visited" "gboolean" T
                         T)))
             (gobject:get-g-type-definition "GtkLinkButton"))))

;;; --- Properties -------------------------------------------------------------

(test gkt-link-button-properties
  (let ((button (make-instance 'gtk:link-button)))
    (is-false (gtk:link-button-uri button))
    (is-false (gtk:link-button-visited button))))


;;; --- Signals ----------------------------------------------------------------

;;;     activate-link

;;; --- Functions --------------------------------------------------------------

;;;     gtk_link_button_new

(test gtk-link-button-new
  (let ((button (gtk:link-button-new "http://crategus.com")))
    (is (string= "http://crategus.com" (gtk:link-button-uri button)))
    (is (string= "http://crategus.com" (gtk:button-label button)))))

;;;     gtk_link_button_new_with_label

(test gtk-link-button-new-with-label
  (let ((button (gtk:link-button-new-with-label "http://crategus.com" "Label")))
    (is (string= "http://crategus.com" (gtk:link-button-uri button)))
    (is (string= "Label" (gtk:button-label button)))))

;;; --- 2023-5-29 --------------------------------------------------------------
