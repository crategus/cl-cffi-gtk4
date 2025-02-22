(in-package :gtk-test)

(def-suite gtk-link-button :in gtk-buttons-and-toggles)
(in-suite gtk-link-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkLinkButton

(test gtk-link-button-class
  ;; Check type
  (is (g:type-is-object "GtkLinkButton"))
  ;; Check registered name
  (is (eq 'gtk:link-button
          (glib:symbol-for-gtype "GtkLinkButton")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkLinkButton")
          (g:gtype (cffi:foreign-funcall "gtk_link_button_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkButton")
          (g:type-parent "GtkLinkButton")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkLinkButton")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkActionable")
             (glib-test:list-interfaces "GtkLinkButton")))
  ;; Check properties
  (is (equal '("uri" "visited")
             (glib-test:list-properties "GtkLinkButton")))
  ;; Check signals
  (is (equal '("activate-link")
             (glib-test:list-signals "GtkLinkButton")))
  ;; Check CSS name
  (is (string= "button"
               (gtk:widget-class-css-name "GtkLinkButton")))
  ;; Check accessible role
  (is (eq :link (gtk:widget-class-accessible-role "GtkLinkButton")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkLinkButton" GTK:LINK-BUTTON
                      (:SUPERCLASS GTK:BUTTON
                       :EXPORT T
                       :INTERFACES
                       ("GtkAccessible" "GtkActionable" "GtkBuildable"
                        "GtkConstraintTarget")
                       :TYPE-INITIALIZER "gtk_link_button_get_type")
                      ((URI LINK-BUTTON-URI "uri" "gchararray" T T)
                       (VISITED LINK-BUTTON-VISITED "visited" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkLinkButton"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-link-button-properties
  (let ((button (make-instance 'gtk:link-button)))
    (is-false (gtk:link-button-uri button))
    (is-false (gtk:link-button-visited button))))

;;; --- Signals ----------------------------------------------------------------

(test gtk-link-button-activate-link-signal
  (let* ((name "activate-link")
         (gtype (g:gtype "GtkLinkButton"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= "activate-link" (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "gboolean") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

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

;;; 2025-2-22
