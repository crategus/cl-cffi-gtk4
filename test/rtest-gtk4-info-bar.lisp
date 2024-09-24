(in-package :gtk-test)

(def-suite gtk-info-bar :in gtk-suite)
(in-suite gtk-info-bar)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkInfoBar

(test gtk-info-bar-class
  ;; Check type
  (is (g:type-is-object "GtkInfoBar"))
  ;; Check registered name
  (is (eq 'gtk:info-bar
          (glib:symbol-for-gtype "GtkInfoBar")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkInfoBar")
          (g:gtype (cffi:foreign-funcall "gtk_info_bar_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkInfoBar")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkInfoBar")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (glib-test:list-interfaces "GtkInfoBar")))
  ;; Check properties
  (is (equal '("message-type" "revealed" "show-close-button")
             (glib-test:list-properties "GtkInfoBar")))
  ;; Check signals
  (is (equal '("close" "response")
             (glib-test:list-signals "GtkInfoBar")))
  ;; Check CSS name
  (is (string= "infobar"
               (gtk:widget-class-css-name "GtkInfoBar")))
  ;; Check accessible role
  (is (eq :widget (gtk:widget-class-accessible-role "GtkInfoBar")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkInfoBar" GTK:INFO-BAR
                      (:SUPERCLASS GTK:WIDGET
                       :EXPORT T
                       :INTERFACES
                       ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
                       :TYPE-INITIALIZER "gtk_info_bar_get_type")
                      ((MESSAGE-TYPE INFO-BAR-MESSAGE-TYPE
                        "message-type" "GtkMessageType" T T)
                       (REVEALED INFO-BAR-REVEALED "revealed" "gboolean" T T)
                       (SHOW-CLOSE-BUTTON INFO-BAR-SHOW-CLOSE-BUTTON
                        "show-close-button" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkInfoBar"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-info-bar-properties
  (let* ((gtk-init:*gtk-warn-deprecated* nil)
         (infobar (make-instance 'gtk:info-bar)))
    (is (eq :info (gtk:info-bar-message-type infobar)))
    (is-true (gtk:info-bar-revealed infobar))
    (is-false (gtk:info-bar-show-close-button infobar))))

;;; --- Signals ----------------------------------------------------------------

;;;     close
;;;     response

;;; --- Functions --------------------------------------------------------------

;;;     gtk_info_bar_new

(test gtk-info-bar-new
  (let* ((gtk-init:*gtk-warn-deprecated* nil))
    (is (typep (gtk:info-bar-new) 'gtk:info-bar))))

;;;     gtk_info_bar_new_with_buttons

(test gtk-info-bar-new-with-buttons
  (let* ((gtk-init:*gtk-warn-deprecated* nil))
    (is (typep (gtk:info-bar-new-with-buttons "_OK" :accept
                                              "_Cancel" :reject
                                              "_New" 100
                                              "_Old" 200)
               'gtk:info-bar))))

;;;     gtk_info_bar_add_action_widget
;;;     gtk_info_bar_remove_action_widget
;;;     gtk_info_bar_add_button
;;;     gtk_info_bar_add_buttons
;;;     gtk_info_bar_set_response_sensitive
;;;     gtk_info_bar_set_default_response
;;;     gtk_info_bar_response
;;;     gtk_info_bar_add_child
;;;     gtk_info_bar_remove_child

;;; 2024-9-20
