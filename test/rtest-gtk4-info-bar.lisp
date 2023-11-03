(in-package :gtk-test)

(def-suite gtk-info-bar :in gtk-suite)
(in-suite gtk-info-bar)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkInfoBar

(test gtk-info-bar-class
  ;; Type check
  (is (g:type-is-object "GtkInfoBar"))
  ;; Check the registered name
  (is (eq 'gtk:info-bar
          (glib:symbol-for-gtype "GtkInfoBar")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkInfoBar")
          (g:gtype (cffi:foreign-funcall "gtk_info_bar_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkInfoBar")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkInfoBar")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (list-interfaces "GtkInfoBar")))
  ;; Check the properties
  (is (equal '("message-type" "revealed" "show-close-button")
             (list-properties "GtkInfoBar")))
  ;; Check the signals
  (is (equal '("close" "response")
             (list-signals "GtkInfoBar")))
  ;; CSS name
  (is (string= "infobar"
               (gtk:widget-class-css-name "GtkInfoBar")))
  ;; CSS classes
  (is (equal '("info")
             (gtk:widget-css-classes (make-instance 'gtk:info-bar))))
  ;; Accessible role
  (is (eq :widget (gtk:widget-class-accessible-role "GtkInfoBar")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkInfoBar" GTK-INFO-BAR
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkBuildable"
                                 "GtkConstraintTarget")
                                :TYPE-INITIALIZER "gtk_info_bar_get_type")
                               ((MESSAGE-TYPE GTK-INFO-BAR-MESSAGE-TYPE
                                 "message-type" "GtkMessageType" T T)
                                (REVEALED GTK-INFO-BAR-REVEALED "revealed"
                                 "gboolean" T T)
                                (SHOW-CLOSE-BUTTON
                                 GTK-INFO-BAR-SHOW-CLOSE-BUTTON
                                 "show-close-button" "gboolean" T T)))
             (gobject:get-g-type-definition "GtkInfoBar"))))

;;; --- Properties -------------------------------------------------------------

;;;     message-type
;;;     revealed
;;;     show-close-button

(test gtk-info-bar-properties
  (let ((infobar (make-instance 'gtk:info-bar)))
    (is (eq :info (gtk:info-bar-message-type infobar)))
    (is-true (gtk:info-bar-revealed infobar))
    (is-false (gtk:info-bar-show-close-button infobar))))

;;; --- Signals ----------------------------------------------------------------

;;;     close
;;;     response

;;; --- Functions --------------------------------------------------------------

;;;     gtk_info_bar_new

(test gtk-info-bar-new
  (is (typep (gtk:info-bar-new) 'gtk:info-bar)))

;;;     gtk_info_bar_new_with_buttons

(test gtk-info-bar-new-with-buttons
  (is (typep (gtk:info-bar-new-with-buttons "_OK" :accept
                                            "_Cancel" :reject
                                            "_New" 100
                                            "_Old" 200)
             'gtk:info-bar)))

;;;     gtk_info_bar_add_action_widget
;;;     gtk_info_bar_remove_action_widget
;;;     gtk_info_bar_add_button
;;;     gtk_info_bar_add_buttons
;;;     gtk_info_bar_set_response_sensitive
;;;     gtk_info_bar_set_default_response
;;;     gtk_info_bar_response
;;;     gtk_info_bar_add_child
;;;     gtk_info_bar_remove_child

;;; --- 2023-8-24 --------------------------------------------------------------
