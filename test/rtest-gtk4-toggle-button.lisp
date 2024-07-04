(in-package :gtk-test)

(def-suite gtk-toggle-button :in gtk-suite)
(in-suite gtk-toggle-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkToggleButton

(test toggle-button-class
  ;; Check type
  (is (g:type-is-object "GtkToggleButton"))
  ;; Check registered name
  (is (eq 'gtk:toggle-button
          (glib:symbol-for-gtype "GtkToggleButton")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkToggleButton")
          (g:gtype (cffi:foreign-funcall "gtk_toggle_button_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkButton")
          (g:type-parent "GtkToggleButton")))
  ;; Check children
  (is (equal '()
             (gtk-test:list-children "GtkToggleButton")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkActionable")
             (gtk-test:list-interfaces "GtkToggleButton")))
  ;; Check properties
  (is (equal '("active" "group")
             (gtk-test:list-properties "GtkToggleButton")))
  ;; Check signals
  (is (equal '("toggled")
             (gtk-test:list-signals "GtkToggleButton")))
  ;; Check CSS name
  (is (string= "button"
               (gtk:widget-class-css-name "GtkToggleButton")))
  ;; Check accessible role
  (is (eq :toggle-button (gtk:widget-class-accessible-role "GtkToggleButton")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkToggleButton" GTK-TOGGLE-BUTTON
                       (:SUPERCLASS GTK-BUTTON :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkActionable" "GtkBuildable"
                         "GtkConstraintTarget")
                        :TYPE-INITIALIZER "gtk_toggle_button_get_type")
                       ((ACTIVE GTK-TOGGLE-BUTTON-ACTIVE "active" "gboolean" T
                         T)
                        (GROUP GTK-TOGGLE-BUTTON-GROUP "group"
                         "GtkToggleButton" NIL T)))
             (gobject:get-g-type-definition "GtkToggleButton"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-toggle-button-properties
  (let ((button (make-instance 'gtk:toggle-button)))
    (is-false (gtk:toggle-button-active button))
    ;; GROUP is not readable
    (signals (error) (gtk:toggle-button-group button))
    (is (typep (setf (gtk:toggle-button-group button)
                     (make-instance 'gtk:toggle-button))
               'gtk:toggle-button))))

;;; --- Signals ----------------------------------------------------------------

(test gtk-toggle-button-toggled-signal
  (let ((query (g:signal-query (g:signal-lookup "toggled" "GtkToggleButton"))))
    (is (string= "toggled" (g:signal-query-signal-name query)))
    (is (string= "GtkToggleButton"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_toggle_button_new
;;;     gtk_toggle_button_new_with_label
;;;     gtk_toggle_button_new_with_mnemonic

(test gtk-toggle-button-new
  (let (button)
    (is (typep (setf button
                     (gtk:toggle-button-new)) 'gtk:toggle-button))
    (is-false (gtk:button-label button))
    (is-false (gtk:toggle-button-active button))

    (is (typep (setf button
                     (gtk:toggle-button-new-with-label "label"))
               'gtk:toggle-button))
    (is (string= "label"(gtk:button-label button)))
    (is-false (gtk:toggle-button-active button))

    (is (typep (setf button
                     (gtk:toggle-button-new-with-mnemonic "_label"))
               'gtk:toggle-button))
    (is (string= "_label" (gtk:button-label button)))
    (is-false (gtk:toggle-button-active button))))

;;;     gtk_toggle_button_toggled

(test gtk-toggle-button-toggled
  (let* ((gtk-init:*gtk-warn-deprecated* nil)
         (button (gtk:toggle-button-new))
         (msg nil))
    (g:signal-connect button "toggled"
                      (lambda (toggle)
                        (is (eq (g:gtype "GtkToggleButton")
                                (g:object-type toggle)))
                        (setf msg "Button TOGGLED")
                        gdk:+event-stop+))
    (is-false (gtk:toggle-button-active button))
    (is-false (gtk:toggle-button-toggled button))
    (is (string= "Button TOGGLED" msg))))

;;; 2024-5-4
