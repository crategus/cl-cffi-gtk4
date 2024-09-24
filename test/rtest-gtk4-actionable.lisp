(in-package :gtk-test)

(def-suite gtk-actionable :in gtk-suite)
(in-suite gtk-actionable)

;;; --- GtkActionable ----------------------------------------------------------

(test gtk-actionable-interface
  ;; Check type
  (is (g:type-is-interface "GtkActionable"))
  ;; Check registered name
  (is (eq 'gtk:actionable
          (glib:symbol-for-gtype "GtkActionable")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkActionable")
          (g:gtype (cffi:foreign-funcall "gtk_actionable_get_type" :size))))
  ;; Check interface prerequisites
  (is (equal '("GtkWidget")
             (glib-test:list-interface-prerequisites "GtkActionable")))
  ;; Check interface properties
  (is (equal '("action-name" "action-target")
             (glib-test:list-interface-properties "GtkActionable")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "GtkActionable" GTK:ACTIONABLE
                      (:EXPORT T
                       :TYPE-INITIALIZER "gtk_actionable_get_type")
                      (ACTION-NAME ACTIONABLE-ACTION-NAME
                       "action-name" "gchararray" T T)
                      (ACTION-TARGET ACTIONABLE-ACTION-TARGET
                       "action-target" "GVariant" T T))
             (gobject:get-gtype-definition "GtkActionable"))))

;;; --- Properties and Accessors -----------------------------------------------

(test gtk-actionable-properties
  (let ((button (make-instance 'gtk:button)))
    ;; Default is false
    (is-false (gtk:actionable-action-name button))
    ;; Default is null-pointer
    (is-true (cffi:null-pointer-p (gtk:actionable-action-target button)))
    ;; Set the name and the target
    (gtk:actionable-set-detailed-action-name button "win.justify::left")
    (is (string= "win.justify"
                 (gtk:actionable-action-name button)))
    (is (string= "left"
                 (g:variant-string (gtk:actionable-action-target button))))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_actionable_set_action_target

(test gtk-actionable-action-target
  (let ((button (make-instance 'gtk:button)))
    (is (cffi:null-pointer-p (gtk:actionable-action-target button)))
    (setf (gtk:actionable-action-target button) (g:variant-new-int16 128))
    (is (= 128 (g:variant-int16 (gtk:actionable-action-target button))))
    (setf (gtk:actionable-action-target button) (cffi:null-pointer))
    (is (cffi:null-pointer-p (gtk:actionable-action-target button)))))

;;;     gtk_actionable_set_detailed_action_name

;;; See gtk-actionable-properties for an example

;;; 2024-9-19
