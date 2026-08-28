(in-package :gtk-test)

(def-suite gtk-check-button :in gtk-buttons-and-toggles)
(in-suite gtk-check-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCheckButton

(test gtk-check-button-class
  ;; Check type
  (is (g:type-is-object "GtkCheckButton"))
  ;; Check registered name
  (is (eq 'gtk:check-button
          (glib:symbol-for-gtype "GtkCheckButton")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkCheckButton")
          (g:gtype (cffi:foreign-funcall "gtk_check_button_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkCheckButton")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkCheckButton")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkActionable")
             (glib-test:list-interfaces "GtkCheckButton")))
  ;; Check properties
  (is (equal '("action-name" "action-target" "active" "child" "group"
               "inconsistent" "label" "use-underline")
             (glib-test:list-properties "GtkCheckButton")))
  ;; Check signals
  (is (equal '("activate" "toggled")
             (glib-test:list-signals "GtkCheckButton")))
  ;; Check CSS information
  (is (string= "checkbutton"
               (gtk:widget-class-css-name "GtkCheckButton")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkCheckButton" GTK:CHECK-BUTTON
                      (:SUPERCLASS GTK:WIDGET
                       :EXPORT T
                       :INTERFACES
                       ("GtkAccessible" "GtkActionable" "GtkBuildable"
                        "GtkConstraintTarget")
                       :TYPE-INITIALIZER "gtk_check_button_get_type")
                      ((ACTIVE CHECK-BUTTON-ACTIVE "active" "gboolean" T T)
                       (CHILD CHECK-BUTTON-CHILD "child" "GtkWidget" T T)
                       (GROUP CHECK-BUTTON-GROUP
                        "group" "GtkCheckButton" NIL T)
                       (INCONSISTENT CHECK-BUTTON-INCONSISTENT
                        "inconsistent" "gboolean" T T)
                       (LABEL CHECK-BUTTON-LABEL "label" "gchararray" T T)
                       (USE-UNDERLINE CHECK-BUTTON-USE-UNDERLINE
                        "use-underline" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkCheckButton"))))

;;; --- Signals ----------------------------------------------------------------

;;;     activate

(test gtk-check-button-activate-signal
  (let* ((name "activate")
         (gtype (g:gtype "GtkCheckButton"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:ACTION :RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     toggled

(test gtk-check-button-toggled-signal
  (let* ((name "toggled")
         (gtype (g:gtype "GtkCheckButton"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Properties -------------------------------------------------------------

(test gtk-check-button-properties
  (let ((group (make-instance 'gtk:check-button))
        (button (make-instance 'gtk:check-button)))
    ;; active
    (is-true (setf (gtk:check-button-active button) t))
    (is-true (gtk:check-button-active button))
    (is (typep (setf (gtk:check-button-child button)
                     (gtk:button-new)) 'gtk:button))
    (is (typep (gtk:check-button-child button) 'gtk:button))
    ;; group
    (is (typep (setf (gtk:check-button-group button) group) 'gtk:check-button))
    ;; group is not readable
    (signals (error) (gtk:check-button-group button))
    ;; inconsistent
    (is-true (setf (gtk:check-button-inconsistent button) t))
    (is-true (gtk:check-button-inconsistent button))
    ;; label
    (is (string= "label" (setf (gtk:check-button-label button) "label")))
    (is (string= "label" (gtk:check-button-label button)))
    ;; use-underline
    (is-true (setf (gtk:check-button-use-underline button) t))
    (is-true (gtk:check-button-use-underline button))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_check_button_new
;;;     gtk_check_button_new_with_label
;;;     gtk_check_button_new_with_mnemonic

(test gtk-check-button-new
  (let (button)
    (is (typep (setf button (gtk:check-button-new)) 'gtk:check-button))
    (is-false (gtk:check-button-label button))
    (is (typep (setf button (gtk:check-button-new-with-label "label"))
               'gtk:check-button))
    (is (string= "label" (gtk:check-button-label button)))
    (is (typep (setf button (gtk:check-button-new-with-mnemonic "_label"))
               'gtk:check-button))
    (is (string= "_label" (gtk:check-button-label button)))
    (is-true (gtk:check-button-use-underline button))))

;;; 2026-07-24
