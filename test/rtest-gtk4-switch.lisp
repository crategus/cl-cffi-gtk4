(in-package :gtk-test)

(def-suite gtk-switch :in gtk-buttons-and-toggles)
(in-suite gtk-switch)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkSwitch

(test gtk-switch-class
  ;; Check type
  (is (g:type-is-object "GtkSwitch"))
  ;; Check registered name
  (is (eq 'gtk:switch
          (glib:symbol-for-gtype "GtkSwitch")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkSwitch")
          (g:gtype (cffi:foreign-funcall "gtk_switch_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkSwitch")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkSwitch")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkActionable")
             (glib-test:list-interfaces "GtkSwitch")))
  ;; Check properties
  (is (equal '("action-name" "action-target" "active" "state")
             (glib-test:list-properties "GtkSwitch")))
  ;; Check signals
  (is (equal '("activate" "state-set")
             (glib-test:list-signals "GtkSwitch")))
  ;; Check CSS information
  (is (string= "switch"
               (gtk:widget-class-css-name "GtkSwitch")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkSwitch" GTK:SWITCH
                      (:SUPERCLASS GTK:WIDGET
                       :EXPORT T
                       :INTERFACES
                       ("GtkAccessible" "GtkActionable" "GtkBuildable"
                        "GtkConstraintTarget")
                       :TYPE-INITIALIZER "gtk_switch_get_type")
                      ((ACTIVE SWITCH-ACTIVE "active" "gboolean" T T)
                       (STATE SWITCH-STATE "state" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkSwitch"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-switch-properties
  (let ((switch (make-instance 'gtk:switch)))
    (is-true (setf (gtk:switch-active switch) t))
    (is-true (gtk:switch-active switch))
    (is-true (setf (gtk:switch-state switch) t))
    (is-true (gtk:switch-active switch))))

;;; --- Signals ----------------------------------------------------------------

;;;     activate

(test gtk-switch-activate-signal
  (let* ((name "activate")
         (gtype (g:gtype "GtkSwitch"))
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

;;;     state-set

(test gtk-switch-state-set-signal
  (let* ((name "state-set")
         (gtype (g:gtype "GtkSwitch"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq (g:gtype "GtkSwitch") (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "gboolean") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("gboolean")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_switch_new

(test gtk-switch-new
  (is (typep (gtk:switch-new) 'gtk:switch)))

;;; 2025-2-22
