(in-package :gtk-test)

(def-suite gtk-switch :in gtk-suite)
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
             (list-children "GtkSwitch")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkActionable")
             (list-interfaces "GtkSwitch")))
  ;; Check properties
  (is (equal '("action-name" "action-target" "active" "state")
             (list-properties "GtkSwitch")))
  ;; Check signals
  (is (equal '("activate" "state-set")
             (list-signals "GtkSwitch")))
  ;; Check CSS information
  (is (string= "switch"
               (gtk:widget-class-css-name "GtkSwitch")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkSwitch" GTK-SWITCH
                       (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkActionable" "GtkBuildable"
                         "GtkConstraintTarget")
                        :TYPE-INITIALIZER "gtk_switch_get_type")
                       ((ACTIVE GTK-SWITCH-ACTIVE "active" "gboolean" T T)
                        (STATE GTK-SWITCH-STATE "state" "gboolean" T T)))
             (gobject:get-g-type-definition "GtkSwitch"))))

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
  (let ((query (g:signal-query (g:signal-lookup "activate" "GtkSwitch"))))
    (is (string= "activate" (g:signal-query-signal-name query)))
    (is (string= "GtkSwitch" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:ACTION :RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;;     state-set

(test gtk-switch-state-set-signal
  (let ((query (g:signal-query (g:signal-lookup "state-set" "GtkSwitch"))))
    (is (string= "state-set" (g:signal-query-signal-name query)))
    (is (string= "GtkSwitch" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "gboolean" (g:type-name (g:signal-query-return-type query))))
    (is (equal '("gboolean")
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_switch_new

(test gtk-switch-new
  (is (typep (gtk:switch-new) 'gtk:switch)))

;;; 2024-5-7
