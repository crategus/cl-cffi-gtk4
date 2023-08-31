(in-package :gtk-test)

(def-suite gtk-window-controls :in gtk-suite)
(in-suite gtk-window-controls)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkWindowControls

(test gtk-window-controls-class
  ;; Type check
  (is (g:type-is-object "GtkWindowControls"))
  ;; Check the registered name
  (is (eq 'gtk:window-controls
          (glib:symbol-for-gtype "GtkWindowControls")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkWindowControls")
          (g:gtype (cffi:foreign-funcall "gtk_window_controls_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkWindowControls")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkWindowControls")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (list-interfaces "GtkWindowControls")))
  ;; Check the properties
  (is (equal '("decoration-layout" "empty" "side")
             (list-properties "GtkWindowControls")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkWindowControls")))
  ;; CSS name
  (is (string= "windowcontrols"
               (gtk:widget-class-css-name "GtkWindowControls")))
  ;; Accessible role
  (is (eq :group (gtk:widget-class-accessible-role "GtkWindowControls")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkWindowControls"
                                             GTK-WINDOW-CONTROLS
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkBuildable"
                                 "GtkConstraintTarget")
                                :TYPE-INITIALIZER
                                "gtk_window_controls_get_type")
                               ((DECORATION-LAYOUT
                                 GTK-WINDOW-CONTROLS-DECORATION-LAYOUT
                                 "decoration-layout" "gchararray" T T)
                                (EMPTY GTK-WINDOW-CONTROLS-EMPTY "empty"
                                 "gboolean" T NIL)
                                (SIDE GTK-WINDOW-CONTROLS-SIDE "side"
                                 "GtkPackType" T T)))
             (gobject:get-g-type-definition "GtkWindowControls"))))

;;; --- Properties -------------------------------------------------------------

;;;     decoration-layout
;;;     empty
;;;     side

(test gtk-window-controls-properties
  (let ((controls (make-instance 'gtk:window-controls)))
    (is-false (gtk:window-controls-decoration-layout controls))
    (is-true (gtk:window-controls-empty controls))
    (is (eq :start (gtk:window-controls-side controls)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_window_controls_new

(test gtk-window-controls-new
  (is (typep (gtk:window-controls-new :start) 'gtk:window-controls))
  (is (typep (gtk:window-controls-new :end) 'gtk:window-controls)))

;;; --- 2023-8-9 ---------------------------------------------------------------
