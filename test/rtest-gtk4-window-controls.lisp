(in-package :gtk-test)

(def-suite gtk-window-controls :in gtk-miscellaneous)
(in-suite gtk-window-controls)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkWindowControls

#-gtk-4-18
(test gtk-window-controls-class
  ;; Check type
  (is (g:type-is-object "GtkWindowControls"))
  ;; Check registered name
  (is (eq 'gtk:window-controls
          (glib:symbol-for-gtype "GtkWindowControls")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkWindowControls")
          (g:gtype (cffi:foreign-funcall "gtk_window_controls_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkWindowControls")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkWindowControls")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (glib-test:list-interfaces "GtkWindowControls")))
  ;; Check properties
  (is (equal '("decoration-layout" "empty" "side")
             (glib-test:list-properties "GtkWindowControls")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkWindowControls")))
  ;; Check CSS name
  (is (string= "windowcontrols"
               (gtk:widget-class-css-name "GtkWindowControls")))
  ;; Check accessible role
  (is (eq :group (gtk:widget-class-accessible-role "GtkWindowControls")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkWindowControls" GTK:WINDOW-CONTROLS
                      (:SUPERCLASS GTK:WIDGET
                       :EXPORT T
                       :INTERFACES
                       ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
                       :TYPE-INITIALIZER "gtk_window_controls_get_type")
                      ((DECORATION-LAYOUT WINDOW-CONTROLS-DECORATION-LAYOUT
                        "decoration-layout" "gchararray" T T)
                       (EMPTY WINDOW-CONTROLS-EMPTY "empty" "gboolean" T NIL)
                       (SIDE WINDOW-CONTROLS-SIDE "side" "GtkPackType" T T)))
             (gobject:get-gtype-definition "GtkWindowControls"))))

#+gtk-4-18
(test gtk-window-controls-class
  ;; Check type
  (is (g:type-is-object "GtkWindowControls"))
  ;; Check registered name
  (is (eq 'gtk:window-controls
          (glib:symbol-for-gtype "GtkWindowControls")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkWindowControls")
          (g:gtype (cffi:foreign-funcall "gtk_window_controls_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkWindowControls")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkWindowControls")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (glib-test:list-interfaces "GtkWindowControls")))
  ;; Check properties
  (is (equal '("decoration-layout" "empty" "side" "use-native-controls")
             (glib-test:list-properties "GtkWindowControls")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkWindowControls")))
  ;; Check CSS name
  (is (string= "windowcontrols"
               (gtk:widget-class-css-name "GtkWindowControls")))
  ;; Check accessible role
  (is (eq :group (gtk:widget-class-accessible-role "GtkWindowControls")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkWindowControls" GTK:WINDOW-CONTROLS
                      (:SUPERCLASS GTK:WIDGET
                       :EXPORT T
                       :INTERFACES
                       ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
                       :TYPE-INITIALIZER "gtk_window_controls_get_type")
                      ((DECORATION-LAYOUT WINDOW-CONTROLS-DECORATION-LAYOUT
                        "decoration-layout" "gchararray" T T)
                       (EMPTY WINDOW-CONTROLS-EMPTY "empty" "gboolean" T NIL)
                       (SIDE WINDOW-CONTROLS-SIDE "side" "GtkPackType" T T)
                       (USE-NATIVE-CONTROLS WINDOW-CONTROLS-USE-NATIVE-CONTROLS
                        "use-native-controls" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkWindowControls"))))

;;; --- Properties -------------------------------------------------------------

;;;     decoration-layout
;;;     empty
;;;     side
;;;     use-native-controls

(test gtk-window-controls-properties
  (glib-test:with-check-memory (controls)
    (setf controls (make-instance 'gtk:window-controls))
    (is-false (gtk:window-controls-decoration-layout controls))
    (is-true (gtk:window-controls-empty controls))
    (is (eq :start (gtk:window-controls-side controls)))
    #+gtk-4-18
    (is-false (gtk:window-controls-use-native-controls controls))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_window_controls_new

(test gtk-window-controls-new
  (glib-test:with-check-memory (controls)
    (is (typep (setf controls
                     (gtk:window-controls-new :start)) 'gtk:window-controls))
    (is (eq :start (gtk:window-controls-side controls)))
    (is (typep (setf controls
                     (gtk:window-controls-new :end)) 'gtk:window-controls))
    (is (eq :end (gtk:window-controls-side controls)))))

;;; 2025-3-29
