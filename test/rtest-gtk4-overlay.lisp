(in-package :gtk-test)

(def-suite gtk-overlay :in gtk-suite)
(in-suite gtk-overlay)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkOverlay

(test gtk-overlay-class
  ;; Check type
  (is (g:type-is-object "GtkOverlay"))
  ;; Check registered name
  (is (eq 'gtk:overlay
          (glib:symbol-for-gtype "GtkOverlay")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkOverlay")
          (g:gtype (cffi:foreign-funcall "gtk_overlay_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkOverlay")))
  ;; Check children
  (is (equal '()
             (gtk-test:list-children "GtkOverlay")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (gtk-test:list-interfaces "GtkOverlay")))
  ;; Check properties
  (is (equal '("child")
             (gtk-test:list-properties "GtkOverlay")))
  ;; Check signals
  (is (equal '("get-child-position")
             (gtk-test:list-signals "GtkOverlay")))
  ;; Check CSS name
  (is (string= "overlay"
               (gtk:widget-class-css-name "GtkOverlay")))
  ;; Check accessible role
  (is (eq :WIDGET (gtk:widget-class-accessible-role "GtkOverlay")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkOverlay" GTK-OVERLAY
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkBuildable"
                                 "GtkConstraintTarget")
                                :TYPE-INITIALIZER "gtk_overlay_get_type")
                               ((CHILD GTK-OVERLAY-CHILD "child" "GtkWidget" T
                                 T)))
             (gobject:get-g-type-definition "GtkOverlay"))))

;;; --- Properties -------------------------------------------------------------

;;;     child

(test gtk-overlay-child
  (let ((overlay (make-instance 'gtk:overlay
                                :child (make-instance 'gtk:box))))
    (is (typep (gtk:overlay-child overlay) 'gtk:box))))

;;; --- Signals ----------------------------------------------------------------

;;;     get-child-position

;;; --- Functions --------------------------------------------------------------

;;;     gtk_overlay_new

(test gtk-overlay-new
  (is (typep (gtk:overlay-new) 'gtk:overlay)))

;;;     gtk_overlay_add_overlay
;;;     gtk_overlay_remove_overlay
;;;     gtk_overlay_get_measure_overlay
;;;     gtk_overlay_set_measure_overlay
;;;     gtk_overlay_get_clip_overlay
;;;     gtk_overlay_set_clip_overlay

;;; 2024-4-22
