(in-package :gtk-test)

(def-suite gtk-overlay :in gtk-suite)
(in-suite gtk-overlay)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkOverlay

(test gtk-overlay-class
  ;; Type check
  (is (g:type-is-object "GtkOverlay"))
  ;; Check the registered name
  (is (eq 'gtk:overlay
          (glib:symbol-for-gtype "GtkOverlay")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkOverlay")
          (g:gtype (cffi:foreign-funcall "gtk_overlay_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkOverlay")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkOverlay")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (list-interfaces "GtkOverlay")))
  ;; Check the properties
  (is (equal '("child")
             (list-properties "GtkOverlay")))
  ;; Check the signals
  (is (equal '("get-child-position")
             (list-signals "GtkOverlay")))
  ;; CSS name
  (is (string= "overlay"
               (gtk:widget-class-css-name "GtkOverlay")))
  ;; Accessible role
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

;;; --- Signals ----------------------------------------------------------------

;;;     get-child-position

;;; --- Functions --------------------------------------------------------------

;;;     gtk_overlay_new
;;;     gtk_overlay_add_overlay
;;;     gtk_overlay_remove_overlay
;;;     gtk_overlay_get_measure_overlay
;;;     gtk_overlay_set_measure_overlay
;;;     gtk_overlay_get_clip_overlay
;;;     gtk_overlay_set_clip_overlay

;;; --- 2023-8-31 --------------------------------------------------------------
