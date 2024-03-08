(in-package :gtk-test)

(def-suite gtk-gesture-zoom :in gtk-suite)
(in-suite gtk-gesture-zoom)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkGestureZoom

(test gesture-zoom-class
  ;; Type check
  (is (g:type-is-object "GtkGestureZoom"))
  ;; Check the registered name
  (is (eq 'gtk:gesture-zoom
          (glib:symbol-for-gtype "GtkGestureZoom")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkGestureZoom")
          (g:gtype (cffi:foreign-funcall "gtk_gesture_zoom_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkGesture")
          (g:type-parent "GtkGestureZoom")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkGestureZoom")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkGestureZoom")))
  ;; Check the properties
  (is (equal '()
             (list-properties "GtkGestureZoom")))
  ;; Check the signals
  (is (equal '("scale-changed")
             (list-signals "GtkGestureZoom")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkGestureZoom" GTK-GESTURE-ZOOM
                       (:SUPERCLASS GTK-GESTURE :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_gesture_zoom_get_type")
                       NIL)
             (gobject:get-g-type-definition "GtkGestureZoom"))))

;;; --- Signals ----------------------------------------------------------------

;;;     scale-changed

;;; --- Functions --------------------------------------------------------------

;;;     gtk_gesture_zoom_new

(test gtk-gesture-zoom-new
  (is (typep (gtk:gesture-zoom-new) 'gtk:gesture-zoom)))

;;;     gtk_gesture_zoom_get_scale_delta

(test gtk-gesture-zoom-scale-delta
  (let ((gesture (gtk:gesture-zoom-new)))
    (is (= 1.0d0 (gtk:gesture-zoom-scale-delta gesture)))))

;;; 2024-2-21
