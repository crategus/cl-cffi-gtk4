(in-package :gtk-test)

(def-suite gtk-gesture-swipe :in gtk-suite)
(in-suite gtk-gesture-swipe)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkGestureSwipe

(test gtk-gesture-swipe-class
  ;; Check type
  (is (g:type-is-object "GtkGestureSwipe"))
  ;; Check registered name
  (is (eq 'gtk:gesture-swipe
          (glib:symbol-for-gtype "GtkGestureSwipe")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkGestureSwipe")
          (g:gtype (cffi:foreign-funcall "gtk_gesture_swipe_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkGestureSingle")
          (g:type-parent "GtkGestureSwipe")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkGestureSwipe")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkGestureSwipe")))
  ;; Check properties
  (is (equal '()
             (glib-test:list-properties "GtkGestureSwipe")))
  ;; Check signals
  (is (equal '("swipe")
             (glib-test:list-signals "GtkGestureSwipe")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkGestureSwipe" GTK:GESTURE-SWIPE
                       (:SUPERCLASS GTK:GESTURE-SINGLE
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_gesture_swipe_get_type")
                       NIL)
             (gobject:get-gtype-definition "GtkGestureSwipe"))))

;;; --- Signals ----------------------------------------------------------------

;;;     swipe

(test gtk-gesture-swipe-swipe-signal
  (let ((query (g:signal-query (g:signal-lookup "swipe" "GtkGestureSwipe"))))
    (is (string= "swipe" (g:signal-query-signal-name query)))
    (is (string= "GtkGestureSwipe"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '("gdouble" "gdouble")
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_gesture_swipe_new

(test gtk-gesture-swipe-new
  (is (typep (gtk:gesture-swipe-new) 'gtk:gesture-swipe)))

;;;     gtk_gesture_swipe_get_velocity

(test gtk-gesture-swipe-velocity
  (let ((gesture (gtk:gesture-swipe-new)))
    (is-false (gtk:gesture-swipe-velocity gesture))))

;;; 2024-9-20
