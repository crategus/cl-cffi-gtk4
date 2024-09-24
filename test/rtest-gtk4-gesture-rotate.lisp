(in-package :gtk-test)

(def-suite gtk-gesture-rotate :in gtk-suite)
(in-suite gtk-gesture-rotate)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkGestureRotate

(test gtk-gesture-rotate-class
  ;; Check type
  (is (g:type-is-object "GtkGestureRotate"))
  ;; Check registered name
  (is (eq 'gtk:gesture-rotate
          (glib:symbol-for-gtype "GtkGestureRotate")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkGestureRotate")
          (g:gtype (cffi:foreign-funcall "gtk_gesture_rotate_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkGesture")
          (g:type-parent "GtkGestureRotate")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkGestureRotate")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkGestureRotate")))
  ;; Check properties
  (is (equal '()
             (glib-test:list-properties "GtkGestureRotate")))
  ;; Check signals
  (is (equal '("angle-changed")
             (glib-test:list-signals "GtkGestureRotate")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkGestureRotate" GTK:GESTURE-ROTATE
                       (:SUPERCLASS GTK:GESTURE
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_gesture_rotate_get_type")
                       NIL)
             (gobject:get-gtype-definition "GtkGestureRotate"))))

;;; --- Signals ----------------------------------------------------------------

;;;     angle-changed

(test gtk-gesture-rotate-angle-changed-signal
  (let* ((name "angle-changed")
         (gtype (g:gtype "GtkGestureRotate"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '("gdouble" "gdouble")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_gesture_rotate_new

(test gtk-gestsure-rotate-new
  (is (typep (gtk:gesture-rotate-new) 'gtk:gesture-rotate)))

;;;     gtk_gesture_rotate_get_angle_delta

(test gtk-gesture-rotate-angle-delta
  (let ((gesture (gtk:gesture-rotate-new)))
    (is (= 0.0d0 (gtk:gesture-rotate-angle-delta gesture)))))

;;; 2024-9-20
