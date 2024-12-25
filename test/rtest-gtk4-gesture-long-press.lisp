(in-package :gtk-test)

(def-suite gtk-gesture-long-press :in gtk-event-handling)
(in-suite gtk-gesture-long-press)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkGestureLongPress

(test gtk-gesture-long-press-class
  ;; Check type
  (is (g:type-is-object "GtkGestureLongPress"))
  ;; Check registered name
  (is (eq 'gtk:gesture-long-press
          (glib:symbol-for-gtype "GtkGestureLongPress")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkGestureLongPress")
          (g:gtype (cffi:foreign-funcall "gtk_gesture_long_press_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkGestureSingle")
          (g:type-parent "GtkGestureLongPress")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkGestureLongPress")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkGestureLongPress")))
  ;; Check properties
  (is (equal '("delay-factor")
             (glib-test:list-properties "GtkGestureLongPress")))
  ;; Check signals
  (is (equal '("cancelled" "pressed")
             (glib-test:list-signals "GtkGestureLongPress")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkGestureLongPress"
                                      GTK:GESTURE-LONG-PRESS
                       (:SUPERCLASS GTK:GESTURE-SINGLE
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_gesture_long_press_get_type")
                       ((DELAY-FACTOR GESTURE-LONG-PRESS-DELAY-FACTOR
                         "delay-factor" "gdouble" T T)))
             (gobject:get-gtype-definition "GtkGestureLongPress"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-gesture-long-press-properties
  (let ((gesture (make-instance 'gtk:gesture-long-press)))
    (is (= 1.0d0 (gtk:gesture-long-press-delay-factor gesture)))))

;;; --- Signals ----------------------------------------------------------------

;;;     cancelled

(test gtk-gesture-long-press-cancelled-signal
  (let ((query (g:signal-query (g:signal-lookup "cancelled"
                                                "GtkGestureLongPress"))))
    (is (string= "cancelled" (g:signal-query-signal-name query)))
    (is (string= "GtkGestureLongPress"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;;     pressed

(test gtk-gesture-long-press-pressed-signal
  (let ((query (g:signal-query (g:signal-lookup "pressed"
                                                "GtkGestureLongPress"))))
    (is (string= "pressed" (g:signal-query-signal-name query)))
    (is (string= "GtkGestureLongPress"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '("gdouble" "gdouble")
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_gesture_long_press_new

(test gtk-gesture-long-press-new
  (is (typep (gtk:gesture-long-press-new) 'gtk:gesture-long-press)))

;;; 2024-9-20
