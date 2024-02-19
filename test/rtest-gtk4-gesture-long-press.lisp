(in-package :gtk-test)

(def-suite gtk-gesture-long-press :in gtk-suite)
(in-suite gtk-gesture-long-press)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkGestureLongPress

(test gesture-long-press-class
  ;; Type check
  (is (g:type-is-object "GtkGestureLongPress"))
  ;; Check the registered name
  (is (eq 'gtk:gesture-long-press
          (glib:symbol-for-gtype "GtkGestureLongPress")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkGestureLongPress")
          (g:gtype (cffi:foreign-funcall "gtk_gesture_long_press_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkGestureSingle")
          (g:type-parent "GtkGestureLongPress")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkGestureLongPress")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkGestureLongPress")))
  ;; Check the properties
  (is (equal '("delay-factor")
             (list-properties "GtkGestureLongPress")))
  ;; Check the signals
  (is (equal '("cancelled" "pressed")
             (list-signals "GtkGestureLongPress")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkGestureLongPress"
                                             GTK-GESTURE-LONG-PRESS
                       (:SUPERCLASS GTK-GESTURE-SINGLE :EXPORT T :INTERFACES
                        NIL :TYPE-INITIALIZER
                        "gtk_gesture_long_press_get_type")
                       ((DELAY-FACTOR GTK-GESTURE-LONG-PRESS-DELAY-FACTOR
                         "delay-factor" "gdouble" T T)))
             (gobject:get-g-type-definition "GtkGestureLongPress"))))

;;; --- Properties -------------------------------------------------------------

;;;     delay-factor

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

;;; 2024-2-19
