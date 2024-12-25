(in-package :gtk-test)

(def-suite gtk-gesture-stylus :in gtk-event-handling)
(in-suite gtk-gesture-stylus)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkGestureStylus

(test gtk-gesture-stylus-class
  ;; Check type
  (is (g:type-is-object "GtkGestureStylus"))
  ;; Check registered name
  (is (eq 'gtk:gesture-stylus
          (glib:symbol-for-gtype "GtkGestureStylus")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkGestureStylus")
          (g:gtype (cffi:foreign-funcall "gtk_gesture_stylus_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkGestureSingle")
          (g:type-parent "GtkGestureStylus")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkGestureStylus")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkGestureStylus")))
  ;; Check properties
  (is (equal '("stylus-only")
             (glib-test:list-properties "GtkGestureStylus")))
  ;; Check signals
  (is (equal '("down" "motion" "proximity" "up")
             (glib-test:list-signals "GtkGestureStylus")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkGestureStylus" GTK:GESTURE-STYLUS
                       (:SUPERCLASS GTK:GESTURE-SINGLE
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_gesture_stylus_get_type")
                       ((STYLUS-ONLY GESTURE-STYLUS-STYLUS-ONLY
                         "stylus-only" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkGestureStylus"))))

;;; --- Properties -------------------------------------------------------------

;;;     stylus-only                                        Since 4.10

(test gtk-gesture-stylus-properties
  (let ((gesture (make-instance 'gtk:gesture-stylus)))
    (is-true (gtk:gesture-stylus-stylus-only gesture))))

;;; --- Signals ----------------------------------------------------------------

;;;     down

(test gtk-gesture-stylus-down-signal
  (let* ((name "down")
         (gtype (g:gtype "GtkGestureStylus"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '("gdouble" "gdouble")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     motion

(test gtk-gesture-stylus-motion-signal
  (let* ((name "motion")
         (gtype (g:gtype "GtkGestureStylus"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '("gdouble" "gdouble")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     proximity

(test gtk-gesture-stylus-proximity-signal
  (let* ((name "proximity")
         (gtype (g:gtype "GtkGestureStylus"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '("gdouble" "gdouble")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     up

(test gtk-gesture-stylus-up-signal
  (let* ((name "up")
         (gtype (g:gtype "GtkGestureStylus"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '("gdouble" "gdouble")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_gesture_stylus_new

(test gtk-gesture-stylus-new
  (is (typep (gtk:gesture-stylus-new) 'gtk:gesture-stylus)))

;;;     gtk_gesture_stylus_get_axis

(test gtk-gesture-stylus-axis
  (let ((gesture (gtk:gesture-stylus-new)))
    (is-false (gtk:gesture-stylus-axis gesture :x))))

;;;     gtk_gesture_stylus_get_axes
;;;     gtk_gesture_stylus_get_backlog

;;;     gtk_gesture_stylus_get_device_tool

(test gtk-gesture-stylus-device-tool
  (let ((gesture (gtk:gesture-stylus-new)))
    (is-false (gtk:gesture-stylus-device-tool gesture))))

;;; 2024-9-20
