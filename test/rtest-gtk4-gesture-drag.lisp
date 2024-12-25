(in-package :gtk-test)

(def-suite gtk-gesture-drag :in gtk-event-handling)
(in-suite gtk-gesture-drag)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkGestureDrag

(test gtk-gesture-drag-class
  ;; Check type
  (is (g:type-is-object "GtkGestureDrag"))
  ;; Check registered name
  (is (eq 'gtk:gesture-drag
          (glib:symbol-for-gtype "GtkGestureDrag")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkGestureDrag")
          (g:gtype (cffi:foreign-funcall "gtk_gesture_drag_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkGestureSingle")
          (g:type-parent "GtkGestureDrag")))
  ;; Check children
  (is (equal '("GtkGesturePan")
             (glib-test:list-children "GtkGestureDrag")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkGestureDrag")))
  ;; Check properties
  (is (equal '()
             (glib-test:list-properties "GtkGestureDrag")))
  ;; Check signals
  (is (equal '("drag-begin" "drag-end" "drag-update")
             (glib-test:list-signals "GtkGestureDrag")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkGestureDrag" GTK:GESTURE-DRAG
                       (:SUPERCLASS GTK:GESTURE-SINGLE
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_gesture_drag_get_type")
                       NIL)
             (gobject:get-gtype-definition "GtkGestureDrag"))))

;;; --- Signals ----------------------------------------------------------------

;;;     drag-begin

(test gtk-gesture-drag-drag-begin-signal
  (let* ((name "drag-begin")
         (gtype (g:gtype "GtkGestureDrag"))
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

;;;     drag-end

(test gtk-gesture-drag-drag-end-signal
  (let* ((name "drag-end")
         (gtype (g:gtype "GtkGestureDrag"))
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

;;;     drag-update

(test gtk-gesture-drag-drag-update-signal
  (let* ((name "drag-update")
         (gtype (g:gtype "GtkGestureDrag"))
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

;;;     gtk_gesture_drag_new

(test gtk-gesture-drag-new
  (is (typep (gtk:gesture-drag-new) 'gtk:gesture-drag)))

;;;     gtk_gesture_drag_get_start_point

(test gtk-gesture-drag-start-point
  (let ((gesture (gtk:gesture-drag-new)))
    (is-false (gtk:gesture-drag-start-point gesture))))

;;;     gtk_gesture_drag_get_offset

(test gtk-gesture-drag-offset
  (let ((gesture (gtk:gesture-drag-new)))
    (is-false (gtk:gesture-drag-offset gesture))))

;;; 2024-7-27
