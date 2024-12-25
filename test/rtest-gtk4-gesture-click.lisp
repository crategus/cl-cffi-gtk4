(in-package :gtk-test)

(def-suite gtk-gesture-click :in gtk-event-handling)
(in-suite gtk-gesture-click)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkGestureClick

(test gtk-gesture-click-class
  ;; Check type
  (is (g:type-is-object "GtkGestureClick"))
  ;; Check registered name
  (is (eq 'gtk:gesture-click
          (glib:symbol-for-gtype "GtkGestureClick")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkGestureClick")
          (g:gtype (cffi:foreign-funcall "gtk_gesture_click_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkGestureSingle")
          (g:type-parent "GtkGestureClick")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkGestureClick")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkGestureClick")))
  ;; Check properties
  (is (equal '()
             (glib-test:list-properties "GtkGestureClick")))
  ;; Check signals
  (is (equal '("pressed" "released" "stopped" "unpaired-release")
             (glib-test:list-signals "GtkGestureClick")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkGestureClick" GTK:GESTURE-CLICK
                       (:SUPERCLASS GTK:GESTURE-SINGLE
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_gesture_click_get_type")
                       NIL)
             (gobject:get-gtype-definition "GtkGestureClick"))))

;;; --- Signals ----------------------------------------------------------------

;;;     pressed

(test gtk-gesture-click-pressed-signal
  (let* ((name "pressed")
         (gtype (g:gtype "GtkGestureClick"))
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
    (is (equal '("gint" "gdouble" "gdouble")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     released

(test gtk-gesture-click-released-signal
  (let* ((name "released")
         (gtype (g:gtype "GtkGestureClick"))
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
    (is (equal '("gint" "gdouble" "gdouble")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     stopped

(test gtk-gesture-click-stopped-signal
  (let* ((name "stopped")
         (gtype (g:gtype "GtkGestureClick"))
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
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     unpaired-release

(test gtk-gesture-click-unpaired-release-signal
  (let* ((name "unpaired-release")
         (gtype (g:gtype "GtkGestureClick"))
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
    (is (equal '("gdouble" "gdouble" "guint" "GdkEventSequence")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_gesture_click_new

(test gtk-gesture-click-new
  (is (typep (gtk:gesture-click-new) 'gtk:gesture-click)))

;;; 2024-9-20
