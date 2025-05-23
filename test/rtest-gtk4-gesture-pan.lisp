(in-package :gtk-test)

(def-suite gtk-gesture-pan :in gtk-event-handling)
(in-suite gtk-gesture-pan)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPanDirection

(test gtk-pan-direction
  ;; Check type
  (is (g:type-is-enum "GtkPanDirection"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkPanDirection")
          (g:gtype (cffi:foreign-funcall "gtk_pan_direction_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:pan-direction
          (glib:symbol-for-gtype "GtkPanDirection")))
  ;; Check names
  (is (equal '("GTK_PAN_DIRECTION_LEFT" "GTK_PAN_DIRECTION_RIGHT"
               "GTK_PAN_DIRECTION_UP" "GTK_PAN_DIRECTION_DOWN")
             (glib-test:list-enum-item-names "GtkPanDirection")))
  ;; Check values
  (is (equal '(0 1 2 3)
             (glib-test:list-enum-item-values "GtkPanDirection")))
  ;; Check nick names
  (is (equal '("left" "right" "up" "down")
             (glib-test:list-enum-item-nicks "GtkPanDirection")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkPanDirection" GTK:PAN-DIRECTION
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_pan_direction_get_type")
                       (:LEFT 0)
                       (:RIGHT 1)
                       (:UP 2)
                       (:DOWN 3))
             (gobject:get-gtype-definition "GtkPanDirection"))))

;;;     GtkGesturePan

(test gtk-gesture-pan-class
  ;; Check type
  (is (g:type-is-object "GtkGesturePan"))
  ;; Check registered name
  (is (eq 'gtk:gesture-pan
          (glib:symbol-for-gtype "GtkGesturePan")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkGesturePan")
          (g:gtype (cffi:foreign-funcall "gtk_gesture_pan_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkGestureDrag")
          (g:type-parent "GtkGesturePan")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkGesturePan")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkGesturePan")))
  ;; Check properties
  (is (equal '("orientation")
             (glib-test:list-properties "GtkGesturePan")))
  ;; Check signals
  (is (equal '("pan")
             (glib-test:list-signals "GtkGesturePan")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkGesturePan" GTK:GESTURE-PAN
                       (:SUPERCLASS GTK:GESTURE-DRAG
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_gesture_pan_get_type")
                       ((ORIENTATION GESTURE-PAN-ORIENTATION
                         "orientation" "GtkOrientation" T T)))
             (gobject:get-gtype-definition "GtkGesturePan"))))

;;; --- Properties -------------------------------------------------------------

;;;     orientation

(test gtk-gesture-pan-properties
  (let ((gesture (make-instance 'gtk:gesture-pan)))
    (is (eq :horizontal (gtk:gesture-pan-orientation gesture)))))

;;; --- Signals ----------------------------------------------------------------

;;;     pan

(test gtk-gesture-pan-pan-signal
  (let ((query (g:signal-query (g:signal-lookup "pan" "GtkGesturePan"))))
    (is (string= "pan" (g:signal-query-signal-name query)))
    (is (string= "GtkGesturePan"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '("GtkPanDirection" "gdouble")
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_gesture_pan_new

(test gtk-gesture-pan-new
  (is (typep (gtk:gesture-pan-new :horizontal) 'gtk:gesture-pan))
  (is (typep (gtk:gesture-pan-new :vertical) 'gtk:gesture-pan)))

;;; 2024-9-21
