(in-package :gtk-test)

(def-suite gtk-gesture-pan :in gtk-suite)
(in-suite gtk-gesture-pan)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPanDirection

;;;     GtkGesturePan

(test gesture-pan-class
  ;; Type check
  (is (g:type-is-object "GtkGesturePan"))
  ;; Check the registered name
  (is (eq 'gtk:gesture-pan
          (glib:symbol-for-gtype "GtkGesturePan")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkGesturePan")
          (g:gtype (cffi:foreign-funcall "gtk_gesture_pan_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkGestureDrag")
          (g:type-parent "GtkGesturePan")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkGesturePan")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkGesturePan")))
  ;; Check the properties
  (is (equal '("orientation")
             (list-properties "GtkGesturePan")))
  ;; Check the signals
  (is (equal '("pan")
             (list-signals "GtkGesturePan")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkGesturePan" GTK-GESTURE-PAN
                       (:SUPERCLASS GTK-GESTURE-DRAG :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_gesture_pan_get_type")
                       ((ORIENTATION GTK-GESTURE-PAN-ORIENTATION "orientation"
                         "GtkOrientation" T T)))
             (gobject:get-g-type-definition "GtkGesturePan"))))

;;; --- Properties -------------------------------------------------------------

;;;     orientation

;;; --- Signals ----------------------------------------------------------------

;;;     pan

;;; --- Functions --------------------------------------------------------------

;;;     gtk_gesture_pan_new

;;; --- 2023-5-29 --------------------------------------------------------------
