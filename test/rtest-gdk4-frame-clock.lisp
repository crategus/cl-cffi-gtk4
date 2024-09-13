(in-package :gtk-test)

(def-suite gdk-frame-clock :in gdk-suite)
(in-suite gdk-frame-clock)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkFrameClockPhase

(test gdk-frame-clock-phase
  ;; Check type
  (is (g:type-is-flags "GdkFrameClockPhase"))
  ;; Check registered name
  (is (eq 'gdk:frame-clock-phase
          (glib:symbol-for-gtype "GdkFrameClockPhase")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkFrameClockPhase")
          (g:gtype (cffi:foreign-funcall "gdk_frame_clock_phase_get_type"
                                         :size))))
  ;; Check names
  (is (equal '("GDK_FRAME_CLOCK_PHASE_NONE"
               "GDK_FRAME_CLOCK_PHASE_FLUSH_EVENTS"
               "GDK_FRAME_CLOCK_PHASE_BEFORE_PAINT"
               "GDK_FRAME_CLOCK_PHASE_UPDATE"
               "GDK_FRAME_CLOCK_PHASE_LAYOUT"
               "GDK_FRAME_CLOCK_PHASE_PAINT"
               "GDK_FRAME_CLOCK_PHASE_RESUME_EVENTS"
               "GDK_FRAME_CLOCK_PHASE_AFTER_PAINT")
             (gtk-test:list-flags-item-name "GdkFrameClockPhase")))
  ;; Check values
  (is (equal '(0 1 2 4 8 16 32 64)
             (gtk-test:list-flags-item-value "GdkFrameClockPhase")))
  ;; Check nick names
  (is (equal '("none" "flush-events" "before-paint" "update" "layout" "paint"
               "resume-events" "after-paint")
             (gtk-test:list-flags-item-nick "GdkFrameClockPhase")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-G-FLAGS "GdkFrameClockPhase" GDK-FRAME-CLOCK-PHASE
                                      (:EXPORT T
                                       :TYPE-INITIALIZER
                                       "gdk_frame_clock_phase_get_type")
                                      (:NONE 0)
                                      (:FLUSH-EVENTS 1)
                                      (:BEFORE-PAINT 2)
                                      (:UPDATE 4)
                                      (:LAYOUT 8)
                                      (:PAINT 16)
                                      (:RESUME-EVENTS 32)
                                      (:AFTER-PAINT 64))
             (gobject:get-g-type-definition "GdkFrameClockPhase"))))

;;;     GdkFrameClock

(test gdk-frame-clock-class
  ;; Check type
  (is (g:type-is-object "GdkFrameClock"))
  ;; Check registered name
  (is (eq 'gdk:frame-clock
          (glib:symbol-for-gtype "GdkFrameClock")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkFrameClock")
          (g:gtype (cffi:foreign-funcall "gdk_frame_clock_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkFrameClock")))
  ;; Check children
  (is (equal '("GdkFrameClockIdle")
             (gtk-test:list-children "GdkFrameClock")))
  ;; Check interfaces
  (is (equal '()
             (gtk-test:list-interfaces "GdkFrameClock")))
  ;; Check properties
  (is (equal '()
             (gtk-test:list-properties "GdkFrameClock")))
  ;; Check signals
  (is (equal '("after-paint" "before-paint" "flush-events" "layout" "paint"
               "resume-events" "update")
             (gtk-test:list-signals "GdkFrameClock")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GdkFrameClock" GDK-FRAME-CLOCK
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                                :TYPE-INITIALIZER "gdk_frame_clock_get_type")
                               NIL)
             (gobject:get-g-type-definition "GdkFrameClock"))))

;;; --- Signals ----------------------------------------------------------------

;;;     after-paint
;;;     before-paint
;;;     flush-events
;;;     layout
;;;     paint
;;;     resume-events
;;;     update

;;; --- Functions --------------------------------------------------------------

;;;     gdk_frame_clock_get_frame_time
;;;     gdk_frame_clock_request_phase
;;;     gdk_frame_clock_begin_updating
;;;     gdk_frame_clock_end_updating
;;;     gdk_frame_clock_get_frame_counter
;;;     gdk_frame_clock_get_history_start
;;;     gdk_frame_clock_get_timings
;;;     gdk_frame_clock_get_current_timings
;;;     gdk_frame_clock_get_refresh_info
;;;     gdk_frame_clock_get_fps

;;; 2024-7-12
