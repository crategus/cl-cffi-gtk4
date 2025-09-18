(in-package :gtk-test)

(def-suite gdk-display :in gdk-suite)
(in-suite gdk-display)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkDisplay

(test gdk-display-class
  ;; Check type
  (is (g:type-is-object "GdkDisplay"))
  ;; Check registered name
  (is (eq 'gdk:display
          (glib:symbol-for-gtype "GdkDisplay")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkDisplay")
          (g:gtype (cffi:foreign-funcall "gdk_display_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkDisplay")))
  ;; Check children
  ;; Returns various children, GdkWaylandDisplay should be present
  #-windows
  (is (member "GdkWaylandDisplay"
              (glib-test:list-children "GdkDisplay") :test #'string=))
  #+windows
  (is (equal '("GdkWin32Display")
             (glib-test:list-children "GdkDisplay")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GdkDisplay")))
  ;; Check properties
  (is (equal '("composited" "dmabuf-formats" "input-shapes" "rgba"
               "shadow-width")
             (glib-test:list-properties "GdkDisplay")))
  ;; Check signals
  (is (equal '("closed" "opened" "seat-added" "seat-removed" "setting-changed")
             (glib-test:list-signals "GdkDisplay")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GdkDisplay" GDK:DISPLAY
                      (:SUPERCLASS G:OBJECT
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gdk_display_get_type")
                      ((COMPOSITED DISPLAY-COMPOSITED
                        "composited" "gboolean" T NIL)
                       (DMABUF-FORMATS DISPLAY-DMABUF-FORMATS
                        "dmabuf-formats" "GdkDmabufFormats" T NIL)
                       (INPUT-SHAPES DISPLAY-INPUT-SHAPES
                        "input-shapes" "gboolean" T NIL)
                       (RGBA DISPLAY-RGBA "rgba" "gboolean" T NIL)
                       (SHADOW-WIDTH DISPLAY-SHADOW-WIDTH
                        "shadow-width" "gboolean" T NIL)))
             (gobject:get-gtype-definition "GdkDisplay"))))

;;; --- Signals ----------------------------------------------------------------

(test gdk-display-closed-signal
  (let* ((name "closed")
         (gtype (g:gtype "GdkDisplay"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    (is (equal '("gboolean")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

(test gdk-display-opened-signal
  (let* ((name "opened")
         (gtype (g:gtype "GdkDisplay"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

(test gdk-display-seat-added-signal
  (let* ((name "seat-added")
         (gtype (g:gtype "GdkDisplay"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    (is (equal '("GdkSeat")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

(test gdk-display-seat-removed-signal
  (let* ((name "seat-removed")
         (gtype (g:gtype "GdkDisplay"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    (is (equal '("GdkSeat")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

(test gdk-display-setting-changed-signal
  (let* ((name "setting-changed")
         (gtype (g:gtype "GdkDisplay"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    (is (equal '("gchararray")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Properties -------------------------------------------------------------

(test gdk-display-properties
  (when *first-run-testsuite*
    (glib-test:with-check-memory (:strong 1)
      (let ((display (gdk:display-default)))
        (is-true (gdk:display-composited display))
        (is (typep (gdk:display-dmabuf-formats display) 'gdk:dmabuf-formats))
        (is-true (gdk:display-input-shapes display))
        (is-true (gdk:display-rgba display))
        (is-true (gdk:display-shadow-width display))))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk_display_open

;; This test adds a strong reference for a GdkDisplay for every run

#-windows
(test gdk-display-open
  (when *first-run-testsuite*
    (glib-test:with-check-memory (:strong 2)
      (let ((name (uiop:getenv "DISPLAY"))
            display)
        (is (typep (setf display (gdk:display-open name)) 'gdk:display))
        (is-false (eq display (gdk:display-default)))))))

#+nil ;; No longer works for Windows?
(test gdk-display-open
  (when *first-run-testsuite*
    (glib-test:with-check-memory (:strong 1)
      (let (display)
        (is (typep (setf display
                         (gdk:display-open "1\\WinSta0\\Default")) 'gdk:display))
        (is (eq display (gdk:display-default)))))))

;;;     gdk_display_get_default

(test gdk-display-default
 (when *first-run-testsuite*
    (glib-test:with-check-memory (:strong 1)
        (is (typep (gdk:display-default) 'gdk:display)))))

;;;     gdk_display_get_name

#-windows
(test gdk-display-name
  (when *first-run-testsuite*
    (glib-test:with-check-memory (:strong 1)
      (let ((display (gdk:display-default)))
        (is (stringp (gdk:display-name display)))))))

#+windows
(test gdk-display-name
  (when *first-run-testsuite*
    (glib-test:with-check-memory (:strong 1)
      (let ((display (gdk:display-default)))
        (is (string= "1\\WinSta0\\Default" (gdk:display-name display)))))))

;;;     gdk_display_device_is_grabbed

#-windows
(test gdk-display-device-is-grabbed
  (when *first-run-testsuite*
    (glib-test:with-check-memory ((seat 3) (keyboard 4) (pointer 4) :strong 4)
      (let ((display (gdk:display-default)))
        (setf seat (gdk:display-default-seat display))
        (is-false (gdk:display-device-is-grabbed display
                                                 (setf keyboard
                                                       (gdk:seat-keyboard seat))))
        (is-false (gdk:display-device-is-grabbed display
                                                 (setf pointer
                                                       (gdk:seat-pointer seat))))))))

#+windows
(test gdk-display-device-is-grabbed
  (when *first-run-testsuite*
    (glib-test:with-check-memory ((seat 2) (keyboard 5) (pointer 5) :strong 4)
       (let ((display (gdk:display-default)))
         (setf seat (gdk:display-default-seat display))
         (is-false (gdk:display-device-is-grabbed display
                                                  (setf keyboard
                                                        (gdk:seat-keyboard seat))))
         (is-false (gdk:display-device-is-grabbed display
                                                  (setf pointer
                                                        (gdk:seat-pointer seat))))))))

;;;     gdk_display_beep

(test gdk-display-beep
  (when *first-run-testsuite*
    (glib-test:with-check-memory (:strong 1)
      (let ((display (gdk:display-default)))
        (is-false (gdk:display-beep display))))))

;;;     gdk_display_sync
;;;     gdk_display_flush
;;;     gdk_display_is_closed

(test gdk-display-sync/flush/is-closed
  (when *first-run-testsuite*
    (glib-test:with-check-memory (:strong 1)
      (let ((display (gdk:display-default)))
        (is-false (gdk:display-sync display))
        (is-false (gdk:display-flush display))
        (is-false (gdk:display-is-closed display))))))

;;;     gdk_display_close

;; Do not close the default display.

;;;     gdk_display_is_rgba
;;;     gdk_display_is_composited
;;;     gdk_display_supports_input_shapes

(test gdk-display-is-rgba/composited/supports-input-shapes
  (when *first-run-testsuite*
    (glib-test:with-check-memory (:strong 1)
      (let ((display (gdk:display-default)))
        (is-true (gdk:display-is-rgba display))
        (is-true (gdk:display-is-composited display))
        (is-true (gdk:display-supports-input-shapes display))))))

;;;     gdk_display_get_app_launch_context

(test gdk-display-app-launch-context
  (when *first-run-testsuite*
    (glib-test:with-check-memory (context :strong 1)
      (let ((display (gdk:display-default)))
        (is (typep (setf context
                         (gdk:display-app-launch-context display))
                   'gdk:app-launch-context))))))

;;;     gdk_display_notify_startup_complete                 Deprecated 4.10

;;;     gdk_display_get_default_seat

#-windows
(test gdk-display-default-seat
  (when *first-run-testsuite*
    (glib-test:with-check-memory ((seat 3) :strong 2)
      (let ((display (gdk:display-default)))
        (is (typep (setf seat (gdk:display-default-seat display)) 'gdk:seat))))))

#+windows
(test gdk-display-default-seat
  (when *first-run-testsuite*
    (glib-test:with-check-memory ((seat 2) :strong 2)
      (let ((display (gdk:display-default)))
        (is (typep (setf seat (gdk:display-default-seat display)) 'gdk:seat))))))

;;;     gdk_display_list_seats

(test gdk-display-list-seats
  (when *first-run-testsuite*
    (glib-test:with-check-memory (:strong 2)
      (let ((display (gdk:display-default)))
        (is (every (lambda (x) (typep x 'gdk:seat))
                   (gdk:display-list-seats display)))))))

;;;     gdk_display_get_monitors

(test gdk-display-monitors
  (when *first-run-testsuite*
    (glib-test:with-check-memory (:strong 4)
      (let ((display (gdk:display-default)))
        (is (every (lambda (x) (typep x 'gdk:monitor))
                   (gdk:display-monitors display)))))))

;;;     gdk_display_get_monitor_at_surface

;;;     gdk_display_get_clipboard

(test gdk-display-clipboard
  (when *first-run-testsuite*
    (glib-test:with-check-memory (:strong 2)
      (let ((display (gdk:display-default)))
        (is (typep (gdk:display-clipboard display) 'gdk:clipboard))))))

;;;     gdk_display_get_primary_clipboard

(test gdk-display-primary-clipboard
  (when *first-run-testsuite*
    (glib-test:with-check-memory (:strong 2)
      (let ((display (gdk:display-default)))
        (is (typep (gdk:display-primary-clipboard display) 'gdk:clipboard))))))

;;;     gdk_display_get_setting

(test gdk-display-setting
  (when *first-run-testsuite*
    (glib-test:with-check-memory (:strong 2)
      (let ((display (gdk:display-default)))
        (is (= (gtk:settings-gtk-double-click-time (gtk:settings-default))
               (gdk:display-setting display "gtk-double-click-time" "gint")))))))

;;;     gdk_display_get_startup_notification_id             Deprecated 4.10
;;;     gdk_display_put_event                               Deprecated 4.10

;;;     gdk_display_map_keyval

#-windows
(test gdk-display-map-keyval
  (when *first-run-testsuite*
    (glib-test:with-check-memory (:strong 1)
      (let ((display (gdk:display-default)))
        (is (equal '((38 0 0) (38 1 0)) (gdk:display-map-keyval display 97)))
        (is (equal '((38 0 1) (38 1 1)) (gdk:display-map-keyval display 65)))))))

#+windows
(test gdk-display-map-keyval
  (when *first-run-testsuite*
    (glib-test:with-check-memory (:strong 1)
      (let ((display (gdk:display-default)))
        (is (equal '((65 0 0)
                     (65 0 3)
                     (65 0 15)
                     (65 0 2)) (gdk:display-map-keyval display 97)))
        (is (equal '((65 0 1)
                     (65 0 4)
                     (65 0 15)
                     (65 0 5)) (gdk:display-map-keyval display 65)))))))

;;;     gdk_display_map_keycode

;; TODO: Check this more carefully. What is expected!?

#-windows
(test gdk-display-map-keycode
  (when *first-run-testsuite*
    (glib-test:with-check-memory (:strong 1)
      (let ((display (gdk:display-default)))
        (is (equal '((97 38 0 0)
                     (65 38 0 1)
                     (230 38 0 2)
                     (198 38 0 3)
                     (97 38 1 0)
                     (65 38 1 1)
                     (230 38 1 2)
                     (198 38 1 3))
                   (gdk:display-map-keycode display 38)))))))

#+windows
(test gdk-display-map-keycode
  (when *first-run-testsuite*
    (glib-test:with-check-memory (:strong 1)
      (let ((display (gdk:display-default)))
        (is (equal '((65362 38 0 0)) (gdk:display-map-keycode display 38)))))))

;;;     gdk_display_translate_key

#-windows
(test gdk-display-translate-key
  (when *first-run-testsuite*
    (glib-test:with-check-memory (:strong 1)
      (let ((display (gdk:display-default)))
        (is (= 97 (gdk:display-translate-key display 38 :no-modifier-mask 0)))
        (is (= 65 (gdk:display-translate-key display 38 :shift-mask 0)))))))

#+windows
(test gdk-display-translate-key
  (when *first-run-testsuite*
    (glib-test:with-check-memory (:strong 1)
      (let ((display (gdk:display-default)))
        (is (= 65362 (gdk:display-translate-key display 38 :no-modifier-mask 0)))
        (is (= 65362 (gdk:display-translate-key display 38 :shift-mask 0)))))))

;;;     gdk_display_prepare_gl                              Since 4.4
;;;     gdk_display_create_gl_context                       Since 4.6

;;; 2025-09-18
