(in-package :gtk-test)

(def-suite gdk-seat :in gdk-suite)
(in-suite gdk-seat)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkSeatCapabilities

(test gdk-seat-capabilities
  ;; Check type
  (is (g:type-is-flags "GdkSeatCapabilities"))
  ;; Check registered name
  (is (eq 'gdk:seat-capabilities
          (glib:symbol-for-gtype "GdkSeatCapabilities")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkSeatCapabilities")
          (g:gtype (cffi:foreign-funcall "gdk_seat_capabilities_get_type"
                                         :size))))
  ;; Check names
  (is (equal '("GDK_SEAT_CAPABILITY_NONE" "GDK_SEAT_CAPABILITY_POINTER"
               "GDK_SEAT_CAPABILITY_TOUCH" "GDK_SEAT_CAPABILITY_TABLET_STYLUS"
               "GDK_SEAT_CAPABILITY_KEYBOARD" "GDK_SEAT_CAPABILITY_TABLET_PAD"
               "GDK_SEAT_CAPABILITY_ALL_POINTING" "GDK_SEAT_CAPABILITY_ALL")
             (glib-test:list-flags-item-names "GdkSeatCapabilities")))
  ;; Check values
  (is (equal '(0 1 2 4 8 16 7 15)
             (glib-test:list-flags-item-values "GdkSeatCapabilities")))
  ;; Check nick names
  (is (equal '("none" "pointer" "touch" "tablet-stylus" "keyboard" "tablet-pad"
               "all-pointing" "all")
             (glib-test:list-flags-item-nicks "GdkSeatCapabilities")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-GFLAGS "GdkSeatCapabilities"
                                     GDK:SEAT-CAPABILITIES
                       (:EXPORT T
                        :TYPE-INITIALIZER "gdk_seat_capabilities_get_type")
                       (:NONE 0)
                       (:POINTER 1)
                       (:TOUCH 2)
                       (:TABLET-STYLUS 4)
                       (:KEYBOARD 8)
                       (:TABLET-PAD 16)
                       (:ALL-POINTING 7)
                       (:ALL 15))
             (gobject:get-gtype-definition "GdkSeatCapabilities"))))

;;;     GdkSeat

(test gdk-seat-class
  ;; Check type
  (is (g:type-is-object "GdkSeat"))
  ;; Check registered name
  (is (eq 'gdk:seat
          (glib:symbol-for-gtype "GdkSeat")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkSeat")
          (g:gtype (cffi:foreign-funcall "gdk_seat_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkSeat")))
  ;; Check children
  #-windows
  (is (member "GdkWaylandSeat"
             (glib-test:list-children "GdkSeat") :test #'string=))
  #+windows
  (is (equal '("GdkSeatDefault")
             (glib-test:list-children "GdkSeat")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GdkSeat")))
  ;; Check properties
  (is (equal '("display")
             (glib-test:list-properties "GdkSeat")))
  ;; Check signals
  (is (equal '("device-added" "device-removed" "tool-added" "tool-removed")
             (glib-test:list-signals "GdkSeat")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GdkSeat" GDK:SEAT
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_seat_get_type")
                       ((DISPLAY SEAT-DISPLAY "display" "GdkDisplay" T NIL)))
             (gobject:get-gtype-definition "GdkSeat"))))

;;; --- Properties -------------------------------------------------------------

;;;     display

(test gdk-seat-properties
  (glib-test:with-check-memory (:strong 2)
    (let ((seat (gdk:display-default-seat (gdk:display-default))))
      (is (typep (gdk:seat-display seat) 'gdk:display)))))

;;; --- Signals ----------------------------------------------------------------

;;;     device-added
;;;     device-removed
;;;     tool-added
;;;     tool-removed

;;; --- Functions --------------------------------------------------------------

;;;     gdk_seat_get_capabilities

(test gdk-seat-capabilities
  (glib-test:with-check-memory (:strong 2)
    (let ((seat (gdk:display-default-seat (gdk:display-default))))
      (is (equal '(:POINTER :KEYBOARD)
                 (gdk:seat-capabilities seat))))))

;;;     gdk_seat_get_pointer

(test gdk-seat-pointer
  (glib-test:with-check-memory (:strong 3)
    (let ((seat (gdk:display-default-seat (gdk:display-default))))
      (is (typep (gdk:seat-pointer seat) 'gdk:device))
      #-windows
      (is (string= "Core Pointer"
                   (gdk:device-name (gdk:seat-pointer seat))))
      #+windows
      (is (string= "Virtual Core Pointer"
                   (gdk:device-name (gdk:seat-pointer seat)))))))

;;;     gdk_seat_get_keyboard

(test gdk-seat-keyboard
  (glib-test:with-check-memory (:strong 3)
    (let ((seat (gdk:display-default-seat (gdk:display-default))))
      (is (typep (gdk:seat-keyboard seat) 'gdk:device))
      #-windows
      (is (string= "Core Keyboard"
                   (gdk:device-name (gdk:seat-keyboard seat))))
      #+windows
      (is (string= "Virtual Core Keyboard"
                   (gdk:device-name (gdk:seat-keyboard seat)))))))

;;;     gdk_seat_get_devices

(test gdk-seat-devices
  (glib-test:with-check-memory (:strong 4)
    (let ((seat (gdk:display-default-seat (gdk:display-default))))
      (is (every (lambda (x) (typep x 'gdk:device))
                 (gdk:seat-devices seat :all))))))

;;;     gdk_seat_get_tools

(test gdk-seat-tools
  (glib-test:with-check-memory (:strong 2)
    (let ((seat (gdk:display-default-seat (gdk:display-default))))
      (is-false (gdk:seat-tools seat)))))

;;; 2024-12-20
