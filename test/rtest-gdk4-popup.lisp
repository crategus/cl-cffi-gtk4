(in-package :gtk-test)

(def-suite gdk-popup :in gdk-suite)
(in-suite gdk-popup)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkPopup

(test gdk-popup-interface
  ;; Type check
  (is (g:type-is-interface "GdkPopup"))
  ;; Check the registered name
  (is (eq 'gdk:popup
          (glib:symbol-for-gtype "GdkPopup")))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkPopup")
          (g:gtype (cffi:foreign-funcall "gdk_popup_get_type" :size))))
  ;; Check the interface prerequisites
  (is (equal '("GdkSurface")
             (list-interface-prerequisites "GdkPopup")))
  ;; Check the interface properties
  (is (equal '("autohide" "parent")
             (list-interface-properties "GdkPopup")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GdkPopup")))
  ;; Get the interface definition
  (is (equal '(GOBJECT:DEFINE-G-INTERFACE "GdkPopup" GDK-POPUP
                            (:EXPORT T :TYPE-INITIALIZER "gdk_popup_get_type")
                            (AUTOHIDE GDK-POPUP-AUTOHIDE "autohide" "gboolean"
                             T NIL)
                            (PARENT GDK-POPUP-PARENT "parent" "GdkSurface" T
                             NIL))
             (gobject:get-g-type-definition "GdkPopup"))))

;;; --- Properties -------------------------------------------------------------

;;;     autohide
;;;     parent

;;; --- Functions --------------------------------------------------------------

;;;     gdk_popup_present
;;;     gdk_popup_get_surface_anchor
;;;     gdk_popup_get_rect_anchor
;;;     gdk_popup_get_position_x
;;;     gdk_popup_get_position_y

;;; --- 2023-7-30 --------------------------------------------------------------
