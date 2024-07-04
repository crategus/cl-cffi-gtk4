(in-package :gtk-test)

(def-suite gdk-popup :in gdk-suite)
(in-suite gdk-popup)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkPopup

(test gdk-popup-interface
  ;; Check type
  (is (g:type-is-interface "GdkPopup"))
  ;; Check registered name
  (is (eq 'gdk:popup
          (glib:symbol-for-gtype "GdkPopup")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkPopup")
          (g:gtype (cffi:foreign-funcall "gdk_popup_get_type" :size))))
  ;; Check interface prerequisites
  (is (equal '("GdkSurface")
             (gtk-test:list-interface-prerequisites "GdkPopup")))
  ;; Check interface properties
  (is (equal '("autohide" "parent")
             (gtk-test:list-interface-properties "GdkPopup")))
  ;; Check signals
  (is (equal '()
             (gtk-test:list-signals "GdkPopup")))
  ;; Check interface definition
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

;;; 2024-7-4
