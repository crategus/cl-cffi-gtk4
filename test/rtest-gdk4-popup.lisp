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
             (glib-test:list-interface-prerequisites "GdkPopup")))
  ;; Check interface properties
  (is (equal '("autohide" "parent")
             (glib-test:list-interface-properties "GdkPopup")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GdkPopup")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "GdkPopup" GDK:POPUP
                            (:EXPORT T
                             :TYPE-INITIALIZER "gdk_popup_get_type")
                            (AUTOHIDE POPUP-AUTOHIDE "autohide" "gboolean" T NIL)
                            (PARENT POPUP-PARENT "parent" "GdkSurface" T NIL))
             (gobject:get-gtype-definition "GdkPopup"))))

;;; --- Properties -------------------------------------------------------------

;;;     autohide
;;;     parent

;;; --- Functions --------------------------------------------------------------

;;;     gdk_popup_present
;;;     gdk_popup_get_surface_anchor
;;;     gdk_popup_get_rect_anchor
;;;     gdk_popup_get_position_x
;;;     gdk_popup_get_position_y

;;; 2024-9-18
