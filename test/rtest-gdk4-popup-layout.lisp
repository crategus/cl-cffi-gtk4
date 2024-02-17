(in-package :gtk-test)

(def-suite gdk-popup-layout :in gdk-suite)
(in-suite gdk-popup-layout)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkAnchorHints

(test gdk-anchor-hints
  ;; Check the type
  (is (g:type-is-flags "GdkAnchorHints"))
  ;; Check the registered name
  (is (eq 'gdk:anchor-hints
          (glib:symbol-for-gtype "GdkAnchorHints")))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkAnchorHints")
          (g:gtype (cffi:foreign-funcall "gdk_anchor_hints_get_type" :size))))
  ;; Check the names
  (is (equal '("GDK_ANCHOR_FLIP_X" "GDK_ANCHOR_FLIP_Y" "GDK_ANCHOR_SLIDE_X"
               "GDK_ANCHOR_SLIDE_Y" "GDK_ANCHOR_RESIZE_X" "GDK_ANCHOR_RESIZE_Y"
               "GDK_ANCHOR_FLIP" "GDK_ANCHOR_SLIDE" "GDK_ANCHOR_RESIZE")
             (list-flags-item-name "GdkAnchorHints")))
  ;; Check the values
  (is (equal '(1 2 4 8 16 32 3 12 48)
             (list-flags-item-value "GdkAnchorHints")))
  ;; Check the nick names
  (is (equal '("flip-x" "flip-y" "slide-x" "slide-y" "resize-x" "resize-y"
               "flip" "slide" "resize")
             (list-flags-item-nick "GdkAnchorHints")))
  ;; Check the flags definition
  (is (equal '(GOBJECT:DEFINE-G-FLAGS "GdkAnchorHints" GDK-ANCHOR-HINTS
                                      (:EXPORT T
                                       :TYPE-INITIALIZER
                                       "gdk_anchor_hints_get_type")
                                      (:FLIP-X 1)
                                      (:FLIP-Y 2)
                                      (:SLIDE-X 4)
                                      (:SLIDE-Y 8)
                                      (:RESIZE-X 16)
                                      (:RESIZE-Y 32)
                                      (:FLIP 3)
                                      (:SLIDE 12)
                                      (:RESIZE 48))
             (gobject:get-g-type-definition "GdkAnchorHints"))))

;;;     GdkPopupLayout

(test gdk-popup-layout-boxed
  ;; Type check
  (is (g:type-is-a (g:gtype "GdkPopupLayout") g:+g-type-boxed+))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkPopupLayout")
          (g:gtype (cffi:foreign-funcall "gdk_popup_layout_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gdk:popup-layout
          (glib:symbol-for-gtype "GdkPopupLayout"))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk_popup_layout_new

(test gdk-popup-layout-new
  (let ((layout (gdk:popup-layout-new (gdk:rectangle-new) :north :west)))
    (is (gdk:rectangle-equal (gdk:rectangle-new)
                             (gdk:popup-layout-anchor-rect layout)))
    (is (eq :north (gdk:popup-layout-rect-anchor layout)))
    (is (eq :west (gdk:popup-layout-surface-anchor layout)))
    (is-false (gdk:popup-layout-anchor-hints layout))
    (is (equal '(0 0)
               (multiple-value-list (gdk:popup-layout-offset layout))))
    (is (equal '(0 0 0 0)
               (multiple-value-list (gdk:popup-layout-shadow-width layout))))))

;;;     gdk_popup_layout_ref
;;;     gdk_popup_layout_unref

;;;     gdk_popup_layout_copy
;;;     gdk_popup_layout_equal

(test gdk-popup-layout-copy/equal
  (let* ((layout1 (gdk:popup-layout-new (gdk:rectangle-new) :north :west))
         (layout2 (gdk:popup-layout-copy layout1)))
    (is (gdk:popup-layout-equal layout1 layout2))))

;;;     gdk_popup_layout_set_anchor_rect
;;;     gdk_popup_layout_get_anchor_rect

(test gdk-popup-layout-anchor-rect
  (let ((layout (gdk:popup-layout-new (gdk:rectangle-new) :north :west)))
    (setf (gdk:popup-layout-anchor-rect layout)
          (gdk:rectangle-new :width 100 :height 200))
    (is (gdk:rectangle-equal (gdk:rectangle-new :width 100 :height 200)
                             (gdk:popup-layout-anchor-rect layout)))))

;;;     gdk_popup_layout_set_rect_anchor
;;;     gdk_popup_layout_get_rect_anchor

(test gdk-popup-layout-rect-anchor
  (let ((layout (gdk:popup-layout-new (gdk:rectangle-new) :north :west)))
    (is (eq :south (setf (gdk:popup-layout-rect-anchor layout) :south)))
    (is (eq :south (gdk:popup-layout-rect-anchor layout)))))

;;;     gdk_popup_layout_set_surface_anchor
;;;     gdk_popup_layout_get_surface_anchor

(test gdk-popup-layout-surface-anchor
  (let ((layout (gdk:popup-layout-new (gdk:rectangle-new) :north :west)))
    (is (eq :south (setf (gdk:popup-layout-surface-anchor layout) :south)))
    (is (eq :south (gdk:popup-layout-surface-anchor layout)))))

;;;     gdk_popup_layout_set_anchor_hints
;;;     gdk_popup_layout_get_anchor_hints

(test gdk-popup-layout-anchor-hints
  (let ((layout (gdk:popup-layout-new (gdk:rectangle-new) :north :west)))
    (is (eq :flip (setf (gdk:popup-layout-anchor-hints layout) :flip)))
    (is (equal '(:flip-x :flip-y) (gdk:popup-layout-anchor-hints layout)))))

;;;     gdk_popup_layout_set_offset
;;;     gdk_popup_layout_get_offset

(test gdk-popup-layout-offset
  (let ((layout (gdk:popup-layout-new (gdk:rectangle-new) :north :west)))
    (is (equal '(1 2)
               (multiple-value-list (setf (gdk:popup-layout-offset layout)
                                          '(1 2)))))
    (is (equal '(1 2)
               (multiple-value-list (gdk:popup-layout-offset layout))))))

;;;     gdk_popup_layout_set_shadow_width                  Since 4.2
;;;     gdk_popup_layout_get_shadow_width                  Since 4.2

(test gdk-popup-layout-shadow-width
  (let ((layout (gdk:popup-layout-new (gdk:rectangle-new) :north :west)))
    (is (equal '(1 2 3 4)
               (multiple-value-list (setf (gdk:popup-layout-shadow-width layout)
                                          '(1 2 3 4)))))
    (is (equal '(1 2 3 4)
               (multiple-value-list (gdk:popup-layout-shadow-width layout))))))

;;; 2024-2-17
