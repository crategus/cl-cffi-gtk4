(in-package :gtk-test)

(def-suite gsk-rounded-rect :in gsk-suite)
(in-suite gsk-rounded-rect)

;;; Types and Values

;;;     GskCorner

(test gsk-corner
  ;; Check the type
  (is (g:type-is-enum "GskCorner"))
  ;; Check the type initializer
  (is (eq (g:gtype "GskCorner")
          (g:gtype (cffi:foreign-funcall "gsk_corner_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gsk:corner
          (glib:symbol-for-gtype "GskCorner")))
  ;; Check names
  (is (equal '("GSK_CORNER_TOP_LEFT" "GSK_CORNER_TOP_RIGHT"
               "GSK_CORNER_BOTTOM_RIGHT" "GSK_CORNER_BOTTOM_LEFT")
             (gtk-test:list-enum-item-name "GskCorner")))
  ;; Check the values
  (is (equal '(0 1 2 3)
             (gtk-test:list-enum-item-value "GskCorner")))
  ;; Check the nick names
  (is (equal '("top-left" "top-right" "bottom-right" "bottom-left")
             (gtk-test:list-enum-item-nick "GskCorner")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GskCorner" GSK-CORNER
                                     (:EXPORT T
                                      :TYPE-INITIALIZER "gsk_corner_get_type")
                                     (:TOP-LEFT 0)
                                     (:TOP-RIGHT 1)
                                     (:BOTTOM-RIGHT 2)
                                     (:BOTTOM-LEFT 3))
             (gobject:get-g-type-definition "GskCorner"))))

;;;     GskRoundedRect

(test gsk-rounded-rect-structure
  (cffi:with-foreign-object (rect '(:struct gsk:rounded-rect))
    (let (bounds corner)
      ;; The structure holds 12 float values of size 4 Bytes
      (is (= 48 (cffi:foreign-type-size '(:struct gsk:rounded-rect))))
      ;; Get the pointer to the BOUNDS slot
      (is (cffi:pointerp (setf bounds
                               (gsk:rounded-rect-bounds rect))))
      (graphene:rect-init-from-rect bounds (graphene:rect-zero))
      (is (= 0 (graphene:rect-x bounds)))
      (is (= 0 (graphene:rect-y bounds)))
      (is (= 0 (graphene:rect-width bounds)))
      (is (= 0 (graphene:rect-height bounds)))
      ;; Get the pointer to the first corner
      (is (cffi:pointerp (setf corner
                               (gsk:rounded-rect-corner rect 0))))
      (graphene:size-init corner 0 0)
      (is (= 0 (graphene:size-width corner)))
      (is (= 0 (graphene:size-height corner)))
      ;; Get the pointer to the second corner
      (is (cffi:pointerp (setf corner
                               (gsk:rounded-rect-corner rect 1))))
      (graphene:size-init corner 1 1)
      (is (= 1 (graphene:size-width corner)))
      (is (= 1 (graphene:size-height corner)))
      ;; Get the pointer to the third corner
      (is (cffi:pointerp (setf corner
                               (gsk:rounded-rect-corner rect 2))))
      (graphene:size-init corner 2 2)
      (is (= 2 (graphene:size-width corner)))
      (is (= 2 (graphene:size-height corner)))
      ;; Get the pointer to the fourth corner
      (is (cffi:pointerp (setf corner
                               (gsk:rounded-rect-corner rect 3))))
      (graphene:size-init corner 3 3)
      (is (= 3 (graphene:size-width corner)))
      (is (= 3 (graphene:size-height corner))))))

;;; --- Functions --------------------------------------------------------------

;;;     gsk_rounded_rect_init

(test gsk-rounded-rect-init.1
  (cffi:with-foreign-object (rect '(:struct gsk:rounded-rect))
    (graphene:with-objects ((bounds graphene:rect-t 0 10 20 30)
                            (top-left graphene:size-t 0.1 0.1)
                            (top-right graphene:size-t 0.2 0.2)
                            (bottom-right graphene:size-t 0.3 0.3)
                            (bottom-left graphene:size-t 0.4 0.4))
      (setf rect
            (gsk:rounded-rect-init rect bounds top-left
                                               top-right
                                               bottom-right
                                               bottom-left))
      ;; Get the values of the bounds rectangle
      (is (=  0.0 (graphene:rect-x (gsk:rounded-rect-bounds rect))))
      (is (= 10.0 (graphene:rect-y (gsk:rounded-rect-bounds rect))))
      (is (= 20.0 (graphene:rect-width (gsk:rounded-rect-bounds rect))))
      (is (= 30.0 (graphene:rect-height (gsk:rounded-rect-bounds rect))))
      ;; Get the values of the first corner
      (is (= 0.1 (graphene:size-width (gsk:rounded-rect-corner rect 0))))
      (is (= 0.1 (graphene:size-height (gsk:rounded-rect-corner rect 0))))
      ;; Get the values of the second corner
      (is (= 0.2 (graphene:size-width (gsk:rounded-rect-corner rect 1))))
      (is (= 0.2 (graphene:size-height (gsk:rounded-rect-corner rect 1))))
      ;; Get the values of the third corner
      (is (= 0.3 (graphene:size-width (gsk:rounded-rect-corner rect 2))))
      (is (= 0.3 (graphene:size-height (gsk:rounded-rect-corner rect 2))))
      ;; Get the values of the fourth corner
      (is (= 0.4 (graphene:size-width (gsk:rounded-rect-corner rect 3))))
      (is (= 0.4 (graphene:size-height (gsk:rounded-rect-corner rect 3)))))))

(test gsk-rounded-rect-init.2
  (cffi:with-foreign-object (rect '(:struct gsk:rounded-rect))
    (graphene:with-objects ((bounds graphene:rect-t 0 10 20 30)
                            (size1 graphene:size-t)
                            (size2 graphene:size-t)
                            (size3 graphene:size-t)
                            (size4 graphene:size-t))
      (setf rect
            (gsk:rounded-rect-init rect
                                   bounds
                                   (graphene:size-init size1 0.1 0.1)
                                   (graphene:size-init size2 0.2 0.2)
                                   (graphene:size-init size3 0.3 0.3)
                                   (graphene:size-init size4 0.4 0.4)))
      ;; Get the values of the bounds rectangle
      (is (=  0.0 (graphene:rect-x (gsk:rounded-rect-bounds rect))))
      (is (= 10.0 (graphene:rect-y (gsk:rounded-rect-bounds rect))))
      (is (= 20.0 (graphene:rect-width (gsk:rounded-rect-bounds rect))))
      (is (= 30.0 (graphene:rect-height (gsk:rounded-rect-bounds rect))))
      ;; Get the values of the first corner
      (is (= 0.1 (graphene:size-width (gsk:rounded-rect-corner rect 0))))
      (is (= 0.1 (graphene:size-height (gsk:rounded-rect-corner rect 0))))
      ;; Get the values of the second corner
      (is (= 0.2 (graphene:size-width (gsk:rounded-rect-corner rect 1))))
      (is (= 0.2 (graphene:size-height (gsk:rounded-rect-corner rect 1))))
      ;; Get the values of the third corner
      (is (= 0.3 (graphene:size-width (gsk:rounded-rect-corner rect 2))))
      (is (= 0.3 (graphene:size-height (gsk:rounded-rect-corner rect 2))))
      ;; Get the values of the fourth corner
      (is (= 0.4 (graphene:size-width (gsk:rounded-rect-corner rect 3))))
      (is (= 0.4 (graphene:size-height (gsk:rounded-rect-corner rect 3)))))))

;;;     gsk_rounded_rect_init_copy

(test gsk-rounded-rect-init-copy
  (cffi:with-foreign-objects ((rect '(:struct gsk:rounded-rect))
                              (src '(:struct gsk:rounded-rect)))
    (graphene:with-objects ((bounds graphene:rect-t 0 10 20 30)
                            (top-left graphene:size-t 0.1 0.1)
                            (top-right graphene:size-t 0.2 0.2)
                            (bottom-right graphene:size-t 0.3 0.3)
                            (bottom-left graphene:size-t 0.4 0.4))
      ;; Initialize src
      (setf src
            (gsk:rounded-rect-init rect bounds top-left
                                               top-right
                                               bottom-right
                                               bottom-left))
      ;; Initialize rect from src
      (setf rect (gsk:rounded-rect-init-copy rect src))
      ;; Get the values of the bounds rectangle
      (is (=  0.0 (graphene:rect-x (gsk:rounded-rect-bounds rect))))
      (is (= 10.0 (graphene:rect-y (gsk:rounded-rect-bounds rect))))
      (is (= 20.0 (graphene:rect-width (gsk:rounded-rect-bounds rect))))
      (is (= 30.0 (graphene:rect-height (gsk:rounded-rect-bounds rect))))
      ;; Get the values of the first corner
      (is (= 0.1 (graphene:size-width (gsk:rounded-rect-corner rect 0))))
      (is (= 0.1 (graphene:size-height (gsk:rounded-rect-corner rect 0))))
      ;; Get the values of the second corner
      (is (= 0.2 (graphene:size-width (gsk:rounded-rect-corner rect 1))))
      (is (= 0.2 (graphene:size-height (gsk:rounded-rect-corner rect 1))))
      ;; Get the values of the third corner
      (is (= 0.3 (graphene:size-width (gsk:rounded-rect-corner rect 2))))
      (is (= 0.3 (graphene:size-height (gsk:rounded-rect-corner rect 2))))
      ;; Get the values of the fourth corner
      (is (= 0.4 (graphene:size-width (gsk:rounded-rect-corner rect 3))))
      (is (= 0.4 (graphene:size-height (gsk:rounded-rect-corner rect 3)))))))

;;;     gsk_rounded_rect_init_from_rect

(test gsk-rounded-rect-init-from-rect
  (cffi:with-foreign-object (rect '(:struct gsk:rounded-rect))
    (graphene:with-object (bounds graphene:rect-t 0 10 20 30)
      (setf rect (gsk:rounded-rect-init-from-rect rect bounds 2))
      ;; Get the values of the bounds rectangle
      (is (=  0.0 (graphene:rect-x (gsk:rounded-rect-bounds rect))))
      (is (= 10.0 (graphene:rect-y (gsk:rounded-rect-bounds rect))))
      (is (= 20.0 (graphene:rect-width (gsk:rounded-rect-bounds rect))))
      (is (= 30.0 (graphene:rect-height (gsk:rounded-rect-bounds rect))))
      ;; Get the values of the first corner
      (is (= 2.0 (graphene:size-width (gsk:rounded-rect-corner rect 0))))
      (is (= 2.0 (graphene:size-height (gsk:rounded-rect-corner rect 0))))
      ;; Get the values of the second corner
      (is (= 2.0 (graphene:size-width (gsk:rounded-rect-corner rect 1))))
      (is (= 2.0 (graphene:size-height (gsk:rounded-rect-corner rect 1))))
      ;; Get the values of the third corner
      (is (= 2.0 (graphene:size-width (gsk:rounded-rect-corner rect 2))))
      (is (= 2.0 (graphene:size-height (gsk:rounded-rect-corner rect 2))))
      ;; Get the values of the fourth corner
      (is (= 2.0 (graphene:size-width (gsk:rounded-rect-corner rect 3))))
      (is (= 2.0 (graphene:size-height (gsk:rounded-rect-corner rect 3)))))))

;;;     gsk_rounded_rect_normalize

;; TODO: Find an example

#+nil
(test gsk-rounded-rect-normalize
  (cffi:with-foreign-object (rect '(:struct gsk:rounded-rect))
    (graphene:with-object (bounds graphene:rect-t -20 -10 0 10)
      (setf rect (gsk:rounded-rect-init-from-rect rect bounds 2))
      (setf rect (gsk:rounded-rect-normalize rect))
      ;; Get the values of the bounds rectangle
      (is (=  0.0 (graphene:rect-x (gsk:rounded-rect-bounds rect))))
      (is (= 10.0 (graphene:rect-y (gsk:rounded-rect-bounds rect))))
      (is (= 20.0 (graphene:rect-width (gsk:rounded-rect-bounds rect))))
      (is (= 30.0 (graphene:rect-height (gsk:rounded-rect-bounds rect))))
      ;; Get the values of the first corner
      (is (= 2.0 (graphene:size-width (gsk:rounded-rect-corner rect 0))))
      (is (= 2.0 (graphene:size-height (gsk:rounded-rect-corner rect 0))))
      ;; Get the values of the second corner
      (is (= 2.0 (graphene:size-width (gsk:rounded-rect-corner rect 1))))
      (is (= 2.0 (graphene:size-height (gsk:rounded-rect-corner rect 1))))
      ;; Get the values of the third corner
      (is (= 2.0 (graphene:size-width (gsk:rounded-rect-corner rect 2))))
      (is (= 2.0 (graphene:size-height (gsk:rounded-rect-corner rect 2))))
      ;; Get the values of the fourth corner
      (is (= 2.0 (graphene:size-width (gsk:rounded-rect-corner rect 3))))
      (is (= 2.0 (graphene:size-height (gsk:rounded-rect-corner rect 3)))))))

;;;     gsk_rounded_rect_offset
;;;     gsk_rounded_rect_shrink
;;;     gsk_rounded_rect_is_rectilinear
;;;     gsk_rounded_rect_contains_point
;;;     gsk_rounded_rect_contains_rect
;;;     gsk_rounded_rect_intersects_rect

;;; 2024-7-4
