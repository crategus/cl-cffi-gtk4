(in-package :gtk-test)

(def-suite gsk-rounded-rect :in gsk-suite)
(in-suite gsk-rounded-rect)

;;; Types and Values

;;;     GskCorner

;;;     GskRoundedRect

(test gsk-rounded-rect-structure
  (cffi:with-foreign-object (rect '(:struct gsk:rounded-rect))

    (let (bounds corner size)

      (is (= 48 (setf size
                      (cffi:foreign-type-size '(:struct gsk:rounded-rect)))))

      (is (cffi:pointerp (setf bounds
                               (gsk:rounded-rect-bounds rect))))

      (graphene:rect-init-from-rect rect (graphene:rect-zero))

      (is (= 0 (graphene:rect-x bounds)))
      (is (= 0 (graphene:rect-y bounds)))
      (is (= 0 (graphene:rect-width bounds)))
      (is (= 0 (graphene:rect-height bounds)))

      (is (cffi:pointerp (setf corner
                               (gsk:rounded-rect-corner rect 0))))

      (graphene:size-init corner 0 0)
      (is (= 0 (graphene:size-width corner)))
      (is (= 0 (graphene:size-height corner)))

      (is (cffi:pointerp (setf corner
                               (gsk:rounded-rect-corner rect 1))))

      (graphene:size-init corner 1 1)
      (is (= 1 (graphene:size-width corner)))
      (is (= 1 (graphene:size-height corner)))

      (is (cffi:pointerp (setf corner
                               (gsk:rounded-rect-corner rect 2))))

      (graphene:size-init corner 2 2)
      (is (= 2 (graphene:size-width corner)))
      (is (= 2 (graphene:size-height corner)))

      (is (cffi:pointerp (setf corner
                               (gsk:rounded-rect-corner rect 3))))

      (graphene:size-init corner 3 3)
      (is (= 3 (graphene:size-width corner)))
      (is (= 3 (graphene:size-height corner)))

)))

;;; --- Functions --------------------------------------------------------------

;;;     GSK_ROUNDED_RECT_INIT()

;;;     gsk_rounded_rect_init

(test gsk-rounded-rect-init

  (cffi:with-foreign-object (rect '(:struct gsk:rounded-rect))

    (graphene:with-graphene-rect (bounds 0 10 20 30)
      (graphene:with-graphene-sizes ((top-left 0.1 0.1)
                                     (top-right 0.2 0.2)
                                     (bottom-right 0.3 0.3)
                                     (bottom-left 0.4 0.4))

      (setf rect
            (gsk:rounded-rect-init rect bounds top-left
                                               top-right
                                               bottom-right
                                               bottom-left))

      (is (= 00.0 (graphene:rect-x (gsk:rounded-rect-bounds rect))))
      (is (= 10.0 (graphene:rect-y (gsk:rounded-rect-bounds rect))))
      (is (= 20.0 (graphene:rect-width (gsk:rounded-rect-bounds rect))))
      (is (= 30.0 (graphene:rect-height (gsk:rounded-rect-bounds rect))))

      (is (= 0.1 (graphene:size-width (gsk:rounded-rect-corner rect 0))))
      (is (= 0.1 (graphene:size-height (gsk:rounded-rect-corner rect 0))))

      (is (= 0.2 (graphene:size-width (gsk:rounded-rect-corner rect 1))))
      (is (= 0.2 (graphene:size-height (gsk:rounded-rect-corner rect 1))))

      (is (= 0.3 (graphene:size-width (gsk:rounded-rect-corner rect 2))))
      (is (= 0.3 (graphene:size-height (gsk:rounded-rect-corner rect 2))))

      (is (= 0.4 (graphene:size-width (gsk:rounded-rect-corner rect 3))))
      (is (= 0.4 (graphene:size-height (gsk:rounded-rect-corner rect 3))))

))))

;;;     gsk_rounded_rect_init_copy
;;;     gsk_rounded_rect_init_from_rect
;;;     gsk_rounded_rect_normalize
;;;     gsk_rounded_rect_offset
;;;     gsk_rounded_rect_shrink
;;;     gsk_rounded_rect_is_rectilinear
;;;     gsk_rounded_rect_contains_point
;;;     gsk_rounded_rect_contains_rect
;;;     gsk_rounded_rect_intersects_rect

;;; --- 2023-9-22 --------------------------------------------------------------
