(in-package :gtk-test)

(def-suite gdk-rectangle :in gdk-suite)
(in-suite gdk-rectangle)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkRectangle

(test gdk-rectangle-boxed
  ;; Check type
  (is (g:type-is-boxed "GdkRectangle"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkRectangle")
          (g:gtype (cffi:foreign-funcall "gdk_rectangle_get_type" :size))))
  ;; Check registered name
  (is (eq 'gdk:rectangle
          (glib:symbol-for-gtype "GdkRectangle"))))

(test gdk-rectangle-properties
  (let ((rect (make-instance 'gdk:rectangle)))
    (is (= 10 (setf (gdk:rectangle-x rect) 10)))
    (is (= 10 (gdk:rectangle-x rect)))
    (is (= 20 (setf (gdk:rectangle-y rect) 20)))
    (is (= 20 (gdk:rectangle-y rect)))
    (is (= 30 (setf (gdk:rectangle-width rect) 30)))
    (is (= 30 (gdk:rectangle-width rect)))
    (is (= 40 (setf (gdk:rectangle-height rect) 40)))
    (is (= 40 (gdk:rectangle-height rect)))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk:rectangle-new

(test gdk-rectangle-new
  (let ((rect (gdk:rectangle-new :x 10 :y 20 :width 30 :height 40)))
    (is (= 10 (gdk:rectangle-x rect)))
    (is (= 20 (gdk:rectangle-y rect)))
    (is (= 30 (gdk:rectangle-width rect)))
    (is (= 40 (gdk:rectangle-height rect)))))

;;;     gdk:rectangle-copy

(test gdk-rectangle-copy
  (let* ((rect1 (gdk:rectangle-new :x 10 :y 20 :width 30 :height 40))
         (rect2 (gdk:rectangle-copy rect1)))
    (is (= 10 (gdk:rectangle-x rect2)))
    (is (= 20 (gdk:rectangle-y rect2)))
    (is (= 30 (gdk:rectangle-width rect2)))
    (is (= 40 (gdk:rectangle-height rect2)))))

;;;     gdk_rectangle_contains_point

(test gdk-rectangle-contains-point
  (let ((rect (gdk:rectangle-new :x 10 :y 10 :width 20 :height 20)))
    (is-true (gdk:rectangle-contains-point rect 15 15))
    (is-false (gdk:rectangle-contains-point rect 0 0))))

;;;     gdk_rectangle_equal

(test gdk-rectangle-equal
  (let* ((rect1 (gdk:rectangle-new :x 10 :y 10 :width 20 :height 20))
         (rect2 (gdk:rectangle-new :x 10 :y 10 :width 20 :height 20))
         (rect3 (gdk:rectangle-copy rect1))
         (rect4 (gdk:rectangle-new :x 20 :y 20 :width 25 :height 25)))
    (is-true (gdk:rectangle-equal rect1 rect2))
    (is-true (gdk:rectangle-equal rect1 rect3))
    (is-false (gdk:rectangle-equal rect1 rect4))))

;;;     gdk_rectangle_intersect

(test gdk-rectangle-intersect
  (let ((rect1 (gdk:rectangle-new :x 10 :y 10 :width 10 :height 10))
        (rect2 (gdk:rectangle-new :x 15 :y 15 :width 10 :height 10))
        (rect3 (gdk:rectangle-new :x 20 :y 20 :width 10 :height 10)))
    (is (gdk:rectangle-equal (gdk:rectangle-new :x 15 :y 15 :width 5 :height 5)
                             (gdk:rectangle-intersect rect1 rect2)))
    (is (gdk:rectangle-equal (gdk:rectangle-new :x 15 :y 15 :width 5 :height 5)
                             (gdk:rectangle-intersect rect2 rect1)))
    (is-false (gdk:rectangle-intersect rect1 rect3))))

;;;     gdk_rectangle_union

(test gdk-rectangle-union
  (let ((rect1 (gdk:rectangle-new :x 10 :y 10 :width 10 :height 10))
        (rect2 (gdk:rectangle-new :x 15 :y 15 :width 10 :height 10))
        (rect3 (gdk:rectangle-new :x 15 :y 15 :width 0 :height 0)))
    (is (gdk:rectangle-equal (gdk:rectangle-new :x 10 :y 10 :width 15 :height 15)
                             (gdk:rectangle-union rect1 rect2)))
    (is (gdk:rectangle-equal (gdk:rectangle-new :x 10 :y 10 :width 15 :height 15)
                             (gdk:rectangle-union rect2 rect1)))
    (is (gdk:rectangle-equal (gdk:rectangle-new :x 10 :y 10 :width 10 :height 10)
                             (gdk:rectangle-union rect1 rect3)))))

;;; 2024-07-04
