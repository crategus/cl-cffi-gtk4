(in-package :gtk-test)

(def-suite gtk-bitset :in gtk-suite)
(in-suite gtk-bitset)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkBitsetIter

;;;     GtkBitset

(test gtk-bitset-boxed
  ;; Check type
  (is (g:type-is-boxed "GtkBitset"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkBitset")
          (g:gtype (cffi:foreign-funcall "gtk_bitset_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:bitset
          (glib:symbol-for-gtype "GtkBitset"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_bitset_new_empty

(test gtk-bitset-new-empty
  (is (typep (gtk:bitset-new-empty) 'gtk:bitset)))

;;;     gtk_bitset_new_range
;;;     gtk_bitset_contains

(test gtk-bitset-new-range
  (let ((bitset (gtk:bitset-new-range 100 100)))
    (is (typep bitset 'gtk:bitset))
    (is-false (gtk:bitset-contains bitset 99))
    (is-true  (gtk:bitset-contains bitset 100))
    (is-true  (gtk:bitset-contains bitset 150))
    (is-true  (gtk:bitset-contains bitset 199))
    (is-false (gtk:bitset-contains bitset 200))))

;;;     gtk_bitset_copy
;;;     gtk_bitset_equals

(test gtk-bitset-copy
  (let* ((bitset1 (gtk:bitset-new-range 100 99))
         (bitset2 (gtk:bitset-new-range 100 49))
         (bitset3 (gtk:bitset-copy bitset2)))
    (is (gtk:bitset-equals bitset1 bitset1))
    (is (gtk:bitset-equals bitset2 bitset2))
    (is (gtk:bitset-equals bitset3 bitset3))
    (is (gtk:bitset-equals bitset2 bitset3))
    (is (not (gtk:bitset-equals bitset1 bitset2)))
    (is (not (gtk:bitset-equals bitset1 bitset3)))))

;;;     gtk_bitset_is_empty

(test gtk-bitset-is-empty
  (is-true (gtk:bitset-is-empty (gtk:bitset-new-empty)))
  (is-false (gtk:bitset-is-empty (gtk:bitset-new-range 100 99))))

;;;     gtk_bitset_get_size_in_range

(test gtk-bitset-size-in-range
  (let ((bitset (gtk:bitset-new-range 100 99)))
    (is (= 10 (gtk:bitset-size-in-range bitset 100 109)))
    (is (= 10 (gtk:bitset-size-in-range bitset 110 119)))
    (is (= 10 (gtk:bitset-size-in-range bitset 120 129)))
    (is (= 30 (gtk:bitset-size-in-range bitset 100 129)))))

;;;     gtk_bitset_remove_all

(test gtk-bitset-remove-all
  (let ((bitset (gtk:bitset-new-range 100 99)))
    (is-false (gtk:bitset-is-empty bitset))
    (is-false (gtk:bitset-remove-all bitset))
    (is-true (gtk:bitset-is-empty bitset))))

;;;     gtk_bitset_add
;;;     gtk_bitset_get_minimum
;;;     gtk_bitset_get_maximum
;;;     gtk_bitset_get_size
;;;     gtk_bitset_get_nth

(test gtk-bitset-add.1
  (let ((bitset (gtk:bitset-new-empty))
        (values '(1 3 5 7 9 11 13 15 16 17 18 19)))
    (is-true (gtk:bitset-is-empty bitset))
    (dolist (value values)
      (is-true (gtk:bitset-add bitset value)))
    (is-false (gtk:bitset-is-empty bitset))
    (is (=  1 (gtk:bitset-minimum bitset)))
    (is (= 19 (gtk:bitset-maximum bitset)))
    (is (= 12 (gtk:bitset-size bitset)))
    (is (= 1 (gtk:bitset-nth bitset 0)))
    (is (= 3 (gtk:bitset-nth bitset 1)))
    (is (= 5 (gtk:bitset-nth bitset 2)))
    (is (equal '(1 3 5 7 9 11 13 15 16 17 18 19 0 0)
               (mapcar (lambda (x)
                         (gtk:bitset-nth bitset x))
                       '( 0 1 2 3 4 5 6 7 8 9 10 11 12 13))))))

(test gtk-bitset-add.2
  (let ((bitset (gtk:bitset-new-empty))
        (values '(19 18 17 16 15 13 11 9 7 5 3 1)))
    (is-true (gtk:bitset-is-empty bitset))
    (dolist (value values)
      (is-true (gtk:bitset-add bitset value)))
    (is-false (gtk:bitset-is-empty bitset))
    (is (=  1 (gtk:bitset-minimum bitset)))
    (is (= 19 (gtk:bitset-maximum bitset)))
    (is (= 12 (gtk:bitset-size bitset)))
    (is (= 1 (gtk:bitset-nth bitset 0)))
    (is (= 3 (gtk:bitset-nth bitset 1)))
    (is (= 5 (gtk:bitset-nth bitset 2)))
    (is (equal '(1 3 5 7 9 11 13 15 16 17 18 19 0 0)
               (mapcar (lambda (x)
                         (gtk:bitset-nth bitset x))
                       '( 0 1 2 3 4 5 6 7 8 9 10 11 12 13))))))

(test gtk-bitset-add.3
  (let ((bitset (gtk:bitset-new-empty))
        (values '(18 17 19 16 13 11 15 9 5 3 7 1)))
    (is-true (gtk:bitset-is-empty bitset))
    (dolist (value values)
      (is-true (gtk:bitset-add bitset value)))
    (is-false (gtk:bitset-is-empty bitset))
    (is (=  1 (gtk:bitset-minimum bitset)))
    (is (= 19 (gtk:bitset-maximum bitset)))
    (is (= 12 (gtk:bitset-size bitset)))
    (is (= 1 (gtk:bitset-nth bitset 0)))
    (is (= 3 (gtk:bitset-nth bitset 1)))
    (is (= 5 (gtk:bitset-nth bitset 2)))
    (is (equal '(1 3 5 7 9 11 13 15 16 17 18 19 0 0)
               (mapcar (lambda (x)
                         (gtk:bitset-nth bitset x))
                       '( 0 1 2 3 4 5 6 7 8 9 10 11 12 13))))))

;;;     gtk_bitset_remove

(test gtk-bitset-remove
  (let ((bitset (gtk:bitset-new-empty))
        (values '(18 17 19 16 13 11 15 9 5 3 7 1)))
    (is-true (gtk:bitset-is-empty bitset))
    (dolist (value values)
      (is-true (gtk:bitset-add bitset value)))
    (is-false (gtk:bitset-is-empty bitset))
    (is-true (gtk:bitset-remove bitset 1))
    (is-true (gtk:bitset-remove bitset 3))
    (is-true (gtk:bitset-remove bitset 5))
    (is (equal '(7 9 11 13 15 16 17 18 19 0 0 0 0 0)
               (mapcar (lambda (x)
                         (gtk:bitset-nth bitset x))
                       '( 0 1 2 3 4 5 6 7 8 9 10 11 12 13))))))

;;;     gtk_bitset_add_range
;;;     gtk_bitset_remove_range

(test gtk-bitset-add/remove-range
  (let ((bitset (gtk:bitset-new-empty)))
    (is-false (gtk:bitset-add-range bitset 1000 2000))
    (is (= 1000 (gtk:bitset-minimum bitset)))
    (is (= 2999 (gtk:bitset-maximum bitset)))
    (is (= 2000 (gtk:bitset-size bitset)))
    (is-false (gtk:bitset-remove-range bitset 2000 500))
    (is (= 1500 (gtk:bitset-size bitset)))
    (is (= 1000 (gtk:bitset-minimum bitset)))
    (is (= 2999 (gtk:bitset-maximum bitset)))))

;;;     gtk_bitset_add_range_closed
;;;     gtk_bitset_remove_range_closed

(test gtk-bitset-add/remove-range-closed
  (let ((bitset (gtk:bitset-new-empty)))
    (is-false (gtk:bitset-add-range-closed bitset 1000 2999))
    (is (= 1000 (gtk:bitset-minimum bitset)))
    (is (= 2999 (gtk:bitset-maximum bitset)))
    (is (= 2000 (gtk:bitset-size bitset)))
    (is-false (gtk:bitset-remove-range-closed bitset 2000 2499))
    (is (= 1500 (gtk:bitset-size bitset)))
    (is (= 1000 (gtk:bitset-minimum bitset)))
    (is (= 2999 (gtk:bitset-maximum bitset)))))

;;;     gtk_bitset_add_rectangle
;;;     gtk_bitset_remove_rectangle

(test gtk-bitset-add/remove-rectangle.1
  (let ((bitset (gtk:bitset-new-empty)))
    (is-false (gtk:bitset-add-rectangle bitset 0 100 100 100))
    (is-false (gtk:bitset-is-empty bitset))
    (is (=     0 (gtk:bitset-minimum bitset)))
    (is (=  9999 (gtk:bitset-maximum bitset)))
    (is (= 10000 (gtk:bitset-size bitset)))
    (is-false (gtk:bitset-remove-rectangle bitset 0 50 50 50))
    (is (= 2500 (gtk:bitset-minimum bitset)))
    (is (= 9999 (gtk:bitset-maximum bitset)))
    (is (= 7500 (gtk:bitset-size bitset)))))

(test gtk-bitset-add/remove-rectangle.2
    (let ((bitset1 (gtk:bitset-new-empty))
          (bitset2 (gtk:bitset-new-empty))
          (start1 0) (width1 100) (height1 100) (stride1 100)
          (start2 0) (width2  50) (height2  50) (stride2  50))
      (gtk:bitset-add-rectangle bitset1 start1 width1 height1 stride1)
      (dotimes (i height1)
        (gtk:bitset-add-range bitset2 (+ (* i stride1) start1) width1))
      (is-true (gtk:bitset-equals bitset1 bitset2))
      (gtk:bitset-remove-rectangle bitset1 start2 width2 height2 stride2)
      (dotimes (i height2)
        (gtk:bitset-remove-range bitset2 (+ (* i stride2) start2) width2))
      (is-true (gtk:bitset-equals bitset1 bitset2))))

;;;     gtk_bitset_union

(test gtk-bitset-union
  (let ((bitset1 (gtk:bitset-new-range 0 100))
        (bitset2 (gtk:bitset-new-range 100 100)))
    (is-false (gtk:bitset-union bitset1 bitset2))
    (is (=   0 (gtk:bitset-minimum bitset1)))
    (is (= 199 (gtk:bitset-maximum bitset1)))
    (is (= 200 (gtk:bitset-size bitset1)))))

;;;     gtk_bitset_intersect

(test gtk-bitset-intersect
  (let ((bitset1 (gtk:bitset-new-range 0 100))
        (bitset2 (gtk:bitset-new-range 50 50)))
    (is-false (gtk:bitset-intersect bitset1 bitset2))
    (is (= 50 (gtk:bitset-minimum bitset1)))
    (is (= 99 (gtk:bitset-maximum bitset1)))
    (is (= 50 (gtk:bitset-size bitset1)))))

;;;     gtk_bitset_subtract

(test gtk-bitset-subtract
  (let ((bitset1 (gtk:bitset-new-range 0 100))
        (bitset2 (gtk:bitset-new-range 50 50)))
    (is-false (gtk:bitset-subtract bitset1 bitset2))
    (is (= 0 (gtk:bitset-minimum bitset1)))
    (is (= 49 (gtk:bitset-maximum bitset1)))
    (is (= 50 (gtk:bitset-size bitset1)))))

;;;     gtk_bitset_difference

(test gtk-bitset-difference
  (let ((bitset1 (gtk:bitset-new-range 0 100))
        (bitset2 (gtk:bitset-new-range 50 100)))
    (is-false (gtk:bitset-difference bitset1 bitset2))
    (is (=   0 (gtk:bitset-minimum bitset1)))
    (is (= 149 (gtk:bitset-maximum bitset1)))
    (is (= 100 (gtk:bitset-size bitset1)))))

;;;     gtk_bitset_shift_left
;;;     gtk_bitset_shift_right

(test gtk-bitset-shift-left/right
  (let ((bitset (gtk:bitset-new-range 0 100)))
    (is-false (gtk:bitset-shift-right bitset 50))
    (is (=  50 (gtk:bitset-minimum bitset)))
    (is (= 149 (gtk:bitset-maximum bitset)))
    (is (= 100 (gtk:bitset-size bitset)))
    (is-false (gtk:bitset-shift-left bitset 50))
    (is (=   0 (gtk:bitset-minimum bitset)))
    (is (=  99 (gtk:bitset-maximum bitset)))
    (is (= 100 (gtk:bitset-size bitset)))))

;;;     gtk_bitset_splice

(test gtk-bitset-splice
  (let ((bitset (gtk:bitset-new-range 0 100)))
    (is-false (gtk:bitset-splice bitset 59 25 50))
    (is (=   0 (gtk:bitset-minimum bitset)))
    (is (= 124 (gtk:bitset-maximum bitset)))
    (is (=  75 (gtk:bitset-size bitset)))))

;;;     gtk_bitset_iter_init_first
;;;     gtk_bitset_iter_next
;;;     gtk_bitset_iter_previous
;;;     gtk_bitset_iter_get_value
;;;     gtk_bitset_iter_is_valid

(test gtk-bitset-init-first
  (let ((bitset (gtk:bitset-new-empty))
        (values '(1 3 5 7 9 11 13 15 16 17 18 19)))
    (dolist (value values)
      (is-true (gtk:bitset-add bitset value)))
    (cffi:with-foreign-object (iter 'gtk:bitset-iter)
      (is (= 1 (gtk:bitset-iter-init-first iter bitset)))
      (is-true (gtk:bitset-iter-is-valid iter))
      (is (= 3  (gtk:bitset-iter-next iter)))
      (is (= 3 (gtk:bitset-iter-value iter)))
      (is (= 5 (gtk:bitset-iter-next iter)))
      (is (= 5 (gtk:bitset-iter-value iter)))
      (is (= 3 (gtk:bitset-iter-previous iter)))
      (is (= 3 (gtk:bitset-iter-value iter)))
      (is (= 1 (gtk:bitset-iter-previous iter)))
      (is (= 1 (gtk:bitset-iter-value iter)))
      (is-false (gtk:bitset-iter-previous iter))
      (is (= 0 (gtk:bitset-iter-value iter))))))

;;;     gtk_bitset_iter_init_last

(test gtk-bitset-init-last
  (let ((bitset (gtk:bitset-new-empty))
        (values '(1 3 5 7 9 11 13 15 16 17 18 19)))
    (dolist (value values)
      (is-true (gtk:bitset-add bitset value)))
    (cffi:with-foreign-object (iter 'gtk:bitset-iter)
      (is (= 19 (gtk:bitset-iter-init-last iter bitset)))
      (is-true (gtk:bitset-iter-is-valid iter))
      (is (= 18 (gtk:bitset-iter-previous iter)))
      (is (= 18 (gtk:bitset-iter-value iter)))
      (is (= 17 (gtk:bitset-iter-previous iter)))
      (is (= 17 (gtk:bitset-iter-value iter)))
      (is (= 18 (gtk:bitset-iter-next iter)))
      (is (= 18 (gtk:bitset-iter-value iter)))
      (is (= 19 (gtk:bitset-iter-next iter)))
      (is (= 19 (gtk:bitset-iter-value iter)))
      (is-false (gtk:bitset-iter-next iter))
      (is (= 0 (gtk:bitset-iter-value iter))))))

;;;     gtk_bitset_iter_init_at

(test gtk-bitset-init-at
  (let ((bitset (gtk:bitset-new-empty))
        (values '(1 3 5 7 9 11 13 15 16 17 18 19)))
    (dolist (value values)
      (is-true (gtk:bitset-add bitset value)))
    (cffi:with-foreign-object (iter 'gtk:bitset-iter)
      (is (= 11 (gtk:bitset-iter-init-at iter bitset 10)))
      (is-true (gtk:bitset-iter-is-valid iter))
      (is (= 13 (gtk:bitset-iter-next iter)))
      (is (= 13 (gtk:bitset-iter-value iter)))
      (is (= 15 (gtk:bitset-iter-next iter)))
      (is (= 15 (gtk:bitset-iter-value iter)))
      (is (= 13 (gtk:bitset-iter-previous iter)))
      (is (= 13 (gtk:bitset-iter-value iter)))
      (is (= 11 (gtk:bitset-iter-previous iter)))
      (is (= 11 (gtk:bitset-iter-value iter)))
      (is (= 9 (gtk:bitset-iter-previous iter))))))

;;; 2024-7-4
