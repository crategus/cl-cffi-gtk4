(in-package :gtk-test)

(def-suite gtk-bitset :in gtk-suite)
(in-suite gtk-bitset)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkBitsetIter

;;;     GtkBitset

(test gtk-bitset-structure
  ;; Type check
  (is (g:type-is-a (g:gtype "GtkBitset") g:+g-type-boxed+))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkBitset")
          (g:gtype (cffi:foreign-funcall "gtk_bitset_get_type" :size))))
  ;; Check the registered name
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
;;;     gtk_bitset_remove_all

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
;;;     gtk_bitset_add_range
;;;     gtk_bitset_remove_range
;;;     gtk_bitset_add_range_closed
;;;     gtk_bitset_remove_range_closed
;;;     gtk_bitset_add_rectangle
;;;     gtk_bitset_remove_rectangle
;;;     gtk_bitset_union
;;;     gtk_bitset_intersect
;;;     gtk_bitset_subtract
;;;     gtk_bitset_difference
;;;     gtk_bitset_shift_left
;;;     gtk_bitset_shift_right
;;;     gtk_bitset_splice

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

    (cffi:with-foreign-objects ((iter 'gtk:bitset-iter) (value :uint))

      (is-true (gtk:bitset-iter-init-first iter bitset value))
      (is-true (gtk:bitset-iter-is-valid iter))

      (is (= 1 (cffi:mem-ref value :uint)))
      (is (= 1 (gtk:bitset-iter-value iter)))
      (is-true (gtk:bitset-iter-next iter value))
      (is (= 3 (cffi:mem-ref value :uint)))
      (is (= 3 (gtk:bitset-iter-value iter)))
      (is-true (gtk:bitset-iter-next iter value))
      (is (= 5 (cffi:mem-ref value :uint)))
      (is (= 5 (gtk:bitset-iter-value iter)))

      (is-true (gtk:bitset-iter-previous iter value))
      (is (= 3 (cffi:mem-ref value :uint)))
      (is (= 3 (gtk:bitset-iter-value iter)))
      (is-true (gtk:bitset-iter-previous iter value))
      (is (= 1 (cffi:mem-ref value :uint)))
      (is (= 1 (gtk:bitset-iter-value iter)))

      (is-false (gtk:bitset-iter-previous iter value))
      (is (= 0 (cffi:mem-ref value :uint))))))

;;;     gtk_bitset_iter_init_last

(test gtk-bitset-init-last
  (let ((bitset (gtk:bitset-new-empty))
        (values '(1 3 5 7 9 11 13 15 16 17 18 19)))

    (dolist (value values)
      (is-true (gtk:bitset-add bitset value)))

    (cffi:with-foreign-objects ((iter 'gtk:bitset-iter) (value :uint))

      (is-true (gtk:bitset-iter-init-last iter bitset value))
      (is-true (gtk:bitset-iter-is-valid iter))

      (is (= 19 (cffi:mem-ref value :uint)))
      (is (= 19 (gtk:bitset-iter-value iter)))
      (is-true (gtk:bitset-iter-previous iter value))
      (is (= 18 (cffi:mem-ref value :uint)))
      (is (= 18 (gtk:bitset-iter-value iter)))
      (is-true (gtk:bitset-iter-previous iter value))
      (is (= 17 (cffi:mem-ref value :uint)))
      (is (= 17 (gtk:bitset-iter-value iter)))

      (is-true (gtk:bitset-iter-next iter value))
      (is (= 18 (cffi:mem-ref value :uint)))
      (is (= 18 (gtk:bitset-iter-value iter)))
      (is-true (gtk:bitset-iter-next iter value))
      (is (= 19 (cffi:mem-ref value :uint)))
      (is (= 19 (gtk:bitset-iter-value iter)))

      (is-false (gtk:bitset-iter-next iter value))
      (is (= 0 (cffi:mem-ref value :uint))))))

;;;     gtk_bitset_iter_init_at

(test gtk-bitset-init-at
  (let ((bitset (gtk:bitset-new-empty))
        (values '(1 3 5 7 9 11 13 15 16 17 18 19)))

    (dolist (value values)
      (is-true (gtk:bitset-add bitset value)))

    (cffi:with-foreign-objects ((iter 'gtk:bitset-iter) (value :uint))

      (is-true (gtk:bitset-iter-init-at iter bitset 10 value))
      (is-true (gtk:bitset-iter-is-valid iter))

      (is (= 11 (cffi:mem-ref value :uint)))
      (is (= 11 (gtk:bitset-iter-value iter)))
      (is-true (gtk:bitset-iter-next iter value))
      (is (= 13 (cffi:mem-ref value :uint)))
      (is (= 13 (gtk:bitset-iter-value iter)))
      (is-true (gtk:bitset-iter-next iter value))
      (is (= 15 (cffi:mem-ref value :uint)))
      (is (= 15 (gtk:bitset-iter-value iter)))

      (is-true (gtk:bitset-iter-previous iter value))
      (is (= 13 (cffi:mem-ref value :uint)))
      (is (= 13 (gtk:bitset-iter-value iter)))
      (is-true (gtk:bitset-iter-previous iter value))
      (is (= 11 (cffi:mem-ref value :uint)))
      (is (= 11 (gtk:bitset-iter-value iter)))

      (is-true (gtk:bitset-iter-previous iter value))
      (is (= 9 (cffi:mem-ref value :uint))))))

;;; --- 2023-9-11 --------------------------------------------------------------
