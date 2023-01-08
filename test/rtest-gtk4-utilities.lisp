;;; Lisp Utilities for the testsuite

(in-package :gtk-test)

;; See https://www.embeddeduse.com/2019/08/26/qt-compare-two-floats/
(let ((eps-factor 1.0d-2))
  (defun approx-equal (x y)
    (or (< (abs (- x y)) eps-factor)
        (< (abs (- x y)) (* eps-factor (max (abs x) (abs y)))))))


(defun list-children (gtype)
  (sort (mapcar #'g:type-name (g:type-children gtype))
        #'string<))

(defun list-interfaces (gtype)
  (mapcar #'g:type-name (g:type-interfaces gtype)))

;; A sorted list of the class property names without inherited properties
(defun list-properties (gtype)
  (sort (set-difference (mapcar #'g:param-spec-name
                                (g:object-class-list-properties gtype))
                        (mapcar #'g:param-spec-name
                                (g:object-class-list-properties
                                  (g:type-parent gtype)))
                        :test #'string=)
        #'string<))

(defun list-interface-properties (gtype)
  (mapcar #'g:param-spec-name
          (g:object-interface-list-properties gtype)))

;; A sorted list of the signal names of a class
(defun list-signals (gtype)
  (sort (mapcar #'g:signal-name
                (g:signal-list-ids gtype)) #'string<))

(defun print-foreign-gobjects-weak ()
  (format t "~&List of foreign gobjects WEAK~&")
  (loop for key being the hash-keys of gobject::*foreign-gobjects-weak*
        using (hash-value value)
        do (format t "key: ~a   ~a~%" key value)))

(defun list-foreign-gobjects-weak ()
  (loop for key being the hash-keys of gobject::*foreign-gobjects-weak*
        using (hash-value value)
        collect value))

;;; --- 2023-1-1 ---------------------------------------------------------------
