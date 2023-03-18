(in-package :gtk-test)

(def-suite gtk-orientable :in gtk-suite)
(in-suite gtk-orientable)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkOrientable

(test orientable-interface
  ;; Type check
  (is (g:type-is-interface "GtkOrientable"))
  ;; Check the registered name
  (is (eq 'gtk:orientable
          (gobject:symbol-for-gtype "GtkOrientable")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkOrientable")
          (g:gtype (cffi:foreign-funcall "gtk_orientable_get_type" :size))))
  ;; Get the names of the interface properties.
  (is (equal '("orientation")
             (mapcar #'g:param-spec-name
                     (g:object-interface-list-properties "GtkOrientable"))))
  ;; Get the interface definition
  (is (equal '(DEFINE-G-INTERFACE "GtkOrientable"
                                  GTK-ORIENTABLE
                                  (:EXPORT T
                                   :TYPE-INITIALIZER "gtk_orientable_get_type")
                                  (ORIENTATION
                                   GTK-ORIENTABLE-ORIENTATION
                                   "orientation" "GtkOrientation" T T))
             (get-g-type-definition "GtkOrientable"))))

;;; --- Properties -------------------------------------------------------------

;;;     orientation

(test orientable-properties
  (let ((orientable (make-instance 'gtk:box)))
    (is (eq :horizontal
            (gtk:orientable-orientation orientable)))
    (is (eq :vertical
            (setf (gtk:orientable-orientation orientable) :vertical)))
    (is (eq :vertical
            (gtk:orientable-orientation orientable)))))

;;; --- 2023-3-18 --------------------------------------------------------------
