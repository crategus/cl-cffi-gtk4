(in-package :gtk-test)

(def-suite gdk-rgba :in gdk-suite)
(in-suite gdk-rgba)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkRGBA

(test gdk-rgba-structure
  ;; Type check
  (is (g-type-is-a (gtype "GdkRGBA") +g-type-boxed+))
  ;; Check the type initializer
  (is (eq (gtype "GdkRGBA")
          (gtype (foreign-funcall "gdk_rgba_get_type" :size)))))

;;; --- Accessors --------------------------------------------------------------

(test gdk-rgba-accessors
  (let ((rgba (gdk-rgba-new :red 0.1 :green 0.2 :blue 0.3 :alpha 0.5)))
    (is (= 0.1 (gdk-rgba-red rgba)))
    (is (= 0.2 (setf (gdk-rgba-red rgba) 0.2)))
    (is (= 0.2 (gdk-rgba-green rgba)))
    (is (= 0.3 (setf (gdk-rgba-green rgba) 0.3)))
    (is (= 0.3 (gdk-rgba-blue rgba)))
    (is (= 0.4 (setf (gdk-rgba-blue rgba) 0.4)))
    (is (= 0.5 (gdk-rgba-alpha rgba)))
    (is (= 0.6 (setf (gdk-rgba-alpha rgba) 0.6)))))

;;; --- Functions --------------------------------------------------------------

;;;    gdk-rgba-new

(test gdk-rgba-new
  (is (typep (gdk-rgba-new) 'gdk-rgba))
  (is (typep (gdk-rgba-new :red 0.1 :green 0.2 :blue 0.3 :alpha 0.5) 'gdk-rgba)))

;;;     gdk_rgba_copy

(test gdk-rgba-copy
  (let ((color (gdk-rgba-new :red 0.1 :green 0.2 :blue 0.3 :alpha 0.4)))
    (is (gdk-rgba-equal color (gdk-rgba-copy color)))))

;;;     gdk_rgba_is_clear
;;;     gdk_rgba_is_opaque

;;;     gdk_rgba_parse

(test gdk-rgba-parse
  (is (typep (gdk-rgba-parse "red") 'gdk-rgba))
  (is (= 1.0 (gdk-rgba-red (gdk-rgba-parse "red"))))
  (is (= 1.0 (gdk-rgba-green (gdk-rgba-parse "lime"))))
  (is (= 1.0 (gdk-rgba-blue (gdk-rgba-parse "blue")))))

;;;     gdk_rgba_hash

(test gdk-rgba-hash
  (is (integerp (gdk-rgba-hash (gdk-rgba-parse "red")))))

;;;     gdk_rgba_equal

(test gdk-rgba-equal
  (is (gdk-rgba-equal (gdk-rgba-parse "red") (gdk-rgba-parse "red"))))

;;;     gdk_rgba_to_string

(test gdk-rgba-to-string
  (is (string= "rgb(255,0,0)" (gdk-rgba-to-string (gdk-rgba-parse "red")))))

;;; 2022-8-28
