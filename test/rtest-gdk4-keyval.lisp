(in-package :gtk-test)

(def-suite gdk-keyval :in gdk-suite)
(in-suite gdk-keyval)

;;; --- Functions --------------------------------------------------------------

;;;     gdk_keyval_name

(test gdk-keyval-name
  (is (string= "a" (gdk:keyval-name 97)))
  (is (string= "A" (gdk:keyval-name 65)))
  (is (string= "space" (gdk:keyval-name 32)))
  (is (string= "exclam" (gdk:keyval-name 33))))

;;;     gdk_keyval_from_name

(test gdk-keyval-from-name
  (is (= 97 (gdk:keyval-from-name "a")))
  (is (= 65 (gdk:keyval-from-name "A")))
  (is (= 32 (gdk:keyval-from-name "space")))
  (is (= 33 (gdk:keyval-from-name "exclam"))))

;;;     gdk_keyval_convert_case

(test gdk-keyval-convert-case
  (is (equal '(97 65) (multiple-value-list (gdk:keyval-convert-case 97))))
  (is (equal '(97 65) (multiple-value-list (gdk:keyval-convert-case 65)))))

;;;     gdk_keyval_to_upper

(test gdk-keyval-to-upper
  (is (= 65 (gdk:keyval-to-upper 97)))
  (is (= 65 (gdk:keyval-to-upper 65)))
  (is (= 32 (gdk:keyval-to-upper 32))))

;;;     gdk_keyval_to_lower

(test gdk-keyval-to-lower
  (is (= 97 (gdk:keyval-to-lower 97)))
  (is (= 97 (gdk:keyval-to-lower 65)))
  (is (= 32 (gdk:keyval-to-lower 32))))

;;;     gdk_keyval_is_upper

(test gdk-keyval-to-upper
  (is-false (gdk:keyval-is-upper 97))
  (is-true (gdk:keyval-is-upper 65))
  (is-true (gdk:keyval-is-upper 32)))

;;;     gdk_keyval_is_lower

(test gdk-keyval-to-lower
  (is-true (gdk:keyval-is-lower 97))
  (is-false (gdk:keyval-is-lower 65))
  (is-true (gdk:keyval-is-lower 32)))

;;;     gdk_keyval_to_unicode

(test gdk-keyval-to-unicode
  (is (= 97 (gdk:keyval-to-unicode 97)))
  (is (= 65 (gdk:keyval-to-unicode 65)))
  (is (= 32 (gdk:keyval-to-unicode 32))))

;;;     gdk_unicode_to_keyval

(test gdk-unicode-to-keyval
  (is (= 97 (gdk:unicode-to-keyval 97)))
  (is (= 65 (gdk:unicode-to-keyval 65)))
  (is (= 32 (gdk:unicode-to-keyval 32))))

;;; --- 2023-4-14 --------------------------------------------------------------
