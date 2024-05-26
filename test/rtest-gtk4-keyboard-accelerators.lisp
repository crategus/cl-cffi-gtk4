(in-package :gtk-test)

(def-suite gtk-keyboard-accelerators :in gtk-suite)
(in-suite gtk-keyboard-accelerators)

;;; --- Functions --------------------------------------------------------------

;;;     gtk_accelerator_valid

(test gtk-accelerator-valid
  (is-true (gtk:accelerator-valid (char-code #\a) :shift-mask))
  (is-true (gtk:accelerator-valid (char-code #\a) :control-mask))
  (is-false (gtk:accelerator-valid (char-code #\fullwidth_macron)
                                   :no-modifier-mask)))

;;;     gtk_accelerator_parse

(test gtk-accelerator-parse
  (is (equal '(97 (:control-mask))
             (multiple-value-list (gtk:accelerator-parse "<Control>a"))))
  (is (equal '(65470 (:shift-mask :alt-mask))
             (multiple-value-list (gtk:accelerator-parse "<Shift><Alt>F1"))))
  (is (equal '(45 (:control-mask))
             (multiple-value-list (gtk:accelerator-parse "<Control>minus"))))
  (is (equal '(0 nil)
             (multiple-value-list (gtk:accelerator-parse "not valid")))))

;;;     gtk_accelerator_name

(test gtk-accelerator-name
  (is (string= "<Control>a" (gtk:accelerator-name 97 :control-mask)))
  (is (string= "<Shift><Alt>F1"
               (gtk:accelerator-name 65470 '(:shift-mask :alt-mask))))
  (is (string= "<Control>minus" (gtk:accelerator-name 45 :control-mask))))

;;;     gtk_accelerator_get_label

(test gtk-accelerator-label
  #-windows
  (is (string= "Ctrl+A" (gtk:accelerator-label 97 :control-mask)))
  #+windows
  (is (string= "Strg+A" (gtk:accelerator-label 97 :control-mask)))
  #-windows
  (is (string= "Shift+Alt+F1"
               (gtk:accelerator-label 65470 '(:shift-mask :alt-mask))))
  #+windows
  (is (string= "Umschalt+Alt+F1"
               (gtk:accelerator-label 65470 '(:shift-mask :alt-mask))))
  #-windows
  (is (string= "Ctrl+-" (gtk:accelerator-label 45 :control-mask)))
  #+windows
  (is (string= "Strg+-" (gtk:accelerator-label 45 :control-mask))))

;;;     gtk_accelerator_parse_with_keycode
;;;     gtk_accelerator_name_with_keycode
;;;     gtk_accelerator_get_label_with_keycode

;;;     gtk_accelerator_get_default_mod_mask

(test gtk-accelerator-default-mod-mask
  (is (equal '(:SHIFT-MASK :CONTROL-MASK :ALT-MASK :SUPER-MASK :HYPER-MASK
               :META-MASK)
             (gtk:accelerator-default-mod-mask))))

;;; 2024-1-9
