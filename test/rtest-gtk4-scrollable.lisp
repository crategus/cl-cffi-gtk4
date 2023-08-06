(in-package :gtk-test)

(def-suite gtk-scrollable :in gtk-suite)
(in-suite gtk-scrollable)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkScrollablePolicy

(test gtk-scrollable-policy
  ;; Check the type
  (is (g:type-is-enum "GtkScrollablePolicy"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkScrollablePolicy")
          (g:gtype (cffi:foreign-funcall "gtk_scrollable_policy_get_type" 
                                         :size))))
  ;; Check the registered name
  (is (eq 'gtk:scrollable-policy
          (glib:symbol-for-gtype "GtkScrollablePolicy")))
  ;; Check the names
  (is (equal '("GTK_SCROLL_MINIMUM" "GTK_SCROLL_NATURAL")
             (list-enum-item-name "GtkScrollablePolicy")))
  ;; Check the values
  (is (equal '(0 1)
             (list-enum-item-value "GtkScrollablePolicy")))
  ;; Check the nick names
  (is (equal '("minimum" "natural")
             (list-enum-item-nick "GtkScrollablePolicy")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkScrollablePolicy"
                             GTK-SCROLLABLE-POLICY
                             (:EXPORT T
                              :TYPE-INITIALIZER
                              "gtk_scrollable_policy_get_type")
                             (:MINIMUM 0)
                             (:NATURAL 1))
             (gobject:get-g-type-definition "GtkScrollablePolicy"))))

;;;     GtkScrollable

(test gtk-scrollable-interface
  ;; Type check
  (is (g:type-is-interface "GtkScrollable"))
  ;; Check the registered name
  (is (eq 'gtk:scrollable
          (glib:symbol-for-gtype "GtkScrollable")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkScrollable")
          (g:gtype (cffi:foreign-funcall "gtk_scrollable_get_type" :size))))
  ;; Check the interface prerequisites
  (is (equal '("GObject")
             (list-interface-prerequisites "GtkScrollable")))
  ;; Check the interface properties.
  (is (equal '("hadjustment" "hscroll-policy" "vadjustment" "vscroll-policy")
             (list-interface-properties "GtkScrollable")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkScrollable")))
  ;; Get the interface definition
  (is (equal '(GOBJECT:DEFINE-G-INTERFACE "GtkScrollable" GTK-SCROLLABLE
                    (:EXPORT T :TYPE-INITIALIZER "gtk_scrollable_get_type")
                    (HADJUSTMENT GTK-SCROLLABLE-HADJUSTMENT "hadjustment"
                     "GtkAdjustment" T T)
                    (HSCROLL-POLICY GTK-SCROLLABLE-HSCROLL-POLICY
                     "hscroll-policy" "GtkScrollablePolicy" T T)
                    (VADJUSTMENT GTK-SCROLLABLE-VADJUSTMENT "vadjustment"
                     "GtkAdjustment" T T)
                    (VSCROLL-POLICY GTK-SCROLLABLE-VSCROLL-POLICY
                     "vscroll-policy" "GtkScrollablePolicy" T T))
             (gobject:get-g-type-definition "GtkScrollable"))))

;;; --- Properties -------------------------------------------------------------

;;;     hadjustment
;;;     hscroll-policy
;;;     vadjustment
;;;     vscroll-policy

(test gtk-scrollable-properties
  (let ((scrollable (make-instance 'gtk:viewport)))  
    (is (typep (gtk:scrollable-hadjustment scrollable) 'gtk:adjustment))
    (is (eq :minimum (gtk:scrollable-hscroll-policy scrollable)))
    (is (typep (gtk:scrollable-vadjustment scrollable) 'gtk:adjustment))
    (is (eq :minimum (gtk:scrollable-vscroll-policy scrollable)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_scrollable_get_border

(test gtk-scrollable-border
  (let ((scrollable (make-instance 'gtk:icon-view))
        (border nil))
    ;; TODO: Can we construct a test different from nil
    (is-false (setf border 
                    (gtk:scrollable-border scrollable)))))

;;; --- 2023-8-6 ---------------------------------------------------------------
