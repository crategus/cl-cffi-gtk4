(in-package :gtk-test)

(def-suite gtk-scrollable :in gtk-scrolling)
(in-suite gtk-scrollable)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkScrollablePolicy

(test gtk-scrollable-policy
  ;; Check type
  (is (g:type-is-enum "GtkScrollablePolicy"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkScrollablePolicy")
          (g:gtype (cffi:foreign-funcall "gtk_scrollable_policy_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gtk:scrollable-policy
          (glib:symbol-for-gtype "GtkScrollablePolicy")))
  ;; Check names
  (is (equal '("GTK_SCROLL_MINIMUM" "GTK_SCROLL_NATURAL")
             (glib-test:list-enum-item-names "GtkScrollablePolicy")))
  ;; Check values
  (is (equal '(0 1)
             (glib-test:list-enum-item-values "GtkScrollablePolicy")))
  ;; Check nick names
  (is (equal '("minimum" "natural")
             (glib-test:list-enum-item-nicks "GtkScrollablePolicy")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkScrollablePolicy" GTK:SCROLLABLE-POLICY
                                    (:EXPORT T
                                     :TYPE-INITIALIZER
                                     "gtk_scrollable_policy_get_type")
                                    (:MINIMUM 0)
                                    (:NATURAL 1))
             (gobject:get-gtype-definition "GtkScrollablePolicy"))))

;;;     GtkScrollable

(test gtk-scrollable-interface
  ;; Check type
  (is (g:type-is-interface "GtkScrollable"))
  ;; Check registered name
  (is (eq 'gtk:scrollable
          (glib:symbol-for-gtype "GtkScrollable")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkScrollable")
          (g:gtype (cffi:foreign-funcall "gtk_scrollable_get_type" :size))))
  ;; Check interface prerequisites
  (is (equal '("GObject")
             (glib-test:list-interface-prerequisites "GtkScrollable")))
  ;; Check interface properties
  (is (equal '("hadjustment" "hscroll-policy" "vadjustment" "vscroll-policy")
             (glib-test:list-interface-properties "GtkScrollable")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkScrollable")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "GtkScrollable" GTK:SCROLLABLE
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_scrollable_get_type")
                       (HADJUSTMENT SCROLLABLE-HADJUSTMENT
                        "hadjustment" "GtkAdjustment" T T)
                       (HSCROLL-POLICY SCROLLABLE-HSCROLL-POLICY
                        "hscroll-policy" "GtkScrollablePolicy" T T)
                       (VADJUSTMENT SCROLLABLE-VADJUSTMENT
                        "vadjustment" "GtkAdjustment" T T)
                       (VSCROLL-POLICY SCROLLABLE-VSCROLL-POLICY
                        "vscroll-policy" "GtkScrollablePolicy" T T))
             (gobject:get-gtype-definition "GtkScrollable"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-scrollable-properties
  (let ((scrollable (make-instance 'gtk:viewport))
        adjustment1 adjustment2)
    (is (typep (setf adjustment1
                     (gtk:scrollable-hadjustment scrollable)) 'gtk:adjustment))
    (is (eq :minimum (gtk:scrollable-hscroll-policy scrollable)))
    (is (typep (setf adjustment2
                     (gtk:scrollable-vadjustment scrollable)) 'gtk:adjustment))
    (is (eq :minimum (gtk:scrollable-vscroll-policy scrollable)))
    ;; Check memory management
    (is-false (setf (gtk:scrollable-hadjustment scrollable) nil))
    (is-false (setf (gtk:scrollable-vadjustment scrollable) nil))
    (is (= 1 (g:object-ref-count adjustment1)))
    (is (= 1 (g:object-ref-count adjustment2)))
    (is (= 1 (g:object-ref-count scrollable)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_scrollable_get_border

(test gtk-scrollable-border
  (let ((scrollable (make-instance 'gtk:text-view))
        (border nil))
    ;; TODO: Can we construct a test different from nil
    (is-false (setf border
                    (gtk:scrollable-border scrollable)))))

;;; 2024-10-27
