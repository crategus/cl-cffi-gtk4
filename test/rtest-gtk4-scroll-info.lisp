(in-package :gtk-test)

(def-suite gtk-scroll-info :in gtk-suite)
(in-suite gtk-scroll-info)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkScrollInfo

(test gtk-scroll-info-boxed
  ;; Check type
  (is (g:type-is-boxed "GtkScrollInfo"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkScrollInfo")
          (g:gtype (cffi:foreign-funcall "gtk_scroll_info_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:scroll-info
          (glib:symbol-for-gtype "GtkScrollInfo"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_scroll_info_new

(test gtk-scroll-info-new
  (is (typep (gtk:scroll-info-new) 'gtk:scroll-info)))

;;;     gtk_scroll_info_get_enable_horizontal
;;;     gtk_scroll_info_set_enable_horizontal
;;;     gtk_scroll_info_get_enable_vertical
;;;     gtk_scroll_info_set_enable_vertical

(test gtk-scroll-info-enable-horizontal/vertical
  (let ((scrollinfo (gtk:scroll-info-new)))
    ;;gtk:scroll-info-enable-horizontal
    (is-true (gtk:scroll-info-enable-horizontal scrollinfo))
    (is-false (setf (gtk:scroll-info-enable-horizontal scrollinfo) nil))
    (is-false (gtk:scroll-info-enable-horizontal scrollinfo))
    ;;gtk:scroll-info-enable-vertical
    (is-true (gtk:scroll-info-enable-vertical scrollinfo))
    (is-false (setf (gtk:scroll-info-enable-vertical scrollinfo) nil))
    (is-false (gtk:scroll-info-enable-vertical scrollinfo))))

;;; 2024-7-4
