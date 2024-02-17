(in-package :gtk-test)

(def-suite gdk-toplevel-layout :in gdk-suite)
(in-suite gdk-toplevel-layout)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkToplevelLayout

(test gdk-toplevel-layout-boxed
  ;; Type check
  (is (g:type-is-a (g:gtype "GdkToplevelLayout") g:+g-type-boxed+))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkToplevelLayout")
          (g:gtype (cffi:foreign-funcall "gdk_toplevel_layout_get_type"
                                         :size))))
  ;; Check the registered name
  (is (eq 'gdk:toplevel-layout
          (glib:symbol-for-gtype "GdkToplevelLayout"))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk_toplevel_layout_new

(test gdk-toplevel-layout-new
  (is (typep (gdk:toplevel-layout-new) 'gdk:toplevel-layout)))

;;;     gdk_toplevel_layout_ref
;;;     gdk_toplevel_layout_unref

;;;     gdk_toplevel_layout_copy

(test gdk-toplevel-layout-copy
  (let ((layout (gdk:toplevel-layout-new)))
    (is (typep (gdk:toplevel-layout-copy layout) 'gdk:toplevel-layout))))

;;;     gdk_toplevel_layout_equal

(test gdk-toplevel-layout-equal
  (let* ((layout1 (gdk:toplevel-layout-new))
         (layout2 (gdk:toplevel-layout-copy layout1)))
    (is (gdk:toplevel-layout-equal layout1 layout2))))

;;;     gdk_toplevel_layout_set_maximized
;;;     gdk_toplevel_layout_get_maximized

(test gdk-toplevel-layout-maximized
  (let ((layout (gdk:toplevel-layout-new)))
    (is-false (gdk:toplevel-layout-maximized layout))
    (is-true (setf (gdk:toplevel-layout-maximized layout) t))
    (is-true (gdk:toplevel-layout-maximized layout))))

;;;     gdk_toplevel_layout_set_fullscreen
;;;     gdk_toplevel_layout_get_fullscreen
;;;     gdk_toplevel_layout_get_fullscreen_monitor

(test gdk-toplevel-layout-fullscreen
  (let ((monitor (first (gdk:display-monitors (gdk:display-default))))
        (layout (gdk:toplevel-layout-new)))
    (is (typep monitor 'gdk:monitor))
    (is-false (gdk:toplevel-layout-fullscreen layout))
    (is-true (setf (gdk:toplevel-layout-fullscreen layout monitor) t))
    (is-true (gdk:toplevel-layout-fullscreen layout))
    (is (eq monitor (gdk:toplevel-layout-fullscreen-monitor layout)))))

;;;     gdk_toplevel_layout_set_resizable
;;;     gdk_toplevel_layout_get_resizable

(test gdk-toplevel-layout-resizable
  (let ((layout (gdk:toplevel-layout-new)))
    (is-true (gdk:toplevel-layout-resizable layout))
    (is-false (setf (gdk:toplevel-layout-resizable layout) nil))
    (is-false (gdk:toplevel-layout-resizable layout))))

;;; 2024-1-9
