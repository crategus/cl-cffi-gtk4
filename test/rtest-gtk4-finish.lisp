(in-package :gtk-test)

(in-suite gtk-test)

;;; ----------------------------------------------------------------------------

;; Finally, check fundamental types known to GTK
#-windows
(test g-type-fundamentals
  (let* ((glib::*warn-unknown-gtype* nil)
         (nmax (cffi:foreign-funcall "g_type_fundamental_next" :size))
         (gtypes (iter (for x from 1 below nmax)
                       (for gtype = (g:gtype (ash x 2)))
                       (when gtype (collect gtype)))))
  (is (equal '("void"
               "GInterface"
               "gchar"
               "guchar"
               "gboolean"
               "gint"
               "guint"
               "glong"
               "gulong"
               "gint64"
               "guint64"
               "GEnum"
               "GFlags"
               "gfloat"
               "gdouble"
               "gchararray"
               "gpointer"
               "GBoxed"
               "GParam"
               "GObject"
               "GVariant"
               "GtkExpression"
               "GdkEvent"
               "GskRenderNode"
               "GstIntRange"
               "GstInt64Range"
               "GstDoubleRange"
               "GstFractionRange"
               "GstFraction"
               "GstBitmask"
               "GstFlagSet"
               "GstValueList"
               "GstValueArray")
             (mapcar #'glib:gtype-name gtypes)))))

#+windows
(test g-type-fundamentals
  (let* ((glib::*warn-unknown-gtype* nil)
         (nmax (cffi:foreign-funcall "g_type_fundamental_next" :size))
         (gtypes (iter (for x from 1 below nmax)
                       (for gtype = (g:gtype (ash x 2)))
                       (when gtype (collect gtype)))))
  (is (equal '("void"
               "GInterface"
               "gchar"
               "guchar"
               "gboolean"
               "gint"
               "guint"
               "glong"
               "gulong"
               "gint64"
               "guint64"
               "GEnum"
               "GFlags"
               "gfloat"
               "gdouble"
               "gchararray"
               "gpointer"
               "GBoxed"
               "GParam"
               "GObject"
               "GVariant"
               "GtkExpression"
               "GdkEvent"
               "GskRenderNode")
;               "GstIntRange"
;               "GstInt64Range"
;               "GstDoubleRange"
;               "GstFractionRange"
;               "GstFraction"
;               "GstBitmask"
;               "GstFlagSet"
;               "GstValueList"
;               "GstValueArray")
             (mapcar #'glib:gtype-name gtypes)))))

;;; ----------------------------------------------------------------------------

(test gtk-test-finished
  (cond ((or *first-run-gtk-test* *first-run-testsuite*)
         (setf *first-run-gtk-test* nil)
         (setf *first-run-testsuite* nil)
         (when *test-dribble*
           (format t "~%First run of the gtk-test suite finished.~%")))
        (t
         (when *test-dribble*
           (format t "~%Second or more run of the gtk-test suite finished.~%")))))

;;; 2024-12-23
