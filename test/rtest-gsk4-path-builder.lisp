(in-package :gtk-test)

(def-suite gsk-path-builder :in gsk-suite)
(in-suite gsk-path-builder)

;;; --- Types and Values -------------------------------------------------------

;;;     GskPathBuilder

(test gsk-path-builder-boxed
  ;; Check type
  (is (g:type-is-boxed "GskPathBuilder"))
  ;; Check type initializer
  (is (eq (g:gtype "GskPathBuilder")
          (g:gtype (cffi:foreign-funcall "gsk_path_builder_get_type" :size))))
  ;; Check registered name
  (is (eq 'gsk:path-builder
          (glib:symbol-for-gtype "GskPathBuilder"))))

;;; --- Functions --------------------------------------------------------------

;;;     gsk_path_builder_new

(test gsk-path-builder-new
  (is (typep (gsk:path-builder-new) 'gsk:path-builder)))

;;;     gsk_path_builder_to_path

(test gsk-path-builder-to-path
  (let ((builder (gsk:path-builder-new)))
    (is (typep (gsk:path-builder-to-path builder) 'gsk:path))))

;;;     gsk_path_builder_free_to_path
;;;     gsk_path_builder_get_current_point

;;;     gsk_path_builder_add_rect

(test gsk-path-builder-add-rect
  (let ((builder (gsk:path-builder-new)))
    (graphene:with-rect (rect 10 20 100 200)
      (is-false (gsk:path-builder-add-rect builder rect))
      (is (string= "M 10 20 h 100 v 200 h -100 z"
                   (gsk:path-to-string (gsk:path-builder-to-path builder)))))))

;;;     gsk_path_builder_add_circle

(test gsk-path-builder-add-circle
  (let ((builder (gsk:path-builder-new)))
    (graphene:with-point (point 0 0)
      (is-false (gsk:path-builder-add-circle builder point 10))
      (is (string= (format nil
                           "M 10 0 o 0 10, -10 10, 0.707106769 o -10 0, -10 ~
                           -10, 0.707106769 o 0 -10, 10 -10, 0.707106769 o 10 ~
                           0, 10 10, 0.707106769 z")
                   (gsk:path-to-string (gsk:path-builder-to-path builder)))))))

;;;     gsk_path_builder_add_layout

#+crategus
(test gsk-path-builder-add-layout
  (glib-test:with-check-memory (label layout)
    (let ((builder (gsk:path-builder-new)))
      (setf layout (gtk:label-layout (setf label (gtk:label-new "-"))))
      (is (= 2 (g:object-ref-count layout)))
      (is-false (gsk:path-builder-add-layout builder layout))
      (is (string= (format nil "M 0.40625 9.796875 ~
                                L 0.40625 8.71875 ~
                                L 3.828125 8.71875 ~
                                L 3.828125 9.796875 ~
                                Z")
                   (gsk:path-to-string (gsk:path-builder-to-path builder))))
      ;; Remove references
      (is (string= "" (setf (gtk:label-label label) ""))))))

#+windows
(test gsk-path-builder-add-layout
  (glib-test:with-check-memory (label layout)
    (let ((builder (gsk:path-builder-new)))
      (setf layout (gtk:label-layout (setf label (gtk:label-new "-"))))
      (is (= 2 (g:object-ref-count layout)))
      (is-false (gsk:path-builder-add-layout builder layout))
      (is (string= (format nil "M 4 10 L 1 10 L 1 9 L 4 9 Z")
                   (gsk:path-to-string (gsk:path-builder-to-path builder))))
      ;; Remove references
      (is (string= "" (setf (gtk:label-label label) ""))))))

;;;     gsk_path_builder_add_path

(test gsk-path-builder-add-path
  (let ((builder (gsk:path-builder-new))
        (path (gsk:path-parse "M 10 20 h 100 v 200 h -100 z")))
    (is-false (gsk:path-builder-add-path builder path))
    (is (string= "M 10 20 h 100 v 200 h -100 z"
                 (gsk:path-to-string (gsk:path-builder-to-path builder))))))

;;;     gsk_path_builder_add_reverse_path

(test gsk-path-builder-add-reverse-path
  (let ((builder (gsk:path-builder-new))
        (path (gsk:path-parse "M 10 20 h 100 v 200 h -100 z")))
    (is-false (gsk:path-builder-add-reverse-path builder path))
    (is (string= "M 110 20 h -100 v 200 h 100 z"
                 (gsk:path-to-string (gsk:path-builder-to-path builder))))))

;;;     gsk_path_builder_add_rounded_rect

(test gsk-path-builder-add-rounded-rect
  (let ((builder (gsk:path-builder-new)))
    (cffi:with-foreign-object (rect '(:struct gsk:rounded-rect))
      (graphene:with-objects ((bounds graphene:rect-t 0 10 20 30)
                              (top-left graphene:size-t 0.1 0.1)
                              (top-right graphene:size-t 0.2 0.2)
                              (bottom-right graphene:size-t 0.3 0.3)
                              (bottom-left graphene:size-t 0.4 0.4))
        (setf rect
              (gsk:rounded-rect-init rect bounds top-left
                                                 top-right
                                                 bottom-right
                                                 bottom-left))
        (is-false (gsk:path-builder-add-rounded-rect builder rect))
        (is (string= (format nil
                             "M 0.100000001 10 L 19.7999992 10 O 20 10, 20 ~
                             10.1999998, 0.707106769 L 20 39.7000008 O 20 40, ~
                             19.7000008 40, 0.707106769 L 0.400000006 40 O 0 ~
                             40, 0 39.5999985, 0.707106769 L 0 10.1000004 O 0 ~
                             10, 0.100000001 10, 0.707106769 Z")
                     (gsk:path-to-string
                         (gsk:path-builder-to-path builder))))))))

;;;     gsk_path_builder_add_segment

(test gsk-path-builder-add-segment
  (let* ((builder (gsk:path-builder-new))
         (path (gsk:path-parse "M 10 20 h 100 v 200 h -100 z"))
         (start (gsk:path-start-point path))
         (end (gsk:path-end-point path)))
    (is-false (gsk:path-builder-add-segment builder path start end))
    (is (string= "M 10 20 L 110 20 L 110 220 L 10 220 L 10 20"
                 (gsk:path-to-string (gsk:path-builder-to-path builder))))))

;;;     gsk_path_builder_add_cairo_path

(test gsk-path-builder-add-cairo-path
  (cairo:with-recording-surface (surface :color)
    (cairo:with-context (context surface)
      (cairo:new-path context)
      (cairo:rectangle context 10 20 30 40)
      (let ((path (cairo:copy-path-flat context))
            (builder (gsk:path-builder-new)))
        (is-false (gsk:path-builder-add-cairo-path builder path))
        (is (string= "M 10 20 L 40 20 L 40 60 L 10 60 Z M 10 20"
                     (gsk:path-to-string (gsk:path-builder-to-path builder))))))))

;;;     gsk_path_builder_arc_to
;;;     gsk_path_builder_close
;;;     gsk_path_builder_conic_to
;;;     gsk_path_builder_cubic_to
;;;     gsk_path_builder_html_arc_to
;;;     gsk_path_builder_line_to
;;;     gsk_path_builder_move_to
;;;     gsk_path_builder_quad_to

;;;     gsk_path_builder_rel_arc_to
;;;     gsk_path_builder_rel_conic_to
;;;     gsk_path_builder_rel_cubic_to
;;;     gsk_path_builder_rel_html_arc_to
;;;     gsk_path_builder_rel_line_to
;;;     gsk_path_builder_rel_move_to
;;;     gsk_path_builder_rel_quad_to
;;;     gsk_path_builder_rel_svg_arc_to
;;;     gsk_path_builder_svg_arc_to
;;;
;;;     gsk_path_builder_ref
;;;     gsk_path_builder_unref

;;; 2025-05-30
