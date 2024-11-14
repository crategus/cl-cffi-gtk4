(in-package :gtk-test)

(def-suite gsk-path :in gsk-suite)
(in-suite gsk-path)

;;; --- Types and Values -------------------------------------------------------

;;;     GskPathForeachFlags

(test gsk-path-foreach-flags
  ;; Check type
  (is (g:type-is-flags "GskPathForeachFlags"))
  ;; Check registered name
  (is (eq 'gsk:path-foreach-flags
          (glib:symbol-for-gtype "GskPathForeachFlags")))
  ;; Check type initializer
  (is (eq (g:gtype "GskPathForeachFlags")
          (g:gtype (cffi:foreign-funcall "gsk_path_foreach_flags_get_type" :size))))
  ;; Check names
  (is (equal '("GSK_PATH_FOREACH_ALLOW_ONLY_LINES" "GSK_PATH_FOREACH_ALLOW_QUAD"
               "GSK_PATH_FOREACH_ALLOW_CUBIC" "GSK_PATH_FOREACH_ALLOW_CONIC")
             (glib-test:list-flags-item-names "GskPathForeachFlags")))
  ;; Check values
  (is (equal '(0 1 2 4)
             (glib-test:list-flags-item-values "GskPathForeachFlags")))
  ;; Check nick names
  (is (equal '("only-lines" "quad" "cubic" "conic")
             (glib-test:list-flags-item-nicks "GskPathForeachFlags")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-GFLAGS "GskPathForeachFlags" GSK:PATH-FOREACH-FLAGS
                       (:EXPORT T
                        :TYPE-INITIALIZER "gsk_path_foreach_flags_get_type")
                       (:ONLY-LINES 0)
                       (:QUAD 1)
                       (:CUBIC 2)
                       (:CONIC 4))
             (gobject:get-gtype-definition "GskPathForeachFlags"))))

;;;     GskPathOperation

(test gsk-path-operation
  ;; Check type
  (is (g:type-is-enum "GskPathOperation"))
  ;; Check type initializer
  (is (eq (g:gtype "GskPathOperation")
          (g:gtype (cffi:foreign-funcall "gsk_path_operation_get_type" :size))))
  ;; Check registered name
  (is (eq 'gsk:path-operation
          (glib:symbol-for-gtype "GskPathOperation")))
  ;; Check names
  (is (equal '("GSK_PATH_MOVE" "GSK_PATH_CLOSE" "GSK_PATH_LINE" "GSK_PATH_QUAD"
               "GSK_PATH_CUBIC" "GSK_PATH_CONIC")
             (glib-test:list-enum-item-names "GskPathOperation")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5)
             (glib-test:list-enum-item-values "GskPathOperation")))
  ;; Check nick names
  (is (equal '("move" "close" "line" "quad" "cubic" "conic")
             (glib-test:list-enum-item-nicks "GskPathOperation")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GskPathOperation" GSK:PATH-OPERATION
                       (:EXPORT T
                        :TYPE-INITIALIZER "gsk_path_operation_get_type")
                       (:MOVE 0)
                       (:CLOSE 1)
                       (:LINE 2)
                       (:QUAD 3)
                       (:CUBIC 4)
                       (:CONIC 5))
             (gobject:get-gtype-definition "GskPathOperation"))))

;;;     GskPathDirection

(test gsk-path-direction
  ;; Check type
  (is (g:type-is-enum "GskPathDirection"))
  ;; Check type initializer
  (is (eq (g:gtype "GskPathDirection")
          (g:gtype (cffi:foreign-funcall "gsk_path_direction_get_type" :size))))
  ;; Check registered name
  (is (eq 'gsk:path-direction
          (glib:symbol-for-gtype "GskPathDirection")))
  ;; Check names
  (is (equal '("GSK_PATH_FROM_START" "GSK_PATH_TO_START" "GSK_PATH_TO_END"
               "GSK_PATH_FROM_END")
             (glib-test:list-enum-item-names "GskPathDirection")))
  ;; Check values
  (is (equal '(0 1 2 3)
             (glib-test:list-enum-item-values "GskPathDirection")))
  ;; Check nick names
  (is (equal '("from-start" "to-start" "to-end" "from-end")
             (glib-test:list-enum-item-nicks "GskPathDirection")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GskPathDirection" GSK:PATH-DIRECTION
                       (:EXPORT T
                        :TYPE-INITIALIZER "gsk_path_direction_get_type")
                       (:FROM-START 0)
                       (:TO-START 1)
                       (:TO-END 2)
                       (:FROM-END 3))
             (gobject:get-gtype-definition "GskPathDirection"))))

;;;     GskLineCap

(test gsk-line-cap
  ;; Check type
  (is (g:type-is-enum "GskLineCap"))
  ;; Check type initializer
  (is (eq (g:gtype "GskLineCap")
          (g:gtype (cffi:foreign-funcall "gsk_line_cap_get_type" :size))))
  ;; Check registered name
  (is (eq 'gsk:line-cap
          (glib:symbol-for-gtype "GskLineCap")))
  ;; Check names
  (is (equal '("GSK_LINE_CAP_BUTT" "GSK_LINE_CAP_ROUND" "GSK_LINE_CAP_SQUARE")
             (glib-test:list-enum-item-names "GskLineCap")))
  ;; Check values
  (is (equal '(0 1 2)
             (glib-test:list-enum-item-values "GskLineCap")))
  ;; Check nick names
  (is (equal '("butt" "round" "square")
             (glib-test:list-enum-item-nicks "GskLineCap")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GskLineCap" GSK:LINE-CAP
                       (:EXPORT T
                        :TYPE-INITIALIZER "gsk_line_cap_get_type")
                       (:BUTT 0)
                       (:ROUND 1)
                       (:SQUARE 2))
             (gobject:get-gtype-definition "GskLineCap"))))

;;;     GskLineJoin

(test gsk-line-join
  ;; Check type
  (is (g:type-is-enum "GskLineJoin"))
  ;; Check type initializer
  (is (eq (g:gtype "GskLineJoin")
          (g:gtype (cffi:foreign-funcall "gsk_line_join_get_type" :size))))
  ;; Check registered name
  (is (eq 'gsk:line-join
          (glib:symbol-for-gtype "GskLineJoin")))
  ;; Check names
  (is (equal '("GSK_LINE_JOIN_MITER" "GSK_LINE_JOIN_ROUND"
               "GSK_LINE_JOIN_BEVEL")
             (glib-test:list-enum-item-names "GskLineJoin")))
  ;; Check values
  (is (equal '(0 1 2)
             (glib-test:list-enum-item-values "GskLineJoin")))
  ;; Check nick names
  (is (equal '("miter" "round" "bevel")
             (glib-test:list-enum-item-nicks "GskLineJoin")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GskLineJoin" GSK:LINE-JOIN
                       (:EXPORT T
                        :TYPE-INITIALIZER "gsk_line_join_get_type")
                       (:MITER 0)
                       (:ROUND 1)
                       (:BEVEL 2))
             (gobject:get-gtype-definition "GskLineJoin"))))

;;;     GskFillRule

(test gsk-fill-rule
  ;; Check type
  (is (g:type-is-enum "GskFillRule"))
  ;; Check type initializer
  (is (eq (g:gtype "GskFillRule")
          (g:gtype (cffi:foreign-funcall "gsk_fill_rule_get_type" :size))))
  ;; Check registered name
  (is (eq 'gsk:fill-rule
          (glib:symbol-for-gtype "GskFillRule")))
  ;; Check names
  (is (equal '("GSK_FILL_RULE_WINDING" "GSK_FILL_RULE_EVEN_ODD")
             (glib-test:list-enum-item-names "GskFillRule")))
  ;; Check values
  (is (equal '(0 1)
             (glib-test:list-enum-item-values "GskFillRule")))
  ;; Check nick names
  (is (equal '("winding" "even-odd")
             (glib-test:list-enum-item-nicks "GskFillRule")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GskFillRule" GSK:FILL-RULE
                       (:EXPORT T
                        :TYPE-INITIALIZER "gsk_fill_rule_get_type")
                       (:WINDING 0)
                       (:EVEN-ODD 1))
             (gobject:get-gtype-definition "GskFillRule"))))

;;;     GskPathMeasure

(test gsk-path-measure-boxed
  ;; Check type
  (is (g:type-is-boxed "GskPathMeasure"))
  ;; Check type initializer
  (is (eq (g:gtype "GskPathMeasure")
          (g:gtype (cffi:foreign-funcall "gsk_path_measure_get_type" :size))))
  ;; Check registered name
  (is (eq 'gsk:path-measure
          (glib:symbol-for-gtype "GskPathMeasure"))))

;;;     GskPathPoint

(test gsk-path-point-boxed
  ;; Check type
  (is (g:type-is-boxed "GskPathPoint"))
  ;; Check type initializer
  (is (eq (g:gtype "GskPathPoint")
          (g:gtype (cffi:foreign-funcall "gsk_path_point_get_type" :size))))
  ;; Check registered name
  (is (eq 'gsk:path-point
          (glib:symbol-for-gtype "GskPathPoint"))))

;;;     GskStroke

(test gsk-stroke-boxed
  ;; Check type
  (is (g:type-is-boxed "GskStroke"))
  ;; Check type initializer
  (is (eq (g:gtype "GskStroke")
          (g:gtype (cffi:foreign-funcall "gsk_stroke_get_type" :size))))
  ;; Check registered name
  (is (eq 'gsk:stroke
          (glib:symbol-for-gtype "GskStroke"))))

;;;     GskPath

(test gsk-path-boxed
  ;; Check type
  (is (g:type-is-boxed "GskPath"))
  ;; Check type initializer
  (is (eq (g:gtype "GskPath")
          (g:gtype (cffi:foreign-funcall "gsk_path_get_type" :size))))
  ;; Check registered name
  (is (eq 'gsk:path
          (glib:symbol-for-gtype "GskPath"))))

;;; -- Functions ---------------------------------------------------------------

;;;     gsk_path_measure_new

(test gsk-path-measure-new
  (let ((path (gsk:path-parse "M 10 20 h 100 v 200 h -100 z")))
    (is (typep (gsk:path-measure-new path) 'gsk:path-measure))))

;;;     gsk_path_measure_new_with_tolerance

(test gsk-path-measure-new-with-tolerance
  (let ((path (gsk:path-parse "M 10 20 h 100 v 200 h -100 z")))
    (is (typep (gsk:path-measure-new-with-tolerance path 1/10) 'gsk:path-measure))))

;;;     gsk_path_measure_get_length

(test gsk-path-measure-length
  (let* ((path (gsk:path-parse "M 10 20 h 100 v 200 h -100 z"))
         (measure (gsk:path-measure-new path)))
    (is (= 600.0 (gsk:path-measure-length measure)))))

;;;     gsk_path_measure_get_path

(test gsk-path-measure-path
  (let* ((path (gsk:path-parse "M 10 20 h 100 v 200 h -100 z"))
         (measure (gsk:path-measure-new path)))
    (is (typep (gsk:path-measure-path measure) 'gsk:path))))

;;;     gsk_path_measure_get_point

(test gsk-path-measure-point
  (let* ((path (gsk:path-parse "M 10 20 h 100 v 200 h -100 z"))
         (measure (gsk:path-measure-new path)))
    (is (typep (gsk:path-measure-point measure 1/10) 'gsk:path-point))))

;;;     gsk_path_measure_get_tolerance

(test gsk-path-measure-tolerance
  (let* ((path (gsk:path-parse "M 10 20 h 100 v 200 h -100 z"))
         (measure (gsk:path-measure-new path)))
    (is (= 0.5 (gsk:path-measure-tolerance measure)))))

;;;     gsk_path_point_get_curvature

(test gsk-path-point-curvature
  (let ((builder (gsk:path-builder-new))
        path pathpoint)
    (graphene:with-point (point 0 0)
      (is-false (gsk:path-builder-add-circle builder point 10))
      (is (typep (setf path (gsk:path-builder-to-path builder)) 'gsk:path))

      (is (cffi:pointerp (graphene:point-init point 10 0)))
      (is (typep (setf pathpoint
                       (gsk:path-closest-point path point 0.1)) 'gsk:path-point))

      (is (= 0.1 (gsk:path-point-curvature pathpoint path :from-start point)))
      (is (= 0 (graphene:point-x point)))
      (is (= 0 (graphene:point-y point))))))

;;;     gsk_path_point_get_distance
;;;     gsk_path_point_get_position
;;;     gsk_path_point_get_rotation
;;;     gsk_path_point_get_tangent

;;;     gsk_path_point_compare
;;;     gsk_path_point_copy
;;;     gsk_path_point_equal
;;;     gsk_path_point_free

;;;     gsk_path_parse
;;;
;;;     GskPathForeachFunc
;;;     gsk_path_foreach
;;;
;;;     gsk_path_get_bounds

;;;     gsk_path_get_closest_point

(test gsk-path-closest-point
  (let ((builder (gsk:path-builder-new))
        path)
    (graphene:with-point (point 0 0)
      (is-false (gsk:path-builder-add-circle builder point 10))
      (is (typep (setf path (gsk:path-builder-to-path builder)) 'gsk:path))
      (is (cffi:pointerp (graphene:point-init point 0 10)))
      (multiple-value-bind (result distance)
          (gsk:path-closest-point path point 0.1)
        (is (typep result 'gsk:path-point))
        (is (= 0 distance))))))

;;;     gsk_path_get_start_point

(test gsk-path-start-point
  (let ((path (gsk:path-parse "M 10 20 h 100 v 200 h -100 z")))
    (is (typep (gsk:path-start-point path) 'gsk:path-point))))

;;;     gsk_path_get_end_point

(test gsk-path-end-point
  (let ((path (gsk:path-parse "M 10 20 h 100 v 200 h -100 z")))
    (is (typep (gsk:path-end-point path) 'gsk:path-point))))

;;;     gsk_path_get_stroke_bounds
;;;     gsk_path_in_fill
;;;     gsk_path_is_closed
;;;     gsk_path_is_empty
;;;     gsk_path_print
;;;     gsk_path_ref
;;;     gsk_path_unref
;;;     gsk_path_to_cairo
;;;     gsk_path_to_string

;;; 2024-11-8
