(in-package :gtk-test)

(def-suite gtk-snapshot :in gtk-miscellaneous)
(in-suite gtk-snapshot)

(defun draw-snapshot-clock (snapshot width height)
  (let ((black (gdk:rgba-new :red 0 :green 0 :blue 0 :alpha 1)))
    ;; save/restore is necessary so we can undo the transforms we start out with
    (gtk:snapshot-save snapshot)
    ;; First, we move the (0, 0) point to the center of the area so
    ;; we can draw everything relative to it.
    (graphene:with-point (point (/ width 2.0) (/ height 2))
      (gtk:snapshot-translate snapshot point))
    ;; Next we scale it, so that we can pretend that the clock is
    ;; 100px in size. That way, we don't need to do any complicated
    ;; math later. We use MIN() here so that we use the smaller
    ;; dimension for sizing. That way we don't overdraw but keep
    ;; the aspect ratio.
    (gtk:snapshot-scale snapshot (/ (min width height) 100.0)
                                 (/ (min width height) 100.0))
    ;; Now we have a circle with diameter 100px (and radius 50px) that
    ;; has its (0, 0) point at the center. Let's draw a simple clock into it.

    ;; First, draw a circle. This is a neat little trick to draw a circle
    ;; without requiring Cairo.
    ;; TODO:Improve the implementation of GskRoundedRect to avoid foreign
    ;; objects
    (cffi:with-foreign-object (outline '(:struct gsk:rounded-rect))
      (graphene:with-rect (rect -50 -50 100 100)
        (gsk:rounded-rect-init-from-rect outline rect 50)
        (gtk:snapshot-append-border snapshot
                                    outline
                                    '(4 4 4 4)
                                    (list black black black black))))
    ;; Next, draw the hour hand.
    ;; We do this using transforms again: Instead of computing where the angle
    ;; points to, we just rotate everything and then draw the hand as if it
    ;; was :00. We don't even need to care about am/pm here because rotations
    ;; just work.
    (let* ((zone local-time:+utc-zone+)
           ;; Create a timestamp with the actuell time
           (time (local-time:now))
           (hour (local-time:timestamp-hour time :timezone zone))
           (minute (local-time:timestamp-minute time :timezone zone))
           (second (local-time:timestamp-second time :timezone zone)))
      (gtk:snapshot-save snapshot)
      (gtk:snapshot-rotate snapshot (+ (* 30 hour) (* 0.5 minute)))
      (cffi:with-foreign-object (outline '(:struct gsk:rounded-rect))
        (graphene:with-rect (rect -2 -23 4 25)
          (gsk:rounded-rect-init-from-rect outline rect 2.0)
          (gtk:snapshot-push-rounded-clip snapshot outline)
          (gtk:snapshot-append-color snapshot
                                     black
                                     (gsk:rounded-rect-bounds outline))
          (gtk:snapshot-pop snapshot)
          (gtk:snapshot-restore snapshot)
          ;; And the same as above for the minute hand. Just make this one
          ;; longer so people can tell the hands apart.
          (gtk:snapshot-save snapshot)
          (gtk:snapshot-rotate snapshot (* 6 minute))
          (graphene:rect-init rect -2 -43 4 45)
          (gsk:rounded-rect-init-from-rect outline rect 2)
          (gtk:snapshot-push-rounded-clip snapshot outline)
          (gtk:snapshot-append-color snapshot
                                     black
                                     (gsk:rounded-rect-bounds outline))
          (gtk:snapshot-pop snapshot)
          (gtk:snapshot-restore snapshot)
          ;; and finally, the second indicator.
          (gtk:snapshot-save snapshot)
          (gtk:snapshot-rotate snapshot (* 6 second))
          (graphene:rect-init rect -2 -43 4 10)
          (gsk:rounded-rect-init-from-rect outline rect 2)
          (gtk:snapshot-push-rounded-clip snapshot outline)
          (gtk:snapshot-append-color snapshot
                                     black
                                     (gsk:rounded-rect-bounds outline))
          (gtk:snapshot-pop snapshot)
          (gtk:snapshot-restore snapshot))))
  ;; And finally, don't forget to restore the initial save() that
  ;; we did for the initial transformations.
  (gtk:snapshot-restore snapshot)))

;;; --- Types and Values -------------------------------------------------------

;;;     GtkSnapshot

(test gtk-snapshot-class
  ;; Check type
  (is (g:type-is-object "GtkSnapshot"))
  ;; Check registered name
  (is (eq 'gtk:snapshot
          (glib:symbol-for-gtype "GtkSnapshot")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkSnapshot")
          (g:gtype (cffi:foreign-funcall "gtk_snapshot_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GdkSnapshot")
          (g:type-parent "GtkSnapshot")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkSnapshot")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkSnapshot")))
  ;; Check class properties
  (is (equal '()
             (glib-test:list-properties "GtkSnapshot")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkSnapshot")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkSnapshot" GTK:SNAPSHOT
                      (:SUPERCLASS GDK:SNAPSHOT
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gtk_snapshot_get_type")
                      NIL)
             (gobject:get-gtype-definition "GtkSnapshot"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_snapshot_new

(test gtk-snapshot-new
  (glib-test:with-check-memory (snapshot)
    (is (typep (setf snapshot (gtk:snapshot-new)) 'gtk:snapshot))))

;;;     gtk_snapshot_to_node

(test gtk-snapshot-to-node.1
  (glib-test:with-check-memory (snapshot)
    (is (typep (setf snapshot (gtk:snapshot-new)) 'gtk:snapshot))
    (is-false (gtk:snapshot-to-node snapshot))))

(test gtk-snapshot-to-node.2
  (let ((filename "test/out/snapshot-draw-clock.png")
        node)
    (glib-test:with-check-memory (snapshot)
      (is (typep (setf snapshot (gtk:snapshot-new)) 'gtk:snapshot))
      (is-false (draw-snapshot-clock snapshot 100 100))
      (is (cffi:pointerp (setf node (gtk:snapshot-to-node snapshot))))
      (cairo:with-context-for-image-surface (context :argb32 100 100)
        (gsk:render-node-draw node context)

        (cairo:surface-write-to-png (cairo:target context)
                                    (glib-sys:sys-path filename))

))))

;;;     gtk_snapshot_to_paintable

(test gtk-snapshot-to-paintable.1
  (glib-test:with-check-memory (snapshot)
    (is (typep (setf snapshot (gtk:snapshot-new)) 'gtk:snapshot))
    (is (string= "GtkRenderNodePaintable"
                 (g:type-name
                     (g:type-from-instance
                         (gtk:snapshot-to-paintable snapshot)))))))

(test gtk-snapshot-to-paintable.2
  (glib-test:with-check-memory (snapshot)
    (is (typep (setf snapshot (gtk:snapshot-new)) 'gtk:snapshot))
    (graphene:with-rect (rect 10 20 100 200)
      (is (string= "GtkRenderNodePaintable"
                   (g:type-name
                       (g:type-from-instance
                           (gtk:snapshot-to-paintable snapshot rect))))))))

;;;     gtk_snapshot_push_opacity

(test gtk-snapshot-push-opacity
  (glib-test:with-check-memory (snapshot)
    (let ((path1 (glib-sys:sys-path "test/out/snapshot-push-opacity.txt"))
          (path2 (glib-sys:sys-path "test/out/snapshot-push-opacity.png"))
          (builder (gsk:path-builder-new))
          node)
      (is (typep (setf snapshot (gtk:snapshot-new)) 'gtk:snapshot))
      (graphene:with-rect (bounds 0 0 100 100)
        ;; Draw snapshot with push opacity
        (is-false (gtk:snapshot-push-opacity snapshot 0.6))
        (is-false (gsk:path-builder-add-rect builder bounds))
        (is-false (gtk:snapshot-append-fill snapshot
                                            (gsk:path-builder-to-path builder)
                                            :winding
                                            (gdk:rgba-new :red 1 :alpha 1)))
        (is-false (gtk:snapshot-pop snapshot))
        ;; Store render node
        (setf node (gtk:snapshot-to-node snapshot))
        ;; Export render node to text file and image file
        (is-true (gsk:render-node-write-to-file node path1))
        (cairo:with-context-for-image-surface (context :argb32 100 100)
          (gsk:render-node-draw node context)
          (cairo:surface-write-to-png (cairo:target context) path2)
          ;; Free render node
          (gsk:render-node-unref node))))))

;;;     gtk_snapshot_push_color_matrix

(test gtk-snapshot-push-color-matrix
  (glib-test:with-check-memory (snapshot)
    (let ((path1 (glib-sys:sys-path "test/out/snapshot-push-color-matrix.txt"))
          (path2 (glib-sys:sys-path "test/out/snapshot-push-color-matrix.png"))
          (builder (gsk:path-builder-new))
          node)

    (is (typep (setf snapshot (gtk:snapshot-new)) 'gtk:snapshot))

    (graphene:with-objects ((vec3 graphene:vec3-t 0.0 0.0 1.0)
                            (matrix graphene:matrix-t 10.0 (vec3 graphene:vec3-t)))
      (graphene:with-vec4 (vec 0.1 0.2 0.3 0.4)

        (is-false (gtk:snapshot-push-color-matrix snapshot matrix vec))

        (graphene:with-rect (bounds 0 0 100 100)
          ;; Draw snapshot with push color-matrix
          (is-false (gsk:path-builder-add-rect builder bounds))
          (is-false (gtk:snapshot-append-fill snapshot
                                              (gsk:path-builder-to-path builder)
                                              :winding
                                              (gdk:rgba-new :red 1 :alpha 1)))

          (is-false (gtk:snapshot-pop snapshot))

          ;; Store render node
          (setf node (gtk:snapshot-to-node snapshot))
          ;; Export render node to text file and image file
          (is-true (gsk:render-node-write-to-file node path1))
          (cairo:with-context-for-image-surface (context :argb32 100 100)
            (gsk:render-node-draw node context)
            (cairo:surface-write-to-png (cairo:target context) path2)
            ;; Free render node
            (gsk:render-node-unref node))))))))

;;;     gtk_snapshot_push_repeat

(test gtk-snapshot-push-repeat
  (glib-test:with-check-memory (snapshot texture)
    (let ((path1 (glib-sys:sys-path "test/resource/ducky.png"))
          (path2 (glib-sys:sys-path "test/out/snapshot-push-repeat.txt"))
          (path3 (glib-sys:sys-path "test/out/snapshot-push-repeat.png"))
          node)
      (is (typep (setf  texture
                        (gdk:texture-new-from-filename path1)) 'gdk:texture))
      (is (typep (setf snapshot (gtk:snapshot-new)) 'gtk:snapshot))

      (graphene:with-rects ((bounds 0 0 400 400) (child-bounds 0 0 200 200))
        ;; Draw snapshot with push repeat
        (is-false (gtk:snapshot-push-repeat snapshot bounds child-bounds))
        (is-false (gtk:snapshot-append-texture snapshot texture bounds))
        (is-false (gtk:snapshot-pop snapshot))
        ;; Store render node
        (setf node (gtk:snapshot-to-node snapshot))
        ;; Export render node to text file and image file
        (is-true (gsk:render-node-write-to-file node path2))
        (cairo:with-context-for-image-surface (context :argb32 400 400)
          (gsk:render-node-draw node context)
          (cairo:surface-write-to-png (cairo:target context) path3)
          ;; Free render node
          (gsk:render-node-unref node))))))

;;;     gtk_snapshot_push_clip

(test gtk-snapshot-push-clip
  (glib-test:with-check-memory (snapshot)
    (let ((path1 (glib-sys:sys-path "test/out/snapshot-push-clip.txt"))
          (path2 (glib-sys:sys-path "test/out/snapshot-push-clip.png"))
          (color (gdk:rgba-parse "orange"))
          node)

    (is (typep (setf snapshot (gtk:snapshot-new)) 'gtk:snapshot))

    (graphene:with-rect (bounds 10 10 80 80)

      ;; Draw snapshot with push clip
      (is-false (gtk:snapshot-push-clip snapshot bounds))
      (is-false (gtk:snapshot-append-color snapshot color bounds))
      (is-false (gtk:snapshot-pop snapshot))

      ;; Store render node
      (setf node (gtk:snapshot-to-node snapshot))
      ;; Export render node to text file and image file
      (is-true (gsk:render-node-write-to-file node path1))
      (cairo:with-context-for-image-surface (context :argb32 100 100)
        (gsk:render-node-draw node context)
        (cairo:surface-write-to-png (cairo:target context) path2)
        ;; Free render node
        (gsk:render-node-unref node))))))

;;;     gtk_snapshot_push_rounded_clip

(test gtk-snapshot-push-rounded-clip
  (glib-test:with-check-memory (snapshot)
    (let ((path1 (glib-sys:sys-path "test/out/snapshot-push-rounded-clip.txt"))
          (path2 (glib-sys:sys-path "test/out/snapshot-push-rounded-clip.png"))
          (color (gdk:rgba-parse "orange"))
          node)

    (is (typep (setf snapshot (gtk:snapshot-new)) 'gtk:snapshot))

    (cffi:with-foreign-object (outline '(:struct gsk:rounded-rect))
      (graphene:with-rects ((rect -50 -50 120 120) (bounds 0 0 100 100))
        (gsk:rounded-rect-init-from-rect outline rect 50)
        (gtk:snapshot-push-rounded-clip snapshot outline)
        (gtk:snapshot-append-color snapshot color bounds)
        (gtk:snapshot-pop snapshot)

        ;; Store render node
        (setf node (gtk:snapshot-to-node snapshot))
        ;; Export render node to text file and image file
        (is-true (gsk:render-node-write-to-file node path1))
        (cairo:with-context-for-image-surface (context :argb32 100 100)
          (gsk:render-node-draw node context)
          (cairo:surface-write-to-png (cairo:target context) path2)
          ;; Free render node
          (gsk:render-node-unref node)))))))

;;;     gtk_snapshot_push_cross_fade
;;;     gtk_snapshot_push_blend
;;;     gtk_snapshot_push_blur

;;;     gtk_snapshot_push_shadow

;; TODO: The output seems not to be corret

(test gtk-snapshot-push-shadow
  (glib-test:with-check-memory (snapshot)
    (let* ((path1 (glib-sys:sys-path "test/out/snapshot-push-shadow.txt"))
           (path2 (glib-sys:sys-path "test/out/snapshot-push-shadow.png"))
           (red (gdk:rgba-new :red 1 :green 0 :blue 0 :alpha 1))
           (blue (gdk:rgba-parse "blue"))
           (green (gdk:rgba-parse "green"))
           (black (gdk:rgba-parse "black"))
           (shadows (list (list blue 10 10 5)
                          (list green 20 20 6)
                          (list black 30 30 7)))
           node)
    (is (typep (setf snapshot (gtk:snapshot-new)) 'gtk:snapshot))

    (graphene:with-rect (bounds 10 10 80 80)

      ;; Draw snapshot with push shadow
      (gtk:snapshot-push-shadow snapshot shadows)
      (gtk:snapshot-append-color snapshot red bounds)
      (gtk:snapshot-pop snapshot)

      ;; Store render node
      (setf node (gtk:snapshot-to-node snapshot)))
      ;; Export render node to text file and image file
      (is-true (gsk:render-node-write-to-file node path1))
      (cairo:with-context-for-image-surface (context :argb32 100 100)
        (gsk:render-node-draw node context)
        (cairo:surface-write-to-png (cairo:target context) path2)
        ;; Free render node
        (gsk:render-node-unref node)))))

;;;     gtk_snapshot_push_debug
;;;     gtk_snapshot_push_gl_shader                         Deprecated 4.16
;;;     gtk_snapshot_push_mask                              Since 4.10

;;;     gtk_snapshot_push_fill                              Since 4.14

(test gtk-snapshot-push-fill
  (let ((path1 (glib-sys:sys-path "test/out/snapshot-push-fill.txt"))
        (path2 (glib-sys:sys-path "test/out/snapshot-push-fill.png"))
        (red (gdk:rgba-new :red 1 :green 0 :blue 0 :alpha 1))
        (builder (gsk:path-builder-new))
        (snapshot (gtk:snapshot-new))
        node)
    (graphene:with-rect (rect 10 10 80 80)
      (is-false (gsk:path-builder-add-rect builder rect))
      (is-false (gtk:snapshot-push-fill snapshot
                                        (gsk:path-builder-to-path builder)
                                        :winding))
      (is-false (gtk:snapshot-append-color snapshot red rect))
      (is-false (gtk:snapshot-pop snapshot))

      (setf node (gtk:snapshot-to-node snapshot))
      (is-true (gsk:render-node-write-to-file node path1))

      (cairo:with-context-for-image-surface (context :argb32 100 100)
        (gsk:render-node-draw node context)
        (cairo:surface-write-to-png (cairo:target context)
                                    (glib-sys:sys-path path2))))))

;;;     gtk_snapshot_push_stroke                            Since 4.14

(test gtk-snapshot-push-stroke
  (let ((path1 (glib-sys:sys-path "test/out/snapshot-push-stroke.txt"))
        (path2 (glib-sys:sys-path "test/out/snapshot-push-stroke.png"))
        (red (gdk:rgba-new :red 1 :green 0 :blue 0 :alpha 1))
        (builder (gsk:path-builder-new))
        (snapshot (gtk:snapshot-new))
        node)
    (graphene:with-rect (rect 10 10 80 80)
      (is-false (gsk:path-builder-add-rect builder rect))
      (is-false (gtk:snapshot-push-stroke snapshot
                                          (gsk:path-builder-to-path builder)
                                          (gsk:stroke-new 5)))
      (gtk:snapshot-append-color snapshot red rect)
      (is-false (gtk:snapshot-pop snapshot))
      (is (cffi:pointerp (setf node (gtk:snapshot-to-node snapshot))))
      (is-true (gsk:render-node-write-to-file node path1))

      (cairo:with-context-for-image-surface (context :argb32 100 100)
        (gsk:render-node-draw node context)
        (cairo:surface-write-to-png (cairo:target context)
                                    (glib-sys:sys-path path2))))))

;;;     gtk_snapshot_pop
;;;     gtk_snapshot_gl_shader_pop_texture                  Deprecated 4.16
;;;
;;;     gtk_snapshot_save
;;;     gtk_snapshot_restore
;;;     gtk_snapshot_transform
;;;     gtk_snapshot_transform_matrix
;;;     gtk_snapshot_translate
;;;     gtk_snapshot_translate_3d
;;;     gtk_snapshot_rotate
;;;     gtk_snapshot_rotate_3d
;;;     gtk_snapshot_scale
;;;     gtk_snapshot_scale_3d
;;;     gtk_snapshot_perspective

;;;     gtk_snapshot_append_node

(test gtk-snapshot-append-node
  (glib-test:with-check-memory (snapshot)
    (let ((path1 (glib-sys:sys-path "test/out/snapshot-append-node.txt"))
          (path2 (glib-sys:sys-path "test/out/snapshot-append-node.png"))
          (color-stops (list (list 0.0 (gdk:rgba-parse "red"))
                             (list 1.0 (gdk:rgba-parse "blue"))))
          node1 node2)
      (is (typep (setf snapshot (gtk:snapshot-new)) 'gtk:snapshot))
      (graphene:with-rect (bounds 0 0 200 200)
        (graphene:with-point (center 100 100)
          (setf node1
                (gsk:radial-gradient-node-new bounds
                                              center
                                              0.5 0.5
                                              0 180
                                              color-stops))
          ;; Draw snapshot with append node
          (is-false (gtk:snapshot-push-clip snapshot bounds))
          (is-false (gtk:snapshot-append-node snapshot node1))
          (is-false (gtk:snapshot-pop snapshot))
        ;; Store render node
        (setf node2 (gtk:snapshot-to-node snapshot))
        ;; Export render node to text file and image file
        (is-true (gsk:render-node-write-to-file node2 path1))
        (cairo:with-context-for-image-surface (context :argb32 200 200)
          (gsk:render-node-draw node2 context)
          (cairo:surface-write-to-png (cairo:target context) path2)
          ;; Free render node
          (gsk:render-node-unref node1)
          (gsk:render-node-unref node2)))))))

;;;     gtk_snapshot_append_cairo

(test gtk-snapshot-append-cairo
  (glib-test:with-check-memory (snapshot)
    (let ((path1 (glib-sys:sys-path "test/out/snapshot-append-cairo.txt"))
          (path2 (glib-sys:sys-path "test/out/snapshot-append-cairo.png"))
          node context)
      (is (typep (setf snapshot (gtk:snapshot-new)) 'gtk:snapshot))
      (graphene:with-rect (bounds 0 0 100 100)
        ;; Draw snapshot with append cairo
        (is-false (gtk:snapshot-push-clip snapshot bounds))
        (is-true (setf context (gtk:snapshot-append-cairo snapshot bounds)))
        ;; Drawing with Cairo
        (cairo:save context)
        ;; Clear surface
        (cairo:set-source-rgb context 1.0 1.0 1.0)
        (cairo:paint context)
        ;; Example is in 1.0 x 1.0 coordinate space
        (cairo:scale context 100 100)
        ;; Drawing code goes here
        (cairo:set-source-rgb context 0.0 0.5 1.0)
        (cairo:rectangle context 0.25 0.25 0.5 0.5)
        (cairo:fill context)
        (cairo:restore context))
        ;; Back to snapshot drawing
        (is-false (gtk:snapshot-pop snapshot))
        ;; Store render node
        (setf node (gtk:snapshot-to-node snapshot))
        ;; Export render node to text file and image file
        (is-true (gsk:render-node-write-to-file node path1))
        (cairo:with-context-for-image-surface (cr :argb32 100 100)
          (gsk:render-node-draw node cr)
          (cairo:surface-write-to-png (cairo:target cr) path2)
          ;; Free render node
          (gsk:render-node-unref node)
          (cairo:destroy context)))))

;;;     gtk_snapshot_append_texture

(test gtk-snapshot-append-texture
  (glib-test:with-check-memory (snapshot texture)
    (let ((path1 (glib-sys:sys-path "test/resource/ducky.png"))
          (path2 (glib-sys:sys-path "test/out/snapshot-append-texture.txt"))
          (path3 (glib-sys:sys-path "test/out/snapshot-append-texture.png"))
          node)
      (is (typep (setf  texture
                        (gdk:texture-new-from-filename path1)) 'gdk:texture))
      (is (typep (setf snapshot (gtk:snapshot-new)) 'gtk:snapshot))
      (graphene:with-rect (bounds 0 0 200 200)
        ;; Draw snapshot with append texture
        (is-false (gtk:snapshot-push-clip snapshot bounds))
        (is-false (gtk:snapshot-append-texture snapshot texture bounds))
        (is-false (gtk:snapshot-pop snapshot))
        ;; Store render node
        (setf node (gtk:snapshot-to-node snapshot))
        ;; Export render node to text file and image file
        (is-true (gsk:render-node-write-to-file node path2))
        (cairo:with-context-for-image-surface (context :argb32 200 200)
          (gsk:render-node-draw node context)
          (cairo:surface-write-to-png (cairo:target context) path3)
          ;; Free render node
          (gsk:render-node-unref node))))))

;;;     gtk_snapshot_append_color

(test gtk-snapshot-append-color
  (glib-test:with-check-memory (snapshot)
    (let ((path1 (glib-sys:sys-path "test/out/snapshot-append-color.txt"))
          (path2 (glib-sys:sys-path "test/out/snapshot-append-color.png"))
          (red (gdk:rgba-new :red 1 :green 0 :blue 0 :alpha 1))
          node)
    (is (typep (setf snapshot (gtk:snapshot-new)) 'gtk:snapshot))
    (graphene:with-rect (bounds 10 10 80 80)
      ;; Draw snapshot with append color
      (gtk:snapshot-push-clip snapshot bounds)
      (gtk:snapshot-append-color snapshot red bounds)
      (gtk:snapshot-pop snapshot)
      ;; Store render node
      (setf node (gtk:snapshot-to-node snapshot)))
      ;; Export render node to text file and image file
      (is-true (gsk:render-node-write-to-file node path1))
      (cairo:with-context-for-image-surface (context :argb32 100 100)
        (gsk:render-node-draw node context)
        (cairo:surface-write-to-png (cairo:target context) path2)
        ;; Free render node
        (gsk:render-node-unref node)))))

;;;     gtk_snapshot_append_layout

;;;     gtk_snapshot_append_linear_gradient

(test gtk-snapshot-append-linear-gradient
  (glib-test:with-check-memory (snapshot)
    (let ((path1 (glib-sys:sys-path "test/out/snapshot-append-linear-gradient.txt"))
          (path2 (glib-sys:sys-path "test/out/snapshot-append-linear-gradient.png"))
          (stops (list (list 0 (gdk:rgba-parse "red"))
                       (list 1 (gdk:rgba-parse "blue"))))
          node)
    (is (typep (setf snapshot (gtk:snapshot-new)) 'gtk:snapshot))

    (graphene:with-rect (bounds 0 0 100 100)
      ;; Draw snapshot with append linear gradient
      (is-false (gtk:snapshot-push-clip snapshot bounds))
      (graphene:with-points ((start 10 10) (end 90 90))

        (is-false (gtk:snapshot-append-linear-gradient snapshot
                                                       bounds
                                                       start
                                                       end
                                                       stops))
        (is-false (gtk:snapshot-pop snapshot))
        ;; Store render node
        (setf node (gtk:snapshot-to-node snapshot)))
        ;; Export render node to text file and image file
        (is-true (gsk:render-node-write-to-file node path1))
        (cairo:with-context-for-image-surface (context :argb32 100 100)
          (gsk:render-node-draw node context)
          (cairo:surface-write-to-png (cairo:target context) path2)
          ;; Free render node
          (gsk:render-node-unref node))))))

;;;     gtk_snapshot_append_repeating_linear_gradient
;;;     gtk_snapshot_append_conic_gradient
;;;     gtk_snapshot_append_border
;;;     gtk_snapshot_append_inset_shadow
;;;     gtk_snapshot_append_outset_shadow
;;;     gtk_snapshot_append_radial_gradient
;;;     gtk_snapshot_append_repeating_radial_gradient
;;;     gtk_snapshot_append_scaled_texture                 Since 4.10
;;;     gtk_snapshot_append_fill                           Since 4.14
;;;     gtk_snapshot_append_stroke                         Since 4.14
;;;
;;;     gtk_snapshot_render_insertion_cursor               Deprecated 4.10
;;;     gtk_snapshot_render_background                     Deprecated 4.10
;;;     gtk_snapshot_render_frame                          Deprecated 4.10
;;;     gtk_snapshot_render_focus                          Deprecated 4.10
;;;     gtk_snapshot_render_layout                         Deprecated 4.10

;;; 2025-09-18
