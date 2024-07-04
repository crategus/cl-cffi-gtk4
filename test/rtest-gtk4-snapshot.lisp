(in-package :gtk-test)

(def-suite gtk-snapshot :in gtk-suite)
(in-suite gtk-snapshot)

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
             (gtk-test:list-children "GtkSnapshot")))
  ;; Check interfaces
  (is (equal '()
             (gtk-test:list-interfaces "GtkSnapshot")))
  ;; Check class properties
  (is (equal '()
             (gtk-test:list-properties "GtkSnapshot")))
  ;; Check signals
  (is (equal '()
             (gtk-test:list-signals "GtkSnapshot")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkSnapshot" GTK-SNAPSHOT
                       (:SUPERCLASS GDK-SNAPSHOT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_snapshot_get_type")
                       NIL)
             (gobject:get-g-type-definition "GtkSnapshot"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_snapshot_new

(test gtk-snapshot-new
  (is (typep (gtk:snapshot-new) 'gtk:snapshot)))

;;;     gtk_snapshot_append_border
;;;     gtk_snapshot_append_cairo
;;;     gtk_snapshot_append_color
;;;     gtk_snapshot_append_conic_gradient
;;;     gtk_snapshot_append_fill                           Since 4.14 unstable
;;;     gtk_snapshot_append_inset_shadow
;;;     gtk_snapshot_append_layout
;;;     gtk_snapshot_append_linear_gradient
;;;     gtk_snapshot_append_node
;;;     gtk_snapshot_append_outset_shadow
;;;     gtk_snapshot_append_radial_gradient
;;;     gtk_snapshot_append_repeating_linear_gradient
;;;     gtk_snapshot_append_repeating_radial_gradient
;;;     gtk_snapshot_append_scaled_texture                 Since 4.10
;;;     gtk_snapshot_append_stroke                         Since 4.14 unstable
;;;     gtk_snapshot_append_texture
;;;     gtk_snapshot_free_to_node
;;;     gtk_snapshot_free_to_paintable
;;;     gtk_snapshot_gl_shader_pop_texture
;;;     gtk_snapshot_perspective
;;;     gtk_snapshot_pop
;;;     gtk_snapshot_push_blend
;;;     gtk_snapshot_push_blur
;;;     gtk_snapshot_push_clip
;;;     gtk_snapshot_push_color_matrix
;;;     gtk_snapshot_push_cross_fade
;;;     gtk_snapshot_push_debug
;;;     gtk_snapshot_push_fill                             Since 4.14 unstable
;;;     gtk_snapshot_push_gl_shader
;;;     gtk_snapshot_push_mask                             Since 4.10
;;;     gtk_snapshot_push_opacity
;;;     gtk_snapshot_push_repeat
;;;     gtk_snapshot_push_rounded_clip
;;;     gtk_snapshot_push_shadow
;;;     gtk_snapshot_push_stroke                           Since 4.14 unstable
;;;     gtk_snapshot_render_background                     Deprecated 4.10
;;;     gtk_snapshot_render_focus                          Deprecated 4.10
;;;     gtk_snapshot_render_frame                          Deprecated 4.10
;;;     gtk_snapshot_render_insertion_cursor               Deprecated 4.10
;;;     gtk_snapshot_render_layout                         Deprecated 4.10
;;;     gtk_snapshot_restore
;;;     gtk_snapshot_rotate
;;;     gtk_snapshot_rotate_3d
;;;     gtk_snapshot_save
;;;     gtk_snapshot_scale
;;;     gtk_snapshot_scale_3d
;;;     gtk_snapshot_to_node
;;;     gtk_snapshot_to_paintable
;;;     gtk_snapshot_transform
;;;     gtk_snapshot_transform_matrix
;;;     gtk_snapshot_translate
;;;     gtk_snapshot_translate_3d

;;; 2024-7-4
