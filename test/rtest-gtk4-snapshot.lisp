(def-suite gtk-snapshot :in gtk-suite)
(in-suite gtk-snapshot)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkSnapshot

(test gtk-snapshot-class
  ;; Type check
  (is (g:type-is-object "GtkSnapshot"))
  ;; Check the registered name
  (is (eq 'gtk-snapshot
          (gobject:symbol-for-gtype "GtkSnapshot")))
  ;; Check the type initializer
  (is (eq (gtype "GtkSnapshot")
          (gtype (foreign-funcall "gtk_snapshot_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GdkSnapshot")
          (g-type-parent "GtkSnapshot")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "GtkSnapshot"))))
  ;; Check the interfaces
  (is (equal '()
             (mapcar #'g-type-name (g-type-interfaces "GtkSnapshot"))))
  ;; Check the class properties
  (is (equal '()
             (list-class-property-names "GtkSnapshot")))
  ;; Check the list of signals
  (is (equal '()
             (sort (mapcar #'g-signal-name
                           (g-signal-list-ids "GtkSnapshot"))
                   #'string<)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkSnapshot" GTK-SNAPSHOT
                       (:SUPERCLASS GDK-SNAPSHOT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_snapshot_get_type")
                       NIL)
             (get-g-type-definition "GtkSnapshot"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_snapshot_new
;;;     gtk_snapshot_to_node
;;;     gtk_snapshot_to_paintable
;;;     gtk_snapshot_free_to_node
;;;     gtk_snapshot_free_to_paintable
;;;     gtk_snapshot_push_opacity
;;;     gtk_snapshot_push_color_matrix
;;;     gtk_snapshot_push_repeat
;;;     gtk_snapshot_push_clip
;;;     gtk_snapshot_push_rounded_clip
;;;     gtk_snapshot_push_cross_fade
;;;     gtk_snapshot_push_blend
;;;     gtk_snapshot_push_blur
;;;     gtk_snapshot_push_shadow
;;;     gtk_snapshot_push_debug
;;;     gtk_snapshot_push_gl_shader
;;;     gtk_snapshot_pop
;;;     gtk_snapshot_gl_shader_pop_texture
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
;;;     gtk_snapshot_append_cairo
;;;     gtk_snapshot_append_texture
;;;     gtk_snapshot_append_color
;;;     gtk_snapshot_append_layout
;;;     gtk_snapshot_append_linear_gradient
;;;     gtk_snapshot_append_repeating_linear_gradient
;;;     gtk_snapshot_append_conic_gradient
;;;     gtk_snapshot_append_border
;;;     gtk_snapshot_append_inset_shadow
;;;     gtk_snapshot_append_outset_shadow
;;;     gtk_snapshot_append_radial_gradient
;;;     gtk_snapshot_append_repeating_radial_gradient
;;;     gtk_snapshot_render_insertion_cursor
;;;     gtk_snapshot_render_background
;;;     gtk_snapshot_render_frame
;;;     gtk_snapshot_render_focus
;;;     gtk_snapshot_render_layout

;;; 2022-7-10
