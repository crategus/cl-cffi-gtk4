(in-package :gtk-test)

(def-suite gtk-cell-renderer :in gtk-suite)
(in-suite gtk-cell-renderer)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCellRendererState

(test gtk-cell-renderer-state
  ;; Check the type
  (is (g:type-is-flags "GtkCellRendererState"))
  ;; Check the registered name
  (is (eq 'gtk:cell-renderer-state
          (glib:symbol-for-gtype "GtkCellRendererState")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkCellRendererState")
          (g:gtype (cffi:foreign-funcall "gtk_cell_renderer_state_get_type"
                                         :size))))
  ;; Check the names
  (is (equal '("GTK_CELL_RENDERER_SELECTED" "GTK_CELL_RENDERER_PRELIT"
               "GTK_CELL_RENDERER_INSENSITIVE" "GTK_CELL_RENDERER_SORTED"
               "GTK_CELL_RENDERER_FOCUSED" "GTK_CELL_RENDERER_EXPANDABLE"
               "GTK_CELL_RENDERER_EXPANDED")
             (list-flags-item-name "GtkCellRendererState")))
  ;; Check the values
  (is (equal '(1 2 4 8 16 32 64)
             (list-flags-item-value "GtkCellRendererState")))
  ;; Check the nick names
  (is (equal '("selected" "prelit" "insensitive" "sorted" "focused"
               "expandable" "expanded")
             (list-flags-item-nick "GtkCellRendererState")))
  ;; Check the flags definition
  (is (equal '(DEFINE-G-FLAGS "GtkCellRendererState"
                              GTK-CELL-RENDERER-STATE
                              (:EXPORT T
                               :TYPE-INITIALIZER
                               "gtk_cell_renderer_state_get_type")
                              (:SELECTED 1)
                              (:PRELIT 2)
                              (:INSENSITIVE 4)
                              (:SORTED 8)
                              (:FOCUSED 16)
                              (:EXPANDABLE 32)
                              (:EXPANDED 64))
             (gobject:get-g-type-definition "GtkCellRendererState"))))

;;;     GtkCellRendererMode

(test gdk-cell-renderer-mode
  ;; Check the type
  (is (g:type-is-enum "GtkCellRendererMode"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkCellRendererMode")
          (g:gtype (cffi:foreign-funcall "gtk_cell_renderer_mode_get_type"
                                         :size))))
  ;; Check the registered name
  (is (eq 'gtk:cell-renderer-mode
          (glib:symbol-for-gtype "GtkCellRendererMode")))
  ;; Check the names
  (is (equal '("GTK_CELL_RENDERER_MODE_INERT"
               "GTK_CELL_RENDERER_MODE_ACTIVATABLE"
               "GTK_CELL_RENDERER_MODE_EDITABLE")
             (list-enum-item-name "GtkCellRendererMode")))
  ;; Check the values
  (is (equal '(0 1 2)
             (list-enum-item-value "GtkCellRendererMode")))
  ;; Check the nick names
  (is (equal '("inert" "activatable" "editable")
             (list-enum-item-nick "GtkCellRendererMode")))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkCellRendererMode"
                             GTK-CELL-RENDERER-MODE
                             (:EXPORT T
                              :TYPE-INITIALIZER
                              "gtk_cell_renderer_mode_get_type")
                             (:INERT 0)
                             (:ACTIVATABLE 1)
                             (:EDITABLE 2))
             (gobject:get-g-type-definition "GtkCellRendererMode"))))

;;;     GtkCellRenderer

(test gtk-cell-renderer-class
  ;; Type check
  (is (g:type-is-object "GtkCellRenderer"))
  ;; Check the registered name
  (is (eq 'gtk:cell-renderer
          (glib:symbol-for-gtype "GtkCellRenderer")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkCellRenderer")
          (g:gtype (cffi:foreign-funcall "gtk_cell_renderer_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GInitiallyUnowned")
          (g:type-parent "GtkCellRenderer")))
  ;; Check the children
  (is (equal '("GtkCellRendererText" "GtkCellRendererPixbuf"
               "GtkCellRendererProgress" "GtkCellRendererToggle"
               "GtkCellRendererSpinner")
             (list-children "GtkCellRenderer")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkCellRenderer")))
  ;; Check the class properties
  (is (equal '("cell-background" "cell-background-rgba" "cell-background-set"
               "editing" "height" "is-expanded" "is-expander" "mode" "sensitive"
               "visible" "width" "xalign" "xpad" "yalign" "ypad")
             (list-properties "GtkCellRenderer")))
  ;; Check the list of signals
  (is (equal '("editing-canceled" "editing-started")
             (list-signals "GtkCellRenderer")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkCellRenderer" GTK-CELL-RENDERER
                       (:SUPERCLASS G-INITIALLY-UNOWNED :EXPORT T :INTERFACES
                        NIL :TYPE-INITIALIZER "gtk_cell_renderer_get_type")
                       ((CELL-BACKGROUND GTK-CELL-RENDERER-CELL-BACKGROUND
                         "cell-background" "gchararray" NIL T)
                        (CELL-BACKGROUND-RGBA
                         GTK-CELL-RENDERER-CELL-BACKGROUND-RGBA
                         "cell-background-rgba" "GdkRGBA" T T)
                        (CELL-BACKGROUND-SET
                         GTK-CELL-RENDERER-CELL-BACKGROUND-SET
                         "cell-background-set" "gboolean" T T)
                        (EDITING GTK-CELL-RENDERER-EDITING "editing" "gboolean"
                         T NIL)
                        (HEIGHT GTK-CELL-RENDERER-HEIGHT "height" "gint" T T)
                        (IS-EXPANDED GTK-CELL-RENDERER-IS-EXPANDED
                         "is-expanded" "gboolean" T T)
                        (IS-EXPANDER GTK-CELL-RENDERER-IS-EXPANDER
                         "is-expander" "gboolean" T T)
                        (MODE GTK-CELL-RENDERER-MODE "mode"
                         "GtkCellRendererMode" T T)
                        (SENSITIVE GTK-CELL-RENDERER-SENSITIVE "sensitive"
                         "gboolean" T T)
                        (VISIBLE GTK-CELL-RENDERER-VISIBLE "visible" "gboolean"
                         T T)
                        (WIDTH GTK-CELL-RENDERER-WIDTH "width" "gint" T T)
                        (XALIGN GTK-CELL-RENDERER-XALIGN "xalign" "gfloat" T T)
                        (XPAD GTK-CELL-RENDERER-XPAD "xpad" "guint" T T)
                        (YALIGN GTK-CELL-RENDERER-YALIGN "yalign" "gfloat" T T)
                        (YPAD GTK-CELL-RENDERER-YPAD "ypad" "guint" T T)))
             (gobject:get-g-type-definition "GtkCellRenderer"))))

;;; --- Properties -------------------------------------------------------------

;;;     cell-background
;;;     cell-background-rgba
;;;     cell-background-set
;;;     editing
;;;     height
;;;     is-expanded
;;;     is-expander
;;;     mode
;;;     sensitive
;;;     visible
;;;     width
;;;     xalign
;;;     xpad
;;;     yalign
;;;     ypad

(test gtk-cell-renderer-properties
  (let ((renderer (make-instance 'gtk-cell-renderer-text)))

    (signals (error) (gtk-cell-renderer-cell-background renderer))
    (is (typep (gtk-cell-renderer-cell-background-rgba renderer) 'gdk:rgba))
    (is-false (gtk-cell-renderer-cell-background-set renderer))
    (is-false (gtk-cell-renderer-editing renderer))
    (is (= -1 (gtk-cell-renderer-height renderer)))
    (is-false (gtk-cell-renderer-is-expanded renderer))
    (is-false (gtk-cell-renderer-is-expander renderer))
    (is (eq :inert (gtk-cell-renderer-mode renderer)))
    (is-true (gtk-cell-renderer-sensitive renderer))
    (is-true (gtk-cell-renderer-visible renderer))
    (is (= -1 (gtk-cell-renderer-width renderer)))
    (is (= 0.0 (gtk-cell-renderer-xalign renderer)))
    (is (= 2 (gtk-cell-renderer-xpad renderer)))
    (is (= 0.5 (gtk-cell-renderer-yalign renderer)))
    (is (= 2 (gtk-cell-renderer-ypad renderer)))
))

;;; --- Signals ----------------------------------------------------------------

;;;     editing-canceled

(test gtk-cell-renderer-editing-canceled-signal
  ;; Query info for "editing-canceled" signal
  (let ((query (g-signal-query (g-signal-lookup "editing-canceled"
                                                "GtkCellRenderer"))))
    (is (string= "editing-canceled" (g-signal-query-signal-name query)))
    (is (string= "GtkCellRenderer"
                 (g-type-name (g-signal-query-owner-type query))))
    (is (equal '(:RUN-FIRST)
               (sort (g-signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g-type-name (g-signal-query-return-type query))))
    (is (equal '()
               (mapcar #'g-type-name (g-signal-query-param-types query))))
    (is-false (g-signal-query-signal-detail query))))

;;;     editing-started

(test gtk-cell-renderer-editing-started-signal
  ;; Query info for "editing-canceled" signal
  (let ((query (g-signal-query (g-signal-lookup "editing-started"
                                                "GtkCellRenderer"))))
    (is (string= "editing-started" (g-signal-query-signal-name query)))
    (is (string= "GtkCellRenderer"
                 (g-type-name (g-signal-query-owner-type query))))
    (is (equal '(:RUN-FIRST)
               (sort (g-signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g-type-name (g-signal-query-return-type query))))
    (is (equal '("GtkCellEditable" "gchararray")
               (mapcar #'g-type-name (g-signal-query-param-types query))))
    (is-false (g-signal-query-signal-detail query))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_cell_renderer_get_aligned_area
;;;     gtk_cell_renderer_snapshot
;;;     gtk_cell_renderer_activate
;;;     gtk_cell_renderer_start_editing
;;;     gtk_cell_renderer_stop_editing
;;;     gtk_cell_renderer_get_fixed_size
;;;     gtk_cell_renderer_set_fixed_size
;;;     gtk_cell_renderer_get_alignment
;;;     gtk_cell_renderer_set_alignment
;;;     gtk_cell_renderer_get_padding
;;;     gtk_cell_renderer_set_padding
;;;     gtk_cell_renderer_get_state
;;;     gtk_cell_renderer_is_activatable
;;;     gtk_cell_renderer_get_preferred_height
;;;     gtk_cell_renderer_get_preferred_height_for_width
;;;     gtk_cell_renderer_get_preferred_size
;;;     gtk_cell_renderer_get_preferred_width
;;;     gtk_cell_renderer_get_preferred_width_for_height
;;;     gtk_cell_renderer_get_request_mode

;;; --- 2023-5-29 --------------------------------------------------------------
