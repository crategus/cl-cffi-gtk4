(in-package :gtk-test)

(def-suite gtk-cell-renderer-accel :in gtk-suite)
(in-suite gtk-cell-renderer-accel)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCellRendererAccelMode

(test gtk-cell-renderer-accel-mode
  ;; Check type
  (is (g:type-is-enum "GtkCellRendererAccelMode"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkCellRendererAccelMode")
          (g:gtype (cffi:foreign-funcall "gtk_cell_renderer_accel_mode_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gtk:cell-renderer-accel-mode
          (glib:symbol-for-gtype "GtkCellRendererAccelMode")))
  ;; Check names
  (is (equal '("GTK_CELL_RENDERER_ACCEL_MODE_GTK"
               "GTK_CELL_RENDERER_ACCEL_MODE_OTHER")
             (list-enum-item-name "GtkCellRendererAccelMode")))
  ;; Check values
  (is (equal '(0 1)
             (list-enum-item-value "GtkCellRendererAccelMode")))
  ;; Check nick names
  (is (equal '("gtk" "other")
             (list-enum-item-nick "GtkCellRendererAccelMode")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkCellRendererAccelMode"
                                     GTK-CELL-RENDERER-ACCEL-MODE
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gtk_cell_renderer_accel_mode_get_type")
                                     (:GTK 0)
                                     (:OTHER 1))
             (gobject:get-g-type-definition "GtkCellRendererAccelMode"))))

;;;     GtkCellRendererAccel

(test gtk-cell-renderer-accel-class
  ;; Check type
  (is (g:type-is-object "GtkCellRendererAccel"))
  ;; Check registered name
  (is (eq 'gtk:cell-renderer-accel
          (glib:symbol-for-gtype "GtkCellRendererAccel")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkCellRendererAccel")
          (g:gtype (cffi:foreign-funcall "gtk_cell_renderer_accel_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkCellRendererText")
          (g:type-parent "GtkCellRendererAccel")))
  ;; Check children
  (is (equal '()
             (list-children "GtkCellRendererAccel")))
  ;; Check interfaces
  (is (equal '()
             (list-interfaces "GtkCellRendererAccel")))
  ;; Check properties
  (is (equal '("accel-key" "accel-mode" "accel-mods" "keycode")
             (list-properties "GtkCellRendererAccel")))
  ;; Check signals
  (is (equal '("accel-cleared" "accel-edited")
             (list-signals "GtkCellRendererAccel")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkCellRendererAccel"
                                             GTK-CELL-RENDERER-ACCEL
                               (:SUPERCLASS GTK-CELL-RENDERER-TEXT :EXPORT T
                                :INTERFACES NIL :TYPE-INITIALIZER
                                "gtk_cell_renderer_accel_get_type")
                               ((ACCEL-KEY GTK-CELL-RENDERER-ACCEL-ACCEL-KEY
                                 "accel-key" "guint" T T)
                                (ACCEL-MODE GTK-CELL-RENDERER-ACCEL-ACCEL-MODE
                                 "accel-mode" "GtkCellRendererAccelMode" T T)
                                (ACCEL-MODS GTK-CELL-RENDERER-ACCEL-ACCEL-MODS
                                 "accel-mods" "GdkModifierType" T T)
                                (KEYCODE GTK-CELL-RENDERER-ACCEL-KEYCODE
                                 "keycode" "guint" T T)))
             (gobject:get-g-type-definition "GtkCellRendererAccel"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-cell-renderer-accel-properties
  (let* ((gtk-init:*gtk-warn-deprecated* nil)
         (renderer (make-instance 'gtk:cell-renderer-accel)))
    (is (= 0 (gtk:cell-renderer-accel-accel-key renderer)))
    (is (eq :gtk (gtk:cell-renderer-accel-accel-mode renderer)))
    (is-false (gtk:cell-renderer-accel-accel-mods renderer))
    (is (= 0 (gtk:cell-renderer-accel-keycode renderer)))))

;;; --- Signals ----------------------------------------------------------------

;;;     accel-cleared

(test gtk-cell-renderer-accel-accel-cleared-signal
  (let* ((name "accel-cleared") (gtype "GtkCellRendererAccel")
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (string= gtype (g:type-name (g:signal-query-owner-type query))))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '("gchararray")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     accel-edited

(test gtk-cell-renderer-accel-accel-edited-signal
  (let* ((name "accel-edited") (gtype "GtkCellRendererAccel")
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (string= gtype (g:type-name (g:signal-query-owner-type query))))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '("gchararray" "guint" "GdkModifierType" "guint")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_cell_renderer_accel_new

(test gtk-cell-renderer-accel-new
  (let* ((gtk-init:*gtk-warn-deprecated* nil))
    (is (typep (gtk:cell-renderer-accel-new) 'gtk:cell-renderer-accel))))

;;; 2024-5-18
