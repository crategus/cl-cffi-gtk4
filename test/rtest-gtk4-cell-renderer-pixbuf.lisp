(in-package :gtk-test)

(def-suite gtk-cell-renderer-pixbuf :in gtk-deprecated)
(in-suite gtk-cell-renderer-pixbuf)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCellRendererPixbuf

(test gtk-cell-renderer-pixbuf-class
  ;; Check type
  (is (g:type-is-object "GtkCellRendererPixbuf"))
  ;; Check registered name
  (is (eq 'gtk:cell-renderer-pixbuf
          (glib:symbol-for-gtype "GtkCellRendererPixbuf")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkCellRendererPixbuf")
          (g:gtype (cffi:foreign-funcall "gtk_cell_renderer_pixbuf_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkCellRenderer")
          (g:type-parent "GtkCellRendererPixbuf")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkCellRendererPixbuf")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkCellRendererPixbuf")))
  ;; Check properties
  (is (equal '("gicon" "icon-name" "icon-size" "pixbuf" "pixbuf-expander-closed"
               "pixbuf-expander-open" "texture")
             (glib-test:list-properties "GtkCellRendererPixbuf")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkCellRendererPixbuf")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkCellRendererPixbuf"
                                      GTK:CELL-RENDERER-PIXBUF
                      (:SUPERCLASS GTK:CELL-RENDERER
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gtk_cell_renderer_pixbuf_get_type")
                      ((GICON CELL-RENDERER-PIXBUF-GICON "gicon" "GIcon" T T)
                       (ICON-NAME CELL-RENDERER-PIXBUF-ICON-NAME
                        "icon-name" "gchararray" T T)
                       (ICON-SIZE CELL-RENDERER-PIXBUF-ICON-SIZE
                        "icon-size" "GtkIconSize" T T)
                       (PIXBUF CELL-RENDERER-PIXBUF-PIXBUF
                        "pixbuf" "GdkPixbuf" NIL T)
                       (PIXBUF-EXPANDER-CLOSED
                        CELL-RENDERER-PIXBUF-PIXBUF-EXPANDER-CLOSED
                        "pixbuf-expander-closed" "GdkPixbuf" T T)
                       (PIXBUF-EXPANDER-OPEN
                        CELL-RENDERER-PIXBUF-PIXBUF-EXPANDER-OPEN
                        "pixbuf-expander-open" "GdkPixbuf" T T)
                       (TEXTURE CELL-RENDERER-PIXBUF-TEXTURE
                        "texture" "GdkTexture" T T)))
             (gobject:get-gtype-definition "GtkCellRendererPixbuf"))))

;;; ---  Properties ------------------------------------------------------------

(test gtk-cell-renderer-pixbuf-properties
  (let* ((gtk-init:*gtk-warn-deprecated* nil)
         (renderer (make-instance 'gtk:cell-renderer-pixbuf)))
    (is-false (gtk:cell-renderer-pixbuf-gicon renderer))
    (is-false (gtk:cell-renderer-pixbuf-icon-name renderer))
    (is (eq :inherit (gtk:cell-renderer-pixbuf-icon-size renderer)))
    ;; Property pixbuf is not readable
    (signals (error) (gtk:cell-renderer-pixbuf-pixbuf renderer))
    (is-false (gtk:cell-renderer-pixbuf-pixbuf-expander-closed renderer))
    (is-false (gtk:cell-renderer-pixbuf-pixbuf-expander-open renderer))
    (is-false (gtk:cell-renderer-pixbuf-texture renderer))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_cell_renderer_pixbuf_new

(test gtk-cell-renderer-pixbuf-new
  (let ((gtk-init:*gtk-warn-deprecated* nil))
    (is (typep (gtk:cell-renderer-pixbuf-new) 'gtk:cell-renderer-pixbuf))))

;;; 2024-9-20
