(in-package :gtk-test)

(def-suite gtk-icon-paintable :in gtk-theming)
(in-suite gtk-icon-paintable)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkSymbolicPaintable

(test gtk-symbolic-paintable-interface
  ;; Check type
  (is (g:type-is-interface "GtkSymbolicPaintable"))
  ;; Check registered name
  (is (eq 'gtk:symbolic-paintable
          (glib:symbol-for-gtype "GtkSymbolicPaintable")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkSymbolicPaintable")
          (g:gtype (cffi:foreign-funcall "gtk_symbolic_paintable_get_type"
                                         :size))))
  ;; Check interface properties
  (is (equal '()
             (glib-test:list-interfaces "GtkSymbolicPaintable")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "GtkSymbolicPaintable"
                                         GTK:SYMBOLIC-PAINTABLE
                      (:EXPORT T
                       :TYPE-INITIALIZER "gtk_symbolic_paintable_get_type"))
             (gobject:get-gtype-definition "GtkSymbolicPaintable"))))

;;; gtk_symbolic_paintable_snapshot_symbolic

;;;     GtkIconPaintable

(test gtk-icon-paintable-class
  ;; Check type
  (is (g:type-is-object "GtkIconPaintable"))
  ;; Check registered name
  (is (eq 'gtk:icon-paintable
          (glib:symbol-for-gtype "GtkIconPaintable")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkIconPaintable")
          (g:gtype (cffi:foreign-funcall "gtk_icon_paintable_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkIconPaintable")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkIconPaintable")))
  ;; Check interfaces
  (is (equal '("GdkPaintable" "GtkSymbolicPaintable")
             (glib-test:list-interfaces "GtkIconPaintable")))
  ;; Check class properties
  (is (equal '("file" "icon-name" "is-symbolic" "scale" "size")
             (glib-test:list-properties "GtkIconPaintable")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkIconPaintable")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkIconPaintable" GTK:ICON-PAINTABLE
                      (:SUPERCLASS GOBJECT:OBJECT
                       :EXPORT T
                       :INTERFACES ("GdkPaintable" "GtkSymbolicPaintable")
                       :TYPE-INITIALIZER "gtk_icon_paintable_get_type")
                      ((FILE ICON-PAINTABLE-FILE "file" "GFile" T NIL)
                       (ICON-NAME ICON-PAINTABLE-ICON-NAME "icon-name"
                        "gchararray" T NIL)
                       (IS-SYMBOLIC ICON-PAINTABLE-IS-SYMBOLIC "is-symbolic"
                        "gboolean" T T)
                       (SCALE ICON-PAINTABLE-SCALE "scale" "gint" T T)
                       (SIZE ICON-PAINTABLE-SIZE "size" "gint" T T)))
             (gobject:get-gtype-definition "GtkIconPaintable"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-icon-paintable-properties
  (glib-test:with-check-memory (paintable)
    (is (typep (setf paintable
                     (make-instance 'gtk:icon-paintable)) 'gtk:icon-paintable))
    (is-false (gtk:icon-paintable-file paintable))
    (is-false (gtk:icon-paintable-icon-name paintable))
    (is-false (gtk:icon-paintable-is-symbolic paintable))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_icon_paintable_new_for_file

(test gtk-icon-paintable-new-for-file
  (glib-test:with-check-memory (paintable :strong 1)
    (let ((path (glib-sys:sys-path "gtk-logo-24.png")))

    (is (typep (setf paintable
                     (gtk:icon-paintable-new-for-file path 24 1))
               'gtk:icon-paintable))
    (is (string= "gtk-logo-24.png"
                 (g:file-basename (gtk:icon-paintable-file paintable))))
    (is-false (gtk:icon-paintable-icon-name paintable))
    (is-false (gtk:icon-paintable-is-symbolic paintable)))))

;;; 2026-01-31
