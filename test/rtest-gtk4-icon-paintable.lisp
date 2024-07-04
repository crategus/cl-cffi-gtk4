(in-package :gtk-test)

(def-suite gtk-icon-paintable :in gtk-suite)
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
             (gtk-test:list-interfaces "GtkSymbolicPaintable")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-G-INTERFACE "GtkSymbolicPaintable"
                                          GTK-SYMBOLIC-PAINTABLE
                    (:EXPORT T :TYPE-INITIALIZER
                     "gtk_symbolic_paintable_get_type"))
             (gobject:get-g-type-definition "GtkSymbolicPaintable"))))

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
             (gtk-test:list-children "GtkIconPaintable")))
  ;; Check interfaces
  (is (equal '("GdkPaintable" "GtkSymbolicPaintable")
             (gtk-test:list-interfaces "GtkIconPaintable")))
  ;; Check class properties
  (is (equal '("file" "icon-name" "is-symbolic")
             (gtk-test:list-properties "GtkIconPaintable")))
  ;; Check signals
  (is (equal '()
             (gtk-test:list-signals "GtkIconPaintable")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkIconPaintable"
                                             GTK-ICON-PAINTABLE
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                        ("GdkPaintable" "GtkSymbolicPaintable")
                        :TYPE-INITIALIZER "gtk_icon_paintable_get_type")
                       ((FILE GTK-ICON-PAINTABLE-FILE "file" "GFile" T NIL)
                        (ICON-NAME GTK-ICON-PAINTABLE-ICON-NAME "icon-name"
                         "gchararray" T NIL)
                        (IS-SYMBOLIC GTK-ICON-PAINTABLE-IS-SYMBOLIC
                         "is-symbolic" "gboolean" T NIL)))
             (gobject:get-g-type-definition "GtkIconPaintable"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-icon-paintable-properties
  (let ((paintable (make-instance 'gtk:icon-paintable)))
    (is-false (gtk:icon-paintable-file paintable))
    (is-false (gtk:icon-paintable-icon-name paintable))
    (is-false (gtk:icon-paintable-is-symbolic paintable))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_icon_paintable_new_for_file

(test gtk-icon-paintable-new-for-file
  (let* ((file (g:file-new-for-path "gtk-logo-24.png"))
         (paintable (gtk:icon-paintable-new-for-file file 24 1)))
    (is (typep paintable 'gtk:icon-paintable))
    (is (string= "gtk-logo-24.png"
                 (g:file-basename (gtk:icon-paintable-file paintable))))
    (is-false (gtk:icon-paintable-icon-name paintable))
    (is-false (gtk:icon-paintable-is-symbolic paintable))))

;;; 2024-7-4
