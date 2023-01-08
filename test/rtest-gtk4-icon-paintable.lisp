(in-package :gtk-test)

(def-suite gtk-icon-paintable :in gtk-suite)
(in-suite gtk-icon-paintable)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkSymbolicPaintable

(test symbolic-paintable-interface
  ;; Type check
  (is (g:type-is-interface "GtkSymbolicPaintable"))
  ;; Check the registered name
  (is (eq 'gtk:symbolic-paintable
          (gobject:symbol-for-gtype "GtkSymbolicPaintable")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkSymbolicPaintable")
          (g:gtype (foreign-funcall "gtk_symbolic_paintable_get_type" :size))))
  ;; Get the names of the interface properties.
  (is (equal '()
             (list-interfaces "GtkSymbolicPaintable")))
  ;; Get the interface definition
  (is (equal '(DEFINE-G-INTERFACE "GtkSymbolicPaintable" GTK-SYMBOLIC-PAINTABLE
                    (:EXPORT T :TYPE-INITIALIZER
                     "gtk_symbolic_paintable_get_type"))
             (gobject:get-g-type-definition "GtkSymbolicPaintable"))))

;;;     GtkIconPaintable

(test icon-paintable-class
  ;; Type check
  (is (g:type-is-object "GtkIconPaintable"))
  ;; Check the registered name
  (is (eq 'gtk:icon-paintable
          (gobject:symbol-for-gtype "GtkIconPaintable")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkIconPaintable")
          (g:gtype (foreign-funcall "gtk_icon_paintable_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkIconPaintable")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkIconPaintable")))
  ;; Check the interfaces
  (is (equal '("GdkPaintable" "GtkSymbolicPaintable")
             (list-interfaces "GtkIconPaintable")))
  ;; Check the class properties
  (is (equal '("file" "icon-name" "is-symbolic")
             (list-properties "GtkIconPaintable")))
  ;; Check the list of signals
  (is (equal '()
             (list-signals "GtkIconPaintable")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkIconPaintable" GTK-ICON-PAINTABLE
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

;;;     file
;;;     icon-name
;;;     is-symbolic

(test icon-paintable-properties
  (let ((paintable (make-instance 'gtk:icon-paintable)))
    (is-false (gtk:icon-paintable-file paintable))
    (is-false (gtk:icon-paintable-icon-name paintable))
    (is-false (gtk:icon-paintable-is-symbolic paintable))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_icon_paintable_new_for_file

(test icon-paintable-new-for-file
  (let* ((file (g:file-new-for-path "gtk-logo-24.png"))
         (paintable (gtk:icon-paintable-new-for-file file 24 1)))
    (is (typep paintable 'gtk:icon-paintable))
    (is (string= "gtk-logo-24.png"
                 (g:file-basename (gtk:icon-paintable-file paintable))))
    (is-false (gtk:icon-paintable-icon-name paintable))
    (is-false (gtk:icon-paintable-is-symbolic paintable))))

;;; 2022-11-10
