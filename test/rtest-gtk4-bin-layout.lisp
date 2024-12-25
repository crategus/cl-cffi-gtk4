(in-package :gtk-test)

(def-suite gtk-bin-layout :in gtk-layout-managers)
(in-suite gtk-bin-layout)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkBinLayout

(test gtk-bin-layout-class
  ;; Check type
  (is (g:type-is-object "GtkBinLayout"))
  ;; Check registered name
  (is (eq 'gtk:bin-layout
          (glib:symbol-for-gtype "GtkBinLayout")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkBinLayout")
          (g:gtype (cffi:foreign-funcall "gtk_bin_layout_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkLayoutManager")
          (g:type-parent "GtkBinLayout")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkBinLayout")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkBinLayout")))
  ;; Check properties
  (is (equal '()
             (glib-test:list-properties "GtkBinLayout")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkBinLayout")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkBinLayout" GTK:BIN-LAYOUT
                      (:SUPERCLASS GTK:LAYOUT-MANAGER
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gtk_bin_layout_get_type")
                      NIL)
             (gobject:get-gtype-definition "GtkBinLayout"))))

;;; --- Functions --------------------------------------------------------------

;;;      gtk_bin_layout_new

(test gtk-bin-layout-new
  (is (typep (gtk:bin-layout-new) 'gtk:bin-layout)))

(test gtk-bin-layout-for-button
  (let* ((button (make-instance 'gtk:button)))
    (is (typep (gtk:widget-layout-manager button) 'gtk:bin-layout))
    (is-false (setf (gtk:widget-layout-manager button) nil))))

;;; 2024-10-19
