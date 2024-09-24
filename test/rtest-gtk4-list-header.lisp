(in-package :gtk-test)

(def-suite gtk-list-header :in gtk-suite)
(in-suite gtk-list-header)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkListHeader

(test gtk-list-header-class
  ;; Check type
  (is (g:type-is-object "GtkListHeader"))
  ;; Check registered name
  (is (eq 'gtk:list-header
          (glib:symbol-for-gtype "GtkListHeader")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkListHeader")
          (g:gtype (cffi:foreign-funcall "gtk_list_header_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkListHeader")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkListHeader")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkListHeader")))
  ;; Check properties
  (is (equal '("child" "end" "item" "n-items" "start")
             (glib-test:list-properties "GtkListHeader")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkListHeader")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkListHeader" GTK:LIST-HEADER
                      (:SUPERCLASS GOBJECT:OBJECT
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gtk_list_header_get_type")
                      ((CHILD LIST-HEADER-CHILD "child" "GtkWidget" T T)
                       (END LIST-HEADER-END "end" "guint" T NIL)
                       (ITEM LIST-HEADER-ITEM "item" "GObject" T NIL)
                       (N-ITEMS LIST-HEADER-N-ITEMS "n-items" "guint" T NIL)
                       (START LIST-HEADER-START "start" "guint" T NIL)))
             (gobject:get-gtype-definition "GtkListHeader"))))

;;; --- Properties -------------------------------------------------------------

;;;     child
;;;     end
;;;     item
;;;     n-items
;;;     start

(test gtk-list-header-properties
  (let ((header (make-instance 'gtk:list-header)))
    (is-false (gtk:list-header-child header))
    (is (= gtk:+invalid-list-position+ (gtk:list-header-end header)))
    (is-false (gtk:list-header-item header))
    (is (= 0 (gtk:list-header-n-items header)))
    (is (= gtk:+invalid-list-position+ (gtk:list-header-start header)))))

;;; 2024-9-19

