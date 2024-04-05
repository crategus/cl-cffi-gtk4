(in-package :gtk-test)

(def-suite gtk-list-header :in gtk-suite)
(in-suite gtk-list-header)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkListHeader

(test gtk-list-header-class
  ;; Type check
  (is (g:type-is-object "GtkListHeader"))
  ;; Check the registered name
  (is (eq 'gtk:list-header
          (glib:symbol-for-gtype "GtkListHeader")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkListHeader")
          (g:gtype (cffi:foreign-funcall "gtk_list_header_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkListHeader")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkListHeader")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkListHeader")))
  ;; Check the properties
  (is (equal '("child" "end" "item" "n-items" "start")
             (list-properties "GtkListHeader")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkListHeader")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkListHeader" GTK-LIST-HEADER
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                                :TYPE-INITIALIZER "gtk_list_header_get_type")
                               ((CHILD GTK-LIST-HEADER-CHILD "child"
                                 "GtkWidget" T T)
                                (END GTK-LIST-HEADER-END "end" "guint" T NIL)
                                (ITEM GTK-LIST-HEADER-ITEM "item" "GObject" T
                                 NIL)
                                (N-ITEMS GTK-LIST-HEADER-N-ITEMS "n-items"
                                 "guint" T NIL)
                                (START GTK-LIST-HEADER-START "start" "guint" T
                                 NIL)))
             (gobject:get-g-type-definition "GtkListHeader"))))

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

;;; --- 2023-11-27 -------------------------------------------------------------

