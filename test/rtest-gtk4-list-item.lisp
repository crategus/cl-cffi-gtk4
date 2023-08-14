(in-package :gtk-test)

(def-suite gtk-list-item :in gtk-suite)
(in-suite gtk-list-item)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkListItem

(test gtk-list-item-class
  ;; Type check
  (is (g:type-is-object "GtkListItem"))
  ;; Check the registered name
  (is (eq 'gtk:list-item
          (glib:symbol-for-gtype "GtkListItem")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkListItem")
          (g:gtype (cffi:foreign-funcall "gtk_list_item_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkListItem")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkListItem")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkListItem")))
  ;; Check the properties
  (is (equal '("activatable" "child" "item" "position" "selectable" "selected")
             (list-properties "GtkListItem")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkListItem")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkListItem" GTK-LIST-ITEM
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                                :TYPE-INITIALIZER "gtk_list_item_get_type")
                               ((ACTIVATABLE GTK-LIST-ITEM-ACTIVATABLE
                                 "activatable" "gboolean" T T)
                                (CHILD GTK-LIST-ITEM-CHILD "child" "GtkWidget"
                                 T T)
                                (ITEM GTK-LIST-ITEM-ITEM "item" "GObject" T
                                 NIL)
                                (POSITION GTK-LIST-ITEM-POSITION "position"
                                          "guint" T NIL)
                                (SELECTABLE GTK-LIST-ITEM-SELECTABLE
                                 "selectable" "gboolean" T T)
                                (SELECTED GTK-LIST-ITEM-SELECTED "selected"
                                 "gboolean" T NIL)))
             (gobject:get-g-type-definition "GtkListItem"))))

;;; --- Properties -------------------------------------------------------------

;;;     activatable
;;;     child
;;;     item
;;;     position
;;;     selectable
;;;     selected

(test gtk-list-item-properties
  (let ((item (make-instance 'gtk:list-item)))
    (is-true (gtk:list-item-activatable item))
    (is-false (gtk:list-item-child item))
    (is-false (gtk:list-item-item item))
    (is (= 4294967295 (gtk:list-item-position item)))
    (is-true (gtk:list-item-selectable item))
    (is-false (gtk:list-item-selected item))))

;;; --- 2023-8-13 --------------------------------------------------------------
