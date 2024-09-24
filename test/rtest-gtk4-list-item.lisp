(in-package :gtk-test)

(def-suite gtk-list-item :in gtk-suite)
(in-suite gtk-list-item)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkListItem

(test gtk-list-item-class
  ;; Check type
  (is (g:type-is-object "GtkListItem"))
  ;; Check registered name
  (is (eq 'gtk:list-item
          (glib:symbol-for-gtype "GtkListItem")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkListItem")
          (g:gtype (cffi:foreign-funcall "gtk_list_item_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkListItem")))
  ;; Check children
  (is (equal '("GtkColumnViewCell")
             (glib-test:list-children "GtkListItem")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkListItem")))
  ;; Check properties
  (is (equal '("accessible-description" "accessible-label" "activatable"
               "child" "focusable" "item" "position" "selectable" "selected")
             (glib-test:list-properties "GtkListItem")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkListItem")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkListItem" GTK:LIST-ITEM
                      (:SUPERCLASS GOBJECT:OBJECT
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gtk_list_item_get_type")
                      ((ACCESSIBLE-DESCRIPTION LIST-ITEM-ACCESSIBLE-DESCRIPTION
                        "accessible-description" "gchararray" T T)
                       (ACCESSIBLE-LABEL LIST-ITEM-ACCESSIBLE-LABEL
                        "accessible-label" "gchararray" T T)
                       (ACTIVATABLE LIST-ITEM-ACTIVATABLE
                        "activatable" "gboolean" T T)
                       (CHILD LIST-ITEM-CHILD "child" "GtkWidget" T T)
                       (FOCUSABLE LIST-ITEM-FOCUSABLE "focusable" "gboolean" T T)
                       (ITEM LIST-ITEM-ITEM "item" "GObject" T NIL)
                       (POSITION LIST-ITEM-POSITION "position" "guint" T NIL)
                       (SELECTABLE LIST-ITEM-SELECTABLE
                        "selectable" "gboolean" T T)
                       (SELECTED LIST-ITEM-SELECTED
                        "selected" "gboolean" T NIL)))
             (gobject:get-gtype-definition "GtkListItem"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-list-item-properties
  (let ((item (make-instance 'gtk:list-item)))
    (is-true (gtk:list-item-activatable item))
    (is-false (gtk:list-item-child item))
    (is-false (gtk:list-item-item item))
    (is (= 4294967295 (gtk:list-item-position item)))
    (is-true (gtk:list-item-selectable item))
    (is-false (gtk:list-item-selected item))))

;;; 2024-7-4
