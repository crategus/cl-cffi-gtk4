(in-package :gtk-test)

(def-suite gtk-list-item-factory :in gtk-list-widgets)
(in-suite gtk-list-item-factory)

;;;     GtkListItemFactory

(test gtk-list-item-factory-class
  ;; Check type
  (is (g:type-is-object "GtkListItemFactory"))
  ;; Check registered name
  (is (eq 'gtk:list-item-factory
          (glib:symbol-for-gtype "GtkListItemFactory")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkListItemFactory")
          (g:gtype (cffi:foreign-funcall "gtk_list_item_factory_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkListItemFactory")))
  ;; Check children
  (is (or (equal '("GtkBuilderListItemFactory" "GtkSignalListItemFactory")
                 (glib-test:list-children "GtkListItemFactory"))
          (equal '("GtkBuilderListItemFactory" "GtkColumnListItemFactory"
                   "GtkSignalListItemFactory")
                 (glib-test:list-children "GtkListItemFactory"))))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkListItemFactory")))
  ;; Check properties
  (is (equal '()
             (glib-test:list-properties "GtkListItemFactory")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkListItemFactory")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkListItemFactory" GTK:LIST-ITEM-FACTORY
                      (:SUPERCLASS GOBJECT:OBJECT
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gtk_list_item_factory_get_type")
                      NIL)
             (gobject:get-gtype-definition "GtkListItemFactory"))))

;;; 2024-11-27
