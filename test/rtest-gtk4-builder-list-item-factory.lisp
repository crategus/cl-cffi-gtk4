(in-package :gtk-test)

(def-suite gtk-builder-list-item-factory :in gtk-list-widgets)
(in-suite gtk-builder-list-item-factory)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkBuilderListItemFactory

(test gtk-builder-list-item-factory-class
  ;; Check type
  (is (g:type-is-object "GtkBuilderListItemFactory"))
  ;; Check registered name
  (is (eq 'gtk:builder-list-item-factory
          (glib:symbol-for-gtype "GtkBuilderListItemFactory")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkBuilderListItemFactory")
          (g:gtype (cffi:foreign-funcall "gtk_builder_list_item_factory_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkListItemFactory")
          (g:type-parent "GtkBuilderListItemFactory")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkBuilderListItemFactory")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkBuilderListItemFactory")))
  ;; Check properties
  (is (equal '("bytes" "resource" "scope")
             (glib-test:list-properties "GtkBuilderListItemFactory")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkBuilderListItemFactory")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkBuilderListItemFactory"
                                      GTK:BUILDER-LIST-ITEM-FACTORY
                      (:SUPERCLASS GTK:LIST-ITEM-FACTORY
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER
                       "gtk_builder_list_item_factory_get_type")
                      ((BYTES BUILDER-LIST-ITEM-FACTORY-BYTES
                        "bytes" "GBytes" T NIL)
                       (RESOURCE BUILDER-LIST-ITEM-FACTORY-RESOURCE
                        "resource" "gchararray" T NIL)
                       (SCOPE BUILDER-LIST-ITEM-FACTORY-SCOPE
                        "scope" "GtkBuilderScope" T NIL)))
             (gobject:get-gtype-definition "GtkBuilderListItemFactory"))))

;;; --- Properties -------------------------------------------------------------

;;;     bytes
;;;     resource
;;;     scope

(test gtk-builder-list-item-factory-properties
  (let ((factory (make-instance 'gtk:builder-list-item-factory)))
    (is-false (gtk:builder-list-item-factory-bytes factory))
    (is-false (gtk:builder-list-item-factory-resource factory))
    (is-false (gtk:builder-list-item-factory-scope factory))))

;;; --- Functions --------------------------------------------------------------

(defvar *ui-definition*
"<interface>
   <template class='GtkListItem'>
     <property name='child'>
       <object class='GtkLabel'>
         <property name='xalign'>0</property>
         <binding name='label'>
         <lookup name='name' type='SettingsKey'>
           <lookup name='item'>GtkListItem</lookup>
         </lookup>
       </binding>
     </object>
   </property>
 </template>
</interface>
")

;;;     gtk_builder_list_item_factory_new_from_bytes

(test gtk-builder-list-item-factory-new-from-bytes
  (multiple-value-bind (data len)
      (cffi:foreign-string-alloc *ui-definition*)
    (let (;; The length of the data is (1- len)
          (bytes (g:bytes-new data (1- len))))
      (is (typep (gtk:builder-list-item-factory-new-from-bytes nil bytes)
                 'gtk:builder-list-item-factory)))))

;;;     gtk_builder_list_item_factory_new_from_resource

(test gtk-builder-list-item-factory-new-from-resource
  (g:with-resource (resource (glib-sys:sys-path "test/rtest-resource.gresource"))
    (let ((path "/com/crategus/test/gtk-builder-list-item-factory.ui")
          (scope (make-instance 'gtk:builder-cl-scope))
          factory)
      (is (typep (setf factory
                       (gtk:builder-list-item-factory-new-from-resource scope path))
                 'gtk:builder-list-item-factory))
)))

;;; 2025-3-16
