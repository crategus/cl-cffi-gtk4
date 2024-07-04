(in-package :gtk-test)

(def-suite gtk-list-item-factory :in gtk-suite)
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
                 (gtk-test:list-children "GtkListItemFactory"))
          (equal '("GtkBuilderListItemFactory" "GtkColumnListItemFactory"
                   "GtkSignalListItemFactory")
                 (gtk-test:list-children "GtkListItemFactory"))))
  ;; Check interfaces
  (is (equal '()
             (gtk-test:list-interfaces "GtkListItemFactory")))
  ;; Check properties
  (is (equal '()
             (gtk-test:list-properties "GtkListItemFactory")))
  ;; Check signals
  (is (equal '()
             (gtk-test:list-signals "GtkListItemFactory")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkListItemFactory"
                                             GTK-LIST-ITEM-FACTORY
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                                :TYPE-INITIALIZER
                                "gtk_list_item_factory_get_type")
                               NIL)
             (gobject:get-g-type-definition "GtkListItemFactory"))))

;;; ----------------------------------------------------------------------------

;;;     GtkSignalListItemFactory

(test gtk-signal-list-item-factory-class
  ;; Check type
  (is (g:type-is-object "GtkSignalListItemFactory"))
  ;; Check registered name
  (is (eq 'gtk:signal-list-item-factory
          (glib:symbol-for-gtype "GtkSignalListItemFactory")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkSignalListItemFactory")
          (g:gtype (cffi:foreign-funcall "gtk_signal_list_item_factory_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkListItemFactory")
          (g:type-parent "GtkSignalListItemFactory")))
  ;; Check children
  (is (equal '()
             (gtk-test:list-children "GtkSignalListItemFactory")))
  ;; Check interfaces
  (is (equal '()
             (gtk-test:list-interfaces "GtkSignalListItemFactory")))
  ;; Check properties
  (is (equal '()
             (gtk-test:list-properties "GtkSignalListItemFactory")))
  ;; Check signals
  (is (equal '("bind" "setup" "teardown" "unbind")
             (gtk-test:list-signals "GtkSignalListItemFactory")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkSignalListItemFactory"
                               GTK-SIGNAL-LIST-ITEM-FACTORY
                               (:SUPERCLASS GTK-LIST-ITEM-FACTORY :EXPORT T
                                :INTERFACES NIL :TYPE-INITIALIZER
                                "gtk_signal_list_item_factory_get_type")
                               NIL)
             (gobject:get-g-type-definition "GtkSignalListItemFactory"))))

;;; --- Signals ----------------------------------------------------------------

;;;     bind
;;;     setup
;;;     teardown
;;;     unbind

;;; --- Functions --------------------------------------------------------------

;;;     gtk_signal_list_item_factory_new

(test gtk-signal-list-item-factory-new
  (is (typep (gtk:signal-list-item-factory-new) 'gtk:signal-list-item-factory)))

;;; ----------------------------------------------------------------------------

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
             (gtk-test:list-children "GtkBuilderListItemFactory")))
  ;; Check interfaces
  (is (equal '()
             (gtk-test:list-interfaces "GtkBuilderListItemFactory")))
  ;; Check properties
  (is (equal '("bytes" "resource" "scope")
             (gtk-test:list-properties "GtkBuilderListItemFactory")))
  ;; Check signals
  (is (equal '()
             (gtk-test:list-signals "GtkBuilderListItemFactory")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkBuilderListItemFactory"
                               GTK-BUILDER-LIST-ITEM-FACTORY
                               (:SUPERCLASS GTK-LIST-ITEM-FACTORY :EXPORT T
                                :INTERFACES NIL :TYPE-INITIALIZER
                                "gtk_builder_list_item_factory_get_type")
                               ((BYTES GTK-BUILDER-LIST-ITEM-FACTORY-BYTES
                                 "bytes" "GBytes" T NIL)
                                (RESOURCE
                                 GTK-BUILDER-LIST-ITEM-FACTORY-RESOURCE
                                 "resource" "gchararray" T NIL)
                                (SCOPE GTK-BUILDER-LIST-ITEM-FACTORY-SCOPE
                                 "scope" "GtkBuilderScope" T NIL)))
             (gobject:get-g-type-definition "GtkBuilderListItemFactory"))))

;;; --- Properties -------------------------------------------------------------

;;;     bytes
;;;     resource
;;;     scope

(test gtk-builder-list-item-factory
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
         </interface>")

;;;     gtk_builder_list_item_factory_new_from_bytes

;; FIXME: Example is from the GTK documentation. What is wrong?
;; (gtk-test:6653): Gtk-WARNING **: 12:36:10.503: Failed to precompile template
;; for GtkBuilderListItemFactory: Fehler in Zeile 14, Zeichen 23: Dokument muss
;; mit einem Element beginnen (e.g. <book>)

#+nil
(test gtk-builder-list-item-factory-new-from-bytes
  (multiple-value-bind (data len)
      (cffi:foreign-string-alloc *ui-definition*)
    (let ((bytes (g:bytes-new data len)))
      (is (typep (gtk:builder-list-item-factory-new-from-bytes nil bytes)
                 'gtk:builder-list-item-factory)))))

;;;     gtk_builder_list_item_factory_new_from_resource

;;; 2024-7-4
