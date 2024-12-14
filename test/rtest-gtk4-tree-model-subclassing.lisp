(in-package :gtk-test)

(def-suite gtk-tree-model-subclassing :in gtk-suite)
(in-suite gtk-tree-model-subclassing)

(in-package :gtk)

(gobject:define-vtable ("GtkTreeModel" tree-model)
  (:skip parent-instance (:pointer (:struct g:type-interface)))
  ;; some signals
  (:skip tree-model-row-changed :pointer)
  (:skip tree-model-row-inserted :pointer)
  (:skip tree-model-row-has-child-toggled :pointer)
  (:skip tree-model-row-deleted :pointer)
  (:skip tree-model-rows-reordered :pointer)
  ;; methods
  (get-flags (tree-model-flags (tree-model g:object)))
  (get-n-columns (:int (tree-model g:object)))
  (get-column-type (g:type-t (tree-model g:object) (index :int)))
  (get-iter (:boolean
             (tree-model g:object)
             (iter (g:boxed tree-iter))
             (path (g:boxed tree-path))))
  (get-path ((g:boxed tree-path :return)
             (tree-model g:object)
             (iter (g:boxed tree-iter))))
  (get-value (:void
              (tree-model g:object)
              (iter (g:boxed tree-iter))
              (n :int)
              (value (:pointer (:struct g:value))))
             :impl-call
             ((tree-model iter n)
              (multiple-value-bind (v type)
                  (tree-model-get-value-impl tree-model iter n)
                (g:value-set value v type))))
  (iter-next (:boolean
              (tree-model g:object)
              (iter (g:boxed tree-iter))))
  (iter-previous (:boolean
                  (tree-model g:object)
                  (iter (g:boxed tree-iter))))
  (iter-children (:boolean
                  (tree-model g:object)
                  (iter (g:boxed tree-iter))
                  (parent (g:boxed tree-iter))))
  (iter-has-child (:boolean
                   (tree-model g:object)
                   (iter (g:boxed tree-iter))))
  (iter-n-children (:int
                    (tree-model g:object)
                    (iter (g:boxed tree-iter))))
  (iter-nth-child (:boolean
                   (tree-model g:object)
                   (iter (g:boxed tree-iter))
                   (parent (g:boxed tree-iter))
                   (n :int)))
  (iter-parent (:boolean
                (tree-model g:object)
                (iter (g:boxed tree-iter))
                (child (g:boxed tree-iter))))
  (ref-node (:void
             (tree-model g:object)
             (iter (g:boxed tree-iter))))
  (unref-node (:void
               (tree-model g:object)
                (iter (g:boxed tree-iter)))))

;; Implementation of array-list-store

(defclass array-list-store (tree-model)
  ((items :initform (make-array 0 :adjustable t :fill-pointer t)
          :reader store-items)
   (column-getters :initform (make-array 0 :adjustable t :fill-pointer t)
                    :reader store-getters)
   (column-types :initform (make-array 0 :adjustable t :fill-pointer t)
                 :reader store-types))
  (:metaclass gobject::gobject-class)
  (:gname . "GtkArrayListStore"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'array-list-store))

(gobject:register-object-type-implementation "GtkArrayListStore"
                                             array-list-store
                                             "GObject"
                                             ("GtkTreeModel")
                                             nil)

;;; ----------------------------------------------------------------------------

(defun store-items-count (store)
  (length (store-items store)))

(defun store-item (store index)
  (aref (store-items store) index))

#+nil
(defun store-add-item (store item)
  (vector-push-extend item (store-items store))
  (let* ((path (make-instance 'tree-path))
         (iter (make-instance 'tree-iter)))
    (setf (tree-path-indices path) (list (1- (length (store-items store)))))
    (setf (tree-iter-stamp iter)
          0
          (tree-iter-user-data iter)
          (1- (length (store-items store))))
    (g:signal-emit store "row-inserted" path iter)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'store-items-count)
  (export 'store-item)
  (export 'store-add-item))

;;; ----------------------------------------------------------------------------

(in-package :gtk-test)

#+nil
(defun create-and-fill-array-store ()
  (let ((listdata '("Name1" "Name2" "Name3" "Name4" "Name5"))
        ;; Create a new list store with three columns
        (liststore (make-instance 'gtk:array-list-store
                                  :column-types
                                  '("gint" "gchararray" "gboolean"))))
    ;; Fill in some data
    (iter (for data in listdata)
          (for i from 0)
          ;; Add a new row to the model
          (gtk:list-store-set liststore
                              (gtk:list-store-append liststore)
                              i
                              data
                              nil))
    ;; Modify a particular row
    (let ((path (gtk:tree-path-new-from-string "2")))
      (gtk:list-store-set-value liststore
                                (gtk:tree-model-iter liststore path)
                                2
                                t))
    ;; Return the new list store
    liststore))

(test gtk-array-list-store-class
  ;; Check type
  (is (g:type-is-object "GtkArrayListStore"))
  ;; Check registered name
  (is (eq 'gtk:array-list-store
          (glib:symbol-for-gtype "GtkArrayListStore")))
  ;; Check type initializer
;  (is (eq (g:gtype "GObject")
;          (g:gtype (cffi:foreign-funcall "gtk_array_list_store_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkArrayListStore")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkArrayListStore")))
  ;; Check interfaces
  (is (equal '("GtkTreeModel")
             (glib-test:list-interfaces "GtkArrayListStore")))
  ;; Check properties
  (is (equal '()
             (glib-test:list-properties "GtkArrayListStore")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkArrayListStore")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkArrayListStore" GTK:ARRAY-LIST-STORE
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES ("GtkTreeModel"))
                       NIL)
             (gobject:get-gtype-definition "GtkArrayListStore"))))

(test array-list-store-item
  (let ((*gtk-warn-deprecated* nil))
    (let ((store (make-instance 'gtk:array-list-store
                                :column-types
                                '("gint" "gchararray" "gboolean"))))
      (is (= 0 (gtk:store-items-count store))))))

;;; 2024-4-27
