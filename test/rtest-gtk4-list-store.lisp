(in-package :gtk-test)

(def-suite gtk-list-store :in gtk-deprecated)
(in-suite gtk-list-store)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkListStore

(test gtk-list-store-class
  ;; Check type
  (is (g:type-is-object "GtkListStore"))
  ;; Check registered name
  (is (eq 'gtk:list-store
          (glib:symbol-for-gtype "GtkListStore")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkListStore")
          (g:gtype (cffi:foreign-funcall "gtk_list_store_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkListStore")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkListStore")))
  ;; Check interfaces
  (is (equal '("GtkTreeModel" "GtkTreeDragSource" "GtkTreeDragDest"
               "GtkTreeSortable" "GtkBuildable")
             (glib-test:list-interfaces "GtkListStore")))
  ;; Check properties
  (is (equal '()
             (glib-test:list-properties "GtkListStore")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkListStore")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkListStore" GTK:LIST-STORE
                      (:SUPERCLASS G:OBJECT
                       :EXPORT T
                       :INTERFACES
                       ("GtkBuildable" "GtkTreeDragDest" "GtkTreeDragSource"
                        "GtkTreeModel" "GtkTreeSortable")
                       :TYPE-INITIALIZER "gtk_list_store_get_type")
                      NIL)
             (gobject:get-gtype-definition "GtkListStore"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_list_store_new

(test gtk-list-store-new
  (let ((gtk-init:*gtk-warn-deprecated* nil))
    (is (typep (gtk:list-store-new "gint" "gchararray" "GdkPixbuf")
               'gtk:list-store))))

;;;     gtk_list_store_set

(test gtk-list-store-set
  (let ((gtk-init:*gtk-warn-deprecated* nil))
    (let ((model (gtk:list-store-new "gchararray" "gchararray" "guint")))

      (is (= 3 (gtk:tree-model-n-columns model)))
      (is (eq (g:gtype "gchararray") (gtk:tree-model-column-type model 0)))
      (is (eq (g:gtype "gchararray") (gtk:tree-model-column-type model 1)))
      (is (eq (g:gtype "guint") (gtk:tree-model-column-type model 2)))

      (is (typep (gtk:list-store-set model
                                     (gtk:list-store-append model)
                                     "Hans" "Müller" 1961)
                 'gtk:tree-iter))

      (is (string= "Hans"
                   (gtk:tree-model-value model
                                         (gtk:tree-model-iter-first model)
                                         0)))
      (is (string= "Müller"
                   (gtk:tree-model-value model
                                         (gtk:tree-model-iter-first model)
                                         1)))
      (is (= 1961 (gtk:tree-model-value model
                                      (gtk:tree-model-iter-first model)
                                      2))))))

;;;     gtk_list_store_set_value

(test gtk-list-store-set-value
  (let ((gtk-init:*gtk-warn-deprecated* nil))
    (let* ((model (gtk:list-store-new "gboolean" "guint" "gchararray"))
           (iter (gtk:list-store-append model)))

      (is-false (gtk:list-store-set-value model iter 0 t))
      (is-false (gtk:list-store-set-value model iter 1 199))
      (is-false (gtk:list-store-set-value model iter 2 "string"))

      (is-true (gtk:tree-model-value model iter 0))
      (is (= 199 (gtk:tree-model-value model iter 1)))
      (is (string= "string" (gtk:tree-model-value model iter 2))))))

;;;     gtk_list_store_remove

;;;     gtk_list_store_insert
;;;     gtk_list_store_insert_before
;;;     gtk_list_store_insert_after

(test gtk-list-store-insert
  (let ((gtk-init:*gtk-warn-deprecated* nil))
    (let ((model (gtk:list-store-new "gint"))
          (iter nil))
      (is (typep (setf iter
                       (gtk:list-store-insert model 0))
                 'gtk:tree-iter))
      (is-false (gtk:list-store-set-value model iter 0 0))
      (is (typep (setf iter
                       (gtk:list-store-insert-before model iter))
                 'gtk:tree-iter))
      (is-false (gtk:list-store-set-value model iter 0 1))
      (is (typep (setf iter
                       (gtk:list-store-insert-after model iter))
                 'gtk:tree-iter))
      (is-false (gtk:list-store-set-value model iter 0 2))
      (let ((iter (gtk:tree-model-iter-first model)))
        (is (= 1 (gtk:tree-model-value model iter 0)))
        (setf iter (gtk:tree-model-iter-next model iter))
        (is (= 2 (gtk:tree-model-value model iter 0)))
        (setf iter (gtk:tree-model-iter-next model iter))
        (is (= 0 (gtk:tree-model-value model iter 0)))))))

;;;     gtk_list_store_insert_with_values

(test gtk-list-store-insert-with-values
  (let ((gtk-init:*gtk-warn-deprecated* nil))
    (let* ((model (gtk:list-store-new "gboolean" "gint" "gchararray"))
           (iter nil))
      (is (typep (setf iter
                       (gtk:list-store-insert-with-values model
                                                         -1
                                                         t
                                                         199
                                                         "string"))
                 'gtk:tree-iter))
      (is-true (gtk:tree-model-value model iter 0))
      (is (= 199 (gtk:tree-model-value model iter 1)))
      (is (string= "string" (gtk:tree-model-value model iter 2))))))

;;;     gtk_list_store_prepend
;;;     gtk_list_store_append
;;;     gtk_list_store_clear
;;;     gtk_list_store_iter_is_valid
;;;     gtk_list_store_reorder
;;;     gtk_list_store_swap
;;;     gtk_list_store_move_before
;;;     gtk_list_store_move_after

;;; 2024-9-20
