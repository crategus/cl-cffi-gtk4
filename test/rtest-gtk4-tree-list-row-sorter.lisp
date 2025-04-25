(in-package :gtk-test)

(def-suite gtk-tree-list-row-sorter :in gtk-tree-support)
(in-suite gtk-tree-list-row-sorter)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkTreeListRowSorter

(test gtk-tree-list-row-sorter-class
  ;; Check type
  (is (g:type-is-object "GtkTreeListRowSorter"))
  ;; Check registered name
  (is (eq 'gtk:tree-list-row-sorter
          (glib:symbol-for-gtype "GtkTreeListRowSorter")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkTreeListRowSorter")
          (g:gtype (cffi:foreign-funcall "gtk_tree_list_row_sorter_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkSorter")
          (g:type-parent "GtkTreeListRowSorter")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkTreeListRowSorter")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkTreeListRowSorter")))
  ;; Check properties
  (is (equal '("sorter")
             (glib-test:list-properties "GtkTreeListRowSorter")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkTreeListRowSorter")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkTreeListRowSorter"
                                      GTK:TREE-LIST-ROW-SORTER
                      (:SUPERCLASS GTK:SORTER :EXPORT T :INTERFACES NIL
                       :TYPE-INITIALIZER "gtk_tree_list_row_sorter_get_type")
                      ((SORTER TREE-LIST-ROW-SORTER-SORTER "sorter"
                        "GtkSorter" T T)))
             (gobject:get-gtype-definition "GtkTreeListRowSorter"))))

;;; --- Properties -------------------------------------------------------------

;;;     sorter

(test gtk-tree-list-row-sorter-properties
  (glib-test:with-check-memory (sorter)
    (is (typep (setf sorter (make-instance 'gtk:tree-list-row-sorter))
               'gtk:tree-list-row-sorter))
    (is-false (gtk:tree-list-row-sorter-sorter sorter))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_tree_list_row_sorter_new

(test gtk-tree-list-row-sorter-new
  (glib-test:with-check-memory (sorter sorter1)
    (let* ((expr (gtk:constant-expression-new "gchararray" "test")))
      (is (typep (setf sorter1
                       (gtk:string-sorter-new expr)) 'gtk:string-sorter))
      (is (typep (setf sorter
                       (gtk:tree-list-row-sorter-new sorter1))
                 'gtk:tree-list-row-sorter))
      (is-false (setf (gtk:tree-list-row-sorter-sorter sorter) nil)))))

;;; 2025-4-16
