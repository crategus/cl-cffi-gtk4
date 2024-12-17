(in-package :gtk-test)

(def-suite gtk-sort-list-model :in gtk-list-model-support)
(in-suite gtk-sort-list-model)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkSortListModel

(test gtk-sort-list-model-class
  ;; Check type
  (is (g:type-is-object "GtkSortListModel"))
  ;; Check registered name
  (is (eq 'gtk:sort-list-model
          (glib:symbol-for-gtype "GtkSortListModel")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkSortListModel")
          (g:gtype (cffi:foreign-funcall "gtk_sort_list_model_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkSortListModel")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkSortListModel")))
  ;; Check interfaces
  (is (equal '("GListModel" "GtkSectionModel")
             (glib-test:list-interfaces "GtkSortListModel")))
  ;; Check properties
  (is (equal '("incremental" "item-type" "model" "n-items" "pending"
               "section-sorter" "sorter")
             (glib-test:list-properties "GtkSortListModel")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkSortListModel")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkSortListModel" GTK:SORT-LIST-MODEL
                      (:SUPERCLASS G:OBJECT
                       :EXPORT T
                       :INTERFACES ("GListModel" "GtkSectionModel")
                       :TYPE-INITIALIZER "gtk_sort_list_model_get_type")
                      ((INCREMENTAL SORT-LIST-MODEL-INCREMENTAL
                        "incremental" "gboolean" T T)
                       (ITEM-TYPE SORT-LIST-MODEL-ITEM-TYPE
                        "item-type" "GType" T NIL)
                       (MODEL SORT-LIST-MODEL-MODEL "model" "GListModel" T T)
                       (N-ITEMS SORT-LIST-MODEL-N-ITEMS "n-items" "guint" T NIL)
                       (PENDING SORT-LIST-MODEL-PENDING "pending" "guint" T NIL)
                       (SECTION-SORTER SORT-LIST-MODEL-SECTION-SORTER
                        "section-sorter" "GtkSorter" T T)
                       (SORTER SORT-LIST-MODEL-SORTER "sorter" "GtkSorter" T T)))
             (gobject:get-gtype-definition "GtkSortListModel"))))

;;; --- Properties -------------------------------------------------------------

;;;     incremental
;;;     item-type                                          Since 4.8
;;;     model
;;;     n-items                                            Since 4.8
;;;     pending
;;;     section-sorter                                     Since 4.12
;;;     sorter

(test gtk-sort-list-model-properties
  (glib-test:with-check-memory (model)
    (setf model (make-instance 'gtk:sort-list-model))
    (is-false (gtk:sort-list-model-incremental model))
    (is (eq (g:gtype "GObject") (gtk:sort-list-model-item-type model)))
    (is-false (gtk:sort-list-model-model model))
    (is (= 0 (gtk:sort-list-model-n-items model)))
    (is (= 0 (gtk:sort-list-model-pending model)))
    (is-false (gtk:sort-list-model-section-sorter model))
    (is-false (gtk:sort-list-model-sorter model))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_sort_list_model_new

(test gtk-sort-list-model-new
  (glib-test:with-check-memory ((store sorter model))
    (setf store (create-string-list-for-package))
    (setf sorter (gtk:custom-sorter-new
                        (lambda (item1 item2)
                          (let ((str1 (gtk:string-object-string item1))
                                (str2 (gtk:string-object-string item2)))
                            (cond ((string< str1 str2) -1)
                                  ((string> str1 str2) +1)
                                  (t 0))))))
    (is (typep (setf model
                     (gtk:sort-list-model-new store sorter))
               'gtk:sort-list-model))
    ;; Remove references
    (is-false (gtk:string-list-splice store
                                      0
                                      (gtk:string-list-n-items store)
                                      '()))
    (is-false (setf (gtk:sort-list-model-sorter model) nil))
    (is-false (setf (gtk:sort-list-model-model model) nil))))

;;; 2024-12-17
