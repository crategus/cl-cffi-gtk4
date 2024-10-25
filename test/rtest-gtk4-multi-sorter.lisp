(in-package :gtk-test)

(def-suite gtk-multi-sorter :in gtk-suite)
(in-suite gtk-multi-sorter)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkMultiSorter

(test gtk-multi-sorter-class
  ;; Check type
  (is (g:type-is-object "GtkMultiSorter"))
  ;; Check registered name
  (is (eq 'gtk:multi-sorter
          (glib:symbol-for-gtype "GtkMultiSorter")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkMultiSorter")
          (g:gtype (cffi:foreign-funcall "gtk_multi_sorter_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkSorter")
          (g:type-parent "GtkMultiSorter")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkMultiSorter")))
  ;; Check interfaces
  (is (equal '("GListModel" "GtkBuildable")
             (glib-test:list-interfaces "GtkMultiSorter")))
  ;; Check properties
  (is (equal '("item-type" "n-items")
             (glib-test:list-properties "GtkMultiSorter")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkMultiSorter")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkMultiSorter" GTK:MULTI-SORTER
                       (:SUPERCLASS GTK:SORTER
                        :EXPORT T
                        :INTERFACES ("GListModel" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_multi_sorter_get_type")
                       ((ITEM-TYPE MULTI-SORTER-ITEM-TYPE
                         "item-type" "GType" T NIL)
                        (N-ITEMS MULTI-SORTER-N-ITEMS "n-items" "guint" T NIL)))
             (gobject:get-gtype-definition "GtkMultiSorter"))))

;;; --- Properties -------------------------------------------------------------

;;;     item-type                                          Since 4.8
;;;     n-items                                            Since 4.8

(test gtk-multi-sorter-properties
  (let ((sorter (make-instance 'gtk:multi-sorter)))
    (is (eq (g:gtype "GtkSorter") (gtk:multi-sorter-item-type sorter)))
    (is (= 0 (gtk:multi-sorter-n-items sorter)))
    (is (= 1 (g:object-ref-count sorter)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_multi_sorter_new

(test gtk-multi-sorter-new
  (let ((sorter (make-instance 'gtk:multi-sorter)))
    (is (eq (g:gtype "GtkSorter") (gtk:multi-sorter-item-type sorter)))
    (is (= 0 (gtk:multi-sorter-n-items sorter)))
    (is (= 1 (g:object-ref-count sorter)))))

;;;     gtk_multi_sorter_append
;;;     gtk_multi_sorter_remove

(test gtk-multi-sorter-append/remove
  (let* ((sorter (gtk:multi-sorter-new))
         (store (gtk:string-list-new '("ccc" "cc" "c"
                                       "bbb" "bb" "b"
                                       "aaa" "aa" "a")))
         (model (gtk:sort-list-model-new store sorter))
         (sorter1 (gtk:custom-sorter-new
                      (lambda (obj1 obj2)
                        (let ((str1 (gtk:string-object-string obj1))
                              (str2 (gtk:string-object-string obj2)))
                          (cond ((< (length str1) (length str2)) -1)
                                ((> (length str1) (length str2)) +1)
                                (t 0))))))
         (sorter2 (gtk:custom-sorter-new
                      (lambda (obj1 obj2)
                        (let ((str1 (gtk:string-object-string obj1))
                              (str2 (gtk:string-object-string obj2)))
                          (cond ((string< str1 str2) -1)
                                ((string> str1 str2) +1)
                                (t 0)))))))

    (is (= 1 (g:object-ref-count sorter1)))
    (is (= 1 (g:object-ref-count sorter2)))

    (is (string= "ccc"
                 (gtk:string-object-string (g:list-model-object model 0))))
    (is (string= "cc"
                 (gtk:string-object-string (g:list-model-object model 1))))
    (is (string= "c"
                 (gtk:string-object-string (g:list-model-object model 2))))

    (is-false (gtk:multi-sorter-append sorter sorter1))
    (is (= 3 (g:object-ref-count sorter1)))

    (is (string= "c"
                 (gtk:string-object-string (g:list-model-object model 0))))
    (is (string= "b"
                 (gtk:string-object-string (g:list-model-object model 1))))
    (is (string= "a"
                 (gtk:string-object-string (g:list-model-object model 2))))

    (is-false (gtk:multi-sorter-append sorter sorter2))
    (is (= 3 (g:object-ref-count sorter2)))

    (is (string= "a"
                 (gtk:string-object-string (g:list-model-object model 0))))
    (is (string= "b"
                 (gtk:string-object-string (g:list-model-object model 1))))
    (is (string= "c"
                 (gtk:string-object-string (g:list-model-object model 2))))

    (is-false (gtk:multi-sorter-remove sorter 0))
    (is (= 1 (g:object-ref-count sorter1)))
    (is (= 3 (g:object-ref-count sorter2)))

    (is (string= "a"
                 (gtk:string-object-string (g:list-model-object model 0))))
    (is (string= "aa"
                 (gtk:string-object-string (g:list-model-object model 1))))
    (is (string= "aaa"
                 (gtk:string-object-string (g:list-model-object model 2))))

    ;; Check memory management
    (is-false (gtk:multi-sorter-remove sorter 0))
    (is-false (gtk:string-list-splice store
                                      0
                                      (gtk:string-list-n-items store)
                                      '()))
    (is-false (setf (gtk:sort-list-model-sorter model) nil))
    (is-false (setf (gtk:sort-list-model-model model) nil))

    (is (= 1 (g:object-ref-count sorter)))
    (is (= 1 (g:object-ref-count store)))
    (is (= 1 (g:object-ref-count model)))
    (is (= 1 (g:object-ref-count sorter1)))
    (is (= 1 (g:object-ref-count sorter2)))))

;;; 2024-10-24
