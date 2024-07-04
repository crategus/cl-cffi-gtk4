(in-package :gtk-test)

(def-suite gtk-filter-list-model :in gtk-suite)
(in-suite gtk-filter-list-model)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFilterListModel

(test gtk-filter-list-model-class
  ;; Check type
  (is (g:type-is-object "GtkFilterListModel"))
  ;; Check registered name
  (is (eq 'gtk:filter-list-model
          (glib:symbol-for-gtype "GtkFilterListModel")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkFilterListModel")
          (g:gtype (cffi:foreign-funcall "gtk_filter_list_model_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkFilterListModel")))
  ;; Check children
  (is (equal '()
             (gtk-test:list-children "GtkFilterListModel")))
  ;; Check interfaces
  (is (equal '("GListModel" "GtkSectionModel")
             (gtk-test:list-interfaces "GtkFilterListModel")))
  ;; Check properties
  (is (equal '("filter" "incremental" "item-type" "model" "n-items" "pending")
             (gtk-test:list-properties "GtkFilterListModel")))
  ;; Check signals
  (is (equal '()
             (gtk-test:list-signals "GtkFilterListModel")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkFilterListModel"
                                             GTK-FILTER-LIST-MODEL
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                                ("GListModel" "GtkSectionModel")
                                :TYPE-INITIALIZER
                                "gtk_filter_list_model_get_type")
                               ((FILTER GTK-FILTER-LIST-MODEL-FILTER "filter"
                                 "GtkFilter" T T)
                                (INCREMENTAL GTK-FILTER-LIST-MODEL-INCREMENTAL
                                 "incremental" "gboolean" T T)
                                (ITEM-TYPE GTK-FILTER-LIST-MODEL-ITEM-TYPE
                                 "item-type" "GType" T NIL)
                                (MODEL GTK-FILTER-LIST-MODEL-MODEL "model"
                                 "GListModel" T T)
                                (N-ITEMS GTK-FILTER-LIST-MODEL-N-ITEMS
                                 "n-items" "guint" T NIL)
                                (PENDING GTK-FILTER-LIST-MODEL-PENDING
                                 "pending" "guint" T NIL)))
             (gobject:get-g-type-definition "GtkFilterListModel"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-filter-list-model-properties
  (let ((model (make-instance 'gtk:filter-list-model)))
    (is-false (gtk:filter-list-model-filter model))
    (is-false (gtk:filter-list-model-incremental model))
    ;; The default is "GObject"!?
    (is (eq (g:gtype "GObject") (gtk:filter-list-model-item-type model)))
    (is-false (gtk:filter-list-model-model model))
    (is (= 0 (gtk:filter-list-model-n-items model)))
    (is (= 0 (gtk:filter-list-model-pending model)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_filter_list_model_new

(test gtk-filter-list-model-new.1
  (let* ((store (g:list-store-new "GtkWidget"))
         (model (gtk:filter-list-model-new store nil)))
    ;; The list store
    (is (eq (g:gtype "GtkWidget") (g:list-store-item-type store)))
    (is (= 0 (g:list-store-n-items store)))
    ;; TODO: The store has the type GtkWidget, but the list model and the
    ;; filter list model return the GObject type. Is something wrong?
    ;; The list model
    (is (eq (g:gtype "GObject") (g:list-model-item-type model)))
    (is (= 0 (g:list-model-n-items model)))
    ;; The filter list model
    (is (eq (g:gtype "GObject") (gtk:filter-list-model-item-type model)))
    (is (= 0 (gtk:filter-list-model-n-items model)))))

(test gtk-filter-list-model-new.2
  (let* ((store (gtk:string-list-new '()))
         (expression (gtk:property-expression-new "GtkStringObject"
                                                  nil "string"))
         (filter (gtk:string-filter-new expression))
         (model (gtk:filter-list-model-new store filter)))
    ;; Fill the string list with strings
    (do-external-symbols (symbol (find-package "GTK"))
      (gtk:string-list-append store (string-downcase (format nil "~a" symbol))))

    (is (eq (g:gtype "GObject") (g:list-model-item-type store)))
    (is (< 3000 (g:list-model-n-items store)))

    (is (eq (g:gtype "GObject") (g:list-model-item-type model)))
    (is (< 3000 (g:list-model-n-items model)))

    (is (eq (g:gtype "GObject") (gtk:filter-list-model-item-type model)))
    (is (< 3000 (gtk:filter-list-model-n-items model)))

    (setf (gtk:string-filter-search filter) "string-filter")

    (is (eq (g:gtype "GObject") (gtk:filter-list-model-item-type model)))
    (is (= 6 (gtk:filter-list-model-n-items model)))
    (is (equal '("string-filter" "string-filter-expression"
                 "string-filter-ignore-case" "string-filter-match-mode"
                 "string-filter-new" "string-filter-search")
               (sort (iter (with n = (gtk:filter-list-model-n-items model))
                           (for i from 0 below n)
                           (for object = (g:list-model-object model i))
                           (collect (gtk:string-object-string object)))
                     #'string<)))))

;;; 2024-4-1
