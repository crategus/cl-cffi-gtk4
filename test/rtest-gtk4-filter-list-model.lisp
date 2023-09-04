(in-package :gtk-test)

(def-suite gtk-filter-list-model :in gtk-suite)
(in-suite gtk-filter-list-model)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFilterListModel

(test gtk-filter-list-model-class
  ;; Type check
  (is (g:type-is-object "GtkFilterListModel"))
  ;; Check the registered name
  (is (eq 'gtk:filter-list-model
          (glib:symbol-for-gtype "GtkFilterListModel")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkFilterListModel")
          (g:gtype (cffi:foreign-funcall "gtk_filter_list_model_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkFilterListModel")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkFilterListModel")))
  ;; Check the interfaces
  (is (equal '("GListModel")
             (list-interfaces "GtkFilterListModel")))
  ;; Check the properties
  (is (equal '("filter" "incremental" "item-type" "model" "n-items" "pending")
             (list-properties "GtkFilterListModel")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkFilterListModel")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkFilterListModel"
                                             GTK-FILTER-LIST-MODEL
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                                ("GListModel") :TYPE-INITIALIZER
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

;;;     filter
;;;     incremental
;;;     item-type
;;;     model
;;;     n-items
;;;     pending

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

(test gtk-filter-list-model-new
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

;;; --- 2023-8-19 --------------------------------------------------------------
