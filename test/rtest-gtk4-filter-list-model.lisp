(in-package :gtk-test)

(def-suite gtk-filter-list-model :in gtk-list-model-support)
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
          (g:gtype (cffi:foreign-funcall "gtk_filter_list_model_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkFilterListModel")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkFilterListModel")))
  ;; Check interfaces
  (is (equal '("GListModel" "GtkSectionModel")
             (glib-test:list-interfaces "GtkFilterListModel")))
  ;; Check properties
  (is (equal '("filter" "incremental" "item-type" "model" "n-items" "pending"
               "watch-items")
             (glib-test:list-properties "GtkFilterListModel")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkFilterListModel")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkFilterListModel" GTK:FILTER-LIST-MODEL
                      (:SUPERCLASS GOBJECT:OBJECT
                       :EXPORT T
                       :INTERFACES ("GListModel" "GtkSectionModel")
                       :TYPE-INITIALIZER "gtk_filter_list_model_get_type")
                      ((FILTER FILTER-LIST-MODEL-FILTER "filter" "GtkFilter" T T)
                       (INCREMENTAL FILTER-LIST-MODEL-INCREMENTAL
                        "incremental" "gboolean" T T)
                       (ITEM-TYPE FILTER-LIST-MODEL-ITEM-TYPE
                        "item-type" "GType" T NIL)
                       (MODEL FILTER-LIST-MODEL-MODEL "model" "GListModel" T T)
                       (N-ITEMS FILTER-LIST-MODEL-N-ITEMS "n-items" "guint" T NIL)
                       (PENDING FILTER-LIST-MODEL-PENDING
                        "pending" "guint" T NIL)
                       (WATCH-ITEMS FILTER-LIST-MODEL-WATCH-ITEMS
                        "watch-items" "gboolean" T T)))

             (gobject:get-gtype-definition "GtkFilterListModel"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-filter-list-model-properties
  (glib-test:with-check-memory (model)
    (setf model (make-instance 'gtk:filter-list-model))
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
  (glib-test:with-check-memory (store model)
    (setf store (g:list-store-new "GtkWidget"))
    (setf model (gtk:filter-list-model-new store nil))
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
    (is (= 0 (gtk:filter-list-model-n-items model)))
    ;; Remove references
    (is-false (setf (gtk:filter-list-model-model model) nil))))

(test gtk-filter-list-model-new.2
  (glib-test:with-check-memory (store filter model)
    (let ((expression (gtk:property-expression-new "GtkStringObject"
                                                   nil "string")))
      (setf store (gtk:string-list-new '()))
      (setf filter (gtk:string-filter-new expression))
      (setf model (gtk:filter-list-model-new store filter))
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
                             (for object = (g:list-model-item model i))
                             (collect (gtk:string-object-string object)))
                       #'string<)))
      (setf (gtk:string-filter-search filter) "unknown")
      (is (= 0 (gtk:filter-list-model-n-items model)))
      ;; Remove references
      (is-false (gtk:string-list-splice store 0 (g:list-model-n-items store) nil))
      (is-false (setf (gtk:filter-list-model-filter model) nil))
      (is-false (setf (gtk:filter-list-model-model model) nil)))))

;;; 2025-11-02
