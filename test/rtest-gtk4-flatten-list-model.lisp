(in-package :gtk-test)

(def-suite gtk-flatten-list-model :in gtk-list-model-support)
(in-suite gtk-flatten-list-model)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFlattenListModel

(test gtk-flatten-list-model-class
  ;; Check type
  (is (g:type-is-object "GtkFlattenListModel"))
  ;; Check registered name
  (is (eq 'gtk:flatten-list-model
          (glib:symbol-for-gtype "GtkFlattenListModel")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkFlattenListModel")
          (g:gtype (cffi:foreign-funcall "gtk_flatten_list_model_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkFlattenListModel")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkFlattenListModel")))
  ;; Check interfaces
  (is (equal '("GListModel" "GtkSectionModel")
             (glib-test:list-interfaces "GtkFlattenListModel")))
  ;; Check properties
  (is (equal '("item-type" "model" "n-items")
             (glib-test:list-properties "GtkFlattenListModel")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkFlattenListModel")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkFlattenListModel" GTK:FLATTEN-LIST-MODEL
                      (:SUPERCLASS GOBJECT:OBJECT
                       :EXPORT T
                       :INTERFACES ("GListModel" "GtkSectionModel")
                       :TYPE-INITIALIZER "gtk_flatten_list_model_get_type")
                      ((ITEM-TYPE FLATTEN-LIST-MODEL-ITEM-TYPE
                        "item-type" "GType" T NIL)
                       (MODEL FLATTEN-LIST-MODEL-MODEL "model" "GListModel" T T)
                       (N-ITEMS FLATTEN-LIST-MODEL-N-ITEMS
                        "n-items" "guint" T NIL)))
             (gobject:get-gtype-definition "GtkFlattenListModel"))))

;;; --- Properties -------------------------------------------------------------

;;;     item-type                                          Since 4.8
;;;     model
;;;     n-items                                            Since 4.8

(test gtk-flatten-list-model-properties
  (let ((flatten (make-instance 'gtk:flatten-list-model)))
    (is (eq (g:gtype "GObject") (gtk:flatten-list-model-item-type flatten)))
    (is-false (gtk:flatten-list-model-model flatten))
    (is (= 0 (gtk:flatten-list-model-n-items flatten)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_flatten_list_model_new
;;;     gtk_flatten_list_model_get_model_for_item

(test gtk-flatten-list-model-new
  (let* ((minutes '( "1 minute"   "2 minutes"  "5 minutes" "10 minutes"
                    "15 minutes" "20 minutes" "25 minutes" "30 minutes"
                    "35 minutes" "40 minutes" "45 minutes" "50 minutes"
                    "55 minutes"))
         (hours '( "1 hour"  "2 hours" "3 hours"  "5 hours"  "6 hours"
                   "7 hours" "8 hours" "9 hours" "10 hours" "11 hours"
                  "12 hours"))
         (minutes-model (gtk:string-list-new minutes))
         (hours-model (gtk:string-list-new hours))
         (model (g:list-store-new "GListModel"))
         (flatten (gtk:flatten-list-model-new model)))
    (g:list-store-append model minutes-model)
    (g:list-store-append model hours-model)
    ;; Some general checks
    (is (eq model (gtk:flatten-list-model-model flatten)))
    (is (eq (g:gtype "GObject")
            (gtk:flatten-list-model-item-type flatten)))
    (is (= 2 (g:list-store-n-items model)))
    (is (= 13 (gtk:string-list-n-items minutes-model)))
    (is (= 11 (gtk:string-list-n-items hours-model)))
    (is (= 24 (gtk:flatten-list-model-n-items flatten)))
    ;; First 13 items are stored in MINUTES-MODEL
    (is (eq minutes-model (gtk:flatten-list-model-model-for-item flatten 0)))
    (is (eq minutes-model (gtk:flatten-list-model-model-for-item flatten 1)))
    (is (eq minutes-model (gtk:flatten-list-model-model-for-item flatten 12)))
    ;; Second 11 items are stored in HOURS-MODEL
    (is (eq hours-model (gtk:flatten-list-model-model-for-item flatten 13)))
    (is (eq hours-model (gtk:flatten-list-model-model-for-item flatten 14)))
    ;; Check memory management
    (is-false (setf (gtk:flatten-list-model-model flatten) nil))
    (is-false (g:list-store-remove-all model))
    (is (= 1 (g:object-ref-count minutes-model)))
    (is (= 1 (g:object-ref-count hours-model)))
    (is (= 1 (g:object-ref-count model)))
    (is (= 1 (g:object-ref-count flatten)))))

;;; 2024-12-9
