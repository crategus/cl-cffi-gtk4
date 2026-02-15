(in-package :gtk-test)

(def-suite gtk-selection-model :in gtk-list-model-support)
(in-suite gtk-selection-model)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkSelectionModel

(test gtk-selection-model-interface
  ;; Check type
  (is (g:type-is-interface "GtkSelectionModel"))
  ;; Check registered name
  (is (eq 'gtk:selection-model
          (glib:symbol-for-gtype "GtkSelectionModel")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkSelectionModel")
          (g:gtype (cffi:foreign-funcall "gtk_selection_model_get_type" :size))))
  ;; Check interface prerequisites
  (is (equal '("GListModel" "GObject")
             (glib-test:list-interface-prerequisites "GtkSelectionModel")))
  ;; Check interface properties
  (is (equal '()
             (glib-test:list-interface-properties "GtkSelectionModel")))
  ;; Check signals
  (is (equal '("selection-changed")
             (glib-test:list-signals "GtkSelectionModel")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "GtkSelectionModel" GTK:SELECTION-MODEL
                      (:EXPORT T
                       :TYPE-INITIALIZER "gtk_selection_model_get_type"))
             (gobject:get-gtype-definition "GtkSelectionModel"))))

;;; --- Signals ----------------------------------------------------------------

;;;     selection-changed

(test gtk-selection-model-selection-changed-signal
  (let* ((name "selection-changed")
         (gtype (g:gtype "GtkSelectionModel"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("guint" "guint")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_selection_model_is_selected
;;;     gtk_selection_model_select_item
;;;     gtk_selection_model_unselect_item

(test gtk-selection-model-is-selected
  (glib-test:with-check-memory (model selection)
    (setf model (create-string-list-for-package "GSK"))
    (setf selection (gtk:multi-selection-new model))
    (is (eq (g:gtype "GObject") (gtk:multi-selection-item-type selection)))
    (is (<= 280 (gtk:multi-selection-n-items selection)))
    (is-true (gtk:selection-model-select-item selection 50 t))
    (is-true (gtk:selection-model-is-selected selection 50))
    (is-true (gtk:selection-model-select-item selection 60 nil))
    (is-true (gtk:selection-model-is-selected selection 50))
    (is-true (gtk:selection-model-is-selected selection 60))
    (is-true (gtk:selection-model-unselect-item selection 50))
    (is-false (gtk:selection-model-is-selected selection 50))
    (is-true (gtk:selection-model-is-selected selection 60))
    ;; Remove references
    (is-false (setf (gtk:multi-selection-model selection) nil))))

;;;     gtk_selection_model_select_range
;;;     gtk_selection_model_unselect_range
;;;     gtk_selection_model_get_selection
;;;     gtk_selection_model_set_selection

(test gtk-selection-model-select-range
  (glib-test:with-check-memory (model selection)
    (let (bitset)
      (setf model (create-string-list-for-package "GSK"))
      (setf selection (gtk:multi-selection-new model))
      (is-true (gtk:selection-model-select-range selection 50 10 t))
      (is (typep (setf bitset (gtk:selection-model-selection selection))
                 'gtk:bitset))
      (is (= 10 (gtk:bitset-size bitset)))
      (is (= 50 (gtk:bitset-minimum bitset)))
      (is (= 59 (gtk:bitset-maximum bitset)))
      (is-true (gtk:selection-model-unselect-range selection 50 5))
      (is (typep (setf bitset (gtk:selection-model-selection selection))
                 'gtk:bitset))
      (is (= 5 (gtk:bitset-size bitset)))
      (is (= 55 (gtk:bitset-minimum bitset)))
      (is (= 59 (gtk:bitset-maximum bitset)))
      ;; Remove references
    (is-false (setf (gtk:multi-selection-model selection) nil)))))

;;;     gtk_selection_model_select_all
;;;     gtk_selection_model_unselect_all
;;;     gtk_selection_model_get_selection_in_range

;; TODO: GTK:SELECTION-MODEL-SELECTION-IN-RANGE does not work as expected.

(test gtk-selection-model-select-all
  (glib-test:with-check-memory (model selection)
    (let (bitset)
      (setf model (create-string-list-for-package "GSK"))
      (setf selection (gtk:multi-selection-new model))
      (is-true (gtk:selection-model-select-all selection))
      (is (typep (setf bitset
                       (gtk:selection-model-selection-in-range selection 50 10))
                 'gtk:bitset))
      (is (<= 299 (gtk:bitset-size bitset)))
      (is (=    0 (gtk:bitset-minimum bitset)))
      (is (<= 298 (gtk:bitset-maximum bitset)))
      (is-true (gtk:selection-model-unselect-all selection))
      (is (typep (setf bitset
                       (gtk:selection-model-selection-in-range selection 50 10))
                 'gtk:bitset))
      (is (= 0 (gtk:bitset-size bitset)))
      (is (= gtk:+invalid-list-position+ (gtk:bitset-minimum bitset)))
      (is (= 0 (gtk:bitset-maximum bitset)))
      ;; Remove references
      (is-false (setf (gtk:multi-selection-model selection) nil)))))

;;;     gtk_selection_model_selection_changed

(test gtk-selection-model-selection-changed
  (glib-test:with-check-memory (model selection)
    (setf model (create-string-list-for-package "GSK"))
    (setf selection (gtk:multi-selection-new model))
    (g:signal-connect selection "selection-changed"
        (lambda (model pos n-items)
          (is (eq selection model))
          (is (= 50 pos))
          (is (= 10 n-items))))
    (is-false (gtk:selection-model-selection-changed selection 50 10))
    ;; Remove references
    (is-false (setf (gtk:multi-selection-model selection) nil))))

;;; 2026-02-15
