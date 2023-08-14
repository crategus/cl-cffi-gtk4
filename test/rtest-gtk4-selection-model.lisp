(in-package :gtk-test)

(def-suite gtk-selection-model :in gtk-suite)
(in-suite gtk-selection-model)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkSelectionModel

(test gtk-selection-model-interface
  ;; Type check
  (is (g:type-is-interface "GtkSelectionModel"))
  ;; Check the registered name
  (is (eq 'gtk:selection-model
          (glib:symbol-for-gtype "GtkSelectionModel")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkSelectionModel")
          (g:gtype (cffi:foreign-funcall "gtk_selection_model_get_type" 
                                         :size))))
  ;; Check the interface prerequisites
  (is (equal '("GListModel" "GObject")
             (list-interface-prerequisites "GtkSelectionModel")))
  ;; Check the interface properties
  (is (equal '()
             (list-interface-properties "GtkSelectionModel")))
  ;; Check the signals
  (is (equal '("selection-changed")
             (list-signals "GtkSelectionModel")))
  ;; Get the interface definition
  (is (equal '(GOBJECT:DEFINE-G-INTERFACE "GtkSelectionModel" 
                                          GTK-SELECTION-MODEL
                            (:EXPORT T :TYPE-INITIALIZER
                             "gtk_selection_model_get_type"))
             (gobject:get-g-type-definition "GtkSelectionModel"))))

;;; --- Signals ----------------------------------------------------------------

;;;     selection-changed

(test gtk-selection-model-signal-selection-changed
  (let ((query (g:signal-query (g:signal-lookup "selection-changed" 
                                                "GtkSelectionModel"))))
    (is (string= "selection-changed" (g:signal-query-signal-name query)))
    (is (string= "GtkSelectionModel" 
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '("guint" "guint")
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_selection_model_is_selected 
;;;     gtk_selection_model_get_selection 
;;;     gtk_selection_model_get_selection_in_range 
;;;     gtk_selection_model_select_item 
;;;     gtk_selection_model_unselect_item 
;;;     gtk_selection_model_select_range 
;;;     gtk_selection_model_unselect_range 
;;;     gtk_selection_model_select_all 
;;;     gtk_selection_model_unselect_all 
;;;     gtk_selection_model_set_selection 
;;;     gtk_selection_model_selection_changed 

;;; --- 2023-8-10 --------------------------------------------------------------
