(in-package :gtk-test)

(def-suite gtk-section-model :in gtk-list-model-support)
(in-suite gtk-section-model)

;;; --- Types and Valus --------------------------------------------------------

;;;     GtkSectionModel

(test gtk-section-model-interface
  ;; Check type
  (is (g:type-is-interface "GtkSectionModel"))
  ;; Check registered name
  (is (eq 'gtk:section-model
          (glib:symbol-for-gtype "GtkSectionModel")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkSectionModel")
          (g:gtype (cffi:foreign-funcall "gtk_section_model_get_type" :size))))
  ;; Check interface prerequisites
  (is (equal '("GListModel" "GObject")
             (glib-test:list-interface-prerequisites "GtkSectionModel")))
  ;; Check interface properties
  (is (equal '()
             (glib-test:list-interface-properties "GtkSectionModel")))
  ;; Check interface signals
  (is (equal '("sections-changed")
             (glib-test:list-signals "GtkSectionModel")))
  ;; Get interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "GtkSectionModel" GTK:SECTION-MODEL
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_section_model_get_type"))
             (gobject:get-gtype-definition "GtkSectionModel"))))

;;; --- Signals ----------------------------------------------------------------

;;;     sections-changed

(test gtk-section-model-sections-changed-signal
  (let* ((name "sections-changed")
         (gtype (g:gtype "GtkSectionModel"))
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

;;;     gtk_section_model_get_section

(test gtk-section-model-section
  (glib-test:with-check-memory (model selection)
    (setf model (create-string-list-for-package))
    (setf selection (gtk:single-selection-new model))
    (is (equal (list 0 gtk:+invalid-list-position+)
               (multiple-value-list (gtk:section-model-section selection 100))))
    ;; Remove references
    (is-false (setf (gtk:single-selection-model selection) nil))))

;;;     gtk_section_model_sections_changed

(test gtk-section-model-sections-changed
  (glib-test:with-check-memory (model selection)
    (setf model (create-string-list-for-package))
    (setf selection (gtk:single-selection-new model))
    ;; Connect signal handler
    (g:signal-connect selection "sections-changed"
            (lambda (model pos n-items)
              (is (typep model 'gtk:section-model))
              (is (= 100 pos))
              (is (= 10 n-items))))
    ;; Emit signal
    (is-false (g:signal-emit selection "sections-changed" 100 10))
    ;; Remove references
    (is-false (setf (gtk:single-selection-model selection) nil))))

;;; 2024-12-17
