(in-package :gtk-test)

(def-suite gtk-section-model :in gtk-suite)
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
  (is (equal '()
             (glib-test:list-interface-prerequisites "GtkSectionModel")))
  ;; Check interface properties
  (is (equal '()
             (glib-test:list-interface-properties "GtkSectionModel")))
  ;; Check interface signals
  (is (equal '()
             (glib-test:list-signals "GtkSectionModel")))
  ;; Get interface definition
  (is (equal '()
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
    (is (equal '(:ACTION :DETAILED :NO-HOOKS :NO-RECURSE :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("GParam")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_section_model_get_section
;;;     gtk_section_model_sections_changed

;;; 2024-11-14
