(in-package :gtk-test)

(def-suite gtk-cell-renderer-combo :in gtk-deprecated)
(in-suite gtk-cell-renderer-combo)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCellRendererCombo

(test gtk-cell-renderer-combo-class
  ;; Check type
  (is (g:type-is-object "GtkCellRendererCombo"))
  ;; Check registered name
  (is (eq 'gtk:cell-renderer-combo
          (glib:symbol-for-gtype "GtkCellRendererCombo")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkCellRendererCombo")
          (g:gtype (cffi:foreign-funcall "gtk_cell_renderer_combo_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkCellRendererText")
          (g:type-parent "GtkCellRendererCombo")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkCellRendererCombo")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkCellRendererCombo")))
  ;; Check properties
  (is (equal '("has-entry" "model" "text-column")
             (glib-test:list-properties "GtkCellRendererCombo")))
  ;; Check signals
  (is (equal '("changed")
             (glib-test:list-signals "GtkCellRendererCombo")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkCellRendererCombo"
                                      GTK:CELL-RENDERER-COMBO
                      (:SUPERCLASS GTK:CELL-RENDERER-TEXT
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gtk_cell_renderer_combo_get_type")
                      ((HAS-ENTRY CELL-RENDERER-COMBO-HAS-ENTRY
                        "has-entry" "gboolean" T T)
                       (MODEL CELL-RENDERER-COMBO-MODEL
                        "model" "GtkTreeModel" T T)
                       (TEXT-COLUMN CELL-RENDERER-COMBO-TEXT-COLUMN
                        "text-column" "gint" T T)))
             (gobject:get-gtype-definition "GtkCellRendererCombo"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-cell-renderer-combo-properties
  (let* ((gtk-init:*warn-deprecated* nil)
         (renderer (make-instance 'gtk:cell-renderer-combo)))
    (is-true (gtk:cell-renderer-combo-has-entry renderer))
    (is-false (gtk:cell-renderer-combo-model renderer))
    (is (= -1 (gtk:cell-renderer-combo-text-column renderer)))))

;;; --- Signals ----------------------------------------------------------------

;;;     changed

(test gtk-cell-renderer-combo-changed-signal
  (let* ((name "changed") (gtype "GtkCellRendererCombo")
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (string= gtype (g:type-name (g:signal-query-owner-type query))))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '("gchararray" "GtkTreeIter")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_cell_renderer_combo_new

(test gtk-cell-renderer-combo-new
  (let* ((gtk-init:*warn-deprecated* nil))
    (is (typep (gtk:cell-renderer-combo-new) 'gtk:cell-renderer-combo))))

;;; 2024-9-20
