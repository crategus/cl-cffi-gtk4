(in-package :gtk-test)

(def-suite gtk-cell-renderer-toggle :in gtk-suite)
(in-suite gtk-cell-renderer-toggle)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCellRendererToggle

(test gtk-cell-renderer-toggle-class
  ;; Check type
  (is (g:type-is-object "GtkCellRendererToggle"))
  ;; Check registered name
  (is (eq 'gtk:cell-renderer-toggle
          (glib:symbol-for-gtype "GtkCellRendererToggle")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkCellRendererToggle")
          (g:gtype (cffi:foreign-funcall "gtk_cell_renderer_toggle_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkCellRenderer")
          (g:type-parent "GtkCellRendererToggle")))
  ;; Check children
  (is (equal '()
             (list-children "GtkCellRendererToggle")))
  ;; Check interfaces
  (is (equal '()
             (list-interfaces "GtkCellRendererToggle")))
  ;; Check properties
  (is (equal '("activatable" "active" "inconsistent" "radio")
             (list-properties "GtkCellRendererToggle")))
  ;; Check signals
  (is (equal '("toggled")
             (list-signals "GtkCellRendererToggle")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkCellRendererToggle"
                               GTK-CELL-RENDERER-TOGGLE
                               (:SUPERCLASS GTK-CELL-RENDERER :EXPORT T
                                :INTERFACES NIL :TYPE-INITIALIZER
                                "gtk_cell_renderer_toggle_get_type")
                               ((ACTIVATABLE
                                 GTK-CELL-RENDERER-TOGGLE-ACTIVATABLE
                                 "activatable" "gboolean" T T)
                                (ACTIVE GTK-CELL-RENDERER-TOGGLE-ACTIVE
                                 "active" "gboolean" T T)
                                (INCONSISTENT
                                 GTK-CELL-RENDERER-TOGGLE-INCONSISTENT
                                 "inconsistent" "gboolean" T T)
                                (RADIO GTK-CELL-RENDERER-TOGGLE-RADIO "radio"
                                 "gboolean" T T)))
             (gobject:get-g-type-definition "GtkCellRendererToggle"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-cell-renderer-toggle-properties
  (let* ((gtk-init:*gtk-warn-deprecated* nil)
         (renderer (make-instance 'gtk:cell-renderer-toggle)))
    (is-true (gtk:cell-renderer-toggle-activatable renderer))
    (is-false (gtk:cell-renderer-toggle-active renderer))
    (is-false (gtk:cell-renderer-toggle-inconsistent renderer))
    (is-false (gtk:cell-renderer-toggle-radio renderer))))

;;; ---- Signals ---------------------------------------------------------------

(test gtk-cell-renderer-toggle-toggled-signal
  (let* ((name "toggled") (gtype "GtkCellRendererToggle")
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
    (is (equal '("gchararray")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_cell_renderer_toggle_new

(test gtk-cell-renderer-toggle-new
  (let* ((gtk-init:*gtk-warn-deprecated* nil))
    (is (typep (gtk:cell-renderer-toggle-new) 'gtk:cell-renderer-toggle))))

;;; 2024-5-18
