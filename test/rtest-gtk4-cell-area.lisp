(in-package :gtk-test)

(def-suite gtk-cell-area :in gtk-suite)
(in-suite gtk-cell-area)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCellArea

(test gtk-cell-area-class
  ;; Type check
  (is (g:type-is-object "GtkCellArea"))
  ;; Check the registered name
  (is (eq 'gtk:cell-area
          (gobject:symbol-for-gtype "GtkCellArea")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkCellArea")
          (g:gtype (cffi:foreign-funcall "gtk_cell_area_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GInitiallyUnowned")
          (g:type-parent "GtkCellArea")))
  ;; Check the children
  (is (equal '("GtkCellAreaBox")
             (list-children "GtkCellArea")))
  ;; Check the interfaces
  (is (equal '("GtkCellLayout" "GtkBuildable")
             (list-interfaces "GtkCellArea")))
  ;; Check the class properties
  (is (equal '("edit-widget" "edited-cell" "focus-cell")
             (list-properties "GtkCellArea")))
  ;; Check the list of signals
  (is (equal '("add-editable" "apply-attributes" "focus-changed"
               "remove-editable")
             (list-signals "GtkCellArea")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkCellArea" GTK-CELL-AREA
                       (:SUPERCLASS G-INITIALLY-UNOWNED :EXPORT T :INTERFACES
                        ("GtkBuildable" "GtkCellLayout") :TYPE-INITIALIZER
                        "gtk_cell_area_get_type")
                       ((EDIT-WIDGET GTK-CELL-AREA-EDIT-WIDGET "edit-widget"
                         "GtkCellEditable" T NIL)
                        (EDITED-CELL GTK-CELL-AREA-EDITED-CELL "edited-cell"
                         "GtkCellRenderer" T NIL)
                        (FOCUS-CELL GTK-CELL-AREA-FOCUS-CELL "focus-cell"
                         "GtkCellRenderer" T T)))
             (get-g-type-definition "GtkCellArea"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-cell-area-properties
  (let ((area (make-instance 'gtk-cell-area-box)))
    (is-false (gtk-cell-area-edit-widget area))
    (signals (error)
             (setf (gtk-cell-area-edit-widget area) (make-instance 'gtk-entry)))
    (is-false (gtk-cell-area-edited-cell area))
    (signals (error)
             (setf (gtk-cell-area-edited-cell area)
                   (make-instance 'gtk-cell-renderer-text)))
    (is-false (gtk-cell-area-focus-cell area))
    (is (typep (setf (gtk-cell-area-focus-cell area)
                     (make-instance 'gtk-cell-renderer-text))
               'gtk-cell-renderer-text))
    (is (typep (gtk-cell-area-focus-cell area) 'gtk-cell-renderer-text))))

;;; --- Signals ----------------------------------------------------------------

;;;     add-editable

(test gtk-cell-area-add-editable-signal
  (let ((query (g-signal-query (g-signal-lookup "add-editable" "GtkCellArea"))))
    (is (string= "add-editable" (g-signal-query-signal-name query)))
    (is (string= "GtkCellArea" (g-type-name (g-signal-query-owner-type query))))
    (is (equal '(:RUN-FIRST)
               (sort (g-signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g-type-name (g-signal-query-return-type query))))
    (is (equal '("GtkCellRenderer" "GtkCellEditable" "GdkRectangle" "gchararray")
               (mapcar #'g-type-name (g-signal-query-param-types query))))
    (is-false (g-signal-query-signal-detail query))))

;;;     apply-attributes

(test gtk-cell-area-signal-apply-attributes-signal
  (let ((query (g-signal-query (g-signal-lookup "apply-attributes"
                                                "GtkCellArea"))))
    (is (string= "apply-attributes" (g-signal-query-signal-name query)))
    (is (string= "GtkCellArea" (g-type-name (g-signal-query-owner-type query))))
    (is (equal '(:RUN-FIRST)
               (sort (g-signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g-type-name (g-signal-query-return-type query))))
    (is (equal '("GtkTreeModel" "GtkTreeIter" "gboolean" "gboolean")
               (mapcar #'g-type-name (g-signal-query-param-types query))))
    (is-false (g-signal-query-signal-detail query))))

;;;     focus-changed

(test gtk-cell-area-signal-apply-attributes-signal
  (let ((query (g-signal-query (g-signal-lookup "focus-changed" "GtkCellArea"))))
    (is (string= "focus-changed" (g-signal-query-signal-name query)))
    (is (string= "GtkCellArea" (g-type-name (g-signal-query-owner-type query))))
    (is (equal '(:RUN-FIRST)
               (sort (g-signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g-type-name (g-signal-query-return-type query))))
    (is (equal '("GtkCellRenderer" "gchararray")
               (mapcar #'g-type-name (g-signal-query-param-types query))))
    (is-false (g-signal-query-signal-detail query))))

;;;     remove-editable

(test gtk-cell-area-signal-apply-attributes-signal
  (let ((query (g-signal-query (g-signal-lookup "remove-editable" "GtkCellArea"))))
    (is (string= "remove-editable" (g-signal-query-signal-name query)))
    (is (string= "GtkCellArea" (g-type-name (g-signal-query-owner-type query))))
    (is (equal '(:RUN-FIRST)
               (sort (g-signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g-type-name (g-signal-query-return-type query))))
    (is (equal '("GtkCellRenderer" "GtkCellEditable")
               (mapcar #'g-type-name (g-signal-query-param-types query))))
    (is-false (g-signal-query-signal-detail query))))

;;; --- Functions --------------------------------------------------------------

;;;     GtkCellCallback
;;;     GtkCellAllocCallback

;;;     GTK_CELL_AREA_WARN_INVALID_CELL_PROPERTY_ID

;;;     gtk_cell_area_add
;;;     gtk_cell_area_remove
;;;     gtk_cell_area_has_renderer
;;;     gtk_cell_area_foreach
;;;     gtk_cell_area_foreach_alloc
;;;     gtk_cell_area_event
;;;     gtk_cell_area_snapshot
;;;     gtk_cell_area_get_cell_allocation
;;;     gtk_cell_area_get_cell_at_position
;;;     gtk_cell_area_create_context
;;;     gtk_cell_area_copy_context
;;;     gtk_cell_area_get_request_mode
;;;     gtk_cell_area_get_preferred_width
;;;     gtk_cell_area_get_preferred_height_for_width
;;;     gtk_cell_area_get_preferred_height
;;;     gtk_cell_area_get_preferred_width_for_height
;;;     gtk_cell_area_get_current_path_string
;;;     gtk_cell_area_apply_attributes
;;;     gtk_cell_area_attribute_connect
;;;     gtk_cell_area_attribute_disconnect
;;;     gtk_cell_area_attribute_get_column
;;;     gtk_cell_area_class_install_cell_property
;;;     gtk_cell_area_class_find_cell_property
;;;     gtk_cell_area_class_list_cell_properties
;;;     gtk_cell_area_add_with_properties
;;;     gtk_cell_area_cell_set
;;;     gtk_cell_area_cell_get
;;;     gtk_cell_area_cell_set_valist
;;;     gtk_cell_area_cell_get_valist
;;;     gtk_cell_area_cell_set_property
;;;     gtk_cell_area_cell_get_property
;;;     gtk_cell_area_is_activatable
;;;     gtk_cell_area_activate
;;;     gtk_cell_area_focus
;;;     gtk_cell_area_add_focus_sibling
;;;     gtk_cell_area_remove_focus_sibling
;;;     gtk_cell_area_is_focus_sibling
;;;     gtk_cell_area_get_focus_siblings
;;;     gtk_cell_area_get_focus_from_sibling
;;;     gtk_cell_area_activate_cell
;;;     gtk_cell_area_stop_editing
;;;     gtk_cell_area_inner_cell_area
;;;     gtk_cell_area_request_renderer

;;; --- 2023-3-18 --------------------------------------------------------------
