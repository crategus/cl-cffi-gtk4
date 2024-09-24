(in-package :gtk-test)

(def-suite gtk-cell-area :in gtk-suite)
(in-suite gtk-cell-area)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCellArea

(test gtk-cell-area-class
  ;; Check type
  (is (g:type-is-object "GtkCellArea"))
  ;; Check registered name
  (is (eq 'gtk:cell-area
          (glib:symbol-for-gtype "GtkCellArea")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkCellArea")
          (g:gtype (cffi:foreign-funcall "gtk_cell_area_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GInitiallyUnowned")
          (g:type-parent "GtkCellArea")))
  ;; Check children
  (is (equal '("GtkCellAreaBox")
             (glib-test:list-children "GtkCellArea")))
  ;; Check interfaces
  (is (equal '("GtkCellLayout" "GtkBuildable")
             (glib-test:list-interfaces "GtkCellArea")))
  ;; Check class properties
  (is (equal '("edit-widget" "edited-cell" "focus-cell")
             (glib-test:list-properties "GtkCellArea")))
  ;; Check signals
  (is (equal '("add-editable" "apply-attributes" "focus-changed"
               "remove-editable")
             (glib-test:list-signals "GtkCellArea")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkCellArea" GTK:CELL-AREA
                       (:SUPERCLASS G:INITIALLY-UNOWNED
                        :EXPORT T
                        :INTERFACES ("GtkBuildable" "GtkCellLayout")
                        :TYPE-INITIALIZER "gtk_cell_area_get_type")
                       ((EDIT-WIDGET CELL-AREA-EDIT-WIDGET
                         "edit-widget" "GtkCellEditable" T NIL)
                        (EDITED-CELL CELL-AREA-EDITED-CELL
                         "edited-cell" "GtkCellRenderer" T NIL)
                        (FOCUS-CELL CELL-AREA-FOCUS-CELL
                         "focus-cell" "GtkCellRenderer" T T)))
             (gobject:get-gtype-definition "GtkCellArea"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-cell-area-properties
  (let* ((gtk-init:*gtk-warn-deprecated* nil)
         (area (make-instance 'gtk:cell-area-box)))
    (is-false (gtk:cell-area-edit-widget area))
    (signals (error)
             (setf (gtk:cell-area-edit-widget area) (make-instance 'gtk:entry)))
    (is-false (gtk:cell-area-edited-cell area))
    (signals (error)
             (setf (gtk:cell-area-edited-cell area)
                   (make-instance 'gtk:cell-renderer-text)))
    (is-false (gtk:cell-area-focus-cell area))
    (is (typep (setf (gtk:cell-area-focus-cell area)
                     (make-instance 'gtk:cell-renderer-text))
               'gtk:cell-renderer-text))
    (is (typep (gtk:cell-area-focus-cell area) 'gtk:cell-renderer-text))))

;;; --- Signals ----------------------------------------------------------------

;;;     add-editable

(test gtk-cell-area-add-editable-signal
  (let ((query (g:signal-query (g:signal-lookup "add-editable" "GtkCellArea"))))
    (is (string= "add-editable" (g:signal-query-signal-name query)))
    (is (string= "GtkCellArea" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '("GtkCellRenderer" "GtkCellEditable" "GdkRectangle" "gchararray")
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;;     apply-attributes

(test gtk-cell-area-signal-apply-attributes-signal
  (let ((query (g:signal-query (g:signal-lookup "apply-attributes"
                                                "GtkCellArea"))))
    (is (string= "apply-attributes" (g:signal-query-signal-name query)))
    (is (string= "GtkCellArea" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '("GtkTreeModel" "GtkTreeIter" "gboolean" "gboolean")
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;;     focus-changed

(test gtk-cell-area-signal-apply-attributes-signal
  (let ((query (g:signal-query (g:signal-lookup "focus-changed" "GtkCellArea"))))
    (is (string= "focus-changed" (g:signal-query-signal-name query)))
    (is (string= "GtkCellArea" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '("GtkCellRenderer" "gchararray")
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;;     remove-editable

(test gtk-cell-area-signal-apply-attributes-signal
  (let ((query (g:signal-query (g:signal-lookup "remove-editable" "GtkCellArea"))))
    (is (string= "remove-editable" (g:signal-query-signal-name query)))
    (is (string= "GtkCellArea" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '("GtkCellRenderer" "GtkCellEditable")
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_cell_area_add
;;;     gtk_cell_area_remove
;;;     gtk_cell_area_has_renderer

(test gtk-cell-area-add/remove
  (let ((gtk-init:*gtk-warn-deprecated* nil))
    (let ((area (gtk:cell-area-box-new))
          (renderer1 (gtk:cell-renderer-pixbuf-new))
          (renderer2 (gtk:cell-renderer-progress-new))
          (renderer3 (gtk:cell-renderer-text-new)))
      ;; Add cell renderer
      (is-false (gtk:cell-area-add area renderer1))
      (is-true (gtk:cell-area-has-renderer area renderer1))
      (is-false (gtk:cell-area-add area renderer2))
      (is-true (gtk:cell-area-has-renderer area renderer2))
      (is-false (gtk:cell-area-add area renderer3))
      (is-true (gtk:cell-area-has-renderer area renderer3))
      ;; Remove cell renderer
      (is-false (gtk:cell-area-remove area renderer1))
      (is-false (gtk:cell-area-has-renderer area renderer1))
      (is-false (gtk:cell-area-remove area renderer2))
      (is-false (gtk:cell-area-has-renderer area renderer2))
      (is-false (gtk:cell-area-remove area renderer3))
      (is-false (gtk:cell-area-has-renderer area renderer3)))))

;;;     GtkCellCallback
;;;     gtk_cell_area_foreach

(test gtk-cell-area-foreach
  (let* ((gtk-init:*gtk-warn-deprecated* nil)
         (area (gtk:cell-area-box-new))
         (message nil))
    ;; Add five cell renderer to the area box
    (gtk:cell-area-box-pack-start area (gtk:cell-renderer-pixbuf-new))
    (gtk:cell-area-box-pack-start area (gtk:cell-renderer-progress-new))
    (gtk:cell-area-box-pack-start area (gtk:cell-renderer-spinner-new))
    (gtk:cell-area-box-pack-start area (gtk:cell-renderer-text-new))
    (gtk:cell-area-box-pack-start area (gtk:cell-renderer-toggle-new))
    ;; Collect types of the renderers
    (gtk:cell-area-foreach area
                           (lambda (renderer)
                             (push (type-of renderer) message)
                             nil))
    ;; Check types of the renderers
    (is (equal '(GTK:CELL-RENDERER-PIXBUF GTK:CELL-RENDERER-PROGRESS
                 GTK:CELL-RENDERER-SPINNER GTK:CELL-RENDERER-TEXT
                 GTK:CELL-RENDERER-TOGGLE)
               (reverse message)))))

;;;     GtkCellAllocCallback
;;;     gtk_cell_area_foreach_alloc

;;;     gtk_cell_area_event
;;;     gtk_cell_area_snapshot
;;;     gtk_cell_area_get_cell_allocation
;;;     gtk_cell_area_get_cell_at_position

;;;     gtk_cell_area_create_context

(test gtk-cell-area-create-context
  (let* ((gtk-init:*gtk-warn-deprecated* nil)
         (area (gtk:cell-area-box-new)))
    (is (typep (gtk:cell-area-create-context area) 'gtk:cell-area-context))))

;;;     gtk_cell_area_copy_context

(test gtk-cell-area-copy-context
  (let* ((gtk-init:*gtk-warn-deprecated* nil)
         (area (gtk:cell-area-box-new))
         (context (gtk:cell-area-create-context area)))
    (is (typep (gtk:cell-area-copy-context area context)
               'gtk:cell-area-context))))

;;;     gtk_cell_area_get_request_mode

(test gtk-cell-area-request-mode
  (let* ((gtk-init:*gtk-warn-deprecated* nil)
         (area (gtk:cell-area-box-new)))
    (is (eq :HEIGHT-FOR-WIDTH (gtk:cell-area-request-mode area)))))

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

(test gtk-cell-area-class-find-cell-property
  (let* ((gtk-init:*gtk-warn-deprecated* nil)
         (area (make-instance 'gtk:cell-area-box))
         (renderer (make-instance 'gtk:cell-renderer-pixbuf)))
    (is-false (gtk:cell-area-add area renderer))
    (is (cffi:pointerp (gtk:cell-area-class-find-cell-property "GtkCellAreaBox"
                                                               "align")))
    (is (cffi:pointerp (gtk:cell-area-class-find-cell-property "GtkCellAreaBox"
                                                               "expand")))
    (is (cffi:pointerp (gtk:cell-area-class-find-cell-property "GtkCellAreaBox"
                                                               "fixed-size")))
    (is (cffi:pointerp (gtk:cell-area-class-find-cell-property "GtkCellAreaBox"
                                                               "pack-type")))))

;;;     gtk_cell_area_class_list_cell_properties

(test gtk-cell-area-class-find-cell-property
  (let* ((gtk-init:*gtk-warn-deprecated* nil)
         (area (make-instance 'gtk:cell-area-box))
         (renderer (make-instance 'gtk:cell-renderer-pixbuf)))
    (is-false (gtk:cell-area-add area renderer))
    (is (cffi:pointerp (gtk:cell-area-class-find-cell-property "GtkCellAreaBox"
                                                               "align")))
    (is (cffi:pointerp (gtk:cell-area-class-find-cell-property "GtkCellAreaBox"
                                                               "expand")))
    (is (cffi:pointerp (gtk:cell-area-class-find-cell-property "GtkCellAreaBox"
                                                               "fixed-size")))
    (is (cffi:pointerp (gtk:cell-area-class-find-cell-property "GtkCellAreaBox"
                                                               "pack-type")))))

;;;     gtk_cell_area_add_with_properties

;;;     gtk_cell_area_cell_set
;;;     gtk_cell_area_cell_get

(test gtk-cell-area-cell-set/get
  (let* ((gtk-init:*gtk-warn-deprecated* nil)
         (area (make-instance 'gtk:cell-area-box))
         (renderer (make-instance 'gtk:cell-renderer-pixbuf)))
    (is-false (gtk:cell-area-add area renderer))
    (is-false (gtk:cell-area-cell-set area
                                      renderer
                                      "align" t
                                      "expand" t
                                      "fixed-size" nil
                                      "pack-type" :end))
    (is (equal '(t t nil :end)
               (gtk:cell-area-cell-get area
                                       renderer
                                       "align"
                                       "expand"
                                       "fixed-size"
                                       "pack-type")))))

;;;     gtk_cell_area_cell_set_valist
;;;     gtk_cell_area_cell_get_valist

;;;     gtk_cell_area_cell_set_property
;;;     gtk_cell_area_cell_get_property

(test gtk-cell-area-cell-property
  (let* ((gtk-init:*gtk-warn-deprecated* nil)
         (area (make-instance 'gtk:cell-area-box))
         (renderer (make-instance 'gtk:cell-renderer-pixbuf)))
    (is-false (gtk:cell-area-add area renderer))
    (is-false (gtk:cell-area-cell-property area renderer "align"))
    (is-false (gtk:cell-area-cell-property area renderer "expand"))
    (is-true (gtk:cell-area-cell-property area renderer "fixed-size"))
    (is (eq :start (gtk:cell-area-cell-property area renderer "pack-type")))))

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

;;; 2024-9-20
