(in-package :gtk-test)

(def-suite gtk-flow-box :in gtk-suite)
(in-suite gtk-flow-box)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFlowBoxChild

(test gtk-flow-box-child-class
  ;; Check type
  (is (g:type-is-object "GtkFlowBoxChild"))
  ;; Check registered name
  (is (eq 'gtk:flow-box-child
          (glib:symbol-for-gtype "GtkFlowBoxChild")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkFlowBoxChild")
          (g:gtype (cffi:foreign-funcall "gtk_flow_box_child_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkFlowBoxChild")))
  ;; Check children
  #-windows
  (if *first-run-gtk-test*
      (is (equal '()
                 (list-children "GtkFlowBoxChild"))))
  #+windows
  (is (equal '()
             (list-children "GtkFlowBoxChild")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (list-interfaces "GtkFlowBoxChild")))
  ;; Check properties
  (is (equal '("child")
             (list-properties "GtkFlowBoxChild")))
  ;; Check signals
  (is (equal '("activate")
             (list-signals "GtkFlowBoxChild")))
  ;; Check CSS name
  (is (string= "flowboxchild"
               (gtk:widget-class-css-name "GtkFlowBoxChild")))
  ;; Check CSS classes
  (is (equal '()
             (gtk:widget-css-classes (make-instance 'gtk:flow-box-child))))
  ;; Check accessible role
  (is (eq :grid-cell (gtk:widget-class-accessible-role "GtkFlowBoxChild")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkFlowBoxChild" GTK-FLOW-BOX-CHILD
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkBuildable"
                                 "GtkConstraintTarget")
                                :TYPE-INITIALIZER
                                "gtk_flow_box_child_get_type")
                               ((CHILD GTK-FLOW-BOX-CHILD-CHILD "child"
                                 "GtkWidget" T T)))
             (gobject:get-g-type-definition "GtkFlowBoxChild"))))

;;; --- Properties -------------------------------------------------------------

;;;     child

(test gtk-flow-box-child-properties
  (let* ((button (gtk:button-new))
         (child (make-instance 'gtk:flow-box-child
                               :child button)))
    (is (eq button (gtk:flow-box-child-child child)))))

;;; --- Signals ----------------------------------------------------------------

;;;     activate

;;; --- Functions --------------------------------------------------------------

;;;     gtk_flow_box_child_new

(test gtk-flow-box-child-new
  (is (typep (gtK:flow-box-child-new) 'gtk:flow-box-child)))

;;;     gtk_flow_box_child_get_index
;;;     gtk_flow_box_child_is_selected
;;;     gtk_flow_box_child_changed

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFlowBox

(test gtk-flow-box-class
  ;; Check type
  (is (g:type-is-object "GtkFlowBox"))
  ;; Check registered name
  (is (eq 'gtk:flow-box
          (glib:symbol-for-gtype "GtkFlowBox")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkFlowBox")
          (g:gtype (cffi:foreign-funcall "gtk_flow_box_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkFlowBox")))
  ;; Check children
  (is (equal '()
             (list-children "GtkFlowBox")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkOrientable")
             (list-interfaces "GtkFlowBox")))
  ;; Check properties
  (is (equal '("accept-unpaired-release" "activate-on-single-click"
               "column-spacing" "homogeneous" "max-children-per-line"
               "min-children-per-line" "orientation" "row-spacing"
               "selection-mode")
             (list-properties "GtkFlowBox")))
  ;; Check signals
  (is (equal '("activate-cursor-child" "child-activated" "move-cursor"
               "select-all" "selected-children-changed" "toggle-cursor-child"
               "unselect-all")
             (list-signals "GtkFlowBox")))
  ;; Check CSS name
  (is (string= "flowbox"
               (gtk:widget-class-css-name "GtkFlowBox")))
  ;; Check CSS classes
  (is (equal '()
             (gtk:widget-css-classes (make-instance 'gtk:flow-box-child))))
  ;; Check accessible role
  (is (eq :GRID (gtk:widget-class-accessible-role "GtkFlowBox")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkFlowBox" GTK-FLOW-BOX
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkBuildable"
                                 "GtkConstraintTarget" "GtkOrientable")
                                :TYPE-INITIALIZER "gtk_flow_box_get_type")
                               ((ACCEPT-UNPAIRED-RELEASE
                                 GTK-FLOW-BOX-ACCEPT-UNPAIRED-RELEASE
                                 "accept-unpaired-release" "gboolean" T T)
                                (ACTIVATE-ON-SINGLE-CLICK
                                 GTK-FLOW-BOX-ACTIVATE-ON-SINGLE-CLICK
                                 "activate-on-single-click" "gboolean" T T)
                                (COLUMN-SPACING GTK-FLOW-BOX-COLUMN-SPACING
                                 "column-spacing" "guint" T T)
                                (HOMOGENEOUS GTK-FLOW-BOX-HOMOGENEOUS
                                 "homogeneous" "gboolean" T T)
                                (MAX-CHILDREN-PER-LINE
                                 GTK-FLOW-BOX-MAX-CHILDREN-PER-LINE
                                 "max-children-per-line" "guint" T T)
                                (MIN-CHILDREN-PER-LINE
                                 GTK-FLOW-BOX-MIN-CHILDREN-PER-LINE
                                 "min-children-per-line" "guint" T T)
                                (ROW-SPACING GTK-FLOW-BOX-ROW-SPACING
                                 "row-spacing" "guint" T T)
                                (SELECTION-MODE GTK-FLOW-BOX-SELECTION-MODE
                                 "selection-mode" "GtkSelectionMode" T T)))
             (gobject:get-g-type-definition "GtkFlowBox"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-flow-box-properties
  (let ((flowbox (make-instance 'gtk:flow-box)))
    (is-false (gtk:flow-box-accept-unpaired-release flowbox))
    (is-true (gtk:flow-box-activate-on-single-click flowbox))
    (is (= 0 (gtk:flow-box-column-spacing flowbox)))
    (is-false (gtk:flow-box-homogeneous flowbox))
    (is (= 7 (gtk:flow-box-max-children-per-line flowbox)))
    (is (= 0 (gtk:flow-box-min-children-per-line flowbox)))
    (is (= 0 (gtk:flow-box-row-spacing flowbox)))
    (is (eq :single (gtk:flow-box-selection-mode flowbox)))))

;;; --- Signals ----------------------------------------------------------------

;;;     activate-cursor-child
;;;     child-activated
;;;     move-cursor
;;;     select-all
;;;     selected-children-changed
;;;     toggle-cursor-child
;;;     unselect-all

;;; --- Functions --------------------------------------------------------------

;;;     gtk_flow_box_new

(test gtk-flow-box-new
  (is (typep (gtk:flow-box-new) 'gtk:flow-box)))

;;;     gtk_flow_box_insert
;;;     gtk_flow_box_append                                Since 4.6
;;;     gtk_flow_box_prepend                               Since 4.6
;;;     gtk_flow_box_remove
;;;     gtk_flow_box_remove_all                            Since 4.12
;;;     gtk_flow_box_get_child_at_index
;;;     gtk_flow_box_get_child_at_pos
;;;     gtk_flow_box_set_hadjustment
;;;     gtk_flow_box_set_vadjustment

;;;     GtkFlowBoxForeachFunc

;;;     gtk_flow_box_selected_foreach
;;;     gtk_flow_box_get_selected_children
;;;     gtk_flow_box_select_child
;;;     gtk_flow_box_unselect_child
;;;     gtk_flow_box_select_all
;;;     gtk_flow_box_unselect_all

;;;     GtkFlowBoxFilterFunc

;;;     gtk_flow_box_set_filter_func
;;;     gtk_flow_box_invalidate_filter

;;;     GtkFlowBoxSortFunc

;;;     gtk_flow_box_set_sort_func
;;;     gtk_flow_box_invalidate_sort

;;;     GtkFlowBoxCreateWidgetFunc

;;;     gtk_flow_box_bind_model

;;; 2024-4-14
