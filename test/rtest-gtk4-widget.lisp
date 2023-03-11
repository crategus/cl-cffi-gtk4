(in-package :gtk-test)

(def-suite gtk-widget :in gtk-suite)
(in-suite gtk-widget)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkRequisition

(test requisition
  ;; Type check
  (is-true (g:type-is-a (g:gtype "GtkRequisition") +g-type-boxed+))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkRequisition")
          (g:gtype (foreign-funcall "gtk_requisition_get_type" :size)))))

(test requisition-new
  (is (typep (gtk:requisition-new) 'gtk:requisition)))

(test requisition-copy
  (let ((requisition (gtk:requisition-new)))
    (is (typep (gtk:requisition-copy requisition) 'gtk:requisition))))

(test requisition-accessors
  (let ((requisition (gtk:requisition-new)))
    (is (= 0 (gtk:requisition-width requisition)))
    (is (= 0 (gtk:requisition-height requisition)))))

;;; --- GtkWidget --------------------------------------------------------------

(test widget-class
  ;; Type check
  (is (g:type-is-object "GtkWidget"))
  ;; Check the registered name
  (is (eq 'gtk:widget
          (gobject:symbol-for-gtype "GtkWidget")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkWidget")
          (g:gtype (foreign-funcall "gtk_widget_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GInitiallyUnowned")
          (g:type-parent "GtkWidget")))
  ;; Check the children
  (is (or (equal '("GtkActionBar" "GtkAppChooserButton" "GtkAppChooserWidget"
                   "GtkAspectFrame" "GtkBox" "GtkBuiltinIcon" "GtkButton"
                   "GtkCalendar" "GtkCellView" "GtkCenterBox" "GtkCheckButton"
                   "GtkColorButton" "GtkColorChooserWidget" "GtkColorPlane"
                   "GtkColorSwatch" "GtkComboBox" "GtkDrawingArea" "GtkDropDown"
                   "GtkEditableLabel" "GtkEntry" "GtkExpander"
                   "GtkFileChooserErrorStack" "GtkFileChooserWidget" "GtkFixed"
                   "GtkFlowBox" "GtkFlowBoxChild" "GtkFontButton"
                   "GtkFontChooserWidget" "GtkFrame" "GtkGLArea" "GtkGizmo"
                   "GtkGrid" "GtkHeaderBar" "GtkIconView" "GtkImage"
                   "GtkInfoBar" "GtkLabel" "GtkLevelBar" "GtkListBase"
                   "GtkListBox" "GtkListBoxRow" "GtkListItemWidget"
                   "GtkMediaControls" "GtkMenuButton" "GtkModelButton"
                   "GtkNotebook" "GtkOverlay" "GtkPaned" "GtkPanedHandle"
                   "GtkPasswordEntry" "GtkPathBar" "GtkPicture"
                   "GtkPlacesSidebar" "GtkPopover" "GtkPopoverContent"
                   "GtkPopoverMenuBar" "GtkProgressBar" "GtkRange" "GtkRevealer"
                   "GtkScaleButton" "GtkScrollbar" "GtkScrolledWindow"
                   "GtkSearchBar" "GtkSearchEntry" "GtkSeparator"
                   "GtkShortcutLabel" "GtkShortcutsShortcut" "GtkSpinButton"
                   "GtkSpinner" "GtkStack" "GtkStackSidebar" "GtkStackSwitcher"
                   "GtkStatusbar" "GtkSwitch" "GtkText" "GtkTextView"
                   "GtkTreeView" "GtkVideo" "GtkViewport" "GtkWindow"
                   "GtkWindowControls" "GtkWindowHandle")
                 (list-children "GtkWidget"))
          (equal '("GtkActionBar" "GtkAppChooserButton" "GtkAppChooserWidget"
                   "GtkAspectFrame" "GtkBox" "GtkBuiltinIcon" "GtkButton"
                   "GtkCalendar" "GtkCellView" "GtkCenterBox" "GtkCheckButton"
                   "GtkColorButton" "GtkColorChooserWidget" "GtkColorPlane"
                   "GtkColorSwatch" "GtkColumnView" "GtkColumnViewTitle"
                   "GtkComboBox" "GtkDrawingArea" "GtkDropDown"
                   "GtkEditableLabel" "GtkEntry" "GtkExpander"
                   "GtkFileChooserErrorStack" "GtkFileChooserWidget" "GtkFixed"
                   "GtkFlowBox" "GtkFlowBoxChild" "GtkFontButton"
                   "GtkFontChooserWidget" "GtkFrame" "GtkGLArea" "GtkGizmo"
                   "GtkGrid" "GtkHeaderBar" "GtkIconView" "GtkImage"
                   "GtkInfoBar" "GtkLabel" "GtkLevelBar" "GtkListBase"
                   "GtkListBox" "GtkListBoxRow" "GtkListItemWidget"
                   "GtkMediaControls" "GtkMenuButton" "GtkModelButton"
                   "GtkNotebook" "GtkOverlay" "GtkPaned" "GtkPanedHandle"
                   "GtkPasswordEntry" "GtkPathBar" "GtkPicture"
                   "GtkPlacesSidebar" "GtkPopover" "GtkPopoverContent"
                   "GtkPopoverMenuBar" "GtkProgressBar" "GtkRange"
                   "GtkRevealer" "GtkScaleButton" "GtkScrollbar"
                   "GtkScrolledWindow" "GtkSearchBar" "GtkSearchEntry"
                   "GtkSeparator" "GtkShortcutLabel" "GtkShortcutsShortcut"
                   "GtkSpinButton" "GtkSpinner" "GtkStack" "GtkStackSidebar"
                   "GtkStackSwitcher" "GtkStatusbar" "GtkSwitch" "GtkText"
                   "GtkTextView" "GtkTreeView" "GtkVideo" "GtkViewport"
                   "GtkWindow" "GtkWindowControls" "GtkWindowHandle")
             (list-children "GtkWidget"))))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (list-interfaces "GtkWidget")))
  ;; Get the class properties
  (is (equal '("accessible-role" "can-focus" "can-target" "css-classes"
               "css-name" "cursor" "focus-on-click" "focusable" "halign"
               "has-default" "has-focus" "has-tooltip" "height-request"
               "hexpand" "hexpand-set" "layout-manager" "margin-bottom"
               "margin-end" "margin-start" "margin-top" "name" "opacity"
               "overflow" "parent" "receives-default" "root" "scale-factor"
               "sensitive" "tooltip-markup" "tooltip-text" "valign" "vexpand"
               "vexpand-set" "visible" "width-request")
             (list-properties "GtkWidget")))
  ;; Check the signals
  (is (equal '("destroy" "direction-changed" "hide" "keynav-failed" "map"
               "mnemonic-activate" "move-focus" "query-tooltip" "realize" "show"
               "state-flags-changed" "unmap" "unrealize")
             (list-signals "GtkWidget")))
  ;; CSS information
  ;; No CSS information for a abstract class
  ;; Get the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkWidget" GTK-WIDGET
                       (:SUPERCLASS G-INITIALLY-UNOWNED :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
                        :TYPE-INITIALIZER "gtk_widget_get_type")
                       ((CAN-FOCUS GTK-WIDGET-CAN-FOCUS "can-focus" "gboolean"
                         T T)
                        (CAN-TARGET GTK-WIDGET-CAN-TARGET "can-target"
                         "gboolean" T T)
                        (CSS-CLASSES GTK-WIDGET-CSS-CLASSES "css-classes"
                         "GStrv" T T)
                        (CSS-NAME GTK-WIDGET-CSS-NAME "css-name" "gchararray" T
                         NIL)
                        (CURSOR GTK-WIDGET-CURSOR "cursor" "GdkCursor" T T)
                        (FOCUS-ON-CLICK GTK-WIDGET-FOCUS-ON-CLICK
                         "focus-on-click" "gboolean" T T)
                        (FOCUSABLE GTK-WIDGET-FOCUSABLE "focusable" "gboolean"
                         T T)
                        (HALIGN GTK-WIDGET-HALIGN "halign" "GtkAlign" T T)
                        (HAS-DEFAULT GTK-WIDGET-HAS-DEFAULT "has-default"
                         "gboolean" T NIL)
                        (HAS-FOCUS GTK-WIDGET-HAS-FOCUS "has-focus" "gboolean"
                         T NIL)
                        (HAS-TOOLTIP GTK-WIDGET-HAS-TOOLTIP "has-tooltip"
                         "gboolean" T T)
                        (HEIGHT-REQUEST GTK-WIDGET-HEIGHT-REQUEST
                         "height-request" "gint" T T)
                        (HEXPAND GTK-WIDGET-HEXPAND "hexpand" "gboolean" T T)
                        (HEXPAND-SET GTK-WIDGET-HEXPAND-SET "hexpand-set"
                         "gboolean" T T)
                        (LAYOUT-MANAGER GTK-WIDGET-LAYOUT-MANAGER
                         "layout-manager" "GtkLayoutManager" T T)
                        (MARGIN-BOTTOM GTK-WIDGET-MARGIN-BOTTOM "margin-bottom"
                         "gint" T T)
                        (MARGIN-END GTK-WIDGET-MARGIN-END "margin-end" "gint" T
                         T)
                        (MARGIN-START GTK-WIDGET-MARGIN-START "margin-start"
                         "gint" T T)
                        (MARGIN-TOP GTK-WIDGET-MARGIN-TOP "margin-top" "gint" T
                         T)
                        (NAME GTK-WIDGET-NAME "name" "gchararray" T T)
                        (OPACITY GTK-WIDGET-OPACITY "opacity" "gdouble" T T)
                        (OVERFLOW GTK-WIDGET-OVERFLOW "overflow" "GtkOverflow"
                         T T)
                        (PARENT GTK-WIDGET-PARENT "parent" "GtkWidget" T NIL)
                        (RECEIVES-DEFAULT GTK-WIDGET-RECEIVES-DEFAULT
                         "receives-default" "gboolean" T T)
                        (ROOT GTK-WIDGET-ROOT "root" "GtkRoot" T NIL)
                        (SCALE-FACTOR GTK-WIDGET-SCALE-FACTOR "scale-factor"
                         "gint" T NIL)
                        (SENSITIVE GTK-WIDGET-SENSITIVE "sensitive" "gboolean"
                         T T)
                        (TOOLTIP-MARKUP GTK-WIDGET-TOOLTIP-MARKUP
                         "tooltip-markup" "gchararray" T T)
                        (TOOLTIP-TEXT GTK-WIDGET-TOOLTIP-TEXT "tooltip-text"
                         "gchararray" T T)
                        (VALIGN GTK-WIDGET-VALIGN "valign" "GtkAlign" T T)
                        (VEXPAND GTK-WIDGET-VEXPAND "vexpand" "gboolean" T T)
                        (VEXPAND-SET GTK-WIDGET-VEXPAND-SET "vexpand-set"
                         "gboolean" T T)
                        (VISIBLE GTK-WIDGET-VISIBLE "visible" "gboolean" T T)
                        (WIDTH-REQUEST GTK-WIDGET-WIDTH-REQUEST "width-request"
                         "gint" T T)))
             (gobject:get-g-type-definition "GtkWidget"))))

;;; --- Properties -------------------------------------------------------------

(test widget-properties-for-label
  (let ((widget (make-instance 'gtk:label)))
    (is-true (gtk:widget-can-focus widget))
    (is-true (gtk:widget-can-target widget))
    (is-false (gtk:widget-css-classes widget))
    (is (string= "label" (gtk:widget-css-name widget)))
    (is-false (gtk:widget-cursor widget))
    (is-true (gtk:widget-focus-on-click widget))
    (is-false (gtk:widget-focusable widget))
    (is (eq :fill (gtk:widget-halign widget)))
    (is-false (gtk:widget-has-default widget))
    (is-false (gtk:widget-has-focus widget))
    (is-false (gtk:widget-has-tooltip widget))
    (is (= -1 (gtk:widget-height-request widget)))
    (is-false (gtk:widget-hexpand widget))
    (is-false (gtk:widget-hexpand-set widget))
    (is-false (gtk:widget-layout-manager widget))
    (is (= 0 (gtk:widget-margin-bottom widget)))
    (is (= 0 (gtk:widget-margin-end widget)))
    (is (= 0 (gtk:widget-margin-start widget)))
    (is (= 0 (gtk:widget-margin-top widget)))
    (is (string= "" (gtk:widget-name widget)))
    (is (= 1.0d0 (gtk:widget-opacity widget)))
    (is (eq :visible (gtk:widget-overflow widget)))
    (is-false (gtk:widget-parent widget))
    (is-false (gtk:widget-receives-default widget))
    (is-false (gtk:widget-root widget))
    (is (= 1 (gtk:widget-scale-factor widget)))
    (is-true (gtk:widget-sensitive widget))
    (is-false (gtk:widget-tooltip-markup widget))
    (is-false (gtk:widget-tooltip-text widget))
    (is (eq :fill (gtk:widget-valign widget)))
    (is-false (gtk:widget-vexpand widget))
    (is-false (gtk:widget-vexpand-set widget))
    (is-true (gtk:widget-visible widget))
    (is (= -1 (gtk:widget-width-request widget)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_widget_in_destruction
;;;     gtk_widget_unparent
;;;     gtk_widget_show
;;;     gtk_widget_hide
;;;     gtk_widget_map
;;;     gtk_widget_unmap
;;;     gtk_widget_realize
;;;     gtk_widget_unrealize
;;;     gtk_widget_queue_draw
;;;     gtk_widget_queue_resize
;;;     gtk_widget_queue_allocate
;;;     gtk_widget_get_frame_clock

;;;     GtkTickCallback

;;;     gtk_widget_add_tick_callback
;;;     gtk_widget_remove_tick_callback
;;;     gtk_widget_size_allocate
;;;     gtk_widget_allocate
;;;     gtk_widget_class_add_shortcut
;;;     gtk_widget_class_add_binding
;;;     gtk_widget_class_add_binding_signal
;;;     gtk_widget_class_add_binding_action
;;;     gtk_widget_class_get_layout_manager_type
;;;     gtk_widget_class_set_layout_manager_type
;;;     gtk_widget_class_set_activate_signal_from_name
;;;     gtk_widget_class_get_activate_signal
;;;     gtk_widget_class_set_activate_signal
;;;     gtk_widget_activate
;;;     gtk_widget_is_focus
;;;     gtk_widget_grab_focus
;;;     gtk_widget_get_native
;;;     gtk_widget_get_ancestor
;;;     gtk_widget_is_ancestor
;;;     gtk_widget_translate_coordinates
;;;     gtk_widget_add_controller
;;;     gtk_widget_remove_controller
;;;     gtk_widget_get_direction
;;;     gtk_widget_set_direction
;;;     gtk_widget_get_default_direction
;;;     gtk_widget_set_default_direction
;;;     gtk_widget_create_pango_context
;;;     gtk_widget_get_pango_context
;;;     gtk_widget_get_font_options
;;;     gtk_widget_set_font_options
;;;     gtk_widget_get_font_map
;;;     gtk_widget_set_font_map
;;;     gtk_widget_create_pango_layout
;;;     gtk_widget_set_cursor_from_name
;;;     gtk_widget_mnemonic_activate
;;;     gtk_widget_class_get_accessible_role
;;;     gtk_widget_class_set_accessible_role
;;;     gtk_widget_child_focus
;;;     gtk_widget_get_child_visible
;;;     gtk_widget_set_child_visible
;;;     gtk_widget_get_settings
;;;     gtk_widget_get_clipboard
;;;     gtk_widget_get_primary_clipboard
;;;     gtk_widget_get_display
;;;     gtk_widget_get_size_request
;;;     gtk_widget_set_size_request
;;;     gtk_widget_list_mnemonic_labels
;;;     gtk_widget_add_mnemonic_label
;;;     gtk_widget_remove_mnemonic_label
;;;     gtk_widget_error_bell
;;;     gtk_widget_keynav_failed
;;;     gtk_widget_trigger_tooltip_query
;;;     gtk_widget_get_allocated_width
;;;     gtk_widget_get_allocated_height
;;;     gtk_widget_get_allocation
;;;     gtk_widget_get_allocated_baseline
;;;     gtk_widget_get_width
;;;     gtk_widget_get_height
;;;     gtk_widget_get_size
;;;     gtk_widget_compute_bounds
;;;     gtk_widget_compute_transform
;;;     gtk_widget_compute_point
;;;     gtk_widget_contains
;;;     gtk_widget_pick
;;;     gtk_widget_get_focus_child
;;;     gtk_widget_set_focus_child
;;;     gtk_widget_is_sensitive
;;;     gtk_widget_is_visible
;;;     gtk_widget_get_state_flags
;;;     gtk_widget_set_state_flags
;;;     gtk_widget_unset_state_flags
;;;     gtk_widget_has_visible_focus
;;;     gtk_widget_is_drawable
;;;     gtk_widget_get_realized
;;;     gtk_widget_get_mapped
;;;     gtk_widget_measure
;;;     gtk_widget_snapshot_child
;;;     gtk_widget_get_next_sibling
;;;     gtk_widget_get_prev_sibling
;;;     gtk_widget_get_first_child
;;;     gtk_widget_get_last_child
;;;     gtk_widget_insert_before
;;;     gtk_widget_insert_after
;;;     gtk_widget_should_layout
;;;     gtk_widget_add_css_class
;;;     gtk_widget_remove_css_class
;;;     gtk_widget_has_css_class
;;;     gtk_widget_get_style_context
;;;     gtk_widget_class_get_css_name
;;;     gtk_widget_class_set_css_name
;;;     gtk_requisition_new
;;;     gtk_requisition_copy
;;;     gtk_requisition_free
;;;     gtk_widget_get_request_mode
;;;     gtk_widget_get_preferred_size
;;;     gtk_distribute_natural_allocation
;;;     gtk_widget_compute_expand
;;;     gtk_widget_init_template
;;;     gtk_widget_class_set_template
;;;     gtk_widget_class_set_template_from_resource
;;;     gtk_widget_get_template_child
;;;     gtk_widget_class_bind_template_child
;;;     gtk_widget_class_bind_template_child_internal
;;;     gtk_widget_class_bind_template_child_private
;;;     gtk_widget_class_bind_template_child_internal_private
;;;     gtk_widget_class_bind_template_child_full
;;;     gtk_widget_class_bind_template_callback
;;;     gtk_widget_class_bind_template_callback_full
;;;     gtk_widget_class_set_template_scope
;;;     gtk_widget_observe_children
;;;     gtk_widget_observe_controllers
;;;     gtk_widget_insert_action_group
;;;     gtk_widget_activate_action
;;;     gtk_widget_activate_action_variant
;;;     gtk_widget_activate_default
;;;
;;;     GtkWidgetActionActivateFunc
;;;
;;;     gtk_widget_class_install_action
;;;     gtk_widget_class_install_property_action
;;;     gtk_widget_class_query_action
;;;     gtk_widget_action_set_enabled

;;; --- 2023-3-11 --------------------------------------------------------------
