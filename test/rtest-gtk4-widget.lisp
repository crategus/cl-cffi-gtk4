(in-package :gtk-test)

(def-suite gtk-widget :in gtk-abstract-widgets)
(in-suite gtk-widget)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkRequisition

(test gtk-requisition-boxed
  ;; Check type
  (is (g:type-is-boxed "GtkRequisition"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkRequisition")
          (g:gtype (cffi:foreign-funcall "gtk_requisition_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:requisition
          (glib:symbol-for-gtype "GtkRequisition"))))

;;;     gtk_requisition_new

(test gtk-requisition-new
  (is (typep (gtk:requisition-new) 'gtk:requisition)))

;;;     gtk_requisition_copy

(test gtk-requisition-copy
  (let ((requisition (gtk:requisition-new)))
    (is (typep (gtk:requisition-copy requisition) 'gtk:requisition))))

(test gtk-requisition-accessors
  (let ((requisition (gtk:requisition-new)))
    (is (= 0 (gtk:requisition-width requisition)))
    (is (= 0 (gtk:requisition-height requisition)))))

;;; --- GtkWidget --------------------------------------------------------------

(test gtk-widget-class
  ;; Check type
  (is (g:type-is-object "GtkWidget"))
  ;; Check registered name
  (is (eq 'gtk:widget
          (glib:symbol-for-gtype "GtkWidget")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkWidget")
          (g:gtype (cffi:foreign-funcall "gtk_widget_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GInitiallyUnowned")
          (g:type-parent "GtkWidget")))
  ;; Check children (for a subset)
  (is (subsetp '("GtkActionBar" "GtkAppChooserButton" "GtkAppChooserWidget"
                 "GtkAspectFrame" "GtkBox" "GtkButton" "GtkCalendar"
                 "GtkCellView" "GtkCenterBox" "GtkCheckButton" "GtkColorButton"
                 "GtkColorChooserWidget" "GtkColorDialogButton" "GtkColumnView"
                 "GtkComboBox" "GtkDragIcon" "GtkDrawingArea" "GtkDropDown"
                 "GtkEditableLabel" "GtkEntry" "GtkExpander"
                 "GtkFileChooserWidget" "GtkFixed" "GtkFlowBox"
                 "GtkFlowBoxChild" "GtkFontButton" "GtkFontChooserWidget"
                 "GtkFontDialogButton" "GtkFrame" "GtkGLArea"
                 "GtkGraphicsOffload" "GtkGrid" "GtkHeaderBar" "GtkIconView"
                 "GtkImage" "GtkInfoBar" "GtkInscription" "GtkLabel"
                 "GtkLevelBar" "GtkListBase" "GtkListBox" "GtkListBoxRow"
                 "GtkMediaControls" "GtkMenuButton" "GtkNotebook" "GtkOverlay"
                 "GtkPaned" "GtkPasswordEntry" "GtkPicture" "GtkPopover"
                 "GtkPopoverMenuBar" "GtkProgressBar" "GtkRange" "GtkRevealer"
                 "GtkScaleButton" "GtkScrollbar" "GtkScrolledWindow"
                 "GtkSearchBar" "GtkSearchEntry" "GtkSeparator"
                 "GtkShortcutLabel" "GtkShortcutsShortcut" "GtkSpinButton"
                 "GtkSpinner" "GtkStack" "GtkStackSidebar" "GtkStackSwitcher"
                 "GtkStatusbar" "GtkSwitch" "GtkText" "GtkTextView"
                 "GtkTreeExpander" "GtkTreeView" "GtkVideo" "GtkViewport"
                 "GtkWindow" "GtkWindowControls" "GtkWindowHandle")
                 (glib-test:list-children "GtkWidget") :test #'string=))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (glib-test:list-interfaces "GtkWidget")))
  ;; Check class properties
  #-gtk-4-18
  (is (equal '("accessible-role" "can-focus" "can-target" "css-classes"
               "css-name" "cursor" "focus-on-click" "focusable" "halign"
               "has-default" "has-focus" "has-tooltip" "height-request"
               "hexpand" "hexpand-set" "layout-manager" "margin-bottom"
               "margin-end" "margin-start" "margin-top" "name" "opacity"
               "overflow" "parent" "receives-default" "root" "scale-factor"
               "sensitive" "tooltip-markup" "tooltip-text" "valign" "vexpand"
               "vexpand-set" "visible" "width-request")
             (glib-test:list-properties "GtkWidget")))
  #+gtk-4-18
  (is (equal '("accessible-role" "can-focus" "can-target" "css-classes"
               "css-name" "cursor" "focus-on-click" "focusable" "halign"
               "has-default" "has-focus" "has-tooltip" "height-request"
               "hexpand" "hexpand-set" "layout-manager" "limit-events"
               "margin-bottom" "margin-end" "margin-start" "margin-top" "name"
               "opacity" "overflow" "parent" "receives-default" "root"
               "scale-factor" "sensitive" "tooltip-markup" "tooltip-text"
               "valign" "vexpand" "vexpand-set" "visible" "width-request")
             (glib-test:list-properties "GtkWidget")))
  ;; Check signals
  (is (equal '("destroy" "direction-changed" "hide" "keynav-failed" "map"
               "mnemonic-activate" "move-focus" "query-tooltip" "realize" "show"
               "state-flags-changed" "unmap" "unrealize")
             (glib-test:list-signals "GtkWidget")))
  ;; Check CSS information
  ;; No CSS information for a abstract class
  ;; Check class definition
  #-gtk-4-18
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkWidget" GTK:WIDGET
                      (:SUPERCLASS G:INITIALLY-UNOWNED
                       :EXPORT T
                       :INTERFACES
                       ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
                       :TYPE-INITIALIZER "gtk_widget_get_type")
                      ((CAN-FOCUS WIDGET-CAN-FOCUS "can-focus" "gboolean" T T)
                       (CAN-TARGET WIDGET-CAN-TARGET "can-target" "gboolean" T T)
                       (CSS-CLASSES WIDGET-CSS-CLASSES "css-classes" "GStrv" T T)
                       (CSS-NAME WIDGET-CSS-NAME "css-name" "gchararray" T NIL)
                       (CURSOR WIDGET-CURSOR "cursor" "GdkCursor" T T)
                       (FOCUS-ON-CLICK WIDGET-FOCUS-ON-CLICK
                        "focus-on-click" "gboolean" T T)
                       (FOCUSABLE WIDGET-FOCUSABLE "focusable" "gboolean" T T)
                       (HALIGN WIDGET-HALIGN "halign" "GtkAlign" T T)
                       (HAS-DEFAULT WIDGET-HAS-DEFAULT
                        "has-default" "gboolean" T NIL)
                       (HAS-FOCUS WIDGET-HAS-FOCUS "has-focus" "gboolean" T NIL)
                       (HAS-TOOLTIP WIDGET-HAS-TOOLTIP
                        "has-tooltip" "gboolean" T T)
                       (HEIGHT-REQUEST WIDGET-HEIGHT-REQUEST
                        "height-request" "gint" T T)
                       (HEXPAND WIDGET-HEXPAND "hexpand" "gboolean" T T)
                       (HEXPAND-SET WIDGET-HEXPAND-SET
                        "hexpand-set" "gboolean" T T)
                       (LAYOUT-MANAGER WIDGET-LAYOUT-MANAGER
                        "layout-manager" "GtkLayoutManager" T T)
                       (MARGIN-BOTTOM WIDGET-MARGIN-BOTTOM
                        "margin-bottom" "gint" T T)
                       (MARGIN-END WIDGET-MARGIN-END "margin-end" "gint" T T)
                       (MARGIN-START WIDGET-MARGIN-START
                        "margin-start" "gint" T T)
                       (MARGIN-TOP WIDGET-MARGIN-TOP "margin-top" "gint" T T)
                       (NAME WIDGET-NAME "name" "gchararray" T T)
                       (OPACITY WIDGET-OPACITY "opacity" "gdouble" T T)
                       (OVERFLOW WIDGET-OVERFLOW "overflow" "GtkOverflow" T T)
                       (PARENT WIDGET-PARENT "parent" "GtkWidget" T NIL)
                       (RECEIVES-DEFAULT WIDGET-RECEIVES-DEFAULT
                        "receives-default" "gboolean" T T)
                       (ROOT WIDGET-ROOT "root" "GtkRoot" T NIL)
                       (SCALE-FACTOR WIDGET-SCALE-FACTOR
                        "scale-factor" "gint" T NIL)
                       (SENSITIVE WIDGET-SENSITIVE "sensitive" "gboolean" T T)
                       (TOOLTIP-MARKUP WIDGET-TOOLTIP-MARKUP
                        "tooltip-markup" "gchararray" T T)
                       (TOOLTIP-TEXT WIDGET-TOOLTIP-TEXT
                        "tooltip-text" "gchararray" T T)
                       (VALIGN WIDGET-VALIGN "valign" "GtkAlign" T T)
                       (VEXPAND WIDGET-VEXPAND "vexpand" "gboolean" T T)
                       (VEXPAND-SET WIDGET-VEXPAND-SET
                        "vexpand-set" "gboolean" T T)
                       (VISIBLE WIDGET-VISIBLE "visible" "gboolean" T T)
                       (WIDTH-REQUEST WIDGET-WIDTH-REQUEST
                        "width-request" "gint" T T)))
             (gobject:get-gtype-definition "GtkWidget")))
  #+gtk-4-18
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkWidget" GTK:WIDGET
                      (:SUPERCLASS G:INITIALLY-UNOWNED
                       :EXPORT T
                       :INTERFACES
                       ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
                       :TYPE-INITIALIZER "gtk_widget_get_type")
                      ((CAN-FOCUS WIDGET-CAN-FOCUS "can-focus" "gboolean" T T)
                       (CAN-TARGET WIDGET-CAN-TARGET "can-target" "gboolean" T T)
                       (CSS-CLASSES WIDGET-CSS-CLASSES "css-classes" "GStrv" T T)
                       (CSS-NAME WIDGET-CSS-NAME "css-name" "gchararray" T NIL)
                       (CURSOR WIDGET-CURSOR "cursor" "GdkCursor" T T)
                       (FOCUS-ON-CLICK WIDGET-FOCUS-ON-CLICK
                        "focus-on-click" "gboolean" T T)
                       (FOCUSABLE WIDGET-FOCUSABLE "focusable" "gboolean" T T)
                       (HALIGN WIDGET-HALIGN "halign" "GtkAlign" T T)
                       (HAS-DEFAULT WIDGET-HAS-DEFAULT
                        "has-default" "gboolean" T NIL)
                       (HAS-FOCUS WIDGET-HAS-FOCUS "has-focus" "gboolean" T NIL)
                       (HAS-TOOLTIP WIDGET-HAS-TOOLTIP
                        "has-tooltip" "gboolean" T T)
                       (HEIGHT-REQUEST WIDGET-HEIGHT-REQUEST
                        "height-request" "gint" T T)
                       (HEXPAND WIDGET-HEXPAND "hexpand" "gboolean" T T)
                       (HEXPAND-SET WIDGET-HEXPAND-SET
                        "hexpand-set" "gboolean" T T)
                       (LAYOUT-MANAGER WIDGET-LAYOUT-MANAGER
                        "layout-manager" "GtkLayoutManager" T T)
                       (LIMIT-EVENTS WIDGET-LIMIT-EVENTS
                        "limit-events" "gboolean" T T)
                       (MARGIN-BOTTOM WIDGET-MARGIN-BOTTOM
                        "margin-bottom" "gint" T T)
                       (MARGIN-END WIDGET-MARGIN-END "margin-end" "gint" T T)
                       (MARGIN-START WIDGET-MARGIN-START
                        "margin-start" "gint" T T)
                       (MARGIN-TOP WIDGET-MARGIN-TOP "margin-top" "gint" T T)
                       (NAME WIDGET-NAME "name" "gchararray" T T)
                       (OPACITY WIDGET-OPACITY "opacity" "gdouble" T T)
                       (OVERFLOW WIDGET-OVERFLOW "overflow" "GtkOverflow" T T)
                       (PARENT WIDGET-PARENT "parent" "GtkWidget" T NIL)
                       (RECEIVES-DEFAULT WIDGET-RECEIVES-DEFAULT
                        "receives-default" "gboolean" T T)
                       (ROOT WIDGET-ROOT "root" "GtkRoot" T NIL)
                       (SCALE-FACTOR WIDGET-SCALE-FACTOR
                        "scale-factor" "gint" T NIL)
                       (SENSITIVE WIDGET-SENSITIVE "sensitive" "gboolean" T T)
                       (TOOLTIP-MARKUP WIDGET-TOOLTIP-MARKUP
                        "tooltip-markup" "gchararray" T T)
                       (TOOLTIP-TEXT WIDGET-TOOLTIP-TEXT
                        "tooltip-text" "gchararray" T T)
                       (VALIGN WIDGET-VALIGN "valign" "GtkAlign" T T)
                       (VEXPAND WIDGET-VEXPAND "vexpand" "gboolean" T T)
                       (VEXPAND-SET WIDGET-VEXPAND-SET
                        "vexpand-set" "gboolean" T T)
                       (VISIBLE WIDGET-VISIBLE "visible" "gboolean" T T)
                       (WIDTH-REQUEST WIDGET-WIDTH-REQUEST
                        "width-request" "gint" T T)))
             (gobject:get-gtype-definition "GtkWidget"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-widget-properties-for-label
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
    ;; has-default is not writable
    (signals (error) (setf (gtk:widget-has-default widget) t))
    (is-false (gtk:widget-has-focus widget))
    ;; has-focus is not writable
    (signals (error) (setf (gtk:widget-has-focus widget) t))
    (is-false (gtk:widget-has-tooltip widget))
    (is (= -1 (gtk:widget-height-request widget)))
    (is-false (gtk:widget-hexpand widget))
    (is-false (gtk:widget-hexpand-set widget))
    (is-false (gtk:widget-layout-manager widget))
    #+gtk-4-18
    (is-false (gtk:widget-limit-events widget))
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

;;;     gtk_widget_show
;;;     gtk_widget_hide

;;;     gtk_widget_map
;;;     gtk_widget_unmap
;;;     gtk_widget_get_mapped

;; TODO: Can we avoid the warnings from GSK?

;; Gsk-Message: 17:32:52.746: Failed to realize renderer of type
;; 'GskNglRenderer' for surface 'GdkWaylandToplevel':
;; Es wird versucht EGL zu verwenden, aber X11 GLX ist bereits in Verwendung

;; Gsk-Message: 17:32:52.746: Failed to realize renderer of type
;; 'GskGLRenderer' for surface 'GdkWaylandToplevel':
;;; Es wird versucht EGL zu verwenden, aber X11 GLX ist bereits in Verwendung

(test gtk-widget-map/unmap
  (let* ((button (gtk:button-new-with-label "label"))
         (window (make-instance 'gtk:window
                                :child button)))
    (is (eq window (gtk:widget-root button)))
    (is-false (gtk:widget-realized button))
    (is-false (gtk:widget-mapped button))
    ;; Map button
    (is-false (gtk:widget-map button))
    (is-true (gtk:widget-realized button))
    (is-true (gtk:widget-mapped button))
    ;; Unmap button
    (is-false (gtk:widget-unmap button))
    (is-true (gtk:widget-realized button))
    (is-false (gtk:widget-mapped button))
    ;; Remove BUTTON from WINDOW and destroy WINDOW
    (is-false (setf (gtk:window-child window) nil))
    (is-false (gtk:window-destroy window))
    (is (= 1 (g:object-ref-count button)))
    (is (= 1 (g:object-ref-count window)))))

;;;     gtk_widget_realize
;;;     gtk_widget_unrealize
;;;     gtk_widget_get_realized

;; TODO: Can we avoid the warnings from GSK?

;; Gsk-Message: 17:32:52.753: Failed to realize renderer of type
;;; 'GskNglRenderer' for surface 'GdkWaylandToplevel':
;;; Es wird versucht EGL zu verwenden, aber X11 GLX ist bereits in Verwendung

;;; Gsk-Message: 17:32:52.753: Failed to realize renderer of type
;; 'GskGLRenderer' for surface 'GdkWaylandToplevel':
;;; Es wird versucht EGL zu verwenden, aber X11 GLX ist bereits in Verwendung

(test gtk-widget-realize/unrealize
  (let* ((button (gtk:button-new-with-label "label"))
         (window (make-instance 'gtk:window
                                :child button)))
    (is (eq window (gtk:widget-root button)))
    (is-false (gtk:widget-realized button))
    (is-false (gtk:widget-mapped button))
    ;; Realize button
    (is-false (gtk:widget-realize button))
    (is-true (gtk:widget-realized button))
    (is-false (gtk:widget-mapped button))
    ;; Unrealize button
    (is-false (gtk:widget-unrealize button))
    (is-false (gtk:widget-realized button))
    (is-false (gtk:widget-mapped button))
    ;; Remove BUTTON from WINDOW and destroy WINDOW
    (is-false (setf (gtk:window-child window) nil))
    (is-false (gtk:window-destroy window))
    (is (= 1 (g:object-ref-count button)))
    (is (= 1 (g:object-ref-count window)))))

;;;     gtk_widget_queue_draw
;;;     gtk_widget_queue_resize
;;;     gtk_widget_queue_allocate

;;;     gtk_widget_get_frame_clock

(test gtk-widget-frame-clock
  (let* ((button (gtk:button-new-with-label "label"))
         (window (make-instance 'gtk:window
                                :child button)))
    (is-false (gtk:widget-realize button))
    (is (typep (gtk:widget-frame-clock button) 'gdk:frame-clock))

    ;; Remove BUTTON from WINDOW and destroy WINDOW
    (is-false (setf (gtk:window-child window) nil))
    (is-false (gtk:window-destroy window))
    (is (= 1 (g:object-ref-count button)))
    (is (= 1 (g:object-ref-count window)))))

;;;     GtkTickCallback

;;;     gtk_widget_add_tick_callback
;;;     gtk_widget_remove_tick_callback

;;;     gtk_widget_size_allocate

;; TODO: Allocation of baseline does not work in this example

(test gtk-widget-size-allocate
  (let ((box (gtk:center-box-new))
        (allocation (gdk:rectangle-new :width 200 :height 100)))
    ;; No size allocated
    (is (= 0 (gtk:widget-width box)))
    (is (= 0 (gtk:widget-height box)))
    (is (= -1 (gtk:widget-baseline box)))
    ;; Allocate size
    (is-false (gtk:widget-size-allocate box allocation 10))
    ;; Check allocated size
    (is (= 200 (gtk:widget-width box)))
    (is (= 100 (gtk:widget-height box)))
    (is (=  -1 (gtk:widget-baseline box)))
    ;; Check memory management
    (is (= 1 (g:object-ref-count box)))))

;;;     gtk_widget_allocate

;; TODO: Allocation of baseline does not work in this example

(test gtk-widget-allocate
  (let ((label (gtk:label-new "text"))
        (transform (gsk:transform-parse "translate(20,10)")))
    ;; No size allocated
    (is (= 0 (gtk:widget-width label)))
    (is (= 0 (gtk:widget-height label)))
    (is (= -1 (gtk:widget-baseline label)))
    ;; Allocate size
    (is-false (gtk:widget-allocate label 200 100 10 transform))
    ;; Check allocated size
    (is (= 200 (gtk:widget-width label)))
    (is (= 100 (gtk:widget-height label)))
    (is (= -1 (gtk:widget-baseline label)))
    ;; Check memory management
    (is (= 1 (g:object-ref-count label)))))

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

(test gtk-widget-activate
  (let ((button (make-instance 'gtk:button))
        (message nil))
    (g:signal-connect button "activate"
                      (lambda (button)
                        (declare (ignore button))
                        (setf message "Button is activated")))
    (is-true (gtk:widget-activate button))
    (is (string= "Button is activated" message))))

;;;     gtk_widget_is_focus

(test gtk-widget-is-focus
  (let ((button (make-instance 'gtk:button)))
    (is-false (gtk:widget-is-focus button))))

;;;     gtk_widget_grab_focus

;;;     gtk_widget_set_parent
;;;     gtk_widget_unparent

(test gtk-widget-set-parent/unparent
  (let (frame label)
    (is (typep (setf frame (gtk:frame-new)) 'gtk:frame))
    (is (typep (setf label (gtk:label-new)) 'gtk:label))

    (is-false (gtk:widget-set-parent label frame))
    (is (eq frame (gtk:widget-parent label)))
    (is-false (gtk:widget-unparent label))
    (is-false (gtk:frame-child frame) nil)

    (is (= 1 (g:object-ref-count frame)))
    (is (= 1 (g:object-ref-count label)))))

;;;     gtk_widget_get_native

;;;     gtk_widget_get_ancestor
;;;     gtk_widget_is_ancestor

(test gtk-widget-ancestor/is-ancestor
  (let ((box (make-instance 'gtk:box))
        (button (make-instance 'gtk:button)))
    (is-false (gtk:box-append box button))
    (is (eq box (gtk:widget-ancestor button "GtkBox")))
    (is-true (gtk:widget-is-ancestor button box))
    (is-false (gtk:widget-is-ancestor box button))))

;;;     gtk_widget_translate_coordinates

;;;     gtk_widget_add_controller
;;;     gtk_widget_remove_controller

(test gtk-widget-add/remove-controller
  (let ((button (gtk:button-new))
        (controller (gtk:event-controller-key-new)))

    (is-false (gtk:widget-add-controller button controller))
    (is (eq button (gtk:event-controller-widget controller)))
    (is-false (gtk:widget-remove-controller button controller))
    (is-false (gtk:event-controller-widget controller))

    (is (= 1 (g:object-ref-count controller)))
    (is (= 1 (g:object-ref-count button)))))

;;;     gtk_widget_get_direction
;;;     gtk_widget_set_direction
;;;     gtk_widget_get_default_direction
;;;     gtk_widget_set_default_direction

;;;     gtk_widget_create_pango_context

(test gtk-widget-create-pango-context
  (let ((button (make-instance 'gtk:button))
        context)
    (is (typep (setf context
                     (gtk:widget-create-pango-context button)) 'pango:context))
    (is (typep (pango:context-font-map context) 'pango:font-map))
    #-windows
    (is (string= "Ubuntu 14.667px"
                 (pango:font-description-to-string
                     (pango:context-font-description context))))
    #+windows
    (is (string= "Segoe UI 12px"
                 (pango:font-description-to-string
                     (pango:context-font-description context))))
    (is (eq :ltr (pango:context-base-dir context)))))

;;;     gtk_widget_get_pango_context

(test gtk-widget-pango-context
  (let ((button (make-instance 'gtk:button))
        context)
    (is (typep (setf context
                     (gtk:widget-pango-context button)) 'pango:context))
    (is (typep (pango:context-font-map context) 'pango:font-map))
    #-windows
    (is (string= "Ubuntu 14.667px"
                 (pango:font-description-to-string
                     (pango:context-font-description context))))
    #+windows
    (is (string= "Segoe UI 12px"
                 (pango:font-description-to-string
                     (pango:context-font-description context))))
    (is (eq :ltr (pango:context-base-dir context)))))

;;;     gtk_widget_get_font_options
;;;     gtk_widget_set_font_options
;;;     gtk_widget_get_font_map
;;;     gtk_widget_set_font_map

;;;     gtk_widget_create_pango_layout

(test gtk-widget-create-pango-layout.1
  (let ((button (make-instance 'gtk:button))
        layout)
    (is (typep (setf layout
                     (gtk:widget-create-pango-layout button nil)) 'pango:layout))
    (is (string= "" (pango:layout-text layout)))))

(test gtk-widget-create-pango-layout.2
  (let ((button (make-instance 'gtk:button))
        layout)
    (is (typep (setf layout
                     (gtk:widget-create-pango-layout button "text"))
               'pango:layout))
    (is (string= "text" (pango:layout-text layout)))))

;;;     gtk_widget_set_cursor_from_name

;;;     gtk_widget_class_get_accessible_role
;;;     gtk_widget_class_set_accessible_role

(test gtk-widget-class-accessible-role
  (let ((role nil))
    (is (eq :button
            (setf role (gtk:widget-class-accessible-role "GtkButton"))))
    (is (eq :button
            (setf (gtk:widget-class-accessible-role "GtkButton") role)))))

;;;     gtk_widget_child_focus
;;;     gtk_widget_get_child_visible
;;;     gtk_widget_set_child_visible

;;;     gtk_widget_get_settings

(test gtk-widget-settings
  (let ((window (make-instance 'gtk:window))
        (button (gtk:button-new-with-label "label")))
    (is (eq button (setf (gtk:window-child window) button)))
    (is (typep (gtk:widget-settings button) 'gtk:settings))
    (is (eq (gtk:settings-default) (gtk:widget-settings button)))
    (is-false (setf (gtk:window-child window) nil))
    (is-false (gtk:window-destroy window))))

;;;     gtk_widget_get_clipboard
;;;     gtk_widget_get_primary_clipboard
;;;     gtk_widget_get_display
;;;     gtk_widget_get_size_request
;;;     gtk_widget_set_size_request

;;;     gtk_widget_list_mnemonic_labels

(test gtk-widget-list-mnemonic-labels
  (let ((button (gtk:button-new-with-mnemonic "_New")))
    (is (every (lambda (x) (typep x 'gtk:label))
               (gtk:widget-list-mnemonic-labels button)))
    (is-false (setf (gtk:button-child button) nil))))

;;;     gtk_widget_add_mnemonic_label
;;;     gtk_widget_remove_mnemonic_label

(test gtk-widget-add-mnemonic-label
  (let ((button (make-instance 'gtk:button))
        (label (gtk:label-new-with-mnemonic "_New")))
    (is-false (gtk:widget-add-mnemonic-label button label))
    (is (eq label (first (gtk:widget-list-mnemonic-labels button))))
    (is-false (gtk:widget-remove-mnemonic-label button label))
    (is-false (gtk:widget-list-mnemonic-labels button))))

;;;     gtk_widget_mnemonic_activate

(test gtk-widget-mnemonic-activate
  (let ((button (gtk:button-new-with-mnemonic "_New"))
        (msg nil))
    (g:signal-connect button "mnemonic-activate"
        (lambda (widget cycling)
          (declare (ignore widget))
          (is-false cycling)
          (setf msg "in mnemonic-activate")
          gdk:+event-stop+))
    (is-true (gtk:widget-mnemonic-activate button nil))
    (is (string= "in mnemonic-activate" msg))))

;;;     gtk_widget_error_bell

(test gtk-widget-error-bell
  (let ((button (gtk:button-new)))
    (is-false (gtk:widget-error-bell button))))

;;;     gtk_widget_keynav_failed

(test gtk-widget-keynav-failed
  (let ((entry (make-instance 'gtk:entry))
        (msg nil))
    (g:signal-connect entry "keynav-failed"
        (lambda (widget direction)
          (declare (ignore widget))
          (is (eq :up direction))
          (setf msg "in keynav-failed")
          gdk:+event-stop+))
    (is-true (gtk:widget-keynav-failed entry :up))
    (is (string= "in keynav-failed" msg))))

;;;     gtk_widget_trigger_tooltip_query

;;;     gtk_widget_get_allocated_width
;;;     gtk_widget_get_allocated_height
;;;     gtk_widget_get_allocation
;;;     gtk_widget_get_allocated_baseline

;;;     gtk_widget_get_width
;;;     gtk_widget_get_height
;;;     gtk_widget_get_size
;;;     gtk_widget_get_baseline

;;;     gtk_widget_compute_bounds

(test gtk-widget-compute-bounds
  (let ((label (gtk:label-new "text"))
        (rect (gdk:rectangle-new :x 20 :y 10 :width 200 :height 100)))
    (graphene:with-rect (bounds)
      (is (cffi:pointerp (gtk:widget-compute-bounds label label bounds)))
      (is (= 0 (graphene:rect-x bounds)))
      (is (= 0 (graphene:rect-y bounds)))
      (is (= 0 (graphene:rect-width bounds)))
      (is (= 0 (graphene:rect-height bounds)))

      (is-false (gtk:widget-size-allocate label rect -1))
      (is (cffi:pointerp (gtk:widget-compute-bounds label label bounds)))

      (is (=   0.0 (graphene:rect-x bounds)))
      (is (=   0.0 (graphene:rect-y bounds)))
      (is (= 200.0 (graphene:rect-width bounds)))
      (is (= 100.0 (graphene:rect-height bounds))))))

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

(test gtk-widget-state/unset-flags
  (let ((button (gtk:button-new)))
    (is (equal '(:dir-ltr) (gtk:widget-state-flags button)))
    (is (equal '(:link :visited)
               (setf (gtk:widget-state-flags button) '(:link :visited))))
    (is (equal '(:dir-ltr :link :visited)
                (gtk:widget-state-flags button)))
    (is (equal '(:active :checked)
               (setf (gtk:widget-state-flags button t) '(:active :checked))))
    (is (equal '(:active :dir-ltr :checked) (gtk:widget-state-flags button)))
    (is-false (gtk:widget-unset-state-flags button '(:active)))
    (is (equal '(:DIR-LTR :CHECKED) (gtk:widget-state-flags button)))))

;;;     gtk_widget_has_visible_focus

(test gtk-widget-has-visible-focus
  (let ((button (make-instance 'gtk:button)))
    (is-false (gtk:widget-has-visible-focus button))))

;;;     gtk_widget_is_drawable

;;;     gtk_widget_measure

#-windows
(test gtk-widget-measure
  (is (equal '(32 32 -1 -1)
             (multiple-value-list
                 (gtk:widget-measure (gtk:label-new "label") :horizontal -1))))
  (is (equal '(17 17 14 14)
             (multiple-value-list
                 (gtk:widget-measure (gtk:label-new "label") :vertical -1)))))

#+windows
(test gtk-widget-measure
  (is (equal '(25 25 -1 -1)
             (multiple-value-list
                 (gtk:widget-measure (gtk:label-new "label") :horizontal -1))))
  (is (equal '(17 17 13 13)
             (multiple-value-list
                 (gtk:widget-measure (gtk:label-new "label") :vertical -1)))))

;;;     gtk_widget_snapshot
;;;     gtk_widget_snapshot_child

;; TODO: Improve the test, the test does not return a render node

#+nil
(test gtk-widget-snapshot
  (when *first-run-testsuite*
    (glib-test:with-check-memory (window button snapshot :strong 1)
      (setf button (gtk:button-new-with-label "label"))
      (setf window (make-instance 'gtk:window :child button))
      (setf snapshot (gtk:snapshot-new))
      (is-false (gtk:widget-realize button))
      (is-false (gtk:widget-snapshot window snapshot))
      (is-false (gtk:snapshot-to-node snapshot))
      ;; Remove references
      (is-false (setf (gtk:window-child window) nil))
      (is-false (gtk:window-destroy window)))))

;;;     gtk_widget_get_next_sibling
;;;     gtk_widget_get_prev_sibling

;; TODO: Make a better test example

(test gtk-widget-next/prev-sibling
  (let* ((path (glib-sys:sys-path "test/resource/stack.ui"))
         (builder (gtk:builder-new-from-file path))
         (window1 (gtk:builder-object builder "window1"))
         (grid1 (gtk:builder-object builder "grid1"))
         (stack1 (gtk:builder-object builder "stack1")))

    (is-false (gtk:widget-next-sibling window1))
    (is-false (gtk:widget-prev-sibling window1))

    (is-false (gtk:widget-next-sibling grid1))
    (is-false (gtk:widget-prev-sibling grid1))

    (is-false (gtk:widget-next-sibling stack1))
    (is (typep (gtk:widget-prev-sibling stack1) 'gtk:stack-switcher))
))

;;;     gtk_widget_get_first_child
;;;     gtk_widget_get_last_child

(test gtk-widget-first/last-sibling
  (let* ((path (glib-sys:sys-path "test/resource/stack.ui"))
         (builder (gtk:builder-new-from-file path))
         (window1 (gtk:builder-object builder "window1"))
         (grid1 (gtk:builder-object builder "grid1"))
         (stack1 (gtk:builder-object builder "stack1"))
         (image1 (gtk:builder-object builder "image1"))
         (spinner1 (gtk:builder-object builder "spinner1")))
    (is (typep window1 'gtk:window))
    (is (eq window1  (gtk:widget-root window1)))
    (is (eq grid1 (gtk:widget-first-child window1)))
    (is (eq grid1 (gtk:widget-last-child window1)))
    ;; TODO: The first and last children are not the pages, but the child
    ;; widgets for the pages.
    (is (typep stack1 'gtk:stack))
    (is (eq image1 (gtk:widget-first-child stack1)))
    (is (eq spinner1 (gtk:widget-last-child stack1)))))

;;;     gtk_widget_insert_before
;;;     gtk_widget_insert_after

;; TODO: Finish this example

(test gtk-widget-insert-before/after
  (let ((widget (make-instance 'gtk:box)))

    (is-false (gtk:widget-insert-after (gtk:button-new) widget nil))
    (is-false (gtk:widget-next-sibling widget))

    (is-false (gtk:widget-insert-before (gtk:button-new) widget nil))
    (is-false (gtk:widget-prev-sibling widget))
))

;;;     gtk_widget_should_layout

;;;     gtk_widget_get_color

(test gtk-widget-color.1
  (let ((widget (make-instance 'gtk:label)))
    (is (gdk:rgba-equal (gdk:rgba-new :red 1 :green 1 :blue 1 :alpha 1)
                        (gtk:widget-color widget)))))

#-windows
(test gtk-widget-color.2
  (let ((widget (make-instance 'gtk:window)))
    (is (gdk:rgba-equal (gdk:rgba-new :red 0.23921569
                                      :green 0.23921569
                                      :blue 0.23921569
                                      :alpha 1)
                        (gtk:widget-color widget)))))

#+windows
(test gtk-widget-color.2
  (let ((widget (make-instance 'gtk:window)))
    (is (gdk:rgba-equal (gdk:rgba-new :red 0.18039216
                                      :green 0.20392157
                                      :blue 0.21176471
                                      :alpha 1.0)
                        (gtk:widget-color widget)))))

;;;     gtk_widget_add_css_class
;;;     gtk_widget_remove_css_class
;;;     gtk_widget_has_css_class

(test gtk-widget-add/remove/has-css-class
  (let ((button (gtk:button-new)))
    (is (equal '() (gtk:widget-css-classes button)))
    ;; Add a first class
    (is-false (gtk:widget-add-css-class button "circular"))
    (is-true (gtk:widget-has-css-class button "circular"))
    (is (equal '("circular") (gtk:widget-css-classes button)))
    ;; Add a second class
    (is-false (gtk:widget-add-css-class button "text-button"))
    (is-true (gtk:widget-has-css-class button "text-button"))
    (is (equal '("text-button" "circular") (gtk:widget-css-classes button)))
    ;; Remove class "circular"
    (is-false (gtk:widget-remove-css-class button "circular"))
    (is-false (gtk:widget-has-css-class button "circular"))
    (is (equal '("text-button") (gtk:widget-css-classes button)))))

;;;     gtk_widget_class_get_css_name
;;;     gtk_widget_class_set_css_name

(test gtk-widget-class-css-name.1
  (let ((button (gtk:button-new)))
    (is (string= "button" (gtk:widget-css-name button)))
    (is (string= "button" (gtk:widget-class-css-name "GtkButton")))))

(test gtk-widget-class-css-name.2
  (let ((button (make-instance 'gtk:button
                               :css-name "mybutton")))
    (is (string= "mybutton" (gtk:widget-css-name button)))
    (is (string= "button" (gtk:widget-class-css-name "GtkButton")))))

(test gtk-widget-class-css-name.3
  (let ((save (gtk:widget-class-css-name "GtkButton")))
    (unwind-protect
      (progn
        ;; Change the CSS name for GtkButton
        (is (string= "mybutton"
                     (setf (gtk:widget-class-css-name "GtkButton") "mybutton")))
        (is (string= "mybutton" (gtk:widget-class-css-name "GtkButton")))
        (let ((button (gtk:button-new)))
          (is (string= "mybutton" (gtk:widget-css-name button)))
          (is (string= "mybutton" (gtk:widget-class-css-name "GtkButton")))))
      ;; Restore the CSS name for GtkButton
      (is (string= "button"
                   (setf (gtk:widget-class-css-name "GtkButton") save))))))

;;;     gtk_widget_get_style_context

;;;     gtk:widget-add-provider
;;;     gtk:widget-remove-provider

(test gtk-widget-add/remove-provider
  (let ((provider (gtk:css-provider-new))
        (widget (make-instance 'gtk:button))
        (cssid nil))
    (is (stringp (setf cssid (gtk:widget-add-provider widget provider))))
    (is-false (gtk:widget-remove-provider widget cssid))))

;;;     gtk_widget_get_request_mode

(test gtk-widget-request-mode
  (let ((button (gtk:button-new)))
    (is (eq :constant-size (gtk:widget-request-mode button)))))

;;;     gtk_widget_get_preferred_size
;;;     gtk_distribute_natural_allocation

;;;     gtk_widget_compute_expand

(test gtk-widget-compute-expand.1
  (let ((box (gtk:box-new :horizontal))
        (button (make-instance 'gtk:button
                               :label "label"
                               :hexpand t
                               :vexpand nil)))
    (is-false (gtk:box-append box button))
    (is-true (gtk:widget-compute-expand box :horizontal))
    (is-false (gtk:widget-compute-expand box :vertical))))

(test gtk-widget-compute-expand.2
  (let ((box (gtk:box-new :horizontal))
        (button (make-instance 'gtk:button
                               :label "label"
                               :hexpand nil
                               :vexpand t)))
    (is-false (gtk:box-append box button))
    (is-false (gtk:widget-compute-expand box :horizontal))
    (is-true (gtk:widget-compute-expand box :vertical))))

;;;     gtk_widget_init_template
;;;     gtk_widget_dispose_template                         Since 4.8
;;;     gtk_widget_get_template_child

;;;     gtk_widget_class_set_template
;;;     gtk_widget_class_set_template_from_resource
;;;     gtk_widget_class_set_template_scope

;;;     gtk_widget_class_bind_template_child
;;;     gtk_widget_class_bind_template_child_internal
;;;     gtk_widget_class_bind_template_child_private
;;;     gtk_widget_class_bind_template_child_internal_private
;;;     gtk_widget_class_bind_template_child_full
;;;     gtk_widget_class_bind_template_callback
;;;     gtk_widget_class_bind_template_callback_full

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

;;; 2025-12-06
