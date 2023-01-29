;;; ----------------------------------------------------------------------------
;;; cl-cffi-gtk4.asd
;;;
;;; Copyright (C) 2021 - 2023 Dieter Kaiser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
;;; ----------------------------------------------------------------------------

(defsystem :cl-cffi-gtk4
  :name "cl-cffi-gtk4"
  :version "0.9.0"
  :author "Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :components
  ((:module gdk
    :serial t
    :components
    ((:file "gdk4.package")

     ;; Interfaces
     (:file "gdk4.paintable")             ; GdkPaintable

     (:file "gdk4.display-manager")       ; GdkDisplayManager
     (:file "gdk4.display")               ; GdkDisplay
     (:file "gdk4.seat")                  ; GdkSeat
     (:file "gdk4.device")                ; GdkDevice
     (:file "gdk4.device-pad")            ; GdkDevicePad
     (:file "gdk4.monitor")               ; GdkMonitor
     (:file "gdk4.rectangle")             ; GdkRectangle
     (:file "gdk4.texture")               ; GdkTexture
     (:file "gdk4.rgba")                  ; GdkRGBA
     (:file "gdk4.cursor")                ; GdkCursor
     (:file "gdk4.surface")               ; GdkSurface
     (:file "gdk4.toplevel")              ; GdkToplevel
     (:file "gdk4.toplevel-layout")       ; GdkToplevelLayout
     (:file "gdk4.toplevel-size")         ; GdkToplevelSize
     (:file "gdk4.popup")                 ; GdkPopup
     (:file "gdk4.popup-layout")          ; GdkPopupLayout
     (:file "gdk4.frame-clock")           ; GdkFrameClock
     (:file "gdk4.frame-timings")         ; GdkFrameTimings
     (:file "gdk4.draw-context")          ; GdkDrawContext
     (:file "gdk4.gl-context")            ; GdkGLContext
     (:file "gdk4.vulkan-context")        ; GdkVulkanContext
     (:file "gdk4.cairo-context")         ; GdkCairoContext
     (:file "gdk4.event")                 ; GdkEvent
     (:file "gdk4.keyval")                ; Key Values
     (:file "gdk4.clipboard")             ; GdkClipboard
     (:file "gdk4.drag-and-drop")         ; GdkDrag, GdkDrop
     (:file "gdk4.content-formats")       ; GdkContentFormats
     (:file "gdk4.content-provider")      ; GdkContentProvider
     (:file "gdk4.content-serializer")    ; GdkContentSerializer
     (:file "gdk4.content-deserializer")  ; GdkContentDeserializer
     (:file "gdk4.pixbuf-interaction")    ; GdkPixbuf interaction
     (:file "gdk4.cairo-interaction")     ; Cairo interaction
     (:file "gdk4.app-launch-context")    ; GdkAppLaunchContext
     (:file "gdk4.x11-interaction")
     (:file "gdk4.wayland-interaction")
    ))
   (:module gsk
    :serial t
    :components
    ((:file "gsk.package")
     (:file "gsk.renderer")         ; GskRenderer
     (:file "gsk.render-node")
     (:file "gsk.rounded-rect")
     (:file "gsk.transform")        ; GskTransform
     (:file "gsk.gl-shader")
    ))
   (:module gtk
    :serial t
    :components
    ((:file "gtk4.package")

      ;; GListModel support
;      (:file "gtk4.expression")            ; GtkExpression

     (:file "gtk4.filter")                ; GtkFilter
;    GtkCustomFilter — Filtering with callbacks
;    GtkMultiFilter — Combining multiple filters
;    GtkBoolFilter — Filtering by boolean expressions
;    GtkStringFilter — Filtering by strings
     (:file "gtk4.file-filter")           ; GtkFileFilter

     ;; GTK Core
     (:file "gtk4.version")               ; Version Information
     (:file "gtk4.enumerations")          ; Standard Enumerations
     (:file "gtk4.main-loop")             ; Main event loop, and events
     (:file "gtk4.mount-operation")       ; Filesystem utilities

     ;; Theming in GTK
     (:file "gtk4.style-provider")        ; GtkStyleProvider (interface)
     (:file "gtk4.css-provider")          ; GtkCssProvider
     (:file "gtk4.style-context")         ; GtkStyleContext
     (:file "gtk4.icon-paintable")        ; GtkIconPaintable
     (:file "gtk4.icon-theme")            ; GtkIconTheme

     ;; Interfaces
     (:file "gtk4.accessible")            ; GtkAccessible
     (:file "gtk4.actionable")            ; GtkActionable
     (:file "gtk4.cell-editable")         ; GtkCellEditable
     (:file "gtk4.native")                ; GtkNative
     (:file "gtk4.orientable")            ; GtkOrientable
     (:file "gtk4.root")                  ; GtkRoot
     (:file "gtk4.scrollable")            ; GtkScrollable
     (:file "gtk4.shortcut-manager")      ; GtkShortcutManager

     ;; Settings
     (:file "gtk4.settings")              ; Sharing settings between applications

     ;; Interface builder
     (:file "gtk4.buildable")             ; GtkBuildable
     (:file "gtk4.builder")               ; GtkBuilder

     ;; Layout Managers
     (:file "gtk4.layout-manager")        ; GtkLayoutManager
     (:file "gtk4.layout-child")          ; GtkLayoutChild
     (:file "gtk4.bin-layout")            ; GtkBinLayout
     (:file "gtk4.box-layout")            ; GtkBoxLayout
     (:file "gtk4.center-layout")         ; GtkCenterLayout
     (:file "gtk4.fixed-layout")          ; GtkFixedLayout
     (:file "gtk4.grid-layout")           ; GtkGridLayout
     (:file "gtk4.overlay-layout")        ; GtkOverlayLayout
     (:file "gtk4.custom-layout")         ; GtkCustomLayout
     (:file "gtk4.constraint-layout")     ; GtkConstraintLayout

     (:file "gtk4.constraint")            ; GtkConstraint, GtkConstraintTarget
     (:file "gtk4.constraint-guide")      ; GtkConstraintGuide

     ;; Abstract Base Classes
     (:file "gtk4.widget")                ; GtkWidget
     (:file "gtk4.range")                 ; GtkRange

     ;; Layout Containers
     (:file "gtk4.box")                   ; GtkBox
     (:file "gtk4.center-box")            ; GtkCenterBox
     (:file "gtk4.grid")                  ; GtkGrid
     (:file "gtk4.revealer")              ; GtkRevealer
     (:file "gtk4.list-box")              ; GtkListBox
     (:file "gtk4.flow-box")              ; GtkFlowBox
     (:file "gtk4.stack")                 ; GtkStack
     (:file "gtk4.stack-switcher")        ; GtkStackSwitcher
     (:file "gtk4.stack-sidebar")         ; GtkStackSidebar
     (:file "gtk4.action-bar")            ; GtkActionBar
     (:file "gtk4.header-bar")            ; GtkHeaderBar
     (:file "gtk4.overlay")               ; GtkOverlay
     (:file "gtk4.paned")                 ; GtkPaned
     (:file "gtk4.notebook")              ; GtkNotebook
     (:file "gtk4.expander")              ; GtkExpander
     (:file "gtk4.aspect-frame")          ; GtkAspectFrame
     (:file "gtk4.fixed")                 ; GtkFixed

     ;; Display Widgets
     (:file "gtk4.label")                 ; GtkLabel
     (:file "gtk4.image")                 ; GtkImage
     (:file "gtk4.picture")               ; GtkPicture
     (:file "gtk4.spinner")               ; GtkSpinner
     (:file "gtk4.info-bar")              ; GtkInfoBar
     (:file "gtk4.progress-bar")          ; GtkProgressBar
     (:file "gtk4.level-bar")             ; GtkLevelBar
     (:file "gtk4.statusbar")             ; GtkStatusbar
     (:file "gtk4.calendar")              ; GtkCalendar

     ;; Media Support
     (:file "gtk4.video")                 ; GtkVideo
     (:file "gtk4.media-controls")        ; GtkMediaControls
     (:file "gtk4.media-stream")          ; GtkMediaStream
     (:file "gtk4.media-file")            ; GtkMediaFile

     ;; Buttons and Toggles
     (:file "gtk4.button")                ; GtkButton
     (:file "gtk4.check-button")          ; GtkCheckButton
     (:file "gtk4.toggle-button")         ; GtkToggleButton
     (:file "gtk4.link-button")           ; GtkLinkButton
     (:file "gtk4.menu-button")           ; GtkMenuButton
     (:file "gtk4.switch")                ; GtkSwitch
     (:file "gtk4.scale-button")          ; GtkScaleButton
     (:file "gtk4.volume-button")         ; GtkVolumeButton
     (:file "gtk4.lock-button")           ; GtkLockButton

     ;; Multiline Text Editor
     (:file "gtk4.text-iter")             ; GtkTextIter
     (:file "gtk4.text-tag")              ; GtkTextTag
     (:file "gtk4.text-tag-table")        ; GtkTextTagTable
     (:file "gtk4.text-mark")             ; GtkTextMark
     (:file "gtk4.text-buffer")           ; GtkTextBuffer
     (:file "gtk4.text-view")             ; GtkTextView

     ;; Tree, List and Icon Grid Widgets
     (:file "gtk4.tree-model")            ; GtkTreeModel, GtkTreeIter, ...
     (:file "gtk4.tree-selection")        ; GtkTreeSelection
     (:file "gtk4.tree-view-column")      ; GtkTreeViewColumn
     (:file "gtk4.tree-view")             ; GtkTreeView
     (:file "gtk4.tree-view-dnd")         ; GtkTreeView Drag & Drop

     (:file "gtk4.cell-layout")           ; GtkCellLayout
     (:file "gtk4.cell-view")             ; GtkCellView
     (:file "gtk4.icon-view")             ; GtkIconView

     (:file "gtk4.tree-sortable")         ; GtkTreeSortable
     (:file "gtk4.tree-model-sort")       ; GtkTreeModelSort
     (:file "gtk4.tree-model-filter")     ; GtkTreeModelFilter

     (:file "gtk4.cell-renderer")         ; GtkCellRenderer
     (:file "gtk4.cell-renderer-text")    ; GtkCellRendererText
     (:file "gtk4.cell-renderer-accel")   ; GtkCellRendererAccel
     (:file "gtk4.cell-renderer-combo")   ; GtkCellRendererCombo
     (:file "gtk4.cell-renderer-pixbuf")  ; GtkCellRendererPixbuf
     (:file "gtk4.cell-renderer-progress"); GtkCellRendererProgress
     (:file "gtk4.cell-renderer-spin")    ; GtkCellRendererSpin
     (:file "gtk4.cell-renderer-toggle")  ; GtkCellRendererToggle
     (:file "gtk4.cell-renderer-spinner") ; GtkCellRendererSpinner

     (:file "gtk4.cell-area")             ; GtkCellArea
     (:file "gtk4.cell-area-box")         ; GtkCellAreaBox
     (:file "gtk4.cell-area-context")     ; GtkCellAreaCenter

     (:file "gtk4.list-store")            ; GtkListStore
     (:file "gtk4.tree-store")            ; GtkTreeStore

     ;; Numeric and Text Data Entry
     (:file "gtk4.editable")              ; GtkEditable
     (:file "gtk4.entry-buffer")          ; GtkEntryBuffer
     (:file "gtk4.text")                  ; GtkText
     (:file "gtk4.entry")                 ; GtkEntry
     (:file "gtk4.entry-completion")      ; GtkEntryCompletion
     (:file "gtk4.password-entry")        ; GtkPasswordEntry
     (:file "gtk4.scale")                 ; GtkScale
     (:file "gtk4.spin-button")           ; GtkSpinButton
     (:file "gtk4.search-entry")          ; GtkSearchEntry
     (:file "gtk4.search-bar")            ; GtkSearchBar
     (:file "gtk4.editable-label")        ; GtkEditableLabel

     ;; Combo Box, Popover
     (:file "gtk4.combo-box")             ; GtkComboBox
     (:file "gtk4.combo-box-text")        ; GtkComboBoxText
     (:file "gtk4.popover")               ; GtkPopover
     (:file "gtk4.popover-menu")          ; GtkPopoverMenu
     (:file "gtk4.popover-menu-bar")      ; GtkPopoverMenuBar
;     (:file "gtk4.drop-down")              GtkDropDown

     ;; Selector Widgets and Dialogs
     (:file "gtk4.color-chooser")         ; GtkColorChooser
     (:file "gtk4.color-button")          ; GtkColorButton
     (:file "gtk4.color-chooser-widget")  ; GtkColorChooserWidget
     (:file "gtk4.color-chooser-dialog")  ; GtkColorChooserDialog
     (:file "gtk4.file-chooser")          ; GtkFileChooser
     (:file "gtk4.file-chooser-native")   ; GtkFileChooserNative
     (:file "gtk4.file-chooser-dialog")   ; GtkFileChooserDialog
     (:file "gtk4.file-chooser-widget")   ; GtkFileChooserWidget
     (:file "gtk4.font-chooser")          ; GtkFontChooser
     (:file "gtk4.font-button")           ; GtkFontButton
     (:file "gtk4.font-chooser-widget")   ; GtkFontChooserWidget
     (:file "gtk4.font-chooser-dialog")   ; GtkFontChooserDialog
     (:file "gtk4.emoji-chooser")         ; GtkEmojiChooser

     ;; Widgets for custom drawing
     (:file "gtk4.drawing-area")          ; GtkDrawingArea
     (:file "gtk4.gl-area")               ; GtkGlArea

     ;; Ornaments
     (:file "gtk4.frame")                 ; GtkFrame
     (:file "gtk4.separator")             ; GtkSeparator

     ;; Scrolling
     (:file "gtk4.scrollbar")             ; GtkScrollbar
     (:file "gtk4.scrolled-window")       ; GtkScrolledWindow
     (:file "gtk4.viewport")              ; GtkViewport

     ;; Windows
     (:file "gtk4.window")                ; GtkWindow
     (:file "gtk4.dialog")                ; GtkDialog
     (:file "gtk4.message-dialog")        ; GtkMessageDialog
     (:file "gtk4.about-dialog")          ; GtkAboutDialog
     (:file "gtk4.assistant")             ; GtkAssistant
     (:file "gtk4.window-group")          ; GtkWindowGroup
     (:file "gtk4.native-dialog")         ; GtkNativeDialog

     ;; Printing
     (:file "gtk4.print-operation")          ; GtkPrintOperation
     (:file "gtk4.print-context")            ; GtkPrintContext
     (:file "gtk4.paper-size")               ; GtkPaperSize
     (:file "gtk4.print-settings")           ; GtkPrintSettings
     (:file "gtk4.page-setup")               ; GtkPageSetup
     (:file "gtk4.page-setup-unix-dialog")   ; GtkPageSetupUnixDialog
     (:file "gtk4.print-unix-dialog")        ; GtkPrintUnixDialog
     (:file "gtk4.print-job")                ; GtkPrintJob
     (:file "gtk4.printer")                  ; GtkPrinter

     ;; Shortcuts Widgets
     (:file "gtk4.shortcuts-window")      ; GtkShortcutsWindow
     (:file "gtk4.shortcuts-section")     ; GtkShortcutsSection
     (:file "gtk4.shortcuts-group")       ; GtkShortcutsGroup
     (:file "gtk4.shortcuts-shortcut")    ; GtkShortcutsShortcut
     (:file "gtk4.shortcut-label")        ; GtkShortcutLabel

     ;; Accessibility
     (:file "gtk4.at-context")            ; GtkATContext

     ;; Input Methods
     (:file "gtk4.im-context")            ; GtkIMContext
     (:file "gtk4.im-context-simple")     ; GtkIMContextSimple
     (:file "gtk4.im-multicontext")       ; GtkIMMulticontext

     ;; Recently Used Documents
     (:file "gtk4.recent-manager")        ; Managing recently used files

     ;; Choosing from installed applications
     (:file "gtk4.app-chooser")             ; GtkAppChooser
     (:file "gtk4.app-chooser-button")      ; GtkAppChooserButton
     (:file "gtk4.app-chooser-dialog")      ; GtkAppChooserDialog
     (:file "gtk4.app-chooser-widget")      ; GtkAppChooserWidget

     ;; Gestures and event handling
     (:file "gtk4.event-controller")        ; GtkEventController
     (:file "gtk4.event-controller-key")    ; GtkEventControllerKey
     (:file "gtk4.event-controller-focus")  ; GtkEventControllerFocus
     (:file "gtk4.event-controller-legacy") ; GtkEventControllerLegacy
     (:file "gtk4.event-controller-scroll") ; GtkEventControllerScroll
     (:file "gtk4.event-controller-motion") ; GtkEventControllerMotion

     (:file "gtk4.gesture")                 ; GtkGesture
     (:file "gtk4.gesture-single")          ; GtkGestureSingle
     (:file "gtk4.gesture-drag")            ; GtkGestureDrag
     (:file "gtk4.gesture-long-press")      ; GtkGestureLongPress
     (:file "gtk4.gesture-click")           ; GtkGestureClick
     (:file "gtk4.gesture-pan")             ; GtkGesturePan
     (:file "gtk4.gesture-swipe")           ; GtkGestureSwipe
     (:file "gtk4.gesture-rotate")          ; GtkGestureRotate
     (:file "gtk4.gesture-zoom")            ; GtkGestureZoom
     (:file "gtk4.gesture-stylus")          ; GtkGestureStylus

     (:file "gtk4.pad-controller")          ; GtkPadController
     (:file "gtk4.shortcut-controller")     ; GtkShortcutController

     ;; Keyboard shortcuts
     (:file "gtk4.keyboard-accelerators")   ; Utilities for accelerators
     (:file "gtk4.shortcut")                ; GtkShortcut
     (:file "gtk4.shortcut-trigger")        ; GtkShortcutTrigger
     (:file "gtk4.shortcut-action")         ; GtkShortcutAction

     ;; Data exchange, clipboards and Drag-and-Drop
     (:file "gtk4.drag-source")             ; GtkDragSource
     (:file "gtk4.drag-icon")               ; GtkDragIcon
     (:file "gtk4.drop-target")             ; GtkDropTarget
     (:file "gtk4.drop-target-async")       ; GtkDropTargetAsync
     (:file "gtk4.drop-controller-motion")  ; GtkDropControllerMotion

     ;; Miscellaneous
     (:file "gtk4.adjustment")              ; GtkAdjustment
     (:file "gtk4.size-group")              ; GtkSizeGroup
     (:file "gtk4.snapshot")                ; GtkSnapshot
     (:file "gtk4.tooltip")                 ; GtkTooltip
     (:file "gtk4.widget-paintable")        ; GtkWidgetPaintable
     (:file "gtk4.window-controls")         ; GtkWindowControls
     (:file "gtk4.window-handle")           ; GtkWindowHandle

     ;; Application support
     (:file "gtk4.application")             ; GtkApplication
     (:file "gtk4.application-window")      ; GtkApplicationWindow

     ;; Miscellaneous
     (:file "gtk4.init")                    ; More initialization
  )))
  :in-order-to ((asdf:test-op (test-op "cl-cffi-gtk4/test")))
  :depends-on (:cl-cffi-gtk4-init
               :cl-cffi-glib
               :cl-cffi-gdk-pixbuf
               :cl-cffi-cairo
               :cl-cffi-pango
               :cl-cffi-graphene
               :cffi
               :iterate
               :trivial-features
               :split-sequence))

;; Definine a test operation for the library

(defsystem :cl-cffi-gtk4/test
  :name "cl-cffi-gtk4/test"
  :depends-on (:cl-cffi-gtk4 :fiveam)
  :perform (test-op (o c)
               (uiop:symbol-call :fiveam :run!
                                 (uiop:find-symbol* :gtk-test
                                                    :gtk-test)))
  :components
  ((:module test
    :serial nil
    :components
    (;; GTK tests
     (:file "rtest-gtk4")

      ;; GListModel support
;      (:file "gtk4.expression")             ; GtkExpression

     (:file "rtest-gtk4-filter")             ; GtkFilter
;    GtkCustomFilter — Filtering with callbacks
;    GtkMultiFilter — Combining multiple filters
;    GtkBoolFilter — Filtering by boolean expressions
;    GtkStringFilter — Filtering by strings
     (:file "rtest-gtk4-file-filter")        ; GtkFileFilter

     ;; GTK Core
     (:file "rtest-gtk4-version")            ; Version Information
     (:file "rtest-gtk4-enumerations")       ; Standard Enumerations
     (:file "rtest-gtk4-main-loop")          ; Main event loop, and events
;     (:file "gtk4.mount-operation")         ; Filesystem utilities

     ;; Theming in GTK
     (:file "rtest-gtk4-style-provider")     ; GtkStyleProvider (interface)
     (:file "rtest-gtk4-css-provider")       ; GtkCssProvider
     (:file "rtest-gtk4-style-context")      ; GtkStyleContext
     (:file "rtest-gtk4-icon-paintable")     ; GtkIconPaintable
     (:file "rtest-gtk4-icon-theme")         ; GtkIconTheme

     ;; Interfaces
     (:file "rtest-gtk4-accessible")         ; GtkAccessible
     (:file "rtest-gtk4-actionable")         ; GtkActionable
     (:file "rtest-gtk4-cell-editable")      ; GtkCellEditable
     (:file "rtest-gtk4-native")             ; GtkNative
     (:file "rtest-gtk4-orientable")         ; GtkOrientable
     (:file "rtest-gtk4-root")               ; GtkRoot
     (:file "rtest-gtk4-scrollable")         ; GtkScrollable
     (:file "rtest-gtk4-shortcut-manager")   ; GtkShortcutManager

     ;; Settings
     (:file "rtest-gtk4-settings")           ; GtkSettings

     ;; Interface builder
     (:file "rtest-gtk4-buildable")          ; GtkBuildable
     (:file "rtest-gtk4-builder")            ; GtkBuilder

     ;; Layout Managers
;    (:file "gtk4.layout-manager")           ; GtkLayoutManager
;    (:file "gtk4.layout-child")             ; GtkLayoutChild
;    (:file "gtk4.bin-layout")               ; GtkBinLayout
;    (:file "gtk4.box-layout")               ; GtkBoxLayout
;    (:file "gtk4.center-layout")            ; GtkCenterLayout
;    (:file "gtk4.fixed-layout")             ; GtkFixedLayout
;    (:file "gtk4.grid-layout")              ; GtkGridLayout
;    (:file "gtk4.overlay-layout")           ; GtkOverlayLayout
;    (:file "gtk4.custom-layout")            ; GtkCustomLayout
;    (:file "gtk4.constraint-layout")        ; GtkConstraintLayout

;    (:file "gtk4.constraint")               ; GtkConstraint, GtkConstraintTarget
;    (:file "gtk4.constraint-guide")         ; GtkConstraintGuide

     ;; Abstract Base Classes
     (:file "rtest-gtk4-widget")             ; GtkWidget
     (:file "rtest-gtk4-range")              ; GtkRange

     ;; Layout Containers
     (:file "rtest-gtk4-box")                ; GtkBox
     (:file "rtest-gtk4-center-box")         ; GtkCenterBox
     (:file "rtest-gtk4-grid")               ; GtkGrid
     (:file "rtest-gtk4-revealer")           ; GtkRevealer
;    (:file "gtk4.list-box")                 ; GtkListBox
;    (:file "gtk4.flow-box")                 ; GtkFlowBox
;    (:file "gtk4.stack")                    ; GtkStack
;    (:file "gtk4.stack-switcher")           ; GtkStackSwitcher
;    (:file "gtk4.stack-sidebar")            ; GtkStackSidebar
;    (:file "gtk4.action-bar")               ; GtkActionBar
;    (:file "gtk4.header-bar")               ; GtkHeaderBar
;    (:file "gtk4.overlay")                  ; GtkOverlay
     (:file "rtest-gtk4-paned")              ; GtkPaned
;    (:file "gtk4.notebook")                 ; GtkNotebook
;    (:file "gtk4.expander")                 ; GtkExpander
;    (:file "gtk4.aspect-frame")             ; GtkAspectFrame
     (:file "rtest-gtk4-fixed")              ; GtkFixed

     ;; Display Widgets
     (:file "rtest-gtk4-label")              ; GtkLabel
     (:file "rtest-gtk4-image")              ; GtkImage
     (:file "rtest-gtk4-picture")            ; GtkPicture
;    (:file "gtk4.spinner")                  ; GtkSpinner
;    (:file "gtk4.info-bar")                 ; GtkInfoBar
     (:file "rtest-gtk4-progress-bar")       ; GtkProgressBar
;    (:file "gtk4.level-bar")                ; GtkLevelBar
;    (:file "gtk4.statusbar")                ; GtkStatusbar
;    (:file "gtk4.calendar")                 ; GtkCalendar

     ;; Media Support
;    (:file "gtk4.video")                    ; GtkVideo
;    (:file "gtk4.media-controls")           ; GtkMediaControls
;    (:file "gtk4.media-stream")             ; GtkMediaStream
;    (:file "gtk4.media-file")               ; GtkMediaFile

     ;; Buttons and Toggles
     (:file "rtest-gtk4-button")             ; GtkButton
;    (:file "gtk4.check-button")             ; GtkCheckButton
     (:file "rtest-gtk4-toggle-button")      ; GtkToggleButton
     (:file "rtest-gtk4-link-button")        ; GtkLinkButton
;    (:file "gtk4.menu-button")              ; GtkMenuButton
;    (:file "gtk4.switch")                   ; GtkSwitch
     (:file "rtest-gtk4-scale-button")       ; GtkScaleButton
     (:file "rtest-gtk4-volume-button")      ; GtkVolumeButton
     (:file "rtest-gtk4-lock-button")        ; GtkLockButton

     ;; Multiline Text Editor
;    (:file "gtk4.text-iter")             ; GtkTextIter
;    (:file "gtk4.text-tag")              ; GtkTextTag
;    (:file "gtk4.text-tag-table")        ; GtkTextTagTable
;    (:file "gtk4.text-mark")             ; GtkTextMark
;    (:file "gtk4.text-buffer")           ; GtkTextBuffer
;    (:file "gtk4.text-view")             ; GtkTextView

     ;; Tree, List and Icon Grid Widgets
     (:file "rtest-gtk4-tree-model")      ; GtkTreeModel, GtkTreeIter, GtkTreePath
;    (:file "gtk4.tree-selection")        ; GtkTreeSelection
;    (:file "gtk4.tree-view-column")      ; GtkTreeViewColumn
;    (:file "gtk4.tree-view")             ; GtkTreeView
;    (:file "gtk4.tree-view-dnd")         ; GtkTreeView Drag & Drop

;    (:file "gtk4.cell-layout")           ; GtkCellLayout
;    (:file "gtk4.cell-view")             ; GtkCellView
;    (:file "gtk4.icon-view")             ; GtkIconView

;    (:file "gtk4.tree-sortable")         ; GtkTreeSortable
;    (:file "gtk4.tree-model-sort")       ; GtkTreeModelSort
;    (:file "gtk4.tree-model-filter")     ; GtkTreeModelFilter

;    (:file "gtk4.cell-renderer")         ; GtkCellRenderer
;    (:file "gtk4.cell-renderer-text")    ; GtkCellRendererText
;    (:file "gtk4.cell-renderer-accel")   ; GtkCellRendererAccel
;    (:file "gtk4.cell-renderer-combo")   ; GtkCellRendererCombo
;    (:file "gtk4.cell-renderer-pixbuf")  ; GtkCellRendererPixbuf
;    (:file "gtk4.cell-renderer-progress"); GtkCellRendererProgress
;    (:file "gtk4.cell-renderer-spin")    ; GtkCellRendererSpin
;    (:file "gtk4.cell-renderer-toggle")  ; GtkCellRendererToggle
;    (:file "gtk4.cell-renderer-spinner") ; GtkCellRendererSpinner

;    (:file "gtk4.cell-area")             ; GtkCellArea
;    (:file "gtk4.cell-area-box")         ; GtkCellAreaBox
;    (:file "gtk4.cell-area-context")     ; GtkCellAreaCenter

;    (:file "gtk4.list-store")            ; GtkListStore
;    (:file "gtk4.tree-store")            ; GtkTreeStore

     ;; Numeric and Text Data Entry
     (:file "rtest-gtk4-editable")                ; GtkEditable
     (:file "rtest-gtk4-entry-buffer")            ; GtkEntryBuffer
     (:file "rtest-gtk4-text")                    ; GtkText
     (:file "rtest-gtk4-entry")                   ; GtkEntry
     (:file "rtest-gtk4-entry-completion")        ; GtkEntryCompletion
;    (:file "gtk4.password-entry")                ; GtkPasswordEntry
;    (:file "gtk4.scale")                         ; GtkScale
;    (:file "gtk4.spin-button")                   ; GtkSpinButton
;    (:file "gtk4.search-entry")                  ; GtkSearchEntry
;    (:file "gtk4.search-bar")                    ; GtkSearchBar
;    (:file "gtk4.editable-label")                ; GtkEditableLabel

     ;; Combo Box, Popover
;    (:file "gtk4.combo-box")                 ; GtkComboBox
;    (:file "gtk4.combo-box-text")            ; GtkComboBoxText
     (:file "rtest-gtk4-popover")             ; GtkPopover
     (:file "rtest-gtk4-popover-menu")        ; GtkPopoverMenu
;    (:file "gtk4.popover-menu-bar")          ; GtkPopoverMenuBar
;    (:file "gtk4.drop-down")                 ; GtkDropDown

     ;; Selector Widgets and Dialogs
;     (:file "rtest-gtk4-color-chooser")           ; GtkColorChooser
;     (:file "rtest-gtk4-color-button")            ; GtkColorButton
;     (:file "rtest-gtk4-color-chooser-widget")    ; GtkColorChooserWidget
;     (:file "rtest-gtk4-color-chooser-dialog")    ; GtkColorChooserDialog

     (:file "rtest-gtk4-file-chooser")            ; GtkFileChooser
;    (:file "gtk4.file-chooser-native")           ; GtkFileChooserNative
     (:file "rtest-gtk4-file-chooser-dialog")     ; GtkFileChooserDialog
     (:file "rtest-gtk4-file-chooser-widget")     ; GtkFileChooserWidget
;    (:file "gtk4.font-chooser")                  ; GtkFontChooser
;    (:file "gtk4.font-button")                   ; GtkFontButton
;    (:file "gtk4.font-chooser-widget")           ; GtkFontChooserWidget
     (:file "rtest-gtk4-font-chooser-dialog")     ; GtkFontChooserDialog
;    (:file "gtk4.emoji-chooser")                 ; GtkEmojiChooser

     ;; Widgets for custom drawing
;    (:file "gtk4.drawing-area")              ; GtkDrawingArea
;    (:file "gtk4.gl-area")                   ; GtkGlArea

     ;; Ornaments
     (:file "rtest-gtk4-frame")               ; GtkFrame
     (:file "rtest-gtk4-separator")           ; GtkSeparator

     ;; Scrolling
;    (:file "gtk4.scrollbar")                 ; GtkScrollbar
;    (:file "gtk4.scrolled-window")           ; GtkScrolledWindow
;    (:file "gtk4.viewport")                  ; GtkViewport

     ;; Windows
     (:file "rtest-gtk4-window")              ; GtkWindow
     (:file "rtest-gtk4-dialog")              ; GtkDialog
     (:file "rtest-gtk4-message-dialog")      ; GtkMessageDialog
     (:file "rtest-gtk4-about-dialog")        ; GtkAboutDialog
     (:file "rtest-gtk4-assistant")           ; GtkAssistant
;    (:file "gtk4.window-group")              ; GtkWindowGroup
;    (:file "gtk4.native-dialog")             ; GtkNativeDialog

     ;; Printing
     (:file "rtest-gtk4-print-operation")     ; GtkPrintOperation
;    (:file "gtk4.print-context")             ; GtkPrintContext
;    (:file "gtk4.paper-size")                ; GtkPaperSize
;    (:file "gtk4.print-settings")            ; GtkPrintSettings
;    (:file "gtk4.page-setup")                ; GtkPageSetup
     #-win32
     (:file "rtest-gtk4-print-unix-dialog")   ; GtkPrintUnixDialog
;    (:file "gtk4.printer")                   ; GtkPrinter

     ;; Shortcuts Widgets
     (:file "rtest-gtk4-shortcuts-window")    ; GtkShortcutsWindow
;    (:file "gtk4.shortcuts-section")         ; GtkShortcutsSection
;    (:file "gtk4.shortcuts-group")           ; GtkShortcutsGroup
;    (:file "gtk4.shortcuts-shortcut")        ; GtkShortcutsShortcut
;    (:file "gtk4.shortcut-label")            ; GtkShortcutLabel

     ;; Accessibility
;    (:file "gtk4.at-context")                ; GtkATContext

     ;; Input Methods
;    (:file "gtk4.im-context")                ; GtkIMContext
;    (:file "gtk4.im-context-simple")         ; GtkIMContextSimple
;    (:file "gtk4.im-multicontext")           ; GtkIMMulticontext

     ;; Recently Used Documents
     (:file "rtest-gtk4-recent-manager")      ; Managing recently used files

     ;; Choosing from installed applications
;    (:file "gtk4.app-chooser")              ; GtkAppChooser
;    (:file "gtk4.app-chooser-button")       ; GtkAppChooserButton
     (:file "rtest-gtk4-app-chooser-dialog") ; GtkAppChooserDialog
;    (:file "gtk4.app-chooser-widget")       ; GtkAppChooserWidget

     ;; Gestures and event handling
     (:file "rtest-gtk4-event-controller")        ; GtkEventController
     (:file "rtest-gtk4-event-controller-key")    ; GtkEventControllerKey
     (:file "rtest-gtk4-event-controller-focus")  ; GtkEventControllerFocus
     (:file "rtest-gtk4-event-controller-legacy") ; GtkEventControllerLegacy
     (:file "rtest-gtk4-event-controller-scroll") ; GtkEventControllerScroll
     (:file "rtest-gtk4-event-controller-motion") ; GtkEventControllerMotion

     (:file "rtest-gtk4-gesture")                 ; GtkGesture
     (:file "rtest-gtk4-gesture-single")          ; GtkGestureSingle
     (:file "rtest-gtk4-gesture-drag")            ; GtkGestureDrag
     (:file "rtest-gtk4-gesture-long-press")      ; GtkGestureLongPress
     (:file "rtest-gtk4-gesture-click")           ; GtkGestureClick
     (:file "rtest-gtk4-gesture-pan")             ; GtkGesturePan
     (:file "rtest-gtk4-gesture-swipe")           ; GtkGestureSwipe
     (:file "rtest-gtk4-gesture-rotate")          ; GtkGestureRotate
     (:file "rtest-gtk4-gesture-zoom")            ; GtkGestureZoom
     (:file "rtest-gtk4-gesture-stylus")          ; GtkGestureStylus

     (:file "rtest-gtk4-pad-controller")          ; GtkPadController
     (:file "rtest-gtk4-shortcut-controller")     ; GtkShortcutController

     ;; Keyboard shortcuts
     (:file "rtest-gtk4-keyboard-accelerators")   ; Utilities for accelerators
;    (:file "gtk4.shortcut")                      ; GtkShortcut
;    (:file "gtk4.shortcut-trigger")              ; GtkShortcutTrigger
;    (:file "gtk4.shortcut-action")               ; GtkShortcutAction

     ;; Data exchange, clipboards and Drag-and-Drop
     (:file "rtest-gtk4-drag-source")             ; GtkDragSource
;    (:file "gtk4.drag-icon")                     ; GtkDragIcon
;    (:file "gtk4.drop-target")                   ; GtkDropTarget
;    (:file "gtk4.drop-target-async")             ; GtkDropTargetAsync
;    (:file "gtk4.drop-controller-motion")        ; GtkDropControllerMotion

     ;; Miscellaneous
     (:file "rtest-gtk4-adjustment")        ; GtkAdjustment
;    (:file "gtk4.size-group")              ; GtkSizeGroup
;    (:file "gtk4.snapshot")                ; GtkSnapshot
;    (:file "gtk4.tooltip")                 ; GtkTooltip
;    (:file "gtk4.widget-paintable")        ; GtkWidgetPaintable
;    (:file "gtk4.window-controls")         ; GtkWindowControls
;    (:file "gtk4.window-handle")           ; GtkWindowHandle

     ;; Application support
     (:file "rtest-gtk4-application")        ; GtkApplication
     (:file "rtest-gtk4-application-window") ; GtkApplicationWindow

     ;; GDK
     (:file "rtest-gdk4-rgba")
     (:file "rtest-gdk4-event")

     ;; GSK
))))

;;; --- cl-cffi-gtk4.asd -------------------------------------------------------
