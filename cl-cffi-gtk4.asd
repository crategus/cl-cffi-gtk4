;;; ----------------------------------------------------------------------------
;;; cl-cffi-gtk4.asd
;;;
;;; Copyright (C) 2021 - 2023 Dieter Kaiser
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;; ----------------------------------------------------------------------------

(defsystem :cl-cffi-gtk4
  :name "cl-cffi-gtk4"
  :version "0.3.0"
  :author "Dieter Kaiser"
  :license "MIT"
  :serial t
  :components
  ((:module gdk
    :serial t
    :components
    ((:file "gdk4.package")

     ;; General
     (:file "gdk4.enumerations")
     (:file "gdk4.rectangle")
     (:file "gdk4.rgba")
     (:file "gdk4.keyval")

     ;; Display, Seat, Device, Monitor
     (:file "gdk4.display-manager")
     (:file "gdk4.display")
     (:file "gdk4.device")
     (:file "gdk4.device-pad")
     (:file "gdk4.monitor")
     (:file "gdk4.seat")

     ;; Paintables, Textures
     (:file "gdk4.paintable")
     (:file "gdk4.texture")

     ;; Surfaces, Toplevels, Popups
     (:file "gdk4.surface")
     (:file "gdk4.toplevel-layout")
     (:file "gdk4.toplevel")
     (:file "gdk4.toplevel-size")
     (:file "gdk4.popup")
     (:file "gdk4.popup-layout")

     ;; Draw Contexts
     (:file "gdk4.draw-context")
     (:file "gdk4.gl-context")
     (:file "gdk4.vulkan-context")
     (:file "gdk4.cairo-context")

     ;; Clipboard, Drag and Drop
     (:file "gdk4.content-formats")
     (:file "gdk4.content-provider")
     (:file "gdk4.content-serializer")
     (:file "gdk4.content-deserializer")
     (:file "gdk4.clipboard")
     (:file "gdk4.drag")
     (:file "gdk4.drop")
     (:file "gdk4.drag-surface")

     ;; Applicationn launching
     (:file "gdk4.app-launch-context")

     ;; Miscellaneous
     (:file "gdk4.event")
     (:file "gdk4.cursor")
     (:file "gdk4.frame-timings")
     (:file "gdk4.frame-clock")

     ;; Pixbuf, Pango, Cairo, Backends interaction
     (:file "gdk4.pixbuf-interaction")
     (:file "gdk4.pango-interaction")
     (:file "gdk4.cairo-interaction")
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
     (:file "gtk4.toggle-button")         ; GtkToggleButton
     (:file "gtk4.check-button")          ; GtkCheckButton
     (:file "gtk4.menu-button")           ; GtkMenuButton
     (:file "gtk4.link-button")           ; GtkLinkButton
     (:file "gtk4.lock-button")           ; GtkLockButton
     (:file "gtk4.scale-button")          ; GtkScaleButton
     (:file "gtk4.volume-button")         ; GtkVolumeButton
     (:file "gtk4.switch")                ; GtkSwitch

     ;; Multiline Text Editor
     (:file "gtk4.text-iter")             ; Text buffer iterator
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

     (:file "gtk4.cell-area")
     (:file "gtk4.cell-area-box")
     (:file "gtk4.cell-area-context")

     (:file "gtk4.list-store")            ; GtkListStore
     (:file "gtk4.tree-store")            ; GtkTreeStore

     ;; Numeric and Text Data Entry
     (:file "gtk4.editable")              ; GtkEditable
     (:file "gtk4.entry-buffer")          ; Text buffer for GtkEntry
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
     (:file "gtk4.color-dialog" :if-feature :gtk-4-10)
     (:file "gtk4.color-dialog-button" :if-feature :gtk-4-10)

     (:file "gtk4.file-chooser")
     (:file "gtk4.file-chooser-native")
     (:file "gtk4.file-chooser-dialog")
     (:file "gtk4.file-chooser-widget")
     (:file "gtk4.file-dialog" :if-feature :gtk-4-10)

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
     (:file "gtk4.paper-size")               ; Support for named paper sizes
     (:file "gtk4.print-settings")           ; GtkPrintSettings
     (:file "gtk4.page-setup")               ; GtkPageSetup

     (:file "gtk4.page-setup-unix-dialog"  :if-feature (:not :windows))
     (:file "gtk4.print-unix-dialog"       :if-feature (:not :windows))
     (:file "gtk4.print-job"               :if-feature (:not :windows))
     (:file "gtk4.printer"                 :if-feature (:not :windows))

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
  :defsystem-depends-on (:cl-cffi-gtk4-init)
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
    ((:file "rtest-gtk4")

     ;; GDK General
     (:file "rtest-gdk4-enumerations")
     (:file "rtest-gdk4-rectangle")
     (:file "rtest-gdk4-rgba")
     (:file "rtest-gdk4-keyval")

     ;; Displays, Devices, Monitors, Seats
     (:file "rtest-gdk4-display-manager")
     (:file "rtest-gdk4-display")
     (:file "rtest-gdk4-device")
     (:file "rtest-gdk4-device-pad")
     (:file "rtest-gdk4-monitor")
     (:file "rtest-gdk4-seat")

     ;; Paintables
     (:file "rtest-gdk4-paintable")
     (:file "rtest-gdk4-texture")

     ;; Surfaces, Toplevels, Popups
     (:file "rtest-gdk4-toplevel")
     (:file "rtest-gdk4-popup")

     ;; Daw contexts
     (:file "rtest-gdk4-cairo-context")

     ;; Clipboard, Drag and Drop
     (:file "rtest-gdk4-clipboard")
     (:file "rtest-gdk4-drag")
     (:file "rtest-gdk4-drop")
     (:file "rtest-gdk4-drag-surface")

     ;; Application launching
     (:file "rtest-gdk4-app-launch-context")

     ;; Miscellaneous
     (:file "rtest-gdk4-event")

     ;; Pixbuf, Pango, Cairo, Backends interaction

     ;; GSK

      ;; GListModel support
;      (:file "gtk4.expression")             ; GtkExpression

     (:file "rtest-gtk4-filter")
;    GtkCustomFilter — Filtering with callbacks
;    GtkMultiFilter — Combining multiple filters
;    GtkBoolFilter — Filtering by boolean expressions
;    GtkStringFilter — Filtering by strings
     (:file "rtest-gtk4-file-filter")

     ;; GTK Core
     (:file "rtest-gtk4-version")
     (:file "rtest-gtk4-enumerations")
     (:file "rtest-gtk4-main-loop")
;     (:file "gtk4.mount-operation")         ; Filesystem utilities

     ;; Theming in GTK
     (:file "rtest-gtk4-style-provider")
     (:file "rtest-gtk4-css-provider")
     (:file "rtest-gtk4-style-context")
     (:file "rtest-gtk4-icon-paintable")
     (:file "rtest-gtk4-icon-theme")

     ;; Interfaces
     (:file "rtest-gtk4-accessible")
     (:file "rtest-gtk4-actionable")
     (:file "rtest-gtk4-cell-editable")
     (:file "rtest-gtk4-native")
     (:file "rtest-gtk4-orientable")
     (:file "rtest-gtk4-root")
     (:file "rtest-gtk4-scrollable")
     (:file "rtest-gtk4-shortcut-manager")

     ;; Settings
     (:file "rtest-gtk4-settings")

     ;; Interface builder
     (:file "rtest-gtk4-buildable")
     (:file "rtest-gtk4-builder")

     ;; Layout Managers
;    (:file "gtk4.layout-manager")           ; GtkLayoutManager
;    (:file "gtk4.layout-child")             ; GtkLayoutChild
;    (:file "gtk4.bin-layout")               ; GtkBinLayout
;    (:file "gtk4.box-layout")               ; GtkBoxLayout
;    (:file "gtk4.center-layout")            ; GtkCenterLayout
     (:file "rtest-gtk4-fixed-layout")
;    (:file "gtk4.grid-layout")              ; GtkGridLayout
;    (:file "gtk4.overlay-layout")           ; GtkOverlayLayout
;    (:file "gtk4.custom-layout")            ; GtkCustomLayout
;    (:file "gtk4.constraint-layout")        ; GtkConstraintLayout

;    (:file "gtk4.constraint")               ; GtkConstraint, GtkConstraintTarget
;    (:file "gtk4.constraint-guide")         ; GtkConstraintGuide

     ;; Abstract Base Classes
     (:file "rtest-gtk4-widget")
     (:file "rtest-gtk4-range")

     ;; Layout Containers
     (:file "rtest-gtk4-box")
     (:file "rtest-gtk4-center-box")
     (:file "rtest-gtk4-grid")
     (:file "rtest-gtk4-revealer")
;    (:file "gtk4.list-box")                 ; GtkListBox
;    (:file "gtk4.flow-box")                 ; GtkFlowBox
;    (:file "gtk4.stack")                    ; GtkStack
;    (:file "gtk4.stack-switcher")           ; GtkStackSwitcher
;    (:file "gtk4.stack-sidebar")            ; GtkStackSidebar
;    (:file "gtk4.action-bar")               ; GtkActionBar
;    (:file "gtk4.header-bar")               ; GtkHeaderBar
;    (:file "gtk4.overlay")                  ; GtkOverlay
     (:file "rtest-gtk4-paned")
;    (:file "gtk4.notebook")                 ; GtkNotebook
;    (:file "gtk4.expander")                 ; GtkExpander
;    (:file "gtk4.aspect-frame")             ; GtkAspectFrame
     (:file "rtest-gtk4-fixed")

     ;; Display Widgets
     (:file "rtest-gtk4-label")
     (:file "rtest-gtk4-image")
     (:file "rtest-gtk4-picture")
;    (:file "gtk4.spinner")                  ; GtkSpinner
;    (:file "gtk4.info-bar")                 ; GtkInfoBar
     (:file "rtest-gtk4-progress-bar")
;    (:file "gtk4.level-bar")                ; GtkLevelBar
;    (:file "gtk4.statusbar")                ; GtkStatusbar
;    (:file "gtk4.calendar")                 ; GtkCalendar

     ;; Media Support
     (:file "rtest-gtk4-video"          :if-feature (:not :windows))
     (:file "rtest-gtk4-media-controls" :if-feature (:not :windows))
     (:file "rtest-gtk4-media-stream"   :if-feature (:not :windows))
     (:file "rtest-gtk4-media-file"     :if-feature (:not :windows))

     ;; Buttons and Toggles
     (:file "rtest-gtk4-button")
     (:file "rtest-gtk4-toggle-button")
     (:file "rtest-gtk4-check-button")
     (:file "rtest-gtk4-menu-button")
     (:file "rtest-gtk4-link-button")
     (:file "rtest-gtk4-lock-button")
     (:file "rtest-gtk4-scale-button")
     (:file "rtest-gtk4-volume-button")
     (:file "rtest-gtk4-switch")

     ;; Multiline Text Editor
     (:file "rtest-gtk4-text-iter")
;    (:file "gtk4.text-tag")                 ; GtkTextTag
;    (:file "gtk4.text-tag-table")           ; GtkTextTagTable
;    (:file "gtk4.text-mark")                ; GtkTextMark
;    (:file "gtk4.text-buffer")              ; GtkTextBuffer
;    (:file "gtk4.text-view")                ; GtkTextView

     ;; Tree, List and Icon Grid Widgets
     (:file "rtest-gtk4-tree-model")
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

     (:file "rtest-gtk4-cell-area") ; !!!!!
;    (:file "gtk4.cell-area-box")         ; GtkCellAreaBox
;    (:file "gtk4.cell-area-context")     ; GtkCellAreaCenter

;    (:file "gtk4.list-store")            ; GtkListStore
;    (:file "gtk4.tree-store")            ; GtkTreeStore

     ;; Numeric and Text Data Entry
     (:file "rtest-gtk4-editable")
     (:file "rtest-gtk4-entry-buffer")
     (:file "rtest-gtk4-text")
     (:file "rtest-gtk4-entry")
     (:file "rtest-gtk4-entry-completion")
;    (:file "gtk4.password-entry")                ; GtkPasswordEntry
;    (:file "gtk4.scale")                         ; GtkScale
;    (:file "gtk4.spin-button")                   ; GtkSpinButton
;    (:file "gtk4.search-entry")                  ; GtkSearchEntry
;    (:file "gtk4.search-bar")                    ; GtkSearchBar
;    (:file "gtk4.editable-label")                ; GtkEditableLabel

     ;; Combo Box, Popover
;    (:file "gtk4.combo-box")                 ; GtkComboBox
;    (:file "gtk4.combo-box-text")            ; GtkComboBoxText
     (:file "rtest-gtk4-popover")
     (:file "rtest-gtk4-popover-menu")
;    (:file "gtk4.popover-menu-bar")          ; GtkPopoverMenuBar
;    (:file "gtk4.drop-down")                 ; GtkDropDown

     ;; Selector Widgets and Dialogs
;     (:file "rtest-gtk4-color-chooser")           ; GtkColorChooser
;     (:file "rtest-gtk4-color-button")            ; GtkColorButton
;     (:file "rtest-gtk4-color-chooser-widget")    ; GtkColorChooserWidget
;     (:file "rtest-gtk4-color-chooser-dialog")    ; GtkColorChooserDialog

     (:file "rtest-gtk4-color-dialog"         :if-feature :gtk-4-10)
     (:file "rtest-gtk4-color-dialog-button"  :if-feature :gtk-4-10)

     (:file "rtest-gtk4-file-chooser")
     (:file "rtest-gtk4-file-chooser-native")
     (:file "rtest-gtk4-file-chooser-dialog")
     (:file "rtest-gtk4-file-chooser-widget")

     (:file "rtest-gtk4-file-dialog"          :if-feature :gtk-4-10)

;    (:file "gtk4.font-chooser")                  ; GtkFontChooser
;    (:file "gtk4.font-button")                   ; GtkFontButton
;    (:file "gtk4.font-chooser-widget")           ; GtkFontChooserWidget
     (:file "rtest-gtk4-font-chooser-dialog")
;    (:file "gtk4.emoji-chooser")                 ; GtkEmojiChooser

     ;; Widgets for custom drawing
;    (:file "gtk4.drawing-area")              ; GtkDrawingArea
;    (:file "gtk4.gl-area")                   ; GtkGlArea

     ;; Ornaments
     (:file "rtest-gtk4-frame")
     (:file "rtest-gtk4-separator")

     ;; Scrolling
;    (:file "gtk4.scrollbar")                 ; GtkScrollbar
;    (:file "gtk4.scrolled-window")           ; GtkScrolledWindow
;    (:file "gtk4.viewport")                  ; GtkViewport

     ;; Windows
     (:file "rtest-gtk4-window")
     (:file "rtest-gtk4-dialog")
     (:file "rtest-gtk4-message-dialog")
     (:file "rtest-gtk4-about-dialog")
     (:file "rtest-gtk4-assistant")
;    (:file "gtk4.window-group")              ; GtkWindowGroup
     (:file "rtest-gtk4-native-dialog")

     ;; Printing
     (:file "rtest-gtk4-print-operation")
;    (:file "gtk4.print-context")             ; GtkPrintContext
     (:file "rtest-gtk4-paper-size")
;    (:file "gtk4.print-settings")            ; GtkPrintSettings
;    (:file "gtk4.page-setup")                ; GtkPageSetup
     (:file "rtest-gtk4-print-unix-dialog" :if-feature (:not :windows))
;    (:file "gtk4.printer")                   ; GtkPrinter

     ;; Shortcuts Widgets
     (:file "rtest-gtk4-shortcuts-window")
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
     (:file "rtest-gtk4-recent-manager")

     ;; Choosing from installed applications
;    (:file "gtk4.app-chooser")              ; GtkAppChooser
;    (:file "gtk4.app-chooser-button")       ; GtkAppChooserButton
     (:file "rtest-gtk4-app-chooser-dialog")
;    (:file "gtk4.app-chooser-widget")       ; GtkAppChooserWidget

     ;; Gestures and event handling
     (:file "rtest-gtk4-event-controller")
     (:file "rtest-gtk4-event-controller-key")
     (:file "rtest-gtk4-event-controller-focus")
     (:file "rtest-gtk4-event-controller-legacy")
     (:file "rtest-gtk4-event-controller-scroll")
     (:file "rtest-gtk4-event-controller-motion")

     (:file "rtest-gtk4-gesture")
     (:file "rtest-gtk4-gesture-single")
     (:file "rtest-gtk4-gesture-drag")
     (:file "rtest-gtk4-gesture-long-press")
     (:file "rtest-gtk4-gesture-click")
     (:file "rtest-gtk4-gesture-pan")
     (:file "rtest-gtk4-gesture-swipe")
     (:file "rtest-gtk4-gesture-rotate")
     (:file "rtest-gtk4-gesture-zoom")
     (:file "rtest-gtk4-gesture-stylus")

     (:file "rtest-gtk4-pad-controller")
     (:file "rtest-gtk4-shortcut-controller")

     ;; Keyboard shortcuts
     (:file "rtest-gtk4-keyboard-accelerators")
     (:file "rtest-gtk4-shortcut")
     (:file "rtest-gtk4-shortcut-trigger")
     (:file "rtest-gtk4-shortcut-action")

     ;; Data exchange, clipboards and Drag-and-Drop
     (:file "rtest-gtk4-drag-source")
;    (:file "gtk4.drag-icon")                     ; GtkDragIcon
;    (:file "gtk4.drop-target")                   ; GtkDropTarget
;    (:file "gtk4.drop-target-async")             ; GtkDropTargetAsync
;    (:file "gtk4.drop-controller-motion")        ; GtkDropControllerMotion

     ;; Miscellaneous
     (:file "rtest-gtk4-adjustment")
     (:file "rtest-gtk4-size-group")
;    (:file "gtk4.snapshot")
;    (:file "gtk4.tooltip")
;    (:file "gtk4.widget-paintable")
;    (:file "gtk4.window-controls")
     (:file "rtest-gtk4-window-handle")

     ;; Application support
     (:file "rtest-gtk4-application")             ; GtkApplication
     (:file "rtest-gtk4-application-window")      ; GtkApplicationWindow
))))

;;; --- End of file cl-cffi-gtk4.asd -------------------------------------------
