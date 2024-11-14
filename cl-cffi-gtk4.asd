;;; ----------------------------------------------------------------------------
;;; cl-cffi-gtk4.asd
;;;
;;; Copyright (C) 2021 - 2024 Dieter Kaiser
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
  :version "0.9.0"
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
     (:file "gdk4.dmabuf-formats"                 :if-feature :gtk-4-14)
     (:file "gdk4.color-state"                    :if-feature :gtk-4-16)
     (:file "gdk4.cicp-params"                    :if-feature :gtk-4-16)

     ;; Display, Seat, Device, Monitor
     (:file "gdk4.display-manager")
     (:file "gdk4.display")
     (:file "gdk4.device-tool")
     (:file "gdk4.device")
     (:file "gdk4.device-pad")
     (:file "gdk4.monitor")
     (:file "gdk4.seat")

     ;; Paintables, Textures
     (:file "gdk4.paintable")
     (:file "gdk4.texture")
     (:file "gdk4.texture-downloader"             :if-feature :gtk-4-10)
     (:file "gdk4.dmabuf-texture"                 :if-feature :gtk-4-14)

     ;; Events
     (:file "gdk4.event")

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

     ;; Application launching
     (:file "gdk4.app-launch-context")

     ;; Miscellaneous
     (:file "gdk4.cursor")
     (:file "gdk4.frame-timings")
     (:file "gdk4.frame-clock")

     ;; Pixbuf, Pango, Cairo, Backends interaction
     (:file "gdk4.pixbuf-interaction")
     (:file "gdk4.pango-interaction")
     (:file "gdk4.cairo-interaction")
    ))
   (:module gsk
    :serial t
    :components
    ((:file "gsk4.package")
     (:file "gsk4.rounded-rect")
     (:file "gsk4.transform")
     (:file "gsk4.path"                           :if-feature :gtk-4-14)
     (:file "gsk4.path-builder"                   :if-feature :gtk-4-14)
     (:file "gsk4.renderer")
     (:file "gsk4.render-node")
    ))
   (:module gtk
    :serial t
    :components
    ((:file "gtk4.package")

     ;; GTK Core
     (:file "gtk4.version")
     (:file "gtk4.enumerations")
     (:file "gtk4.main-loop")
     (:file "gtk4.mount-operation")

     ;; Theming in GTK
     (:file "gtk4.style-provider")
     (:file "gtk4.css-provider")
     (:file "gtk4.style-context")
     (:file "gtk4.icon-paintable")
     (:file "gtk4.icon-theme")

     ;; Interfaces
     (:file "gtk4.accessible")
     (:file "gtk4.accessible-range"               :if-feature :gtk-4-10)
     (:file "gtk4.accessible-text"                :if-feature :gtk-4-14)
     (:file "gtk4.actionable")
     (:file "gtk4.cell-editable")
     (:file "gtk4.native")
     (:file "gtk4.orientable")
     (:file "gtk4.root")
     (:file "gtk4.scrollable")
     (:file "gtk4.shortcut-manager")

     ;; Settings
     (:file "gtk4.settings")

     ;; Interface builder
     (:file "gtk4.buildable")
     (:file "gtk4.builder")

     ;; GListModel support
     (:file "gtk4.bitset")
     (:file "gtk4.expression")

     (:file "gtk4.filter")
     (:file "gtk4.custom-filter")
     (:file "gtk4.multi-filter")
     (:file "gtk4.bool-filter")
     (:file "gtk4.string-filter")
     (:file "gtk4.file-filter")

     (:file "gtk4.sorter")
     (:file "gtk4.custom-sorter")
     (:file "gtk4.multi-sorter")
     (:file "gtk4.string-sorter")
     (:file "gtk4.numeric-sorter")
     (:file "gtk4.column-view-sorter"             :if-feature :gtk-4-10)

     (:file "gtk4.section-model"                  :if-feature :gtk-4-12)
     (:file "gtk4.selection-model")
     (:file "gtk4.no-selection")
     (:file "gtk4.single-selection")
     (:file "gtk4.multi-selection")

     (:file "gtk4.filter-list-model")
     (:file "gtk4.flatten-list-model")
     (:file "gtk4.map-list-model")
     (:file "gtk4.slice-list-model")
     (:file "gtk4.sort-list-model")
     (:file "gtk4.selection-filter-model")

     (:file "gtk4.bookmark-list")
     (:file "gtk4.directory-list")
     (:file "gtk4.string-list")

     ;; Layout Managers
     (:file "gtk4.layout-manager")
     (:file "gtk4.layout-child")
     (:file "gtk4.bin-layout")
     (:file "gtk4.box-layout")
     (:file "gtk4.center-layout")
     (:file "gtk4.fixed-layout")
     (:file "gtk4.grid-layout")
     (:file "gtk4.overlay-layout")
     (:file "gtk4.custom-layout")
     (:file "gtk4.constraint-layout")

     (:file "gtk4.constraint")
     (:file "gtk4.constraint-guide")

     ;; Abstract Base Classes
     (:file "gtk4.widget")
     (:file "gtk4.range")

     ;; List-based Widgets
     (:file "gtk4.list-item")
     (:file "gtk4.list-item-factory")
     (:file "gtk4.signal-list-item-factory")
     (:file "gtk4.builder-list-item-factory")
     (:file "gtk4.scroll-info"                    :if-feature :gtk-4-12)
     (:file "gtk4.list-header"                    :if-feature :gtk-4-12)
     (:file "gtk4.list-view")
     (:file "gtk4.grid-view")
     (:file "gtk4.column-view")
     (:file "gtk4.column-view-column")
     (:file "gtk4.column-view-cell"               :if-feature :gtk-4-12)
     (:file "gtk4.column-view-row"                :if-feature :gtk-4-12)
     (:file "gtk4.drop-down")

     ;; Tree support
     (:file "gtk4.tree-list-model")
     (:file "gtk4.tree-list-row-sorter")
     (:file "gtk4.tree-expander")

     ;; Layout Containers
     (:file "gtk4.box")
     (:file "gtk4.center-box")
     (:file "gtk4.grid")
     (:file "gtk4.revealer")
     (:file "gtk4.list-box")
     (:file "gtk4.flow-box")
     (:file "gtk4.stack")
     (:file "gtk4.stack-switcher")
     (:file "gtk4.stack-sidebar")
     (:file "gtk4.action-bar")
     (:file "gtk4.header-bar")
     (:file "gtk4.overlay")
     (:file "gtk4.paned")
     (:file "gtk4.notebook")
     (:file "gtk4.expander")
     (:file "gtk4.aspect-frame")
     (:file "gtk4.fixed")

     ;; Display Widgets
     (:file "gtk4.label")
     (:file "gtk4.inscription"                    :if-feature :gtk-4-8)
     (:file "gtk4.image")
     (:file "gtk4.picture")
     (:file "gtk4.spinner")
     (:file "gtk4.info-bar")
     (:file "gtk4.progress-bar")
     (:file "gtk4.level-bar")
     (:file "gtk4.statusbar")
     (:file "gtk4.calendar")

     ;; Media Support
     (:file "gtk4.graphics-offload"               :if-feature :gtk-4-14)
     (:file "gtk4.video")
     (:file "gtk4.media-controls")
     (:file "gtk4.media-stream")
     (:file "gtk4.media-file")

     ;; Buttons and Toggles
     (:file "gtk4.button")
     (:file "gtk4.toggle-button")
     (:file "gtk4.check-button")
     (:file "gtk4.menu-button")
     (:file "gtk4.link-button")
     (:file "gtk4.lock-button")
     (:file "gtk4.scale-button")
     (:file "gtk4.volume-button")
     (:file "gtk4.switch")

     ;; Multiline Text Editor
     (:file "gtk4.text-iter")
     (:file "gtk4.text-tag")
     (:file "gtk4.text-tag-table")
     (:file "gtk4.text-mark")
     (:file "gtk4.text-buffer")
     (:file "gtk4.text-view")

     ;; Tree, List and Icon Grid Widgets
     (:file "gtk4.tree-model")
     (:file "gtk4.tree-selection")
     (:file "gtk4.cell-layout")
     (:file "gtk4.tree-view-column")
     (:file "gtk4.tree-view")
     (:file "gtk4.tree-view-dnd")

     (:file "gtk4.cell-view")
     (:file "gtk4.icon-view")

     (:file "gtk4.tree-sortable")
     (:file "gtk4.tree-model-sort")
     (:file "gtk4.tree-model-filter")

     (:file "gtk4.cell-renderer")
     (:file "gtk4.cell-renderer-text")
     (:file "gtk4.cell-renderer-accel")
     (:file "gtk4.cell-renderer-combo")
     (:file "gtk4.cell-renderer-pixbuf")
     (:file "gtk4.cell-renderer-progress")
     (:file "gtk4.cell-renderer-spin")
     (:file "gtk4.cell-renderer-toggle")
     (:file "gtk4.cell-renderer-spinner")

     (:file "gtk4.cell-area")
     (:file "gtk4.cell-area-box")
     (:file "gtk4.cell-area-context")

     (:file "gtk4.list-store")
     (:file "gtk4.tree-store")

     ;; Numeric and Text Data Entry
     (:file "gtk4.editable")
     (:file "gtk4.entry-buffer")
     (:file "gtk4.text")
     (:file "gtk4.entry")
     (:file "gtk4.entry-completion")
     (:file "gtk4.password-entry")
     (:file "gtk4.scale")
     (:file "gtk4.spin-button")
     (:file "gtk4.search-entry")
     (:file "gtk4.search-bar")
     (:file "gtk4.editable-label")

     ;; Combo Box, Popover
     (:file "gtk4.combo-box")
     (:file "gtk4.combo-box-text")
     (:file "gtk4.popover")
     (:file "gtk4.popover-menu")
     (:file "gtk4.popover-menu-bar")

     ;; Selector Widgets and Dialogs
     (:file "gtk4.color-chooser")
     (:file "gtk4.color-button")
     (:file "gtk4.color-chooser-widget")
     (:file "gtk4.color-chooser-dialog")

     (:file "gtk4.color-dialog"                   :if-feature :gtk-4-10)
     (:file "gtk4.color-dialog-button"            :if-feature :gtk-4-10)

     (:file "gtk4.file-chooser")
     (:file "gtk4.file-chooser-native")
     (:file "gtk4.file-chooser-dialog")
     (:file "gtk4.file-chooser-widget")

     (:file "gtk4.file-dialog"                    :if-feature :gtk-4-10)
     (:file "gtk4.file-launcher"                  :if-feature :gtk-4-10)
     (:file "gtk4.uri-launcher"                   :if-feature :gtk-4-10)

     (:file "gtk4.font-chooser")
     (:file "gtk4.font-button")
     (:file "gtk4.font-chooser-widget")
     (:file "gtk4.font-chooser-dialog")

     (:file "gtk4.font-dialog"                    :if-feature :gtk-4-10)
     (:file "gtk4.font-dialog-button"             :if-feature :gtk-4-10)

     (:file "gtk4.emoji-chooser")

     ;; Widgets for custom drawing
     (:file "gtk4.drawing-area")
     (:file "gtk4.gl-area")

     ;; Ornaments
     (:file "gtk4.frame")
     (:file "gtk4.separator")

     ;; Scrolling
     (:file "gtk4.scrollbar")
     (:file "gtk4.scrolled-window")
     (:file "gtk4.viewport")

     ;; Windows
     (:file "gtk4.window")
     (:file "gtk4.dialog")
     (:file "gtk4.message-dialog")
     (:file "gtk4.about-dialog")
     (:file "gtk4.alert-dialog"                   :if-feature :gtk-4-10)
     (:file "gtk4.assistant")
     (:file "gtk4.window-group")
     (:file "gtk4.native-dialog")

     ;; Printing
     (:file "gtk4.print-operation-preview")
     (:file "gtk4.print-operation")
     (:file "gtk4.print-context")
     (:file "gtk4.paper-size")
     (:file "gtk4.print-settings")
     (:file "gtk4.page-setup")
     (:file "gtk4.page-setup-unix-dialog"         :if-feature (:not :windows))
     (:file "gtk4.print-unix-dialog"              :if-feature (:not :windows))
     (:file "gtk4.printer"                        :if-feature (:not :windows))
     (:file "gtk4.print-job"                      :if-feature (:not :windows))
     (:file "gtk4.print-setup"                    :if-feature :gtk-4-14)
     (:file "gtk4.print-dialog"                   :if-feature :gtk-4-14)

     ;; Shortcuts Widgets
     (:file "gtk4.shortcuts-window")
     (:file "gtk4.shortcuts-section")
     (:file "gtk4.shortcuts-group")
     (:file "gtk4.shortcuts-shortcut")
     (:file "gtk4.shortcut-label")

     ;; Accessibility
     (:file "gtk4.at-context")

     ;; Input Methods
     (:file "gtk4.im-context")
     (:file "gtk4.im-context-simple")
     (:file "gtk4.im-multicontext")

     ;; Recently Used Documents
     (:file "gtk4.recent-manager")

     ;; Choosing from installed applications
     (:file "gtk4.app-chooser")
     (:file "gtk4.app-chooser-button")
     (:file "gtk4.app-chooser-dialog")
     (:file "gtk4.app-chooser-widget")

     ;; Gestures and event handling
     (:file "gtk4.event-controller")
     (:file "gtk4.event-controller-key")
     (:file "gtk4.event-controller-focus")
     (:file "gtk4.event-controller-legacy")
     (:file "gtk4.event-controller-scroll")
     (:file "gtk4.event-controller-motion")

     (:file "gtk4.gesture")
     (:file "gtk4.gesture-single")
     (:file "gtk4.gesture-drag")
     (:file "gtk4.gesture-long-press")
     (:file "gtk4.gesture-click")
     (:file "gtk4.gesture-pan")
     (:file "gtk4.gesture-swipe")
     (:file "gtk4.gesture-rotate")
     (:file "gtk4.gesture-zoom")
     (:file "gtk4.gesture-stylus")

     (:file "gtk4.pad-controller")
     (:file "gtk4.shortcut-controller")

     ;; Keyboard shortcuts
     (:file "gtk4.keyboard-accelerators")
     (:file "gtk4.shortcut")
     (:file "gtk4.shortcut-trigger")
     (:file "gtk4.shortcut-action")

     ;; Data exchange, clipboards and Drag-and-Drop
     (:file "gtk4.drag-source")
     (:file "gtk4.drag-icon")
     (:file "gtk4.drop-target")
     (:file "gtk4.drop-target-async")
     (:file "gtk4.drop-controller-motion")

     ;; Miscellaneous
     (:file "gtk4.adjustment")
     (:file "gtk4.size-group")
     (:file "gtk4.snapshot")
     (:file "gtk4.tooltip")
     (:file "gtk4.widget-paintable")
     (:file "gtk4.window-controls")
     (:file "gtk4.window-handle")

     ;; Application support
     (:file "gtk4.application")
     (:file "gtk4.application-window")

     ;; Miscellaneous
     (:file "gtk4.init")
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
  :version "0.9.0"
  :author "Dieter Kaiser"
  :license "MIT"
  :depends-on (:cl-cffi-gtk4 :cl-cffi-glib/test :fiveam :iterate)
  :perform (test-op (o c)
               (uiop:symbol-call :fiveam :run!
                                 (uiop:find-symbol* :gtk-test
                                                    :gtk-test)))
  :components
  ((:module test
    :serial nil
    :components
    ((:file "rtest-gtk4")

     ;; GListModel support
     (:file "rtest-gtk4-bitset")
     (:file "rtest-gtk4-expression")

     (:file "rtest-gtk4-filter")
     (:file "rtest-gtk4-custom-filter")
     (:file "rtest-gtk4-multi-filter")
     (:file "rtest-gtk4-bool-filter")
     (:file "rtest-gtk4-string-filter")
     (:file "rtest-gtk4-file-filter")

     (:file "rtest-gtk4-sorter")
     (:file "rtest-gtk4-custom-sorter")
     (:file "rtest-gtk4-multi-sorter")
     (:file "rtest-gtk4-string-sorter")
     (:file "rtest-gtk4-numeric-sorter")
     (:file "rtest-gtk4-column-view-sorter"       :if-feature :gtk-4-10)

;    (:file "rtest-gtk4-section-model"            :if-feature :gtk-4-12)
     (:file "rtest-gtk4-selection-model")
     (:file "rtest-gtk4-no-selection")
     (:file "rtest-gtk4-single-selection")
     (:file "rtest-gtk4-multi-selection")

     (:file "rtest-gtk4-filter-list-model")
     (:file "rtest-gtk4-flatten-list-model")
     (:file "rtest-gtk4-map-list-model")
     (:file "rtest-gtk4-slice-list-model")
     (:file "rtest-gtk4-sort-list-model")
     (:file "rtest-gtk4-selection-filter-model")

     (:file "rtest-gtk4-bookmark-list")
     (:file "rtest-gtk4-directory-list")
     (:file "rtest-gtk4-string-list")

     ;; List-based widgets
     (:file "rtest-gtk4-list-item")
     (:file "rtest-gtk4-list-item-factory")
;    gtk4.signal-list-item-factory.lisp
;    gtk4.builder-list-item-factory.lisp
     (:file "rtest-gtk4-scroll-info"              :if-feature :gtk-4-12)
     (:file "rtest-gtk4-list-header"              :if-feature :gtk-4-12)
     (:file "rtest-gtk4-list-view")
;    gtk4.grid-view.lisp
;    gtk4.column-view.lisp
;    gtk4.column-view-column.lisp
     (:file "rtest-gtk4-column-view-cell"         :if-feature :gtk-4-12)
     (:file "rtest-gtk4-column-view-row"          :if-feature :gtk-4-12)
     (:file "rtest-gtk4-drop-down")

     ;; Tree support
     (:file "rtest-gtk4-tree-list-model")
     (:file "rtest-gtk4-tree-list-row-sorter")
     (:file "rtest-gtk4-tree-expander")

     ;; GTK Core
     (:file "rtest-gtk4-version")
     (:file "rtest-gtk4-enumerations")
     (:file "rtest-gtk4-main-loop")
;     (:file "gtk4.mount-operation")

     ;; Theming in GTK
     (:file "rtest-gtk4-style-provider")
     (:file "rtest-gtk4-css-provider")
     (:file "rtest-gtk4-style-context")
     (:file "rtest-gtk4-icon-paintable")
     (:file "rtest-gtk4-icon-theme")

     ;; Interfaces
     (:file "rtest-gtk4-accessible")
     (:file "rtest-gtk4-accessible-range"         :if-feature :gtk-4-10)
     (:file "rtest-gtk4-accessible-text"          :if-feature :gtk-4-14)
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
     (:file "rtest-gtk4-layout-manager")
     (:file "rtest-gtk4-layout-child")
     (:file "rtest-gtk4-bin-layout")
     (:file "rtest-gtk4-box-layout")
     (:file "rtest-gtk4-center-layout")
     (:file "rtest-gtk4-fixed-layout")
     (:file "rtest-gtk4-grid-layout")
     (:file "rtest-gtk4-overlay-layout")
     (:file "rtest-gtk4-custom-layout")
     (:file "rtest-gtk4-constraint-layout")

     (:file "rtest-gtk4-constraint")
     (:file "rtest-gtk4-constraint-guide")

     ;; Abstract Base Classes
     (:file "rtest-gtk4-widget")
;    (:file "rtest-gtk4-widget-subclassing")
     (:file "rtest-gtk4-range")

     ;; Layout Containers
     (:file "rtest-gtk4-box")
     (:file "rtest-gtk4-center-box")
     (:file "rtest-gtk4-grid")
     (:file "rtest-gtk4-revealer")
     (:file "rtest-gtk4-list-box")
     (:file "rtest-gtk4-flow-box")
     (:file "rtest-gtk4-stack")
     (:file "rtest-gtk4-stack-switcher")
     (:file "rtest-gtk4-stack-sidebar")
     (:file "rtest-gtk4-action-bar")
     (:file "rtest-gtk4-header-bar")
     (:file "rtest-gtk4-overlay")
     (:file "rtest-gtk4-paned")
     (:file "rtest-gtk4-notebook")
     (:file "rtest-gtk4-expander")
     (:file "rtest-gtk4-aspect-frame")
     (:file "rtest-gtk4-fixed")

     ;; Display Widgets
     (:file "rtest-gtk4-label")
     (:file "rtest-gtk4-inscription"              :if-feature :gtk-4-8)
     (:file "rtest-gtk4-image")
     (:file "rtest-gtk4-picture")
     (:file "rtest-gtk4-spinner")
     (:file "rtest-gtk4-info-bar")
     (:file "rtest-gtk4-progress-bar")
     (:file "rtest-gtk4-level-bar")
     (:file "rtest-gtk4-statusbar")
     (:file "rtest-gtk4-calendar")

     ;; Media Support
     (:file "rtest-gtk4-graphics-offload"         :if-feature :gtk-4-14)
     (:file "rtest-gtk4-video"                    :if-feature (:not :windows))
     (:file "rtest-gtk4-media-controls"           :if-feature (:not :windows))
     (:file "rtest-gtk4-media-stream"             :if-feature (:not :windows))
     (:file "rtest-gtk4-media-file"               :if-feature (:not :windows))

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
     (:file "rtest-gtk4-text-tag")
     (:file "rtest-gtk4-text-tag-table")
     (:file "rtest-gtk4-text-mark")
     (:file "rtest-gtk4-text-buffer")
     (:file "rtest-gtk4-text-view")

     ;; Tree, List and Icon Grid Widgets
     (:file "rtest-gtk4-tree-model")
     (:file "rtest-gtk4-tree-model-subclassing")
     (:file "rtest-gtk4-tree-selection")
     (:file "rtest-gtk4-tree-view-column")
     (:file "rtest-gtk4-tree-view")
     (:file "rtest-gtk4-tree-view-dnd")

     (:file "rtest-gtk4-cell-layout")
     (:file "rtest-gtk4-cell-view")
     (:file "rtest-gtk4-icon-view")

     (:file "rtest-gtk4-tree-sortable")
     (:file "rtest-gtk4-tree-model-sort")
     (:file "rtest-gtk4-tree-model-filter")

     (:file "rtest-gtk4-cell-renderer")
     (:file "rtest-gtk4-cell-renderer-text")
     (:file "rtest-gtk4-cell-renderer-accel")
     (:file "rtest-gtk4-cell-renderer-combo")
     (:file "rtest-gtk4-cell-renderer-pixbuf")
     (:file "rtest-gtk4-cell-renderer-progress")
     (:file "rtest-gtk4-cell-renderer-spin")
     (:file "rtest-gtk4-cell-renderer-toggle")
     (:file "rtest-gtk4-cell-renderer-spinner")

     (:file "rtest-gtk4-cell-area")
     (:file "rtest-gtk4-cell-area-box")
     (:file "rtest-gtk4-cell-area-context")

     (:file "rtest-gtk4-list-store")
     (:file "rtest-gtk4-tree-store")

     ;; Numeric and Text Data Entry
     (:file "rtest-gtk4-editable")
     (:file "rtest-gtk4-entry-buffer")
     (:file "rtest-gtk4-text")
     (:file "rtest-gtk4-entry")
     (:file "rtest-gtk4-entry-completion")
     (:file "rtest-gtk4-password-entry")
     (:file "rtest-gtk4-scale")
     (:file "rtest-gtk4-spin-button")
     (:file "rtest-gtk4-search-entry")
     (:file "rtest-gtk4-search-bar")
     (:file "rtest-gtk4-editable-label")

     ;; Combo Box, Popover
     (:file "rtest-gtk4-combo-box")
     (:file "rtest-gtk4-combo-box-text")
     (:file "rtest-gtk4-popover")
     (:file "rtest-gtk4-popover-menu")
     (:file "rtest-gtk4-popover-menu-bar")

     ;; Selector Widgets and Dialogs
     (:file "rtest-gtk4-color-chooser")
     (:file "rtest-gtk4-color-button")
     (:file "rtest-gtk4-color-chooser-widget")
     (:file "rtest-gtk4-color-chooser-dialog")

     (:file "rtest-gtk4-color-dialog"             :if-feature :gtk-4-10)
     (:file "rtest-gtk4-color-dialog-button"      :if-feature :gtk-4-10)

     (:file "rtest-gtk4-file-chooser"             :if-feature (:not :windows))
     (:file "rtest-gtk4-file-chooser-native"      :if-feature (:not :windows))
     (:file "rtest-gtk4-file-chooser-dialog"      :if-feature (:not :windows))
     (:file "rtest-gtk4-file-chooser-widget"      :if-feature (:not :windows))

     (:file "rtest-gtk4-file-dialog"              :if-feature :gtk-4-10)
     (:file "rtest-gtk4-file-launcher"            :if-feature :gtk-4-10)
     (:file "rtest-gtk4-uri-launcher"             :if-feature :gtk-4-10)

     (:file "rtest-gtk4-font-chooser")
     (:file "rtest-gtk4-font-button")
     (:file "rtest-gtk4-font-chooser-widget")
     (:file "rtest-gtk4-font-chooser-dialog")

     (:file "rtest-gtk4-font-dialog"              :if-feature :gtk-4-10)
     (:file "rtest-gtk4-font-dialog-button"       :if-feature :gtk-4-10)

     (:file "rtest-gtk4-emoji-chooser")

     ;; Widgets for custom drawing
     (:file "rtest-gtk4-drawing-area")
     (:file "rtest-gtk4-gl-area")

     ;; Ornaments
     (:file "rtest-gtk4-frame")
     (:file "rtest-gtk4-separator")

     ;; Scrolling
     (:file "rtest-gtk4-scrollbar")
     (:file "rtest-gtk4-scrolled-window")
     (:file "rtest-gtk4-viewport")

     ;; Windows
     (:file "rtest-gtk4-window")
     (:file "rtest-gtk4-dialog")
     (:file "rtest-gtk4-message-dialog")
     (:file "rtest-gtk4-about-dialog")
     (:file "rtest-gtk4-alert-dialog"             :if-feature :gtk-4-10)
     (:file "rtest-gtk4-assistant")
     (:file "rtest-gtk4-window-group")
     (:file "rtest-gtk4-native-dialog")

     ;; Printing
     (:file "rtest-gtk4-print-operation-preview")
     (:file "rtest-gtk4-print-operation")
     (:file "rtest-gtk4-print-context")
     (:file "rtest-gtk4-paper-size")
     (:file "rtest-gtk4-print-settings")
     (:file "rtest-gtk4-page-setup")
     (:file "rtest-gtk4-page-setup-unix-dialog"   :if-feature (:not :windows))
     (:file "rtest-gtk4-print-unix-dialog"        :if-feature (:not :windows))
     (:file "rtest-gtk4-printer"                  :if-feature (:not :windows))
     (:file "rtest-gtk4-print-job"                :if-feature (:not :windows))
     (:file "rtest-gtk4-print-setup"              :if-feature :gtk-4-14)
     (:file "rtest-gtk4-print-dialog"             :if-feature :gtk-4-14)

     ;; Shortcuts Widgets
     (:file "rtest-gtk4-shortcuts-window")
     (:file "rtest-gtk4-shortcuts-section")
     (:file "rtest-gtk4-shortcuts-group")
     (:file "rtest-gtk4-shortcuts-shortcut")
     (:file "rtest-gtk4-shortcut-label")

     ;; Accessibility
     (:file "rtest-gtk4-at-context")

     ;; Input Methods
     (:file "rtest-gtk4-im-context")
     (:file "rtest-gtk4-im-context-simple")
     (:file "rtest-gtk4-im-multicontext")

     ;; Recently Used Documents
     (:file "rtest-gtk4-recent-manager")

     ;; Choosing from installed applications
     (:file "rtest-gtk4-app-chooser")
     (:file "rtest-gtk4-app-chooser-button")
     (:file "rtest-gtk4-app-chooser-dialog")
     (:file "rtest-gtk4-app-chooser-widget")

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
     (:file "rtest-gtk4-drag-icon")
     (:file "rtest-gtk4-drop-target")
     (:file "rtest-gtk4-drop-target-async")
     (:file "rtest-gtk4-drop-controller-motion")

     ;; Miscellaneous
     (:file "rtest-gtk4-adjustment")
     (:file "rtest-gtk4-size-group")
     (:file "rtest-gtk4-snapshot")
     (:file "rtest-gtk4-tooltip")
     (:file "rtest-gtk4-widget-paintable")
     (:file "rtest-gtk4-window-controls")
     (:file "rtest-gtk4-window-handle")

     ;; Application support
     (:file "rtest-gtk4-application")
     (:file "rtest-gtk4-application-window")

     ;; GDK General
     (:file "rtest-gdk4-enumerations")
     (:file "rtest-gdk4-rectangle")
     (:file "rtest-gdk4-rgba")
     (:file "rtest-gdk4-keyval")
     (:file "rtest-gdk4-dmabuf-formats"           :if-feature :gtk-4-14)
     (:file "rtest-gdk4-color-state"              :if-feature :gtk-4-16)
     (:file "rtest-gdk4-cicp-params"              :if-feature :gtk-4-16)

     ;; Displays, Devices, Monitors, Seats
     (:file "rtest-gdk4-display-manager")
     (:file "rtest-gdk4-display")
     (:file "rtest-gdk4-device-tool")
     (:file "rtest-gdk4-device")
     (:file "rtest-gdk4-device-pad")
     (:file "rtest-gdk4-monitor")
     (:file "rtest-gdk4-seat")

     ;; Paintables, Textures
     (:file "rtest-gdk4-paintable")
     (:file "rtest-gdk4-paintable-subclassing")
     (:file "rtest-gdk4-texture")
;    (:file "rtest-gdk4-texture-downloader"      :if-feature :gtk4-10)

     ;; Events
     (:file "rtest-gdk4-event")

     ;; Surfaces, Toplevels, Popups
     (:file "rtest-gdk4-surface")
     (:file "rtest-gdk4-toplevel-layout")
     (:file "rtest-gdk4-toplevel")
     (:file "rtest-gdk4-toplevel-size")
     (:file "rtest-gdk4-popup")
     (:file "rtest-gdk4-popup-layout")

     ;; Daw contexts
;    (:file "rtest-gdk4-draw-context")
;    (:file "rtest-gdk4-gl-context")
;    (:file "rtest-gdk4-vulkan-context")
     (:file "rtest-gdk4-cairo-context")

     ;; Clipboard, Drag and Drop
;    (:file "rtest-gdk4-content-formats")
;    (:file "rtest-gdk4-content-provider")
;    (:file "rtest-gdk4-content-serializer")
;    (:file "rtest-gdk4-content-deserializer")
     (:file "rtest-gdk4-clipboard")
     (:file "rtest-gdk4-drag")
     (:file "rtest-gdk4-drop")
     (:file "rtest-gdk4-drag-surface")

     ;; Application launching
     (:file "rtest-gdk4-app-launch-context")

     ;; Miscellaneous
     (:file "rtest-gdk4-cursor")
     (:file "rtest-gdk4-frame-timings")
     (:file "rtest-gdk4-frame-clock")

     ;; Pixbuf, Pango, Cairo, Backends interaction
;    (:file "rtest-gdk4-pixbuf-interaction")
;    (:file "rtest-gdk4-pango-interaction")
;    (:file "rtest-gdk4-cairo-interaction")

     ;; GSK
     (:file "rtest-gsk4-renderer")
     (:file "rtest-gsk4-transform")
     (:file "rtest-gsk4-path"                     :if-feature :gtk-4-14)
     (:file "rtest-gsk4-path-builder"             :if-feature :gtk-4-14)
     (:file "rtest-gsk4-render-node")
     (:file "rtest-gsk4-rounded-rect")

     (:file "rtest-gtk4-finish")
))))

;;; --- End of file cl-cffi-gtk4.asd -------------------------------------------
