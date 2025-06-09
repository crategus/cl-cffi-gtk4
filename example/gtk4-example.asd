;;;; gtk-example.asd

(asdf:defsystem :gtk4-example
  :author "Dieter Kaiser"
  :license "MIT"
  :serial nil
  :depends-on (:cl-cffi-gtk4
               :split-sequence
               :local-time)
  :components ((:file "gtk4-example")

               ;; Interface builder
               (:file "builder")

               ;; Windows
               (:file "window-simple")
               (:file "dialog-quick-message")
               (:file "dialog-various")
               (:file "message-dialog-simple")
               (:file "message-dialog-various")
               (:file "alert-dialog")
               (:file "assistant")
               (:file "application-window")

               ;; Layout Containers
               (:file "box-append")
               (:file "box-center")
               (:file "grid-spacing")
               (:file "grid-buildable")
               (:file "revealer")
               (:file "revealer-icon")
               (:file "list-box-complex")
               (:file "list-box-controls")
               (:file "flow-box")
               (:file "stack")
               (:file "stack-sidebar")
               (:file "action-bar")
               (:file "header-bar")
               (:file "overlay-interactive")
               (:file "overlay-decorative")
               (:file "notebook")
               (:file "paned")
               (:file "expander")
               (:file "aspect-frame")
               (:file "fixed")

               ;; Layout Managers
               (:file "fixed-layout")
               (:file "constraint-simple")
               (:file "constraint-interactive")
               (:file "constraint-builder")

               ;; Display Widgets
               (:file "label-various")
               (:file "images")
               (:file "spinner")
               (:file "info-bar")
               (:file "progress-bar")
               (:file "level-bar")
               (:file "statusbar")
               (:file "calendar")

               ;; List-based widgets
               (:file "list-view-applauncher")
               (:file "list-view-recent-manager")
               (:file "list-view-bookmarks")
               (:file "column-view-content-types")
               (:file "column-view-ucd")
               (:file "grid-view-clocks")
               (:file "grid-view-colors")
               (:file "drop-down")

               ;; Media Support
               (:file "video")

               ;; Buttons and Toggle Widgets
               (:file "button-various")
               (:file "toggle-button")
               (:file "toggle-button-action")
               (:file "check-button")
               (:file "menu-button")
               (:file "link-button")
               (:file "scale-button")
               (:file "switch")

               ;; Numeric/Text Data Entry
               (:file "entry")
               (:file "entry-buffer")
               (:file "entry-completion")
               (:file "password-entry")
               (:file "scale-widget")
               (:file "spin-button")
               (:file "search-bar")

               ;; Multiline Text Editor
               (:file "text-view-simple")
               (:file "text-view-attributes")
               (:file "text-view-tags")
               (:file "text-view-search")
               (:file "text-view-hypertext")
               (:file "text-view-markup")
               (:file "text-view-tabs")
               (:file "text-view-multiple")
               (:file "text-view-undo")
               (:file "text-view-scroll")

               ;; Tree, List and Icon Grid Widgets
               (:file "tree-view-simple")
               (:file "tree-view-path")
               (:file "tree-view-content-type")
;               (:file "icon-view")

               ;; Menus, Combo Box, Toolbar
               (:file "combo-box")
               (:file "combo-box-text")

               ;; Selector Widgets and Dialogs
               (:file "color-button")
               (:file "color-button-label")
               (:file "color-chooser-dialog")
               (:file "color-chooser-widget")
               (:file "color-chooser-palette")

               (:file "file-chooser-dialog")
               (:file "file-chooser-native")
               (:file "file-dialog-open")

               (:file "font-button")
               (:file "font-button-label")

               (:file "pickers")

               ;; Ornaments
               (:file "frame")
               (:file "frame-properties")

               ;; Widgets for custom drawing
               (:file "drawing-area")
               (:file "drawing-area-scribble")

               ;; Scrolling
               (:file "scrolled-window")

               ;; Printing
               (:file "page-setup-dialog"         :if-feature (:not :windows))

               ;; Data exchange, Clipboards, Drag and Drop
               (:file "clipboard")
               (:file "drag-and-drop")

               ;; Miscellaneous
               (:file "cursors")
               (:file "size-group")
               (:file "emblemed-icon")
               (:file "event-controller")
               (:file "widget-template")

               ;; Theming in GTK
               (:file "css-accordion")
               (:file "css-basics")
               (:file "css-blendmodes")
               (:file "css-multiplebgs")
               (:file "css-pixbufs")
               (:file "css-shadows")

               ;; Subclassing
               (:file "subclassing-figure")

               ;; GDK examples
               (:file "app-launch-context")
               (:file "frame-clock")

               ;; Paintable, Texture
               (:file "paintable-simple")
               (:file "paintable-animation")
               (:file "paintable-emblem")
              ))

;;; --- 2023-10-24 -------------------------------------------------------------
