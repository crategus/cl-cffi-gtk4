;;;; gtk-example.asd

(asdf:defsystem :gtk4-example
  :author "Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :depends-on (:cl-cffi-gtk4
               :split-sequence)
  :components ((:file "gtk4-example")

               ;; Windows
               (:file "window-simple")
               (:file "dialog-quick-message")
               (:file "dialog-various")
               (:file "message-dialog-simple")
               (:file "message-dialog-various")
               (:file "assistant")

               ;; Layout Containers
               (:file "box-append")
               (:file "box-center")
               (:file "grid-spacing")
               (:file "revealer")
               (:file "revealer-icon")
               (:file "paned")
               (:file "fixed")

               ;; Display Widgets
               (:file "label-various")
               (:file "images")
               (:file "spinner")
               (:file "info-bar")
               (:file "progress-bar")
               (:file "level-bar")
               (:file "statusbar")

               ;; Media Support
               (:file "video")

               ;; Buttons and Toggle Widgets
               (:file "button-more")
               (:file "toggle-button")
               (:file "toggle-button-action")
               (:file "check-button")
               (:file "link-button")
               (:file "switch")
               (:file "scale-button")

               ;; Numeric/Text Data Entry
               (:file "entry")
               (:file "entry-buffer")
               (:file "entry-completion")
               (:file "scale-widget")
               (:file "spin-button")

               ;; Multiline Text Editor
               (:file "text-view-simple")
               (:file "text-view-attributes")
               (:file "text-view-tags")
               (:file "text-view-search")
;               (:file "text-view-tooltip")

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

               (:file "font-button")
               (:file "font-button-label")

               ;; Ornaments
               (:file "frame")
               (:file "frame-properties")

               ;; Widgets for custom drawing
               (:file "drawing-area")

               ;; Scrolling
               (:file "scrolled-window")

               ;; Miscellaneous
               (:file "size-group")
;               (:file "event-controller")

               ;; Theming in GTK
               (:file "css-accordion")
               (:file "css-basics")
               (:file "css-blendmodes")
               (:file "css-multiplebgs")
               (:file "css-pixbufs")
               (:file "css-shadows")
              ))

;;; --- 2023-3-11 --------------------------------------------------------------
