;;;; GTK 4 Demo
;;;;
;;;; 2024-4-1

(in-package :gtk4-demo)

(defparameter *gtk-demos*
   '(("GTK 4 Lisp Demo"
      nil
      nil
      "gtk4-demo"
      "gtk4-demo.lisp"
      "gtk4-demo-catalog.lisp"
      "gtk4-demo-package.lisp"
      "gtk4-demo.asd"
      "main.ui"
      "main-listitem.ui"
      "gtk4-demo.xml")

     "Application"
     (("Simple Application"
       ":application"
       "APPLICATION-SIMPLE"
       "gtk4-application"
       "application-simple.lisp"
       :keywords
       "GtkApplication"
       "gtk:application"
       "g:application-flags"
       "g:application-hold"
       "g:application-release"
       "g:application-run")
      ("Application Resources"
       ":application"
       "APPLICATION-RESOURCES"
       "gtk4-application"
       "application-resources.lisp")
      ("Application Menubar"
       ":application"
       "APPLICATION-MENUBAR"
       "gtk4-application"
       "application-menubar.lisp")
      ("Sunny"
       ":application"
       "SUNNY"
       "gtk4-application"
       "sunny.lisp"))

     "Theming in GTK"                                       ; Title
     (("CSS Accordion"                                      ; Title
       ":WINDOW"                                            ; Type
       "DO-CSS-ACCORDION"                                   ; Function name
       "gtk4-example"                                       ; Package
       "css-accordion.lisp"                                 ; Filename
       "resource/css-accordion.css"                         ; Resources
       :keywords                                            ; Keywords
       "gtk:css-provider"
       "GtkCssProvider"
       "css-provider-load-from-path"
       "style-context-add-provider-for-display"
       "style-context-remove-provider-for-display"
       "widget-add-class")
      ("CSS Basics"
       ":WINDOW"
       "DO-CSS-BASICS"
       "gtk4-example"
       "css-basics.lisp"
       "resource/css-basics.css"
       :keywords
       "gtk:css-provider"
       "GtkCssProvider"
       "css-provider"
       )
      ("CSS Blendmodes"
       ":WINDOW"
       "DO-CSS-BLENDMODES"
       "gtk4-example"
       "css-blendmodes.lisp"
       "resource/css-blendmodes.ui"
       "resource/css-blendmodes.css")
      ("CSS Multiple Backgrounds"
       ":window"
       "DO-CSS-MULTIPLEBGS"
       "gtk4-example"
       "css-multiplebgs.lisp"
       "resource/css-multiplebgs.css")
      ("CSS Pixbufs"
       ":window"
       "DO-CSS-PIXBUFS"
       "gtk4-example"
       "css-pixbufs.lisp"
       "resource/css-pixbufs.css")
      ("CSS Shadows"
       ":window"
       "DO-CSS-SHADOWS"
       "gtk4-example"
       "css-shadows.lisp"
       "resource/css-shadows.css"))

     "Interface Builder"
     (("Builder"
       ":window"
       "DO-BUILDER"
       "gtk4-example"
       "builder.lisp"
       "resource/builder.ui"
       :keywords
       "GtkBuilder"
       "gtk:builder"))

     "Windows"
     (("Simple Window"
       ":window"
       "DO-WINDOW-SIMPLE"
       "gtk4-example"
       "window-simple.lisp"
       :keywords
       "GtkWindow"
       "gtk:window")
      ("Simple Message Dialog"
       ":dialog"
       "CREATE-MESSAGE-DIALOG-SIMPLE"
       "gtk4-example"
       "message-dialog-simple.lisp"
       :keywords
       "GtkMessageDialog"
       "gtk:message-dialog"
       "gtk:message-dialog-new"
       "gtk:message-dialog-secondary-text")
       (("Variant 1"
       ":dialog"
       "CREATE-MESSAGE-DIALOG-SIMPLE"
       "gtk4-example"
       "message-dialog-simple.lisp")
       ("Variant 2"
       ":dialog"
       "CREATE-MESSAGE-DIALOG-SIMPLE2"
       "gtk4-example"
       "message-dialog-simple.lisp"))

      ("Various Dialogs"
       ":window"
       "DO-DIALOG-VARIOUS"
       "gtk4-example"
       "dialog-various.lisp"
       "resource/dialog.ui")
      ("Alert Dialog"
       ":dialog"
       "CREATE-ALERT-DIALOG"
       "gtk4-example"
       "alert-dialog.lisp")
      ("Assistant"
       ":window"
       "DO-ASSISTANT"
       "gtk4-example"
       "assistant.lisp")
       ("Application Window"
        ":window"
        "DO-APPLICATION-WINDOW"
        "gtk4-example"
        "application-window.lisp"))

     "Layout Containers"
     (("Box Append"
       ":window"
       "DO-BOX-APPEND"
       "gtk4-example"
       "box-append.lisp"
       :keywords
       "gtk:box"
       "gtk:box-append"
       "gtk:button-new-with-label")
      ("Center Box"
       ":window"
       "DO-BOX-CENTER"
       "gtk4-example"
       "box-center.lisp"
       :keywords
       "gtk:button"
       "gtk:button-child"
       "gtk:center-box"
       "gtk:center-box-start-widget"
       "gtk:center-box-center-widget"
       "gtk:center-box-end-widget"
       "gtk:css-provider"
       "gtk:css-provider-load-from-string"
       "gtk:style-context-add-provider-for-display"
       "gtk:widget-width-request"
       "gtk:widget-height-request"
       "gtk:widget-add-css-class"
       "gtk:widget-display")
      ("Grid with spacing"
       ":window"
       "DO-GRID-SPACING"
       "gtk4-example"
       "grid-spacing.lisp")
      ("Revealer"
       ":window"
       "DO-REVEALER"
       "gtk4-example"
       "revealer.lisp")
      ("Revealer Icon"
       ":window"
       "DO-REVEALER-ICON"
       "gtk4-example"
       "revealer-icon.lisp"
       "resource/revealer-icon.ui")
      ("List Box Complex"
       ":window"
       "DO-LIST-BOX-COMPLEX"
       "gtk4-example"
       "list-box-complex.lisp"
       "resource/list-box-complex.ui"
       "resource/list-box-message.txt")
      ("List Box Controls"
       ":window"
       "DO-LIST-BOX-CONTROLS"
       "gtk4-example"
       "list-box-controls.lisp"
       "resource/list-box-controls.ui")
      ("Flow Box"
       ":window"
       "DO-FLOW-BOX"
       "gtk4-example"
       "flow-box.lisp"
       :keywords
       "GtkFlowBox"
       "GtkFlowBoxChild"
       "gtk:flow-box"
       "gtk:flow-box-child"
       "gtk:flow-box-child-child"
       "gtk:flow-box-invalidate-filter"
       "gtk:flow-box-set-filter-func"
       "gtk:flow-box-set-sort-func"
       "gtk:flow-box-append"
       "gtk:flow-box-select-child")
      ("Stack"
       ":window"
       "DO-STACK"
       "gtk4-example"
       "stack.lisp"
       "resource/stack.ui")
      ("Stack Sidebar"
       ":window"
       "DO-STACK-SIDEBAR"
       "gtk4-example"
       "stack-sidebar.lisp")
      ("Action Bar"
       ":window"
       "DO-ACTION-BAR"
       "gtk4-example"
       "action-bar.lisp"
       :keywords
       "gtk:action-bar"
       "gtk:action-bar-pack-start"
       "gtk:action-bar-pack-end"
       "gtk:action-bar-center-widget")
      ("Header Bar"
       ":window"
       "DO-HEADER-BAR"
       "gtk4-example"
       "header-bar.lisp")
      ("Interactive overlay"
       ":window"
       "DO-OVERLAY-INTERACTIVE"
       "gtk4-example"
       "overlay-interactive.lisp")
      ("Decorative overlay"
       ":window"
       "DO-OVERLAY-DECORATIVE"
       "gtk4-example"
       "overlay-decorative.lisp")
      ("Notebook"
       ":window"
       "DO-NOTEBOOK"
       "gtk4-example"
       "notebook.lisp")
      ("Paned Window"
       ":window"
       "DO-PANED"
       "gtk4-example"
       "paned.lisp")
      ("Expander"
       ":dialog"
       "CREATE-EXPANDER"
       "gtk4-example"
       "expander.lisp")
      ("Aspect Frame"
       ":window"
       "DO-ASPECT-FRAME"
       "gtk4-example"
       "aspect-frame.lisp"
       :keywords
       "gtk:aspect-frame")
      ("Fixed Container"
       ":window"
       "DO-FIXED"
       "gtk4-example"
       "fixed.lisp"
       :keywords
       "GtkFixed"
       "gtk:fixed"
       "gtk:fixed-put"
       "gtk:fixed-move"
       "gtk:fixed-child-position"
       "gtk:fixed-child-transform"
       "gtk:transform-to-string"
       "gtk:transform-category"
       "gtk:widget-width"
       "gtk:widget-height"
       ))

     ;; Layout Managers
     "Layout Managers"
     (("Fixed Layout"
       ":window"
       "DO-FIXED-LAYOUT"
       "gtk4-example"
       "fixed-layout.lisp"
       :keywords
       "GtkFixed"
       "gtk:fixed"
       "gtk:fixed-put"
       "gtk:fixed-child-transform"
       "gsk:transform"
       "gsk:transform-new"
       "gsk:transform-translate"
       "gsk:transform-translate-3d"
       "gsk:transform-perspective"
       "gsk:transform-rotate-3d"
       "graphene:point-t"
       "graphene:point3d-t"
       "graphene:point3d-init"
       "graphene:vec3-x-axis"
       "graphene:vec3-y-axis")
      ("Simple Constraints"
       ":window"
       "DO-CONSTRAINT-SIMPLE"
       "gtk4-example"
       "constraint-simple.lisp")
      ("Interactive Constraints"
       ":window"
       "DO-CONSTRAINT-INTERACTIVE"
       "gtk4-example"
       "constraint-interactive.lisp")
      ("Constraints Builder"
       ":window"
       "DO-CONSTRAINT-BUILDER"
       "gtk4-example"
       "constraint-builder.lisp"
       "resource/constraint-builder.ui"
       :keywords
       "GtkConstraintLayout"
       "gtk:constraint-layout"))

     "Display Widgets"
     (("Various Label"
       ":window"
       "DO-LABEL-VARIOUS"
       "gtk4-example"
       "label-various.lisp")
      ("Images"
       ":window"
       "DO-IMAGES"
       "gtk4-example"
       "images.lisp")
      ("Spinner"
       ":window"
       "DO-SPINNER"
       "gtk4-example"
       "spinner.lisp")
      ("Info Bar"
       ":window"
       "DO-INFO-BAR"
       "gtk4-example"
       "info-bar.lisp")
      ("Progress Bar"
       ":window"
       "DO-PROGRESS-BAR"
       "gtk4-example"
       "progress-bar.lisp")
      ("Level Bar"
       ":window"
       "DO-LEVEL-BAR"
       "gtk4-example"
       "level-bar.lisp")
      ("Statusbar"
       ":window"
       "DO-STATUSBAR"
       "gtk4-example"
       "statusbar.lisp")
      ("Calendar"
       ":window"
       "DO-CALENDAR"
       "gtk4-example"
       "calendar.lisp"
       :keywords
       "GtkCalendar"
       "gtk:calendar"
       "gtk:calendar-day"
       "gtk:calendar-month"
       "gtk:calendar-year"
       "gtk:calendar-mark-day"))

     "List-based widgets"
     (("Application Launcher"
       ":window"
       "DO-LIST-VIEW-APPLAUNCHER"
       "gtk4-example"
       "list-view-applauncher.lisp")
      ("World Clocks"
       ":window"
       "DO-GRID-VIEW-CLOCKS"
       "gtk4-example"
       "grid-view-clocks.lisp"
       :keywords
       "define-g-object-subclass"
       "gdk:paintable"
       "gdk:paintable-snapshot"
       "gtk:grid-view"
       )
; TODO: The example is not finished
;      ("Colors"
;       ":window"
;       "DO-GRID-VIEW-COLORS"
;       "gtk4-example"
;       "grid-view-colors.lisp")
      ("Drop Down"
       ":window"
       "DO-DROP-DOWN"
       "gtk4-example"
       "drop-down.lisp"))

     "Media Support"
     (("Video"
       ":window"
       "DO-VIDEO"
       "gtk4-example"
       "video.lisp"))

     "Button and Toggle Widgets"
     (("Various Buttons"
       ":window"
       "DO-BUTTON-VARIOUS"
       "gtk4-example"
       "button-various.lisp"
       :keywords
       "gtk:button"
       "gtk:button-new-with-label"
       "gtk:button-new-with-mnemonic"
       "gtk:button-new-with-icon-name"
       "gtk:size-group")
      ("Toggle Buttons"
       ":window"
       "DO-TOGGLE-BUTTON"
       "gtk4-example"
       "toggle-button.lisp")
      ("Toggle Buttons Action"
       ":window"
       "DO-TOGGLE-BUTTON-ACTION"
       "gtk4-example"
       "toggle-button-action.lisp")
      ("Check Buttons"
       ":window"
       "DO-CHECK-BUTTON"
       "gtk4-example"
       "check-button.lisp")
      ("Menu Buttons"
       ":window"
       "DO-MENU-BUTTON"
       "gtk4-example"
       "menu-button.lisp")
      ("Link Button"
       ":window"
       "DO-LINK-BUTTON"
       "gtk4-example"
       "link-button.lisp")
      ("Scale Button"
       ":window"
       "DO-SCALE-BUTTON"
       "gtk4-example"
       "scale-button.lisp")
      ("Switch"
       ":window"
       "DO-SWITCH"
       "gtk4-example"
       "switch.lisp"))

     "Numeric/Text Data Entry"
     (("Entry"
       ":window"
       "DO-ENTRY"
       "gtk4-example"
       "entry.lisp")
      ("Entry Buffer"
       ":window"
       "DO-ENTRY-BUFFER"
       "gtk4-example"
       "entry-buffer.lisp")
      ("Entry Completion"
       ":window"
       "DO-ENTRY-COMPLETION"
       "gtk4-example"
       "entry-completion.lisp")
      ("Password Entry"
       ":window"
       "DO-PASSWORD-ENTRY"
       "gtk4-example"
       "password-entry.lisp")
      ("Scale Widgets"
       ":window"
       "DO-SCALE-WIDGET"
       "gtk4-example"
       "scale-widget.lisp")
      ("Spin Button"
       ":window"
       "DO-SPIN-BUTTON"
       "gtk4-example"
       "spin-button.lisp")
      ("Search Bar"
       ":window"
       "DO-SEARCH-BAR"
       "gtk4-example"
       "search-bar.lisp"
       :keywords
       "gtk:search-bar"
       "gtk:search-entry"
       "gtk:search-bar-key-capture-widget"
       "gtk:search-bar-connect-entry"))

     "Multiline Text Editor"
     (("Simple Text View"
       ":window"
       "DO-TEXT-VIEW-SIMPLE"
       "gtk4-example"
       "text-view-simple.lisp")
      ("Text View Attributes"
       ":window"
       "DO-TEXT-VIEW-ATTRIBUTES"
       "gtk4-example"
       "text-view-attributes.lisp")
      ("Text View Tags"
       ":window"
       "DO-TEXT-VIEW-TAGS"
       "gtk4-example"
       "text-view-tags.lisp")
      ("Text View Search"
       ":window"
       "DO-TEXT-VIEW-SEARCH"
       "gtk4-example"
       "text-view-search.lisp")
      ("Hypertext"
       ":window"
       "DO-TEXT-VIEW-HYPERTEXT"
       "gtk4-example"
       "text-view-hypertext.lisp")
      ("Markup"
       ":window"
       "DO-TEXT-VIEW-MARKUP"
       "gtk4-example"
       "text-view-markup.lisp")
      ("Tabulator"
       ":window"
       "DO-TEXT-VIEW-TABS"
       "gtk4-example"
       "text-view-tabs.lisp")
      ("Multiple"
       ":window"
       "DO-TEXT-VIEW-MULTIPLE"
       "gtk4-example"
       "text-view-multiple.lisp")
      ("Undo and Redo"
       ":window"
       "DO-TEXT-VIEW-undo"
       "gtk4-example"
       "text-view-undo.lisp")
      ("Scrolling"
       ":window"
       "DO-TEXT-VIEW-SCROLL"
       "gtk4-example"
       "text-view-scroll.lisp"))

     "Tree, List and Icon Grid Widgets"
     (("Simple Tree View"
       ":window"
       "DO-TREE-VIEW-SIMPLE"
       "gtk4-example"
       "tree-view-simple.lisp")
      ("Tree View Path"
       ":window"
       "DO-TREE-VIEW-PATH"
       "gtk4-example"
       "tree-view-path.lisp")
      ("Tree View Content Type"
       ":window"
       "DO-TREE-VIEW-CONTENT-TYPE"
       "gtk4-example"
       "tree-view-content-type.lisp"))

     "Menus, Combo Box, Toolbar"
     (("Combo Box"
       ":window"
       "DO-COMBO-BOX"
       "gtk4-example"
       "combo-box.lisp"
       :keywords
       "GtkComboBox"
       "gtk:combo-box"
       "gtk:combo-box-set-row-separator-func"
       "gtk:combo-box-active-id"
       "gtk:combo-box-id-column"
       "gtk:combo-box-active")
      ("Combo Box Text"
       ":window"
       "DO-COMBO-BOX-TEXT"
       "gtk4-example"
       "combo-box-text.lisp"
       :keywords
       "GtkComboBoxText"
       "gtk:combo-box-active"
       "gtk:combo-box-child"
       "gtk:combo-box-text"
       "gtk:combo-box-text-append-text"
       "gtk:combo-box-text-active-text"
       "gtk:combo-box-text-remove"))

     "Selectors (Color/File/Font)"
     (("Color Button"
       ":window"
       "DO-COLOR-BUTTON"
       "gtk4-example"
       "color-button.lisp")
      ("Color Button Label"
       ":window"
       "DO-COLOR-BUTTON-LABEL"
       "gtk4-example"
       "color-button-label.lisp")
      ("Color Chooser Dialog"
       ":window"
       "DO-COLOR-CHOOSER-DIALOG"
       "gtk4-example"
       "color-chooser-dialog.lisp")
      ("Color Chooser Widget"
       ":window"
       "DO-COLOR-CHOOSER-WIDGET"
       "gtk4-example"
       "color-chooser-widget.lisp")
      ("Color Chooser Palette"
       ":window"
       "DO-COLOR-CHOOSER-PALETTE"
       "gtk4-example"
       "color-chooser-palette.lisp")
      ("File Chooser Dialog"
       ":dialog"
       "CREATE-FILE-CHOOSER-DIALOG"
       "gtk4-example"
       "file-chooser-dialog.lisp"
       :keywords
       "GtkFileChooserDialog"
       "gtk:file-chooser-dialog")
      ("File Chooser Native"
       ":dialog"
       "CREATE-FILE-CHOOSER-NATIVE"
       "gtk4-example"
       "file-chooser-native.lisp"
       :keywords
       "GtkFileChooserNative"
       "gtk:file-chooser-native")
      ("Font Button"
       ":window"
       "DO-FONT-BUTTON"
       "gtk4-example"
       "font-button.lisp")
      ("Font Button Label"
       ":window"
       "DO-FONT-BUTTON-LABEL"
       "gtk4-example"
       "font-button-label.lisp"))

     "Widgets for custom drawing"
     (("Drawing Area"
       ":window"
       "DO-DRAWING-AREA"
       "gtk4-example"
       "drawing-area.lisp")
      ("Scribble on Drawing Area"
       ":window"
       "DO-DRAWING-AREA-SCRIBBLE"
       "gtk4-example"
       "drawing-area-scribble.lisp"
       :keywords
       "GtkDrawingArea"
       "gtk:drawing-area"))

     "Ornaments"
     (("Frame"
       ":window"
       "DO-FRAME"
       "gtk4-example"
       "frame.lisp")
      ("Frame Properties"
       ":window"
       "DO-FRAME-PROPERTIES"
       "gtk4-example"
       "frame-properties.lisp"))

     "Scrolling"
     (("Scrolled Window"
       ":window"
       "DO-SCROLLED-WINDOW"
       "gtk4-example"
       "scrolled-window.lisp"))

     "Printing"
     (#-windows
      ("Page Setup Dialog"
       ":dialog"
       "CREATE-PAGE-SETUP-DIALOG"
       "gtk4-example"
       "page-setup-dialog.lisp"
       "resource/page-setup.ini"))

     "Drag and Drop, Clipboard"
     (("Clipboard"
       ":window"
       "DO-CLIPBOARD"
       "gtk4-example"
       "clipboard.lisp"
       "resource/clipboard.ui")
     )

     "Paintable"
     (("Nuclear Icon"
       ":window"
       "DO-PAINTABLE-SIMPLE"
       "gtk4-example"
       "paintable-simple.lisp")
      ("Nuclear Animation"
       ":window"
       "DO-PAINTABLE-ANIMATION"
       "gtk4-example"
       "paintable-animation.lisp")
      ("Emblemed Icons"
       ":window"
       "DO-PAINTABLE-EMBLEM"
       "gtk4-example"
       "paintable-emblem.lisp"))

     "Subclassing"
     (("GtkFigure subclass"
       ":window"
       "DO-SUBCLASSING-FIGURE"
       "gtk4-example"
       "subclassing-figure.lisp"))

     ;; Pango demos
     "Pango"
     (("Draw centered text"
       ":drawfunc"
       "pango-draw-text-centered"
       "pango-example"
       "text-centered.lisp")
      ("Draw text metrics"
       ":drawfunc"
       "pango-draw-text-metrics"
       "pango-example"
       "text-metrics.lisp")
      ("Draw text soulmate"
       ":drawfunc"
       "pango-draw-text-soulmate"
       "pango-example"
       "text-soulmate.lisp")
      ("Pango Cairo rendering"
       ":drawfunc"
       "pango-draw-cairo-rendering"
       "pango-example"
       "cairo-rendering.lisp"))

     ;; Cairo demos
     "Cairo"
     ("Examples from cairographics.org"
      (("Arc"
        ":drawfunc"
        "draw-arc"
        "cairo-example"
        "draw-arc.lisp")
       ("Arc Negative"
        ":drawfunc"
        "draw-arc-negative"
        "cairo-example"
        "draw-arc-negative.lisp")
       ("Clip"
        ":drawfunc"
        "draw-clip"
        "cairo-example"
        "draw-clip.lisp")
       ("Clip Image"
        ":drawfunc"
        "draw-clip-image"
        "cairo-example"
        "draw-clip-image.lisp")
       ("Curve To"
        ":drawfunc"
        "draw-curve-to"
        "cairo-example"
        "draw-curve-to.lisp")
       ("Dash"
        ":drawfunc"
        "draw-dash"
        "cairo-example"
        "draw-path.lisp")
       ("Preserve Fill"
        ":drawfunc"
        "draw-fill-preserve"
        "cairo-example"
        "draw-fill-preserve.lisp")
       ("Fill Style"
        ":drawfunc"
        "draw-fill-style"
        "cairo-example"
        "draw-fill-style.lisp")
       ("Gradient"
        ":drawfunc"
        "draw-gradient"
        "cairo-example"
        "draw-gradient.lisp")
       ("Image"
        ":drawfunc"
        "draw-image"
        "cairo-example"
        "draw-image.lisp")
       ("Image Pattern"
        ":drawfunc"
        "draw-image-pattern"
        "cairo-example"
        "draw-image-pattern.lisp")
       ("Multi Caps"
        ":drawfunc"
        "draw-multi-caps"
        "cairo-example"
        "draw-multi-caps.lisp")
       ("Rounded Rectangle"
        ":drawfunc"
        "draw-rounded-rectangle"
        "cairo-example"
        "draw-rounded-rectangle.lisp")
       ("Line Cap"
        ":drawfunc"
        "draw-line-cap"
        "cairo-example"
        "draw-line-cap.lisp")
       ("Line Join"
        ":drawfunc"
        "draw-line-join"
        "cairo-example"
        "draw-line-join.lisp")
       ("Text"
        ":drawfunc"
        "draw-text"
        "cairo-example"
        "draw-text.lisp")
       ("Text Align Center"
        ":drawfunc"
        "draw-text-align-center"
        "cairo-example"
        "draw-text-align-center.lisp")
       ("Text Extents"
        ":drawfunc"
        "draw-text-extents"
        "cairo-example"
        "draw-text-extents.lisp"))

      "More Examples"
      (("Cairo Stroke"
       ":drawfunc"
       "cairo-draw-stroke"
       "cairo-example"
       "draw-stroke.lisp")
       ("Cairo Fill"
        ":drawfunc"
        "cairo-draw-fill"
        "cairo-example"
        "draw-fill.lisp")
       ("Cairo Text Letter"
        ":drawfunc"
        "cairo-draw-text-letter"
        "cairo-example"
        "draw-text-letter.lisp")
       ("Cairo Paint"
        ":drawfunc"
        "cairo-draw-paint"
        "cairo-example"
        "draw-paint.lisp")
       ("Cairo Mask"
        ":drawfunc"
        "cairo-draw-mask"
        "cairo-example"
        "draw-mask.lisp")
       ("Cairo Source RGBA"
        ":drawfunc"
        "cairo-draw-source-rgba"
        "cairo-example"
        "draw-source-rgba.lisp")
       ("Cairo Source Gradient"
        ":drawfunc"
        "cairo-draw-source-gradient"
        "cairo-example"
        "draw-source-gradient.lisp")
       ("Cairo Path"
        ":drawfunc"
        "cairo-draw-path"
        "cairo-example"
        "draw-path.lisp")
       ("Cairo Dashes"
        ":drawfunc"
        "cairo-draw-dashes"
        "cairo-example"
        "draw-dashes.lisp")
       ("Cairo Joins"
        ":drawfunc"
        "cairo-draw-joins"
        "cairo-example"
        "draw-joins.lisp")
       ("Cairo Text centered"
        ":drawfunc"
        "cairo-draw-text-centered"
        "cairo-example"
        "draw-text-centered.lisp")
       ("Cairo Text Glyph"
        ":drawfunc"
        "cairo-draw-text-glyph"
        "cairo-example"
        "draw-text-glyph.lisp")
       ("Cairo Text Gradient"
        ":drawfunc"
        "cairo-draw-text-gradient"
        "cairo-example"
        "draw-text-gradient.lisp")
       ("Cairo Text Shaded"
        ":drawfunc"
        "cairo-draw-text-shaded"
        "cairo-example"
        "draw-text-shaded.lisp")
       ("Cairo Text Soulmate"
        ":drawfunc"
        "cairo-draw-text-soulmate"
        "cairo-example"
        "draw-text-soulmate.lisp")
       ("Cairo Draw Logo"
        ":drawfunc"
        "cairo-draw-logo"
        "cairo-example"
        "draw-logo.lisp")
       ("Cairo Draw Logo Translate"
        ":drawfunc"
        "cairo-draw-logo-translate"
        "cairo-example"
        "draw-logo.lisp")))
     "Miscellaneous"
     (("Cursors"
       ":window"
       "DO-CURSORS"
       "gtk4-example"
       "cursors.lisp"
       "resource/cursors.ui"
       :keywords
       "GdkCursor"
       "gdk:cursor")
      ("Size Groups"
       ":window"
       "DO-SIZE-GROUP"
       "gtk4-example"
       "size-group.lisp")
      ("GdkAppLaunchContext"
       ":window"
       "DO-APP-LAUNCH-CONTEXT"
       "gtk4-example"
       "app-launch-context.lisp")
      ("GdkAppLaunchContext Asyn"
       ":window"
       "DO-APP-LAUNCH-CONTEXT-ASYNC"
       "gtk4-example"
       "app-launch-context.lisp"))
))
