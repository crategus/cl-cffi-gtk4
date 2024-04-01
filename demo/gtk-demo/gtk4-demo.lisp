;;;; GTK 4 Demo
;;;;
;;;; GTK Demo is a collection of useful examples to demonstrate GTK widgets
;;;; and features. It is a useful example in itself.
;;;;
;;;; You can select examples in the sidebar or search for them by typing a
;;;; search term. Double-clicking or hitting the "Run" button will run the
;;;; demo. The source code and other resources used in the demo are shown in
;;;; this area.
;;;;
;;;; You can also use the GTK Inspector, available from the menu on the top
;;;; right, to poke at the running demos, and see how they are put together.
;;;;
;;;; 2024-3-30

(defpackage :gtk4-demo
  (:use :common-lisp)
  (:export :gtk4-demo))

(in-package :gtk4-demo)

;; Globals
(defvar *infoview* nil)
(defvar *sourceview* nil)
(defvar *currentfile* nil)
(defvar *notebook* nil)
(defvar *selection* nil)
(defvar *searchneedle* nil)

(defparameter *gtk-demos*
   '(("GTK 4 Lisp Demo"
      nil
      nil
      "gtk4-demo"
      "gtk4-demo.lisp"
      "main.ui"
      "main-listitem.ui")
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
       )
      ("CSS Basics"
       ":WINDOW"
       "DO-CSS-BASICS"
       "gtk4-example"
       "css-basics.lisp"
       "resource/css-basics.css"
       :keywords
       "gtk:css-provider"
       "GtkCssProvider")
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

     "Windows"
     (("Simple Window"
       ":window"
       "DO-WINDOW-SIMPLE"
       "gtk4-example"
       "window-simple.lisp"
       :keywords
       "GtkWindow" "gtk:window"
       )
      ("Simple Message Dialog"
       ":dialog"
       "CREATE-MESSAGE-DIALOG-SIMPLE"
       "gtk4-example"
       "message-dialog-simple.lisp")
      ("Simple Message Dialog with Constructor"
       ":dialog"
       "CREATE-MESSAGE-DIALOG-SIMPLE2"
       "gtk4-example"
       "message-dialog-simple.lisp")
      ("Various Dialogs"
       ":window"
       "DO-DIALOG-VARIOUS"
       "gtk4-example"
       "dialog-various.lisp")
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
       "box-append.lisp")
      ("Center Box"
       ":window"
       "DO-BOX-CENTER"
       "gtk4-example"
       "box-center.lisp")
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
      ("Paned Widgets"
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
       "aspect-frame.lisp")
      ("Fixed Container"
       ":window"
       "DO-FIXED"
       "gtk4-example"
       "fixed.lisp"))

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
       "calendar.lisp"))

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
       "grid-view-clocks.lisp")
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
     (("More Buttons"
       ":window"
       "DO-BUTTON-MORE"
       "gtk4-example"
       "button-more.lisp")
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
       "search-bar.lisp")
       )

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
       "combo-box.lisp")
      ("Combo Box Text"
       ":window"
       "DO-COMBO-BOX-TEXT"
       "gtk4-example"
       "combo-box-text.lisp"))

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
       "file-chooser-dialog.lisp")
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
       "drawing-area.lisp"))

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
     (("Size Groups"
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

;;; ----------------------------------------------------------------------------

(gobject:define-g-object-subclass "GtkDemo" gtk-demo
  (:superclass g:object
   :export t
   :interfaces ())
  ((title
    gtk-demo-title
    "title" "gchararray" t t)
   (ftype
    gtk-demo-ftype
    "ftype" "gchararray" t t)
   (name
    gtk-demo-name
    "name" "gchararray" t t)
   (package
    gtk-demo-package
    "package" "gchararray" t t)
   (filename
    gtk-demo-filename
    "filename" "gchararray" t t)
   (resources
    gtk-demo-resources
    "resources" "GStrv" t t)
   (keywords
    gtk-demo-keywords
    "keywords" "GStrv" t t)
   (children
    gtk-demo-children
    "children" "GListStore" t t)))

;;; ----------------------------------------------------------------------------

;; Create the list store as the model for the data

(defun mklist (obj)
  (if (listp obj)
      obj
      (list obj)))

(defun create-and-fill-list-store (data &optional (parent nil))
  (let ((model (g:list-store-new "GtkDemo")))
    (dolist (entry (mklist data))
      (cond ((or (atom entry) (every #'atom entry))
             (destructuring-bind (title &optional ftype name package file
                                        &rest args)
                 (mklist entry)
               (let* (;; Extract keywords from args
                      (keywords (rest (member :keywords args)))
                      ;; More keywords from the title
                      (keywords2 (split-sequence:split-sequence #\space title))
                      ;; Extract resources from args
                      (resources (if keywords
                                     (butlast args (1+ (length keywords)))
                                     args))
                      ;; Create GtkDemo object
                      (item (make-instance 'gtk-demo
                                           :title title
                                           :ftype ftype
                                           :name name
                                           :package package
                                           :filename file
                                           :resources resources
                                           :keywords
                                           (append keywords keywords2)
                                           :children nil)))
                 (setf parent item)
                 (g:list-store-append model item))))
            ((some #'listp entry)
             (let ((model (create-and-fill-list-store entry parent)))
               (setf (gtk-demo-children parent) model)))))
  model))

(defun find-demo-by-name (data name &optional result)
  (dolist (entry (mklist data))
    (cond ((or (atom entry) (every #'atom entry))
           (destructuring-bind (title
                                &optional ftype name1 package file
                                &rest args)
               (mklist entry)
             (declare (ignore args))
             (when (string-equal name name1)
               (setf result (make-instance 'gtk-demo
                                           :title title
                                           :ftype ftype
                                           :name name
                                           :package package
                                           :filename file
                                           :children nil))
               (return))))
          ((some #'listp entry)
           (setf result (find-demo-by-name entry name result)))))
    result)

(defun dump-list-store (model &optional (prefix ""))
  (dotimes (i (g:list-store-n-items model))
    (let ((object (g:list-model-object model i)))
      (format t "~a~a~%" prefix (gtk-demo-title object))
    (when (gtk-demo-children object)
      (let ((model (gtk-demo-children object)))
      (dump-list-store model (concatenate 'string "  " prefix)))))))

;;; ----------------------------------------------------------------------------

;; Load selected source file and the resources in notebook tabs

;; TODO: Complete the implementation and rewrite the SELECTION-CB and
;; ADD-DATA-TABS functions more elegantly

(defun sys-path (filename &optional (package :gtk4-demo))
  (let ((system-path (asdf:system-source-directory package)))
    (princ-to-string (merge-pathnames filename system-path))))

(defun clear-buffer (buffer)
  (multiple-value-bind (start end)
      (gtk:text-buffer-bounds buffer)
    (gtk:text-buffer-delete buffer start end)))

(defun load-file (filename)
  (let ((sourcebuffer (gtk:text-buffer-new))
        (infobuffer (gtk:text-buffer-new)))
    (gtk:text-buffer-create-tag infobuffer "title"
                                :size (* 12 pango:+scale+)
                                :weight 700
                                :pixels-below-lines 3)
    (with-open-file (stream filename)
      ;; Read the info-header of the file
      (clear-buffer infobuffer)
      (let ((firstline t)
            (startiter (gtk:text-buffer-start-iter infobuffer)))
        (do ((line (read-line stream nil)
                   (read-line stream nil)))
            ((or (null line)
                 (not (>= (length line) 4))
                 (not (string= line ";;;;" :start1 0 :end1 4))))
          (cond (firstline
                 (setf firstline nil)
                 (gtk:text-buffer-insert-with-tags infobuffer
                                                   startiter
                                                   (string-left-trim ";" line)
                                                   "title"))
                (t
                 (gtk:text-buffer-insert infobuffer
                                         :cursor
                                         (string-left-trim ";" line))))
          (gtk:text-buffer-insert infobuffer :cursor (format nil "~%"))))
      ;; Read the source code of the file
      (clear-buffer sourcebuffer)
      (do ((line (read-line stream nil)
                 (read-line stream nil)))
          ((null line))
        (gtk:text-buffer-insert sourcebuffer :cursor line)
        (gtk:text-buffer-insert sourcebuffer :cursor (format nil "~%")))
      (setf (gtk:text-view-buffer *sourceview*) sourcebuffer)
      (setf (gtk:text-view-buffer *infoview*) infobuffer))))

(defun load-file-buffer (buffer filename)
  (with-open-file (stream filename)
    (clear-buffer buffer)
    (do ((line (read-line stream nil)
               (read-line stream nil)))
        ((null line))
      (gtk:text-buffer-insert buffer :cursor line)
      (gtk:text-buffer-insert buffer :cursor (format nil "~%")))))

(defun add-data-tabs (demo)
  (let ((resources (gtk-demo-resources demo)))
    ;; Remove existing data tabs from the notebook
    (dotimes (i (- (gtk:notebook-n-pages *notebook*) 2))
      (gtk:notebook-remove-page *notebook* 2))
    (dolist (resource resources)
      (let ((file (sys-path resource (gtk-demo-package demo))))
        (cond ((member (pathname-type file) '("css" "ui") :test #'string-equal)
               (let* ((buffer (gtk:text-buffer-new))
                      (view (make-instance 'gtk:text-view
                                           :buffer buffer
                                           :left-margin 12
                                           :right-margin 12
                                           :top-margin 12
                                           :bottom-margin 12
                                           :pixels-above-lines 2
                                           :pixels-below-lines 2
                                           :monospace t))
                      (scrolled (make-instance 'gtk:scrolled-window
                                               :child view))
                      (label (make-instance 'gtk:label
                                            :label (file-namestring file))))
                 (load-file-buffer buffer file)
                 (gtk:notebook-add-page *notebook* scrolled label)))
              (t
               ;; Add code to display images and videos
               (format t " not implemented for type ~a~%"
                         (pathname-type file))))))))

(defun selection-cb (window selection)
  (let* ((row (gtk:single-selection-selected-item selection))
         (demo (if row (gtk:tree-list-row-item row))))
    (when (and demo (gtk-demo-filename demo))
      (load-file (sys-path (gtk-demo-filename demo) (gtk-demo-package demo)))
      (add-data-tabs demo)
      (setf (gtk:window-title window) (gtk-demo-title demo)))))

;;; ----------------------------------------------------------------------------

;; Run a demo

(defun window-draw-func (title drawfunc application
                         &optional (width 600) (height 600))
  (let* ((area (make-instance 'gtk:drawing-area))
         (window (make-instance 'gtk:window
                                 :application application
                                 :child area
                                 :title title
                                 :default-width width
                                 :default-height height)))
    ;; Set the drawing function
    (gtk:drawing-area-set-draw-func area
        (lambda (widget cr width height)
          (declare (ignore widget))
          (funcall drawfunc cr width height)))
    ;; Show the window.
    (setf (gtk:widget-visible window) t)))

(defun gtk-demo-run (demo application)
  (let* ((title (gtk-demo-title demo))
         (functype (read-from-string (gtk-demo-ftype demo)))
         (funcname (string-upcase (gtk-demo-name demo)))
         (package  (string-upcase (gtk-demo-package demo)))
         (func (find-symbol funcname (find-package package))))
    (cond (;; Example as a window for application
           (eq functype :window)
           (funcall func application))
          (;; Example as a parent to the active application window
           (eq functype :dialog)
           (funcall func (gtk:application-active-window application)))
          (;; Example called from a draw handler
           (eq functype :drawfunc)
           (window-draw-func title func application))
          (t (format t "NO function found.")))))

;;; ----------------------------------------------------------------------------

;; Callback functions for actions

(defun activate-about (action parameter)
  (declare (ignore action parameter))
  (let ((version (asdf:component-version (asdf:find-system :gtk4-demo))))
    (gtk:show-about-dialog nil
                           :modal t
                           :program-name "GTK4 Lisp Demo"
                           :version version
                           :copyright "(c) Dieter Kaiser"
                           :website "github.com/crategus/cl-cffi-gtk4"
                           :website-label "Project web site"
                           :license "MIT"
                           :authors '("Dieter Kaiser")
                           :documenters '("Dieter Kaiser")
                           :artists '("None")
                           :logo-icon-name "applications-development"
                           :wrap-license t)))

(defun activate-quit (application)
  (let ((windows (gtk:application-windows application)))
    (dolist (window windows)
      (gtk:window-destroy window))))

(defun activate-inspector (action parameter)
  (declare (ignore action parameter))
  (gtk:window-set-interactive-debugging t))

(defun activate-run (application action parameter)
  (declare (ignore action parameter))
  (let* ((row (gtk:single-selection-selected-item *selection*))
         (demo (gtk:tree-list-row-item row)))
    (when (gtk-demo-name demo)
      (gtk-demo-run demo application))))

(defun activate-cb (listview position application)
  (let* ((model (gtk:list-view-model listview))
         (row (g:list-model-object model position))
         (demo (gtk:tree-list-row-item row)))
    (when (gtk-demo-name demo)
      (gtk-demo-run demo application))))

;;; ----------------------------------------------------------------------------

;; Functions for searching examples

(defun clear-search (searchbar filter)
  (unless (gtk:search-bar-search-mode-enabled searchbar)
    (let ((entry (gtk:search-bar-child searchbar)))
      (setf *searchneedle* nil)
      (setf (gtk:editable-text entry) "")
      (gtk:filter-changed filter :different))))

(defun demo-search-changed-cb (entry filter)
  (let ((text (gtk:editable-text entry)))
    (if (= 0 (length text))
        (setf *searchneedle* nil)
        (setf *searchneedle* (split-sequence:split-sequence #\space text)))
    (gtk:filter-changed filter :different)))

(defun demo-filter-by-name (row)
  (let* ((demo (gtk:tree-list-row-item row))
         (keywords (gtk-demo-keywords demo)))
    (subsetp *searchneedle* keywords :test #'string-equal)))

;;; ----------------------------------------------------------------------------

;; Callback functions for application handler

(defun get-child-model (object)
  (when (gtk-demo-children object)
    ;; We have to increase the reference counter
    ;; Can we improve the implementation at another place?
    (gobject:object-ref (gtk-demo-children object))))

(defun activate (application)
  ;; Register the resources
  (gio:with-g-resources (resource (sys-path "gtk4-demo.gresource"))
    (let* ((builder (gtk:builder-new-from-resource "/ui/main.ui"))
           (window (gtk:builder-object builder "window"))
           (listview (gtk:builder-object builder "listview"))
           (searchbar (gtk:builder-object builder "searchbar"))
           (action (g:simple-action-new "run" nil))
           (listmodel (create-and-fill-list-store *gtk-demos*))
           (treemodel (gtk:tree-list-model-new listmodel
                                               nil
                                               t
                                               #'get-child-model))
           (filter (gtk:custom-filter-new #'demo-filter-by-name))
           (filtermodel (gtk:filter-list-model-new treemodel filter))
           (selection (gtk:single-selection-new filtermodel))
           (searchentry (gtk:builder-object builder "search-entry")))
      (gtk:application-add-window application window)
      (g:signal-connect window "close-request"
                        (lambda (window)
                          (declare (ignore window))
                          (activate-quit application)))
      (g:signal-connect action "activate"
                        (lambda (action parameter)
                          (activate-run application action parameter)))
      (g:action-map-add-action window action)
      (setf *notebook* (gtk:builder-object builder "notebook"))
      (setf *infoview* (gtk:builder-object builder "info-textview"))
      (setf *sourceview* (gtk:builder-object builder "source-textview"))
      (setf *selection* selection)
      (g:signal-connect listview "activate"
                        (lambda (listview position)
                          (activate-cb listview position application)))
      (g:signal-connect searchbar "notify::search-mode-enabled"
                        (lambda (searchbar pspec)
                          (declare (ignore pspec))
                          (clear-search searchbar filter)))
      (g:signal-connect searchentry "search-changed"
                        (lambda (entry)
                          (demo-search-changed-cb entry filter)))
      (g:signal-connect selection "notify::selected-item"
                        (lambda (selection pspec)
                          (declare (ignore pspec))
                          (selection-cb window selection)))
      (setf (gtk:list-view-model listview) selection)
      (selection-cb window selection))))

(defun command-line (application cmdline)
  (let ((options (g:application-command-line-options-dict cmdline))
        (status 0))
    ;; Activate the application
    (g:application-activate application)
    ;; Check for options LIST and RUN
    (cond ((g:variant-dict-contains options "list")
           (dump-list-store (create-and-fill-list-store *gtk-demos*))
           (activate-quit application))
          ((g:variant-dict-contains options "run")
           (let* ((variant (g:variant-dict-lookup-value options "run" "s"))
                  (name (g:variant-string variant))
                  (demo (find-demo-by-name *gtk-demos* name))
                  (functype (when demo
                                  (read-from-string (gtk-demo-ftype demo)))))
             (cond ((and demo (eq :window functype))
                    (gtk-demo-run demo application)
                    (let ((window (gtk:application-active-window application)))
                      (g:signal-connect window "close-request"
                                        (lambda (window)
                                          (declare (ignore window))
                                          (activate-quit application)))))
                   ((and demo (eq :dialog functype))
                    (let ((window (make-instance 'gtk:application-window
                                                 :application application
                                                 :title (gtk-demo-title demo)
                                                 :default-width 600
                                                 :default-height 400)))
                     (setf (gtk:widget-visible window) t)
                     (gtk-demo-run demo application)
                     (g:signal-connect window "close-request"
                                       (lambda (window)
                                         (declare (ignore window))
                                         (activate-quit application)))))
                   ((and demo (eq :drawfunc functype))
                    (gtk-demo-run demo application)
                    (let ((window (gtk:application-active-window application)))
                      (g:signal-connect window "close-request"
                                        (lambda (window)
                                          (declare (ignore window))
                                          (activate-quit application)))))
                   (t
                    (format t "Example ~a not found~%" (string-upcase name))
                    (g:application-quit application)))))
           (t
            ;; Show the application window
            (gtk:window-present (gtk:application-active-window application))))
    ;; Option AUTOQUIT
    (when (g:variant-dict-contains options "autoquit")
      (g:timeout-add-seconds 3
                             (lambda ()
                               (activate-quit application)
                               glib:+g-source-remove+)))
    status))

(defun local-options (application options)
  (declare (ignore application))
  (let ((status -1))
    (when (g:variant-dict-contains options "version")
      (format t "GTK 4 Demo Version ~a~%"
                (asdf:component-version (asdf:find-system :gtk4-demo)))
      (setf status 0))
    status))

;;; ----------------------------------------------------------------------------

(defun gtk4-demo (&rest argv)
  (let* (;; Create the application
         (app (make-instance 'gtk:application
                             :application-id "com.crategus.gtk4-demo"
                             :flags :handles-command-line))
         ;; Get the command line arguments
         (argv (cons "gtk4-demo" (or argv (uiop:command-line-arguments))))
         ;; Define action entries
         (entries (list (list "about" #'activate-about)
                        (list "quit"
                              (lambda (action parameter)
                                (declare (ignore action parameter))
                                (activate-quit app)))
                        (list "inspector" #'activate-inspector)))
         ;; Define options
         (options (list (list "version"                     ; long name
                              #\v                           ; short name
                              :none                         ; flags
                              :none                         ; arg
                              nil                           ; arg data
                              "Show program version"        ; description
                              nil)                          ; arg description
                        (list "run" #\r
                              :none
                              :string
                              nil
                              "Run Example"
                              "EXAMPLE")
                         (list "list" #\l
                               :none
                               :none
                               nil
                               "List examples"
                               nil)
                         (list "autoquit" #\a
                               :none
                               :none
                               nil
                               "Quit after a delay"
                               nil))))
    ;; Add actions entries for the application
    (g:action-map-add-action-entries app entries)
    ;; Add accelerators for the application
    (setf (gtk:application-accels-for-action app "app.about") "F1")
    (setf (gtk:application-accels-for-action app "app.quit") "<Control>q")
    (setf (gtk:application-accels-for-action app "app.inspector") "<Control>d")
    ;; Add comand line options
    (g:application-add-main-option-entries app options)
    ;; Connect signal handlers for the application
    (g:signal-connect app "activate" #'activate)
    (g:signal-connect app "command-line" #'command-line)
    (g:signal-connect app "handle-local-options" #'local-options)
    ;; Run the application
    (g:application-run app argv)))

;;; --- End of file gtk4-demo.lisp ---------------------------------------------
