;;; ----------------------------------------------------------------------------
;;; gtk4-demo.lisp
;;;
;;; Copyright (C) 2013 - 2023 Dieter Kaiser
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

(defpackage :gtk4-demo
  (:use :common-lisp)
  (:export :gtk4-demo))

(in-package :gtk4-demo)

;;; ----------------------------------------------------------------------------

(defvar info-buffer (make-instance 'gtk:text-buffer))
(defvar source-buffer (make-instance 'gtk:text-buffer))
(defvar ui-buffer (make-instance 'gtk:text-buffer))
(defvar css-buffer (make-instance 'gtk:text-buffer))

;;; ----------------------------------------------------------------------------

(defun sys-path (filename &optional (package :gtk4-demo))
  (let ((system-path (asdf:system-source-directory package)))
    (princ-to-string (merge-pathnames filename system-path))))

(defun clear-buffer (buffer)
  (multiple-value-bind (start end)
      (gtk:text-buffer-bounds buffer)
    (gtk:text-buffer-delete buffer start end)))

(defun load-file (filename)
  (with-open-file (stream filename)
    ;; Read the info-header of the file
    (clear-buffer info-buffer)
    (do ((line (read-line stream nil)
               (read-line stream nil)))
        ((or (null line)
             (not (>= (length line) 4))
             (not (string= line ";;;;" :start1 0 :end1 4))))
      (gtk:text-buffer-insert info-buffer (string-left-trim ";" line))
      (gtk:text-buffer-insert info-buffer (format nil "~%")))
    ;; Read the source code of the file
    (clear-buffer source-buffer)
    (do ((line (read-line stream nil)
               (read-line stream nil)))
        ((null line))
      (gtk:text-buffer-insert source-buffer line)
      (gtk:text-buffer-insert source-buffer (format nil "~%")))))

(defun load-file-buffer (buffer filename)
  (with-open-file (stream filename)
    (clear-buffer buffer)
    (do ((line (read-line stream nil)
               (read-line stream nil)))
        ((null line))
      (gtk:text-buffer-insert buffer line)
      (gtk:text-buffer-insert buffer (format nil "~%")))))

(defun read-file (filename)
  (with-open-file (instream filename :direction :input :if-does-not-exist nil)
    (when instream
      (let ((string (make-string (file-length instream))))
        (read-sequence string instream)
        string))))

;;; ----------------------------------------------------------------------------

(defun create-text (buffer is-source)
  (let* ((view (make-instance 'gtk:text-view
                              :buffer buffer
                              :editable nil
                              :cursor-visible nil))
         (scrolled (make-instance 'gtk:scrolled-window
                                  :child view
                                  :hscrollbar-policy :automatic
                                  :vscrollbar-policy :automatic)))
    (when is-source
      (setf (gtk:text-view-monospace view) t))
    ;; return the scrolled window
    scrolled))

;;; ----------------------------------------------------------------------------

(defparameter coltitle 0)
(defparameter coltype 1)
(defparameter colfunc 2)
(defparameter colpackage 3)
(defparameter colfile 4)
(defparameter colui 5)
(defparameter colcss 6)

(defparameter *tree-model*
   '(("gchararray"        ; Title
      "gchararray"        ; Type
      "gchararray"        ; Function name
      "gchararray"        ; Package
      "gchararray"        ; Filename
      "gchararray"        ; UI Definition
      "gchararray")       ; CSS Definition

     "Theming in GTK"
     (("CSS Accordion"
       ":WINDOW"
       "DO-CSS-ACCORDION"
       "gtk4-example"
       "css-accordion.lisp"
       ""
       "resource/css-accordion.css")
      ("CSS Basics"
       ":WINDOW"
       "DO-CSS-BASICS"
       "gtk4-example"
       "css-basics.lisp"
       ""
       "resource/css-basics.css")
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
       ""
       "resource/css-multiplebgs.css")
      ("CSS Pixbufs"
       ":window"
       "DO-CSS-PIXBUFS"
       "gtk4-example"
       "css-pixbufs.lisp"
       ""
       "resource/css-pixbufs.css")
      ("CSS Shadows"
       ":window"
       "DO-CSS-SHADOWS"
       "gtk4-example"
       "css-shadows.lisp"
       ""
       "resource/css-shadows.css"))

     "Windows"
     (("Simple Window"
       ":window"
       "DO-WINDOW-SIMPLE"
       "gtk4-example"
       "window-simple.lisp")
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
      ("Assistant"
       ":window"
       "DO-ASSISTANT"
       "gtk4-example"
       "assistant.lisp"))

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
       "text-view-search.lisp"))

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
      ("Pango Cario rendering"
       ":drawfunc"
       "pango-draw-cairo-rendering"
       "pango-example"
       "cairo-rendering.lisp"))

     ;; Cairo demos
     "Cairo"
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
      ("Cairo Text"
       ":drawfunc"
       "cairo-draw-text"
       "cairo-example"
       "draw-text.lisp")
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
      ("Cairo Dash"
       ":drawfunc"
       "cairo-draw-dash"
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
       "draw-logo.lisp"))

     "Miscellaneous"
     (("Size Groups"
       ":window"
       "DO-SIZE-GROUP"
       "gtk4-example"
       "size-group.lisp"))
))

;;; ----------------------------------------------------------------------------

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

;;; ----------------------------------------------------------------------------

(defun mklist (obj)
  (if (listp obj)
      obj
      (list obj)))

(defun create-and-fill-tree-store (data &optional (model nil) (iter nil))
  (unless model
    (setf model (apply #'gtk:tree-store-new (mklist (first data))))
    (setf data (rest data)))
  (let ((parent iter))
    (dolist (entry (mklist data))
      (cond ((or (atom entry) (every #'atom entry)) ; entry is never an atom?
             (setf parent
                   (apply #'gtk:tree-store-set model
                                               (gtk:tree-store-append model
                                                                      iter)
                                               (mklist entry))))
            ((some #'listp entry)
             (create-and-fill-tree-store entry
                                         model
                                         parent)))))
  model)

;;; ----------------------------------------------------------------------------

(defun create-view-and-model (&optional application)
  (let* ((model (create-and-fill-tree-store *tree-model*))
         (view (make-instance 'gtk:tree-view
                              :model model))
         (selection (gtk:tree-view-selection view)))
    ;; Create renderers for the cells
    (let* ((renderer (gtk:cell-renderer-text-new))
           (column (gtk:tree-view-column-new-with-attributes "Example"
                                                             renderer
                                                             "text" 0)))
      (gtk:tree-view-append-column view column))

    #+nil
    (g:signal-connect view "row-activated"
       (lambda (tree-view path column)
         (declare (ignore column))
         (let* ((model (gtk:tree-view-model tree-view))
                (iter (gtk:tree-model-iter model path))
                (func-name (gtk:tree-model-value model iter colfunc))
                (func (and (stringp func-name)
                           (find-symbol func-name :gtk4-demo))))
           (if func
               (if (gtk:tree-model-value model iter coltype)
                   (funcall func application)
                   (funcall func))))))

    (g:signal-connect view "row-activated"
       (lambda (view path column)
         (declare (ignore column))
         (let* ((model (gtk:tree-view-model view))
                (iter (gtk:tree-model-iter model path))
                (title (gtk:tree-model-value model iter coltitle))
                (funcname (gtk:tree-model-value model iter colfunc))
                (functype (gtk:tree-model-value model iter coltype))
                (package (gtk:tree-model-value model iter colpackage))
                (func nil))

           (when functype
             (setf functype (read-from-string functype))
             (setf funcname (string-upcase funcname))
             (setf package (string-upcase package))
             (setf func (find-symbol funcname (find-package package))))

           (cond (;; Example as a window for application
                  (eq functype :window)
                  (funcall func application))
                 (;; Example as a parent to the active application window
                  (eq functype :dialog)
                  (funcall func (gtk:application-active-window application)))
                 (;; Example called from a draw handler
                  (eq functype :drawfunc)
                  (window-draw-func title func application))
                 (t (format t "NO function found.")))
               )))

    (setf (gtk:tree-selection-mode selection) :browse)
    ;; The selection has changed.
    (g:signal-connect selection "changed"
       (lambda (tree-selection)
         (let* ((iter (gtk:tree-selection-selected tree-selection))
                (package (gtk:tree-model-value model iter colpackage))
                (filename (gtk:tree-model-value model iter colfile))
                (ui-file (gtk:tree-model-value model iter colui))
                (css-file (gtk:tree-model-value model iter colcss)))
           ;; TODO: Improve this peace of code. Use pathname functions.
           (when package
             (when filename (setf filename (sys-path filename package)))
             (when (> (length ui-file) 0)
               (setf ui-file (sys-path ui-file package)))
             (when (> (length css-file) 0)
               (setf css-file (sys-path css-file package))))
           (if (> (length filename) 0)
               (load-file filename))
           (if (> (length ui-file) 0)
               (load-file-buffer ui-buffer ui-file)
               (clear-buffer ui-buffer))
           (if (> (length css-file) 0)
               (load-file-buffer css-buffer css-file)
               (clear-buffer css-buffer)))))
      view))

(defun gtk-demo-activate (application)
  (let* (;; The text view
         (view (create-view-and-model application))
         ;; The scrollable
         (scroller (make-instance 'gtk:scrolled-window
                                  :child view
                                  :hscrollbar-policy :never
                                  :vscrollbar-policy :automatic
                                  :hexpand t
                                  :vexpand t))
         ;; The notebook
         (notebook (make-instance 'gtk:notebook
                                  :scrollable t))
         ;; The horizontal pane
         (content (make-instance 'gtk:paned
                                 :orientation :horizontal
                                 :position 320
                                 :resize-start-child nil
                                 :start-child scroller
                                 :end-child notebook))
         ;; The application window
         (window (make-instance 'gtk:application-window
                                :application application
                                :title "GTK Lisp Code Demos"
                                :child content
                                :show-menubar t
                                :default-width 1000
                                :default-height 800)))

    (g:signal-connect window "close-request"
        (lambda (window)
          (declare (ignore window))
          (let ((action (g:action-map-lookup-action application "quit")))
            (g:action-activate action))))

    ;; Set an icon for the application
;    (let ((pixbuf (gdk-pixbuf-new-from-file (rel-path "gtk:logo-rgb.gif"))))
;      (setq pixbuf (gdk-pixbuf-add-alpha pixbuf t 255 255 255))
;      (setf (gtk:window-default-icon-list) (list pixbuf)))

    ;; Add the notebook pages to the notebook
    (gtk:notebook-add-page notebook
                           (create-text info-buffer nil)
                           (gtk:label-new-with-mnemonic "_Info"))
    (gtk:notebook-add-page notebook
                           (create-text source-buffer t)
                           (gtk:label-new-with-mnemonic "_Source"))
    (gtk:notebook-add-page notebook
                           (create-text ui-buffer t)
                           (gtk:label-new-with-mnemonic "_UI Definition"))
    (gtk:notebook-add-page notebook
                           (create-text css-buffer t)
                           (gtk:label-new-with-mnemonic "_CSS Definition"))
    ;; Show the window
    (setf (gtk:widget-visible window) t)))

(defun activate-about-dialog ()
  (gtk:show-about-dialog nil
                         :modal t
                         :program-name "GTK4 Lisp Demo"
                         :version "0.9"
                         :copyright "(c) Dieter Kaiser"
                         :website
                         "github.com/crategus/cl-cffi-gtk"
                         :website-label "Project web site"
                         :license "MIT"
                         :authors '("Dieter Kaiser")
                         :documenters '("Dieter Kaiser")
                         :artists '("None")
                         :logo-icon-name
                         "applications-development"
                         :wrap-license t))

(defvar *gtk4-demo-menu*
"<interface>
  <menu id=\"appmenu\">
    <submenu>
      <attribute name='label'>GTK4 Lisp Demo</attribute>
      <section>
        <item>
          <attribute name=\"label\" translatable=\"yes\">Inspector</attribute>
          <attribute name=\"action\">app.inspector</attribute>
        </item>
        <item>
          <attribute name=\"label\" translatable=\"yes\">About</attribute>
          <attribute name=\"action\">app.about</attribute>
        </item>
        <item>
          <attribute name=\"label\" translatable=\"yes\">_Quit</attribute>
          <attribute name=\"action\">app.quit</attribute>
          <attribute name=\"accel\">&lt;Primary&gt;q</attribute>
        </item>
      </section>
    </submenu>
  </menu>
</interface>")

(defun gtk-demo-startup (application)
  ;; Load the application menu
  (let ((builder (make-instance 'gtk:builder)))
    (gtk:builder-add-from-string builder *gtk4-demo-menu*)
    ;; TODO: Improve the implemenation of the menubar.
    (setf (gtk:application-menubar application)
          (gtk:builder-object builder "appmenu")))
  ;; Add action "inspector" to the application
  (let ((action (g:simple-action-new "inspector" nil)))
    ;; Connect a handler to the signal "activate"
    (g:signal-connect action "activate"
       (lambda (action parameter)
         (declare (ignore action parameter))
         (gtk:window-set-interactive-debugging t)))
    ;; Add the action to the action map of the application
    (g:action-map-add-action application action))
  ;; Add action "about" to the application
  (let ((action (g:simple-action-new "about" nil)))
    ;; Connect a handler to the signal "activate"
    (g:signal-connect action "activate"
       (lambda (action parameter)
         (declare (ignore action parameter))
         (activate-about-dialog)))
    ;; Add the action to the action map of the application
    (g:action-map-add-action application action))
  ;; Add action "quit" to the application
  (let ((action (g:simple-action-new "quit" nil)))
    ;; Connect a handler to the signal activate
    (g:signal-connect action "activate"
       (lambda (action parameter)
         (declare (ignore action parameter))
         ;; Destroy all windows of the application
         (dolist (window (gtk:application-windows application))
           (gtk:window-destroy window))))
    ;; Add the action to action map of the application
    (g:action-map-add-action application action)))

(defun gtk4-demo (&rest argv)
  (unless (string= "GTK4 Lisp Demo" (g:application-name))
    (setf (g:application-name) "GTK4 Lisp Demo"))
  (let ((resource (g:resource-load (sys-path "resource/gtk4-example.gresource"
                                             :gtk4-example)))
        (gtk-demo (make-instance 'gtk:application
                                 :application-id "com.crategus.gtk4-demo"
                                 :register-session t
                                 :resource-base-path (cffi:null-pointer))))
    (g:resources-register resource)
    ;; Connect signal handlers to the application
    (g:signal-connect gtk-demo "startup" #'gtk-demo-startup)
    (g:signal-connect gtk-demo "activate" #'gtk-demo-activate)
    (g:signal-connect gtk-demo "shutdown"
        (lambda (application)
          (declare (ignore application))
          (g:resources-unregister resource)))
    ;; Start the application
    (g:application-run gtk-demo argv)))

;;; --- End of file gtk4-demo.lisp ---------------------------------------------
