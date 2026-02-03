;;; ----------------------------------------------------------------------------
;;; gtk4.color-chooser-dialog.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.12 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2012 - 2026 Dieter Kaiser
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
;;;
;;; GtkColorChooserDialog
;;;
;;;     A dialog for choosing colors
;;;
;;; Types and Values
;;;
;;;     GtkColorChooserDialog
;;;
;;; Functions
;;;
;;;     gtk_color_chooser_dialog_new
;;;
;;; Properties
;;;
;;;     show-editor
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkWindow
;;;                 ╰── GtkDialog
;;;                     ╰── GtkColorChooserDialog
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;     GtkNative
;;;     GtkShortcutManager
;;;     GtkRoot
;;;     GtkColorChooser
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkColorChooserDialog
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkColorChooserDialog" color-chooser-dialog
  (:superclass dialog
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkNative"
                "GtkRoot"
                "GtkShortcutManager"
                "GtkColorChooser")
   :type-initializer "gtk_color_chooser_dialog_get_type")
  ((show-editor
    color-chooser-dialog-show-editor
    "show-editor" "gboolean" t t)))

#+(and gtk-4-10 gtk-warn-deprecated)
(defmethod initialize-instance :after ((obj color-chooser-dialog) &key)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:COLOR-CHOOSER-DIALOG is deprecated since 4.10")))

#+liber-documentation
(setf (documentation 'color-chooser-dialog 'type)
 "@version{2026-01-24}
  @begin{short}
    The @class{gtk:color-chooser-dialog} widget is a dialog for choosing a
    color.
  @end{short}
  It implements the @class{gtk:color-chooser} interface and does not provide
  much API of its own.

  @image[color-chooser-dialog]{Figure: GtkColorChooserDialog}

  To create a @class{gtk:color-chooser-dialog} widget, use the
  @fun{gtk:color-chooser-dialog-new} function. To get or change the initially
  selected color, use the @fun{gtk:color-chooser-rgba} function.
  @begin[Examples]{dictionary}
    Clicking on the drawing area opens a color chooser dialog to select a
    background color for the drawing area. The default palettes are replaced
    for this color chooser dialog.
    @begin{pre}
(let ((message \"Click to change the background color.\")
      (bg-color (gdk:rgba-parse \"White\"))
      ;; Color palette with 9 Red RGBA colors
      (palette1 (list (gdk:rgba-parse \"IndianRed\")
                      (gdk:rgba-parse \"LightCoral\")
                      (gdk:rgba-parse \"Salmon\")
                      (gdk:rgba-parse \"DarkSalmon\")
                      (gdk:rgba-parse \"LightSalmon\")
                      (gdk:rgba-parse \"Crimson\")
                      (gdk:rgba-parse \"Red\")
                      (gdk:rgba-parse \"FireBrick\")
                      (gdk:rgba-parse \"DarkRed\")))
      ;; Gray palette with 9 gray RGBA colors
      (palette2 (list (gdk:rgba-parse \"Gainsboro\")
                      (gdk:rgba-parse \"LightGray\")
                      (gdk:rgba-parse \"Silver\")
                      (gdk:rgba-parse \"DarkGray\")
                      (gdk:rgba-parse \"Gray\")
                      (gdk:rgba-parse \"DimGray\")
                      (gdk:rgba-parse \"LightSlateGray\")
                      (gdk:rgba-parse \"SlateGray\")
                      (gdk:rgba-parse \"DarkSlateGray\"))))

  (defun do-color-chooser-dialog (&optional application)
    (let* ((area (make-instance 'gtk:drawing-area))
           (window (make-instance 'gtk:window
                                  :title \"Color Chooser Dialog\"
                                  :application application
                                  :child area
                                  :default-width 400))
           (gesture (make-instance 'gtk:gesture-click)))
      ;; Draw the background color and a hint on the drawing area
      (gtk:drawing-area-set-draw-func area
          (lambda (widget cr width height)
            (declare (ignore widget width height))
            (let ((red (gdk:rgba-red bg-color))
                  (green (gdk:rgba-green bg-color))
                  (blue (gdk:rgba-blue bg-color)))
                  ;; Paint the current color on the drawing area
                  (cairo:set-source-rgb cr red green blue)
                  (cairo:paint cr)
                  ;; Print a hint on the drawing area
                  (cairo:set-source-rgb cr (- 1 red)
                                           (- 1 green)
                                           (- 1 blue))
                  (cairo:select-font-face cr \"Sans\")
                  (cairo:set-font-size cr 12)
                  (cairo:move-to cr 12 24)
                  (cairo:show-text cr message))))
      ;; Add the controller to the drawing area
      (gtk:widget-add-controller area gesture)
      ;; Create and run a color chooser dialog to select a background color
      (g:signal-connect gesture \"pressed\"
          (lambda (gesture n x y)
            (declare (ignore gesture n x y))
            (let ((dialog (make-instance 'gtk:color-chooser-dialog
                                         :transient-for window
                                         :use-alpha nil)))
              ;; Add a custom palette to the dialog
              (gtk:color-chooser-add-palette dialog :vertical 1 palette1)
              ;; Add a second coustom palette to the dialog
              (gtk:color-chooser-add-palette dialog :vertical 1 palette2)
              ;; Set the actual background color for the color chooser
              (setf (gtk:color-chooser-rgba dialog) bg-color)
              ;; Connect handler to the response signal of the dialog
              (g:signal-connect dialog \"response\"
                  (lambda (dialog response)
                    (when (= -5 response) ; the :ok value
                      ;; Change the background color for the drawing area
                      (format t \"new color is ~a~%\"
                                (gtk:color-chooser-rgba dialog))
                      (setf bg-color (gtk:color-chooser-rgba dialog))
                      (gtk:widget-queue-draw area))
                    (gtk:window-destroy dialog)))
              (gtk:window-present dialog))))
      ;; Show the window
      (gtk:window-present window))))
    @end{pre}
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @class{gtk:color-chooser-dialog} implementation is deprecated since
    4.10. Use the @class{gtk:color-dialog} object instead.
  @end{dictionary}
  @see-constructor{gtk:color-chooser-dialog-new}
  @see-slot{gtk:color-chooser-dialog-show-editor}
  @see-class{gtk:color-chooser}
  @see-class{gtk:color-button}
  @see-class{gtk:dialog}")

;;; ----------------------------------------------------------------------------
;;; Property and Accesor Details
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-editor"
                                               'color-chooser-dialog) t)
 "The @code{show-editor} property of type @code{:boolean} (Read / Write) @br{}
  @em{True} when the color chooser dialog is showing the single-color editor.
  It can be set to switch the color chooser into single-color editing mode.@br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'color-chooser-dialog-show-editor)
      "Accessor"
      (documentation 'color-chooser-dialog-show-editor 'function)
 "@version{2025-09-28}
  @syntax{(gtk:color-chooser-dialog-show-editor object) => show-editor}
  @syntax{(setf (gtk:color-chooser-dialog-show-editor object) show-editor)}
  @argument[object]{a @class{gtk:color-chooser-dialog} widget}
  @argument[show-editor]{a boolean whether to show the single-color editor}
  @begin{short}
    The accessor for the @slot[gtk:color-chooser-dialog]{show-editor} slot of
    the @class{gtk:color-chooser-dialog} class gets or sets whether the color
    chooser dialog is showing the single-color editor.
  @end{short}
  It can be set to switch the color chooser into single-color editing mode.
  @begin[Warning]{dictionary}
    The @class{gtk:color-chooser-dialog} implementation is deprecated since
    4.10. Use the @class{gtk:color-dialog} object instead.
  @end{dictionary}
  @see-class{gtk:color-chooser-dialog}")

;;; ----------------------------------------------------------------------------
;;; gtk_color_chooser_dialog_new
;;; ----------------------------------------------------------------------------

(defun color-chooser-dialog-new (title parent)
 #+liber-documentation
 "@version{2025-07-25}
  @argument[title]{a string for the title of the dialog, or @code{nil}}
  @argument[parent]{a @class{gtk:window} transient parent for the dialog,
    or @code{nil}}
  @return{The new @class{gtk:color-chooser-dialog} widget.}
  @short{Creates a new color chooser dialog.}
  @begin[Warning]{dictionary}
    The @class{gtk:color-chooser-dialog} implementation is deprecated since
    4.10. Use the @class{gtk:color-dialog} object instead.
  @end{dictionary}
  @see-class{gtk:window}
  @see-class{gtk:color-chooser-dialog}"
  (cffi:foreign-funcall "gtk_color_chooser_dialog_new"
                        :string (or title (cffi:null-pointer))
                        (g:object window) (or parent (cffi:null-pointer))
                        (g:object color-chooser-dialog)))

(export 'color-chooser-dialog-new)

;;; --- End of file gtk4.color-chooser-dialog.lisp -----------------------------
