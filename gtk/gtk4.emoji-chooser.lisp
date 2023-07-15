;;; ----------------------------------------------------------------------------
;;; gtk4.emoji-chooser.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2023 Dieter Kaiser
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
;;; GtkEmojiChooser
;;;
;;;     A popover to choose an Emoji character
;;;
;;; Types and Values
;;;
;;;     GtkEmojiChooser
;;;
;;; Functions
;;;
;;;     gtk_emoji_chooser_new
;;;
;;; Signals
;;;
;;;     emoji-picked
;;;
;;; Actions
;;;
;;;     scroll.section
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkPopover
;;;                 ╰── GtkEmojiChooser
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;     GtkShortcutManager
;;;     GtkNative
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkEmojiChooser
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkEmojiChooser" emoji-chooser
  (:superclass popover
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkNative"
                "GtkShortcutManager")
   :type-initializer "gtk_emoji_chooser_get_type")
  nil)

#+liber-documentation
(setf (documentation 'emoji-chooser 'type)
 "@version{#2022-6-28}
  @begin{short}
    The @sym{gtk:emoji-chooser} popover is used by text widgets such as the
    @class{gtk:entry} or @class{gtk:text-view} widgets to offer users a
    convenient way to insert Emoji characters.
  @end{short}

  @image[emoji-chooser]{Figure: GtkEmojiChooser}
  The @sym{gtk:emoji-chooser} popover emits the \"emoji-picked\" signal when an
  Emoji is selected.
  @begin[CSS nodes]{dictionary}
    @begin{pre}
popover
├── box.emoji-searchbar
│   ╰── entry.search
╰── box.emoji-toolbar
    ├── button.image-button.emoji-section
    ├── ...
    ╰── button.image-button.emoji-section
    @end{pre}
    Every @sym{gtk:emoji-chooser} widget consists of a main node called
    @code{popover}. The contents of the popover are largely implementation
    defined and supposed to inherit general styles. The top searchbar used to
    search emoji and gets the @code{.emoji-searchbar} style class itself. The
    bottom toolbar used to switch between different emoji categories consists of
    buttons with the @code{.emoji-section} style class and gets the
    @code{.emoji-toolbar} style class itself.
  @end{dictionary}
  @begin[Action Details]{dictionary}
    @subheading{The \"scroll.section\" action}
      Scrolls to the next or previous section.
      @begin[code]{table}
        @entry[direction]{1 to scroll forward, -1 to scroll back}
      @end{table}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"emoji-picked\" signal}
      @begin{pre}
lambda (chooser text)    :run-last
      @end{pre}
      The signal is emitted when the user selects an Emoji.
      @begin[code]{table}
        @entry[chooser]{The @sym{gtk:emoji-chooser} widget.}
        @entry[text]{A string with the Unicode sequence for the picked Emoji,
          in UTF-8.}
      @end{table}
  @end{dictionary}
  @see-class{gtk:popover}")

;;; ----------------------------------------------------------------------------
;;; gtk_emoji_chooser_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline emoji-chooser-new))

(defun emoji-chooser-new ()
 #+liber-documentation
 "@version{#2022-6-28}
  @return{A new @class{gtk:emoji-chooser} widget.}
  @short{Creates a new Emoji chooser dialog.}
  @see-class{gtk:emoji-chooser}"
  (make-instance 'emoji-chooser))

(export 'emoji-chooser-new)

;;; --- End of file gtk4.emoji-chooser.lisp ------------------------------------
