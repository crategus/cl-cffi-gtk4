;;; ----------------------------------------------------------------------------
;;; gtk.emoji-chooser.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 Dieter Kaiser
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

(define-g-object-class "GtkEmojiChooser" emoji-chooser
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

;;; --- End of file gtk.emoji-chooser.lisp -------------------------------------
