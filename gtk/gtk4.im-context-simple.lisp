;;; ----------------------------------------------------------------------------
;;; gtk.im-context-simple.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2022 Dieter Kaiser
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
;;; GtkIMContextSimple
;;;
;;;     An input method context supporting table-based input methods
;;;
;;; Types and Values
;;;
;;;     GtkIMContextSimple
;;;
;;;     GTK_MAX_COMPOSE_LEN
;;;
;;; Functions
;;;
;;;     gtk_im_context_simple_new
;;;     gtk_im_context_simple_add_table
;;;     gtk_im_context_simple_add_compose_file
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkIMContext
;;;         ╰── GtkIMContextSimple
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GTK_MAX_COMPOSE_LEN
;;;
;;; #define GTK_MAX_COMPOSE_LEN 7
;;;
;;; The maximum length of sequences in compose tables.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkIMContextSimple
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkIMContextSimple" im-context-simple
  (:superclass im-context
   :export t
   :interfaces nil
   :type-initializer "gtk_im_context_simple_get_type")
  nil)

#+liber-documentation
(setf (documentation 'im-context-simple 'type)
 "@version{#2022-7-10}
  @begin{short}
    The @sym{gtk:im-context-simple} object is a simple input method context
    supporting table-based input methods.
  @end{short}
  It has a built-in table of compose sequences that is derived from the X11
  Compose files.

  The @sym{gtk:im-context-simple} object reads additional compose sequences
  from the first of the following files that is found:
  @code{~/.config/gtk-4.0/Compose},
  @code{~/.XCompose},
  @code{/usr/share/X11/locale/$locale/Compose},
  for locales that have a nontrivial Compose file. The syntax of these files is
  described in the Compose(5) manual page.

  @subheading{Unicode characters}
  The @sym{gtk:im-context-simple} object also supports numeric entry of Unicode
  characters by typing the @kbd{Ctrl-Shift-u} key, followed by a hexadecimal
  Unicode codepoint. For example, @kbd{Ctrl-Shift-u 1 2 3 Enter} yields U+0123
  LATIN SMALL LETTER G WITH CEDILLA, i.e. ģ.
  @see-class{gtk:im-context}")

;;; ----------------------------------------------------------------------------
;;; gtk_im_context_simple_new ()
;;; ----------------------------------------------------------------------------

(defun im-context-simple-new ()
 #+liber-documentation
 "@version{#2022-7-10}
  @return{A new @class{gtk:im-context-simple} object.}
  @short{Creates a new simple input method.}
  @see-class{gtk:im-context-simple}"
  (make-instance 'im-context-simple))

(export 'im-context-simple-new)

;;; ----------------------------------------------------------------------------
;;; gtk_im_context_simple_add_table ()
;;;
;;; void
;;; gtk_im_context_simple_add_table (GtkIMContextSimple *context_simple,
;;;                                  guint16 *data,
;;;                                  int max_seq_len,
;;;                                  int n_seqs);
;;;
;;; Adds an additional table to search to the input context. Each row of the
;;; table consists of max_seq_len key symbols followed by two guint16
;;; interpreted as the high and low words of a gunicode value. Tables are
;;; searched starting from the last added.
;;;
;;; The table must be sorted in dictionary order on the numeric value of the key
;;; symbol fields. (Values beyond the length of the sequence should be zero.)
;;;
;;; context_simple :
;;;     A GtkIMContextSimple
;;;
;;; data :
;;;     the table.
;;;
;;; max_seq_len :
;;;     Maximum length of a sequence in the table (cannot be greater than
;;;     GTK_MAX_COMPOSE_LEN)
;;;
;;; n_seqs :
;;;     number of sequences in the table
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_im_context_simple_add_compose_file ()
;;;
;;; void
;;; gtk_im_context_simple_add_compose_file (GtkIMContextSimple *context_simple,
;;;                                         const char *compose_file);
;;;
;;; Adds an additional table from the X11 compose file.
;;;
;;; context_simple :
;;;     A GtkIMContextSimple
;;;
;;; compose_file :
;;;     The path of compose file
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.im-context-simple.lisp ---------------------------------
