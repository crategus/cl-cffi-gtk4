;;; ----------------------------------------------------------------------------
;;; gtk4.shortcut-trigger.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2024 Dieter Kaiser
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
;;; GtkShortcutTrigger
;;;
;;;     Triggers to track if shortcuts should be activated
;;;
;;; Types and Values
;;;
;;;     GtkShortcutTrigger
;;;     GtkKeyvalTrigger
;;;     GtkMnemonicTrigger
;;;     GtkAlternativeTrigger
;;;     GtkNeverTrigger
;;;
;;; Functions
;;;
;;;     gtk_shortcut_trigger_trigger
;;;     gtk_shortcut_trigger_hash                          not implemented
;;;     gtk_shortcut_trigger_equal                         not implemented
;;;     gtk_shortcut_trigger_compare                       not implemented
;;;     gtk_shortcut_trigger_to_string
;;;     gtk_shortcut_trigger_print
;;;     gtk_shortcut_trigger_to_label
;;;     gtk_shortcut_trigger_print_label
;;;     gtk_shortcut_trigger_parse_string
;;;
;;;     gtk_keyval_trigger_new
;;;     gtk_keyval_trigger_get_modifiers                   Accessor
;;;     gtk_keyval_trigger_get_keyval                      Accessor
;;;
;;;     gtk_mnemonic_trigger_new
;;;     gtk_mnemonic_trigger_get_keyval
;;;
;;;     gtk_alternative_trigger_new
;;;     gtk_alternative_trigger_get_first
;;;     gtk_alternative_trigger_get_second
;;;
;;;     gtk_never_trigger_get
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkShortcutTrigger
;;;         ├── GtkKeyvalTrigger
;;;         ├── GtkNeverTrigger
;;;         ╰── GtkAlternativeTrigger
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkShortcutTrigger
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkShortcutTrigger" shortcut-trigger
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gtk_shortcut_trigger_get_type")
  nil)

#+liber-documentation
(setf (documentation 'shortcut-trigger 'type)
 "@version{#2022-8-26}
  @begin{short}
    A trigger for a key shortcut.
  @end{short}
  The @class{gtk:shortcut-trigger} object is the object used to track if a
  @class{gtk:shortcut} object should be activated. For this purpose, the
  @fun{gtk:shortcut-trigger-trigger} function can be called on a
  @class{gdk:event} event.

  The @class{gtk:shortcut-trigger} implementation contain functions that allow
  easy presentation to end users as well as being printed for debugging.

  All @class{gtk:shortcut-trigger} objects are immutable, you can only specify
  their properties during construction. If you want to change a trigger, you
  have to replace it with a new one.
  @see-class{gtk:shortcut-action}
  @see-function{gtk:shortcut-trigger-trigger}")

;;; ----------------------------------------------------------------------------
;;; GtkKeyvalTrigger
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkKeyvalTrigger" keyval-trigger
  (:superclass shortcut-trigger
   :export t
   :interfaces nil
   :type-initializer "gtk_keyval_trigger_get_type")
  ((keyval
    keyval-trigger-keyval
    "keyval" "guint" t nil)
   (modifiers
    keyval-trigger-modifiers
    "modifiers" "GdkModifierType" t nil)))

#+liber-documentation
(setf (documentation 'keyval-trigger 'type)
 "@version{#2022-8-26}
  @begin{short}
    A @class{gtk:shortcut-trigger} object that triggers when a specific keyval
    and (optionally) modifiers are pressed.
  @end{short}
  @see-slot{gtk:keyval-trigger-keyval}
  @see-slot{gtk:keyval-trigger-modifiers}
  @see-constructor{gtk:keyval-trigger-new}
  @see-class{gtk:shortcut-trigger}")

;;; --- gtk:keyval-trigger-keyval ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "keyval" 'keyval-trigger) t)
 "The @code{keyval} property of type @code{:uint}
 (Read / Write / Construct only) @br{}
  The key value for the trigger.")

#+liber-documentation
(setf (liber:alias-for-function 'keyval-trigger-keyval)
      "Accessor"
      (documentation 'keyval-trigger-keyval 'function)
 "@version{#2022-8-26}
  @syntax{(gtk:keyval-trigger-keyval object) => keyval)}
  @argument[object]{a @class{gtk:keyval-trigger} object}
  @argument[keyval]{an unsigned integer with the keyval}
  @begin{short}
    Accessor of the @slot[gtk:keyval-trigger]{keyval} slot of the
    @class{gtk:keyval-trigger} class.
  @end{short}
  The @fun{gtk:keyval-trigger-keyval} function returns the keyval that must be
  pressed to succed triggering @arg{object}.
  @see-class{gtk:keyval-trigger}")

;;; --- gtk:keyval-trigger-modifiers -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "modifiers"
                                               'keyval-trigger) t)
 "The @code{modifiers} property of type @symbol{gdk:modifier-type}
 (Read / Write / Construct only) @br{}
  The key modifiers for the trigger.")

#+liber-documentation
(setf (liber:alias-for-function 'keyval-trigger-modifiers)
      "Accessor"
      (documentation 'keyval-trigger-modifiers 'function)
 "@version{#2022-8-26}
  @syntax{(gtk:keyval-trigger-modifiers object) => modifiers)}
  @argument[object]{a @class{gtk:keyval-trigger} object}
  @argument[modifiers]{a @symbol{gdk:modifiers-type} value}
  @begin{short}
    Accessor of the @slot[gtk:keyval-trigger]{modifiers} slot of the
    @class{gtk:keyval-trigger} class.
  @end{short}
  The @fun{gtk:keyval-trigger-modifiers} function returns the modifiers that
  must be present to succed triggering @arg{object}.
  @see-class{gtk:keyval-trigger}")

;;; ----------------------------------------------------------------------------
;;; GtkMnemonicTrigger
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkMnemonicTrigger" mnemonic-trigger
  (:superclass shortcut-trigger
   :export t
   :interfaces nil
   :type-initializer "gtk_mnemonic_trigger_get_type")
  ((keyval
    mnemonic-trigger-keyval
    "keyval" "guint" t nil)))

#+liber-documentation
(setf (documentation 'mnemonic-trigger 'type)
 "@version{#2022-8-26}
  @begin{short}
    A @class{gtk:shortcut-trigger} object that triggers when a specific
    mnemonic is pressed.
  @end{short}
  @see-slot{gtk:mnemonic-trigger-keyval}
  @see-constructor{gtk:mnemonic-trigger-new}
  @see-class{gtk:shortcut-trigger}")

;;; --- gtk:mnemonic-trigger-keyval --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "keyval"
                                               'mnemonic-trigger) t)
 "The @code{keyval} property of type @code{:uint}
 (Read / Write / Construct only) @br{}
  The key value for the trigger.")

#+liber-documentation
(setf (liber:alias-for-function 'mnemonic-trigger-keyval)
      "Accessor"
      (documentation 'mnemonic-trigger-keyval 'function)
 "@version{#2022-8-26}
  @syntax{(gtk:mnemonic-trigger-keyval object) => keyval)}
  @argument[object]{a @class{gtk:mnemonic-trigger} object}
  @argument[keyval]{an unsigned integer with the keyval}
  @begin{short}
    Accessor of the @slot[gtk:mnemonic-trigger]{keyval} slot of the
    @class{gtk:mnemonic-trigger} class.
  @end{short}
  The @fun{gtk:mnemonic-trigger-keyval} function returns the keyval that must
  be pressed to succed triggering @arg{object}.
  @see-class{gtk:mnemonic-trigger}")

;;; ----------------------------------------------------------------------------
;;; GtkAlternativeTrigger
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkAlternativeTrigger" alternative-trigger
  (:superclass shortcut-trigger
   :export t
   :interfaces nil
   :type-initializer "gtk_alternative_trigger_get_type")
  ((first
    alternative-trigger-first
    "first" "GtkShortcutTrigger" t nil)
   (second
    alternative-trigger-second
    "second" "GtkShortcutTrigger" t nil)))

#+liber-documentation
(setf (documentation 'alternative-trigger 'type)
 "@version{#2022-8-26}
  @begin{short}
    A @class{gtk:shortcut-trigger} object that triggers when either of two
    @class{gtk:shortcut-trigger} objects trigger.
  @end{short}
  @see-slot{gtk:alternative-trigger-first}
  @see-slot{gtk:alternative-trigger-second}
  @see-constructor{gtk:alternative-trigger-new}
  @see-class{gtk:shortcut-trigger}")

;;; --- gtk:alternative-trigger-first ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "first"
                                               'alternative-trigger) t)
 "The @code{first} property of type @class{gtk:shortcut-trigger}
 (Read / Write / Construct only) @br{}
  The first trigger to check.")

#+liber-documentation
(setf (liber:alias-for-function 'alternative-trigger-first)
      "Accessor"
      (documentation 'alternative-trigger-first 'function)
 "@version{#2022-8-26}
  @syntax{(gtk:alternative-trigger-first object) => shortcut)}
  @argument[object]{a @class{gtk:mnemonic-trigger} object}
  @argument[shortcut]{a first @class{gtk:shortcut-trigger} object}
  @begin{short}
    Accessor of the @slot[gtk:alternative-trigger]{first} slot of the
    @class{gtk:alternative-trigger} class.
  @end{short}
  The @fun{gtk:alternative-trigger-first} function gets the first of the two
  alternative triggers that may trigger @arg{shortcut}. The
  @fun{gtk:alternative-trigger-second} function will return the other one.
  @see-class{gtk:alternative-trigger}
  @see-function{gtk:alternative-trigger-second}")

;;; --- gtk:alternative-trigger-second -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "second"
                                               'alternative-trigger) t)
 "The @code{second} property of type @class{gtk:shortcut-trigger}
 (Read / Write / Construct only) @br{}
  The second trigger to check.")

#+liber-documentation
(setf (liber:alias-for-function 'alternative-trigger-second)
      "Accessor"
      (documentation 'alternative-trigger-second 'function)
 "@version{#2022-8-26}
  @syntax{(gtk:alternative-trigger-second object) => shortcut)}
  @argument[object]{a @class{gtk:mnemonic-trigger} object}
  @argument[shortcut]{a second @class{gtk:shortcut-trigger} object}
  @begin{short}
    Accessor of the @slot[gtk:alternative-trigger]{second} slot of the
    @class{gtk:alternative-trigger} class.
  @end{short}
  The @fun{gtk:alternative-trigger-second} function gets the second of the two
  alternative triggers that may trigger @arg{shortcut}. The
  @fun{gtk:alternative-trigger-first} function will return the other one.
  @see-class{gtk:alternative-trigger}
  @see-function{gtk:alternative-trigger-first}")

;;; ----------------------------------------------------------------------------
;;; GtkNeverTrigger
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkNeverTrigger" never-trigger
  (:superclass shortcut-trigger
   :export t
   :interfaces nil
   :type-initializer "gtk_never_trigger_get_type")
  nil)

#+liber-documentation
(setf (documentation 'never-trigger 'type)
 "@version{#2022-8-26}
  @begin{short}
    A @class{gtk:shortcut-trigger} object that never triggers.
  @end{short}
  @see-class{gtk:shortcut-trigger}")

;;; ----------------------------------------------------------------------------
;;; gtk_shortcut_trigger_trigger
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_shortcut_trigger_trigger" shortcut-trigger-trigger)
    gdk:key-match
 #+liber-documentation
 "@version{#2022-8-26}
  @argument[shortcut]{a @class{gtk:shortcut-trigger} object}
  @argument[event]{a @class{gdk:event} event}
  @argument[enable]{a boolean whether mnemonics should trigger}
  @return{The @symbol{gdk:key-match} value whether the event triggered the
    shortcut.}
  @begin{short}
    Checks if the given event triggers @arg{shortcut}.
  @end{short}
  Usually the value of @arg{enable} is determined by checking that the passed
  in event is a key event and has the right modifiers set.
  @see-class{gtk:shortcut-trigger}"
  (shortcut (g:object shortcut-trigger))
  (event gdk:event)
  (enable :boolean))

(export 'shortcut-trigger-trigger)

;;; ----------------------------------------------------------------------------
;;; gtk_shortcut_trigger_hash ()
;;;
;;; guint
;;; gtk_shortcut_trigger_hash (gconstpointer trigger);
;;;
;;; Generates a hash value for a GtkShortcutTrigger.
;;;
;;; The output of this function is guaranteed to be the same for a given value
;;; only per-process. It may change between different processor architectures or
;;; even different versions of GTK. Do not use this function as a basis for
;;; building protocols or file formats.
;;;
;;; The types of trigger is gconstpointer only to allow use of this function
;;; with GHashTable. They must each be a GtkShortcutTrigger.
;;;
;;; trigger :
;;;     a GtkShortcutTrigger.
;;;
;;; Returns :
;;;     a hash value corresponding to trigger
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_shortcut_trigger_equal ()
;;;
;;; gboolean
;;; gtk_shortcut_trigger_equal (gconstpointer trigger1,
;;;                             gconstpointer trigger2);
;;;
;;; Checks if trigger1 and trigger2 trigger under the same conditions.
;;;
;;; The types of one and two are gconstpointer only to allow use of this
;;; function with GHashTable. They must each be a GtkShortcutTrigger.
;;;
;;; trigger1 :
;;;     a GtkShortcutTrigger.
;;;
;;; trigger2 :
;;;     a GtkShortcutTrigger.
;;;
;;; Returns :
;;;     TRUE if trigger1 and trigger2 are equal
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_shortcut_trigger_compare ()
;;;
;;; int
;;; gtk_shortcut_trigger_compare (gconstpointer trigger1,
;;;                               gconstpointer trigger2);
;;;
;;; The types of trigger1 and trigger2 are gconstpointer only to allow use of
;;; this function as a GCompareFunc. They must each be a GtkShortcutTrigger.
;;;
;;; trigger1 :
;;;     a GtkShortcutTrigger.
;;;
;;; trigger2 :
;;;     a GtkShortcutTrigger.
;;;
;;; Returns :
;;;     An integer less than, equal to, or greater than zero if trigger1 is
;;;     found, respectively, to be less than, to match, or be greater than
;;;     trigger2 .
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_shortcut_trigger_to_string
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_shortcut_trigger_to_string" shortcut-trigger-to-string)
    :string
 #+liber-documentation
 "@version{#2022-8-26}
  @argument[shortcut]{a @class{gtk:shortcut-trigger} object}
  @return{The human-readable string.}
  @begin{short}
    Prints the given trigger into a human-readable string.
  @end{short}
  @see-class{gtk:shortcut-trigger}"
  (shortcut (g:object shortcut-trigger)))

(export 'shortcut-trigger-to-string)

;;; ----------------------------------------------------------------------------
;;; gtk_shortcut_trigger_print ()
;;;
;;; void
;;; gtk_shortcut_trigger_print (GtkShortcutTrigger *self,
;;;                             GString *string);
;;;
;;; Prints the given trigger into a string for the developer. This is meant for
;;; debugging and logging.
;;;
;;; The form of the representation may change at any time and is not guaranteed
;;; to stay identical.
;;;
;;; self :
;;;     a GtkShortcutTrigger
;;;
;;; string :
;;;     a GString to print into
;;; ----------------------------------------------------------------------------

;; not needed, see the SHORTCUT-TRIGGER-TO-STRING function

;;; ----------------------------------------------------------------------------
;;; gtk_shortcut_trigger_to_label
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_shortcut_trigger_to_label" shortcut-trigger-to-label)
    :string
 #+liber-documentation
 "@version{#2022-8-26}
  @argument[shortcut]{a @class{gtk:shortcut-trigger} object}
  @argument[display]{a @class{gdk:display} object}
  @return{The string with the textual representation for the given trigger.}
  @begin{short}
    Gets textual representation for the given trigger.
  @end{short}
  This function is returning a translated string for presentation to end users
  for example in menu items or in help texts.

  The display in use may influence the resulting string in various forms, such
  as resolving hardware keycodes or by causing display-specific modifier names.
  The form of the representation may change at any time and is not guaranteed
  to stay identical.
  @see-class{gtk:shortcut-trigger}
  @see-class{gdk:display}"
  (shortcut (g:object shortcut-trigger))
  (display (g:object gdk:display)))

(export 'shortcut-trigger-to-label)

;;; ----------------------------------------------------------------------------
;;; gtk_shortcut_trigger_print_label ()
;;;
;;; gboolean
;;; gtk_shortcut_trigger_print_label (GtkShortcutTrigger *self,
;;;                                   GdkDisplay *display,
;;;                                   GString *string);
;;;
;;; Prints the given trigger into a string. This function is returning a
;;; translated string for presentation to end users for example in menu items
;;; or in help texts.
;;;
;;; The display in use may influence the resulting string in various forms, such
;;; as resolving hardware keycodes or by causing display-specific modifier
;;; names.
;;;
;;; The form of the representation may change at any time and is not guaranteed
;;; to stay identical.
;;;
;;; self :
;;;     a GtkShortcutTrigger
;;;
;;; display :
;;;     GdkDisplay to print for
;;;
;;; string :
;;;     a GString to print into
;;;
;;; Returns :
;;;     TRUE if something was printed or FALSE if the trigger did not have a
;;;     textual representation suitable for end users.
;;; ----------------------------------------------------------------------------

;; not needed, see the SHORTCUT-TRIGGER-PRINT-LABEL function

;;; ----------------------------------------------------------------------------
;;; gtk_shortcut_trigger_parse_string
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_shortcut_trigger_parse_string"
               shortcut-trigger-parse-string) (g:object shortcut-trigger)
 #+liber-documentation
 "@version{#2022-8-26}
  @argument[str]{a string to parse}
  @return{The new @class{gtk:shortcut-trigger} object, or @code{nil} on error.}
  @begin{short}
    Tries to parse the given string into a trigger.
  @end{short}
  On success, the parsed trigger is returned. When parsing failed, @code{nil}
  is returned. The accepted strings are:
  @begin{itemize}
    @item{@code{never}: for GtkNeverTrigger}
    @item{a string parsed by the @fun{gtk:accelerator-parse} function, for a
      @class{gtk:keyval-trigger} object, e.g. @code{<Control>C}}
    @item{underscore, followed by a single character, for a
      @class{gtk:mnemonic-trigger} object, e.g. @code{_l}}
    @item{two valid trigger strings, separated by a @code{|} character, for a
      @class{gtk:alternative-trigger} object, e. g.
      @code{<Control>q|&lt;Control>w}}
  @end{itemize}
  Note that you will have to escape the @code{<} and @code{&gt}; characters when
  specifying triggers in XML files, such as GtkBuilder UI files. Use @code{&lt;}
  instead of @code{<} and @code{&gt;} instead of @code{&gt;}.

  @see-class{gtk:shortcut-trigger}
  @see-function{gtk:accelerator-parse}"
  (str :string))

(export 'shortcut-trigger-parse-string)

;;; ----------------------------------------------------------------------------
;;; gtk_keyval_trigger_new
;;; ----------------------------------------------------------------------------

(declaim (inline keyval-trigger-new))

(defun keyval-trigger-new (keyval modifiers)
 #+liber-documentation
 "@version{#2022-8-26}
  @argument[keyval]{an unsigned integer with the keyval to trigger for}
  @argument[modifiers]{a @symbol{gdk:modifier-type} value that need to be
    present}
  @return{The new @class{gtk:shortcut-trigger} object.}
  @begin{short}
    Creates a @class{gtk:shortcut-trigger} object that will trigger whenever the
    key with the given keyval and modifiers is pressed.
  @end{short}
  @see-class{keyval-trigger}"
  (make-instance 'keyval-trigger
                 :keyval keyval
                 :modifiers modifiers))

(export 'keyval-trigger-new)

;;; ----------------------------------------------------------------------------
;;; gtk_mnemonic_trigger_new
;;; ----------------------------------------------------------------------------

(declaim (inline mnemonic-trigger-new))

(defun mnemonic-trigger-new (keyval)
 #+liber-documentation
 "@version{#2022-8-26}
  @argument[keyval]{an unsigned integer with the keyval to trigger for}
  @return{The new @class{gtk:mnemonic-trigger} object.}
  @begin{short}
    Creates a @class{gtk:shortcut-trigger} object that will trigger whenever
    the key with the given keyval is pressed and mnemonics have been activated.
  @end{short}
  Mnemonics are activated by calling code when a key event with the right
  modifiers is detected.
  @see-class{gtk:mnemonic-trigger}"
  (make-instance 'mnemonic-trigger
                 :keyval keyval))

(export 'mnemonic-trigger-new)

;;; ----------------------------------------------------------------------------
;;; gtk_alternative_trigger_new
;;; ----------------------------------------------------------------------------

(declaim (inline alternative-trigger-new))

(defun alternative-trigger-new (first second)
 #+liber-documentation
 "@version{#2022-8-26}
  @argument[first]{a @class{gtk:shortcut-trigger} object}
  @argument[second]{a @class{gtk:shortcut-trigger} object}
  @return{The new @class{gtk:shortcut-trigger} object.}
  @begin{short}
    Creates a @class{gtk:shortcut-trigger} object that will trigger whenever
    either of the two given triggers gets triggered.
  @end{short}
  Note that nesting is allowed, so if you want more than two alternative,
  create a new alternative trigger for each option.
  @see-class{gtk:alternative-trigger}"
  (make-instance 'alternative-trigger
                 :first first
                 :second second))

(export 'alternative-trigger-new)

;;; ----------------------------------------------------------------------------
;;; gtk_never_trigger_get
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_never_trigger_get" never-trigger-get)
    (g:object shortcut-trigger)
 #+liber-documentation
 "@version{#2022-8-26}
  @return{The @class{gtk:never-trigger} object.}
  @begin{short}
    Gets the never trigger.
  @end{short}
  This is a singleton for a trigger that never triggers. Use this trigger
  because it implements all virtual functions.
  @see-class{gtk:never-trigger}")

(export 'never-trigger-get)

;;; --- End of file gtk3.shortcut-trigger.lisp ---------------------------------
