;;; ----------------------------------------------------------------------------
;;; gdk.seat.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2019 - 2022 Dieter Kaiser
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
;;; GdkSeat
;;;
;;;     Object representing a user seat
;;;
;;; Types and Values
;;;
;;;     GdkSeat
;;;     GdkSeatCapabilities
;;;
;;; Accessors
;;;
;;;     gdk_seat_get_display
;;;
;;; Functions
;;;
;;;     gdk_seat_get_capabilities
;;;     gdk_seat_get_pointer
;;;     gdk_seat_get_keyboard
;;;     gdk_seat_get_devices
;;;     gdk_seat_get_tools
;;;
;;; Properties
;;;
;;;     display
;;;
;;; Signals
;;;
;;;     device-added
;;;     device-removed
;;;     tool-added
;;;     tool-removed
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GdkSeat
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; enum GdkSeatCapabilities
;;; ----------------------------------------------------------------------------

(define-g-flags "GdkSeatCapabilities" seat-capabilities
  (:export t
   :type-initializer "gdk_seat_capabilities_get_type")
  (:none 0)
  (:pointer       #.(ash 1 0))
  (:touch         #.(ash 1 1))
  (:tablet-stylus #.(ash 1 2))
  (:keyboard      #.(ash 1 3))
  (:tablet-pad    #.(ash 1 4))
  (:all-pointing 7)                    ; :pointer | :touch | :tablet-stylus
  (:all 15))                           ; :all-pointing | :keyboard

#+liber-documentation
(setf (liber:alias-for-symbol 'seat-capabilities)
      "GFlags"
      (liber:symbol-documentation 'seat-capabilities)
 "@version{#2022-11-26}
  @begin{short}
    Flags describing the seat capabilities.
  @end{short}
  @begin{pre}
(define-g-flags \"GdkSeatCapabilities\" seat-capabilities
  (:export t
   :type-initializer \"gdk_seat_capabilities_get_type\")
  (:none 0)
  (:pointer       #.(ash 1 0))
  (:touch         #.(ash 1 1))
  (:tablet-stylus #.(ash 1 2))
  (:keyboard      #.(ash 1 3))
  (:tablet-pad    #.(ash 1 4))
  (:all-pointing 7)
  (:all 15))
  @end{pre}
  @begin[code]{table}
    @entry[:none]{No input capabilities.}
    @entry[:pointer]{The seat has a pointer, e.g. mouse.}
    @entry[:touch]{The seat has touchscreen(s) attached.}
    @entry[:tablet-stylus]{The seat has drawing tablet(s) attached.}
    @entry[:keyboard]{The seat has keyboard(s) attached.}
    @entry[:tablet-pad]{The seat has drawing tablet pad(s) attached.}
    @entry[:all-pointing]{The union of all pointing capabilities.}
    @entry[:all]{The union of all capabilities.}
  @end{table}
  @see-class{gdk:seat}")

;;; ----------------------------------------------------------------------------
;;; GdkSeat
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkSeat" seat
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gdk_seat_get_type")
  ((display
    seat-display
    "display" "GdkDisplay" t t)))

#+liber-documentation
(setf (documentation 'seat 'type)
 "@version{#2022-11-28}
  @begin{short}
    The @sym{gdk:seat} object represents a collection of input devices that
    belong to a user.
  @end{short}
  @begin[Signal Details]{dictionary}
    @subheading{The \"device-added\" signal}
      @begin{pre}
lambda (seat device)    : Run Last
      @end{pre}
      The signal is emitted when a new input device is related to this seat.
      @begin[code]{table}
        @entry[seat]{The @sym{gdk:seat} object on which the signal is emitted.}
        @entry[device]{The newly added @class{gdk:device} object.}
      @end{table}
    @subheading{The \"device-removed\" signal}
      @begin{pre}
lambda (seat device)    : Run Last
      @end{pre}
      The signal is emitted when an input device is removed, e.g. unplugged.
      @begin[code]{table}
        @entry[seat]{The @sym{gdk:seat} object on which the signal is emitted.}
        @entry[device]{The just removed @class{gdk:device} object.}
      @end{table}
    @subheading{The \"tool-added\" signal}
      @begin{pre}
lambda (seat tool)    : Run Last
      @end{pre}
      The signal is emitted whenever a new tool is made known to the seat. The
      tool may later be assigned to a device, i.e. on proximity with a tablet.
      The device will emit the \"tool-changed\" signal accordingly. A same tool
      may be used by several devices.
      @begin[code]{table}
        @entry[seat]{The @sym{gdk:seat} object on which the signal is emitted.}
        @entry[tool]{The new @class{gdk:device-tool} object known to the seat.}
      @end{table}
    @subheading{The \"tool-removed\" signal}
      @begin{pre}
lambda (seat tool)    :run-last
      @end{pre}
      The signal is emitted whenever a tool is no longer known to this seat.
      @begin[code]{table}
        @entry[seat]{The @sym{gdk:seat} object on which the signal is emitted.}
        @entry[tool]{The just removed @class{gdk:device-tool} object.}
      @end{table}
  @end{dictionary}
  @see-slot{gdk:seat-display}
  @see-class{gdk:display}
  @see-class{gdk:device}")

;;; ----------------------------------------------------------------------------
;;; Property Details
;;; ----------------------------------------------------------------------------

;;; --- seat-display -----------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "display" 'seat) t)
 "The @code{display} property of type @class{gdk:display}
  (Read / Write / Construct) @br{}
  The display of this seat.")

#+liber-documentation
(setf (liber:alias-for-function 'seat-display)
      "Accessor"
      (documentation 'seat-display 'function)
 "@version{#2022-11-28}
  @syntax[]{(gdk:seat-display object) => display}
  @argument[object]{a @class{gdk:seat} object}
  @argument[display]{a @class{gdk:display} object}
  @begin{short}
    Accessor of the @slot[gdk:seat]{display} slot of the @class{gdk:seat} class.
  @end{short}
  The @sym{gdk-seat-display} function returns the display this seat belongs to.
  @see-class{gdk:seat}
  @see-class{gdk:display}")

;;; ----------------------------------------------------------------------------
;;; gdk_seat_get_capabilities ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_seat_get_capabilities" seat-capabilities) seat-capabilities
 #+liber-documentation
 "@version{#2022-11-28}
  @argument[seat]{a @class{gdk:seat} object}
  @return{The seat capabilities as a @symbol{gdk:seat-capabilities} value.}
  @begin{short}
    Returns the capabilities this @class{gdk:seat} object currently has.
  @end{short}
  @see-class{gdk:seat}
  @see-symbol{gdk:seat-capabilities}"
  (seat (g:object seat)))

(export 'seat-capabilities)

;;; ----------------------------------------------------------------------------
;;; gdk_seat_get_pointer ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_seat_get_pointer" seat-pointer) (g:object device)
 #+liber-documentation
 "@version{#2022-11-28}
  @argument[seat]{a @class{gdk:seat} object}
  @return{A master @class{gdk:device} object with pointer capabilities.}
  @begin{short}
    Returns the master device that routes pointer events.
  @end{short}
  @see-class{gdk:seat}
  @see-class{gdk:device}"
  (seat (g:object seat)))

(export 'seat-pointer)

;;; ----------------------------------------------------------------------------
;;; gdk_seat_get_keyboard ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_seat_get_keyboard" seat-keyboard) (g:object device)
 #+liber-documentation
 "@version{#2022-11-28}
  @argument[seat]{a @class{gdk:seat} object}
  @return{A master @class{gdk:device} object with keyboard capabilities.}
  @begin{short}
    Returns the master device that routes keyboard events.
  @end{short}
  @see-class{gdk:seat}
  @see-class{gdk:device}"
  (seat (g:object seat)))

(export 'seat-keyboard)

;;; ----------------------------------------------------------------------------
;;; gdk_seat_get_devices ()
;;;
;;; GList *
;;; gdk_seat_get_devices (GdkSeat *seat,
;;;                       GdkSeatCapabilities capabilities);
;;;
;;; Returns the devices that match the given capabilities.
;;;
;;; seat :
;;;     a GdkSeat
;;;
;;; capabilities :
;;;     capabilities to get devices for
;;;
;;; Returns :
;;;     A list of GdkDevices. The list must be freed with g_list_free(), the
;;;     elements are owned by GTK and must not be freed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_seat_get_tools ()
;;;
;;; GList *
;;; gdk_seat_get_tools (GdkSeat *seat);
;;;
;;; Returns all GdkDeviceTools that are known to the application.
;;;
;;; seat :
;;;     A GdkSeat
;;;
;;; Returns :
;;;     A list of tools. Free with g_list_free().
;;; ----------------------------------------------------------------------------

;;; --- End of file gdk.seat.lisp ----------------------------------------------
