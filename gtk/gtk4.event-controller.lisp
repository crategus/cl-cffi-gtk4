;;; ----------------------------------------------------------------------------
;;; gtk4.event-controller.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.6 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2019 - 2023 Dieter Kaiser
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
;;; GtkEventController
;;;
;;;     Self-contained handler of series of events
;;;
;;; Types and Values
;;;
;;;     GtkEventController
;;;     GtkPropagationPhase
;;;     GtkPropagationLimit
;;;
;;; Accessors
;;;
;;;     gtk_event_controller_get_name
;;;     gtk_event_controller_set_name
;;;     gtk_event_controller_get_propagation_limit
;;;     gtk_event_controller_set_propagation_limit
;;;     gtk_event_controller_get_propagation_phase
;;;     gtk_event_controller_set_propagation_phase
;;;     gtk_event_controller_get_widget
;;;
;;; Functions
;;;
;;;     gtk_event_controller_reset
;;;     gtk_event_controller_get_current_event
;;;     gtk_event_controller_get_current_event_device
;;;     gtk_event_controller_get_current_event_state
;;;     gtk_event_controller_get_current_event_time
;;;
;;; Properties
;;;
;;;     name
;;;     propagation-limit
;;;     propagation-phase
;;;     widget
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkEventController
;;;         ├── GtkGesture
;;;         ├── GtkDropTarget
;;;         ├── GtkDropTargetAsync
;;;         ├── GtkEventControllerKey
;;;         ├── GtkEventControllerFocus
;;;         ├── GtkEventControllerLegacy
;;;         ├── GtkEventControllerMotion
;;;         ├── GtkEventControllerScroll
;;;         ├── GtkPadController
;;;         ╰── GtkShortcutController
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkPropagationPhase
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GtkPropagationPhase" propagation-phase
  (:export t
   :type-initializer "gtk_propagation_phase_get_type")
  (:none 0)
  (:capture 1)
  (:bubble 2)
  (:target 3))

#+liber-documentation
(setf (liber:alias-for-symbol 'propagation-phase)
      "GEnum"
      (liber:symbol-documentation 'propagation-phase)
 "@version{#2022-8-22}
  @begin{short}
    Describes the stage at which events are fed into a
    @class{gtk:event-controller} object.
  @end{short}
  @begin{pre}
(gobject:define-g-enum \"GtkPropagationPhase\" propagation-phase
  (:export t
   :type-initializer \"gtk_propagation_phase_get_type\")
  (:none 0)
  (:capture 1)
  (:bubble 2)
  (:target 3))
  @end{pre}
  @begin[code]{table}
    @entry[:none]{Events are not delivered.}
    @entry[:capture]{Events are delivered in the capture phase. The capture
      phase happens before the bubble phase, runs from the toplevel down to the
      event widget. This option should only be used on containers that might
      possibly handle events before their children do.}
    @entry[:bubble]{Events are delivered in the bubble phase. The bubble phase
      happens after the capture phase, and before the default handlers are run.
      This phase runs from the event widget, up to the toplevel.}
    @entry[:target]{Events are delivered in the default widget event handlers,
      note that widget implementations must chain up on button, motion, touch
      and grab broken handlers for controllers in this phase to be run.}
  @end{table}
  @see-class{gtk:event-controller}")

;;; ----------------------------------------------------------------------------
;;; enum GtkPropagationLimit
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GtkPropagationLimit" propagation-limit
  (:export t
   :type-initializer "gtk_propagation_limit_get_type")
  (:none 0)
  (:same-native 1))

#+liber-documentation
(setf (liber:alias-for-symbol 'propagation-limit)
      "GEnum"
      (liber:symbol-documentation 'propagation-limit)
 "@version{#2022-8-22}
  @begin{short}
    Describes limits of a @class{gtk:event-controller} object for handling
    events targeting other widgets.
  @end{short}
  @begin{pre}
(gobject:define-g-enum \"GtkPropagationLimit\" propagation-limit
  (:export t
   :type-initializer \"gtk_propagation_limit_get_type\")
  (:none 0)
  (:same-native 1))
  @end{pre}
  @begin[code]{table}
    @entry[:none]{Events are handled regardless of what their target is.}
    @entry[:same-native]{Events are only handled if their target is in the same
    @class{gtk:native} widget as the event controllers widget. Note that some
      event types have two targets (origin and destination).}
  @end{table}
  @see-class{gtk:event-controller}
  @see-class{gtk:native}")

;;; ----------------------------------------------------------------------------
;;; struct GtkEventController
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkEventController" event-controller
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gtk_event_controller_get_type")
  ((name
    event-controller-name
    "name" "gchararray" t t)
   (propagation-limit
    event-controller-propagation-limit
    "propagation-limit" "GtkPropagationLimit" t t)
   (propagation-phase
    event-controller-propagation-phase
    "propagation-phase" "GtkPropagationPhase" t t)
   (widget
    event-controller-widget
    "widget" "GtkWidget" t nil)))

#+liber-documentation
(setf (documentation 'event-controller 'type)
 "@version{#2022-8-22}
  @begin{short}
    The @sym{gtk:event-controller} class is the base class for event
    controllers.
  @end{short}
  These are ancillary objects associated to widgets, which react to
  @class{gdk:event} events, and possibly trigger actions as a consequence.

  Event controllers are added to a widget with the
  @fun{gtk:widget-add-controller} function. It is rarely necessary to explicitly
  remove a controller with the @fun{gtk:widget-remove-controller} function.

  See the chapter on input handling for an overview of the basic concepts, such
  as the capture and bubble phases of even propagation.
  @see-slot{gtk:event-controller-name}
  @see-slot{gtk:event-controller-propagation-limit}
  @see-slot{gtk:event-controller-propagation-phase}
  @see-slot{gtk:event-controller-widget}
  @see-class{gdk:event}
  @see-class{gtk:gesture}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- event-controller-name ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "name" 'event-controller) t)
 "The @code{name} property of type @code{:string} (Read / Write) @br{}
  The name for this controller, typically used for debugging purposes. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'event-controller-name)
      "Accessor"
      (documentation 'event-controller-name 'function)
 "@version{#2022-8-22}
  @syntax[]{(gtk:event-controller-name object) => name)}
  @syntax[]{(setf (gtk:event-controller-name object) name)}
  @argument[object]{a @class{gtk:event-controller} object}
  @argument[name]{a string with the name for the controller}
  @begin{short}
    Accessor of the @slot[gtk:event-controller]{name} slot of the
    @class{gtk:event-controller} class.
  @end{short}
  The @sym{gtk:event-controller-name} function gets the name of the event
  controller. The @sym{(setf gtk:event-controller-name)} function sets a name.
  The name can be used for debugging purposes.
  @see-class{gtk:event-controller}")

;;; --- event-controller-propagation-limit ---------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "propagation-limit"
                                               'event-controller) t)
 "The @code{propagation-limit} property of type
  @symbol{gtk:propagation-limit} (Read / Write) @br{}
  The limit for which events the controller will handle. @br{}
  Default value: @code{:same-native}")

#+liber-documentation
(setf (liber:alias-for-function 'event-controller-propagation-limit)
      "Accessor"
      (documentation 'event-controller-propagation-limit 'function)
 "@version{#2022-8-22}
  @syntax[]{(gtk:event-controller-propagation-limit object) => limit)}
  @syntax[]{(setf (gtk:event-controller-propagation-limit object) limit)}
  @argument[object]{a @class{gtk:event-controller} object}
  @argument[limit]{a value of the @symbol{gtk:propagation-limit} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:event-controller]{propagation-limit} slot of the
    @class{gtk:event-controller} class.
  @end{short}
  The @sym{gtk:event-controller-propagation-limit} function gets the propagation
  limit of the event controller. The
  @sym{(setf gtk:event-controller-propagation-limit)} function sets the event
  propagation limit.

  If the limit is set to @code{:same-native}, the controller will not handle
  events that are targeted at widgets on a different surface, such as popovers.
  @see-class{gtk:event-controller}
  @see-symbol{gtk:propagation-limit}")

;;; --- event-controller-propagation-phase ---------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "propagation-phase"
                                               'event-controller) t)
 "The @code{propagation-phase} property of type
  @symbol{gtk:propagation-phase} (Read / Write) @br{}
  The propagation phase at which this controller will handle events. @br{}
  Default value: @code{:bubble}")

#+liber-documentation
(setf (liber:alias-for-function 'event-controller-propagation-phase)
      "Accessor"
      (documentation 'event-controller-propagation-phase 'function)
 "@version{#2022-8-22}
  @syntax[]{(gtk:event-controller-propagation-phase object) => phase)}
  @syntax[]{(setf (gtk:event-controller-propagation-phase object) phase)}
  @argument[object]{a @class{gtk:event-controller} object}
  @argument[phase]{a value of the @symbol{gtk:propagation-phase} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:event-controller]{propagation-phase} slot of the
    @class{gtk:event-controller} class.
  @end{short}
  The @sym{gtk:event-controller-propagation-phase} function gets the
  propagation phase at which controller handles events. The
  @sym{(setf gtk:event-controller-propagation-phase)} sets the propagation
  phase.

  If @arg{phase} is @code{:none}, no automatic event handling will be performed,
  but other additional gesture maintenance will.
  @see-class{gtk:event-controller}
  @see-symbol{gtk:propagation-phase}")

;;; --- event-controller-widget --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "widget"
                                               'event-controller) t)
 "The @code{widget} property of type @class{gtk:widget} (Read) @br{}
  The widget receiving the @class{gdk:event} events that the controller will
  handle.")

#+liber-documentation
(setf (liber:alias-for-function 'event-controller-widget)
      "Accessor"
      (documentation 'event-controller-widget 'function)
 "@version{#2022-8-22}
  @syntax[]{(gtk:event-controller-widget object) => widget)}
  @syntax[]{(setf (gtk:event-controller-widget object) widget)}
  @argument[object]{a @class{gtk:event-controller} object}
  @argument[widget]{a @class{gtk:widget} widget}
  @begin{short}
    Accessor of the @slot[gtk:event-controller]{widget} slot of the
    @class{gtk:event-controller} class.
  @end{short}
  The @sym{gtk:event-controller-widget} function returns the @class{gtk:widget}
  the event controller relates to.
  @see-class{gtk:event-controller}
  @see-class{gtk:widget}")

;;; ----------------------------------------------------------------------------
;;; gtk_event_controller_reset ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_event_controller_reset" event-controller-reset) :void
 #+liber-documentation
 "@version{#2022-8-22}
  @argument[controller]{a @class{gtk:event-controller} object}
  @begin{short}
    Resets the controller to a clean state.
  @end{short}
  Every interaction the controller did through the
  @fun{gtk:event-controller-handle-event} function will be dropped at this
  point.
  @see-class{gtk:event-controller}
  @see-function{gtk:event-controller-handle-event}"
  (controller (g:object event-controller)))

(export 'event-controller-reset)

;;; ----------------------------------------------------------------------------
;;; gtk_event_controller_get_current_event ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_event_controller_get_current_event"
           event-controller-current-event) gdk:event
 #+liber-documentation
 "@version{#2022-8-22}
  @argument[controller]{a @class{gtk:event-controller} object}
  @return{The @class{gdk:event} event that is currently handled by
    @arg{controller}.}
  @begin{short}
    Returns the event that is currently being handled by the controller, and
    @code{nil} at other times.
  @end{short}
  @see-class{gtk:event-controller}
  @see-class{gdk:event}"
  (controller (g:object event-controller)))

(export 'event-controller-current-event)

;;; ----------------------------------------------------------------------------
;;; gtk_event_controller_get_current_event_device ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_event_controller_get_current_event_device"
           event-controller-current-event-device) (g:object gdk-device)
 #+liber-documentation
 "@version{#2022-8-22}
  @argument[controller]{a @class{gtk:event-controller} object}
  @return{The @class{gdk-device} object that is currently handled by
    @arg{controller}.}
  @begin{short}
    Returns the device of the event that is currently being handled by the
    controller, and @code{nil} otherwise.
  @end{short}
  @see-class{gtk:event-controller}
  @see-class{gdk-device}"
  (controller (g:object event-controller)))

(export 'event-controller-current-event-device)

;;; ----------------------------------------------------------------------------
;;; gtk_event_controller_get_current_event_state ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_event_controller_get_current_event_state"
           event-controller-current-event-state) gdk:modifier-type
 #+liber-documentation
 "@version{#2022-8-22}
  @argument[controller]{a @class{gtk:event-controller} object}
  @return{The @symbol{gdk:modifier-type} state that is currently handled by
    @arg{controller}.}
  @begin{short}
    Returns the modifier state of the event that is currently being handled by
    the controller.
  @end{short}
  @see-class{gtk:event-controller}
  @see-symbol{gdk:modifier-type}"
  (controller (g:object event-controller)))

(export 'event-controller-current-event-state)

;;; ----------------------------------------------------------------------------
;;; gtk_event_controller_get_current_event_time ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_event_controller_get_current_event_time"
           event-controller-current-event-time) :uint32
 #+liber-documentation
 "@version{#2022-8-22}
  @argument[controller]{a @class{gtk:event-controller} object}
  @return{An unsigned integer with the timestamp of the event that is currently
    handled @arg{controller}.}
  @begin{short}
    Returns the timestamp of the event that is currently being handled by the
    controller, and 0 otherwise.
  @end{short}
  @see-class{gtk:event-controller}"
  (controller (g:object event-controller)))

(export 'event-controller-current-event-time)

;;; --- End of file gtk4.event-controller.lisp ---------------------------------
