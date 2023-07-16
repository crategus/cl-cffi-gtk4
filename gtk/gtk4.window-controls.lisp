;;; ----------------------------------------------------------------------------
;;; gtk4.window-controls.lisp
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
;;; GtkWindowControls
;;;
;;;     A widget displaying window buttons
;;;
;;; Types and Values
;;;
;;;     GtkWindowControls
;;;
;;; Accessors
;;;
;;;     gtk_window_controls_get_side
;;;     gtk_window_controls_set_side
;;;     gtk_window_controls_get_decoration_layout
;;;     gtk_window_controls_set_decoration_layout
;;;     gtk_window_controls_get_empty
;;;
;;; Functions
;;;
;;;     gtk_window_controls_new
;;;
;;; Properties
;;;
;;;     decoration-layout
;;;     empty
;;;     side
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkWindowControls
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;; ----------------------------------------------------------------------------

(in-package :gtk)


;;;GtkWindowControls shows window frame controls, such as minimize, maximize and close buttons, and the window icon.

;;;GtkWindowControls only displays start or end side of the controls (see “side”), so it's intended to be always used in pair with another GtkWindowControls using the opposite side, for example:

;;;CSS nodes
;;;<object class="GtkBox">
;;;  <child>
;;;    <object class="GtkWindowControls">
;;;      <property name="side">start</property>
;;;    </object>
;;;  </child>

;;;  ...

;;;  <child>
;;;    <object class="GtkWindowControls">
;;;      <property name="side">end</property>
;;;    </object>
;;;  </child>
;;;</object>
;;;A GtkWindowControls' CSS node is called windowcontrols. It contains subnodes corresponding to each title button. Which of the title buttons exist and where they are placed exactly depends on the desktop environment and “decoration-layout” value.

;;;When “empty” is TRUE, it gets the .empty style class.

;;;Accessibility
;;;GtkWindowHandle uses the GTK_ACCESSIBLE_ROLE_GROUP role.

;;;Functions
;;;gtk_window_controls_new ()
;;;GtkWidget *
;;;gtk_window_controls_new (GtkPackType side);
;;;Creates a new GtkWindowControls.

;;;Parameters
;;;side

;;;the side

;;;Returns
;;;a new GtkWindowControls.

;;;gtk_window_controls_get_side ()
;;;GtkPackType
;;;gtk_window_controls_get_side (GtkWindowControls *self);
;;;Gets the side set with gtk_window_controls_set_side().

;;;Parameters
;;;self

;;;a GtkWindowControls

;;;Returns
;;;the side

;;;gtk_window_controls_set_side ()
;;;void
;;;gtk_window_controls_set_side (GtkWindowControls *self,
;;;                              GtkPackType side);
;;;Sets the side for self , determining which part of decoration layout it uses.

;;;See gtk_window_controls_set_decoration_layout()

;;;Parameters
;;;self

;;;a GtkWindowControls

;;;side

;;;a side

;;;gtk_window_controls_get_decoration_layout ()
;;;const char *
;;;gtk_window_controls_get_decoration_layout
;;;                               (GtkWindowControls *self);
;;;Gets the decoration layout set with gtk_window_controls_set_decoration_layout().

;;;Parameters
;;;self

;;;a GtkWindowControls

;;;Returns
;;;the decoration layout or NULL if it is unset.

;;;[nullable]

;;;gtk_window_controls_set_decoration_layout ()
;;;void
;;;gtk_window_controls_set_decoration_layout
;;;                               (GtkWindowControls *self,
;;;                                const char *layout);
;;;Sets the decoration layout for the title buttons, overriding the “gtk-decoration-layout” setting.

;;;The format of the string is button names, separated by commas. A colon separates the buttons that should appear on the left from those on the right. Recognized button names are minimize, maximize, close and icon (the window icon).

;;;For example, “icon:minimize,maximize,close” specifies a icon on the left, and minimize, maximize and close buttons on the right.

;;;If “side” value is GTK_PACK_START , self will display the part before the colon, otherwise after that.

;;;Parameters
;;;self

;;;a GtkWindowControls

;;;layout

;;;a decoration layout, or NULL to unset the layout.

;;;[nullable]
;;;gtk_window_controls_get_empty ()
;;;gboolean
;;;gtk_window_controls_get_empty (GtkWindowControls *self);
;;;Gets whether the widget has any window buttons.

;;;Parameters
;;;self

;;;a GtkWindowControls

;;;Returns
;;;TRUE if the widget has window buttons, otherwise FALSE

;;;Types and Values
;;;GtkWindowControls
;;;typedef struct _GtkWindowControls GtkWindowControls;
;;;Property Details
;;;The “decoration-layout” property
;;;  “decoration-layout”        char *
;;;The decoration layout for window buttons. If this property is not set, the “gtk-decoration-layout” setting is used.

;;;See gtk_window_controls_set_decoration_layout() for information about the format of this string.

;;;Owner: GtkWindowControls

;;;Flags: Read / Write

;;;Default value: NULL

;;;The “empty” property
;;;  “empty”                    gboolean
;;;Whether the widget has any window buttons.

;;;Owner: GtkWindowControls

;;;Flags: Read

;;;Default value: TRUE

;;;The “side” property
;;;  “side”                     GtkPackType
;;;Whether the widget shows start or end side of the decoration layout.

;;;See gtk_window_controls_set_decoration_layout().

;;;Owner: GtkWindowControls

;;;Flags: Read / Write

;;;Default value: GTK_PACK_START

;;;See Also
;;;GtkHeaderBar


;;; --- End of file gtk4.window-controls.lisp ----------------------------------
