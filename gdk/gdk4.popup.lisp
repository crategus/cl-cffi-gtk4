;;; ----------------------------------------------------------------------------
;;; gdk.popup.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GDK library.
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
;;; Popups
;;;
;;;     Interface for popup surfaces
;;;
;;; Types and Values
;;;
;;;     GdkPopup
;;;
;;; Functions
;;;
;;;     gdk_popup_present
;;;     gdk_popup_get_surface_anchor
;;;     gdk_popup_get_rect_anchor
;;;     gdk_popup_get_parent
;;;     gdk_popup_get_position_x
;;;     gdk_popup_get_position_y
;;;     gdk_popup_get_autohide
;;;
;;; Properties
;;;
;;;     autohide
;;;     parent
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GdkPopup
;;; ----------------------------------------------------------------------------

(in-package :gdk)


;;;Description
;;;A GdkPopup is a surface that is attached to another surface, called its “parent”, and is positioned relative to it.

;;;GdkPopups are typically used to implement menus and similar popups. They can be modal, which is indicated by the “autohide” property.

;;;Functions
;;;gdk_popup_present ()
;;;gboolean
;;;gdk_popup_present (GdkPopup *popup,
;;;                   int width,
;;;                   int height,
;;;                   GdkPopupLayout *layout);
;;;Present popup after having processed the GdkPopupLayout rules. If the popup was previously now showing, it will be showed, otherwise it will change position according to layout .

;;;After calling this function, the result should be handled in response to the “layout” signal being emitted. The resulting popup position can be queried using gdk_popup_get_position_x(), gdk_popup_get_position_y(), and the resulting size will be sent as parameters in the layout signal. Use gdk_popup_get_rect_anchor() and gdk_popup_get_surface_anchor() to get the resulting anchors.

;;;Presenting may fail, for example if the popup is set to autohide and is immediately hidden upon being presented. If presenting failed, the “layout” signal will not me emitted.

;;;Parameters
;;;popup

;;;the GdkPopup to show

;;;
;;;width

;;;the unconstrained popup width to layout

;;;
;;;height

;;;the unconstrained popup height to layout

;;;
;;;layout

;;;the GdkPopupLayout object used to layout

;;;
;;;Returns
;;;FALSE if it failed to be presented, otherwise TRUE.

;;;gdk_popup_get_surface_anchor ()
;;;GdkGravity
;;;gdk_popup_get_surface_anchor (GdkPopup *popup);
;;;Gets the current popup surface anchor.

;;;The value returned may change after calling gdk_popup_present(), or after the “layout” signal is emitted.

;;;Parameters
;;;popup

;;;a GdkPopup

;;;
;;;Returns
;;;the current surface anchor value of popup

;;;gdk_popup_get_rect_anchor ()
;;;GdkGravity
;;;gdk_popup_get_rect_anchor (GdkPopup *popup);
;;;Gets the current popup rectangle anchor.

;;;The value returned may change after calling gdk_popup_present(), or after the “layout” signal is emitted.

;;;Parameters
;;;popup

;;;a GdkPopup

;;;
;;;Returns
;;;the current rectangle anchor value of popup

;;;gdk_popup_get_parent ()
;;;GdkSurface *
;;;gdk_popup_get_parent (GdkPopup *popup);
;;;Returns the parent surface of a popup.

;;;Parameters
;;;popup

;;;a GdkPopup

;;;
;;;Returns
;;;the parent surface.

;;;[transfer none]

;;;gdk_popup_get_position_x ()
;;;int
;;;gdk_popup_get_position_x (GdkPopup *popup);
;;;Obtains the position of the popup relative to its parent.

;;;Parameters
;;;popup

;;;a GdkPopup

;;;
;;;Returns
;;;the X coordinate of popup position

;;;gdk_popup_get_position_y ()
;;;int
;;;gdk_popup_get_position_y (GdkPopup *popup);
;;;Obtains the position of the popup relative to its parent.

;;;Parameters
;;;popup

;;;a GdkPopup

;;;
;;;Returns
;;;the Y coordinate of popup position

;;;gdk_popup_get_autohide ()
;;;gboolean
;;;gdk_popup_get_autohide (GdkPopup *popup);
;;;Returns whether this popup is set to hide on outside clicks.

;;;Parameters
;;;popup

;;;a GdkPopup

;;;
;;;Returns
;;;TRUE if popup will autohide

;;;Types and Values
;;;GdkPopup
;;;typedef struct _GdkPopup GdkPopup;
;;;Interface for popup surfaces.

;;;Property Details
;;;The “autohide” property
;;;  “autohide”                 gboolean
;;;Whether to hide on outside clicks.

;;;Owner: GdkPopup

;;;Flags: Read / Write / Construct Only

;;;Default value: FALSE

;;;The “parent” property
;;;  “parent”                   GdkSurface *
;;;The parent surface.

;;;Owner: GdkPopup

;;;Flags: Read / Write / Construct Only

;;;See Also
;;;GdkToplevel, GdkSurface

;;; --- End of file gdk.popup.lisp ---------------------------------------------
