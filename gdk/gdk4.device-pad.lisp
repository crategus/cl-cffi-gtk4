;;; ----------------------------------------------------------------------------
;;; gdk.device-pad.lisp
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
;;; GtkDevicePad
;;;
;;;     Pad device interface
;;;
;;; Types and Values
;;;
;;;     GdkDevicePad
;;;     GdkDevicePadFeature
;;;
;;; Functions
;;;
;;;     gdk_device_pad_get_n_groups
;;;     gdk_device_pad_get_group_n_modes
;;;     gdk_device_pad_get_n_features
;;;     gdk_device_pad_get_feature_group
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GdkDevicePad
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; enum GdkDevicePadFeature
;;;
;;; A pad feature.
;;;
;;; GDK_DEVICE_PAD_FEATURE_BUTTON
;;;     a button
;;;
;;; GDK_DEVICE_PAD_FEATURE_RING
;;;     a ring-shaped interactive area
;;;
;;; GDK_DEVICE_PAD_FEATURE_STRIP
;;;     a straight interactive area
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GdkDevicePad
;;;
;;; GdkDevicePad is an interface implemented by devices of type
;;; GDK_SOURCE_TABLET_PAD, it allows querying the features provided by the pad
;;; device.
;;;
;;; Tablet pads may contain one or more groups, each containing a subset of the
;;; buttons/rings/strips available. gdk_device_pad_get_n_groups() can be used to
;;; obtain the number of groups, gdk_device_pad_get_n_features() and
;;; gdk_device_pad_get_feature_group() can be combined to find out the number of
;;; buttons/rings/strips the device has, and how are they grouped.
;;;
;;; Each of those groups have different modes, which may be used to map each
;;; individual pad feature to multiple actions. Only one mode is effective
;;; (current) for each given group, different groups may have different current
;;; modes. The number of available modes in a group can be found out through
;;; gdk_device_pad_get_group_n_modes(), and the current mode for a given group
;;; will be notified through events of type GDK_PAD_GROUP_MODE.
;;; ----------------------------------------------------------------------------


;;; ----------------------------------------------------------------------------
;;; gdk_device_pad_get_n_groups ()
;;;
;;; int
;;; gdk_device_pad_get_n_groups (GdkDevicePad *pad);
;;;
;;; Returns the number of groups this pad device has. Pads have at least one
;;; group. A pad group is a subcollection of buttons/strip/rings that is
;;; affected collectively by a same current mode.
;;;
;;; pad :
;;;     a GdkDevicePad
;;;
;;; Returns :
;;;     The number of button/ring/strip groups in the pad.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_device_pad_get_group_n_modes ()
;;;
;;; int
;;; gdk_device_pad_get_group_n_modes (GdkDevicePad *pad,
;;;                                   int group_idx);
;;;
;;; Returns the number of modes that group may have.
;;;
;;; pad :
;;;     a GdkDevicePad
;;;
;;; group_idx :
;;;     group to get the number of available modes from
;;;
;;; Returns :
;;;     The number of modes available in group .
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_device_pad_get_n_features ()
;;;
;;; int
;;; gdk_device_pad_get_n_features (GdkDevicePad *pad,
;;;                                GdkDevicePadFeature feature);
;;;
;;; Returns the number of features a tablet pad has.
;;;
;;; pad :
;;;     a GdkDevicePad
;;;
;;; feature :
;;;     a pad feature
;;;
;;; Returns :
;;;     The amount of elements of type feature that this pad has.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_device_pad_get_feature_group ()
;;;
;;; int
;;; gdk_device_pad_get_feature_group (GdkDevicePad *pad,
;;;                                   GdkDevicePadFeature feature,
;;;                                   int feature_idx);
;;;
;;; Returns the group the given feature and idx belong to, or -1 if
;;; feature/index do not exist in pad .
;;;
;;; pad :
;;;     a GdkDevicePad
;;;
;;; feature :
;;;     the feature type to get the group from
;;;
;;; feature_idx :
;;;     the index of the feature to get the group from
;;;
;;; Returns :
;;;     The group number of the queried pad feature.
;;; ----------------------------------------------------------------------------

;;; --- End of file gdk.device-pad.lisp ----------------------------------------
