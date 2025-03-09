;;; ----------------------------------------------------------------------------
;;; gdk4.device-pad.lisp
;;;
;;; The documentation in this file is taken from the GDK 4 Reference Manual
;;; Version 4.16 and modified to document the Lisp binding to the GDK library,
;;; see <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2025 Dieter Kaiser
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
;;; GdkDevicePadFeature
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GdkDevicePadFeature" device-pad-feature
  (:export t
   :type-initializer "gdk_device_pad_feature_get_type")
  :button
  :ring
  :strip)

#+liber-documentation
(setf (liber:alias-for-symbol 'device-pad-feature)
      "GEnum"
      (liber:symbol-documentation 'device-pad-feature)
 "@version{2025-2-28}
  @begin{declaration}
(gobject:define-genum \"GdkDevicePadFeature\" device-pad-feature
  (:export t
   :type-initializer \"gdk_device_pad_feature_get_type\")
  :button
  :ring
  :strip)
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:button]{A button.}
      @entry[:ring]{A ring-shaped interactive area.}
      @entry[:strip]{A straight interactive area.}
    @end{table}
  @end{values}
  @begin{short}
    A pad feature.
  @end{short}
  @see-class{gdk:device-pad}")

;;; ----------------------------------------------------------------------------
;;; GdkDevicePad
;;; ----------------------------------------------------------------------------

(gobject:define-ginterface "GdkDevicePad" device-pad
  (:superclass device
   :export t
   :type-initializer "gdk_device_pad_get_type")
  nil)

#+liber-documentation
(setf (liber:alias-for-class 'device-pad)
      "Interface"
      (documentation 'device-pad 'type)
 "@version{2025-2-28}
  @begin{short}
    The @class{gdk:device-pad} interface is an interface implemented by devices
    of @code{:tablet-pad} type of the @symbol{gdk:input-source} enumeration.
  @end{short}
  It allows querying the features provided by the pad device

  Tablet pads may contain one or more groups, each containing a subset of the
  buttons/rings/strips available. The @fun{gdk:device-pad-n-groups} function
  can be used to obtain the number of groups, the
  @fun{gdk:device-pad-n-features} and @fun{gdk:device-pad-feature-group}
  functions can be combined to find out the number of buttons/rings/strips the
  device has, and how are they grouped.

  Each of those groups have different modes, which may be used to map each
  individual pad feature to multiple actions. Only one mode is effective
  (current) for each given group, different groups may have different current
  modes. The number of available modes in a group can be found out through the
  @fun{gdk:device-pad-group-n-modes} function, and the current mode for a
  given group will be notified through events of @code{:pad-group-mode} type of
  the @symbol{gdk:event-type} enumeration.
  @see-class{gdk:device}
  @see-symbol{gdk:input-source}
  @see-symbol{gdk:event-type}")

;;; ----------------------------------------------------------------------------
;;; gdk_device_pad_get_n_groups
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_device_pad_get_n_groups" device-pad-n-groups) :int
 #+liber-documentation
 "@version{#2025-2-28}
  @argument[pad]{a @class{gdk:device-pad} object}
  @begin{return}
    The integer with the number of button/ring/strip groups in the pad device.
  @end{return}
  @begin{short}
    Returns the number of groups this pad device has.
  @end{short}
  Pads have at least one group. A pad group is a subcollection of
  buttons/strip/rings that is affected collectively by a same current mode.
  @see-class{gdk:device-pad}"
  (pad (g:object device-pad)))

(export 'device-pad-n-groups)

;;; ----------------------------------------------------------------------------
;;; gdk_device_pad_get_group_n_modes
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_device_pad_get_group_n_modes" device-pad-group-n-modes) :int
 #+liber-documentation
 "@version{#2025-2-28}
  @argument[pad]{a @class{gdk:device-pad} object}
  @argument[index]{an integer for the group to get the number of available
    modes from}
  @return{The integer with the number of modes available in the group,}
  @begin{short}
    Returns the number of modes that the group may have.
  @end{short}
  @see-class{gdk:device-pad}"
  (pad (g:object device-pad))
  (index :int))

(export 'device-pad-group-n-modes)

;;; ----------------------------------------------------------------------------
;;; gdk_device_pad_get_n_features
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_device_pad_get_n_features" device-pad-n-features) :int
 #+liber-documentation
 "@version{#2025-2-28}
  @argument[pad]{a @class{gdk:device-pad} object}
  @argument[feature]{a @symbol{gdk:device-pad-feature} value for the pad
    feature}
  @begin{return}
    The integer with the amount of elements of type feature that this pad has.
  @end{return}
  @begin{short}
    Returns the number of features a tablet pad has.
  @end{short}
  @see-class{gdk:device-pad}
  @see-symbol{gdk:device-pad-feature}"
  (pad (g:object device-pad))
  (feature device-pad-feature))

(export 'device-pad-n-features)

;;; ----------------------------------------------------------------------------
;;; gdk_device_pad_get_feature_group
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_device_pad_get_feature_group" device-pad-feature-group) :int
 #+liber-documentation
 "@version{#2025-2-28}
  @argument[pad]{a @class{gdk:device-pad} object}
  @argument[feature]{a @symbol{gdk:device-pad-feature} value for the pad
    feature to get the group from}
  @argument[index]{an integer for the index of the feature to get the
    group from}
  @return{The integer with the group number of the queried pad feature.}
  @begin{short}
    Returns the group the given feature and @arg{index} belong to, or -1 if
    feature/index do not exist in pad .
  @end{short}
  @see-class{gdk:device-pad}
  @see-symbol{gdk:device-pad-feature}"
  (pad (g:object device-pad))
  (feature device-pad-feature)
  (index :int))

(export 'device-pad-feature-group)

;;; --- End of file gdk4.device-pad.lisp ---------------------------------------
