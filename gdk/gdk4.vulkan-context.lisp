;;; ----------------------------------------------------------------------------
;;; gdk4.vulkan-context.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.16 and modified to document the Lisp binding to the GDK library.
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
;;; GdkVulkanContext
;;;
;;;     Vulkan draw context
;;;
;;; Types and Values
;;;
;;;     GdkVulkanContext
;;;     GdkVulkanError
;;;
;;; Functions
;;;
;;;     gdk_vulkan_context_get_device
;;;     gdk_vulkan_context_get_draw_index
;;;     gdk_vulkan_context_get_draw_semaphore
;;;     gdk_vulkan_context_get_image
;;;     gdk_vulkan_context_get_image_format
;;;     gdk_vulkan_context_get_instance
;;;     gdk_vulkan_context_get_n_images
;;;     gdk_vulkan_context_get_physical_device
;;;     gdk_vulkan_context_get_queue
;;;     gdk_vulkan_context_get_queue_family_index
;;;
;;; Signals
;;;
;;;     images-updated
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GdkDrawContext
;;;         ╰── GdkVulkanContext
;;;
;;; Implemented Interfaces
;;;
;;;     GInitable
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkVulkanContext
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GdkVulkanContext" vulkan-context
  (:superclass draw-context
   :export t
   :interfaces nil
   :type-initializer "gdk_vulkan_context_get_type")
  nil)

#+(and gtk-4-14 gtk-warn-deprecated)
(defmethod initialize-instance :after ((obj vulkan-context) &key)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GDK:VULKAN-CONTEXT is deprecated since 4.14")))

#+liber-documentation
(setf (documentation 'vulkan-context 'type)
 "@version{#2024-11-7}
  @begin{short}
    The @class{gdk:vulkan-context} object is an object representing the platform
    specific Vulkan draw context.
  @end{short}
  The @sym{gdk:vulkan-context} object is created for a @class{gdk:surface}
  object using the @fun{gdk:surface-create-vulkan-context} function, and the
  Vulkan context will match the characteristics of the surface.

  Support for the @class{gdk:vulkan-context} object is platform specific,
  context creation can fail, returning a @code{NULL} context.
  @begin[Warning]{dictionary}
    The @class{gdk:vulkan-context} class is deprecated since 4.14. GTK does not
    expose any Vulkan internals. This struct is a leftover that was accidentally
    exposed.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"images-updated\" signal}
      @begin{pre}
lambda (context)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[context]{The @class{gdk:vulkan-context} object on which the
          signal is emitted.}
      @end{table}
      The signal is emitted when the images managed by this context have
      changed. Usually this means that the swapchain had to be recreated, for
      example in response to a change of the surface size.
  @end{dictionary}
  @see-class{gdk:draw-context}")

;;; --- End of file gdk4.vulkan-context.lisp -----------------------------------
