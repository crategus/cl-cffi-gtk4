;;; ----------------------------------------------------------------------------
;;; gdk4.vulkan-context.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.10 and modified to document the Lisp binding to the GDK library.
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
;;; enum GdkVulkanError
;;;
;;; Error enumeration for GdkVulkanContext.
;;;
;;; GDK_VULKAN_ERROR_UNSUPPORTED
;;;     Vulkan is not supported on this backend or has not been compiled in.
;;;
;;; GDK_VULKAN_ERROR_NOT_AVAILABLE
;;;     Vulkan support is not available on this Surface
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GdkVulkanContext
;;;
;;; typedef struct _GdkVulkanContext GdkVulkanContext;
;;;
;;; The GdkVulkanContext struct contains only private fields and should not be
;;; accessed directly.
;;;
;;; Description
;;;
;;; GdkVulkanContext is an object representing the platform-specific Vulkan
;;; draw context.
;;;
;;; GdkVulkanContexts are created for a GdkSurface using
;;; gdk_surface_create_vulkan_context(), and the context will match the the
;;; characteristics of the surface.
;;;
;;; Support for GdkVulkanContext is platform-specific, context creation can
;;; fail, returning NULL context.
;;;
;;; Signal Details
;;;
;;; The “images-updated” signal
;;;
;;; void
;;; user_function (GdkVulkanContext *context,
;;;                gpointer          user_data)
;;;
;;; This signal is emitted when the images managed by this context have changed.
;;; Usually this means that the swapchain had to be recreated, for example in
;;; response to a change of the surface size.
;;;
;;; context :
;;;     the object on which the signal is emitted
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; Flags: Run Last
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkVulkanContext" vulkan-context
  (:superclass draw-context
   :export t
   :interfaces nil
   :type-initializer "gdk_vulkan_context_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; gdk_vulkan_context_get_device ()
;;;
;;; VkDevice
;;; gdk_vulkan_context_get_device (GdkVulkanContext *context);
;;;
;;; Gets the Vulkan device that this context is using.
;;;
;;; context :
;;;     a GdkVulkanContext
;;;
;;; Returns :
;;;     the VkDevice.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_vulkan_context_get_draw_index ()
;;;
;;; uint32_t
;;; gdk_vulkan_context_get_draw_index (GdkVulkanContext *context);
;;;
;;; Gets the index of the image that is currently being drawn.
;;;
;;; This function can only be used between gdk_draw_context_begin_frame() and
;;; gdk_draw_context_end_frame() calls.
;;;
;;; context :
;;;     a GdkVulkanContext
;;;
;;; Returns:
;;;     the index of the images that is being drawn
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_vulkan_context_get_draw_semaphore ()
;;;
;;; VkSemaphore
;;; gdk_vulkan_context_get_draw_semaphore (GdkVulkanContext *context);
;;;
;;; Gets the Vulkan semaphore that protects access to the image that is
;;; currently being drawn.
;;;
;;; This function can only be used between gdk_draw_context_begin_frame() and
;;; gdk_draw_context_end_frame() calls.
;;;
;;; context :
;;;     a GdkVulkanContext
;;;
;;; Returns :
;;;     the VkSemaphore.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_vulkan_context_get_image ()
;;;
;;; VkImage
;;; gdk_vulkan_context_get_image (GdkVulkanContext *context,
;;;                               guint id);
;;;
;;; Gets the image with index id that this context is using.
;;;
;;; context :
;;;     a GdkVulkanContext
;;;
;;; id :
;;;     the index of the image to return
;;;
;;; Returns :
;;;     the VkImage.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_vulkan_context_get_image_format ()
;;;
;;; VkFormat
;;; gdk_vulkan_context_get_image_format (GdkVulkanContext *context);
;;;
;;; Gets the image format that this context is using.
;;;
;;; context :
;;;     a GdkVulkanContext
;;;
;;; Returns :
;;;     the VkFormat.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_vulkan_context_get_instance ()
;;;
;;; VkInstance
;;; gdk_vulkan_context_get_instance (GdkVulkanContext *context);
;;;
;;; Gets the Vulkan instance that is associated with context .
;;;
;;; context :
;;;     a GdkVulkanContext
;;;
;;; Returns :
;;;     the VkInstance.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_vulkan_context_get_n_images ()
;;;
;;; uint32_t
;;; gdk_vulkan_context_get_n_images (GdkVulkanContext *context);
;;;
;;; Gets the number of images that this context is using in its swap chain.
;;;
;;; context :
;;;     a GdkVulkanContext
;;;
;;; Returns :
;;;     the number of images
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_vulkan_context_get_physical_device ()
;;;
;;; VkPhysicalDevice
;;; gdk_vulkan_context_get_physical_device
;;;                                (GdkVulkanContext *context);
;;;
;;; Gets the Vulkan physical device that this context is using.
;;;
;;; context :
;;;     a GdkVulkanContext
;;;
;;; Returns :
;;;     the VkPhysicalDevice.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_vulkan_context_get_queue ()
;;;
;;; VkQueue
;;; gdk_vulkan_context_get_queue (GdkVulkanContext *context);
;;;
;;; Gets the Vulkan queue that this context is using.
;;;
;;; context :
;;;     a GdkVulkanContext
;;;
;;; Returns :
;;;     the VkQueue.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_vulkan_context_get_queue_family_index ()
;;;
;;; uint32_t
;;; gdk_vulkan_context_get_queue_family_index
;;;                                (GdkVulkanContext *context);
;;;
;;; Gets the family index for the queue that this context is using. See
;;; vkGetPhysicalDeviceQueueFamilyProperties().
;;;
;;; context :
;;;     a GdkVulkanContext
;;;
;;; Returns :
;;;     the index
;;; ----------------------------------------------------------------------------

;;; --- End of file gdk4.vulkan-context.lisp -----------------------------------
