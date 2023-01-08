;;; ----------------------------------------------------------------------------
;;; gdk.vulkan-context.lisp
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


;;;GdkVulkanContext
;;;GdkVulkanContext — Vulkan draw context

;;;Functions
;;;VkDevice	gdk_vulkan_context_get_device ()
;;;uint32_t	gdk_vulkan_context_get_draw_index ()
;;;VkSemaphore	gdk_vulkan_context_get_draw_semaphore ()
;;;VkImage	gdk_vulkan_context_get_image ()
;;;VkFormat	gdk_vulkan_context_get_image_format ()
;;;VkInstance	gdk_vulkan_context_get_instance ()
;;;uint32_t	gdk_vulkan_context_get_n_images ()
;;;VkPhysicalDevice	gdk_vulkan_context_get_physical_device ()
;;;VkQueue	gdk_vulkan_context_get_queue ()
;;;uint32_t	gdk_vulkan_context_get_queue_family_index ()
;;;Signals
;;;void	images-updated	Run Last
;;;Types and Values
;;; 	GdkVulkanContext
;;;enum	GdkVulkanError
;;;Object Hierarchy
;;;    GObject
;;;    ╰── GdkDrawContext
;;;        ╰── GdkVulkanContext
;;;Implemented Interfaces
;;;GdkVulkanContext implements GInitable.

(in-package :gdk)


;;;Description
;;;GdkVulkanContext is an object representing the platform-specific Vulkan draw context.

;;;GdkVulkanContexts are created for a GdkSurface using gdk_surface_create_vulkan_context(), and the context will match the the characteristics of the surface.

;;;Support for GdkVulkanContext is platform-specific, context creation can fail, returning NULL context.

;;;Functions
;;;gdk_vulkan_context_get_device ()
;;;VkDevice
;;;gdk_vulkan_context_get_device (GdkVulkanContext *context);
;;;Gets the Vulkan device that this context is using.

;;;Parameters
;;;context

;;;a GdkVulkanContext

;;;
;;;Returns
;;;the VkDevice.

;;;[transfer none]

;;;gdk_vulkan_context_get_draw_index ()
;;;uint32_t
;;;gdk_vulkan_context_get_draw_index (GdkVulkanContext *context);
;;;Gets the index of the image that is currently being drawn.

;;;This function can only be used between gdk_draw_context_begin_frame() and gdk_draw_context_end_frame() calls.

;;;Parameters
;;;context

;;;a GdkVulkanContext

;;;
;;;Returns
;;;the index of the images that is being drawn

;;;gdk_vulkan_context_get_draw_semaphore ()
;;;VkSemaphore
;;;gdk_vulkan_context_get_draw_semaphore (GdkVulkanContext *context);
;;;Gets the Vulkan semaphore that protects access to the image that is currently being drawn.

;;;This function can only be used between gdk_draw_context_begin_frame() and gdk_draw_context_end_frame() calls.

;;;Parameters
;;;context

;;;a GdkVulkanContext

;;;
;;;Returns
;;;the VkSemaphore.

;;;[transfer none]

;;;gdk_vulkan_context_get_image ()
;;;VkImage
;;;gdk_vulkan_context_get_image (GdkVulkanContext *context,
;;;                              guint id);
;;;Gets the image with index id that this context is using.

;;;Parameters
;;;context

;;;a GdkVulkanContext

;;;
;;;id

;;;the index of the image to return

;;;
;;;Returns
;;;the VkImage.

;;;[transfer none]

;;;gdk_vulkan_context_get_image_format ()
;;;VkFormat
;;;gdk_vulkan_context_get_image_format (GdkVulkanContext *context);
;;;Gets the image format that this context is using.

;;;Parameters
;;;context

;;;a GdkVulkanContext

;;;
;;;Returns
;;;the VkFormat.

;;;[transfer none]

;;;gdk_vulkan_context_get_instance ()
;;;VkInstance
;;;gdk_vulkan_context_get_instance (GdkVulkanContext *context);
;;;Gets the Vulkan instance that is associated with context .

;;;Parameters
;;;context

;;;a GdkVulkanContext

;;;
;;;Returns
;;;the VkInstance.

;;;[transfer none]

;;;gdk_vulkan_context_get_n_images ()
;;;uint32_t
;;;gdk_vulkan_context_get_n_images (GdkVulkanContext *context);
;;;Gets the number of images that this context is using in its swap chain.

;;;Parameters
;;;context

;;;a GdkVulkanContext

;;;
;;;Returns
;;;the number of images

;;;gdk_vulkan_context_get_physical_device ()
;;;VkPhysicalDevice
;;;gdk_vulkan_context_get_physical_device
;;;                               (GdkVulkanContext *context);
;;;Gets the Vulkan physical device that this context is using.

;;;Parameters
;;;context

;;;a GdkVulkanContext

;;;
;;;Returns
;;;the VkPhysicalDevice.

;;;[transfer none]

;;;gdk_vulkan_context_get_queue ()
;;;VkQueue
;;;gdk_vulkan_context_get_queue (GdkVulkanContext *context);
;;;Gets the Vulkan queue that this context is using.

;;;Parameters
;;;context

;;;a GdkVulkanContext

;;;
;;;Returns
;;;the VkQueue.

;;;[transfer none]

;;;gdk_vulkan_context_get_queue_family_index ()
;;;uint32_t
;;;gdk_vulkan_context_get_queue_family_index
;;;                               (GdkVulkanContext *context);
;;;Gets the family index for the queue that this context is using. See vkGetPhysicalDeviceQueueFamilyProperties().

;;;Parameters
;;;context

;;;a GdkVulkanContext

;;;
;;;Returns
;;;the index

;;;Types and Values
;;;GdkVulkanContext
;;;typedef struct _GdkVulkanContext GdkVulkanContext;
;;;The GdkVulkanContext struct contains only private fields and should not be accessed directly.

;;;enum GdkVulkanError
;;;Error enumeration for GdkVulkanContext.

;;;Members
;;;GDK_VULKAN_ERROR_UNSUPPORTED

;;;Vulkan is not supported on this backend or has not been compiled in.

;;;
;;;GDK_VULKAN_ERROR_NOT_AVAILABLE

;;;Vulkan support is not available on this Surface

;;;
;;;Signal Details
;;;The “images-updated” signal
;;;void
;;;user_function (GdkVulkanContext *context,
;;;               gpointer          user_data)
;;;This signal is emitted when the images managed by this context have changed. Usually this means that the swapchain had to be recreated, for example in response to a change of the surface size.

;;;Parameters
;;;context

;;;the object on which the signal is emitted

;;;
;;;user_data

;;;user data set when the signal handler was connected.

;;;
;;;Flags: Run Last

;;; --- End of file gdk.vulkan-context.lisp ------------------------------------
