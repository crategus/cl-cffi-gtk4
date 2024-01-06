;;; ----------------------------------------------------------------------------
;;; gdk4.texture-downloader.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2023 Dieter Kaiser
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
;;; Types and Values
;;;
;;;     TextureDownloader
;;;
;;; Functions
;;;
;;;     gdk_texture_downloader_new
;;;
;;;     gdk_texture_downloader_copy
;;;     gdk_texture_downloader_download_bytes
;;;     gdk_texture_downloader_download_into
;;;     gdk_texture_downloader_free
;;;     gdk_texture_downloader_get_format
;;;     gdk_texture_downloader_set_format
;;;     gdk_texture_downloader_get_texture
;;;     gdk_texture_downloader_set_texture
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; Struct GdkTextureDownloader
;;;
;;; struct GdkTextureDownloader {
;;;   /* No available fields */
;;; }
;;;
;;; The GdkTextureDownloader is used to download the contents of a GdkTexture.
;;;
;;; It is intended to be created as a short-term object for a single download,
;;; but can be used for multipe downloads of different textures or with
;;; different settings.
;;;
;;; GdkTextureDownloader can be used to convert data between different formats.
;;; Create a GdkTexture for the existing format and then download it in a
;;; different format.
;;;
;;; Since 4.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_texture_downloader_new
;;;
;;; Creates a new texture downloader for texture.
;;;
;;; Since 4.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_texture_downloader_copy
;;;
;;; Creates a copy of the downloader.
;;;
;;; Since 4.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_texture_downloader_download_bytes
;;;
;;; Downloads the given texture pixels into a GBytes. The rowstride will be
;;; stored in the stride value.
;;;
;;; Since 4.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_texture_downloader_download_into
;;;
;;; Downloads the texture into local memory.
;;;
;;; Since 4.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_texture_downloader_free
;;;
;;; Frees the given downloader and all its associated resources.
;;;
;;; Since 4.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_texture_downloader_get_format
;;;
;;; Gets the format that the data will be downloaded in.
;;;
;;; Since 4.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_texture_downloader_set_format
;;;
;;; Sets the format the downloader will download.
;;;
;;; Since 4.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_texture_downloader_get_texture
;;;
;;; Gets the texture that the downloader will download.
;;;
;;; Since 4.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_texture_downloader_set_texture
;;;
;;; Changes the texture the downloader will download.
;;;
;;; Since 4.10
;;; ----------------------------------------------------------------------------

;;; --- End of file gdk4.texture-downloader.lisp -------------------------------
