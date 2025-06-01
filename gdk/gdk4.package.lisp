;;; ----------------------------------------------------------------------------
;;; gdk4.package.lisp
;;;
;;; The documentation in this file is taken from the GDK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GDK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2025 Dieter Kaiser
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

(defpackage :gdk
  (:use :iterate :common-lisp)
  (:import-from #:cffi)
  (:import-from #:glib)
  (:import-from #:gobject)
  ;; Import the symbols from GDK-PIXUF
  ;; TODO: We have a problem with the documentation. The symbols are documented
  ;; twice as gdk:pixbuf and as gdk-pixbuf:pixbuf. Resolve this issue.
  (:import-from #:gdk-pixbuf ;; Symbols from gdk-pixbuf.structure.lisp
                             #:colorspace
                             #:pixbuf
                             #:pixbuf-bits-per-sample
                             #:pixbuf-colorspace
                             #:pixbuf-has-alpha
                             #:pixbuf-height
                             #:pixbuf-n-channels
                             #:pixbuf-pixel-bytes
                             #:pixbuf-pixels
                             #:pixbuf-rowstride
                             #:pixbuf-width
                             #:pixbuf-pixels-with-length
                             #:pixbuf-byte-length
                             #:pixbuf-option
                             #:pixbuf-remove-option
                             #:pixbuf-copy-options
                             #:pixbuf-read-pixels
                             ;; Symbols from gdk-pixbuf.load.lisp
                             #:pixbuf-file-info
                             #:pixbuf-new-from-file
                             #:pixbuf-new-from-file-at-size
                             #:pixbuf-new-from-file-at-scale
                             #:pixbuf-new-from-resource
                             #:pixbuf-new-from-resource-at-scale
                            ;; Symbols from gdk-pixbuf.loader.lisp
                             #:pixbuf-loader
                             #:pixbuf-loader-new
                             #:pixbuf-loader-write
                             #:pixbuf-loader-set-size
                             #:pixbuf-loader-pixbuf
                             #:pixbuf-loader-animation
                             #:pixbuf-loader-close
                             ;; Symbols from gdk-pixbuf.save.lisp
                             #:pixbuf-save
                             ;; Symbols from gdk-pixbuf.memory.lisp
                             #:pixbuf-new
                             #:pixbuf-new-subpixbuf
                             #:pixbuf-copy
                             ;; Symbols from gdk-pixbuf.scaling.lisp
                             #:pixbuf-interp-type
                             #:pixbuf-rotation
                             #:pixbuf-scale-simple
                             #:pixbuf-scale
                             #:pixbuf-composite-color-simple
                             #:pixbuf-composite
                             #:pixbuf-composite-color
                             #:pixbuf-rotate-simple
                             #:pixbuf-flip
                             ;; Symbols from gdk-pixbuf.utilities.lisp
                             #:pixbuf-add-alpha
                             #:pixbuf-copy-area
                             #:pixbuf-fill
                             ;; Symbols from gdk-pixbuf.animation.lisp
                             #:pixbuf-animation-iter
                             #:pixbuf-animation-iter-pixbuf
                             #:pixbuf-animation-iter-advance
                             #:pixbuf-animation-iter-delay-time
                             #:pixbuf-animation-iter-on-currently-loading-frame

                             #:pixbuf-animation
                             #:pixbuf-animation-new-from-file
                             #:pixbuf-animation-new-from-resource
                             #:pixbuf-animation-width
                             #:pixbuf-animation-height
                             #:pixbuf-animation-iter
                             #:pixbuf-animation-is-static-image
                             #:pixbuf-animation-static-image

                             #:pixbuf-simple-anim
                             #:pixbuf-simple-anim-loop
                             #:pixbuf-simple-anim-new
                             #:pixbuf-simple-anim-add-frame)
  ;; Export the symbols for GDK-PIXBUF
  (:export                   ;; Symbols from gdk-pixbuf.structure.lisp
                             #:colorspace
                             #:pixbuf
                             #:pixbuf-bits-per-sample
                             #:pixbuf-colorspace
                             #:pixbuf-has-alpha
                             #:pixbuf-height
                             #:pixbuf-n-channels
                             #:pixbuf-pixel-bytes
                             #:pixbuf-pixels
                             #:pixbuf-rowstride
                             #:pixbuf-width
                             #:pixbuf-pixels-with-length
                             #:pixbuf-byte-length
                             #:pixbuf-option
                             #:pixbuf-remove-option
                             #:pixbuf-copy-options
                             #:pixbuf-read-pixels
                             ;; Symbols from gdk-pixbuf.load.lisp
                             #:pixbuf-file-info
                             #:pixbuf-new-from-file
                             #:pixbuf-new-from-file-at-size
                             #:pixbuf-new-from-file-at-scale
                             #:pixbuf-new-from-resource
                             #:pixbuf-new-from-resource-at-scale
                             ;; Symbols from gdk-pixbuf.loader.lisp
                             #:pixbuf-loader
                             #:pixbuf-loader-new
                             #:pixbuf-loader-write
                             #:pixbuf-loader-set-size
                             #:pixbuf-loader-pixbuf
                             #:pixbuf-loader-animation
                             #:pixbuf-loader-close
                             ;; Symbols from gdk-pixbuf.save.lisp
                             #:pixbuf-save
                             ;; Symbols from gdk-pixbuf.memory.lisp
                             #:pixbuf-new
                             #:pixbuf-new-subpixbuf
                             #:pixbuf-copy
                             ;; Symbols from gdk-pixbuf.scaling.lisp
                             #:pixbuf-interp-type
                             #:pixbuf-rotation
                             #:pixbuf-scale-simple
                             #:pixbuf-scale
                             #:pixbuf-composite-color-simple
                             #:pixbuf-composite
                             #:pixbuf-composite-color
                             #:pixbuf-rotate-simple
                             #:pixbuf-flip
                             ;; Symbols from gdk-pixbuf.utilities.lisp
                             #:pixbuf-add-alpha
                             #:pixbuf-copy-area
                             #:pixbuf-fill
                             ;; Symbols from gdk-pixbuf.animation.lisp
                             #:pixbuf-animation-iter
                             #:pixbuf-animation-iter-pixbuf
                             #:pixbuf-animation-iter-advance
                             #:pixbuf-animation-iter-delay-time
                             #:pixbuf-animation-iter-on-currently-loading-frame
                             #:pixbuf-animation
                             #:pixbuf-animation-new-from-file
                             #:pixbuf-animation-new-from-resource
                             #:pixbuf-animation-width
                             #:pixbuf-animation-height
                             #:pixbuf-animation-iter
                             #:pixbuf-animation-is-static-image
                             #:pixbuf-animation-static-image
                             #:pixbuf-simple-anim
                             #:pixbuf-simple-anim-loop
                             #:pixbuf-simple-anim-new
                             #:pixbuf-simple-anim-add-frame))

(in-package :gdk)

#+sbcl
(when (and (find-package "SB-INT")
           (find-symbol "SET-FLOATING-POINT-MODES" (find-package "SB-INT")))
  (funcall (find-symbol "SET-FLOATING-POINT-MODES" (find-package "SB-INT"))
           :traps nil))

;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (documentation (find-package :gdk) t)
 "GDK is an intermediate layer which isolates GTK from the details of the
  windowing system. This is the API documentation of a Lisp binding to GDK.
  @begin[General]{section}
    @begin[Enumerations]{subsection}
      @about-symbol{gdk:gravity}
      @about-variable{gdk:+modifier-mask+}
      @about-symbol{gdk:modifier-type}
    @end{subsection}
    @begin[GdkRectangle]{subsection}
      @about-struct{rectangle}
      @about-function{rectangle-x}
      @about-function{rectangle-y}
      @about-function{rectangle-width}
      @about-function{rectangle-height}
      @about-function{rectangle-new}
      @about-function{rectangle-copy}
      @about-function{rectangle-contains-point}
      @about-function{rectangle-equal}
      @about-function{rectangle-intersect}
      @about-function{rectangle-union}
    @end{subsection}
    @begin[GdkRGBA]{subsection}
      @about-struct{rgba}
      @about-function{rgba-red}
      @about-function{rgba-green}
      @about-function{rgba-blue}
      @about-function{rgba-alpha}
      @about-function{rgba-new}
      @about-function{rgba-copy}
      @about-function{rgba-is-clear}
      @about-function{rgba-is-opaque}
      @about-function{rgba-parse}
      @about-function{rgba-hash}
      @about-function{rgba-equal}
      @about-function{rgba-to-string}
    @end{subsection}
    @begin[Key Values]{subsection}
      @about-function{keyval-name}
      @about-function{keyval-from-name}
      @about-function{keyval-convert-case}
      @about-function{keyval-to-upper}
      @about-function{keyval-to-lower}
      @about-function{keyval-is-upper}
      @about-function{keyval-is-lower}
      @about-function{keyval-to-unicode}
      @about-function{unicode-to-keyval}
    @end{subsection}
    @begin[GdkDmabufFormats]{subsection}
      @about-class{dmabuf-formats}
      @about-function{dmabuf-formats-contains}
      @about-function{dmabuf-formats-equal}
      @about-function{dmabuf-formats-format}
      @about-function{dmabuf-formats-n-formats}
    @end{subsection}
    @begin[GdkColorState]{subsection}
      @about-class{color-state}
      @about-function{color-state-oklab}
      @about-function{color-state-oklch}
      @about-function{color-state-rec2100-linear}
      @about-function{color-state-rec2100-pq}
      @about-function{color-state-srgb}
      @about-function{color-state-srgb-linear}
      @about-function{color-state-create-cicp-params}
      @about-function{color-state-equal}
    @end{subsection}
    @begin[GdkCicpParams]{subsection}
      @about-symbol{cicp-range}
      @about-class{cicp-params}
      @about-generic{cicp-params-color-primaries}
      @about-generic{cicp-params-matrix-coefficients}
      @about-generic{cicp-params-range}
      @about-generic{cicp-params-transfer-function}
      @about-function{cicp-params-new}
      @about-function{cicp-params-build-color-state}
    @end{subsection}
  @end{section}
  @begin[Displays, Devices, Monitors, and Seats]{section}
    @begin[GdkDisplayManager]{subsection}
      @about-class{display-manager}
      @about-generic{display-manager-default-display}
      @about-function{display-manager-get}
      @about-function{display-manager-list-displays}
      @about-function{display-manager-open-display}
      @about-function{set-allowed-backends}
    @end{subsection}
    @begin[GdkDisplay]{subsection}
      @about-class{display}
      @about-generic{display-composited}
      @about-generic{display-dmabuf-formats}
      @about-generic{display-input-shapes}
      @about-generic{display-rgba}
      @about-generic{display-shadow-width}
      @about-function{display-open}
      @about-function{display-default}
      @about-function{display-name}
      @about-function{display-device-is-grabbed}
      @about-function{display-beep}
      @about-function{display-sync}
      @about-function{display-flush}
      @about-function{display-close}
      @about-function{display-is-closed}
      @about-function{display-is-rgba}
      @about-function{display-is-composited}
      @about-function{display-supports-input-shapes}
      @about-function{display-app-launch-context}
      @about-function{display-notify-startup-complete}
      @about-function{display-default-seat}
      @about-function{display-list-seats}
      @about-function{display-monitors}
      @about-function{display-monitor-at-surface}
      @about-function{display-clipboard}
      @about-function{display-primary-clipboard}
      @about-function{display-setting}
      @about-function{display-startup-notification-id}
      @about-function{display-put-event}
      @about-function{display-map-keyval}
      @about-function{display-map-keycode}
      @about-function{display-translate-key}
      @about-function{display-prepare-gl}
      @about-function{display-create-gl-context}
    @end{subsection}
    @begin[GdkDeviceTool]{subsection}
      @about-symbol{device-tool-type}
      @about-class{device-tool}
      @about-generic{device-tool-axes}
      @about-generic{device-tool-hardware-id}
      @about-generic{device-tool-serial}
      @about-generic{device-tool-tool-type}
    @end{subsection}
    @begin[GdkDevice]{subsection}
      @about-symbol{input-source}
      @about-symbol{axis-use}
      @about-symbol{axis-flags}
      @about-class{device}
      @ablut-generic{device-active-layout-index}
      @about-generic{device-caps-lock-state}
      @about-generic{device-direction}
      @about-generic{device-display}
      @about-generic{device-has-bidi-layouts}
      @about-generic{device-has-cursor}
      @about-generic{device-layout-names}
      @about-generic{device-modifier-state}
      @about-generic{device-n-axes}
      @about-generic{device-name}
      @about-generic{device-num-lock-state}
      @about-generic{device-num-touches}
      @about-generic{device-product-id}
      @about-generic{device-scroll-lock-state}
      @about-generic{device-seat}
      @about-generic{device-source}
      @about-generic{device-tool}
      @about-generic{device-vendor-id}
      @about-function{device-has-cursor}
      @about-function{device-device-tool}
      @about-function{device-has-bidi-layouts}
      @about-function{device-surface-at-position}
      @about-function{device-timestamp}
    @end{subsection}
    @begin[GdkDevicePad]{subsection}
      @about-symbol{device-pad-feature}
      @about-class{device-pad}
      @about-function{device-pad-n-groups}
      @about-function{device-pad-group-n-modes}
      @about-function{device-pad-n-features}
      @about-function{device-pad-feature-group}
    @end{subsection}
    @begin[GdkMonitor]{subsection}
      @about-symbol{subpixel-layout}
      @about-class{monitor}
      @about-generic{monitor-connector}
      @about-generic{monitor-display}
      @about-generic{monitor-geometry}
      @about-generic{monitor-height-mm}
      @about-generic{monitor-manufacturer}
      @about-generic{monitor-model}
      @about-generic{monitor-refresh-rate}
      @about-generic{monitor-scale}
      @about-generic{monitor-scale-factor}
      @about-generic{monitor-subpixel-layout}
      @about-generic{monitor-valid}
      @about-generic{monitor-width-mm}
      @about-function{monitor-is-valid}
    @end{subsection}
    @begin[GdkSeat]{subsection}
      @about-symbol{seat-capabilities}
      @about-class{seat}
      @about-generic{seat-display}
      @about-function{seat-capabilities}
      @about-function{seat-pointer}
      @about-function{seat-keyboard}
      @about-function{seat-devices}
      @about-function{seat-tools}
    @end{subsection}
  @end{section}
  @begin[Paintables]{section}
    @begin[GdkPaintable]{subsection}
      @about-symbol{paintable-flags}
      @about-class{snapshot}
      @about-class{paintable}
      @about-symbol{paintable-vtable}
      @about-generic{paintable-snapshot-impl}
      @about-generic{paintable-get-current-image-impl}
      @about-generic{paintable-get-flags-impl}
      @about-generic{paintable-get-intrinsic-width-impl}
      @about-generic{paintable-get-intrinsic-height-impl}
      @about-generic{paintable-get-intrinsic-aspect-ratio-impl}
      @about-function{paintable-current-image}
      @about-function{paintable-snapshot}
      @about-function{paintable-flags}
      @about-function{paintable-intrinsic-width}
      @about-function{paintable-intrinsic-height}
      @about-function{paintable-intrinsic-aspect-ratio}
      @about-function{paintable-compute-concrete-size}
      @about-function{paintable-invalidate-contents}
      @about-function{paintable-invalidate-size}
      @about-function{paintable-new-empty}
    @end{subsection}
    @begin[GdkTexture]{subsection}
      @about-variable{+memory-default+}
      @about-symbol{memory-format}
      @about-class{texture}
      @about-generic{texture-color-state}
      @about-generic{texture-height}
      @about-generic{texture-width}
      @about-function{texture-new-for-pixbuf}
      @about-function{texture-new-from-resource}
      @about-function{texture-new-from-file}
      @about-function{texture-new-from-filename}
      @about-function{texture-new-from-bytes}
      @about-function{texture-download}
      @about-function{texture-save-to-png}
      @about-function{texture-save-to-png-bytes}
      @about-function{texture-save-to-tiff}
      @about-function{texture-save-to-tiff-bytes}
      @about-function{texture-format}
      @about-class{memory-texture}
      @about-function{memory-texture-new}
      @about-class{gl-texture}
      @about-function{gl-texture-new}
      @about-function{gl-texture-release}
    @end{subsection}
    @begin[GdkTextureDownloader]{subsection}
      @about-class{texture-downloader}
      @about-function{texture-downloader-new}
      @about-function{texture-downloader-copy}
      @about-function{texture-downloader-download-bytes}
      @about-function{texture-downloader-download-into}
      @about-function{texture-downloader-color-state}
      @about-function{texture-downloader-format}
      @about-function{texture-downloader-texture}
    @end{subsection}
    @begin[GdkDmabufTexture]{subsection}
      @about-class{dmabuf-texture}
      @about-class{dmabuf-texture-builder}
      @about-generic{dmabuf-texture-builder-display}
      @about-generic{dmabuf-texture-builder-fourcc}
      @about-generic{dmabuf-texture-builder-height}
      @about-generic{dmabuf-texture-builder-modifier}
      @about-generic{dmabuf-texture-builder-n-planes}
      @about-generic{dmabuf-texture-builder-premultiplied}
      @about-generic{dmabuf-texture-builder-update-region}
      @about-generic{dmabuf-texture-builder-update-texture}
      @about-generic{dmabuf-texture-builder-width}
      @about-function{dmabuf-texture-builder-new}
      @about-function{dmabuf-texture-builder-build}
      @about-function{dmabuf-texture-builder-fd}
      @about-function{dmabuf-texture-builder-offset}
      @about-function{dmabuf-texture-builder-stride}
    @end{subsection}
  @end{section}
  @begin[Surfaces, Toplevels, Popups]{section}
    @begin[Surfaces]{subsection}
      @about-class{surface}
      @about-generic{surface-cursor}
      @about-generic{surface-display}
      @about-generic{surface-frame-clock}
      @about-generic{surface-height}
      @about-generic{surface-mapped}
      @about-generic{surface-scale}
      @about-generic{surface-scale-factor}
      @about-generic{surface-width}
      @about-function{surface-new-toplevel}
      @about-function{surface-new-popup}
      @about-function{surface-destroy}
      @about-function{surface-is-destroyed}
      @about-function{surface-hide}
      @about-function{surface-translate-coordinates}
      @about-function{surface-beep}
      @about-function{surface-scale}
      @about-function{surface-set-opaque-region}
      @about-function{surface-create-gl-context}
      @about-function{surface-create-cairo-context}
      @about-function{surface-create-similar-surface}
      @about-function{surface-queue-render}
      @about-function{surface-request-layout}
      @about-function{surface-set-input-region}
      @about-function{surface-device-position}
      @about-function{surface-device-cursor}
    @end{subsection}
    @begin[GdkToplevelSize]{subsection}
      @about-symbol{toplevel-size}
      @about-function{toplevel-size-bounds}
      @about-function{toplevel-size-set-size}
      @about-function{toplevel-size-set-min-size}
      @about-function{toplevel-size-set-shadow-width}
    @end{subsection}
    @begin[GdkToplevelLayout]{subsection}
      @about-class{toplevel-layout}
      @about-function{toplevel-layout-new}
      @about-function{toplevel-layout-ref}
      @about-function{toplevel-layout-unref}
      @about-function{toplevel-layout-copy}
      @about-function{toplevel-layout-equal}
      @about-function{toplevel-layout-maximized}
      @about-function{toplevel-layout-fullscreen}
      @about-function{toplevel-layout-fullscreen-monitor}
      @about-function{toplevel-layout-resizable}
    @end{subsection}
    @begin[GdkToplevel]{subsection}
      @about-symbol{toplevel-state}
      @about-symbol{fullscreen-mode}
      @about-symbol{surface-edge}
      @about-symbol{titlebar-gesture}
      @about-class{toplevel}
      @about-generic{toplevel-decorated}
      @about-generic{toplevel-deletable}
      @about-generic{toplevel-fullscreen-mode}
      @about-generic{toplevel-icon-list}
      @about-generic{toplevel-modal}
      @about-generic{toplevel-shortcuts-inhibited}
      @about-generic{toplevel-startup-id}
      @about-generic{toplevel-state}
      @about-generic{toplevel-title}
      @about-generic{toplevel-transient-for}
      @about-function{toplevel-present}
      @about-function{toplevel-minimize}
      @about-function{toplevel-lower}
      @about-function{toplevel-focus}
      @about-function{toplevel-show-window-menu}
      @about-function{toplevel-supports-edge-constraints}
      @about-function{toplevel-inhibit-system-shortcuts}
      @about-function{toplevel-restore-system-shortcuts}
      @about-function{toplevel-begin-resize}
      @about-function{toplevel-begin-move}
      @about-function{toplevel-titlebar-gesture}
    @end{subsection}
    @begin[GdkPopupLayout]{subsection}
      @about-symbol{anchor-hints}
      @about-class{popup-layout}
      @about-function{popup-layout-new}
      @about-function{popup-layout-ref}
      @about-function{popup-layout-unref}
      @about-function{popup-layout-copy}
      @about-function{popup-layout-equal}
      @about-function{popup-layout-anchor-rect}
      @about-function{popup-layout-rect-anchor}
      @about-function{popup-layout-surface-anchor}
      @about-function{popup-layout-anchor-hints}
      @about-function{popup-layout-offset}
      @about-function{popup-layout-shadow-width}
      @end{subsection}
    @begin[GdkPopup]{subsection}
      @about-class{popup}
      @about-generic{popup-autohide}
      @about-generic{popup-parent}
      @about-function{popup-present}
      @about-function{popup-surface-anchor}
      @about-function{popup-rect-anchor}
      @about-function{popup-position-x}
      @about-function{popup-position-y}
    @end{subsection}
  @end{section}
  @begin[Draw contexts]{section}
    @begin[GdkDrawContext]{subsection}
      @about-class{draw-context}
      @about-generic{draw-context-display}
      @about-generic{draw-context-surface}
      @about-function{draw-context-begin-frame}
      @about-function{draw-context-end-frame}
      @about-function{draw-context-is-in-frame}
      @about-function{draw-context-frame-region}
    @end{subsection}
    @begin[GdkGLContext]{subsection}
      @about-symbol{gl-api}
      @about-symbol{gl-error}
      @about-class{gl-context}
      @about-generic{gl-context-allowed-apis}
      @about-generic{gl-context-api}
      @about-generic{gl-context-shared-context}
      @about-function{gl-context-display}
      @about-function{gl-context-surface}
      @about-function{gl-context-version}
      @about-function{gl-context-required-version}
      @about-function{gl-context-debug-enabled}
      @about-function{gl-context-forward-compatible}
      @about-function{gl-context-use-es}
      @about-function{gl-context-is-legacy}
      @about-function{gl-context-realize}
      @about-function{gl-context-make-current}
      @about-function{gl-context-current}
      @about-function{gl-context-clear-current}
      @about-function{gl-context-is-shared}
    @end{subsection}
    @begin[GdkCairoContext]{subsection}
      @about-class{cairo-context}
      @about-function{cairo-context-cairo-create}
    @end{subsection}
  @end{section}
  @begin[Clipboard, Drag and Drop]{section}
    @begin[Content Formats]{subsection}
      @about-class{content-formats}
      @about-function{intern-mime-type}
      @about-function{content-formats-new}
      @about-function{content-formats-new-for-gtype}
      @about-function{content-formats-ref}
      @about-function{content-formats-unref}
      @about-function{content-formats-print}
      @about-function{content-formats-to-string}
      @about-function{content-formats-parse}
      @about-function{content-formats-gtypes}
      @about-function{content-formats-mime-types}
      @about-function{content-formats-union}
      @about-function{content-formats-match}
      @about-function{content-formats-match-gtype}
      @about-function{content-formats-match-mime-type}
      @about-function{content-formats-contain-gtype}
      @about-function{content-formats-contain-mime-type}
      @about-function{content-formats-union-serialize-gtypes}
      @about-function{content-formats-union-deserialize-gtypes}
      @about-function{content-formats-union-serialize-mime-types}
      @about-function{content-formats-union-deserialize-mime-types}
      @about-symbol{content-formats-builder}
      @about-function{content-formats-builder-new}
      @about-function{content-formats-builder-free-to-formats}
      @about-function{content-formats-builder-add-formats}
      @about-function{content-formats-builder-add-gtype}
      @about-function{content-formats-builder-add-mime-type}
      @about-function{content-formats-builder-ref}
      @about-function{content-formats-builder-unref}
      @about-function{content-formats-builder-to-formats}
    @end{subsection}
    @begin[GdkContentProvider]{subsection}
      @about-class{content-provider}
      @about-generic{content-provider-formats}
      @about-generic{content-provider-storable-formats}
      @about-function{content-provider-new-for-value}
      @about-function{content-provider-new-typed}
      @about-function{content-provider-new-for-bytes}
      @about-function{content-provider-new-union}
      @about-function{content-provider-ref-formats}
      @about-function{content-provider-ref-storable-formats}
      @about-function{content-provider-content-changed}
      @about-function{content-provider-write-mime-type-async}
      @about-function{content-provider-write-mime-type-finish}
      @about-function{content-provider-value}
    @end{subsection}
    @begin[GdkContentSerializer]{subsection}
      @about-class{content-serializer}
      @about-symbol{content-serialize-func}
      @about-function{content-serializer-mime-type}
      @about-function{content-serializer-gtype}
      @about-function{content-serializer-value}
      @about-function{content-serializer-output-stream}
      @about-function{content-serializer-priority}
      @about-function{content-serializer-cancellable}
      @about-function{content-serializer-user-data}
      @about-function{content-serializer-task-data}
      @about-function{content-serializer-return-success}
      @about-function{content-serializer-return-error}
      @about-function{content-register-serializer}
      @about-function{content-serialize-async}
      @about-function{content-serialize-finish}
    @end{subsection}
    @begin[GdkContentDeserializer]{subsection}
      @about-class{content-deserializer}
      @about-symbol{content-deserialize-func}
      @about-function{content-deserializer-mime-type}
      @about-function{content-deserializer-gtype}
      @about-function{content-deserializer-value}
      @about-function{content-deserializer-input-stream}
      @about-function{content-deserializer-priority}
      @about-function{content-deserializer-cancellable}
      @about-function{content-deserializer-user-data}
      @about-function{content-deserializer-task-data}
      @about-function{content-deserializer-return-success}
      @about-function{content-deserializer-return-error}
      @about-function{content-register-deserializer}
      @about-function{content-deserialize-async}
      @about-function{content-deserialize-finish}
    @end{subsection}
    @begin[Clipboards]{subsection}
      @about-class{clipboard}
      @about-generic{clipboard-content}
      @about-generic{clipboard-display}
      @about-generic{clipboard-formats}
      @about-generic{clipboard-local}
      @about-function{clipboard-is-local}
      @about-function{clipboard-store-async}
      @about-function{clipboard-store-finish}
      @about-function{clipboard-read-async}
      @about-function{clipboard-read-finish}
      @about-function{clipboard-read-value-async}
      @about-function{clipboard-read-value-finish}
      @about-function{clipboard-read-texture-async}
      @about-function{clipboard-read-texture-finish}
      @about-function{clipboard-read-text-async}
      @about-function{clipboard-read-text-finish}
      @about-function{clipboard-set-content}
      @about-function{clipboard-set}
      @about-function{clipboard-set-valist}
      @about-function{clipboard-set-value}
      @about-function{clipboard-set-text}
      @about-function{clipboard-set-texture}
    @end{subsection}
    @begin[GdkDrag]{subsection}
      @about-symbol{drag-cancel-reason}
      @about-symbol{drag-action}
      @about-class{drag}
      @about-generic{drag-actions}
      @about-generic{drag-content}
      @about-generic{drag-device}
      @about-generic{drag-display}
      @about-generic{drag-formats}
      @about-generic{drag-selected-action}
      @about-generic{drag-surface}
      @about-function{drag-drop-done}
      @about-function{drag-begin}
      @about-function{drag-drag-surface}
      @about-function{drag-set-hotspot}
      @about-function{drag-action-is-unique}
    @end{subsection}
    @begin[GdkDrop]{subsection}
      @about-class{drop}
      @about-generic{drop-actions}
      @about-generic{drop-device}
      @about-generic{drop-drag}
      @about-generic{drop-formats}
      @about-generic{drop-surface}
      @about-function{drop-status}
      @about-function{drop-finish}
      @about-function{drop-read-async}
      @about-function{drop-read-finish}
      @about-function{drop-read-value-async}
      @about-function{drop-read-value-finish}
    @end{subsection}
    @begin[GdkDragSurface]{subsection}
      @about-symbol{drag-surface-size}
      @about-function{drag-surface-size-set-size}
      @about-class{drag-surface}
      @about-function{drag-surface-present}
    @end{subsection}
  @end{section}
  @begin[Application launching]{section}
    @begin[GdkAppLaunchContext]{subsection}
      @about-class{app-launch-context}
      @about-generic{app-launch-context-display}
      @about-function{app-launch-context-set-desktop}
      @about-function{app-launch-context-set-timestamp}
      @about-function{app-launch-context-set-icon}
      @about-function{app-launch-context-set-icon-name}
    @end{subsection}
  @end{section}
  @begin[GDK Miscellaneous]{section}
    @begin[Events]{subsection}
      @about-variable{+current-time+}
      @about-variable{+priority-events+}
      @about-variable{+priority-redraw+}
      @about-variable{+event-propagate+}
      @about-variable{+event-stop+}
      @about-variable{+button-primary+}
      @about-variable{+button-middle+}
      @about-variable{+button-secondary+}
      @about-symbol{event-type}
      @about-symbol{keymap-key}
      @about-symbol{key-match}
      @about-symbol{touchpad-gesture-phase}
      @about-symbol{scroll-direction}
      @about-symbol{crossing-mode}
      @about-symbol{scroll-unit}
      @about-symbol{notify-type}
      @about-class{event-sequence}
      @about-class{event}
      @about-class{button-event}
      @about-class{scroll-event}
      @about-class{motion-event}
      @about-class{key-event}
      @about-class{focus-event}
      @about-class{crossing-event}
      @about-class{grab-broken-event}
      @about-class{delete-event}
      @about-class{dnd-event}
      @about-class{touch-event}
      @about-class{touchpad-event}
      @about-class{pad-event}
      @about-class{proximity-event}
      @about-function{event-ref}
      @about-function{event-unref}
      @about-function{event-event-type}
      @about-function{event-surface}
      @about-function{event-device}
      @about-function{event-device-tool}
      @about-function{event-time}
      @about-function{event-display}
      @about-function{event-seat}
      @about-function{event-event-sequence}
      @about-function{event-modifier-state}
      @about-function{event-position}
      @about-function{event-axes}
      @about-function{event-axis}
      @about-function{event-history}
      @about-function{event-pointer-emulated}
      @about-function{event-triggers-context-menu}
      @about-function{button-event-button}
      @about-function{scroll-event-direction}
      @about-function{scroll-event-deltas}
      @about-function{scroll-event-is-stop}
      @about-function{scroll-event-unit}
      @about-function{key-event-keyval}
      @about-function{key-event-keycode}
      @about-function{key-event-consumed-modifiers}
      @about-function{key-event-layout}
      @about-function{key-event-level}
      @about-function{key-event-is-modifier}
      @about-function{key-event-matches}
      @about-function{key-event-match}
      @about-function{focus-event-in}
      @about-function{touch-event-emulating-pointer}
      @about-function{crossing-event-mode}
      @about-function{crossing-event-detail}
      @about-function{crossing-event-focus}
      @about-function{grab-broken-event-grab-surface}
      @about-function{grab-broken-event-implicit}
      @about-function{dnd-event-drop}
      @about-function{touchpad-event-gesture-phase}
      @about-function{touchpad-event-n-fingers}
      @about-function{touchpad-event-deltas}
      @about-function{touchpad-event-pinch-angle-delta}
      @about-function{touchpad-event-pinch-scale}
      @about-function{pad-event-axis-value}
      @about-function{pad-event-button}
      @about-function{pad-event-group-mode}
      @about-function{events-angle}
      @about-function{events-center}
      @about-function{events-distance}
    @end{subsection}
    @begin[Cursors]{subsection}
      @about-class{cursor}
      @about-generic{cursor-fallback}
      @about-generic{cursor-hotspot-x}
      @about-generic{cursor-hotspot-y}
      @about-generic{cursor-name}
      @about-generic{cursor-texture}
      @about-function{cursor-new-from-texture}
      @about-function{cursor-new-from-name}
      @about-function{cursor-new-from-callback}
      @about-symbol{cursor-get-texture-callback}
    @end{subsection}
    @begin[GdkFrameTimings]{subsection}
      @about-class{frame-timings}
      @about-function{frame-timings-ref}
      @about-function{frame-timings-unref}
      @about-function{frame-timings-frame-counter}
      @about-function{frame-timings-complete}
      @about-function{frame-timings-frame-time}
      @about-function{frame-timings-presentation-time}
      @about-function{frame-timings-refresh-interval}
      @about-function{frame-timings-predicted-presentation-time}
    @end{subsection}
    @begin[GdkFrameClock]{subsection}
      @about-symbol{frame-clock-phase}
      @about-class{frame-clock}
      @about-function{frame-clock-frame-time}
      @about-function{frame-clock-request-phase}
      @about-function{frame-clock-begin-updating}
      @about-function{frame-clock-end-updating}
      @about-function{frame-clock-frame-counter}
      @about-function{frame-clock-history-start}
      @about-function{frame-clock-timings}
      @about-function{frame-clock-current-timings}
      @about-function{frame-clock-refresh-info}
      @about-function{frame-clock-fps}
    @end{subsection}
  @end{section}
  @begin[Pixbuf, Pango, and Backends interaction]{section}
    @begin[Introduction to GdkPixbuf Interaction]{subsection}
      Pixbufs are client-side images. For details on how to create and
      manipulate pixbufs, see the GdkPixbuf API documentation. The functions
      described here allow to obtain pixbufs from GdkSurfaces and Cairo
      surfaces.
    @end{subsection}
    @begin[Functions for GDKPixbuf Interaction]{subsection}
      @about-function{pixbuf-from-surface}
      @about-function{pixbuf-from-texture}
    @end{subsection}
    @begin[Introduction to Pango Interaction]{subsection}
      Pango is the text layout system used by GDK and GTK. The functions and
      types in this section are used to obtain clip regions for Pango Layouts,
      and to get @class{pango:context} objects that can be used with GDK.

      Creating a @class{pango:layout} object is the first step in rendering
      text, and requires getting a handle to a @class{pango:context} object.
      For GTK programs, you will usually want to use the
      @fun{gtk:widget-pango-context} function, or the
      @fun{gtk:widget-create-pango-layout} function. Once you have a
      @class{pango:layout} object, you can set the text and attributes of it
      with Pango functions like the @fun{pango:layout-text} function and get its
      size with the @fun{pango:layout-size} function. Note that Pango uses a
      fixed point system internally, so converting between Pango units and
      pixels using the @var{pango:+scale+} value or the @fun{pango:pixels}
      function.

      Rendering a Pango layout is done most simply with the
      @fun{pango:cairo-show-layout} function. You can also draw pieces of the
      layout with the @fun{pango:cairo-show-layout-line} function.
    @end{subsection}
    @begin[Functions for Pango Interaction]{subsection}
      @about-function{pango-layout-clip-region}
      @about-function{pango-layout-line-clip-region}
    @end{subsection}
    @begin[Introduction to Cairo Interaction]{subsection}
      Functions to support using Cairo. GDK does not wrap the Cairo API,
      instead it allows to create Cairo contexts which can be used to draw on
      GdkSurfaces. Additional functions allow to use GdkRectangles with Cairo
      and to use GdkRGBAs, GdkPixbufs and GdkSurfaces as sources for drawing
      operations.
    @end{subsection}
    @begin[Functions for Cairo Interaction]{subsection}
      @about-function{cairo-set-source-rgba}
      @about-function{cairo-set-source-pixbuf}
      @about-function{cairo-rectangle}
      @about-function{cairo-region}
      @about-function{cairo-region-create-from-surface}
      @about-function{cairo-draw-from-gl}
    @end{subsection}
  @end{section}")

;;; --- End of file gdk4.package.lisp ------------------------------------------
