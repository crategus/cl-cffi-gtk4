;;; ----------------------------------------------------------------------------
;;; gtk4.settings.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.14 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2024 Dieter Kaiser
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
;;; Settings
;;;
;;;     Sharing settings between applications
;;;
;;; Types and Values
;;;
;;;     GtkSettings
;;;     GtkSettingsValue                                    not implemented
;;;     GtkSystemSetting
;;;
;;; Functions
;;;
;;;     gtk_settings_get_default
;;;     gtk_settings_get_for_display
;;;     gtk_settings_reset_property
;;;
;;; Properties
;;;
;;;     gtk-alternative-button-order
;;;     gtk-alternative-sort-arrows
;;;     gtk-application-prefer-dark-theme
;;;     gtk-cursor-aspect-ratio
;;;     gtk-cursor-blink
;;;     gtk-cursor-blink-time
;;;     gtk-cursor-blink-timeout
;;;     gtk-cursor-theme-name
;;;     gtk-cursor-theme-size
;;;     gtk-decoration-layout
;;;     gtk-dialogs-use-header
;;;     gtk-dnd-drag-threshold
;;;     gtk-double-click-distance
;;;     gtk-double-click-time
;;;     gtk-enable-accels
;;;     gtk-enable-animations
;;;     gtk-enable-event-sounds
;;;     gtk-enable-input-feedback-sounds
;;;     gtk-enable-primary-paste
;;;     gtk-entry-password-hint-timeout
;;;     gtk-entry-select-on-focus
;;;     gtk-error-bell
;;;     gtk-font-name
;;;     gtk-fontconfig-timestamp
;;;     gtk-hint-font-metrics                               Since 4.6
;;;     gtk-icon-theme-name
;;;     gtk-im-module
;;;     gtk-keynav-use-caret
;;;     gtk-label-select-on-focus
;;;     gtk-long-press-time
;;;     gtk-overlay-scrolling
;;;     gtk-primary-button-warps-slider
;;;     gtk-print-backends
;;;     gtk-print-preview-command
;;;     gtk-recent-files-enabled
;;;     gtk-recent-files-max-age
;;;     gtk-shell-shows-app-menu
;;;     gtk-shell-shows-desktop
;;;     gtk-shell-shows-menubar
;;;     gtk-show-status-shapes                              Since 4.14
;;;     gtk-sound-theme-name
;;;     gtk-split-cursor
;;;     gtk-theme-name
;;;     gtk-titlebar-double-click
;;;     gtk-titlebar-middle-click
;;;     gtk-titlebar-right-click
;;;     gtk-xft-antialias
;;;     gtk-xft-dpi
;;;     gtk-xft-hinting
;;;     gtk-xft-hintstyle
;;;     gtk-xft-rgba
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkSettings
;;;
;;; Implemented Interfaces
;;;
;;;     GtkStyleProvider
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkSettingsValue
;;;
;;; struct GtkSettingsValue {
;;;   /* origin should be something like "filename:linenumber" for rc files,
;;;    * or e.g. "XProperty" for other sources
;;;    */
;;;   char *origin;
;;;
;;;   /* valid types are LONG, DOUBLE and STRING corresponding to the token
;;;    * parsed, or a GSTRING holding an unparsed statement
;;;    */
;;;   GValue value;
;;; };
;;;
;;; char *origin :
;;;     Origin should be something like “filename:linenumber” for rc files, or
;;;     e.g. “XProperty” for other sources.
;;;
;;; GValue value :
;;;     Valid types are LONG, DOUBLE and STRING corresponding to the token
;;;     parsed, or a GSTRING holding an unparsed statement
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkSystemSetting
;;; ----------------------------------------------------------------------------

;; TODO: Consider to remove the implementation.

(gobject:define-g-enum "GtkSystemSetting" system-setting
  (:export t
   :type-initializer "gtk_system_setting_get_type")
  :dpi
  :font-name
  :font-config
  :display
  :icon-theme)

#+liber-documentation
(setf (liber:alias-for-symbol 'system-setting)
      "GEnum"
      (liber:symbol-documentation 'system-setting)
 "@version{2024-3-8}
  @begin{declaration}
(gobject:define-g-enum \"GtkSystemSetting\" system-setting
  (:export t
   :type-initializer \"gtk_system_setting_get_type\")
  :dpi
  :font-name
  :font-config
  :display
  :icon-theme)
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:dpi]{The @slot[gtk:settings]{gtk-xft-dpi} setting has changed.}
      @entry[:font-name]{The @slot[gtk:settings]{gtk-font-name} setting has
        changed.}
      @entry[:font-config]{The font configuration has changed in a way that
        requires text to be redrawn. This can be any of the
        @slot[gtk:settings]{gtk-xft-antialias},
        @slot[gtk:settings]{gtk-xft-hinting},
        @slot[gtk:settings]{gtk-xft-hintstyle}
        @slot[gtk:settings]{gtk-xft-rgba} or
        @slot[gtk:settings]{gtk-fontconfig-timestamp} settings.}
      @entry[:display]{The display has changed.}
      @entry[:icon-theme]{The icon theme has changed in a way that requires
        icons to be looked up again.}
    @end{table}
  @end{values}
  @begin{short}
    Values that can be passed to the @code{GtkWidget::system_setting_changed}
    virtual function.
  @end{short}
  The values indicate which system setting has changed. Widgets may need to drop
  caches, or react otherwise. Most of the values correspond to
  @class{gtk:settings} properties.
  @see-class{gtk:settings}
  @see-function{gtk:settings-gtk-xft-dpi}
  @see-function{gtk:settings-gtk-font-name}
  @see-function{gtk:settings-gtk-xft-antialias}
  @see-function{gtk:settings-gtk-xft-hinting}
  @see-function{gtk:settings-gtk-xft-hintstyle}
  @see-function{gtk:settings-gtk-xft-rgba}
  @see-function{gtk:settings-gtk-fontconfig-timestamp}")

;;; ----------------------------------------------------------------------------
;;; GtkSettings
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkSettings" settings
  (:superclass g:object
   :export t
   :interfaces ("GtkStyleProvider")
   :type-initializer "gtk_settings_get_type")
  ((gtk-alternative-button-order
    settings-gtk-alternative-button-order
    "gtk-alternative-button-order" "gboolean" t t)
   (gtk-alternative-sort-arrows
    settings-gtk-alternative-sort-arrows
    "gtk-alternative-sort-arrows" "gboolean" t t)
   (gtk-application-prefer-dark-theme
    settings-gtk-application-prefer-dark-theme
    "gtk-application-prefer-dark-theme" "gboolean" t t)
   (gtk-cursor-aspect-ratio
    settings-gtk-cursor-aspect-ratio
    "gtk-cursor-aspect-ratio" "gfloat" t t)
   (gtk-cursor-blink
    settings-gtk-cursor-blink
    "gtk-cursor-blink" "gboolean" t t)
   (gtk-cursor-blink-time
    settings-gtk-cursor-blink-time
    "gtk-cursor-blink-time" "gint" t t)
   (gtk-cursor-blink-timeout
    settings-gtk-cursor-blink-timeout
    "gtk-cursor-blink-timeout" "gint" t t)
   (gtk-cursor-theme-name
    settings-gtk-cursor-theme-name
    "gtk-cursor-theme-name" "gchararray" t t)
   (gtk-cursor-theme-size
    settings-gtk-cursor-theme-size
    "gtk-cursor-theme-size" "gint" t t)
   (gtk-decoration-layout
    settings-gtk-decoration-layout
    "gtk-decoration-layout" "gchararray" t t)
   (gtk-dialogs-use-header
    settings-gtk-dialogs-use-header
    "gtk-dialogs-use-header" "gboolean" t t)
   (gtk-dnd-drag-threshold
    settings-gtk-dnd-drag-threshold
    "gtk-dnd-drag-threshold" "gint" t t)
   (gtk-double-click-distance
    settings-gtk-double-click-distance
    "gtk-double-click-distance" "gint" t t)
   (gtk-double-click-time
    settings-gtk-double-click-time
    "gtk-double-click-time" "gint" t t)
   (gtk-enable-accels
    settings-gtk-enable-accels
    "gtk-enable-accels" "gboolean" t t)
   (gtk-enable-animations
    settings-gtk-enable-animations
    "gtk-enable-animations" "gboolean" t t)
   (gtk-enable-event-sounds
    settings-gtk-enable-event-sounds
    "gtk-enable-event-sounds" "gboolean" t t)
   (gtk-enable-input-feedback-sounds
    settings-gtk-enable-input-feedback-sounds
    "gtk-enable-input-feedback-sounds" "gboolean" t t)
   (gtk-enable-primary-paste
    settings-gtk-enable-primary-paste
    "gtk-enable-primary-paste" "gboolean" t t)
   (gtk-entry-password-hint-timeout
    settings-gtk-entry-password-hint-timeout
    "gtk-entry-password-hint-timeout" "guint" t t)
   (gtk-entry-select-on-focus
    settings-gtk-entry-select-on-focus
    "gtk-entry-select-on-focus" "gboolean" t t)
   (gtk-error-bell
    settings-gtk-error-bell
    "gtk-error-bell" "gboolean" t t)
   (gtk-font-name
    settings-gtk-font-name
    "gtk-font-name" "gchararray" t t)
   (gtk-fontconfig-timestamp
    settings-gtk-fontconfig-timestamp
    "gtk-fontconfig-timestamp" "guint" t t)
   #+gtk-4-6
   (gtk-hint-font-metrics
    settings-gtk-hint-font-metrics
    "gtk-hint-font-metrics" "gboolean" t t)
   (gtk-icon-theme-name
    settings-gtk-icon-theme-name
    "gtk-icon-theme-name" "gchararray" t t)
   (gtk-im-module
    settings-gtk-im-module "gtk-im-module"
    "gchararray" t t)
   (gtk-keynav-use-caret
    settings-gtk-keynav-use-caret
    "gtk-keynav-use-caret" "gboolean" t t)
   (gtk-label-select-on-focus
    settings-gtk-label-select-on-focus
    "gtk-label-select-on-focus" "gboolean" t t)
   (gtk-long-press-time
    settings-gtk-long-press-time
    "gtk-long-press-time" "guint" t t)
   (gtk-overlay-scrolling
    settings-gtk-overlay-scrolling
    "gtk-overlay-scrolling" "gboolean" t t)
   (gtk-primary-button-warps-slider
    settings-gtk-primary-button-warps-slider
    "gtk-primary-button-warps-slider" "gboolean" t t)
   (gtk-print-backends
    settings-gtk-print-backends
    "gtk-print-backends" "gchararray" t t)
   (gtk-print-preview-command
    settings-gtk-print-preview-command
    "gtk-print-preview-command" "gchararray" t t)
   (gtk-recent-files-enabled
    settings-gtk-recent-files-enabled
    "gtk-settings-gtk-recent-files-enabled" "gboolean" t t)
   (gtk-recent-files-max-age
    settings-gtk-recent-files-max-age
    "gtk-recent-files-max-age" "gint" t t)
   (gtk-shell-shows-app-menu
    settings-gtk-shell-shows-app-menu
    "gtk-shell-shows-app-menu" "gboolean" t t)
   (gtk-shell-shows-desktop
    settings-gtk-shell-shows-desktop
    "gtk-shell-shows-desktop" "gboolean" t t)
   (gtk-shell-shows-menubar
    settings-gtk-shell-shows-menubar
    "gtk-shell-shows-menubar" "gboolean" t t)
   #+gtk-4-14
   (gtk-show-status-shapes
    settings-gtk-show-status-shapes
    "gtk-show-status-shapes" "gboolean" t t)
   (gtk-sound-theme-name
    settings-gtk-sound-theme-name
    "gtk-sound-theme-name" "gchararray" t t)
   (gtk-split-cursor
    settings-gtk-split-cursor
    "gtk-split-cursor" "gboolean" t t)
   (gtk-theme-name
    settings-gtk-theme-name
    "gtk-theme-name" "gchararray" t t)
   (gtk-titlebar-double-click
    settings-gtk-titlebar-double-click
    "gtk-titlebar-double-click" "gchararray" t t)
   (gtk-titlebar-middle-click
    settings-gtk-titlebar-middle-click
    "gtk-titlebar-middle-click" "gchararray" t t)
   (gtk-titlebar-right-click
    settings-gtk-titlebar-right-click
    "gtk-titlebar-right-click" "gchararray" t t)
   (gtk-xft-antialias
    settings-gtk-xft-antialias
    "gtk-xft-antialias" "gint" t t)
   (gtk-xft-dpi
    settings-gtk-xft-dpi
    "gtk-xft-dpi" "gint" t t)
   (gtk-xft-hinting
    settings-gtk-xft-hinting
    "gtk-xft-hinting" "gint" t t)
   (gtk-xft-hintstyle
    settings-gtk-xft-hintstyle
    "gtk-xft-hintstyle" "gchararray" t t)
   (gtk-xft-rgba
    settings-gtk-xft-rgba
    "gtk-xft-rgba" "gchararray" t t)))

#+liber-documentation
(setf (documentation 'settings 'type)
 "@version{2024-5-25}
  @begin{short}
    The @class{gtk:settings} object provide a mechanism to share global settings
    between applications.
  @end{short}

  On the X window system, this sharing is realized by an XSettings manager
  that is usually part of the desktop environment, along with utilities that
  let the user change these settings. On Wayland, the settings are obtained
  either via a settings portal, or by reading desktop settings from DConf.

  In the absence of these sharing mechanisms, GTK reads default values for
  settings from @file{settings.ini} files in @file{/etc/gtk-4.0},
  @code{$XDG_CONFIG_DIRS/gtk-4.0} and @file{$XDG_CONFIG_HOME/gtk-4.0}. These
  files must be valid key files, see the @type{g:key-file} API, and have a
  section called \"Settings\". Themes can also provide default values for
  settings by installing a @file{settings.ini} file next to their @file{gtk.css}
  file.

  Applications can override system-wide settings with the accessor functions
  of the slots. This should be restricted to special cases though. The
  @class{gtk:settings} settings are not meant as an application configuration
  facility.

  There is one @class{gtk:settings} instance per display. It can be obtained
  with the @fun{gtk:settings-for-display} function, but in many cases, it is
  more convenient to use the @fun{gtk:widget-settings} function.
  @see-slot{gtk:settings-gtk-alternative-button-order}
  @see-slot{gtk:settings-gtk-alternative-sort-arrows}
  @see-slot{gtk:settings-gtk-application-prefer-dark-theme}
  @see-slot{gtk:settings-gtk-cursor-aspect-ratio}
  @see-slot{gtk:settings-gtk-cursor-blink}
  @see-slot{gtk:settings-gtk-cursor-blink-time}
  @see-slot{gtk:settings-gtk-cursor-blink-timeout}
  @see-slot{gtk:settings-gtk-cursor-theme-name}
  @see-slot{gtk:settings-gtk-cursor-theme-size}
  @see-slot{gtk:settings-gtk-decoration-layout}
  @see-slot{gtk:settings-gtk-dialogs-use-header}
  @see-slot{gtk:settings-gtk-dnd-drag-threshold}
  @see-slot{gtk:settings-gtk-double-click-distance}
  @see-slot{gtk:settings-gtk-double-click-time}
  @see-slot{gtk:settings-gtk-enable-accels}
  @see-slot{gtk:settings-gtk-enable-animations}
  @see-slot{gtk:settings-gtk-enable-event-sounds}
  @see-slot{gtk:settings-gtk-enable-input-feedback-sounds}
  @see-slot{gtk:settings-gtk-enable-primary-paste}
  @see-slot{gtk:settings-gtk-entry-password-hint-timeout}
  @see-slot{gtk:settings-gtk-entry-select-on-focus}
  @see-slot{gtk:settings-gtk-error-bell}
  @see-slot{gtk:settings-gtk-font-name}
  @see-slot{gtk:settings-gtk-fontconfig-timestamp}
  @see-slot{gtk:settings-gtk-icon-theme-name}
  @see-slot{gtk:settings-gtk-im-module}
  @see-slot{gtk:settings-gtk-keynav-use-caret}
  @see-slot{gtk:settings-gtk-label-select-on-focus}
  @see-slot{gtk:settings-gtk-long-press-time}
  @see-slot{gtk:settings-gtk-overlay-scrolling}
  @see-slot{gtk:settings-gtk-primary-button-warps-slider}
  @see-slot{gtk:settings-gtk-print-backends}
  @see-slot{gtk:settings-gtk-print-preview-command}
  @see-slot{gtk:settings-gtk-recent-files-enabled}
  @see-slot{gtk:settings-gtk-recent-files-max-age}
  @see-slot{gtk:settings-gtk-shell-shows-app-menu}
  @see-slot{gtk:settings-gtk-shell-shows-desktop}
  @see-slot{gtk:settings-gtk-shell-shows-menubar}
  @see-slot{gtk:settings-gtk-show-status-shapes}
  @see-slot{gtk:settings-gtk-sound-theme-name}
  @see-slot{gtk:settings-gtk-split-cursor}
  @see-slot{gtk:settings-gtk-theme-name}
  @see-slot{gtk:settings-gtk-titlebar-double-click}
  @see-slot{gtk:settings-gtk-titlebar-middle-click}
  @see-slot{gtk:settings-gtk-titlebar-right-click}
  @see-slot{gtk-xft-antialias}
  @see-slot{gtk:settings-gtk-xft-dpi}
  @see-slot{gtk:settings-gtk-xft-hinting}
  @see-slot{gtk:settings-gtk-xft-hintstyle}
  @see-slot{gtk:settings-gtk-xft-rgba}
  @see-class{g:key-file}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:settings-gtk-alternative-button-order ------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-alternative-button-order"
                                               'settings) t)
 "The @code{gtk-alternative-button-order} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether buttons in dialogs should use the alternative button order. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-alternative-button-order)
      "Accessor"
      (documentation 'settings-gtk-alternative-button-order 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk:alternative-button-order object) => setting}
  @syntax{(setf (gtk:settings-gtk-alternative-button-order object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether buttons in dialogs should use the
   alternative button order}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-alternative-button-order} slot of
    the @class{gtk:settings} class.
  @end{short}
  Whether buttons in dialogs should use the alternative button order.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-alternative-sort-arrows -------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-alternative-sort-arrows"
                                               'settings) t)
 "The @code{gtk-alternative-sort-arrows} property of type @code{:boolean}
  (Read / Write) @br{}
  Controls the direction of the sort indicators in sorted list and tree views.
  By default an arrow pointing down means the column is sorted in ascending
  order. When set to @em{true}, this order will be inverted. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-alternative-sort-arrows)
      "Accessor"
      (documentation 'settings-gtk-alternative-sort-arrows 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk-alternative-sort-arrows object) => setting}
  @syntax{(setf (gtk:settings-gtk-alternative-sort-arrows object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean which controls the direction of sort indicators}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-alternative-sort-arrows} slot of
    the @class{gtk:settings} class.
  @end{short}
  Controls the direction of the sort indicators in sorted list and tree views.
  By default an arrow pointing down means the column is sorted in ascending
  order. When set to @em{true}, this order will be inverted.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-application-prefer-dark-theme -------------------------

#+liber-documentation
(setf (documentation
        (liber:slot-documentation "gtk-application-prefer-dark-theme"
                                  'settings) t)
 "The @code{gtk-application-prefer-dark-theme} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the application prefers to use a dark theme. If a GTK theme includes
  a dark variant, it will be used instead of the configured theme. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-application-prefer-dark-theme)
      "Accessor"
      (documentation 'settings-gtk-application-prefer-dark-theme 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk-application-prefer-dark-theme object) => setting}
  @syntax{(setf (gtk:settings-gtk-application-prefer-dark-theme object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether the application prefers to use a dark
    theme}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-application-prefer-dark-theme} slot
    of the @class{gtk:settings} class.
  @end{short}
  Whether the application prefers to use a dark theme. If a GTK theme includes
  a dark variant, it will be used instead of the configured theme.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-cursor-aspect-ratio -----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-cursor-aspect-ratio"
                                               'settings) t)
 "The @code{gtk-cursor-aspect-ratio} property of type @code{:float}
  (Read / Write) @br{}
  The aspect ratio of the text caret. @br{}
  Allowed values: [0.0, 1.0] @br{}
  Default value: 0.04")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-cursor-aspect-ratio)
      "Accessor"
      (documentation 'settings-gtk-cursor-aspect-ratio 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk-cursor-aspect-ratio object) => setting}
  @syntax{(setf (gtk:settings-gtk-cursor-aspect-ratio object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a float with the aspect ratio of the text caret}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-cursor-aspect-ratio} slot of the
    @class{gtk:settings} class.
  @end{short}
  The aspect ratio of the text caret.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-cursor-blink ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-cursor-blink"
                                               'settings) t)
 "The @code{gtk-cursor-blink} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether the cursor should blink. Also see the @code{gtk-cursor-blink-timeout}
  setting, which allows more flexible control over cursor blinking. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-cursor-blink)
      "Accessor"
      (documentation 'settings-gtk-cursor-blink 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk-cursor-blink object) => setting}
  @syntax{(setf (gtk:settings-gtk-cursor-blink object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether the cursor should blink}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-cursor-blink} slot of the
    @class{gtk:settings} class.
  @end{short}
  Whether the cursor should blink. Also see the
  @slot[gtk:settings]{gtk-cursor-blink-timeout} setting, which allows more
  flexible control over cursor blinking.
  @see-class{gtk:settings}
  @see-function{gtk:settings-gtk-cursor-blink-timeout}")

;;; --- gtk:settings-gtk-cursor-blink-time -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-cursor-blink-time"
                                               'settings) t)
 "The @code{gtk-cursor-blink-time} property of type @code{:int} (Read / Write)
  @br{}
  Length of the cursor blink cycle, in milliseconds. @br{}
  Allowed values: >= 100 @br{}
  Default value: 1200")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-cursor-blink-time)
      "Accessor"
      (documentation 'settings-gtk-cursor-blink-time 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk-cursor-blink-time object) => setting}
  @syntax{(setf (gtk:settings-gtk-cursor-blink-time object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{an integer with the length of the cursor blink cycle,
    in milliseconds}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-cursor-blink-time} slot of the
    @class{gtk:settings} class.
  @end{short}
  Length of the cursor blink cycle, in milliseconds.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-cursor-blink-timeout ----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-cursor-blink-timeout"
                                               'settings) t)
 "The @code{gtk-cursor-blink-timeout} property of type @code{:int}
  (Read / Write) @br{}
  Time after which the cursor stops blinking, in seconds. The timer is reset
  after each user interaction. Setting this to zero has the same effect as
  setting the @code{gtk-cursor-blink} property to @em{false}. @br{}
  Allowed values: >= 1 @br{}
  Default value: 10")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-cursor-blink-timeout)
      "Accessor"
      (documentation 'settings-gtk-cursor-blink-timeout 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk-cursor-blink-timeout object) => setting}
  @syntax{(setf (gtk:settings-gtk-cursor-blink-timeout object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{an integer with the time after which the cursor stops
    blinking, in seconds}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-cursor-blink-timeout} slot of the
    @class{gtk:settings} class.
  @end{short}
  Time after which the cursor stops blinking, in seconds. The timer is reset
  after each user interaction. Setting this to zero has the same effect as
  setting the @slot[gtk:settings]{gtk-cursor-blink} property to @em{false}.
  @see-class{gtk:settings}
  @see-function{gtk:settings-gtk-cursor-blink}")

;;; --- gtk:settings-gtk-cursor-theme-name -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-cursor-theme-name"
                                               'settings) t)
 "The @code{gtk-cursor-theme-name} property of type @code{:string}
  (Read / Write) @br{}
  Name of the cursor theme to use, or @code{nil} to use the default theme.")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-cursor-theme-name)
      "Accessor"
      (documentation 'settings-gtk-cursor-theme-name 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk-cursor-theme-name object) => setting}
  @syntax{(setf (gtk:settings-gtk-cursor-theme-name object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a string with the cursor theme name}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-cursor-theme-name} slot of the
    @class{gtk:settings} class.
  @end{short}
  Name of the cursor theme to use, or @code{nil} to use the default theme.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-cursor-theme-size -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-cursor-theme-size"
                                               'settings) t)
 "The @code{gtk-cursor-theme-size} property of type @code{:int} (Read / Write)
  @br{}
  Size to use for cursors, or 0 to use the default size. @br{}
  Allowed values: [0,128] @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-cursor-theme-size)
      "Accessor"
      (documentation 'settings-gtk-cursor-theme-size 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk-cursor-theme-size object) => setting}
  @syntax{(setf (gtk:settings-gtk-cursor-theme-size object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{an integer with the size to use for cursors}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-cursor-theme-size} slot of the
    @class{gtk:settings} class.
  @end{short}
  Size to use for cursors, or 0 to use the default size.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-decoration-layout -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-decoration-layout"
                                               'settings) t)
 "The @code{gtk-decoration-layout} property of type @code{:string}
  (Read / Write) @br{}
  This setting determines which buttons should be put in the titlebar of
  client-side decorated windows, and whether they should be placed at the left
  or right. The format of the string is button names, separated by commas. A
  colon separates the buttons that should appear on the left from those on the
  right. Recognized button names are minimize, maximize, close, icon (the
  window icon) and menu (a menu button for the fallback app menu). For example,
  \"menu:minimize,maximize,close\" specifies a menu on the left, and Minimize,
   Maximize and Close buttons on the right. Note that buttons will only be shown
  when they are meaningful. E.g. a menu button only appears when the desktop
  shell does not show the application menu, and a Close button only appears on
  a window that can be closed. Also note that the setting can be overridden
  with the @slot[gtk:header-bar]{decoration-layout} property of the header bar.
  @br{}
  Default value: \"menu:minimize,maximize,close\"")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-decoration-layout)
      "Accessor"
      (documentation 'settings-gtk-decoration-layout 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk-decoration-layout object) => setting}
  @syntax{(setf (gtk:settings-gtk-decoration-layout object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a string with the settings for buttons}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-decoration-layout} slot of the
    @class{gtk:settings} class.
  @end{short}
  This setting determines which buttons should be put in the titlebar of
  client-side decorated windows, and whether they should be placed at the left
  of right.

  The format of the string is button names, separated by commas. A colon
  separates the buttons that should appear on the left from those on the right.
  Recognized button names are minimize, maximize, close, icon (the window icon)
  and menu (a menu button for the fallback app menu).

  For example, \"menu:minimize,maximize,close\" specifies a menu on the left,
  and Minimize, Maximize and Close buttons on the right.

  Note that buttons will only be shown when they are meaningful. E.g. a menu
  button only appears when the desktop shell does not show the app menu, and a
  Close button only appears on a window that can be closed.

  Also note that the setting can be overridden with the
  @slot[gtk:header-bar]{decoration-layout} property of the header bar.
  @see-class{gtk:settings}
  @see-class{gtk:header-bar}
  @see-function{gtk:header-bar-decoration-layout}")

;;; --- gtk:settings-gtk-dialogs-use-header ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-dialogs-use-header"
                                               'settings) t)
 "The @code{gtk-dialogs-use-header} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether built-in GTK dialogs such as the file chooser, the color chooser or
  the font chooser will use a header bar at the top to show action widgets, or
  an action area at the bottom. This setting does not affect custom dialogs
  using the @class{gtk:dialog} widget directly, or message dialogs. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-dialogs-use-header)
      "Accessor"
      (documentation 'settings-gtk-dialogs-use-header 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk-dialogs-use-header object) => setting}
  @syntax{(setf (gtk:settings-gtk-dialogs-use-header object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether dialogs use a header bar}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-dialogs-use-header} slot of the
    @class{gtk:settings} class.
  @end{short}
  Whether built-in GTK dialogs such as the file chooser, the color chooser or
  the font chooser will use a header bar at the top to show action widgets, or
  an action area at the bottom. This setting does not affect custom dialogs
  using the @class{gtk:dialog} widget directly, or message dialogs.
  @see-class{gtk:settings}
  @see-class{gtk:dialog}")

;;; --- gtk:settings-gtk-dnd-drag-threshold ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-dnd-drag-threshold"
                                               'settings) t)
 "The @code{gtk-dnd-drag-threshold} property of type @code{:int} (Read / Write)
  @br{}
  Number of pixels the cursor can move before dragging. @br{}
  Allowed values: >= 1 @br{}
  Default value: 8")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-dnd-drag-threshold)
      "Accessor"
      (documentation 'settings-gtk-dnd-drag-threshold 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk-dnd-drag-threshold object) => setting}
  @syntax{(setf (gtk:settings-gtk-dnd-drag-threshold object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{an integer with the number of pixels the cursor can move}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-dnd-drag-threshold} slot of the
    @class{gtk:settings} class.
  @end{short}
  Number of pixels the cursor can move before dragging.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-double-click-distance ---------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-double-click-distance"
                                               'settings) t)
 "The @code{gtk-double-click-distance} property of type @code{:int}
  (Read / Write) @br{}
  Maximum distance in pixels allowed between two clicks for them to be
  considered a double click. @br{}
  Allowed values: >= 0 @br{}
  Default value: 5")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-double-click-distance)
      "Accessor"
      (documentation 'settings-gtk-double-click-distance 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk-double-click-distance object) => setting}
  @syntax{(setf (gtk:settings-gtk-double-click-distance object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{an integer with the maximum distance in pixels}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-double-click-distance} slot of the
    @class{gtk:settings} class.
  @end{short}
  Maximum distance in pixels allowed between two clicks for them to be
  considered a double click.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-double-click-time -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-double-click-time"
                                               'settings) t)
 "The @code{gtk-double-click-time} property of type @code{:int} (Read / Write)
  @br{}
  Maximum time allowed between two clicks for them to be considered a double
  click, in milliseconds. @br{}
  Allowed values: >= 0 @br{}
  Default value: 250")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-double-click-time)
      "Accessor"
      (documentation 'settings-gtk-double-click-time 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk-double-click-time object) => setting}
  @syntax{(setf (gtk:settings-gtk-double-click-time object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{an integer with the maximum time allowed between two
    clicks}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-double-click-time} slot of the
    @class{gtk:settings} class.
  @end{short}
  Maximum time allowed between two clicks for them to be considered a double
  click, in milliseconds.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-enable-accels -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-enable-accels"
                                               'settings) t)
 "The @code{gtk-enable-accels} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether menu items should have visible accelerators which can be
  activated. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-enable-accels)
      "Accessor"
      (documentation 'settings-gtk-enable-accels 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk-enable-accels object) => setting}
  @syntax{(setf (gtk:settings-gtk-enable-accels object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether menu items should have visible
    accelerators}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-enable-accels} slot of the
    @class{gtk:settings} class.
  @end{short}
  Whether menu items should have visible accelerators which can be
  activated.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-enable-animations -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-enable-animations"
                                               'settings) t)
 "The @code{gtk-enable-animations} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether to enable toolkit-wide animations. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-enable-animations)
      "Accessor"
      (documentation 'settings-gtk-enable-animations 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk-enable-animations object) => setting}
  @syntax{(setf (gtk:settings-gtk-enable-animations object) setting)}
  @argument[object]{the @class{gtk:settings} object}
  @argument[setting]{a boolean whether to enable animations}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-enable-animations} slot of the
    @class{gtk:settings} class.
  @end{short}
  Whether to enable toolkit-wide animations. The default value is @em{true}.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-enable-event-sounds -----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-enable-event-sounds"
                                               'settings) t)
 "The @code{gtk-enable-event-sounds} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether to play any event sounds at all. See the Sound Theme specification
  for more information on event sounds and sound themes. GTK itself does not
  support event sounds, you have to use a loadable module like the one that
  comes with the @code{libcanberra} library. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-enable-event-sounds)
      "Accessor"
      (documentation 'settings-gtk-enable-event-sounds 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk-enable-event-sounds object) => setting}
  @syntax{(setf (gtk:settings-gtk-enable-event-sounds object) setting)}
  @argument[object]{the @class{gtk:settings} object}
  @argument[setting]{a boolean whether to play any event sounds at all}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-enable-event-sounds} slot of the
    @class{gtk:settings} class.
  @end{short}
  Whether to play any event sounds at all. See the Sound Theme specification
  for more information on event sounds and sound themes. GTK itself does not
  support event sounds, you have to use a loadable module like the one that
  comes with the @code{libcanberra} library.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-enable-input-feedback-sounds --------------------------

#+liber-documentation
(setf (documentation
        (liber:slot-documentation "gtk-enable-input-feedback-sounds"
                                  'settings) t)
 "The @code{gtk-enable-input-feedback-sounds} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether to play event sounds as feedback to user input. See the Sound Theme
  specification for more information on event sounds and sound themes. GTK
  itself does not support event sounds, you have to use a loadable module like
  the one that comes with the @code{libcanberra} library. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-enable-input-feedback-sounds)
      "Accessor"
      (documentation 'settings-gtk-enable-input-feedback-sounds 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk-enable-input-feedback-sounds object) => setting}
  @syntax{(setf (gtk:settings-gtk-enable-input-feedback-sounds object) setting)}
  @argument[object]{the @class{gtk:settings} object}
  @argument[setting]{a boolean whether to play any event sounds at all}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-enable-input-feedback-sounds} slot
    of the @class{gtk:settings} class.
  @end{short}
  Whether to play event sounds as feedback to user input. See the Sound Theme
  specification for more information on event sounds and sound themes. GTK
  itself does not support event sounds, you have to use a loadable module like
  the one that comes with the @code{libcanberra} library.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-enable-primary-paste ----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-enable-primary-paste"
                                               'settings) t)
 "The @code{gtk-enable-primary-paste} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether a middle click on a mouse should paste the \"PRIMARY\" clipboard
  content at the cursor location. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-enable-primary-paste)
      "Accessor"
      (documentation 'settings-gtk-enable-primary-paste 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk-enable-primary-paste object) => setting}
  @syntax{(setf (gtk:settings-gtk-enable-primary-paste object) setting)}
  @argument[object]{the @class{gtk:settings} object}
  @argument[setting]{a boolean whether a middle click should paste the
    \"PRIMARY\" clipboard}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-enable-primary-paste} slot of the
    @class{gtk:settings} class.
  @end{short}
  Whether a middle click on a mouse should paste the \"PRIMARY\" clipboard
  content at the cursor location.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-entry-password-hint-timeout ---------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-entry-password-hint-timeout"
                                               'settings) t)
 "The @code{gtk-entry-password-hint-timeout} property of type @code{:uint}
  (Read / Write) @br{}
  How long to show the last input character in hidden entries. This value is
  in milliseconds. The value 0 disables showing the last char. The value 600 is
  a good value for enabling it. @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-entry-password-hint-timeout)
      "Accessor"
      (documentation 'settings-gtk-entry-password-hint-timeout 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk-entry-password-hint-timeout object) => setting}
  @syntax{(setf (gtk:settings-gtk-entry-password-hint-timeout object) setting)}
  @argument[object]{the @class{gtk:settings} object}
  @argument[setting]{an unsigned integer for how long to show the last input}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-entry-password-hint-timeout} slot
    of the @class{gtk:settings} class.
  @end{short}
  How long to show the last input character in hidden entries. This value is
  in milliseconds. The value 0 disables showing the last char. The value 600 is
  a good value for enabling it.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-entry-select-on-focus ---------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-entry-select-on-focus"
                                               'settings) t)
 "The @code{gtk-entry-select-on-focus} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether to select the contents of an entry when it is focused. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-entry-select-on-focus)
      "Accessor"
      (documentation 'settings-gtk-entry-select-on-focus 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk-entry-select-on-focus object) => setting}
  @syntax{(setf (gtk:settings-gtk-entry-select-on-focus object) setting)}
  @argument[object]{the @class{gtk:settings} object}
  @argument[setting]{a boolean whether to select the contents of an entry}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-entry-select-on-focus} slot of the
    @class{gtk:settings} class.
  @end{short}
  Whether to select the contents of an entry when it is focused.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-error-bell --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-error-bell" 'settings) t)
 "The @code{gtk-error-bell} property of type @code{:boolean} (Read / Write)
  @br{}
  When @em{true}, keyboard navigation and other input-related errors will cause
  a beep. Since the error bell is implemented using the @fun{gdk:surface-beep}
  function, the windowing system may offer ways to configure the error bell in
  many ways, such as flashing the window or similar visual effects. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-error-bell)
      "Accessor"
      (documentation 'settings-gtk-error-bell 'function)
 "@version{2023-10-2}
  @syntax{(gtk:settings-gtk-error-bell object) => setting}
  @syntax{(setf (gtk:settings-gtk-error-bell object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether errors well cause a beep}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-error-bell} slot of the
    @class{gtk:settings} class.
  @end{short}
  When @em{true}, keyboard navigation and other input-related errors will cause
  a beep. Since the error bell is implemented using the @fun{gdk:surface-beep}
  function, the windowing system may offer ways to configure the error bell in
  many ways, such as flashing the window or similar visual effects.
  @see-class{gtk:settings}
  @see-function{gdk:surface-beep}")

;;; --- gtk:settings-gtk-font-name ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-font-name" 'settings) t)
 "The @code{gtk-font-name} property of type @code{:string} (Read / Write) @br{}
  Name of default font to use.")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-font-name)
      "Accessor"
      (documentation 'settings-gtk-font-name 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk-font-name object) => setting}
  @syntax{(setf (gtk:settings-gtk-font-name object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a string with the name of the default font to use}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-font-name} slot of the
    @class{gtk:settings} class.
  @end{short}
  Name of the default font to use.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-fontconfig-timestamp ----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-fontconfig-timestamp"
                                               'settings) t)
 "The @code{gtk-fontconfig-timestamp} property of type @code{:uint}
  (Read / Write) @br{}
  Timestamp of the current fontconfig configuration. @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-fontconfig-timestamp)
      "Accessor"
      (documentation 'settings-gtk-fontconfig-timestamp 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk-fontconfig-timestamp object) => setting}
  @syntax{(setf (gtk:settings-gtk-fontconfig-timestamp object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{an unsigned integer with the timestamp}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-fontconfig-timestamp} slot of the
    @class{gtk:settings} class.
  @end{short}
  Timestamp of the current fontconfig configuration.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-hint-font-metrics -------------------------------------

#+(and gtk-4-6 liber-documentation)
(setf (documentation (liber:slot-documentation "gtk-hint-font-metrics"
                                               'settings) t)
 "The @code{gtk-hint-font-metrics} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether hinting should be applied to font metrics. Note that this also turns
  off subpixel positioning of glyphs, since it conflicts with metrics hinting.
  Since 4.6")

#+(and gtk-4-6 liber-documentation)
(setf (liber:alias-for-function 'settings-gtk-hint-font-metrics)
      "Accessor"
      (documentation 'settings-gtk-hint-font-metrics 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk-hint-font-metrics object) => setting}
  @syntax{(setf (gtk:settings-gtk-hint-font-metrics object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether hinting should be applied to font
    metrics}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-hint-font-metrics} slot of the
    @class{gtk:settings} class.
  @end{short}
  Whether hinting should be applied to font metrics. Note that this also turns
  off subpixel positioning of glyphs, since it conflicts with metrics hinting.

  Since 4.6
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-icon-theme-name ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-icon-theme-name"
                                               'settings) t)
 "The @code{gtk-icon-theme-name} property of type @code{:string} (Read / Write)
  @br{}
  Name of the icon theme to use.")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-icon-theme-name)
      "Accessor"
      (documentation 'settings-gtk-icon-theme-name 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk-icon-theme-name object) => setting}
  @syntax{(setf (gtk:settings-gtk-icon-theme-name object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a string with the icon theme name}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-icon-theme-name} slot of the
    @class{gtk:settings} class.
  @end{short}
  Name of the icon theme to use.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-im-module ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-im-module" 'settings) t)
 "The @code{gtk-im-module} property of type @code{:string} (Read / Write) @br{}
  Which IM (input method) module should be used by default. This is the input
  method that will be used if the user has not explicitly chosen another input
  method from the IM context menu. This also can be a colon-separated list of
  input methods, which GTK will try in turn until it finds one available on
  the system. See the @class{gtk:im-context} class and the
  @code{gtk-show-input-method-menu} setting. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-im-module)
      "Accessor"
      (documentation 'settings-gtk-im-module 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk-im-module object) => setting}
  @syntax{(setf (gtk:settings-gtk-im-module object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a string with the IM module}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-im-module} slot of the
    @class{gtk:settings} class.
  @end{short}
  Which IM (input method) module should be used by default. This is the input
  method that will be used if the user has not explicitly chosen another input
  method from the IM context menu. This also can be a colon-separated list of
  input methods, which GTK will try in turn until it finds one available on
  the system. See the @class{gtk:im-context} class and the
  @slot[gtk:settings]{gtk-show-input-method-menu} setting.
  @see-class{gtk:settings}
  @see-class{gtk:im-context}
  @see-function{gtk:settings-gtk-show-input-method-menu}")

;;; --- gtk:settings-gtk-keynav-use-caret --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-keynav-use-caret"
                                               'settings) t)
 "The @code{gtk-keynav-use-caret} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether GTK should make sure that text can be navigated with a caret, even
  if it is not editable. This is useful when using a screen reader. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-keynav-use-caret)
      "Accessor"
      (documentation 'settings-gtk-keynav-use-caret 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk-keynav-use-caret object) => setting}
  @syntax{(setf (gtk:settings-gtk-keynav-use-caret object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether GTK should make sure that text can be
    navigated with a caret}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-keynav-use-caret} slot of the
    @class{gtk:settings} class.
  @end{short}
  Whether GTK should make sure that text can be navigated with a caret, even
  if it is not editable. This is useful when using a screen reader.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-label-select-on-focus ---------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-label-select-on-focus"
                                               'settings) t)
 "The @code{gtk-label-select-on-focus} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether to select the contents of a selectable label when it is focused. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-label-select-on-focus)
      "Accessor"
      (documentation 'settings-gtk-label-select-on-focus 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk-label-select-on-focus object) => setting}
  @syntax{(setf (gtk:settings-gtk-label-select-on-focus object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether to select the contents of a selectable
    label}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-label-select-on-focus} slot of the
    @class{gtk:settings} class.
  @end{short}
  Whether to select the contents of a selectable label when it is focused.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-long-press-time ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-long-press-time"
                                               'settings) t)
 "The @code{gtk-long-press-time} property of type @code{:uint} (Read / Write)
  @br{}
  The time for a button or touch press to be considered a \"long press\". @br{}
  Allowed values: <= @code{G_MAXINT} @br{}
  Default value: 50")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-long-press-time)
      "Accessor"
      (documentation 'settings-gtk-long-press-time 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk-long-press-time object) => setting}
  @syntax{(setf (gtk:settings-gtk-long-press-time object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{an unsigned integer with the time for a button or touch
    press to be considered a \"long press\"}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-long-press-time} slot of the
    @class{gtk:settings} class.
  @end{short}
  The time for a button or touch press to be considered a \"long press\".
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-overlay-scrolling -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-overlay-scrolling"
                                               'settings) t)
 "The @code{gtk-overlay-scrolling} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether scrolled windows may use overlayed scrolling indicators. If this is
  set to @em{false}, scrolled windows will have permanent scrollbars. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-overlay-scrolling)
      "Accessor"
      (documentation 'settings-gtk-overlay-scrolling 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk-overlay-scrolling object) => setting}
  @syntax{(setf (gtk:settings-gtk-overlay-scrolling object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether scrolled windows may use overlayed
    scrolled indicators}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-overlay-scrolling} slot of the
    @class{gtk:settings} class.
  @end{short}
  Whether scrolled windows may use overlayed scrolling indicators. If this is
  set to @em{false}, scrolled windows will have permanent scrollbars.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-primary-button-warps-slider ---------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-primary-button-warps-slider"
                                               'settings) t)
 "The @code{gtk-primary-button-warps-slider} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether a click in a @class{gtk:range} widget trough should scroll to the
  click position or scroll by a single page in the respective direction. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-primary-button-warps-slider)
      "Accessor"
      (documentation 'settings-gtk-primary-button-warps-slider 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk-primary-button-warps-slider object) => setting}
  @syntax{(setf (gtk:settings-gtk-primary-button-warps-slider object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether a click should scroll to the click
    position}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-primary-buton-warps-slider} slot of
    the @class{gtk:settings} class.
  @end{short}
  Whether a click in a @class{gtk:range} widget trough should scroll to the
  click position or scroll by a single page in the respective direction.
  @see-class{gtk:settings}
  @see-class{gtk:range}")

;;; --- gtk:settings-gtk-print-backends ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-print-backends"
                                               'settings) t)
 "The @code{gtk-print-backends} property of type @code{:string} (Read / Write)
  @br{}
  A comma-separated list of print backends to use in the print dialog.
  Available print backends depend on the GTK installation, and may include
  @code{\"file\"}, @code{\"cups\"}, @code{\"lpr\"} or @code{\"papi\"}. @br{}
  Default value: @code{\"file,cups\"}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-print-backends)
      "Accessor"
      (documentation 'settings-gtk-print-backends 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk-print-backends object) => setting}
  @syntax{(setf (gtk:settings-gtk-print-backends object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a string with a list of print backends}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-print-backends} slot of the
    @class{gtk:settings} class.
  @end{short}
  A comma-separated list of print backends to use in the print dialog.
  Available print backends depend on the GTK installation, and may include
  @code{\"file\"}, @code{\"cups\"}, @code{\"lpr\"} or @code{\"papi\"}.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-print-preview-command ---------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-print-preview-command"
                                               'settings) t)
 "The @code{gtk-print-preview-command} property of type @code{:string}
  (Read / Write) @br{}
  A command to run for displaying the print preview. The command should
  contain a @code{f} placeholder, which will get replaced by the path to the
  PDF file. The command may also contain a @code{s} placeholder, which will get
  replaced by the path to a file containing the print settings in the format
  produced by the @fun{gtk:print-settings-to-file} function. The preview
  application is responsible for removing the PDF file and the print settings
  file when it is done. @br{}
  Default value:
  @code{\"evince --unlink-tempfile --preview --print-settings %s %f\"}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-print-preview-command)
      "Accessor"
      (documentation 'settings-gtk-print-preview-command 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk-print-preview-command object) => setting}
  @syntax{(setf (gtk:settings-gtk-print-preview-command object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a string with a command to run for displaying the print
    preview}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-print-preview-command} slot of the
    @class{gtk:settings} class.
  @end{short}
  A command to run for displaying the print preview. The command should
  contain a @code{f} placeholder, which will get replaced by the path to the
  PDF file. The command may also contain a @code{s} placeholder, which will get
  replaced by the path to a file containing the print settings in the format
  produced by the @fun{gtk:print-settings-to-file} function. The preview
  application is responsible for removing the PDF file and the print settings
  file when it is done.
  @see-class{gtk:settings}
  @see-function{gtk:print-settings-to-file}")

;;; --- gtk:settings-gtk-recent-files-enabled ----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-recent-files-enabled"
                                               'settings) t)
 "The @code{gtk-recent-files-enabled} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether GTK should keep track of items inside the recently used resources
  list. If set to @em{false}, the list will always be empty. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-recent-files-enabled)
      "Accessor"
      (documentation 'settings-gtk-recent-files-enabled 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk-recent-files-enabled object) => setting}
  @syntax{(setf (gtk:settings-gtk-recent-files-enabled object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether GTK should keep track of items inside
    the recently used resources list}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-recent-files-enabled} slot of the
    @class{gtk:settings} class.
  @end{short}
  Whether GTK should keep track of items inside the recently used resources
  list. If set to @em{false}, the list will always be empty.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-recent-files-max-age ----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-recent-files-max-age"
                                               'settings) t)
 "The @code{gtk-recent-files-max-age} property of type @code{:int}
  (Read / Write) @br{}
  The maximum age, in days, of the items inside the recently used resources
  list. Items older than this setting will be excised from the list. If set to
  0, the list will always be empty; if set to -1, no item will be
  removed. @br{}
  Allowed values: >= 30 @br{}
  Default value: 30")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-recent-files-max-age)
      "Accessor"
      (documentation 'settings-gtk-recent-files-max-age 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk-recent-files-max-age object) => setting}
  @syntax{(setf (gtk:settings-gtk-recent-files-max-age object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{an integer with the maximum age, in days}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-recent-files-max-page} slot of the
    @class{gtk:settings} class.
  @end{short}
  The maximum age, in days, of the items inside the recently used resources
  list. Items older than this setting will be excised from the list. If set to
  0, the list will always be empty, if set to -1, no item will be
  removed.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-shell-shows-app-menu ----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-shell-shows-app-menu"
                                               'settings) t)
 "The @code{shell-shows-app-menu} property of type @code{:boolean}
  (Read / Write) @br{}
  Set to @em{true} if the desktop environment is displaying the application
  menu, @em{false} if the application should display it itself. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-shell-shows-app-menu)
      "Accessor"
      (documentation 'settings-gtk-shell-shows-app-menu 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk-shell-shows-app-menu object) => setting}
  @syntax{(setf (gtk:settings-gtk-shell-shows-app-menu object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether the environment is displaying the
    application menu}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-shell-shows-app-menu} slot of the
    @class{gtk:settings} class.
  @end{short}
  Set to @em{true} if the desktop environment is displaying the application
  menu, @em{false} if the application should display it itself.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-shell-shows-desktop -----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-shell-shows-desktop"
                                               'settings) t)
 "The @code{gtk-shell-shows-desktop} property of type @code{:boolean}
  (Read / Write) @br{}
  Set to @em{true} if the desktop environment is displaying the desktop folder,
  @em{false} if not. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-shell-shows-desktop)
      "Accessor"
      (documentation 'settings-gtk-shell-shows-desktop 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk-shell-shows-desktop object) => setting}
  @syntax{(setf (gtk:settings-gtk-shell-shows-desktop object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether the environment is displaying the
    desktop folder}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-shell-shows-desktop} slot of the
    @class{gtk:settings} class.
  @end{short}
  Set to @em{true} if the desktop environment is displaying the desktop folder,
  @em{false} if not.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-shell-shows-menubar -----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-shell-shows-menubar"
                                               'settings) t)
 "The @code{gtk-shell-shows-menubar} property of type @code{:boolean}
  (Read / Write) @br{}
  Set to @em{true} if the desktop environment is displaying the menubar,
  @em{false} if the application should display it itself. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-shell-shows-menubar)
      "Accessor"
      (documentation 'settings-gtk-shell-shows-menubar 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk-shell-shows-menubar object) => setting}
  @syntax{(setf (gtk:settings-gtk-shell-shows-menubar object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether the environment is displaying the
    menubar}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-shell-shows-menubar} slot of the
    @class{gtk:settings} class.
  @end{short}
  Set to @em{true} if the desktop environment is displaying the menubar,
  @em{false} if the application should display it itself.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-show-status-shapes ------------------------------------

#+(and gtk-4-14 liber-documentation)
(setf (documentation (liber:slot-documentation "gtk-show-status-shapes"
                                               'settings) t)
 "The @code{gtk-show-status-shapes} property of type @code{:boolean}
  (Read / Write) @br{}
  If @em{true}, widgets like switches include shapes to indicate their on/off
  state. Since 4.14 @br{}
  Default value: @em{false}")

#+(and gtk-4-14 liber-documentation)
(setf (liber:alias-for-function 'settings-gtk-show-status-shapes)
      "Accessor"
      (documentation 'settings-gtk-show-status-shapes 'function)
 "@version{2024-5-26}
  @syntax{(gtk:settings-gtk-shell-shows-menubar object) => setting}
  @syntax{(setf (gtk:settings-gtk-shell-shows-menubar object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether widgets include shapes to indicate their
    on/off state}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-shell-shows-menubar} slot of the
    @class{gtk:settings} class.
  @end{short}
  If @em{true}, widgets like switches include shapes to indicate their on/off
  state.

  Since 4.14
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-sound-theme-name --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-sound-theme-name"
                                               'settings) t)
 "The @code{gtk-sound-theme-name} property of type @code{:string} (Read / Write)
  @br{}
  The XDG sound theme to use for event sounds. See the Sound Theme specification
  for more information on event sounds and sound themes. GTK itself does not
  support event sounds, you have to use a loadable module like the one that
  comes with the @code{libcanberra} library.")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-sound-theme-name)
      "Accessor"
      (documentation 'settings-gtk-sound-theme-name 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk-sound-theme-name object) => setting}
  @syntax{(setf (gtk:settings-gtk-sound-theme-name object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a string with the sound theme name}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-sound-theme-name} slot of the
    @class{gtk:settings} class.
  @end{short}
  The XDG sound theme to use for event sounds. See the Sound Theme specification
  for more information on event sounds and sound themes. GTK itself does not
  support event sounds, you have to use a loadable module like the one that
  comes with the @code{libcanberra} library.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-split-cursor ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-split-cursor" 'settings) t)
 "The @code{gtk-split-cursor} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether two cursors should be displayed for mixed left-to-right and
  right-to-left text. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-split-cursor)
      "Accessor"
      (documentation 'settings-gtk-split-cursor 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk-split-cursor object) => setting}
  @syntax{(setf (gtk:settings-gtk-split-cursor object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether two cursors should be displayed for
    mixed left-to-right and right-to-left text}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-split-cursor} slot of the
    @class{gtk:settings} class.
  @end{short}
  Whether two cursors should be displayed for mixed left-to-right and
  right-to-left text.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-theme-name --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-theme-name" 'settings) t)
 "The @code{gtk-theme-name} property of type @code{:string} (Read / Write) @br{}
  Name of the theme to load.")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-theme-name)
      "Accessor"
      (documentation 'settings-gtk-theme-name 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk-theme-name object) => setting}
  @syntax{(setf (gtk:settings-gtk-theme-name object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a string with the theme name}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-theme-name} slot of the
    @class{gtk:settings} class.
  @end{short}
  Name of the theme to load.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-titlebar-double-click ---------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-titlebar-double-click"
                                               'settings) t)
 "The @code{gtk-titlebar-double-click} property of type @code{:string}
  (Read / Write) @br{}
  This setting determines the action to take when a double click occures on the
  titlebar of client-side decorated windows. Recognized actions are
  \"minimize\", \"toggle-maximize\", \"menu\", \"lower\" or \"none\". @br{}
  Default value: \"toggle-maximize\"")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-titlebar-double-click)
      "Accessor"
      (documentation 'settings-gtk-titlebar-double-click 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk-titlebar-double-click object) => setting}
  @syntax{(setf (gtk:settings-gtk-titlebar-double-click object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a string with an action}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-titlebar-double-click} slot of the
    @class{gtk:settings} class.
  @end{short}
  This setting determines the action to take when a double click occures on the
  titlebar of client-side decorated windows. Recognized actions are
  \"minimize\", \"toggle-maximize\", \"menu\", \"lower\" or \"none\".
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-titlebar-middle-click ---------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-titlebar-middle-click"
                                               'settings) t)
 "The @code{gtk-titlebar-middle-click} property of type @code{:string}
  (Read / Write) @br{}
  This setting determines the action to take when a middle-click occurs on the
  titlebar of client-side decorated windows. Recognized actions are
  \"minimize\", \"toggle-maximize\", \"menu\", \"lower\" or \"none\". @br{}
  Default value: \"none\"")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-titlebar-middle-click)
      "Accessor"
      (documentation 'settings-gtk-titlebar-middle-click 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk-titlebar-middle-click object) => setting}
  @syntax{(setf (gtk:settings-gtk-titlebar-middle-click object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a string with an action}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-titlebar-middle-click} slot of the
    @class{gtk:settings} class.
  @end{short}
  This setting determines the action to take when a middle-click occurs on the
  titlebar of client-side decorated windows. Recognized actions are
  \"minimize\", \"toggle-maximize\", \"menu\", \"lower\" or \"none\". @br{}
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-titlebar-right-click ----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-titlebar-right-click"
                                               'settings) t)
 "The @code{gtk-titlebar-right-click} property of type @code{:string}
  (Read / Write) @br{}
  This setting determines the action to take when a right-click occurs on the
  titlebar of client-side decorated windows. Recognized actions are
  \"minimize\", \"toggle-maximize\", \"menu\", \"lower\" or \"none\". @br{}
  Default value: \"menu\"")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-titlebar-right-click)
      "Accessor"
      (documentation 'settings-gtk-titlebar-right-click 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk-titlebar-right-click object) => setting}
  @syntax{(setf (gtk:settings-gtk-titlebar-right-click object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a string with an action}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-titlebar-right-click} slot of the
    @class{gtk:settings} class.
  @end{short}
  This setting determines the action to take when a right-click occurs on the
  titlebar of client-side decorated windows. Recognized actions are
  \"minimize\", \"toggle-maximize\", \"menu\", \"lower\" or \"none\".
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-xft-antialias -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-xft-antialias"
                                               'settings) t)
 "The @code{gtk-xft-antialias} property of type @code{:int} (Read / Write) @br{}
  Whether to antialias Xft fonts: 0 = no, 1 = yes, -1 = default. @br{}
  Allowed values: [-1,1] @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-xft-antialias)
      "Accessor"
      (documentation 'settings-gtk-xft-antialias 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk-xft-antialias object) => setting}
  @syntax{(setf (gtk:settings-gtk-xft-antialias object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether to antialias Xft fonts}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-xft-antialias} slot of the
    @class{gtk:settings} class.
  @end{short}
  Whether to antialias Xft fonts: 0 = no, 1 = yes, -1 = default.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-xft-dpi -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-xft-dpi" 'settings) t)
 "The @code{gtk-xft-dpi} property of type @code{:int} (Read / Write) @br{}
  Resolution for Xft, in 1024 * dots/inch. -1 to use default value. @br{}
  Allowed values: [-1,1048576] @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-xft-dpi)
      "Accessor"
      (documentation 'settings-gtk-xft-dpi 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk-xft-dpi object) => setting}
  @syntax{(setf (gtk:settings-gtk-xft-dpi object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{an integer with the resolution for Xft}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-xft-dpi} slot of the
    @class{gtk:settings} class.
  @end{short}
  Resolution for Xft, in 1024 * dots/inch. -1 to use default value.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-xft-hinting -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-xft-hinting"
                                               'settings) t)
 "The @code{gtk-xft-hinting} property of type @code{:int} (Read / Write) @br{}
  Whether to hint Xft fonts: 0 = no, 1 = yes, -1 = default. @br{}
  Allowed values: [-1,1] @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-xft-hinting)
      "Accessor"
      (documentation 'settings-gtk-xft-hinting 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk-xft-hinting object) => setting}
  @syntax{(setf (gtk:settings-gtk-xft-hinting object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{an integer whether to hint Xft fonts}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-xft-hinting} slot of the
    @class{gtk:settings} class.
  @end{short}
  Whether to hint Xft fonts: 0 = no, 1 = yes, -1 = default.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-xft-hintstyle -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-xft-hintstyle"
                                               'settings) t)
 "The @code{gtk-xft-hintstyle} property of type @code{:string} (Read / Write)
  @br{}
  What degree of hinting to use: @code{hintnone}, @code{hintslight},
  @code{hintmedium}, or @code{hintfull}. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-xft-hintstyle)
      "Accessor"
      (documentation 'settings-gtk-xft-hintstyle 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk-xft-hintstyle object) => setting}
  @syntax{(setf (gtk:settings-gtk-xft-hintstyle object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a string with the deegree of hinting}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-xft-hintstyle} slot of the
    @class{gtk:settings} class.
  @end{short}
  What degree of hinting to use: @code{hintnone}, @code{hintslight},
  @code{hintmedium}, or @code{hintfull}.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-xft-rgba ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-xft-rgba" 'settings) t)
 "The @code{gtk-xft-rgba} property of type @code{:string} (Read / Write) @br{}
  Type of subpixel antialiasing:
  @code{none}, @code{rgb}, @code{bgr}, @code{vrgb}, @code{vbgr}. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-xft-rgba)
      "Accessor"
      (documentation 'settings-gtk-xft-rgba 'function)
 "@version{2023-8-30}
  @syntax{(gtk:settings-gtk-xft-rgba object) => setting}
  @syntax{(setf (gtk:settings-gtk-xft-rgba object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a string with the type of subpixel antialiasing}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-xft-rgba} slot of the
    @class{gtk:settings} class.
  @end{short}
  Type of subpixel antialiasing:
  @code{none}, @code{rgb}, @code{bgr}, @code{vrgb}, @code{vbgr}.
  @see-class{gtk:settings}")

;;; ----------------------------------------------------------------------------
;;; gtk_settings_get_default
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_settings_get_default" settings-default)
    (g:object settings)
 #+liber-documentation
 "@version{2023-8-30}
  @begin{return}
    A @class{gtk:settings} object. If there is no default display, then returns
    @code{nil}.
  @end{return}
  @begin{short}
    Gets the @class{gtk:settings} object for the default GDK display, creating
    it if necessary.
  @end{short}
  See the @fun{gtk:settings-for-display} function.
  @see-class{gtk:settings}
  @see-function{gtk:settings-for-display}")

(export 'settings-default)

;;; ----------------------------------------------------------------------------
;;; gtk_settings_get_for_display
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_settings_get_for_display" settings-for-display)
    (g:object settings)
 #+liber-documentation
 "@version{#2023-8-30}
  @argument[display]{a @class{gdk:display} object}
  @return{The @class{gtk:settings} object.}
  @begin{short}
    Gets the @class{gtk:settings} object for @arg{display}, creating it if
    necessary.
  @end{short}
  @see-class{gtk:settings}
  @see-class{gdk:display}"
  (screen (g:object gdk:display)))

(export 'settings-for-display)

;;; ----------------------------------------------------------------------------
;;; gtk_settings_reset_property
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_settings_reset_property" settings-reset-property) :void
 #+liber-documentation
 "@version{2023-8-30}
  @argument[settings]{a @class{gtk:settings} object}
  @argument[name]{a string with the name of the setting to reset}
  @begin{short}
    Undoes the effect of calling the @fun{g:object-property} function to install
    an application-specific value for a setting.
  @end{short}
  After this call, the setting will again follow the session-wide value for
  this setting.
  @see-class{gtk:settings}
  @see-function{g:object-property}"
  (settings (g:object settings))
  (name :string))

(export 'settings-reset-property)

;;; --- End of file gtk4.settings.lisp -----------------------------------------
