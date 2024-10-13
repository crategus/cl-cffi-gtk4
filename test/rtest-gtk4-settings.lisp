(in-package :gtk-test)

(def-suite gtk-settings :in gtk-suite)
(in-suite gtk-settings)

;;; --- Types and Values ------------------------------------------------------

;;;     GtkSettingsValue
;;;     GtkSystemSetting

;;;     GtkSettings

(test gtk-settings-class
  ;; Check type
  (is (g:type-is-object "GtkSettings"))
  ;; Check registered name
  (is (eq 'gtk:settings
          (glib:symbol-for-gtype "GtkSettings")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkSettings")
          (g:gtype (cffi:foreign-funcall "gtk_settings_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkSettings")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkSettings")))
  ;; Check interfaces
  (is (equal '("GtkStyleProvider")
             (glib-test:list-interfaces "GtkSettings")))
  ;; Check class properties
  (is (equal '("gtk-alternative-button-order" "gtk-alternative-sort-arrows"
               "gtk-application-prefer-dark-theme" "gtk-cursor-aspect-ratio"
               "gtk-cursor-blink" "gtk-cursor-blink-time"
               "gtk-cursor-blink-timeout" "gtk-cursor-theme-name"
               "gtk-cursor-theme-size" "gtk-decoration-layout"
               "gtk-dialogs-use-header" "gtk-dnd-drag-threshold"
               "gtk-double-click-distance" "gtk-double-click-time"
               "gtk-enable-accels" "gtk-enable-animations"
               "gtk-enable-event-sounds" "gtk-enable-input-feedback-sounds"
               "gtk-enable-primary-paste" "gtk-entry-password-hint-timeout"
               "gtk-entry-select-on-focus" "gtk-error-bell" "gtk-font-name"
               "gtk-font-rendering" "gtk-fontconfig-timestamp"
               "gtk-hint-font-metrics" "gtk-icon-theme-name" "gtk-im-module"
               "gtk-keynav-use-caret" "gtk-label-select-on-focus"
               "gtk-long-press-time" "gtk-overlay-scrolling"
               "gtk-primary-button-warps-slider" "gtk-print-backends"
               "gtk-print-preview-command" "gtk-recent-files-enabled"
               "gtk-recent-files-max-age" "gtk-shell-shows-app-menu"
               "gtk-shell-shows-desktop" "gtk-shell-shows-menubar"
               "gtk-show-status-shapes"
               "gtk-sound-theme-name" "gtk-split-cursor" "gtk-theme-name"
               "gtk-titlebar-double-click" "gtk-titlebar-middle-click"
               "gtk-titlebar-right-click" "gtk-xft-antialias"
               "gtk-xft-dpi" "gtk-xft-hinting" "gtk-xft-hintstyle"
               "gtk-xft-rgba")
             (glib-test:list-properties "GtkSettings")))
  ;; Check signals
  (is (equal '("notify")
             (glib-test:list-signals "GObject")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkSettings" GTK:SETTINGS
                      (:SUPERCLASS GOBJECT:OBJECT
                       :EXPORT T
                       :INTERFACES ("GtkStyleProvider")
                       :TYPE-INITIALIZER "gtk_settings_get_type")
                      ((GTK-ALTERNATIVE-BUTTON-ORDER
                        SETTINGS-GTK-ALTERNATIVE-BUTTON-ORDER
                        "gtk-alternative-button-order" "gboolean" T T)
                       (GTK-ALTERNATIVE-SORT-ARROWS
                        SETTINGS-GTK-ALTERNATIVE-SORT-ARROWS
                        "gtk-alternative-sort-arrows" "gboolean" T T)
                       (GTK-APPLICATION-PREFER-DARK-THEME
                        SETTINGS-GTK-APPLICATION-PREFER-DARK-THEME
                        "gtk-application-prefer-dark-theme" "gboolean" T T)
                       (GTK-CURSOR-ASPECT-RATIO
                        SETTINGS-GTK-CURSOR-ASPECT-RATIO
                        "gtk-cursor-aspect-ratio" "gdouble" T T)
                       (GTK-CURSOR-BLINK SETTINGS-GTK-CURSOR-BLINK
                        "gtk-cursor-blink" "gboolean" T T)
                       (GTK-CURSOR-BLINK-TIME
                        SETTINGS-GTK-CURSOR-BLINK-TIME
                        "gtk-cursor-blink-time" "gint" T T)
                       (GTK-CURSOR-BLINK-TIMEOUT
                        SETTINGS-GTK-CURSOR-BLINK-TIMEOUT
                        "gtk-cursor-blink-timeout" "gint" T T)
                       (GTK-CURSOR-THEME-NAME
                        SETTINGS-GTK-CURSOR-THEME-NAME
                        "gtk-cursor-theme-name" "gchararray" T T)
                       (GTK-CURSOR-THEME-SIZE
                        SETTINGS-GTK-CURSOR-THEME-SIZE
                        "gtk-cursor-theme-size" "gint" T T)
                       (GTK-DECORATION-LAYOUT
                        SETTINGS-GTK-DECORATION-LAYOUT
                        "gtk-decoration-layout" "gchararray" T T)
                       (GTK-DIALOGS-USE-HEADER
                        SETTINGS-GTK-DIALOGS-USE-HEADER
                        "gtk-dialogs-use-header" "gboolean" T T)
                       (GTK-DND-DRAG-THRESHOLD
                        SETTINGS-GTK-DND-DRAG-THRESHOLD
                        "gtk-dnd-drag-threshold" "gint" T T)
                       (GTK-DOUBLE-CLICK-DISTANCE
                        SETTINGS-GTK-DOUBLE-CLICK-DISTANCE
                        "gtk-double-click-distance" "gint" T T)
                       (GTK-DOUBLE-CLICK-TIME
                        SETTINGS-GTK-DOUBLE-CLICK-TIME
                        "gtk-double-click-time" "gint" T T)
                       (GTK-ENABLE-ACCELS SETTINGS-GTK-ENABLE-ACCELS
                        "gtk-enable-accels" "gboolean" T T)
                       (GTK-ENABLE-ANIMATIONS
                        SETTINGS-GTK-ENABLE-ANIMATIONS
                        "gtk-enable-animations" "gboolean" T T)
                       (GTK-ENABLE-EVENT-SOUNDS
                        SETTINGS-GTK-ENABLE-EVENT-SOUNDS
                        "gtk-enable-event-sounds" "gboolean" T T)
                       (GTK-ENABLE-INPUT-FEEDBACK-SOUNDS
                        SETTINGS-GTK-ENABLE-INPUT-FEEDBACK-SOUNDS
                        "gtk-enable-input-feedback-sounds" "gboolean" T T)
                       (GTK-ENABLE-PRIMARY-PASTE
                        SETTINGS-GTK-ENABLE-PRIMARY-PASTE
                        "gtk-enable-primary-paste" "gboolean" T T)
                       (GTK-ENTRY-PASSWORD-HINT-TIMEOUT
                        SETTINGS-GTK-ENTRY-PASSWORD-HINT-TIMEOUT
                        "gtk-entry-password-hint-timeout" "guint" T T)
                       (GTK-ENTRY-SELECT-ON-FOCUS
                        SETTINGS-GTK-ENTRY-SELECT-ON-FOCUS
                        "gtk-entry-select-on-focus" "gboolean" T T)
                       (GTK-ERROR-BELL SETTINGS-GTK-ERROR-BELL
                        "gtk-error-bell" "gboolean" T T)
                       (GTK-FONT-NAME SETTINGS-GTK-FONT-NAME
                        "gtk-font-name" "gchararray" T T)
                       (GTK-FONT-RENDERING SETTINGS-GTK-FONT-RENDERING
                        "gtk-font-rendering" "GtkFontRendering" T T)
                       (GTK-FONTCONFIG-TIMESTAMP
                        SETTINGS-GTK-FONTCONFIG-TIMESTAMP
                        "gtk-fontconfig-timestamp" "guint" T T)
                       (GTK-HINT-FONT-METRICS
                        SETTINGS-GTK-HINT-FONT-METRICS
                        "gtk-hint-font-metrics" "gboolean" T T)
                       (GTK-ICON-THEME-NAME SETTINGS-GTK-ICON-THEME-NAME
                        "gtk-icon-theme-name" "gchararray" T T)
                       (GTK-IM-MODULE SETTINGS-GTK-IM-MODULE
                        "gtk-im-module" "gchararray" T T)
                       (GTK-KEYNAV-USE-CARET SETTINGS-GTK-KEYNAV-USE-CARET
                        "gtk-keynav-use-caret" "gboolean" T T)
                       (GTK-LABEL-SELECT-ON-FOCUS
                        SETTINGS-GTK-LABEL-SELECT-ON-FOCUS
                        "gtk-label-select-on-focus" "gboolean" T T)
                       (GTK-LONG-PRESS-TIME SETTINGS-GTK-LONG-PRESS-TIME
                        "gtk-long-press-time" "guint" T T)
                       (GTK-OVERLAY-SCROLLING
                        SETTINGS-GTK-OVERLAY-SCROLLING
                        "gtk-overlay-scrolling" "gboolean" T T)
                       (GTK-PRIMARY-BUTTON-WARPS-SLIDER
                        SETTINGS-GTK-PRIMARY-BUTTON-WARPS-SLIDER
                        "gtk-primary-button-warps-slider" "gboolean" T T)
                       (GTK-PRINT-BACKENDS SETTINGS-GTK-PRINT-BACKENDS
                        "gtk-print-backends" "gchararray" T T)
                       (GTK-PRINT-PREVIEW-COMMAND
                        SETTINGS-GTK-PRINT-PREVIEW-COMMAND
                        "gtk-print-preview-command" "gchararray" T T)
                       (GTK-RECENT-FILES-ENABLED
                        SETTINGS-GTK-RECENT-FILES-ENABLED
                        "gtk-recent-files-enabled" "gboolean" T T)
                       (GTK-RECENT-FILES-MAX-AGE
                        SETTINGS-GTK-RECENT-FILES-MAX-AGE
                        "gtk-recent-files-max-age" "gint" T T)
                       (GTK-SHELL-SHOWS-APP-MENU
                        SETTINGS-GTK-SHELL-SHOWS-APP-MENU
                        "gtk-shell-shows-app-menu" "gboolean" T T)
                       (GTK-SHELL-SHOWS-DESKTOP
                        SETTINGS-GTK-SHELL-SHOWS-DESKTOP
                        "gtk-shell-shows-desktop" "gboolean" T T)
                       (GTK-SHELL-SHOWS-MENUBAR
                        SETTINGS-GTK-SHELL-SHOWS-MENUBAR
                        "gtk-shell-shows-menubar" "gboolean" T T)
                       (GTK-SHOW-STATUS-SHAPES
                        SETTINGS-GTK-SHOW-STATUS-SHAPES
                        "gtk-show-status-shapes" "gboolean" T T)
                       (GTK-SOUND-THEME-NAME SETTINGS-GTK-SOUND-THEME-NAME
                        "gtk-sound-theme-name" "gchararray" T T)
                       (GTK-SPLIT-CURSOR SETTINGS-GTK-SPLIT-CURSOR
                        "gtk-split-cursor" "gboolean" T T)
                       (GTK-THEME-NAME SETTINGS-GTK-THEME-NAME
                        "gtk-theme-name" "gchararray" T T)
                       (GTK-TITLEBAR-DOUBLE-CLICK
                        SETTINGS-GTK-TITLEBAR-DOUBLE-CLICK
                        "gtk-titlebar-double-click" "gchararray" T T)
                       (GTK-TITLEBAR-MIDDLE-CLICK
                        SETTINGS-GTK-TITLEBAR-MIDDLE-CLICK
                        "gtk-titlebar-middle-click" "gchararray" T T)
                       (GTK-TITLEBAR-RIGHT-CLICK
                        SETTINGS-GTK-TITLEBAR-RIGHT-CLICK
                        "gtk-titlebar-right-click" "gchararray" T T)
                       (GTK-XFT-ANTIALIAS SETTINGS-GTK-XFT-ANTIALIAS
                        "gtk-xft-antialias" "gint" T T)
                       (GTK-XFT-DPI SETTINGS-GTK-XFT-DPI "gtk-xft-dpi"
                        "gint" T T)
                       (GTK-XFT-HINTING SETTINGS-GTK-XFT-HINTING
                        "gtk-xft-hinting" "gint" T T)
                       (GTK-XFT-HINTSTYLE SETTINGS-GTK-XFT-HINTSTYLE
                        "gtk-xft-hintstyle" "gchararray" T T)
                       (GTK-XFT-RGBA SETTINGS-GTK-XFT-RGBA "gtk-xft-rgba"
                        "gchararray" T T)))
             (gobject:get-gtype-definition "GtkSettings"))))

;;; --- Properties -------------------------------------------------------------

#-windows
(test gtk-settings-properties
  (let ((settings (gtk:settings-default)))
    (is-false (gtk:settings-gtk-alternative-button-order settings))
    (is-false (gtk:settings-gtk-alternative-sort-arrows settings))
    (is-false (gtk:settings-gtk-application-prefer-dark-theme settings))
    (is (= 0.04d0 (gtk:settings-gtk-cursor-aspect-ratio settings)))
    (is-true (gtk:settings-gtk-cursor-blink settings))
    (is (= 1200 (gtk:settings-gtk-cursor-blink-time settings)))
    (is (= 10 (gtk:settings-gtk-cursor-blink-timeout settings)))
    (is (string= "Yaru" (gtk:settings-gtk-cursor-theme-name settings)))
    (is (= 24 (gtk:settings-gtk-cursor-theme-size settings)))
    (is (string= ":minimize,maximize,close"
                 (gtk:settings-gtk-decoration-layout settings)))
    (is-true (gtk:settings-gtk-dialogs-use-header settings))
    (is (= 8 (gtk:settings-gtk-dnd-drag-threshold settings)))
    (is (= 5 (gtk:settings-gtk-double-click-distance settings)))
    (is (= 400 (gtk:settings-gtk-double-click-time settings)))
    (is-true (gtk:settings-gtk-enable-accels settings))
    (is-true (gtk:settings-gtk-enable-animations settings))
    (is-true (gtk:settings-gtk-enable-event-sounds settings))
    (is-true (gtk:settings-gtk-enable-input-feedback-sounds settings))
    (is-true (gtk:settings-gtk-enable-primary-paste settings))
    (is-true (gtk:settings-gtk-entry-password-hint-timeout settings))
    (is-true (gtk:settings-gtk-entry-select-on-focus settings))
    (is-true (gtk:settings-gtk-error-bell settings))
    (is (string= "Ubuntu 11" (gtk:settings-gtk-font-name settings)))
    (is (eq :automatic (gtk:settings-gtk-font-rendering settings)))
    (is (= 0 (gtk:settings-gtk-fontconfig-timestamp settings)))
    (is (string= "Yaru" (gtk:settings-gtk-icon-theme-name settings)))
    (is (stringp (gtk:settings-gtk-im-module settings)))
    (is-false (gtk:settings-gtk-keynav-use-caret settings))
    (is-true (gtk:settings-gtk-label-select-on-focus settings))
    (is (= 500 (gtk:settings-gtk-long-press-time settings)))
    (is-true (gtk:settings-gtk-overlay-scrolling settings))
    (is-true (gtk:settings-gtk-primary-button-warps-slider settings))
    (is (string= "cpdb,file" (gtk:settings-gtk-print-backends settings)))
    (is (string= "evince --unlink-tempfile --preview --print-settings %s %f"
                 (gtk:settings-gtk-print-preview-command settings)))
    ;; FIXME: Signals an error: null-pointer in parse-g-param-spec.
    (signals (error) (gtk:settings-gtk-recent-files-enabled settings))
    (is (= -1 (gtk:settings-gtk-recent-files-max-age settings)))
    (is-false (gtk:settings-gtk-shell-shows-app-menu settings))
    ;; FIXME: Can be TRUE. Why?
    (is-false (gtk:settings-gtk-shell-shows-desktop settings))
    (is-false (gtk:settings-gtk-shell-shows-menubar settings))
    (is (string= "Yaru" (gtk:settings-gtk-sound-theme-name settings)))
    (is-false (gtk:settings-gtk-split-cursor settings))
    (is (string= "Yaru" (gtk:settings-gtk-theme-name settings)))
    (is (string= "toggle-maximize"
                 (gtk:settings-gtk-titlebar-double-click settings)))
    (is (string= "lower" (gtk:settings-gtk-titlebar-middle-click settings)))
    (is (string= "menu" (gtk:settings-gtk-titlebar-right-click settings)))
    (is (= 1 (gtk:settings-gtk-xft-antialias settings)))
    (is (= 98304 (gtk:settings-gtk-xft-dpi settings)))
    (is (= 1 (gtk:settings-gtk-xft-hinting settings)))
    (is (string= "hintslight" (gtk:settings-gtk-xft-hintstyle settings)))
    (is (string= "rgb" (gtk:settings-gtk-xft-rgba settings)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_settings_get_default

(test gtk-settings-default
  (is (typep (gtk:settings-default) 'gtk:settings)))

;;;     gtk_settings_get_for_display

;;;     gtk_settings_reset_property

(test gtk-settings-reset-property
  (let ((settings (gtk:settings-default)))
    (is-false (gtk:settings-reset-property settings
                                           "gtk-alternative-button-order"))
    (is (equal '("gtk-alternative-button-order" "gtk-alternative-sort-arrows"
                 "gtk-application-prefer-dark-theme" "gtk-cursor-aspect-ratio"
                 "gtk-cursor-blink" "gtk-cursor-blink-time"
                 "gtk-cursor-blink-timeout" "gtk-cursor-theme-name"
                 "gtk-cursor-theme-size" "gtk-decoration-layout"
                 "gtk-dialogs-use-header" "gtk-dnd-drag-threshold"
                 "gtk-double-click-distance" "gtk-double-click-time"
                 "gtk-enable-accels" "gtk-enable-animations"
                 "gtk-enable-event-sounds" "gtk-enable-input-feedback-sounds"
                 "gtk-enable-primary-paste" "gtk-entry-password-hint-timeout"
                 "gtk-entry-select-on-focus" "gtk-error-bell" "gtk-font-name"
                 "gtk-font-rendering" "gtk-fontconfig-timestamp"
                 "gtk-hint-font-metrics" "gtk-icon-theme-name"
                 "gtk-im-module" "gtk-keynav-use-caret"
                 "gtk-label-select-on-focus" "gtk-long-press-time"
                 "gtk-overlay-scrolling" "gtk-primary-button-warps-slider"
                 "gtk-print-backends" "gtk-print-preview-command"
                 "gtk-recent-files-enabled" "gtk-recent-files-max-age"
                 "gtk-shell-shows-app-menu" "gtk-shell-shows-desktop"
                 "gtk-shell-shows-menubar" "gtk-show-status-shapes"
                 "gtk-sound-theme-name"
                 "gtk-split-cursor" "gtk-theme-name" "gtk-titlebar-double-click"
                 "gtk-titlebar-middle-click" "gtk-titlebar-right-click"
                 "gtk-xft-antialias" "gtk-xft-dpi" "gtk-xft-hinting"
                 "gtk-xft-hintstyle" "gtk-xft-rgba")
               (mapc (lambda (x)
                        (gtk:settings-reset-property settings x))
                     (sort (mapcar #'g:param-spec-name
                                   (g:object-class-list-properties
                                       "GtkSettings"))
                           #'string<))))))

;;; 2024-10-13
