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
             (gtk-test:list-children "GtkSettings")))
  ;; Check interfaces
  (is (equal '("GtkStyleProvider")
             (gtk-test:list-interfaces "GtkSettings")))
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
               "gtk-fontconfig-timestamp"
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
             (gtk-test:list-properties "GtkSettings")))
  ;; Check signals
  (is (equal '("notify")
             (gtk-test:list-signals "GObject")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkSettings" GTK-SETTINGS
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                        ("GtkStyleProvider") :TYPE-INITIALIZER
                        "gtk_settings_get_type")
                       ((GTK-ALTERNATIVE-BUTTON-ORDER
                         GTK-SETTINGS-GTK-ALTERNATIVE-BUTTON-ORDER
                         "gtk-alternative-button-order" "gboolean" T T)
                        (GTK-ALTERNATIVE-SORT-ARROWS
                         GTK-SETTINGS-GTK-ALTERNATIVE-SORT-ARROWS
                         "gtk-alternative-sort-arrows" "gboolean" T T)
                        (GTK-APPLICATION-PREFER-DARK-THEME
                         GTK-SETTINGS-GTK-APPLICATION-PREFER-DARK-THEME
                         "gtk-application-prefer-dark-theme" "gboolean" T T)
                        (GTK-CURSOR-ASPECT-RATIO
                         GTK-SETTINGS-GTK-CURSOR-ASPECT-RATIO
                         "gtk-cursor-aspect-ratio" "gdouble" T T)
                        (GTK-CURSOR-BLINK GTK-SETTINGS-GTK-CURSOR-BLINK
                         "gtk-cursor-blink" "gboolean" T T)
                        (GTK-CURSOR-BLINK-TIME
                         GTK-SETTINGS-GTK-CURSOR-BLINK-TIME
                         "gtk-cursor-blink-time" "gint" T T)
                        (GTK-CURSOR-BLINK-TIMEOUT
                         GTK-SETTINGS-GTK-CURSOR-BLINK-TIMEOUT
                         "gtk-cursor-blink-timeout" "gint" T T)
                        (GTK-CURSOR-THEME-NAME
                         GTK-SETTINGS-GTK-CURSOR-THEME-NAME
                         "gtk-cursor-theme-name" "gchararray" T T)
                        (GTK-CURSOR-THEME-SIZE
                         GTK-SETTINGS-GTK-CURSOR-THEME-SIZE
                         "gtk-cursor-theme-size" "gint" T T)
                        (GTK-DECORATION-LAYOUT
                         GTK-SETTINGS-GTK-DECORATION-LAYOUT
                         "gtk-decoration-layout" "gchararray" T T)
                        (GTK-DIALOGS-USE-HEADER
                         GTK-SETTINGS-GTK-DIALOGS-USE-HEADER
                         "gtk-dialogs-use-header" "gboolean" T T)
                        (GTK-DND-DRAG-THRESHOLD
                         GTK-SETTINGS-GTK-DND-DRAG-THRESHOLD
                         "gtk-dnd-drag-threshold" "gint" T T)
                        (GTK-DOUBLE-CLICK-DISTANCE
                         GTK-SETTINGS-GTK-DOUBLE-CLICK-DISTANCE
                         "gtk-double-click-distance" "gint" T T)
                        (GTK-DOUBLE-CLICK-TIME
                         GTK-SETTINGS-GTK-DOUBLE-CLICK-TIME
                         "gtk-double-click-time" "gint" T T)
                        (GTK-ENABLE-ACCELS GTK-SETTINGS-GTK-ENABLE-ACCELS
                         "gtk-enable-accels" "gboolean" T T)
                        (GTK-ENABLE-ANIMATIONS
                         GTK-SETTINGS-GTK-ENABLE-ANIMATIONS
                         "gtk-enable-animations" "gboolean" T T)
                        (GTK-ENABLE-EVENT-SOUNDS
                         GTK-SETTINGS-GTK-ENABLE-EVENT-SOUNDS
                         "gtk-enable-event-sounds" "gboolean" T T)
                        (GTK-ENABLE-INPUT-FEEDBACK-SOUNDS
                         GTK-SETTINGS-GTK-ENABLE-INPUT-FEEDBACK-SOUNDS
                         "gtk-enable-input-feedback-sounds" "gboolean" T T)
                        (GTK-ENABLE-PRIMARY-PASTE
                         GTK-SETTINGS-GTK-ENABLE-PRIMARY-PASTE
                         "gtk-enable-primary-paste" "gboolean" T T)
                        (GTK-ENTRY-PASSWORD-HINT-TIMEOUT
                         GTK-SETTINGS-GTK-ENTRY-PASSWORD-HINT-TIMEOUT
                         "gtk-entry-password-hint-timeout" "guint" T T)
                        (GTK-ENTRY-SELECT-ON-FOCUS
                         GTK-SETTINGS-GTK-ENTRY-SELECT-ON-FOCUS
                         "gtk-entry-select-on-focus" "gboolean" T T)
                        (GTK-ERROR-BELL GTK-SETTINGS-GTK-ERROR-BELL
                         "gtk-error-bell" "gboolean" T T)
                        (GTK-FONT-NAME GTK-SETTINGS-GTK-FONT-NAME
                         "gtk-font-name" "gchararray" T T)
                        (GTK-FONTCONFIG-TIMESTAMP
                         GTK-SETTINGS-GTK-FONTCONFIG-TIMESTAMP
                         "gtk-fontconfig-timestamp" "guint" T T)
                        (GTK-HINT-FONT-METRICS
                         GTK-SETTINGS-GTK-HINT-FONT-METRICS
                         "gtk-hint-font-metrics" "gboolean" T T)
                        (GTK-ICON-THEME-NAME GTK-SETTINGS-GTK-ICON-THEME-NAME
                         "gtk-icon-theme-name" "gchararray" T T)
                        (GTK-IM-MODULE GTK-SETTINGS-GTK-IM-MODULE
                         "gtk-im-module" "gchararray" T T)
                        (GTK-KEYNAV-USE-CARET GTK-SETTINGS-GTK-KEYNAV-USE-CARET
                         "gtk-keynav-use-caret" "gboolean" T T)
                        (GTK-LABEL-SELECT-ON-FOCUS
                         GTK-SETTINGS-GTK-LABEL-SELECT-ON-FOCUS
                         "gtk-label-select-on-focus" "gboolean" T T)
                        (GTK-LONG-PRESS-TIME GTK-SETTINGS-GTK-LONG-PRESS-TIME
                         "gtk-long-press-time" "guint" T T)
                        (GTK-OVERLAY-SCROLLING
                         GTK-SETTINGS-GTK-OVERLAY-SCROLLING
                         "gtk-overlay-scrolling" "gboolean" T T)
                        (GTK-PRIMARY-BUTTON-WARPS-SLIDER
                         GTK-SETTINGS-GTK-PRIMARY-BUTTON-WARPS-SLIDER
                         "gtk-primary-button-warps-slider" "gboolean" T T)
                        (GTK-PRINT-BACKENDS GTK-SETTINGS-GTK-PRINT-BACKENDS
                         "gtk-print-backends" "gchararray" T T)
                        (GTK-PRINT-PREVIEW-COMMAND
                         GTK-SETTINGS-GTK-PRINT-PREVIEW-COMMAND
                         "gtk-print-preview-command" "gchararray" T T)
                        (GTK-RECENT-FILES-ENABLED
                         GTK-SETTINGS-GTK-RECENT-FILES-ENABLED
                         "gtk-recent-files-enabled" "gboolean" T T)
                        (GTK-RECENT-FILES-MAX-AGE
                         GTK-SETTINGS-GTK-RECENT-FILES-MAX-AGE
                         "gtk-recent-files-max-age" "gint" T T)
                        (GTK-SHELL-SHOWS-APP-MENU
                         GTK-SETTINGS-GTK-SHELL-SHOWS-APP-MENU
                         "gtk-shell-shows-app-menu" "gboolean" T T)
                        (GTK-SHELL-SHOWS-DESKTOP
                         GTK-SETTINGS-GTK-SHELL-SHOWS-DESKTOP
                         "gtk-shell-shows-desktop" "gboolean" T T)
                        (GTK-SHELL-SHOWS-MENUBAR
                         GTK-SETTINGS-GTK-SHELL-SHOWS-MENUBAR
                         "gtk-shell-shows-menubar" "gboolean" T T)
                        (GTK-SHOW-STATUS-SHAPES
                         GTK-SETTINGS-GTK-SHOW-STATUS-SHAPES
                         "gtk-show-status-shapes" "gboolean" T T)
                        (GTK-SOUND-THEME-NAME GTK-SETTINGS-GTK-SOUND-THEME-NAME
                         "gtk-sound-theme-name" "gchararray" T T)
                        (GTK-SPLIT-CURSOR GTK-SETTINGS-GTK-SPLIT-CURSOR
                         "gtk-split-cursor" "gboolean" T T)
                        (GTK-THEME-NAME GTK-SETTINGS-GTK-THEME-NAME
                         "gtk-theme-name" "gchararray" T T)
                        (GTK-TITLEBAR-DOUBLE-CLICK
                         GTK-SETTINGS-GTK-TITLEBAR-DOUBLE-CLICK
                         "gtk-titlebar-double-click" "gchararray" T T)
                        (GTK-TITLEBAR-MIDDLE-CLICK
                         GTK-SETTINGS-GTK-TITLEBAR-MIDDLE-CLICK
                         "gtk-titlebar-middle-click" "gchararray" T T)
                        (GTK-TITLEBAR-RIGHT-CLICK
                         GTK-SETTINGS-GTK-TITLEBAR-RIGHT-CLICK
                         "gtk-titlebar-right-click" "gchararray" T T)
                        (GTK-XFT-ANTIALIAS GTK-SETTINGS-GTK-XFT-ANTIALIAS
                         "gtk-xft-antialias" "gint" T T)
                        (GTK-XFT-DPI GTK-SETTINGS-GTK-XFT-DPI "gtk-xft-dpi"
                         "gint" T T)
                        (GTK-XFT-HINTING GTK-SETTINGS-GTK-XFT-HINTING
                         "gtk-xft-hinting" "gint" T T)
                        (GTK-XFT-HINTSTYLE GTK-SETTINGS-GTK-XFT-HINTSTYLE
                         "gtk-xft-hintstyle" "gchararray" T T)
                        (GTK-XFT-RGBA GTK-SETTINGS-GTK-XFT-RGBA "gtk-xft-rgba"
                         "gchararray" T T)))
             (gobject:get-g-type-definition "GtkSettings"))))

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
    (is (= 0 (gtk:settings-gtk-fontconfig-timestamp settings)))
    (is (string= "Yaru" (gtk:settings-gtk-icon-theme-name settings)))
    (is (stringp (gtk:settings-gtk-im-module settings)))
    (is-false (gtk:settings-gtk-keynav-use-caret settings))
    (is-true (gtk:settings-gtk-label-select-on-focus settings))
    (is (= 500 (gtk:settings-gtk-long-press-time settings)))
    (is-true (gtk:settings-gtk-overlay-scrolling settings))
    (is-true (gtk:settings-gtk-primary-button-warps-slider settings))
    (is (string= "cups,file" (gtk:settings-gtk-print-backends settings)))
    (is (string= "evince --unlink-tempfile --preview --print-settings %s %f"
                 (gtk:settings-gtk-print-preview-command settings)))
    ;; FIXME: Signals an error: null-pointer in parse-g-param-spec.
    (signals (error) (gtk:settings-gtk-recent-files-enabled settings))
    (is (= -1 (gtk:settings-gtk-recent-files-max-age settings)))
    (is-false (gtk:settings-gtk-shell-shows-app-menu settings))
    (is-true (gtk:settings-gtk-shell-shows-desktop settings))
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
                 "gtk-fontconfig-timestamp"
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

;;; 2024-7-3
