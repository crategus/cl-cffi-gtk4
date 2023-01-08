(in-package :gtk-test)

(def-suite gtk-about-dialog :in gtk-suite)
(in-suite gtk-about-dialog)

;;; ---Types and Values --------------------------------------------------------

;;;     GtkLicense

(test license
  ;; Check the type
  (is (g:type-is-enum "GtkLicense"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkLicense")
          (g:gtype (foreign-funcall "gtk_license_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:license
          (gobject:symbol-for-gtype "GtkLicense")))
  ;; Check the names
  (is (equal '("GTK_LICENSE_UNKNOWN" "GTK_LICENSE_CUSTOM" "GTK_LICENSE_GPL_2_0"
               "GTK_LICENSE_GPL_3_0" "GTK_LICENSE_LGPL_2_1"
               "GTK_LICENSE_LGPL_3_0" "GTK_LICENSE_BSD" "GTK_LICENSE_MIT_X11"
               "GTK_LICENSE_ARTISTIC" "GTK_LICENSE_GPL_2_0_ONLY"
               "GTK_LICENSE_GPL_3_0_ONLY" "GTK_LICENSE_LGPL_2_1_ONLY"
               "GTK_LICENSE_LGPL_3_0_ONLY" "GTK_LICENSE_AGPL_3_0"
               "GTK_LICENSE_AGPL_3_0_ONLY" "GTK_LICENSE_BSD_3"
               "GTK_LICENSE_APACHE_2_0" "GTK_LICENSE_MPL_2_0")
             (mapcar #'gobject:enum-item-name
                     (gobject:get-enum-items "GtkLicense"))))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17)
             (mapcar #'gobject:enum-item-value
                     (gobject:get-enum-items "GtkLicense"))))
  ;; Check the nick names
  (is (equal '("unknown" "custom" "gpl-2-0" "gpl-3-0" "lgpl-2-1" "lgpl-3-0"
               "bsd" "mit-x11" "artistic" "gpl-2-0-only" "gpl-3-0-only"
               "lgpl-2-1-only" "lgpl-3-0-only" "agpl-3-0" "agpl-3-0-only"
               "bsd-3" "apache-2-0" "mpl-2-0")
             (mapcar #'gobject:enum-item-nick
                     (gobject:get-enum-items "GtkLicense"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkLicense"
                             GTK-LICENSE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_license_get_type")
                             (:UNKNOWN 0)
                             (:CUSTOM 1)
                             (:GPL-2-0 2)
                             (:GPL-3-0 3)
                             (:LGPL-2-1 4)
                             (:LGPL-3-0 5)
                             (:BSD 6)
                             (:MIT-X11 7)
                             (:ARTISTIC 8)
                             (:GPL-2-0-ONLY 9)
                             (:GPL-3-0-ONLY 10)
                             (:LGPL-2-1-ONLY 11)
                             (:LGPL-3-0-ONLY 12)
                             (:AGPL-3-0 13)
                             (:AGPL-3-0-ONLY 14)
                             (:BSD-3 15)
                             (:APACHE-2-0 16)
                             (:MPL-2-0 17))
             (gobject:get-g-type-definition "GtkLicense"))))

;;;     GtkAboutDialog

(test about-dialog-class
  ;; Type check
  (is (g:type-is-object "GtkAboutDialog"))
  ;; Check the registered name
  (is (eq 'gtk:about-dialog
          (gobject:symbol-for-gtype "GtkAboutDialog")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkAboutDialog")
          (g:gtype (foreign-funcall "gtk_about_dialog_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWindow")
          (g:type-parent "GtkAboutDialog")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkAboutDialog")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget" "GtkNative"
               "GtkShortcutManager" "GtkRoot")
             (list-interfaces "GtkAboutDialog")))
  ;; Check the class properties
  (is (equal '("artists" "authors" "comments" "copyright" "documenters"
               "license" "license-type" "logo" "logo-icon-name" "program-name"
               "system-information" "translator-credits" "version" "website"
               "website-label" "wrap-license")
             (list-properties "GtkAboutDialog")))
  ;; Check the list of signals
  (is (equal '("activate-link")
             (list-signals "GtkAboutDialog")))
  ;; CSS information
  (is (string= "window"
               (gtk:widget-class-css-name "GtkAboutDialog")))
  (is (string=
"[window.aboutdialog.background.csd:dir(ltr)]
"
               (gtk:style-context-to-string
                   (gtk:widget-style-context (make-instance 'gtk:about-dialog))
                   :none)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkAboutDialog" GTK-ABOUT-DIALOG
                       (:SUPERCLASS GTK-WINDOW :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                         "GtkNative" "GtkRoot" "GtkShortcutManager")
                        :TYPE-INITIALIZER "gtk_about_dialog_get_type")
                       ((ARTISTS GTK-ABOUT-DIALOG-ARTISTS "artists" "GStrv" T
                         T)
                        (AUTHORS GTK-ABOUT-DIALOG-AUTHORS "authors" "GStrv" T
                         T)
                        (COMMENTS GTK-ABOUT-DIALOG-COMMENTS "comments"
                         "gchararray" T T)
                        (COPYRIGHT GTK-ABOUT-DIALOG-COPYRIGHT "copyright"
                         "gchararray" T T)
                        (DOCUMENTERS GTK-ABOUT-DIALOG-DOCUMENTERS "documenters"
                         "GStrv" T T)
                        (LICENSE GTK-ABOUT-DIALOG-LICENSE "license"
                         "gchararray" T T)
                        (LICENSE-TYPE GTK-ABOUT-DIALOG-LICENSE-TYPE
                         "license-type" "GtkLicense" T T)
                        (LOGO GTK-ABOUT-DIALOG-LOGO "logo" "GdkPaintable" T T)
                        (LOGO-ICON-NAME GTK-ABOUT-DIALOG-LOGO-ICON-NAME
                         "logo-icon-name" "gchararray" T T)
                        (PROGRAM-NAME GTK-ABOUT-DIALOG-PROGRAM-NAME
                         "program-name" "gchararray" T T)
                        (SYSTEM-INFORMATION GTK-ABOUT-DIALOG-SYSTEM-INFORMATION
                         "system-information" "gchararray" T T)
                        (TRANSLATOR-CREDITS GTK-ABOUT-DIALOG-TRANSLATOR-CREDITS
                         "translator-credits" "gchararray" T T)
                        (VERSION GTK-ABOUT-DIALOG-VERSION "version"
                         "gchararray" T T)
                        (WEBSITE GTK-ABOUT-DIALOG-WEBSITE "website"
                         "gchararray" T T)
                        (WEBSITE-LABEL GTK-ABOUT-DIALOG-WEBSITE-LABEL
                         "website-label" "gchararray" T T)
                        (WRAP-LICENSE GTK-ABOUT-DIALOG-WRAP-LICENSE
                         "wrap-license" "gboolean" T T)))
             (gobject:get-g-type-definition "GtkAboutDialog"))))

;;; --- Signals ----------------------------------------------------------------

;;;     activate-link

(test about-dialog-activate-link-signal
  (let ((query (g:signal-query (g:signal-lookup "activate-link"
                                                "GtkAboutDialog"))))
    (is (string= "activate-link" (g:signal-query-signal-name query)))
    (is (string= "GtkAboutDialog"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "gboolean" (g:type-name (g:signal-query-return-type query))))
    (is (equal '("gchararray")
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;; --- Properties -------------------------------------------------------------

;;;     artists
;;;     authors
;;;     comments
;;;     copyright
;;;     documenters
;;;     license
;;;     license-type
;;;     logo
;;;     logo-icon-name
;;;     program-name
;;;     system-information
;;;     translator-credits
;;;     version
;;;     website
;;;     website-label
;;;     wrap-license

(test about-dialog-properties
  (let ((dialog (make-instance 'gtk:about-dialog)))
    (is-false (gtk:about-dialog-artists dialog))
    (is-false (gtk:about-dialog-authors dialog))
    (is-false (gtk:about-dialog-comments dialog))
    (is-false (gtk:about-dialog-copyright dialog))
    (is-false (gtk:about-dialog-documenters dialog))
    (is-false (gtk:about-dialog-license dialog))
    (is (eq :unknown (gtk:about-dialog-license-type dialog)))
    (is-false (gtk:about-dialog-logo dialog))
    (is-false (gtk:about-dialog-logo-icon-name dialog))
    (is-false (gtk:about-dialog-program-name dialog))
    (is-false (gtk:about-dialog-system-information dialog))
    (is-false (gtk:about-dialog-translator-credits dialog))
    (is-false (gtk:about-dialog-version dialog))
    (is-false (gtk:about-dialog-website dialog))
    (is-false (gtk:about-dialog-website-label dialog))
    (is-false (gtk:about-dialog-wrap-license dialog))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_about_dialog_new

(test about-dialog-new
  (is (typep (gtk:about-dialog-new) 'gtk:about-dialog)))

;;;     gtk_about_dialog_add_credit_section
;;;     gtk_show_about_dialog

;;; 2022-9-1
