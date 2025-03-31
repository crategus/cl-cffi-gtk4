(in-package :gtk-test)

(def-suite gtk-about-dialog :in gtk-windows)
(in-suite gtk-about-dialog)

;;; ---Types and Values --------------------------------------------------------

;;;     GtkLicense

(test gtk-license
  ;; Check type
  (is (g:type-is-enum "GtkLicense"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkLicense")
          (g:gtype (cffi:foreign-funcall "gtk_license_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:license
          (glib:symbol-for-gtype "GtkLicense")))
  ;; Check names
  (is (equal '("GTK_LICENSE_UNKNOWN" "GTK_LICENSE_CUSTOM" "GTK_LICENSE_GPL_2_0"
               "GTK_LICENSE_GPL_3_0" "GTK_LICENSE_LGPL_2_1"
               "GTK_LICENSE_LGPL_3_0" "GTK_LICENSE_BSD" "GTK_LICENSE_MIT_X11"
               "GTK_LICENSE_ARTISTIC" "GTK_LICENSE_GPL_2_0_ONLY"
               "GTK_LICENSE_GPL_3_0_ONLY" "GTK_LICENSE_LGPL_2_1_ONLY"
               "GTK_LICENSE_LGPL_3_0_ONLY" "GTK_LICENSE_AGPL_3_0"
               "GTK_LICENSE_AGPL_3_0_ONLY" "GTK_LICENSE_BSD_3"
               "GTK_LICENSE_APACHE_2_0" "GTK_LICENSE_MPL_2_0"
               "GTK_LICENSE_0BSD")
             (glib-test:list-enum-item-names "GtkLicense")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18)
             (glib-test:list-enum-item-values "GtkLicense")))
  ;; Check nick names
  (is (equal '("unknown" "custom" "gpl-2-0" "gpl-3-0" "lgpl-2-1" "lgpl-3-0"
               "bsd" "mit-x11" "artistic" "gpl-2-0-only" "gpl-3-0-only"
               "lgpl-2-1-only" "lgpl-3-0-only" "agpl-3-0" "agpl-3-0-only"
               "bsd-3" "apache-2-0" "mpl-2-0" "0bsd")
             (glib-test:list-enum-item-nicks "GtkLicense")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkLicense" GTK:LICENSE
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
                       (:MPL-2-0 17)
                       (:0BSD 18))
             (gobject:get-gtype-definition "GtkLicense"))))

;;;     GtkAboutDialog

(test gtk-about-dialog-class
  ;; Check type
  (is (g:type-is-object "GtkAboutDialog"))
  ;; Check registered name
  (is (eq 'gtk:about-dialog
          (glib:symbol-for-gtype "GtkAboutDialog")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkAboutDialog")
          (g:gtype (cffi:foreign-funcall "gtk_about_dialog_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWindow")
          (g:type-parent "GtkAboutDialog")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkAboutDialog")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget" "GtkNative"
               "GtkShortcutManager" "GtkRoot")
             (glib-test:list-interfaces "GtkAboutDialog")))
  ;; Check class properties
  (is (equal '("artists" "authors" "comments" "copyright" "documenters"
               "license" "license-type" "logo" "logo-icon-name" "program-name"
               "system-information" "translator-credits" "version" "website"
               "website-label" "wrap-license")
             (glib-test:list-properties "GtkAboutDialog")))
  ;; Check signals
  (is (equal '("activate-link")
             (glib-test:list-signals "GtkAboutDialog")))
  ;; Check CSS name
  (is (string= "window"
               (gtk:widget-class-css-name "GtkAboutDialog")))
  ;; Check accessible role
  #-gtk-4-18
  (is (eq :application (gtk:widget-class-accessible-role "GtkAboutDialog")))
  #+gtk+4-18
  (is (eq :window (gtk:widget-class-accessible-role "GtkAboutDialog")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkAboutDialog" GTK:ABOUT-DIALOG
                       (:SUPERCLASS GTK:WINDOW
                        :EXPORT T
                        :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                         "GtkNative" "GtkRoot" "GtkShortcutManager")
                        :TYPE-INITIALIZER "gtk_about_dialog_get_type")
                       ((ARTISTS ABOUT-DIALOG-ARTISTS "artists" "GStrv" T T)
                        (AUTHORS ABOUT-DIALOG-AUTHORS "authors" "GStrv" T T)
                        (COMMENTS ABOUT-DIALOG-COMMENTS
                         "comments" "gchararray" T T)
                        (COPYRIGHT ABOUT-DIALOG-COPYRIGHT
                         "copyright" "gchararray" T T)
                        (DOCUMENTERS ABOUT-DIALOG-DOCUMENTERS
                         "documenters" "GStrv" T T)
                        (LICENSE ABOUT-DIALOG-LICENSE
                         "license" "gchararray" T T)
                        (LICENSE-TYPE ABOUT-DIALOG-LICENSE-TYPE
                         "license-type" "GtkLicense" T T)
                        (LOGO ABOUT-DIALOG-LOGO "logo" "GdkPaintable" T T)
                        (LOGO-ICON-NAME ABOUT-DIALOG-LOGO-ICON-NAME
                         "logo-icon-name" "gchararray" T T)
                        (PROGRAM-NAME ABOUT-DIALOG-PROGRAM-NAME
                         "program-name" "gchararray" T T)
                        (SYSTEM-INFORMATION ABOUT-DIALOG-SYSTEM-INFORMATION
                         "system-information" "gchararray" T T)
                        (TRANSLATOR-CREDITS ABOUT-DIALOG-TRANSLATOR-CREDITS
                         "translator-credits" "gchararray" T T)
                        (VERSION ABOUT-DIALOG-VERSION
                         "version" "gchararray" T T)
                        (WEBSITE ABOUT-DIALOG-WEBSITE
                         "website" "gchararray" T T)
                        (WEBSITE-LABEL ABOUT-DIALOG-WEBSITE-LABEL
                         "website-label" "gchararray" T T)
                        (WRAP-LICENSE ABOUT-DIALOG-WRAP-LICENSE
                         "wrap-license" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkAboutDialog"))))

;;; --- Signals ----------------------------------------------------------------

(test gtk-about-dialog-activate-link-signal
  (let* ((name "activate-link")
         (gtype (g:gtype "GtkAboutDialog"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    (is (string= "activate-link" (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (eq (g:gtype "gboolean") (g:signal-query-return-type query)))
    (is (equal '("gchararray")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Properties -------------------------------------------------------------

(test gtk-about-dialog-properties
  (glib-test:with-check-memory (dialog)
    (is (typep (setf dialog
                     (make-instance 'gtk:about-dialog)) 'gtk:about-dialog))
    (is-false (gtk:about-dialog-artists dialog))
    (is-false (gtk:about-dialog-authors dialog))
    (is-false (gtk:about-dialog-comments dialog))
    (is-false (gtk:about-dialog-copyright dialog))
    (is-false (gtk:about-dialog-documenters dialog))
    (is-false (gtk:about-dialog-license dialog))
    (is (eq :unknown (gtk:about-dialog-license-type dialog)))
    (is-false (gtk:about-dialog-logo dialog))
    (is-false (gtk:about-dialog-logo-icon-name dialog))
    (is (string= "gtk-test" (gtk:about-dialog-program-name dialog)))
    (is-false (gtk:about-dialog-system-information dialog))
    (is-false (gtk:about-dialog-translator-credits dialog))
    (is-false (gtk:about-dialog-version dialog))
    (is-false (gtk:about-dialog-website dialog))
    (is-false (gtk:about-dialog-website-label dialog))
    (is-false (gtk:about-dialog-wrap-license dialog))
    (is-false (gtk:window-destroy dialog))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_about_dialog_new

(test gtk-about-dialog-new
  (glib-test:with-check-memory (dialog)
    (is (typep (setf dialog (gtk:about-dialog-new)) 'gtk:about-dialog))
    (is (= 2 (g:object-ref-count dialog)))
    (is-false (gtk:window-destroy dialog))))

;;;     gtk_about_dialog_add_credit_section
;;;     gtk_show_about_dialog

;;; 2025-3-31
