(in-package :gtk-test)

(def-suite gtk-icon-theme :in gtk-suite)
(in-suite gtk-icon-theme)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkIconLookupFlags

(test gtk-icon-lookup-flags
  ;; Check the type
  (is (g:type-is-flags "GtkIconLookupFlags"))
  ;; Check the registered name
  (is (eq 'gtk:icon-lookup-flags
          (glib:symbol-for-gtype "GtkIconLookupFlags")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkIconLookupFlags")
          (g:gtype (cffi:foreign-funcall "gtk_icon_lookup_flags_get_type"
                                         :size))))
  ;; Check the names
  (is (equal '("GTK_ICON_LOOKUP_FORCE_REGULAR" "GTK_ICON_LOOKUP_FORCE_SYMBOLIC"
               "GTK_ICON_LOOKUP_PRELOAD")
             (list-flags-item-name "GtkIconLookupFlags")))
  ;; Check the values
  (is (equal '(1 2 4)
             (list-flags-item-value "GtkIconLookupFlags")))
  ;; Check the nick names
  (is (equal '("force-regular" "force-symbolic" "preload")
             (list-flags-item-nick "GtkIconLookupFlags")))
  ;; Check the flags definition
  (is (equal '(GOBJECT:DEFINE-G-FLAGS "GtkIconLookupFlags"
                                      GTK-ICON-LOOKUP-FLAGS
                                      (:EXPORT T
                                       :TYPE-INITIALIZER
                                       "gtk_icon_lookup_flags_get_type")
                                      (:FORCE-REGULAR 1)
                                      (:FORCE-SYMBOLIC 2)
                                      (:PRELOAD 4))
             (gobject:get-g-type-definition "GtkIconLookupFlags"))))

;;;     GtkIconTheme

(test gtk-icon-theme-class
  ;; Type check
  (is (g:type-is-object "GtkIconTheme"))
  ;; Check the registered name
  (is (eq 'gtk:icon-theme
          (glib:symbol-for-gtype "GtkIconTheme")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkIconTheme")
          (g:gtype (cffi:foreign-funcall "gtk_icon_theme_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkIconTheme")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkIconTheme")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkIconTheme")))
  ;; Check the class properties
  (is (equal '("display" "icon-names" "resource-path" "search-path"
               "theme-name")
             (list-properties "GtkIconTheme")))
  ;; Check the list of signals
  (is (equal '("changed")
             (list-signals "GtkIconTheme")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkIconTheme" GTK-ICON-THEME
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_icon_theme_get_type")
                       ((DISPLAY GTK-ICON-THEME-DISPLAY "display" "GdkDisplay"
                         T T)
                        (ICON-NAMES GTK-ICON-THEME-ICON-NAMES "icon-names"
                         "GStrv" T NIL)
                        (RESOURCE-PATH GTK-ICON-THEME-RESOURCE-PATH
                         "resource-path" "GStrv" T T)
                        (SEARCH-PATH GTK-ICON-THEME-SEARCH-PATH "search-path"
                         "GStrv" T T)
                        (THEME-NAME GTK-ICON-THEME-THEME-NAME "theme-name"
                         "gchararray" T T)))
             (gobject:get-g-type-definition "GtkIconTheme"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-icon-theme-properties
  (let ((theme (gtk:icon-theme-for-display (gdk:display-default))))
    (is (typep (gtk:icon-theme-display theme) 'gdk:display))
    (is (every #'stringp (gtk:icon-theme-icon-names theme)))
    (is (every #'stringp (gtk:icon-theme-resource-path theme)))
    (is (every #'stringp (gtk:icon-theme-search-path theme)))
    (is (stringp (gtk:icon-theme-theme-name theme)))))

;;; --- Signals ----------------------------------------------------------------

;;;     changed

(test gtk-icon-theme-signals
  ;; Query info for "changed" signal
  (let ((query (g:signal-query (g:signal-lookup "changed" "GtkIconTheme"))))
    (is (string= "changed" (g:signal-query-signal-name query)))
    (is (string= "GtkIconTheme"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (sort (mapcar #'g:type-name (g:signal-query-param-types query))
                     #'string<)))
    (is-false (g:signal-query-signal-detail query))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_icon_theme_new

(test gtk-icon-theme-new
  (is (typep (gtk:icon-theme-new) 'gtk:icon-theme)))

;;;     gtk_icon_theme_get_for_display

(test gtk-icon-theme-for-display
  (is (typep (gtk:icon-theme-for-display (gdk:display-default)) 'gtk:icon-theme)))

;;;     gtk_icon_theme_add_search_path
;;;     gtk_icon_theme_add_resource_path

;;;     gtk_icon_theme_has_icon

#-windows
(test gtk-icon-theme-has-icon
  (let ((theme (gtk:icon-theme-for-display (gdk:display-default))))
    (is-true (gtk:icon-theme-has-icon theme "gtk-ok"))
    (is-false (gtk:icon-theme-has-icon theme "unkown"))))

;;;     gtk_icon_theme_has_gicon

;;;     gtk_icon_theme_lookup_icon

(test gtk-icon-theme-lookup-icon
  (let* ((theme (gtk:icon-theme-for-display (gdk:display-default)))
         (paintable (gtk:icon-theme-lookup-icon theme
                                                "gtk-ok"
                                                nil
                                                48
                                                1
                                                :ltr
                                                :none)))
    (is (typep paintable 'gtk:icon-paintable))))

;;;     gtk_icon_theme_lookup_by_gicon
;;;     gtk_icon_theme_get_icon_sizes

;;; --- 2023-8-29 --------------------------------------------------------------
