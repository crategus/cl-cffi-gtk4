(in-package :gtk-test)

(def-suite gtk-icon-theme :in gtk-theming)
(in-suite gtk-icon-theme)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkIconLookupFlags

(test gtk-icon-lookup-flags
  ;; Check type
  (is (g:type-is-flags "GtkIconLookupFlags"))
  ;; Check registered name
  (is (eq 'gtk:icon-lookup-flags
          (glib:symbol-for-gtype "GtkIconLookupFlags")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkIconLookupFlags")
          (g:gtype (cffi:foreign-funcall "gtk_icon_lookup_flags_get_type"
                                         :size))))
  ;; Check names
  (is (equal '("GTK_ICON_LOOKUP_NONE" "GTK_ICON_LOOKUP_FORCE_REGULAR"
               "GTK_ICON_LOOKUP_FORCE_SYMBOLIC" "GTK_ICON_LOOKUP_PRELOAD")
             (glib-test:list-flags-item-names "GtkIconLookupFlags")))
  ;; Check values
  (is (equal '(0 1 2 4)
             (glib-test:list-flags-item-values "GtkIconLookupFlags")))
  ;; Check nick names
  (is (equal '("none" "force-regular" "force-symbolic" "preload")
             (glib-test:list-flags-item-nicks "GtkIconLookupFlags")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-GFLAGS "GtkIconLookupFlags" GTK:ICON-LOOKUP-FLAGS
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gtk_icon_lookup_flags_get_type")
                                     (:NONE 0)
                                     (:FORCE-REGULAR 1)
                                     (:FORCE-SYMBOLIC 2)
                                     (:PRELOAD 4))
             (gobject:get-gtype-definition "GtkIconLookupFlags"))))

;;;     GtkIconTheme

(test gtk-icon-theme-class
  ;; Check type
  (is (g:type-is-object "GtkIconTheme"))
  ;; Check registered name
  (is (eq 'gtk:icon-theme
          (glib:symbol-for-gtype "GtkIconTheme")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkIconTheme")
          (g:gtype (cffi:foreign-funcall "gtk_icon_theme_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkIconTheme")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkIconTheme")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkIconTheme")))
  ;; Check class properties
  (is (equal '("display" "icon-names" "resource-path" "search-path"
               "theme-name")
             (glib-test:list-properties "GtkIconTheme")))
  ;; Check signals
  (is (equal '("changed")
             (glib-test:list-signals "GtkIconTheme")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkIconTheme" GTK:ICON-THEME
                      (:SUPERCLASS GOBJECT:OBJECT
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gtk_icon_theme_get_type")
                      ((DISPLAY ICON-THEME-DISPLAY "display" "GdkDisplay" T T)
                       (ICON-NAMES ICON-THEME-ICON-NAMES
                        "icon-names" "GStrv" T NIL)
                       (RESOURCE-PATH ICON-THEME-RESOURCE-PATH
                        "resource-path" "GStrv" T T)
                       (SEARCH-PATH ICON-THEME-SEARCH-PATH
                        "search-path" "GStrv" T T)
                       (THEME-NAME ICON-THEME-THEME-NAME
                        "theme-name" "gchararray" T T)))
             (gobject:get-gtype-definition "GtkIconTheme"))))

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

;;; --- Properties -------------------------------------------------------------

(test gtk-icon-theme-properties
  (glib-test:with-check-memory (:strong 2)
    (let ((theme (gtk:icon-theme-for-display (gdk:display-default))))
      (is (typep (gtk:icon-theme-display theme) 'gdk:display))
      (is (every #'stringp (gtk:icon-theme-icon-names theme)))
      (is (every #'stringp (gtk:icon-theme-resource-path theme)))
      (is (every #'stringp (gtk:icon-theme-search-path theme)))
      (is (stringp (gtk:icon-theme-theme-name theme))))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_icon_theme_new

(test gtk-icon-theme-new
  (glib-test:with-check-memory (theme)
    (is (typep (setf theme (gtk:icon-theme-new)) 'gtk:icon-theme))))

;;;     gtk_icon_theme_get_for_display

(test gtk-icon-theme-for-display
  (let (theme)
    (is (typep (setf theme
                     (gtk:icon-theme-for-display (gdk:display-default)))
               'gtk:icon-theme))))

;;;     gtk_icon_theme_add_search_path
;;;     gtk_icon_theme_add_resource_path

;;;     gtk_icon_theme_has_icon

#+crategus
(test gtk-icon-theme-has-icon
  (glib-test:with-check-memory (:strong 2)
    (let ((theme (gtk:icon-theme-for-display (gdk:display-default))))
      (is-true (gtk:icon-theme-has-icon theme "gtk-ok"))
      (is-false (gtk:icon-theme-has-icon theme "unkown")))))

;;;     gtk_icon_theme_has_gicon

;;;     gtk_icon_theme_lookup_icon

;; TODO: PAINTABLE holds two references. Why?

(test gtk-icon-theme-lookup-icon
  (glib-test:with-check-memory ((paintable 2) :strong 1)
    (let ((theme (gtk:icon-theme-for-display (gdk:display-default))))
      (setf paintable
            (gtk:icon-theme-lookup-icon theme
                                        "gtk-ok"
                                        nil
                                        48 1
                                        :ltr
                                        :none))
      (is (typep paintable 'gtk:icon-paintable)))))

;;;     gtk_icon_theme_lookup_by_gicon
;;;     gtk_icon_theme_get_icon_sizes

;;; 2025-05-25
