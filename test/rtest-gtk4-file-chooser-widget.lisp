(in-package :gtk-test)

(def-suite gtk-file-chooser-widget :in gtk-suite)
(in-suite gtk-file-chooser-widget)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFileChooserWidget

(test file-chooser-widget-class
  ;; Type check
  (is (g:type-is-object "GtkFileChooserWidget"))
  ;; Check the registered name
  (is (eq 'gtk:file-chooser-widget
          (glib:symbol-for-gtype "GtkFileChooserWidget")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkFileChooserWidget")
          (g:gtype (cffi:foreign-funcall "gtk_file_chooser_widget_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkFileChooserWidget")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkFileChooserWidget")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkFileChooser")
             (list-interfaces "GtkFileChooserWidget")))
  ;; Check the properties
  (is (equal '("action" "create-folders" "filter" "filters" "search-mode"
               "select-multiple" "shortcut-folders" "show-time" "subtitle")
             (list-properties "GtkFileChooserWidget")))
  ;; Check the signals
  (is (equal '("desktop-folder" "down-folder" "home-folder" "location-popup"
               "location-popup-on-paste" "location-toggle-popup"
               "places-shortcut" "quick-bookmark" "recent-shortcut"
               "search-shortcut" "show-hidden" "up-folder")
             (list-signals "GtkFileChooserWidget")))
  ;; CSS information
  (is (string= "filechooser"
               (gtk:widget-class-css-name "GtkFileChooserWidget")))
  (is (string=
"filechooser:dir(ltr)
"
               (gtk:style-context-to-string
                   (gtk:widget-style-context
                       (make-instance 'gtk:file-chooser-widget))
                   :none)))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkFileChooserWidget"
                                     GTK-FILE-CHOOSER-WIDGET
                       (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                         "GtkFileChooser")
                        :TYPE-INITIALIZER "gtk_file_chooser_widget_get_type")
                       ((SEARCH-MODE GTK-FILE-CHOOSER-WIDGET-SEARCH-MODE
                         "search-mode" "gboolean" T T)
                        (SHOW-TIME GTK-FILE-CHOOSER-WIDGET-SHOW-TIME
                         "show-time" "gboolean" T NIL)
                        (SUBTITLE GTK-FILE-CHOOSER-WIDGET-SUBTITLE "subtitle"
                         "gchararray" T NIL)))
             (gobject:get-g-type-definition "GtkFileChooserWidget"))))

;;; --- Properties -------------------------------------------------------------

;;;     search-mode
;;;     show-time                                          Since 4.10
;;;     subtitle

;;; --- Signals ----------------------------------------------------------------

;;;     desktop-folder
;;;     down-folder
;;;     home-folder
;;;     location-popup
;;;     location-popup-on-paste
;;;     location-toggle-popup
;;;     places-shortcut
;;;     quick-bookmark
;;;     recent-shortcut
;;;     search-shortcut
;;;     show-hidden
;;;     up-folder

;;; --- Functions --------------------------------------------------------------

;;;     gtk_file_chooser_widget_new

;;; --- 2023-5-29 --------------------------------------------------------------
