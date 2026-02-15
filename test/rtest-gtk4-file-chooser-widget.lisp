(in-package :gtk-test)

(def-suite gtk-file-chooser-widget :in gtk-deprecated)
(in-suite gtk-file-chooser-widget)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFileChooserWidget

(test gtk-file-chooser-widget-class
  ;; Check type
  (is (g:type-is-object "GtkFileChooserWidget"))
  ;; Check registered name
  (is (eq 'gtk:file-chooser-widget
          (glib:symbol-for-gtype "GtkFileChooserWidget")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkFileChooserWidget")
          (g:gtype (cffi:foreign-funcall "gtk_file_chooser_widget_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkFileChooserWidget")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkFileChooserWidget")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkFileChooser")
             (glib-test:list-interfaces "GtkFileChooserWidget")))
  ;; Check properties
  (is (equal '("action" "create-folders" "filter" "filters" "search-mode"
               "select-multiple" "shortcut-folders" "show-time" "subtitle")
             (glib-test:list-properties "GtkFileChooserWidget")))
  ;; Check signals
  (is (equal '("desktop-folder" "down-folder" "home-folder" "location-popup"
               "location-popup-on-paste" "location-toggle-popup"
               "places-shortcut" "quick-bookmark" "recent-shortcut"
               "search-shortcut" "show-hidden" "up-folder")
             (glib-test:list-signals "GtkFileChooserWidget")))
  ;; Check CSS information
  (is (string= "filechooser"
               (gtk:widget-class-css-name "GtkFileChooserWidget")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkFileChooserWidget"
                                      GTK:FILE-CHOOSER-WIDGET
                       (:SUPERCLASS GTK:WIDGET
                        :EXPORT T
                        :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                         "GtkFileChooser")
                        :TYPE-INITIALIZER "gtk_file_chooser_widget_get_type")
                       ((SEARCH-MODE FILE-CHOOSER-WIDGET-SEARCH-MODE
                         "search-mode" "gboolean" T T)
                        (SHOW-TIME FILE-CHOOSER-WIDGET-SHOW-TIME
                         "show-time" "gboolean" T NIL)
                        (SUBTITLE FILE-CHOOSER-WIDGET-SUBTITLE
                         "subtitle" "gchararray" T NIL)))
             (gobject:get-gtype-definition "GtkFileChooserWidget"))))

;;; --- Properties -------------------------------------------------------------

#+nil
(test gtk-file-chooser-widget-properties
  (let ((gtk-init:*warn-deprecated* nil))
    (let ((widget (make-instance 'gtk:file-chooser-widget)))
      (is-false (gtk:file-chooser-widget-search-mode widget))
      (is-false (gtk:file-chooser-widget-show-time widget))
      (is-false (gtk:file-chooser-widget-subtitle widget)))))

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

#+nil
(test gtk-file-chooser-widget-new
  (let ((gtk-init:*warn-deprecated* nil))
    (is (typep (gtk:file-chooser-widget-new :open) 'gtk:file-chooser-widget))
    (is (typep (gtk:file-chooser-widget-new :save) 'gtk:file-chooser-widget))
    (is (typep (gtk:file-chooser-widget-new :select-folder)
               'gtk:file-chooser-widget))))

;;; 2024-9-20
