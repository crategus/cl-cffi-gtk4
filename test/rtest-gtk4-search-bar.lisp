(in-package :gtk-test)

(def-suite gtk-search-bar :in gtk-suite)
(in-suite gtk-search-bar)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkSearchBar

(test gtk-search-bar-class
  ;; Check type
  (is (g:type-is-object "GtkSearchBar"))
  ;; Check registered name
  (is (eq 'gtk:search-bar
          (glib:symbol-for-gtype "GtkSearchBar")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkSearchBar")
          (g:gtype (cffi:foreign-funcall "gtk_search_bar_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkSearchBar")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkSearchBar")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (glib-test:list-interfaces "GtkSearchBar")))
  ;; Check properties
  (is (equal '("child" "key-capture-widget" "search-mode-enabled"
               "show-close-button")
             (glib-test:list-properties "GtkSearchBar")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkSearchBar")))
  ;; Check CSS name
  (is (string= "searchbar"
               (gtk:widget-class-css-name "GtkSearchBar")))
  ;; Check accessible role
  (is (eq :search (gtk:widget-class-accessible-role "GtkSearchBar")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkSearchBar" GTK:SEARCH-BAR
                      (:SUPERCLASS GTK:WIDGET
                       :EXPORT T
                       :INTERFACES
                       ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
                       :TYPE-INITIALIZER "gtk_search_bar_get_type")
                      ((CHILD SEARCH-BAR-CHILD "child" "GtkWidget" T T)
                       (KEY-CAPTURE-WIDGET SEARCH-BAR-KEY-CAPTURE-WIDGET
                        "key-capture-widget" "GtkWidget" T T)
                       (SEARCH-MODE-ENABLED SEARCH-BAR-SEARCH-MODE-ENABLED
                        "search-mode-enabled" "gboolean" T T)
                       (SHOW-CLOSE-BUTTON SEARCH-BAR-SHOW-CLOSE-BUTTON
                        "show-close-button" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkSearchBar"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-search-bar-properties
  (let ((searchbar (make-instance 'gtk:search-bar)))
    (is-false (gtk:search-bar-child searchbar))
    (is-false (gtk:search-bar-key-capture-widget searchbar))
    (is-false (gtk:search-bar-search-mode-enabled searchbar))
    (is-false (gtk:search-bar-show-close-button searchbar))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_search_bar_new

(test gtk-search-bar-new
  (is (typep (gtk:search-bar-new) 'gtk:search-bar)))

;;;     gtk_search_bar_connect_entry

;;; 2024-9-20
