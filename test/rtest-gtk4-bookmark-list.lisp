(in-package :gtk-test)

(def-suite gtk-bookmark-list :in gtk-list-model-support)
(in-suite gtk-bookmark-list)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkBookmarkList

(test gtk-bookmark-list-class
  ;; Check type
  (is (g:type-is-object "GtkBookmarkList"))
  ;; Check registered name
  (is (eq 'gtk:bookmark-list
          (glib:symbol-for-gtype "GtkBookmarkList")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkBookmarkList")
          (g:gtype (cffi:foreign-funcall "gtk_bookmark_list_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkBookmarkList")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkBookmarkList")))
  ;; Check interfaces
  (is (equal '("GListModel")
             (glib-test:list-interfaces "GtkBookmarkList")))
  ;; Check properties
  (is (equal '("attributes" "filename" "io-priority" "item-type" "loading"
               "n-items")
             (glib-test:list-properties "GtkBookmarkList")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkBookmarkList")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkBookmarkList" GTK:BOOKMARK-LIST
                      (:SUPERCLASS GOBJECT:OBJECT
                       :EXPORT T
                       :INTERFACES ("GListModel")
                       :TYPE-INITIALIZER "gtk_bookmark_list_get_type")
                      ((ATTRIBUTES BOOKMARK-LIST-ATTRIBUTES "attributes"
                        "gchararray" T T)
                       (FILENAME BOOKMARK-LIST-FILENAME "filename" "gchararray"
                        T NIL)
                       (IO-PRIORITY BOOKMARK-LIST-IO-PRIORITY "io-priority"
                        "gint" T T)
                       (ITEM-TYPE BOOKMARK-LIST-ITEM-TYPE "item-type" "GType"
                        T NIL)
                       (LOADING BOOKMARK-LIST-LOADING "loading" "gboolean" T
                        NIL)
                       (N-ITEMS BOOKMARK-LIST-N-ITEMS "n-items" "guint" T
                        NIL)))
             (gobject:get-gtype-definition "GtkBookmarkList"))))

;;; --- Properties -------------------------------------------------------------

;;;     attributes
;;;     filename
;;;     io-priority
;;;     item-type                                          Since 4.8
;;;     loading
;;;     n-items                                            Since 4.8

(test gtk-bookmark-list-properties
  (let* ((path (cffi:foreign-funcall "g_get_user_data_dir" :string))
         ;; Default location of the bookmark file
         (filename (concatenate 'string path "/recently-used.xbel"))
         (bookmark (make-instance 'gtk:bookmark-list)))
    (is-false (gtk:bookmark-list-attributes bookmark))
    ;; Default location
    #-windows
    (is (string= filename
                 (gtk:bookmark-list-filename bookmark)))
    ;; Default io priority
    (is (= g:+priority-default+ (gtk:bookmark-list-io-priority bookmark)))
    (is (eq (g:gtype "GFileInfo") (gtk:bookmark-list-item-type bookmark)))
    (is-true (gtk:bookmark-list-loading bookmark))
    (is (= 0 (gtk:bookmark-list-n-items bookmark)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_bookmark_list_new
;;;     gtk_bookmark_list_is_loading

(test gtk-bookmark-list-new
  (let* ((path (cffi:foreign-funcall "g_get_user_data_dir" :string))
         (filename (concatenate 'string path "/recently-used.xbel"))
         (bookmark (gtk:bookmark-list-new filename "*")))
    (is (string= filename (gtk:bookmark-list-filename bookmark)))
    (is (string= "*" (gtk:bookmark-list-attributes bookmark)))
    (is-true (gtk:bookmark-list-is-loading bookmark))
    (is-true (gtk:bookmark-list-loading bookmark))
    ;; TODO: Bookmarks are loaded, but we have no items in the list. Why?
    (is (= 0 (gtk:bookmark-list-n-items bookmark)))))

;;; 2024-12-15
