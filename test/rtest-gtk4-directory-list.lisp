(in-package :gtk-test)

(def-suite gtk-directory-list :in gtk-suite)
(in-suite gtk-directory-list)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkDirectoryList

(test gtk-directory-list-class
  ;; Check type
  (is (g:type-is-object "GtkDirectoryList"))
  ;; Check registered name
  (is (eq 'gtk:directory-list
          (glib:symbol-for-gtype "GtkDirectoryList")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkDirectoryList")
          (g:gtype (cffi:foreign-funcall "gtk_directory_list_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkDirectoryList")))
  ;; Check children
  (is (equal '()
             (gtk-test:list-children "GtkDirectoryList")))
  ;; Check interfaces
  (is (equal '("GListModel")
             (gtk-test:list-interfaces "GtkDirectoryList")))
  ;; Check properties
  (is (equal '("attributes" "error" "file" "io-priority" "item-type" "loading"
               "monitored" "n-items")
             (gtk-test:list-properties "GtkDirectoryList")))
  ;; Check signals
  (is (equal '()
             (gtk-test:list-signals "GtkDirectoryList")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkDirectoryList"
                                             GTK-DIRECTORY-LIST
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                                ("GListModel") :TYPE-INITIALIZER
                                "gtk_directory_list_get_type")
                               ((ATTRIBUTES GTK-DIRECTORY-LIST-ATTRIBUTES
                                 "attributes" "gchararray" T T)
                                (ERROR GTK-DIRECTORY-LIST-ERROR "error"
                                       "GError" T NIL)
                                (FILE GTK-DIRECTORY-LIST-FILE "file" "GFile" T
                                 T)
                                (IO-PRIORITY GTK-DIRECTORY-LIST-IO-PRIORITY
                                 "io-priority" "gint" T T)
                                (ITEM-TYPE GTK-DIRECTORY-LIST-ITEM-TYPE
                                 "item-type" "GType" T NIL)
                                (LOADING GTK-DIRECTORY-LIST-LOADING "loading"
                                 "gboolean" T NIL)
                                (MONITORED GTK-DIRECTORY-LIST-MONITORED
                                 "monitored" "gboolean" T T)
                                (N-ITEMS GTK-DIRECTORY-LIST-N-ITEMS "n-items"
                                 "guint" T NIL)))
             (gobject:get-g-type-definition "GtkDirectoryList"))))

;;; --- Properties -------------------------------------------------------------

;;;     attributes
;;;     error
;;;     file
;;;     io-priority
;;;     item-type                                          Since 4.8
;;;     loading
;;;     monitored
;;;     n-items                                            Since 4.8

;;; --- Functions --------------------------------------------------------------

;;;     gtk_directory_list_new

;; TODO: The model is loading. But we do not get items in the model.

(test gtk-directory-list-new
  (let* ((path (sys-path ""))
         (file (g:file-new-for-path path))
         (model (gtk:directory-list-new "standard::name" file)))

    (is-false (gtk:directory-list-error model))
    (is (string= "standard::name" (gtk:directory-list-attributes model)))

    (is-true (gtk:directory-list-is-loading model))

    (is (g:type-is-object (gtk:directory-list-item-type model)))
    (is (= 0 (gtk:directory-list-n-items model)))

))

;;;     gtk_directory_list_is_loading

;;; 2024-7-4
