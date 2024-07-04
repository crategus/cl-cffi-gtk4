(in-package :gtk-test)

(def-suite gtk-recent-manager :in gtk-suite)
(in-suite gtk-recent-manager)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkRecentInfo
;;;     GtkRecentData
;;;     GtkRecentManagerError

;;;     GtkRecentManager

(test gtk-recent-manager-class
  ;; Check type
  (is (g:type-is-object "GtkRecentManager"))
  ;; Check registered name
  (is (eq 'gtk:recent-manager
          (glib:symbol-for-gtype "GtkRecentManager")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkRecentManager")
          (g:gtype (cffi:foreign-funcall "gtk_recent_manager_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkRecentManager")))
  ;; Check children
  (is (equal '()
             (gtk-test:list-children "GtkRecentManager")))
  ;; Check interfaces
  (is (equal '()
             (gtk-test:list-interfaces "GtkRecentManager")))
  ;; Check class properties
  (is (equal '("filename" "size")
             (gtk-test:list-properties "GtkRecentManager")))
  ;; Check signals
  (is (equal '("changed")
             (gtk-test:list-signals "GtkRecentManager")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkRecentManager" GTK-RECENT-MANAGER
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_recent_manager_get_type")
                       ((FILENAME GTK-RECENT-MANAGER-FILENAME "filename"
                         "gchararray" T NIL)
                        (SIZE GTK-RECENT-MANAGER-SIZE "size" "gint" T NIL)))
             (gobject:get-g-type-definition "GtkRecentManager"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-recent-manager-properties
  (let ((manager (gtk:recent-manager-default)))
    (is (stringp (gtk:recent-manager-filename manager)))
    (is (integerp (gtk:recent-manager-size manager)))))

;;; --- Signals ----------------------------------------------------------------

;;;     changed

;;; --- Functions --------------------------------------------------------------

;;;     gtk_recent_manager_new

;;;     gtk_recent_manager_get_default

(test gtk-recent-manager-default
  (is (typep (gtk:recent-manager-default) 'gtk:recent-manager)))

;;;     gtk_recent_manager_add_item
;;;     gtk_recent_manager_has_item
;;;     gtk_recent_manager_lookup_item

;; TODO: Test can fail, improve it

#+nil
(test gtk-recent-manager-add-item
  (let* ((recent (gtk:recent-manager-default))
         (filename (namestring (sys-path "rtest-gtk4-recent-manager.lisp")))
         (uri (concatenate 'string "file://" filename)))
    (is-true (gtk:recent-manager-add-item recent uri))
    (is-true (gtk:recent-manager-has-item recent uri))
    (is (string= uri
                 (gtk:recent-info-uri
                     (gtk:recent-manager-lookup-item recent uri))))))

;;;     gtk_recent_manager_add_full
;;;     gtk_recent_manager_remove_item
;;;     gtk_recent_manager_move_item

;;;     gtk_recent_manager_get_items

(test gtk-recent-manager-items
  (is (every (lambda (x) (typep x 'gtk:recent-info))
             (gtk:recent-manager-items (gtk:recent-manager-default)))))

;;;     gtk_recent_manager_purge_items

;;;     gtk_recent_info_get-uri
;;;     gtk_recent_info_get_display_name
;;;     gtk_recent_info_get_description
;;;     gtk_recent_info_get_mime_type
;;;     gtk_recent_info_get_added
;;;     gtk_recent_info_get_modified
;;;     gtk_recent_info_get_visited
;;;     gtk_recent_info_get_private_hint

;; TODO: Test can fail, improve it

#+nil
(test gtk-recent-info-get
  (let* ((recent (gtk:recent-manager-default))
         (filename (namestring (sys-path "rtest-gtk4-recent-manager.lisp")))
         (uri (concatenate 'string "file://" filename))
         (info (gtk:recent-manager-lookup-item recent uri)))
    (is (typep info 'gtk:recent-info))
    (is (string= uri (gtk:recent-info-uri info)))
    #-windows
    (is (string= "rtest-gtk4-recent-manager.lisp"
                 (gtk:recent-info-display-name info)))
    #+windows
    (is (string= "file: rtest-gtk4-recent-manager.lisp"
                 (gtk:recent-info-display-name info)))
    (is-false (gtk:recent-info-description info))
    #-windows
    (is (string= "text/x-common-lisp" (gtk:recent-info-mime-type info)))
    #+windows
    (is (string= "application/x-ext-lisp" (gtk:recent-info-mime-type info)))
    (is (integerp (gtk:recent-info-added info)))
    (is (integerp (gtk:recent-info-modified info)))
    (is (integerp (gtk:recent-info-visited info)))
    (is-false (gtk:recent-info-private-hint info))))

;;;     gtk_recent_info_get_application_info
;;;     gtk_recent_info_get_applications
;;;     gtk_recent_info_last_application
;;;     gtk_recent_info_has_application

;; TODO: Test can fail, improve it

#+nil
(test gtk-recent-info-application
  (let* ((recent (gtk:recent-manager-default))
         (filename (namestring (sys-path "rtest-gtk4-recent-manager.lisp")))
         (uri (concatenate 'string "file://" filename))
         (info (gtk:recent-manager-lookup-item recent uri))
         (last (gtk:recent-info-last-application info)))
    (is (every #'stringp
               (gtk:recent-info-applications info)))
    (is (stringp (gtk:recent-info-last-application info)))
    (is-true (gtk:recent-info-has-application info last))
    (is (stringp (first (multiple-value-list
                              (gtk:recent-info-application-info info last)))))
    (is (integerp (second (multiple-value-list
                              (gtk:recent-info-application-info info last)))))
    (is (integerp (third (multiple-value-list
                              (gtk:recent-info-application-info info last)))))))

;;;     gtk_recent_info_create_app_info
;;;     gtk_recent_info_get_groups
;;;     gtk_recent_info_has_group
;;;     gtk_recent_info_get_gicon
;;;     gtk_recent_info_get_short_name
;;;     gtk_recent_info_get_uri_display
;;;     gtk_recent_info_get_age
;;;     gtk_recent_info_is_local
;;;     gtk_recent_info_exists
;;;     gtk_recent_info_match

;;; 2024-7-3
