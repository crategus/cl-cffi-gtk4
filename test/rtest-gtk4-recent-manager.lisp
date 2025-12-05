(in-package :gtk-test)

(def-suite gtk-recent-manager :in gtk-recent-manager)
(in-suite gtk-recent-manager)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkRecentInfo

(test gtk-recent-info-boxed
  ;; Check type
  (is (g:type-is-boxed "GtkRecentInfo"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkRecentInfo")
          (g:gtype (cffi:foreign-funcall "gtk_recent_info_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:recent-info
          (glib:symbol-for-gtype "GtkRecentInfo"))))

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
             (glib-test:list-children "GtkRecentManager")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkRecentManager")))
  ;; Check class properties
  (is (equal '("filename" "size")
             (glib-test:list-properties "GtkRecentManager")))
  ;; Check signals
  (is (equal '("changed")
             (glib-test:list-signals "GtkRecentManager")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkRecentManager" GTK:RECENT-MANAGER
                      (:SUPERCLASS G:OBJECT
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gtk_recent_manager_get_type")
                      ((FILENAME RECENT-MANAGER-FILENAME
                        "filename" "gchararray" T NIL)
                       (SIZE RECENT-MANAGER-SIZE "size" "gint" T NIL)))
             (gobject:get-gtype-definition "GtkRecentManager"))))

;;; --- Signals ----------------------------------------------------------------

;;;     changed

(test gtk-recent-info-changed-signal
  (let* ((name "changed")
         (gtype (g:gtype "GtkRecentManager"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Properties -------------------------------------------------------------

(test gtk-recent-manager-properties
  (when *first-run-testsuite*
    (glib-test:with-check-memory (:strong 1)
      (let (manager)
        (is (typep (setf manager
                         (gtk:recent-manager-default)) 'gtk:recent-manager))
        (is (stringp (gtk:recent-manager-filename manager)))
        (is (integerp (gtk:recent-manager-size manager)))))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_recent_manager_new

(test gtk-recent-manager-new
  (glib-test:with-check-memory (manager)
    (is (typep (setf manager
                     (gtk:recent-manager-new)) 'gtk:recent-manager))))

;;;     gtk_recent_manager_get_default

(test gtk-recent-manager-default
  (when *first-run-testsuite*
    (glib-test:with-check-memory (:strong 1)
      (is (typep (gtk:recent-manager-default) 'gtk:recent-manager)))))

;;;     gtk_recent_manager_add_item
;;;     gtk_recent_manager_has_item
;;;     gtk_recent_manager_lookup_item

#-windows
(test gtk-recent-manager-add-item
  (when *first-run-testsuite*
    (glib-test:with-check-memory (:strong 1)
      (let* ((recent (gtk:recent-manager-default))
             (filename (glib-sys:sys-path "test/rtest-gtk4-recent-manager.lisp"))
             (uri (concatenate 'string "file://" (namestring filename))))
        (is-true (gtk:recent-manager-add-item recent uri))
        (is-true (gtk:recent-manager-has-item recent uri))
        (is (string= uri
                     (gtk:recent-info-uri
                         (gtk:recent-manager-lookup-item recent uri))))))))

;;;     gtk_recent_manager_remove_item

(test gtk-recent-manager-remove-item
  (when *first-run-testsuite*
    (glib-test:with-check-memory (:strong 1)
      (let* ((recent (gtk:recent-manager-default))
             (info (first (last (gtk:recent-manager-items recent))))
             (uri (gtk:recent-info-uri info)))
        (is-true (gtk:recent-manager-has-item recent uri))
        (is-true (gtk:recent-manager-remove-item recent uri))
        (is-false (gtk:recent-manager-has-item recent uri))))))

;;;     gtk_recent_manager_add_full                         not implemented

;;;     gtk_recent_manager_move_item

(test gtk-recent-manager-move-item
  (when *first-run-testsuite*
    (glib-test:with-check-memory (:strong 1)
      (let* ((recent (gtk:recent-manager-default))
             (info (first (last (gtk:recent-manager-items recent))))
             (uri (gtk:recent-info-uri info))
             (filename1 (glib-sys:sys-path "new-uri.lisp"))
             (uri1 (concatenate 'string "file://" (namestring filename1))))
        (is-true (gtk:recent-manager-move-item recent uri uri1))
        (is-true (gtk:recent-manager-has-item recent uri1))
        (is-true (gtk:recent-manager-move-item recent uri1 nil)
        (is-false (gtk:recent-manager-has-item recent uri1)))))))

;;;     gtk_recent_manager_get_items

(test gtk-recent-manager-items
  (when *first-run-testsuite*
    (glib-test:with-check-memory (:strong 1)
      (is (every (lambda (x) (typep x 'gtk:recent-info))
                 (gtk:recent-manager-items (gtk:recent-manager-default)))))))

;;;     gtk_recent_manager_purge_items

;;;     gtk_recent_info_get-uri
;;;     gtk_recent_info_get_display_name
;;;     gtk_recent_info_get_description
;;;     gtk_recent_info_get_mime_type
;;;     gtk_recent_info_get_added
;;;     gtk_recent_info_get_modified
;;;     gtk_recent_info_get_visited
;;;     gtk_recent_info_get_private_hint

#+crategus
(test gtk-recent-info-get
  (glib-test:with-check-memory ()
    (let* ((recent (gtk:recent-manager-default))
           (path (glib-sys:sys-path "test/rtest-gtk4-recent-manager.lisp"))
           (filename (namestring path))
           (uri (concatenate 'string "file://" filename))
           (info (gtk:recent-manager-lookup-item recent uri)))
      (is (typep info 'gtk:recent-info))
      (is (string= uri (gtk:recent-info-uri info)))
      (is (string= "rtest-gtk4-recent-manager.lisp"
                   (gtk:recent-info-display-name info)))
      (is-false (gtk:recent-info-description info))
      (is (string= "text/plain" (gtk:recent-info-mime-type info)))
      (is (integerp (gtk:recent-info-added info)))
      (is (integerp (gtk:recent-info-modified info)))
      (is (integerp (gtk:recent-info-visited info)))
      (is-false (gtk:recent-info-private-hint info)))))

;;;     gtk_recent_info_get_application_info
;;;     gtk_recent_info_get_applications
;;;     gtk_recent_info_last_application
;;;     gtk_recent_info_has_application

#-windows
(test gtk-recent-info-application
  (glib-test:with-check-memory ()
    (let* ((recent (gtk:recent-manager-default))
           (filename (glib-sys:sys-path "test/rtest-gtk4-recent-manager.lisp"))
           (uri (concatenate 'string "file://" (namestring filename)))
           (info (gtk:recent-manager-lookup-item recent uri))
           last)
      (when info
        (setf last (gtk:recent-info-last-application info))
        (is (every #'stringp
                   (gtk:recent-info-applications info)))
        (is (stringp (gtk:recent-info-last-application info)))
        (is-true (gtk:recent-info-has-application info last))
        (is (stringp (first (multiple-value-list
                              (gtk:recent-info-application-info info last)))))
        (is (integerp (second (multiple-value-list
                                (gtk:recent-info-application-info info last)))))
        (is (integerp (third (multiple-value-list
                               (gtk:recent-info-application-info info last)))))))))

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

;;; 2025-11-16
