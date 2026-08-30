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

(test gtk-recent-manager-properties.1
  (glib-test:with-check-memory ((manager 2) :strong 1)
    (is (typep (setf manager
                     (gtk:recent-manager-default)) 'gtk:recent-manager))
    (is (stringp (gtk:recent-manager-filename manager)))
    (is (integerp (gtk:recent-manager-size manager)))))

#+crategus
(test gtk-recent-manager-properties.2
  (glib-test:with-check-memory ((manager 2) :strong 1)
    (is (typep (setf manager
                     (gtk:recent-manager-default)) 'gtk:recent-manager))
    (is (string= "/home/dieter/.local/share/recently-used.xbel"
                 (gtk:recent-manager-filename manager)))
    (is (= 1000 (gtk:recent-manager-size manager)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_recent_manager_new

(test gtk-recent-manager-new
  (glib-test:with-check-memory (manager)
    (is (typep (setf manager
                     (gtk:recent-manager-new)) 'gtk:recent-manager))))

;;;     gtk_recent_manager_get_default

(test gtk-recent-manager-default
  (glib-test:with-check-memory ((manager 2) :strong 1)
    (is (typep (setf manager
                     (gtk:recent-manager-default)) 'gtk:recent-manager))))

;;;     gtk_recent_manager_get_items

(test gtk-recent-manager-items
  (glib-test:with-check-memory ((recent 2) :strong 1)
    (is (typep (setf recent (gtk:recent-manager-default)) 'gtk:recent-manager))
    (is (every (lambda (x) (typep x 'gtk:recent-info))
               (gtk:recent-manager-items recent)))))

;;;     gtk_recent_manager_has_item
;;;     gtk_recent_manager_lookup_item

(test gtk-recent-manager-has/lookup-item
  (glib-test:with-check-memory ()
    (let* ((recent (gtk:recent-manager-default))
           (info (first (last (gtk:recent-manager-items recent))))
           (uri (gtk:recent-info-uri info))
           item)
      (is-true (gtk:recent-manager-has-item recent uri))
      (is (typep (setf item
                       (gtk:recent-manager-lookup-item recent uri)) 'gtk:recent-info))
      (is (string= uri
                   (gtk:recent-info-uri item))))))

;;;     gtk_recent_manager_add_item
;;;     gtk_recent_manager_move_item
;;;     gtk_recent_manager_remove_item
;;;     gtk_recent_manager_purge_items

;; gtk:recent-manager-add-item does not work as expeceted. The URI is removed,
;; but not added. Furthermore, the new recent manager operates still on the
;; global unique default recent files.

#+nil
(test gtk-recent-manager-add-item
  (glib-test:with-check-memory ((manager 2) :strong 1)
    (let* ((path (glib-sys:sys-path "test/rtest-gtk4-recent-manager.lisp"))
           (filename (namestring path))
           (uri (concatenate 'string "file://" filename)))

    (is-true (gtk:settings-gtk-recent-files-enabled (gtk:settings-default)))
    (is (= -1 (gtk:settings-gtk-recent-files-max-age (gtk:settings-default))))

    (is (= 1 (g:object-ref-count (setf manager (gtk:recent-manager-new)))))

    ;; Add URI to recent files
    (is-true (gtk:recent-manager-add-item manager uri))
    (is-true (gtk:recent-manager-has-item manager uri))

    ;; Remove URI from recent files
    (is-true (gtk:recent-manager-has-item manager uri))
    (is-true (gtk:recent-manager-remove-item manager uri))
    (is-false (gtk:recent-manager-has-item manager uri))
)))

;;;     gtk_recent_manager_add_full                         not implemented

;;; ----------------------------------------------------------------------------

;;;     gtk_recent_info_get_uri

(test gtk-recent-info-uri
  (glib-test:with-check-memory ((recent 2) :strong 1)
    (is (typep (setf recent (gtk:recent-manager-default)) 'gtk:recent-manager))
    ;; Get last item from recent files
    (let ((info (first (last (gtk:recent-manager-items recent)))))
      (is (stringp (gtk:recent-info-uri info))))))

;;;     gtk_recent_info_get_display_name
;;;     gtk_recent_info_get_description
;;;     gtk_recent_info_get_mime_type
;;;     gtk_recent_info_get_added
;;;     gtk_recent_info_get_modified
;;;     gtk_recent_info_get_visited
;;;     gtk_recent_info_get_private_hint

#+crategus
(test gtk-recent-info-get.1
  (glib-test:with-check-memory ()
    (let* ((recent (gtk:recent-manager-default))
           ;; Look up this file from recent files
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

;;;     gtk_recent_info_get_gicon
;;;     gtk_recent_info_get_short_name
;;;     gtk_recent_info_get_uri_display
;;;     gtk_recent_info_get_age
;;;     gtk_recent_info_is_local
;;;     gtk_recent_info_exists

;; Adds a strong reference for the GThemedIcon object

#+crategus
(test gtk-recent-info-get.2
  (glib-test:with-check-memory (:strong 1)
    (let* ((recent (gtk:recent-manager-default))
           ;; Look up this file from recent files
           (path (glib-sys:sys-path "test/rtest-gtk4-recent-manager.lisp"))
           (filename (namestring path))
           (uri (concatenate 'string "file://" filename))
           (info (gtk:recent-manager-lookup-item recent uri)))
      (is (typep info 'gtk:recent-info))
      (is (string= uri (gtk:recent-info-uri info)))
      (is (typep (gtk:recent-info-gicon info) 'g:themed-icon))
      (is (string= "rtest-gtk4-recent-manager.lisp"
                   (gtk:recent-info-short-name info)))
      (is (string= "/home/dieter/Lisp/github/gtk4/test/rtest-gtk4-recent-manager.lisp"
                   (gtk:recent-info-uri-display info)))
      (is (<= 0 (gtk:recent-info-age info)))
      (is-true (gtk:recent-info-is-local info))
      (is-true (gtk:recent-info-exists info)))))

;;;     gtk_recent_info_get_application_info
;;;     gtk_recent_info_get_applications
;;;     gtk_recent_info_last_application
;;;     gtk_recent_info_has_application

#+crategus
(test gtk-recent-info-application
  (glib-test:with-check-memory ()
    (let* ((recent (gtk:recent-manager-default))
           (filename (glib-sys:sys-path "test/rtest-gtk4-recent-manager.lisp"))
           (uri (concatenate 'string "file://" (namestring filename)))
           (info (gtk:recent-manager-lookup-item recent uri))
           app)
      (when info
        (is (stringp (setf app (gtk:recent-info-last-application info))))
        (is (every #'stringp
                   (gtk:recent-info-applications info)))
        (is (stringp (gtk:recent-info-last-application info)))
        (is-true (gtk:recent-info-has-application info app))
        (is (stringp (first (multiple-value-list
                              (gtk:recent-info-application-info info app)))))
        (is (integerp (second (multiple-value-list
                                (gtk:recent-info-application-info info app)))))
        (is (integerp (third (multiple-value-list
                               (gtk:recent-info-application-info info app)))))))))

;;;     gtk_recent_info_create_app_info

#+crategus
(test gtk-recent-info-create-app-info
  (glib-test:with-check-memory ()
    (let* ((recent (gtk:recent-manager-default))
           (filename (glib-sys:sys-path "test/rtest-gtk4-recent-manager.lisp"))
           (uri (concatenate 'string "file://" (namestring filename)))
           (info (gtk:recent-manager-lookup-item recent uri))
           appinfo)
      (is (typep (setf appinfo (gtk:recent-info-create-app-info info nil)) 'g:object))
      (is (string= "org.gnome.TextEditor.desktop" (gio:app-info-id appinfo)))
      (is (string= "Texteditor" (gio:app-info-name appinfo)))
      (is (string= "Texteditor" (gio:app-info-display-name appinfo))))))

;;;     gtk_recent_info_get_groups
;;;     gtk_recent_info_has_group

(test gtk-recent-info-groups
  (glib-test:with-check-memory ()
    (let* ((recent (gtk:recent-manager-default))
           (info (first (last (gtk:recent-manager-items recent))))
           groups)
      (is (equal '("geany") (setf groups
                                  (gtk:recent-info-groups info))))
      (is-true (gtk:recent-info-has-group info (first groups))))))

;;;     gtk_recent_info_match

(test gtk-recent-info-match
  (glib-test:with-check-memory ()
    (let* ((manager (gtk:recent-manager-default))
           (items (gtk:recent-manager-items manager))
           (info1 (first items))
           (info2 (second items)))
      (is-true (gtk:recent-info-match info1 info1))
      (is-true (gtk:recent-info-match info2 info2))
      (is-false (gtk:recent-info-match info1 info2)))))

;;; 2026-07-14
