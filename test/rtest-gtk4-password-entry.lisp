(in-package :gtk-test)

(def-suite gtk-password-entry :in gtk-suite)
(in-suite gtk-password-entry)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPasswordEntryBuffer                             Since 4.4

(test gtk-password-entry-buffer-class
  ;; Check type
  (is (g:type-is-object "GtkPasswordEntryBuffer"))
  ;; Check registered name
  (is (eq 'gtk:password-entry-buffer
          (glib:symbol-for-gtype "GtkPasswordEntryBuffer")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkPasswordEntryBuffer")
          (g:gtype (cffi:foreign-funcall "gtk_password_entry_buffer_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkEntryBuffer")
          (g:type-parent "GtkPasswordEntryBuffer")))
  ;; Check children
  (is (equal '()
             (gtk-test:list-children "GtkPasswordEntryBuffer")))
  ;; Check interfaces
  (is (equal '()
             (gtk-test:list-interfaces "GtkPasswordEntryBuffer")))
  ;; Check properties
  (is (equal '()
             (gtk-test:list-properties "GtkPasswordEntryBuffer")))
  ;; Check signals
  (is (equal '()
             (gtk-test:list-signals "GtkPasswordEntryBuffer")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkPasswordEntryBuffer"
                               GTK-PASSWORD-ENTRY-BUFFER
                               (:SUPERCLASS GTK-ENTRY-BUFFER :EXPORT T
                                :INTERFACES NIL :TYPE-INITIALIZER
                                "gtk_password_entry_buffer_get_type")
                               NIL)
             (gobject:get-g-type-definition "GtkPasswordEntryBuffer"))))

;;;     gtk_password_entry_buffer_new                      Since 4.4

(test gtk-password-entry-buffer-new
  (is (typep (gtk:password-entry-buffer-new) 'gtk:password-entry-buffer)))

;;;     GtkPasswordEntry

(test gtk-password-entry-class
  ;; Check type
  (is (g:type-is-object "GtkPasswordEntry"))
  ;; Check registered name
  (is (eq 'gtk:password-entry
          (glib:symbol-for-gtype "GtkPasswordEntry")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkPasswordEntry")
          (g:gtype (cffi:foreign-funcall "gtk_password_entry_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkPasswordEntry")))
  ;; Check children
  (is (equal '()
             (gtk-test:list-children "GtkPasswordEntry")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkEditable")
             (gtk-test:list-interfaces "GtkPasswordEntry")))
  ;; Check properties
  (is (equal '("activates-default" "cursor-position" "editable" "enable-undo"
               "extra-menu" "max-width-chars" "placeholder-text"
               "selection-bound" "show-peek-icon" "text" "width-chars" "xalign")
             (gtk-test:list-properties "GtkPasswordEntry")))
  ;; Check signals
  (is (equal '("activate")
             (gtk-test:list-signals "GtkPasswordEntry")))
  ;; Check CSS name
  (is (string= "entry"
               (gtk:widget-class-css-name "GtkPasswordEntry")))
  ;; Check accessible role
  (is (eq :TEXT-BOX (gtk:widget-class-accessible-role "GtkPasswordEntry")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkPasswordEntry"
                                             GTK-PASSWORD-ENTRY
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkBuildable"
                                 "GtkConstraintTarget" "GtkEditable")
                                :TYPE-INITIALIZER
                                "gtk_password_entry_get_type")
                               ((ACTIVATES-DEFAULT
                                 GTK-PASSWORD-ENTRY-ACTIVATES-DEFAULT
                                 "activates-default" "gboolean" T T)
                                (EXTRA-MENU GTK-PASSWORD-ENTRY-EXTRA-MENU
                                 "extra-menu" "GMenuModel" T T)
                                (PLACEHOLDER-TEXT
                                 GTK-PASSWORD-ENTRY-PLACEHOLDER-TEXT
                                 "placeholder-text" "gchararray" T T)
                                (SHOW-PEEK-ICON
                                 GTK-PASSWORD-ENTRY-SHOW-PEEK-ICON
                                 "show-peek-icon" "gboolean" T T)))
             (gobject:get-g-type-definition "GtkPasswordEntry"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-password-entry-properties
  (let ((entry (make-instance 'gtk:password-entry)))
    (is-false (gtk:password-entry-activates-default entry))
    (is-false (gtk:password-entry-extra-menu entry))
    (is-false (gtk:password-entry-placeholder-text entry))
    (is-false (gtk:password-entry-show-peek-icon entry))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_password_entry_new

(test gtk-password-entry-new
  (is (typep (gtk:password-entry-new) 'gtk:password-entry)))

;;; 2024-7-4
