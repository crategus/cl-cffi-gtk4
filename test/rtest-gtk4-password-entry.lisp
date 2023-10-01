(in-package :gtk-test)

(def-suite gtk-password-entry :in gtk-suite)
(in-suite gtk-password-entry)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPasswordEntry

(test gtk-password-entry-class
  ;; Type check
  (is (g:type-is-object "GtkPasswordEntry"))
  ;; Check the registered name
  (is (eq 'gtk:password-entry
          (glib:symbol-for-gtype "GtkPasswordEntry")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkPasswordEntry")
          (g:gtype (cffi:foreign-funcall "gtk_password_entry_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkPasswordEntry")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkPasswordEntry")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkEditable")
             (list-interfaces "GtkPasswordEntry")))
  ;; Check the properties
  (is (equal '("activates-default" "cursor-position" "editable" "enable-undo"
               "extra-menu" "max-width-chars" "placeholder-text"
               "selection-bound" "show-peek-icon" "text" "width-chars" "xalign")
             (list-properties "GtkPasswordEntry")))
  ;; Check the signals
  (is (equal '("activate")
             (list-signals "GtkPasswordEntry")))
  ;; CSS name
  (is (string= "entry"
               (gtk:widget-class-css-name "GtkPasswordEntry")))
  ;; CSS classes
  (is (equal '("password")
             (gtk:widget-css-classes (make-instance 'gtk:password-entry))))
  ;; Accessible role
  (is (eq :TEXT-BOX (gtk:widget-class-accessible-role "GtkPasswordEntry")))
  ;; Check the class definition
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

;;;     activates-default
;;;     extra-menu
;;;     placeholder-text
;;;     show-peek-icon

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

;;; --- 2023-9-30 --------------------------------------------------------------
