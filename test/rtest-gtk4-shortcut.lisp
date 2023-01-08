(def-suite gtk-shortcut :in gtk-suite)
(in-suite gtk-shortcut)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkShortcut

(test gtk-shortcut-class
  ;; Type check
  (is (g:type-is-object "GtkShortcut"))
  ;; Check the registered name
  (is (eq 'gtk-shortcut
          (gobject:symbol-for-gtype "GtkShortcut")))
  ;; Check the type initializer
  (is (eq (gtype "GtkShortcut")
          (gtype (foreign-funcall "gtk_shortcut_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GObject")
          (g-type-parent "GtkShortcut")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "GtkShortcut"))))
  ;; Check the interfaces
  (is (equal '()
             (mapcar #'g-type-name (g-type-interfaces "GtkShortcut"))))
  ;; Check the class properties
  (is (equal '("action" "arguments" "trigger")
             (list-class-property-names "GtkShortcut")))
  ;; Check the list of signals
  (is (equal '()
             (sort (mapcar #'g-signal-name
                           (g-signal-list-ids "GtkShortcut"))
                   #'string<)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkShortcut" GTK-SHORTCUT
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_shortcut_get_type")
                       ((ACTION GTK-SHORTCUT-ACTION "action"
                         "GtkShortcutAction" T T)
                        (ARGUMENTS GTK-SHORTCUT-ARGUMENTS "arguments"
                         "GVariant" T T)
                        (TRIGGER GTK-SHORTCUT-TRIGGER "trigger"
                         "GtkShortcutTrigger" T T)))
             (get-g-type-definition "GtkShortcut"))))

;;; --- Properties -------------------------------------------------------------

;;;     action
;;;     arguments
;;;     trigger

(test gtk-shortcut-properties
  (let ((shortcut (make-instance 'gtk-shortcut)))
    (is (typep (gtk-shortcut-action shortcut) 'gtk-nothing-action))
    (is (typep (setf (gtk-shortcut-action shortcut)
                     (make-instance 'gtk-activate-action)) 'gtk-activate-action))
    (is (typep (gtk-shortcut-action shortcut) 'gtk-activate-action))
    (is (null-pointer-p (gtk-shortcut-arguments shortcut)))
    (is (typep (gtk-shortcut-trigger shortcut) 'gtk-never-trigger))
    (is (typep (setf (gtk-shortcut-trigger shortcut)
                     (make-instance 'gtk-keyval-trigger)) 'gtk-keyval-trigger))
    (is (typep (gtk-shortcut-trigger shortcut) 'gtk-keyval-trigger))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_shortcut_new
;;;     gtk_shortcut_new_with_arguments

;;; 2022-8-24
