(in-package :gtk-test)

(def-suite gtk-shortcut :in gtk-suite)
(in-suite gtk-shortcut)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkShortcut

(test gtk-shortcut-class
  ;; Type check
  (is (g:type-is-object "GtkShortcut"))
  ;; Check the registered name
  (is (eq 'gtk:shortcut
          (glib:symbol-for-gtype "GtkShortcut")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkShortcut")
          (g:gtype (cffi:foreign-funcall "gtk_shortcut_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkShortcut")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkShortcut")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkShortcut")))
  ;; Check the class properties
  (is (equal '("action" "arguments" "trigger")
             (list-properties "GtkShortcut")))
  ;; Check the list of signals
  (is (equal '()
             (list-signals "GtkShortcut")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkShortcut" GTK-SHORTCUT
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_shortcut_get_type")
                       ((ACTION GTK-SHORTCUT-ACTION "action"
                         "GtkShortcutAction" T T)
                        (ARGUMENTS GTK-SHORTCUT-ARGUMENTS "arguments"
                         "GVariant" T T)
                        (TRIGGER GTK-SHORTCUT-TRIGGER "trigger"
                         "GtkShortcutTrigger" T T)))
             (gobject:get-g-type-definition "GtkShortcut"))))

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
    (is (cffi:null-pointer-p (gtk-shortcut-arguments shortcut)))
    (is (typep (gtk-shortcut-trigger shortcut) 'gtk-never-trigger))
    (is (typep (setf (gtk-shortcut-trigger shortcut)
                     (make-instance 'gtk-keyval-trigger)) 'gtk-keyval-trigger))
    (is (typep (gtk-shortcut-trigger shortcut) 'gtk-keyval-trigger))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_shortcut_new
;;;     gtk_shortcut_new_with_arguments

;;; --- 2023-5-29 --------------------------------------------------------------
