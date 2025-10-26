(in-package :gtk-test)

(def-suite gtk-shortcut :in gtk-keyboard-shortcuts)
(in-suite gtk-shortcut)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkShortcut

(test gtk-shortcut-class
  ;; Check type
  (is (g:type-is-object "GtkShortcut"))
  ;; Check registered name
  (is (eq 'gtk:shortcut
          (glib:symbol-for-gtype "GtkShortcut")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkShortcut")
          (g:gtype (cffi:foreign-funcall "gtk_shortcut_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkShortcut")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkShortcut")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkShortcut")))
  ;; Check class properties
  (is (equal '("action" "arguments" "trigger")
             (glib-test:list-properties "GtkShortcut")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkShortcut")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkShortcut" GTK:SHORTCUT
                      (:SUPERCLASS G:OBJECT
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gtk_shortcut_get_type")
                      ((ACTION SHORTCUT-ACTION "action" "GtkShortcutAction" T T)
                       (ARGUMENTS SHORTCUT-ARGUMENTS "arguments" "GVariant" T T)
                       (TRIGGER SHORTCUT-TRIGGER
                        "trigger" "GtkShortcutTrigger" T T)))
             (gobject:get-gtype-definition "GtkShortcut"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-shortcut-properties.1
  (glib-test:with-check-memory (shortcut :strong 2)
    (let (trigger action)
      (is (typep (setf action (gtk:nothing-action-get)) 'gtk:nothing-action))
      (is (typep (setf trigger (gtk:never-trigger-get)) 'gtk:never-trigger))
      (is (typep (setf shortcut (gtk:shortcut-new trigger action)) 'gtk:shortcut))
      (is (eq action (gtk:shortcut-action shortcut)))
      (is (cffi:null-pointer-p (gtk:shortcut-arguments shortcut)))
      (is (eq trigger (gtk:shortcut-trigger shortcut))))))

(test gtk-shortcut-properties.2
  (glib-test:with-check-memory (shortcut :strong 2)
    (let (trigger action)
      (is (typep (setf action (gtk:nothing-action-get)) 'gtk:nothing-action))
      (is (typep (setf trigger (gtk:never-trigger-get)) 'gtk:never-trigger))
      (is (typep (setf shortcut (gtk:shortcut-new trigger action)) 'gtk:shortcut))
      (is (= 999
             (g:variant-int16 (setf (gtk:shortcut-arguments shortcut)
                                    (g:variant-new-int16 999)))))
      (is (= 999 (g:variant-int16 (gtk:shortcut-arguments shortcut)))))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_shortcut_new

(test gtk-shortcut-new
  (glib-test:with-check-memory (shortcut (trigger 2) (action 2) :strong 2)
    (is (typep (setf trigger
                     (gtk:keyval-trigger-new 113 :control-mask)) 'gtk:keyval-trigger))
    (is (typep (setf action (gtk:named-action-new "quit")) 'gtk:named-action))
    (is (typep (setf shortcut
                     (gtk:shortcut-new trigger action)) 'gtk:shortcut))
    (is (eq trigger (gtk:shortcut-trigger shortcut)))
    (is (eq action (gtk:shortcut-action shortcut)))))

;;;     gtk_shortcut_new_with_arguments

;;; 2025-10-26
