(in-package :gtk-test)

(def-suite gtk-shortcut :in gtk-suite)
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

;;;     action
;;;     arguments
;;;     trigger

(test gtk-shortcut-properties.1
  (let ((shortcut (make-instance 'gtk:shortcut)))
    (is (typep (gtk:shortcut-action shortcut) 'gtk:nothing-action))
    (is (cffi:null-pointer-p (gtk:shortcut-arguments shortcut)))
    (is (typep (gtk:shortcut-trigger shortcut) 'gtk:never-trigger))))

(test gtk-shortcut-properties.2
  (let ((shortcut (make-instance 'gtk:shortcut)))
    (is (= 999 (g:variant-int16 (setf (gtk:shortcut-arguments shortcut)
                                      (g:variant-new-int16 999)))))
    (is (= 999 (g:variant-int16 (gtk:shortcut-arguments shortcut))))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_shortcut_new

;; TODO: This test causes a fatal error when executed several times.

;; Gtk:ERROR:../../../gtk/gtkshortcuttrigger.c:416:gtk_never_trigger_finalize:
;; code should not be reached
;; Bail out! Gtk:ERROR:../../../gtk/gtkshortcuttrigger.c:
;; 416:gtk_never_trigger_finalize: code should not be reached
;; fatal error encountered in SBCL pid 13449 tid 13449:
;; SIGABRT received.

#+nil
(test gtk-shortcut-new
  (let ((trigger (make-instance 'gtk:never-trigger))
        (action (make-instance 'gtk:nothing-action))
        (shortcut nil))
  (is (typep (setf shortcut
                   (gtk:shortcut-new trigger action)) 'gtk:shortcut))
  (is (eq trigger (gtk:shortcut-trigger shortcut)))
  (is (eq action (gtk:shortcut-action shortcut)))))

;;;     gtk_shortcut_new_with_arguments

;;; 2024-9-20
