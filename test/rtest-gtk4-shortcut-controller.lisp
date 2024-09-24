(in-package :gtk-test)

(def-suite gtk-shortcut-controller :in gtk-suite)
(in-suite gtk-shortcut-controller)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkShortcutScope                         -> gtk.enumerations.lisp

;;;     GtkShortcutController

(test gtk-shortcut-controller-class
  ;; Check type
  (is (g:type-is-object "GtkShortcutController"))
  ;; Check registered name
  (is (eq 'gtk:shortcut-controller
          (glib:symbol-for-gtype "GtkShortcutController")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkShortcutController")
          (g:gtype (cffi:foreign-funcall "gtk_shortcut_controller_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkEventController")
          (g:type-parent "GtkShortcutController")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkShortcutController")))
  ;; Check interfaces
  (is (equal '("GListModel" "GtkBuildable")
             (glib-test:list-interfaces "GtkShortcutController")))
  ;; Check properties
  (is (equal '("item-type" "mnemonic-modifiers" "model" "n-items" "scope")
             (glib-test:list-properties "GtkShortcutController")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkShortcutController")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkShortcutController"
                                      GTK:SHORTCUT-CONTROLLER
                       (:SUPERCLASS GTK:EVENT-CONTROLLER
                        :EXPORT T
                        :INTERFACES ("GListModel" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_shortcut_controller_get_type")
                       ((ITEM-TYPE SHORTCUT-CONTROLLER-ITEM-TYPE
                         "item-type" "GType" T NIL)
                        (MNEMONIC-MODIFIERS
                         SHORTCUT-CONTROLLER-MNEMONIC-MODIFIERS
                         "mnemonic-modifiers" "GdkModifierType" T T)
                        (MODEL SHORTCUT-CONTROLLER-MODEL
                         "model" "GListModel" NIL NIL)
                        (N-ITEMS SHORTCUT-CONTROLLER-N-ITEMS
                         "n-items" "guint" T NIL)
                        (SCOPE SHORTCUT-CONTROLLER-SCOPE
                         "scope" "GtkShortcutScope" T T)))
             (gobject:get-gtype-definition "GtkShortcutController"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-shortcut-controller-properties
  (let ((controller (make-instance 'gtk:shortcut-controller)))
    ;; TODO: Returns a pointer? Why? Check this with a list model.
    (is (cffi:pointerp (gtk:shortcut-controller-item-type controller)))
    (is (equal '(:ALT-MASK)
               (gtk:shortcut-controller-mnemonic-modifiers controller)))
    ;; model is not readable
    (signals (error) (gtk:shortcut-controller-model controller))
    (is (= 0 (gtk:shortcut-controller-n-items controller)))
    (is (eq :local (gtk:shortcut-controller-scope controller)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_shortcut_controller_new

(test gtk-shortcut-controller-new
  (is (typep (gtk:shortcut-controller-new) 'gtk:shortcut-controller)))

;;;     gtk_shortcut_controller_new_for_model
;;;     gtk_shortcut_controller_add_shortcut
;;;     gtk_shortcut_controller_remove_shortcut

;;; 2024-9-20
