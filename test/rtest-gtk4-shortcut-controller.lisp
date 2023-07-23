(in-package :gtk-test)

(def-suite gtk-shortcut-controller :in gtk-suite)
(in-suite gtk-shortcut-controller)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkShortcutScope                         -> gtk.enumerations.lisp

;;;     GtkShortcutController

(test gtk-shortcut-controller-class
  ;; Type check
  (is (g:type-is-object "GtkShortcutController"))
  ;; Check the registered name
  (is (eq 'gtk:shortcut-controller
          (glib:symbol-for-gtype "GtkShortcutController")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkShortcutController")
          (g:gtype (cffi:foreign-funcall "gtk_shortcut_controller_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkEventController")
          (g:type-parent "GtkShortcutController")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkShortcutController")))
  ;; Check the interfaces
  (is (equal '("GListModel" "GtkBuildable")
             (list-interfaces "GtkShortcutController")))
  ;; Check the properties
  (is (equal '("item-type" "mnemonic-modifiers" "model" "n-items" "scope")
             (list-properties "GtkShortcutController")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkShortcutController")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkShortcutController"
                                             GTK-SHORTCUT-CONTROLLER
                       (:SUPERCLASS GTK-EVENT-CONTROLLER
                        :EXPORT T
                        :INTERFACES ("GListModel" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_shortcut_controller_get_type")
                       ((ITEM-TYPE GTK-SHORTCUT-CONTROLLER-ITEM-TYPE
                         "item-type" "GType" T NIL)
                        (MNEMONIC-MODIFIERS
                         GTK-SHORTCUT-CONTROLLER-MNEMONIC-MODIFIERS
                         "mnemonic-modifiers" "GdkModifierType" T T)
                        (MODEL GTK-SHORTCUT-CONTROLLER-MODEL "model"
                         "GListModel" NIL NIL)
                        (N-ITEMS GTK-SHORTCUT-CONTROLLER-N-ITEMS "n-items"
                         "guint" T NIL)
                        (SCOPE GTK-SHORTCUT-CONTROLLER-SCOPE "scope"
                         "GtkShortcutScope" T T)))
             (gobject:get-g-type-definition "GtkShortcutController"))))

;;; --- Properties -------------------------------------------------------------

;;;     item-type
;;;     mnemonic-modifiers
;;;     model
;;;     n-items
;;;     scope

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

;;; --- 2023-7-23 --------------------------------------------------------------
