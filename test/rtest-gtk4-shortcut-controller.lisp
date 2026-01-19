(in-package :gtk-test)

(def-suite gtk-shortcut-controller :in gtk-event-handling)
(in-suite gtk-shortcut-controller)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkShortcutScope

(test gtk-shortcut-scope
  ;; Check type
  (is (g:type-is-enum "GtkShortcutScope"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkShortcutScope")
          (g:gtype (cffi:foreign-funcall "gtk_shortcut_scope_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:shortcut-scope
          (glib:symbol-for-gtype "GtkShortcutScope")))
  ;; Check names
  (is (equal '("GTK_SHORTCUT_SCOPE_LOCAL" "GTK_SHORTCUT_SCOPE_MANAGED"
               "GTK_SHORTCUT_SCOPE_GLOBAL")
             (glib-test:list-enum-item-names "GtkShortcutScope")))
  ;; Check values
  (is (equal '(0 1 2)
             (glib-test:list-enum-item-values "GtkShortcutScope")))
  ;; Check nick names
  (is (equal '("local" "managed" "global")
             (glib-test:list-enum-item-nicks "GtkShortcutScope")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkShortcutScope" GTK:SHORTCUT-SCOPE
                                    (:EXPORT T
                                     :TYPE-INITIALIZER
                                     "gtk_shortcut_scope_get_type")
                                    (:LOCAL 0)
                                    (:MANAGED 1)
                                    (:GLOBAL 2))
             (gobject:get-gtype-definition "GtkShortcutScope"))))

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
    (is (eq (g:gtype "GObject")
            (gtk:shortcut-controller-item-type controller)))
    (is (equal '(:ALT-MASK)
               (gtk:shortcut-controller-mnemonic-modifiers controller)))
    ;; model is not readable and writable and not exported
    (signals (error) (gtk::shortcut-controller-model controller))
    (is (= 0 (gtk:shortcut-controller-n-items controller)))
    (is (eq :local (gtk:shortcut-controller-scope controller)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_shortcut_controller_new

(test gtk-shortcut-controller-new
  (glib-test:with-check-memory (controller)
    (is (typep (setf controller
                     (gtk:shortcut-controller-new)) 'gtk:shortcut-controller))))

;;;     gtk_shortcut_controller_new_for_model

(test gtk-shortcut-controller-new-for-model
  (when *first-run-testsuite*
    (glib-test:with-check-memory ((model 2) controller :strong 5)
      (is (typep (setf model
                       (g:list-store-new "GtkShortcut")) 'g:list-model))
      (is (typep (setf controller
                       (gtk:shortcut-controller-new-for-model model))
                 'gtk:shortcut-controller))
      (is (eq (g:gtype "GObject")
              (gtk:shortcut-controller-item-type controller)))
      (is (= 0 (gtk:shortcut-controller-n-items controller)))

      (is-false (g:list-store-append model
                    (gtk:shortcut-new (gtk:never-trigger-get)
                                      (gtk:nothing-action-get))))
      (is-false (g:list-store-append model
                    (gtk:shortcut-new (gtk:never-trigger-get)
                                      (gtk:nothing-action-get))))
      (is (eq (g:gtype "GObject")
              (gtk:shortcut-controller-item-type controller)))
      (is (= 2 (gtk:shortcut-controller-n-items controller))))))

;;;     gtk_shortcut_controller_add_shortcut
;;;     gtk_shortcut_controller_remove_shortcut

(test gtk-shortcut-controller-add/remove-shortcut
  (glib-test:with-check-memory (controller shortcut)
    (is (typep (setf controller
                     (gtk:shortcut-controller-new)) 'gtk:shortcut-controller))
    (is-false (gtk:shortcut-controller-add-shortcut
                  controller
                  (setf shortcut
                        (gtk:shortcut-new (gtk:never-trigger-get)
                                          (gtk:nothing-action-get)))))
    (is (eq (g:gtype "GObject")
            (gtk:shortcut-controller-item-type controller)))
    (is (= 1 (gtk:shortcut-controller-n-items controller)))
    (is-false (gtk:shortcut-controller-remove-shortcut controller shortcut))
    (is (= 0 (gtk:shortcut-controller-n-items controller)))))

;;; 2026-01-11
