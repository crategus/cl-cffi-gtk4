(in-package :gtk-test)

(def-suite gtk-shortcut-action :in gtk-keyboard-shortcuts)
(in-suite gtk-shortcut-action)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkShortcutActionFlags

(test gtk-shortcut-action-flags
  ;; Check type
  (is (g:type-is-flags "GtkShortcutActionFlags"))
  ;; Check registered name
  (is (eq 'gtk:shortcut-action-flags
          (glib:symbol-for-gtype "GtkShortcutActionFlags")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkShortcutActionFlags")
          (g:gtype (cffi:foreign-funcall "gtk_shortcut_action_flags_get_type"
                                         :size))))
  ;; Check names
  (is (equal '("GTK_SHORTCUT_ACTION_EXCLUSIVE")
             (glib-test:list-flags-item-names "GtkShortcutActionFlags")))
  ;; Check values
  (is (equal '(1)
             (glib-test:list-flags-item-values "GtkShortcutActionFlags")))
  ;; Check nick names
  (is (equal '("exclusive")
             (glib-test:list-flags-item-nicks "GtkShortcutActionFlags")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-GFLAGS "GtkShortcutActionFlags"
                                      GTK:SHORTCUT-ACTION-FLAGS
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gtk_shortcut_action_flags_get_type")
                                     (:EXCLUSIVE 1))
             (gobject:get-gtype-definition "GtkShortcutActionFlags"))))

;;;     GtkShortcutAction

(test gtk-shortcut-action-class
  ;; Check type
  (is (g:type-is-object "GtkShortcutAction"))
  ;; Check registered name
  (is (eq 'gtk:shortcut-action
          (glib:symbol-for-gtype "GtkShortcutAction")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkShortcutAction")
          (g:gtype (cffi:foreign-funcall "gtk_shortcut_action_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkShortcutAction")))
  ;; Check children
  (is (equal '("GtkActivateAction" "GtkCallbackAction" "GtkMnemonicAction"
               "GtkNamedAction" "GtkNothingAction" "GtkSignalAction")
             (glib-test:list-children "GtkShortcutAction")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkShortcutAction")))
  ;; Check class properties
  (is (equal '()
             (glib-test:list-properties "GtkShortcutAction")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkShortcutAction")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkShortcutAction" GTK:SHORTCUT-ACTION
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_shortcut_action_get_type")
                       NIL)
             (gobject:get-gtype-definition "GtkShortcutAction"))))

;;;     GtkNothingAction

(test gtk-nothing-action-class
  ;; Check type
  (is (g:type-is-object "GtkNothingAction"))
  ;; Check registered name
  (is (eq 'gtk:nothing-action
          (glib:symbol-for-gtype "GtkNothingAction")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkNothingAction")
          (g:gtype (cffi:foreign-funcall "gtk_nothing_action_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkShortcutAction")
          (g:type-parent "GtkNothingAction")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkNothingAction")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkNothingAction")))
  ;; Check class properties
  (is (equal '()
             (glib-test:list-properties "GtkNothingAction")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkNothingAction")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkNothingAction" GTK:NOTHING-ACTION
                       (:SUPERCLASS GTK:SHORTCUT-ACTION
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_nothing_action_get_type")
                       NIL)
             (gobject:get-gtype-definition "GtkNothingAction"))))

;;;     GtkCallbackAction

(test gtk-callback-action-class
  ;; Check type
  (is (g:type-is-object "GtkCallbackAction"))
  ;; Check registered name
  (is (eq 'gtk:callback-action
          (glib:symbol-for-gtype "GtkCallbackAction")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkCallbackAction")
          (g:gtype (cffi:foreign-funcall "gtk_callback_action_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkShortcutAction")
          (g:type-parent "GtkCallbackAction")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkCallbackAction")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkCallbackAction")))
  ;; Check class properties
  (is (equal '()
             (glib-test:list-properties "GtkCallbackAction")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkCallbackAction")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkCallbackAction" GTK:CALLBACK-ACTION
                       (:SUPERCLASS GTK:SHORTCUT-ACTION
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_callback_action_get_type")
                       NIL)
             (gobject:get-gtype-definition "GtkCallbackAction"))))

;;;     GtkMnemonicAction

(test gtk-mnemonic-action-class
  ;; Check type
  (is (g:type-is-object "GtkMnemonicAction"))
  ;; Check registered name
  (is (eq 'gtk:mnemonic-action
          (glib:symbol-for-gtype "GtkMnemonicAction")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkMnemonicAction")
          (g:gtype (cffi:foreign-funcall "gtk_mnemonic_action_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkShortcutAction")
          (g:type-parent "GtkMnemonicAction")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkMnemonicAction")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkMnemonicAction")))
  ;; Check class properties
  (is (equal '()
             (glib-test:list-properties "GtkMnemonicAction")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkMnemonicAction")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkMnemonicAction" GTK:MNEMONIC-ACTION
                       (:SUPERCLASS GTK:SHORTCUT-ACTION
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_mnemonic_action_get_type")
                       NIL)
             (gobject:get-gtype-definition "GtkMnemonicAction"))))

;;;     GtkActivateAction

(test gtk-activate-action-class
  ;; Check type
  (is (g:type-is-object "GtkActivateAction"))
  ;; Check registered name
  (is (eq 'gtk:activate-action
          (glib:symbol-for-gtype "GtkActivateAction")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkActivateAction")
          (g:gtype (cffi:foreign-funcall "gtk_activate_action_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkShortcutAction")
          (g:type-parent "GtkActivateAction")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkActivateAction")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkActivateAction")))
  ;; Check class properties
  (is (equal '()
             (glib-test:list-properties "GtkActivateAction")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkActivateAction")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkActivateAction" GTK:ACTIVATE-ACTION
                       (:SUPERCLASS GTK:SHORTCUT-ACTION
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_activate_action_get_type")
                       NIL)
             (gobject:get-gtype-definition "GtkActivateAction"))))

;;;     GtkSignalAction

(test gtk-signal-action-class
  ;; Check type
  (is (g:type-is-object "GtkSignalAction"))
  ;; Check registered name
  (is (eq 'gtk:signal-action
          (glib:symbol-for-gtype "GtkSignalAction")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkSignalAction")
          (g:gtype (cffi:foreign-funcall "gtk_signal_action_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkShortcutAction")
          (g:type-parent "GtkSignalAction")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkSignalAction")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkSignalAction")))
  ;; Check class properties
  (is (equal '("signal-name")
             (glib-test:list-properties "GtkSignalAction")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkSignalAction")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkSignalAction" GTK:SIGNAL-ACTION
                       (:SUPERCLASS GTK:SHORTCUT-ACTION
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_signal_action_get_type")
                       ((SIGNAL-NAME SIGNAL-ACTION-SIGNAL-NAME
                         "signal-name" "gchararray" T NIL)))
             (gobject:get-gtype-definition "GtkSignalAction"))))

;;;     gtk-signal-action-signal-name

(test gtk-signal-action-signal-name
  (let ((action (gtk:signal-action-new "name")))
    (is (string= "name" (gtk:signal-action-signal-name action)))))

;;;     GtkNamedAction

(test gtk-named-action-class
  ;; Check type
  (is (g:type-is-object "GtkNamedAction"))
  ;; Check registered name
  (is (eq 'gtk:named-action
          (glib:symbol-for-gtype "GtkNamedAction")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkNamedAction")
          (g:gtype (cffi:foreign-funcall "gtk_named_action_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkShortcutAction")
          (g:type-parent "GtkNamedAction")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkNamedAction")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkNamedAction")))
  ;; Check class properties
  (is (equal '("action-name")
             (glib-test:list-properties "GtkNamedAction")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkNamedAction")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkNamedAction" GTK:NAMED-ACTION
                       (:SUPERCLASS GTK:SHORTCUT-ACTION
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_named_action_get_type")
                       ((ACTION-NAME NAMED-ACTION-ACTION-NAME
                         "action-name" "gchararray" T NIL)))
             (gobject:get-gtype-definition "GtkNamedAction"))))

;;;     gtk-named-action-action-name

(test gtk-named-action-action-name
  (let ((action (gtk:named-action-new "name")))
    (is (string= "name" (gtk:named-action-action-name action)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_shortcut_action_to_string
;;;     gtk_shortcut_action_print

(test gtk-shortcut-action-to-string
  (is (string= "signal(signal)"
               (gtk:shortcut-action-to-string (gtk:signal-action-new "signal"))))
  (is (string= "action(named)"
               (gtk:shortcut-action-to-string (gtk:named-action-new "named")))))

;;;     gtk_shortcut_action_parse_string

(test gtk-shortcut-action-parse-string
  (is (typep (gtk:shortcut-action-parse-string "nothing")
             'gtk:nothing-action))
  (is (typep (gtk:shortcut-action-parse-string "activate")
             'gtk:activate-action))
  (is (typep (gtk:shortcut-action-parse-string "mnemonic-activate")
             'gtk:mnemonic-action))
  (is (typep (gtk:shortcut-action-parse-string "action(name)")
             'gtk:named-action))
  (is (typep (gtk:shortcut-action-parse-string "signal(name)")
             'gtk:signal-action)))

;;;     gtk_shortcut_action_activate

;;;     gtk_nothing_action_get

(test gtk-nothing-action-get
  (is (typep (gtk:nothing-action-get) 'gtk:nothing-action)))

;;;     GtkShortcutFunc
;;;     gtk_callback_action_new

(test gtk-callback-action-new
  (is (typep (gtk:callback-action-new #'(lambda () (+ 1 1)))
             'gtk:callback-action)))

;;;     gtk_mnemonic_action_get

(test gtk-mnemonic-action-get
  (is (typep (gtk:mnemonic-action-get) 'gtk:mnemonic-action)))

;;;     gtk_activate_action_get

(test gtk-activate-action-get
  (is (typep (gtk:activate-action-get) 'gtk:activate-action)))

;;;     gtk_signal_action_new

(test gtk-signal-action-new
  (is (typep (gtk:signal-action-new "signal") 'gtk:signal-action)))

;;;     gtk_named_action_new

(test gtk-named-action-new
  (is (typep (gtk:named-action-new "named") 'gtk:named-action)))

;;; 2024-11-1
