(in-package :gtk-test)

(def-suite gtk-shortcut-action :in gtk-suite)
(in-suite gtk-shortcut-action)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkShortcutActionFlags

(test gtk-shortcut-action-flags
  ;; Check the type
  (is (g:type-is-flags "GtkShortcutActionFlags"))
  ;; Check the registered name
  (is (eq 'gtk:shortcut-action-flags
          (glib:symbol-for-gtype "GtkShortcutActionFlags")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkShortcutActionFlags")
          (g:gtype (cffi:foreign-funcall "gtk_shortcut_action_flags_get_type"
                                         :size))))
  ;; Check the names
  (is (equal '("GTK_SHORTCUT_ACTION_EXCLUSIVE")
             (list-flags-item-name "GtkShortcutActionFlags")))
  ;; Check the values
  (is (equal '(1)
             (list-flags-item-value "GtkShortcutActionFlags")))
  ;; Check the nick names
  (is (equal '("exclusive")
             (list-flags-item-nick "GtkShortcutActionFlags")))
  ;; Check the flags definition
  (is (equal '(GOBJECT:DEFINE-G-FLAGS "GtkShortcutActionFlags"
                                      GTK-SHORTCUT-ACTION-FLAGS
                              (:EXPORT T
                               :TYPE-INITIALIZER
                               "gtk_shortcut_action_flags_get_type")
                              (:EXCLUSIVE 1))
             (gobject:get-g-type-definition "GtkShortcutActionFlags"))))

;;;     GtkShortcutAction

(test gtk-shortcut-action-class
  ;; Type check
  (is (g:type-is-object "GtkShortcutAction"))
  ;; Check the registered name
  (is (eq 'gtk:shortcut-action
          (glib:symbol-for-gtype "GtkShortcutAction")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkShortcutAction")
          (g:gtype (cffi:foreign-funcall "gtk_shortcut_action_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkShortcutAction")))
  ;; Check the children
  (is (equal '("GtkActivateAction" "GtkCallbackAction" "GtkMnemonicAction"
               "GtkNamedAction" "GtkNothingAction" "GtkSignalAction")
             (list-children "GtkShortcutAction")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkShortcutAction")))
  ;; Check the class properties
  (is (equal '()
             (list-properties "GtkShortcutAction")))
  ;; Check the list of signals
  (is (equal '()
             (list-signals "GtkShortcutAction")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkShortcutAction"
                                             GTK-SHORTCUT-ACTION
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_shortcut_action_get_type")
                       NIL)
             (gobject:get-g-type-definition "GtkShortcutAction"))))

;;;     GtkNothingAction

(test gtk-nothing-action-class
  ;; Type check
  (is (g:type-is-object "GtkNothingAction"))
  ;; Check the registered name
  (is (eq 'gtk:nothing-action
          (glib:symbol-for-gtype "GtkNothingAction")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkNothingAction")
          (g:gtype (cffi:foreign-funcall "gtk_nothing_action_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkShortcutAction")
          (g:type-parent "GtkNothingAction")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkNothingAction")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkNothingAction")))
  ;; Check the class properties
  (is (equal '()
             (list-properties "GtkNothingAction")))
  ;; Check the list of signals
  (is (equal '()
             (list-signals "GtkNothingAction")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkNothingAction"
                                             GTK-NOTHING-ACTION
                       (:SUPERCLASS GTK-SHORTCUT-ACTION :EXPORT T :INTERFACES
                        NIL :TYPE-INITIALIZER "gtk_nothing_action_get_type")
                       NIL)
             (gobject:get-g-type-definition "GtkNothingAction"))))

;;;     GtkCallbackAction

(test gtk-callback-action-class
  ;; Type check
  (is (g:type-is-object "GtkCallbackAction"))
  ;; Check the registered name
  (is (eq 'gtk:callback-action
          (glib:symbol-for-gtype "GtkCallbackAction")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkCallbackAction")
          (g:gtype (cffi:foreign-funcall "gtk_callback_action_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkShortcutAction")
          (g:type-parent "GtkCallbackAction")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkCallbackAction")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkCallbackAction")))
  ;; Check the class properties
  (is (equal '()
             (list-properties "GtkCallbackAction")))
  ;; Check the list of signals
  (is (equal '()
             (list-signals "GtkCallbackAction")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkCallbackAction"
                                             GTK-CALLBACK-ACTION
                       (:SUPERCLASS GTK-SHORTCUT-ACTION :EXPORT T :INTERFACES
                        NIL :TYPE-INITIALIZER "gtk_callback_action_get_type")
                       NIL)
             (gobject:get-g-type-definition "GtkCallbackAction"))))

;;;     GtkMnemonicAction

(test gtk-mnemonic-action-class
  ;; Type check
  (is (g:type-is-object "GtkMnemonicAction"))
  ;; Check the registered name
  (is (eq 'gtk:mnemonic-action
          (glib:symbol-for-gtype "GtkMnemonicAction")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkMnemonicAction")
          (g:gtype (cffi:foreign-funcall "gtk_mnemonic_action_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkShortcutAction")
          (g:type-parent "GtkMnemonicAction")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkMnemonicAction")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkMnemonicAction")))
  ;; Check the class properties
  (is (equal '()
             (list-properties "GtkMnemonicAction")))
  ;; Check the list of signals
  (is (equal '()
             (list-signals "GtkMnemonicAction")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkMnemonicAction"
                                             GTK-MNEMONIC-ACTION
                       (:SUPERCLASS GTK-SHORTCUT-ACTION :EXPORT T :INTERFACES
                        NIL :TYPE-INITIALIZER "gtk_mnemonic_action_get_type")
                       NIL)
             (gobject:get-g-type-definition "GtkMnemonicAction"))))

;;;     GtkActivateAction

(test gtk-activate-action-class
  ;; Type check
  (is (g:type-is-object "GtkActivateAction"))
  ;; Check the registered name
  (is (eq 'gtk:activate-action
          (glib:symbol-for-gtype "GtkActivateAction")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkActivateAction")
          (g:gtype (cffi:foreign-funcall "gtk_activate_action_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkShortcutAction")
          (g:type-parent "GtkActivateAction")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkActivateAction")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkActivateAction")))
  ;; Check the class properties
  (is (equal '()
             (list-properties "GtkActivateAction")))
  ;; Check the list of signals
  (is (equal '()
             (list-signals "GtkActivateAction")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkActivateAction"
                                             GTK-ACTIVATE-ACTION
                       (:SUPERCLASS GTK-SHORTCUT-ACTION :EXPORT T :INTERFACES
                        NIL :TYPE-INITIALIZER "gtk_activate_action_get_type")
                       NIL)
             (gobject:get-g-type-definition "GtkActivateAction"))))

;;;     GtkSignalAction

(test gtk-signal-action-class
  ;; Type check
  (is (g:type-is-object "GtkSignalAction"))
  ;; Check the registered name
  (is (eq 'gtk:signal-action
          (glib:symbol-for-gtype "GtkSignalAction")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkSignalAction")
          (g:gtype (cffi:foreign-funcall "gtk_signal_action_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkShortcutAction")
          (g:type-parent "GtkSignalAction")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkSignalAction")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkSignalAction")))
  ;; Check the class properties
  (is (equal '("signal-name")
             (list-properties "GtkSignalAction")))
  ;; Check the list of signals
  (is (equal '()
             (list-signals "GtkSignalAction")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkSignalAction" GTK-SIGNAL-ACTION
                       (:SUPERCLASS GTK-SHORTCUT-ACTION :EXPORT T :INTERFACES
                        NIL :TYPE-INITIALIZER "gtk_signal_action_get_type")
                       ((SIGNAL-NAME GTK-SIGNAL-ACTION-SIGNAL-NAME
                         "signal-name" "gchararray" T NIL)))
             (gobject:get-g-type-definition "GtkSignalAction"))))

;;;     gtk-signal-action-signal-name

(test gtk-signal-action-signal-name
  (let ((action (gtk:signal-action-new "name")))
    (is (string= "name" (gtk:signal-action-signal-name action)))))

;;;     GtkNamedAction

(test gtk-named-action-class
  ;; Type check
  (is (g:type-is-object "GtkNamedAction"))
  ;; Check the registered name
  (is (eq 'gtk:named-action
          (glib:symbol-for-gtype "GtkNamedAction")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkNamedAction")
          (g:gtype (cffi:foreign-funcall "gtk_named_action_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkShortcutAction")
          (g:type-parent "GtkNamedAction")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkNamedAction")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkNamedAction")))
  ;; Check the class properties
  (is (equal '("action-name")
             (list-properties "GtkNamedAction")))
  ;; Check the list of signals
  (is (equal '()
             (list-signals "GtkNamedAction")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkNamedAction" GTK-NAMED-ACTION
                       (:SUPERCLASS GTK-SHORTCUT-ACTION :EXPORT T :INTERFACES
                        NIL :TYPE-INITIALIZER "gtk_named_action_get_type")
                       ((ACTION-NAME GTK-NAMED-ACTION-ACTION-NAME "action-name"
                         "gchararray" T NIL)))
             (gobject:get-g-type-definition "GtkNamedAction"))))

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

;;; --- 2023-7-23 --------------------------------------------------------------
