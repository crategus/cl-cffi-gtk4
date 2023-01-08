(def-suite gtk-shortcut-action :in gtk-suite)
(in-suite gtk-shortcut-action)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkShortcutActionFlags

(test gtk-shortcut-action-flags
  ;; Check the type
  (is (g-type-is-flags "GtkShortcutActionFlags"))
  ;; Check the registered name
  (is (eq 'gtk-shortcut-action-flags
          (gobject:symbol-for-gtype "GtkShortcutActionFlags")))
  ;; Check the type initializer
  (is (eq (gtype "GtkShortcutActionFlags")
          (gtype (foreign-funcall "gtk_shortcut_action_flags_get_type" g-size))))
  ;; Check the names
  (is (equal '("GTK_SHORTCUT_ACTION_EXCLUSIVE")
             (mapcar #'flags-item-name
                     (get-flags-items "GtkShortcutActionFlags"))))
  ;; Check the values
  (is (equal '(1)
             (mapcar #'flags-item-value
                     (get-flags-items "GtkShortcutActionFlags"))))
  ;; Check the nick names
  (is (equal '("exclusive")
             (mapcar #'flags-item-nick
                     (get-flags-items "GtkShortcutActionFlags"))))
  ;; Check the flags definition
  (is (equal '(DEFINE-G-FLAGS "GtkShortcutActionFlags"
                              GTK-SHORTCUT-ACTION-FLAGS
                              (:EXPORT T
                               :TYPE-INITIALIZER
                               "gtk_shortcut_action_flags_get_type")
                              (:EXCLUSIVE 1))
             (get-g-type-definition "GtkShortcutActionFlags"))))

;;;     GtkShortcutAction

(test gtk-shortcut-action-class
  ;; Type check
  (is (g:type-is-object "GtkShortcutAction"))
  ;; Check the registered name
  (is (eq 'gtk-shortcut-action
          (gobject:symbol-for-gtype "GtkShortcutAction")))
  ;; Check the type initializer
  (is (eq (gtype "GtkShortcutAction")
          (gtype (foreign-funcall "gtk_shortcut_action_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GObject")
          (g-type-parent "GtkShortcutAction")))
  ;; Check the children
  (is (equal '("GtkNothingAction" "GtkCallbackAction" "GtkMnemonicAction"
               "GtkActivateAction" "GtkSignalAction" "GtkNamedAction")
             (mapcar #'g-type-name (g-type-children "GtkShortcutAction"))))
  ;; Check the interfaces
  (is (equal '()
             (mapcar #'g-type-name (g-type-interfaces "GtkShortcutAction"))))
  ;; Check the class properties
  (is (equal '()
             (list-class-property-names "GtkShortcutAction")))
  ;; Check the list of signals
  (is (equal '()
             (sort (mapcar #'g-signal-name
                           (g-signal-list-ids "GtkShortcutAction"))
                   #'string<)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkShortcutAction" GTK-SHORTCUT-ACTION
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_shortcut_action_get_type")
                       NIL)
             (get-g-type-definition "GtkShortcutAction"))))

;;;     GtkNothingAction

(test gtk-nothing-action-class
  ;; Type check
  (is (g:type-is-object "GtkNothingAction"))
  ;; Check the registered name
  (is (eq 'gtk-nothing-action
          (gobject:symbol-for-gtype "GtkNothingAction")))
  ;; Check the type initializer
  (is (eq (gtype "GtkNothingAction")
          (gtype (foreign-funcall "gtk_nothing_action_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GtkShortcutAction")
          (g-type-parent "GtkNothingAction")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "GtkNothingAction"))))
  ;; Check the interfaces
  (is (equal '()
             (mapcar #'g-type-name (g-type-interfaces "GtkNothingAction"))))
  ;; Check the class properties
  (is (equal '()
             (list-class-property-names "GtkNothingAction")))
  ;; Check the list of signals
  (is (equal '()
             (sort (mapcar #'g-signal-name
                           (g-signal-list-ids "GtkNothingAction"))
                   #'string<)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkNothingAction" GTK-NOTHING-ACTION
                       (:SUPERCLASS GTK-SHORTCUT-ACTION :EXPORT T :INTERFACES
                        NIL :TYPE-INITIALIZER "gtk_nothing_action_get_type")
                       NIL)
             (get-g-type-definition "GtkNothingAction"))))

;;;     GtkCallbackAction

(test gtk-callback-action-class
  ;; Type check
  (is (g:type-is-object "GtkCallbackAction"))
  ;; Check the registered name
  (is (eq 'gtk-callback-action
          (gobject:symbol-for-gtype "GtkCallbackAction")))
  ;; Check the type initializer
  (is (eq (gtype "GtkCallbackAction")
          (gtype (foreign-funcall "gtk_callback_action_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GtkShortcutAction")
          (g-type-parent "GtkCallbackAction")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "GtkCallbackAction"))))
  ;; Check the interfaces
  (is (equal '()
             (mapcar #'g-type-name (g-type-interfaces "GtkCallbackAction"))))
  ;; Check the class properties
  (is (equal '()
             (list-class-property-names "GtkCallbackAction")))
  ;; Check the list of signals
  (is (equal '()
             (sort (mapcar #'g-signal-name
                           (g-signal-list-ids "GtkCallbackAction"))
                   #'string<)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkCallbackAction" GTK-CALLBACK-ACTION
                       (:SUPERCLASS GTK-SHORTCUT-ACTION :EXPORT T :INTERFACES
                        NIL :TYPE-INITIALIZER "gtk_callback_action_get_type")
                       NIL)
             (get-g-type-definition "GtkCallbackAction"))))

;;;     GtkMnemonicAction

(test gtk-mnemonic-action-class
  ;; Type check
  (is (g:type-is-object "GtkMnemonicAction"))
  ;; Check the registered name
  (is (eq 'gtk-mnemonic-action
          (gobject:symbol-for-gtype "GtkMnemonicAction")))
  ;; Check the type initializer
  (is (eq (gtype "GtkMnemonicAction")
          (gtype (foreign-funcall "gtk_mnemonic_action_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GtkShortcutAction")
          (g-type-parent "GtkMnemonicAction")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "GtkMnemonicAction"))))
  ;; Check the interfaces
  (is (equal '()
             (mapcar #'g-type-name (g-type-interfaces "GtkMnemonicAction"))))
  ;; Check the class properties
  (is (equal '()
             (list-class-property-names "GtkMnemonicAction")))
  ;; Check the list of signals
  (is (equal '()
             (sort (mapcar #'g-signal-name
                           (g-signal-list-ids "GtkMnemonicAction"))
                   #'string<)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkMnemonicAction" GTK-MNEMONIC-ACTION
                       (:SUPERCLASS GTK-SHORTCUT-ACTION :EXPORT T :INTERFACES
                        NIL :TYPE-INITIALIZER "gtk_mnemonic_action_get_type")
                       NIL)
             (get-g-type-definition "GtkMnemonicAction"))))

;;;     GtkActivateAction

(test gtk-activate-action-class
  ;; Type check
  (is (g:type-is-object "GtkActivateAction"))
  ;; Check the registered name
  (is (eq 'gtk-activate-action
          (gobject:symbol-for-gtype "GtkActivateAction")))
  ;; Check the type initializer
  (is (eq (gtype "GtkActivateAction")
          (gtype (foreign-funcall "gtk_activate_action_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GtkShortcutAction")
          (g-type-parent "GtkActivateAction")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "GtkActivateAction"))))
  ;; Check the interfaces
  (is (equal '()
             (mapcar #'g-type-name (g-type-interfaces "GtkActivateAction"))))
  ;; Check the class properties
  (is (equal '()
             (list-class-property-names "GtkActivateAction")))
  ;; Check the list of signals
  (is (equal '()
             (sort (mapcar #'g-signal-name
                           (g-signal-list-ids "GtkActivateAction"))
                   #'string<)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkActivateAction" GTK-ACTIVATE-ACTION
                       (:SUPERCLASS GTK-SHORTCUT-ACTION :EXPORT T :INTERFACES
                        NIL :TYPE-INITIALIZER "gtk_activate_action_get_type")
                       NIL)
             (get-g-type-definition "GtkActivateAction"))))

;;;     GtkSignalAction

(test gtk-signal-action-class
  ;; Type check
  (is (g:type-is-object "GtkSignalAction"))
  ;; Check the registered name
  (is (eq 'gtk-signal-action
          (gobject:symbol-for-gtype "GtkSignalAction")))
  ;; Check the type initializer
  (is (eq (gtype "GtkSignalAction")
          (gtype (foreign-funcall "gtk_signal_action_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GtkShortcutAction")
          (g-type-parent "GtkSignalAction")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "GtkSignalAction"))))
  ;; Check the interfaces
  (is (equal '()
             (mapcar #'g-type-name (g-type-interfaces "GtkSignalAction"))))
  ;; Check the class properties
  (is (equal '("signal-name")
             (list-class-property-names "GtkSignalAction")))
  ;; Check the list of signals
  (is (equal '()
             (sort (mapcar #'g-signal-name
                           (g-signal-list-ids "GtkSignalAction"))
                   #'string<)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkSignalAction" GTK-SIGNAL-ACTION
                       (:SUPERCLASS GTK-SHORTCUT-ACTION :EXPORT T :INTERFACES
                        NIL :TYPE-INITIALIZER "gtk_signal_action_get_type")
                       ((SIGNAL-NAME GTK-SIGNAL-ACTION-SIGNAL-NAME
                         "signal-name" "gchararray" T NIL)))
             (get-g-type-definition "GtkSignalAction"))))

;;;     gtk-signal-action-signal-name

(test gtk-signal-action-signal-name
  (let ((action (gtk-signal-action-new "name")))
    (is (string= "name" (gtk-signal-action-signal-name action)))))

;;;     GtkNamedAction

(test gtk-named-action-class
  ;; Type check
  (is (g:type-is-object "GtkNamedAction"))
  ;; Check the registered name
  (is (eq 'gtk-named-action
          (gobject:symbol-for-gtype "GtkNamedAction")))
  ;; Check the type initializer
  (is (eq (gtype "GtkNamedAction")
          (gtype (foreign-funcall "gtk_named_action_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GtkShortcutAction")
          (g-type-parent "GtkNamedAction")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "GtkNamedAction"))))
  ;; Check the interfaces
  (is (equal '()
             (mapcar #'g-type-name (g-type-interfaces "GtkNamedAction"))))
  ;; Check the class properties
  (is (equal '("action-name")
             (list-class-property-names "GtkNamedAction")))
  ;; Check the list of signals
  (is (equal '()
             (sort (mapcar #'g-signal-name
                           (g-signal-list-ids "GtkNamedAction"))
                   #'string<)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkNamedAction" GTK-NAMED-ACTION
                       (:SUPERCLASS GTK-SHORTCUT-ACTION :EXPORT T :INTERFACES
                        NIL :TYPE-INITIALIZER "gtk_named_action_get_type")
                       ((ACTION-NAME GTK-NAMED-ACTION-ACTION-NAME "action-name"
                         "gchararray" T NIL)))
             (get-g-type-definition "GtkNamedAction"))))

;;;     gtk-named-action-action-name

(test gtk-named-action-action-name
  (let ((action (gtk-named-action-new "name")))
    (is (string= "name" (gtk-named-action-action-name action)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_shortcut_action_to_string
;;;     gtk_shortcut_action_print

(test gtk-shortcut-action-to-string
  (is (string= "signal(signal)"
               (gtk-shortcut-action-to-string (gtk-signal-action-new "signal"))))
  (is (string= "action(named)"
               (gtk-shortcut-action-to-string (gtk-named-action-new "named")))))

;;;     gtk_shortcut_action_parse_string

(test gtk-shortcut-action-parse-string
  (is (typep (gtk-shortcut-action-parse-string "nothing")
             'gtk-nothing-action))
  (is (typep (gtk-shortcut-action-parse-string "activate")
             'gtk-activate-action))
  (is (typep (gtk-shortcut-action-parse-string "mnemonic-activate")
             'gtk-mnemonic-action))
  (is (typep (gtk-shortcut-action-parse-string "action(name)")
             'gtk-named-action))
  (is (typep (gtk-shortcut-action-parse-string "signal(name)")
             'gtk-signal-action)))

;;;     gtk_shortcut_action_activate

;;;     gtk_nothing_action_get

(test gtk-nothing-action-get
  (is (typep (gtk-nothing-action-get) 'gtk-nothing-action)))

;;;     GtkShortcutFunc
;;;     gtk_callback_action_new

(test gtk-callback-action-new
  (is (typep (gtk-callback-action-new #'(lambda () (+ 1 1)))
             'gtk-callback-action)))

;;;     gtk_mnemonic_action_get

(test gtk-mnemonic-action-get
  (is (typep (gtk-mnemonic-action-get) 'gtk-mnemonic-action)))

;;;     gtk_activate_action_get

(test gtk-activate-action-get
  (is (typep (gtk-activate-action-get) 'gtk-activate-action)))

;;;     gtk_signal_action_new

(test gtk-signal-action-new
  (is (typep (gtk-signal-action-new "signal") 'gtk-signal-action)))

;;;     gtk_named_action_new

(test gtk-named-action-new
  (is (typep (gtk-named-action-new "named") 'gtk-named-action)))

;;; 2022-8-26
