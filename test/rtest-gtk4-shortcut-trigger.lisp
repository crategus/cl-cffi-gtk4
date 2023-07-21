(in-package :gtk-testsuite)

(def-suite gtk-shortcut-trigger :in gtk-suite)
(in-suite gtk-shortcut-trigger)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkShortcutTrigger

(test gtk-shortcut-trigger-class
  ;; Type check
  (is (g:type-is-object "GtkShortcutTrigger"))
  ;; Check the registered name
  (is (eq 'gtk:shortcut-trigger
          (glib:symbol-for-gtype "GtkShortcutTrigger")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkShortcutTrigger")
          (g:gtype (cffi:foreign-funcall "gtk_shortcut_trigger_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkShortcutTrigger")))
  ;; Check the children
  (is (equal '("GtkKeyvalTrigger" "GtkMnemonicTrigger" "GtkAlternativeTrigger"
               "GtkNeverTrigger")
             (list-children "GtkShortcutTrigger")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkShortcutTrigger")))
  ;; Check the class properties
  (is (equal '()
             (list-properties "GtkShortcutTrigger")))
  ;; Check the list of signals
  (is (equal '()
             (list-signals "GtkShortcutTrigger")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkShortcutTrigger" GTK-SHORTCUT-TRIGGER
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_shortcut_trigger_get_type")
                       NIL)
             (gobject:get-g-type-definition "GtkShortcutTrigger"))))

;;;     GtkKeyvalTrigger

(test gtk-keyval-trigger-class
  ;; Type check
  (is (g:type-is-object "GtkKeyvalTrigger"))
  ;; Check the registered name
  (is (eq 'gtk:keyval-trigger
          (glib:symbol-for-gtype "GtkKeyvalTrigger")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkKeyvalTrigger")
          (g:gtype (cffi:foreign-funcall "gtk_keyval_trigger_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkShortcutTrigger")
          (g:type-parent "GtkKeyvalTrigger")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkKeyvalTrigger")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkKeyvalTrigger")))
  ;; Check the class properties
  (is (equal '("keyval" "modifiers")
             (list-properties "GtkKeyvalTrigger")))
  ;; Check the list of signals
  (is (equal '()
             (list-signals "GtkKeyvalTrigger")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkKeyvalTrigger" GTK-KEYVAL-TRIGGER
                       (:SUPERCLASS GTK-SHORTCUT-TRIGGER :EXPORT T :INTERFACES
                        NIL :TYPE-INITIALIZER "gtk_keyval_trigger_get_type")
                       ((KEYVAL GTK-KEYVAL-TRIGGER-KEYVAL "keyval" "guint" T
                         NIL)
                        (MODIFIERS GTK-KEYVAL-TRIGGER-MODIFIERS "modifiers"
                         "GdkModifierType" T NIL)))
             (gobject:get-g-type-definition "GtkKeyvalTrigger"))))

;;;     GtkMnemonicTrigger

(test gtk-mnemonic-trigger-class
  ;; Type check
  (is (g:type-is-object "GtkMnemonicTrigger"))
  ;; Check the registered name
  (is (eq 'gtk:mnemonic-trigger
          (glib:symbol-for-gtype "GtkMnemonicTrigger")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkMnemonicTrigger")
          (g:gtype (cffi:foreign-funcall "gtk_mnemonic_trigger_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkShortcutTrigger")
          (g:type-parent "GtkMnemonicTrigger")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkMnemonicTrigger")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkMnemonicTrigger")))
  ;; Check the class properties
  (is (equal '("keyval")
             (list-properties "GtkMnemonicTrigger")))
  ;; Check the list of signals
  (is (equal '()
             (list-signals "GtkMnemonicTrigger")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkMnemonicTrigger" 
                                             GTK-MNEMONIC-TRIGGER
                       (:SUPERCLASS GTK-SHORTCUT-TRIGGER :EXPORT T :INTERFACES
                        NIL :TYPE-INITIALIZER "gtk_mnemonic_trigger_get_type")
                       ((KEYVAL GTK-MNEMONIC-TRIGGER-KEYVAL "keyval" "guint" T
                         NIL)))
             (gobject:get-g-type-definition "GtkMnemonicTrigger"))))

;;;     GtkAlternativeTrigger

(test gtk-alternative-trigger-class
  ;; Type check
  (is (g:type-is-object "GtkAlternativeTrigger"))
  ;; Check the registered name
  (is (eq 'gtk:alternative-trigger
          (glib:symbol-for-gtype "GtkAlternativeTrigger")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkAlternativeTrigger")
          (g:gtype (cffi:foreign-funcall "gtk_alternative_trigger_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (gtype "GtkShortcutTrigger")
          (g-type-parent "GtkAlternativeTrigger")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkAlternativeTrigger")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkAlternativeTrigger")))
  ;; Check the class properties
  (is (equal '("first" "second")
             (list-properties "GtkAlternativeTrigger")))
  ;; Check the list of signals
  (is (equal '()
             (list-signals "GtkAlternativeTrigger")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkAlternativeTrigger"
                                     GTK-ALTERNATIVE-TRIGGER
                       (:SUPERCLASS GTK-SHORTCUT-TRIGGER :EXPORT T :INTERFACES
                        NIL :TYPE-INITIALIZER
                        "gtk_alternative_trigger_get_type")
                       ((FIRST GTK-ALTERNATIVE-TRIGGER-FIRST "first"
                               "GtkShortcutTrigger" T NIL)
                        (SECOND GTK-ALTERNATIVE-TRIGGER-SECOND "second"
                                "GtkShortcutTrigger" T NIL)))
             (gobject:get-g-type-definition "GtkAlternativeTrigger"))))

;;;     GtkNeverTrigger

(test gtk-never-trigger-class
  ;; Type check
  (is (g:type-is-object "GtkNeverTrigger"))
  ;; Check the registered name
  (is (eq 'gtk:never-trigger
          (glib:symbol-for-gtype "GtkNeverTrigger")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkNeverTrigger")
          (g:gtype (cffi:foreign-funcall "gtk_never_trigger_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkShortcutTrigger")
          (g:type-parent "GtkNeverTrigger")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkNeverTrigger")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkNeverTrigger")))
  ;; Check the class properties
  (is (equal '()
             (list-properties "GtkNeverTrigger")))
  ;; Check the list of signals
  (is (equal '()
             (list-signals "GtkNeverTrigger")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkNeverTrigger" GTK-NEVER-TRIGGER
                       (:SUPERCLASS GTK-SHORTCUT-TRIGGER :EXPORT T :INTERFACES
                        NIL :TYPE-INITIALIZER "gtk_never_trigger_get_type")
                       NIL)
             (gobject:get-g-type-definition "GtkNeverTrigger"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_shortcut_trigger_trigger
;;;     gtk_shortcut_trigger_hash
;;;     gtk_shortcut_trigger_equal
;;;     gtk_shortcut_trigger_compare
;;;     gtk_shortcut_trigger_to_string
;;;     gtk_shortcut_trigger_print
;;;     gtk_shortcut_trigger_to_label
;;;     gtk_shortcut_trigger_print_label
;;;     gtk_shortcut_trigger_parse_string

;;;     gtk_keyval_trigger_new
;;;     gtk_keyval_trigger_get_modifiers
;;;     gtk_keyval_trigger_get_keyval

;;;     gtk_mnemonic_trigger_new
;;;     gtk_mnemonic_trigger_get_keyval

;;;     gtk_alternative_trigger_new
;;;     gtk_alternative_trigger_get_first
;;;     gtk_alternative_trigger_get_second

;;;     gtk_never_trigger_get

;;; --- 2023-5-29 --------------------------------------------------------------
