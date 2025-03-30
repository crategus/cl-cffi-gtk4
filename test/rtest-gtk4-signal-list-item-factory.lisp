(in-package :gtk-test)

(def-suite gtk-signal-list-item-factory :in gtk-list-widgets)
(in-suite gtk-signal-list-item-factory)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkSignalListItemFactory

(test gtk-signal-list-item-factory-class
  ;; Check type
  (is (g:type-is-object "GtkSignalListItemFactory"))
  ;; Check registered name
  (is (eq 'gtk:signal-list-item-factory
          (glib:symbol-for-gtype "GtkSignalListItemFactory")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkSignalListItemFactory")
          (g:gtype (cffi:foreign-funcall "gtk_signal_list_item_factory_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkListItemFactory")
          (g:type-parent "GtkSignalListItemFactory")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkSignalListItemFactory")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkSignalListItemFactory")))
  ;; Check properties
  (is (equal '()
             (glib-test:list-properties "GtkSignalListItemFactory")))
  ;; Check signals
  (is (equal '("bind" "setup" "teardown" "unbind")
             (glib-test:list-signals "GtkSignalListItemFactory")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkSignalListItemFactory"
                                      GTK:SIGNAL-LIST-ITEM-FACTORY
                      (:SUPERCLASS GTK:LIST-ITEM-FACTORY
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gtk_signal_list_item_factory_get_type")
                      NIL)
             (gobject:get-gtype-definition "GtkSignalListItemFactory"))))

;;; --- Signals ----------------------------------------------------------------

;;;     bind

(test gtk-signal-list-item-factory-bind-signal
  (let* ((name "bind")
         (gtype (g:gtype "GtkSignalListItemFactory"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("GObject")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     setup

(test gtk-signal-list-item-factory-setup-signal
  (let* ((name "setup")
         (gtype (g:gtype "GtkSignalListItemFactory"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("GObject")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     teardown

(test gtk-signal-list-item-factory-teardown-signal
  (let* ((name "teardown")
         (gtype (g:gtype "GtkSignalListItemFactory"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("GObject")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     unbind

(test gtk-signal-list-item-factory-unbind-signal
  (let* ((name "unbind")
         (gtype (g:gtype "GtkSignalListItemFactory"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("GObject")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_signal_list_item_factory_new

(test gtk-signal-list-item-factory-new
  (is (typep (gtk:signal-list-item-factory-new) 'gtk:signal-list-item-factory)))

;;; 2024-11-27
