(in-package :gtk-test)

(def-suite gtk-drop-target-async :in gtk-drag-and-drop)
(in-suite gtk-drop-target-async)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkDropTargetAsync

(test gtk-drop-target-async-class
  ;; Check type
  (is (g:type-is-object "GtkDropTargetAsync"))
  ;; Check registered name
  (is (eq 'gtk:drop-target-async
          (glib:symbol-for-gtype "GtkDropTargetAsync")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkDropTargetAsync")
          (g:gtype (cffi:foreign-funcall "gtk_drop_target_async_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkEventController")
          (g:type-parent "GtkDropTargetAsync")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkDropTargetAsync")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkDropTargetAsync")))
  ;; Check properties
  (is (equal '("actions" "formats")
             (glib-test:list-properties "GtkDropTargetAsync")))
  ;; Check signals
  (is (equal '("accept" "drag-enter" "drag-leave" "drag-motion" "drop")
             (glib-test:list-signals "GtkDropTargetAsync")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkDropTargetAsync" GTK:DROP-TARGET-ASYNC
                       (:SUPERCLASS GTK:EVENT-CONTROLLER
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_drop_target_async_get_type")
                       ((ACTIONS DROP-TARGET-ASYNC-ACTIONS "actions"
                         "GdkDragAction" T T)
                        (FORMATS DROP-TARGET-ASYNC-FORMATS "formats"
                         "GdkContentFormats" T T)))
             (gobject:get-gtype-definition "GtkDropTargetAsync"))))

;;; --- Signals ----------------------------------------------------------------

;;;     accept

(test gtk-drop-target-async-accept-signal
  (let* ((name "accept")
         (gtype (g:gtype "GtkDropTargetAsync"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "gboolean") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("GdkDrop")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     drag-enter

(test gtk-drop-target-async-drag-enter-signal
  (let* ((name "drag-enter")
         (gtype (g:gtype "GtkDropTargetAsync"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "GdkDragAction") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("GdkDrop" "gdouble" "gdouble")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     drag-leave

(test gtk-drop-target-async-drag-leave-signal
  (let* ((name "drag-leave")
         (gtype (g:gtype "GtkDropTargetAsync"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("GdkDrop")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     drag-motion

(test gtk-drop-target-async-drag-motion-signal
  (let* ((name "drag-motion")
         (gtype (g:gtype "GtkDropTargetAsync"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "GdkDragAction") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("GdkDrop" "gdouble" "gdouble")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     drop

(test gtk-drop-target-async-drop-signal
  (let* ((name "drop")
         (gtype (g:gtype "GtkDropTargetAsync"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "gboolean") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("GdkDrop" "gdouble" "gdouble")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Properties -------------------------------------------------------------

;;;     actions
;;;     formats

(test gtk-drop-target-async-properties
  (glib-test:with-check-memory (target)
    (is (typep (setf target
                     (make-instance 'gtk:drop-target-async))
               'gtk:drop-target-async))
    (is-false (gtk:drop-target-async-actions target))
    (is-false (gtk:drop-target-async-formats target))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_drop_target_async_new

(test gtk-drop-target-async-new
  (glib-test:with-check-memory (target)

    (let ((formats (gdk:content-formats-new '("text/plain")))
          (actions '(:copy :ask)))
      (is (typep (setf target
                       (gtk:drop-target-async-new formats actions))
                 'gtk:drop-target-async))
      (is (equal '(:copy :ask) (gtk:drop-target-async-actions target)))
      (is (typep (gtk:drop-target-async-formats target) 'gdk:content-formats)))))

;;;     gtk_drop_target_async_reject_drop

;;; 2026-01-15
