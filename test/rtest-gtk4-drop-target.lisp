(in-package :gtk-test)

(def-suite gtk-drop-target :in gtk-drag-and-drop)
(in-suite gtk-drop-target)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkDropTarget

(test gtk-drop-target-class
  ;; Check type
  (is (g:type-is-object "GtkDropTarget"))
  ;; Check registered name
  (is (eq 'gtk:drop-target
          (glib:symbol-for-gtype "GtkDropTarget")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkDropTarget")
          (g:gtype (cffi:foreign-funcall "gtk_drop_target_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkEventController")
          (g:type-parent "GtkDropTarget")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkDropTarget")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkDropTarget")))
  ;; Check properties
  (is (equal '("actions" "current-drop" "drop" "formats" "preload" "value")
             (glib-test:list-properties "GtkDropTarget")))
  ;; Check signals
  (is (equal '("accept" "drop" "enter" "leave" "motion")
             (glib-test:list-signals "GtkDropTarget")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkDropTarget" GTK:DROP-TARGET
                      (:SUPERCLASS GTK:EVENT-CONTROLLER
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gtk_drop_target_get_type")
                      ((ACTIONS DROP-TARGET-ACTIONS
                        "actions" "GdkDragAction" T T)
                       (CURRENT-DROP DROP-TARGET-CURRENT-DROP
                        "current-drop" "GdkDrop" T NIL)
                       (DROP DROP-TARGET-DROP "drop" "GdkDrop" T NIL)
                       (FORMATS DROP-TARGET-FORMATS
                        "formats" "GdkContentFormats" T NIL)
                       (PRELOAD DROP-TARGET-PRELOAD "preload" "gboolean" T T)
                       (VALUE DROP-TARGET-VALUE "value" "GValue" T NIL)))
             (gobject:get-gtype-definition "GtkDropTarget"))))

;;; --- Properties -------------------------------------------------------------

;;;     actions
;;;     current-drop                                       Since 4.4
;;;     drop                                               Deprecated 4.4
;;;     formats
;;;     preload
;;;     value

(test gtk-drop-target-properties
  (glib-test:with-check-memory (target)
    (is (typep (setf target (make-instance 'gtk:drop-target)) 'gtk:drop-target))
    (is-false (gtk:drop-target-actions target))
    (is-false (gtk:drop-target-current-drop target))
    (is-false (gtk:drop-target-drop target))
    (is (typep (gtk:drop-target-formats target) 'gdk:content-formats))
    (is-false (gtk:drop-target-preload target))
    (is (cffi:pointerp (gtk:drop-target-value target)))))

;;; --- Signals ----------------------------------------------------------------

;;;     accept

(test gtk-drop-target-accept-signal
  (let* ((name "accept")
         (gtype (g:gtype "GtkDropTarget"))
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

;;;     drop

(test gtk-drop-target-drop-signal
  (let* ((name "drop")
         (gtype (g:gtype "GtkDropTarget"))
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
    (is (equal '("GValue" "gdouble" "gdouble")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     enter

(test gtk-drop-target-enter-signal
  (let* ((name "enter")
         (gtype (g:gtype "GtkDropTarget"))
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
    (is (equal '("gdouble" "gdouble")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     leave

(test gtk-drop-target-leave-signal
  (let* ((name "leave")
         (gtype (g:gtype "GtkDropTarget"))
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
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     motion

(test gtk-drop-target-motion-signal
  (let* ((name "motion")
         (gtype (g:gtype "GtkDropTarget"))
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
    (is (equal '("gdouble" "gdouble")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_drop_target_new

(test gtk-drop-target-new
  (glib-test:with-check-memory (target)
    (is (typep (setf target
                     (gtk:drop-target-new "GtkBox" :none)) 'gtk:drop-target))))

;;;     gtk_drop_target_set_gtypes
;;;     gtk_drop_target_get_gtypes

(test gtk-drop-target-gtypes.1
  (glib-test:with-check-memory (target)
    (is (typep (setf target
                     (gtk:drop-target-new "GtkBox" :none)) 'gtk:drop-target))
    (is (equal '("GtkBox")
               (mapcar #'g:type-name (gtk:drop-target-gtypes target))))))

(test gtk-drop-target-gtypes.2
  (let ((target (gtk:drop-target-new nil :none)))
    (is (equal '("GtkButton" "GtkLabel")
               (mapcar #'g:type-name
                       (setf (gtk:drop-target-gtypes target)
                             '("GtkButton" "GtkLabel")))))
    (is (equal '("GtkButton" "GtkLabel")
               (mapcar #'g:type-name (gtk:drop-target-gtypes target))))
    (is (= 1 (g:object-ref-count target)))))

;;;     gtk_drop_target_reject

;;; 2026-01-13
