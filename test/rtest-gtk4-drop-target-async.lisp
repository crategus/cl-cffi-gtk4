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
;;;     drag-enter
;;;     drag-leave
;;;     drag-motion
;;;     drop

;;; --- Properties -------------------------------------------------------------

;;;     actions
;;;     formats

;;; --- Functions --------------------------------------------------------------

;;;     gtk_drop_target_async_new
;;;     gtk_drop_target_async_reject_drop

;;; 2024-11-2
