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

;;; --- Signals ----------------------------------------------------------------

;;;     accept
;;;     drop
;;;     enter
;;;     leave
;;;     motion

;;; --- Functions --------------------------------------------------------------

;;;     gtk_drop_target_new

(test gtk-drop-target-new
  (let (target)
    (is (typep (setf target
                     (gtk:drop-target-new "GtkBox" :none)) 'gtk:drop-target))
    (is (= 1 (g:object-ref-count target)))))

;;;     gtk_drop_target_set_gtypes
;;;     gtk_drop_target_get_gtypes

(test gtk-drop-target-gtypes.1
  (let ((target (gtk:drop-target-new "GtkBox" :none)))
    (is (equal '("GtkBox")
               (mapcar #'g:type-name (gtk:drop-target-gtypes target))))
    (is (= 1 (g:object-ref-count target)))))

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

;;; 2024-11-2
