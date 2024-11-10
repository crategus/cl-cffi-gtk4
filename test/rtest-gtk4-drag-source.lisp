(in-package :gtk-test)

(def-suite gtk-drag-source :in gtk-drag-and-drop)
(in-suite gtk-drag-source)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkDragSource

(test drag-source-class
  ;; Check type
  (is (g:type-is-object "GtkDragSource"))
  ;; Check registered name
  (is (eq 'gtk:drag-source
          (glib:symbol-for-gtype "GtkDragSource")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkDragSource")
          (g:gtype (cffi:foreign-funcall "gtk_drag_source_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkGestureSingle")
          (g:type-parent "GtkDragSource")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkDragSource")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkDragSource")))
  ;; Check properties
  (is (equal '("actions" "content")
             (glib-test:list-properties "GtkDragSource")))
  ;; Check signals
  (is (equal '("drag-begin" "drag-cancel" "drag-end" "prepare")
             (glib-test:list-signals "GtkDragSource")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkDragSource" GTK:DRAG-SOURCE
                       (:SUPERCLASS GTK:GESTURE-SINGLE
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_drag_source_get_type")
                       ((ACTIONS DRAG-SOURCE-ACTIONS
                         "actions" "GdkDragAction" T T)
                        (CONTENT DRAG-SOURCE-CONTENT
                         "content" "GdkContentProvider" T T)))
             (gobject:get-gtype-definition "GtkDragSource"))))

;;; --- Properties -------------------------------------------------------------

;;;     actions
;;;     content

(test gtk-drag-source-properties
  (let ((source (make-instance 'gtk:drag-source)))
    (is (equal '(:copy) (gtk:drag-source-actions source)))
    (is-false (gtk:drag-source-content source))
    (is (= 1 (g:object-ref-count source)))))

;;; --- Signals ----------------------------------------------------------------

;;;     drag-begin
;;;     drag-cancel
;;;     drag-end
;;;     prepare

;;; --- Functions --------------------------------------------------------------

;;;     gtk_drag_source_new

(test gtk-drag-source-new
  (let (source)
    (is (typep (setf source (gtk:drag-source-new)) 'gtk:drag-source))
    (is (= 1 (g:object-ref-count source)))))

;;;     gtk_drag_source_set_icon
;;;     gtk_drag_source_drag_cancel
;;;     gtk_drag_source_get_drag
;;;     gtk_drag_check_threshold

;;; 2024-11-2
