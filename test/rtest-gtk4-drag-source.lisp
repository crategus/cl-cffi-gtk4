(in-package :gtk-test)

(def-suite gtk-drag-source :in gtk-suite)
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
             (gtk-test:list-children "GtkDragSource")))
  ;; Check interfaces
  (is (equal '()
             (gtk-test:list-interfaces "GtkDragSource")))
  ;; Check properties
  (is (equal '("actions" "content")
             (gtk-test:list-properties "GtkDragSource")))
  ;; Check signals
  (is (equal '("drag-begin" "drag-cancel" "drag-end" "prepare")
             (gtk-test:list-signals "GtkDragSource")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkDragSource" GTK-DRAG-SOURCE
                       (:SUPERCLASS GTK-GESTURE-SINGLE :EXPORT T :INTERFACES
                        NIL :TYPE-INITIALIZER "gtk_drag_source_get_type")
                       ((ACTIONS GTK-DRAG-SOURCE-ACTIONS "actions"
                         "GdkDragAction" T T)
                        (CONTENT GTK-DRAG-SOURCE-CONTENT "content"
                         "GdkContentProvider" T T)))
             (gobject:get-g-type-definition "GtkDragSource"))))

;;; --- Properties -------------------------------------------------------------

;;;     actions
;;;     content

;;; --- Signals ----------------------------------------------------------------

;;;     drag-begin
;;;     drag-cancel
;;;     drag-end
;;;     prepare

;;; --- Functions --------------------------------------------------------------

;;;     gtk_drag_source_new
;;;     gtk_drag_source_set_icon
;;;     gtk_drag_source_drag_cancel
;;;     gtk_drag_source_get_drag
;;;     gtk_drag_check_threshold

;;; 2024-7-4
