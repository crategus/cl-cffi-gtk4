(in-package :gtk-test)

(def-suite gtk-drag-source :in gtk-suite)
(in-suite gtk-drag-source)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkDragSource

(test drag-source-class
  ;; Type check
  (is (g:type-is-object "GtkDragSource"))
  ;; Check the registered name
  (is (eq 'gtk:drag-source
          (gobject:symbol-for-gtype "GtkDragSource")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkDragSource")
          (g:gtype (cffi:foreign-funcall "gtk_drag_source_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkGestureSingle")
          (g:type-parent "GtkDragSource")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkDragSource")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkDragSource")))
  ;; Check the properties
  (is (equal '("actions" "content")
             (list-properties "GtkDragSource")))
  ;; Check the signals
  (is (equal '("drag-begin" "drag-cancel" "drag-end" "prepare")
             (list-signals "GtkDragSource")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkDragSource" GTK-DRAG-SOURCE
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

;;; --- 2023-3-18 --------------------------------------------------------------
