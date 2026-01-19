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
  (glib-test:with-check-memory (source)
    (is (typep (setf source (make-instance 'gtk:drag-source)) 'gtk:drag-source))
    (is (equal '(:copy) (gtk:drag-source-actions source)))
    (is-false (gtk:drag-source-content source))))

;;; --- Signals ----------------------------------------------------------------

;;;     drag-begin

(test gtk-drag-source-drag-begin-signal
  (let* ((name "drag-begin")
         (gtype (g:gtype "GtkDragSource"))
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
    (is (equal '("GdkDrag")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     drag-cancel

(test gtk-drag-source-drag-cancel-signal
  (let* ((name "drag-cancel")
         (gtype (g:gtype "GtkDragSource"))
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
    (is (equal '("GdkDrag" "GdkDragCancelReason")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     drag-end

(test gtk-drag-source-drag-end-signal
  (let* ((name "drag-end")
         (gtype (g:gtype "GtkDragSource"))
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
    (is (equal '("GdkDrag" "gboolean")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     prepare

(test gtk-drag-source-prepare-signal
  (let* ((name "prepare")
         (gtype (g:gtype "GtkDragSource"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "GdkContentProvider") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("gdouble" "gdouble")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_drag_source_new

(test gtk-drag-source-new
  (glib-test:with-check-memory (source)
    (is (typep (setf source (gtk:drag-source-new)) 'gtk:drag-source))))

;;;     gtk_drag_source_set_icon
;;;     gtk_drag_source_drag_cancel
;;;     gtk_drag_source_get_drag
;;;     gtk_drag_check_threshold

;;; 2026-01-12
