(in-package :gtk-test)

(def-suite gdk-drag :in gdk-suite)
(in-suite gdk-drag)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkDragCancelReason

(test gdk-drag-cancel-reason
  ;; Check type
  (is (g:type-is-enum "GdkDragCancelReason"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkDragCancelReason")
          (g:gtype (cffi:foreign-funcall "gdk_drag_cancel_reason_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gdk:drag-cancel-reason
          (glib:symbol-for-gtype "GdkDragCancelReason")))
  ;; Check names
  (is (equal '("GDK_DRAG_CANCEL_NO_TARGET" "GDK_DRAG_CANCEL_USER_CANCELLED"
               "GDK_DRAG_CANCEL_ERROR")
             (glib-test:list-enum-item-names "GdkDragCancelReason")))
  ;; Check values
  (is (equal '(0 1 2)
             (glib-test:list-enum-item-values "GdkDragCancelReason")))
  ;; Check nick names
  (is (equal '("no-target" "user-cancelled" "error")
             (glib-test:list-enum-item-nicks "GdkDragCancelReason")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GdkDragCancelReason" GDK:DRAG-CANCEL-REASON
                                    (:EXPORT T
                                     :TYPE-INITIALIZER
                                     "gdk_drag_cancel_reason_get_type")
                                    (:NO-TARGET 0)
                                    (:USER-CANCELLED 1)
                                    (:ERROR 2))
             (gobject:get-gtype-definition "GdkDragCancelReason"))))

;;;     GdkDragAction

(test gdk-drag-action
  ;; Check type
  (is (g:type-is-flags "GdkDragAction"))
  ;; Check registered name
  (is (eq 'gdk:drag-action
          (glib:symbol-for-gtype "GdkDragAction")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkDragAction")
          (g:gtype (cffi:foreign-funcall "gdk_drag_action_get_type" :size))))
  ;; Check names
  (is (equal '("GDK_ACTION_NONE" "GDK_ACTION_COPY" "GDK_ACTION_MOVE"
               "GDK_ACTION_LINK" "GDK_ACTION_ASK")
             (glib-test:list-flags-item-names "GdkDragAction")))
  ;; Check values
  (is (equal '(0 1 2 4 8)
             (glib-test:list-flags-item-values "GdkDragAction")))
  ;; Check nick names
  (is (equal '("none" "copy" "move" "link" "ask")
             (glib-test:list-flags-item-nicks "GdkDragAction")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-GFLAGS "GdkDragAction" GDK:DRAG-ACTION
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gdk_drag_action_get_type")
                                     (:NONE 0)
                                     (:COPY 1)
                                     (:MOVE 2)
                                     (:LINK 4)
                                     (:ASK 8))
             (gobject:get-gtype-definition "GdkDragAction"))))

;;;     GdkDrag

(test gdk-drag-class
  ;; Check type
  (is (g:type-is-object "GdkDrag"))
  ;; Check registered name
  (is (eq 'gdk:drag
          (glib:symbol-for-gtype "GdkDrag")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkDrag")
          (g:gtype (cffi:foreign-funcall "gdk_drag_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkDrag")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GdkDrag")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GdkDrag")))
  ;; Check properties
  (is (equal '("actions" "content" "device" "display" "formats"
               "selected-action" "surface")
             (glib-test:list-properties "GdkDrag")))
  ;; Check signals
  (is (equal '("cancel" "dnd-finished" "drop-performed")
             (glib-test:list-signals "GdkDrag")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GdkDrag" GDK:DRAG
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_drag_get_type")
                       ((ACTIONS DRAG-ACTIONS "actions" "GdkDragAction" T T)
                        (CONTENT DRAG-CONTENT "content" "GdkContentProvider" T NIL)
                        (DEVICE DRAG-DEVICE "device" "GdkDevice" T NIL)
                        (DISPLAY DRAG-DISPLAY "display" "GdkDisplay" T NIL)
                        (FORMATS DRAG-FORMATS "formats" "GdkContentFormats" T NIL)
                        (SELECTED-ACTION DRAG-SELECTED-ACTION
                         "selected-action" "GdkDragAction" T T)
                        (SURFACE DRAG-SURFACE "surface" "GdkSurface" T NIL)))
             (gobject:get-gtype-definition "GdkDrag"))))

;;; --- Properties -------------------------------------------------------------

;;;     actions
;;;     content
;;;     device
;;;     display
;;;     formats
;;;     selected-action
;;;     surface

;;; --- Signals ----------------------------------------------------------------

;;;     cancel

(test gdk-drag-cancel-signal
  (let ((query (g:signal-query (g:signal-lookup "cancel" "GdkDrag"))))
    (is (string= "cancel" (g:signal-query-signal-name query)))
    (is (string= "GdkDrag" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '("GdkDragCancelReason")
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;;     dnd-finished

(test gdk-drag-dnd-finished-signal
  (let ((query (g:signal-query (g:signal-lookup "dnd-finished" "GdkDrag"))))
    (is (string= "dnd-finished" (g:signal-query-signal-name query)))
    (is (string= "GdkDrag" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;;     drop-performed

(test gdk-drag-drop-performed-signal
  (let ((query (g:signal-query (g:signal-lookup "drop-performed" "GdkDrag"))))
    (is (string= "drop-performed" (g:signal-query-signal-name query)))
    (is (string= "GdkDrag" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk_drag_drop_done
;;;     gdk_drag_begin
;;;     gdk_drag_get_drag_surface
;;;     gdk_drag_set_hotspot
;;;     gdk_drag_action_is_unique

;;; 2025-11-02
