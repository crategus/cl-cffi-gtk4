(in-package :gtk-test)

(def-suite gdk-drag :in gdk-suite)
(in-suite gdk-drag)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkDragCancelReason

(test gdk-drag-cancel-reason
  ;; Check the type
  (is (g:type-is-enum "GdkDragCancelReason"))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkDragCancelReason")
          (g:gtype (cffi:foreign-funcall "gdk_drag_cancel_reason_get_type"
                                         :size))))
  ;; Check the registered name
  (is (eq 'gdk:drag-cancel-reason
          (glib:symbol-for-gtype "GdkDragCancelReason")))
  ;; Check the names
  (is (equal '("GDK_DRAG_CANCEL_NO_TARGET" "GDK_DRAG_CANCEL_USER_CANCELLED"
               "GDK_DRAG_CANCEL_ERROR")
             (list-enum-item-name "GdkDragCancelReason")))
  ;; Check the values
  (is (equal '(0 1 2)
             (list-enum-item-value "GdkDragCancelReason")))
  ;; Check the nick names
  (is (equal '("no-target" "user-cancelled" "error")
             (list-enum-item-nick "GdkDragCancelReason")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GdkDragCancelReason"
                                     GDK-DRAG-CANCEL-REASON
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gdk_drag_cancel_reason_get_type")
                                     (:NO-TARGET 0)
                                     (:USER-CANCELLED 1)
                                     (:ERROR 2))
             (gobject:get-g-type-definition "GdkDragCancelReason"))))

;;;     GdkDragAction

(test gdk-drag-action
  ;; Check the type
  (is (g:type-is-flags "GdkDragAction"))
  ;; Check the registered name
  (is (eq 'gdk:drag-action
          (glib:symbol-for-gtype "GdkDragAction")))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkDragAction")
          (g:gtype (cffi:foreign-funcall "gdk_drag_action_get_type" :size))))
  ;; Check the names
  (is (equal '("GDK_ACTION_COPY" "GDK_ACTION_MOVE" "GDK_ACTION_LINK"
               "GDK_ACTION_ASK")
             (list-flags-item-name "GdkDragAction")))
  ;; Check the values
  (is (equal '(1 2 4 8)
             (list-flags-item-value "GdkDragAction")))
  ;; Check the nick names
  (is (equal '("copy" "move" "link" "ask")
             (list-flags-item-nick "GdkDragAction")))
  ;; Check the flags definition
  (is (equal '(GOBJECT:DEFINE-G-FLAGS "GdkDragAction" GDK-DRAG-ACTION
                                      (:EXPORT T
                                       :TYPE-INITIALIZER
                                       "gdk_drag_action_get_type")
                                      (:COPY 1)
                                      (:MOVE 2)
                                      (:LINK 4)
                                      (:ASK 8))
             (gobject:get-g-type-definition "GdkDragAction"))))

;;;     GdkDrag

(test gdk-drag-class
  ;; Type check
  (is (g:type-is-object "GdkDrag"))
  ;; Check the registered name
  (is (eq 'gdk:drag
          (glib:symbol-for-gtype "GdkDrag")))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkDrag")
          (g:gtype (cffi:foreign-funcall "gdk_drag_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkDrag")))
  ;; Check the children
  (is (equal '()
             (list-children "GdkDrag")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GdkDrag")))
  ;; Check the properties
  (is (equal '("actions" "content" "device" "display" "formats"
               "selected-action" "surface")
             (list-properties "GdkDrag")))
  ;; Check the signals
  (is (equal '("cancel" "dnd-finished" "drop-performed")
             (list-signals "GdkDrag")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GdkDrag" GDK-DRAG
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                                :TYPE-INITIALIZER "gdk_drag_get_type")
                               ((ACTIONS GDK-DRAG-ACTIONS "actions"
                                 "GdkDragAction" T T)
                                (CONTENT GDK-DRAG-CONTENT "content"
                                 "GdkContentProvider" T NIL)
                                (DEVICE GDK-DRAG-DEVICE "device" "GdkDevice" T
                                 NIL)
                                (DISPLAY GDK-DRAG-DISPLAY "display"
                                 "GdkDisplay" T NIL)
                                (FORMATS GDK-DRAG-FORMATS "formats"
                                 "GdkContentFormats" T NIL)
                                (SELECTED-ACTION GDK-DRAG-SELECTED-ACTION
                                 "selected-action" "GdkDragAction" T T)
                                (SURFACE GDK-DRAG-SURFACE "surface"
                                 "GdkSurface" T NIL)))
             (gobject:get-g-type-definition "GdkDrag"))))

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

;;; 2024-1-7
