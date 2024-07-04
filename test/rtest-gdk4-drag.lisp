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
             (gtk-test:list-enum-item-name "GdkDragCancelReason")))
  ;; Check values
  (is (equal '(0 1 2)
             (gtk-test:list-enum-item-value "GdkDragCancelReason")))
  ;; Check nick names
  (is (equal '("no-target" "user-cancelled" "error")
             (gtk-test:list-enum-item-nick "GdkDragCancelReason")))
  ;; Check enum definition
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
  ;; Check type
  (is (g:type-is-flags "GdkDragAction"))
  ;; Check registered name
  (is (eq 'gdk:drag-action
          (glib:symbol-for-gtype "GdkDragAction")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkDragAction")
          (g:gtype (cffi:foreign-funcall "gdk_drag_action_get_type" :size))))
  ;; Check names
  (is (equal '("GDK_ACTION_COPY" "GDK_ACTION_MOVE" "GDK_ACTION_LINK"
               "GDK_ACTION_ASK")
             (gtk-test:list-flags-item-name "GdkDragAction")))
  ;; Check values
  (is (equal '(1 2 4 8)
             (gtk-test:list-flags-item-value "GdkDragAction")))
  ;; Check nick names
  (is (equal '("copy" "move" "link" "ask")
             (gtk-test:list-flags-item-nick "GdkDragAction")))
  ;; Check flags definition
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
             (gtk-test:list-children "GdkDrag")))
  ;; Check interfaces
  (is (equal '()
             (gtk-test:list-interfaces "GdkDrag")))
  ;; Check properties
  (is (equal '("actions" "content" "device" "display" "formats"
               "selected-action" "surface")
             (gtk-test:list-properties "GdkDrag")))
  ;; Check signals
  (is (equal '("cancel" "dnd-finished" "drop-performed")
             (gtk-test:list-signals "GdkDrag")))
  ;; Check class definition
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
