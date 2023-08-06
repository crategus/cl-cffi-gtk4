(in-package :gtk-test)

(def-suite gdk-clipboard :in gdk-suite)
(in-suite gdk-clipboard)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkClipboard

(test gdk-clipboard-class
  ;; Type check
  (is (g:type-is-object "GdkClipboard"))
  ;; Check the registered name
  (is (eq 'gdk:clipboard
          (glib:symbol-for-gtype "GdkClipboard")))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkClipboard")
          (g:gtype (cffi:foreign-funcall "gdk_clipboard_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkClipboard")))
  ;; Check the children
  #-windows
  (is (equal '("GdkWaylandClipboard" "GdkWaylandPrimary" "GdkX11Clipboard")
             (list-children "GdkClipboard")))
  #+windows
  (is (equal '("GdkWin32Clipboard")
             (list-children "GdkClipboard")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GdkClipboard")))
  ;; Check the properties
  (is (equal '("content" "display" "formats" "local")
             (list-properties "GdkClipboard")))
  ;; Check the signals
  (is (equal '("changed")
             (list-signals "GdkClipboard")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GdkClipboard" GDK-CLIPBOARD
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                                :TYPE-INITIALIZER "gdk_clipboard_get_type")
                               ((CONTENT GDK-CLIPBOARD-CONTENT "content"
                                 "GdkContentProvider" T NIL)
                                (DISPLAY GDK-CLIPBOARD-DISPLAY "display"
                                 "GdkDisplay" T NIL)
                                (FORMATS GDK-CLIPBOARD-FORMATS "formats"
                                 "GdkContentFormats" T NIL)
                                (LOCAL GDK-CLIPBOARD-LOCAL "local" "gboolean" T
                                 NIL)))
             (gobject:get-g-type-definition "GdkClipboard"))))

;;; --- Properties -------------------------------------------------------------

;;;     content
;;;     display
;;;     formats
;;;     local

(test gdk-clipboard-properties.1
  (let* ((display (gdk:display-default))
         (clipboard (gdk:display-clipboard display)))

    (is (typep display 'gdk:display))
    (is (typep clipboard 'gdk:clipboard))

    (signals (error) (setf (gdk:clipboard-content clipboard) nil))
    (is-false (gdk:clipboard-content clipboard))
    ;; TODO: The display property is construct only. We do not get an error?
;    (is (typep (setf (gdk:clipboard-display clipboard) (gdk:display-default))
;               'gdk:display))
    (is (typep (gdk:clipboard-display clipboard) 'gdk:display))

    (signals (error) (setf (gdk:clipboard-formats clipboard) nil))
    (is (typep (gdk:clipboard-formats clipboard) 'gdk:content-formats))

    (signals (error) (setf (gdk:clipboard-local clipboard) nil))
    ;; The value is false in the second run of the testsuite.
    (is-true (or (gdk:clipboard-local clipboard)
                 (not (gdk:clipboard-local clipboard))))))

(test gdk-clipboard-properties.2
  (let* ((display (gdk:display-default))
         (clipboard (gdk:display-primary-clipboard display)))

    (is (typep display 'gdk:display))
    (is (typep clipboard 'gdk:clipboard))

    (is-false (gdk:clipboard-content clipboard))
    (is (typep (gdk:clipboard-display clipboard) 'gdk:display))
    (is (typep (gdk:clipboard-formats clipboard) 'gdk:content-formats))
    (is-true (gdk:clipboard-local clipboard))))

;;; --- Signals ----------------------------------------------------------------

;;;     changed

(test gdk-clipboard-changed-signal
  (let ((query (g:signal-query (g:signal-lookup "changed" "GdkClipboard"))))
    (is (string= "changed" (g:signal-query-signal-name query)))
    (is (string= "GdkClipboard"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk_clipboard_is_local

(test gdk-clipboard-is-local
  (let* ((display (gdk:display-default))
         (clipboard (gdk:display-clipboard display)))
    (is-true (or (gdk:clipboard-is-local clipboard)
                 (not (gdk:clipboard-is-local clipboard))))))

;;;     gdk_clipboard_set_content

(test gdk-clipboard-set-content.1
  (let* ((display (gdk:display-default))
         (clipboard (gdk:display-clipboard display)))
    (is-true (gdk:clipboard-set-content clipboard nil))))

(test gdk-clipboard-set-content.2
  (let* ((display (gdk:display-default))
         (clipboard (gdk:display-clipboard display))
         (provider nil))
    (cffi:with-foreign-object (gvalue '(:struct g:value))
      (is (= 123 (g:value-set gvalue 123 "gint")))
      (is (typep (setf provider
                       (gdk:content-provider-new-for-value gvalue))
                 'gdk:content-provider))
      (is-true (gdk:clipboard-set-content clipboard provider))
      (is (typep (gdk:clipboard-content clipboard) 'gdk:content-provider))
      (is-true (gdk:clipboard-set-content clipboard nil))
      (is-false (gdk:clipboard-content clipboard)))))

;;;     gdk_clipboard_store_async
;;;     gdk_clipboard_store_finish
;;;     gdk_clipboard_read_async
;;;     gdk_clipboard_read_finish
;;;     gdk_clipboard_read_value_async
;;;     gdk_clipboard_read_value_finish
;;;     gdk_clipboard_read_texture_async
;;;     gdk_clipboard_read_texture_finish
;;;     gdk_clipboard_read_text_async
;;;     gdk_clipboard_read_text_finish
;;;     gdk_clipboard_set
;;;     gdk_clipboard_set_valist
;;;     gdk_clipboard_set_value
;;;     gdk_clipboard_set_text
;;;     gdk_clipboard_set_texture

;;; --- 2023-7-31 --------------------------------------------------------------
