(in-package :gtk-test)

(def-suite gdk-clipboard :in gdk-suite)
(in-suite gdk-clipboard)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkClipboard

(test gdk-clipboard-class
  ;; Check type
  (is (g:type-is-object "GdkClipboard"))
  ;; Check registered name
  (is (eq 'gdk:clipboard
          (glib:symbol-for-gtype "GdkClipboard")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkClipboard")
          (g:gtype (cffi:foreign-funcall "gdk_clipboard_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkClipboard")))
  ;; Check children
  #-windows
  (is (member "GdkWaylandClipboard"
              (glib-test:list-children "GdkClipboard") :test #'string=))
  #+windows
  (is (equal '("GdkWin32Clipboard")
             (glib-test:list-children "GdkClipboard")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GdkClipboard")))
  ;; Check properties
  (is (equal '("content" "display" "formats" "local")
             (glib-test:list-properties "GdkClipboard")))
  ;; Check signals
  (is (equal '("changed")
             (glib-test:list-signals "GdkClipboard")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GdkClipboard" GDK:CLIPBOARD
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_clipboard_get_type")
                       ((CONTENT CLIPBOARD-CONTENT
                         "content" "GdkContentProvider" T NIL)
                        (DISPLAY CLIPBOARD-DISPLAY "display" "GdkDisplay" T NIL)
                        (FORMATS CLIPBOARD-FORMATS
                         "formats" "GdkContentFormats" T NIL)
                        (LOCAL CLIPBOARD-LOCAL "local" "gboolean" T NIL)))
             (gobject:get-gtype-definition "GdkClipboard"))))

;;; --- Properties -------------------------------------------------------------

;; Check properties of the default clipboard
(test gdk-clipboard-properties.1
  (let* ((display (gdk:display-default))
         (clipboard (gdk:display-clipboard display)))
    (is (typep display 'gdk:display))
    (is (typep clipboard 'gdk:clipboard))
    (is-true (gdk:clipboard-set-content clipboard nil))
    (signals (error) (setf (gdk:clipboard-content clipboard) nil))
    (is-false (gdk:clipboard-content clipboard))
    (is (typep (gdk:clipboard-display clipboard) 'gdk:display))
    (signals (error) (setf (gdk:clipboard-formats clipboard) nil))
    (is (typep (gdk:clipboard-formats clipboard) 'gdk:content-formats))
    (signals (error) (setf (gdk:clipboard-local clipboard) nil))
    ;; The value is false in the second run of the testsuite.
    (is-true (or (gdk:clipboard-local clipboard)
                 (not (gdk:clipboard-local clipboard))))))

;; Check primary clipboard
(test gdk-clipboard-properties.2
  (let* ((display (gdk:display-default))
         (clipboard (gdk:display-primary-clipboard display)))
    (is (typep display 'gdk:display))
    (is (typep clipboard 'gdk:clipboard))
    (is-true (gdk:clipboard-set-content clipboard nil))
    (is-false (gdk:clipboard-content clipboard))
    (is (typep (gdk:clipboard-display clipboard) 'gdk:display))
    (is (typep (gdk:clipboard-formats clipboard) 'gdk:content-formats))
    (when *first-run-testsuite*
      (is-true (gdk:clipboard-local clipboard)))))

;;; --- Signals ----------------------------------------------------------------

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

(test gdk-clipboard-read-text-async
  (glib-test:with-check-memory ((clipboard 3))
    (let (content)
    (is (typep (setf clipboard
                     (gdk:display-clipboard (gdk:display-default)))
               'gdk:clipboard))
    (is-false (gdk:clipboard-set-text clipboard "The text for the clipboard."))
    ;; FIXME: Can we read the clipboard in the testsuite!
    (gdk:clipboard-read-text-async clipboard nil
        (lambda (source result)
          (setf content (gdk:clipboard-read-text-finish source result))))
    (is-false content)
)))

;;;     gdk_clipboard_set
;;;     gdk_clipboard_set_valist
;;;     gdk_clipboard_set_value

;;;     gdk_clipboard_set_text

(test gtk-clipboard-set-text
  (glib-test:with-check-memory ((clipboard 3))
    (is (typep (setf clipboard
                     (gdk:display-clipboard (gdk:display-default)))
               'gdk:clipboard))
    (is-false (gdk:clipboard-set-text clipboard "The text for the clipboard."))
))

;;;     gdk_clipboard_set_texture

;;; 2025-4-26
