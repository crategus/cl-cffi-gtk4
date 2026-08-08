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

;;; --- Signals ----------------------------------------------------------------

;;;     changed

(test gdk-clipboard-changed-signal
  (let* ((name "changed")
         (gtype (g:gtype "GdkClipboard"))
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
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Properties -------------------------------------------------------------

;; Check properties of the default clipboard
(test gdk-clipboard-properties.1
  (glib-test:with-check-memory (:strong 1)
    (let ((clipboard (gdk:display-clipboard (gdk:display-default))))
      ;; Clear clipboard
      (is-true (gdk:clipboard-set-content clipboard nil))
      ;; Property CONTENT
      (is-false (gdk:clipboard-content clipboard))
      (signals (error) (setf (gdk:clipboard-content clipboard) nil))
      ;; Property DISPLAY
      (is (typep (gdk:clipboard-display clipboard) 'gdk:display))
      (signals (error) (setf (gdk:clipboard-formats clipboard) nil))
      ;; Property FORMATS
      (is (typep (gdk:clipboard-formats clipboard) 'gdk:content-formats))
      (signals (error) (setf (gdk:clipboard-local clipboard) nil))
      ;; Property LOCAL
      (when *first-run-testsuite*
        ;; The value is false in the second run of the testsuite
        (is-true (gdk:clipboard-local clipboard)))
      (signals (error) (setf (gdk:clipboard-local clipboard) nil)))))

;; Check primary clipboard
(test gdk-clipboard-properties.2
  (glib-test:with-check-memory (:strong 1)
    (let ((clipboard (gdk:display-primary-clipboard (gdk:display-default))))
      ;; Clear clipboard
      (is-true (gdk:clipboard-set-content clipboard nil))
      ;; Property CONTENT
      (is-false (gdk:clipboard-content clipboard))
      ;; Property DISPLAY
      (is (typep (gdk:clipboard-display clipboard) 'gdk:display))
      ;; Property FORMATS
      (is (typep (gdk:clipboard-formats clipboard) 'gdk:content-formats))
      ;; Property LOCAL
      (when *first-run-testsuite*
        (is-true (gdk:clipboard-local clipboard))))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk_clipboard_is_local

(test gdk-clipboard-is-local
  (glib-test:with-check-memory (:strong 1)
    (let ((clipboard (gdk:display-clipboard (gdk:display-default))))
      (is-true (or (gdk:clipboard-is-local clipboard)
                   (not (gdk:clipboard-is-local clipboard)))))))

;;;     gdk_clipboard_set_content

(test gdk-clipboard-set-content.1
  (glib-test:with-check-memory (:strong 1)
    (let ((clipboard (gdk:display-clipboard (gdk:display-default))))
      (is-true (gdk:clipboard-set-content clipboard nil)))))

(test gdk-clipboard-set-content.2
  (glib-test:with-check-memory (provider :strong 1)
    (let ((clipboard (gdk:display-clipboard (gdk:display-default))))
      (cffi:with-foreign-object (gvalue '(:struct g:value))
        (is (= 123 (g:value-set gvalue 123 "gint")))
        (is (typep (setf provider
                         (gdk:content-provider-new-for-value gvalue))
                   'gdk:content-provider))
        (is-true (gdk:clipboard-set-content clipboard provider))
        (is (typep (gdk:clipboard-content clipboard) 'gdk:content-provider))
        (is-true (gdk:clipboard-set-content clipboard nil))
        (is-false (gdk:clipboard-content clipboard))))))

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
  (glib-test:with-check-memory ()
    (let ((clipboard (gdk:display-clipboard (gdk:display-default)))
          content)
      (is-false (gdk:clipboard-set-text clipboard "The text for the clipboard."))
      ;; FIXME: Can we read the clipboard in the testsuite!
      (gdk:clipboard-read-text-async clipboard nil
          (lambda (source result)
            (setf content (gdk:clipboard-read-text-finish source result))))
      (is-false content))))

;;;     gdk_clipboard_set
;;;     gdk_clipboard_set_valist
;;;     gdk_clipboard_set_value

;;;     gdk_clipboard_set_text

;; TODO: Check the memory usage for the clipboard

(test gdk-clipboard-set-text
  (glib-test:with-check-memory ()
    (let ((clipboard (gdk:display-clipboard (gdk:display-default))))
      (is-false (gdk:clipboard-set-text clipboard "The text for the clipboard.")))))

;;;     gdk_clipboard_set_texture

;;; 2026-07-30
