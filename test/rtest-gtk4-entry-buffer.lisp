(in-package :gtk-test)

(def-suite gtk-entry-buffer :in gtk-suite)
(in-suite gtk-entry-buffer)

;; Ensure the initialization of GtkPasswordEntryBuffer
(eval-when (:compile-toplevel :load-toplevel :execute)
  (foreign-funcall "gtk_password_entry_buffer_get_type" :size))

;;; --- Types and Values -------------------------------------------------------

;;;     GtkEntryBuffer

(test entry-buffer-class
  ;; Type check
  (is (g:type-is-object "GtkEntryBuffer"))
  ;; Check the registered name
  (is (eq 'gtk:entry-buffer
          (gobject:symbol-for-gtype "GtkEntryBuffer")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkEntryBuffer")
          (g:gtype (foreign-funcall "gtk_entry_buffer_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkEntryBuffer")))
  ;; Check the children
  (is (equal '("GtkPasswordEntryBuffer")
             (list-children "GtkEntryBuffer")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkEntryBuffer")))
  ;; Check the class properties
  (is (equal '("length" "max-length" "text")
             (list-properties "GtkEntryBuffer")))
  ;; Check the list of signals
  (is (equal '("deleted-text" "inserted-text")
             (list-signals "GtkEntryBuffer")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkEntryBuffer" GTK-ENTRY-BUFFER
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_entry_buffer_get_type")
                       ((LENGTH GTK-ENTRY-BUFFER-LENGTH "length" "guint" T NIL)
                        (MAX-LENGTH GTK-ENTRY-BUFFER-MAX-LENGTH "max-length"
                         "gint" T T)
                        (TEXT GTK-ENTRY-BUFFER-TEXT "text" "gchararray" T T)))
             (gobject:get-g-type-definition "GtkEntryBuffer"))))

;;; --- Properties -------------------------------------------------------------

(test entry-buffer-properties
  (let ((buffer (make-instance 'gtk:entry-buffer)))
    (is (= 0 (gtk:entry-buffer-length buffer)))
    (signals (error) (setf (gtk:entry-buffer-length buffer) 10))
    (is (=  0 (gtk:entry-buffer-max-length buffer)))
    (is (= 10 (setf (gtk:entry-buffer-max-length buffer) 10)))
    (is (string= "" (gtk:entry-buffer-text buffer)))
    (is (string= "text" (setf (gtk:entry-buffer-text buffer) "text")))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_entry_buffer_new

(test entry-buffer-new
  (is (typep (gtk:entry-buffer-new) 'gtk:entry-buffer))
  (is (string= "" (gtk:entry-buffer-text (gtk:entry-buffer-new))))
  (is (string= "This is text."
               (gtk:entry-buffer-text (gtk:entry-buffer-new "This is text.")))))

;;;     gtk_entry_buffer_get_bytes

(test entry-buffer-bytes
  (is (= 13 (gtk:entry-buffer-length (gtk:entry-buffer-new "This is text."))))
  (is (= 13 (gtk:entry-buffer-bytes (gtk:entry-buffer-new "This is text."))))
  (is (= 7 (gtk:entry-buffer-length (gtk:entry-buffer-new "Ägypten"))))
  (is (= 8 (gtk:entry-buffer-bytes (gtk:entry-buffer-new "Ägypten")))))

;;;     gtk_entry_buffer_insert_text
;;;     gtk_entry_buffer_delete_text

(test entry-buffer-insert/delete-text
  (let ((buffer (make-instance 'gtk:entry-buffer)))
    (is (string= "" (gtk:entry-buffer-text buffer)))
    ;; Insert text
    (is (= 7 (gtk:entry-buffer-insert-text buffer 0 "Ägypten")))
    (is (string= "Ägypten" (gtk:entry-buffer-text buffer)))
    ;; Insert text
    (is (= 10 (gtk:entry-buffer-insert-text buffer 8 " ein Land.")))
    (is (string= "Ägypten ein Land." (gtk:entry-buffer-text buffer)))
    ;; Insert text
    (is (= 4 (gtk:entry-buffer-insert-text buffer 8 "ist ")))
    (is (string= "Ägypten ist ein Land." (gtk:entry-buffer-text buffer)))
    ;; Delete text
    (is (= 4 (gtk:entry-buffer-delete-text buffer 8 4)))
    (is (string= "Ägypten ein Land." (gtk:entry-buffer-text buffer)))))

;;;   gtk_entry_buffer_emit_deleted_text

(test entry-buffer-emit-deleted-text
  (let ((buffer (gtk:entry-buffer-new "first second third")))
    (g:signal-connect buffer "deleted-text"
        (lambda (object position n-chars)
          (is (eq buffer object))
          (is (string= "first second third"
                       (gtk:entry-buffer-text object)))
          (is (= 6 position))
          (is (= 7 n-chars))
          nil))
    (is-false (gtk:entry-buffer-emit-deleted-text buffer 6 7))))

;;;   gtk_entry_buffer_emit_inserted_text

(test entry-buffer-emit-inserted-text
  (let ((buffer (gtk:entry-buffer-new "first second third")))
    (g:signal-connect buffer "inserted-text"
        (lambda (object position text n-chars)
          (is (eq buffer object))
          (is (= 6 position))
          (is (equal "text" text))
          (is (= 7 n-chars))
          nil))
    (gtk:entry-buffer-emit-inserted-text buffer 6 "text" 7)))

;;; --- 2023-1-30 --------------------------------------------------------------
