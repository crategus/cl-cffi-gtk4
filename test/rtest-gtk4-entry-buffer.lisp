(in-package :gtk-test)

(def-suite gtk-entry-buffer :in gtk-data-entry)
(in-suite gtk-entry-buffer)

;; Ensure the initialization of GtkPasswordEntryBuffer
(eval-when (:compile-toplevel :load-toplevel :execute)
  (g:type-ensure "GtkPasswordEntryBuffer"))

;;; --- Types and Values -------------------------------------------------------

;;;     GtkEntryBuffer

(test gtk-entry-buffer-class
  ;; Check type
  (is (g:type-is-object "GtkEntryBuffer"))
  ;; Check registered name
  (is (eq 'gtk:entry-buffer
          (glib:symbol-for-gtype "GtkEntryBuffer")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkEntryBuffer")
          (g:gtype (cffi:foreign-funcall "gtk_entry_buffer_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkEntryBuffer")))
  ;; Check children
  (is (equal '("GtkPasswordEntryBuffer")
             (glib-test:list-children "GtkEntryBuffer")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkEntryBuffer")))
  ;; Check class properties
  (is (equal '("length" "max-length" "text")
             (glib-test:list-properties "GtkEntryBuffer")))
  ;; Check signals
  (is (equal '("deleted-text" "inserted-text")
             (glib-test:list-signals "GtkEntryBuffer")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkEntryBuffer" GTK:ENTRY-BUFFER
                      (:SUPERCLASS GOBJECT:OBJECT
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gtk_entry_buffer_get_type")
                      ((LENGTH ENTRY-BUFFER-LENGTH "length" "guint" T NIL)
                       (MAX-LENGTH ENTRY-BUFFER-MAX-LENGTH
                        "max-length" "gint" T T)
                       (TEXT ENTRY-BUFFER-TEXT "text" "gchararray" T T)))
             (gobject:get-gtype-definition "GtkEntryBuffer"))))

;;; --- Signals ----------------------------------------------------------------

;;;     deleted-text

(test gtk-entry-buffer-deleted-text-signal
  (let* ((name "deleted-text") (gtype "GtkEntryBuffer")
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (string= gtype (g:type-name (g:signal-query-owner-type query))))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '("guint" "guint")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     inserted-text

(test gtk-entry-buffer-inserted-text-signal
  (let* ((name "inserted-text") (gtype "GtkEntryBuffer")
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (string= gtype (g:type-name (g:signal-query-owner-type query))))
    ;; Check flags
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '("guint" "gchararray" "guint")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Properties -------------------------------------------------------------

(test gtk-entry-buffer-properties
  (glib-test:with-check-memory (buffer)
    (is (typep (setf buffer (make-instance 'gtk:entry-buffer))
               'gtk:entry-buffer))
    (is (= 0 (gtk:entry-buffer-length buffer)))
    (signals (error) (setf (gtk:entry-buffer-length buffer) 10))
    (is (=  0 (gtk:entry-buffer-max-length buffer)))
    (is (= 10 (setf (gtk:entry-buffer-max-length buffer) 10)))
    (is (string= "" (gtk:entry-buffer-text buffer)))
    (is (string= "text" (setf (gtk:entry-buffer-text buffer) "text")))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_entry_buffer_new

(test gtk-entry-buffer-new
  (glib-test:with-check-memory (buffer)
    (is (typep (setf buffer
                     (gtk:entry-buffer-new)) 'gtk:entry-buffer))
    (is (string= "" (gtk:entry-buffer-text (gtk:entry-buffer-new))))
    (is (string= "This is text."
                 (gtk:entry-buffer-text (gtk:entry-buffer-new "This is text."))))))

;;;     gtk_entry_buffer_get_bytes

(test gtk-entry-buffer-bytes
  (glib-test:with-check-memory ()
    (is (= 13 (gtk:entry-buffer-length (gtk:entry-buffer-new "This is text."))))
    (is (= 13 (gtk:entry-buffer-bytes (gtk:entry-buffer-new "This is text."))))
    (is (= 7 (gtk:entry-buffer-length (gtk:entry-buffer-new "Ägypten"))))
    (is (= 8 (gtk:entry-buffer-bytes (gtk:entry-buffer-new "Ägypten"))))))

;;;     gtk_entry_buffer_insert_text
;;;     gtk_entry_buffer_delete_text

(test gtk-entry-buffer-insert/delete-text
  (glib-test:with-check-memory (buffer)
    (is (typep (setf buffer (make-instance 'gtk:entry-buffer))
               'gtk:entry-buffer))
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

(test gtk-entry-buffer-emit-deleted-text
  (glib-test:with-check-memory (buffer)
    (is (typep (setf buffer (gtk:entry-buffer-new "first second third"))
               'gtk:entry-buffer))
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

(test gtk-entry-buffer-emit-inserted-text
  (glib-test:with-check-memory (buffer)
    (is (typep (setf buffer
                     (gtk:entry-buffer-new "first second third")) 'gtk:entry-buffer))
    (g:signal-connect buffer "inserted-text"
        (lambda (object position text n-chars)
          (is (eq buffer object))
          (is (= 6 position))
          (is (equal "text" text))
          (is (= 7 n-chars))
          nil))
    (gtk:entry-buffer-emit-inserted-text buffer 6 "text" 7)))

;;; 2025-1-5
