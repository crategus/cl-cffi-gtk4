(in-package :gtk-test)

(def-suite gtk-text-mark :in gtk-multiline-editor)
(in-suite gtk-text-mark)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkTextMark

(test gtk-text-mark-class
  ;; Check type
  (is (g:type-is-object "GtkTextMark"))
  ;; Check registered name
  (is (eq 'gtk:text-mark
          (glib:symbol-for-gtype "GtkTextMark")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkTextMark")
          (g:gtype (cffi:foreign-funcall "gtk_text_mark_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkTextMark")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkTextMark")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkTextMark")))
  ;; Check properties
  (is (equal '("left-gravity" "name")
             (glib-test:list-properties "GtkTextMark")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkTextMark")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkTextMark" GTK:TEXT-MARK
                      (:SUPERCLASS G:OBJECT
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gtk_text_mark_get_type")
                      ((LEFT-GRAVITY TEXT-MARK-LEFT-GRAVITY
                        "left-gravity" "gboolean" T NIL)
                       (NAME TEXT-MARK-NAME "name" "gchararray" T NIL)))
             (gobject:get-gtype-definition "GtkTextMark"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-text-mark-properties
  (let ((mark (make-instance 'gtk:text-mark)))
    (is-false (gtk:text-mark-left-gravity mark))
    (is-false (gtk:text-mark-name mark))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_text_mark_new

(test gtk-text-mark-new
  (is (typep (gtk:text-mark-new "mark") 'gtk:text-mark))
  (is (typep (gtk:text-mark-new "mark" t) 'gtk:text-mark))
  (is (typep (gtk:text-mark-new "mark" nil) 'gtk:text-mark))
  (is (typep (gtk:text-mark-new nil) 'gtk:text-mark))
  (is (typep (gtk:text-mark-new nil t) 'gtk:text-mark))
  (is (typep (gtk:text-mark-new nil nil) 'gtk:text-mark)))

;;;     gtk_text_mark_set_visible
;;;     gtk_text_mark_get_visible

(test gtk-text-mark-visible
  (let ((mark (gtk:text-mark-new "mark")))
    (is-false (gtk:text-mark-visible mark))
    (is-true (setf (gtk:text-mark-visible mark) t))
    (is-true (gtk:text-mark-visible mark))))

;;;     gtk_text_mark_get_deleted
;;;     gtk_text_mark_get_buffer

(test gtk-text-mark-deleted
  (let* ((buffer (make-instance 'gtk:text-buffer
                                :text "Some text in the text buffer."))
         (iter (gtk:text-buffer-start-iter buffer))
         (mark (gtk:text-mark-new "mark")))

    (is-false (gtk:text-buffer-add-mark buffer mark iter))
    (is (eq buffer (gtk:text-mark-buffer mark)))
    (is-false (gtk:text-mark-deleted mark))

    (is-false (gtk:text-buffer-delete-mark buffer mark))
    (is-false (gtk:text-mark-buffer mark))
    (is-true (gtk:text-mark-deleted mark))))

;;; 2024-9-20
