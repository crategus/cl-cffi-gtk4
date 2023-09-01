(in-package :gtk-test)

(def-suite gtk-text-mark :in gtk-suite)
(in-suite gtk-text-mark)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkTextMark

(test gtk-text-mark-class
  ;; Type check
  (is (g:type-is-object "GtkTextMark"))
  ;; Check the registered name
  (is (eq 'gtk:text-mark
          (glib:symbol-for-gtype "GtkTextMark")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkTextMark")
          (g:gtype (cffi:foreign-funcall "gtk_text_mark_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkTextMark")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkTextMark")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkTextMark")))
  ;; Check the properties
  (is (equal '("left-gravity" "name")
             (list-properties "GtkTextMark")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkTextMark")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkTextMark" GTK-TEXT-MARK
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                                :TYPE-INITIALIZER "gtk_text_mark_get_type")
                               ((LEFT-GRAVITY GTK-TEXT-MARK-LEFT-GRAVITY
                                 "left-gravity" "gboolean" T NIL)
                                (NAME GTK-TEXT-MARK-NAME "name" "gchararray" T
                                 NIL)))
             (gobject:get-g-type-definition "GtkTextMark"))))

;;; --- Properties -------------------------------------------------------------

;;;     left-gravity
;;;     name

;;; --- Functions --------------------------------------------------------------

;;;     gtk_text_mark_new
;;;     gtk_text_mark_set_visible
;;;     gtk_text_mark_get_visible
;;;     gtk_text_mark_get_deleted
;;;     gtk_text_mark_get_buffer

;;; --- 2023-8-26 --------------------------------------------------------------
