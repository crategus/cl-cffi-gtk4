(in-package :gtk-test)

(def-suite gtk-im-multicontext :in gtk-suite)
(in-suite gtk-im-multicontext)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkIMMulticontext

(test gtk-im-multicontext-class
  ;; Check type
  (is (g:type-is-object "GtkIMMulticontext"))
  ;; Check registered name
  (is (eq 'gtk:im-multicontext
          (glib:symbol-for-gtype "GtkIMMulticontext")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkIMMulticontext")
          (g:gtype (cffi:foreign-funcall "gtk_im_multicontext_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkIMContext")
          (g:type-parent "GtkIMMulticontext")))
  ;; Check children
  (is (equal '()
             (gtk-test:list-children "GtkIMMulticontext")))
  ;; Check interfaces
  (is (equal '()
             (gtk-test:list-interfaces "GtkIMMulticontext")))
  ;; Check class properties
  (is (equal '()
             (gtk-test:list-properties "GtkIMMulticontext")))
  ;; Check signals
  (is (equal '()
             (gtk-test:list-signals "GtkIMMulticontext")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkIMMulticontext"
                                             GTK-I-M-MULTICONTEXT
                       (:SUPERCLASS GTK-I-M-CONTEXT :EXPORT T :INTERFACES NIL)
                       NIL)
             (gobject:get-g-type-definition "GtkIMMulticontext"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_im_multicontext_new

(test gtk-im-multicontext-new
  (let ((method (gtk:im-multicontext-new)))
    (is (typep method 'gtk:im-multicontext))
    (is (equal '() (gtk:im-context-input-hints method)))
    (is (eq :free-form (gtk:im-context-input-purpose method)))))

;;;     gtk_im_multicontext_get_context_id
;;;     gtk_im_multicontext_set_context_id

;;; ... 2023-8-29 --------------------------------------------------------------
