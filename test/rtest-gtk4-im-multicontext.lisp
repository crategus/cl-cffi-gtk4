(in-package :gtk-test)

(def-suite gtk-im-multicontext :in gtk-suite)
(in-suite gtk-im-multicontext)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkIMMulticontext

(test gtk-im-multicontext-class
  ;; Type check
  (is (g:type-is-object "GtkIMMulticontext"))
  ;; Check the registered name
  (is (eq 'gtk:im-multicontext
          (glib:symbol-for-gtype "GtkIMMulticontext")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkIMMulticontext")
          (g:gtype (cffi:foreign-funcall "gtk_im_multicontext_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkIMContext")
          (g:type-parent "GtkIMMulticontext")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkIMMulticontext")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkIMMulticontext")))
  ;; Check the class properties
  (is (equal '()
             (list-properties "GtkIMMulticontext")))
  ;; Check the list of signals
  (is (equal '()
             (list-signals "GtkIMMulticontext")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkIMMulticontext" GTK-I-M-MULTICONTEXT
                       (:SUPERCLASS GTK-I-M-CONTEXT :EXPORT T :INTERFACES NIL)
                       NIL)
             (gobject:get-g-type-definition "GtkIMMulticontext"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_im_multicontext_new

(test gtk-im-multicontext-new
  (let ((method (gtk-im-multicontext-new)))
    (is (typep method 'gtk-im-multicontext))
    (is (equal '() (gtk-im-context-input-hints method)))
    (is (eq :free-form (gtk-im-context-input-purpose method)))))

;;;     gtk_im_multicontext_get_context_id
;;;     gtk_im_multicontext_set_context_id

;;; ... 2023-5-29 --------------------------------------------------------------
