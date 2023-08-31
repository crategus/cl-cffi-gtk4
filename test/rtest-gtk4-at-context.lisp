(in-package :gtk-test)

(def-suite gtk-at-context :in gtk-suite)
(in-suite gtk-at-context)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkATContext

(test gtk-at-context-class
  ;; Type check
  (is (g:type-is-object "GtkATContext"))
  ;; Check the registered name
  (is (eq 'gtk:at-context
          (glib:symbol-for-gtype "GtkATContext")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkATContext")
          (g:gtype (cffi:foreign-funcall "gtk_at_context_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkATContext")))
  ;; Check the children
  (is (equal '("GtkAtSpiContext")
             (list-children "GtkATContext")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkATContext")))
  ;; Check the properties
  (is (equal '("accessible" "accessible-role" "display")
             (list-properties "GtkATContext")))
  ;; Check the signals
  (is (equal '("state-change")
             (list-signals "GtkATContext")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkATContext" GTK-A-T-CONTEXT
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL)
                               ((ACCESSIBLE GTK-A-T-CONTEXT-ACCESSIBLE
                                 "accessible" "GtkAccessible" T NIL)
                                (ACCESSIBLE-ROLE
                                 GTK-A-T-CONTEXT-ACCESSIBLE-ROLE
                                 "accessible-role" "GtkAccessibleRole" T T)
                                (DISPLAY GTK-A-T-CONTEXT-DISPLAY "display"
                                 "GdkDisplay" T T)))
             (gobject:get-g-type-definition "GtkATContext"))))

;;; --- Properties -------------------------------------------------------------

;;;     accessible
;;;     accessible-role
;;;     display

(test gtk-at-context-properties
  (let* ((display (gdk:display-default))
         (button (make-instance 'gtk:button))
         (context (gtk:at-context-create :button button display)))
    (is (typep (gtk:at-context-accessible context) 'gtk:button))
    (is (eq :button (gtk:at-context-accessible-role context)))
    (is (typep (gtk:at-context-display context) 'gdk:display))))

;;; --- Signals ----------------------------------------------------------------

;;;     state-change

;;; --- Functions --------------------------------------------------------------

;;;     gtk_at_context_create

(test gtk-at-context-create
  (let ((display (gdk:display-default))
        (button (make-instance 'gtk:button)))
    (is (typep (gtk:at-context-create :button button display) 'gtk:at-context))))

;;; --- 2023-8-31 --------------------------------------------------------------
