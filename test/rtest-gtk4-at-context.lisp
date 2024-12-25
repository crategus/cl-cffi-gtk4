(in-package :gtk-test)

(def-suite gtk-at-context :in gtk-accessibility)
(in-suite gtk-at-context)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkATContext

(test gtk-at-context-class
  ;; Check type
  (is (g:type-is-object "GtkATContext"))
  ;; Check registered name
  (is (eq 'gtk:at-context
          (glib:symbol-for-gtype "GtkATContext")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkATContext")
          (g:gtype (cffi:foreign-funcall "gtk_at_context_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkATContext")))
  ;; Check children
  #-windows
  (is (equal '("GtkAtSpiContext")
             (glib-test:list-children "GtkATContext")))
  #+windows
  (is (equal '("GtkTestATContext")
             (glib-test:list-children "GtkATContext")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkATContext")))
  ;; Check properties
  (is (equal '("accessible" "accessible-role" "display")
             (glib-test:list-properties "GtkATContext")))
  ;; Check signals
  (is (equal '("state-change")
             (glib-test:list-signals "GtkATContext")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkATContext" GTK:AT-CONTEXT
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES NIL)
                       ((ACCESSIBLE AT-CONTEXT-ACCESSIBLE
                         "accessible" "GtkAccessible" T NIL)
                        (ACCESSIBLE-ROLE AT-CONTEXT-ACCESSIBLE-ROLE
                         "accessible-role" "GtkAccessibleRole" T T)
                        (DISPLAY AT-CONTEXT-DISPLAY
                         "display" "GdkDisplay" T T)))
             (gobject:get-gtype-definition "GtkATContext"))))

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

;;; 2024-7-4
