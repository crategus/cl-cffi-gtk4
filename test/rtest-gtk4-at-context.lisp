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
  (is (or (equal '()
                 (glib-test:list-children "GtkATContext"))
          (equal '("GtkAtSpiContext")
                 (glib-test:list-children "GtkATContext"))))
  #+windows
  (is (equal '("GtkTestATContext")
             (glib-test:list-children "GtkATContext")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkATContext")))
  ;; Check properties
  (is (equal '("accessible" "accessible-role" "display" "realized")
             (glib-test:list-properties "GtkATContext")))
  ;; Check signals
  (is (equal '("state-change")
             (glib-test:list-signals "GtkATContext")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkATContext" GTK:AT-CONTEXT
                      (:SUPERCLASS GOBJECT:OBJECT
                       :EXPORT T
                       :INTERFACES NIL)
                      ((ACCESSIBLE AT-CONTEXT-ACCESSIBLE "accessible"
                        "GtkAccessible" T NIL)
                       (ACCESSIBLE-ROLE AT-CONTEXT-ACCESSIBLE-ROLE
                        "accessible-role" "GtkAccessibleRole" T T)
                       (DISPLAY AT-CONTEXT-DISPLAY "display" "GdkDisplay" T T)
                       (REALIZED AT-CONTEXT-REALIZED "realized" "gboolean" T NIL)))
             (gobject:get-gtype-definition "GtkATContext"))))

;;; --- Signals ----------------------------------------------------------------

;;;     state-change

(test gtk-at-context-state-changed-signal
  (let* ((name "state-change")
         (gtype (g:gtype "GtkATContext"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Properties -------------------------------------------------------------

(test gtk-at-context-properties
  (glib-test:with-check-memory (context)
    (let ((display (gdk:display-default))
          (button (gtk:button-new)))
      ;; Create AT context
      (setf context (gtk:at-context-create :button button display))
      ;; Property ACCESSIBLE
      (is (equal '(:CONSTRUCT-ONLY :READABLE :WRITABLE)
                 (glib-test:list-param-flags "GtkATContext" "accessible")))
      (is (eq button (gtk:at-context-accessible context)))
      (signals (error) (setf (gtk:at-context-accessible context) (gtk:button-new)))
      ;; Property ACCESSIBLE-ROLE
      (is (equal '(:CONSTRUCT :READABLE :WRITABLE)
                 (glib-test:list-param-flags "GtkATContext" "accessible-role")))
      (is (eq :button (gtk:at-context-accessible-role context)))
      (is (eq :none (setf (gtk:at-context-accessible-role context) :none)))
      ;; Property DISPLAY
      (is (equal '(:READABLE :WRITABLE)
                 (glib-test:list-param-flags "GtkATContext" "display")))
      (is (eq display (gtk:at-context-display context)))
      (is (eq display (setf (gtk:at-context-display context) display)))
      ;; Property REALIZED since 4.22
      #+gtk-4-22
      (is (equal '(:READABLE)
                 (glib-test:list-param-flags "GtkATContext" "realized")))
      #+gtk-4-22
      (is-false (gtk:at-context-realized context))
      #+gtk-4-22
      (signals (error) (setf (gtk:at-context-realized context) t)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_at_context_create

(test gtk-at-context-create
  (glib-test:with-check-memory (context)
    (let ((display (gdk:display-default))
          (button (gtk:button-new)))
      (is (typep (setf context
                       (gtk:at-context-create :button button display))
                 'gtk:at-context)))))

;;; 2026-05-16

