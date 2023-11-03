(in-package :gtk-test)

(def-suite gtk-paned :in gtk-suite)
(in-suite gtk-paned)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPaned

(test paned-class
  ;; Type check
  (is (g:type-is-object "GtkPaned"))
  ;; Check the registered name
  (is (eq 'gtk:paned
          (glib:symbol-for-gtype "GtkPaned")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkPaned")
          (g:gtype (cffi:foreign-funcall "gtk_paned_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkPaned")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkPaned")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkOrientable" "GtkAccessibleRange")
             (list-interfaces "GtkPaned")))
  ;; Check the properties
  (is (equal '("end-child" "max-position" "min-position" "orientation"
               "position" "position-set" "resize-end-child" "resize-start-child"
               "shrink-end-child" "shrink-start-child" "start-child"
               "wide-handle")
             (list-properties "GtkPaned")))
  ;; Check the signals
  (is (equal '("accept-position" "cancel-position" "cycle-child-focus"
               "cycle-handle-focus" "move-handle" "toggle-handle-focus")
             (list-signals "GtkPaned")))
  ;; CSS name
  (is (string= "paned"
               (gtk:widget-class-css-name "GtkPaned")))
  ;; CSS classes
  (is (equal '("horizontal")
             (gtk:widget-css-classes (make-instance 'gtk:paned))))
  ;; Accessible role
  (is (eq :widget (gtk:widget-class-accessible-role "GtkPaned")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkPaned" GTK-PANED
                       (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkAccessibleRange" "GtkBuildable"
                         "GtkConstraintTarget" "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_paned_get_type")
                       ((END-CHILD GTK-PANED-END-CHILD "end-child" "GtkWidget"
                         T T)
                        (MAX-POSITION GTK-PANED-MAX-POSITION "max-position"
                         "gint" T NIL)
                        (MIN-POSITION GTK-PANED-MIN-POSITION "min-position"
                         "gint" T NIL)
                        (POSITION GTK-PANED-POSITION "position" "gint" T T)
                        (POSITION-SET GTK-PANED-POSITION-SET "position-set"
                         "gboolean" T T)
                        (RESIZE-END-CHILD GTK-PANED-RESIZE-END-CHILD
                         "resize-end-child" "gboolean" T T)
                        (RESIZE-START-CHILD GTK-PANED-RESIZE-START-CHILD
                         "resize-start-child" "gboolean" T T)
                        (SHRINK-END-CHILD GTK-PANED-SHRINK-END-CHILD
                         "shrink-end-child" "gboolean" T T)
                        (SHRINK-START-CHILD GTK-PANED-SHRINK-START-CHILD
                         "shrink-start-child" "gboolean" T T)
                        (START-CHILD GTK-PANED-START-CHILD "start-child"
                         "GtkWidget" T T)
                        (WIDE-HANDLE GTK-PANED-WIDE-HANDLE "wide-handle"
                         "gboolean" T T)))
             (gobject:get-g-type-definition "GtkPaned"))))

;;; --- Properties -------------------------------------------------------------

;;;     end-child
;;;     max-position
;;;     min-position
;;;     position
;;;     position-set
;;;     resize-end-child
;;;     resize-start-child
;;;     shrink-end-child
;;;     shrink-start-child
;;;     start-child
;;;     wide-handle

;;; --- Signals ----------------------------------------------------------------

;;;     accept-position
;;;     cancel-position
;;;     cycle-child-focus
;;;     cycle-handle-focus
;;;     move-handle
;;;     toggle-handle-focus

;;; --- Functions --------------------------------------------------------------

;;;     gtk_paned_new

;;; --- 2023-5-29 --------------------------------------------------------------
