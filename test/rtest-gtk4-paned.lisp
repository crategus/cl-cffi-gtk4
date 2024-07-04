(in-package :gtk-test)

(def-suite gtk-paned :in gtk-suite)
(in-suite gtk-paned)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPaned

(test gtk-paned-class
  ;; Check type
  (is (g:type-is-object "GtkPaned"))
  ;; Check registered name
  (is (eq 'gtk:paned
          (glib:symbol-for-gtype "GtkPaned")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkPaned")
          (g:gtype (cffi:foreign-funcall "gtk_paned_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkPaned")))
  ;; Check children
  (is (equal '()
             (gtk-test:list-children "GtkPaned")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkOrientable" "GtkAccessibleRange")
             (gtk-test:list-interfaces "GtkPaned")))
  ;; Check properties
  (is (equal '("end-child" "max-position" "min-position" "orientation"
               "position" "position-set" "resize-end-child" "resize-start-child"
               "shrink-end-child" "shrink-start-child" "start-child"
               "wide-handle")
             (gtk-test:list-properties "GtkPaned")))
  ;; Check signals
  (is (equal '("accept-position" "cancel-position" "cycle-child-focus"
               "cycle-handle-focus" "move-handle" "toggle-handle-focus")
             (gtk-test:list-signals "GtkPaned")))
  ;; Check CSS name
  (is (string= "paned"
               (gtk:widget-class-css-name "GtkPaned")))
  ;; Check accessible role
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

(test gtk-paned-properties
  (let* ((start (make-instance 'gtk:box))
         (end (make-instance 'gtk:box))
         (paned (make-instance 'gtk:paned
                               :start-child start
                               :end-child end)))

    (is (eq end (gtk:paned-end-child paned)))
    (is (= 2147483647 (gtk:paned-max-position paned)))
    (is (= 0 (gtk:paned-min-position paned)))
    (is (= 0 (gtk:paned-position paned)))
    (is-false (gtk:paned-position-set paned))
    (is-true (gtk:paned-resize-end-child paned))
    (is-true (gtk:paned-resize-start-child paned))
    (is-true (gtk:paned-shrink-end-child paned))
    (is-true (gtk:paned-shrink-start-child paned))
    (is (eq start (gtk:paned-start-child paned)))
    (is-false (gtk:paned-wide-handle paned))))

;;; --- Signals ----------------------------------------------------------------

;;;     accept-position
;;;     cancel-position
;;;     cycle-child-focus
;;;     cycle-handle-focus
;;;     move-handle
;;;     toggle-handle-focus

;;; --- Functions --------------------------------------------------------------

;;;     gtk_paned_new

(test gtk-paned-new
  (is (typep (gtk:paned-new :vertical) 'gtk:paned)))

;;; 2024-4-22
