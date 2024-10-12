(in-package :gtk-test)

(def-suite gtk-window-group :in gtk-suite)
(in-suite gtk-window-group)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkWindowGroup

(test gtk-window-group-class
  ;; Check type
  (is (g:type-is-object "GtkWindowGroup"))
  ;; Check registered name
  (is (eq 'gtk:window-group
          (glib:symbol-for-gtype "GtkWindowGroup")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkWindowGroup")
          (g:gtype (cffi:foreign-funcall "gtk_window_group_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkWindowGroup")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkWindowGroup")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkWindowGroup")))
  ;; Check class properties
  (is (equal '()
             (glib-test:list-properties "GtkWindowGroup")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkWindowGroup")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkWindowGroup" GTK:WINDOW-GROUP
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_window_group_get_type")
                       NIL)
             (gobject:get-gtype-definition "GtkWindowGroup"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_window_group_new

(test gtk-window-group-new
  (let (group)
    (is (typep (setf group (gtk:window-group-new)) 'gtk:window-group))
    (is (= 1 (g:object-ref-count group)))))

;;;     gtk_window_group_add_window
;;;     gtk_window_group_remove_window
;;;     gtk_window_group_list_windows

(test gtk-window-group-list-windows
  (let ((group (gtk:window-group-new))
        (window1 (gtk:window-new))
        (window2 (gtk:window-new)))
    (is (= 1 (g:object-ref-count group)))
    ;; The list is empty
    (is (equal '()
               (gtk:window-group-list-windows group)))
    ;; Add a window to the group
    (is-false (gtk:window-group-add-window group window1))
    (is (= 2 (g:object-ref-count group)))
    (is (= 2 (g:object-ref-count window1)))
    (is (equal '(GTK:WINDOw)
               (mapcar #'type-of (gtk:window-group-list-windows group))))
    ;; Add a second window to the group
    (is-false (gtk:window-group-add-window group window2))
    (is (= 3 (g:object-ref-count group)))
    (is (= 2 (g:object-ref-count window2)))
    (is (equal '(GTK:WINDOw GTK:WINDOW)
               (mapcar #'type-of (gtk:window-group-list-windows group))))
    ;; Remove the windows from the group
    (is-false (gtk:window-group-remove-window group window1))
    (is (equal '(GTK:WINDOW)
               (mapcar #'type-of (gtk:window-group-list-windows group))))
    (is-false (gtk:window-group-remove-window group window2))
    (is (equal '()
               (mapcar #'type-of (gtk:window-group-list-windows group))))
    (is-false (gtk:window-destroy window1))
    (is-false (gtk:window-destroy window2))
    (is (= 1 (g:object-ref-count group)))))

;;; 2024-10-9
