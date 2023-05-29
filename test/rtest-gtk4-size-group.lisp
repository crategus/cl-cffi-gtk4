(in-package :gtk-test)

(def-suite gtk-size-group :in gtk-suite)
(in-suite gtk-size-group)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkSizeGroupMode

(test gtk-size-group-mode
  ;; Check the type
  (is (g:type-is-enum "GtkSizeGroupMode"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkSizeGroupMode")
          (g:gtype (cffi:foreign-funcall "gtk_size_group_mode_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:size-group-mode
          (glib:symbol-for-gtype "GtkSizeGroupMode")))
  ;; Check the names
  (is (equal '("GTK_SIZE_GROUP_NONE" "GTK_SIZE_GROUP_HORIZONTAL"
               "GTK_SIZE_GROUP_VERTICAL" "GTK_SIZE_GROUP_BOTH")
             (list-enum-item-name "GtkSizeGroupMode")))
  ;; Check the values
  (is (equal '(0 1 2 3)
             (list-enum-item-value "GtkSizeGroupMode")))
  ;; Check the nick names
  (is (equal '("none" "horizontal" "vertical" "both")
             (list-enum-item-nick "GtkSizeGroupMode")))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkSizeGroupMode"
                             GTK-SIZE-GROUP-MODE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_size_group_mode_get_type")
                             (:NONE 0)
                             (:HORIZONTAL 1)
                             (:VERTICAL 2)
                             (:BOTH 3))
             (gobject:get-g-type-definition "GtkSizeGroupMode"))))

;;;     GtkSizeGroup

(test gtk-size-group-class
  ;; Type check
  (is (g:type-is-object "GtkSizeGroup"))
  ;; Check the registered name
  (is (eq 'gtk:size-group
          (glib:symbol-for-gtype "GtkSizeGroup")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkSizeGroup")
          (g:gtype (cffi:foreign-funcall "gtk_size_group_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkSizeGroup")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkSizeGroup")))
  ;; Check the interfaces
  (is (equal '("GtkBuildable")
             (list-interfaces "GtkSizeGroup")))
  ;; Check the class properties
  (is (equal '("mode")
             (list-properties "GtkSizeGroup")))
  ;; Check the list of signals
  (is (equal '()
             (list-signals "GtkSizeGroup")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkSizeGroup" GTK-SIZE-GROUP
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                        ("GtkBuildable") :TYPE-INITIALIZER
                        "gtk_size_group_get_type")
                       ((MODE GTK-SIZE-GROUP-MODE "mode" "GtkSizeGroupMode" T
                         T)))
             (gobject:get-g-type-definition "GtkSizeGroup"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-size-group-properties
  (let ((group (make-instance 'gtk:size-group)))
    (is (typep group 'gtk:size-group))
    (is (eq :horizontal (gtk:size-group-mode group)))
    (is (eq :both (setf (gtk:size-group-mode group) :both)))
    (is (eq :both (gtk:size-group-mode group)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_size_group_new

(test gtk-size-group-new
  (let ((group (gtk:size-group-new :both)))
    (is (typep group 'gtk:size-group))
    (is (eq :both (gtk:size-group-mode group)))))

;;;     gtk_size_group_add_widget
;;;     gtk_size_group_remove_widget
;;;     gtk_size_group_get_widgets

(test gtk-size-group-add/remove-widget
  (let ((group (gtk:size-group-new :both))
        (label (make-instance 'gtk:label))
        (button (make-instance 'gtk:button)))
    (is (equal '() (gtk:size-group-widgets group)))
    (is-false (gtk:size-group-add-widget group label))
    (is (equal '("GtkLabel")
                (mapcar (lambda (x)
                          (g:type-name (g:type-from-instance x)))
                        (gtk:size-group-widgets group))))
    (is-false (gtk:size-group-add-widget group button))
    (is (equal '("GtkButton" "GtkLabel")
                (mapcar (lambda (x)
                          (g:type-name (g:type-from-instance x)))
                        (gtk:size-group-widgets group))))
    (is-false (gtk:size-group-remove-widget group label))
    (is (equal '("GtkButton")
                (mapcar (lambda (x)
                          (g:type-name (g:type-from-instance x)))
                        (gtk:size-group-widgets group))))
    (is-false (gtk:size-group-remove-widget group button))
    (is (equal '()
                (mapcar (lambda (x)
                          (g:type-name (g:type-from-instance x)))
                        (gtk:size-group-widgets group))))))

;;; --- 2023-5-29 --------------------------------------------------------------
