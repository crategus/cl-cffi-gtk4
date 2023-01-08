(def-suite gtk-size-group :in gtk-suite)
(in-suite gtk-size-group)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkSizeGroupMode

(test gtk-size-group-mode
  ;; Check the type
  (is (g-type-is-enum "GtkSizeGroupMode"))
  ;; Check the type initializer
  (is (eq (gtype "GtkSizeGroupMode")
          (gtype (foreign-funcall "gtk_size_group_mode_get_type" g-size))))
  ;; Check the registered name
  (is (eq 'gtk-size-group-mode
          (gobject:symbol-for-gtype "GtkSizeGroupMode")))
  ;; Check the names
  (is (equal '("GTK_SIZE_GROUP_NONE" "GTK_SIZE_GROUP_HORIZONTAL"
               "GTK_SIZE_GROUP_VERTICAL" "GTK_SIZE_GROUP_BOTH")
             (mapcar #'enum-item-name
                     (get-enum-items "GtkSizeGroupMode"))))
  ;; Check the values
  (is (equal '(0 1 2 3)
             (mapcar #'enum-item-value
                     (get-enum-items "GtkSizeGroupMode"))))
  ;; Check the nick names
  (is (equal '("none" "horizontal" "vertical" "both")
             (mapcar #'enum-item-nick
                     (get-enum-items "GtkSizeGroupMode"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkSizeGroupMode"
                             GTK-SIZE-GROUP-MODE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_size_group_mode_get_type")
                             (:NONE 0)
                             (:HORIZONTAL 1)
                             (:VERTICAL 2)
                             (:BOTH 3))
             (get-g-type-definition "GtkSizeGroupMode"))))

;;;     GtkSizeGroup

(test gtk-size-group-class
  ;; Type check
  (is (g:type-is-object "GtkSizeGroup"))
  ;; Check the registered name
  (is (eq 'gtk-size-group
          (gobject:symbol-for-gtype "GtkSizeGroup")))
  ;; Check the type initializer
  (is (eq (gtype "GtkSizeGroup")
          (gtype (foreign-funcall "gtk_size_group_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GObject")
          (g-type-parent "GtkSizeGroup")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "GtkSizeGroup"))))
  ;; Check the interfaces
  (is (equal '("GtkBuildable")
             (mapcar #'g-type-name (g-type-interfaces "GtkSizeGroup"))))
  ;; Check the class properties
  (is (equal '("mode")
             (list-class-property-names "GtkSizeGroup")))
  ;; Check the list of signals
  (is (equal '()
             (sort (mapcar #'g-signal-name
                           (g-signal-list-ids "GtkSizeGroup"))
                   #'string<)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkSizeGroup" GTK-SIZE-GROUP
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                        ("GtkBuildable") :TYPE-INITIALIZER
                        "gtk_size_group_get_type")
                       ((MODE GTK-SIZE-GROUP-MODE "mode" "GtkSizeGroupMode" T
                         T)))
             (get-g-type-definition "GtkSizeGroup"))))

;;; --- Properties -------------------------------------------------------------

;;;     mode

(test gtk-size-group-properties
  (let ((group (make-instance 'gtk-size-group)))
    (is (typep group 'gtk-size-group))
    (is (eq :horizontal (gtk-size-group-mode group)))
    (is (eq :both (setf (gtk-size-group-mode group) :both)))
    (is (eq :both (gtk-size-group-mode group)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_size_group_new

(test gtk-size-group-new
  (let ((group (gtk-size-group-new :both)))
    (is (typep group 'gtk-size-group))
    (is (eq :both (gtk-size-group-mode group)))))

;;;     gtk_size_group_add_widget
;;;     gtk_size_group_remove_widget
;;;     gtk_size_group_get_widgets

(test gtk-size-group-add-widget
  (let ((group (gtk-size-group-new :both))
        (label (make-instance 'gtk-label))
        (button (make-instance 'gtk:button)))
    (is (equal '() (gtk-size-group-widgets group)))
    (is-false (gtk-size-group-add-widget group label))
    (is (equal '("GtkLabel")
                (mapcar (lambda (x)
                          (g-type-name (g-type-from-instance x)))
                        (gtk-size-group-widgets group))))
    (is-false (gtk-size-group-add-widget group button))
    (is (equal '("GtkButton" "GtkLabel")
                (mapcar (lambda (x)
                          (g-type-name (g-type-from-instance x)))
                        (gtk-size-group-widgets group))))
    (is-false (gtk-size-group-remove-widget group label))
    (is (equal '("GtkButton")
                (mapcar (lambda (x)
                          (g-type-name (g-type-from-instance x)))
                        (gtk-size-group-widgets group))))
    (is-false (gtk-size-group-remove-widget group button))
    (is (equal '()
                (mapcar (lambda (x)
                          (g-type-name (g-type-from-instance x)))
                        (gtk-size-group-widgets group))))))

;;; 2022-7-10
