(in-package :gtk-test)

(def-suite gtk-size-group :in gtk-miscellaneous)
(in-suite gtk-size-group)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkSizeGroupMode

(test gtk-size-group-mode
  ;; Check type
  (is (g:type-is-enum "GtkSizeGroupMode"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkSizeGroupMode")
          (g:gtype (cffi:foreign-funcall "gtk_size_group_mode_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:size-group-mode
          (glib:symbol-for-gtype "GtkSizeGroupMode")))
  ;; Check names
  (is (equal '("GTK_SIZE_GROUP_NONE" "GTK_SIZE_GROUP_HORIZONTAL"
               "GTK_SIZE_GROUP_VERTICAL" "GTK_SIZE_GROUP_BOTH")
             (glib-test:list-enum-item-names "GtkSizeGroupMode")))
  ;; Check values
  (is (equal '(0 1 2 3)
             (glib-test:list-enum-item-values "GtkSizeGroupMode")))
  ;; Check nick names
  (is (equal '("none" "horizontal" "vertical" "both")
             (glib-test:list-enum-item-nicks "GtkSizeGroupMode")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkSizeGroupMode" GTK:SIZE-GROUP-MODE
                                    (:EXPORT T
                                     :TYPE-INITIALIZER
                                     "gtk_size_group_mode_get_type")
                                    (:NONE 0)
                                    (:HORIZONTAL 1)
                                    (:VERTICAL 2)
                                    (:BOTH 3))
             (gobject:get-gtype-definition "GtkSizeGroupMode"))))

;;;     GtkSizeGroupMode

(test gtk-size-group-mode
  ;; Check type
  (is (g:type-is-enum "GtkSizeGroupMode"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkSizeGroupMode")
          (g:gtype (cffi:foreign-funcall "gtk_size_group_mode_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:size-group-mode
          (glib:symbol-for-gtype "GtkSizeGroupMode")))
  ;; Check names
  (is (equal '("GTK_SIZE_GROUP_NONE" "GTK_SIZE_GROUP_HORIZONTAL"
               "GTK_SIZE_GROUP_VERTICAL" "GTK_SIZE_GROUP_BOTH")
             (glib-test:list-enum-item-names "GtkSizeGroupMode")))
  ;; Check values
  (is (equal '(0 1 2 3)
             (glib-test:list-enum-item-values "GtkSizeGroupMode")))
  ;; Check nick names
  (is (equal '("none" "horizontal" "vertical" "both")
             (glib-test:list-enum-item-nicks "GtkSizeGroupMode")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkSizeGroupMode" GTK:SIZE-GROUP-MODE
                      (:EXPORT T
                       :TYPE-INITIALIZER "gtk_size_group_mode_get_type")
                      (:NONE 0)
                      (:HORIZONTAL 1)
                      (:VERTICAL 2)
                      (:BOTH 3))
             (gobject:get-gtype-definition "GtkSizeGroupMode"))))

;;;     GtkSizeGroup

(test gtk-size-group-class
  ;; Check type
  (is (g:type-is-object "GtkSizeGroup"))
  ;; Check registered name
  (is (eq 'gtk:size-group
          (glib:symbol-for-gtype "GtkSizeGroup")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkSizeGroup")
          (g:gtype (cffi:foreign-funcall "gtk_size_group_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkSizeGroup")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkSizeGroup")))
  ;; Check interfaces
  (is (equal '("GtkBuildable")
             (glib-test:list-interfaces "GtkSizeGroup")))
  ;; Check class properties
  (is (equal '("mode")
             (glib-test:list-properties "GtkSizeGroup")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkSizeGroup")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkSizeGroup" GTK:SIZE-GROUP
                      (:SUPERCLASS GOBJECT:OBJECT
                       :EXPORT T
                       :INTERFACES ("GtkBuildable")
                       :TYPE-INITIALIZER "gtk_size_group_get_type")
                      ((MODE SIZE-GROUP-MODE "mode" "GtkSizeGroupMode" T T)))
             (gobject:get-gtype-definition "GtkSizeGroup"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-size-group-properties
  (glib-test:with-check-memory (group)
    (is (typep (setf group (make-instance 'gtk:size-group)) 'gtk:size-group))
    (is (eq :horizontal (gtk:size-group-mode group)))
    (is (eq :both (setf (gtk:size-group-mode group) :both)))
    (is (eq :both (gtk:size-group-mode group)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_size_group_new

(test gtk-size-group-new
  (glib-test:with-check-memory (group)
    (is (typep (setf group (gtk:size-group-new :both)) 'gtk:size-group))
    (is (eq :both (gtk:size-group-mode group)))))

;;;     gtk_size_group_add_widget
;;;     gtk_size_group_remove_widget
;;;     gtk_size_group_get_widgets

(test gtk-size-group-add/remove-widget
  (glib-test:with-check-memory (group label button)
    ;; Create size group and two widgets
    (setf group (gtk:size-group-new :both))
    (setf label (make-instance 'gtk:label))
    (setf button (make-instance 'gtk:button))
    ;; Add, list, remove widgets from the size group
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

;;; 2025-2-23
