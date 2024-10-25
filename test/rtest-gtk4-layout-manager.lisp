(in-package :gtk-test)

(def-suite gtk-layout-manager :in gtk-suite)
(in-suite gtk-layout-manager)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkLayoutManager

(test gtk-layout-manager-class
  ;; Check type
  (is (g:type-is-object "GtkLayoutManager"))
  ;; Check registered name
  (is (eq 'gtk:layout-manager
          (glib:symbol-for-gtype "GtkLayoutManager")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkLayoutManager")
          (g:gtype (cffi:foreign-funcall "gtk_layout_manager_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkLayoutManager")))
  ;; Check children
  (is (equal '("GtkBinLayout" "GtkBoxLayout" "GtkCenterLayout"
               "GtkConstraintLayout" "GtkCustomLayout" "GtkFixedLayout"
               "GtkGridLayout" "GtkOverlayLayout")
             (glib-test:list-children "GtkLayoutManager")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkLayoutManager")))
  ;; Check class properties
  (is (equal '()
             (glib-test:list-properties "GtkLayoutManager")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkLayoutManager")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkLayoutManager" GTK:LAYOUT-MANAGER
                       (:SUPERCLASS GOBJECT:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_layout_manager_get_type")
                       NIL)
             (gobject:get-gtype-definition "GtkLayoutManager"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_layout_manager_measure

#-windows
(test gtk-layout-manager-measure.1
  (let* ((button (make-instance 'gtk:button :label "Button"))
         (layout (gtk:widget-layout-manager button)))
    (is (typep layout 'gtk:bin-layout))
    (is (equal '(47 47 -1 -1)
               (multiple-value-list
                   (gtk:layout-manager-measure layout button :horizontal -1))))
    (is-false (setf (gtk:widget-layout-manager button) nil))
    ;; Check refcount
    (is (= 1 (g:object-ref-count button)))
    (is (= 1 (g:object-ref-count layout)))))

#+windows
(test gtk-layout-manager-measure.1
  (let* ((button (make-instance 'gtk:button :label "Button"))
         (layout (gtk:widget-layout-manager button)))
    (is (typep layout 'gtk:bin-layout))
    (is (equal '(36 36 -1 -1)
               (multiple-value-list
                   (gtk:layout-manager-measure layout button :horizontal -1))))))

(test gtk-layout-manager-measure.2
  (let* ((button (make-instance 'gtk:button :label "Button"))
         (box (make-instance 'gtk:box :orientation :vertical))
         (layout (gtk:widget-layout-manager box)))
    (is-false (gtk:box-append box button))
    (is (typep layout 'gtk:box-layout))
    (is (equal '(17 17 -1 -1)
               (multiple-value-list
                   (gtk:layout-manager-measure layout button :vertical -1))))
    (is-false (gtk:box-remove box button))
    (is-false (setf (gtk:widget-layout-manager box) nil))
    ;; Check refcount
    (is (= 1 (g:object-ref-count button)))
    (is (= 1 (g:object-ref-count box)))
    (is (= 1 (g:object-ref-count layout)))))

;;;     gtk_layout_manager_allocate

;; TODO: Implement a working test

(test gtk-layout-manager-allocate
  (let* ((button (make-instance 'gtk:button :label "Button"))
         (box (make-instance 'gtk:box :orientation :vertical))
         (layout (gtk:widget-layout-manager box)))
    (is-false (gtk:box-append box button))
    (is (equal '(34 34 -1 -1)
               (multiple-value-list
                   (gtk:layout-manager-measure layout box :vertical -1))))
    (is-false (gtk:layout-manager-allocate layout box 200 100 -1))
    (is (equal '(34 34 -1 -1)
               (multiple-value-list
                   (gtk:layout-manager-measure layout box :vertical -1))))
    (is-false (gtk:box-remove box button))
    (is-false (setf (gtk:widget-layout-manager box) nil))
    ;; Check refcount
    (is (= 1 (g:object-ref-count button)))
    (is (= 1 (g:object-ref-count box)))
    (is (= 1 (g:object-ref-count layout)))))

;;;     gtk_layout_manager_get_request_mode

(test gtk-layout-manager-request-mode.1
  (let* ((button (make-instance 'gtk:button :label "Button"))
         (layout (gtk:widget-layout-manager button)))
    (is (eq :constant-size (gtk:layout-manager-request-mode layout)))
    (is-false (setf (gtk:widget-layout-manager button) nil))
    ;; Check refcount
    (is (= 1 (g:object-ref-count button)))
    (is (= 1 (g:object-ref-count layout)))))

(test gtk-layout-manager-request-mode.2
  (let* ((box (make-instance 'gtk:box :orientation :horizontal))
         (layout (gtk:widget-layout-manager box)))
    (is (eq :constant-size (gtk:layout-manager-request-mode layout)))
    (is-false (setf (gtk:widget-layout-manager box) nil))
    ;; Check refcount
    (is (= 1 (g:object-ref-count box)))
    (is (= 1 (g:object-ref-count layout)))))

;;;     gtk_layout_manager_get_widget

(test gtk-layout-manager-widget
  (let* ((button (make-instance 'gtk:button :label "Button"))
         (layout (gtk:widget-layout-manager button)))
    (is (eq button (gtk:layout-manager-widget layout)))
    (is-false (setf (gtk:widget-layout-manager button) nil))
    ;; Check refcount
    (is (= 1 (g:object-ref-count button)))
    (is (= 1 (g:object-ref-count layout)))))

;;;     gtk_layout_manager_get_layout_child

(test gtk-layout-manager-layout-child
  (let* ((grid (make-instance 'gtk:grid :orientation :vertical))
         (button (make-instance 'gtk:button :label "Button"))
         (layout (gtk:widget-layout-manager grid))
         (child nil))
    (is-false (gtk:grid-attach grid button 0 0 2 1))
    (is (typep (setf child
                     (gtk:layout-manager-layout-child layout button))
               'gtk:layout-child))
    (is-false (gtk:grid-remove grid button))
    (is-false (setf (gtk:widget-layout-manager grid) nil))
    ;; Check refcount
    (is (= 1 (g:object-ref-count grid)))
    (is (= 1 (g:object-ref-count button)))
    (is (= 1 (g:object-ref-count layout)))
    (is (= 1 (g:object-ref-count child)))))

;;;     gtk_layout_manager_layout_changed

;;; 2024-10-13
