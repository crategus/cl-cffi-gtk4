(in-package :gtk-test)

(def-suite gtk-layout-manager :in gtk-suite)
(in-suite gtk-layout-manager)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkLayoutManager

(test gtk-layout-manager-class
  ;; Type check
  (is (g:type-is-object "GtkLayoutManager"))
  ;; Check the registered name
  (is (eq 'gtk:layout-manager
          (gobject:symbol-for-gtype "GtkLayoutManager")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkLayoutManager")
          (g:gtype (cffi:foreign-funcall "gtk_layout_manager_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkLayoutManager")))
  ;; Check the children
  (is (equal '("GtkBinLayout" "GtkBoxLayout" "GtkCenterLayout" "GtkFixedLayout"
               "GtkGridLayout" "GtkOverlayLayout" "GtkCustomLayout"
               "GtkConstraintLayout")
             (list-children "GtkLayoutManager")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkLayoutManager")))
  ;; Check the class properties
  (is (equal '()
             (list-properties "GtkLayoutManager")))
  ;; Check the list of signals
  (is (equal '()
             (list-signals "GtkLayoutManager")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkLayoutManager" GTK-LAYOUT-MANAGER
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_layout_manager_get_type")
                       NIL)
             (get-g-type-definition "GtkLayoutManager"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_layout_manager_measure

(test gtk-layout-manager-measure.1
  (let* ((button (make-instance 'gtk:button :label "Button"))
         (layout (gtk-widget-layout-manager button)))
    (is (typep layout 'gtk-layout-manager))
    (is (equal '(46 46 -1 -1)
               (multiple-value-list
                   (gtk-layout-manager-measure layout button :horizontal -1))))))

(test gtk-layout-manager-measure.2
  (let* ((button (make-instance 'gtk:button :label "Button"))
         (box (make-instance 'gtk-box :orientation :vertical))
         (layout (gtk-widget-layout-manager box)))
    (is-false (gtk-box-append box button))
    (is (typep layout 'gtk-layout-manager))
    (is (equal '(17 17 -1 -1)
               (multiple-value-list
                   (gtk-layout-manager-measure layout button :vertical -1))))))

;;;     gtk_layout_manager_allocate

;; FIXME: This is not a working test.

(test gtk-layout-manager-allocate
  (let* ((button (make-instance 'gtk:button :label "Button"))
         (box (make-instance 'gtk-box :orientation :vertical))
         (layout (gtk-widget-layout-manager box)))
    (is-false (gtk-box-append box button))
    (is (equal '(34 34 -1 -1)
               (multiple-value-list
                   (gtk-layout-manager-measure layout box :vertical -1))))
    (is-false (gtk-layout-manager-allocate layout box 200 100 -1))
    (is (equal '(34 34 -1 -1)
               (multiple-value-list
                   (gtk-layout-manager-measure layout box :vertical -1))))))

;;;     gtk_layout_manager_get_request_mode

(test gtk-layout-manager-request-mode
  (let* ((button (make-instance 'gtk:button :label "Button"))
         (layout (gtk-widget-layout-manager button)))
    (is (eq :constant-size (gtk-layout-manager-request-mode layout)))))

;;;     gtk_layout_manager_get_widget

(test gtk-layout-manager-widget
  (let* ((button (make-instance 'gtk:button :label "Button"))
         (layout (gtk-widget-layout-manager button)))
    (is (eq button (gtk-layout-manager-widget layout)))))

;;;     gtk_layout_manager_get_layout_child

(test gtk-layout-manager-layout-child
  (let* ((grid (make-instance 'gtk-grid :orientation :vertical))
         (button (make-instance 'gtk:button :label "Button"))
         (layout (gtk-widget-layout-manager grid)))
    (is-false (gtk-grid-attach grid button 0 0 2 1))
    (is (typep (gtk-layout-manager-layout-child layout button)
               'gtk-layout-child))))

;;;     gtk_layout_manager_layout_changed

;;; --- 2023-3-18 --------------------------------------------------------------
