(in-package :gtk-test)

(def-suite gtk-layout-manager :in gtk-layout-managers)
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
  (glib-test:with-check-memory (layout button)
    (is (typep (setf button (gtk:button-new-with-label "Button")) 'gtk:button))
    (is (typep (setf layout (gtk:widget-layout-manager button)) 'gtk:bin-layout))
    (is (equal '(47 47 -1 -1)
               (multiple-value-list
                   (gtk:layout-manager-measure layout button :horizontal -1))))
    ;; Remove references
    (is-false (setf (gtk:widget-layout-manager button) nil))))

#+windows
(test gtk-layout-manager-measure.1
  (glib-test:with-check-memory (layout button)
    (is (typep (setf button (gtk:button-new-with-label "Button")) 'gtk:button))
    (is (typep (setf layout (gtk:widget-layout-manager button)) 'gtk:bin-layout))
    (is (equal '(36 36 -1 -1)
               (multiple-value-list
                   (gtk:layout-manager-measure layout button :horizontal -1))))
    ;; Remove references
    (is-false (setf (gtk:widget-layout-manager button) nil))))

(test gtk-layout-manager-measure.2
  (glib-test:with-check-memory (layout box button)
    (is (typep (setf button (gtk:button-new-with-label "Button")) 'gtk:button))
    (is (typep (setf box (gtk:box-new :vertical)) 'gtk:box))
    (is (typep (setf layout (gtk:widget-layout-manager box)) 'gtk:box-layout))
    (is-false (gtk:box-append box button))
    (is (equal '(34 34 -1 -1)
               (multiple-value-list
                   (gtk:layout-manager-measure layout box :vertical -1))))
    #-windows
    (is (equal '(81 81 -1 -1)
               (multiple-value-list
                   (gtk:layout-manager-measure layout box :horizontal -1))))
    #+windows
    (is (equal '(70 70 -1 -1)
               (multiple-value-list
                   (gtk:layout-manager-measure layout box :horizontal -1))))
    ;; Remove references
    (is-false (gtk:box-remove box button))
    (is-false (setf (gtk:widget-layout-manager box) nil))))

;;;     gtk_layout_manager_allocate

;; TODO: Implement a working test

(test gtk-layout-manager-allocate
 (glib-test:with-check-memory (layout box button)
    (is (typep (setf button (gtk:button-new-with-label "Button")) 'gtk:button))
    (is (typep (setf box (gtk:box-new :vertical)) 'gtk:box))
    (is (typep (setf layout (gtk:widget-layout-manager box)) 'gtk:box-layout))
    (is-false (gtk:box-append box button))
    (is-false (gtk:layout-manager-allocate layout box 200 100 -1))
    (is-false (gtk:layout-manager-layout-changed layout))
    ;; Does not change the values of the MEASURE function
    (is (equal '(34 34 -1 -1)
               (multiple-value-list
                   (gtk:layout-manager-measure layout box :vertical -1))))
    #-windows
    (is (equal '(81 81 -1 -1)
               (multiple-value-list
                   (gtk:layout-manager-measure layout box :horizontal -1))))
    #+windows
    (is (equal '(70 70 -1 -1)
               (multiple-value-list
                   (gtk:layout-manager-measure layout box :horizontal -1))))
    (is-false (gtk:box-remove box button))
    (is-false (setf (gtk:widget-layout-manager box) nil))))

;;;     gtk_layout_manager_get_request_mode

(test gtk-layout-manager-request-mode.1
  (glib-test:with-check-memory (layout button)
    (is (typep (setf button (gtk:button-new-with-label "Button")) 'gtk:button))
    (is (typep (setf layout (gtk:widget-layout-manager button)) 'gtk:bin-layout))
    (is (eq :constant-size (gtk:layout-manager-request-mode layout)))
    (is-false (setf (gtk:widget-layout-manager button) nil))))

(test gtk-layout-manager-request-mode.2
  (glib-test:with-check-memory (layout box)
    (is (typep (setf box (gtk:box-new :horizontal)) 'gtk:box))
    (is (typep (setf layout (gtk:widget-layout-manager box)) 'gtk:box-layout))
    (is (eq :constant-size (gtk:layout-manager-request-mode layout)))
    (is-false (setf (gtk:widget-layout-manager box) nil))))

;;;     gtk_layout_manager_get_widget

(test gtk-layout-manager-widget
  (glib-test:with-check-memory (layout button)
    (is (typep (setf button (gtk:button-new-with-label "Button")) 'gtk:button))
    (is (typep (setf layout (gtk:widget-layout-manager button)) 'gtk:bin-layout))
    (is (eq button (gtk:layout-manager-widget layout)))
    (is-false (setf (gtk:widget-layout-manager button) nil))))

;;;     gtk_layout_manager_get_layout_child

(test gtk-layout-manager-layout-child
  (glib-test:with-check-memory (layout grid button)
    (is (typep (setf grid (gtk:grid-new)) 'gtk:grid))
    (is (typep (setf button (gtk:button-new-with-label "Button")) 'gtk:button))
    (is (typep (setf layout (gtk:widget-layout-manager grid)) 'gtk:grid-layout))
    (let (child)
      (is-false (gtk:grid-attach grid button 0 0 2 1))
      (is (typep (setf child
                       (gtk:layout-manager-layout-child layout button))
                 'gtk:layout-child))
      (is-false (gtk:grid-remove grid button))
      (is-false (setf (gtk:widget-layout-manager grid) nil)))))

;;;     gtk_layout_manager_layout_changed

;;; 2026-07-17
