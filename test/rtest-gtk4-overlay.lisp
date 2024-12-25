(in-package :gtk-test)

(def-suite gtk-overlay :in gtk-layout-widgets)
(in-suite gtk-overlay)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkOverlay

(test gtk-overlay-class
  ;; Check type
  (is (g:type-is-object "GtkOverlay"))
  ;; Check registered name
  (is (eq 'gtk:overlay
          (glib:symbol-for-gtype "GtkOverlay")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkOverlay")
          (g:gtype (cffi:foreign-funcall "gtk_overlay_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkOverlay")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkOverlay")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (glib-test:list-interfaces "GtkOverlay")))
  ;; Check properties
  (is (equal '("child")
             (glib-test:list-properties "GtkOverlay")))
  ;; Check signals
  (is (equal '("get-child-position")
             (glib-test:list-signals "GtkOverlay")))
  ;; Check CSS name
  (is (string= "overlay"
               (gtk:widget-class-css-name "GtkOverlay")))
  ;; Check accessible role
  (is (eq :WIDGET (gtk:widget-class-accessible-role "GtkOverlay")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkOverlay" GTK:OVERLAY
                       (:SUPERCLASS GTK:WIDGET
                        :EXPORT T
                        :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
                        :TYPE-INITIALIZER "gtk_overlay_get_type")
                       ((CHILD OVERLAY-CHILD "child" "GtkWidget" T T)))
             (gobject:get-gtype-definition "GtkOverlay"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-overlay-child.1
  (let ((overlay (make-instance 'gtk:overlay
                                :child (make-instance 'gtk:box)))
        (child nil))
    (is (typep (setf child
                     (gtk:overlay-child overlay)) 'gtk:box))
    (is (= 1 (g:object-ref-count overlay)))
    (is (= 2 (g:object-ref-count child)))
    (is (= 2 (g:object-ref-count (gtk:overlay-child overlay))))

    (is-false (setf (gtk:overlay-child overlay) nil))))

(test gtk-overlay-child.2
  (let* ((child (make-instance 'gtk:paned))
         (overlay (make-instance 'gtk:overlay :child child)))
    (is (= 1 (g:object-ref-count overlay)))
    (is (= 2 (g:object-ref-count child)))
    (is (= 2 (g:object-ref-count (gtk:overlay-child overlay))))

    (is-false (setf (gtk:overlay-child overlay) nil))))

;;; --- Signals ----------------------------------------------------------------

(test gtk-overlay-get-child-position-signal
  (let* ((name "get-child-position")
         (gtype (g:gtype "GtkOverlay"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "gboolean") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("GtkWidget" "GdkRectangle")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_overlay_new

(test gtk-overlay-new
  ;; Create with constructor function
  (is (typep (gtk:overlay-new) 'gtk:overlay))
  (is (= 1 (g:object-ref-count (gtk:overlay-new))))
  (is-false (gtk:overlay-child (gtk:overlay-new)))
  ;; Create with make-instance
  (is (typep (make-instance 'gtk:overlay) 'gtk:overlay))
  (is (= 1 (g:object-ref-count (make-instance 'gtk:overlay))))
  (is-false (gtk:overlay-child (make-instance 'gtk:overlay)))
  ;; Create with g:object-new
  (is (typep (g:object-new "GtkOverlay") 'gtk:overlay))
  (is (= 1 (g:object-ref-count (g:object-new "GtkOverlay"))))
  (is-false (gtk:overlay-child (g:object-new "GtkOverlay"))))

;;;     gtk_overlay_add_overlay
;;;     gtk_overlay_remove_overlay

(test gtk-overlay-add/remove-overlay
  (let ((overlay (make-instance 'gtk:overlay
                                :child (make-instance 'gtk:box)))
        (button (make-instance 'gtk:button))
        (label (make-instance 'gtk:label)))
    ;; Check ref count for the overlay widgets
    (is (= 1 (g:object-ref-count button)))
    (is (= 1 (g:object-ref-count label)))
    ;; Add overlay widgets
    (is-false (gtk:overlay-add-overlay overlay button))
    (is-false (gtk:overlay-add-overlay overlay label))
    ;; Ref count now 2
    (is (= 2 (g:object-ref-count button)))
    (is (= 2 (g:object-ref-count label)))
    ;; Remove overlay widgets
    (is-false (gtk:overlay-remove-overlay overlay button))
    (is-false (gtk:overlay-remove-overlay overlay label))
    ;; Ref count now 1 again
    (is (= 1 (g:object-ref-count button)))
    (is (= 1 (g:object-ref-count label)))
    (is-false (setf (gtk:overlay-child overlay) nil))))

;;;     gtk_overlay_get_measure_overlay
;;;     gtk_overlay_set_measure_overlay

(test gtk-overlay-measure-overlay
  (let ((overlay (make-instance 'gtk:overlay
                                :child (make-instance 'gtk:box)))
        (button (make-instance 'gtk:button))
        (label (make-instance 'gtk:label)))
    (is-false (gtk:overlay-add-overlay overlay button))
    (is-false (gtk:overlay-add-overlay overlay label))
    (is-false (gtk:overlay-measure-overlay overlay button))
    (is-false (gtk:overlay-measure-overlay overlay label))
    (is-true (setf (gtk:overlay-measure-overlay overlay button) t))
    (is-true (gtk:overlay-measure-overlay overlay button))
    (is-true (setf (gtk:overlay-measure-overlay overlay label) t))
    (is-true (gtk:overlay-measure-overlay overlay label))

    (is-false (gtk:overlay-remove-overlay overlay button))
    (is-false (gtk:overlay-remove-overlay overlay label))
    (is-false (setf (gtk:overlay-child overlay) nil))))

;;;     gtk_overlay_get_clip_overlay
;;;     gtk_overlay_set_clip_overlay

(test gtk-overlay-clip-overlay
  (let ((overlay (make-instance 'gtk:overlay
                                :child (make-instance 'gtk:box)))
        (button (make-instance 'gtk:button))
        (label (make-instance 'gtk:label)))
    (is-false (gtk:overlay-add-overlay overlay button))
    (is-false (gtk:overlay-add-overlay overlay label))
    (is-false (gtk:overlay-clip-overlay overlay button))
    (is-false (gtk:overlay-clip-overlay overlay label))
    (is-true (setf (gtk:overlay-clip-overlay overlay button) t))
    (is-true (gtk:overlay-clip-overlay overlay button))
    (is-true (setf (gtk:overlay-clip-overlay overlay label) t))
    (is-true (gtk:overlay-clip-overlay overlay label))

    (is-false (gtk:overlay-remove-overlay overlay button))
    (is-false (gtk:overlay-remove-overlay overlay label))
    (is-false (setf (gtk:overlay-child overlay) nil))))

;;; 2024-10-10
