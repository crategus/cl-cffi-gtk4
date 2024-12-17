(in-package :gtk-test)

(def-suite gtk-map-list-model :in gtk-list-model-support)
(in-suite gtk-map-list-model)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkMapListModel

(test gtk-map-list-model-class
  ;; Check type
  (is (g:type-is-object "GtkMapListModel"))
  ;; Check registered name
  (is (eq 'gtk:map-list-model
          (glib:symbol-for-gtype "GtkMapListModel")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkMapListModel")
          (g:gtype (cffi:foreign-funcall "gtk_map_list_model_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkMapListModel")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkMapListModel")))
  ;; Check interfaces
  (is (equal '("GListModel" "GtkSectionModel")
             (glib-test:list-interfaces "GtkMapListModel")))
  ;; Check properties
  (is (equal '("has-map" "item-type" "model" "n-items")
             (glib-test:list-properties "GtkMapListModel")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkMapListModel")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkMapListModel" GTK:MAP-LIST-MODEL
                      (:SUPERCLASS G:OBJECT
                       :EXPORT T
                       :INTERFACES ("GListModel" "GtkSectionModel")
                       :TYPE-INITIALIZER "gtk_map_list_model_get_type")
                      ((HAS-MAP MAP-LIST-MODEL-HAS-MAP "has-map" "gboolean" T NIL)
                       (ITEM-TYPE MAP-LIST-MODEL-ITEM-TYPE
                        "item-type" "GType" T NIL)
                       (MODEL MAP-LIST-MODEL-MODEL "model" "GListModel" T NIL)
                       (N-ITEMS MAP-LIST-MODEL-N-ITEMS
                        "n-items" "guint" T NIL)))
             (gobject:get-gtype-definition "GtkMapListModel"))))

;;; --- Properties -------------------------------------------------------------

;;;     has-map
;;;     item-type                                          Since 4.8
;;;     model
;;;     n-items                                            Since 4.8

(test gtk-map-list-model-properties
  (glib-test:with-check-memory (model)
    (setf model (make-instance 'gtk:map-list-model))
    (is-false (gtk:map-list-model-has-map model))
    (is (eq (g:gtype "GObject") (gtk:map-list-model-item-type model)))
    (is-false (gtk:map-list-model-model model))
    (is (= 0 (gtk:map-list-model-n-items model)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_map_list_model_new
;;;     gtk_map_list_model_has_map

;; FIXME: Does not work! Crashes after about 100 runs.

#+nil
(test gtk-map-list-model-new
  (glib-test:with-check-memory ((box
                                (button1 2)
                                (button2 2)
                                controller1 controller2
                                controllers widgets model) :strong 2)


      (setf box (make-instance 'gtk:box))

      (gtk:box-append box (setf button1 (make-instance 'gtk:button)))
      (gtk:widget-add-controller button1
                                 (setf controller1
                                       (gtk:event-controller-key-new)))
      (gtk:box-append box (setf button2 (make-instance 'gtk:button)))
      (gtk:widget-add-controller button2
                                 (setf controller2
                                       (gtk:event-controller-key-new)))

      (is (g:is-object (setf widgets (gtk:widget-observe-children box))))

      (is (g:is-object (setf controllers
                             (gtk:map-list-model-new widgets
                                 (lambda (item)
                                   (format t "~&  item : ~a~%" item)
                                   (gtk:widget-observe-controllers item))))))

      (is (g:is-object (setf model
                             (gtk:flatten-list-model-new controllers))))

      (is (gtk:map-list-model-has-map controllers))
      (is (= 8 (gtk:flatten-list-model-n-items model)))
      ;; Remove references
      (is-false (setf (gtk:flatten-list-model-model model) nil))

      (is-false (gtk:widget-remove-controller button1 controller1))
      (is-false (gtk:widget-remove-controller button2 controller2))
      (is-false (gtk:box-remove box button1))
      (is-false (gtk:box-remove box button2))))

;;;     gtk_map_list_model_set_map_func

;; FIXME: Does not work!

#+nil
(test gtk-map-list-model-set-map-func
  (glib-test:with-check-memory ((box
                                 (button1 2)
                                 (button2 2)
                                 controller1 controller2 model) :strong 3)
    (let (widgets controllers)

      (setf box (make-instance 'gtk:box))

      (gtk:box-append box (setf button1 (make-instance 'gtk:button)))
      (gtk:widget-add-controller button1
                                 (setf controller1
                                       (gtk:event-controller-key-new)))
      (gtk:box-append box (setf button2 (make-instance 'gtk:button)))
      (gtk:widget-add-controller button2
                                 (setf controller2
                                       (gtk:event-controller-key-new)))

      (is (g:is-object (setf widgets (gtk:widget-observe-children box))))

      (is (g:is-object (setf controllers
                             (gtk:map-list-model-new widgets nil))))

      (is-false (gtk:map-list-model-set-map-func controllers
                        (lambda (item)
                          (gtk:widget-observe-controllers item))))

      (is (g:is-object (setf model
                             (gtk:flatten-list-model-new controllers))))

      (is (gtk:map-list-model-has-map controllers))
      (is (= 8 (gtk:flatten-list-model-n-items model)))
      ;; Remove references
      (is-false (setf (gtk:flatten-list-model-model model) nil))
      (is-false (gtk:widget-remove-controller button1 controller1))
      (is-false (gtk:widget-remove-controller button2 controller2))
      (is-false (gtk:box-remove box button1))
      (is-false (gtk:box-remove box button2)))))

;;; 2024-12-17
