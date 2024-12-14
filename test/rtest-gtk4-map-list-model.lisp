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

;;; --- Functions --------------------------------------------------------------

;;;     GtkMapListModelMapFunc

;;;     gtk_map_list_model_new

#|
         (widgets (gtk:widget-observe-children widget))
         (controllers (gtk:map-list-model-new widgets
                              (lambda (item)
                                (gtk:widget-observe-controllers item))))
         (model (gtk:flatten-list-model-new controllers)))
|#

;; TODO: Finish this example. There is a problem with the return type
;; of the callback function.

#+nil
(test gtk-map-list-model-new
  (let ((box (make-instance 'gtk:box))
        (button nil)
        (widgets nil)
        (controllers nil)
        (model nil))

    (gtk:box-append box (setf button (make-instance 'gtk:button)))
    (gtk:widget-add-controller button
                               (gtk:event-controller-key-new))
    (gtk:box-append box (setf button (make-instance 'gtk:button)))
    (gtk:widget-add-controller button
                               (gtk:event-controller-key-new))

    (is (g:is-object (setf widgets (gtk:widget-observe-children box))))
    (is (g:is-object (setf controllers
                           (gtk:map-list-model-new widgets
                               (lambda (item)
                                 (format t "~&in MAP function for ~a~%" item)
                                 ;; FIXME: What is the correct implementation?
                                 (g:object-pointer
                                     (gtk:widget-observe-controllers item)))))))
    (is (g:is-object (setf model
                           (gtk:flatten-list-model-new controllers))))
    (is (= 8 (gtk:flatten-list-model-n-items model)))
))

;;;     gtk_map_list_model_set_map_func
;;;     gtk_map_list_model_has_map

;;; 2024-12-9
