(in-package :gtk-test)

(def-suite gtk-single-selection :in gtk-list-model-support)
(in-suite gtk-single-selection)

;;; --- Types and Values -------------------------------------------------------

;;;     GTK_INVALID_LIST_POSITION

(test gtk-invalid-list-position
  (is (= 4294967295 gtk:+invalid-list-position+)))

;;;     GtkSingleSelection

(test gtk-single-selection-class
  ;; Check type
  (is (g:type-is-object "GtkSingleSelection"))
  ;; Check registered name
  (is (eq 'gtk:single-selection
          (glib:symbol-for-gtype "GtkSingleSelection")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkSingleSelection")
          (g:gtype (cffi:foreign-funcall "gtk_single_selection_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkSingleSelection")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkSingleSelection")))
  ;; Check interfaces
  (is (equal '("GListModel" "GtkSectionModel" "GtkSelectionModel")
             (glib-test:list-interfaces "GtkSingleSelection")))
  ;; Check properties
  (is (equal '("autoselect" "can-unselect" "item-type" "model" "n-items"
               "selected" "selected-item")
             (glib-test:list-properties "GtkSingleSelection")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkSingleSelection")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkSingleSelection" GTK:SINGLE-SELECTION
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES
                        ("GListModel" "GtkSectionModel" "GtkSelectionModel")
                        :TYPE-INITIALIZER "gtk_single_selection_get_type")
                       ((AUTOSELECT SINGLE-SELECTION-AUTOSELECT
                         "autoselect" "gboolean" T T)
                        (CAN-UNSELECT SINGLE-SELECTION-CAN-UNSELECT
                         "can-unselect" "gboolean" T T)
                        (ITEM-TYPE SINGLE-SELECTION-ITEM-TYPE
                         "item-type" "GType" T NIL)
                        (MODEL SINGLE-SELECTION-MODEL "model" "GListModel" T T)
                        (N-ITEMS SINGLE-SELECTION-N-ITEMS "n-items" "guint" T NIL)
                        (SELECTED SINGLE-SELECTION-SELECTED "selected" "guint" T T)
                        (SELECTED-ITEM SINGLE-SELECTION-SELECTED-ITEM
                         "selected-item" "GObject" T NIL)))
             (gobject:get-gtype-definition "GtkSingleSelection"))))

;;; --- Properties -------------------------------------------------------------

;;;     autoselect
;;;     can-unselect
;;;     item-type                                          Since 4.8
;;;     model
;;;     n-items                                            Since 4.8
;;;     selected
;;;     selected-item

(test gtk-single-selection-properties
  (glib-test:with-check-memory (selection)
    (setf selection (gtk:single-selection-new (g:list-store-new "GObject")))
    (is-true (gtk:single-selection-autoselect selection))
    (is-false (gtk:single-selection-can-unselect selection))
    (is (eq (g:gtype "GObject")
            (gtk:single-selection-item-type selection)))
    (is (typep (gtk:single-selection-model selection) 'g:list-store))
    (is (= 0 (gtk:single-selection-n-items selection)))
    (is (= gtk:+invalid-list-position+
           (gtk:single-selection-selected selection)))
    (is-false (gtk:single-selection-selected-item selection))
    ;; Remove references
    (is-false (setf (gtk:single-selection-model selection) nil))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_single_selection_new

(test gtk-single-selection-new
  (glib-test:with-check-memory (selection store)
    (is (typep (setf selection (gtk:single-selection-new)) 'gtk:single-selection))
    (is (typep (setf selection (gtk:single-selection-new nil))
               'gtk:single-selection))
    (is (typep (setf selection
                     (gtk:single-selection-new (setf store
                                                     (g:list-store-new "GObject"))))
               'gtk:single-selection))
    ;; Remove references
    (is-false (setf (gtk:single-selection-model selection) nil))))

;;; 2024-12-17
