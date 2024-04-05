(in-package :gtk-test)

(def-suite gtk-single-selection :in gtk-suite)
(in-suite gtk-single-selection)

;;; --- Types and Values -------------------------------------------------------

;;;     GTK_INVALID_LIST_POSITION

(test gtk-invalid-list-position
  (is (= 4294967295 gtk:+invalid-list-position+)))

;;;     GtkSingleSelection

(test gtk-single-selection-class
  ;; Type check
  (is (g:type-is-object "GtkSingleSelection"))
  ;; Check the registered name
  (is (eq 'gtk:single-selection
          (glib:symbol-for-gtype "GtkSingleSelection")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkSingleSelection")
          (g:gtype (cffi:foreign-funcall "gtk_single_selection_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkSingleSelection")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkSingleSelection")))
  ;; Check the interfaces
  (is (equal '("GListModel" "GtkSectionModel" "GtkSelectionModel")
             (list-interfaces "GtkSingleSelection")))
  ;; Check the properties
  (is (equal '("autoselect" "can-unselect" "item-type" "model" "n-items"
               "selected" "selected-item")
             (list-properties "GtkSingleSelection")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkSingleSelection")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkSingleSelection"
                                             GTK-SINGLE-SELECTION
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                                ("GListModel" "GtkSectionModel"
                                 "GtkSelectionModel")
                                :TYPE-INITIALIZER
                                "gtk_single_selection_get_type")
                               ((AUTOSELECT GTK-SINGLE-SELECTION-AUTOSELECT
                                 "autoselect" "gboolean" T T)
                                (CAN-UNSELECT GTK-SINGLE-SELECTION-CAN-UNSELECT
                                 "can-unselect" "gboolean" T T)
                                (ITEM-TYPE GTK-SINGLE-SELECTION-ITEM-TYPE
                                 "item-type" "GType" T NIL)
                                (MODEL GTK-SINGLE-SELECTION-MODEL "model"
                                 "GListModel" T T)
                                (N-ITEMS GTK-SINGLE-SELECTION-N-ITEMS "n-items"
                                 "guint" T NIL)
                                (SELECTED GTK-SINGLE-SELECTION-SELECTED
                                 "selected" "guint" T T)
                                (SELECTED-ITEM
                                 GTK-SINGLE-SELECTION-SELECTED-ITEM
                                 "selected-item" "GObject" T NIL)))
             (gobject:get-g-type-definition "GtkSingleSelection"))))

;;; --- Properties -------------------------------------------------------------

;;;     autoselect
;;;     can-unselect
;;;     item-type                                          Since 4.8
;;;     model
;;;     n-items                                            Since 4.8
;;;     selected
;;;     selected-item

(test gtk-single-selection-properties
  (let ((selection (gtk:single-selection-new (g:list-store-new "GObject"))))
    (is-true (gtk:single-selection-autoselect selection))
    (is-false (gtk:single-selection-can-unselect selection))
    (is (eq (g:gtype "GObject")
            (gtk:single-selection-item-type selection)))
    (is (typep (gtk:single-selection-model selection) 'g:list-store))
    (is (= 0 (gtk:single-selection-n-items selection)))
    (is (= gtk:+invalid-list-position+
           (gtk:single-selection-selected selection)))
    (is-false (gtk:single-selection-selected-item selection))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_single_selection_new

(test gtk-single-selection-new
  (is (typep (gtk:single-selection-new) 'gtk:single-selection))
  (is (typep (gtk:single-selection-new nil) 'gtk:single-selection))
  (is (typep (gtk:single-selection-new (g:list-store-new "GObject"))
             'gtk:single-selection)))

;;; --- 2023-11-26 -------------------------------------------------------------
