(in-package :gtk-test)

(def-suite gtk-multi-selection :in gtk-suite)
(in-suite gtk-multi-selection)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkMultiSelection

(test gtk-multi-selection-class
  ;; Type check
  (is (g:type-is-object "GtkMultiSelection"))
  ;; Check the registered name
  (is (eq 'gtk:multi-selection
          (glib:symbol-for-gtype "GtkMultiSelection")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkMultiSelection")
          (g:gtype (cffi:foreign-funcall "gtk_multi_selection_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkMultiSelection")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkMultiSelection")))
  ;; Check the interfaces
  (is (equal '("GListModel" "GtkSelectionModel")
             (list-interfaces "GtkMultiSelection")))
  ;; Check the properties
  (is (equal '("item-type" "model" "n-items")
             (list-properties "GtkMultiSelection")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkMultiSelection")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkMultiSelection"
                                             GTK-MULTI-SELECTION
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                                ("GListModel" "GtkSelectionModel")
                                :TYPE-INITIALIZER
                                "gtk_multi_selection_get_type")
                               ((ITEM-TYPE GTK-MULTI-SELECTION-ITEM-TYPE
                                 "item-type" "GType" T NIL)
                                (MODEL GTK-MULTI-SELECTION-MODEL "model"
                                 "GListModel" T T)
                                (N-ITEMS GTK-MULTI-SELECTION-N-ITEMS "n-items"
                                 "guint" T NIL)))
             (gobject:get-g-type-definition "GtkMultiSelection"))))

;;; --- Properties -------------------------------------------------------------

;;;     item-type                                          Since 4.8
;;;     model
;;;     n-items                                            Since 4.8

;;; --- Functions --------------------------------------------------------------

;;;     gtk_multi_selection_new

;;; --- 2023-9-6 ---------------------------------------------------------------
