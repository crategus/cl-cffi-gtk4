(in-package :gtk-test)

(def-suite gtk-drop-down :in gtk-suite)
(in-suite gtk-drop-down)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkDropDown

(test gtk-drop-down-class
  ;; Type check
  (is (g:type-is-object "GtkDropDown"))
  ;; Check the registered name
  (is (eq 'gtk:drop-down
          (gobject:symbol-for-gtype "GtkDropDown")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkDropDown")
          (g:gtype (cffi:foreign-funcall "gtk_drop_down_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget") (g:type-parent "GtkDropDown")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkDropDown")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (list-interfaces "GtkDropDown")))
  ;; Check the class properties
  (is (equal '("enable-search" "expression" "factory" "list-factory" "model"
               "selected" "selected-item" "show-arrow")
             (list-properties "GtkDropDown")))
  ;; Check the list of signals
  (is (equal '("activate")
             (list-signals "GtkDropDown")))
  ;; CSS information
  (is (string= "dropdown"
               (gtk:widget-class-css-name "GtkDropDown")))
  (is (string=
"dropdown:dir(ltr)
  button.toggle:dir(ltr)
    box.horizontal:dir(ltr)
      stack:dir(ltr)
        label:dir(ltr)
        row:dir(ltr)
      arrow:dir(ltr)
  [popover.background.menu:dir(ltr)]
    contents:dir(ltr)
      box.vertical:dir(ltr)
        [box.dropdown-searchbar.horizontal:dir(ltr)]
          entry.search:dir(ltr)
            image:dir(ltr)
            text:dir(ltr)
              placeholder:dir(ltr)
              undershoot.left:dir(ltr)
              undershoot.right:dir(ltr)
            image:dir(ltr)
        scrolledwindow:dir(ltr)
          listview.view:dir(ltr)
          scrollbar.bottom.horizontal:dir(ltr)
            range.horizontal:dir(ltr)
              trough:dir(ltr)
                slider:dir(ltr)
          scrollbar.right.vertical:dir(ltr)
            range.vertical:dir(ltr)
              trough:dir(ltr)
                slider:dir(ltr)
          overshoot.left:dir(ltr)
          undershoot.left:dir(ltr)
          overshoot.right:dir(ltr)
          undershoot.right:dir(ltr)
          overshoot.top:dir(ltr)
          undershoot.top:dir(ltr)
          overshoot.bottom:dir(ltr)
          undershoot.bottom:dir(ltr)
          junction:dir(ltr)
    arrow:dir(ltr)
"
               (gtk:style-context-to-string
                   (gtk:widget-style-context (make-instance 'gtk:drop-down))
                   :none)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkDropDown" GTK-DROP-DOWN
                       (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
                        :TYPE-INITIALIZER "gtk_drop_down_get_type")
                       ((ENABLE-SEARCH GTK-DROP-DOWN-ENABLE-SEARCH
                         "enable-search" "gboolean" T T)
                        (EXPRESSION GTK-DROP-DOWN-EXPRESSION "expression"
                         "GtkExpression" T T)
                        (FACTORY GTK-DROP-DOWN-FACTORY "factory"
                         "GtkListItemFactory" T T)
                        (LIST-FACTORY GTK-DROP-DOWN-LIST-FACTORY "list-factory"
                         "GtkListItemFactory" T T)
                        (MODEL GTK-DROP-DOWN-MODEL "model" "GListModel" T T)
                        (SELECTED GTK-DROP-DOWN-SELECTED "selected" "guint" T
                         T)
                        (SELECTED-ITEM GTK-DROP-DOWN-SELECTED-ITEM
                         "selected-item" "GObject" T NIL)
                        (SHOW-ARROW GTK-DROP-DOWN-SHOW-ARROW "show-arrow"
                         "gboolean" T T)))
             (get-g-type-definition "GtkDropDown"))))

;;; --- Properties -------------------------------------------------------------

;;;     enable-search
;;;     expression
;;;     factory
;;;     list-factory
;;;     model
;;;     selected
;;;     selected-item
;;;     show-arrow

#+nil
(test gtk-drop-down-properties
  (let ((dropdown (make-instance 'gtk-drop-down)))
    (is-false (gtk-drop-down-enable-search dropdown))
    (is-false (gtk-drop-down-expression dropdown))
    (is-false (gtk-drop-down-factory dropdown))
    (is-false (gtk-drop-down-list-factory dropdown))
    (is-false (gtk-drop-down-model dropdown))
    (is-false (gtk-drop-down-selected dropdown))
    (is-false (gtk-drop-down-selected-item dropdown))
    (is-false (gtk-drop-down-show-arrow dropdown))
))

;;; --- Signals ----------------------------------------------------------------

;;;     activate

;;; --- Functions --------------------------------------------------------------

;;;     gtk_drop_down_new
;;;     gtk_drop_down_new_from_strings

;;; --- 2023-3-18 --------------------------------------------------------------
