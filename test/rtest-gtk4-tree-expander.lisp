(in-package :gtk-test)

(def-suite gtk-tree-expander :in gtk-tree-support)
(in-suite gtk-tree-expander)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkTreeExpander

(test widget-class
  ;; Check type
  (is (g:type-is-object "GtkTreeExpander"))
  ;; Check registered name
  (is (eq 'gtk:tree-expander
          (glib:symbol-for-gtype "GtkTreeExpander")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkTreeExpander")
          (g:gtype (cffi:foreign-funcall "gtk_tree_expander_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkTreeExpander")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkTreeExpander")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (glib-test:list-interfaces "GtkTreeExpander")))
  ;; Check properties
  (is (equal '("child" "hide-expander" "indent-for-depth" "indent-for-icon"
               "item" "list-row")
             (glib-test:list-properties "GtkTreeExpander")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkTreeExpander")))
  ;; Check CSS name
  (is (string= "treeexpander"
               (gtk:widget-class-css-name "GtkTreeExpander")))
  ;; Check accessible role
  (is (eq :button
          (gtk:widget-class-accessible-role "GtkTreeExpander")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkTreeExpander" GTK:TREE-EXPANDER
                      (:SUPERCLASS GTK:WIDGET
                       :EXPORT T
                       :INTERFACES
                       ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
                       :TYPE-INITIALIZER "gtk_tree_expander_get_type")
                      ((CHILD TREE-EXPANDER-CHILD "child" "GtkWidget" T T)
                       (HIDE-EXPANDER TREE-EXPANDER-HIDE-EXPANDER
                        "hide-expander" "gboolean" T T)
                       (INDENT-FOR-DEPTH TREE-EXPANDER-INDENT-FOR-DEPTH
                        "indent-for-depth" "gboolean" T T)
                       (INDENT-FOR-ICON TREE-EXPANDER-INDENT-FOR-ICON
                        "indent-for-icon" "gboolean" T T)
                       (ITEM TREE-EXPANDER-ITEM "item" "GObject" T NIL)
                       (LIST-ROW TREE-EXPANDER-LIST-ROW "list-row"
                        "GtkTreeListRow" T T)))
             (gobject:get-gtype-definition "GtkTreeExpander"))))

;;; --- Properties -------------------------------------------------------------

;;;     child
;;;     hide-expander                                       Since 4.10
;;;     indent-for-depth                                    Since 4.10
;;;     indent-for-icon                                     Since 4.6
;;;     item
;;;     list-row

(test gtk-tree-expander-properties
  (glib-test:with-check-memory (expander)
    (is (typep (setf expander
                     (make-instance 'gtk:tree-expander)) 'gtk:tree-expander))
    (is-false (gtk:tree-expander-child expander))
    (is-false (gtk:tree-expander-hide-expander expander))
    (is-true (gtk:tree-expander-indent-for-depth expander))
    (is-true (gtk:tree-expander-indent-for-icon expander))
    (is-false (gtk:tree-expander-item expander))
    (is-false (gtk:tree-expander-list-row expander))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_tree_expander_new

(test gtk-tree-expander-new
  (glib-test:with-check-memory (expander)
    (is (typep (setf expander (gtk:tree-expander-new)) 'gtk:tree-expander))))

;;; 2025-4-16
