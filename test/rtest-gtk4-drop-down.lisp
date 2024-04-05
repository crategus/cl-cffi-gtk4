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
          (glib:symbol-for-gtype "GtkDropDown")))
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
  (is (equal '("enable-search" "expression" "factory" "header-factory"
               "list-factory" "model" "search-match-mode" "selected"
               "selected-item" "show-arrow")
             (list-properties "GtkDropDown")))
  ;; Check the list of signals
  (is (equal '("activate")
             (list-signals "GtkDropDown")))
  ;; CSS name
  (is (string= "dropdown"
               (gtk:widget-class-css-name "GtkDropDown")))
  ;; CSS classes
  (is (equal '()
             (gtk:widget-css-classes (make-instance 'gtk:drop-down))))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkDropDown" GTK-DROP-DOWN
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkBuildable"
                                 "GtkConstraintTarget")
                                :TYPE-INITIALIZER "gtk_drop_down_get_type")
                               ((ENABLE-SEARCH GTK-DROP-DOWN-ENABLE-SEARCH
                                 "enable-search" "gboolean" T T)
                                (EXPRESSION GTK-DROP-DOWN-EXPRESSION
                                 "expression" "GtkExpression" T T)
                                (FACTORY GTK-DROP-DOWN-FACTORY "factory"
                                 "GtkListItemFactory" T T)
                                (HEADER-FACTORY GTK-DROP-DOWN-HEADER-FACTORY
                                 "header-factory" "GtkListItemFactory" T T)
                                (LIST-FACTORY GTK-DROP-DOWN-LIST-FACTORY
                                 "list-factory" "GtkListItemFactory" T T)
                                (MODEL GTK-DROP-DOWN-MODEL "model" "GListModel"
                                 T T)
                                (SEARCH-MATCH-MODE
                                 GTK-DROP-DOWN-SEARCH-MATCH-MODE
                                 "search-match-mode" "GtkStringFilterMatchMode"
                                 T T)
                                (SELECTED GTK-DROP-DOWN-SELECTED "selected"
                                 "guint" T T)
                                (SELECTED-ITEM GTK-DROP-DOWN-SELECTED-ITEM
                                 "selected-item" "GObject" T NIL)
                                (SHOW-ARROW GTK-DROP-DOWN-SHOW-ARROW
                                 "show-arrow" "gboolean" T T)))
             (gobject:get-g-type-definition "GtkDropDown"))))

;;; --- Properties -------------------------------------------------------------

;;;     enable-search
;;;     expression
;;;     factory
;;;     list-factory
;;;     model
;;;     selected
;;;     selected-item
;;;     show-arrow

(test gtk-drop-down-properties
  (let ((dropdown (make-instance 'gtk:drop-down)))
    (is-false (gtk:drop-down-enable-search dropdown))
    (is (cffi:null-pointer-p (gtk:drop-down-expression dropdown)))
    (is (typep (gtk:drop-down-factory dropdown) 'gtk:list-item-factory))
    (is-false (gtk:drop-down-list-factory dropdown))
    (is-false (gtk:drop-down-model dropdown))
    (is (= gtk:+invalid-list-position+ (gtk:drop-down-selected dropdown)))
    (is-false (gtk:drop-down-selected-item dropdown))
    (is-true (gtk:drop-down-show-arrow dropdown))))

;;; --- Signals ----------------------------------------------------------------

;;;     activate

(test gtk-drop-down-signal
  (let ((query (g:signal-query (g:signal-lookup "activate" "GtkDropDown"))))
    (is (string= "activate" (g:signal-query-signal-name query)))
    (is (string= "GtkDropDown" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:ACTION :RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_drop_down_new

(test gtk-drop-down-new
  (is (typep (gtk:drop-down-new nil nil) 'gtk:drop-down)))

;;;     gtk_drop_down_new_from_strings

(test gtk-drop-down-new-from-strings
  (is (typep (gtk:drop-down-new-from-strings '("string1" "string2" "string3"))
             'gtk:drop-down)))

;;; 2024-1-10
