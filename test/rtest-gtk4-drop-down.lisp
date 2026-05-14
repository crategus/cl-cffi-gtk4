(in-package :gtk-test)

(def-suite gtk-drop-down :in gtk-list-widgets)
(in-suite gtk-drop-down)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkDropDown

(test gtk-drop-down-class
  ;; Check type
  (is (g:type-is-object "GtkDropDown"))
  ;; Check registered name
  (is (eq 'gtk:drop-down
          (glib:symbol-for-gtype "GtkDropDown")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkDropDown")
          (g:gtype (cffi:foreign-funcall "gtk_drop_down_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget") (g:type-parent "GtkDropDown")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkDropDown")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (glib-test:list-interfaces "GtkDropDown")))
  ;; Check class properties
  (is (equal '("enable-search" "expression" "factory" "header-factory"
               "list-factory" "model" "search-match-mode" "selected"
               "selected-item" "show-arrow")
             (glib-test:list-properties "GtkDropDown")))
  ;; Check signals
  (is (equal '("activate")
             (glib-test:list-signals "GtkDropDown")))
  ;; Check CSS name
  (is (string= "dropdown"
               (gtk:widget-class-css-name "GtkDropDown")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkDropDown" GTK:DROP-DOWN
                      (:SUPERCLASS GTK:WIDGET
                       :EXPORT T
                       :INTERFACES
                       ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
                       :TYPE-INITIALIZER "gtk_drop_down_get_type")
                      ((ENABLE-SEARCH DROP-DOWN-ENABLE-SEARCH
                        "enable-search" "gboolean" T T)
                       (EXPRESSION DROP-DOWN-EXPRESSION
                        "expression" "GtkExpression" T T)
                       (FACTORY DROP-DOWN-FACTORY
                        "factory" "GtkListItemFactory" T T)
                       (HEADER-FACTORY DROP-DOWN-HEADER-FACTORY
                        "header-factory" "GtkListItemFactory" T T)
                       (LIST-FACTORY DROP-DOWN-LIST-FACTORY
                        "list-factory" "GtkListItemFactory" T T)
                       (MODEL DROP-DOWN-MODEL "model" "GListModel" T T)
                       (SEARCH-MATCH-MODE DROP-DOWN-SEARCH-MATCH-MODE
                        "search-match-mode" "GtkStringFilterMatchMode" T T)
                       (SELECTED DROP-DOWN-SELECTED "selected" "guint" T T)
                       (SELECTED-ITEM DROP-DOWN-SELECTED-ITEM
                        "selected-item" "GObject" T NIL)
                       (SHOW-ARROW DROP-DOWN-SHOW-ARROW
                        "show-arrow" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkDropDown"))))

;;; --- Properties -------------------------------------------------------------

;;;     enable-search
;;;     expression
;;;     factory
;;;     header-factory
;;;     list-factory
;;;     model
;;;     search-match-mode
;;;     selected
;;;     selected-item
;;;     show-arrow

(test gtk-drop-down-properties
  (let ((dropdown (make-instance 'gtk:drop-down)))
    (is-false (gtk:drop-down-enable-search dropdown))
    (is (cffi:null-pointer-p (gtk:drop-down-expression dropdown)))
    (is (typep (gtk:drop-down-factory dropdown) 'gtk:list-item-factory))
    (is-false (gtk:drop-down-header-factory dropdown))
    (is-false (gtk:drop-down-list-factory dropdown))
    (is-false (gtk:drop-down-model dropdown))
    (is (eq :prefix (gtk:drop-down-search-match-mode dropdown)))
    (is (= gtk:+invalid-list-position+ (gtk:drop-down-selected dropdown)))
    (is-false (gtk:drop-down-selected-item dropdown))
    (is-true (gtk:drop-down-show-arrow dropdown))))

;;; --- Signals ----------------------------------------------------------------

;;;     activate

(test gtk-drop-down-activate-signal
  (let* ((name "activate")
         (gtype (g:gtype "GtkDropDown"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:ACTION :RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_drop_down_new

(test gtk-drop-down-new.1
  (glib-test:with-check-memory (dropdown)
    (is (typep (setf dropdown (gtk:drop-down-new)) 'gtk:drop-down))))

;; Adds a strong reference for GtkStringObject instance

(test gtk-drop-down-new.2
  (glib-test:with-check-memory (dropdown model :strong 1)
    ;; Create model and dropdown
    (is (typep (setf model
                     (gtk:string-list-new '("Factory" "Home" "Subway")))
               'g:list-model))
    (is (typep (setf dropdown (gtk:drop-down-new model)) 'gtk:drop-down))
    ;; Check properties
    (is-false (gtk:drop-down-enable-search dropdown))
    (is (cffi:null-pointer-p (gtk:drop-down-expression dropdown)))
    (is (typep (gtk:drop-down-factory dropdown) 'gtk:list-item-factory))
    (is-false (gtk:drop-down-header-factory dropdown))
    (is-false (gtk:drop-down-list-factory dropdown))
    (is (typep (gtk:drop-down-model dropdown) 'gtk:string-list))
    (is (eq :prefix (gtk:drop-down-search-match-mode dropdown)))
    (is (= 0 (gtk:drop-down-selected dropdown)))
    (is (typep (gtk:drop-down-selected-item dropdown) 'gtk:string-object))
    (is-true (gtk:drop-down-show-arrow dropdown))
    ;; Get selected item
    (is (string= "Factory"
                 (gtk:string-object-string
                     (gtk:drop-down-selected-item dropdown))))
    ;; Remove references
    (is-false (setf (gtk:drop-down-model dropdown) nil))
    (is-false (setf (gtk:drop-down-factory dropdown) nil))))

;; Adds a strong reference for GtkLabel widget

(test gtk-drop-down-new.3
  (let (expression)
    (glib-test:with-check-memory (dropdown model :strong 1)
      ;; Create model
      (is (typep (setf model
                       (g:list-store-new "GObject"))
                 'g:list-model))
      ;; Create GListModel
      (is-false (g:list-store-append model (make-instance 'gtk:label)))
      (is-false (g:list-store-append model (make-instance 'gtk:button)))
      (is-false (g:list-store-append model (make-instance 'gtk:text)))
      ;; Create expression
      (is (cffi:pointerp (setf expression
                               (gtk:property-expression-new "GtkWidget" nil "css-name"))))
      ;; Create dropdown
      (is (typep (setf dropdown
                       (gtk:drop-down-new model expression)) 'gtk:drop-down))
      ;; Check properties
      (is-false (gtk:drop-down-enable-search dropdown))
      (is (cffi:pointer-eq expression (gtk:drop-down-expression dropdown)))
      (is (typep (gtk:drop-down-factory dropdown) 'gtk:list-item-factory))
      (is-false (gtk:drop-down-header-factory dropdown))
      (is-false (gtk:drop-down-list-factory dropdown))
      (is (typep (gtk:drop-down-model dropdown) 'g:list-store))
      (is (eq :prefix (gtk:drop-down-search-match-mode dropdown)))
      (is (= 0 (gtk:drop-down-selected dropdown)))
      (is (typep (gtk:drop-down-selected-item dropdown) 'gtk:label))
      (is-true (gtk:drop-down-show-arrow dropdown))
      ;; Remove references
      (is-false (setf (gtk:drop-down-model dropdown) nil))
      (is-false (g:list-store-remove-all model))
      (is-false (setf (gtk:drop-down-factory dropdown) nil)))))

;;;     gtk_drop_down_new_from_strings

;; Adds a strong reference for GtkStringObject instance

(test gtk-drop-down-new-from-strings
  (glib-test:with-check-memory (dropdown :strong 1)
    (is (typep (setf dropdown
                     (gtk:drop-down-new-from-strings "string1" "string2" "string3"))
               'gtk:drop-down))
    ;; Check properties
    (is-false (gtk:drop-down-enable-search dropdown))
    (is (cffi:null-pointer-p (gtk:drop-down-expression dropdown)))
    (is (typep (gtk:drop-down-factory dropdown) 'gtk:list-item-factory))
    (is-false (gtk:drop-down-header-factory dropdown))
    (is-false (gtk:drop-down-list-factory dropdown))
    (is (typep (gtk:drop-down-model dropdown) 'gtk:string-list))
    (is (eq :prefix (gtk:drop-down-search-match-mode dropdown)))
    (is (= 0 (gtk:drop-down-selected dropdown)))
    (is (typep (gtk:drop-down-selected-item dropdown) 'gtk:string-object))
    (is-true (gtk:drop-down-show-arrow dropdown))
    ;; Get selected item
    (is (string= "string1"
                 (gtk:string-object-string
                     (gtk:drop-down-selected-item dropdown))))
    ;; Remove references
    (is-false (setf (gtk:drop-down-model dropdown) nil))
    (is-false (setf (gtk:drop-down-factory dropdown) nil))))

;;; 2026-04-12
