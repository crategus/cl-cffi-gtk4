(in-package :gtk-test)

(def-suite gtk-list-box :in gtk-layout-widgets)
(in-suite gtk-list-box)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkListBoxRow

(test gtk-list-box-row-class
  ;; Check type
  (is (g:type-is-object "GtkListBoxRow"))
  ;; Check registered name
  (is (eq 'gtk:list-box-row
          (glib:symbol-for-gtype "GtkListBoxRow")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkListBoxRow")
          (g:gtype (cffi:foreign-funcall "gtk_list_box_row_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkListBoxRow")))
  ;; Check children
  #-windows
  (is (or (equal '()
                 (glib-test:list-children "GtkListBoxRow"))
          (equal '("GtkPlacesViewRow" "GtkSidebarRow")
                 (glib-test:list-children "GtkListBoxRow"))))
  #+windows
  (when *first-run-testsuite*
    (is (equal '()
               (glib-test:list-children "GtkListBoxRow"))))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkActionable")
             (glib-test:list-interfaces "GtkListBoxRow")))
  ;; Check properties
  (is (equal '("action-name" "action-target" "activatable" "child" "selectable")
             (glib-test:list-properties "GtkListBoxRow")))
  ;; Check signals
  (is (equal '("activate")
             (glib-test:list-signals "GtkListBoxRow")))
  ;; Check CSS name
  (is (string= "row"
               (gtk:widget-class-css-name "GtkListBoxRow")))
  ;; Check accessible role
  (is (eq :list-item (gtk:widget-class-accessible-role "GtkListBoxRow")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkListBoxRow" GTK:LIST-BOX-ROW
                       (:SUPERCLASS GTK:WIDGET
                        :EXPORT T
                        :INTERFACES
                        ("GtkAccessible" "GtkActionable" "GtkBuildable"
                         "GtkConstraintTarget")
                        :TYPE-INITIALIZER "gtk_list_box_row_get_type")
                       ((ACTIVATABLE LIST-BOX-ROW-ACTIVATABLE
                         "activatable" "gboolean" T T)
                        (CHILD LIST-BOX-ROW-CHILD "child" "GtkWidget" T T)
                        (SELECTABLE LIST-BOX-ROW-SELECTABLE
                         "selectable" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkListBoxRow"))))

;;; --- Signals ----------------------------------------------------------------

;;;     activate

(test gtk-list-box-activate-signal
  (let* ((name "activate")
         (gtype (g:gtype "GtkListBoxRow"))
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

;;; --- Properties -------------------------------------------------------------

(test gtk-list-box-row-properties
  (let ((row (make-instance 'gtk:list-box-row)))
    (is-true (gtk:list-box-row-activatable row))
    (is-false (gtk:list-box-row-child row))
    (is-true (gtk:list-box-row-selectable row))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_list_box_row_new

(test gtk-list-box-row-new
  (is (typep (gtk:list-box-row-new) 'gtk:list-box-row)))

;;;     gtk_list_box_row_changed
;;;     gtk_list_box_row_is_selected

(test gtk-list-box-row-changed/is-selected
  (let ((row (make-instance 'gtk:list-box-row)))
    (is-false (gtk:list-box-row-changed row))
    (is-false (gtk:list-box-row-is-selected row))))

;;;     gtk_list_box_row_get_header
;;;     gtk_list_box_row_set_header

(test gtk-list-box-row-header
  (let ((row (gtk:list-box-row-new))
        (box (gtk:box-new)))
    (is-false (gtk:list-box-row-header row))
    (is (eq box (setf (gtk:list-box-row-header row) box)))
    (is (eq box (gtk:list-box-row-header row)))
    (is-false (setf (gtk:list-box-row-header row) nil))))

;;;     gtk_list_box_row_get_index

(test gtk-list-box-row-index
  (let ((row (gtk:list-box-row-new)))
    (is (= -1 (gtk:list-box-row-index row)))))

;;; --- Types and Values -------------------------------------------------------

;;;     GtkListBox

#-gtk-4-18
(test gtk-list-box-class
  ;; Check type
  (is (g:type-is-object "GtkListBox"))
  ;; Check registered name
  (is (eq 'gtk:list-box
          (glib:symbol-for-gtype "GtkListBox")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkListBox")
          (g:gtype (cffi:foreign-funcall "gtk_list_box_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkListBox")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkListBox")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (glib-test:list-interfaces "GtkListBox")))
  ;; Check properties
  (is (equal '("accept-unpaired-release" "activate-on-single-click"
               "selection-mode" "show-separators")
             (glib-test:list-properties "GtkListBox")))
  ;; Check signals
  (is (equal '("activate-cursor-row" "move-cursor" "row-activated"
               "row-selected" "select-all" "selected-rows-changed"
               "toggle-cursor-row" "unselect-all")
             (glib-test:list-signals "GtkListBox")))
  ;; Check CSS name
  (is (string= "list"
               (gtk:widget-class-css-name "GtkListBox")))
  ;; Check accessible role
  (is (eq :list (gtk:widget-class-accessible-role "GtkListBox")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkListBox" GTK:LIST-BOX
                       (:SUPERCLASS GTK:WIDGET
                        :EXPORT T
                        :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
                        :TYPE-INITIALIZER "gtk_list_box_get_type")
                       ((ACCEPT-UNPAIRED-RELEASE LIST-BOX-ACCEPT-UNPAIRED-RELEASE
                         "accept-unpaired-release" "gboolean" T T)
                        (ACTIVATE-ON-SINGLE-CLICK
                         LIST-BOX-ACTIVATE-ON-SINGLE-CLICK
                         "activate-on-single-click" "gboolean" T T)
                        (SELECTION-MODE LIST-BOX-SELECTION-MODE
                         "selection-mode" "GtkSelectionMode" T T)
                        (SHOW-SEPARATORS LIST-BOX-SHOW-SEPARATORS
                         "show-separators" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkListBox"))))

#+gtk-4-18
(test gtk-list-box-class
  ;; Check type
  (is (g:type-is-object "GtkListBox"))
  ;; Check registered name
  (is (eq 'gtk:list-box
          (glib:symbol-for-gtype "GtkListBox")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkListBox")
          (g:gtype (cffi:foreign-funcall "gtk_list_box_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkListBox")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkListBox")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (glib-test:list-interfaces "GtkListBox")))
  ;; Check properties
  (is (equal '("accept-unpaired-release" "activate-on-single-click"
               "selection-mode" "show-separators" "tab-behavior")
             (glib-test:list-properties "GtkListBox")))
  ;; Check signals
  (is (equal '("activate-cursor-row" "move-cursor" "row-activated"
               "row-selected" "select-all" "selected-rows-changed"
               "toggle-cursor-row" "unselect-all")
             (glib-test:list-signals "GtkListBox")))
  ;; Check CSS name
  (is (string= "list"
               (gtk:widget-class-css-name "GtkListBox")))
  ;; Check accessible role
  (is (eq :list (gtk:widget-class-accessible-role "GtkListBox")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkListBox" GTK:LIST-BOX
                      (:SUPERCLASS GTK:WIDGET
                       :EXPORT T
                       :INTERFACES
                       ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
                       :TYPE-INITIALIZER "gtk_list_box_get_type")
                      ((ACCEPT-UNPAIRED-RELEASE
                        LIST-BOX-ACCEPT-UNPAIRED-RELEASE
                        "accept-unpaired-release" "gboolean" T T)
                       (ACTIVATE-ON-SINGLE-CLICK
                        LIST-BOX-ACTIVATE-ON-SINGLE-CLICK
                        "activate-on-single-click" "gboolean" T T)
                       (SELECTION-MODE LIST-BOX-SELECTION-MODE
                        "selection-mode" "GtkSelectionMode" T T)
                       (SHOW-SEPARATORS LIST-BOX-SHOW-SEPARATORS
                        "show-separators" "gboolean" T T)
                       (TAB-BEHAVIOR LIST-BOX-TAB-BEHAVIOR "tab-behavior"
                        "GtkListTabBehavior" T T)))
             (gobject:get-gtype-definition "GtkListBox"))))

;;; --- Signals ----------------------------------------------------------------

;;;     activate-cursor-row

(test gtk-list-box-activate-cursor-row-signal
  (let* ((name "activate-cursor-row")
         (gtype (g:gtype "GtkListBox"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     move-cursor

(test gtk-list-box-move-cursor-signal
  (let* ((name "move-cursor")
         (gtype (g:gtype "GtkListBox"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("GtkMovementStep" "gint" "gboolean" "gboolean")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     row-activated

(test gtk-list-box-row-activated-signal
  (let* ((name "row-activated")
         (gtype (g:gtype "GtkListBox"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("GtkListBoxRow")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     row-selected

(test gtk-list-box-row-selected-signal
  (let* ((name "row-selected")
         (gtype (g:gtype "GtkListBox"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("GtkListBoxRow")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     select-all

(test gtk-list-box-select-all-signal
  (let* ((name "select-all")
         (gtype (g:gtype "GtkListBox"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     selected-rows-changed

(test gtk-list-box-selected-rows-changed-signal
  (let* ((name "selected-rows-changed")
         (gtype (g:gtype "GtkListBox"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     toggle-cursor-row

(test gtk-list-box-toggle-cursor-row-signal
  (let* ((name "toggle-cursor-row")
         (gtype (g:gtype "GtkListBox"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     unselect-all

(test gtk-list-box-unselect-all-signal
  (let* ((name "unselect-all")
         (gtype (g:gtype "GtkListBox"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Properties -------------------------------------------------------------

(test gtk-list-box-properties
  (let ((listbox (make-instance 'gtk:list-box)))
    (is-false (gtk:list-box-accept-unpaired-release listbox))
    (is-true (gtk:list-box-activate-on-single-click listbox))
    (is (eq :single (gtk:list-box-selection-mode listbox)))
    (is-false (gtk:list-box-show-separators listbox))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_list_box_new

(test gtk-list-box-new
  (is (typep (gtk:list-box-new) 'gtk:list-box)))

;;;     gtk_list_box_prepend

(test gtk-list-box-prepend
  (let ((listbox (gtk:list-box-new))
        (widget1 (gtk:box-new))
        (widget2 (gtk:frame-new))
        (widget3 (gtk:paned-new)))
    (is-false (gtk:list-box-prepend listbox widget1))
    (is-false (gtk:list-box-prepend listbox widget2))
    (is-false (gtk:list-box-prepend listbox widget3))

    (is (= 2 (g:object-ref-count widget1)))
    (is (= 2 (g:object-ref-count widget2)))
    (is (= 2 (g:object-ref-count widget3)))

    (is (eq widget3
            (gtk:list-box-row-child (gtk:list-box-row-at-index listbox 0))))
    (is (eq widget2
            (gtk:list-box-row-child (gtk:list-box-row-at-index listbox 1))))
    (is (eq widget1
            (gtk:list-box-row-child (gtk:list-box-row-at-index listbox 2))))
    ;; Remove children from the list box rows
    (is-false (setf (gtk:list-box-row-child
                        (gtk:list-box-row-at-index listbox 0)) nil))
    (is-false (setf (gtk:list-box-row-child
                        (gtk:list-box-row-at-index listbox 1)) nil))
    (is-false (setf (gtk:list-box-row-child
                        (gtk:list-box-row-at-index listbox 2)) nil))
    ;; Remove list box rows from list box
    (is-false (gtk:list-box-remove-all listbox))
    ;; Check memory management
    (is (= 1 (g:object-ref-count listbox)))
    (is (= 1 (g:object-ref-count widget1)))
    (is (= 1 (g:object-ref-count widget2)))
    (is (= 1 (g:object-ref-count widget3)))))

;;;     gtk_list_box_append

(test gtk-list-box-append
  (let ((listbox (gtk:list-box-new))
        (widget1 (gtk:box-new))
        (widget2 (gtk:frame-new))
        (widget3 (gtk:paned-new)))
    (is-false (gtk:list-box-append listbox widget1))
    (is-false (gtk:list-box-append listbox widget2))
    (is-false (gtk:list-box-append listbox widget3))

    (is (= 2 (g:object-ref-count widget1)))
    (is (= 2 (g:object-ref-count widget2)))
    (is (= 2 (g:object-ref-count widget3)))

    (is (eq widget1
            (gtk:list-box-row-child (gtk:list-box-row-at-index listbox 0))))
    (is (eq widget2
            (gtk:list-box-row-child (gtk:list-box-row-at-index listbox 1))))
    (is (eq widget3
            (gtk:list-box-row-child (gtk:list-box-row-at-index listbox 2))))

    ;; Remove children from the list box rows
    (is-false (setf (gtk:list-box-row-child
                        (gtk:list-box-row-at-index listbox 0)) nil))
    (is-false (setf (gtk:list-box-row-child
                        (gtk:list-box-row-at-index listbox 1)) nil))
    (is-false (setf (gtk:list-box-row-child
                        (gtk:list-box-row-at-index listbox 2)) nil))
    ;; Remove list box rows from list box
    (is-false (gtk:list-box-remove-all listbox))
    ;; Check memory management
    (is (= 1 (g:object-ref-count listbox)))
    (is (= 1 (g:object-ref-count widget1)))
    (is (= 1 (g:object-ref-count widget2)))
    (is (= 1 (g:object-ref-count widget3)))))

;;;     gtk_list_box_insert

(test gtk-list-box-insert
  (let ((listbox (gtk:list-box-new))
        (widget1 (gtk:box-new))
        (widget2 (gtk:frame-new))
        (widget3 (gtk:paned-new)))
    (is-false (gtk:list-box-insert listbox widget1 0))
    (is-false (gtk:list-box-insert listbox widget2 1))
    (is-false (gtk:list-box-insert listbox widget3 0))

    (is (eq widget3
            (gtk:list-box-row-child (gtk:list-box-row-at-index listbox 0))))
    (is (eq widget1
            (gtk:list-box-row-child (gtk:list-box-row-at-index listbox 1))))
    (is (eq widget2
            (gtk:list-box-row-child (gtk:list-box-row-at-index listbox 2))))

    ;; Remove children from the list box rows
    (is-false (setf (gtk:list-box-row-child
                        (gtk:list-box-row-at-index listbox 0)) nil))
    (is-false (setf (gtk:list-box-row-child
                        (gtk:list-box-row-at-index listbox 1)) nil))
    (is-false (setf (gtk:list-box-row-child
                        (gtk:list-box-row-at-index listbox 2)) nil))
    ;; Remove list box rows from list box
    (is-false (gtk:list-box-remove-all listbox))
    ;; Check memory management
    (is (= 1 (g:object-ref-count listbox)))
    (is (= 1 (g:object-ref-count widget1)))
    (is (= 1 (g:object-ref-count widget2)))
    (is (= 1 (g:object-ref-count widget3)))))

;;;     gtk_list_box_remove
;;;     gtk_list_box_remove_all                            Since 4.12
;;;     gtk_list_box_get_row_at_index

(test gtk-list-box-remove
  (let ((listbox (gtk:list-box-new))
        (widget1 (gtk:box-new))
        (widget2 (gtk:frame-new))
        (widget3 (gtk:paned-new))
        (row nil))
    (is-false (gtk:list-box-append listbox widget1))
    (is-false (gtk:list-box-append listbox widget2))
    (is-false (gtk:list-box-append listbox widget3))

    (is (eq widget1 (gtk:list-box-row-child (gtk:list-box-row-at-index listbox 0))))
    (is (eq widget2 (gtk:list-box-row-child (gtk:list-box-row-at-index listbox 1))))
    (is (eq widget3 (gtk:list-box-row-child (gtk:list-box-row-at-index listbox 2))))

    (is-false (gtk:list-box-remove listbox
                  (setf row (gtk:list-box-row-at-index listbox 1))))
    (is (eq widget2 (gtk:list-box-row-child row)))
    (is-false (setf (gtk:list-box-row-child row) nil))

    (is (eq widget1 (gtk:list-box-row-child (gtk:list-box-row-at-index listbox 0))))
    (is (eq widget3 (gtk:list-box-row-child (gtk:list-box-row-at-index listbox 1))))

    ;; Remove children from the list box rows
    (is-false (setf (gtk:list-box-row-child
                        (gtk:list-box-row-at-index listbox 0)) nil))
    (is-false (setf (gtk:list-box-row-child
                        (gtk:list-box-row-at-index listbox 1)) nil))
    ;; Remove list box rows from list box
    (is-false (gtk:list-box-remove-all listbox))
    ;; Check memory management
    (is (= 1 (g:object-ref-count listbox)))
    (is (= 1 (g:object-ref-count widget1)))
    (is (= 1 (g:object-ref-count widget2)))
    (is (= 1 (g:object-ref-count widget3)))))

;;;     gtk_list_box_select_row
;;;     gtk_list_box_unselect_row
;;;     gtk_list_box_get_selected_row

(test gtk-list-box-select/unselect-row
  (let ((listbox (gtk:list-box-new))
        (widget1 (gtk:box-new))
        (widget2 (gtk:frame-new))
        (widget3 (gtk:paned-new)))
    (is-false (gtk:list-box-append listbox widget1))
    (is-false (gtk:list-box-append listbox widget2))
    (is-false (gtk:list-box-append listbox widget3))

    (is-false (gtk:list-box-select-row listbox (gtk:list-box-row-at-index listbox 1)))
    (is (eq widget2 (gtk:list-box-row-child (gtk:list-box-selected-row listbox))))
    (is-false (gtk:list-box-unselect-row listbox (gtk:list-box-row-at-index listbox 1)))
    (is-false (gtk:list-box-selected-row listbox))

    ;; Remove children from the list box rows
    (is-false (setf (gtk:list-box-row-child
                        (gtk:list-box-row-at-index listbox 0)) nil))
    (is-false (setf (gtk:list-box-row-child
                        (gtk:list-box-row-at-index listbox 1)) nil))
    (is-false (setf (gtk:list-box-row-child
                        (gtk:list-box-row-at-index listbox 2)) nil))
    ;; Remove list box rows from list box
    (is-false (gtk:list-box-remove-all listbox))
    ;; Check memory management
    (is (= 1 (g:object-ref-count listbox)))
    (is (= 1 (g:object-ref-count widget1)))
    (is (= 1 (g:object-ref-count widget2)))
    (is (= 1 (g:object-ref-count widget3)))))

;;;     gtk_list_box_select_all
;;;     gtk_list_box_unselect_all
;;;     gtk_list_box_get_selected_rows

(test gtk-list-box-select/unselect-all
  (let ((listbox (gtk:list-box-new))
        (widget1 (gtk:box-new))
        (widget2 (gtk:frame-new))
        (widget3 (gtk:paned-new)))
    (is (eq :multiple
            (setf (gtk:list-box-selection-mode listbox) :multiple)))
    (is-false (gtk:list-box-append listbox widget1))
    (is-false (gtk:list-box-append listbox widget2))
    (is-false (gtk:list-box-append listbox widget3))

    (is-false (gtk:list-box-select-all listbox))
    (is (equal '(GTK:BOX GTK:FRAME GTK:PANED)
               (mapcar #'type-of
                       (mapcar #'gtk:list-box-row-child
                               (gtk:list-box-selected-rows listbox)))))

    (is-false (gtk:list-box-unselect-all listbox))
    (is (equal '()
               (mapcar #'type-of
                       (mapcar #'gtk:list-box-row-child
                               (gtk:list-box-selected-rows listbox)))))

    ;; Remove children from the list box rows
    (is-false (setf (gtk:list-box-row-child
                        (gtk:list-box-row-at-index listbox 0)) nil))
    (is-false (setf (gtk:list-box-row-child
                        (gtk:list-box-row-at-index listbox 1)) nil))
    (is-false (setf (gtk:list-box-row-child
                        (gtk:list-box-row-at-index listbox 2)) nil))
    ;; Remove list box rows from list box
    (is-false (gtk:list-box-remove-all listbox))
    ;; Check memory management
    (is (= 1 (g:object-ref-count listbox)))
    (is (= 1 (g:object-ref-count widget1)))
    (is (= 1 (g:object-ref-count widget2)))
    (is (= 1 (g:object-ref-count widget3)))))

;;;     gtk_list_box_get_adjustment
;;;     gtk_list_box_set_adjustment

(test gtk-list-box-adjustment
  (let ((listbox (gtk:list-box-new))
        (adjustment (make-instance 'gtk:adjustment)))
    (is-false (gtk:list-box-adjustment listbox))
    (is (eq adjustment (setf (gtk:list-box-adjustment listbox) adjustment)))
    (is (eq adjustment (gtk:list-box-adjustment listbox)))
    (is-false (setf (gtk:list-box-adjustment listbox) nil))))

;;;     gtk_list_box_set_placeholder

;; TODO: We have no function to retrieve and remove the placeholder widget
;; from the list box. Therefore we cannot remove the strong reference from the
;; label widget.

(test gtk-list-box-set-placeholder
  (let ((listbox (gtk:list-box-new))
        (label (gtk:label-new "label")))
    (is-false (gtk:list-box-set-placeholder listbox label))))

;;;     gtk_list_box_get_row_at_y

;;;     GtkListBoxForeachFunc
;;;     gtk_list_box_selected_foreach

(test gtk-list-box-selected-foreach
  (let ((listbox (gtk:list-box-new))
        (widget1 (gtk:box-new))
        (widget2 (gtk:frame-new))
        (widget3 (gtk:paned-new))
        (msg nil))
    (is (eq :multiple
            (setf (gtk:list-box-selection-mode listbox) :multiple)))
    (is-false (gtk:list-box-append listbox widget1))
    (is-false (gtk:list-box-append listbox widget2))
    (is-false (gtk:list-box-append listbox widget3))

    (is-false (gtk:list-box-select-all listbox))

    (is-false (gtk:list-box-selected-foreach listbox
                  (lambda (listbox row)
                    (declare (ignore listbox))
                    (push (type-of (gtk:list-box-row-child row)) msg))))
    (is (equal '(GTK:PANED GTK:FRAME GTK:BOX) msg))

    ;; Remove children from the list box rows
    (is-false (setf (gtk:list-box-row-child
                        (gtk:list-box-row-at-index listbox 0)) nil))
    (is-false (setf (gtk:list-box-row-child
                        (gtk:list-box-row-at-index listbox 1)) nil))
    (is-false (setf (gtk:list-box-row-child
                        (gtk:list-box-row-at-index listbox 2)) nil))
    ;; Remove list box rows from list box
    (is-false (gtk:list-box-remove-all listbox))
    ;; Check memory management
    (is (= 1 (g:object-ref-count listbox)))
    (is (= 1 (g:object-ref-count widget1)))
    (is (= 1 (g:object-ref-count widget2)))
    (is (= 1 (g:object-ref-count widget3)))))

;;;     GtkListBoxUpdateHeaderFunc
;;;     gtk_list_box_set_header_func
;;;     gtk_list_box_invalidate_headers

;;;     GtkListBoxFilterFunc
;;;     gtk_list_box_set_filter_func
;;;     gtk_list_box_invalidate_filter

;;;     GtkListBoxSortFunc
;;;     gtk_list_box_set_sort_func
;;;     gtk_list_box_invalidate_sort

;;;     gtk_list_box_drag_highlight_row
;;;     gtk_list_box_drag_unhighlight_row

;;;     GtkListBoxCreateWidgetFunc
;;;     gtk_list_box_bind_model

;;; 2025-4-26
