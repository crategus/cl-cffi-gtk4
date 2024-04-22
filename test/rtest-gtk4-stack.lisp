(in-package :gtk-test)

(def-suite gtk-stack :in gtk-suite)
(in-suite gtk-stack)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkStackTransitionType

(test gtk-stack-transition-type
  ;; Check type
  (is (g:type-is-enum "GtkStackTransitionType"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkStackTransitionType")
          (g:gtype (cffi:foreign-funcall "gtk_stack_transition_type_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gtk:stack-transition-type
          (glib:symbol-for-gtype "GtkStackTransitionType")))
  ;; Check names
  (is (equal '("GTK_STACK_TRANSITION_TYPE_NONE"
               "GTK_STACK_TRANSITION_TYPE_CROSSFADE"
               "GTK_STACK_TRANSITION_TYPE_SLIDE_RIGHT"
               "GTK_STACK_TRANSITION_TYPE_SLIDE_LEFT"
               "GTK_STACK_TRANSITION_TYPE_SLIDE_UP"
               "GTK_STACK_TRANSITION_TYPE_SLIDE_DOWN"
               "GTK_STACK_TRANSITION_TYPE_SLIDE_LEFT_RIGHT"
               "GTK_STACK_TRANSITION_TYPE_SLIDE_UP_DOWN"
               "GTK_STACK_TRANSITION_TYPE_OVER_UP"
               "GTK_STACK_TRANSITION_TYPE_OVER_DOWN"
               "GTK_STACK_TRANSITION_TYPE_OVER_LEFT"
               "GTK_STACK_TRANSITION_TYPE_OVER_RIGHT"
               "GTK_STACK_TRANSITION_TYPE_UNDER_UP"
               "GTK_STACK_TRANSITION_TYPE_UNDER_DOWN"
               "GTK_STACK_TRANSITION_TYPE_UNDER_LEFT"
               "GTK_STACK_TRANSITION_TYPE_UNDER_RIGHT"
               "GTK_STACK_TRANSITION_TYPE_OVER_UP_DOWN"
               "GTK_STACK_TRANSITION_TYPE_OVER_DOWN_UP"
               "GTK_STACK_TRANSITION_TYPE_OVER_LEFT_RIGHT"
               "GTK_STACK_TRANSITION_TYPE_OVER_RIGHT_LEFT"
               "GTK_STACK_TRANSITION_TYPE_ROTATE_LEFT"
               "GTK_STACK_TRANSITION_TYPE_ROTATE_RIGHT"
               "GTK_STACK_TRANSITION_TYPE_ROTATE_LEFT_RIGHT")
             (list-enum-item-name "GtkStackTransitionType")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22)
             (list-enum-item-value "GtkStackTransitionType")))
  ;; Check nick names
  (is (equal '("none" "crossfade" "slide-right" "slide-left" "slide-up"
               "slide-down" "slide-left-right" "slide-up-down" "over-up"
               "over-down" "over-left" "over-right" "under-up" "under-down"
               "under-left" "under-right" "over-up-down" "over-down-up"
               "over-left-right" "over-right-left" "rotate-left" "rotate-right"
               "rotate-left-right")
             (list-enum-item-nick "GtkStackTransitionType")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkStackTransitionType"
                                     GTK-STACK-TRANSITION-TYPE
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gtk_stack_transition_type_get_type")
                                     (:NONE 0)
                                     (:CROSSFADE 1)
                                     (:SLIDE-RIGHT 2)
                                     (:SLIDE-LEFT 3)
                                     (:SLIDE-UP 4)
                                     (:SLIDE-DOWN 5)
                                     (:SLIDE-LEFT-RIGHT 6)
                                     (:SLIDE-UP-DOWN 7)
                                     (:OVER-UP 8)
                                     (:OVER-DOWN 9)
                                     (:OVER-LEFT 10)
                                     (:OVER-RIGHT 11)
                                     (:UNDER-UP 12)
                                     (:UNDER-DOWN 13)
                                     (:UNDER-LEFT 14)
                                     (:UNDER-RIGHT 15)
                                     (:OVER-UP-DOWN 16)
                                     (:OVER-DOWN-UP 17)
                                     (:OVER-LEFT-RIGHT 18)
                                     (:OVER-RIGHT-LEFT 19)
                                     (:ROTATE-LEFT 20)
                                     (:ROTATE-RIGHT 21)
                                     (:ROTATE-LEFT-RIGHT 22))
             (gobject:get-g-type-definition "GtkStackTransitionType"))))

;;;     GtkStackPage

(test gtk-stack-page-class
  ;; Check type
  (is (g:type-is-object "GtkStackPage"))
  ;; Check registered name
  (is (eq 'gtk:stack-page
          (glib:symbol-for-gtype "GtkStackPage")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkStackPage")
          (g:gtype (cffi:foreign-funcall "gtk_stack_page_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkStackPage")))
  ;; Check children
  (is (equal '()
             (list-children "GtkStackPage")))
  ;; Check interfaces
  (is (equal '("GtkAccessible")
             (list-interfaces "GtkStackPage")))
  ;; Check properties
  (is (equal '("accessible-role" "child" "icon-name" "name" "needs-attention"
               "title" "use-underline" "visible")
             (list-properties "GtkStackPage")))
  ;; Check signals
  (is (equal '()
             (list-signals "GtkStackPage")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkStackPage" GTK-STACK-PAGE
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                                ("GtkAccessible") :TYPE-INITIALIZER
                                "gtk_stack_page_get_type")
                               ((CHILD GTK-STACK-PAGE-CHILD "child" "GtkWidget"
                                 T NIL)
                                (ICON-NAME GTK-STACK-PAGE-ICON-NAME "icon-name"
                                 "gchararray" T T)
                                (NAME GTK-STACK-PAGE-NAME "name" "gchararray" T
                                 T)
                                (NEEDS-ATTENTION GTK-STACK-PAGE-NEEDS-ATTENTION
                                 "needs-attention" "gboolean" T T)
                                (TITLE GTK-STACK-PAGE-TITLE "title"
                                 "gchararray" T T)
                                (USE-UNDERLINE GTK-STACK-PAGE-USE-UNDERLINE
                                 "use-underline" "gboolean" T T)
                                (VISIBLE GTK-STACK-PAGE-VISIBLE "visible"
                                 "gboolean" T T)))
             (gobject:get-g-type-definition "GtkStackPage"))))

;;; --- Properties (GtkStackPage) ----------------------------------------------

(test gtk-stack-page-properties
  (let ((stack (gtk:stack-new))
        (label (gtk:label-new "Stack Page Label"))
        (page nil))
    ;; Add a page to the stack
    (is-false (gtk:stack-add-titled stack label "label" "Label"))
    ;; Get the stack page
    (is (typep (setf page (gtk:stack-page stack label)) 'gtk:stack-page))
    ;; Check the properties
    (is (eq label (gtk:stack-page-child page)))
    (is-false (gtk:stack-page-icon-name page))
    (is (string= "label" (gtk:stack-page-name page)))
    (is-false (gtk:stack-page-needs-attention page))
    (is (string= "Label" (gtk:stack-page-title page)))
    (is-false (gtk:stack-page-use-underline page))
    (is-true (gtk:stack-page-visible page))))

;;; ----------------------------------------------------------------------------

;;;     GtkStack

(test gtk-stack-class
  ;; Check type
  (is (g:type-is-object "GtkStack"))
  ;; Check registered name
  (is (eq 'gtk:stack
          (glib:symbol-for-gtype "GtkStack")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkStack")
          (g:gtype (cffi:foreign-funcall "gtk_stack_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkStack")))
  ;; Check children
  (is (equal '()
             (list-children "GtkStack")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (list-interfaces "GtkStack")))
  ;; Check properties
  (is (equal '("hhomogeneous" "interpolate-size" "pages" "transition-duration"
               "transition-running" "transition-type" "vhomogeneous"
               "visible-child" "visible-child-name")
             (list-properties "GtkStack")))
  ;; Check signals
  (is (equal '()
             (list-signals "GtkStack")))
  ;; Check CSS name
  (is (string= "stack"
               (gtk:widget-class-css-name "GtkStack")))
  ;; Check CSS classes
  (is (equal '()
             (gtk:widget-css-classes (make-instance 'gtk:stack))))
  ;; Check accessible role
  (is (eq :group (gtk:widget-class-accessible-role "GtkStack")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkStack" GTK-STACK
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkBuildable"
                                 "GtkConstraintTarget")
                                :TYPE-INITIALIZER "gtk_stack_get_type")
                               ((HHOMOGENEOUS GTK-STACK-HHOMOGENEOUS
                                 "hhomogeneous" "gboolean" T T)
                                (INTERPOLATE-SIZE GTK-STACK-INTERPOLATE-SIZE
                                 "interpolate-size" "gboolean" T T)
                                (PAGES GTK-STACK-PAGES "pages"
                                 "GtkSelectionModel" T NIL)
                                (TRANSITION-DURATION
                                 GTK-STACK-TRANSITION-DURATION
                                 "transition-duration" "guint" T T)
                                (TRANSITION-RUNNING
                                 GTK-STACK-TRANSITION-RUNNING
                                 "transition-running" "gboolean" T NIL)
                                (TRANSITION-TYPE GTK-STACK-TRANSITION-TYPE
                                 "transition-type" "GtkStackTransitionType" T
                                 T)
                                (VHOMOGENEOUS GTK-STACK-VHOMOGENEOUS
                                 "vhomogeneous" "gboolean" T T)
                                (VISIBLE-CHILD GTK-STACK-VISIBLE-CHILD
                                 "visible-child" "GtkWidget" T T)
                                (VISIBLE-CHILD-NAME
                                 GTK-STACK-VISIBLE-CHILD-NAME
                                 "visible-child-name" "gchararray" T T)))
             (gobject:get-g-type-definition "GtkStack"))))

;;; --- Properties (GtkStack) --------------------------------------------------

(test gtk-stack-properties
  (let ((stack (make-instance 'gtk:stack)))
    (is-true (gtk:stack-hhomogeneous stack))
    (is-false (gtk:stack-interpolate-size stack))
    ;; Should return a GtkSelectionModel, we check for g:object type
    (is (g:type-is-object (g:type-from-instance (gtk:stack-pages stack))))
    (is (= 200 (gtk:stack-transition-duration stack)))
    (is-false (gtk:stack-transition-running stack))
    (is (eq :none (gtk:stack-transition-type stack)))
    (is-true (gtk:stack-vhomogeneous stack))
    (is-false (gtk:stack-visible-child stack))
    (is-false (gtk:stack-visible-child-name stack))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_stack_new

(test gtk-stack-new
  (is (typep (gtk:stack-new) 'gtk:stack)))

;;;     gtk_stack_add_child
;;;     gtk_stack_add_named
;;;     gtk_stack_add_titled
;;;     gtk_stack_remove
;;;     gtk_stack_get_child_by_name
;;;     gtk_stack_get_page
;;;     gtk_stack_set_visible_child_full

;;; 2024-4-14
