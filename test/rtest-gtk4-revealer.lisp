(in-package :gtk-test)

(def-suite gtk-revealer :in gtk-layout-widgets)
(in-suite gtk-revealer)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkRevealerTransitionType

(test gtk-revealer-transition-type
  ;; Check type
  (is (g:type-is-enum "GtkRevealerTransitionType"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkRevealerTransitionType")
          (g:gtype (cffi:foreign-funcall "gtk_revealer_transition_type_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gtk:revealer-transition-type
          (glib:symbol-for-gtype "GtkRevealerTransitionType")))
  ;; Check names
  (is (equal '("GTK_REVEALER_TRANSITION_TYPE_NONE"
               "GTK_REVEALER_TRANSITION_TYPE_CROSSFADE"
               "GTK_REVEALER_TRANSITION_TYPE_SLIDE_RIGHT"
               "GTK_REVEALER_TRANSITION_TYPE_SLIDE_LEFT"
               "GTK_REVEALER_TRANSITION_TYPE_SLIDE_UP"
               "GTK_REVEALER_TRANSITION_TYPE_SLIDE_DOWN"
               "GTK_REVEALER_TRANSITION_TYPE_SWING_RIGHT"
               "GTK_REVEALER_TRANSITION_TYPE_SWING_LEFT"
               "GTK_REVEALER_TRANSITION_TYPE_SWING_UP"
               "GTK_REVEALER_TRANSITION_TYPE_SWING_DOWN"
               "GTK_REVEALER_TRANSITION_TYPE_FADE_SLIDE_RIGHT"
               "GTK_REVEALER_TRANSITION_TYPE_FADE_SLIDE_LEFT"
               "GTK_REVEALER_TRANSITION_TYPE_FADE_SLIDE_UP"
               "GTK_REVEALER_TRANSITION_TYPE_FADE_SLIDE_DOWN")
             (glib-test:list-enum-item-names "GtkRevealerTransitionType")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5 6 7 8 9 10 11 12 13)
             (glib-test:list-enum-item-values "GtkRevealerTransitionType")))
  ;; Check nick names
  (is (equal '("none" "crossfade" "slide-right" "slide-left" "slide-up"
               "slide-down" "swing-right" "swing-left" "swing-up" "swing-down"
               "fade-slide-right" "fade-slide-left" "fade-slide-up"
               "fade-slide-down")
             (glib-test:list-enum-item-nicks "GtkRevealerTransitionType")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkRevealerTransitionType"
                                    GTK:REVEALER-TRANSITION-TYPE
                                    (:EXPORT T
                                     :TYPE-INITIALIZER
                                     "gtk_revealer_transition_type_get_type")
                                    (:NONE 0)
                                    (:CROSSFADE 1)
                                    (:SLIDE-RIGHT 2)
                                    (:SLIDE-LEFT 3)
                                    (:SLIDE-UP 4)
                                    (:SLIDE-DOWN 5)
                                    (:SWING-RIGHT 6)
                                    (:SWING-LEFT 7)
                                    (:SWING-UP 8)
                                    (:SWING-DOWN 9)
                                    (:FADE-SLIDE-RIGHT 10)
                                    (:FADE-SLIDE-LEFT 11)
                                    (:FADE-SLIDE-UP 12)
                                    (:FADE-SLIDE-DOWN 13))
             (gobject:get-gtype-definition "GtkRevealerTransitionType"))))

;;;     GtkRevealer

(test gtk-revealer-class
  ;; Check type
  (is (g:type-is-object "GtkRevealer"))
  ;; Check registered name
  (is (eq 'gtk:revealer
          (glib:symbol-for-gtype "GtkRevealer")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkRevealer")
          (g:gtype (cffi:foreign-funcall "gtk_revealer_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkRevealer")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkRevealer")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (glib-test:list-interfaces "GtkRevealer")))
  ;; Check properties
  (is (equal '("child" "child-revealed" "reveal-child" "transition-duration"
               "transition-type")
             (glib-test:list-properties "GtkRevealer")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkRevealer")))
  ;; Check CSS name
  (is (string= "revealer"
               (gtk:widget-class-css-name "GtkRevealer")))
  ;; Check accessible role
  (is (eq :group (gtk:widget-class-accessible-role "GtkRevealer")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkRevealer" GTK:REVEALER
                      (:SUPERCLASS GTK:WIDGET
                       :EXPORT T
                       :INTERFACES
                       ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
                       :TYPE-INITIALIZER "gtk_revealer_get_type")
                      ((CHILD REVEALER-CHILD "child" "GtkWidget" T T)
                       (CHILD-REVEALED REVEALER-CHILD-REVEALED
                        "child-revealed" "gboolean" T NIL)
                       (REVEAL-CHILD REVEALER-REVEAL-CHILD
                        "reveal-child" "gboolean" T T)
                       (TRANSITION-DURATION REVEALER-TRANSITION-DURATION
                        "transition-duration" "guint" T T)
                       (TRANSITION-TYPE REVEALER-TRANSITION-TYPE
                        "transition-type" "GtkRevealerTransitionType" T T)))
             (gobject:get-gtype-definition "GtkRevealer"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-revealer-properties
  (glib-test:with-check-memory (revealer child)
    (setf revealer (make-instance 'gtk:revealer))
    (setf child (gtk:button-new))
    ;; Property CHILD
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkRevealer" "child")))
    (is-false (gtk:revealer-child revealer))
    (is (eq child (setf (gtk:revealer-child revealer) child)))
    ;; Property CHILD-REVEALED
    (is (equal '(:READABLE)
               (glib-test:list-param-flags "GtkRevealer" "child-revealed")))
    (is-false (gtk:revealer-child-revealed revealer))
    ;; Property REVEAL-CHILD
    (is (equal '(:CONSTRUCT :READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkRevealer" "reveal-child")))
    (is-false (gtk:revealer-reveal-child revealer))
    (is-true (setf (gtk:revealer-reveal-child revealer) t))
    ;; Property TRANSITION-DURATION
    (is (equal '(:CONSTRUCT :READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkRevealer" "transition-duration")))
    (is (= 250 (gtk:revealer-transition-duration revealer)))
    (is (= 100 (setf (gtk:revealer-transition-duration revealer) 100)))
    ;; Property TRANSITION-TYPE
    (is (equal '(:CONSTRUCT :READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkRevealer" "transition-type")))
    (is (eq :slide-down (gtk:revealer-transition-type revealer)))
    (is (eq :swing-down (setf (gtk:revealer-transition-type revealer) :swing-down)))
    ;; Remove references
    (is-false (setf (gtk:revealer-child revealer) nil))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_revealer_new

(test gtk-revealer-new
  (glib-test:with-check-memory (revealer)
    (is (typep (setf revealer (gtk:revealer-new)) 'gtk:revealer))))

;;; 2026-05-23
