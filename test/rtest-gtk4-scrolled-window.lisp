(in-package :gtk-test)

(def-suite gtk-scrolled-window :in gtk-suite)
(in-suite gtk-scrolled-window)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPolicyType

(test gtk-policy-type
  ;; Check the type
  (is (g:type-is-enum "GtkPolicyType"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkPolicyType")
          (g:gtype (cffi:foreign-funcall "gtk_policy_type_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:policy-type
          (glib:symbol-for-gtype "GtkPolicyType")))
  ;; Check the names
  (is (equal '("GTK_POLICY_ALWAYS" "GTK_POLICY_AUTOMATIC" "GTK_POLICY_NEVER"
               "GTK_POLICY_EXTERNAL")
             (list-enum-item-name "GtkPolicyType")))
  ;; Check the values
  (is (equal '(0 1 2 3)
             (list-enum-item-value "GtkPolicyType")))
  ;; Check the nick names
  (is (equal '("always" "automatic" "never" "external")
             (list-enum-item-nick "GtkPolicyType")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkPolicyType"
                                     GTK-POLICY-TYPE
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gtk_policy_type_get_type")
                                     (:ALWAYS 0)
                                     (:AUTOMATIC 1)
                                     (:NEVER 2)
                                     (:EXTERNAL 3))
             (gobject:get-g-type-definition "GtkPolicyType"))))

;;;     GtkCornerType

(test gdk-window-type
  ;; Check the type
  (is (g:type-is-enum "GtkCornerType"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkCornerType")
          (g:gtype (cffi:foreign-funcall "gtk_corner_type_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:corner-type
          (glib:symbol-for-gtype "GtkCornerType")))
  ;; Check the names
  (is (equal '("GTK_CORNER_TOP_LEFT" "GTK_CORNER_BOTTOM_LEFT"
               "GTK_CORNER_TOP_RIGHT" "GTK_CORNER_BOTTOM_RIGHT")
             (list-enum-item-name "GtkCornerType")))
  ;; Check the values
  (is (equal '(0 1 2 3)
             (list-enum-item-value "GtkCornerType")))
  ;; Check the nick names
  (is (equal '("top-left" "bottom-left" "top-right" "bottom-right")
             (list-enum-item-nick "GtkCornerType")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkCornerType"
                                     GTK-CORNER-TYPE
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gtk_corner_type_get_type")
                                     (:TOP-LEFT 0)
                                     (:BOTTOM-LEFT 1)
                                     (:TOP-RIGHT 2)
                                     (:BOTTOM-RIGHT 3))
             (gobject:get-g-type-definition "GtkCornerType"))))

;;;     GtkScrolledWindow

(test gtk-scrolled-window-class
  ;; Type check
  (is (g:type-is-object "GtkScrolledWindow"))
  ;; Check the registered name
  (is (eq 'gtk:scrolled-window
          (glib:symbol-for-gtype "GtkScrolledWindow")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkScrolledWindow")
          (g:gtype (cffi:foreign-funcall "gtk_scrolled_window_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkScrolledWindow")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkScrolledWindow")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (list-interfaces "GtkScrolledWindow")))
  ;; Check the properties
  (is (equal '("child" "hadjustment" "has-frame" "hscrollbar-policy"
               "kinetic-scrolling" "max-content-height" "max-content-width"
               "min-content-height" "min-content-width" "overlay-scrolling"
               "propagate-natural-height" "propagate-natural-width"
               "vadjustment" "vscrollbar-policy" "window-placement")
             (list-properties "GtkScrolledWindow")))
  ;; Check the signals
  (is (equal '("edge-overshot" "edge-reached" "move-focus-out" "scroll-child")
             (list-signals "GtkScrolledWindow")))
  ;; CSS name
  (is (string= "scrolledwindow"
               (gtk:widget-class-css-name "GtkScrolledWindow")))
  ;; CSS classes
  (is (equal '()
             (gtk:widget-css-classes (make-instance 'gtk:scrolled-window))))
  ;; Accessible role
  (is (eq :group (gtk:widget-class-accessible-role "GtkScrolledWindow")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkScrolledWindow" GTK-SCROLLED-WINDOW
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkBuildable"
                                 "GtkConstraintTarget")
                                :TYPE-INITIALIZER
                                "gtk_scrolled_window_get_type")
                               ((CHILD GTK-SCROLLED-WINDOW-CHILD "child"
                                 "GtkWidget" T T)
                                (HADJUSTMENT GTK-SCROLLED-WINDOW-HADJUSTMENT
                                 "hadjustment" "GtkAdjustment" T T)
                                (HAS-FRAME GTK-SCROLLED-WINDOW-HAS-FRAME
                                 "has-frame" "gboolean" T T)
                                (HSCROLLBAR-POLICY
                                 GTK-SCROLLED-WINDOW-HSCROLLBAR-POLICY
                                 "hscrollbar-policy" "GtkPolicyType" T T)
                                (KINETIC-SCROLLING
                                 GTK-SCROLLED-WINDOW-KINETIC-SCROLLING
                                 "kinetic-scrolling" "gboolean" T T)
                                (MAX-CONTENT-HEIGHT
                                 GTK-SCROLLED-WINDOW-MAX-CONTENT-HEIGHT
                                 "max-content-height" "gint" T T)
                                (MAX-CONTENT-WIDTH
                                 GTK-SCROLLED-WINDOW-MAX-CONTENT-WIDTH
                                 "max-content-width" "gint" T T)
                                (MIN-CONTENT-HEIGHT
                                 GTK-SCROLLED-WINDOW-MIN-CONTENT-HEIGHT
                                 "min-content-height" "gint" T T)
                                (MIN-CONTENT-WIDTH
                                 GTK-SCROLLED-WINDOW-MIN-CONTENT-WIDTH
                                 "min-content-width" "gint" T T)
                                (OVERLAY-SCROLLING
                                 GTK-SCROLLED-WINDOW-OVERLAY-SCROLLING
                                 "overlay-scrolling" "gboolean" T T)
                                (PROPAGATE-NATURAL-HEIGHT
                                 GTK-SCROLLED-WINDOW-PROPAGATE-NATURAL-HEIGHT
                                 "propagate-natural-height" "gboolean" T T)
                                (PROPAGATE-NATURAL-WIDTH
                                 GTK-SCROLLED-WINDOW-PROPAGATE-NATURAL-WIDTH
                                 "propagate-natural-width" "gboolean" T T)
                                (VADJUSTMENT GTK-SCROLLED-WINDOW-VADJUSTMENT
                                 "vadjustment" "GtkAdjustment" T T)
                                (VSCROLLBAR-POLICY
                                 GTK-SCROLLED-WINDOW-VSCROLLBAR-POLICY
                                 "vscrollbar-policy" "GtkPolicyType" T T)
                                (WINDOW-PLACEMENT
                                 GTK-SCROLLED-WINDOW-WINDOW-PLACEMENT
                                 "window-placement" "GtkCornerType" T T)))
             (gobject:get-g-type-definition "GtkScrolledWindow"))))

;;; --- Properties -------------------------------------------------------------

;;;     child
;;;     hadjustment
;;;     has-frame
;;;     hscrollbar-policy
;;;     kinetic-scrolling
;;;     max-content-height
;;;     max-content-width
;;;     min-content-height
;;;     min-content-width
;;;     overlay-scrolling
;;;     propagate-natural-height
;;;     propagate-natural-width
;;;     vadjustment
;;;     vscrollbar-policy
;;;     window-placement

(test gtk-scrolled-window-properties
  (let ((window (make-instance 'gtk:scrolled-window)))
    (is-false (gtk:scrolled-window-child window))
    (is (typep (gtk:scrolled-window-hadjustment window) 'gtk:adjustment))
    (is-false (gtk:scrolled-window-has-frame window))
    (is (eq :automatic (gtk:scrolled-window-hscrollbar-policy window)))
    (is-true (gtk:scrolled-window-kinetic-scrolling window))
    (is (= -1 (gtk:scrolled-window-max-content-height window)))
    (is (= -1 (gtk:scrolled-window-max-content-width window)))
    (is (= -1 (gtk:scrolled-window-min-content-height window)))
    (is (= -1 (gtk:scrolled-window-min-content-width window)))
    (is-true (gtk:scrolled-window-overlay-scrolling window))
    (is-false (gtk:scrolled-window-propagate-natural-height window))
    (is-false (gtk:scrolled-window-propagate-natural-width window))
    (is (typep (gtk:scrolled-window-vadjustment window) 'gtk:adjustment))
    (is (eq :automatic (gtk:scrolled-window-vscrollbar-policy window)))
    (is (eq :top-left (gtk:scrolled-window-window-placement window)))))

;;; --- Signals ----------------------------------------------------------------

;;;     edge-overshot
;;;     edge-reached
;;;     move-focus-out
;;;     scroll-child

;;; --- Functions --------------------------------------------------------------

;;;     gtk_scrolled_window_new

(test gtk-scrolled-window-new
  (is (typep (gtk:scrolled-window-new) 'gtk:scrolled-window))
  (is (typep (gtk:scrolled-window-new (make-instance 'gtk:adjustment))
             'gtk:scrolled-window))
  (is (typep (gtk:scrolled-window-new (make-instance 'gtk:adjustment)
                                      (make-instance 'gtk:adjustment))
             'gtk:scrolled-window)))

;;;     gtk_scrolled_window_get_hscrollbar
;;;     gtk_scrolled_window_get_vscrollbar
;;;     gtk_scrolled_window_get_policy
;;;     gtk_scrolled_window_set_policy
;;;     gtk_scrolled_window_get_placement
;;;     gtk_scrolled_window_set_placement
;;;     gtk_scrolled_window_unset_placement

;;; --- 2023-8-6 ---------------------------------------------------------------
