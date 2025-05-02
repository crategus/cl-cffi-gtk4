(in-package :gtk-test)

(def-suite gtk-scrolled-window :in gtk-scrolling)
(in-suite gtk-scrolled-window)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPolicyType

(test gtk-policy-type
  ;; Check type
  (is (g:type-is-enum "GtkPolicyType"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkPolicyType")
          (g:gtype (cffi:foreign-funcall "gtk_policy_type_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:policy-type
          (glib:symbol-for-gtype "GtkPolicyType")))
  ;; Check names
  (is (equal '("GTK_POLICY_ALWAYS" "GTK_POLICY_AUTOMATIC" "GTK_POLICY_NEVER"
               "GTK_POLICY_EXTERNAL")
             (glib-test:list-enum-item-names "GtkPolicyType")))
  ;; Check values
  (is (equal '(0 1 2 3)
             (glib-test:list-enum-item-values "GtkPolicyType")))
  ;; Check nick names
  (is (equal '("always" "automatic" "never" "external")
             (glib-test:list-enum-item-nicks "GtkPolicyType")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkPolicyType" GTK:POLICY-TYPE
                                    (:EXPORT T
                                     :TYPE-INITIALIZER
                                     "gtk_policy_type_get_type")
                                    (:ALWAYS 0)
                                    (:AUTOMATIC 1)
                                    (:NEVER 2)
                                    (:EXTERNAL 3))
             (gobject:get-gtype-definition "GtkPolicyType"))))

;;;     GtkCornerType

(test gtk-corner-type
  ;; Check type
  (is (g:type-is-enum "GtkCornerType"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkCornerType")
          (g:gtype (cffi:foreign-funcall "gtk_corner_type_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:corner-type
          (glib:symbol-for-gtype "GtkCornerType")))
  ;; Check names
  (is (equal '("GTK_CORNER_TOP_LEFT" "GTK_CORNER_BOTTOM_LEFT"
               "GTK_CORNER_TOP_RIGHT" "GTK_CORNER_BOTTOM_RIGHT")
             (glib-test:list-enum-item-names "GtkCornerType")))
  ;; Check values
  (is (equal '(0 1 2 3)
             (glib-test:list-enum-item-values "GtkCornerType")))
  ;; Check nick names
  (is (equal '("top-left" "bottom-left" "top-right" "bottom-right")
             (glib-test:list-enum-item-nicks "GtkCornerType")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkCornerType" GTK:CORNER-TYPE
                                    (:EXPORT T
                                     :TYPE-INITIALIZER
                                     "gtk_corner_type_get_type")
                                    (:TOP-LEFT 0)
                                    (:BOTTOM-LEFT 1)
                                    (:TOP-RIGHT 2)
                                    (:BOTTOM-RIGHT 3))
             (gobject:get-gtype-definition "GtkCornerType"))))

;;;     GtkScrolledWindow

(test gtk-scrolled-window-class
  ;; Check type
  (is (g:type-is-object "GtkScrolledWindow"))
  ;; Check registered name
  (is (eq 'gtk:scrolled-window
          (glib:symbol-for-gtype "GtkScrolledWindow")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkScrolledWindow")
          (g:gtype (cffi:foreign-funcall "gtk_scrolled_window_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkScrolledWindow")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkScrolledWindow")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (glib-test:list-interfaces "GtkScrolledWindow")))
  ;; Check properties
  (is (equal '("child" "hadjustment" "has-frame" "hscrollbar-policy"
               "kinetic-scrolling" "max-content-height" "max-content-width"
               "min-content-height" "min-content-width" "overlay-scrolling"
               "propagate-natural-height" "propagate-natural-width"
               "vadjustment" "vscrollbar-policy" "window-placement")
             (glib-test:list-properties "GtkScrolledWindow")))
  ;; Check signals
  (is (equal '("edge-overshot" "edge-reached" "move-focus-out" "scroll-child")
             (glib-test:list-signals "GtkScrolledWindow")))
  ;; Check CSS name
  (is (string= "scrolledwindow"
               (gtk:widget-class-css-name "GtkScrolledWindow")))
  ;; Check accessible role
  (is (eq :generic (gtk:widget-class-accessible-role "GtkScrolledWindow")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkScrolledWindow" GTK:SCROLLED-WINDOW
                      (:SUPERCLASS GTK:WIDGET
                       :EXPORT T
                       :INTERFACES
                       ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
                       :TYPE-INITIALIZER "gtk_scrolled_window_get_type")
                      ((CHILD SCROLLED-WINDOW-CHILD "child" "GtkWidget" T T)
                       (HADJUSTMENT
                        SCROLLED-WINDOW-HADJUSTMENT
                        "hadjustment" "GtkAdjustment" T T)
                       (HAS-FRAME
                        SCROLLED-WINDOW-HAS-FRAME
                        "has-frame" "gboolean" T T)
                       (HSCROLLBAR-POLICY
                        SCROLLED-WINDOW-HSCROLLBAR-POLICY
                        "hscrollbar-policy" "GtkPolicyType" T T)
                       (KINETIC-SCROLLING
                        SCROLLED-WINDOW-KINETIC-SCROLLING
                        "kinetic-scrolling" "gboolean" T T)
                       (MAX-CONTENT-HEIGHT
                        SCROLLED-WINDOW-MAX-CONTENT-HEIGHT
                        "max-content-height" "gint" T T)
                       (MAX-CONTENT-WIDTH
                        SCROLLED-WINDOW-MAX-CONTENT-WIDTH
                        "max-content-width" "gint" T T)
                       (MIN-CONTENT-HEIGHT
                        SCROLLED-WINDOW-MIN-CONTENT-HEIGHT
                        "min-content-height" "gint" T T)
                       (MIN-CONTENT-WIDTH
                        SCROLLED-WINDOW-MIN-CONTENT-WIDTH
                        "min-content-width" "gint" T T)
                       (OVERLAY-SCROLLING
                        SCROLLED-WINDOW-OVERLAY-SCROLLING
                        "overlay-scrolling" "gboolean" T T)
                       (PROPAGATE-NATURAL-HEIGHT
                        SCROLLED-WINDOW-PROPAGATE-NATURAL-HEIGHT
                        "propagate-natural-height" "gboolean" T T)
                       (PROPAGATE-NATURAL-WIDTH
                        SCROLLED-WINDOW-PROPAGATE-NATURAL-WIDTH
                        "propagate-natural-width" "gboolean" T T)
                       (VADJUSTMENT
                        SCROLLED-WINDOW-VADJUSTMENT
                        "vadjustment" "GtkAdjustment" T T)
                       (VSCROLLBAR-POLICY
                        SCROLLED-WINDOW-VSCROLLBAR-POLICY
                        "vscrollbar-policy" "GtkPolicyType" T T)
                       (WINDOW-PLACEMENT
                        SCROLLED-WINDOW-WINDOW-PLACEMENT
                        "window-placement" "GtkCornerType" T T)))
             (gobject:get-gtype-definition "GtkScrolledWindow"))))

;;; --- Signals ----------------------------------------------------------------

;;;     edge-overshot

(test gtk-scrolled-window-edge-overshot-signal
  (let* ((name "edge-overshot")
         (gtype (g:gtype "GtkScrolledWindow"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '("GtkPositionType")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     edge-reached

(test gtk-scrolled-window-edge-reached-signal
  (let* ((name "edge-reached")
         (gtype (g:gtype "GtkScrolledWindow"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '("GtkPositionType")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     move-focus-out

(test gtk-scrolled-window-move-focus-out-signal
  (let* ((name "move-focus-out")
         (gtype (g:gtype "GtkScrolledWindow"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '("GtkDirectionType")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     scroll-child

(test gtk-scrolled-window-scroll-child-signal
  (let* ((name "scroll-child")
         (gtype (g:gtype "GtkScrolledWindow"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "gboolean" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '("GtkScrollType" "gboolean")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Properties -------------------------------------------------------------

(test gtk-scrolled-window-properties
  (glib-test:with-check-memory (window)
    (is (typep (setf window
                     (make-instance 'gtk:scrolled-window)) 'gtk:scrolled-window))
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
    (is (eq :top-left (gtk:scrolled-window-window-placement window)))
    ;; Remove references
    (is-false (setf (gtk:scrolled-window-hadjustment window) nil))
    (is-false (setf (gtk:scrolled-window-vadjustment window) nil))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_scrolled_window_new

(test gtk-scrolled-window-new
  (glib-test:with-check-memory (window)
    (is (typep (setf window (gtk:scrolled-window-new)) 'gtk:scrolled-window))
    (is-false (setf (gtk:scrolled-window-hadjustment window) nil))
    (is-false (setf (gtk:scrolled-window-vadjustment window) nil))

    (is (typep (setf window
                     (gtk:scrolled-window-new (make-instance 'gtk:adjustment)))
               'gtk:scrolled-window))
    (is-false (setf (gtk:scrolled-window-hadjustment window) nil))
    (is-false (setf (gtk:scrolled-window-vadjustment window) nil))

    (is (typep (setf window
                     (gtk:scrolled-window-new (make-instance 'gtk:adjustment)
                                              (make-instance 'gtk:adjustment)))
               'gtk:scrolled-window))
    (is-false (setf (gtk:scrolled-window-hadjustment window) nil))
    (is-false (setf (gtk:scrolled-window-vadjustment window) nil))))

;;;     gtk_scrolled_window_get_hscrollbar
;;;     gtk_scrolled_window_get_vscrollbar

(test gtk-scrolled-window-scrollbar
  (when *first-run-testsuite*
    (glib-test:with-check-memory (scrolled :strong 2)
      (is (typep (setf scrolled (gtk:scrolled-window-new)) 'gtk:scrolled-window))
      (is (typep (gtk:scrolled-window-hscrollbar scrolled) 'gtk:scrollbar))
      (is (typep (gtk:scrolled-window-vscrollbar scrolled) 'gtk:scrollbar)))))

;;;     gtk_scrolled_window_get_policy
;;;     gtk_scrolled_window_set_policy

(test gtk-scrolled-window-policy
  (let ((scrolled (gtk:scrolled-window-new)))
    (is (equal '(:automatic :automatic)
               (multiple-value-list (gtk:scrolled-window-policy scrolled))))
    (is (equal '(:always :never)
               (multiple-value-list (setf (gtk:scrolled-window-policy scrolled)
                                          '(:always :never)))))
    (is (equal '(:always :never)
               (multiple-value-list (gtk:scrolled-window-policy scrolled))))))

;;;     gtk_scrolled_window_get_placement
;;;     gtk_scrolled_window_set_placement
;;;     gtk_scrolled_window_unset_placement

(test gtk-scrolled-window-placement
  (let ((scrolled (gtk:scrolled-window-new)))
    (is (eq :top-left (gtk:scrolled-window-placement scrolled)))
    (is (eq :bottom-left
            (setf (gtk:scrolled-window-placement scrolled) :bottom-left)))
    (is (eq :bottom-left (gtk:scrolled-window-placement scrolled)))
    (is-false (gtk:scrolled-window-unset-placement scrolled))
    (is (eq :top-left (gtk:scrolled-window-placement scrolled)))))

;;; 2024-10-27
