(in-package :gtk-test)

(def-suite gtk-spin-button :in gtk-suite)
(in-suite gtk-spin-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkSpinButtonUpdatePolicy

(test gtk-spint-button-update-policy
  ;; Check the type
  (is (g:type-is-enum "GtkSpinButtonUpdatePolicy"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkSpinButtonUpdatePolicy")
          (g:gtype (cffi:foreign-funcall "gtk_spin_button_update_policy_get_type" 
                                         :size))))
  ;; Check the registered name
  (is (eq 'gtk:spin-button-update-policy
          (glib:symbol-for-gtype "GtkSpinButtonUpdatePolicy")))
  ;; Check the names
  (is (equal '("GTK_UPDATE_ALWAYS" "GTK_UPDATE_IF_VALID")
             (list-enum-item-name "GtkSpinButtonUpdatePolicy")))
  ;; Check the values
  (is (equal '(0 1)
             (list-enum-item-value "GtkSpinButtonUpdatePolicy")))
  ;; Check the nick names
  (is (equal '("always" "if-valid")
             (list-enum-item-nick "GtkSpinButtonUpdatePolicy")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkSpinButtonUpdatePolicy"
                                     GTK-SPIN-BUTTON-UPDATE-POLICY
                                     (:EXPORT T 
                                      :TYPE-INITIALIZER 
                                      "gtk_spin_button_update_policy_get_type")
                                     (:ALWAYS 0)
                                     (:IF-VALID 1))
             (gobject:get-g-type-definition "GtkSpinButtonUpdatePolicy"))))

;;;     GtkSpinType

(test gtk-spin-type
  ;; Check the type
  (is (g:type-is-enum "GtkSpinType"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkSpinType")
          (g:gtype (cffi:foreign-funcall "gtk_spin_type_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:spin-type
          (glib:symbol-for-gtype "GtkSpinType")))
  ;; Check the names
  (is (equal '("GTK_SPIN_STEP_FORWARD" "GTK_SPIN_STEP_BACKWARD" 
               "GTK_SPIN_PAGE_FORWARD" "GTK_SPIN_PAGE_BACKWARD" "GTK_SPIN_HOME" 
               "GTK_SPIN_END" "GTK_SPIN_USER_DEFINED")
             (list-enum-item-name "GtkSpinType")))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5 6)
             (list-enum-item-value "GtkSpinType")))
  ;; Check the nick names
  (is (equal '("step-forward" "step-backward" "page-forward" "page-backward" 
               "home" "end" "user-defined")
             (list-enum-item-nick "GtkSpinType")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkSpinType" GTK-SPIN-TYPE
                                     (:EXPORT T 
                                      :TYPE-INITIALIZER 
                                      "gtk_spin_type_get_type")
                                     (:STEP-FORWARD 0)
                                     (:STEP-BACKWARD 1)
                                     (:PAGE-FORWARD 2)
                                     (:PAGE-BACKWARD 3)
                                     (:HOME 4)
                                     (:END 5)
                                     (:USER-DEFINED 6))
             (gobject:get-g-type-definition "GtkSpinType"))))

;;;     GtkSpinButton

(test gtk-spin-button-class
  ;; Type check
  (is (g:type-is-object "GtkSpinButton"))
  ;; Check the registered name
  (is (eq 'gtk:spin-button
          (glib:symbol-for-gtype "GtkSpinButton")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkSpinButton")
          (g:gtype (cffi:foreign-funcall "gtk_spin_button_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkSpinButton")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkSpinButton")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget" 
               "GtkOrientable" "GtkAccessibleRange" "GtkEditable" 
               "GtkCellEditable")
             (list-interfaces "GtkSpinButton")))
  ;; Check the properties
  (is (equal '("adjustment" "climb-rate" "cursor-position" "digits" "editable"
               "editing-canceled" "enable-undo" "max-width-chars" "numeric" 
               "orientation" "selection-bound" "snap-to-ticks" "text" 
               "update-policy" "value" "width-chars" "wrap" "xalign")
             (list-properties "GtkSpinButton")))
  ;; Check the signals
  (is (equal '("change-value" "input" "output" "value-changed" "wrapped")
             (list-signals "GtkSpinButton")))
  ;; CSS name
  (is (string= "spinbutton"
               (gtk:widget-class-css-name "GtkSpinButton")))
  ;; CSS classes
  (is (equal '("horizontal")
             (gtk:widget-css-classes (make-instance 'gtk:spin-button))))
  ;; Accessible role
  (is (eq :SPIN-BUTTON
          (gtk:widget-class-accessible-role "GtkSpinButton")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkSpinButton" GTK-SPIN-BUTTON
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkAccessibleRange"
                                 "GtkBuildable" "GtkCellEditable"
                                 "GtkConstraintTarget" "GtkEditable"
                                 "GtkOrientable")
                                :TYPE-INITIALIZER "gtk_spin_button_get_type")
                               ((ADJUSTMENT GTK-SPIN-BUTTON-ADJUSTMENT
                                 "adjustment" "GtkAdjustment" T T)
                                (CLIMB-RATE GTK-SPIN-BUTTON-CLIMB-RATE
                                 "climb-rate" "gdouble" T T)
                                (DIGITS GTK-SPIN-BUTTON-DIGITS "digits" "guint"
                                 T T)
                                (NUMERIC GTK-SPIN-BUTTON-NUMERIC "numeric"
                                 "gboolean" T T)
                                (SNAP-TO-TICKS GTK-SPIN-BUTTON-SNAP-TO-TICKS
                                 "snap-to-ticks" "gboolean" T T)
                                (UPDATE-POLICY GTK-SPIN-BUTTON-UPDATE-POLICY
                                 "update-policy" "GtkSpinButtonUpdatePolicy" T
                                 T)
                                (VALUE GTK-SPIN-BUTTON-VALUE "value" "gdouble"
                                 T T)
                                (WRAP GTK-SPIN-BUTTON-WRAP "wrap" "gboolean" T
                                 T)))
             (gobject:get-g-type-definition "GtkSpinButton"))))

;;; --- Properties -------------------------------------------------------------

;;;     adjustment
;;;     climb-rate
;;;     digits
;;;     numeric
;;;     snap-to-ticks
;;;     update-policy
;;;     value
;;;     wrap

(test gtk-spin-button-properties
  (let ((spinbutton (make-instance 'gtk:spin-button)))
    (is (typep (gtk:spin-button-adjustment spinbutton) 'gtk:adjustment))
    (is (= 0.0d0 (gtk:spin-button-climb-rate spinbutton)))
    (is (= 0 (gtk:spin-button-digits spinbutton)))
    (is-false (gtk:spin-button-numeric spinbutton))
    (is-false (gtk:spin-button-snap-to-ticks spinbutton))
    (is (eq :always (gtk:spin-button-update-policy spinbutton)))
    (is (= 0.0d0 (gtk:spin-button-value spinbutton)))
    (is-false (gtk:spin-button-wrap spinbutton))))

;;; --- Signals ----------------------------------------------------------------

;;;     change-value

(test gtk-spin-button-change-value-signal
  (let ((query (g:signal-query (g:signal-lookup "change-value" 
                                                "GtkSpinButton"))))
    (is (string= "change-value" (g:signal-query-signal-name query)))
    (is (string= "GtkSpinButton" 
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '("GtkScrollType")
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;;     input

(test gtk-spin-button-input-signal
  (let ((query (g:signal-query (g:signal-lookup "input" "GtkSpinButton"))))
    (is (string= "input" (g:signal-query-signal-name query)))
    (is (string= "GtkSpinButton" 
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "gint" (g:type-name (g:signal-query-return-type query))))
    (is (equal '("gpointer")
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))
    
;;;     output

(test gtk-spin-button-output-signal
  (let ((query (g:signal-query (g:signal-lookup "output" "GtkSpinButton"))))
    (is (string= "output" (g:signal-query-signal-name query)))
    (is (string= "GtkSpinButton" 
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "gboolean" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;;     value-changed

(test gtk-spin-button-value-changed-signal
  (let ((query (g:signal-query (g:signal-lookup "value-changed" 
                                                "GtkSpinButton"))))
    (is (string= "value-changed" (g:signal-query-signal-name query)))
    (is (string= "GtkSpinButton" 
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;;     wrapped

(test gtk-spin-button-wrapped-signal
  (let ((query (g:signal-query (g:signal-lookup "wrapped" "GtkSpinButton"))))
    (is (string= "wrapped" (g:signal-query-signal-name query)))
    (is (string= "GtkSpinButton" 
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_spin_button_new

(test gtk-spin-button-new
  (is (typep (gtk:spin-button-new (make-instance 'gtk:adjustment) 2 10)
             'gtk:spin-button))
  (is (typep (gtk:spin-button-new nil 2 10) 'gtk:spin-button))
  (is (typep (gtk:spin-button-new nil 1.0d0 10) 'gtk:spin-button)))
  
;;;     gtk_spin_button_new_with_range

(test gtk-spin-button-new-with-range
  (is (typep (gtk:spin-button-new-with-range 5 15 5) 'gtk:spin-button)))

;;;     gtk_spin_button_set_increments
;;;     gtk_spin_button_get_increments
;;;     gtk_spin_button_set_range
;;;     gtk_spin_button_get_range
;;;     gtk_spin_button_get_value_as_int
;;;     gtk_spin_button_configure
;;;     gtk_spin_button_spin
;;;     gtk_spin_button_update

;;; --- 2023-9-30 --------------------------------------------------------------
