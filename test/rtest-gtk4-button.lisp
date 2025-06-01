(in-package :gtk-test)

(def-suite gtk-button :in gtk-buttons-and-toggles)
(in-suite gtk-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkButton

(test gtk-button-class
  ;; Check type
  (is (g:type-is-object "GtkButton"))
  ;; Check registered name
  (is (eq 'gtk:button
          (glib:symbol-for-gtype "GtkButton")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkButton")
          (g:gtype (cffi:foreign-funcall "gtk_button_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkButton")))
  ;; Check children
  (is (equal '("GtkLinkButton" "GtkLockButton" "GtkToggleButton")
             (glib-test:list-children "GtkButton")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkActionable")
             (glib-test:list-interfaces "GtkButton")))
  ;; Check class properties
  (is (equal '("action-name" "action-target" "can-shrink" "child" "has-frame"
               "icon-name" "label" "use-underline")
             (glib-test:list-properties "GtkButton")))
  ;; Check signals
  (is (equal '("activate" "clicked")
             (glib-test:list-signals "GtkButton")))
  ;; Check CSS information
  (is (string="button"
               (gtk:widget-class-css-name "GtkButton")))
  ;; Check accessibility role
  (is (eq :button (gtk:widget-class-accessible-role "GtkButton")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkButton" GTK:BUTTON
                      (:SUPERCLASS GTK:WIDGET
                       :EXPORT T
                       :INTERFACES
                       ("GtkAccessible" "GtkActionable" "GtkBuildable"
                        "GtkConstraintTarget")
                       :TYPE-INITIALIZER "gtk_button_get_type")
                      ((CAN-SHRINK BUTTON-CAN-SHRINK "can-shrink" "gboolean" T T)
                       (CHILD BUTTON-CHILD "child" "GtkWidget" T T)
                       (HAS-FRAME BUTTON-HAS-FRAME "has-frame" "gboolean" T T)
                       (ICON-NAME BUTTON-ICON-NAME "icon-name" "gchararray" T T)
                       (LABEL BUTTON-LABEL "label" "gchararray" T T)
                       (USE-UNDERLINE BUTTON-USE-UNDERLINE
                        "use-underline" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkButton"))))

;;; --- Signals ----------------------------------------------------------------

(test gtk-button-activate-signal
  (let ((query (g:signal-query (g:signal-lookup "activate" "GtkButton"))))
    (is (string= "activate" (g:signal-query-signal-name query)))
    (is (string= "GtkButton" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:ACTION :RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

(test gtk-button-clicked-signal
  (let ((query (g:signal-query (g:signal-lookup "clicked" "GtkButton"))))
    (is (string= "clicked" (g:signal-query-signal-name query)))
    (is (string= "GtkButton" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:ACTION :RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;; --- Properties -------------------------------------------------------------

(test gtk-button-properties
  (glib-test:with-check-memory (button)
    (is (typep (setf button (make-instance 'gtk:button)) 'gtk:button))
    (is-false (gtk:button-child button))
    (is-true  (gtk:button-has-frame button))
    (is-false (gtk:button-icon-name button))
    (is-false (gtk:button-label button))
    (is-false (gtk:button-use-underline button))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_button_new

(test gtk-button-new
  (glib-test:with-check-memory (button)
    (is (typep (setf button (gtk:button-new)) 'gtk:button))
    (is-false (gtk:button-child button))
    (is-true  (gtk:button-has-frame button))
    (is-false (gtk:button-icon-name button))
    (is-false (gtk:button-label button))
    (is-false (gtk:button-use-underline button))))

;;;     gtk_button_new_from_icon_name

(test gtk-button-new-from-icon-name
  (glib-test:with-check-memory (button)
    (is (typep (setf button
                     (gtk:button-new-from-icon-name "battery")) 'gtk:button))
    (is (eq (g:gtype "GtkImage")
            (g:type-from-instance (gtk:button-child button))))
    (is-true  (gtk:button-has-frame button))
    (is (string= "battery" (gtk:button-icon-name button)))
    (is-false (gtk:button-label button))
    (is-false (gtk:button-use-underline button))
    ;; Remove references
    (is-false (setf (gtk:button-child button) nil))))

;;;     gtk_button_new_with_label

(test gtk-button-new-with-label
  (glib-test:with-check-memory (button)
    (is (typep (setf button (gtk:button-new-with-label "battery")) 'gtk:button))
    (is (eq (g:gtype "GtkLabel")
            (g:type-from-instance (gtk:button-child button))))
    (is-true  (gtk:button-has-frame button))
    (is-false (gtk:button-icon-name button))
    (is (string= "battery" (gtk:button-label button)))
    (is-false (gtk:button-use-underline button))
    ;; Remove references
    (is-false (setf (gtk:button-child button) nil))))

;;;     gtk_button_new_with_mnemonic

(test gtk-button-new-with-mnemonic
  (glib-test:with-check-memory (button)
    (is (typep (setf button
                     (gtk:button-new-with-mnemonic "_battery")) 'gtk:button))
    (is (eq (g:gtype "GtkLabel")
            (g:type-from-instance (gtk:button-child button))))
    (is-true  (gtk:button-has-frame button))
    (is-false (gtk:button-icon-name button))
    (is (string= "_battery" (gtk:button-label button)))
    (is-true (gtk:button-use-underline button))
    ;; Remove references
    (is-false (setf (gtk:button-child button) nil))))

;;; 2025-05-30
