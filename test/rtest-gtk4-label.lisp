(in-package :gtk-test)

(def-suite gtk-label :in gtk-suite)
(in-suite gtk-label)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkLabel

(test gtk-label-class
  ;; Check type
  (is (g:type-is-object "GtkLabel"))
  ;; Check registered name
  (is (eq 'gtk:label
          (glib:symbol-for-gtype "GtkLabel")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkLabel")
          (g:gtype (cffi:foreign-funcall "gtk_label_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkLabel")))
  ;; Check children
  (is (equal '()
             (list-children "GtkLabel")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (list-interfaces "GtkLabel")))
  ;; Check properties
  (is (equal '("attributes" "ellipsize" "extra-menu" "justify" "label" "lines"
               "max-width-chars" "mnemonic-keyval" "mnemonic-widget"
               "natural-wrap-mode" "selectable" "single-line-mode" "tabs"
               "use-markup" "use-underline" "width-chars" "wrap" "wrap-mode"
               "xalign" "yalign")
             (list-properties "GtkLabel")))
  ;; Check signals
  (is (equal '("activate-current-link" "activate-link" "copy-clipboard"
               "move-cursor")
             (list-signals "GtkLabel")))
  ;; Check CSS name
  (is (string= "label"
               (gtk:widget-class-css-name "GtkLabel")))
  ;; Check CSS classes
  (is (equal '()
             (gtk:widget-css-classes (make-instance 'gtk:label))))
  ;; Check accessible role
  (is (eq :label (gtk:widget-class-accessible-role "GtkLabel")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkLabel" GTK-LABEL
                       (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
                        :TYPE-INITIALIZER "gtk_label_get_type")
                       ((ATTRIBUTES GTK-LABEL-ATTRIBUTES "attributes"
                         "PangoAttrList" T T)
                        (ELLIPSIZE GTK-LABEL-ELLIPSIZE "ellipsize"
                         "PangoEllipsizeMode" T T)
                        (EXTRA-MENU GTK-LABEL-EXTRA-MENU "extra-menu"
                         "GMenuModel" T T)
                        (JUSTIFY GTK-LABEL-JUSTIFY "justify" "GtkJustification"
                         T T)
                        (LABEL GTK-LABEL-LABEL "label" "gchararray" T T)
                        (LINES GTK-LABEL-LINES "lines" "gint" T T)
                        (MAX-WIDTH-CHARS GTK-LABEL-MAX-WIDTH-CHARS
                         "max-width-chars" "gint" T T)
                        (MNEMONIC-KEYVAL GTK-LABEL-MNEMONIC-KEYVAL
                         "mnemonic-keyval" "guint" T NIL)
                        (MNEMONIC-WIDGET GTK-LABEL-MNEMONIC-WIDGET
                         "mnemonic-widget" "GtkWidget" T T)
                        (NATURAL-WRAP-MODE GTK-LABEL-NATURAL-WRAP-MODE
                         "natural-wrap-mode" "GtkNaturalWrapMode" T T)
                        (SELECTABLE GTK-LABEL-SELECTABLE "selectable"
                         "gboolean" T T)
                        (SINGLE-LINE-MODE GTK-LABEL-SINGLE-LINE-MODE
                         "single-line-mode" "gboolean" T T)
                        (TABS GTK-LABEL-TABS "tabs" "PangoTabArray" T T)
                        (USE-MARKUP GTK-LABEL-USE-MARKUP "use-markup"
                         "gboolean" T T)
                        (USE-UNDERLINE GTK-LABEL-USE-UNDERLINE "use-underline"
                         "gboolean" T T)
                        (WIDTH-CHARS GTK-LABEL-WIDTH-CHARS "width-chars" "gint"
                         T T)
                        (WRAP GTK-LABEL-WRAP "wrap" "gboolean" T T)
                        (WRAP-MODE GTK-LABEL-WRAP-MODE "wrap-mode"
                         "PangoWrapMode" T T)
                        (XALIGN GTK-LABEL-XALIGN "xalign" "gfloat" T T)
                        (YALIGN GTK-LABEL-YALIGN "yalign" "gfloat" T T)))
             (gobject:get-g-type-definition "GtkLabel"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-label-properties
  (let ((label (make-instance 'gtk:label)))
    (is-false (gtk:label-attributes label))
    (is (eq :none (gtk:label-ellipsize label)))
    (is-false (gtk:label-extra-menu label))
    (is (eq :left (gtk:label-justify label)))
    (is (string= "" (gtk:label-label label)))
    (is (= -1 (gtk:label-lines label)))
    (is (= -1 (gtk:label-max-width-chars label)))
    (is (= 16777215 (gtk:label-mnemonic-keyval label)))
    (is-false (gtk:label-mnemonic-widget label))
    (is (eq :inherit (gtk:label-natural-wrap-mode label)))
    (is-false (gtk:label-selectable label))
    (is-false (gtk:label-single-line-mode label))
    #+gtk-4-8
    (is-false (gtk:label-tabs label))
    (is-false (gtk:label-use-markup label))
    (is-false (gtk:label-use-underline label))
    (is (= -1 (gtk:label-width-chars label)))
    (is-false (gtk:label-wrap label))
    (is (eq :word (gtk:label-wrap-mode label)))
    (is (= 0.5 (gtk:label-xalign label)))
    (is (= 0.5 (gtk:label-yalign label)))))

;;; --- Signals ----------------------------------------------------------------

;;;     activate-current-link
;;;     activate-link
;;;     copy-clipboard
;;;     move-cursor

;;; --- Functions --------------------------------------------------------------

;;;     gtk_label_new

(test gtk-label-new.1
  (is (typep (gtk:label-new "label") 'gtk:label)))

(test gtk-label-new.2
  (let ((label (gtk:label-new "label")))
    (is (string= "label" (gtk:label-label label)))
    (is (string= "label" (gtk:label-text label)))))

;;;     gtk_label_new_with_mnemonic

(test gtk-label-new-with-mnemonic.1
  (is (typep (gtk:label-new-with-mnemonic "_label") 'gtk:label)))

(test gtk-label-new-with-mnemonic.2
  (let ((label (gtk:label-new-with-mnemonic "_label")))
    (is (string= "_label" (gtk:label-label label)))
    (is (string= "label" (gtk:label-text label)))
    (is-true (gtk:label-use-underline label))))

;;;     gtk_label_get_text
;;;     gtk_label_set_text

(test gtk-label-text
  (let ((label (gtk:label-new-with-mnemonic "_label")))

    (is (string= "_label" (gtk:label-label label)))
    (is (string= "label" (gtk:label-text label)))

    (is (string= "New" (setf (gtk:label-text label) "New")))
    (is (string= "New" (gtk:label-label label)))
    (is (string= "New" (gtk:label-text label)))))

;;;     gtk_label_set_markup

(test gtk-label-set-markup
  (let ((label (make-instance 'gtk:label)))
    (is-false (gtk:label-set-markup label
                                    "<small>Small text</small>"))
    (is-true (gtk:label-use-markup label))
    (is-false (gtk:label-use-underline label))
    (is (string= "<small>Small text</small>"
                 (gtk:label-label label)))
    (is (string= "Small text" (gtk:label-text label)))))

;;;     gtk_label_set_text_with_mnemonic

(test gtk-label-set-text-with-mnemonic
  (let ((label (make-instance 'gtk:label)))

    (is-false (gtk:label-set-text-with-mnemonic label "_label"))

    (is (string= "_label" (gtk:label-label label)))
    (is (string= "label" (gtk:label-text label)))))

;;;     gtk_label_set_markup_with_mnemonic

(test gtk-label-set-markup-with-mnemonic
  (let ((label (make-instance 'gtk:label)))
    (is-false (gtk:label-set-markup-with-mnemonic label
                                                  "<small>_Small text</small>"))
    (is-true (gtk:label-use-markup label))
    (is-true (gtk:label-use-underline label))
    (is (string= "<small>_Small text</small>"
                 (gtk:label-label label)))
    (is (string= "Small text" (gtk:label-text label)))))

;;;     gtk_label_get_layout

(test gtk-label-layout
  (let ((label (gtk:label-new "label")))
    (is (typep (gtk:label-layout label) 'pango:layout))))

;;;     gtk_label_get_layout_offsets

(test gtk-label-layout-offsets
  (let ((label (gtk:label-new "label")))

    (is (equal '(0 -9)
               (multiple-value-list (gtk:label-layout-offsets label))))))

;;;     gtk_label_select_region
;;;     gtk_label_get_selection_bounds

;; TODO: This test can cause a memory fault. Why?

;; GTK-LABEL-SELECT-REGION in GTK-LABEL []:
;;      Unexpected Error: #<SB-SYS:MEMORY-FAULT-ERROR {10025972C3}>
;; Unhandled memory fault at #xC..

#+nil
(test gtk-label-select-region
  (let ((label (gtk:label-new "a long label")))
    (is-true (setf (gtk:label-selectable label) t))
    (is (= 12 (length (gtk:label-text label))))
    (is-false (gtk:label-select-region label 3 7))
;    (is (equal '(3 7)
;               (multiple-value-list (gtk:label-selection-bounds label))))
))

;;;     gtk_label_get_current_uri

;;; 2024-4-24
