(in-package :gtk-test)

(def-suite gtk-label :in gtk-suite)
(in-suite gtk-label)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkLabel

(test label-class
  ;; Type check
  (is (g:type-is-object "GtkLabel"))
  ;; Check the registered name
  (is (eq 'gtk:label
          (gobject:symbol-for-gtype "GtkLabel")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkLabel")
          (g:gtype (foreign-funcall "gtk_label_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkLabel")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkLabel")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (list-interfaces "GtkLabel")))
  ;; Check the class properties
  (is (equal '("attributes" "ellipsize" "extra-menu" "justify" "label" "lines"
               "max-width-chars" "mnemonic-keyval" "mnemonic-widget"
               "natural-wrap-mode" "selectable" "single-line-mode" "use-markup"
               "use-underline" "width-chars" "wrap" "wrap-mode" "xalign"
               "yalign")
             (list-properties "GtkLabel")))
  ;; Check the list of signals
  (is (equal '("activate-current-link" "activate-link" "copy-clipboard"
               "move-cursor")
             (list-signals "GtkLabel")))
  ;; CSS information
  (is (string= "label"
               (gtk:widget-class-css-name "GtkLabel")))
  (is (string=
"label:dir(ltr)
"
               (gtk:style-context-to-string
                   (gtk:widget-style-context (make-instance 'gtk:label))
                   :none)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkLabel" GTK-LABEL
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

;;;     attributes
;;;     ellipsize
;;;     extra-menu
;;;     justify
;;;     label
;;;     lines
;;;     max-width-chars
;;;     mnemonic-keyval
;;;     mnemonic-widget
;;;     selectable
;;;     single-line-mode
;;;     use-markup
;;;     use-underline
;;;     width-chars
;;;     wrap
;;;     wrap-mode
;;;     xalign
;;;     yalign

(test label-properties
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
    (is-false (gtk:label-selectable label))
    (is-false (gtk:label-single-line-mode label))
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

;;; Functions

;;;     gtk_label_new
;;;     gtk_label_set_text
;;;     gtk_label_set_markup
;;;     gtk_label_set_markup_with_mnemonic
;;;     gtk_label_get_layout_offsets
;;;     gtk_label_get_text
;;;     gtk_label_new_with_mnemonic
;;;     gtk_label_select_region
;;;     gtk_label_set_text_with_mnemonic
;;;     gtk_label_get_layout
;;;     gtk_label_get_selection_bounds
;;;     gtk_label_get_current_uri

;;; 2022-11-11
