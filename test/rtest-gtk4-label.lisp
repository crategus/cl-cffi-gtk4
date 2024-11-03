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
             (glib-test:list-children "GtkLabel")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkAccessibleText")
             (glib-test:list-interfaces "GtkLabel")))
  ;; Check properties
  (is (equal '("attributes" "ellipsize" "extra-menu" "justify" "label" "lines"
               "max-width-chars" "mnemonic-keyval" "mnemonic-widget"
               "natural-wrap-mode" "selectable" "single-line-mode" "tabs"
               "use-markup" "use-underline" "width-chars" "wrap" "wrap-mode"
               "xalign" "yalign")
             (glib-test:list-properties "GtkLabel")))
  ;; Check signals
  (is (equal '("activate-current-link" "activate-link" "copy-clipboard"
               "move-cursor")
             (glib-test:list-signals "GtkLabel")))
  ;; Check CSS name
  (is (string= "label"
               (gtk:widget-class-css-name "GtkLabel")))
  ;; Check accessible role
  (is (eq :label (gtk:widget-class-accessible-role "GtkLabel")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkLabel" GTK:LABEL
                       (:SUPERCLASS GTK:WIDGET
                        :EXPORT T
                        :INTERFACES
                        ("GtkAccessible" "GtkAccessibleText" "GtkBuildable"
                         "GtkConstraintTarget")
                        :TYPE-INITIALIZER "gtk_label_get_type")
                       ((ATTRIBUTES LABEL-ATTRIBUTES
                         "attributes" "PangoAttrList" T T)
                        (ELLIPSIZE LABEL-ELLIPSIZE
                         "ellipsize" "PangoEllipsizeMode" T T)
                        (EXTRA-MENU LABEL-EXTRA-MENU
                         "extra-menu" "GMenuModel" T T)
                        (JUSTIFY LABEL-JUSTIFY "justify" "GtkJustification" T T)
                        (LABEL LABEL-LABEL "label" "gchararray" T T)
                        (LINES LABEL-LINES "lines" "gint" T T)
                        (MAX-WIDTH-CHARS LABEL-MAX-WIDTH-CHARS
                         "max-width-chars" "gint" T T)
                        (MNEMONIC-KEYVAL LABEL-MNEMONIC-KEYVAL
                         "mnemonic-keyval" "guint" T NIL)
                        (MNEMONIC-WIDGET LABEL-MNEMONIC-WIDGET
                         "mnemonic-widget" "GtkWidget" T T)
                        (NATURAL-WRAP-MODE LABEL-NATURAL-WRAP-MODE
                         "natural-wrap-mode" "GtkNaturalWrapMode" T T)
                        (SELECTABLE LABEL-SELECTABLE "selectable"
                         "gboolean" T T)
                        (SINGLE-LINE-MODE LABEL-SINGLE-LINE-MODE
                         "single-line-mode" "gboolean" T T)
                        (TABS LABEL-TABS "tabs" "PangoTabArray" T T)
                        (USE-MARKUP LABEL-USE-MARKUP "use-markup"
                         "gboolean" T T)
                        (USE-UNDERLINE LABEL-USE-UNDERLINE
                         "use-underline" "gboolean" T T)
                        (WIDTH-CHARS LABEL-WIDTH-CHARS "width-chars" "gint" T T)
                        (WRAP LABEL-WRAP "wrap" "gboolean" T T)
                        (WRAP-MODE LABEL-WRAP-MODE
                         "wrap-mode" "PangoWrapMode" T T)
                        (XALIGN LABEL-XALIGN "xalign" "gfloat" T T)
                        (YALIGN LABEL-YALIGN "yalign" "gfloat" T T)))
             (gobject:get-gtype-definition "GtkLabel"))))

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

(test gtk-label-activate-current-link-signal
  (let* ((name "activate-current-link")
         (gtype (g:gtype "GtkLabel"))
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

;;;     activate-link

(test gtk-label-activate-link-signal
  (let* ((name "activate-link")
         (gtype (g:gtype "GtkLabel"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "gboolean") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("gchararray")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     copy-clipboard

(test gtk-label-copy-clipboard-signal
  (let* ((name "copy-clipboard")
         (gtype (g:gtype "GtkLabel"))
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

(test gtk-label-move-cursor-signal
  (let* ((name "move-cursor")
         (gtype (g:gtype "GtkLabel"))
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
    (is (equal '("GtkMovementStep" "gint" "gboolean")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

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
  (let ((label (gtk:label-new "label"))
        (layout nil))
    (is (typep (setf layout
                     (gtk:label-layout label)) 'pango:layout))
    ;; Check memory management
    (is (string= "" (setf (gtk:label-label label) ""))) ; Clears layout
    (is (= 1 (g:object-ref-count layout)))
    (is (= 1 (g:object-ref-count label)))))

;;;     gtk_label_get_layout_offsets

(test gtk-label-layout-offsets
  (let ((label (gtk:label-new "label")))
    (is (equal '(-16 -9)
               (multiple-value-list (gtk:label-layout-offsets label))))))

;;;     gtk_label_select_region
;;;     gtk_label_get_selection_bounds

(test gtk-label-select-region
  (let ((label (gtk:label-new "a long label")))
    (is-true (setf (gtk:label-selectable label) t))
    (is (= 12 (length (gtk:label-text label))))
    (is-false (gtk:label-select-region label 3 7))
    (is (equal '(3 7)
               (multiple-value-list (gtk:label-selection-bounds label))))))

;;;     gtk_label_get_current_uri

;; TODO: How to select the link?

(test gtk-label-current-uri
  (let ((label (make-instance 'gtk:label
                              :use-markup t
                              :selectable t
                              :label
                              (format nil "Go to <a href=\"http://gtk.org/\">~
                                           GTK Website</a> ..."))))
    (is-false (gtk:label-select-region label 8 17))
    (is-false (gtk:label-current-uri label))))

;;; 2024-10-28
