(in-package :gtk-test)

(def-suite gtk-inscription :in gtk-display-widgets)
(in-suite gtk-inscription)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkInscriptionOverflow

(test gtk-inscription-overflow
  ;; Check type
  (is (g:type-is-enum "GtkInscriptionOverflow"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkInscriptionOverflow")
          (g:gtype (cffi:foreign-funcall "gtk_inscription_overflow_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gtk:inscription-overflow
          (glib:symbol-for-gtype "GtkInscriptionOverflow")))
  ;; Check names
  (is (equal '("GTK_INSCRIPTION_OVERFLOW_CLIP"
               "GTK_INSCRIPTION_OVERFLOW_ELLIPSIZE_START"
               "GTK_INSCRIPTION_OVERFLOW_ELLIPSIZE_MIDDLE"
               "GTK_INSCRIPTION_OVERFLOW_ELLIPSIZE_END")
             (glib-test:list-enum-item-names "GtkInscriptionOverflow")))
  ;; Check values
  (is (equal '(0 1 2 3)
             (glib-test:list-enum-item-values "GtkInscriptionOverflow")))
  ;; Check nick names
  (is (equal '("clip" "ellipsize-start" "ellipsize-middle" "ellipsize-end")
             (glib-test:list-enum-item-nicks "GtkInscriptionOverflow")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkInscriptionOverflow"
                                    GTK:INSCRIPTION-OVERFLOW
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_inscription_overflow_get_type")
                       (:CLIP 0)
                       (:ELLIPSIZE-START 1)
                       (:ELLIPSIZE-MIDDLE 2)
                       (:ELLIPSIZE-END 3))
             (gobject:get-gtype-definition "GtkInscriptionOverflow"))))

;;;     GtkInscription

(test gtk-inscription-class
  ;; Check type
  (is (g:type-is-object "GtkInscription"))
  ;; Check registered name
  (is (eq 'gtk:inscription
          (glib:symbol-for-gtype "GtkInscription")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkInscription")
          (g:gtype (cffi:foreign-funcall "gtk_inscription_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkInscription")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkInscription")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkAccessibleText")
             (glib-test:list-interfaces "GtkInscription")))
  ;; Check properties
  (is (equal '("attributes" "markup" "min-chars" "min-lines" "nat-chars"
               "nat-lines" "text" "text-overflow" "wrap-mode" "xalign" "yalign")
             (glib-test:list-properties "GtkInscription")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkInscription")))
  ;; Check CSS name
  (is (string= "label"
               (gtk:widget-class-css-name "GtkInscription")))
  ;; Check accessible role
  (is (eq :label (gtk:widget-class-accessible-role "GtkInscription")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkInscription" GTK:INSCRIPTION
                      (:SUPERCLASS GTK:WIDGET
                       :EXPORT T
                       :INTERFACES
                       ("GtkAccessible" "GtkAccessibleText" "GtkBuildable"
                        "GtkConstraintTarget")
                       :TYPE-INITIALIZER "gtk_inscription_get_type")
                      ((ATTRIBUTES INSCRIPTION-ATTRIBUTES
                        "attributes" "PangoAttrList" T T)
                       (MARKUP INSCRIPTION-MARKUP "markup" "gchararray" NIL T)
                       (MIN-CHARS INSCRIPTION-MIN-CHARS "min-chars" "guint" T T)
                       (MIN-LINES INSCRIPTION-MIN-LINES "min-lines" "guint" T T)
                       (NAT-CHARS INSCRIPTION-NAT-CHARS "nat-chars" "guint" T T)
                       (NAT-LINES INSCRIPTION-NAT-LINES "nat-lines" "guint" T T)
                       (TEXT INSCRIPTION-TEXT "text" "gchararray" T T)
                       (TEXT-OVERFLOW INSCRIPTION-TEXT-OVERFLOW
                        "text-overflow" "GtkInscriptionOverflow" T T)
                       (WRAP-MODE INSCRIPTION-WRAP-MODE
                        "wrap-mode" "PangoWrapMode" T T)
                       (XALIGN INSCRIPTION-XALIGN "xalign" "gfloat" T T)
                       (YALIGN INSCRIPTION-YALIGN "yalign" "gfloat" T T)))
             (gobject:get-gtype-definition "GtkInscription"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-inscription-properties
  (glib-test:with-check-memory (widget)
    (is (typep (setf widget (make-instance 'gtk:inscription)) 'gtk:inscription))
    (is-false (gtk:inscription-attributes widget))
    ;; markup is not readable
    (signals (error) (gtk:inscription-markup widget))
    (is (= 3 (gtk:inscription-min-chars widget)))
    (is (= 1 (gtk:inscription-min-lines widget)))
    (is (= 0 (gtk:inscription-nat-chars widget)))
    (is (= 0 (gtk:inscription-nat-lines widget)))
    (is-false (gtk:inscription-text widget))
    (is (eq :clip (gtk:inscription-text-overflow widget)))
    (is (eq :word-char (gtk:inscription-wrap-mode widget)))
    (is (= 0.0 (gtk:inscription-xalign widget)))
    (is (= 0.5 (gtk:inscription-yalign widget)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_inscription_new

(test gtk-inscription-new
  (glib-test:with-check-memory (inscription)
    (is (typep (setf inscription (gtk:inscription-new)) 'gtk:inscription))
    (is (typep (setf inscription (gtk:inscription-new nil)) 'gtk:inscription))
    (is (typep (setf inscription (gtk:inscription-new "text")) 'gtk:inscription))))

;;; 2025-05-05
