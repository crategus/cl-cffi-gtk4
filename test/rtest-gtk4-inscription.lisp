(in-package :gtk-test)

(def-suite gtk-inscription :in gtk-suite)
(in-suite gtk-inscription)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkInscriptionOverflow

(test gtk-inscription-overflow
  ;; Check type
  (is (g:type-is-enum "GtkInscriptionOverflow"))
  ;; Check the type initializer
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
             (list-enum-item-name "GtkInscriptionOverflow")))
  ;; Check values
  (is (equal '(0 1 2 3)
             (list-enum-item-value "GtkInscriptionOverflow")))
  ;; Check nick names
  (is (equal '("clip" "ellipsize-start" "ellipsize-middle" "ellipsize-end")
             (list-enum-item-nick "GtkInscriptionOverflow")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkInscriptionOverflow"
                                     GTK-INSCRIPTION-OVERFLOW
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gtk_inscription_overflow_get_type")
                                     (:CLIP 0)
                                     (:ELLIPSIZE-START 1)
                                     (:ELLIPSIZE-MIDDLE 2)
                                     (:ELLIPSIZE-END 3))
             (gobject:get-g-type-definition "GtkInscriptionOverflow"))))

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
             (list-children "GtkInscription")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (list-interfaces "GtkInscription")))
  ;; Check properties
  (is (equal '("attributes" "markup" "min-chars" "min-lines" "nat-chars"
               "nat-lines" "text" "text-overflow" "wrap-mode" "xalign" "yalign")
             (list-properties "GtkInscription")))
  ;; Check signals
  (is (equal '()
             (list-signals "GtkInscription")))
  ;; Check CSS name
  (is (string= "label"
               (gtk:widget-class-css-name "GtkInscription")))
  ;; Check CSS classes
  (is (equal '()
             (gtk:widget-css-classes (make-instance 'gtk:inscription))))
  ;; Check accessible role
  (is (eq :label (gtk:widget-class-accessible-role "GtkInscription")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkInscription" GTK-INSCRIPTION
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkBuildable"
                                 "GtkConstraintTarget")
                                :TYPE-INITIALIZER "gtk_inscription_get_type")
                               ((ATTRIBUTES GTK-INSCRIPTION-ATTRIBUTES
                                 "attributes" "PangoAttrList" T T)
                                (MARKUP GTK-INSCRIPTION-MARKUP "markup"
                                 "gchararray" NIL T)
                                (MIN-CHARS GTK-INSCRIPTION-MIN-CHARS
                                 "min-chars" "guint" T T)
                                (MIN-LINES GTK-INSCRIPTION-MIN-LINES
                                 "min-lines" "guint" T T)
                                (NAT-CHARS GTK-INSCRIPTION-NAT-CHARS
                                 "nat-chars" "guint" T T)
                                (NAT-LINES GTK-INSCRIPTION-NAT-LINES
                                 "nat-lines" "guint" T T)
                                (TEXT GTK-INSCRIPTION-TEXT "text" "gchararray"
                                 T T)
                                (TEXT-OVERFLOW GTK-INSCRIPTION-TEXT-OVERFLOW
                                 "text-overflow" "GtkInscriptionOverflow" T T)
                                (WRAP-MODE GTK-INSCRIPTION-WRAP-MODE
                                 "wrap-mode" "PangoWrapMode" T T)
                                (XALIGN GTK-INSCRIPTION-XALIGN "xalign"
                                 "gfloat" T T)
                                (YALIGN GTK-INSCRIPTION-YALIGN "yalign"
                                 "gfloat" T T)))
             (gobject:get-g-type-definition "GtkInscription"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-inscription-properties
  (let ((widget (make-instance 'gtk:inscription)))
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
  (is (typep (gtk:inscription-new nil) 'gtk:inscription))
  (is (typep (gtk:inscription-new "text") 'gtk:inscription)))

;;; 2024-4-25
