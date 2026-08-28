(in-package :gtk-test)

(def-suite gtk-accessible-text :in gtk-accessibility)
(in-suite gtk-accessible-text)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkAccessibleTextContentChange

(test gtk-accessible-text-content-change
  ;; Check type
  (is (g:type-is-enum "GtkAccessibleTextContentChange"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkAccessibleTextContentChange")
          (g:gtype (cffi:foreign-funcall "gtk_accessible_text_content_change_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gtk:accessible-text-content-change
          (glib:symbol-for-gtype "GtkAccessibleTextContentChange")))
  ;; Check names
  (is (equal '("GTK_ACCESSIBLE_TEXT_CONTENT_CHANGE_INSERT"
               "GTK_ACCESSIBLE_TEXT_CONTENT_CHANGE_REMOVE")
             (glib-test:list-enum-item-names "GtkAccessibleTextContentChange")))
  ;; Check values
  (is (equal '(0 1)
             (glib-test:list-enum-item-values "GtkAccessibleTextContentChange")))
  ;; Check nick names
  (is (equal '("insert" "remove")
             (glib-test:list-enum-item-nicks "GtkAccessibleTextContentChange")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkAccessibleTextContentChange"
                                    GTK:ACCESSIBLE-TEXT-CONTENT-CHANGE
                                    (:EXPORT T
                                     :TYPE-INITIALIZER
                                     "gtk_accessible_text_content_change_get_type")
                                    (:INSERT 0)
                                    (:REMOVE 1))
             (gobject:get-gtype-definition "GtkAccessibleTextContentChange"))))

;;;     GtkAccessibleTextGranularity

(test gtk-accessible-text-granularity
  ;; Check type
  (is (g:type-is-enum "GtkAccessibleTextGranularity"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkAccessibleTextGranularity")
          (g:gtype (cffi:foreign-funcall "gtk_accessible_text_granularity_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gtk:accessible-text-granularity
          (glib:symbol-for-gtype "GtkAccessibleTextGranularity")))
  ;; Check names
  (is (equal '("GTK_ACCESSIBLE_TEXT_GRANULARITY_CHARACTER"
               "GTK_ACCESSIBLE_TEXT_GRANULARITY_WORD"
               "GTK_ACCESSIBLE_TEXT_GRANULARITY_SENTENCE"
               "GTK_ACCESSIBLE_TEXT_GRANULARITY_LINE"
               "GTK_ACCESSIBLE_TEXT_GRANULARITY_PARAGRAPH")
             (glib-test:list-enum-item-names "GtkAccessibleTextGranularity")))
  ;; Check values
  (is (equal '(0 1 2 3 4)
             (glib-test:list-enum-item-values "GtkAccessibleTextGranularity")))
  ;; Check nick names
  (is (equal '("character" "word" "sentence" "line" "paragraph")
             (glib-test:list-enum-item-nicks "GtkAccessibleTextGranularity")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkAccessibleTextGranularity"
                                    GTK:ACCESSIBLE-TEXT-GRANULARITY
                                    (:EXPORT T
                                     :TYPE-INITIALIZER
                                     "gtk_accessible_text_granularity_get_type")
                                    (:CHARACTER 0)
                                    (:WORD 1)
                                    (:SENTENCE 2)
                                    (:LINE 3)
                                    (:PARAGRAPH 4))
             (gobject:get-gtype-definition "GtkAccessibleTextGranularity"))))

;;;     GtkAccessibleText

(test gtk-accessible-text-interface
  ;; Check type
  (is (g:type-is-interface "GtkAccessibleText"))
  ;; Check registered name
  (is (eq 'gtk:accessible-text
          (glib:symbol-for-gtype "GtkAccessibleText")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkAccessibleText")
          (g:gtype (cffi:foreign-funcall "gtk_accessible_text_get_type" :size))))
  ;; Check interface prerequisites
  (is (equal '("GtkAccessible" "GObject")
             (glib-test:list-interface-prerequisites "GtkAccessibleText")))
  ;; Check interface properties
  (is (equal '()
             (glib-test:list-interface-properties "GtkAccessibleText")))
  ;; Check interface signals
  (is (equal '()
             (glib-test:list-signals "GtkAccessibleText")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "GtkAccessibleText" GTK:ACCESSIBLE-TEXT
                      (:EXPORT T
                       :TYPE-INITIALIZER "gtk_accessible_text_get_type"))
             (gobject:get-gtype-definition "GtkAccessibleText"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_accessible_text_get_attributes

(test gtk-accessible-text-attributes
  (glib-test:with-check-memory (accessible)
    (setf accessible (gtk:label-new "label"))
    (is-false (gtk:accessible-text-attributes accessible 0 0 nil nil nil))))

;;;     gtk_accessible_text_get_caret_position
;;;     gtk_accessible_text_set_caret_position

(test gtk-accessible-text-caret-position
  (glib-test:with-check-memory (accessible)
    (setf accessible (gtk:label-new "label"))
    (is-false (setf (gtk:accessible-text-caret-position accessible) 0))
    (is (= 0 (gtk:accessible-text-caret-position accessible)))))

;;;     gtk_accessible_text_get_contents

;; Note: This virtual method is implemented for GtkLabel as an example.

(test gtk-accessible-text-contents
  (glib-test:with-check-memory (accessible)
    (setf accessible (gtk:label-new "label"))
    (let ((bytes (gtk:accessible-text-contents accessible 1 4)))
      (is (string= "abe"
                   (cffi:foreign-string-to-lisp (g:bytes-data bytes)))))))

;;;     gtk_accessible_text_get_contents_at

(test gtk-accessible-text-contents-at
  (glib-test:with-check-memory (accessible)
    (setf accessible (gtk:label-new "label"))
    (is-false (gtk:accessible-text-contents-at accessible 0 :word))))

;;;     gtk_accessible_text_get_default_attributes

(test gtk-accessible-text-default-attributes
  (glib-test:with-check-memory (accessible)
    (setf accessible (gtk:label-new "label"))
    (is-false (gtk:accessible-text-default-attributes accessible nil nil))))

;;;     gtk_accessible_text_get_extents                     Since 4.16

(test gtk-accessible-text-extents
  (glib-test:with-check-memory (accessible)
    (setf accessible (gtk:label-new "label"))
    (graphene:with-rect (rect)
      (is-false (gtk:accessible-text-extents accessible 0 5 rect)))))

;;;     gtk_accessible_text_get_offset                      Since 4.16

(test gtk-accessible-text-offset
  (glib-test:with-check-memory (accessible)
    (setf accessible (gtk:label-new "label"))
    (graphene:with-point (point)
      (is-false (gtk:accessible-text-offset accessible point)))))

;;;     gtk_accessible_text_get_selection
;;;     gtk_accessible_text_set_selection

(test gtk-accessible-text-selection
  (glib-test:with-check-memory (accessible)
    (setf accessible (gtk:label-new "label"))
    (is-false (setf (gtk:accessible-text-selection accessible 0) nil))
    (is-false (gtk:accessible-text-selection accessible nil nil))))

;;;     gtk_accessible_text_update_caret_position

(test gtk-accessible-text-update-caret-position
  (glib-test:with-check-memory (accessible)
    (setf accessible (gtk:label-new "label"))
    (is-false (gtk:accessible-text-update-caret-position accessible))))

;;;     gtk_accessible_text_update_contents

(test gtk-accessible-text-update-contents
  (glib-test:with-check-memory (accessible)
    (setf accessible (gtk:label-new "label"))
    (is-false (gtk:accessible-text-update-contents accessible
                                                   :insert 2 4))))

;;;     gtk_accessible_text_update_selection_bound

(test gtk-accessible-text-update-selection-bound
  (glib-test:with-check-memory (accessible)
    (setf accessible (gtk:label-new "label"))
    (is-false (gtk:accessible-text-update-selection-bound accessible))))

;;; 2026-08-27
