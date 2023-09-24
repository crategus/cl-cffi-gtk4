(in-package :gtk-test)

(def-suite gtk-editable :in gtk-suite)
(in-suite gtk-editable)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkEditableProperties

;;;     GtkEditable

(test gtk-editable-interface
  ;; Type check
  (is (g:type-is-interface "GtkEditable"))
  ;; Check the registered name
  (is (eq 'gtk:editable
          (glib:symbol-for-gtype "GtkEditable")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkEditable")
          (g:gtype (cffi:foreign-funcall "gtk_editable_get_type" :size))))
  ;; Get the names of the interface properties.
  (is (equal '("cursor-position" "editable" "enable-undo" "max-width-chars"
               "selection-bound" "text" "width-chars" "xalign")
             (list-interface-properties "GtkEditable")))
  ;; Check the signals
  (is (equal '("changed" "delete-text" "insert-text")
             (list-signals "GtkEditable")))
  ;; Get the interface definition
  (is (equal '(GOBJECT:DEFINE-G-INTERFACE "GtkEditable"
                                  GTK-EDITABLE
                                  (:EXPORT T
                                   :TYPE-INITIALIZER "gtk_editable_get_type")
                  (CURSOR-POSITION GTK-EDITABLE-CURSOR-POSITION
                   "cursor-position" "gint" T NIL)
                  (EDITABLE GTK-EDITABLE-EDITABLE
                   "editable" "gboolean" T T)
                  (ENABLE-UNDO GTK-EDITABLE-ENABLE-UNDO
                   "enable-undo" "gboolean" T T)
                  (MAX-WIDTH-CHARS GTK-EDITABLE-MAX-WIDTH-CHARS
                   "max-width-chars" "gint" T T)
                  (SELECTION-BOUND GTK-EDITABLE-SELECTION-BOUND
                   "selection-bound" "gint" T NIL)
                  (TEXT GTK-EDITABLE-TEXT
                   "text" "gchararray" T T)
                  (WIDTH-CHARS GTK-EDITABLE-WIDTH-CHARS
                   "width-chars" "gint" T T)
                  (XALIGN GTK-EDITABLE-XALIGN
                   "xalign" "gfloat" T T))
             (gobject:get-g-type-definition "GtkEditable"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-editable-properties
  (let ((editable (make-instance 'gtk:text)))
    (is (= 0 (gtk:editable-cursor-position editable)))
    (signals (error) (setf (gtk:editable-cursor-position editable) 1))
    (is-true (gtk:editable-editable editable))
    (is-false (setf (gtk:editable-editable editable) nil))
    (is-true (gtk:editable-enable-undo editable))
    (is-false (setf (gtk:editable-enable-undo editable) nil))
    (is (= -1 (gtk:editable-max-width-chars editable)))
    (is (= 10 (setf (gtk:editable-max-width-chars editable) 10)))
    (is (= 0 (gtk:editable-selection-bound editable)))
    (signals (error) (setf (gtk:editable-selection-bound editable) 10))
    (is (string= "" (gtk:editable-text editable)))
    (is (string= "text" (setf (gtk:editable-text editable) "text")))
    (is (= -1 (gtk:editable-width-chars editable)))
    (is (= 10 (setf (gtk:editable-width-chars editable) 10)))
    (is (= 0.0 (gtk:editable-xalign editable)))
    (is (= 0.5 (setf (gtk:editable-xalign editable) 1/2)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_editable_get_chars

(test gtk-editable-chars
  (let ((editable (make-instance 'gtk:text
                                 :text "This is some text.")))
    (is (string= "This" (gtk:editable-chars editable :start 0 :end  4)))
    (is (string= "some" (gtk:editable-chars editable :start  8 :end 12)))
    (is (string= "text" (gtk:editable-chars editable :start 13 :end 17 )))
    (is (string= "This is some text." (gtk:editable-chars editable)))
    (is (string= "some text." (gtk:editable-chars editable :start 8)))
    (is (string= "This is some" (gtk:editable-chars editable :end 12)))))

;;;     gtk_editable_insert_text

(test gtk-editable-insert-text
  (let ((editable (make-instance 'gtk:text
                                 :text "This is text.")))
    (is (string= "This is text." (gtk:editable-chars editable)))
    (is (= 13 (gtk:editable-insert-text editable "some " 8)))
    (is (string= "This is some text." (gtk:editable-chars editable)))))

;;;     gtk_editable_delete_text

(test gtk-editable-delete-text
  (let ((editable (make-instance 'gtk:text
                                 :text "This is some text.")))
    (is (string= "This is some text." (gtk:editable-chars editable)))
    (is-false (gtk:editable-delete-text editable :start 8 :end 13))
    (is (string= "This is text." (gtk:editable-chars editable)))
    (is-false (gtk:editable-delete-text editable :start 7))
    (is (string= "This is" (gtk:editable-chars editable)))
    (is-false (gtk:editable-delete-text editable :end 5))
    (is (string= "is" (gtk:editable-chars editable)))))

;;;     gtk_editable_get_selection_bounds
;;;     gtk_editable_select_region
;;;     gtk_editable_delete_selection

(test gtk-editable-selection-bound
  (let ((editable (make-instance 'gtk:text
                                 :text "This is some text.")))
    (is (equal '(nil 0 0)
               (multiple-value-list (gtk:editable-selection-bounds editable))))
    (is-false (gtk:editable-select-region editable :start 8 :end 13))
    (is (equal '(t 8 13)
               (multiple-value-list (gtk:editable-selection-bounds editable))))
    (is-false (gtk:editable-delete-selection editable))
    (is (string= "This is text." (gtk:editable-chars editable)))
    (is-false (gtk:editable-select-region editable :start 7))
    (is (equal '(t 7 13)
               (multiple-value-list (gtk:editable-selection-bounds editable))))
    (is-false (gtk:editable-delete-selection editable))
    (is (string= "This is" (gtk:editable-chars editable)))

    (is-false (gtk:editable-select-region editable :end 5))
    (is (equal '(t 0 5)
               (multiple-value-list (gtk:editable-selection-bounds editable))))
    (is-false (gtk:editable-delete-selection editable))
    (is (string= "is" (gtk:editable-chars editable)))))

;;;     gtk_editable_set_position
;;;     gtk_editable_get_position

(test gtk-editable-position
  (let ((editable (make-instance 'gtk:text
                                 :text "This is some text.")))
    (is (= 0 (gtk:editable-position editable)))
    (is (= 8 (setf (gtk:editable-position editable) 8)))
    (is (= 8 (gtk:editable-position editable)))
    (is (= 8 (gtk:editable-selection-bound editable)))))

;;;     gtk_editable_set_alignment
;;;     gtk_editable_get_alignment

(test gtk-editable-alignment
  (let ((editable (make-instance 'gtk:text
                                 :text "This is some text.")))
    (is (= 0.0 (gtk:editable-alignment editable)))
    (is (= 0.5 (setf (gtk:editable-alignment editable) 0.5)))
    (is (= 0.5 (gtk:editable-alignment editable)))))

;;;     gtk_editable_install_properties
;;;     gtk_editable_get_delegate
;;;     gtk_editable_init_delegate
;;;     gtk_editable_finish_delegate
;;;     gtk_editable_delegate_set_property
;;;     gtk_editable_delegate_get_property

;;; --- 2023-7-16 --------------------------------------------------------------
