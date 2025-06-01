(in-package :gtk-test)

(def-suite gtk-editable :in gtk-data-entry)
(in-suite gtk-editable)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkEditableProperties

;;;     GtkEditable

(test gtk-editable-interface
  ;; Check type
  (is (g:type-is-interface "GtkEditable"))
  ;; Check registered name
  (is (eq 'gtk:editable
          (glib:symbol-for-gtype "GtkEditable")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkEditable")
          (g:gtype (cffi:foreign-funcall "gtk_editable_get_type" :size))))
  ;; Check interface prerequisites
  (is (equal '("GtkWidget")
             (glib-test:list-interface-prerequisites "GtkEditable")))
  ;; Check interface properties
  (is (equal '("cursor-position" "editable" "enable-undo" "max-width-chars"
               "selection-bound" "text" "width-chars" "xalign")
             (glib-test:list-interface-properties "GtkEditable")))
  ;; Check signals
  (is (equal '("changed" "delete-text" "insert-text")
             (glib-test:list-signals "GtkEditable")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "GtkEditable" GTK:EDITABLE
                      (:EXPORT T
                       :TYPE-INITIALIZER "gtk_editable_get_type")
                      (CURSOR-POSITION EDITABLE-CURSOR-POSITION
                       "cursor-position" "gint" T NIL)
                      (EDITABLE EDITABLE-EDITABLE
                       "editable" "gboolean" T T)
                      (ENABLE-UNDO EDITABLE-ENABLE-UNDO
                       "enable-undo" "gboolean" T T)
                      (MAX-WIDTH-CHARS EDITABLE-MAX-WIDTH-CHARS
                       "max-width-chars" "gint" T T)
                      (SELECTION-BOUND EDITABLE-SELECTION-BOUND
                       "selection-bound" "gint" T NIL)
                      (TEXT EDITABLE-TEXT "text" "gchararray" T T)
                      (WIDTH-CHARS EDITABLE-WIDTH-CHARS
                       "width-chars" "gint" T T)
                      (XALIGN EDITABLE-XALIGN "xalign" "gfloat" T T))
             (gobject:get-gtype-definition "GtkEditable"))))

;;; --- Signals ----------------------------------------------------------------

;;;     changed

(test gtk-editable-changed-signal
  (let* ((name "changed") (gtype "GtkEditable")
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (string= gtype (g:type-name (g:signal-query-owner-type query))))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     delete-text

(test gtk-editable-delete-text-signal
  (let* ((name "delete-text") (gtype "GtkEditable")
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (string= gtype (g:type-name (g:signal-query-owner-type query))))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '("gint" "gint")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     insert-text

(test gtk-editable-insert-text-signal
  (let* ((name "insert-text") (gtype "GtkEditable")
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (string= gtype (g:type-name (g:signal-query-owner-type query))))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '("gchararray" "gint" "gpointer")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Properties -------------------------------------------------------------

(test gtk-editable-properties.1
  (glib-test:with-check-memory (editable)
    (is (typep (setf editable
                     (make-instance 'gtk:editable-label)) 'gtk:editable-label))
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

(test gtk-editable-properties.2
  (glib-test:with-check-memory (editable)
    (is (typep (setf editable (make-instance 'gtk:text)) 'gtk:text))
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
  (glib-test:with-check-memory (editable)
    (is (typep (setf editable
                     (make-instance 'gtk:text
                                    :text "This is some text.")) 'gtk:text))
    (is (string= "This" (gtk:editable-chars editable :start 0 :end  4)))
    (is (string= "some" (gtk:editable-chars editable :start  8 :end 12)))
    (is (string= "text" (gtk:editable-chars editable :start 13 :end 17 )))
    (is (string= "This is some text." (gtk:editable-chars editable)))
    (is (string= "some text." (gtk:editable-chars editable :start 8)))
    (is (string= "This is some" (gtk:editable-chars editable :end 12)))))

;;;     gtk_editable_insert_text

(test gtk-editable-insert-text
  (glib-test:with-check-memory (editable)
    (is (typep (setf editable
                     (make-instance 'gtk:text
                                    :text "This is text.")) 'gtk:text))
    (is (string= "This is text." (gtk:editable-chars editable)))
    (is (= 13 (gtk:editable-insert-text editable "some " 8)))
    (is (string= "This is some text." (gtk:editable-chars editable)))))

;;;     gtk_editable_delete_text

(test gtk-editable-delete-text
  (glib-test:with-check-memory (editable)
    (is (typep (setf editable
                     (make-instance 'gtk:text
                                    :text "This is some text.")) 'gtk:text))
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

(test gtk-editable-selection-bounds
  (glib-test:with-check-memory (editable)
    (is (typep (setf editable
                     (make-instance 'gtk:text
                                    :text "This is some text.")) 'gtk:text))
    (is-false (gtk:editable-selection-bounds editable))
    (is-false (gtk:editable-select-region editable :start 8 :end 13))
    (is (equal '(8 13)
               (multiple-value-list (gtk:editable-selection-bounds editable))))
    (is-false (gtk:editable-delete-selection editable))
    (is (string= "This is text." (gtk:editable-chars editable)))
    (is-false (gtk:editable-select-region editable :start 7))
    (is (equal '(7 13)
               (multiple-value-list (gtk:editable-selection-bounds editable))))
    (is-false (gtk:editable-delete-selection editable))
    (is (string= "This is" (gtk:editable-chars editable)))

    (is-false (gtk:editable-select-region editable :end 5))
    (is (equal '(0 5)
               (multiple-value-list (gtk:editable-selection-bounds editable))))
    (is-false (gtk:editable-delete-selection editable))
    (is (string= "is" (gtk:editable-chars editable)))))

;;;     gtk_editable_set_position
;;;     gtk_editable_get_position

(test gtk-editable-position
  (glib-test:with-check-memory (editable)
    (is (typep (setf editable
                     (make-instance 'gtk:text
                                    :text "This is some text.")) 'gtk:text))
    (is (= 0 (gtk:editable-position editable)))
    (is (= 8 (setf (gtk:editable-position editable) 8)))
    (is (= 8 (gtk:editable-position editable)))
    (is (= 8 (gtk:editable-selection-bound editable)))))

;;;     gtk_editable_set_alignment
;;;     gtk_editable_get_alignment

(test gtk-editable-alignment
  (glib-test:with-check-memory (editable)
    (is (typep (setf editable
                     (make-instance 'gtk:text
                                    :text "This is some text.")) 'gtk:text))
    (is (= 0.0 (gtk:editable-alignment editable)))
    (is (= 0.5 (setf (gtk:editable-alignment editable) 0.5)))
    (is (= 0.5 (gtk:editable-alignment editable)))))

;;;     gtk_editable_install_properties
;;;     gtk_editable_get_delegate
;;;     gtk_editable_init_delegate
;;;     gtk_editable_finish_delegate
;;;     gtk_editable_delegate_set_property
;;;     gtk_editable_delegate_get_property

;;; 2025-05-31
