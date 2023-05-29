(in-package :gtk-test)

(def-suite gtk-buildable :in gtk-suite)
(in-suite gtk-buildable)

(defvar *dialog*
"<interface>
  <object class='GtkDialog' id='dialog1'>
    <child internal-child='content_area'>
      <object class='GtkBox' id='vbox1'>
        <child internal-child='action_area'>
          <object class='GtkBox' id='hbuttonbox1'>
            <child>
              <object class='GtkButton' id='ok_button'>
                <property name='label' translatable='yes'>_Ok</property>
                <property name='use-underline'>True</property>
              </object>
            </child>
          </object>
        </child>
      </object>
    </child>
  </object>
</interface>")

;;; --- Types and Values -------------------------------------------------------

;;;     GtkBuildable

(test buildable-interface
  ;; Type check
  (is (g:type-is-interface "GtkBuildable"))
  ;; Check the registered name
  (is (eq 'gtk:buildable
          (glib:symbol-for-gtype "GtkBuildable")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkBuildable")
          (g:gtype (cffi:foreign-funcall "gtk_buildable_get_type" :size))))
  ;; Get the names of the interface properties.
  (is (equal '()
             (list-interface-properties "GtkBuildable")))
  ;; Get the interface definition
  (is (equal '(DEFINE-G-INTERFACE "GtkBuildable" GTK-BUILDABLE
                    (:EXPORT T :TYPE-INITIALIZER "gtk_buildable_get_type"))
             (gobject:get-g-type-definition "GtkBuildable"))))

;;;     GtkBuildableParser                                 not implemented

;;; --- Functions --------------------------------------------------------------

;;;     gtk_buildable_get_buildable_id

(test buildable-buildable-id
  (let* ((builder (gtk:builder-new-from-string *dialog*))
         (dialog (gtk:builder-object builder "dialog1"))
         (button (gtk:builder-object builder "ok_button")))
    (is (string= "dialog1" (gtk:buildable-buildable-id dialog)))
    (is (string= "ok_button" (gtk:buildable-buildable-id button)))))

;;;     gtk_buildable_parse_context_get_element            not implemented
;;;     gtk_buildable_parse_context_get_element_stack      not implemented
;;;     gtk_buildable_parse_context_get_position           not implemented
;;;     gtk_buildable_parse_context_pop                    not implemented
;;;     gtk_buildable_parse_context_push                   not implemented

;;; --- 2023-5-29 --------------------------------------------------------------
