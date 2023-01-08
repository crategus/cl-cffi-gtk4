(in-package :gtk-test)

(def-suite gtk-builder :in gtk-suite)
(in-suite gtk-builder)

(defvar *menus*
  "<interface>
    <menu id='app-menu'>
     <section>
      <item>
       <attribute name='label' translatable='yes'>_New Window</attribute>
       <attribute name='action'>app.new</attribute>
       <attribute name='accel'>&lt;Primary&gt;n</attribute>
      </item>
     </section>
     <section>
      <item>
       <attribute name='label' translatable='yes'>_About Bloatpad</attribute>
       <attribute name='action'>app.about</attribute>
      </item>
     </section>
     <section>
      <item>
       <attribute name='label' translatable='yes'>_Quit</attribute>
       <attribute name='action'>app.quit</attribute>
       <attribute name='accel'>&lt;Primary&gt;q</attribute>
      </item>
     </section>
     </menu>
    <menu id='menubar'>
     <submenu>
      <attribute name='label' translatable='yes'>_Edit</attribute>
      <section>
       <item>
        <attribute name='label' translatable='yes'>_Copy</attribute>
        <attribute name='action'>win.copy</attribute>
        <attribute name='accel'>&lt;Primary&gt;c</attribute>
       </item>
       <item>
        <attribute name='label' translatable='yes'>_Paste</attribute>
        <attribute name='action'>win.paste</attribute>
        <attribute name='accel'>&lt;Primary&gt;v</attribute>
       </item>
      </section>
     </submenu>
     <submenu>
      <attribute name='label' translatable='yes'>_View</attribute>
      <section>
       <item>
        <attribute name='label' translatable='yes'>_Fullscreen</attribute>
        <attribute name='action'>win.fullscreen</attribute>
        <attribute name='accel'>F11</attribute>
       </item>
      </section>
     </submenu>
    </menu>
   </interface>")

;; Example from the GtkBuilder API documentation:
;; TODO: This example does not work. The definition of a signal handler
;; needs the implementation of support for reading callback functions.

#+nil
(defvar *dialog1*
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
                <signal name='clicked' handler='ok_button_clicked'/>
              </object>
            </child>
          </object>
        </child>
      </object>
    </child>
  </object>
</interface>")

;;; --- Types and Values -------------------------------------------------------

;;;     GtkBuilderError

;;;     GtkBuilder

(test builder-class
  ;; Type check
  (is (g:type-is-object "GtkBuilder"))
  ;; Check the registered name
  (is (eq 'gtk:builder
          (gobject:symbol-for-gtype "GtkBuilder")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkBuilder")
          (g:gtype (foreign-funcall "gtk_builder_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkBuilder")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkBuilder")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkBuilder")))
  ;; Check the class properties
  (is (equal '("current-object" "scope" "translation-domain")
             (list-properties "GtkBuilder")))
  ;; Check the list of signals
  (is (equal '()
             (list-signals "GtkBuilder")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkBuilder" GTK-BUILDER
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_builder_get_type")
                       ((CURRENT-OBJECT GTK-BUILDER-CURRENT-OBJECT
                         "current-object" "GObject" T T)
                        (SCOPE GTK-BUILDER-SCOPE "scope" "GtkBuilderScope" T T)
                        (TRANSLATION-DOMAIN GTK-BUILDER-TRANSLATION-DOMAIN
                         "translation-domain" "gchararray" T T)))
             (gobject:get-g-type-definition "GtkBuilder"))))

;;; ---  Properties ------------------------------------------------------------

(test builder-properties.1
  (let ((builder (make-instance 'gtk:builder)))
    (is-false (gtk:builder-current-object builder))
    (is (typep (gtk:builder-scope builder) 'g:object))
    (is (typep (setf (gtk:builder-scope builder)
                     (gtk:builder-scope builder)) 'g:object))
    (is-false (gtk:builder-translation-domain builder))
    (is (string= "domain"
                 (setf (gtk:builder-translation-domain builder) "domain")))))

(test builder-properties.2
  (let ((builder (make-instance 'gtk:builder :from-string *dialog*)))
    (is-false (gtk:builder-current-object builder))
    (is (typep (gtk:builder-scope builder) 'g:object))
    (is-false (gtk:builder-translation-domain builder))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_builder_new

(test builder-new
  ;; gtk:builder-new is implemented with make-instance
  (is (typep (gtk:builder-new) 'gtk:builder))
  ;; Check Lisp extension for initializing builder
  (let ((builder (make-instance 'gtk:builder :from-string *dialog*)))
    (is (typep (gtk:builder-object builder "dialog1") 'gtk:dialog)))
  (let ((builder (make-instance 'gtk:builder
                                :from-file
                                (sys-path "test/application.ui"))))
    (is (typep (gtk:builder-object builder "menubar") 'g:menu))))

;;;     gtk_builder_new_from_file

(test builder-new-from-file
  (is (typep (gtk:builder-new-from-file (sys-path "test/application.ui") )
             'gtk:builder)))

;;;     gtk_builder_new_from_resource

(test builder-new-from-resource
  (with-g-resource (resource (sys-path "test/rtest-resource.gresource"))
    (is (typep (gtk:builder-new-from-resource "/com/crategus/test/dialog.ui")
               'gtk:builder))))

;;;     gtk_builder_new_from_string

(test builder-new-from-string
  (is (typep (gtk:builder-new-from-string *menus*) 'gtk:builder)))

;;;     gtk_builder_add_from_file

(test builder-add-from-file
  (let ((builder (gtk:builder-new)))
    (is-true (gtk:builder-add-from-file builder (sys-path "test/application.ui")))))

;;;     gtk_builder_add_from_resource

(test builder-add-from-resource
  (with-g-resource (resource (sys-path "test/rtest-resource.gresource"))
    (let ((builder (gtk:builder-new)))
      (is-true (gtk:builder-add-from-resource builder
                                              "/com/crategus/test/dialog.ui")))))

;;;     gtk_builder_add_from_string

(test builder-add-from-string
  (let ((builder (gtk:builder-new)))
    (is-true (gtk:builder-add-from-string builder *menus*))))

;;;     gtk_builder_create_closure

;;;     gtk_builder_add_objects_from_file

(test builder-add-objects-from-file.1
  (let ((builder (gtk:builder-new)))
    (is-true (gtk:builder-add-objects-from-file builder
                                                (sys-path "test/dialog.ui")
                                                "dialog1"))
    (is (typep (gtk:builder-object builder "dialog1") 'gtk:dialog))
    (is (equal '(gtk:dialog gtk:box gtk:box gtk:button)
               (mapcar 'type-of (gtk:builder-objects builder))))))

(test builder-add-objects-from-file.2
  (let ((builder (gtk:builder-new)))
    (is-true (gtk:builder-add-objects-from-file builder
                                                (sys-path "test/dialog.ui")
                                                "dialog1"
                                                "ok_button"))
    (is (typep (gtk:builder-object builder "dialog1") 'gtk:dialog))
    (is (equal '(gtk:dialog gtk:box gtk:box gtk:button)
               (mapcar 'type-of (gtk:builder-objects builder))))))

;;;     gtk_builder_add_objects_from_resource

(test builder-add-objects-from-resource
  (with-g-resource (resource (sys-path "test/rtest-resource.gresource"))
    (let ((builder (gtk:builder-new)))
      (is-true (gtk:builder-add-objects-from-resource builder
                            "/com/crategus/test/dialog.ui"
                            "dialog1"))
      (is (typep (gtk:builder-object builder "dialog1") 'gtk:dialog))
      (is (equal '(gtk:dialog gtk:box gtk:box gtk:button)
                 (mapcar 'type-of (gtk:builder-objects builder)))))))

;;;     gtk_builder_add_objects_from_string

(test builder-add-objects-from-string.1
  (let ((builder (gtk:builder-new)))
    (is-true (gtk:builder-add-objects-from-string builder *dialog* "dialog1"))
    (is (typep (gtk:builder-object builder "dialog1") 'gtk:dialog))
    (is (equal '(gtk:dialog gtk:box gtk:box gtk:button)
               (mapcar 'type-of (gtk:builder-objects builder))))))

(test builder-add-objects-from-string.2
  (let ((builder (gtk:builder-new)))
    (is-true (gtk:builder-add-objects-from-string builder
                                                  *dialog*
                                                  "ok_button" "hbuttonbox1"))
    (is (equal '(gtk:box gtk:button)
               (mapcar 'type-of (gtk:builder-objects builder))))))

;;;     gtk_builder_extend_with_template

;;;     gtk_builder_get_object

(test builder-object
  (let* ((builder (gtk:builder-new-from-string *dialog*))
         (dialog (gtk:builder-object builder "dialog1"))
         (button (gtk:builder-object builder "ok_button")))
    (is (typep dialog 'gtk:dialog))
    (is (string= "dialog1" (gtk:buildable-buildable-id dialog)))
    (is (typep button 'gtk:button))
    (is (string= "ok_button" (gtk:buildable-buildable-id button)))))

;;;     gtk_builder_get_objects

(test builder-objects
  (let ((builder (gtk:builder-new)))
    (is (typep builder 'gtk:builder))
    (is (equal '() (gtk:builder-objects builder)))
    (is-true (gtk:builder-add-from-string builder *menus*))
    (is (equal '(gio:menu gio:menu)
               (mapcar 'type-of (gtk:builder-objects builder))))
    (is-true (gtk:builder-add-from-string builder *dialog*))
    (is (equal '(gtk:dialog gio:menu gio:menu gtk:box gtk:box gtk:button)
               (mapcar #'type-of (gtk:builder-objects builder))))))

;;;     gtk_builder_expose_object

(test builder-expose-object
  (let ((builder (gtk:builder-new-from-string *dialog*))
        (button (make-instance 'gtk:check-button)))
    (is-false (gtk:builder-expose-object builder "checkbutton" button))
    (is (eq button (gtk:builder-object builder "checkbutton")))))

;;;     gtk_builder_get_type_from_name
;;;     gtk_builder_value_from_string
;;;     gtk_builder_value_from_string_type

;;; 2022-11-5
