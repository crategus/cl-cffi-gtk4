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

;;;     GtkBuilder

(test gtk-builder-class
  ;; Check type
  (is (g:type-is-object "GtkBuilder"))
  ;; Check registered name
  (is (eq 'gtk:builder
          (glib:symbol-for-gtype "GtkBuilder")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkBuilder")
          (g:gtype (cffi:foreign-funcall "gtk_builder_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkBuilder")))
  ;; Check children
  (is (equal '()
             (gtk-test:list-children "GtkBuilder")))
  ;; Check interfaces
  (is (equal '()
             (gtk-test:list-interfaces "GtkBuilder")))
  ;; Check class properties
  (is (equal '("current-object" "scope" "translation-domain")
             (gtk-test:list-properties "GtkBuilder")))
  ;; Check signals
  (is (equal '()
             (gtk-test:list-signals "GtkBuilder")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkBuilder" GTK-BUILDER
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_builder_get_type")
                       ((CURRENT-OBJECT GTK-BUILDER-CURRENT-OBJECT
                         "current-object" "GObject" T T)
                        (SCOPE GTK-BUILDER-SCOPE "scope" "GtkBuilderScope" T T)
                        (TRANSLATION-DOMAIN GTK-BUILDER-TRANSLATION-DOMAIN
                         "translation-domain" "gchararray" T T)))
             (gobject:get-g-type-definition "GtkBuilder"))))

;;; ---  Properties ------------------------------------------------------------

(test gtk-builder-properties.1
  (let ((builder (make-instance 'gtk:builder)))
    (is-false (gtk:builder-current-object builder))
    (is (typep (gtk:builder-scope builder) 'g:object))
    (is (typep (setf (gtk:builder-scope builder)
                     (gtk:builder-scope builder)) 'g:object))
    (is-false (gtk:builder-translation-domain builder))
    (is (string= "domain"
                 (setf (gtk:builder-translation-domain builder) "domain")))))

(test gtk-builder-properties.2
  (let ((builder (make-instance 'gtk:builder :from-string *stack-ui*)))
    (is-false (gtk:builder-current-object builder))
    (is (typep (gtk:builder-scope builder) 'g:object))
    (is-false (gtk:builder-translation-domain builder))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_builder_new

(test gtk-builder-new
  ;; gtk:builder-new is implemented with make-instance
  (is (typep (gtk:builder-new) 'gtk:builder))
  ;; Check Lisp extension for initializing builder
  (let ((builder (make-instance 'gtk:builder :from-string *stack-ui*)))
    (is (typep (gtk:builder-object builder "stack") 'gtk:stack)))
  (let* ((path (glib-sys:sys-path "test/resource/application.ui"))
         (builder (make-instance 'gtk:builder
                                :from-file path)))
    (is (typep (gtk:builder-object builder "menubar") 'g:menu))))

;;;     gtk_builder_new_from_file

(test gtk-builder-new-from-file
  (let ((path (glib-sys:sys-path "test/resource/application.ui")))
    (is (typep (gtk:builder-new-from-file path) 'gtk:builder))))

;;;     gtk_builder_new_from_resource

(test gtk-builder-new-from-resource
  (g:with-resource (resource (glib-sys:sys-path "test/rtest-gtk4.gresource"))
    (is (typep (gtk:builder-new-from-resource "/com/crategus/test/stack.ui")
               'gtk:builder))))

;;;     gtk_builder_new_from_string

(test gtk-builder-new-from-string
  (is (typep (gtk:builder-new-from-string *menus*) 'gtk:builder)))

;;;     gtk_builder_add_from_file

(test gtk-builder-add-from-file
  (let ((path (glib-sys:sys-path "test/resource/application.ui"))
        (builder (gtk:builder-new)))
    (is-true (gtk:builder-add-from-file builder path))))

;;;     gtk_builder_add_from_resource

(test gtk-builder-add-from-resource
  (g:with-resource (resource (glib-sys:sys-path "test/rtest-gtk4.gresource"))
    (let ((builder (gtk:builder-new)))
      (is-true (gtk:builder-add-from-resource builder
                                              "/com/crategus/test/stack.ui")))))

;;;     gtk_builder_add_from_string

(test gtk-builder-add-from-string
  (let ((builder (gtk:builder-new)))
    (is-true (gtk:builder-add-from-string builder *menus*))))

;;;     gtk_builder_create_closure

;;;     gtk_builder_add_objects_from_file

(test gtk-builder-add-objects-from-file.1
  (let ((path (glib-sys:sys-path "test/resource/stack.ui"))
        (builder (gtk:builder-new)))
    (is-true (gtk:builder-add-objects-from-file builder path "window1"))
    (is (typep (gtk:builder-object builder "window1") 'gtk:window))
    (is (equal '(GTK:STACK-PAGE GTK:GRID GTK:CHECK-BUTTON GTK:STACK-SWITCHER
                 GTK:STACK-PAGE GTK:STACK-PAGE GTK:WINDOW GTK:IMAGE GTK:STACK
                 GTK:SPINNER)
               (mapcar 'type-of (gtk:builder-objects builder))))))

(test gtk-builder-add-objects-from-file.2
  (let ((path (glib-sys:sys-path "test/resource/stack.ui"))
        (builder (gtk:builder-new)))
    (is-true (gtk:builder-add-objects-from-file builder path "window1" "stack"))
    (is (typep (gtk:builder-object builder "window1") 'gtk:window))
    (is (equal '(GTK:STACK-PAGE GTK:GRID GTK:CHECK-BUTTON GTK:STACK-SWITCHER
                 GTK:STACK-PAGE GTK:STACK-PAGE GTK:WINDOW GTK:IMAGE GTK:STACK
                 GTK:SPINNER)
               (mapcar 'type-of (gtk:builder-objects builder))))))

;;;     gtk_builder_add_objects_from_resource

(test gtk-builder-add-objects-from-resource
  (g:with-resource (resource (glib-sys:sys-path "test/rtest-gtk4.gresource"))
    (let ((builder (gtk:builder-new)))
      (is-true (gtk:builder-add-objects-from-resource
                       builder
                       "/com/crategus/test/stack.ui"
                       "window1"))
      (is (typep (gtk:builder-object builder "window1") 'gtk:window))
      (is (equal '(GTK:STACK-PAGE GTK:GRID GTK:CHECK-BUTTON GTK:STACK-SWITCHER
                   GTK:STACK-PAGE GTK:STACK-PAGE GTK:WINDOW GTK:IMAGE GTK:STACK
                   GTK:SPINNER)
                 (mapcar 'type-of (gtk:builder-objects builder)))))))

;;;     gtk_builder_add_objects_from_string

(test gtk-builder-add-objects-from-string.1
  (let ((builder (gtk:builder-new)))
    (is-true (gtk:builder-add-objects-from-string builder *stack-ui* "stack"))
    (is (typep (gtk:builder-object builder "stack") 'gtk:stack))
    (is (equal '(GTK:IMAGE GTK:CHECK-BUTTON GTK:STACK-PAGE GTK:STACK
                 GTK:SPINNER GTK:STACK-PAGE GTK:STACK-PAGE)
               (mapcar 'type-of (gtk:builder-objects builder))))))

(test gtk-builder-add-objects-from-string.2
  (let ((builder (gtk:builder-new)))
    (is-true (gtk:builder-add-objects-from-string builder
                                                  *stack-ui*
                                                  "stack" "image"))
    (is (equal '(GTK:IMAGE GTK:CHECK-BUTTON GTK:STACK-PAGE GTK:STACK
                 GTK:SPINNER GTK:STACK-PAGE GTK:STACK-PAGE)
               (mapcar 'type-of (gtk:builder-objects builder))))))

;;;     gtk_builder_extend_with_template

;;;     gtk_builder_get_object

(test gtk-builder-object
  (let* ((builder (gtk:builder-new-from-string *stack-ui*))
         (window (gtk:builder-object builder "window1"))
         (stack (gtk:builder-object builder "stack")))
    (is (typep window 'gtk:window))
    (is (string= "window1" (gtk:buildable-buildable-id window)))
    (is (typep stack 'gtk:stack))
    (is (string= "stack" (gtk:buildable-buildable-id stack)))))

;;;     gtk_builder_get_objects

(test gtk-builder-objects
  (let ((builder (gtk:builder-new)))
    (is (typep builder 'gtk:builder))
    (is (equal '() (gtk:builder-objects builder)))
    (is-true (gtk:builder-add-from-string builder *menus*))
    (is (equal '(gio:menu gio:menu)
               (mapcar 'type-of (gtk:builder-objects builder))))
    (is-true (gtk:builder-add-from-string builder *stack-ui*))
    (is (equal '(GIO:MENU GTK:GRID GTK:CHECK-BUTTON GTK:STACK-PAGE
                 GTK:STACK-SWITCHER GIO:MENU GTK:STACK-PAGE GTK:IMAGE
                 GTK:WINDOW GTK:STACK GTK:STACK-PAGE GTK:SPINNER)
               (mapcar #'type-of (gtk:builder-objects builder))))))

;;;     gtk_builder_expose_object

(test gtk-builder-expose-object
  (let ((builder (gtk:builder-new-from-string *stack-ui*))
        (image (make-instance 'gtk:image)))
    (is-false (gtk:builder-expose-object builder "image1" image))
    (is (eq image (gtk:builder-object builder "image1")))))

;;;     gtk_builder_get_type_from_name
;;;     gtk_builder_value_from_string
;;;     gtk_builder_value_from_string_type

;;; 2024-5-13
