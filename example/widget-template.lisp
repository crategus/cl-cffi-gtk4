;;;; Widget from template
;;;;
;;;; Last version: 2024-9-14

(in-package :gtk4-example)

(defvar template
"<interface>
  <template class='MyAppWindow' parent='GtkApplicationWindow'>
    <property name='title' translatable='yes'>Example Application</property>
    <property name='default-width'>600</property>
    <property name='default-height'>400</property>
    <child>
      <object class='GtkBox' id='content_box'>
        <property name='orientation'>vertical</property>
        <child>
          <object class='GtkStack' id='stack'/>
        </child>
      </object>
    </child>
  </template>
</interface>")

(gobject:define-gobject-subclass "MyAppWindow" my-app-window
  (:superclass gtk:application-window
   :export t
   :interfaces ())
  ((:cl
    template-p :initform nil
               :accessor my-app-window-template-p
               :allocation :class)))

(defmethod initialize-instance :after ((obj my-app-window) &key)

  (format t "in INITIAlIZE-INSTANCE for ~a~%" obj)

  ;; Load template from resource for application window
  (unless (my-app-window-template-p obj)
    (format t "Load resource window.ui for MyAppWindow~%")
    (setf (my-app-window-template-p obj) t)

    ;; Load template from resource for application window
    ;; FIXME: This function does not work
;    (gtk:widget-class-set-template-from-resource "MyAppWindow"
;                                                 "/template/windows.ui")

    (gtk:widget-class-set-template-from-resource "MyAppWindow"
                                                   "/template/windows.ui")
    (gtk:widget-class-bind-template-child "MyAppWindow" "content_box")
    (gtk:widget-class-bind-template-child "MyAppWindow" "stack")
    (gtk:widget-class-bind-template-child "MyAppWindow" "page")

    )

  ;; Initialize templates for the application window
  (format t "Initialize template~%")
  (gtk:widget-init-template obj)

)

(defun do-widget-template (&optional application)
  (let* ((menus
          "<interface>
            <menu id='menubar'>
              <submenu>
                <attribute name='label'>_Edit</attribute>
                <item>
                  <attribute name='label'>_Copy</attribute>
                  <attribute name='action'>win.copy</attribute>
                </item>
                <item>
                  <attribute name='label'>_Paste</attribute>
                  <attribute name='action'>win.paste</attribute>
                </item>
              </submenu>
            </menu>
          </interface>")
         (builder (make-instance 'gtk:builder))
         (window (make-instance 'my-app-window
                                :application application
                                :title "Application Window"
                                :show-menubar t))
         (stack (gtk:widget-template-child window "MyAppWindow" "stack"))
         (box (gtk:widget-template-child window "MyAppWindow" "content_box"))
         (page (gtk:widget-template-child window "MyAppWindow" "page"))
)

    (format t "  stack : ~a~%" stack)
    (format t "    box : ~a~%" box)
    (format t "   page : ~a~%" page)
    (format t "  pages : ~a~%" (gtk:stack-pages stack))
    (format t "visible : ~a~%" (gtk:stack-visible-child-name stack))

    (setf (gtk:stack-visible-child-name stack) "page1")

    ;; Read the menus from a string
    (gtk:builder-add-from-string builder menus)

    ;; Set the menubar
;    (setf (gtk:application-menubar application)
;          (gtk:builder-object builder "menubar"))
    ;; Present the application window
    (gtk:window-present window)))
