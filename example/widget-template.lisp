;;;; Widget from template
;;;;
;;;; Last version: 2024-9-14

(in-package :gtk4-example)

(defvar template
"<?xml version='1.0' encoding='UTF-8'?>
<interface>
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
</interface>
")

(gobject:define-gobject-subclass "MyAppWindow" my-app-window
  (:superclass gtk:application-window
   :export t
   :interfaces nil)
  ((stack
    my-app-window-stack
    "stack" "GtkWidget" t t)
   (gears
    my-app-window-gears
    "gears" "GtkWidget" t t)
   (search
    my-app-window-search
    "search" "GtkWidget" t t)
   (searchbar
    my-app-window-searchbar
    "searchbar" "GtkWidget" t t)
   (sidebar
    my-app-window-sidebar
    "sidebar" "GtkWidget" t t)
   (words
    my-app-window-words
    "words" "GtkWidget" t t)
   (lines
    my-app-window-lines
    "lines" "GtkWidget" t t)
   (lines-label
    my-app-window-lines-label
    "lines-label" "GtkWidget" t t)
   (:cl
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
    (gtk:widget-class-set-template-from-resource "MyAppWindow"
                                                 "/template/windows.ui")

;; This function does not work. What is wrong?
;;  Gtk-WARNING : Failed to precompile template for class MyAppWindow:
;; Fehler in Zeile 17, Zeichen 2: Dokument muss mit einem Element beginnen
;; (e.g. <book>)
;   (gtk:widget-class-set-template "MyAppWindow" template)

    (gtk:widget-class-bind-template-child "MyAppWindow" "header")
    (gtk:widget-class-bind-template-child "MyAppWindow" "content_box")
    (gtk:widget-class-bind-template-child "MyAppWindow" "stack")
    (gtk:widget-class-bind-template-child "MyAppWindow" "gears")

    )

  ;; Initialize templates for the application window
  (format t "Initialize template~%")
  (gtk:widget-init-template obj)

  (setf (my-app-window-gears obj)
        (gtk:widget-template-child obj "MyAppWindow" "gears"))


  (let* ((builder (gtk:builder-new-from-resource "/template/gears-menu.ui"))
         (menu (gtk:builder-object builder "menu")))

    (setf (gtk:menu-button-menu-model (my-app-window-gears obj)) menu)

  ))

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
         (box (gtk:widget-template-child window "MyAppWindow" "content_box"))
         (header (gtk:widget-template-child window "MyAppWindow" "header"))
         (stack (gtk:widget-template-child window "MyAppWindow" "stack"))
         (gears (gtk:widget-template-child window "MyAppWindow" "gears"))
        )

    (format t "    box : ~a~%" box)
    (format t " header : ~a~%" header)
    (format t "  stack : ~a~%" stack)


    ;; Read the menus from a string
    (gtk:builder-add-from-string builder menus)

    ;; Set the menubar
    (setf (gtk:application-menubar application)
          (gtk:builder-object builder "menubar"))
    ;; Present the application window
    (gtk:window-present window)))
