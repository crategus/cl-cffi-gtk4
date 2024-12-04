;;;; Application Window with Menubar
;;;;
;;;; The <tt>gtk:application-window</tt> class is a <tt>gtk:window</tt> subclass
;;;; that offers some extra functionality for better integration with
;;;; <tt>gtk:application</tt> features. Notably, it can handle an application
;;;; menubar.
;;;;
;;;; Last version: 2024-5-24

(in-package :gtk4-example)

(defun do-application-window (&optional application)
  (let ((menus
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
        (window (make-instance 'gtk:application-window
                               :application application
                               :title "Application Window"
                               :show-menubar t)))
    ;; Read the menus from a string
    (gtk:builder-add-from-string builder menus)
    ;; Set the menubar
    (setf (gtk:application-menubar application)
          (gtk:builder-object builder "menubar"))
    ;; Present the application window
    (gtk:window-present window)))
