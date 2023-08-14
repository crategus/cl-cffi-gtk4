;;;; Example Application Window with Menubar - 2023-8-7

(in-package :gtk4-example)

(defvar *menus*
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

(defun do-application-window (&optional (application nil))
  (let ((builder (make-instance 'gtk:builder))
        (window (make-instance 'gtk:application-window
                               :application application
                               :title "Application Window"
                               :show-menubar t)))
    ;; Read the menus from a string
    (gtk:builder-add-from-string builder *menus*)
    ;; Set the menubar
    (setf (gtk:application-menubar application)
          (gtk:builder-object builder "menubar"))
    ;; Show the application window
    (setf (gtk:widget-visible window) t)))
