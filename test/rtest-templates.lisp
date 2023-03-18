;;; --- Template for a Object Class --------------------------------------------

(test g-object-class
  ;; Type check
  (is (g:type-is-object "GObject"))
  ;; Check the registered name
  (is (eq 'g:object
          (gobject:symbol-for-gtype "GObject")))
  ;; Check the type initializer
  (is (eq (g:gtype "GObject")
          (g:gtype (cffi:foreign-funcall "g_object_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GObject")))
  ;; Check the children
  (is (equal '()
             (list-children "GObject")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GObject")))
  ;; Check the properties
  (is (equal '()
             (list-properties "GObject")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GObject")))
  ;; Check the class definition
  (is (equal '()
             (gobject:get-g-type-definition "GObject"))))

;;; --- Template for Signals ---------------------------------------------------

(test object-signals
  (let ((query (g:signal-query (g:signal-lookup "notify" "GObject"))))
    (is (string= "notify" (g:signal-query-signal-name query)))
    (is (string= "GObject" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-FIRST :NO-RECURSE :DETAILED :ACTION :NO-HOOKS)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '("GParam")
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;; --- Template for a GtkWidget Class -----------------------------------------

(test widget-class
  ;; Type check
  (is (g:type-is-object "GtkWidget"))
  ;; Check the registered name
  (is (eq 'gtk:widget
          (gobject:symbol-for-gtype "GtkWidget")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkWidget")
          (g:gtype (cffi:foreign-funcall "gtk_widget_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkWidget")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkWidget")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkWidget")))
  ;; Check the properties
  (is (equal '()
             (list-properties "GtkWidget")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkWidget")))
  ;; CSS information
  (is (string= ""
               (gtk:widget-class-css-name "GtkWidget")))
  (is (string= ""
               (gtk:style-context-to-string
                   (gtk:widget-style-context (make-instance 'gtk:widget))
                   :none)))
  ;; Check the class definition
  (is (equal '()
             (gobject:get-g-type-definition "GtkWidget"))))

;;; --- Template for an Interface ----------------------------------------------

(test style-provider-interface
  ;; Type check
  (is (g:type-is-interface "GtkStyleProvider"))
  ;; Check the registered name
  (is (eq 'gtk:style-provider
          (gobject:symbol-for-gtype "GtkStyleProvider")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkStyleProvider")
          (g:gtype (cffi:foreign-funcall "gtk_style_provider_get_type" :size))))
  ;; Get the names of the interface properties.
  (is (equal '()
             (list-interface-properties "GtkStyleProvider")))
  ;; Get the interface definition
  (is (equal '()
             (gobject:get-g-type-definition "GtkStyleProvider"))))

;;; --- Template for flags -----------------------------------------------------

(test gtk-application-inhibit-flags
  ;; Check the type
  (is (g:type-is-flags "GtkApplicationInhibitFlags"))
  ;; Check the registered name
  (is (eq 'gtk:application-inhibit-flags
          (gobject:symbol-for-gtype "GtkApplicationInhibitFlags")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkTextAttributes")
          (g:gtype (cffi:foreign-funcall "gtk_text_attributes_get_type" :size))))
  ;; Check the names
  (is (equal '()
             (list-flags-item-name "GtkApplicationInhibitFlags")))
  ;; Check the values
  (is (equal '()
             (list-flags-item-value "GtkApplicationInhibitFlags")))
  ;; Check the nick names
  (is (equal '()
             (list-flags-item-nick "GtkApplicationInhibitFlags")))
  ;; Check the flags definition
  (is (equal '()
             (gobject:get-g-type-definition "GtkApplicationInhibitFlags"))))

;;; --- Template for an enumeration --------------------------------------------

(test gdk-window-type
  ;; Check the type
  (is (g:type-is-enum "GtkWindowType"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkTextAttributes")
          (g:gtype (cffi:foreign-funcall "gtk_text_attributes_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:window-type
          (gobject:symbol-for-gtype "GtkWindowType")))
  ;; Check the names
  (is (equal '()
             (list-enum-name "GdkWindowType")))
  ;; Check the values
  (is (equal '()
             (list-enum-value "GdkWindowType")))
  ;; Check the nick names
  (is (equal '()
             (list-enum-item-nick "GdkWindowType")))
  ;; Check the enum definition
  (is (equal '()
             (gobject:get-g-type-definition "GdkWindowType"))))

;;; --- Template for a g-boxed-cstruct -----------------------------------------

(test gtk-text-attributes
  ;; Type check
  (is (g:type-is-a (gtype "GtkTextAttributes") +g-type-boxed+))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkTextAttributes")
          (g:gtype (cffi:foreign-funcall "gtk_text_attributes_get_type" :size)))))

;;; --- Template for g-signal-emit ---------------------------------------------

(test gtk-widget-show-help-signal
  (let* ((message nil)
         (button (make-instance 'gtk:button))
         (handler-id (g-signal-connect button "show-help"
                       (lambda (widget help-type)
                         (setf message "Signal show-help")
                         (is (typep widget 'gtk-widget))
                         (is (eq :whats-this help-type))
                         t))))
    ;; Emit the signal
    (is-true (g-signal-emit button "show-help" :whats-this))
    (is (string= "Signal show-help" message))
    (is-false (g-signal-handler-disconnect display handler-id))))

#+nil
(test gtk-action-activate
  (let ((message nil))
    (within-main-loop
      (let ((action (gtk-action-new "action")))
        (g-signal-connect action "activate"
           (lambda (action)
             (setf message "ACTIVATE CALLED")
             (when *verbose-gtk-action*
               (format t "~&Signal ACTIVATE for ~A~%" (gtk-action-name action)))
             (leave-gtk-main)))
        (gtk-action-activate action)))
    (join-gtk-main)
    (is (equal "ACTIVATE CALLED" message))))

;;; 2021-8-14
