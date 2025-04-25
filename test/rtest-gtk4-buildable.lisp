(in-package :gtk-test)

(def-suite gtk-buildable :in gtk-suite)
(in-suite gtk-buildable)

(defvar *stack-ui*
"<interface>
  <object class='GtkWindow' id='window1'>
    <property name='title' translatable='yes'>Stack</property>
    <property name='resizable'>0</property>
    <child>
      <object class='GtkGrid'>
        <child>
          <object class='GtkStackSwitcher'>
            <property name='stack'>stack</property>
            <property name='halign'>center</property>
            <layout>
              <property name='column'>0</property>
              <property name='row'>0</property>
            </layout>
          </object>
        </child>
        <child>
          <object class='GtkStack' id='stack'>
            <property name='transition-type'>crossfade</property>
            <child>
              <object class='GtkStackPage'>
                <property name='name'>page1</property>
                <property name='title' translatable='yes'>Page 1</property>
                <property name='child'>
                  <object class='GtkImage' id='image'>
                    <property name='margin-top'>20</property>
                    <property name='margin-bottom'>20</property>
                    <property name='pixel-size'>100</property>
                    <property name='icon-name'>org.gtk.Demo4</property>
                  </object>
                </property>
              </object>
            </child>
            <child>
              <object class='GtkStackPage'>
                <property name='name'>page2</property>
                <property name='title' translatable='yes'>Page 2</property>
                <property name='child'>
                  <object class='GtkCheckButton' id='checkbutton'>
                    <property name='label' translatable='yes'>Page 2</property>
                    <property name='halign'>center</property>
                    <property name='valign'>center</property>
                  </object>
                </property>
              </object>
            </child>
            <child>
              <object class='GtkStackPage'>
                <property name='name'>page3</property>
                <property name='icon-name'>face-laugh-symbolic</property>
                <property name='child'>
                  <object class='GtkSpinner' id='spinner'>
                    <property name='halign'>center</property>
                    <property name='valign'>center</property>
                    <property name='spinning'>1</property>
                  </object>
                </property>
              </object>
            </child>
            <layout>
              <property name='column'>0</property>
              <property name='row'>1</property>
            </layout>
          </object>
        </child>
      </object>
    </child>
  </object>
</interface>")

;;; --- Types and Values -------------------------------------------------------

;;;     GtkBuildable

(test gtk-buildable-interface
  ;; Check type
  (is (g:type-is-interface "GtkBuildable"))
  ;; Check registered name
  (is (eq 'gtk:buildable
          (glib:symbol-for-gtype "GtkBuildable")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkBuildable")
          (g:gtype (cffi:foreign-funcall "gtk_buildable_get_type" :size))))
  ;; Check interface prerequisites
  (is (equal '("GObject")
             (glib-test:list-interface-prerequisites "GtkBuildable")))
  ;; Check interface properties
  (is (equal '()
             (glib-test:list-interface-properties "GtkBuildable")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkBuildable")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "GtkBuildable" GTK:BUILDABLE
                      (:EXPORT T
                       :TYPE-INITIALIZER "gtk_buildable_get_type"))
             (gobject:get-gtype-definition "GtkBuildable"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_buildable_get_buildable_id

(test gtk-buildable-buildable-id
  (glib-test:with-check-memory (builder
                                (window1 2) (stack 4) (image 4)
                                (button 4) (spinner 4) :strong 6)
    (setf builder (gtk:builder-new-from-string *stack-ui*))
    (setf window1 (gtk:builder-object builder "window1"))
    (setf stack (gtk:builder-object builder "stack"))
    ;; Get ID from object
    (is (string= "window1" (gtk:buildable-buildable-id window1)))
    (is (string= "stack" (gtk:buildable-buildable-id stack)))
    (is (string= "image"
                 (gtk:buildable-buildable-id
                     (setf image (gtk:builder-object builder "image")))))
    (is (string= "checkbutton"
                 (gtk:buildable-buildable-id
                     (setf button (gtk:builder-object builder "checkbutton")))))
    (is (string= "spinner"
                 (gtk:buildable-buildable-id
                     (setf spinner (gtk:builder-object builder "spinner")))))
    ;; Destroy window
    (is-false (gtk:window-destroy window1))))

;;; 2025-4-17
