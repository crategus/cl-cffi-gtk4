(in-package :gtk-test)

(def-suite gtk-style-context :in gtk-suite)
(in-suite gtk-style-context)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkStyleContextPrintFlags

(test gtk-style-context-print-flags
  ;; Check type
  (is (g:type-is-flags "GtkStyleContextPrintFlags"))
  ;; Check registered name
  (is (eq 'gtk:style-context-print-flags
          (glib:symbol-for-gtype "GtkStyleContextPrintFlags")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkStyleContextPrintFlags")
          (g:gtype (cffi:foreign-funcall "gtk_style_context_print_flags_get_type"
                                         :size))))
  ;; Check names
  (is (equal '("GTK_STYLE_CONTEXT_PRINT_NONE" "GTK_STYLE_CONTEXT_PRINT_RECURSE"
               "GTK_STYLE_CONTEXT_PRINT_SHOW_STYLE"
               "GTK_STYLE_CONTEXT_PRINT_SHOW_CHANGE")
             (glib-test:list-flags-item-names "GtkStyleContextPrintFlags")))
  ;; Check values
  (is (equal '(0 1 2 4)
             (glib-test:list-flags-item-values "GtkStyleContextPrintFlags")))
  ;; Check nick names
  (is (equal '("none" "recurse" "show-style" "show-change")
             (glib-test:list-flags-item-nicks "GtkStyleContextPrintFlags")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-GFLAGS "GtkStyleContextPrintFlags"
                                     GTK:STYLE-CONTEXT-PRINT-FLAGS
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_style_context_print_flags_get_type")
                       (:NONE 0)
                       (:RECURSE 1)
                       (:SHOW-STYLE 2)
                       (:SHOW-CHANGE 4))
             (gobject:get-gtype-definition "GtkStyleContextPrintFlags"))))

;;;     GtkBorder

(test gtk-border-boxed
  ;; Check type
  (is (g:type-is-boxed "GtkBorder"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkBorder")
          (g:gtype (cffi:foreign-funcall "gtk_border_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:border
          (glib:symbol-for-gtype "GtkBorder"))))

(test gtk-border-properties
  (let ((border (gtk:border-new)))
    (is (= 0 (gtk:border-left border)))
    (is (= 0 (gtk:border-right border)))
    (is (= 0 (gtk:border-top border)))
    (is (= 0 (gtk:border-bottom border)))))

(test gtk-border-new/copy
  (let* ((border1 (gtk:border-new :left 10 :right 20 :top 30 :bottom 40))
         (border2 (gtk:border-copy border1)))
    (is (= 10 (gtk:border-left border2)))
    (is (= 20 (gtk:border-right border2)))
    (is (= 30 (gtk:border-top border2)))
    (is (= 40 (gtk:border-bottom border2)))))

;;;     GtkStyleContext

(test gtk-style-context-class
  ;; Check type
  (is (g:type-is-object "GtkStyleContext"))
  ;; Check registered name
  (is (eq 'gtk:style-context
          (glib:symbol-for-gtype "GtkStyleContext")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkStyleContext")
          (g:gtype (cffi:foreign-funcall "gtk_style_context_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkStyleContext")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkStyleContext")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkStyleContext")))
  ;; Check properties
  (is (equal '("display")
             (glib-test:list-properties "GtkStyleContext")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkStyleContext")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkStyleContext" GTK:STYLE-CONTEXT
                      (:SUPERCLASS GOBJECT:OBJECT
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gtk_style_context_get_type")
                      ((DISPLAY STYLE-CONTEXT-DISPLAY
                        "display" "GdkDisplay" T T)))
             (gobject:get-gtype-definition "GtkStyleContext"))))

;;; --- Properties -------------------------------------------------------------

;;;     display

(test gtk-style-context-properties
  (let* ((*gtk-warn-deprecated* nil)
         (context (gtk:widget-style-context (make-instance 'gtk:box)))
         (display nil))
    (is (typep (setf display
                     (gtk:style-context-display context)) 'gdk:display))
    (is (eq display (gdk:display-default)))))

;;; --- Signals ----------------------------------------------------------------

;;;     changed

;;; --- Functions --------------------------------------------------------------

;;;     gtk_style_context_add_provider
;;;     gtk_style_context_add_provider_for_display
;;;     gtk_style_context_get_state
;;;     gtk_style_context_set_state
;;;     gtk_style_context_get_color
;;;     gtk_style_context_get_border
;;;     gtk_style_context_get_padding
;;;     gtk_style_context_get_margin
;;;     gtk_style_context_lookup_color
;;;     gtk_style_context_remove_provider
;;;     gtk_style_context_remove_provider_for_display
;;;     gtk_style_context_restore
;;;     gtk_style_context_save
;;;     gtk_style_context_add_class
;;;     gtk_style_context_remove_class
;;;     gtk_style_context_has_class
;;;     gtk_style_context_set_scale
;;;     gtk_style_context_get_scale

;;;     gtk_style_context_to_string

(test gtk-style-context-to-string
  (let* ((*gtk-warn-deprecated* nil)
         (dialog (make-instance 'gtk:dialog))
         (context (gtk:widget-style-context dialog)))
    (is (string=
"[window.background.dialog:dir(ltr)]
  box.dialog-vbox.vertical:dir(ltr)
    box.vertical:dir(ltr)
    box.dialog-action-box.horizontal:dir(ltr)
      box.dialog-action-area.horizontal:dir(ltr)
"
                 (gtk:style-context-to-string context :recurse)))
    (is-false (gtk:window-destroy dialog))
    (is (= 1 (g:object-ref-count dialog)))))

;;;     gtk_render_activity
;;;     gtk_render_arrow
;;;     gtk_render_background
;;;     gtk_render_check
;;;     gtk_render_expander
;;;     gtk_render_focus
;;;     gtk_render_frame
;;;     gtk_render_handle
;;;     gtk_render_icon
;;;     gtk_render_layout
;;;     gtk_render_line
;;;     gtk_render_option

;;; 2024-10-13
