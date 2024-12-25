(in-package :gtk-test)

(def-suite gtk-graphics-offload :in gtk-media-support)
(in-suite gtk-graphics-offload)

;;; --- Types and values -------------------------------------------------------

;;;     GtkGraphicsOffloadEnabled

(test gtk-graphics-offload-enabled
  ;; Check type
  (is (g:type-is-enum "GtkGraphicsOffloadEnabled"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkGraphicsOffloadEnabled")
          (g:gtype (cffi:foreign-funcall "gtk_graphics_offload_enabled_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gtk:graphics-offload-enabled
          (glib:symbol-for-gtype "GtkGraphicsOffloadEnabled")))
  ;; Check names
  (is (equal '("GTK_GRAPHICS_OFFLOAD_ENABLED" "GTK_GRAPHICS_OFFLOAD_DISABLED")
             (glib-test:list-enum-item-names "GtkGraphicsOffloadEnabled")))
  ;; Check values
  (is (equal '(0 1)
             (glib-test:list-enum-item-values "GtkGraphicsOffloadEnabled")))
  ;; Check nick names
  (is (equal '("enabled" "disabled")
             (glib-test:list-enum-item-nicks "GtkGraphicsOffloadEnabled")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkGraphicsOffloadEnabled"
                                    GTK:GRAPHICS-OFFLOAD-ENABLED
                       (:EXPORT T
                        :TYPE-INITIALIZER
                        "gtk_graphics_offload_enabled_get_type")
                       (:ENABLED 0)
                       (:DISABLED 1))
             (gobject:get-gtype-definition "GtkGraphicsOffloadEnabled"))))

;;;     GtkGraphicsOffload

(test gtk-graphics-offload-class
  ;; Check type
  (is (g:type-is-object "GtkGraphicsOffload"))
  ;; Check registered name
  (is (eq 'gtk:graphics-offload
          (glib:symbol-for-gtype "GtkGraphicsOffload")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkGraphicsOffload")
          (g:gtype (cffi:foreign-funcall "gtk_graphics_offload_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkGraphicsOffload")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkGraphicsOffload")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (glib-test:list-interfaces "GtkGraphicsOffload")))
  ;; Check properties
  (is (equal '("black-background" "child" "enabled")
             (glib-test:list-properties "GtkGraphicsOffload")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkGraphicsOffload")))
  ;; Check CSS name
  (is (string= "graphicsoffload"
               (gtk:widget-class-css-name "GtkGraphicsOffload")))
  ;; Check accessible role
  (is (eq :widget (gtk:widget-class-accessible-role "GtkGraphicsOffload")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkGraphicsOffload" GTK:GRAPHICS-OFFLOAD
                       (:SUPERCLASS GTK:WIDGET
                        :EXPORT T
                        :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
                        :TYPE-INITIALIZER "gtk_graphics_offload_get_type")
                       ((BLACK-BACKGROUND GRAPHICS-OFFLOAD-BLACK-BACKGROUND
                         "black-background" "gboolean" T T)
                        (CHILD GRAPHICS-OFFLOAD-CHILD "child" "GtkWidget" T T)
                        (ENABLED GRAPHICS-OFFLOAD-ENABLED "enabled"
                         "GtkGraphicsOffloadEnabled" T T)))
             (gobject:get-gtype-definition "GtkGraphicsOffload"))))

;;; --- Properties -------------------------------------------------------------

;;;     black-background
;;;     child
;;;     enabled

(test gtk-graphics-offload-properties
  (let ((offload (make-instance 'gtk:graphics-offload)))
    (is-false (gtk:graphics-offload-black-background offload))
    (is-false (gtk:graphics-offload-child offload))
    (is (eq :enabled (gtk:graphics-offload-enabled offload)))
    (is (= 1 (g:object-ref-count offload)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_graphics_offload_new

(test gtk-graphics-offload-new.1
  (let (offload)
    (is (typep (setf offload (gtk:graphics-offload-new)) 'gtk:graphics-offload))
    (is (= 1 (g:object-ref-count offload)))))

(test gtk-graphics-offload-new.2
  (let ((video(gtk:video-new))
        offload)
    (is (typep (setf offload
                     (gtk:graphics-offload-new video)) 'gtk:graphics-offload))
    ;; Check memory management
    (is-false (setf (gtk:graphics-offload-child offload) nil))
    (is (= 1 (g:object-ref-count video)))
    (is (= 1 (g:object-ref-count offload)))))

;;; 2024-11-2
