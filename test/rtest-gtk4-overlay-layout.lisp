(in-package :gtk-test)

(def-suite gtk-overlay-layout :in gtk-suite)
(in-suite gtk-overlay-layout)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkOverlayLayoutChild

(test gtk-overlay-layout-child-class
  ;; Check type
  (is (g:type-is-object "GtkOverlayLayoutChild"))
  ;; Check registered name
  (is (eq 'gtk:overlay-layout-child
          (glib:symbol-for-gtype "GtkOverlayLayoutChild")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkOverlayLayoutChild")
          (g:gtype (cffi:foreign-funcall "gtk_overlay_layout_child_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkLayoutChild")
          (g:type-parent "GtkOverlayLayoutChild")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkOverlayLayoutChild")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkOverlayLayoutChild")))
  ;; Check properties
  (is (equal '("clip-overlay" "measure")
             (glib-test:list-properties "GtkOverlayLayoutChild")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkOverlayLayoutChild")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkOverlayLayoutChild"
                                      GTK:OVERLAY-LAYOUT-CHILD
                      (:SUPERCLASS GTK:LAYOUT-CHILD
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gtk_overlay_layout_child_get_type")
                      ((CLIP-OVERLAY OVERLAY-LAYOUT-CHILD-CLIP-OVERLAY
                        "clip-overlay" "gboolean" T T)
                       (MEASURE OVERLAY-LAYOUT-CHILD-MEASURE
                        "measure" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkOverlayLayoutChild"))))

;;; --- Properties -------------------------------------------------------------

;;;     clip-overlay
;;;     measure

;;; --- Types and Values -------------------------------------------------------

;;;     GtkOverlayLayout

(test gtk-overlay-layout-class
  ;; Check type
  (is (g:type-is-object "GtkOverlayLayout"))
  ;; Check registered name
  (is (eq 'gtk:overlay-layout
          (glib:symbol-for-gtype "GtkOverlayLayout")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkOverlayLayout")
          (g:gtype (cffi:foreign-funcall "gtk_overlay_layout_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkLayoutManager")
          (g:type-parent "GtkOverlayLayout")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkOverlayLayout")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkOverlayLayout")))
  ;; Check properties
  (is (equal '()
             (glib-test:list-properties "GtkOverlayLayout")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkOverlayLayout")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkOverlayLayout" GTK:OVERLAY-LAYOUT
                      (:SUPERCLASS GTK:LAYOUT-MANAGER
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gtk_overlay_layout_get_type")
                      NIL)
             (gobject:get-gtype-definition "GtkOverlayLayout"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_overlay_layout_new

(test gtk-overlay-layout-new
  (is (typep (gtk:overlay-layout-new) 'gtk:overlay-layout)))

;;; 2024-9-19
