(in-package :gtk-test)

(def-suite gdk-cursor :in gdk-suite)
(in-suite gdk-cursor)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkCursor

(test gdk-cursor-class
  ;; Check type
  (is (g:type-is-object "GdkCursor"))
  ;; Check registered name
  (is (eq 'gdk:cursor
          (glib:symbol-for-gtype "GdkCursor")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkCursor")
          (g:gtype (cffi:foreign-funcall "gdk_cursor_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkCursor")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GdkCursor")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GdkCursor")))
  ;; Check properties
  (is (equal '("fallback" "hotspot-x" "hotspot-y" "name" "texture")
             (glib-test:list-properties "GdkCursor")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GdkCursor")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GdkCursor" GDK:CURSOR
                      (:SUPERCLASS G:OBJECT
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gdk_cursor_get_type")
                      ((FALLBACK CURSOR-FALLBACK "fallback" "GdkCursor" T NIL)
                       (HOTSPOT-X CURSOR-HOTSPOT-X "hotspot-x" "gint" T NIL)
                       (HOTSPOT-Y CURSOR-HOTSPOT-Y "hotspot-y" "gint" T NIL)
                       (NAME CURSOR-NAME "name" "gchararray" T NIL)
                       (TEXTURE CURSOR-TEXTURE "texture" "GdkTexture" T NIL)))
             (gobject:get-gtype-definition "GdkCursor"))))

;;; --- Properties -------------------------------------------------------------

(test gdk-cursor-properties.1
  (let ((cursor (make-instance 'gdk:cursor)))
    (is-false (gdk:cursor-fallback cursor))
    (is (= 0 (gdk:cursor-hotspot-x cursor)))
    (is (= 0 (gdk:cursor-hotspot-y cursor)))
    (is-false (gdk:cursor-name cursor))
    (is-false (gdk:cursor-texture cursor))))

(test gdk-cursor-properties.2
  (let* ((fallback (gdk:cursor-new-from-name "default"))
         (cursor (gdk:cursor-new-from-name "help" fallback)))
    (is (eq fallback (gdk:cursor-fallback cursor)))
    (is (= 0 (gdk:cursor-hotspot-x cursor)))
    (is (= 0 (gdk:cursor-hotspot-y cursor)))
    (is (string= "help" (gdk:cursor-name cursor)))
    (is-false (gdk:cursor-texture cursor))))

(test gdk-cursor-properties.3
  (let* ((path (glib-sys:sys-path "test/resource/gtk-logo-24.png"))
         (texture (gdk:texture-new-from-filename path))
         (fallback (gdk:cursor-new-from-name "default"))
         (cursor (gdk:cursor-new-from-texture texture 5 10 fallback)))
    (is (eq fallback (gdk:cursor-fallback cursor)))
    (is (=  5 (gdk:cursor-hotspot-x cursor)))
    (is (= 10 (gdk:cursor-hotspot-y cursor)))
    (is-false (gdk:cursor-name cursor))
    (is (eq texture (gdk:cursor-texture cursor)))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk_cursor_new_from_texture

(test gdk-cursor-new-from-texture
  (let* ((path (glib-sys:sys-path "test/resource/gtk-logo-24.png"))
         (texture (gdk:texture-new-from-filename path))
         (fallback (gdk:cursor-new-from-name "default")))
    (is (typep (gdk:cursor-new-from-texture texture 0 0) 'gdk:cursor))
    (is (typep (gdk:cursor-new-from-texture texture 0 0 nil) 'gdk:cursor))
    (is (typep (gdk:cursor-new-from-texture texture 0 0 fallback) 'gdk:cursor))))

;;;     gdk_cursor_new_from_name

(test gdk-cursor-new-from-name
  (let ((fallback (gdk:cursor-new-from-name "default")))
    (is (typep (gdk:cursor-new-from-name "help") 'gdk:cursor))
    (is (typep (gdk:cursor-new-from-name "help" nil) 'gdk:cursor))
    (is (typep (gdk:cursor-new-from-name "help" fallback) 'gdk:cursor))))

;;;     gdk_cursor_new_from_callback

(defun cursor-callback (cursor size scale width height xhotspot yhotspot)
  (declare (ignore cursor))
  (let* ((scaled (ceiling (* size scale)))
         (path (glib-sys:sys-path "/resource/gtk_logo_cursor.svg"))
         (pixbuf (gdk:pixbuf-new-from-file-at-scale path scaled scaled t)))
    (setf (cffi:mem-ref width :int) (truncate size))
    (setf (cffi:mem-ref height :int) (truncate size))
    (setf (cffi:mem-ref xhotspot :int) (truncate (* 18 (/ size 32))))
    (setf (cffi:mem-ref yhotspot :int) (truncate (* 2 (/ size 32))))
    (when pixbuf
      (gdk:texture-new-for-pixbuf pixbuf))))

(test gdk-cursor-new-from-callback
  (glib-test:with-check-memory (cursor)
    (is (typep (setf cursor
                     (gdk:cursor-new-from-callback #'cursor-callback))
               'gdk:cursor))
    (is-false (gdk:cursor-fallback cursor))
    (is-false (gdk:cursor-texture cursor))
    (is (= 0 (gdk:cursor-hotspot-x cursor)))
    (is (= 0 (gdk:cursor-hotspot-y cursor)))))

;;; 2026-02-22
