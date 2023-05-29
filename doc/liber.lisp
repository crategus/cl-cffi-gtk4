;;; ----------------------------------------------------------------------------
;;; liber.lisp
;;;
;;; Copyright (C) 2021 - 2023 Dieter Kaiser
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;; ----------------------------------------------------------------------------

#-liber-documentation
(push :liber-documentation *features*)

(asdf:load-system :liber)
(asdf:load-system :cl-cffi-cairo :force t)
(asdf:load-system :cl-cffi-graphene :force t)
(asdf:load-system :cl-cffi-glib :force t)
(asdf:load-system :cl-cffi-gtk4 :force t)

(defpackage :liber-gtk4
  (:use :common-lisp)
  (:import-from :liber)
  (:export :generate-html
           :generate-html-single-page))

(in-package :liber-gtk4)

(unexport 'glib:allocate-stable-pointer :glib)
(unexport 'glib:stable-pointer-destroy-notify :glib)
(unexport 'glib:get-stable-pointer-value :glib)
(unexport 'glib:free-stable-pointer :glib)
(unexport 'glib:set-stable-pointer-value :glib)
(unexport 'glib:with-stable-pointer :glib)
(unexport 'glib:with-catching-to-g-error :glib)
(unexport 'glib:with-g-error :glib)
(unexport 'glib:with-ignore-g-error :glib)

(unexport 'gobject:*debug-gc* :gobject)
(unexport 'gobject:*debug-subclass* :gobject)
(unexport 'gobject:*gobject-debug* :gobject)

(unexport 'gobject:create-fn-ref :gobject)
(unexport 'gobject:define-foreign-g-object-class :gobject)
(unexport 'gobject:define-boxed-opaque-accessor :gobject)
(unexport 'gobject:define-cb-methods :gobject)
(unexport 'gobject:define-g-boxed-cstruct :gobject)
(unexport 'gobject:define-g-boxed-opaque :gobject)
(unexport 'gobject:define-g-boxed-variant-cstruct :gobject)
(unexport 'gobject:define-g-enum :gobject)
(unexport 'gobject:define-g-flags :gobject)
(unexport 'gobject:define-g-interface :gobject)
(unexport 'gobject:define-g-object-class :gobject)
(unexport 'gobject:define-vtable :gobject)
(unexport 'gobject:get-g-type-definition :gobject)
(unexport 'gobject:get-g-enum-definition :gobject)
(unexport 'gobject:get-g-flags-definition :gobject)
(unexport 'gobject:get-enum-items :gobject)
(unexport 'gobject:enum-item-name :gobject)
(unexport 'gobject:enum-item-nick :gobject)
(unexport 'gobject:enum-item-value :gobject)
(unexport 'gobject:get-flags-items :gobject)
(unexport 'gobject:flags-item-name :gobject)
(unexport 'gobject:flags-item-nick :gobject)
(unexport 'gobject:flags-item-value :gobject)
(unexport 'gobject:gobject-class :gobject)
(unexport 'gobject:gobject-class-direct-g-type-name :gobject)
(unexport 'gobject:gobject-class-g-type-initializer :gobject)
(unexport 'gobject:gobject-class-g-type-name :gobject)
(unexport 'gobject:gobject-class-interface-p :gobject)
(unexport 'gobject:parse-g-value :gobject)
(unexport 'gobject:set-g-value :gobject)
(unexport 'gobject:with-foreign-boxed-array :gobject)
(unexport 'gobject::g-initially-unowned :gobject)
(unexport 'gobject:lisp-closure :gobject)
(unexport 'gobject:gtype :gobject)

;;; ---------------------------------------------------------------------------

(defun generate-html ()
  (let* ((base (asdf:component-pathname (asdf:find-system :cl-cffi-gtk4)))
         (output-directory (merge-pathnames "../books/cl-cffi-gtk4/" base)))
    (format t "Generate HTML to ~a~%" output-directory)
    (ensure-directories-exist output-directory)
    (liber:generate-html-documentation
      '(:gtk :gdk :gsk
        :gdk-pixbuf
        :gobject
        :glib
        :gio
        :pango
        :cairo
        :graphene)
      output-directory
      :author "Crategus"
      :author-url "http://www.crategus.com"
      :index-title "cl-cffi-gtk4 API documentation"
      :heading "cl-cffi-gtk4"
      :css "crategus.css"
      :single-page-p nil
      :paginate-section-p nil
      :include-slot-definitions-p t
      :include-internal-symbols-p nil)))

(defun generate-html-single-page ()
  (let* ((base (asdf:component-pathname (asdf:find-system :cl-cffi-gtk4)))
         (output-directory
             (merge-pathnames "../books/cl-cffi-gtk4/single-page/" base)))
    (format t "Generate Single PAGE HTML to ~a~%" output-directory)
    (ensure-directories-exist output-directory)
    (liber:generate-html-documentation
      '(:gtk :gdk :gsk
        :gdk-pixbuf
        :gobject
        :glib
        :gio
        :pango
        :cairo
        :graphene)
      output-directory
      :author "Crategus"
      :author-url "http://www.crategus.com"
      :index-title "cl-cffi-gtk4 API documentation (single page)"
      :heading "cl-cffi-gtk4"
      :css "crategus.css"
      :single-page-p t
      :include-slot-definitions-p t
      :include-internal-symbols-p nil)))

;;; --- End of file liber.lisp -------------------------------------------------
