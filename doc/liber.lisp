;;; ----------------------------------------------------------------------------
;;; liber.lisp
;;;
;;; Copyright (C) 2021 - 2022 Dieter Kaiser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
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

(unexport 'gobject:boxed-related-symbols :gobject)
(unexport 'gobject:copy-boxed-slots-to-foreign :gobject)
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
(unexport 'gobject:list-signals :gobject)
(unexport 'gobject:parse-g-param-spec :gobject)
(unexport 'gobject:parse-g-value :gobject)
(unexport 'gobject:set-g-value :gobject)
(unexport 'gobject:signal-info :gobject)
(unexport 'gobject:using* :gobject)
(unexport 'gobject:with-foreign-boxed-array :gobject)
(unexport 'gobject::g-initially-unowned :gobject)

(unexport 'gobject:lisp-closure :gobject)

(unexport 'gobject:gtype :gobject)
(unexport 'gobject:gtype-from-id :gobject)
(unexport 'gobject:gtype-from-name :gobject)
(unexport 'gobject:gtype-id :gobject)
(unexport 'gobject:gtype-name :gobject)

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
