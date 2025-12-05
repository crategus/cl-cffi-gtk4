;;; ----------------------------------------------------------------------------
;;; gtk4.tree-view-dnd.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.12 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2025 Dieter Kaiser
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
;;;
;;; GtkTreeView drag-and-drop
;;;
;;;     Interfaces for drag-and-drop support in GtkTreeView
;;;
;;; Types and Values
;;;
;;;     GtkTreeDragSource
;;;     GtkTreeDragSourceIface
;;;
;;;     GtkTreeDragDest
;;;     GtkTreeDragDestIface
;;;
;;; Functions
;;;
;;;     gtk_tree_drag_source_drag_data_delete
;;;     gtk_tree_drag_source_drag_data_get
;;;     gtk_tree_drag_source_row_draggable
;;;
;;;     gtk_tree_drag_dest_drag_data_received
;;;     gtk_tree_drag_dest_row_drop_possible
;;;
;;;     gtk_tree_create_row_drag_content
;;;     gtk_tree_get_row_drag_data                          not exported
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ├── GtkTreeDragDest
;;;     ╰── GtkTreeDragSource
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkTreeDragSource
;;; ----------------------------------------------------------------------------

(gobject:define-ginterface "GtkTreeDragSource" tree-drag-source
  (:export t
   :type-initializer "gtk_tree_drag_source_get_type")
  nil)

#+(and gtk-4-10 gtk-warn-deprecated)
(defmethod initialize-instance :after ((obj tree-drag-source) &key)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:TREE-DRAG-SOURCE is deprecated since 4.10")))

#+liber-documentation
(setf (liber:alias-for-class 'tree-drag-source)
      "Interface"
      (documentation 'tree-drag-source 'type)
 "@version{2024-05-28}
  @begin{short}
    Interface for Drag-and-Drop destinations in the @class{gtk:tree-view}
    widget.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-drag-source} implementation is deprecated since 4.10.
    List views use widgets to display their contents. You can use the
    @class{gtk:drag-source} implementation to implement a drag source.
  @end{dictionary}
  @see-class{gtk:list-store}
  @see-class{gtk:tree-store}
  @see-class{gtk:tree-model-filter}
  @see-class{gtk:tree-model-sort}
  @see-class{gtk:drag-source}")

;;; ----------------------------------------------------------------------------
;;; GtkTreeDragSourceIface
;;; ----------------------------------------------------------------------------

(gobject:define-vtable ("GtkTreeDragSource" tree-drag-source)
  (:skip parent-instance (:pointer (:struct gobject:type-interface)))
  ;;methods
  (row-draggable (:boolean
                  (tree-drag-source g:object)
                  (path (g:boxed tree-path))))
  (drag-data-get ((g:object gkd-content-provider)
                  (tree-drag-source g:object)
                  (path (g:boxed tree-path))))
  (drag-data-delete (:boolean
                     (tree-drag-source g:object)
                     (path (g:boxed tree-path)))))

;;; ----------------------------------------------------------------------------
;;; gtk_tree_drag_source_drag_data_delete
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_drag_source_drag_data_delete"
               tree-drag-source-drag-data-delete) :boolean
 #+liber-documentation
 "@version{#2025-04-15}
  @argument[source]{a @class{gtk:tree-drag-source} object}
  @argument[path]{a @class{gtk:tree-path} instance for the row that was being
    dragged}
  @return{@em{True} if the row was successfully deleted.}
  @begin{short}
    Asks the @class{gtk:tree-drag-source} object to delete the row at
    @arg{path}, because it was moved somewhere else via drag-and-drop.
  @end{short}
  Returns @em{false} if the deletion fails because @arg{path} no longer exists,
  or for some model-specific reason. Should robustly handle a path no longer
  found in the model.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-drag-source} implementation is deprecated since 4.10.
    Use list models instead.
  @end{dictionary}
  @see-class{gtk:tree-drag-source}
  @see-class{gtk:tree-path}"
  (source (g:object tree-drag-source))
  (path (g:boxed tree-path)))

(export 'tree-drag-source-drag-data-delete)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_drag_source_drag_data_get
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_drag_source_drag_data_get"
               tree-drag-source-drag-data-get) (g:object gdk:content-provider)
 #+liber-documentation
 "@version{#2025-07-22}
  @argument[source]{a @class{gtk:tree-drag-source} object}
  @argument[path]{a @class{gtk:tree-path} instance for the row that was dragged}
  @begin{return}
    The @class{gdk:content-provider} instance for the given @arg{path} or
    @code{nil} if none exists.
  @end{return}
  @begin{short}
    Asks the @class{gtk:tree-drag-source} object to return a
    @class{gdk:content-provider} object representing the row at @arg{path}.
  @end{short}
  Should robustly handle a @arg{path} no longer found in the model.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-drag-source} implementation is deprecated since 4.10.
    Use list models instead.
  @end{dictionary}
  @see-class{gtk:tree-drag-source}
  @see-class{gtk:tree-path}
  @see-class{gdk:content-provider}"
  (source (g:object tree-drag-source))
  (path (g:boxed tree-path)))

(export 'tree-drag-source-drag-data-get)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_drag_source_row_draggable
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_drag_source_row_draggable"
               tree-drag-source-row-draggable) :boolean
 #+liber-documentation
 "@version{#2025-04-15}
  @argument[source]{a @class{gtk:tree-drag-source} object}
  @argument[path]{a @class{gtk:tree-path} instance for the row on which user
    is initiating a drag}
  @return{@em{True} if the row can be dragged.}
  @begin{short}
    Asks the @class{gtk:tree-drag-source} object whether a particular row can
    be used as the source of a DND operation.
  @end{short}
  If the source does not implement this interface, the row is assumed draggable.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-drag-source} implementation is deprecated since 4.10.
    Use list models instead.
  @end{dictionary}
  @see-class{gtk:tree-drag-source}
  @see-class{gtk:tree-path}"
  (source (g:object tree-drag-source))
  (path (g:boxed tree-path)))

(export 'tree-drag-source-row-draggable)

;;; ----------------------------------------------------------------------------
;;; GtkTreeDragDest
;;; ----------------------------------------------------------------------------

(gobject:define-ginterface "GtkTreeDragDest" tree-drag-dest
  (:export t
   :type-initializer "gtk_tree_drag_dest_get_type")
  nil)

#+(and gtk-4-10 gtk-warn-deprecated)
(defmethod initialize-instance :after ((obj tree-drag-dest) &key)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:TREE-DRAG-DEST is deprecated since 4.10")))

#+liber-documentation
(setf (liber:alias-for-class 'tree-drag-dest)
      "Interface"
      (documentation 'tree-drag-dest 'type)
 "@version{2024-05-01}
  @begin{short}
    Interface for Drag-and-Drop destinations in the @class{gtk:tree-view}
    widget.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:tree-drag-dest} implementation is deprecated since 4.10.
    List views use widgets to display their contents. You can use the
    @class{gtk:drop-target} implementation to implement a drop destination.
  @end{dictionary}
  @see-class{gtk:list-store}
  @see-class{gtk:tree-store}
  @see-class{gtk:drop-target}")

;;; ----------------------------------------------------------------------------
;;; GtkTreeDragDestIface
;;; ----------------------------------------------------------------------------

(gobject:define-vtable ("GtkTreeDragDest" tree-drag-dest)
  (:skip parent-instance (:pointer (:struct gobject:type-interface)))
  ;;methods
  (drag-data-received (:boolean
               (tree-drag-dest g:object)
               (path (g:boxed tree-path))
               (value (:pointer (:struct g:value)))))
  (row-drop-possible (:boolean
              (tree-drag-dest g:object)
              (path (g:boxed tree-path))
              (value (:pointer (:struct g:value))))))

;;; ----------------------------------------------------------------------------
;;; gtk_tree_drag_dest_drag_data_received
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_drag_dest_drag_data_received"
               tree-drag-dest-drag-data-received) :boolean
 #+liber-documentation
 "@version{#2025-07-22}
  @argument[dest]{a @class{gtk:tree-drag-dest} object}
  @argument[path]{a @class{gtk:tree-path} instance for the row to drop in
    front of}
  @argument[value]{a @sym{g:value} instance for the data to drop}
  @return{The boolean whether a new row was created before position @arg{dest}.}
  @begin{short}
    Asks the @class{gtk:tree-drag-dest} object to insert a row before the path
    @arg{dest}, deriving the contents of the row from @arg{value}.
  @end{short}
  If @arg{dest} is outside the tree so that inserting before it is impossible,
  @em{false} will be returned. Also, @em{false} may be returned if the new row
  is not created for some model-specific reason. Should robustly handle a dest
  no longer found in the model.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-drag-dest} implementation is deprecated since 4.10.
    Use list models instead.
  @end{dictionary}
  @see-class{gtk:tree-drag-dest}
  @see-class{gtk:tree-path}
  @see-symol{g:value}"
  (dest (g:object tree-drag-dest))
  (path (g:boxed tree-path))
  (value (:pointer (:struct g:value))))

(export 'tree-drag-dest-drag-data-received)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_drag_dest_row_drop_possible
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_drag_dest_row_drop_possible"
               tree-drag-dest-row-drop-possible) :boolean
 #+liber-documentation
 "@version{#2025-07-22}
  @argument[dest]{a @class{gtk:tree-drag-dest} object}
  @argument[path]{a @class{gtk:tree-path} instance for the destination row}
  @argument[value]{a @sym{g:value} instance for the data being dropped}
  @return{@em{True} if a drop is possible before @arg{dest}.}
  @begin{short}
    Determines whether a drop is possible before the given @arg{dest}, at the
    same depth as @arg{dest}.
  @end{short}
  That is, can we drop the data in @arg{data} at that location. The @arg{dest}
  argument does not have to exist. The return value will almost certainly be
  @em{false} if the parent of @arg{dest} does not exist, though.
  @begin[Warning]{dictionary}
    The @class{gtk:tree-drag-dest} implementation is deprecated since 4.10.
    Use list models instead.
  @end{dictionary}
  @see-class{gtk:tree-drag-dest}
  @see-class{gtk:tree-path}
  @see-symbol{g:value}"
  (dest (g:object tree-drag-dest))
  (path (g:boxed tree-path))
  (value (:pointer (:struct g:value))))

(export 'tree-drag-dest-row-drop-possible)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_create_row_drag_content
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_create_row_drag_content" tree-create-row-drag-content)
    (g:object gdk:content-provider)
 #+liber-documentation
 "@version{#2025-11-16}
  @argument[model]{a @class{gtk:tree-model} object}
  @argument[path]{a @class{gtk:tree-path} instance for a row in @arg{model}}
  @return{The new @class{gdk:content-provider} object.}
  @begin{short}
    Creates a content provider for dragging @arg{path} from @arg{model}.
  @end{short}
  @begin[Warning]{dictionary}
    This function is deprecated since 4.10. Use list models instead.
  @end{dictionary}
  @see-class{gtk:tree-model}
  @see-class{gtk:tree-path}
  @see-class{gdk:content-provider}"
  (model (g:object tree-model))
  (path (g:boxed tree-path)))

(export 'tree-create-row-drag-content)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_get_row_drag_data                              not exported
;;; ----------------------------------------------------------------------------

;; TODO: This function is not fully implemented. The model and path arguments
;; contains the returned values.

(cffi:defcfun ("gtk_tree_get_row_drag_data" tree-get-row-drag-data) :boolean
 #+liber-documentation
 "@version{#2025-07-13}
  @argument[data]{a @sym{g:value} instance}
  @argument[model]{a @class{gtk:tree-model} object}
  @argument[path]{a @class{gtk:tree-path} for a row in @arg{model}.}
  @begin{return}
    @em{True} if @arg{data} had target type @code{GTK_TREE_MODEL_ROW} and
    is otherwise valid.
  @end{return}
  @begin{short}
    Obtains a tree_model and path from selection data of target type
    @code{GTK_TREE_MODEL_ROW}.
  @end{short}
  Normally called from a @code{drag_data_received} handler. This function can
  only be used if @arg{data} originates from the same process that is calling
  this function, because a pointer to the tree model is being passed around. If
  you are not in the same process, then you will get memory corruption. In the
  @code{drag_data_received} handler, you can assume that selection data of type
  @code{GTK_TREE_MODEL_ROW} is in from the current process.
  @begin[Warning]{dictionary}
    This function is deprecated since 4.10. Use list models instead.
  @end{dictionary}
  @see-class{gtk:selection-data}
  @see-class{gtk:tree-model}
  @see-class{gtk:tree-path}"
  (value (:pointer (:struct g:value)))
  (model (g:object tree-model))
  (path (g:boxed tree-path)))

;;; --- End of file gtk4.tree-view-dnd.lisp ------------------------------------
