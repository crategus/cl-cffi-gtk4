;;; ----------------------------------------------------------------------------
;;; gtk4.assistant.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2023 Dieter Kaiser
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
;;; GtkAssistant
;;;
;;;     A widget used to guide users through multi-step operations
;;;
;;; Types and Values
;;;
;;;     GtkAssistant
;;;     GtkAssistantPage
;;;     GtkAssistantPageType
;;;
;;; Acessors
;;;
;;;     gtk_assistant_page_get_child
;;;
;;;     gtk_assistant_set_page_complete
;;;     gtk_assistant_get_page_complete
;;;     gtk_assistant_set_page_type
;;;     gtk_assistant_get_page_type
;;;     gtk_assistant_set_page_title
;;;     gtk_assistant_get_page_title
;;;
;;;     gtk_assistant_get_pages
;;;
;;; Functions
;;;
;;;     gtk_assistant_new
;;;     gtk_assistant_get_page
;;;     gtk_assistant_get_current_page
;;;     gtk_assistant_set_current_page
;;;     gtk_assistant_get_n_pages
;;;     gtk_assistant_get_nth_page
;;;     gtk_assistant_prepend_page
;;;     gtk_assistant_append_page
;;;     gtk_assistant_insert_page
;;;     gtk_assistant_remove_page
;;;
;;;     GtkAssistantPageFunc
;;;
;;;     gtk_assistant_set_forward_page_func
;;;     gtk_assistant_add_action_widget
;;;     gtk_assistant_remove_action_widget
;;;     gtk_assistant_update_buttons_state
;;;     gtk_assistant_commit
;;;     gtk_assistant_next_page
;;;     gtk_assistant_previous_page
;;
;;; Properties
;;;
;;;     pages
;;;     use-header-bar
;;;
;;;     child
;;;     complete
;;;     page-type
;;;     title
;;;
;;; Signals
;;;
;;;     apply
;;;     cancel
;;;     close
;;;     escape
;;;     prepare
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ├── GInitiallyUnowned
;;;     │   ╰── GtkWidget
;;;     │       ╰── GtkWindow
;;;     │           ╰── GtkAssistant
;;;     ╰── GtkAssistantPage
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;     GtkNative
;;;     GtkRoot
;;;     GtkShortcutManager
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkAssistantPageType
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkAssistantPageType" assistant-page-type
  (:export t
   :type-initializer "gtk_assistant_page_type_get_type")
  (:content  0)
  (:intro    1)
  (:confirm  2)
  (:summary  3)
  (:progress 4)
  (:custom   5))

#+liber-documentation
(setf (liber:alias-for-symbol 'assistant-page-type)
      "GEnum"
      (liber:symbol-documentation 'assistant-page-type)
 "@version{#2022-1-31}
  @begin{short}
    An enumeration for determining the page role inside the
    @class{gtk:assistant} widget. It is used to handle buttons sensitivity and
    visibility.
  @end{short}

  Note that an assistant needs to end its page flow with a page of
  @code{:confirm}, @code{:summary} or @code{:progress} type to be correct. The
  Cancel button will only be shown if the page is not \"committed\". See the
  @fun{gtk:assistant-commit} function for details.
  @begin{pre}
(define-g-enum \"GtkAssistantPageType\" assistant-page-type
  (:export t
   :type-initializer \"gtk_assistant_page_type_get_type\")
  (:content  0)
  (:intro    1)
  (:confirm  2)
  (:summary  3)
  (:progress 4)
  (:custom   5))
  @end{pre}
  @begin[code]{table}
    @entry[:content]{The page has regular contents. Both the Back and Forward
      buttons will be shown.}
    @entry[:intro]{The page contains an introduction to the assistant task.
      Only the Forward button will be shown if there is a Next page.}
    @entry[:confirm]{The page lets the user confirm or deny the changes. The
      Back and Apply buttons will be shown.}
    @entry[:summary]{The page informs the user of the changes done. Only the
      Close button will be shown.}
    @entry[:progress]{Used for tasks that take a long time to complete, blocks
      the assistant until the page is marked as complete. Only the Back button
      will be shown.}
    @entry[:custom]{Used for when other page types are not appropriate. No
      buttons will be shown, and the application must add its own buttons
      through the @fun{gtk:assistant-add-action-widget} function.}
  @end{table}
  @see-class{gtk:assistant}
  @see-function{gtk:assistant-commit}
  @see-function{gtk:assistant-add-action-widget}")

;;; ----------------------------------------------------------------------------
;;; GtkAssistantPage
;;; ----------------------------------------------------------------------------

;; NOTE: The gtk:assistant-page-complete and gtk:assistant-page-page-title,
;; and gtk:assistant-page-page-type accessor functions are not exported. These
;; functions are replaced with corresponding functions for the GTkAssistant
;; class.

(define-g-object-class "GtkAssistantPage" assistant-page
  (:superclass g:object
   :export nil
   :interfaces ()
   :type-initializer "gtk_assistant_page_get_type")
  ((child
    assistant-page-child
    "child" "GtkWidget" t t)
   (complete
    %assistant-page-complete
    "complete" "gboolean" t t)
   (page-type
    %assistant-page-page-type
    "page-type" "GtkAssistantPageType" t t)
   (title
    %assistant-page-title
    "title" "gchararray" t t)))

#+liber-documentation
(setf (documentation 'assistant-page 'type)
 "@version{#2022-1-31}
  @begin{short}
    The @sym{gtk:assistant-page} object is an auxiliary object used by
    the @class{gtk:assistant} class.
  @end{short}
  @begin[Note]{dictionary}
    The accessor functions for the @code{complete}, @code{page-type}, and
    @code{title} properties are not exported for use. The values of the
    properties are accessed with the @fun{gtk:assistant-page-complete},
    @fun{gtk:assistant-page-type}, and @fun{gtk:assistant-page-title} functions,
    which are defined for the @class{gtk:assistant} widget.
  @end{dictionary}
  @see-slot{gtk:assistant-page-child}
  @see-class{gtk:assistant}
  @see-function{gtk:assistant-page-complete}
  @see-function{gtk:assistant-page-type}
  @see-function{gtk:assistant-page-title}")

;; We only export the class and the assistant-page-child function.
(export 'assistant-page)
(export 'assistant-page-child)

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- assistant-page-child -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "child" 'assistant-page) t)
 "The @code{child} property of type @class{gtk:widget}
  (Read / Write / Construct only) @br{}
  The content of the assistant page.")

#+liber-documentation
(setf (liber:alias-for-function 'assistant-page-child)
      "Accessor"
      (documentation 'assistant-page-child 'function)
 "@version{#2022-1-31}
  @syntax[]{(gtk:assistant-page-child object) => page}
  @argument[object]{a @class{gtk:assistant-page} object}
  @argument[page]{a @class{gtk:widget} child widget}
  @begin{short}
    Accessor of the @slot[gtk:assistant-page]{child} slot of the
    @class{gtk:assistant-page} class.
  @end{short}

  The @sym{gtk:assistant-page-child} function returns the child widget to which
  the page belongs.
  @see-class{gtk:assistant-page}
  @see-class{gtk:widget}")

;;; --- assistant-page-complete --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "complete"
                                               'assistant-page) t)
 "The @code{complete} property of type @code{:boolean} (Read / Write) @br{}
  Setting the @code{complete} property to @em{true} marks a page as complete,
  i.e. all the required fields are filled out. GTK uses this information to
  control the sensitivity of the navigation buttons. @br{}
  Default value: @em{false}")

;;; --- assistant-page-page-type -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "page-type"
                                               'assistant-page) t)
 "The @code{page-type} property of type @symbol{gtk:assistant-page-type}
  (Read / Write) @br{}
  The type of the assistant page. @br{}
  Default value: @code{:content}")

;;; --- assistant-page-title -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "title"
                                               'assistant-page) t)
 "The @code{title} property of type @code{:string} (Read / Write) @br{}
  The title of the page. @br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------
;;; GtkAssistant
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkAssistant" assistant
  (:superclass window
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkNative"
                "GtkRoot"
                "GtkShortcutManager")
   :type-initializer "gtk_assistant_get_type")
  ((pages
    assistant-pages
    "pages" "GListModel" t nil)
   (use-header-bar
    assistant-use-header-bar
    "use-header-bar" "gint" t t)))

#+liber-documentation
(setf (documentation 'assistant 'type)
 "@version{#2011-1-31}
  @begin{short}
    A @sym{gtk:assistant} widget is used to represent a generally complex
    operation splitted in several steps.
  @end{short}
  Each step consists of one or more pages. The @sym{gtk:assistant} widget
  guides the user through the pages, and controls the page flow to collect the
  data needed for the operation.

  @image[assistant]{Figure: GtkAssistant}

  The @sym{gtk:assistant} widget handles which buttons to show and to make
  sensitive based on page sequence knowledge and the type of each page in
  addition to state information like the completion and committed page statuses.

  If you have a case that does not quite fit in the @sym{gtk:assistant} widgets
  way of handling buttons, you can use the @code{:custom} value of the
  @symbol{gtk:assistant-page-type} enumeration and handle buttons yourself.

  The @sym{gtk:assistant} widget maintains a @class{gtk:assistant-page} object
  for each added child, which holds additional per-child properties. You obtain
  the @class{gtk:assistant-page} object for a child widget with the
  @fun{gtk:assistant-page} function.
  @begin[GtkAssistant as GtkBuildable]{dictionary}
    The @sym{gtk:assistant} implementation of the @class{gtk:buildable}
    interface exposes the action area as internal children with the
    name @code{\"action_area\"}. To add pages to an assistant in the
    @class{gtk:builder} object, simply add it as a child widget to the
    @sym{gtk:assistant} widget. If you need to set per-object properties,
    create a @class{gtk:assistant-page} object explicitly, and set the child
    widget as a property on it.
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    The @sym{gtk:assistant} implementation has a single CSS node with the name
    @code{window} and @code{.assistant} style class.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"apply\" signal}
      @begin{pre}
lambda (assistant)    :run-last
      @end{pre}
      The signal is emitted when the Apply button is clicked. The default
      behavior of the assistant is to switch to the page after the current page,
      unless the current page is the last one. A handler for the \"apply\"
      signal should carry out the actions for which the wizard has collected
      data. If the action takes a long time to complete, you might consider
      putting a @code{:progress} page after the @code{:confirm} page and handle
      this operation within the \"prepare\" signal of the progress page.
      @begin[code]{table}
        @entry[assistant]{The @sym{gtk:assistant} widget which received the
          signal.}
    @end{table}
    @subheading{The \"cancel\" signal}
      @begin{pre}
lambda (assistant)    :run-last
      @end{pre}
      The signal is emitted when the Cancel button is clicked.
      @begin[code]{table}
        @entry[assistant]{The @sym{gtk:assistant} widget which received the
          signal.}
      @end{table}
    @subheading{The \"close\" signal}
      @begin{pre}
lambda (assistant)    :run-last
      @end{pre}
      The signal is emitted either when the Close button of a summary page is
      clicked, or when the Apply button in the last page in the flow is clicked,
      which is the @code{:confirm} page.
      @begin[code]{table}
        @entry[assistant]{The @sym{gtk:assistant} widget which received the
          signal.}
      @end{table}
    @subheading{The \"escape\" signal}
      @begin{pre}
lambda (assistant)    :action
      @end{pre}
      No documentation.
      @begin[code]{table}
        @entry[assistant]{The @sym{gtk:assistant} widget which received the
          signal.}
      @end{table}
    @subheading{The \"prepare\" signal}
      @begin{pre}
lambda (assistant page)    :run-last
      @end{pre}
      The signal is emitted when a new page is set as the assistants current
      page, before making the new page visible. A handler for this signal can
      do any preparations which are necessary before showing the page.
      @begin[code]{table}
        @entry[assistant]{The @sym{gtk:assistant} widget which received the
          signal.}
      @entry[page]{The @class{gtk:widget} widget for the current page.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk:assistant-pages}
  @see-slot{gtk:assistant-use-header-bar}
  @see-constructor{gtk:assistant-new}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- assistant-pages ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "pages" 'assistant) t)
 "The @code{pages} property of type @class{g-list-model} (Read) @br{}
  The pages of the assistant.")

#+liber-documentation
(setf (liber:alias-for-function 'assistant-pages)
      "Accessor"
      (documentation 'assistant-pages 'function)
 "@version{#2022-1-31}
  @syntax[]{(gtk:assistant-pages object) => pages}
  @syntax[]{(setf (gtk:assistant-pages object) pages)}
  @argument[object]{a @class{gtk:assistant} widget}
  @argument[pages]{a @class{g-list-model} object of the pages}
  @begin{short}
    Accessor of the @slot[gtk:assistant]{pages} slot of the
    @class{gtk:assistant} class.
  @end{short}
  The @sym{gtk:assistant-pages} function gets a list model of the assistant
  pages.
  @see-class{gtk:assistant}
  @see-class{g-list-model}")

;;; --- assistant-use-header-bar -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "use-header-bar"
                                               'assistant) t)
 "The @code{use-header-bar} property of type @code{:int}
  (Read / Write / Construct) @br{}
  @em{True} if the assistant uses a header bar for action buttons instead of the
  action area. For technical reasons, this property is declared as an integer
  property, use the value 1 for @em{true} or -1 for @em{false}. @br{}
  Allowed values: [-1, 1] @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'assistant-use-header-bar)
      "Accessor"
      (documentation 'assistant-use-header-bar 'function)
 "@version{#2022-1-31}
  @syntax[]{(gtk:assistant-use-header-bar object) => setting}
  @syntax[]{(setf (gtk:assistant-use-header-bar object) setting)}
  @argument[object]{a @class{gtk:assistant} widget}
  @argument[setting]{@em{true} if the assistant uses a header bar}
  @begin{short}
    Accessor of the @slot[gtk:assistant]{use-header-bar} slot of the
    @class{gtk:assistant} class.
  @end{short}
  @em{True} if the assistant uses a header bar for action buttons instead of the
  action area. For technical reasons, this property is declared as an integer
  property, use the value 1 for @em{true} or -1 for @em{false}.
  @see-class{gtk:assistant}
  @see-class{gtk:header-bar}")

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_new
;;; ----------------------------------------------------------------------------

(defun assistant-new ()
 #+liber-documentation
 "@version{#2021-10-26}
  @return{A @class{gtk:assistant} widget.}
  @begin{short}
    Creates a new assistant.
  @end{short}
  @see-class{gtk:assistant}"
  (make-instance 'assistant))

(export 'assistant-new)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_get_page -> assistant-page
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_assistant_get_page" assistant-page)
    (g:object assistant-page)
 #+liber-documentation
 "@version{#2022-1-31}
  @argument[assistant]{a @class{gtk:assistant} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @begin{short}
    Returns the @class{gtk:assistant-page} object for the child widget.
  @end{short}
  @see-class{gtk:assistant}
  @see-class{gtk:assistant-page}
  @see-class{gtk:widget}"
  (assistant (g:object assistant))
  (child (g:object widget)))

(export 'assistant-page)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_get_current_page
;;; gtk_assistant_set_current_page -> assistant-current-page
;;; ----------------------------------------------------------------------------

(defun (setf assistant-current-page) (index assistant)
  (cffi:foreign-funcall "gtk_assistant_set_current_page"
                        (g:object assistant) assistant
                        :int index
                        :void)
  index)

(defcfun ("gtk_assistant_get_current_page" assistant-current-page) :int
 #+liber-documentation
 "@version{#2021-11-1}
  @syntax[]{(gtk:assistant-current-page assistant) => index}
  @syntax[]{(setf (gtk:assistant-current-page assistant) index)}
  @argument[assistant]{a @class{gtk:assistant} widget}
  @argument[index]{an integer with the index of the page to switch to, starting
    from 0, if negative, the last page will be used, if greater than the number
    of pages in the assistant, nothing will be done}
  @begin{short}
    Accessor of the current page of the assistant.
  @end{short}
  The @sym{gtk:assistant-current-page} function returns the page number of the
  current page in the assistant. The @sym{(setf gtk:assistant-current-page)}
  function switches the page in the assistant to @arg{index}.

  Note that this will only be necessary in custom buttons, as the assistant
  flow can be set with the @fun{gtk:assistant-set-forward-page-func} function.
  @see-class{gtk:assistant}
  @see-function{gtk:assistant-set-forward-page-func}"
  (assistant (g:object assistant)))

(export 'assistant-current-page)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_get_n_pages -> assistant-n-pages
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_assistant_get_n_pages" assistant-n-pages) :int
 #+liber-documentation
 "@version{#2021-11-1}
  @argument[assistant]{a @class{gtk:assistant} widget}
  @return{An integer with the number of pages in @arg{assistant}.}
  @begin{short}
    Returns the number of pages in the assistant.
  @end{short}
  @see-class{gtk:assistant}"
  (assistant (g:object assistant)))

(export 'assistant-n-pages)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_get_nth_page -> assistant-nth-page
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_assistant_get_nth_page" assistant-nth-page)
    (g:object widget)
 #+liber-documentation
 "@version{#2021-11-2}
  @argument[assistant]{a @class{gtk:assistant} widget}
  @argument[index]{an integer with the index of a page in @arg{assistant},
    or -1 to get the last page}
  @return{The @class{gtk:widget} child widget, or @code{nil} if the @arg{index}
    argument is out of bounds.}
  @begin{short}
    Returns the child widget contained in the assistant with the given page
    index.
  @end{short}
  @see-class{gtk:assistant}
  @see-class{gtk:widget}"
  (assistant (g:object assistant))
  (index :int))

(export 'assistant-nth-page)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_prepend_page
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_assistant_prepend_page" assistant-prepend-page) :int
 #+liber-documentation
 "@version{#2021-10-26}
  @argument[assistant]{a @class{gtk:assistant} widget}
  @argument[page]{a @class{gtk:widget} page of the assistant}
  @return{An integer with the index starting at 0 of the inserted page.}
  @begin{short}
    Prepends a page to the assistant.
  @end{short}
  @see-class{gtk:assistant}
  @see-class{gtk:widget}
  @see-function{gtk:assistant-append-page}
  @see-function{gtk:assistant-insert-page}
  @see-function{gtk:assistant-remove-page}"
  (assistant (g:object assistant))
  (page (g:object widget)))

(export 'assistant-prepend-page)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_append_page
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_assistant_append_page" assistant-append-page) :int
 #+liber-documentation
 "@version{#2021-11-1}
  @argument[assistant]{a @class{gtk:assistant} widget}
  @argument[page]{a @class{gtk:widget} page of the assistant}
  @return{An integer with the index starting at 0 of the inserted page.}
  @begin{short}
    Appends a page to the assistant.
  @end{short}
  @see-class{gtk:assistant}
  @see-class{gtk:widget}
  @see-function{gtk:assistant-prepend-page}
  @see-function{gtk:assistant-insert-page}
  @see-function{gtk:assistant-remove-page}"
  (assistant (g:object assistant))
  (page (g:object widget)))

(export 'assistant-append-page)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_insert_page
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_assistant_insert_page" assistant-insert-page) :int
 #+liber-documentation
 "@version{#2021-10-26}
  @argument[assistant]{a @class{gtk:assistant} widget}
  @argument[page]{a @class{gtk:widget} page of the assistant}
  @argument[position]{an integer with the index starting at 0 at which to
    insert @arg{page}, or -1 to append @arg{page} to the assistant}
  @return{The index starting from 0 of the inserted page.}
  @begin{short}
    Inserts a page in the assistant at a given position.
  @end{short}
  @see-class{gtk:assistant}
  @see-class{gtk:widget}
  @see-function{gtk:assistant-append-page}
  @see-function{gtk:assistant-prepend-page}
  @see-function{gtk:assistant-remove-page}"
  (assistant (g:object assistant))
  (page (g:object widget))
  (position :int))

(export 'assistant-insert-page)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_remove_page
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_assistant_remove_page" assistant-remove-page) :void
 #+liber-documentation
 "@version{#2021-11-1}
  @argument[assistant]{a @class{gtk:assistant} widget}
  @argument[index]{an integer with the index of a page in the assistant, or -1
    to remove the last page}
  @begin{short}
    Removes the page with the given page index from the assistant.
  @end{short}
  @see-class{gtk:assistant}
  @see-function{gtk:assistant-append-page}
  @see-function{gtk:assistant-prepend-page}
  @see-function{gtk:assistant-insert-page}"
  (assistant (g:object assistant))
  (index :int))

(export 'assistant-remove-page)

;;; ----------------------------------------------------------------------------
;;; GtkAssistantPageFunc
;;; ----------------------------------------------------------------------------

(define-cb-methods assistant-page-func :int ((current-page :int)))

#+liber-documentation
(setf (liber:alias-for-symbol 'assistant-page-func)
      "Callback"
      (liber:symbol-documentation 'assistant-page-func)
 "@version{#2021-10-26}
  @begin{short}
    A callback function used by the @fun{gtk:assistant-set-forward-page-func}
    function to know which is the Next page given a current one.
  @end{short}
  It is called both for computing the Next page when the user presses the
  Forward button and for handling the behavior of the Last button.
  @begin{pre}
 lambda (current)
  @end{pre}
  @begin[code]{table}
    @entry[current]{An integer with the page number used to calculate the Next
      page.}
    @entry[Returns]{An integer with the next page number.}
  @end{table}
  @see-class{gtk:assistant}
  @see-function{gtk:assistant-set-forward-page-func}")

(export 'assistant-page-func)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_set_forward_page_func
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_assistant_set_forward_page_func"
          %assistant-set-forward-page-func) :void
  (assistant (g:object assistant))
  (func :pointer)
  (data :pointer)
  (destroy :pointer))

(defun assistant-set-forward-page-func (assistant func)
 #+liber-documentation
 "@version{#2021-10-26}
  @argument[assistant]{a @class{gtk:assistant} widget}
  @argument[func]{a @symbol{gtk:assistant-page-func} page forwarding callback
    function, or @code{nil} to use the default one}
  @begin{short}
    Sets the page forwarding function to be @arg{func}.
  @end{short}
  This function will be used to determine what will be the Next page when the
  user presses the Forward button. Setting @arg{func} to @code{nil} will make
  the assistant to use the default forward function, which just goes to the Next
  visible page.
  @see-class{gtk:assistant}
  @see-symbol{gtk:assistant-page-func}"
  (if func
      (%assistant-set-forward-page-func
          assistant
          (cffi:callback assistant-page-func)
          (gobject:create-fn-ref assistant func)
          (cffi:callback assistant-page-func-destroy-notify))
      (%assistant-set-forward-page-func assistant (cffi:null-pointer)
                                                  (cffi:null-pointer)
                                                  (cffi:null-pointer))))

(export 'assistant-set-forward-page-func)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_set_page_type
;;; gtk_assistant_get_page_type -> assistant-page-type
;;; ----------------------------------------------------------------------------

(defun (setf assistant-page-type) (value assistant page)
  (cffi:foreign-funcall "gtk_assistant_set_page_type"
                        (g:object assistant) assistant
                        (g:object widget) page
                        assistant-page-type value
                        :void)
  value)

(defcfun ("gtk_assistant_get_page_type" assistant-page-type)
    assistant-page-type
 #+liber-documentation
 "@version{#2022-1-31}
  @syntax[]{(gtk:assistant-page-type assistant page) => ptype}
  @syntax[]{(setf (gtk:assistant-page-type assistant page) ptype)}
  @argument[assistant]{a @class{gtk:assistant} widget}
  @argument[page]{a @class{gtk:widget} page of @arg{assistant}}
  @argument[ptype]{a value of the @symbol{gtk:assistant-page-type} enumeration}
  @begin{short}
    Accessor of the page type of a page in the assistant.
  @end{short}

  The page type determines the page behavior in the assistant.
  @see-class{gtk:assistant}
  @see-class{gtk:widget}
  @see-symbol{gtk:assistant-page-type}"
  (assistant (g:object assistant))
  (page (g:object widget)))

(export 'assistant-page-type)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_set_page_title
;;; gtk_assistant_get_page_title -> assistant-page-title
;;; ----------------------------------------------------------------------------

(defun (setf assistant-page-title) (value assistant page)
  (cffi:foreign-funcall "gtk_assistant_set_page_title"
                        (g:object assistant) assistant
                        (g:object widget) page
                        :string value
                        :void)
  value)

(defcfun ("gtk_assistant_get_page_title" assistant-page-title) :string
 #+liber-documentation
 "@version{#2021-11-2}
  @syntax[]{(gtk:assistant-page-title assistant page) => title}
  @syntax[]{(setf (gtk:assistant-page-title assistant page) title)}
  @argument[assistant]{a @class{gtk:assistant} widget}
  @argument[page]{a @class{gtk:widget} page of @arg{assistant}}
  @argument[title]{a string with the new title for @arg{page}}
  @begin{short}
    Accessor of the title for the page in the assistant.
  @end{short}

  The @sym{gtk:assistant-page-title} function gets the title for the page in the
  assistant. The @sym{(setf gtk:assistant-page-title)} function sets a title.
  The title is displayed in the header area of the assistant when the page is
  the current page.
  @see-class{gtk:assistant}
  @see-class{gtk:widget}"
  (assistant (g:object assistant))
  (page (g:object widget)))

(export 'assistant-page-title)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_set_page_complete
;;; gtk_assistant_get_page_complete -> assistant-page-complete
;;; ----------------------------------------------------------------------------

(defun (setf assistant-page-complete) (value assistant page)
  (cffi:foreign-funcall "gtk_assistant_set_page_complete"
                        (g:object assistant) assistant
                        (g:object widget) page
                        :boolean value
                        :void)
  value)

(defcfun ("gtk_assistant_get_page_complete" assistant-page-complete)
    :boolean
 #+liber-documentation
 "@version{#2021-11-2}
  @syntax[]{(gtk:assistant-page-complete assistant page) => complete}
  @syntax[]{(setf (gtk:assistant-page-complete assistant page) complete)}
  @argument[assistant]{a @class{gtk:assistant} widget}
  @argument[page]{a @class{gtk:widget} page of @arg{assistant}}
  @argument[complete]{a boolean with the completeness status of the page}
  @begin{short}
    Accessor of the completeness status of the page in the assistant.
  @end{short}

  The @sym{gtk:assistant-page-complete} function gets whether the page is
  complete. The @sym{(setf gtk:assistant-page-complete)} function sets whether
  the page contents are complete. This will make the assistant update the
  buttons state to be able to continue the task.
  @see-class{gtk:assistant}
  @see-class{gtk:widget}"
  (assistant (g:object assistant))
  (page (g:object widget)))

(export 'assistant-page-complete)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_add_action_widget
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_assistant_add_action_widget" assistant-add-action-widget)
    :void
 #+liber-documentation
 "@version{#2021-11-2}
  @argument[assistant]{a @class{gtk:assistant} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @begin{short}
    Adds a child widget to the action area of the assistant.
  @end{short}
  @see-class{gtk:assistant}
  @see-class{gtk:widget}
  @see-function{gtk:assistant-remove-action-widget}"
  (assistant (g:object assistant))
  (child (g:object widget)))

(export 'assistant-add-action-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_remove_action_widget
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_assistant_remove_action_widget"
           assistant-remove-action-widget) :void
 #+liber-documentation
 "@version{#2021-11-2}
  @argument[assistant]{a @class{gtk:assistant} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @begin{short}
    Removes a child widget from the action area of the assistant.
  @end{short}
  @see-class{gtk:assistant}
  @see-class{gtk:widget}
  @see-function{gtk:assistant-add-action-widget}"
  (assistant (g:object assistant))
  (child (g:object widget)))

(export 'assistant-remove-action-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_update_buttons_state
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_assistant_update_buttons_state"
          assistant-update-buttons-state) :void
 #+liber-documentation
 "@version{#2021-10-26}
  @argument[assistant]{a @class{gtk:assistant} widget}
  @begin{short}
    Forces the assistant to recompute the buttons state.
  @end{short}
  GTK automatically takes care of this in most situations, e.g. when the user
  goes to a different page, or when the visibility or completeness of a page
  changes. One situation where it can be necessary to call this function is
  when changing a value on the current page affects the future page flow of the
  assistant.
  @see-class{gtk:assistant}"
  (assistant (g:object assistant)))

(export 'assistant-update-buttons-state)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_commit
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_assistant_commit" assistant-commit) :void
 #+liber-documentation
 "@version{#2021-11-1}
  @argument[assistant]{a @class{gtk:assistant} widget}
  @begin{short}
    Erases the visited page history so the Back button is not shown on the
    current page, and removes the Cancel button from subsequent pages.
  @end{short}
  Use this when the information provided up to the current page is hereafter
  deemed permanent and cannot be modified or undone. For example, showing a
  progress page to track a long running, unreversible operation after the user
  has clicked the Apply button on a confirmation page.
  @see-class{gtk:assistant}"
  (assistant (g:object assistant)))

(export 'assistant-commit)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_next_page
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_assistant_next_page" assistant-next-page) :void
 #+liber-documentation
 "@version{#2021-11-2}
  @argument[assistant]{a @class{gtk:assistant} widget}
  @begin{short}
    Navigate to the Next page.
  @end{short}
  It is a programming error to call this function when there is no Next page.
  This function is for use when creating pages with the @code{:custom} value of
  the @symbol{gtk:assistant-page-type} enumeration.
  @see-class{gtk:assistant}
  @see-symbol{gtk:assistant-page-type}
  @see-function{gtk:assistant-previous-page}"
  (assistant (g:object assistant)))

(export 'assistant-next-page)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_previous_page
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_assistant_previous_page" assistant-previous-page) :void
 #+liber-documentation
 "@version{#2021-11-2}
  @argument[assistant]{a @class{gtk:assistant} widget}
  @begin{short}
    Navigate to the previous visited page.
  @end{short}
  It is a programming error to call this function when no previous page is
  available. This function is for use when creating pages with the
  @code{:custom} value of the @symbol{gtk:assistant-page-type} enumeration.
  @see-class{gtk:assistant}
  @see-symbol{gtk:assistant-page-type}
  @see-function{gtk:assistant-next-page}"
  (assistant (g:object assistant)))

(export 'assistant-previous-page)

;;; --- End of file gtk4.assistant.lisp ----------------------------------------
