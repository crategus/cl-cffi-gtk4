(in-package :gtk-test)

(def-suite gtk-assistant :in gtk-suite)
(in-suite gtk-assistant)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkAssistantPageType

(test gtk-assistant-page-type-enumeration
  ;; Check the type
  (is (g:type-is-enum "GtkAssistantPageType"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkAssistantPageType")
          (g:gtype (cffi:foreign-funcall "gtk_assistant_page_type_get_type"
                                         :size))))
  ;; Check the registered name
  (is (eq 'gtk:assistant-page-type
          (glib:symbol-for-gtype "GtkAssistantPageType")))
  ;; Check the names
  (is (equal '("GTK_ASSISTANT_PAGE_CONTENT" "GTK_ASSISTANT_PAGE_INTRO"
               "GTK_ASSISTANT_PAGE_CONFIRM" "GTK_ASSISTANT_PAGE_SUMMARY"
               "GTK_ASSISTANT_PAGE_PROGRESS" "GTK_ASSISTANT_PAGE_CUSTOM")
             (list-enum-item-name "GtkAssistantPageType")))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5)
             (list-enum-item-value "GtkAssistantPageType")))
  ;; Check the nick names
  (is (equal '("content" "intro" "confirm" "summary" "progress" "custom")
             (list-enum-item-nick "GtkAssistantPageType")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkAssistantPageType"
                             GTK-ASSISTANT-PAGE-TYPE
                             (:EXPORT T
                              :TYPE-INITIALIZER
                              "gtk_assistant_page_type_get_type")
                             (:CONTENT 0)
                             (:INTRO 1)
                             (:CONFIRM 2)
                             (:SUMMARY 3)
                             (:PROGRESS 4)
                             (:CUSTOM 5))
             (gobject:get-g-type-definition "GtkAssistantPageType"))))

;;;     GtkAssistantPage

(test gtk-assistant-page-class
  ;; Type check
  (is (g:type-is-object "GtkAssistantPage"))
  ;; Check the registered name
  (is (eq 'gtk:assistant-page
          (glib:symbol-for-gtype "GtkAssistantPage")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkAssistantPage")
          (g:gtype (cffi:foreign-funcall "gtk_assistant_page_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkAssistantPage")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkAssistantPage")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkAssistantPage")))
  ;; Check the class properties
  (is (equal '("child" "complete" "page-type" "title")
             (list-properties "GtkAssistantPage")))
  ;; Check the list of signals
  (is (equal '()
             (list-signals "GtkAssistantPage")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkAssistantPage"
                                             GTK-ASSISTANT-PAGE
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_assistant_page_get_type")
                       ((CHILD GTK-ASSISTANT-PAGE-CHILD "child" "GtkWidget" T
                         NIL)
                        (COMPLETE GTK-ASSISTANT-PAGE-COMPLETE "complete"
                         "gboolean" T T)
                        (PAGE-TYPE GTK-ASSISTANT-PAGE-PAGE-TYPE "page-type"
                         "GtkAssistantPageType" T T)
                        (TITLE GTK-ASSISTANT-PAGE-TITLE "title" "gchararray" T
                         T)))
             (gobject:get-g-type-definition "GtkAssistantPage"))))

;;; --- Properties -------------------------------------------------------------

;;;     child
;;;     complete
;;;     page-type
;;;     title

(test gtk-assistant-page-properties
  (let ((*gtk-warn-deprecated* nil))
    (let ((page (make-instance 'gtk:assistant-page)))
      (is-false (gtk:assistant-page-child page)))))

;;;     GtkAssistant

(test gtk-assistant-class
  (let ((*gtk-warn-deprecated* nil))
    ;; Type check
    (is (g:type-is-object "GtkAssistant"))
    ;; Check the registered name
    (is (eq 'gtk:assistant
            (glib:symbol-for-gtype "GtkAssistant")))
    ;; Check the type initializer
    (is (eq (g:gtype "GtkAssistant")
            (g:gtype (cffi:foreign-funcall "gtk_assistant_get_type" :size))))
    ;; Check the parent
    (is (eq (g:gtype "GtkWindow")
            (g:type-parent "GtkAssistant")))
    ;; Check the children
    (is (equal '()
               (list-children "GtkAssistant")))
    ;; Check the interfaces
    (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                 "GtkNative" "GtkShortcutManager" "GtkRoot")
               (list-interfaces "GtkAssistant")))
    ;; Check the class properties
    (is (equal '("pages" "use-header-bar")
               (list-properties "GtkAssistant")))
    ;; Check the list of signals
    (is (equal '("apply" "cancel" "close" "escape" "prepare")
               (list-signals "GtkAssistant")))
    ;; CSS name
    (is (string= "window"
                 (gtk:widget-class-css-name "GtkAssistant")))
    ;; CSS classes
    #-windows
    (is (equal '("background" "assistant" "csd")
               (gtk:widget-css-classes (make-instance 'gtk:assistant))))
    #+windows
    (is (equal '("background" "assistant")
               (gtk:widget-css-classes (make-instance 'gtk:assistant))))
    ;; Accessible role
    (is (eq :window (gtk:widget-class-accessible-role "GtkAssistant")))
    ;; Check the class definition
    (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkAssistant" GTK-ASSISTANT
                         (:SUPERCLASS GTK-WINDOW :EXPORT T :INTERFACES
                          ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                           "GtkNative" "GtkRoot" "GtkShortcutManager")
                          :TYPE-INITIALIZER "gtk_assistant_get_type")
                         ((PAGES GTK-ASSISTANT-PAGES "pages" "GListModel" T NIL)
                          (USE-HEADER-BAR GTK-ASSISTANT-USE-HEADER-BAR
                           "use-header-bar" "gint" T NIL)))
               (gobject:get-g-type-definition "GtkAssistant")))))

;;; --- Properties -------------------------------------------------------------

;;;     pages
;;;     use-header-bar

(test gtk-assistant-properties
  (let ((*gtk-warn-deprecated* nil))
    (let ((assistant (make-instance 'gtk:assistant)))
      (is (typep (gtk:assistant-pages assistant) 'g:object))
      #-windows
      (is (= 1 (gtk:assistant-use-header-bar assistant))))))

;;; --- Signals ----------------------------------------------------------------

;;;     apply
;;;     cancel
;;;     close
;;;     escape
;;;     prepare

;;; --- Functions --------------------------------------------------------------

;;;     gtk_assistant_new

(test gtk-assistant-new
  (let ((*gtk-warn-deprecated* nil))
    (is (typep (gtk:assistant-new) 'gtk:assistant))))

;;;     gtk_assistant_get_page
;;;     gtk_assistant_get_current_page
;;;     gtk_assistant_set_current_page
;;;     gtk_assistant_get_n_pages
;;;     gtk_assistant_get_nth_page
;;;     gtk_assistant_prepend_page
;;;     gtk_assistant_append_page
;;;     gtk_assistant_insert_page
;;;     gtk_assistant_remove_page

;;;     GtkAssistantPageFunc

;;;     gtk_assistant_set_forward_page_func
;;;     gtk_assistant_add_action_widget
;;;     gtk_assistant_remove_action_widget
;;;     gtk_assistant_update_buttons_state
;;;     gtk_assistant_commit
;;;     gtk_assistant_next_page
;;;     gtk_assistant_previous_page

;;; --- 2023-11-1 --------------------------------------------------------------
