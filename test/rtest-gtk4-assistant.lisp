(in-package :gtk-test)

(def-suite gtk-assistant :in gtk-deprecated)
(in-suite gtk-assistant)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkAssistantPageType

(test gtk-assistant-page-type-enumeration
  ;; Check type
  (is (g:type-is-enum "GtkAssistantPageType"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkAssistantPageType")
          (g:gtype (cffi:foreign-funcall "gtk_assistant_page_type_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gtk:assistant-page-type
          (glib:symbol-for-gtype "GtkAssistantPageType")))
  ;; Check names
  (is (equal '("GTK_ASSISTANT_PAGE_CONTENT" "GTK_ASSISTANT_PAGE_INTRO"
               "GTK_ASSISTANT_PAGE_CONFIRM" "GTK_ASSISTANT_PAGE_SUMMARY"
               "GTK_ASSISTANT_PAGE_PROGRESS" "GTK_ASSISTANT_PAGE_CUSTOM")
             (glib-test:list-enum-item-names "GtkAssistantPageType")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5)
             (glib-test:list-enum-item-values "GtkAssistantPageType")))
  ;; Check nick names
  (is (equal '("content" "intro" "confirm" "summary" "progress" "custom")
             (glib-test:list-enum-item-nicks "GtkAssistantPageType")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkAssistantPageType"
                                    GTK:ASSISTANT-PAGE-TYPE
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_assistant_page_type_get_type")
                       (:CONTENT 0)
                       (:INTRO 1)
                       (:CONFIRM 2)
                       (:SUMMARY 3)
                       (:PROGRESS 4)
                       (:CUSTOM 5))
             (gobject:get-gtype-definition "GtkAssistantPageType"))))

;;;     GtkAssistantPage

(test gtk-assistant-page-class
  ;; Check type
  (is (g:type-is-object "GtkAssistantPage"))
  ;; Check registered name
  (is (eq 'gtk:assistant-page
          (glib:symbol-for-gtype "GtkAssistantPage")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkAssistantPage")
          (g:gtype (cffi:foreign-funcall "gtk_assistant_page_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkAssistantPage")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkAssistantPage")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkAssistantPage")))
  ;; Check class properties
  (is (equal '("child" "complete" "page-type" "title")
             (glib-test:list-properties "GtkAssistantPage")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkAssistantPage")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkAssistantPage" GTK:ASSISTANT-PAGE
                      (:SUPERCLASS G:OBJECT
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gtk_assistant_page_get_type")
                      ((CHILD ASSISTANT-PAGE-CHILD "child" "GtkWidget" T NIL)
                       (COMPLETE ASSISTANT-PAGE-COMPLETE
                        "complete" "gboolean" T T)
                       (PAGE-TYPE ASSISTANT-PAGE-PAGE-TYPE
                        "page-type" "GtkAssistantPageType" T T)
                       (TITLE ASSISTANT-PAGE-TITLE "title" "gchararray" T T)))
             (gobject:get-gtype-definition "GtkAssistantPage"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-assistant-page-properties
  (let ((gtk-init:*warn-deprecated* nil))
    (glib-test:with-check-memory (page)
      (setf page (make-instance 'gtk:assistant-page))
      (is-false (gtk:assistant-page-child page)))))

;;;     GtkAssistant

(test gtk-assistant-class
  (let ((gtk-init:*warn-deprecated* nil))
    ;; Check type
    (is (g:type-is-object "GtkAssistant"))
    ;; Check registered name
    (is (eq 'gtk:assistant
            (glib:symbol-for-gtype "GtkAssistant")))
    ;; Check type initializer
    (is (eq (g:gtype "GtkAssistant")
            (g:gtype (cffi:foreign-funcall "gtk_assistant_get_type" :size))))
    ;; Check parent
    (is (eq (g:gtype "GtkWindow")
            (g:type-parent "GtkAssistant")))
    ;; Check children
    (is (equal '()
               (glib-test:list-children "GtkAssistant")))
    ;; Check interfaces
    (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                 "GtkNative" "GtkShortcutManager" "GtkRoot")
               (glib-test:list-interfaces "GtkAssistant")))
    ;; Check class properties
    (is (equal '("pages" "use-header-bar")
               (glib-test:list-properties "GtkAssistant")))
    ;; Check signals
    (is (equal '("apply" "cancel" "close" "escape" "prepare")
               (glib-test:list-signals "GtkAssistant")))
    ;; Check CSS name
    (is (string= "window"
                 (gtk:widget-class-css-name "GtkAssistant")))
    ;; Check accessible role
    #-gtk-4-18
    (is (eq :application (gtk:widget-class-accessible-role "GtkAssistant")))
    #+gtk-4-18
    (is (eq :window (gtk:widget-class-accessible-role "GtkAssistant")))
    ;; Check class definition
    (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkAssistant" GTK:ASSISTANT
                        (:SUPERCLASS GTK:WINDOW
                         :EXPORT T
                         :INTERFACES
                         ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                          "GtkNative" "GtkRoot" "GtkShortcutManager")
                         :TYPE-INITIALIZER "gtk_assistant_get_type")
                        ((PAGES ASSISTANT-PAGES "pages" "GListModel" T NIL)
                         (USE-HEADER-BAR ASSISTANT-USE-HEADER-BAR
                          "use-header-bar" "gint" T NIL)))
               (gobject:get-gtype-definition "GtkAssistant")))))

;;; --- Signals ----------------------------------------------------------------

;;;     apply
;;;     cancel
;;;     close
;;;     escape
;;;     prepare

;;; --- Properties -------------------------------------------------------------

(test gtk-assistant-properties
  (when *first-run-testsuite*
    (let ((gtk-init:*warn-deprecated* nil))
      (glib-test:with-check-memory (assistant :strong 1)
        (setf assistant (make-instance 'gtk:assistant))
        (is (typep (gtk:assistant-pages assistant) 'g:object))
        #-windows
        (is (= 1 (gtk:assistant-use-header-bar assistant)))
        (is-false (gtk:window-destroy assistant))))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_assistant_new

(test gtk-assistant-new
  (let ((gtk-init:*warn-deprecated* nil))
    (glib-test:with-check-memory (assistant)
      (is (typep (setf assistant (gtk:assistant-new)) 'gtk:assistant))
      (is-false (gtk:window-destroy assistant)))))

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

;;; 2024-12-24
