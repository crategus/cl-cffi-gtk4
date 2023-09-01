(in-package :gtk-test)

(def-suite gtk-file-chooser-widget :in gtk-suite)
(in-suite gtk-file-chooser-widget)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFileChooserWidget

(test gtk-file-chooser-widget-class
  ;; Type check
  (is (g:type-is-object "GtkFileChooserWidget"))
  ;; Check the registered name
  (is (eq 'gtk:file-chooser-widget
          (glib:symbol-for-gtype "GtkFileChooserWidget")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkFileChooserWidget")
          (g:gtype (cffi:foreign-funcall "gtk_file_chooser_widget_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkFileChooserWidget")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkFileChooserWidget")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkFileChooser")
             (list-interfaces "GtkFileChooserWidget")))
  ;; Check the properties
  (is (equal '("action" "create-folders" "filter" "filters" "search-mode"
               "select-multiple" "shortcut-folders" "show-time" "subtitle")
             (list-properties "GtkFileChooserWidget")))
  ;; Check the signals
  (is (equal '("desktop-folder" "down-folder" "home-folder" "location-popup"
               "location-popup-on-paste" "location-toggle-popup"
               "places-shortcut" "quick-bookmark" "recent-shortcut"
               "search-shortcut" "show-hidden" "up-folder")
             (list-signals "GtkFileChooserWidget")))
  ;; CSS information
  (is (string= "filechooser"
               (gtk:widget-class-css-name "GtkFileChooserWidget")))
  (is (string=
"filechooser:dir(ltr)
  box.vertical:dir(ltr)
    box.vertical:dir(ltr)
      paned.horizontal:dir(ltr)
        placessidebar.sidebar:dir(ltr)
          scrolledwindow:dir(ltr)
            viewport:dir(ltr)
              list.navigation-sidebar:dir(ltr)
                row.activatable.sidebar-row:dir(ltr)
                  revealer.sidebar-revealer:dir(ltr)
                    box.horizontal:dir(ltr)
                      image.sidebar-icon:dir(ltr)
                      label.sidebar-label:dir(ltr)
                      [image.sidebar-icon:dir(ltr)]
                      [button.image-button.sidebar-button:dir(ltr)]
                        image:dir(ltr)
                      [spinner:dir(ltr):checked]
                row.activatable.sidebar-row:dir(ltr)
                  revealer.sidebar-revealer:dir(ltr)
                    box.horizontal:dir(ltr)
                      image.sidebar-icon:dir(ltr)
                      label.sidebar-label:dir(ltr)
                      [image.sidebar-icon:dir(ltr)]
                      [button.image-button.sidebar-button:dir(ltr)]
                        image:dir(ltr)
                      [spinner:dir(ltr):checked]
                row.activatable.sidebar-row:dir(ltr)
                  revealer.sidebar-revealer:dir(ltr)
                    box.horizontal:dir(ltr)
                      image.sidebar-icon:dir(ltr)
                      label.sidebar-label:dir(ltr)
                      [image.sidebar-icon:dir(ltr)]
                      [button.image-button.sidebar-button:dir(ltr)]
                        image:dir(ltr)
                      [spinner:dir(ltr):checked]
                row.activatable.sidebar-row:dir(ltr)
                  revealer.sidebar-revealer:dir(ltr)
                    box.horizontal:dir(ltr)
                      image.sidebar-icon:dir(ltr)
                      label.sidebar-label:dir(ltr)
                      [image.sidebar-icon:dir(ltr)]
                      [button.image-button.sidebar-button:dir(ltr)]
                        image:dir(ltr)
                      [spinner:dir(ltr):checked]
                row.activatable.sidebar-row:dir(ltr)
                  revealer.sidebar-revealer:dir(ltr)
                    box.horizontal:dir(ltr)
                      image.sidebar-icon:dir(ltr)
                      label.sidebar-label:dir(ltr)
                      [image.sidebar-icon:dir(ltr)]
                      [button.image-button.sidebar-button:dir(ltr)]
                        image:dir(ltr)
                      [spinner:dir(ltr):checked]
                row.activatable.sidebar-row:dir(ltr)
                  revealer.sidebar-revealer:dir(ltr)
                    box.horizontal:dir(ltr)
                      image.sidebar-icon:dir(ltr)
                      label.sidebar-label:dir(ltr)
                      [image.sidebar-icon:dir(ltr)]
                      [button.image-button.sidebar-button:dir(ltr)]
                        image:dir(ltr)
                      [spinner:dir(ltr):checked]
                row.activatable.sidebar-row:dir(ltr)
                  revealer.sidebar-revealer:dir(ltr)
                    box.horizontal:dir(ltr)
                      image.sidebar-icon:dir(ltr)
                      label.sidebar-label:dir(ltr)
                      [image.sidebar-icon:dir(ltr)]
                      [button.image-button.sidebar-button:dir(ltr)]
                        image:dir(ltr)
                      [spinner:dir(ltr):checked]
                row.activatable.sidebar-row:dir(ltr)
                  revealer.sidebar-revealer:dir(ltr)
                    box.horizontal:dir(ltr)
                      image.sidebar-icon:dir(ltr)
                      label.sidebar-label:dir(ltr)
                      [image.sidebar-icon:dir(ltr)]
                      button.image-button.sidebar-button:dir(ltr)
                        image:dir(ltr)
                      [spinner:dir(ltr):checked]
                [row.activatable.sidebar-new-bookmark-row.sidebar-row:dir(ltr)]
                  revealer.sidebar-revealer:dir(ltr)
                    box.horizontal:dir(ltr)
                      image.sidebar-icon:dir(ltr)
                      label.sidebar-label:dir(ltr)
                      [image.sidebar-icon:dir(ltr)]
                      [button.image-button.sidebar-button:dir(ltr)]
                        image:dir(ltr)
                      [spinner:dir(ltr):checked]
                row.activatable.sidebar-row:dir(ltr)
                  revealer.sidebar-revealer:dir(ltr)
                    box.horizontal:dir(ltr)
                      image.sidebar-icon:dir(ltr)
                      label.sidebar-label:dir(ltr)
                      [image.sidebar-icon:dir(ltr)]
                      [button.image-button.sidebar-button:dir(ltr)]
                        image:dir(ltr)
                      [spinner:dir(ltr):checked]
                separator.horizontal:dir(ltr)
                separator.horizontal:dir(ltr)
            scrollbar.bottom.horizontal:dir(ltr)
              range.horizontal:dir(ltr)
                trough:dir(ltr)
                  slider:dir(ltr)
            scrollbar.right.vertical:dir(ltr)
              range.vertical:dir(ltr)
                trough:dir(ltr)
                  slider:dir(ltr)
            overshoot.left:dir(ltr)
            undershoot.left:dir(ltr)
            overshoot.right:dir(ltr)
            undershoot.right:dir(ltr)
            overshoot.top:dir(ltr)
            undershoot.top:dir(ltr)
            overshoot.bottom:dir(ltr)
            undershoot.bottom:dir(ltr)
            junction:dir(ltr)
        separator:dir(ltr)
        box.vertical:dir(ltr)
          revealer:dir(ltr)
            box#pathbarbox.vertical.view:dir(ltr)
              stack:dir(ltr)
                box.horizontal:dir(ltr)
                  pathbar:dir(ltr)
                    scrolledwindow:dir(ltr)
                      viewport:dir(ltr)
                        box.horizontal.linked:dir(ltr)
                      scrollbar.bottom.horizontal:dir(ltr)
                        range.horizontal:dir(ltr)
                          trough:dir(ltr)
                            slider:dir(ltr)
                      scrollbar.right.vertical:dir(ltr)
                        range.vertical:dir(ltr)
                          trough:dir(ltr)
                            slider:dir(ltr)
                      overshoot.left:dir(ltr)
                      undershoot.left:dir(ltr)
                      overshoot.right:dir(ltr)
                      undershoot.right:dir(ltr)
                      overshoot.top:dir(ltr)
                      undershoot.top:dir(ltr)
                      overshoot.bottom:dir(ltr)
                      undershoot.bottom:dir(ltr)
                      junction:dir(ltr)
                  button.image-button:disabled:dir(ltr)
                    image:disabled:dir(ltr)
                  [menubutton.popup:dir(ltr)]
                    button.image-button.toggle:dir(ltr)
                      box.horizontal:dir(ltr)
                        image:dir(ltr)
                        [arrow.down:dir(ltr)]
                    [popover.background:dir(ltr)]
                      contents:dir(ltr)
                        grid.horizontal:dir(ltr)
                          label:dir(ltr)
                          entry:dir(ltr)
                            text:dir(ltr)
                              undershoot.left:dir(ltr)
                              undershoot.right:dir(ltr)
                          button.default.suggested-action.text-button:disabled:dir(ltr)
                            label:disabled:dir(ltr)
                          widget:dir(ltr)
                            stack:dir(ltr)
                              label:dir(ltr)
                              label:dir(ltr)
                              label:dir(ltr)
                              label:dir(ltr)
                              label:dir(ltr)
                              label:dir(ltr)
                              label:dir(ltr)
                              label:dir(ltr)
                              label:dir(ltr)
                              label:dir(ltr)
                              label:dir(ltr)
                              label:dir(ltr)
                              label:dir(ltr)
                              label:dir(ltr)
                              label:dir(ltr)
                              label:dir(ltr)
                      arrow:dir(ltr)
                box.horizontal:dir(ltr)
                box:dir(ltr)
                  entry.search:dir(ltr)
                    image:dir(ltr)
                    text:dir(ltr)
                      undershoot.left:dir(ltr)
                      undershoot.right:dir(ltr)
                    image:dir(ltr)
                  box.horizontal:dir(ltr)
                    [spinner:dir(ltr)]
                    button.image-button:disabled:dir(ltr)
                      image:disabled:dir(ltr)
          box.horizontal:dir(ltr)
            stack.view:dir(ltr)
              box.vertical:dir(ltr)
                scrolledwindow:dir(ltr)
                  columnview.complex.view:dir(ltr)
                    header:dir(ltr)
                      button:dir(ltr)
                        box.horizontal:dir(ltr)
                          label:dir(ltr)
                          [sort-indicator:dir(ltr)]
                      [button:dir(ltr)]
                        box.horizontal:dir(ltr)
                          label:dir(ltr)
                          [sort-indicator:dir(ltr)]
                      button:dir(ltr)
                        box.horizontal:dir(ltr)
                          label:dir(ltr)
                          [sort-indicator:dir(ltr)]
                      button:dir(ltr)
                        box.horizontal:dir(ltr)
                          label:dir(ltr)
                          [sort-indicator:dir(ltr)]
                      button:dir(ltr)
                        box.horizontal:dir(ltr)
                          label:dir(ltr)
                          [sort-indicator:dir(ltr)]
                    listview.view:dir(ltr)
                  scrollbar.bottom.horizontal:dir(ltr)
                    range.horizontal:dir(ltr)
                      trough:dir(ltr)
                        slider:dir(ltr)
                  scrollbar.right.vertical:dir(ltr)
                    range.vertical:dir(ltr)
                      trough:dir(ltr)
                        slider:dir(ltr)
                  overshoot.left:dir(ltr)
                  undershoot.left:dir(ltr)
                  overshoot.right:dir(ltr)
                  undershoot.right:dir(ltr)
                  overshoot.top:dir(ltr)
                  undershoot.top:dir(ltr)
                  overshoot.bottom:dir(ltr)
                  undershoot.bottom:dir(ltr)
                  junction:dir(ltr)
                [actionbar:dir(ltr)]
                  revealer:dir(ltr)
                    box:dir(ltr)
                      box.horizontal.start:dir(ltr)
                      label:dir(ltr)
                      box.end.horizontal:dir(ltr)
              placesview.vertical:dir(ltr)
                stack:dir(ltr)
                  scrolledwindow:dir(ltr)
                    viewport:dir(ltr)
                      list:dir(ltr)
                        row.activatable:dir(ltr)
                          box.horizontal:dir(ltr)
                            image:dir(ltr)
                            label:dir(ltr)
                            label.dim-label:dir(ltr)
                            label.dim-label:dir(ltr)
                            stack:dir(ltr)
                              [button.image-button.sidebar-button:dir(ltr)]
                                image:dir(ltr)
                              spinner:dir(ltr)
                        row.activatable:disabled:dir(ltr)
                          label:disabled:dir(ltr)
                        box.vertical:dir(ltr)
                          label:dir(ltr)
                          separator.horizontal:dir(ltr)
                        box.vertical:dir(ltr)
                          box.horizontal:dir(ltr)
                            label:dir(ltr)
                            spinner:dir(ltr):checked
                          separator.horizontal:dir(ltr)
                    scrollbar.bottom.horizontal:dir(ltr)
                      range.horizontal:dir(ltr)
                        trough:dir(ltr)
                          slider:dir(ltr)
                    scrollbar.right.vertical:dir(ltr)
                      range.vertical:dir(ltr)
                        trough:dir(ltr)
                          slider:dir(ltr)
                    overshoot.left:dir(ltr)
                    undershoot.left:dir(ltr)
                    overshoot.right:dir(ltr)
                    undershoot.right:dir(ltr)
                    overshoot.top:dir(ltr)
                    undershoot.top:dir(ltr)
                    overshoot.bottom:dir(ltr)
                    undershoot.bottom:dir(ltr)
                    junction:dir(ltr)
                  box.vertical:dir(ltr)
                    image.dim-label:dir(ltr)
                    label:dir(ltr)
                    label.dim-label:dir(ltr)
                actionbar.background:dir(ltr)
                  revealer:dir(ltr)
                    box:dir(ltr)
                      box.horizontal.start:dir(ltr)
                        label:dir(ltr)
                      box.end.horizontal:dir(ltr)
                        box.horizontal.linked:dir(ltr)
                          entry:dir(ltr)
                            text:dir(ltr)
                              placeholder:dir(ltr)
                              undershoot.left:dir(ltr)
                              undershoot.right:dir(ltr)
                            image.right:dir(ltr)
                          menubutton.popup.server-list-button:dir(ltr)
                            button.image-button.toggle:dir(ltr)
                              box.horizontal:dir(ltr)
                                image:dir(ltr)
                                [arrow.up:dir(ltr)]
                            [popover.background:dir(ltr)]
                              contents:dir(ltr)
                                stack:dir(ltr)
                                  box.vertical:dir(ltr)
                                    image.dim-label:dir(ltr)
                                    label.dim-label:dir(ltr)
                                  box.vertical:dir(ltr)
                                    label:dir(ltr)
                                    scrolledwindow.frame:dir(ltr)
                                      viewport:dir(ltr)
                                        list:dir(ltr)
                                      scrollbar.bottom.horizontal:dir(ltr)
                                        range.horizontal:dir(ltr)
                                          trough:dir(ltr)
                                            slider:dir(ltr)
                                      scrollbar.right.vertical:dir(ltr)
                                        range.vertical:dir(ltr)
                                          trough:dir(ltr)
                                            slider:dir(ltr)
                                      overshoot.left:dir(ltr)
                                      undershoot.left:dir(ltr)
                                      overshoot.right:dir(ltr)
                                      undershoot.right:dir(ltr)
                                      overshoot.top:dir(ltr)
                                      undershoot.top:dir(ltr)
                                      overshoot.bottom:dir(ltr)
                                      undershoot.bottom:dir(ltr)
                                      junction:dir(ltr)
                              arrow:dir(ltr)
                        button.text-button:disabled:dir(ltr)
                          label:disabled:dir(ltr)
                [popover.background:dir(ltr)]
                  contents:dir(ltr)
                    box.vertical:dir(ltr)
                      label.dim-label:dir(ltr)
                      label:dir(ltr)
                      label:dir(ltr)
                      grid.horizontal:dir(ltr)
                        label:dir(ltr)
                        label:dir(ltr)
                        label:dir(ltr)
                        label:dir(ltr)
                        label:dir(ltr)
                        label:dir(ltr)
                        label:dir(ltr)
                        label:dir(ltr)
                        label:dir(ltr)
                        label:dir(ltr)
                        label:dir(ltr)
                        label:dir(ltr)
                        label:dir(ltr)
                        label:dir(ltr)
                  arrow:dir(ltr)
              grid.dim-label.horizontal:dir(ltr)
                image.dim-label:dir(ltr)
                label:dir(ltr)
                label.dim-label:dir(ltr)
    [actionbar:dir(ltr)]
      revealer:dir(ltr)
        box:dir(ltr)
          box.horizontal.start:dir(ltr)
            box.horizontal:dir(ltr)
          box.end.horizontal:dir(ltr)
            box.horizontal:dir(ltr)
              dropdown:dir(ltr)
                button.toggle:dir(ltr)
                  box.horizontal:dir(ltr)
                    stack:dir(ltr)
                      label:dir(ltr)
                      row:dir(ltr)
                    arrow:dir(ltr)
                [popover.background.menu:dir(ltr)]
                  contents:dir(ltr)
                    box.vertical:dir(ltr)
                      [box.dropdown-searchbar.horizontal:dir(ltr)]
                        entry.search:dir(ltr)
                          image:dir(ltr)
                          text:dir(ltr)
                            placeholder:dir(ltr)
                            undershoot.left:dir(ltr)
                            undershoot.right:dir(ltr)
                          image:dir(ltr)
                      scrolledwindow:dir(ltr)
                        listview.view:dir(ltr)
                        scrollbar.bottom.horizontal:dir(ltr)
                          range.horizontal:dir(ltr)
                            trough:dir(ltr)
                              slider:dir(ltr)
                        scrollbar.right.vertical:dir(ltr)
                          range.vertical:dir(ltr)
                            trough:dir(ltr)
                              slider:dir(ltr)
                        overshoot.left:dir(ltr)
                        undershoot.left:dir(ltr)
                        overshoot.right:dir(ltr)
                        undershoot.right:dir(ltr)
                        overshoot.top:dir(ltr)
                        undershoot.top:dir(ltr)
                        overshoot.bottom:dir(ltr)
                        undershoot.bottom:dir(ltr)
                        junction:dir(ltr)
                  arrow:dir(ltr)
  [popover.background:dir(ltr)]
    contents:dir(ltr)
      grid.horizontal:dir(ltr)
        label:dir(ltr)
        entry:dir(ltr)
          text:dir(ltr)
            undershoot.left:dir(ltr)
            undershoot.right:dir(ltr)
        button.default.suggested-action.text-button:disabled:dir(ltr)
          label:disabled:dir(ltr)
        widget:dir(ltr)
          stack:dir(ltr)
            label:dir(ltr)
            label:dir(ltr)
            label:dir(ltr)
            label:dir(ltr)
            label:dir(ltr)
            label:dir(ltr)
            label:dir(ltr)
            label:dir(ltr)
            label:dir(ltr)
            label:dir(ltr)
            label:dir(ltr)
            label:dir(ltr)
            label:dir(ltr)
            label:dir(ltr)
            label:dir(ltr)
            label:dir(ltr)
    arrow:dir(ltr)
"
               (print-style-context "GtkFileChooserWidget")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkFileChooserWidget"
                                     GTK-FILE-CHOOSER-WIDGET
                       (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                         "GtkFileChooser")
                        :TYPE-INITIALIZER "gtk_file_chooser_widget_get_type")
                       ((SEARCH-MODE GTK-FILE-CHOOSER-WIDGET-SEARCH-MODE
                         "search-mode" "gboolean" T T)
                        (SHOW-TIME GTK-FILE-CHOOSER-WIDGET-SHOW-TIME
                         "show-time" "gboolean" T NIL)
                        (SUBTITLE GTK-FILE-CHOOSER-WIDGET-SUBTITLE "subtitle"
                         "gchararray" T NIL)))
             (gobject:get-g-type-definition "GtkFileChooserWidget"))))

;;; --- Properties -------------------------------------------------------------

;;;     search-mode
;;;     show-time                                          Since 4.10
;;;     subtitle

;;; --- Signals ----------------------------------------------------------------

;;;     desktop-folder
;;;     down-folder
;;;     home-folder
;;;     location-popup
;;;     location-popup-on-paste
;;;     location-toggle-popup
;;;     places-shortcut
;;;     quick-bookmark
;;;     recent-shortcut
;;;     search-shortcut
;;;     show-hidden
;;;     up-folder

;;; --- Functions --------------------------------------------------------------

;;;     gtk_file_chooser_widget_new

;;; --- 2023-5-29 --------------------------------------------------------------
