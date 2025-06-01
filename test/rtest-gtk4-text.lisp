(in-package :gtk-test)

(def-suite gtk-text :in gtk-data-entry)
(in-suite gtk-text)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkText

(test gtk-text-class
  ;; Check type
  (is (g:type-is-object "GtkText"))
  ;; Check registered name
  (is (eq 'gtk:text
          (glib:symbol-for-gtype "GtkText")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkText")
          (g:gtype (cffi:foreign-funcall "gtk_text_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkText")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkText")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkEditable" "GtkAccessibleText")
             (glib-test:list-interfaces "GtkText")))
  ;; Check class properties
  (is (equal '("activates-default" "attributes" "buffer" "cursor-position"
               "editable" "enable-emoji-completion" "enable-undo" "extra-menu"
               "im-module" "input-hints" "input-purpose" "invisible-char"
               "invisible-char-set" "max-length" "max-width-chars"
               "overwrite-mode" "placeholder-text" "propagate-text-width"
               "scroll-offset" "selection-bound" "tabs" "text"
               "truncate-multiline" "visibility" "width-chars" "xalign")
             (glib-test:list-properties "GtkText")))
  ;; Check signals
  (is (equal '("activate" "backspace" "copy-clipboard" "cut-clipboard"
               "delete-from-cursor" "insert-at-cursor" "insert-emoji"
               "move-cursor" "paste-clipboard" "preedit-changed"
               "toggle-overwrite")
             (glib-test:list-signals "GtkText")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkText" GTK:TEXT
                      (:SUPERCLASS GTK:WIDGET
                       :EXPORT T
                       :INTERFACES
                       ("GtkAccessible" "GtkAccessibleText" "GtkBuildable"
                        "GtkConstraintTarget" "GtkEditable")
                       :TYPE-INITIALIZER "gtk_text_get_type")
                      ((ACTIVATES-DEFAULT TEXT-ACTIVATES-DEFAULT
                        "activates-default" "gboolean" T T)
                       (ATTRIBUTES TEXT-ATTRIBUTES
                        "attributes" "PangoAttrList" T T)
                       (BUFFER TEXT-BUFFER "buffer" "GtkEntryBuffer" T T)
                       (ENABLE-EMOJI-COMPLETION TEXT-ENABLE-EMOJI-COMPLETION
                        "enable-emoji-completion" "gboolean" T T)
                       (EXTRA-MENU TEXT-EXTRA-MENU
                        "extra-menu" "GMenuModel" T T)
                       (IM-MODULE TEXT-IM-MODULE "im-module" "gchararray" T T)
                       (INPUT-HINTS TEXT-INPUT-HINTS
                        "input-hints" "GtkInputHints" T T)
                       (INPUT-PURPOSE TEXT-INPUT-PURPOSE
                        "input-purpose" "GtkInputPurpose" T T)
                       (INVISIBLE-CHAR TEXT-INVISIBLE-CHAR
                        "invisible-char" "guint" T T)
                       (INVISIBLE-CHAR-SET TEXT-INVISIBLE-CHAR-SET
                        "invisible-char-set" "gboolean" T T)
                       (MAX-LENGTH TEXT-MAX-LENGTH "max-length" "gint" T T)
                       (OVERWRITE-MODE TEXT-OVERWRITE-MODE
                        "overwrite-mode" "gboolean" T T)
                       (PLACEHOLDER-TEXT TEXT-PLACEHOLDER-TEXT
                        "placeholder-text" "gchararray" T T)
                       (PROPAGATE-TEXT-WIDTH TEXT-PROPAGATE-TEXT-WIDTH
                        "propagate-text-width" "gboolean" T T)
                       (SCROLL-OFFSET TEXT-SCROLL-OFFSET
                        "scroll-offset" "gint" T NIL)
                       (TABS TEXT-TABS "tabs" "PangoTabArray" T T)
                       (TRUNCATE-MULTILINE TEXT-TRUNCATE-MULTILINE
                        "truncate-multiline" "gboolean" T T)
                       (VISIBILITY TEXT-VISIBILITY
                        "visibility" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkText"))))

;;; --- Signals ----------------------------------------------------------------

;;;     activate

(test gtk-text-activate-signal
  (let* ((name "activate") (gtype "GtkText")
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (string= gtype (g:type-name (g:signal-query-owner-type query))))
    ;; Check flags
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     backspace

(test gtk-text-backspace-signal
  (let* ((name "backspace") (gtype "GtkText")
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (string= gtype (g:type-name (g:signal-query-owner-type query))))
    ;; Check flags
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     copy-clipboard

(test gtk-text-copy-clipboard-signal
  (let* ((name "copy-clipboard") (gtype "GtkText")
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (string= gtype (g:type-name (g:signal-query-owner-type query))))
    ;; Check flags
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     cut-clipboard

(test gtk-text-cut-clipboard-signal
  (let* ((name "cut-clipboard") (gtype "GtkText")
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (string= gtype (g:type-name (g:signal-query-owner-type query))))
    ;; Check flags
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     delete-from-cursor

(test gtk-text-delete-from-cursor-signal
  (let* ((name "delete-from-cursor") (gtype "GtkText")
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (string= gtype (g:type-name (g:signal-query-owner-type query))))
    ;; Check flags
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '("GtkDeleteType" "gint")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     insert-at-cursor

(test gtk-text-insert-at-cursor-signal
  (let* ((name "insert-at-cursor") (gtype "GtkText")
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (string= gtype (g:type-name (g:signal-query-owner-type query))))
    ;; Check flags
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '("gchararray")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     insert-emoji

(test gtk-text-insert-emoji-signal
  (let* ((name "insert-emoji") (gtype "GtkText")
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (string= gtype (g:type-name (g:signal-query-owner-type query))))
    ;; Check flags
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     move-cursor

(test gtk-text-move-cursor-signal
  (let* ((name "move-cursor") (gtype "GtkText")
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (string= gtype (g:type-name (g:signal-query-owner-type query))))
    ;; Check flags
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '("GtkMovementStep" "gint" "gboolean")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     paste-clipboard

(test gtk-text-paste-clipboard-signal
  (let* ((name "paste-clipboard") (gtype "GtkText")
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (string= gtype (g:type-name (g:signal-query-owner-type query))))
    ;; Check flags
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     preedit-changed

(test gtk-text-preedit-changed-signal
  (let* ((name "preedit-changed") (gtype "GtkText")
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (string= gtype (g:type-name (g:signal-query-owner-type query))))
    ;; Check flags
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '("gchararray")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     toggle-overwrite

(test gtk-text-toggle-overwrite-signal
  (let* ((name "toggle-overwrite") (gtype "GtkText")
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (string= gtype (g:type-name (g:signal-query-owner-type query))))
    ;; Check flags
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Properties -------------------------------------------------------------

(test gtk-text-properties
  (glib-test:with-check-memory (text)
    (is (typep (setf text (make-instance 'gtk:text)) 'gtk:text))
    (is-false (gtk:text-activates-default text))
    (is-false (gtk:text-attributes text))
    (is (typep (gtk:text-buffer text) 'gtk:entry-buffer))
    (is-false (gtk:text-enable-emoji-completion text))
    (is-false (gtk:text-extra-menu text))
    (is-false (gtk:text-im-module text))
    (is-false (gtk:text-input-hints text))
    (is (eq :free-form (gtk:text-input-purpose text)))
    (is (= 0 (gtk:text-invisible-char text)))
    (is-false (gtk:text-invisible-char-set text))
    (is (= 0 (gtk:text-max-length text)))
    (is-false (gtk:text-overwrite-mode text))
    (is-false (gtk:text-placeholder-text text))
    (is-false (gtk:text-propagate-text-width text))
    (is (= 0 (gtk:text-scroll-offset text)))
    (is-false (gtk:text-tabs text))
    (is-false (gtk:text-truncate-multiline text))
    (is-true (gtk:text-visibility text))
    ;; Remove text buffer and check the refcount
    (is-false (setf (gtk:text-buffer text) nil))
    (is (= 1 (g:object-ref-count text)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_text_new

(test gtk-text-new
  (glib-test:with-check-memory (text)
    (is (typep (setf text (gtk:text-new)) 'gtk:text))
    (is (typep (gtk:text-buffer text) 'gtk:entry-buffer))
    (is-false (setf (gtk:text-buffer text) nil))))

;;;     gtk_text_new_with_buffer

(test gtk-text-new-with-buffer
  (glib-test:with-check-memory (buffer text)
    (is (typep (setf buffer (gtk:entry-buffer-new)) 'gtk:entry-buffer))
    (is (typep (setf text (gtk:text-new-with-buffer buffer)) 'gtk:text))
    (is (typep (gtk:text-buffer text) 'gtk:entry-buffer))
    (is (eq buffer (gtk:text-buffer text)))
    ;; Remove references
    (is-false (setf (gtk:text-buffer text) nil))))

;;;     gtk_text_unset_invisible_char

(test gtk-text-unset-invisible-char
  (glib-test:with-check-memory (text)
    (is (typep (setf text (gtk:text-new)) 'gtk:text))
    (is (= 0 (gtk:text-invisible-char text)))
    (is-false (gtk:text-invisible-char-set text))
    (is (= 43 (setf (gtk:text-invisible-char text) (char-code #\+))))
    (is (= 43 (gtk:text-invisible-char text)))
    (is-true (gtk:text-invisible-char-set text))
    (is-false (gtk:text-unset-invisible-char text))
    ;; Default char is #\BULLET and not \*
    #+crategus
    (is (= (char-code #\BULLET) (gtk:text-invisible-char text)))
    #+windows
    (is (= (char-code #\BLACK_CIRCLE) (gtk:text-invisible-char text)))
    (is-false (gtk:text-invisible-char-set text))))

;;;     gtk_text_get_text_length

(test gtk-text-text-length
  (glib-test:with-check-memory (buffer text)
    (is (typep (setf buffer (gtk:entry-buffer-new "Ägypen")) 'gtk:entry-buffer))
    (is (typep (setf text (gtk:text-new-with-buffer buffer)) 'gtk:text))
    (is (= 6 (gtk:text-text-length text)))
    (is (= (gtk:entry-buffer-length (gtk:text-buffer text))
           (gtk:text-text-length text)))
    (is (= (gtk:entry-buffer-length buffer) (gtk:text-text-length text)))
    (is-false (setf (gtk:text-buffer text) nil))))

;;;     gtk_text_grab_focus_without_selecting

;;;     gtk_text_compute_cursor_extents

#+crategus
(test gtk-text-compute-cursor-extents.1
  (glib-test:with-check-memory (buffer text)
    (setf buffer (gtk:entry-buffer-new "Ägypten"))
    (setf  text (gtk:text-new-with-buffer buffer))
    (graphene:with-rects (strong weak)
      (is-false (gtk:text-compute-cursor-extents text 2 strong weak))
      ;; Check strong values
      (is (= 17.0 (graphene:rect-x strong)))
      (is (= -8.0 (graphene:rect-y strong)))
      (is (=  0.0 (graphene:rect-width strong)))
      (is (= 16.0 (graphene:rect-height strong)))
      ;; Check weak values
      (is (= 17.0 (graphene:rect-x weak)))
      (is (= -8.0 (graphene:rect-y weak)))
      (is (=  0.0 (graphene:rect-width weak)))
      (is (= 16.0 (graphene:rect-height weak))))
    (is-false (setf (gtk:text-buffer text) nil))))

#+windows
(test gtk-text-compute-cursor-extents.1
  (glib-test:with-check-memory (buffer text)
    (setf buffer (gtk:entry-buffer-new "Ägypten"))
    (setf  text (gtk:text-new-with-buffer buffer))
    (graphene:with-rects (strong weak)
      (is-false (gtk:text-compute-cursor-extents text 2 strong weak))
      ;; Check strong values
      (is (= 15.0 (graphene:rect-x strong)))
      (is (= -8.0 (graphene:rect-y strong)))
      (is (=  0.0 (graphene:rect-width strong)))
      (is (= 17.0 (graphene:rect-height strong)))
      ;; Check weak values
      (is (= 15.0 (graphene:rect-x weak)))
      (is (= -8.0 (graphene:rect-y weak)))
      (is (=  0.0 (graphene:rect-width weak)))
      (is (= 17.0 (graphene:rect-height weak))))
    (is-false (setf (gtk:text-buffer text) nil))))

#+crategus
(test gtk-text-compute-cursor-extents.2
  (glib-test:with-check-memory (buffer text)
    (setf buffer (gtk:entry-buffer-new "Ägypten"))
    (setf text (gtk:text-new-with-buffer buffer))
    (graphene:with-rect (strong)
      (is-false (gtk:text-compute-cursor-extents text 2 strong nil))
      (is (= 17.0 (graphene:rect-x strong)))
      (is (= -8.0 (graphene:rect-y strong)))
      (is (=  0.0 (graphene:rect-width strong)))
      (is (= 16.0 (graphene:rect-height strong))))
    (is-false (setf (gtk:text-buffer text) nil))))

#+windows
(test gtk-text-compute-cursor-extents.2
  (glib-test:with-check-memory (buffer text)
    (setf buffer (gtk:entry-buffer-new "Ägypten"))
    (setf text (gtk:text-new-with-buffer buffer))
    (graphene:with-rect (strong)
      (is-false (gtk:text-compute-cursor-extents text 2 strong nil))
      (is (= 15.0 (graphene:rect-x strong)))
      (is (= -8.0 (graphene:rect-y strong)))
      (is (=  0.0 (graphene:rect-width strong)))
      (is (= 17.0 (graphene:rect-height strong))))
    (is-false (setf (gtk:text-buffer text) nil))))

#+crategus
(test gtk-text-compute-cursor-extents.3
  (glib-test:with-check-memory (buffer text)
    (setf buffer (gtk:entry-buffer-new "Ägypten"))
    (setf text (gtk:text-new-with-buffer buffer))
    (graphene:with-rect (weak)
      (is-false (gtk:text-compute-cursor-extents text 2 nil weak))
      #-windows
      (is (= 17.0 (graphene:rect-x weak)))
      (is (= -8.0 (graphene:rect-y weak)))
      (is (=  0.0 (graphene:rect-width weak)))
      (is (= 16.0 (graphene:rect-height weak))))
    (is-false (setf (gtk:text-buffer text) nil))))

#+windows
(test gtk-text-compute-cursor-extents.3
  (glib-test:with-check-memory (buffer text)
    (setf buffer (gtk:entry-buffer-new "Ägypten"))
    (setf text (gtk:text-new-with-buffer buffer))
    (graphene:with-rect (weak)
      (is-false (gtk:text-compute-cursor-extents text 2 nil weak))
      (is (= 15.0 (graphene:rect-x weak)))
      (is (= -8.0 (graphene:rect-y weak)))
      (is (=  0.0 (graphene:rect-width weak)))
      (is (= 17.0 (graphene:rect-height weak))))
    (is-false (setf (gtk:text-buffer text) nil))))

;;; 2025-05-30
