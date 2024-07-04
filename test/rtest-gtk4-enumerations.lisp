(in-package :gtk-test)

(def-suite gtk-enumerations :in gtk-suite)
(in-suite gtk-enumerations)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkAlign

(test gtk-align
  ;; Check type
  (is (g:type-is-enum "GtkAlign"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkAlign")
          (g:gtype (cffi:foreign-funcall "gtk_align_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:align
          (glib:symbol-for-gtype "GtkAlign")))
  ;; Check names
  (is (equal '("GTK_ALIGN_FILL" "GTK_ALIGN_START" "GTK_ALIGN_END"
               "GTK_ALIGN_CENTER" "GTK_ALIGN_BASELINE_FILL" "GTK_ALIGN_BASELINE"
               "GTK_ALIGN_BASELINE_CENTER")
             (gtk-test:list-enum-item-name "GtkAlign")))
  ;; Check values
  (is (equal '(0 1 2 3 4 4 5)
             (gtk-test:list-enum-item-value "GtkAlign")))
  ;; Check nick names
  (is (equal '("fill" "start" "end" "center" "baseline-fill" "baseline"
               "baseline-center")
             (gtk-test:list-enum-item-nick "GtkAlign")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkAlign" GTK-ALIGN
                                     (:EXPORT T
                                      :TYPE-INITIALIZER "gtk_align_get_type")
                                     (:FILL 0)
                                     (:START 1)
                                     (:END 2)
                                     (:CENTER 3)
                                     (:BASELINE-FILL 4)
                                     (:BASELINE 4)
                                     (:BASELINE-CENTER 5))
             (gobject:get-g-type-definition "GtkAlign"))))

;;;     GtkBaselinePosition

(test gtk-baseline-position
  ;; Check type
  (is (g:type-is-enum "GtkBaselinePosition"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkBaselinePosition")
          (g:gtype (cffi:foreign-funcall "gtk_baseline_position_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:baseline-position
          (glib:symbol-for-gtype "GtkBaselinePosition")))
  ;; Check names
  (is (equal '("GTK_BASELINE_POSITION_TOP" "GTK_BASELINE_POSITION_CENTER"
               "GTK_BASELINE_POSITION_BOTTOM")
             (gtk-test:list-enum-item-name "GtkBaselinePosition")))
  ;; Check values
  (is (equal '(0 1 2)
             (gtk-test:list-enum-item-value "GtkBaselinePosition")))
  ;; Check nick names
  (is (equal '("top" "center" "bottom")
             (gtk-test:list-enum-item-nick "GtkBaselinePosition")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkBaselinePosition"
                             GTK-BASELINE-POSITION
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_baseline_position_get_type")
                             (:TOP 0)
                             (:CENTER 1)
                             (:BOTTOM 2))
             (gobject:get-g-type-definition "GtkBaselinePosition"))))

;;;     GtkDeleteType

(test gtk-delete-type
  ;; Check type
  (is (g:type-is-enum "GtkDeleteType"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkDeleteType")
          (g:gtype (cffi:foreign-funcall "gtk_delete_type_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:delete-type
          (glib:symbol-for-gtype "GtkDeleteType")))
  ;; Check names
  (is (equal '("GTK_DELETE_CHARS" "GTK_DELETE_WORD_ENDS" "GTK_DELETE_WORDS"
               "GTK_DELETE_DISPLAY_LINES" "GTK_DELETE_DISPLAY_LINE_ENDS"
               "GTK_DELETE_PARAGRAPH_ENDS" "GTK_DELETE_PARAGRAPHS"
               "GTK_DELETE_WHITESPACE")
             (gtk-test:list-enum-item-name "GtkDeleteType")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5 6 7)
             (gtk-test:list-enum-item-value "GtkDeleteType")))
  ;; Check nick names
  (is (equal '("chars" "word-ends" "words" "display-lines" "display-line-ends"
               "paragraph-ends" "paragraphs" "whitespace")
             (gtk-test:list-enum-item-nick "GtkDeleteType")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkDeleteType"
                             GTK-DELETE-TYPE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_delete_type_get_type")
                             (:CHARS 0)
                             (:WORD-ENDS 1)
                             (:WORDS 2)
                             (:DISPLAY-LINES 3)
                             (:DISPLAY-LINE-ENDS 4)
                             (:PARAGRAPH-ENDS 5)
                             (:PARAGRAPHS 6)
                             (:WHITESPACE 7))
             (gobject:get-g-type-definition "GtkDeleteType"))))

;;;     GtkDirectionType

(test gtk-direction-type
  ;; Check type
  (is (g:type-is-enum "GtkDirectionType"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkDirectionType")
          (g:gtype (cffi:foreign-funcall "gtk_direction_type_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:direction-type
          (glib:symbol-for-gtype "GtkDirectionType")))
  ;; Check names
  (is (equal '("GTK_DIR_TAB_FORWARD" "GTK_DIR_TAB_BACKWARD" "GTK_DIR_UP"
               "GTK_DIR_DOWN" "GTK_DIR_LEFT" "GTK_DIR_RIGHT")
             (gtk-test:list-enum-item-name "GtkDirectionType")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5)
             (gtk-test:list-enum-item-value "GtkDirectionType")))
  ;; Check nick names
  (is (equal '("tab-forward" "tab-backward" "up" "down" "left" "right")
             (gtk-test:list-enum-item-nick "GtkDirectionType")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkDirectionType" GTK-DIRECTION-TYPE
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gtk_direction_type_get_type")
                                     (:TAB-FORWARD 0)
                                     (:TAB-BACKWARD 1)
                                     (:UP 2)
                                     (:DOWN 3)
                                     (:LEFT 4)
                                     (:RIGHT 5))
             (gobject:get-g-type-definition "GtkDirectionType"))))

;;;     GtkIconSize

(test gtk-icon-size
  ;; Check type
  (is (g:type-is-enum "GtkIconSize"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkIconSize")
          (g:gtype (cffi:foreign-funcall "gtk_icon_size_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:icon-size
          (glib:symbol-for-gtype "GtkIconSize")))
  ;; Check names
  (is (equal '("GTK_ICON_SIZE_INHERIT" "GTK_ICON_SIZE_NORMAL"
               "GTK_ICON_SIZE_LARGE")
             (gtk-test:list-enum-item-name "GtkIconSize")))
  ;; Check values
  (is (equal '(0 1 2)
             (gtk-test:list-enum-item-value "GtkIconSize")))
  ;; Check nick names
  (is (equal '("inherit" "normal" "large")
             (gtk-test:list-enum-item-nick "GtkIconSize")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkIconSize" GTK-ICON-SIZE
                                     (:EXPORT T
                                      :TYPE-INITIALIZER "gtk_icon_size_get_type")
                                     (:INHERIT 0)
                                     (:NORMAL 1)
                                     (:LARGE 2))
             (gobject:get-g-type-definition "GtkIconSize"))))

;;;     GtkResponseType

(test gtk-response-type
  ;; Check type
  (is (g:type-is-enum "GtkResponseType"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkResponseType")
          (g:gtype (cffi:foreign-funcall "gtk_response_type_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:response-type
          (glib:symbol-for-gtype "GtkResponseType")))
  ;; Check names
  (is (equal '("GTK_RESPONSE_NONE" "GTK_RESPONSE_REJECT" "GTK_RESPONSE_ACCEPT"
               "GTK_RESPONSE_DELETE_EVENT" "GTK_RESPONSE_OK"
               "GTK_RESPONSE_CANCEL" "GTK_RESPONSE_CLOSE" "GTK_RESPONSE_YES"
               "GTK_RESPONSE_NO" "GTK_RESPONSE_APPLY" "GTK_RESPONSE_HELP")
             (gtk-test:list-enum-item-name "GtkResponseType")))
  ;; Check values
  (is (equal '(-1 -2 -3 -4 -5 -6 -7 -8 -9 -10 -11)
             (gtk-test:list-enum-item-value "GtkResponseType")))
  ;; Check nick names
  (is (equal '("none" "reject" "accept" "delete-event" "ok" "cancel" "close"
               "yes" "no" "apply" "help")
             (gtk-test:list-enum-item-nick "GtkResponseType")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkResponseType"
                             GTK-RESPONSE-TYPE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_response_type_get_type")
                             (:NONE -1)
                             (:REJECT -2)
                             (:ACCEPT -3)
                             (:DELETE-EVENT -4)
                             (:OK -5)
                             (:CANCEL -6)
                             (:CLOSE -7)
                             (:YES -8)
                             (:NO -9)
                             (:APPLY -10)
                             (:HELP -11))
             (gobject:get-g-type-definition "GtkResponseType"))))

;;;     GtkSensitivityType

(test gtk-sensitivity-type
  ;; Check type
  (is (g:type-is-enum "GtkSensitivityType"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkSensitivityType")
          (g:gtype (cffi:foreign-funcall "gtk_sensitivity_type_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gtk:sensitivity-type
          (glib:symbol-for-gtype "GtkSensitivityType")))
  ;; Check names
  (is (equal '("GTK_SENSITIVITY_AUTO" "GTK_SENSITIVITY_ON"
               "GTK_SENSITIVITY_OFF")
             (gtk-test:list-enum-item-name "GtkSensitivityType")))
  ;; Check values
  (is (equal '(0 1 2)
             (gtk-test:list-enum-item-value "GtkSensitivityType")))
  ;; Check nick names
  (is (equal '("auto" "on" "off")
             (gtk-test:list-enum-item-nick "GtkSensitivityType")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkSensitivityType"
                                     GTK-SENSITIVITY-TYPE
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gtk_sensitivity_type_get_type")
                                     (:AUTO 0)
                                     (:ON 1)
                                     (:OFF 2))
             (gobject:get-g-type-definition "GtkSensitivityType"))))


;;;     GtkTextDirection

(test gtk-text-direction
  ;; Check type
  (is (g:type-is-enum "GtkTextDirection"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkTextDirection")
          (g:gtype (cffi:foreign-funcall "gtk_text_direction_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:text-direction
          (glib:symbol-for-gtype "GtkTextDirection")))
  ;; Check names
  (is (equal '("GTK_TEXT_DIR_NONE" "GTK_TEXT_DIR_LTR" "GTK_TEXT_DIR_RTL")
             (gtk-test:list-enum-item-name "GtkTextDirection")))
  ;; Check values
  (is (equal '(0 1 2)
             (gtk-test:list-enum-item-value "GtkTextDirection")))
  ;; Check nick names
  (is (equal '("none" "ltr" "rtl")
             (gtk-test:list-enum-item-nick "GtkTextDirection")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkTextDirection" GTK-TEXT-DIRECTION
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gtk_text_direction_get_type")
                                     (:NONE 0)
                                     (:LTR 1)
                                     (:RTL 2))
             (gobject:get-g-type-definition "GtkTextDirection"))))

;;;     GtkJustification

(test gtk-justification
  ;; Check type
  (is (g:type-is-enum "GtkJustification"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkJustification")
          (g:gtype (cffi:foreign-funcall "gtk_justification_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:justification
          (glib:symbol-for-gtype "GtkJustification")))
  ;; Check names
  (is (equal '("GTK_JUSTIFY_LEFT" "GTK_JUSTIFY_RIGHT" "GTK_JUSTIFY_CENTER"
               "GTK_JUSTIFY_FILL")
             (gtk-test:list-enum-item-name "GtkJustification")))
  ;; Check values
  (is (equal '(0 1 2 3)
             (gtk-test:list-enum-item-value "GtkJustification")))
  ;; Check nick names
  (is (equal '("left" "right" "center" "fill")
             (gtk-test:list-enum-item-nick "GtkJustification")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkJustification" GTK-JUSTIFICATION
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gtk_justification_get_type")
                                     (:LEFT 0)
                                     (:RIGHT 1)
                                     (:CENTER 2)
                                     (:FILL 3))
             (gobject:get-g-type-definition "GtkJustification"))))

;;;     GtkMessageType

(test gtk-message-type
  ;; Check type
  (is (g:type-is-enum "GtkMessageType"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkMessageType")
          (g:gtype (cffi:foreign-funcall "gtk_message_type_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:message-type
          (glib:symbol-for-gtype "GtkMessageType")))
  ;; Check names
  (is (equal '("GTK_MESSAGE_INFO" "GTK_MESSAGE_WARNING" "GTK_MESSAGE_QUESTION"
               "GTK_MESSAGE_ERROR" "GTK_MESSAGE_OTHER")
             (gtk-test:list-enum-item-name "GtkMessageType")))
  ;; Check values
  (is (equal '(0 1 2 3 4)
             (gtk-test:list-enum-item-value "GtkMessageType")))
  ;; Check nick names
  (is (equal '("info" "warning" "question" "error" "other")
             (gtk-test:list-enum-item-nick "GtkMessageType")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkMessageType"
                             GTK-MESSAGE-TYPE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_message_type_get_type")
                             (:INFO 0)
                             (:WARNING 1)
                             (:QUESTION 2)
                             (:ERROR 3)
                             (:OTHER 4))
             (gobject:get-g-type-definition "GtkMessageType"))))

;;;     GtkMovementStep

(test gtk-movement-step
  ;; Check type
  (is (g:type-is-enum "GtkMovementStep"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkMovementStep")
          (g:gtype (cffi:foreign-funcall "gtk_movement_step_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:movement-step
          (glib:symbol-for-gtype "GtkMovementStep")))
  ;; Check names
  (is (equal '("GTK_MOVEMENT_LOGICAL_POSITIONS" "GTK_MOVEMENT_VISUAL_POSITIONS"
               "GTK_MOVEMENT_WORDS" "GTK_MOVEMENT_DISPLAY_LINES"
               "GTK_MOVEMENT_DISPLAY_LINE_ENDS" "GTK_MOVEMENT_PARAGRAPHS"
               "GTK_MOVEMENT_PARAGRAPH_ENDS" "GTK_MOVEMENT_PAGES"
               "GTK_MOVEMENT_BUFFER_ENDS" "GTK_MOVEMENT_HORIZONTAL_PAGES")
             (gtk-test:list-enum-item-name "GtkMovementStep")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5 6 7 8 9)
             (gtk-test:list-enum-item-value "GtkMovementStep")))
  ;; Check nick names
  (is (equal '("logical-positions" "visual-positions" "words" "display-lines"
               "display-line-ends" "paragraphs" "paragraph-ends" "pages"
               "buffer-ends" "horizontal-pages")
             (gtk-test:list-enum-item-nick "GtkMovementStep")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkMovementStep" GTK-MOVEMENT-STEP
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gtk_movement_step_get_type")
                                     (:LOGICAL-POSITIONS 0)
                                     (:VISUAL-POSITIONS 1)
                                     (:WORDS 2)
                                     (:DISPLAY-LINES 3)
                                     (:DISPLAY-LINE-ENDS 4)
                                     (:PARAGRAPHS 5)
                                     (:PARAGRAPH-ENDS 6)
                                     (:PAGES 7)
                                     (:BUFFER-ENDS 8)
                                     (:HORIZONTAL-PAGES 9))
             (gobject:get-g-type-definition "GtkMovementStep"))))

;;;     GtkNaturalWrapMode

#+gtk-4-6
(test gtk-natural-wrap-mode
  ;; Check type
  (is (g:type-is-enum "GtkNaturalWrapMode"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkNaturalWrapMode")
          (g:gtype (cffi:foreign-funcall "gtk_natural_wrap_mode_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gtk:natural-wrap-mode
          (glib:symbol-for-gtype "GtkNaturalWrapMode")))
  ;; Check names
  (is (equal '("GTK_NATURAL_WRAP_INHERIT" "GTK_NATURAL_WRAP_NONE"
               "GTK_NATURAL_WRAP_WORD")
             (gtk-test:list-enum-item-name "GtkNaturalWrapMode")))
  ;; Check values
  (is (equal '(0 1 2)
             (gtk-test:list-enum-item-value "GtkNaturalWrapMode")))
  ;; Check nick names
  (is (equal '("inherit" "none" "word")
             (gtk-test:list-enum-item-nick "GtkNaturalWrapMode")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkNaturalWrapMode"
                                     GTK-NATURAL-WRAP-MODE
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gtk_natural_wrap_mode_get_type")
                                     (:INHERIT 0)
                                     (:NONE 1)
                                     (:WORD 2))
             (gobject:get-g-type-definition "GtkNaturalWrapMode"))))

;;;     GtkScrollStep                                      not exported

;;;     GtkOrientation

(test gtk-orientation
  ;; Check type
  (is (g:type-is-enum "GtkOrientation"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkOrientation")
          (g:gtype (cffi:foreign-funcall "gtk_orientation_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:orientation
          (glib:symbol-for-gtype "GtkOrientation")))
  ;; Check names
  (is (equal '("GTK_ORIENTATION_HORIZONTAL" "GTK_ORIENTATION_VERTICAL")
             (gtk-test:list-enum-item-name "GtkOrientation")))
  ;; Check values
  (is (equal '(0 1)
             (gtk-test:list-enum-item-value "GtkOrientation")))
  ;; Check nick names
  (is (equal '("horizontal" "vertical")
             (gtk-test:list-enum-item-nick "GtkOrientation")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkOrientation"
                             GTK-ORIENTATION
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_orientation_get_type")
                             (:HORIZONTAL 0)
                             (:VERTICAL 1))
             (gobject:get-g-type-definition "GtkOrientation"))))

;;;     GtkOverflow

(test gtk-overflow
  ;; Check type
  (is (g:type-is-enum "GtkOverflow"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkOverflow")
          (g:gtype (cffi:foreign-funcall "gtk_overflow_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:overflow
          (glib:symbol-for-gtype "GtkOverflow")))
  ;; Check names
  (is (equal '("GTK_OVERFLOW_VISIBLE" "GTK_OVERFLOW_HIDDEN")
             (gtk-test:list-enum-item-name "GtkOverflow")))
  ;; Check values
  (is (equal '(0 1)
             (gtk-test:list-enum-item-value "GtkOverflow")))
  ;; Check nick names
  (is (equal '("visible" "hidden")
             (gtk-test:list-enum-item-nick "GtkOverflow")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkOverflow" GTK-OVERFLOW
                                     (:EXPORT T
                                      :TYPE-INITIALIZER "gtk_overflow_get_type")
                                     (:VISIBLE 0)
                                     (:HIDDEN 1))
             (gobject:get-g-type-definition "GtkOverflow"))))

;;;     GtkPackType

(test gtk-pack-type
  ;; Check type
  (is (g:type-is-enum "GtkPackType"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkPackType")
          (g:gtype (cffi:foreign-funcall "gtk_pack_type_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:pack-type
          (glib:symbol-for-gtype "GtkPackType")))
  ;; Check names
  (is (equal '("GTK_PACK_START" "GTK_PACK_END")
             (gtk-test:list-enum-item-name "GtkPackType")))
  ;; Check values
  (is (equal '(0 1)
             (gtk-test:list-enum-item-value "GtkPackType")))
  ;; Check nick names
  (is (equal '("start" "end")
             (gtk-test:list-enum-item-nick "GtkPackType")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkPackType" GTK-PACK-TYPE
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gtk_pack_type_get_type")
                                     (:START 0)
                                     (:END 1))
             (gobject:get-g-type-definition "GtkPackType"))))

;;;     GtkPositionType

(test gtk-position-type
  ;; Check type
  (is (g:type-is-enum "GtkPositionType"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkPositionType")
          (g:gtype (cffi:foreign-funcall "gtk_position_type_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:position-type
          (glib:symbol-for-gtype "GtkPositionType")))
  ;; Check names
  (is (equal '("GTK_POS_LEFT" "GTK_POS_RIGHT" "GTK_POS_TOP" "GTK_POS_BOTTOM")
             (gtk-test:list-enum-item-name "GtkPositionType")))
  ;; Check values
  (is (equal '(0 1 2 3)
             (gtk-test:list-enum-item-value "GtkPositionType")))
  ;; Check nick names
  (is (equal '("left" "right" "top" "bottom")
             (gtk-test:list-enum-item-nick "GtkPositionType")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkPositionType" GTK-POSITION-TYPE
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gtk_position_type_get_type")
                                     (:LEFT 0)
                                     (:RIGHT 1)
                                     (:TOP 2)
                                     (:BOTTOM 3))
             (gobject:get-g-type-definition "GtkPositionType"))))

;;;     GtkScrollType

(test gtk-scroll-type
  ;; Check type
  (is (g:type-is-enum "GtkScrollType"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkScrollType")
          (g:gtype (cffi:foreign-funcall "gtk_scroll_type_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:scroll-type
          (glib:symbol-for-gtype "GtkScrollType")))
  ;; Check names
  (is (equal '("GTK_SCROLL_NONE" "GTK_SCROLL_JUMP" "GTK_SCROLL_STEP_BACKWARD"
               "GTK_SCROLL_STEP_FORWARD" "GTK_SCROLL_PAGE_BACKWARD"
               "GTK_SCROLL_PAGE_FORWARD" "GTK_SCROLL_STEP_UP"
               "GTK_SCROLL_STEP_DOWN" "GTK_SCROLL_PAGE_UP"
               "GTK_SCROLL_PAGE_DOWN" "GTK_SCROLL_STEP_LEFT"
               "GTK_SCROLL_STEP_RIGHT" "GTK_SCROLL_PAGE_LEFT"
               "GTK_SCROLL_PAGE_RIGHT" "GTK_SCROLL_START" "GTK_SCROLL_END")
             (gtk-test:list-enum-item-name "GtkScrollType")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
             (gtk-test:list-enum-item-value "GtkScrollType")))
  ;; Check nick names
  (is (equal '("none" "jump" "step-backward" "step-forward" "page-backward"
               "page-forward" "step-up" "step-down" "page-up" "page-down"
               "step-left" "step-right" "page-left" "page-right" "start" "end")
             (gtk-test:list-enum-item-nick "GtkScrollType")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkScrollType" GTK-SCROLL-TYPE
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gtk_scroll_type_get_type")
                                     (:NONE 0)
                                     (:JUMP 1)
                                     (:STEP-BACKWARD 2)
                                     (:STEP-FORWARD 3)
                                     (:PAGE-BACKWARD 4)
                                     (:PAGE-FORWARD 5)
                                     (:STEP-UP 6)
                                     (:STEP-DOWN 7)
                                     (:PAGE-UP 8)
                                     (:PAGE-DOWN 9)
                                     (:STEP-LEFT 10)
                                     (:STEP-RIGHT 11)
                                     (:PAGE-LEFT 12)
                                     (:PAGE-RIGHT 13)
                                     (:START 14)
                                     (:END 15))
             (gobject:get-g-type-definition "GtkScrollType"))))

;;;     GtkSelectionMode

(test gtk-selection-mode
  ;; Check type
  (is (g:type-is-enum "GtkSelectionMode"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkSelectionMode")
          (g:gtype (cffi:foreign-funcall "gtk_selection_mode_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:selection-mode
          (glib:symbol-for-gtype "GtkSelectionMode")))
  ;; Check names
  (is (equal '("GTK_SELECTION_NONE" "GTK_SELECTION_SINGLE"
               "GTK_SELECTION_BROWSE" "GTK_SELECTION_MULTIPLE")
             (gtk-test:list-enum-item-name "GtkSelectionMode")))
  ;; Check values
  (is (equal '(0 1 2 3)
             (gtk-test:list-enum-item-value "GtkSelectionMode")))
  ;; Check nick names
  (is (equal '("none" "single" "browse" "multiple")
             (gtk-test:list-enum-item-nick "GtkSelectionMode")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkSelectionMode" GTK-SELECTION-MODE
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gtk_selection_mode_get_type")
                                     (:NONE 0)
                                     (:SINGLE 1)
                                     (:BROWSE 2)
                                     (:MULTIPLE 3))
             (gobject:get-g-type-definition "GtkSelectionMode"))))

;;;     GtkWrapMode

(test gtk-wrap-mode
  ;; Check type
  (is (g:type-is-enum "GtkWrapMode"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkWrapMode")
          (g:gtype (cffi:foreign-funcall "gtk_wrap_mode_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:wrap-mode
          (glib:symbol-for-gtype "GtkWrapMode")))
  ;; Check names
  (is (equal '("GTK_WRAP_NONE" "GTK_WRAP_CHAR" "GTK_WRAP_WORD"
               "GTK_WRAP_WORD_CHAR")
             (gtk-test:list-enum-item-name "GtkWrapMode")))
  ;; Check values
  (is (equal '(0 1 2 3)
             (gtk-test:list-enum-item-value "GtkWrapMode")))
  ;; Check nick names
  (is (equal '("none" "char" "word" "word-char")
             (gtk-test:list-enum-item-nick "GtkWrapMode")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkWrapMode"
                                     GTK-WRAP-MODE
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gtk_wrap_mode_get_type")
                                     (:NONE 0)
                                     (:CHAR 1)
                                     (:WORD 2)
                                     (:WORD-CHAR 3))
             (gobject:get-g-type-definition "GtkWrapMode"))))

;;;     GtkSortType

(test gtk-sort-type
  ;; Check type
  (is (g:type-is-enum "GtkSortType"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkSortType")
          (g:gtype (cffi:foreign-funcall "gtk_sort_type_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:sort-type
          (glib:symbol-for-gtype "GtkSortType")))
  ;; Check names
  (is (equal '("GTK_SORT_ASCENDING" "GTK_SORT_DESCENDING")
             (gtk-test:list-enum-item-name "GtkSortType")))
  ;; Check values
  (is (equal '(0 1)
             (gtk-test:list-enum-item-value "GtkSortType")))
  ;; Check nick names
  (is (equal '("ascending" "descending")
             (gtk-test:list-enum-item-nick "GtkSortType")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkSortType" GTK-SORT-TYPE
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gtk_sort_type_get_type")
                                     (:ASCENDING 0)
                                     (:DESCENDING 1))
             (gobject:get-g-type-definition "GtkSortType"))))

;;;     GtkOrdering

(test gtk-ordering
  ;; Check type
  (is (g:type-is-enum "GtkOrdering"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkOrdering")
          (g:gtype (cffi:foreign-funcall "gtk_ordering_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:ordering
          (glib:symbol-for-gtype "GtkOrdering")))
  ;; Check names
  (is (equal '("GTK_ORDERING_SMALLER" "GTK_ORDERING_EQUAL"
               "GTK_ORDERING_LARGER")
             (gtk-test:list-enum-item-name "GtkOrdering")))
  ;; Check values
  (is (equal '(-1 0 1)
             (gtk-test:list-enum-item-value "GtkOrdering")))
  ;; Check nick names
  (is (equal '("smaller" "equal" "larger")
             (gtk-test:list-enum-item-nick "GtkOrdering")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkOrdering" GTK-ORDERING
                                     (:EXPORT T
                                      :TYPE-INITIALIZER "gtk_ordering_get_type")
                                     (:SMALLER -1)
                                     (:EQUAL 0)
                                     (:LARGER 1))
             (gobject:get-g-type-definition "GtkOrdering"))))

;;;     GtkSizeGroupMode

(test gtk-size-group-mode
  ;; Check type
  (is (g:type-is-enum "GtkSizeGroupMode"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkSizeGroupMode")
          (g:gtype (cffi:foreign-funcall "gtk_size_group_mode_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:size-group-mode
          (glib:symbol-for-gtype "GtkSizeGroupMode")))
  ;; Check names
  (is (equal '("GTK_SIZE_GROUP_NONE" "GTK_SIZE_GROUP_HORIZONTAL"
               "GTK_SIZE_GROUP_VERTICAL" "GTK_SIZE_GROUP_BOTH")
             (gtk-test:list-enum-item-name "GtkSizeGroupMode")))
  ;; Check values
  (is (equal '(0 1 2 3)
             (gtk-test:list-enum-item-value "GtkSizeGroupMode")))
  ;; Check nick names
  (is (equal '("none" "horizontal" "vertical" "both")
             (gtk-test:list-enum-item-nick "GtkSizeGroupMode")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkSizeGroupMode"
                             GTK-SIZE-GROUP-MODE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_size_group_mode_get_type")
                             (:NONE 0)
                             (:HORIZONTAL 1)
                             (:VERTICAL 2)
                             (:BOTH 3))
             (gobject:get-g-type-definition "GtkSizeGroupMode"))))

;;;     GtkSizeRequestMode

(test gtk-size-request-mode
  ;; Check type
  (is (g:type-is-enum "GtkSizeRequestMode"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkSizeRequestMode")
          (g:gtype (cffi:foreign-funcall "gtk_size_request_mode_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gtk:size-request-mode
          (glib:symbol-for-gtype "GtkSizeRequestMode")))
  ;; Check names
  (is (equal '("GTK_SIZE_REQUEST_HEIGHT_FOR_WIDTH"
               "GTK_SIZE_REQUEST_WIDTH_FOR_HEIGHT"
               "GTK_SIZE_REQUEST_CONSTANT_SIZE")
             (gtk-test:list-enum-item-name "GtkSizeRequestMode")))
  ;; Check values
  (is (equal '(0 1 2)
             (gtk-test:list-enum-item-value "GtkSizeRequestMode")))
  ;; Check nick names
  (is (equal '("height-for-width" "width-for-height" "constant-size")
             (gtk-test:list-enum-item-nick "GtkSizeRequestMode")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkSizeRequestMode" GTK-SIZE-REQUEST-MODE
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gtk_size_request_mode_get_type")
                                     (:HEIGHT-FOR-WIDTH 0)
                                     (:WIDTH-FOR-HEIGHT 1)
                                     (:CONSTANT-SIZE 2))
             (gobject:get-g-type-definition "GtkSizeRequestMode"))))

;;;     GtkStateFlags

(test gtk-state-flags
  ;; Check type
  (is (g:type-is-flags "GtkStateFlags"))
  ;; Check registered name
  (is (eq 'gtk:state-flags
          (glib:symbol-for-gtype "GtkStateFlags")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkStateFlags")
          (g:gtype (cffi:foreign-funcall "gtk_state_flags_get_type" :size))))
  ;; Check names
  (is (equal '("GTK_STATE_FLAG_NORMAL" "GTK_STATE_FLAG_ACTIVE"
               "GTK_STATE_FLAG_PRELIGHT" "GTK_STATE_FLAG_SELECTED"
               "GTK_STATE_FLAG_INSENSITIVE" "GTK_STATE_FLAG_INCONSISTENT"
               "GTK_STATE_FLAG_FOCUSED" "GTK_STATE_FLAG_BACKDROP"
               "GTK_STATE_FLAG_DIR_LTR" "GTK_STATE_FLAG_DIR_RTL"
               "GTK_STATE_FLAG_LINK" "GTK_STATE_FLAG_VISITED"
               "GTK_STATE_FLAG_CHECKED" "GTK_STATE_FLAG_DROP_ACTIVE"
               "GTK_STATE_FLAG_FOCUS_VISIBLE" "GTK_STATE_FLAG_FOCUS_WITHIN")
             (gtk-test:list-flags-item-name "GtkStateFlags")))
  ;; Check values
  (is (equal '(0 1 2 4 8 16 32 64 128 256 512 1024 2048 4096 8192 16384)
             (gtk-test:list-flags-item-value "GtkStateFlags")))
  ;; Check nick names
  (is (equal '("normal" "active" "prelight" "selected" "insensitive"
               "inconsistent" "focused" "backdrop" "dir-ltr" "dir-rtl" "link"
               "visited" "checked" "drop-active" "focus-visible" "focus-within")
             (gtk-test:list-flags-item-nick "GtkStateFlags")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-G-FLAGS "GtkStateFlags" GTK-STATE-FLAGS
                                      (:EXPORT T
                                       :TYPE-INITIALIZER
                                       "gtk_state_flags_get_type")
                                      (:NORMAL 0)
                                      (:ACTIVE 1)
                                      (:PRELIGHT 2)
                                      (:SELECTED 4)
                                      (:INSENSITIVE 8)
                                      (:INCONSISTENT 16)
                                      (:FOCUSED 32)
                                      (:BACKDROP 64)
                                      (:DIR-LTR 128)
                                      (:DIR-RTL 256)
                                      (:LINK 512)
                                      (:VISITED 1024)
                                      (:CHECKED 2048)
                                      (:DROP-ACTIVE 4096)
                                      (:FOCUS-VISIBLE 8192)
                                      (:FOCUS-WITHIN 16384))
             (gobject:get-g-type-definition "GtkStateFlags"))))

;;;     GtkBorderStyle

(test gtk-border-style
  ;; Check type
  (is (g:type-is-enum "GtkBorderStyle"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkBorderStyle")
          (g:gtype (cffi:foreign-funcall "gtk_border_style_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:border-style
          (glib:symbol-for-gtype "GtkBorderStyle")))
  ;; Check names
  (is (equal '("GTK_BORDER_STYLE_NONE" "GTK_BORDER_STYLE_HIDDEN"
               "GTK_BORDER_STYLE_SOLID" "GTK_BORDER_STYLE_INSET"
               "GTK_BORDER_STYLE_OUTSET" "GTK_BORDER_STYLE_DOTTED"
               "GTK_BORDER_STYLE_DASHED" "GTK_BORDER_STYLE_DOUBLE"
               "GTK_BORDER_STYLE_GROOVE" "GTK_BORDER_STYLE_RIDGE")
             (gtk-test:list-enum-item-name "GtkBorderStyle")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5 6 7 8 9)
             (gtk-test:list-enum-item-value "GtkBorderStyle")))
  ;; Check nick names
  (is (equal '("none" "hidden" "solid" "inset" "outset" "dotted" "dashed"
               "double" "groove" "ridge")
             (gtk-test:list-enum-item-nick "GtkBorderStyle")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkBorderStyle" GTK-BORDER-STYLE
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gtk_border_style_get_type")
                                     (:NONE 0)
                                     (:HIDDEN 1)
                                     (:SOLID 2)
                                     (:INSET 3)
                                     (:OUTSET 4)
                                     (:DOTTED 5)
                                     (:DASHED 6)
                                     (:DOUBLE 7)
                                     (:GROOVE 8)
                                     (:RIDGE 9))
             (gobject:get-g-type-definition "GtkBorderStyle"))))

;;;     GtkInputPurpose

(test gtk-input-purpose
  ;; Check type
  (is (g:type-is-enum "GtkInputPurpose"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkInputPurpose")
          (g:gtype (cffi:foreign-funcall "gtk_input_purpose_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:input-purpose
          (glib:symbol-for-gtype "GtkInputPurpose")))
  ;; Check names
  (is (equal '("GTK_INPUT_PURPOSE_FREE_FORM" "GTK_INPUT_PURPOSE_ALPHA"
               "GTK_INPUT_PURPOSE_DIGITS" "GTK_INPUT_PURPOSE_NUMBER"
               "GTK_INPUT_PURPOSE_PHONE" "GTK_INPUT_PURPOSE_URL"
               "GTK_INPUT_PURPOSE_EMAIL" "GTK_INPUT_PURPOSE_NAME"
               "GTK_INPUT_PURPOSE_PASSWORD" "GTK_INPUT_PURPOSE_PIN"
 "GTK_INPUT_PURPOSE_TERMINAL")
             (gtk-test:list-enum-item-name "GtkInputPurpose")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5 6 7 8 9 10)
             (gtk-test:list-enum-item-value "GtkInputPurpose")))
  ;; Check nick names
  (is (equal '("free-form" "alpha" "digits" "number" "phone" "url" "email"
               "name" "password" "pin" "terminal")
             (gtk-test:list-enum-item-nick "GtkInputPurpose")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkInputPurpose" GTK-INPUT-PURPOSE
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gtk_input_purpose_get_type")
                                     (:FREE-FORM 0)
                                     (:ALPHA 1)
                                     (:DIGITS 2)
                                     (:NUMBER 3)
                                     (:PHONE 4)
                                     (:URL 5)
                                     (:EMAIL 6)
                                     (:NAME 7)
                                     (:PASSWORD 8)
                                     (:PIN 9)
                                     (:TERMINAL 10))
             (gobject:get-g-type-definition "GtkInputPurpose"))))

;;;     GtkInputHints

(test gtk-input-hints
  ;; Check type
  (is (g:type-is-flags "GtkInputHints"))
  ;; Check registered name
  (is (eq 'gtk:input-hints
          (glib:symbol-for-gtype "GtkInputHints")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkInputHints")
          (g:gtype (cffi:foreign-funcall "gtk_input_hints_get_type" :size))))
  ;; Check names
  (is (equal '("GTK_INPUT_HINT_NONE" "GTK_INPUT_HINT_SPELLCHECK"
               "GTK_INPUT_HINT_NO_SPELLCHECK" "GTK_INPUT_HINT_WORD_COMPLETION"
               "GTK_INPUT_HINT_LOWERCASE" "GTK_INPUT_HINT_UPPERCASE_CHARS"
               "GTK_INPUT_HINT_UPPERCASE_WORDS"
               "GTK_INPUT_HINT_UPPERCASE_SENTENCES"
               "GTK_INPUT_HINT_INHIBIT_OSK" "GTK_INPUT_HINT_VERTICAL_WRITING"
               "GTK_INPUT_HINT_EMOJI" "GTK_INPUT_HINT_NO_EMOJI"
               "GTK_INPUT_HINT_PRIVATE")
             (gtk-test:list-flags-item-name "GtkInputHints")))
  ;; Check values
  (is (equal '(0 1 2 4 8 16 32 64 128 256 512 1024 2048)
             (gtk-test:list-flags-item-value "GtkInputHints")))
  ;; Check nick names
  (is (equal '("none" "spellcheck" "no-spellcheck" "word-completion" "lowercase"
               "uppercase-chars" "uppercase-words" "uppercase-sentences"
               "inhibit-osk" "vertical-writing" "emoji" "no-emoji" "private")
             (gtk-test:list-flags-item-nick "GtkInputHints")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-G-FLAGS "GtkInputHints" GTK-INPUT-HINTS
                                      (:EXPORT T
                                       :TYPE-INITIALIZER
                                       "gtk_input_hints_get_type")
                                      (:NONE 0)
                                      (:SPELLCHECK 1)
                                      (:NO-SPELLCHECK 2)
                                      (:WORD-COMPLETION 4)
                                      (:LOWERCASE 8)
                                      (:UPPERCASE-CHARS 16)
                                      (:UPPERCASE-WORDS 32)
                                      (:UPPERCASE-SENTENCES 64)
                                      (:INHIBIT-OSK 128)
                                      (:VERTICAL-WRITING 256)
                                      (:EMOJI 512)
                                      (:NO-EMOJI 1024)
                                      (:PRIVATE 2048))
             (gobject:get-g-type-definition "GtkInputHints"))))

;;;     GtkPickFlags

(test gtk-pick-flags
  ;; Check type
  (is (g:type-is-flags "GtkPickFlags"))
  ;; Check registered name
  (is (eq 'gtk:pick-flags
          (glib:symbol-for-gtype "GtkPickFlags")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkPickFlags")
          (g:gtype (cffi:foreign-funcall "gtk_pick_flags_get_type" :size))))
  ;; Check names
  (is (equal '("GTK_PICK_DEFAULT" "GTK_PICK_INSENSITIVE"
               "GTK_PICK_NON_TARGETABLE")
             (gtk-test:list-flags-item-name "GtkPickFlags")))
  ;; Check values
  (is (equal '(0 1 2)
             (gtk-test:list-flags-item-value "GtkPickFlags")))
  ;; Check nick names
  (is (equal '("default" "insensitive" "non-targetable")
             (gtk-test:list-flags-item-nick "GtkPickFlags")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-G-FLAGS "GtkPickFlags" GTK-PICK-FLAGS
                                      (:EXPORT T
                                       :TYPE-INITIALIZER
                                       "gtk_pick_flags_get_type")
                                      (:DEFAULT 0)
                                      (:INSENSITIVE 1)
                                      (:NON-TARGETABLE 2))
             (gobject:get-g-type-definition "GtkPickFlags"))))

;;;     GtkConstraintRelation

(test gtk-constraint-relation
  ;; Check type
  (is (g:type-is-enum "GtkConstraintRelation"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkConstraintRelation")
          (g:gtype (cffi:foreign-funcall "gtk_constraint_relation_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gtk:constraint-relation
          (glib:symbol-for-gtype "GtkConstraintRelation")))
  ;; Check names
  (is (equal '("GTK_CONSTRAINT_RELATION_LE" "GTK_CONSTRAINT_RELATION_EQ"
               "GTK_CONSTRAINT_RELATION_GE")
             (gtk-test:list-enum-item-name "GtkConstraintRelation")))
  ;; Check values
  (is (equal '(-1 0 1)
             (gtk-test:list-enum-item-value "GtkConstraintRelation")))
  ;; Check nick names
  (is (equal '("le" "eq" "ge")
             (gtk-test:list-enum-item-nick "GtkConstraintRelation")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkConstraintRelation"
                                     GTK-CONSTRAINT-RELATION
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gtk_constraint_relation_get_type")
                                     (:LE -1)
                                     (:EQ 0)
                                     (:GE 1))
             (gobject:get-g-type-definition "GtkConstraintRelation"))))

;;;     GtkConstraintStrength

(test gtk-constraint-strength
  ;; Check type
  (is (g:type-is-enum "GtkConstraintStrength"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkConstraintStrength")
          (g:gtype (cffi:foreign-funcall "gtk_constraint_strength_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gtk:constraint-strength
          (glib:symbol-for-gtype "GtkConstraintStrength")))
  ;; Check names
  (is (equal '("GTK_CONSTRAINT_STRENGTH_REQUIRED"
               "GTK_CONSTRAINT_STRENGTH_STRONG"
               "GTK_CONSTRAINT_STRENGTH_MEDIUM"
               "GTK_CONSTRAINT_STRENGTH_WEAK")
             (gtk-test:list-enum-item-name "GtkConstraintStrength")))
  ;; Check values
  (is (equal '(1001001000 1000000000 1000 1)
             (gtk-test:list-enum-item-value "GtkConstraintStrength")))
  ;; Check nick names
  (is (equal '("required" "strong" "medium" "weak")
             (gtk-test:list-enum-item-nick "GtkConstraintStrength")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkConstraintStrength"
                                     GTK-CONSTRAINT-STRENGTH
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gtk_constraint_strength_get_type")
                                     (:REQUIRED 1001001000)
                                     (:STRONG 1000000000)
                                     (:MEDIUM 1000)
                                     (:WEAK 1))
             (gobject:get-g-type-definition "GtkConstraintStrength"))))

;;;     GtkConstraintAttribute

(test gtk-constraint-attribute
  ;; Check type
  (is (g:type-is-enum "GtkConstraintAttribute"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkConstraintAttribute")
          (g:gtype (cffi:foreign-funcall "gtk_constraint_attribute_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gtk:constraint-attribute
          (glib:symbol-for-gtype "GtkConstraintAttribute")))
  ;; Check names
  (is (equal '("GTK_CONSTRAINT_ATTRIBUTE_NONE" "GTK_CONSTRAINT_ATTRIBUTE_LEFT"
               "GTK_CONSTRAINT_ATTRIBUTE_RIGHT" "GTK_CONSTRAINT_ATTRIBUTE_TOP"
               "GTK_CONSTRAINT_ATTRIBUTE_BOTTOM"
               "GTK_CONSTRAINT_ATTRIBUTE_START"
               "GTK_CONSTRAINT_ATTRIBUTE_END" "GTK_CONSTRAINT_ATTRIBUTE_WIDTH"
               "GTK_CONSTRAINT_ATTRIBUTE_HEIGHT"
               "GTK_CONSTRAINT_ATTRIBUTE_CENTER_X"
               "GTK_CONSTRAINT_ATTRIBUTE_CENTER_Y"
               "GTK_CONSTRAINT_ATTRIBUTE_BASELINE")
             (gtk-test:list-enum-item-name "GtkConstraintAttribute")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5 6 7 8 9 10 11)
             (gtk-test:list-enum-item-value "GtkConstraintAttribute")))
  ;; Check nick names
  (is (equal '("none" "left" "right" "top" "bottom" "start" "end" "width"
               "height" "center-x" "center-y" "baseline")
             (gtk-test:list-enum-item-nick "GtkConstraintAttribute")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkConstraintAttribute"
                                     GTK-CONSTRAINT-ATTRIBUTE
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gtk_constraint_attribute_get_type")
                                     (:NONE 0)
                                     (:LEFT 1)
                                     (:RIGHT 2)
                                     (:TOP 3)
                                     (:BOTTOM 4)
                                     (:START 5)
                                     (:END 6)
                                     (:WIDTH 7)
                                     (:HEIGHT 8)
                                     (:CENTER-X 9)
                                     (:CENTER-Y 10)
                                     (:BASELINE 11))
             (gobject:get-g-type-definition "GtkConstraintAttribute"))))

;;;     GtkConstraintVflParserError

(test gtk-constraint-vfl-parser-error
  ;; Check type
  (is (g:type-is-enum "GtkConstraintVflParserError"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkConstraintVflParserError")
          (g:gtype (cffi:foreign-funcall "gtk_constraint_vfl_parser_error_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gtk:constraint-vfl-parser-error
          (glib:symbol-for-gtype "GtkConstraintVflParserError")))
  ;; Check names
  (is (equal '("GTK_CONSTRAINT_VFL_PARSER_ERROR_INVALID_SYMBOL"
               "GTK_CONSTRAINT_VFL_PARSER_ERROR_INVALID_ATTRIBUTE"
               "GTK_CONSTRAINT_VFL_PARSER_ERROR_INVALID_VIEW"
               "GTK_CONSTRAINT_VFL_PARSER_ERROR_INVALID_METRIC"
               "GTK_CONSTRAINT_VFL_PARSER_ERROR_INVALID_PRIORITY"
               "GTK_CONSTRAINT_VFL_PARSER_ERROR_INVALID_RELATION")
             (gtk-test:list-enum-item-name "GtkConstraintVflParserError")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5)
             (gtk-test:list-enum-item-value "GtkConstraintVflParserError")))
  ;; Check nick names
  (is (equal '("symbol" "attribute" "view" "metric" "priority" "relation")
             (gtk-test:list-enum-item-nick "GtkConstraintVflParserError")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkConstraintVflParserError"
                                     GTK-CONSTRAINT-VFL-PARSER-ERROR
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gtk_constraint_vfl_parser_error_get_type")
                                     (:SYMBOL 0)
                                     (:ATTRIBUTE 1)
                                     (:VIEW 2)
                                     (:METRIC 3)
                                     (:PRIORITY 4)
                                     (:RELATION 5))
             (gobject:get-g-type-definition "GtkConstraintVflParserError"))))

;;;     GtkSymbolicColor

(test gtk-symbolic-color
  ;; Check type
  (is (g:type-is-enum "GtkSymbolicColor"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkSymbolicColor")
          (g:gtype (cffi:foreign-funcall "gtk_symbolic_color_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:symbolic-color
          (glib:symbol-for-gtype "GtkSymbolicColor")))
  ;; Check names
  (is (equal '("GTK_SYMBOLIC_COLOR_FOREGROUND" "GTK_SYMBOLIC_COLOR_ERROR"
               "GTK_SYMBOLIC_COLOR_WARNING" "GTK_SYMBOLIC_COLOR_SUCCESS")
             (gtk-test:list-enum-item-name "GtkSymbolicColor")))
  ;; Check values
  (is (equal '(0 1 2 3)
             (gtk-test:list-enum-item-value "GtkSymbolicColor")))
  ;; Check nick names
  (is (equal '("foreground" "error" "warning" "success")
             (gtk-test:list-enum-item-nick "GtkSymbolicColor")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkSymbolicColor" GTK-SYMBOLIC-COLOR
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gtk_symbolic_color_get_type")
                                     (:FOREGROUND 0)
                                     (:ERROR 1)
                                     (:WARNING 2)
                                     (:SUCCESS 3))
             (gobject:get-g-type-definition "GtkSymbolicColor"))))

;;;     GtkAccessibleRole

(test gtk-accessible-role
  ;; Check type
  (is (g:type-is-enum "GtkAccessibleRole"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkAccessibleRole")
          (g:gtype (cffi:foreign-funcall "gtk_accessible_role_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:accessible-role
          (glib:symbol-for-gtype "GtkAccessibleRole")))
  ;; Check names
  (is (equal '("GTK_ACCESSIBLE_ROLE_ALERT" "GTK_ACCESSIBLE_ROLE_ALERT_DIALOG"
               "GTK_ACCESSIBLE_ROLE_BANNER" "GTK_ACCESSIBLE_ROLE_BUTTON"
               "GTK_ACCESSIBLE_ROLE_CAPTION" "GTK_ACCESSIBLE_ROLE_CELL"
               "GTK_ACCESSIBLE_ROLE_CHECKBOX"
               "GTK_ACCESSIBLE_ROLE_COLUMN_HEADER"
               "GTK_ACCESSIBLE_ROLE_COMBO_BOX" "GTK_ACCESSIBLE_ROLE_COMMAND"
               "GTK_ACCESSIBLE_ROLE_COMPOSITE" "GTK_ACCESSIBLE_ROLE_DIALOG"
               "GTK_ACCESSIBLE_ROLE_DOCUMENT" "GTK_ACCESSIBLE_ROLE_FEED"
               "GTK_ACCESSIBLE_ROLE_FORM" "GTK_ACCESSIBLE_ROLE_GENERIC"
               "GTK_ACCESSIBLE_ROLE_GRID" "GTK_ACCESSIBLE_ROLE_GRID_CELL"
               "GTK_ACCESSIBLE_ROLE_GROUP" "GTK_ACCESSIBLE_ROLE_HEADING"
               "GTK_ACCESSIBLE_ROLE_IMG" "GTK_ACCESSIBLE_ROLE_INPUT"
               "GTK_ACCESSIBLE_ROLE_LABEL" "GTK_ACCESSIBLE_ROLE_LANDMARK"
               "GTK_ACCESSIBLE_ROLE_LEGEND" "GTK_ACCESSIBLE_ROLE_LINK"
               "GTK_ACCESSIBLE_ROLE_LIST" "GTK_ACCESSIBLE_ROLE_LIST_BOX"
               "GTK_ACCESSIBLE_ROLE_LIST_ITEM" "GTK_ACCESSIBLE_ROLE_LOG"
               "GTK_ACCESSIBLE_ROLE_MAIN" "GTK_ACCESSIBLE_ROLE_MARQUEE"
               "GTK_ACCESSIBLE_ROLE_MATH" "GTK_ACCESSIBLE_ROLE_METER"
               "GTK_ACCESSIBLE_ROLE_MENU" "GTK_ACCESSIBLE_ROLE_MENU_BAR"
               "GTK_ACCESSIBLE_ROLE_MENU_ITEM"
               "GTK_ACCESSIBLE_ROLE_MENU_ITEM_CHECKBOX"
               "GTK_ACCESSIBLE_ROLE_MENU_ITEM_RADIO"
               "GTK_ACCESSIBLE_ROLE_NAVIGATION"
               "GTK_ACCESSIBLE_ROLE_NONE" "GTK_ACCESSIBLE_ROLE_NOTE"
               "GTK_ACCESSIBLE_ROLE_OPTION" "GTK_ACCESSIBLE_ROLE_PRESENTATION"
               "GTK_ACCESSIBLE_ROLE_PROGRESS_BAR" "GTK_ACCESSIBLE_ROLE_RADIO"
               "GTK_ACCESSIBLE_ROLE_RADIO_GROUP" "GTK_ACCESSIBLE_ROLE_RANGE"
               "GTK_ACCESSIBLE_ROLE_REGION" "GTK_ACCESSIBLE_ROLE_ROW"
               "GTK_ACCESSIBLE_ROLE_ROW_GROUP" "GTK_ACCESSIBLE_ROLE_ROW_HEADER"
               "GTK_ACCESSIBLE_ROLE_SCROLLBAR" "GTK_ACCESSIBLE_ROLE_SEARCH"
               "GTK_ACCESSIBLE_ROLE_SEARCH_BOX" "GTK_ACCESSIBLE_ROLE_SECTION"
               "GTK_ACCESSIBLE_ROLE_SECTION_HEAD" "GTK_ACCESSIBLE_ROLE_SELECT"
               "GTK_ACCESSIBLE_ROLE_SEPARATOR" "GTK_ACCESSIBLE_ROLE_SLIDER"
               "GTK_ACCESSIBLE_ROLE_SPIN_BUTTON" "GTK_ACCESSIBLE_ROLE_STATUS"
               "GTK_ACCESSIBLE_ROLE_STRUCTURE" "GTK_ACCESSIBLE_ROLE_SWITCH"
               "GTK_ACCESSIBLE_ROLE_TAB" "GTK_ACCESSIBLE_ROLE_TABLE"
               "GTK_ACCESSIBLE_ROLE_TAB_LIST" "GTK_ACCESSIBLE_ROLE_TAB_PANEL"
               "GTK_ACCESSIBLE_ROLE_TEXT_BOX" "GTK_ACCESSIBLE_ROLE_TIME"
               "GTK_ACCESSIBLE_ROLE_TIMER" "GTK_ACCESSIBLE_ROLE_TOOLBAR"
               "GTK_ACCESSIBLE_ROLE_TOOLTIP" "GTK_ACCESSIBLE_ROLE_TREE"
               "GTK_ACCESSIBLE_ROLE_TREE_GRID" "GTK_ACCESSIBLE_ROLE_TREE_ITEM"
               "GTK_ACCESSIBLE_ROLE_WIDGET" "GTK_ACCESSIBLE_ROLE_WINDOW"
               "GTK_ACCESSIBLE_ROLE_TOGGLE_BUTTON"
               "GTK_ACCESSIBLE_ROLE_APPLICATION" "GTK_ACCESSIBLE_ROLE_PARAGRAPH"
               "GTK_ACCESSIBLE_ROLE_BLOCK_QUOTE" "GTK_ACCESSIBLE_ROLE_ARTICLE"
               "GTK_ACCESSIBLE_ROLE_COMMENT" "GTK_ACCESSIBLE_ROLE_TERMINAL")
             (gtk-test:list-enum-item-name "GtkAccessibleRole")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
               25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46
               47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68
               69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84)
             (gtk-test:list-enum-item-value "GtkAccessibleRole")))
  ;; Check nick names
  (is (equal '("alert" "alert-dialog" "banner" "button" "caption" "cell"
               "checkbox" "column-header" "combo-box" "command" "composite"
               "dialog" "document" "feed" "form" "generic" "grid" "grid-cell"
               "group" "heading" "img" "input" "label" "landmark" "legend"
               "link" "list" "list-box" "list-item" "log" "main" "marquee"
               "math" "meter" "menu" "menu-bar" "menu-item" "menu-item-checkbox"
               "menu-item-radio" "navigation" "none" "note" "option"
               "presentation" "progress-bar" "radio" "radio-group" "range"
               "region" "row" "row-group" "row-header" "scrollbar" "search"
               "search-box" "section" "section-head" "select" "separator"
               "slider" "spin-button" "status" "structure" "switch" "tab"
               "table" "tab-list" "tab-panel" "text-box" "time" "timer"
               "toolbar" "tooltip" "tree" "tree-grid" "tree-item" "widget"
               "window" "toggle-button" "application" "paragraph" "block-quote"
               "article" "comment" "terminal")
             (gtk-test:list-enum-item-nick "GtkAccessibleRole")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkAccessibleRole"
                             GTK-ACCESSIBLE-ROLE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_accessible_role_get_type")
                             (:ALERT 0)
                             (:ALERT-DIALOG 1)
                             (:BANNER 2)
                             (:BUTTON 3)
                             (:CAPTION 4)
                             (:CELL 5)
                             (:CHECKBOX 6)
                             (:COLUMN-HEADER 7)
                             (:COMBO-BOX 8)
                             (:COMMAND 9)
                             (:COMPOSITE 10)
                             (:DIALOG 11)
                             (:DOCUMENT 12)
                             (:FEED 13)
                             (:FORM 14)
                             (:GENERIC 15)
                             (:GRID 16)
                             (:GRID-CELL 17)
                             (:GROUP 18)
                             (:HEADING 19)
                             (:IMG 20)
                             (:INPUT 21)
                             (:LABEL 22)
                             (:LANDMARK 23)
                             (:LEGEND 24)
                             (:LINK 25)
                             (:LIST 26)
                             (:LIST-BOX 27)
                             (:LIST-ITEM 28)
                             (:LOG 29)
                             (:MAIN 30)
                             (:MARQUEE 31)
                             (:MATH 32)
                             (:METER 33)
                             (:MENU 34)
                             (:MENU-BAR 35)
                             (:MENU-ITEM 36)
                             (:MENU-ITEM-CHECKBOX 37)
                             (:MENU-ITEM-RADIO 38)
                             (:NAVIGATION 39)
                             (:NONE 40)
                             (:NOTE 41)
                             (:OPTION 42)
                             (:PRESENTATION 43)
                             (:PROGRESS-BAR 44)
                             (:RADIO 45)
                             (:RADIO-GROUP 46)
                             (:RANGE 47)
                             (:REGION 48)
                             (:ROW 49)
                             (:ROW-GROUP 50)
                             (:ROW-HEADER 51)
                             (:SCROLLBAR 52)
                             (:SEARCH 53)
                             (:SEARCH-BOX 54)
                             (:SECTION 55)
                             (:SECTION-HEAD 56)
                             (:SELECT 57)
                             (:SEPARATOR 58)
                             (:SLIDER 59)
                             (:SPIN-BUTTON 60)
                             (:STATUS 61)
                             (:STRUCTURE 62)
                             (:SWITCH 63)
                             (:TAB 64)
                             (:TABLE 65)
                             (:TAB-LIST 66)
                             (:TAB-PANEL 67)
                             (:TEXT-BOX 68)
                             (:TIME 69)
                             (:TIMER 70)
                             (:TOOLBAR 71)
                             (:TOOLTIP 72)
                             (:TREE 73)
                             (:TREE-GRID 74)
                             (:TREE-ITEM 75)
                             (:WIDGET 76)
                             (:WINDOW 77)
                             (:TOGGLE-BUTTON 78)
                             (:APPLICATION 79)
                             (:PARAGRAPH 80)
                             (:BLOCK-QUOTE 81)
                             (:ARTICLE 82)
                             (:COMMENT 83)
                             (:TERMINAL 84))
             (gobject:get-g-type-definition "GtkAccessibleRole"))))

;;;     GtkAccessibleState

(test gtk-accessible-state
  ;; Check type
  (is (g:type-is-enum "GtkAccessibleState"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkAccessibleState")
          (g:gtype (cffi:foreign-funcall "gtk_accessible_state_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gtk:accessible-state
          (glib:symbol-for-gtype "GtkAccessibleState")))
  ;; Check names
  (is (equal '("GTK_ACCESSIBLE_STATE_BUSY"
               "GTK_ACCESSIBLE_STATE_CHECKED"
               "GTK_ACCESSIBLE_STATE_DISABLED"
               "GTK_ACCESSIBLE_STATE_EXPANDED"
               "GTK_ACCESSIBLE_STATE_HIDDEN"
               "GTK_ACCESSIBLE_STATE_INVALID"
               "GTK_ACCESSIBLE_STATE_PRESSED"
               "GTK_ACCESSIBLE_STATE_SELECTED"
               "GTK_ACCESSIBLE_STATE_VISITED")
             (gtk-test:list-enum-item-name "GtkAccessibleState")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5 6 7 8)
             (gtk-test:list-enum-item-value "GtkAccessibleState")))
  ;; Check nick names
  (is (equal '("busy" "checked" "disabled" "expanded" "hidden" "invalid"
               "pressed" "selected" "visited")
             (gtk-test:list-enum-item-nick "GtkAccessibleState")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkAccessibleState"
                             GTK-ACCESSIBLE-STATE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_accessible_state_get_type")
                             (:BUSY 0)
                             (:CHECKED 1)
                             (:DISABLED 2)
                             (:EXPANDED 3)
                             (:HIDDEN 4)
                             (:INVALID 5)
                             (:PRESSED 6)
                             (:SELECTED 7)
                             (:VISITED 8))
             (gobject:get-g-type-definition "GtkAccessibleState"))))

;;;     GtkAccessibleProperty

(test gtk-accessible-property
  ;; Check type
  (is (g:type-is-enum "GtkAccessibleProperty"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkAccessibleProperty")
          (g:gtype (cffi:foreign-funcall "gtk_accessible_property_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gtk:accessible-property
          (glib:symbol-for-gtype "GtkAccessibleProperty")))
  ;; Check names
  (is (equal '("GTK_ACCESSIBLE_PROPERTY_AUTOCOMPLETE"
               "GTK_ACCESSIBLE_PROPERTY_DESCRIPTION"
               "GTK_ACCESSIBLE_PROPERTY_HAS_POPUP"
               "GTK_ACCESSIBLE_PROPERTY_KEY_SHORTCUTS"
               "GTK_ACCESSIBLE_PROPERTY_LABEL"
               "GTK_ACCESSIBLE_PROPERTY_LEVEL"
               "GTK_ACCESSIBLE_PROPERTY_MODAL"
               "GTK_ACCESSIBLE_PROPERTY_MULTI_LINE"
               "GTK_ACCESSIBLE_PROPERTY_MULTI_SELECTABLE"
               "GTK_ACCESSIBLE_PROPERTY_ORIENTATION"
               "GTK_ACCESSIBLE_PROPERTY_PLACEHOLDER"
               "GTK_ACCESSIBLE_PROPERTY_READ_ONLY"
               "GTK_ACCESSIBLE_PROPERTY_REQUIRED"
               "GTK_ACCESSIBLE_PROPERTY_ROLE_DESCRIPTION"
               "GTK_ACCESSIBLE_PROPERTY_SORT"
               "GTK_ACCESSIBLE_PROPERTY_VALUE_MAX"
               "GTK_ACCESSIBLE_PROPERTY_VALUE_MIN"
               "GTK_ACCESSIBLE_PROPERTY_VALUE_NOW"
               "GTK_ACCESSIBLE_PROPERTY_VALUE_TEXT")
             (gtk-test:list-enum-item-name "GtkAccessibleProperty")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18)
             (gtk-test:list-enum-item-value "GtkAccessibleProperty")))
  ;; Check nick names
  (is (equal '("autocomplete" "description" "has-popup" "key-shortcuts" "label"
               "level" "modal" "multi-line" "multi-selectable" "orientation"
               "placeholder" "read-only" "required" "role-description" "sort"
               "value-max" "value-min" "value-now" "value-text")
             (gtk-test:list-enum-item-nick "GtkAccessibleProperty")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkAccessibleProperty"
                             GTK-ACCESSIBLE-PROPERTY
                             (:EXPORT T
                              :TYPE-INITIALIZER
                              "gtk_accessible_property_get_type")
                             (:AUTOCOMPLETE 0)
                             (:DESCRIPTION 1)
                             (:HAS-POPUP 2)
                             (:KEY-SHORTCUTS 3)
                             (:LABEL 4)
                             (:LEVEL 5)
                             (:MODAL 6)
                             (:MULTI-LINE 7)
                             (:MULTI-SELECTABLE 8)
                             (:ORIENTATION 9)
                             (:PLACEHOLDER 10)
                             (:READ-ONLY 11)
                             (:REQUIRED 12)
                             (:ROLE-DESCRIPTION 13)
                             (:SORT 14)
                             (:VALUE-MAX 15)
                             (:VALUE-MIN 16)
                             (:VALUE-NOW 17)
                             (:VALUE-TEXT 18))
             (gobject:get-g-type-definition "GtkAccessibleProperty"))))

;;;     GtkAccessibleRelation

(test gtk-accessible-relation
  ;; Check type
  (is (g:type-is-enum "GtkAccessibleRelation"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkAccessibleRelation")
          (g:gtype (cffi:foreign-funcall "gtk_accessible_relation_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gtk:accessible-relation
          (glib:symbol-for-gtype "GtkAccessibleRelation")))
  ;; Check names
  (is (equal '("GTK_ACCESSIBLE_RELATION_ACTIVE_DESCENDANT"
               "GTK_ACCESSIBLE_RELATION_COL_COUNT"
               "GTK_ACCESSIBLE_RELATION_COL_INDEX"
               "GTK_ACCESSIBLE_RELATION_COL_INDEX_TEXT"
               "GTK_ACCESSIBLE_RELATION_COL_SPAN"
               "GTK_ACCESSIBLE_RELATION_CONTROLS"
               "GTK_ACCESSIBLE_RELATION_DESCRIBED_BY"
               "GTK_ACCESSIBLE_RELATION_DETAILS"
               "GTK_ACCESSIBLE_RELATION_ERROR_MESSAGE"
               "GTK_ACCESSIBLE_RELATION_FLOW_TO"
               "GTK_ACCESSIBLE_RELATION_LABELLED_BY"
               "GTK_ACCESSIBLE_RELATION_OWNS"
               "GTK_ACCESSIBLE_RELATION_POS_IN_SET"
               "GTK_ACCESSIBLE_RELATION_ROW_COUNT"
               "GTK_ACCESSIBLE_RELATION_ROW_INDEX"
               "GTK_ACCESSIBLE_RELATION_ROW_INDEX_TEXT"
               "GTK_ACCESSIBLE_RELATION_ROW_SPAN"
               "GTK_ACCESSIBLE_RELATION_SET_SIZE")
             (gtk-test:list-enum-item-name "GtkAccessibleRelation")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17)
             (gtk-test:list-enum-item-value "GtkAccessibleRelation")))
  ;; Check nick names
  (is (equal '("active-descendant" "col-count" "col-index" "col-index-text"
               "col-span" "controls" "described-by" "details" "error-message"
               "flow-to" "labelled-by" "owns" "pos-in-set" "row-count"
               "row-index" "row-index-text" "row-span" "set-size")
             (gtk-test:list-enum-item-nick "GtkAccessibleRelation")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkAccessibleRelation"
                             GTK-ACCESSIBLE-RELATION
                             (:EXPORT T
                              :TYPE-INITIALIZER
                              "gtk_accessible_relation_get_type")
                             (:ACTIVE-DESCENDANT 0)
                             (:COL-COUNT 1)
                             (:COL-INDEX 2)
                             (:COL-INDEX-TEXT 3)
                             (:COL-SPAN 4)
                             (:CONTROLS 5)
                             (:DESCRIBED-BY 6)
                             (:DETAILS 7)
                             (:ERROR-MESSAGE 8)
                             (:FLOW-TO 9)
                             (:LABELLED-BY 10)
                             (:OWNS 11)
                             (:POS-IN-SET 12)
                             (:ROW-COUNT 13)
                             (:ROW-INDEX 14)
                             (:ROW-INDEX-TEXT 15)
                             (:ROW-SPAN 16)
                             (:SET-SIZE 17))
             (gobject:get-g-type-definition "GtkAccessibleRelation"))))

;;;     GtkAccessibleTristate

(test gtk-accessible-tristate
  ;; Check type
  (is (g:type-is-enum "GtkAccessibleTristate"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkAccessibleTristate")
          (g:gtype (cffi:foreign-funcall "gtk_accessible_tristate_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gtk:accessible-tristate
          (glib:symbol-for-gtype "GtkAccessibleTristate")))
  ;; Check names
  (is (equal '("GTK_ACCESSIBLE_TRISTATE_FALSE"
               "GTK_ACCESSIBLE_TRISTATE_TRUE"
               "GTK_ACCESSIBLE_TRISTATE_MIXED")
             (gtk-test:list-enum-item-name "GtkAccessibleTristate")))
  ;; Check values
  (is (equal '(0 1 2)
             (gtk-test:list-enum-item-value "GtkAccessibleTristate")))
  ;; Check nick names
  (is (equal '("false" "true" "mixed")
             (gtk-test:list-enum-item-nick "GtkAccessibleTristate")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkAccessibleTristate"
                             GTK-ACCESSIBLE-TRISTATE
                             (:EXPORT T
                              :TYPE-INITIALIZER
                              "gtk_accessible_tristate_get_type")
                             (:FALSE 0)
                             (:TRUE 1)
                             (:MIXED 2))
             (gobject:get-g-type-definition "GtkAccessibleTristate"))))

;;;     GtkAccessibleInvalidState

(test gtk-acessible-invalid-state
  ;; Check type
  (is (g:type-is-enum "GtkAccessibleInvalidState"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkAccessibleInvalidState")
          (g:gtype (cffi:foreign-funcall "gtk_accessible_invalid_state_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gtk:accessible-invalid-state
          (glib:symbol-for-gtype "GtkAccessibleInvalidState")))
  ;; Check names
  (is (equal '("GTK_ACCESSIBLE_INVALID_FALSE"
               "GTK_ACCESSIBLE_INVALID_TRUE"
               "GTK_ACCESSIBLE_INVALID_GRAMMAR"
               "GTK_ACCESSIBLE_INVALID_SPELLING")
             (gtk-test:list-enum-item-name "GtkAccessibleInvalidState")))
  ;; Check values
  (is (equal '(0 1 2 3)
             (gtk-test:list-enum-item-value "GtkAccessibleInvalidState")))
  ;; Check nick names
  (is (equal '("false" "true" "grammar" "spelling")
             (gtk-test:list-enum-item-nick "GtkAccessibleInvalidState")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkAccessibleInvalidState"
                             GTK-ACCESSIBLE-INVALID-STATE
                             (:EXPORT T
                              :TYPE-INITIALIZER
                              "gtk_accessible_invalid_state_get_type")
                             (:FALSE 0)
                             (:TRUE 1)
                             (:GRAMMAR 2)
                             (:SPELLING 3))
             (gobject:get-g-type-definition "GtkAccessibleInvalidState"))))

;;;     GtkAccessibleAutocomplete

(test gtk-acessible-autocomplete
  ;; Check type
  (is (g:type-is-enum "GtkAccessibleAutocomplete"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkAccessibleAutocomplete")
          (g:gtype (cffi:foreign-funcall "gtk_accessible_autocomplete_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gtk:accessible-autocomplete
          (glib:symbol-for-gtype "GtkAccessibleAutocomplete")))
  ;; Check names
  (is (equal '("GTK_ACCESSIBLE_AUTOCOMPLETE_NONE"
               "GTK_ACCESSIBLE_AUTOCOMPLETE_INLINE"
               "GTK_ACCESSIBLE_AUTOCOMPLETE_LIST"
               "GTK_ACCESSIBLE_AUTOCOMPLETE_BOTH")
             (gtk-test:list-enum-item-name "GtkAccessibleAutocomplete")))
  ;; Check values
  (is (equal '(0 1 2 3)
             (gtk-test:list-enum-item-value "GtkAccessibleAutocomplete")))
  ;; Check nick names
  (is (equal '("none" "inline" "list" "both")
             (gtk-test:list-enum-item-nick "GtkAccessibleAutocomplete")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkAccessibleAutocomplete"
                             GTK-ACCESSIBLE-AUTOCOMPLETE
                             (:EXPORT T
                              :TYPE-INITIALIZER
                              "gtk_accessible_autocomplete_get_type")
                             (:NONE 0)
                             (:INLINE 1)
                             (:LIST 2)
                             (:BOTH 3))
             (gobject:get-g-type-definition "GtkAccessibleAutocomplete"))))

;;;     GtkAccessibleSort

(test gtk-acessible-sort
  ;; Check type
  (is (g:type-is-enum "GtkAccessibleSort"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkAccessibleSort")
          (g:gtype (cffi:foreign-funcall "gtk_accessible_sort_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gtk:accessible-sort
          (glib:symbol-for-gtype "GtkAccessibleSort")))
  ;; Check names
  (is (equal '("GTK_ACCESSIBLE_SORT_NONE"
               "GTK_ACCESSIBLE_SORT_ASCENDING"
               "GTK_ACCESSIBLE_SORT_DESCENDING"
               "GTK_ACCESSIBLE_SORT_OTHER")
             (gtk-test:list-enum-item-name "GtkAccessibleSort")))
  ;; Check values
  (is (equal '(0 1 2 3)
             (gtk-test:list-enum-item-value "GtkAccessibleSort")))
  ;; Check nick names
  (is (equal '("none" "ascending" "descending" "other")
             (gtk-test:list-enum-item-nick "GtkAccessibleSort")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkAccessibleSort"
                             GTK-ACCESSIBLE-SORT
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_accessible_sort_get_type")
                             (:NONE 0)
                             (:ASCENDING 1)
                             (:DESCENDING 2)
                             (:OTHER 3))
             (gobject:get-g-type-definition "GtkAccessibleSort"))))

;;; 2024-7-3
