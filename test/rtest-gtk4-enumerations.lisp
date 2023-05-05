(in-package :gtk-test)

(def-suite gtk-enumerations :in gtk-suite)
(in-suite gtk-enumerations)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkAlign

(test align
  ;; Check the type
  (is (g:type-is-enum "GtkAlign"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkAlign")
          (g:gtype (cffi:foreign-funcall "gtk_align_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:align
          (gobject:symbol-for-gtype "GtkAlign")))
  ;; Check the names
  (is (equal '("GTK_ALIGN_FILL" "GTK_ALIGN_START" "GTK_ALIGN_END"
               "GTK_ALIGN_CENTER" "GTK_ALIGN_BASELINE")
             (list-enum-item-name "GtkAlign")))
  ;; Check the values
  (is (equal '(0 1 2 3 4)
             (list-enum-item-value "GtkAlign")))
  ;; Check the nick names
  (is (equal '("fill" "start" "end" "center" "baseline")
             (list-enum-item-nick "GtkAlign")))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkAlign"
                             GTK-ALIGN
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_align_get_type")
                             (:FILL 0)
                             (:START 1)
                             (:END 2)
                             (:CENTER 3)
                             (:BASELINE 4))
             (gobject:get-g-type-definition "GtkAlign"))))

;;;     GtkArrowType

;; Move this to rtest-gtk-menu-button.lisp

#+nil
(test arrow-type
  ;; Check the type
  (is (g:type-is-enum "GtkArrowType"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkArrowType")
          (g:gtype (cffi:foreign-funcall "gtk_arrow_type_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:arrow-type
          (gobject:symbol-for-gtype "GtkArrowType")))
  ;; Check the names
  (is (equal '("GTK_ARROW_UP" "GTK_ARROW_DOWN" "GTK_ARROW_LEFT"
               "GTK_ARROW_RIGHT" "GTK_ARROW_NONE")
             (list-enum-item-name "GtkArrowType")))
  ;; Check the values
  (is (equal '(0 1 2 3 4)
             (list-enum-item-value "GtkArrowType")))
  ;; Check the nick names
  (is (equal '("up" "down" "left" "right" "none")
             (list-enum-item-nick "GtkArrowType")))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkArrowType"
                             GTK-ARROW-TYPE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_arrow_type_get_type")
                             (:UP 0)
                             (:DOWN 1)
                             (:LEFT 2)
                             (:RIGHT 3)
                             (:NONE 4))
             (gobject:get-g-type-definition "GtkArrowType"))))

;;;     GtkBaselinePosition

(test baseline-position
  ;; Check the type
  (is (g:type-is-enum "GtkBaselinePosition"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkBaselinePosition")
          (g:gtype (cffi:foreign-funcall "gtk_baseline_position_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:baseline-position
          (gobject:symbol-for-gtype "GtkBaselinePosition")))
  ;; Check the names
  (is (equal '("GTK_BASELINE_POSITION_TOP" "GTK_BASELINE_POSITION_CENTER"
               "GTK_BASELINE_POSITION_BOTTOM")
             (list-enum-item-name "GtkBaselinePosition")))
  ;; Check the values
  (is (equal '(0 1 2)
             (list-enum-item-value "GtkBaselinePosition")))
  ;; Check the nick names
  (is (equal '("top" "center" "bottom")
             (list-enum-item-nick "GtkBaselinePosition")))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkBaselinePosition"
                             GTK-BASELINE-POSITION
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_baseline_position_get_type")
                             (:TOP 0)
                             (:CENTER 1)
                             (:BOTTOM 2))
             (get-g-type-definition "GtkBaselinePosition"))))

;;;     GtkDeleteType

(test delete-type
  ;; Check the type
  (is (g:type-is-enum "GtkDeleteType"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkDeleteType")
          (g:gtype (cffi:foreign-funcall "gtk_delete_type_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:delete-type
          (gobject:symbol-for-gtype "GtkDeleteType")))
  ;; Check the names
  (is (equal '("GTK_DELETE_CHARS" "GTK_DELETE_WORD_ENDS" "GTK_DELETE_WORDS"
               "GTK_DELETE_DISPLAY_LINES" "GTK_DELETE_DISPLAY_LINE_ENDS"
               "GTK_DELETE_PARAGRAPH_ENDS" "GTK_DELETE_PARAGRAPHS"
               "GTK_DELETE_WHITESPACE")
             (list-enum-item-name "GtkDeleteType")))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5 6 7)
             (list-enum-item-value "GtkDeleteType")))
  ;; Check the nick names
  (is (equal '("chars" "word-ends" "words" "display-lines" "display-line-ends"
               "paragraph-ends" "paragraphs" "whitespace")
             (list-enum-item-nick "GtkDeleteType")))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkDeleteType"
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
             (get-g-type-definition "GtkDeleteType"))))

;;;     GtkDirectionType

;;;     GtkIconSize
;;;     GtkSensitivityType
;;;     GtkTextDirection
;;;     GtkMessageType
;;;     GtkMovementStep
;;;     GtkScrollStep

;;;     GtkOrientation

(test orientation
  ;; Check the type
  (is (g:type-is-enum "GtkOrientation"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkOrientation")
          (g:gtype (cffi:foreign-funcall "gtk_orientation_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:orientation
          (gobject:symbol-for-gtype "GtkOrientation")))
  ;; Check the names
  (is (equal '("GTK_ORIENTATION_HORIZONTAL" "GTK_ORIENTATION_VERTICAL")
             (list-enum-item-name "GtkOrientation")))
  ;; Check the values
  (is (equal '(0 1)
             (list-enum-item-value "GtkOrientation")))
  ;; Check the nick names
  (is (equal '("horizontal" "vertical")
             (list-enum-item-nick "GtkOrientation")))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkOrientation"
                             GTK-ORIENTATION
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_orientation_get_type")
                             (:HORIZONTAL 0)
                             (:VERTICAL 1))
             (get-g-type-definition "GtkOrientation"))))

;;;     GtkOverflow
;;;     GtkPackType
;;;     GtkPositionType
;;;     GtkScrollType
;;;     GtkSelectionMode
;;;     GtkWrapMode
;;;     GtkSortType
;;;     GtkPrintPages
;;;     GtkPageSet
;;;     GtkNumberUpLayout
;;;     GtkOrdering
;;;     GtkPageOrientation
;;;     GtkPrintQuality
;;;     GtkPrintDuplex
;;;     GtkUnit
;;;     GtkTreeViewGridLines

;;;     GtkSizeGroupMode

(test gtk-size-group-mode
  ;; Check the type
  (is (g:type-is-enum "GtkSizeGroupMode"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkSizeGroupMode")
          (g:gtype (cffi:foreign-funcall "gtk_size_group_mode_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:size-group-mode
          (gobject:symbol-for-gtype "GtkSizeGroupMode")))
  ;; Check the names
  (is (equal '("GTK_SIZE_GROUP_NONE" "GTK_SIZE_GROUP_HORIZONTAL"
               "GTK_SIZE_GROUP_VERTICAL" "GTK_SIZE_GROUP_BOTH")
             (list-enum-item-name "GtkSizeGroupMode")))
  ;; Check the values
  (is (equal '(0 1 2 3)
             (list-enum-item-value "GtkSizeGroupMode")))
  ;; Check the nick names
  (is (equal '("none" "horizontal" "vertical" "both")
             (list-enum-item-nick "GtkSizeGroupMode")))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkSizeGroupMode"
                             GTK-SIZE-GROUP-MODE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_size_group_mode_get_type")
                             (:NONE 0)
                             (:HORIZONTAL 1)
                             (:VERTICAL 2)
                             (:BOTH 3))
             (gobject:get-g-type-definition "GtkSizeGroupMode"))))

;;;     GtkSizeRequestMode
;;;     GtkScrollablePolicy
;;;     GtkStateFlags
;;;     GtkBorderStyle
;;;     GtkLevelBarMode
;;;     GtkInputPurpose
;;;     GtkInputHints
;;;     GtkPropagationPhase
;;;     GtkPropagationLimit
;;;     GtkEventSequenceState
;;;     GtkPanDirection
;;;     GtkShortcutScope
;;;     GtkPickFlags
;;;     GtkConstraintRelation
;;;     GtkConstraintStrength
;;;     GtkConstraintAttribute
;;;     GtkConstraintVflParserError
;;;     GtkSystemSetting
;;;     GtkSymbolicColor

;;;     GtkAccessibleRole

(test accessible-role
  ;; Check the type
  (is (g:type-is-enum "GtkAccessibleRole"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkAccessibleRole")
          (g:gtype (cffi:foreign-funcall "gtk_accessible_role_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:accessible-role
          (gobject:symbol-for-gtype "GtkAccessibleRole")))
  ;; Check the names
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
               "GTK_ACCESSIBLE_ROLE_TOGGLE_BUTTON")
             (list-enum-item-name "GtkAccessibleRole")))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
               25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46
               47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68
               69 70 71 72 73 74 75 76 77 78)
             (list-enum-item-value "GtkAccessibleRole")))
  ;; Check the nick names
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
               "window" "toggle-button")
             (list-enum-item-nick "GtkAccessibleRole")))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkAccessibleRole"
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
                             (:TOGGLE-BUTTON 78))
             (get-g-type-definition "GtkAccessibleRole"))))

;;;     GtkAccessibleState

(test accessible-state
  ;; Check the type
  (is (g:type-is-enum "GtkAccessibleState"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkAccessibleState")
          (g:gtype (cffi:foreign-funcall "gtk_accessible_state_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:accessible-state
          (gobject:symbol-for-gtype "GtkAccessibleState")))
  ;; Check the names
  (is (equal '("GTK_ACCESSIBLE_STATE_BUSY"
               "GTK_ACCESSIBLE_STATE_CHECKED"
               "GTK_ACCESSIBLE_STATE_DISABLED"
               "GTK_ACCESSIBLE_STATE_EXPANDED"
               "GTK_ACCESSIBLE_STATE_HIDDEN"
               "GTK_ACCESSIBLE_STATE_INVALID"
               "GTK_ACCESSIBLE_STATE_PRESSED"
               "GTK_ACCESSIBLE_STATE_SELECTED")
             (list-enum-item-name "GtkAccessibleState")))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5 6 7)
             (list-enum-item-value "GtkAccessibleState")))
  ;; Check the nick names
  (is (equal '("busy" "checked" "disabled" "expanded" "hidden" "invalid"
               "pressed" "selected")
             (list-enum-item-nick "GtkAccessibleState")))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkAccessibleState"
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
                             (:SELECTED 7))
             (get-g-type-definition "GtkAccessibleState"))))

;;;     GtkAccessibleProperty

(test accessible-property
  ;; Check the type
  (is (g:type-is-enum "GtkAccessibleProperty"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkAccessibleProperty")
          (g:gtype (cffi:foreign-funcall "gtk_accessible_property_get_type"
                                         :size))))
  ;; Check the registered name
  (is (eq 'gtk:accessible-property
          (gobject:symbol-for-gtype "GtkAccessibleProperty")))
  ;; Check the names
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
             (list-enum-item-name "GtkAccessibleProperty")))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18)
             (list-enum-item-value "GtkAccessibleProperty")))
  ;; Check the nick names
  (is (equal '("autocomplete" "description" "has-popup" "key-shortcuts" "label"
               "level" "modal" "multi-line" "multi-selectable" "orientation"
               "placeholder" "read-only" "required" "role-description" "sort"
               "value-max" "value-min" "value-now" "value-text")
             (list-enum-item-nick "GtkAccessibleProperty")))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkAccessibleProperty"
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
             (get-g-type-definition "GtkAccessibleProperty"))))

;;;     GtkAccessibleRelation

(test accessible-relation
  ;; Check the type
  (is (g:type-is-enum "GtkAccessibleRelation"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkAccessibleRelation")
          (g:gtype (cffi:foreign-funcall "gtk_accessible_relation_get_type"
                                         :size))))
  ;; Check the registered name
  (is (eq 'gtk:accessible-relation
          (gobject:symbol-for-gtype "GtkAccessibleRelation")))
  ;; Check the names
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
             (list-enum-item-name "GtkAccessibleRelation")))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17)
             (list-enum-item-value "GtkAccessibleRelation")))
  ;; Check the nick names
  (is (equal '("active-descendant" "col-count" "col-index" "col-index-text"
               "col-span" "controls" "described-by" "details" "error-message"
               "flow-to" "labelled-by" "owns" "pos-in-set" "row-count"
               "row-index" "row-index-text" "row-span" "set-size")
             (list-enum-item-nick "GtkAccessibleRelation")))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkAccessibleRelation"
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
             (get-g-type-definition "GtkAccessibleRelation"))))

;;;     GtkAccessibleTristate

(test accessible-tristate
  ;; Check the type
  (is (g:type-is-enum "GtkAccessibleTristate"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkAccessibleTristate")
          (g:gtype (cffi:foreign-funcall "gtk_accessible_tristate_get_type"
                                         :size))))
  ;; Check the registered name
  (is (eq 'gtk:accessible-tristate
          (gobject:symbol-for-gtype "GtkAccessibleTristate")))
  ;; Check the names
  (is (equal '("GTK_ACCESSIBLE_TRISTATE_FALSE"
               "GTK_ACCESSIBLE_TRISTATE_TRUE"
               "GTK_ACCESSIBLE_TRISTATE_MIXED")
             (list-enum-item-name "GtkAccessibleTristate")))
  ;; Check the values
  (is (equal '(0 1 2)
             (list-enum-item-value "GtkAccessibleTristate")))
  ;; Check the nick names
  (is (equal '("false" "true" "mixed")
             (list-enum-item-nick "GtkAccessibleTristate")))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkAccessibleTristate"
                             GTK-ACCESSIBLE-TRISTATE
                             (:EXPORT T
                              :TYPE-INITIALIZER
                              "gtk_accessible_tristate_get_type")
                             (:FALSE 0)
                             (:TRUE 1)
                             (:MIXED 2))
             (get-g-type-definition "GtkAccessibleTristate"))))

;;;     GtkAccessibleInvalidState

(test acessible-invalid-state
  ;; Check the type
  (is (g:type-is-enum "GtkAccessibleInvalidState"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkAccessibleInvalidState")
          (g:gtype (cffi:foreign-funcall "gtk_accessible_invalid_state_get_type"
                                         :size))))
  ;; Check the registered name
  (is (eq 'gtk:accessible-invalid-state
          (gobject:symbol-for-gtype "GtkAccessibleInvalidState")))
  ;; Check the names
  (is (equal '("GTK_ACCESSIBLE_INVALID_FALSE"
               "GTK_ACCESSIBLE_INVALID_TRUE"
               "GTK_ACCESSIBLE_INVALID_GRAMMAR"
               "GTK_ACCESSIBLE_INVALID_SPELLING")
             (list-enum-item-name "GtkAccessibleInvalidState")))
  ;; Check the values
  (is (equal '(0 1 2 3)
             (list-enum-item-value "GtkAccessibleInvalidState")))
  ;; Check the nick names
  (is (equal '("false" "true" "grammar" "spelling")
             (list-enum-item-nick "GtkAccessibleInvalidState")))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkAccessibleInvalidState"
                             GTK-ACCESSIBLE-INVALID-STATE
                             (:EXPORT T
                              :TYPE-INITIALIZER
                              "gtk_accessible_invalid_state_get_type")
                             (:FALSE 0)
                             (:TRUE 1)
                             (:GRAMMAR 2)
                             (:SPELLING 3))
             (get-g-type-definition "GtkAccessibleInvalidState"))))

;;;     GtkAccessibleAutocomplete

(test acessible-autocomplete
  ;; Check the type
  (is (g:type-is-enum "GtkAccessibleAutocomplete"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkAccessibleAutocomplete")
          (g:gtype (cffi:foreign-funcall "gtk_accessible_autocomplete_get_type"
                                         :size))))
  ;; Check the registered name
  (is (eq 'gtk:accessible-autocomplete
          (gobject:symbol-for-gtype "GtkAccessibleAutocomplete")))
  ;; Check the names
  (is (equal '("GTK_ACCESSIBLE_AUTOCOMPLETE_NONE"
               "GTK_ACCESSIBLE_AUTOCOMPLETE_INLINE"
               "GTK_ACCESSIBLE_AUTOCOMPLETE_LIST"
               "GTK_ACCESSIBLE_AUTOCOMPLETE_BOTH")
             (list-enum-item-name "GtkAccessibleAutocomplete")))
  ;; Check the values
  (is (equal '(0 1 2 3)
             (list-enum-item-value "GtkAccessibleAutocomplete")))
  ;; Check the nick names
  (is (equal '("none" "inline" "list" "both")
             (list-enum-item-nick "GtkAccessibleAutocomplete")))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkAccessibleAutocomplete"
                             GTK-ACCESSIBLE-AUTOCOMPLETE
                             (:EXPORT T
                              :TYPE-INITIALIZER
                              "gtk_accessible_autocomplete_get_type")
                             (:NONE 0)
                             (:INLINE 1)
                             (:LIST 2)
                             (:BOTH 3))
             (get-g-type-definition "GtkAccessibleAutocomplete"))))

;;;     GtkAccessibleSort

(test acessible-sort
  ;; Check the type
  (is (g:type-is-enum "GtkAccessibleSort"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkAccessibleSort")
          (g:gtype (cffi:foreign-funcall "gtk_accessible_sort_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:accessible-sort
          (gobject:symbol-for-gtype "GtkAccessibleSort")))
  ;; Check the names
  (is (equal '("GTK_ACCESSIBLE_SORT_NONE"
               "GTK_ACCESSIBLE_SORT_ASCENDING"
               "GTK_ACCESSIBLE_SORT_DESCENDING"
               "GTK_ACCESSIBLE_SORT_OTHER")
             (list-enum-item-name "GtkAccessibleSort")))
  ;; Check the values
  (is (equal '(0 1 2 3)
             (list-enum-item-value "GtkAccessibleSort")))
  ;; Check the nick names
  (is (equal '("none" "ascending" "descending" "other")
             (list-enum-item-nick "GtkAccessibleSort")))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkAccessibleSort"
                             GTK-ACCESSIBLE-SORT
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_accessible_sort_get_type")
                             (:NONE 0)
                             (:ASCENDING 1)
                             (:DESCENDING 2)
                             (:OTHER 3))
             (get-g-type-definition "GtkAccessibleSort"))))

;;; --- 2023-4-28 --------------------------------------------------------------
