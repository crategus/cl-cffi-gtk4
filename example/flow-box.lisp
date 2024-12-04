;;;; Flow Box
;;;;
;;;; The <tt>GtkFlowBox</tt> widget allows flexible and responsive grids which
;;;; reflow as needed and support sorting and filtering.
;;;;
;;;; The children of a <tt>GtkFlowBox</tt> widget are regular widgets. In this
;;;; example the <tt>GtkFlowBox</tt> widget contains a <tt>GtkBox</tt> widget
;;;; which contains a drawing area and a label.
;;;;
;;;; 2024-4-14

(in-package :gtk4-example)

;; The following is the list of recognized color keywords that can be used
;; as a keyword value for data type <color> in a CSS definition.
;; See https://www.w3.org/TR/SVG11/types.html#ColorKeywords

(defparameter *colors*
  '("AliceBlue"         "AntiqueWhite"      "Aqua"
    "Aquamarine"        "Azure"             "Beige"
    "Bisque"            "Black"             "BlanchedAlmond"
    "Blue"              "BlueViolet"        "Brown"
    "Burlywood"         "CadetBlue"         "Chartreuse"
    "Chocolate"         "Coral"             "CornflowerBlue"
    "Cornsilk"          "Crimson"           "Cyan"
    "DarkBlue"          "DarkCyan"          "DarkGoldenrod"
    "DarkGray"          "DarkGreen"         "DarkGrey"
    "DarkKhaki"         "DarkMagenta"       "DarkOliveGreen"
    "DarkOrange"        "DarkOrchid"        "DarkRed"
    "DarkSalmon"        "DarkSeaGreen"      "DarkSlateBlue"
    "DarkSlateGray"     "DarkSlateGrey"     "DarkTurquoise"
    "DarkViolet"        "DeepPink"          "DeepSkyBlue"
    "DimGray"           "DimGrey"           "DodgerBlue"
    "Firebrick"         "FloralWhite"       "ForestGreen"
    "Fuchsia"           "Gainsboro"         "GhostWhite"
    "Gold"              "Goldenrod"         "Gray"
    "Grey"              "Green"             "GreenYellow"
    "Honeydew"          "HotPink"           "IndianRed"
    "Indigo"            "Ivory"             "Khaki"
    "Lavender"          "LavenderBlush"     "LawnGreen"
    "LemonChiffon"      "LightBlue"         "LightCoral"
    "LightCyan"         "LightGoldenrod"    "LightGoldenrodYellow"
    "LightGray"         "LightGreen"        "LightGrey"
    "LightPink"         "LightSalmon"       "LightSeaGreen"
    "LightSkyBlue"      "LightSlateBlue"    "LightSlateGray"
    "LightSlateGrey"    "LightSteelBlue"    "LightYellow"
    "Lime"              "LimeGreen"         "Linen"
    "Magenta"           "Maroon"            "MediumAquamarine"
    "MediumBlue"        "MediumOrchid"      "MediumPurple"
    "MediumSeaGreen"    "MediumSlateBlue"   "MediumSpringGreen"
    "MediumTurquoise"   "MediumVioletRed"   "MidnightBlue"
    "MintCream"         "MistyRose"         "Moccasin"
    "NavajoWhite"       "NavyBlue"          "OldLace"
    "Olive"             "OliveDrab"         "Orange"
    "OrangeRed"         "Orchid"            "PaleGoldenrod"
    "PaleGreen"         "PaleTurquoise"     "PaleVioletRed"
    "PapayaWhip"        "PeachPuff"         "Peru"
    "Pink"              "Plum"              "PowderBlue"
    "Purple"            "Red"               "RosyBrown"
    "RoyalBlue"         "SaddleBrown"       "Salmon"
    "SandyBrown"        "SeaGreen"          "Seashell"
    "Sienna"            "Silver"            "SkyBlue"
    "SlateBlue"         "SlateGray"         "SlateGrey"
    "Snow"              "SpringGreen"       "SteelBlue"
    "Tan"               "Teal"              "Thistle"
    "Tomato"            "Turquoise"         "Violet"
    "VioletRed"         "Wheat"             "White"
    "WhiteSmoke"        "Yellow"            "YellowGreen"
))

(defun color-swatch-new (color)
  (let* ((vbox (make-instance 'gtk:box
                              :orientation :vertical))
         (button (make-instance 'gtk:button
                                :child vbox
                                ;; Button cannot be selected with the pointer.
                                ;; This makes the flow box child selectable.
                                :can-target nil
                                :has-frame nil
                                :name color))
         (area (make-instance 'gtk:drawing-area
                              :width-request 24
                              :height-request 24))
         (label (make-instance 'gtk:label
                               :label color)))
    (gtk:drawing-area-set-draw-func area
            (lambda (widget cr width height)
              (declare (ignore widget width height))
              (let ((rgba (gdk:rgba-parse color)))
                (when rgba
                  (gdk:cairo-set-source-rgba cr rgba)
                  (cairo:paint cr)))))
    (gtk:box-append vbox area)
    (gtk:box-append vbox label)
    button))

(defun do-flow-box (&optional application)
  (let* ((vbox (make-instance 'gtk:box
                              :orientation :vertical))
         (window (make-instance 'gtk:window
                                 :title "Flow Box"
                                 :child vbox
                                 :application application
                                 :default-width 600
                                 :default-height 480))
         (entry (make-instance 'gtk:search-entry))
         (flowbox (make-instance 'gtk:flow-box
                                 :valign :start
                                 :max-children-per-line 10
                                 :selection-mode :single
                                 :homogeneous t))
         (scrolled (make-instance 'gtk:scrolled-window
                                  :child flowbox
                                  :vexpand t
                                  :hscrollbar-policy :never
                                  :vscrollbar-policy :automatic)))
    (g:signal-connect entry "search-changed"
                      (lambda (entry)
                        (declare (ignore entry))
                        (gtk:flow-box-invalidate-filter flowbox)))
    (gtk:flow-box-set-filter-func flowbox
          (lambda (child)
            (let* ((button (gtk:flow-box-child-child child))
                   (color (gtk:widget-name button))
                   (text (gtk:editable-text entry)))
              (search (string-downcase text) (string-downcase color)))))
    (gtk:flow-box-set-sort-func flowbox
          (lambda (child1 child2)
            (let* ((color1 (gtk:widget-name (gtk:flow-box-child-child child1)))
                   (color2 (gtk:widget-name (gtk:flow-box-child-child child2))))
              (if (string> (string-downcase color1) (string-downcase color2))
                  1
                  0))))
    (g:signal-connect flowbox "child-activated"
          (lambda (flowbox child)
            (declare (ignore flowbox))
            (let* ((button (gtk:flow-box-child-child child))
                   (color (gtk:widget-name button))
                   (rgba (gdk:rgba-parse color)))
              (format t "Color button selected :~%")
              (format t "   color : ~a~%" color)
              (format t "    rgba : ~a~%" rgba))))
    (let ((count 0))
      (dolist (color *colors*)
        (let ((child (make-instance 'gtk:flow-box-child
                                    :child (color-swatch-new color))))
          (incf count)
          (gtk:flow-box-append flowbox child)
          (when (= count 1)
            (gtk:flow-box-select-child flowbox child))))
      (format t "~a colors loaded in the flow box.~%" count))
    (gtk:box-append vbox entry)
    (gtk:box-append vbox scrolled)
    (gtk:window-present window)))
