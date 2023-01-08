;;; ----------------------------------------------------------------------------
;;; gdk.pango-interaction.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.6 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2022 Dieter Kaiser
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

;;;Pango Interaction
;;;Pango Interaction — Using Pango in GDK

;;;Functions
;;;cairo_region_t *	gdk_pango_layout_get_clip_region ()
;;;cairo_region_t *	gdk_pango_layout_line_get_clip_region ()

(in-package :gdk)

;;;Description
;;;Pango is the text layout system used by GDK and GTK. The functions and types in this section are used to obtain clip regions for PangoLayouts, and to get PangoContexts that can be used with GDK.

;;;Creating a PangoLayout object is the first step in rendering text, and requires getting a handle to a PangoContext. For GTK programs, you’ll usually want to use gtk_widget_get_pango_context(), or gtk_widget_create_pango_layout(). Once you have a PangoLayout, you can set the text and attributes of it with Pango functions like pango_layout_set_text() and get its size with pango_layout_get_size(). (Note that Pango uses a fixed point system internally, so converting between Pango units and pixels using PANGO_SCALE or the PANGO_PIXELS() macro.)

;;;Rendering a Pango layout is done most simply with pango_cairo_show_layout(); you can also draw pieces of the layout with pango_cairo_show_layout_line().

;;;Draw transformed text with Pango and cairo
;;;#define RADIUS 100
;;;#define N_WORDS 10
;;;#define FONT "Sans Bold 18"

;;;PangoContext *context;
;;;PangoLayout *layout;
;;;PangoFontDescription *desc;

;;;double radius;
;;;int width, height;
;;;int i;

;;;// Set up a transformation matrix so that the user space coordinates for
;;;// where we are drawing are [-RADIUS, RADIUS], [-RADIUS, RADIUS]
;;;// We first center, then change the scale

;;;width = gdk_surface_get_width (surface);
;;;height = gdk_surface_get_height (surface);
;;;radius = MIN (width, height) / 2.;

;;;cairo_translate (cr,
;;;                 radius + (width - 2 * radius) / 2,
;;;                 radius + (height - 2 * radius) / 2);
;;;                 cairo_scale (cr, radius / RADIUS, radius / RADIUS);

;;;// Create a PangoLayout, set the font and text
;;;context = gdk_pango_context_get_for_display (display);
;;;layout = pango_layout_new (context);
;;;pango_layout_set_text (layout, "Text", -1);
;;;desc = pango_font_description_from_string (FONT);
;;;pango_layout_set_font_description (layout, desc);
;;;pango_font_description_free (desc);

;;;// Draw the layout N_WORDS times in a circle
;;;for (i = 0; i < N_WORDS; i++)
;;;  {
;;;    double red, green, blue;
;;;    double angle = 2 * G_PI * i / n_words;

;;;    cairo_save (cr);

;;;    // Gradient from red at angle == 60 to blue at angle == 300
;;;    red = (1 + cos (angle - 60)) / 2;
;;;    green = 0;
;;;    blue = 1 - red;

;;;    cairo_set_source_rgb (cr, red, green, blue);
;;;    cairo_rotate (cr, angle);

;;;    // Inform Pango to re-layout the text with the new transformation matrix
;;;    pango_cairo_update_layout (cr, layout);

;;;    pango_layout_get_size (layout, &width, &height);

;;;    cairo_move_to (cr, - width / 2 / PANGO_SCALE, - DEFAULT_TEXT_RADIUS);
;;;    pango_cairo_show_layout (cr, layout);

;;;    cairo_restore (cr);
;;;  }

;;;g_object_unref (layout);
;;;g_object_unref (context);
;;;Output of the example above.


;;;Functions
;;;gdk_pango_layout_get_clip_region ()
;;;cairo_region_t *
;;;gdk_pango_layout_get_clip_region (PangoLayout *layout,
;;;                                  int x_origin,
;;;                                  int y_origin,
;;;                                  const int *index_ranges,
;;;                                  int n_ranges);
;;;Obtains a clip region which contains the areas where the given ranges of text would be drawn. x_origin and y_origin are the top left point to center the layout. index_ranges should contain ranges of bytes in the layout’s text.

;;;Note that the regions returned correspond to logical extents of the text ranges, not ink extents. So the drawn layout may in fact touch areas out of the clip region. The clip region is mainly useful for highlightling parts of text, such as when text is selected.

;;;[skip]

;;;Parameters
;;;layout

;;;a PangoLayout

;;;
;;;x_origin

;;;X pixel where you intend to draw the layout with this clip

;;;
;;;y_origin

;;;Y pixel where you intend to draw the layout with this clip

;;;
;;;index_ranges

;;;array of byte indexes into the layout, where even members of array are start indexes and odd elements are end indexes

;;;
;;;n_ranges

;;;number of ranges in index_ranges , i.e. half the size of index_ranges

;;;
;;;Returns
;;;a clip region containing the given ranges

;;;gdk_pango_layout_line_get_clip_region ()
;;;cairo_region_t *
;;;gdk_pango_layout_line_get_clip_region (PangoLayoutLine *line,
;;;                                       int x_origin,
;;;                                       int y_origin,
;;;                                       const int *index_ranges,
;;;                                       int n_ranges);
;;;Obtains a clip region which contains the areas where the given ranges of text would be drawn. x_origin and y_origin are the top left position of the layout. index_ranges should contain ranges of bytes in the layout’s text. The clip region will include space to the left or right of the line (to the layout bounding box) if you have indexes above or below the indexes contained inside the line. This is to draw the selection all the way to the side of the layout. However, the clip region is in line coordinates, not layout coordinates.

;;;Note that the regions returned correspond to logical extents of the text ranges, not ink extents. So the drawn line may in fact touch areas out of the clip region. The clip region is mainly useful for highlightling parts of text, such as when text is selected.

;;;[skip]

;;;Parameters
;;;line

;;;a PangoLayoutLine

;;;
;;;x_origin

;;;X pixel where you intend to draw the layout line with this clip

;;;
;;;y_origin

;;;baseline pixel where you intend to draw the layout line with this clip

;;;
;;;index_ranges

;;;array of byte indexes into the layout, where even members of array are start indexes and odd elements are end indexes.

;;;[array]
;;;n_ranges

;;;number of ranges in index_ranges , i.e. half the size of index_ranges

;;;
;;;Returns
;;;a clip region containing the given ranges

;;; --- End of file gdk.pango-interaction.lisp ---------------------------------
