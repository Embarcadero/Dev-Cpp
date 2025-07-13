(* cairo - a vector graphics library with display and print output
 *
 * Copyright © 2006 Red Hat, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it either under the terms of the GNU Lesser General Public
 * License version 2.1 as published by the Free Software Foundation
 * (the "LGPL") or, at your option, under the terms of the Mozilla
 * Public License Version 1.1 (the "MPL"). If you do not alter this
 * notice, a recipient may use your version of this file under either
 * the MPL or the LGPL.
 *
 * You should have received a copy of the LGPL along with this library
 * in the file COPYING-LGPL-2.1; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * You should have received a copy of the MPL along with this library
 * in the file COPYING-MPL-1.1
 *
 * The contents of this file are subject to the Mozilla Public License
 * Version 1.1 (the "License"); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * This software is distributed on an "AS IS" basis, WITHOUT WARRANTY
 * OF ANY KIND, either express or implied. See the LGPL or the MPL for
 * the specific language governing rights and limitations.
 *
 * The Original Code is the cairo graphics library.
 *
 * The Initial Developer of the Original Code is Red Hat, Inc.
 *
 * Contributor(s):
 * Carl D. Worth <cworth@cworth.org>
 *)

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
  {$PACKRECORDS C}
{$ELSE}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cairo.inc}

unit cairolib;

interface

{$ifdef WIN32}
uses windows;
{$endif}

{$ifdef UNIX}
uses x, xlib, xrender, freetypeh;
{$endif}

const
  CAIRO_VERSION_MAJOR = 1;
  CAIRO_VERSION_MINOR = 10;
  CAIRO_VERSION_MICRO = 0;

function cairo_version: Integer; cdecl;
function cairo_version_string: PAnsiChar; cdecl;

(**
 * cairo_bool_t:
 *
 * #cairo_bool_t is used for boolean values. Returns of type
 * #cairo_bool_t will always be either 0 or 1, but testing against
 * these values explicitly is not encouraged; just use the
 * value as a boolean condition.
 *
 * <informalexample><programlisting>
 *  if (cairo_in_stroke (cr, x, y)) {
 *      /<!-- -->* do something *<!-- -->/
 *  }
 * </programlisting></informalexample>
 **)
type
  cairo_bool_t = Integer;
  TCairoBool = type cairo_bool_t;

(**
 * cairo_t:
 *
 * A #cairo_t contains the current state of the rendering device,
 * including coordinates of yet to be drawn shapes.
 *
 * Cairo contexts, as #cairo_t objects are named, are central to
 * cairo and all drawing with cairo is always done to a #cairo_t
 * object.
 *
 * Memory management of #cairo_t is done with
 * cairo_reference() and cairo_destroy().
 **)
   PCairo = ^TCairo;
   _cairo = record
   end;
   cairo_t = _cairo;
   TCairo = type cairo_t;

(**
 * cairo_surface_t:
 *
 * A #cairo_surface_t represents an image, either as the destination
 * of a drawing operation or as source when drawing onto another
 * surface.  To draw to a #cairo_surface_t, create a cairo context
 * with the surface as the target, using cairo_create().
 *
 * There are different subtypes of #cairo_surface_t for
 * different drawing backends; for example, cairo_image_surface_create()
 * creates a bitmap image in memory.
 * The type of a surface can be queried with cairo_surface_get_type().
 *
 * The initial contents of a surface after creation depend upon the manner
 * of its creation. If cairo creates the surface and backing storage for
 * the user, it will be initially cleared; for example,
 * cairo_image_surface_create() and cairo_surface_create_similar().
 * Alternatively, if the user passes in a reference to some backing storage
 * and asks cairo to wrap that in a #cairo_surface_t, then the contents are
 * not modified; for example, cairo_image_surface_create_for_data() and
 * cairo_xlib_surface_create().
 *
 * Memory management of #cairo_surface_t is done with
 * cairo_surface_reference() and cairo_surface_destroy().
 **)
  PCairoSurface = ^TCairoSurface;
  _cairo_surface = record
  end;
  cairo_surface_t = _cairo_surface;
  TCairoSurface = type cairo_surface_t;

(**
 * cairo_device_t:
 *
 * A #cairo_device_t represents the driver interface for drawing
 * operations to a #cairo_surface_t.  There are different subtypes of
 * #cairo_device_t for different drawing backends; for example,
 * cairo_xcb_device_create() creates a device that wraps the connection
 * to an X Windows System using the XCB library.
 *
 * The type of a device can be queried with cairo_device_get_type().
 *
 * Memory management of #cairo_device_t is done with
 * cairo_device_reference() and cairo_device_destroy().
 *
 * Since: 1.10
 **)
PCairoDevice = ^TCairoDevice;
_cairo_device = record
end;
cairo_device_t = _cairo_device;
TCairoDevice = type cairo_device_t;

(**
 * cairo_pattern_t:
 *
 * A #cairo_pattern_t represents a source when drawing onto a
 * surface. There are different subtypes of #cairo_pattern_t,
 * for different types of sources; for example,
 * cairo_pattern_create_rgb() creates a pattern for a solid
 * opaque color.
 *
 * Other than various cairo_pattern_create_<emphasis>type</emphasis>()
 * functions, some of the pattern types can be implicitly created
 * using various cairo_set_source_<emphasis>type</emphasis>() functions;
 * for example cairo_set_source_rgb().
 *
 * The type of a pattern can be queried with cairo_pattern_get_type().
 *
 * Memory management of #cairo_pattern_t is done with
 * cairo_pattern_reference() and cairo_pattern_destroy().
 **)
   PCairoPattern = ^TCairoPattern;
   _cairo_pattern = record
   end;
   cairo_pattern_t = _cairo_pattern;
   TCairoPattern = type cairo_pattern_t;

(**
 * cairo_destroy_func_t:
 * @data: The data element being destroyed.
 *
 * #cairo_destroy_func_t the type of function which is called when a
 * data element is destroyed. It is passed the pointer to the data
 * element and should free any memory and resources allocated for it.
 **)
   cairo_destroy_func_t = procedure(data: Pointer); cdecl;
   TCairoDestroyFunc = type cairo_destroy_func_t;

(**
 * cairo_user_data_key_t:
 * @unused: not used; ignore.
 *
 * #cairo_user_data_key_t is used for attaching user data to cairo
 * data structures.  The actual contents of the struct is never used,
 * and there is no need to initialize the object; only the unique
 * address of a #cairo_data_key_t object is used.  Typically, you
 * would just use the address of a static #cairo_data_key_t object.
 **)
  PCairoUserDataKey = ^TCairoUserDataKey;
  _cairo_user_data_key = record
    unused: Integer;
  end;
  cairo_user_data_key_t = _cairo_user_data_key;
  TCairoUserDataKey = type cairo_user_data_key_t;

(**
 * cairo_status_t:
 * @CAIRO_STATUS_SUCCESS: no error has occurred
 * @CAIRO_STATUS_NO_MEMORY: out of memory
 * @CAIRO_STATUS_INVALID_RESTORE: cairo_restore() called without matching cairo_save()
 * @CAIRO_STATUS_INVALID_POP_GROUP: no saved group to pop, i.e. cairo_pop_group() without matching cairo_push_group()
 * @CAIRO_STATUS_NO_CURRENT_POINT: no current point defined
 * @CAIRO_STATUS_INVALID_MATRIX: invalid matrix (not invertible)
 * @CAIRO_STATUS_INVALID_STATUS: invalid value for an input #cairo_status_t
 * @CAIRO_STATUS_NULL_POINTER: %NULL pointer
 * @CAIRO_STATUS_INVALID_STRING: input string not valid UTF-8
 * @CAIRO_STATUS_INVALID_PATH_DATA: input path data not valid
 * @CAIRO_STATUS_READ_ERROR: error while reading from input stream
 * @CAIRO_STATUS_WRITE_ERROR: error while writing to output stream
 * @CAIRO_STATUS_SURFACE_FINISHED: target surface has been finished
 * @CAIRO_STATUS_SURFACE_TYPE_MISMATCH: the surface type is not appropriate for the operation
 * @CAIRO_STATUS_PATTERN_TYPE_MISMATCH: the pattern type is not appropriate for the operation
 * @CAIRO_STATUS_INVALID_CONTENT: invalid value for an input #cairo_content_t
 * @CAIRO_STATUS_INVALID_FORMAT: invalid value for an input #cairo_format_t
 * @CAIRO_STATUS_INVALID_VISUAL: invalid value for an input Visual*
 * @CAIRO_STATUS_FILE_NOT_FOUND: file not found
 * @CAIRO_STATUS_INVALID_DASH: invalid value for a dash setting
 * @CAIRO_STATUS_INVALID_DSC_COMMENT: invalid value for a DSC comment (Since 1.2)
 * @CAIRO_STATUS_INVALID_INDEX: invalid index passed to getter (Since 1.4)
 * @CAIRO_STATUS_CLIP_NOT_REPRESENTABLE: clip region not representable in desired format (Since 1.4)
 * @CAIRO_STATUS_TEMP_FILE_ERROR: error creating or writing to a temporary file (Since 1.6)
 * @CAIRO_STATUS_INVALID_STRIDE: invalid value for stride (Since 1.6)
 * @CAIRO_STATUS_FONT_TYPE_MISMATCH: the font type is not appropriate for the operation (Since 1.8)
 * @CAIRO_STATUS_USER_FONT_IMMUTABLE: the user-font is immutable (Since 1.8)
 * @CAIRO_STATUS_USER_FONT_ERROR: error occurred in a user-font callback function (Since 1.8)
 * @CAIRO_STATUS_NEGATIVE_COUNT: negative number used where it is not allowed (Since 1.8)
 * @CAIRO_STATUS_INVALID_CLUSTERS: input clusters do not represent the accompanying text and glyph array (Since 1.8)
 * @CAIRO_STATUS_INVALID_SLANT: invalid value for an input #cairo_font_slant_t (Since 1.8)
 * @CAIRO_STATUS_INVALID_WEIGHT: invalid value for an input #cairo_font_weight_t (Since 1.8)
 * @CAIRO_STATUS_INVALID_SIZE: invalid value (typically too big) for the size of the input (surface, pattern, etc.) (Since 1.10)
 * @CAIRO_STATUS_USER_FONT_NOT_IMPLEMENTED: user-font method not implemented (Since 1.10)
 * @CAIRO_STATUS_DEVICE_TYPE_MISMATCH: the device type is not appropriate for the operation (Since 1.10)
 * @CAIRO_STATUS_DEVICE_ERROR: an operation to the device caused an unspecified error (Since 1.10)
 * @CAIRO_STATUS_LAST_STATUS: this is a special value indicating the number of
 *   status values defined in this enumeration.  When using this value, note
 *   that the version of cairo at run-time may have additional status values
 *   defined than the value of this symbol at compile-time. (Since 1.10)
 *
 * #cairo_status_t is used to indicate errors that can occur when
 * using Cairo. In some cases it is returned directly by functions.
 * but when using #cairo_t, the last error, if any, is stored in
 * the context and can be retrieved with cairo_status().
 *
 * New entries may be added in future versions.  Use cairo_status_to_string()
 * to get a human-readable representation of an error message.
 **)
  _cairo_status = (
    CAIRO_STATUS_SUCCESS = 0,
    CAIRO_STATUS_NO_MEMORY,
    CAIRO_STATUS_INVALID_RESTORE,
    CAIRO_STATUS_INVALID_POP_GROUP,
    CAIRO_STATUS_NO_CURRENT_POINT,
    CAIRO_STATUS_INVALID_MATRIX,
    CAIRO_STATUS_INVALID_STATUS,
    CAIRO_STATUS_NULL_POINTER,
    CAIRO_STATUS_INVALID_STRING,
    CAIRO_STATUS_INVALID_PATH_DATA,
    CAIRO_STATUS_READ_ERROR,
    CAIRO_STATUS_WRITE_ERROR,
    CAIRO_STATUS_SURFACE_FINISHED,
    CAIRO_STATUS_SURFACE_TYPE_MISMATCH,
    CAIRO_STATUS_PATTERN_TYPE_MISMATCH,
    CAIRO_STATUS_INVALID_CONTENT,
    CAIRO_STATUS_INVALID_FORMAT,
    CAIRO_STATUS_INVALID_VISUAL,
    CAIRO_STATUS_FILE_NOT_FOUND,
    CAIRO_STATUS_INVALID_DASH,
    CAIRO_STATUS_INVALID_DSC_COMMENT,
    CAIRO_STATUS_INVALID_INDEX,
    CAIRO_STATUS_CLIP_NOT_REPRESENTABLE,
    CAIRO_STATUS_TEMP_FILE_ERROR,
    CAIRO_STATUS_INVALID_STRIDE,
    CAIRO_STATUS_FONT_TYPE_MISMATCH,
    CAIRO_STATUS_USER_FONT_IMMUTABLE,
    CAIRO_STATUS_USER_FONT_ERROR,
    CAIRO_STATUS_NEGATIVE_COUNT,
    CAIRO_STATUS_INVALID_CLUSTERS,
    CAIRO_STATUS_INVALID_SLANT,
    CAIRO_STATUS_INVALID_WEIGHT,
    CAIRO_STATUS_INVALID_SIZE,
    CAIRO_STATUS_USER_FONT_NOT_IMPLEMENTED,
    CAIRO_STATUS_DEVICE_TYPE_MISMATCH,
    CAIRO_STATUS_DEVICE_ERROR,

    CAIRO_STATUS_LAST_STATUS
  );
  cairo_status_t = _cairo_status;
  TCairoStatus = type cairo_status_t;

(**
 * cairo_matrix_t:
 * @xx: xx component of the affine transformation
 * @yx: yx component of the affine transformation
 * @xy: xy component of the affine transformation
 * @yy: yy component of the affine transformation
 * @x0: X translation component of the affine transformation
 * @y0: Y translation component of the affine transformation
 *
 * A #cairo_matrix_t holds an affine transformation, such as a scale,
 * rotation, shear, or a combination of those. The transformation of
 * a point (x, y) is given by:
 * <programlisting>
 *     x_new = xx * x + xy * y + x0;
 *     y_new = yx * x + yy * y + y0;
 * </programlisting>
 **)

  PCairoMatrix = ^TCairoMatrix;
  TCairoMatrix = object
    xx: Double;
    yx: Double;
    xy: Double;
    yy: Double;
    x0: Double;
    y0: Double;
    procedure Init(axx, ayx, axy, ayy, ax0, ay0: Double);
    procedure InitIdentity;
    procedure InitTranslate(tx, ty: Double);
    procedure InitScale(sx, sy: Double);
    procedure InitRotate(radians: Double);
    procedure Translate(tx, ty: Double);
    procedure Scale(sx, sy: Double);
    procedure Rotate(radians: Double);
    function Invert: TCairoStatus;
    procedure Multiply(const a, b: TCairoMatrix);
    procedure TransformDistance(var dx, dy: Double);
    procedure TransformPoint(var x, y: Double);
  end;
  _cairo_matrix = TCairoMatrix;
  cairo_matrix_t = _cairo_matrix;

(**
 * cairo_content_t:
 * @CAIRO_CONTENT_COLOR: The surface will hold color content only.
 * @CAIRO_CONTENT_ALPHA: The surface will hold alpha content only.
 * @CAIRO_CONTENT_COLOR_ALPHA: The surface will hold color and alpha content.
 *
 * #cairo_content_t is used to describe the content that a surface will
 * contain, whether color information, alpha information (translucence
 * vs. opacity), or both.
 *
 * Note: The large values here are designed to keep #cairo_content_t
 * values distinct from #cairo_format_t values so that the
 * implementation can detect the error if users confuse the two types.
 **)
 _cairo_content = (
    CAIRO_CONTENT_COLOR      = $1000,
    CAIRO_CONTENT_ALPHA      = $2000,
    CAIRO_CONTENT_COLOR_ALPHA = $3000
 );
 cairo_content_t = _cairo_content;
 TCairoContent = type cairo_content_t;

(**
 * cairo_write_func_t:
 * @closure: the output closure
 * @data: the buffer containing the data to write
 * @length: the amount of data to write
 *
 * #cairo_write_func_t is the type of function which is called when a
 * backend needs to write data to an output stream.  It is passed the
 * closure which was specified by the user at the time the write
 * function was registered, the data to write and the length of the
 * data in bytes.  The write function should return
 * %CAIRO_STATUS_SUCCESS if all the data was successfully written,
 * %CAIRO_STATUS_WRITE_ERROR otherwise.
 *
 * Returns: the status code of the write operation
 **)
  cairo_write_func_t = function(closure: Pointer; data: PByte; length: Cardinal): TCairoStatus; cdecl;
  TCairoWriteFunc = type cairo_write_func_t;

(**
 * cairo_read_func_t:
 * @closure: the input closure
 * @data: the buffer into which to read the data
 * @length: the amount of data to read
 *
 * #cairo_read_func_t is the type of function which is called when a
 * backend needs to read data from an input stream.  It is passed the
 * closure which was specified by the user at the time the read
 * function was registered, the buffer to read the data into and the
 * length of the data in bytes.  The read function should return
 * %CAIRO_STATUS_SUCCESS if all the data was successfully read,
 * %CAIRO_STATUS_READ_ERROR otherwise.
 *
 * Returns: the status code of the read operation
 **)
cairo_read_func_t = function(closure: Pointer; data: PAnsiChar; length: Cardinal): TCairoStatus; cdecl;
TCairoReadFunc = type cairo_read_func_t;

(* Functions for manipulating state objects *)

function cairo_create(target: PCairoSurface): PCairo; cdecl;
function cairo_reference(cr: PCairo): PCairo; cdecl;
procedure cairo_destroy(cr: PCairo); cdecl;
function cairo_get_reference_count(cr: PCairo): Cardinal; cdecl;
function cairo_get_user_data(cr: PCairo; const key: PCairoUserDataKey): Pointer; cdecl;
function cairo_set_user_data(cr: PCairo; const key: PCairoUserDataKey; user_data: Pointer; destroy: TCairoDestroyFunc): TCairoStatus; cdecl;
procedure cairo_save(cr: PCairo); cdecl;
procedure cairo_restore(cr: PCairo); cdecl;
procedure cairo_push_group(cr: PCairo); cdecl;
procedure cairo_push_group_with_content(cr: PCairo; content: TCairoContent); cdecl;
function cairo_pop_group(cr: PCairo): PCairoPattern; cdecl;
procedure cairo_pop_group_to_source(cr: PCairo); cdecl;

(* Modify state *)

(**
 * cairo_operator_t:
 * @CAIRO_OPERATOR_CLEAR: clear destination layer (bounded)
 * @CAIRO_OPERATOR_SOURCE: replace destination layer (bounded)
 * @CAIRO_OPERATOR_OVER: draw source layer on top of destination layer
 * (bounded)
 * @CAIRO_OPERATOR_IN: draw source where there was destination content
 * (unbounded)
 * @CAIRO_OPERATOR_OUT: draw source where there was no destination
 * content (unbounded)
 * @CAIRO_OPERATOR_ATOP: draw source on top of destination content and
 * only there
 * @CAIRO_OPERATOR_DEST: ignore the source
 * @CAIRO_OPERATOR_DEST_OVER: draw destination on top of source
 * @CAIRO_OPERATOR_DEST_IN: leave destination only where there was
 * source content (unbounded)
 * @CAIRO_OPERATOR_DEST_OUT: leave destination only where there was no
 * source content
 * @CAIRO_OPERATOR_DEST_ATOP: leave destination on top of source content
 * and only there (unbounded)
 * @CAIRO_OPERATOR_XOR: source and destination are shown where there is only
 * one of them
 * @CAIRO_OPERATOR_ADD: source and destination layers are accumulated
 * @CAIRO_OPERATOR_SATURATE: like over, but assuming source and dest are
 * disjoint geometries
 * @CAIRO_OPERATOR_MULTIPLY: source and destination layers are multiplied.
 * This causes the result to be at least as dark as the darker inputs.
 * @CAIRO_OPERATOR_SCREEN: source and destination are complemented and
 * multiplied. This causes the result to be at least as light as the lighter
 * inputs.
 * @CAIRO_OPERATOR_OVERLAY: multiplies or screens, depending on the
 * lightness of the destination color.
 * @CAIRO_OPERATOR_DARKEN: replaces the destination with the source if it
 * is darker, otherwise keeps the source.
 * @CAIRO_OPERATOR_LIGHTEN: replaces the destination with the source if it
 * is lighter, otherwise keeps the source.
 * @CAIRO_OPERATOR_COLOR_DODGE: brightens the destination color to reflect
 * the source color.
 * @CAIRO_OPERATOR_COLOR_BURN: darkens the destination color to reflect
 * the source color.
 * @CAIRO_OPERATOR_HARD_LIGHT: Multiplies or screens, dependant on source
 * color.
 * @CAIRO_OPERATOR_SOFT_LIGHT: Darkens or lightens, dependant on source
 * color.
 * @CAIRO_OPERATOR_DIFFERENCE: Takes the difference of the source and
 * destination color.
 * @CAIRO_OPERATOR_EXCLUSION: Produces an effect similar to difference, but
 * with lower contrast.
 * @CAIRO_OPERATOR_HSL_HUE: Creates a color with the hue of the source
 * and the saturation and luminosity of the target.
 * @CAIRO_OPERATOR_HSL_SATURATION: Creates a color with the saturation
 * of the source and the hue and luminosity of the target. Painting with
 * this mode onto a gray area prduces no change.
 * @CAIRO_OPERATOR_HSL_COLOR: Creates a color with the hue and saturation
 * of the source and the luminosity of the target. This preserves the gray
 * levels of the target and is useful for coloring monochrome images or
 * tinting color images.
 * @CAIRO_OPERATOR_HSL_LUMINOSITY: Creates a color with the luminosity of
 * the source and the hue and saturation of the target. This produces an
 * inverse effect to @CAIRO_OPERATOR_HSL_COLOR.
 *
 * #cairo_operator_t is used to set the compositing operator for all cairo
 * drawing operations.
 *
 * The default operator is %CAIRO_OPERATOR_OVER.
 *
 * The operators marked as <firstterm>unbounded</firstterm> modify their
 * destination even outside of the mask layer (that is, their effect is not
 * bound by the mask layer).  However, their effect can still be limited by
 * way of clipping.
 *
 * To keep things simple, the operator descriptions here
 * document the behavior for when both source and destination are either fully
 * transparent or fully opaque.  The actual implementation works for
 * translucent layers too.
 * For a more detailed explanation of the effects of each operator, including
 * the mathematical definitions, see
 * <ulink url="http://cairographics.org/operators/">http://cairographics.org/operators/</ulink>.
 **)
type
  _cairo_operator = (
    CAIRO_OPERATOR_CLEAR,

    CAIRO_OPERATOR_SOURCE,
    CAIRO_OPERATOR_OVER,
    CAIRO_OPERATOR_IN,
    CAIRO_OPERATOR_OUT,
    CAIRO_OPERATOR_ATOP,

    CAIRO_OPERATOR_DEST,
    CAIRO_OPERATOR_DEST_OVER,
    CAIRO_OPERATOR_DEST_IN,
    CAIRO_OPERATOR_DEST_OUT,
    CAIRO_OPERATOR_DEST_ATOP,

    CAIRO_OPERATOR_XOR,
    CAIRO_OPERATOR_ADD,
    CAIRO_OPERATOR_SATURATE,

    CAIRO_OPERATOR_MULTIPLY,
    CAIRO_OPERATOR_SCREEN,
    CAIRO_OPERATOR_OVERLAY,
    CAIRO_OPERATOR_DARKEN,
    CAIRO_OPERATOR_LIGHTEN,
    CAIRO_OPERATOR_COLOR_DODGE,
    CAIRO_OPERATOR_COLOR_BURN,
    CAIRO_OPERATOR_HARD_LIGHT,
    CAIRO_OPERATOR_SOFT_LIGHT,
    CAIRO_OPERATOR_DIFFERENCE,
    CAIRO_OPERATOR_EXCLUSION,
    CAIRO_OPERATOR_HSL_HUE,
    CAIRO_OPERATOR_HSL_SATURATION,
    CAIRO_OPERATOR_HSL_COLOR,
    CAIRO_OPERATOR_HSL_LUMINOSITY
  );
  cairo_operator_t = _cairo_operator;
  TCairoOperator = type cairo_operator_t;

procedure cairo_set_operator(cr: PCairo; op: TCairoOperator); cdecl;
procedure cairo_set_source(cr: PCairo; source: PCairoPattern); cdecl;
procedure cairo_set_source_rgb(cr: PCairo; red, green, blue: Double); cdecl;
procedure cairo_set_source_rgba(cr: PCairo; red, green, blue, alpha: Double); cdecl;
procedure cairo_set_source_surface(cr: PCairo;  surface: PCairoSurface; x, y: Double); cdecl;
procedure cairo_set_tolerance(cr: PCairo; tolerance: Double); cdecl;

(**
 * cairo_antialias_t:
 * @CAIRO_ANTIALIAS_DEFAULT: Use the default antialiasing for
 *   the subsystem and target device
 * @CAIRO_ANTIALIAS_NONE: Use a bilevel alpha mask
 * @CAIRO_ANTIALIAS_GRAY: Perform single-color antialiasing (using
 *  shades of gray for black text on a white background, for example).
 * @CAIRO_ANTIALIAS_SUBPIXEL: Perform antialiasing by taking
 *  advantage of the order of subpixel elements on devices
 *  such as LCD panels
 *
 * Specifies the type of antialiasing to do when rendering text or shapes.
 **)
type
  _cairo_antialias = (
    CAIRO_ANTIALIAS_DEFAULT,
    CAIRO_ANTIALIAS_NONE,
    CAIRO_ANTIALIAS_GRAY,
    CAIRO_ANTIALIAS_SUBPIXEL
  );
  cairo_antialias_t = _cairo_antialias;
  TCairoAntialias = type cairo_antialias_t;

procedure cairo_set_antialias(cr: PCairo; antialias: TCairoAntialias); cdecl;

(**
 * cairo_fill_rule_t:
 * @CAIRO_FILL_RULE_WINDING: If the path crosses the ray from
 * left-to-right, counts +1. If the path crosses the ray
 * from right to left, counts -1. (Left and right are determined
 * from the perspective of looking along the ray from the starting
 * point.) If the total count is non-zero, the point will be filled.
 * @CAIRO_FILL_RULE_EVEN_ODD: Counts the total number of
 * intersections, without regard to the orientation of the contour. If
 * the total number of intersections is odd, the point will be
 * filled.
 *
 * #cairo_fill_rule_t is used to select how paths are filled. For both
 * fill rules, whether or not a point is included in the fill is
 * determined by taking a ray from that point to infinity and looking
 * at intersections with the path. The ray can be in any direction,
 * as long as it doesn't pass through the end point of a segment
 * or have a tricky intersection such as intersecting tangent to the path.
 * (Note that filling is not actually implemented in this way. This
 * is just a description of the rule that is applied.)
 *
 * The default fill rule is %CAIRO_FILL_RULE_WINDING.
 *
 * New entries may be added in future versions.
 **)
type
  _cairo_fill_rule = (
    CAIRO_FILL_RULE_WINDING,
    CAIRO_FILL_RULE_EVEN_ODD
  );
  cairo_fill_rule_t = _cairo_fill_rule;
  TCairoFillRule = type cairo_fill_rule_t;

procedure cairo_set_fill_rule(cr: PCairo; fill_rule: TCairoFillRule); cdecl;
procedure cairo_set_line_width(cr: PCairo; width: Double); cdecl;

(**
 * cairo_line_cap_t:
 * @CAIRO_LINE_CAP_BUTT: start(stop) the line exactly at the start(end) point
 * @CAIRO_LINE_CAP_ROUND: use a round ending, the center of the circle is the end point
 * @CAIRO_LINE_CAP_SQUARE: use squared ending, the center of the square is the end point
 *
 * Specifies how to render the endpoints of the path when stroking.
 *
 * The default line cap style is %CAIRO_LINE_CAP_BUTT.
 **)
type
  _cairo_line_cap = (
    CAIRO_LINE_CAP_BUTT,
    CAIRO_LINE_CAP_ROUND,
    CAIRO_LINE_CAP_SQUARE
  );
  cairo_line_cap_t = _cairo_line_cap;
  TCairoLineCap = type cairo_line_cap_t;

procedure cairo_set_line_cap(cr: PCairo; line_cap: TCairoLineCap); cdecl;

(**
 * cairo_line_join_t:
 * @CAIRO_LINE_JOIN_MITER: use a sharp (angled) corner, see
 * cairo_set_miter_limit()
 * @CAIRO_LINE_JOIN_ROUND: use a rounded join, the center of the circle is the
 * joint point
 * @CAIRO_LINE_JOIN_BEVEL: use a cut-off join, the join is cut off at half
 * the line width from the joint point
 *
 * Specifies how to render the junction of two lines when stroking.
 *
 * The default line join style is %CAIRO_LINE_JOIN_MITER.
 **)
type
  _cairo_line_join = (
    CAIRO_LINE_JOIN_MITER,
    CAIRO_LINE_JOIN_ROUND,
    CAIRO_LINE_JOIN_BEVEL
  );
  cairo_line_join_t = _cairo_line_join;
  TCairoLineJoin = type cairo_line_join_t;

procedure cairo_set_line_join(cr: PCairo; line_join: TCairoLineJoin); cdecl;
procedure cairo_set_dash(cr: PCairo; const dashes: PDouble; num_dashes: Integer; offset: Double); cdecl;
procedure cairo_set_miter_limit(cr: PCairo; limit: Double); cdecl;
procedure cairo_translate(cr: PCairo; tx, ty: Double); cdecl;
procedure cairo_scale(cr: PCairo; sx, sy: Double); cdecl;
procedure cairo_rotate(cr: PCairo; angle: Double); cdecl;
procedure cairo_transform(cr: PCairo; const matrix: PCairoMatrix); cdecl;
procedure cairo_set_matrix(cr: PCairo; const matrix: PCairoMatrix); cdecl;
procedure cairo_identity_matrix(cr: PCairo); cdecl;
procedure cairo_user_to_device(cr: PCairo; x, y: PDouble); cdecl;
procedure cairo_user_to_device_distance(cr: PCairo; dx, dy: PDouble); cdecl;
procedure cairo_device_to_user(cr: PCairo; x, y: PDouble); cdecl;
procedure cairo_device_to_user_distance(cr: PCairo; dx, dy: PDouble); cdecl;

(* Path creation functions *)

procedure cairo_new_path(cr: PCairo); cdecl;
procedure cairo_move_to(cr: PCairo; x, y: Double); cdecl;
procedure cairo_new_sub_path(cr: PCairo); cdecl;
procedure cairo_line_to(cr: PCairo; x, y: Double); cdecl;
procedure cairo_curve_to(cr: PCairo; x1, y1, x2, y2, x3, y3: Double); cdecl;
procedure cairo_arc(cr: PCairo; xc, yc, radius, angle1, angle2: Double); cdecl;
procedure cairo_arc_negative(cr: PCairo; xc, yc, radius, angle1, angle2: Double); cdecl;
procedure cairo_rel_move_to(cr: PCairo; dx, dy: Double); cdecl;
procedure cairo_rel_line_to(cr: PCairo; dx, dy: Double); cdecl;
procedure cairo_rel_curve_to(cr: PCairo; dx1, dy1, dx2, dy2, dx3, dy3: Double); cdecl;
procedure cairo_rectangle(cr: PCairo; x, y, width, height: Double); cdecl;
procedure cairo_close_path(cr: PCairo); cdecl;
procedure cairo_path_extents(cr: PCairo; x1, y1, x2, y2: PDouble); cdecl;

(* Painting functions *)

procedure cairo_paint(cr: PCairo); cdecl;
procedure cairo_paint_with_alpha(cr: PCairo; alpha: Double); cdecl;
procedure cairo_mask(cr: PCairo; pattern: PCairoPattern); cdecl;
procedure cairo_mask_surface(cr: PCairo; surface: PCairoSurface; surface_x, surface_y: Double); cdecl;
procedure cairo_stroke(cr: PCairo); cdecl;
procedure cairo_stroke_preserve(cr: PCairo); cdecl;
procedure cairo_fill(cr: PCairo); cdecl;
procedure cairo_fill_preserve(cr: PCairo); cdecl;
procedure cairo_copy_page(cr: PCairo); cdecl;
procedure cairo_show_page(cr: PCairo); cdecl;

(* Insideness testing *)

function cairo_in_stroke(cr: PCairo; x, y: Double): TCairoBool; cdecl;
function cairo_in_fill(cr: PCairo; x, y: Double ): TCairoBool; cdecl;
function cairo_in_clip(cr: PCairo; x, y: Double): TCairoBool; cdecl;


(* Rectangular extents *)

procedure cairo_stroke_extents(cr: PCairo; x1, y1, x2, y2: PDouble); cdecl;
procedure cairo_fill_extents(cr: PCairo; x1, y1, x2, y2: PDouble); cdecl;

(* Clipping *)

procedure cairo_reset_clip(cr: PCairo); cdecl;
procedure cairo_clip(cr: PCairo); cdecl;
procedure cairo_clip_preserve(cr: PCairo); cdecl;
procedure cairo_clip_extents(cr: PCairo; x1, y1, x2, y2: PDouble); cdecl;

(**
 * cairo_rectangle_t:
 * @x: X coordinate of the left side of the rectangle
 * @y: Y coordinate of the the top side of the rectangle
 * @width: width of the rectangle
 * @height: height of the rectangle
 *
 * A data structure for holding a rectangle.
 *
 * Since: 1.4
 **)
type
  PCairoRectangle = ^TCairoRectangle;
  _cairo_rectangle = record
    x, y, width, height: Double;
  end;
  cairo_rectangle_t = _cairo_rectangle;
  TCairoRectangle = type cairo_rectangle_t;

(**
 * cairo_rectangle_list_t:
 * @status: Error status of the rectangle list
 * @rectangles: Array containing the rectangles
 * @num_rectangles: Number of rectangles in this list
 *
 * A data structure for holding a dynamically allocated
 * array of rectangles.
 *
 * Since: 1.4
 **)
  PCairoRectangleList = ^TCairoRectangleList;
  _cairo_rectangle_list = record
    status: TCairoStatus;
    rectangles: PCairoRectangle;
    num_rectangles: Integer;
  end;
  cairo_rectangle_list_t = _cairo_rectangle_list;
  TCairoRectangleList = type cairo_rectangle_list_t;

function cairo_copy_clip_rectangle_list(cr: PCairo): PCairoRectangleList; cdecl;
procedure cairo_rectangle_list_destroy(rectangle_list: PCairoRectangleList); cdecl;

(* Font/Text functions *)

(**
 * cairo_scaled_font_t:
 *
 * A #cairo_scaled_font_t is a font scaled to a particular size and device
 * resolution. A #cairo_scaled_font_t is most useful for low-level font
 * usage where a library or application wants to cache a reference
 * to a scaled font to speed up the computation of metrics.
 *
 * There are various types of scaled fonts, depending on the
 * <firstterm>font backend</firstterm> they use. The type of a
 * scaled font can be queried using cairo_scaled_font_get_type().
 *
 * Memory management of #cairo_scaled_font_t is done with
 * cairo_scaled_font_reference() and cairo_scaled_font_destroy().
 **)
type
  PCairoScaledFont = ^TCairoScaledFont;
  _cairo_scaled_font = record
  end;
  cairo_scaled_font_t = _cairo_scaled_font;
  TCairoScaledFont = type cairo_scaled_font_t;

(**
 * cairo_font_face_t:
 *
 * A #cairo_font_face_t specifies all aspects of a font other
 * than the size or font matrix (a font matrix is used to distort
 * a font by sheering it or scaling it unequally in the two
 * directions) . A font face can be set on a #cairo_t by using
 * cairo_set_font_face(); the size and font matrix are set with
 * cairo_set_font_size() and cairo_set_font_matrix().
 *
 * There are various types of font faces, depending on the
 * <firstterm>font backend</firstterm> they use. The type of a
 * font face can be queried using cairo_font_face_get_type().
 *
 * Memory management of #cairo_font_face_t is done with
 * cairo_font_face_reference() and cairo_font_face_destroy().
 **)
  PCairoFontFace = ^TCairoFontFace;
  _cairo_font_face = record
  end;
  cairo_font_face_t = _cairo_font_face;
  TCairoFontFace = type cairo_font_face_t;

(**
 * cairo_glyph_t:
 * @index: glyph index in the font. The exact interpretation of the
 *      glyph index depends on the font technology being used.
 * @x: the offset in the X direction between the origin used for
 *     drawing or measuring the string and the origin of this glyph.
 * @y: the offset in the Y direction between the origin used for
 *     drawing or measuring the string and the origin of this glyph.
 *
 * The #cairo_glyph_t structure holds information about a single glyph
 * when drawing or measuring text. A font is (in simple terms) a
 * collection of shapes used to draw text. A glyph is one of these
 * shapes. There can be multiple glyphs for a single character
 * (alternates to be used in different contexts, for example), or a
 * glyph can be a <firstterm>ligature</firstterm> of multiple
 * characters. Cairo doesn't expose any way of converting input text
 * into glyphs, so in order to use the Cairo interfaces that take
 * arrays of glyphs, you must directly access the appropriate
 * underlying font system.
 *
 * Note that the offsets given by @x and @y are not cumulative. When
 * drawing or measuring text, each glyph is individually positioned
 * with respect to the overall origin
 **)
  PPCairoGlyph = ^PCairoGlyph;
  PCairoGlyph = ^TCairoGlyph;
  cairo_glyph_t = record
    index: Cardinal;
    x, y: Double;
  end;
  TCairoGlyph = type cairo_glyph_t;

  TCairoGlyphArray = array[0..high(Integer) div sizeof(TCairoGlyph) -1] of TCairoGlyph;
  PCairoGlyphArray = ^TCairoGlyphArray;


function cairo_glyph_allocate(num_glyphs: Integer): PCairoGlyph; cdecl;
procedure cairo_glyph_free(glyphs: PCairoGlyph); cdecl;

(**
 * cairo_text_cluster_t:
 * @num_bytes: the number of bytes of UTF-8 text covered by cluster
 * @num_glyphs: the number of glyphs covered by cluster
 *
 * The #cairo_text_cluster_t structure holds information about a single
 * <firstterm>text cluster</firstterm>.  A text cluster is a minimal
 * mapping of some glyphs corresponding to some UTF-8 text.
 *
 * For a cluster to be valid, both @num_bytes and @num_glyphs should
 * be non-negative, and at least one should be non-zero.
 * Note that clusters with zero glyphs are not as well supported as
 * normal clusters.  For example, PDF rendering applications typically
 * ignore those clusters when PDF text is being selected.
 *
 * See cairo_show_text_glyphs() for how clusters are used in advanced
 * text operations.
 *
 * Since: 1.8
 **)
type
  PPCairoTextCluster = ^PCairoTextCluster;
  PCairoTextCluster = ^TCairoTextCluster;
  cairo_text_cluster_t = record
    num_bytes: Integer;
    num_glyphs: Integer;
  end;
  TCairoTextCluster = type cairo_text_cluster_t;

  TCairoTextClusterArray = array[0..high(Integer) div sizeof(TCairoTextCluster) -1] of TCairoTextCluster;
  PCairoTextClusterArray = ^TCairoTextClusterArray;

function cairo_text_cluster_allocate(num_clusters: Integer): PCairoTextCluster; cdecl;
procedure cairo_text_cluster_free(clusters: PCairoTextCluster); cdecl;

(**
 * cairo_text_cluster_flags_t:
 * @CAIRO_TEXT_CLUSTER_FLAG_BACKWARD: The clusters in the cluster array
 * map to glyphs in the glyph array from end to start.
 *
 * Specifies properties of a text cluster mapping.
 *
 * Since: 1.8
 **)
type
  PCairoTextClusterFlags = ^TCairoTextClusterFlags;
  _cairo_text_cluster_flags = (
    CAIRO_TEXT_CLUSTER_FLAG_BACKWARD = $00000001
  );
  cairo_text_cluster_flags_t = _cairo_text_cluster_flags;
  TCairoTextClusterFlags = type cairo_text_cluster_flags_t;

(**
 * cairo_text_extents_t:
 * @x_bearing: the horizontal distance from the origin to the
 *   leftmost part of the glyphs as drawn. Positive if the
 *   glyphs lie entirely to the right of the origin.
 * @y_bearing: the vertical distance from the origin to the
 *   topmost part of the glyphs as drawn. Positive only if the
 *   glyphs lie completely below the origin; will usually be
 *   negative.
 * @width: width of the glyphs as drawn
 * @height: height of the glyphs as drawn
 * @x_advance:distance to advance in the X direction
 *    after drawing these glyphs
 * @y_advance: distance to advance in the Y direction
 *   after drawing these glyphs. Will typically be zero except
 *   for vertical text layout as found in East-Asian languages.
 *
 * The #cairo_text_extents_t structure stores the extents of a single
 * glyph or a string of glyphs in user-space coordinates. Because text
 * extents are in user-space coordinates, they are mostly, but not
 * entirely, independent of the current transformation matrix. If you call
 * <literal>cairo_scale(cr, 2.0, 2.0)</literal>, text will
 * be drawn twice as big, but the reported text extents will not be
 * doubled. They will change slightly due to hinting (so you can't
 * assume that metrics are independent of the transformation matrix),
 * but otherwise will remain unchanged.
 **)

  PCairoTextExtents = ^TCairoTextExtents;
  cairo_text_extents_t = record
    x_bearing: Double;
    y_bearing: Double;
    width: Double;
    height: Double;
    x_advance: Double;
    y_advance: Double;
  end;
  TCairoTextExtents = type cairo_text_extents_t;

(**
 * cairo_font_extents_t:
 * @ascent: the distance that the font extends above the baseline.
 *          Note that this is not always exactly equal to the maximum
 *          of the extents of all the glyphs in the font, but rather
 *          is picked to express the font designer's intent as to
 *          how the font should align with elements above it.
 * @descent: the distance that the font extends below the baseline.
 *           This value is positive for typical fonts that include
 *           portions below the baseline. Note that this is not always
 *           exactly equal to the maximum of the extents of all the
 *           glyphs in the font, but rather is picked to express the
 *           font designer's intent as to how the the font should
 *           align with elements below it.
 * @height: the recommended vertical distance between baselines when
 *          setting consecutive lines of text with the font. This
 *          is greater than @ascent+@descent by a
 *          quantity known as the <firstterm>line spacing</firstterm>
 *          or <firstterm>external leading</firstterm>. When space
 *          is at a premium, most fonts can be set with only
 *          a distance of @ascent+@descent between lines.
 * @max_x_advance: the maximum distance in the X direction that
 *         the the origin is advanced for any glyph in the font.
 * @max_y_advance: the maximum distance in the Y direction that
 *         the the origin is advanced for any glyph in the font.
 *         this will be zero for normal fonts used for horizontal
 *         writing. (The scripts of East Asia are sometimes written
 *         vertically.)
 *
 * The #cairo_font_extents_t structure stores metric information for
 * a font. Values are given in the current user-space coordinate
 * system.
 *
 * Because font metrics are in user-space coordinates, they are
 * mostly, but not entirely, independent of the current transformation
 * matrix. If you call <literal>cairo_scale(cr, 2.0, 2.0)</literal>,
 * text will be drawn twice as big, but the reported text extents will
 * not be doubled. They will change slightly due to hinting (so you
 * can't assume that metrics are independent of the transformation
 * matrix), but otherwise will remain unchanged.
 **)
  PCairoFontExtents = ^TCairoFontExtents;
  cairo_font_extents_t = record
    ascent: Double;
    descent: Double;
    height: Double;
    max_x_advance: Double;
    max_y_advance: Double;
  end;
  TCairoFontExtents = type cairo_font_extents_t;

(**
 * cairo_font_slant_t:
 * @CAIRO_FONT_SLANT_NORMAL: Upright font style
 * @CAIRO_FONT_SLANT_ITALIC: Italic font style
 * @CAIRO_FONT_SLANT_OBLIQUE: Oblique font style
 *
 * Specifies variants of a font face based on their slant.
 **)
  _cairo_font_slant = (
    CAIRO_FONT_SLANT_NORMAL,
    CAIRO_FONT_SLANT_ITALIC,
    CAIRO_FONT_SLANT_OBLIQUE
  );
  cairo_font_slant_t = _cairo_font_slant;
  TCairoFontSlant = type cairo_font_slant_t;

(**
 * cairo_font_weight_t:
 * @CAIRO_FONT_WEIGHT_NORMAL: Normal font weight
 * @CAIRO_FONT_WEIGHT_BOLD: Bold font weight
 *
 * Specifies variants of a font face based on their weight.
 **)
  _cairo_font_weight = (
    CAIRO_FONT_WEIGHT_NORMAL,
    CAIRO_FONT_WEIGHT_BOLD
  );
  cairo_font_weight_t = _cairo_font_weight;
  TCairoFontWeight = type cairo_font_weight_t;

(**
 * cairo_subpixel_order_t:
 * @CAIRO_SUBPIXEL_ORDER_DEFAULT: Use the default subpixel order for
 *   for the target device
 * @CAIRO_SUBPIXEL_ORDER_RGB: Subpixel elements are arranged horizontally
 *   with red at the left
 * @CAIRO_SUBPIXEL_ORDER_BGR:  Subpixel elements are arranged horizontally
 *   with blue at the left
 * @CAIRO_SUBPIXEL_ORDER_VRGB: Subpixel elements are arranged vertically
 *   with red at the top
 * @CAIRO_SUBPIXEL_ORDER_VBGR: Subpixel elements are arranged vertically
 *   with blue at the top
 *
 * The subpixel order specifies the order of color elements within
 * each pixel on the display device when rendering with an
 * antialiasing mode of %CAIRO_ANTIALIAS_SUBPIXEL.
 **)
  _cairo_subpixel_order = (
    CAIRO_SUBPIXEL_ORDER_DEFAULT,
    CAIRO_SUBPIXEL_ORDER_RGB,
    CAIRO_SUBPIXEL_ORDER_BGR,
    CAIRO_SUBPIXEL_ORDER_VRGB,
    CAIRO_SUBPIXEL_ORDER_VBGR
  );
  cairo_subpixel_order_t = _cairo_subpixel_order;
  TCairoSubpixelOrder = type cairo_subpixel_order_t;

(**
 * cairo_hint_style_t:
 * @CAIRO_HINT_STYLE_DEFAULT: Use the default hint style for
 *   font backend and target device
 * @CAIRO_HINT_STYLE_NONE: Do not hint outlines
 * @CAIRO_HINT_STYLE_SLIGHT: Hint outlines slightly to improve
 *   contrast while retaining good fidelity to the original
 *   shapes.
 * @CAIRO_HINT_STYLE_MEDIUM: Hint outlines with medium strength
 *   giving a compromise between fidelity to the original shapes
 *   and contrast
 * @CAIRO_HINT_STYLE_FULL: Hint outlines to maximize contrast
 *
 * Specifies the type of hinting to do on font outlines. Hinting
 * is the process of fitting outlines to the pixel grid in order
 * to improve the appearance of the result. Since hinting outlines
 * involves distorting them, it also reduces the faithfulness
 * to the original outline shapes. Not all of the outline hinting
 * styles are supported by all font backends.
 *
 * New entries may be added in future versions.
 **)
  _cairo_hint_style = (
    CAIRO_HINT_STYLE_DEFAULT,
    CAIRO_HINT_STYLE_NONE,
    CAIRO_HINT_STYLE_SLIGHT,
    CAIRO_HINT_STYLE_MEDIUM,
    CAIRO_HINT_STYLE_FULL
  );
  cairo_hint_style_t = _cairo_hint_style;
  TCairoHintStyle = type cairo_hint_style_t;

(**
 * cairo_hint_metrics_t:
 * @CAIRO_HINT_METRICS_DEFAULT: Hint metrics in the default
 *  manner for the font backend and target device
 * @CAIRO_HINT_METRICS_OFF: Do not hint font metrics
 * @CAIRO_HINT_METRICS_ON: Hint font metrics
 *
 * Specifies whether to hint font metrics; hinting font metrics
 * means quantizing them so that they are integer values in
 * device space. Doing this improves the consistency of
 * letter and line spacing, however it also means that text
 * will be laid out differently at different zoom factors.
 **)
  _cairo_hint_metrics = (
    CAIRO_HINT_METRICS_DEFAULT,
    CAIRO_HINT_METRICS_OFF,
    CAIRO_HINT_METRICS_ON
  );
  cairo_hint_metrics_t = _cairo_hint_metrics;
  TCairoHintMetrics = type cairo_hint_metrics_t;

(**
 * cairo_font_options_t:
 *
 * An opaque structure holding all options that are used when
 * rendering fonts.
 *
 * Individual features of a #cairo_font_options_t can be set or
 * accessed using functions named
 * cairo_font_options_set_<emphasis>feature_name</emphasis> and
 * cairo_font_options_get_<emphasis>feature_name</emphasis>, like
 * cairo_font_options_set_antialias() and
 * cairo_font_options_get_antialias().
 *
 * New features may be added to a #cairo_font_options_t in the
 * future.  For this reason, cairo_font_options_copy(),
 * cairo_font_options_equal(), cairo_font_options_merge(), and
 * cairo_font_options_hash() should be used to copy, check
 * for equality, merge, or compute a hash value of
 * #cairo_font_options_t objects.
 **)
  PCairoFontOptions = ^TCairoFontOptions;
  _cairo_font_options = record
  end;
  cairo_font_options_t = _cairo_font_options;
  TCairoFontOptions = type cairo_font_options_t;

function cairo_font_options_create: PCairoFontOptions; cdecl;
function cairo_font_options_copy(const original: PCairoFontOptions): PCairoFontOptions; cdecl;
procedure cairo_font_options_destroy(options: PCairoFontOptions); cdecl;
function cairo_font_options_status(options: PCairoFontOptions): TCairoStatus; cdecl;
procedure cairo_font_options_merge(options: PCairoFontOptions; const other: PCairoFontOptions); cdecl;
function cairo_font_options_equal(const options: PCairoFontOptions; const other: PCairoFontOptions): TCairoBool; cdecl;
function cairo_font_options_hash(const options: PCairoFontOptions): Cardinal; cdecl;
procedure cairo_font_options_set_antialias(options: PCairoFontOptions; antialias: TCairoAntialias); cdecl;
function cairo_font_options_get_antialias(const options: PCairoFontOptions): TCairoAntialias; cdecl;
procedure cairo_font_options_set_subpixel_order(options: PCairoFontOptions; subpixel_order: TCairoSubpixelOrder); cdecl;
function cairo_font_options_get_subpixel_order(const options: PCairoFontOptions): TCairoSubpixelOrder; cdecl;
procedure cairo_font_options_set_hint_style(options: PCairoFontOptions; hint_style: TCairoHintStyle); cdecl;
function cairo_font_options_get_hint_style(const options: PCairoFontOptions): TCairoHintStyle; cdecl;
procedure cairo_font_options_set_hint_metrics(options: PCairoFontOptions; hint_metrics: TCairoHintMetrics); cdecl;
function cairo_font_options_get_hint_metrics(const options: PCairoFontOptions): TCairoHintMetrics; cdecl;

(* This interface is for dealing with text as text, not caring about the
   font object inside the the cairo_t. *)

procedure cairo_select_font_face(cr: PCairo; const family: PAnsiChar; slant: TCairoFontSlant; weight: TCairoFontWeight); cdecl;
procedure cairo_set_font_size(cr: PCairo; size: Double); cdecl;
procedure cairo_set_font_matrix(cr: PCairo; const matrix: PCairoMatrix); cdecl;
procedure cairo_get_font_matrix(cr: PCairo; matrix: PCairoMatrix); cdecl;
procedure cairo_set_font_options(cr: PCairo; const options: PCairoFontOptions); cdecl;
procedure cairo_get_font_options(cr: PCairo; options: PCairoFontOptions); cdecl;
procedure cairo_set_font_face(cr: PCairo; font_face: PCairoFontFace); cdecl;
function cairo_get_font_face(cr: PCairo): PCairoFontFace; cdecl;
procedure cairo_set_scaled_font(cr: PCairo; const scaled_font: PCairoScaledFont); cdecl;
function cairo_get_scaled_font(cr: PCairo): PCairoScaledFont; cdecl;
procedure cairo_show_text(cr: PCairo; const utf8: PAnsiChar); cdecl;
procedure cairo_show_glyphs(cr: PCairo; const glyphs: PCairoGlyph; num_glyphs: Integer); cdecl;
procedure cairo_show_text_glyphs(cr: PCairo; const utf8: PAnsiChar; utf8_len: Integer; const glyphs: PCairoGlyph; num_glyphs: Integer; const clusters: PCairoTextCluster; num_clusters: Integer; cluster_flags: TCairoTextClusterFlags); cdecl;
procedure cairo_text_path(cr: PCairo; const utf8: PAnsiChar); cdecl;
procedure cairo_glyph_path(cr: PCairo; const glyphs: PCairoGlyph; num_glyphs: Integer); cdecl;
procedure cairo_text_extents(cr: PCairo; const utf8: PAnsiChar; extents: PCairoTextExtents); cdecl;
procedure cairo_glyph_extents(cr: PCairo; const glyphs: PCairoGlyph; num_glyphs: Integer; extents: PCairoTextExtents); cdecl;
procedure cairo_font_extents(cr: PCairo; extents: PCairoFontExtents); cdecl;

(* Generic identifier for a font style *)

function cairo_font_face_reference(font_face: PCairoFontFace): PCairoFontFace; cdecl;
procedure cairo_font_face_destroy(font_face: PCairoFontFace); cdecl;
function cairo_font_face_get_reference_count(font_face: PCairoFontFace): Cardinal; cdecl;
function cairo_font_face_status(font_face: PCairoFontFace): TCairoStatus; cdecl;

(**
 * cairo_font_type_t:
 * @CAIRO_FONT_TYPE_TOY: The font was created using cairo's toy font api
 * @CAIRO_FONT_TYPE_FT: The font is of type FreeType
 * @CAIRO_FONT_TYPE_WIN32: The font is of type Win32
 * @CAIRO_FONT_TYPE_QUARTZ: The font is of type Quartz (Since: 1.6)
 * @CAIRO_FONT_TYPE_USER: The font was create using cairo's user font api (Since: 1.8)
 *
 * #cairo_font_type_t is used to describe the type of a given font
 * face or scaled font. The font types are also known as "font
 * backends" within cairo.
 *
 * The type of a font face is determined by the function used to
 * create it, which will generally be of the form
 * cairo_<emphasis>type</emphasis>_font_face_create(). The font face type can be queried
 * with cairo_font_face_get_type()
 *
 * The various #cairo_font_face_t functions can be used with a font face
 * of any type.
 *
 * The type of a scaled font is determined by the type of the font
 * face passed to cairo_scaled_font_create(). The scaled font type can
 * be queried with cairo_scaled_font_get_type()
 *
 * The various #cairo_scaled_font_t functions can be used with scaled
 * fonts of any type, but some font backends also provide
 * type-specific functions that must only be called with a scaled font
 * of the appropriate type. These functions have names that begin with
 * cairo_<emphasis>type</emphasis>_scaled_font() such as cairo_ft_scaled_font_lock_face().
 *
 * The behavior of calling a type-specific function with a scaled font
 * of the wrong type is undefined.
 *
 * New entries may be added in future versions.
 *
 * Since: 1.2
 **)
type
  _cairo_font_type  = (
    CAIRO_FONT_TYPE_TOY,
    CAIRO_FONT_TYPE_FT,
    CAIRO_FONT_TYPE_WIN32,
    CAIRO_FONT_TYPE_QUARTZ,
    CAIRO_FONT_TYPE_USER
  );
  cairo_font_type_t = _cairo_font_type;
  TCairoFontType = type cairo_font_type_t;

function cairo_font_face_get_type(font_face: PCairoFontFace): TCairoFontType; cdecl;
function cairo_font_face_get_user_data(font_face: PCairoFontFace; const key: PCairoUserDataKey): Pointer; cdecl;
function cairo_font_face_set_user_data(font_face: PCairoFontFace; const key: PCairoUserDataKey; user_data: Pointer; destroy: TCairoDestroyFunc): TCairoStatus; cdecl;

(* Portable interface to general font features. *)

function cairo_scaled_font_create(font_face: PCairoFontFace; const font_matrix, ctm: PCairoMatrix; const options: PCairoFontOptions): PCairoScaledFont; cdecl;
function cairo_scaled_font_reference(scaled_font: PCairoScaledFont): PCairoScaledFont; cdecl;
procedure cairo_scaled_font_destroy(scaled_font: PCairoScaledFont); cdecl;
function cairo_scaled_font_get_reference_count(scaled_font: PCairoScaledFont): Cardinal; cdecl;
function cairo_scaled_font_status(scaled_font: PCairoScaledFont): TCairoStatus; cdecl;
function cairo_scaled_font_get_type(scaled_font: PCairoScaledFont): TCairoFontType; cdecl;
function cairo_scaled_font_get_user_data(scaled_font: PCairoScaledFont; const key: PCairoUserDataKey): Pointer; cdecl;
function cairo_scaled_font_set_user_data(scaled_font: PCairoScaledFont; const key: PCairoUserDataKey; user_data: Pointer; destroy: TCairoDestroyFunc): TCairoStatus; cdecl;
procedure cairo_scaled_font_extents(scaled_font: PCairoScaledFont; extents: PCairoFontExtents); cdecl;
procedure cairo_scaled_font_text_extents(scaled_font: PCairoScaledFont; const utf8: PAnsiChar; extents: PCairoTextExtents); cdecl;
procedure cairo_scaled_font_glyph_extents(scaled_font: PCairoScaledFont; const glyphs: PCairoGlyph; num_glyphs: Integer; extents: PCairoTextExtents); cdecl;
function cairo_scaled_font_text_to_glyphs(scaled_font: PCairoScaledFont; x: Double; y: Double; const utf8: PAnsiChar; utf8_len: Integer; glyphs: PPCairoGlyph; num_glyphs: PInteger; clusters: PPCairoTextCluster; num_clusters: PInteger; cluster_flags: PCairoTextClusterFlags): TCairoStatus; cdecl;
function cairo_scaled_font_get_font_face(scaled_font: PCairoScaledFont): PCairoFontFace; cdecl;
procedure cairo_scaled_font_get_font_matrix(scaled_font: PCairoScaledFont; font_matrix: PCairoMatrix); cdecl;
procedure cairo_scaled_font_get_ctm(scaled_font: PCairoScaledFont; ctm: PCairoMatrix); cdecl;
procedure cairo_scaled_font_get_scale_matrix(scaled_font: PCairoScaledFont; scale_matrix: PCairoMatrix); cdecl;
procedure cairo_scaled_font_get_font_options(scaled_font: PCairoScaledFont; options: PCairoFontOptions); cdecl;

(* Toy fonts *)

function cairo_toy_font_face_create(const family: PAnsiChar; slant: TCairoFontSlant; weight: TCairoFontWeight): PCairoFontFace; cdecl;
function cairo_toy_font_face_get_family(font_face: PCairoFontFace): PAnsiChar; cdecl;
function cairo_toy_font_face_get_slant(font_face: PCairoFontFace): TCairoFontSlant; cdecl;
function cairo_toy_font_face_get_weight(font_face: PCairoFontFace): TCairoFontWeight; cdecl;

(* User fonts *)

function cairo_user_font_face_create: PCairoFontFace; cdecl;

(* User-font method signatures *)

(**
 * cairo_user_scaled_font_init_func_t:
 * @scaled_font: the scaled-font being created
 * @cr: a cairo context, in font space
 * @extents: font extents to fill in, in font space
 *
 * #cairo_user_scaled_font_init_func_t is the type of function which is
 * called when a scaled-font needs to be created for a user font-face.
 *
 * The cairo context @cr is not used by the caller, but is prepared in font
 * space, similar to what the cairo contexts passed to the render_glyph
 * method will look like.  The callback can use this context for extents
 * computation for example.  After the callback is called, @cr is checked
 * for any error status.
 *
 * The @extents argument is where the user font sets the font extents for
 * @scaled_font.  It is in font space, which means that for most cases its
 * ascent and descent members should add to 1.0.  @extents is preset to
 * hold a value of 1.0 for ascent, height, and max_x_advance, and 0.0 for
 * descent and max_y_advance members.
 *
 * The callback is optional.  If not set, default font extents as described
 * in the previous paragraph will be used.
 *
 * Note that @scaled_font is not fully initialized at this
 * point and trying to use it for text operations in the callback will result
 * in deadlock.
 *
 * Returns: %CAIRO_STATUS_SUCCESS upon success, or an error status on error.
 *
 * Since: 1.8
 **)
type
  cairo_user_scaled_font_init_func_t = function(scaled_font: PCairoScaledFont; cr: PCairo; extents: PCairoFontExtents): TCairoStatus; cdecl;
  TCairoUserScaledFontInitFunc = type cairo_user_scaled_font_init_func_t;

(**
 * cairo_user_scaled_font_render_glyph_func_t:
 * @scaled_font: user scaled-font
 * @glyph: glyph code to render
 * @cr: cairo context to draw to, in font space
 * @extents: glyph extents to fill in, in font space
 *
 * #cairo_user_scaled_font_render_glyph_func_t is the type of function which
 * is called when a user scaled-font needs to render a glyph.
 *
 * The callback is mandatory, and expected to draw the glyph with code @glyph to
 * the cairo context @cr.  @cr is prepared such that the glyph drawing is done in
 * font space.  That is, the matrix set on @cr is the scale matrix of @scaled_font,
 * The @extents argument is where the user font sets the font extents for
 * @scaled_font.  However, if user prefers to draw in user space, they can
 * achieve that by changing the matrix on @cr.  All cairo rendering operations
 * to @cr are permitted, however, the result is undefined if any source other
 * than the default source on @cr is used.  That means, glyph bitmaps should
 * be rendered using cairo_mask() instead of cairo_paint().
 *
 * Other non-default settings on @cr include a font size of 1.0 (given that
 * it is set up to be in font space), and font options corresponding to
 * @scaled_font.
 *
 * The @extents argument is preset to have <literal>x_bearing</literal>,
 * <literal>width</literal>, and <literal>y_advance</literal> of zero,
 * <literal>y_bearing</literal> set to <literal>-font_extents.ascent</literal>,
 * <literal>height</literal> to <literal>font_extents.ascent+font_extents.descent</literal>,
 * and <literal>x_advance</literal> to <literal>font_extents.max_x_advance</literal>.
 * The only field user needs to set in majority of cases is
 * <literal>x_advance</literal>.
 * If the <literal>width</literal> field is zero upon the callback returning
 * (which is its preset value), the glyph extents are automatically computed
 * based on the drawings done to @cr.  This is in most cases exactly what the
 * desired behavior is.  However, if for any reason the callback sets the
 * extents, it must be ink extents, and include the extents of all drawing
 * done to @cr in the callback.
 *
 * Returns: %CAIRO_STATUS_SUCCESS upon success, or
 * %CAIRO_STATUS_USER_FONT_ERROR or any other error status on error.
 *
 * Since: 1.8
 **)
  cairo_user_scaled_font_render_glyph_func_t = function(scaled_font: PCairoScaledFont; glyph: Cardinal; cr: PCairo; extents: PCairoTextExtents): TCairoStatus; cdecl;
  TCairoUserScaledFontRenderGlyphFunc = type cairo_user_scaled_font_render_glyph_func_t;

(**
 * cairo_user_scaled_font_text_to_glyphs_func_t:
 * @scaled_font: the scaled-font being created
 * @utf8: a string of text encoded in UTF-8
 * @utf8_len: length of @utf8 in bytes
 * @glyphs: pointer to array of glyphs to fill, in font space
 * @num_glyphs: pointer to number of glyphs
 * @clusters: pointer to array of cluster mapping information to fill, or %NULL
 * @num_clusters: pointer to number of clusters
 * @cluster_flags: pointer to location to store cluster flags corresponding to the
 *                 output @clusters
 *
 * #cairo_user_scaled_font_text_to_glyphs_func_t is the type of function which
 * is called to convert input text to an array of glyphs.  This is used by the
 * cairo_show_text() operation.
 *
 * Using this callback the user-font has full control on glyphs and their
 * positions.  That means, it allows for features like ligatures and kerning,
 * as well as complex <firstterm>shaping</firstterm> required for scripts like
 * Arabic and Indic.
 *
 * The @num_glyphs argument is preset to the number of glyph entries available
 * in the @glyphs buffer. If the @glyphs buffer is %NULL, the value of
 * @num_glyphs will be zero.  If the provided glyph array is too short for
 * the conversion (or for convenience), a new glyph array may be allocated
 * using cairo_glyph_allocate() and placed in @glyphs.  Upon return,
 * @num_glyphs should contain the number of generated glyphs.  If the value
 * @glyphs points at has changed after the call, the caller will free the
 * allocated glyph array using cairo_glyph_free().
 * The callback should populate the glyph indices and positions (in font space)
 * assuming that the text is to be shown at the origin.
 *
 * If @clusters is not %NULL, @num_clusters and @cluster_flags are also
 * non-%NULL, and cluster mapping should be computed. The semantics of how
 * cluster array allocation works is similar to the glyph array.  That is,
 * if @clusters initially points to a non-%NULL value, that array may be used
 * as a cluster buffer, and @num_clusters points to the number of cluster
 * entries available there.  If the provided cluster array is too short for
 * the conversion (or for convenience), a new cluster array may be allocated
 * using cairo_text_cluster_allocate() and placed in @clusters.  Upon return,
 * @num_clusters should contain the number of generated clusters.
 * If the value @clusters points at has changed after the call, the caller
 * will free the allocated cluster array using cairo_text_cluster_free().
 *
 * The callback is optional.  If @num_glyphs is negative upon
 * the callback returning or if the return value
 * is %CAIRO_STATUS_USER_FONT_NOT_IMPLEMENTED, the unicode_to_glyph callback
 * is tried.  See #cairo_user_scaled_font_unicode_to_glyph_func_t.
 *
 * Note: While cairo does not impose any limitation on glyph indices,
 * some applications may assume that a glyph index fits in a 16-bit
 * unsigned integer.  As such, it is advised that user-fonts keep their
 * glyphs in the 0 to 65535 range.  Furthermore, some applications may
 * assume that glyph 0 is a special glyph-not-found glyph.  User-fonts
 * are advised to use glyph 0 for such purposes and do not use that
 * glyph value for other purposes.
 *
 * Returns: %CAIRO_STATUS_SUCCESS upon success,
 * %CAIRO_STATUS_USER_FONT_NOT_IMPLEMENTED if fallback options should be tried,
 * or %CAIRO_STATUS_USER_FONT_ERROR or any other error status on error.
 *
 * Since: 1.8
 **)
 cairo_user_scaled_font_text_to_glyphs_func_t = function(scaled_font: PCairoScaledFont; const utf8: PAnsiChar; utf8_len: Integer; glyphs: PPCairoGlyph; num_glyphs: PInteger; clusters: PPCairoTextCluster; num_clusters: PInteger; cluster_flags: PCairoTextClusterFlags): TCairoStatus; cdecl;
 TCairoUserScaledFontTextToGlyphsFunc = type cairo_user_scaled_font_text_to_glyphs_func_t;

(**
 * cairo_user_scaled_font_unicode_to_glyph_func_t:
 * @scaled_font: the scaled-font being created
 * @unicode: input unicode character code-point
 * @glyph_index: output glyph index
 *
 * #cairo_user_scaled_font_unicode_to_glyph_func_t is the type of function which
 * is called to convert an input Unicode character to a single glyph.
 * This is used by the cairo_show_text() operation.
 *
 * This callback is used to provide the same functionality as the
 * text_to_glyphs callback does (see #cairo_user_scaled_font_text_to_glyphs_func_t)
 * but has much less control on the output,
 * in exchange for increased ease of use.  The inherent assumption to using
 * this callback is that each character maps to one glyph, and that the
 * mapping is context independent.  It also assumes that glyphs are positioned
 * according to their advance width.  These mean no ligatures, kerning, or
 * complex scripts can be implemented using this callback.
 *
 * The callback is optional, and only used if text_to_glyphs callback is not
 * set or fails to return glyphs.  If this callback is not set or if it returns
 * %CAIRO_STATUS_USER_FONT_NOT_IMPLEMENTED, an identity mapping from Unicode
 * code-points to glyph indices is assumed.
 *
 * Note: While cairo does not impose any limitation on glyph indices,
 * some applications may assume that a glyph index fits in a 16-bit
 * unsigned integer.  As such, it is advised that user-fonts keep their
 * glyphs in the 0 to 65535 range.  Furthermore, some applications may
 * assume that glyph 0 is a special glyph-not-found glyph.  User-fonts
 * are advised to use glyph 0 for such purposes and do not use that
 * glyph value for other purposes.
 *
 * Returns: %CAIRO_STATUS_SUCCESS upon success,
 * %CAIRO_STATUS_USER_FONT_NOT_IMPLEMENTED if fallback options should be tried,
 * or %CAIRO_STATUS_USER_FONT_ERROR or any other error status on error.
 *
 * Since: 1.8
 **)
 cairo_user_scaled_font_unicode_to_glyph_func_t = function(scaled_font: PCairoScaledFont; unicode: Cardinal; glyph_index: PCardinal): TCairoStatus; cdecl;
 TCairoUserScaledFontUnicodeToGlyphFunc = type cairo_user_scaled_font_unicode_to_glyph_func_t;

 (* User-font method setters *)

procedure cairo_user_font_face_set_init_func(font_face: PCairoFontFace; init_func: TCairoUserScaledFontInitFunc); cdecl;
procedure cairo_user_font_face_set_render_glyph_func(font_face: PCairoFontFace; render_glyph_func: TCairoUserScaledFontRenderGlyphFunc); cdecl;
procedure cairo_user_font_face_set_text_to_glyphs_func(font_face: PCairoFontFace; text_to_glyphs_func: TCairoUserScaledFontTextToGlyphsFunc); cdecl;
procedure cairo_user_font_face_set_unicode_to_glyph_func(font_face: PCairoFontFace; unicode_to_glyph_func: TCairoUserScaledFontUnicodeToGlyphFunc); cdecl;

(* User-font method getters *)

function cairo_user_font_face_get_init_func(font_face: PCairoFontFace): TCairoUserScaledFontInitFunc; cdecl;
function cairo_user_font_face_get_render_glyph_func(font_face: PCairoFontFace): TCairoUserScaledFontRenderGlyphFunc; cdecl;
function cairo_user_font_face_get_text_to_glyphs_func(font_face: PCairoFontFace): TCairoUserScaledFontTextToGlyphsFunc; cdecl;
function cairo_user_font_face_get_unicode_to_glyph_func(font_face: PCairoFontFace): TCairoUserScaledFontUnicodeToGlyphFunc; cdecl;

(* Query functions *)

function cairo_get_operator(cr: PCairo): TCairoOperator; cdecl;
function cairo_get_source(cr: PCairo): PCairoPattern; cdecl;
function cairo_get_tolerance(cr: PCairo): Double; cdecl;
function cairo_get_antialias(cr: PCairo): TCairoAntialias; cdecl;
function cairo_has_current_point(cr: PCairo): TCairoBool; cdecl;
procedure cairo_get_current_point(cr: PCairo; x: PDouble; y: PDouble); cdecl;
function cairo_get_fill_rule(cr: PCairo): TCairoFillRule; cdecl;
function cairo_get_line_width(cr: PCairo): Double; cdecl;
function cairo_get_line_cap(cr: PCairo): TCairoLineCap; cdecl;
function cairo_get_line_join(cr: PCairo): TCairoLineJoin; cdecl;
function cairo_get_miter_limit(cr: PCairo): Double; cdecl;
function cairo_get_dash_count(cr: PCairo): Integer; cdecl;
procedure cairo_get_dash(cr: PCairo; dashes: PDouble; offset: PDouble); cdecl;
procedure cairo_get_matrix(cr: PCairo; matrix: PCairoMatrix); cdecl;
function cairo_get_target(cr: PCairo): PCairoSurface; cdecl;
function cairo_get_group_target(cr: PCairo): PCairoSurface; cdecl;

(**
 * cairo_path_data_type_t:
 * @CAIRO_PATH_MOVE_TO: A move-to operation
 * @CAIRO_PATH_LINE_TO: A line-to operation
 * @CAIRO_PATH_CURVE_TO: A curve-to operation
 * @CAIRO_PATH_CLOSE_PATH: A close-path operation
 *
 * #cairo_path_data_t is used to describe the type of one portion
 * of a path when represented as a #cairo_path_t.
 * See #cairo_path_data_t for details.
 **)
type
  _cairo_path_data_type = (
    CAIRO_PATH_MOVE_TO,
    CAIRO_PATH_LINE_TO,
    CAIRO_PATH_CURVE_TO,
    CAIRO_PATH_CLOSE_PATH
  );
  cairo_path_data_type_t = _cairo_path_data_type;
  TCairoPathDataType = type cairo_path_data_type_t;

(**
 * cairo_path_data_t:
 *
 * #cairo_path_data_t is used to represent the path data inside a
 * #cairo_path_t.
 *
 * The data structure is designed to try to balance the demands of
 * efficiency and ease-of-use. A path is represented as an array of
 * #cairo_path_data_t, which is a union of headers and points.
 *
 * Each portion of the path is represented by one or more elements in
 * the array, (one header followed by 0 or more points). The length
 * value of the header is the number of array elements for the current
 * portion including the header, (ie. length == 1 + # of points), and
 * where the number of points for each element type is as follows:
 *
 * <programlisting>
 *     %CAIRO_PATH_MOVE_TO:     1 point
 *     %CAIRO_PATH_LINE_TO:     1 point
 *     %CAIRO_PATH_CURVE_TO:    3 points
 *     %CAIRO_PATH_CLOSE_PATH:  0 points
 * </programlisting>
 *
 * The semantics and ordering of the coordinate values are consistent
 * with cairo_move_to(), cairo_line_to(), cairo_curve_to(), and
 * cairo_close_path().
 *
 * Here is sample code for iterating through a #cairo_path_t:
 *
 * <informalexample><programlisting>
 *      Integer i;
 *      cairo_path_t *path;
 *      cairo_path_data_t *data;
 * &nbsp;
 *      path = cairo_copy_path (cr);
 * &nbsp;
 *      for (i=0; i < path->num_data; i += path->data[i].header.length) {
 *          data = &amp;path->data[i];
 *          switch (data->header.type) {
 *          case CAIRO_PATH_MOVE_TO:
 *              do_move_to_things (data[1].point.x, data[1].point.y);
 *              break;
 *          case CAIRO_PATH_LINE_TO:
 *              do_line_to_things (data[1].point.x, data[1].point.y);
 *              break;
 *          case CAIRO_PATH_CURVE_TO:
 *              do_curve_to_things (data[1].point.x, data[1].point.y,
 *                                  data[2].point.x, data[2].point.y,
 *                                  data[3].point.x, data[3].point.y);
 *              break;
 *          case CAIRO_PATH_CLOSE_PATH:
 *              do_close_path_things ();
 *              break;
 *          }
 *      }
 *      cairo_path_destroy (path);
 * </programlisting></informalexample>
 *
 * As of cairo 1.4, cairo does not mind if there are more elements in
 * a portion of the path than needed.  Such elements can be used by
 * users of the cairo API to hold extra values in the path data
 * structure.  For this reason, it is recommended that applications
 * always use <literal>data->header.length</literal> to
 * iterate over the path data, instead of hardcoding the number of
 * elements for each element type.
 **)

  PCairoPathData = ^TCairoPathData;
  _cairo_path_data_t = record
    case integer of
      0: (
        header: record
          type_: TCairoPathDataType;
          length: Integer;
        end;
      );
      1: (
        point: record
          x, y: Double;
        end;
      );
  end;
  cairo_path_data_t = _cairo_path_data_t;
  TCairoPathData = type cairo_path_data_t;

(**
 * cairo_path_t:
 * @status: the current error status
 * @data: the elements in the path
 * @num_data: the number of elements in the data array
 *
 * A data structure for holding a path. This data structure serves as
 * the return value for cairo_copy_path() and
 * cairo_copy_path_flat() as well the input value for
 * cairo_append_path().
 *
 * See #cairo_path_data_t for hints on how to iterate over the
 * actual data within the path.
 *
 * The num_data member gives the number of elements in the data
 * array. This number is larger than the number of independent path
 * portions (defined in #TCairoPathDataType), since the data
 * includes both headers and coordinates for each portion.
 **)
  PCairoPath = ^TCairoPath;
  cairo_path = record
    status: TCairoStatus;
    data: PCairoPathData;
    num_data: Integer;
  end;
  cairo_path_t = cairo_path;
  TCairoPath = type cairo_path_t;

function cairo_copy_path(cr: PCairo): PCairoPath; cdecl;
function cairo_copy_path_flat(cr: PCairo): PCairoPath; cdecl;
procedure cairo_append_path(cr: PCairo; const path: PCairoPath); cdecl;
procedure cairo_path_destroy(path: PCairoPath); cdecl;

(* Error status queries *)

function cairo_status(cr: PCairo): TCairoStatus; cdecl;
function cairo_status_to_string(status: TCairoStatus): PAnsiChar; cdecl;

(* Backend device manipulation *)

function cairo_device_reference(device: PCairoDevice): PCairoDevice; cdecl;

(**
 * cairo_device_type_t:
 * @CAIRO_DEVICE_TYPE_DRM: The surface is of type Direct Render Manager
 * @CAIRO_DEVICE_TYPE_GL: The surface is of type OpenGL
 * @CAIRO_DEVICE_TYPE_SCRIPT: The surface is of type script
 * @CAIRO_DEVICE_TYPE_XCB: The surface is of type xcb
 * @CAIRO_DEVICE_TYPE_XLIB: The surface is of type xlib
 * @CAIRO_DEVICE_TYPE_XML: The surface is of type XML
 *   cairo_surface_create_for_rectangle()
 *
 * #cairo_device_type_t is used to describe the type of a given
 * device. The devices types are also known as "backends" within cairo.
 *
 * The device type can be queried with cairo_device_get_type()
 *
 * The various #cairo_device_t functions can be used with surfaces of
 * any type, but some backends also provide type-specific functions
 * that must only be called with a device of the appropriate
 * type. These functions have names that begin with
 * cairo_<emphasis>type</emphasis>_device<!-- --> such as cairo_xcb_device_debug_set_render_version().
 *
 * The behavior of calling a type-specific function with a surface of
 * the wrong type is undefined.
 *
 * New entries may be added in future versions.
 *
 * Since: 1.10
 **)
type
  _cairo_device_type = (
      CAIRO_DEVICE_TYPE_DRM,
      CAIRO_DEVICE_TYPE_GL,
      CAIRO_DEVICE_TYPE_SCRIPT,
      CAIRO_DEVICE_TYPE_XCB,
      CAIRO_DEVICE_TYPE_XLIB,
      CAIRO_DEVICE_TYPE_XML);
  cairo_device_type_t = _cairo_device_type;
  TCairoDeviceType = type cairo_device_type_t;

function cairo_device_get_type(device: PCairoDevice): TCairoDeviceType; cdecl;
function cairo_device_status(device: PCairoDevice): TCairoStatus; cdecl;
function cairo_device_acquire (device: PCairoDevice): TCairoStatus; cdecl;
procedure cairo_device_release(device: PCairoDevice); cdecl;
procedure cairo_device_flush(device: PCairoDevice); cdecl;
procedure cairo_device_finish(device: PCairoDevice); cdecl;
procedure cairo_device_destroy(device: PCairoDevice); cdecl;
function cairo_device_get_reference_count(device: PCairoDevice): Cardinal; cdecl;
function cairo_device_get_user_data(device: PCairoDevice; const key: PCairoUserDataKey): Pointer; cdecl;
function cairo_device_set_user_data (device: PCairoDevice; const key: PCairoUserDataKey; user_data: Pointer; destroy: TCairoDestroyFunc): TCairoStatus; cdecl;

(* Surface manipulation *)

function cairo_surface_create_similar(other: PCairoSurface; content: TCairoContent; width: Integer; height: Integer): PCairoSurface; cdecl;
function cairo_surface_create_for_rectangle(target: PCairoSurface; x, y, width, height: Double): PCairoSurface; cdecl;
function cairo_surface_reference(surface: PCairoSurface): PCairoSurface; cdecl;
procedure cairo_surface_finish(surface: PCairoSurface); cdecl;
procedure cairo_surface_destroy(surface: PCairoSurface); cdecl;
function cairo_surface_get_device(surface: PCairoSurface): PCairoDevice; cdecl;
function cairo_surface_get_reference_count(surface: PCairoSurface): Cardinal; cdecl;
function cairo_surface_status(surface: PCairoSurface): TCairoStatus; cdecl;

(**
 * cairo_surface_type_t:
 * @CAIRO_SURFACE_TYPE_IMAGE: The surface is of type image
 * @CAIRO_SURFACE_TYPE_PDF: The surface is of type pdf
 * @CAIRO_SURFACE_TYPE_PS: The surface is of type ps
 * @CAIRO_SURFACE_TYPE_XLIB: The surface is of type xlib
 * @CAIRO_SURFACE_TYPE_XCB: The surface is of type xcb
 * @CAIRO_SURFACE_TYPE_GLITZ: The surface is of type glitz
 * @CAIRO_SURFACE_TYPE_QUARTZ: The surface is of type quartz
 * @CAIRO_SURFACE_TYPE_WIN32: The surface is of type win32
 * @CAIRO_SURFACE_TYPE_BEOS: The surface is of type beos
 * @CAIRO_SURFACE_TYPE_DIRECTFB: The surface is of type directfb
 * @CAIRO_SURFACE_TYPE_SVG: The surface is of type svg
 * @CAIRO_SURFACE_TYPE_OS2: The surface is of type os2
 * @CAIRO_SURFACE_TYPE_WIN32_PRINTING: The surface is a win32 printing surface
 * @CAIRO_SURFACE_TYPE_QUARTZ_IMAGE: The surface is of type quartz_image
 * @CAIRO_SURFACE_TYPE_SCRIPT: The surface is of type script, since 1.10
 * @CAIRO_SURFACE_TYPE_QT: The surface is of type Qt, since 1.10
 * @CAIRO_SURFACE_TYPE_RECORDING: The surface is of type recording, since 1.10
 * @CAIRO_SURFACE_TYPE_VG: The surface is a OpenVG surface, since 1.10
 * @CAIRO_SURFACE_TYPE_GL: The surface is of type OpenGL, since 1.10
 * @CAIRO_SURFACE_TYPE_DRM: The surface is of type Direct Render Manager, since 1.10
 * @CAIRO_SURFACE_TYPE_TEE: The surface is of type 'tee' (a multiplexing surface), since 1.10
 * @CAIRO_SURFACE_TYPE_XML: The surface is of type XML (for debugging), since 1.10
 * @CAIRO_SURFACE_TYPE_SKIA: The surface is of type Skia, since 1.10
 * @CAIRO_SURFACE_TYPE_SUBSURFACE: The surface is a subsurface created with
 *   cairo_surface_create_for_rectangle(), since 1.10
 *
 * #cairo_surface_type_t is used to describe the type of a given
 * surface. The surface types are also known as "backends" or "surface
 * backends" within cairo.
 *
 * The type of a surface is determined by the function used to create
 * it, which will generally be of the form cairo_<emphasis>type</emphasis>_surface_create(),
 * (though see cairo_surface_create_similar() as well).
 *
 * The surface type can be queried with cairo_surface_get_type()
 *
 * The various #cairo_surface_t functions can be used with surfaces of
 * any type, but some backends also provide type-specific functions
 * that must only be called with a surface of the appropriate
 * type. These functions have names that begin with
 * cairo_<emphasis>type</emphasis>_surface<!-- --> such as cairo_image_surface_get_width().
 *
 * The behavior of calling a type-specific function with a surface of
 * the wrong type is undefined.
 *
 * New entries may be added in future versions.
 *
 * Since: 1.2
 **)
type
  _cairo_surface_type = (
    CAIRO_SURFACE_TYPE_IMAGE,
    CAIRO_SURFACE_TYPE_PDF,
    CAIRO_SURFACE_TYPE_PS,
    CAIRO_SURFACE_TYPE_XLIB,
    CAIRO_SURFACE_TYPE_XCB,
    CAIRO_SURFACE_TYPE_GLITZ,
    CAIRO_SURFACE_TYPE_QUARTZ,
    CAIRO_SURFACE_TYPE_WIN32,
    CAIRO_SURFACE_TYPE_BEOS,
    CAIRO_SURFACE_TYPE_DIRECTFB,
    CAIRO_SURFACE_TYPE_SVG,
    CAIRO_SURFACE_TYPE_OS2,
    CAIRO_SURFACE_TYPE_WIN32_PRINTING,
    CAIRO_SURFACE_TYPE_QUARTZ_IMAGE,
    CAIRO_SURFACE_TYPE_SCRIPT,
    CAIRO_SURFACE_TYPE_QT,
    CAIRO_SURFACE_TYPE_RECORDING,
    CAIRO_SURFACE_TYPE_VG,
    CAIRO_SURFACE_TYPE_GL,
    CAIRO_SURFACE_TYPE_DRM,
    CAIRO_SURFACE_TYPE_TEE,
    CAIRO_SURFACE_TYPE_XML,
    CAIRO_SURFACE_TYPE_SKIA,
    CAIRO_SURFACE_TYPE_SUBSURFACE
  );
  cairo_surface_type_t = _cairo_surface_type;
  TCairoSurfaceType = type cairo_surface_type_t;

function cairo_surface_get_type(surface: PCairoSurface): TCairoSurfaceType; cdecl;
function cairo_surface_get_content(surface: PCairoSurface): TCairoContent; cdecl;

{$ifdef CAIRO_HAS_PNG_FUNCTIONS}
function cairo_surface_write_to_png(surface: PCairoSurface; const filename: PAnsiChar): TCairoStatus; cdecl;
function cairo_surface_write_to_png_stream(surface: PCairoSurface; write_func: TCairoWriteFunc; closure: Pointer): TCairoStatus; cdecl;
{$endif}

function cairo_surface_get_user_data(surface: PCairoSurface; const key: PCairoUserDataKey): Pointer; cdecl;
function cairo_surface_set_user_data(surface: PCairoSurface; const key: PCairoUserDataKey; user_data: Pointer; destroy: TCairoDestroyFunc): TCairoStatus; cdecl;

const
  CAIRO_MIME_TYPE_JPEG: PAnsiChar = 'image/jpeg';
  CAIRO_MIME_TYPE_PNG: PAnsiChar  = 'image/png';
  CAIRO_MIME_TYPE_JP2: PAnsiChar  = 'image/jp2';
  CAIRO_MIME_TYPE_URI: PAnsiChar  = 'text/x-uri';

procedure cairo_surface_get_mime_data (surface: PCairoSurface; const mime_type: PAnsiChar; out data: Pointer; out length: Cardinal); cdecl;
function cairo_surface_set_mime_data (surface: PCairoSurface; const mime_type: PAnsiChar; const data : Pointer; length: Cardinal;	destroy: TCairoDestroyFunc; closure: Pointer): TCairoStatus; cdecl;

procedure cairo_surface_get_font_options(surface: PCairoSurface; options: PCairoFontOptions); cdecl;
procedure cairo_surface_flush(surface: PCairoSurface); cdecl;
procedure cairo_surface_mark_dirty(surface: PCairoSurface); cdecl;
procedure cairo_surface_mark_dirty_rectangle(surface: PCairoSurface; x, y, width, height: Integer); cdecl;
procedure cairo_surface_set_device_offset(surface: PCairoSurface; x_offset, y_offset: Double); cdecl;
procedure cairo_surface_get_device_offset(surface: PCairoSurface; x_offset, y_offset: PDouble); cdecl;
procedure cairo_surface_set_fallback_resolution(surface: PCairoSurface; x_pixels_per_inch, y_pixels_per_inch: Double); cdecl;
procedure cairo_surface_get_fallback_resolution(surface: PCairoSurface; x_pixels_per_inch, y_pixels_per_inch: PDouble); cdecl;
procedure cairo_surface_copy_page(surface: PCairoSurface); cdecl;
procedure cairo_surface_show_page(surface: PCairoSurface); cdecl;
function cairo_surface_has_show_text_glyphs(surface: PCairoSurface): TCairoBool; cdecl;

(* Image-surface functions *)

(**
 * cairo_format_t:
 * @CAIRO_FORMAT_INVALID: no such format exists or is supported.
 * @CAIRO_FORMAT_ARGB32: each pixel is a 32-bit quantity, with
 *   alpha in the upper 8 bits, then red, then green, then blue.
 *   The 32-bit quantities are stored native-endian. Pre-multiplied
 *   alpha is used. (That is, 50% transparent red is 0x80800000,
 *   not 0x80ff0000.)
 * @CAIRO_FORMAT_RGB24: each pixel is a 32-bit quantity, with
 *   the upper 8 bits unused. Red, Green, and Blue are stored
 *   in the remaining 24 bits in that order.
 * @CAIRO_FORMAT_A8: each pixel is a 8-bit quantity holding
 *   an alpha value.
 * @CAIRO_FORMAT_A1: each pixel is a 1-bit quantity holding
 *   an alpha value. Pixels are packed together into 32-bit
 *   quantities. The ordering of the bits matches the
 *   endianess of the platform. On a big-endian machine, the
 *   first pixel is in the uppermost bit, on a little-endian
 *   machine the first pixel is in the least-significant bit.
 * @CAIRO_FORMAT_RGB16_565: each pixel is a 16-bit quantity
 *   with red in the upper 5 bits, then green in the middle
 *   6 bits, and blue in the lower 5 bits.
 *
 * #cairo_format_t is used to identify the memory format of
 * image data.
 *
 * New entries may be added in future versions.
 **)
type
  _cairo_format = (
    CAIRO_FORMAT_INVALID   = -1,
    CAIRO_FORMAT_ARGB32    = 0,
    CAIRO_FORMAT_RGB24     = 1,
    CAIRO_FORMAT_A8        = 2,
    CAIRO_FORMAT_A1        = 3,
    CAIRO_FORMAT_RGB16_565 = 4

    (* The value of 4 is reserved by a deprecated enum value.
     * The next format added must have an explicit value of 5.
    CAIRO_FORMAT_RGB16_565 = 4,
    *)
  );
  cairo_format_t = _cairo_format;
  TCairoFormat = type cairo_format_t;

function cairo_image_surface_create(format: TCairoFormat; width: Integer; height: Integer): PCairoSurface; cdecl;
function cairo_format_stride_for_width(format: TCairoFormat; width: Integer): Integer; cdecl;
function cairo_image_surface_create_for_data(data: PByte; format: TCairoFormat; width: Integer; height: Integer; stride: Integer): PCairoSurface; cdecl;
function cairo_image_surface_get_data(surface: PCairoSurface): PByte; cdecl;
function cairo_image_surface_get_format(surface: PCairoSurface): TCairoFormat; cdecl;
function cairo_image_surface_get_width(surface: PCairoSurface): Integer; cdecl;
function cairo_image_surface_get_height(surface: PCairoSurface): Integer; cdecl;
function cairo_image_surface_get_stride(surface: PCairoSurface): Integer; cdecl;

{$ifdef CAIRO_HAS_PNG_FUNCTIONS}
function cairo_image_surface_create_from_png(const filename: PAnsiChar): PCairoSurface; cdecl;
function cairo_image_surface_create_from_png_stream(read_func: TCairoReadFunc; closure: Pointer): PCairoSurface; cdecl;
{$endif}

(* Recording-surface functions *)

function cairo_recording_surface_create(content: TCairoContent; const extents: PCairoRectangle): PCairoSurface; cdecl;
procedure cairo_recording_surface_ink_extents(surface: PCairoSurface; x0, y0, width, height: PDouble); cdecl;

(* Pattern creation functions *)

function cairo_pattern_create_rgb(red: Double; green: Double; blue: Double): PCairoPattern; cdecl;
function cairo_pattern_create_rgba(red: Double; green: Double; blue: Double; alpha: Double): PCairoPattern; cdecl;
function cairo_pattern_create_for_surface(surface: PCairoSurface): PCairoPattern; cdecl;
function cairo_pattern_create_linear(x0: Double; y0: Double; x1: Double; y1: Double): PCairoPattern; cdecl;
function cairo_pattern_create_radial(cx0: Double; cy0: Double; radius0: Double; cx1: Double; cy1: Double; radius1: Double): PCairoPattern; cdecl;
function cairo_pattern_reference(pattern: PCairoPattern): PCairoPattern; cdecl;
procedure cairo_pattern_destroy(pattern: PCairoPattern); cdecl;
function cairo_pattern_get_reference_count(pattern: PCairoPattern): Cardinal; cdecl;
function cairo_pattern_status(pattern: PCairoPattern): TCairoStatus; cdecl;
function cairo_pattern_get_user_data(pattern: PCairoPattern; const key: PCairoUserDataKey): Pointer; cdecl;
function cairo_pattern_set_user_data(pattern: PCairoPattern; const key: PCairoUserDataKey; user_data: Pointer; destroy: TCairoDestroyFunc): TCairoStatus; cdecl;

(**
 * cairo_pattern_type_t:
 * @CAIRO_PATTERN_TYPE_SOLID: The pattern is a solid (uniform)
 * color. It may be opaque or translucent.
 * @CAIRO_PATTERN_TYPE_SURFACE: The pattern is a based on a surface (an image).
 * @CAIRO_PATTERN_TYPE_LINEAR: The pattern is a linear gradient.
 * @CAIRO_PATTERN_TYPE_RADIAL: The pattern is a radial gradient.
 *
 * #cairo_pattern_type_t is used to describe the type of a given pattern.
 *
 * The type of a pattern is determined by the function used to create
 * it. The cairo_pattern_create_rgb() and cairo_pattern_create_rgba()
 * functions create SOLID patterns. The remaining
 * cairo_pattern_create<!-- --> functions map to pattern types in obvious
 * ways.
 *
 * The pattern type can be queried with cairo_pattern_get_type()
 *
 * Most #cairo_pattern_t functions can be called with a pattern of any
 * type, (though trying to change the extend or filter for a solid
 * pattern will have no effect). A notable exception is
 * cairo_pattern_add_color_stop_rgb() and
 * cairo_pattern_add_color_stop_rgba() which must only be called with
 * gradient patterns (either LINEAR or RADIAL). Otherwise the pattern
 * will be shutdown and put into an error state.
 *
 * New entries may be added in future versions.
 *
 * Since: 1.2
 **)
type
  _cairo_pattern_type = (
    CAIRO_PATTERN_TYPE_SOLID,
    CAIRO_PATTERN_TYPE_SURFACE,
    CAIRO_PATTERN_TYPE_LINEAR,
    CAIRO_PATTERN_TYPE_RADIAL
  );
  cairo_pattern_type_t = _cairo_pattern_type;
  TCairoPatternType = type cairo_pattern_type_t;

function cairo_pattern_get_type(pattern: PCairoPattern): TCairoPatternType; cdecl;
procedure cairo_pattern_add_color_stop_rgb(pattern: PCairoPattern; offset, red, green, blue: Double); cdecl;
procedure cairo_pattern_add_color_stop_rgba(pattern: PCairoPattern; offset, red, green, blue, alpha: Double); cdecl;
procedure cairo_pattern_set_matrix(pattern: PCairoPattern; const matrix: PCairoMatrix); cdecl;
procedure cairo_pattern_get_matrix(pattern: PCairoPattern; matrix: PCairoMatrix); cdecl;

(**
 * cairo_extend_t:
 * @CAIRO_EXTEND_NONE: pixels outside of the source pattern
 *   are fully transparent
 * @CAIRO_EXTEND_REPEAT: the pattern is tiled by repeating
 * @CAIRO_EXTEND_REFLECT: the pattern is tiled by reflecting
 *   at the edges (Implemented for surface patterns since 1.6)
 * @CAIRO_EXTEND_PAD: pixels outside of the pattern copy
 *   the closest pixel from the source (Since 1.2; but only
 *   implemented for surface patterns since 1.6)
 *
 * #cairo_extend_t is used to describe how pattern color/alpha will be
 * determined for areas "outside" the pattern's natural area, (for
 * example, outside the surface bounds or outside the gradient
 * geometry).
 *
 * The default extend mode is %CAIRO_EXTEND_NONE for surface patterns
 * and %CAIRO_EXTEND_PAD for gradient patterns.
 *
 * New entries may be added in future versions.
 **)
type
  _cairo_extend = (
    CAIRO_EXTEND_NONE,
    CAIRO_EXTEND_REPEAT,
    CAIRO_EXTEND_REFLECT,
    CAIRO_EXTEND_PAD
  );
  cairo_extend_t = _cairo_extend;
  TCairoExtend = type cairo_extend_t;

procedure cairo_pattern_set_extend(pattern: PCairoPattern; extend: TCairoExtend); cdecl;
function cairo_pattern_get_extend(pattern: PCairoPattern): TCairoExtend; cdecl;

(**
 * cairo_filter_t:
 * @CAIRO_FILTER_FAST: A high-performance filter, with quality similar
 *     to %CAIRO_FILTER_NEAREST
 * @CAIRO_FILTER_GOOD: A reasonable-performance filter, with quality
 *     similar to %CAIRO_FILTER_BILINEAR
 * @CAIRO_FILTER_BEST: The highest-quality available, performance may
 *     not be suitable for interactive use.
 * @CAIRO_FILTER_NEAREST: Nearest-neighbor filtering
 * @CAIRO_FILTER_BILINEAR: Linear interpolation in two dimensions
 * @CAIRO_FILTER_GAUSSIAN: This filter value is currently
 *     unimplemented, and should not be used in current code.
 *
 * #cairo_filter_t is used to indicate what filtering should be
 * applied when reading pixel values from patterns. See
 * cairo_pattern_set_source() for indicating the desired filter to be
 * used with a particular pattern.
 *)
type
  _cairo_filter = (
    CAIRO_FILTER_FAST,
    CAIRO_FILTER_GOOD,
    CAIRO_FILTER_BEST,
    CAIRO_FILTER_NEAREST,
    CAIRO_FILTER_BILINEAR,
    CAIRO_FILTER_GAUSSIAN
  );
  cairo_filter_t = _cairo_filter;
  TCairoFilter = type cairo_filter_t;

procedure cairo_pattern_set_filter(pattern: PCairoPattern; filter: TCairoFilter); cdecl;
function cairo_pattern_get_filter(pattern: PCairoPattern): TCairoFilter; cdecl;
function cairo_pattern_get_rgba(pattern: PCairoPattern; red, green, blue, alpha: PDouble): TCairoStatus; cdecl;
function cairo_pattern_get_surface(pattern: PCairoPattern; var surface: PCairoSurface): TCairoStatus; cdecl;
function cairo_pattern_get_color_stop_rgba(pattern: PCairoPattern; index: Integer; offset, red, green, blue, alpha: PDouble): TCairoStatus; cdecl;
function cairo_pattern_get_color_stop_count(pattern: PCairoPattern; count: PInteger): TCairoStatus; cdecl;
function cairo_pattern_get_linear_points(pattern: PCairoPattern; x0, y0, x1, y1: PDouble): TCairoStatus; cdecl;
function cairo_pattern_get_radial_circles(pattern: PCairoPattern; x0, y0, r0, x1, y1, r1: PDouble): TCairoStatus; cdecl;

(* functions Matrix *)

procedure cairo_matrix_init(matrix: PCairoMatrix; xx, yx, xy, yy, x0, y0: Double); cdecl;
procedure cairo_matrix_init_identity(matrix: PCairoMatrix); cdecl;
procedure cairo_matrix_init_translate(matrix: PCairoMatrix; tx, ty: Double); cdecl;
procedure cairo_matrix_init_scale(matrix: PCairoMatrix; sx, sy: Double); cdecl;
procedure cairo_matrix_init_rotate(matrix: PCairoMatrix; radians: Double); cdecl;
procedure cairo_matrix_translate(matrix: PCairoMatrix; tx, ty: Double); cdecl;
procedure cairo_matrix_scale(matrix: PCairoMatrix; sx, sy: Double); cdecl;
procedure cairo_matrix_rotate(matrix: PCairoMatrix; radians: Double); cdecl;
function cairo_matrix_invert(matrix: PCairoMatrix): TCairoStatus; cdecl;
procedure cairo_matrix_multiply(result: PCairoMatrix; const a, b: PCairoMatrix); cdecl;
procedure cairo_matrix_transform_distance(const matrix: PCairoMatrix; dx, dy: PDouble); cdecl;
procedure cairo_matrix_transform_point(const matrix: PCairoMatrix; x, y: PDouble); cdecl;

(* Region functions *)

(**
 * cairo_region_t:
 *
 * A #cairo_region_t represents a set of integer-aligned rectangles.
 *
 * It allows set-theoretical operations like cairo_region_union() and
 * cairo_region_intersect() to be performed on them.
 *
 * Memory management of #cairo_region_t is done with
 * cairo_region_reference() and cairo_region_destroy().
 *
 * Since: 1.10
 **)
type
  PCairoRegion = ^TCairoRegion;
  _cairo_region = record
  end;
  cairo_region_t = _cairo_region;
  TCairoRegion = type cairo_region_t;

(**
 * cairo_rectangle_int_t:
 * @x: X coordinate of the left side of the rectangle
 * @y: Y coordinate of the the top side of the rectangle
 * @width: width of the rectangle
 * @height: height of the rectangle
 *
 * A data structure for holding a rectangle with integer coordinates.
 *
 * Since: 1.10
 **)

  PCairoRectangleInt = ^TCairoRectangleInt;
  _cairo_rectangle_int = record
    x, y, width, height: Integer;
  end;
  cairo_rectangle_int_t = _cairo_rectangle_int;
  TCairoRectangleInt = type cairo_rectangle_int_t;

  _cairo_region_overlap = (
    CAIRO_REGION_OVERLAP_IN,		(* completely inside region *)
    CAIRO_REGION_OVERLAP_OUT,		(* completely outside region *)
    CAIRO_REGION_OVERLAP_PART		(* partly inside region *)
  );
  cairo_region_overlap_t = _cairo_region_overlap;
  TCairoRegionOverlap = type cairo_region_overlap_t;


function cairo_region_create: PCairoRegion; cdecl;
function cairo_region_create_rectangle(const rectangle: PCairoRectangleInt): PCairoRegion; cdecl;
function cairo_region_create_rectangles(const rects: PCairoRectangleInt; count: Integer): PCairoRegion; cdecl;
function cairo_region_copy(const original: PCairoRegion): PCairoRegion; cdecl;
function cairo_region_reference(region: PCairoRegion): PCairoRegion; cdecl;
procedure cairo_region_destroy(region: PCairoRegion); cdecl;
function cairo_region_equal(const a, b: PCairoRegion): TCairoBool; cdecl;
function cairo_region_status(const region: PCairoRegion): TCairoStatus; cdecl;
procedure cairo_region_get_extents(const region: PCairoRegion; extents: PCairoRectangleInt); cdecl;
function cairo_region_num_rectangles (const region: PCairoRegion): Integer; cdecl;
procedure cairo_region_get_rectangle(const region: PCairoRegion; nth: Integer; rectangle: PCairoRectangleInt); cdecl;
function cairo_region_is_empty(const region: PCairoRegion): TCairoBool; cdecl;
function cairo_region_contains_rectangle (const region: PCairoRegion; const rectangle: PCairoRectangleInt): TCairoRegionOverlap; cdecl;
function cairo_region_contains_point(const region: PCairoRegion; x, y: Integer): TCairoBool; cdecl;
procedure cairo_region_translate(region: PCairoRegion; dx, dy: Integer); cdecl;
function cairo_region_subtract (dst: PCairoRegion; const other: PCairoRegion): TCairoStatus; cdecl;
function cairo_region_subtract_rectangle (dst: PCairoRegion; const rectangle: PCairoRectangleInt): TCairoStatus; cdecl;
function cairo_region_intersect(dst: PCairoRegion; const other: PCairoRegion): TCairoStatus; cdecl;
function cairo_region_intersect_rectangle (dst: PCairoRegion; const rectangle: PCairoRectangleInt): TCairoStatus; cdecl;
function cairo_region_union (dst: PCairoRegion; const other: PCairoRegion): TCairoStatus; cdecl;
function cairo_region_union_rectangle (dst: PCairoRegion; const rectangle: PCairoRectangleInt): TCairoStatus; cdecl;
function cairo_region_xor(dst: PCairoRegion; const other: PCairoRegion): TCairoStatus; cdecl;
function cairo_region_xor_rectangle (dst: PCairoRegion; const rectangle: PCairoRectangleInt): TCairoStatus; cdecl;

(* Functions to be used while debugging (not intended for use in code: production *)

procedure cairo_debug_reset_static_data; cdecl;


{$ifdef CAIRO_HAS_WIN32_SURFACE}

function cairo_win32_surface_create(hdc: HDC): PCairoSurface; cdecl;
function cairo_win32_printing_surface_create(hdc: HDC): PCairoSurface; cdecl;
function cairo_win32_surface_create_with_ddb(hdc: HDC; format: TCairoFormat; width: Integer; height: Integer): PCairoSurface; cdecl;
function cairo_win32_surface_create_with_dib(format: TCairoFormat; width: Integer; height: Integer): PCairoSurface; cdecl;
function cairo_win32_surface_get_dc(surface: PCairoSurface): HDC; cdecl;
function cairo_win32_surface_get_image(surface: PCairoSurface): PCairoSurface; cdecl;

{$ifdef CAIRO_HAS_WIN32_FONT}

(*
 * Win32 font support
 *)

function cairo_win32_font_face_create_for_logfontw(logfont: PLOGFONTW): PCairoFontFace; cdecl;
function cairo_win32_font_face_create_for_hfont(font: HFONT): PCairoFontFace; cdecl;
function cairo_win32_font_face_create_for_logfontw_hfont(logfont: PLOGFONTW; font: HFONT): PCairoFontFace; cdecl;
function cairo_win32_scaled_font_select_font(scaled_font: PCairoScaledFont; hdc: HDC): TCairoStatus; cdecl;
procedure cairo_win32_scaled_font_done_font(scaled_font: PCairoScaledFont); cdecl;
function cairo_win32_scaled_font_get_metrics_factor(scaled_font: PCairoScaledFont): Double; cdecl;
procedure cairo_win32_scaled_font_get_logical_to_device(scaled_font: PCairoScaledFont; logical_to_device: PCairoMatrix); cdecl;
procedure cairo_win32_scaled_font_get_device_to_logical(scaled_font: PCairoScaledFont; device_to_logical: PCairoMatrix); cdecl;

{$endif} (* CAIRO_HAS_WIN32_FONT *)
{$endif} (* CAIRO_HAS_WIN32_SURFACE *)

{$ifdef CAIRO_HAS_XLIB_SURFACE}
function cairo_xlib_surface_create(dpy: PDisplay; drawable: TDrawable; visual: PVisual; width: Integer; height: Integer): PCairoSurface; cdecl;
function cairo_xlib_surface_create_for_bitmap(dpy: PDisplay; bitmap: TPixmap; screen: PScreen; width: Integer; height: Integer): PCairoSurface; cdecl;
procedure cairo_xlib_surface_set_size(surface: PCairoSurface; width: Integer; height: Integer); cdecl;
procedure cairo_xlib_surface_set_drawable(surface: PCairoSurface; drawable: TDrawable; width: Integer; height: Integer); cdecl;
function cairo_xlib_surface_get_display(surface: PCairoSurface): PDisplay; cdecl;
function cairo_xlib_surface_get_drawable(surface: PCairoSurface): TDrawable; cdecl;
function cairo_xlib_surface_get_screen(surface: PCairoSurface): PScreen; cdecl;
function cairo_xlib_surface_get_visual(surface: PCairoSurface): PVisual; cdecl;
function cairo_xlib_surface_get_depth(surface: PCairoSurface): Integer; cdecl;
function cairo_xlib_surface_get_width(surface: PCairoSurface): Integer; cdecl;
function cairo_xlib_surface_get_height(surface: PCairoSurface): Integer; cdecl;
{$endif} (* CAIRO_HAS_XLIB_SURFACE *)


{$ifdef CAIRO_HAS_XLIB_XRENDER_SURFACE}
function cairo_xlib_surface_create_with_xrender_format(dpy: PDisplay; drawable: TDrawable; screen: PScreen; format: PXRenderPictFormat; width, height: Integer): PCairoSurface; cdecl;
function cairo_xlib_surface_get_xrender_format(surface: PCairoSurface): PXRenderPictFormat; cdecl;
{$endif} (* CAIRO_HAS_XLIB_XRENDER_SURFACE *)

{$ifdef CAIRO_HAS_FT_FONT}

(* Fontconfig/Freetype platform-specific font interface *)

type
  FcPattern = Pointer;
  PFcPattern = ^FcPattern;

function cairo_ft_font_face_create_for_pattern(pattern: PFcPattern): PCairoFontFace; cdecl;
procedure cairo_ft_font_options_substitute(const options: PCairoFontOptions; pattern: PFcPattern); cdecl;
function cairo_ft_font_face_create_for_ft_face(face: TFT_Face; load_flags: Integer): PCairoFontFace; cdecl;
function cairo_ft_scaled_font_lock_face(scaled_font: PCairoScaledFont): TFT_Face; cdecl;
procedure cairo_ft_scaled_font_unlock_face(scaled_font: PCairoScaledFont); cdecl;
{$endif} (* CAIRO_HAS_FT_FONT *)


{$ifdef CAIRO_HAS_SVG_SURFACE}
(**
 * cairo_svg_version_t:
 * @CAIRO_SVG_VERSION_1_1: The version 1.1 of the SVG specification.
 * @CAIRO_SVG_VERSION_1_2: The version 1.2 of the SVG specification.
 *
 * #cairo_svg_version_t is used to describe the version number of the SVG
 * specification that a generated SVG file will conform to.
 *)
type
  PCairoSVGVersion = ^TCairoSVGVersion;
  _cairo_svg_version = (
    CAIRO_SVG_VERSION_1_1,
    CAIRO_SVG_VERSION_1_2
  );
  cairo_svg_version_t = _cairo_svg_version;
  TCairoSVGVersion = type cairo_svg_version_t;

function cairo_svg_surface_create(const filename: PAnsiChar; width_in_points, height_in_points: Double): PCairoSurface; cdecl;
function cairo_svg_surface_create_for_stream(write_func: TCairoWriteFunc; closure: Pointer; width_in_points, height_in_points: Double): PCairoSurface; cdecl;
procedure cairo_svg_surface_restrict_to_version(surface: PCairoSurface; version: TCairoSVGVersion); cdecl;
procedure cairo_svg_get_versions(var versions: PCairoSVGVersion; num_versions: PInteger); cdecl;
function cairo_svg_version_to_string(version: TCairoSVGVersion): PAnsiChar; cdecl;

{$endif} (* CAIRO_HAS_SVG_SURFACE *)

{$ifdef CAIRO_HAS_PDF_SURFACE}

(**
 * cairo_pdf_version_t:
 * @CAIRO_PDF_VERSION_1_4: The version 1.4 of the PDF specification.
 * @CAIRO_PDF_VERSION_1_5: The version 1.5 of the PDF specification.
 *
 * #cairo_pdf_version_t is used to describe the version number of the PDF
 * specification that a generated PDF file will conform to.
 *
 * Since 1.10
 *)
type
 PCairoPdfVersion = ^TCairoPdfVersion;
  _cairo_pdf_version = (
    CAIRO_PDF_VERSION_1_4,
    CAIRO_PDF_VERSION_1_5
  );
  cairo_pdf_version_t = _cairo_pdf_version;
  TCairoPdfVersion = type cairo_pdf_version_t;

function cairo_pdf_surface_create(const filename: PAnsiChar; width_in_points, height_in_points: Double): PCairoSurface; cdecl;
function cairo_pdf_surface_create_for_stream(write_func: TCairoWriteFunc; closure: Pointer; width_in_points, height_in_points: Double): PCairoSurface; cdecl;
procedure cairo_pdf_surface_set_size(surface: PCairoSurface; width_in_points: Double; height_in_points: Double); cdecl;
procedure cairo_pdf_surface_restrict_to_version(surface: PCairoSurface; version: TCairoPdfVersion); cdecl;
procedure cairo_pdf_get_versions(out versions: PCairoPdfVersion; out num_versions: Integer); cdecl;
function cairo_pdf_version_to_string(version: TCairoPdfVersion): PAnsiChar; cdecl;

{$endif} (* CAIRO_HAS_PDF_SURFACE *)

{$ifdef CAIRO_HAS_PS_SURFACE}

(* PS-surface functions *)

(**
 * cairo_ps_level_t:
 * @CAIRO_PS_LEVEL_2: The language level 2 of the PostScript specification.
 * @CAIRO_PS_LEVEL_3: The language level 3 of the PostScript specification.
 *
 * #cairo_ps_level_t is used to describe the language level of the
 * PostScript Language Reference that a generated PostScript file will
 * conform to.
 *)
type
  PCairoPSLevel = ^TCairoPSLevel;
  _cairo_ps_level = (
    CAIRO_PS_LEVEL_2,
    CAIRO_PS_LEVEL_3
  );
  cairo_ps_level_t = _cairo_ps_level;
  TCairoPSLevel = type cairo_ps_level_t;

function cairo_ps_surface_create(const filename: PAnsiChar; width_in_points: Double; height_in_points: Double): PCairoSurface; cdecl;
function cairo_ps_surface_create_for_stream(write_func: TCairoWriteFunc; closure: Pointer; width_in_points: Double; height_in_points: Double): PCairoSurface; cdecl;
procedure cairo_ps_surface_restrict_to_level(surface: PCairoSurface; level: TCairoPSLevel); cdecl;
procedure cairo_ps_get_levels(var levels: PCairoPSLevel; num_levels: PInteger); cdecl;
function cairo_ps_level_to_string(level: TCairoPSLevel): PAnsiChar; cdecl;
procedure cairo_ps_surface_set_eps(surface: PCairoSurface; eps: TCairoBool); cdecl;
function cairo_ps_surface_get_eps(surface: PCairoSurface): TCairoBool; cdecl;
procedure cairo_ps_surface_set_size(surface: PCairoSurface; width_in_points: Double; height_in_points: Double); cdecl;
procedure cairo_ps_surface_dsc_comment(surface: PCairoSurface; const comment: PAnsiChar); cdecl;
procedure cairo_ps_surface_dsc_begin_setup(surface: PCairoSurface); cdecl;
procedure cairo_ps_surface_dsc_begin_page_setup(surface: PCairoSurface); cdecl;

{$endif} (* CAIRO_HAS_PS_SURFACE *)

implementation
uses Math;

{ TCairoMatrix }

procedure TCairoMatrix.InitIdentity;
begin
  //cairo_matrix_init_identity(@self)
  init(1, 0,
		   0, 1,
		   0, 0);
end;

procedure TCairoMatrix.Init(axx, ayx, axy, ayy, ax0, ay0: Double);
begin
  //cairo_matrix_init(@self, axx, ayx, axy, ayy, ax0, ay0);
  xx := axx;
  yx := ayx;
  xy := axy;
  yy := ayy;
  x0 := ax0;
  y0 := ay0;
end;

procedure TCairoMatrix.InitRotate(radians: Double);
var
  s, c: Extended;
begin
  //cairo_matrix_init_rotate(@self, radians);
  SinCos(radians, s, c);
  Init( c, s,
       -s, c,
        0, 0);
end;

procedure TCairoMatrix.InitScale(sx, sy: Double);
begin
  //cairo_matrix_init_scale(@self, sy, sy);
  Init(sx,  0,
		    0, sy,
		    0, 0);
end;

procedure TCairoMatrix.InitTranslate(tx, ty: Double);
begin
  //cairo_matrix_init_translate(@xx, tx, ty);
  Init( 1,  0,
		    0,  1,
		   tx, ty);
end;

function TCairoMatrix.Invert: TCairoStatus;
var
  det: double;
begin
  //Result := cairo_matrix_invert(@self);
  (* inv (A) = 1/det (A) * adj (A) *)
  det := xx*yy - yx*xy;

  if (det = 0) or (IsNan(det)) then
  begin
    Result := CAIRO_STATUS_INVALID_MATRIX;
    exit;
  end;

  Init(yy, -yx, -xy, xx, xy*y0 - yy*x0, yx*x0 - xx*y0);

  xx := xx / det;
  yx := yx / det;
  xy := xy / det;
  yy := yy / det;
  x0 := x0 / det;
  y0 := y0 / det;

  Result := CAIRO_STATUS_SUCCESS;
end;

procedure TCairoMatrix.Multiply(const a, b: TCairoMatrix);
var
  tmp: TCairoMatrix;
begin
  //cairo_matrix_multiply(@self, @a, @b);
  tmp.xx := a.xx * b.xx + a.yx * b.xy;
  tmp.yx := a.xx * b.yx + a.yx * b.yy;

  tmp.xy := a.xy * b.xx + a.yy * b.xy;
  tmp.yy := a.xy * b.yx + a.yy * b.yy;

  tmp.x0 := a.x0 * b.xx + a.y0 * b.xy + b.x0;
  tmp.y0 := a.x0 * b.yx + a.y0 * b.yy + b.y0;

  Self := tmp;
end;

procedure TCairoMatrix.Rotate(radians: Double);
var
  tmp: TCairoMatrix;
begin
  //cairo_matrix_rotate(@self, radians);
  tmp.InitRotate(radians);
  multiply(tmp, self);
end;

procedure TCairoMatrix.Scale(sx, sy: Double);
var
  tmp: TCairoMatrix;
begin
  //cairo_matrix_scale(@self, sx, sy);
  tmp.InitScale(sx, sy);
  Multiply(tmp, Self);
end;

procedure TCairoMatrix.TransformDistance(var dx, dy: Double);
var
  new_x, new_y: double;
begin
  //cairo_matrix_transform_distance(@self, @dx, @dy);
  new_x := (xx * dx + xy * dy);
  new_y := (yx * dx + yy * dy);

  dx := new_x;
  dy := new_y;
end;

procedure TCairoMatrix.TransformPoint(var x, y: Double);
begin
  //cairo_matrix_transform_point(@self, @x, @y);
  TransformDistance(x, y);
  x := x + x0;
  y := y + y0;
end;

procedure TCairoMatrix.Translate(tx, ty: Double);
begin
  cairo_matrix_translate(@self, tx, ty);
end;


const
{$ifdef UNIX}
  libcairo = 'cairo';
{$else}
  libcairo = 'libcairo-2.dll';
{$endif}

function cairo_version: Integer; cdecl; external libcairo;
function cairo_version_string: PAnsiChar; cdecl; external libcairo;
function cairo_create(target: PCairoSurface): PCairo; cdecl; external libcairo;
function cairo_reference(cr: PCairo): PCairo; cdecl; external libcairo;
procedure cairo_destroy(cr: PCairo); cdecl; external libcairo;
function cairo_get_reference_count(cr: PCairo): Cardinal; cdecl; external libcairo;
function cairo_get_user_data(cr: PCairo; const key: PCairoUserDataKey): Pointer; cdecl; external libcairo;
function cairo_set_user_data(cr: PCairo; const key: PCairoUserDataKey; user_data: Pointer; destroy: TCairoDestroyFunc): TCairoStatus; cdecl; external libcairo;
procedure cairo_save(cr: PCairo); cdecl; external libcairo;
procedure cairo_restore(cr: PCairo); cdecl; external libcairo;
procedure cairo_push_group(cr: PCairo); cdecl; external libcairo;
procedure cairo_push_group_with_content(cr: PCairo; content: TCairoContent); cdecl; external libcairo;
function cairo_pop_group(cr: PCairo): PCairoPattern; cdecl; external libcairo;
procedure cairo_pop_group_to_source(cr: PCairo); cdecl; external libcairo;
procedure cairo_set_operator(cr: PCairo; op: TCairoOperator); cdecl; external libcairo;
procedure cairo_set_source(cr: PCairo; source: PCairoPattern); cdecl; external libcairo;
procedure cairo_set_source_rgb(cr: PCairo; red, green, blue: Double); cdecl; external libcairo;
procedure cairo_set_source_rgba(cr: PCairo; red, green, blue, alpha: Double); cdecl; external libcairo;
procedure cairo_set_source_surface(cr: PCairo;  surface: PCairoSurface; x, y: Double); cdecl; external libcairo;
procedure cairo_set_tolerance(cr: PCairo; tolerance: Double); cdecl; external libcairo;
procedure cairo_set_antialias(cr: PCairo; antialias: TCairoAntialias); cdecl; external libcairo;
procedure cairo_set_fill_rule(cr: PCairo; fill_rule: TCairoFillRule); cdecl; external libcairo;
procedure cairo_set_line_width(cr: PCairo; width: Double); cdecl; external libcairo;
procedure cairo_set_line_cap(cr: PCairo; line_cap: TCairoLineCap); cdecl; external libcairo;
procedure cairo_set_line_join(cr: PCairo; line_join: TCairoLineJoin); cdecl; external libcairo;
procedure cairo_set_dash(cr: PCairo; const dashes: PDouble; num_dashes: Integer; offset: Double); cdecl; external libcairo;
procedure cairo_set_miter_limit(cr: PCairo; limit: Double); cdecl; external libcairo;
procedure cairo_translate(cr: PCairo; tx, ty: Double); cdecl; external libcairo;
procedure cairo_scale(cr: PCairo; sx, sy: Double); cdecl; external libcairo;
procedure cairo_rotate(cr: PCairo; angle: Double); cdecl; external libcairo;
procedure cairo_transform(cr: PCairo; const matrix: PCairoMatrix); cdecl; external libcairo;
procedure cairo_set_matrix(cr: PCairo; const matrix: PCairoMatrix); cdecl; external libcairo;
procedure cairo_identity_matrix(cr: PCairo); cdecl; external libcairo;
procedure cairo_user_to_device(cr: PCairo; x, y: PDouble); cdecl; external libcairo;
procedure cairo_user_to_device_distance(cr: PCairo; dx, dy: PDouble); cdecl; external libcairo;
procedure cairo_device_to_user(cr: PCairo; x, y: PDouble); cdecl; external libcairo;
procedure cairo_device_to_user_distance(cr: PCairo; dx, dy: PDouble); cdecl; external libcairo;
procedure cairo_new_path(cr: PCairo); cdecl; external libcairo;
procedure cairo_move_to(cr: PCairo; x, y: Double); cdecl; external libcairo;
procedure cairo_new_sub_path(cr: PCairo); cdecl; external libcairo;
procedure cairo_line_to(cr: PCairo; x, y: Double); cdecl; external libcairo;
procedure cairo_curve_to(cr: PCairo; x1, y1, x2, y2, x3, y3: Double); cdecl; external libcairo;
procedure cairo_arc(cr: PCairo; xc, yc, radius, angle1, angle2: Double); cdecl; external libcairo;
procedure cairo_arc_negative(cr: PCairo; xc, yc, radius, angle1, angle2: Double); cdecl; external libcairo;
procedure cairo_rel_move_to(cr: PCairo; dx, dy: Double); cdecl; external libcairo;
procedure cairo_rel_line_to(cr: PCairo; dx, dy: Double); cdecl; external libcairo;
procedure cairo_rel_curve_to(cr: PCairo; dx1, dy1, dx2, dy2, dx3, dy3: Double); cdecl; external libcairo;
procedure cairo_rectangle(cr: PCairo; x, y, width, height: Double); cdecl; external libcairo;
procedure cairo_close_path(cr: PCairo); cdecl; external libcairo;
procedure cairo_path_extents(cr: PCairo; x1, y1, x2, y2: PDouble); cdecl; external libcairo;
procedure cairo_paint(cr: PCairo); cdecl; external libcairo;
procedure cairo_paint_with_alpha(cr: PCairo; alpha: Double); cdecl; external libcairo;
procedure cairo_mask(cr: PCairo; pattern: PCairoPattern); cdecl; external libcairo;
procedure cairo_mask_surface(cr: PCairo; surface: PCairoSurface; surface_x, surface_y: Double); cdecl; external libcairo;
procedure cairo_stroke(cr: PCairo); cdecl; external libcairo;
procedure cairo_stroke_preserve(cr: PCairo); cdecl; external libcairo;
procedure cairo_fill(cr: PCairo); cdecl; external libcairo;
procedure cairo_fill_preserve(cr: PCairo); cdecl; external libcairo;
procedure cairo_copy_page(cr: PCairo); cdecl; external libcairo;
procedure cairo_show_page(cr: PCairo); cdecl; external libcairo;
function cairo_in_stroke(cr: PCairo; x, y: Double): TCairoBool; cdecl; external libcairo;
function cairo_in_fill(cr: PCairo; x, y: Double ): TCairoBool; cdecl; external libcairo;
function cairo_in_clip(cr: PCairo; x, y: Double): TCairoBool; cdecl; external libcairo;
procedure cairo_stroke_extents(cr: PCairo; x1, y1, x2, y2: PDouble); cdecl; external libcairo;
procedure cairo_fill_extents(cr: PCairo; x1, y1, x2, y2: PDouble); cdecl; external libcairo;
procedure cairo_reset_clip(cr: PCairo); cdecl; external libcairo;
procedure cairo_clip(cr: PCairo); cdecl; external libcairo;
procedure cairo_clip_preserve(cr: PCairo); cdecl; external libcairo;
procedure cairo_clip_extents(cr: PCairo; x1, y1, x2, y2: PDouble); cdecl; external libcairo;
function cairo_copy_clip_rectangle_list(cr: PCairo): PCairoRectangleList; cdecl; external libcairo;
procedure cairo_rectangle_list_destroy(rectangle_list: PCairoRectangleList); cdecl; external libcairo;
function cairo_glyph_allocate(num_glyphs: Integer): PCairoGlyph; cdecl; external libcairo;
procedure cairo_glyph_free(glyphs: PCairoGlyph); cdecl; external libcairo;
function cairo_text_cluster_allocate(num_clusters: Integer): PCairoTextCluster; cdecl; external libcairo;
procedure cairo_text_cluster_free(clusters: PCairoTextCluster); cdecl; external libcairo;
function cairo_font_options_create: PCairoFontOptions; cdecl; external libcairo;
function cairo_font_options_copy(const original: PCairoFontOptions): PCairoFontOptions; cdecl; external libcairo;
procedure cairo_font_options_destroy(options: PCairoFontOptions); cdecl; external libcairo;
function cairo_font_options_status(options: PCairoFontOptions): TCairoStatus; cdecl; external libcairo;
procedure cairo_font_options_merge(options: PCairoFontOptions; const other: PCairoFontOptions); cdecl; external libcairo;
function cairo_font_options_equal(const options: PCairoFontOptions; const other: PCairoFontOptions): TCairoBool; cdecl; external libcairo;
function cairo_font_options_hash(const options: PCairoFontOptions): Cardinal; cdecl; external libcairo;
procedure cairo_font_options_set_antialias(options: PCairoFontOptions; antialias: TCairoAntialias); cdecl; external libcairo;
function cairo_font_options_get_antialias(const options: PCairoFontOptions): TCairoAntialias; cdecl; external libcairo;
procedure cairo_font_options_set_subpixel_order(options: PCairoFontOptions; subpixel_order: TCairoSubpixelOrder); cdecl; external libcairo;
function cairo_font_options_get_subpixel_order(const options: PCairoFontOptions): TCairoSubpixelOrder; cdecl; external libcairo;
procedure cairo_font_options_set_hint_style(options: PCairoFontOptions; hint_style: TCairoHintStyle); cdecl; external libcairo;
function cairo_font_options_get_hint_style(const options: PCairoFontOptions): TCairoHintStyle; cdecl; external libcairo;
procedure cairo_font_options_set_hint_metrics(options: PCairoFontOptions; hint_metrics: TCairoHintMetrics); cdecl; external libcairo;
function cairo_font_options_get_hint_metrics(const options: PCairoFontOptions): TCairoHintMetrics; cdecl; external libcairo;
procedure cairo_select_font_face(cr: PCairo; const family: PAnsiChar; slant: TCairoFontSlant; weight: TCairoFontWeight); cdecl; external libcairo;
procedure cairo_set_font_size(cr: PCairo; size: Double); cdecl; external libcairo;
procedure cairo_set_font_matrix(cr: PCairo; const matrix: PCairoMatrix); cdecl; external libcairo;
procedure cairo_get_font_matrix(cr: PCairo; matrix: PCairoMatrix); cdecl; external libcairo;
procedure cairo_set_font_options(cr: PCairo; const options: PCairoFontOptions); cdecl; external libcairo;
procedure cairo_get_font_options(cr: PCairo; options: PCairoFontOptions); cdecl; external libcairo;
procedure cairo_set_font_face(cr: PCairo; font_face: PCairoFontFace); cdecl; external libcairo;
function cairo_get_font_face(cr: PCairo): PCairoFontFace; cdecl; external libcairo;
procedure cairo_set_scaled_font(cr: PCairo; const scaled_font: PCairoScaledFont); cdecl; external libcairo;
function cairo_get_scaled_font(cr: PCairo): PCairoScaledFont; cdecl; external libcairo;
procedure cairo_show_text(cr: PCairo; const utf8: PAnsiChar); cdecl; external libcairo;
procedure cairo_show_glyphs(cr: PCairo; const glyphs: PCairoGlyph; num_glyphs: Integer); cdecl; external libcairo;
procedure cairo_show_text_glyphs(cr: PCairo; const utf8: PAnsiChar; utf8_len: Integer; const glyphs: PCairoGlyph; num_glyphs: Integer; const clusters: PCairoTextCluster; num_clusters: Integer; cluster_flags: TCairoTextClusterFlags); cdecl; external libcairo;
procedure cairo_text_path(cr: PCairo; const utf8: PAnsiChar); cdecl; external libcairo;
procedure cairo_glyph_path(cr: PCairo; const glyphs: PCairoGlyph; num_glyphs: Integer); cdecl; external libcairo;
procedure cairo_text_extents(cr: PCairo; const utf8: PAnsiChar; extents: PCairoTextExtents); cdecl; external libcairo;
procedure cairo_glyph_extents(cr: PCairo; const glyphs: PCairoGlyph; num_glyphs: Integer; extents: PCairoTextExtents); cdecl; external libcairo;
procedure cairo_font_extents(cr: PCairo; extents: PCairoFontExtents); cdecl; external libcairo;
function cairo_font_face_reference(font_face: PCairoFontFace): PCairoFontFace; cdecl; external libcairo;
procedure cairo_font_face_destroy(font_face: PCairoFontFace); cdecl; external libcairo;
function cairo_font_face_get_reference_count(font_face: PCairoFontFace): Cardinal; cdecl; external libcairo;
function cairo_font_face_status(font_face: PCairoFontFace): TCairoStatus; cdecl; external libcairo;
function cairo_font_face_get_type(font_face: PCairoFontFace): TCairoFontType; cdecl; external libcairo;
function cairo_font_face_get_user_data(font_face: PCairoFontFace; const key: PCairoUserDataKey): Pointer; cdecl; external libcairo;
function cairo_font_face_set_user_data(font_face: PCairoFontFace; const key: PCairoUserDataKey; user_data: Pointer; destroy: TCairoDestroyFunc): TCairoStatus; cdecl; external libcairo;
function cairo_scaled_font_create(font_face: PCairoFontFace; const font_matrix, ctm: PCairoMatrix; const options: PCairoFontOptions): PCairoScaledFont; cdecl; external libcairo;
function cairo_scaled_font_reference(scaled_font: PCairoScaledFont): PCairoScaledFont; cdecl; external libcairo;
procedure cairo_scaled_font_destroy(scaled_font: PCairoScaledFont); cdecl; external libcairo;
function cairo_scaled_font_get_reference_count(scaled_font: PCairoScaledFont): Cardinal; cdecl; external libcairo;
function cairo_scaled_font_status(scaled_font: PCairoScaledFont): TCairoStatus; cdecl; external libcairo;
function cairo_scaled_font_get_type(scaled_font: PCairoScaledFont): TCairoFontType; cdecl; external libcairo;
function cairo_scaled_font_get_user_data(scaled_font: PCairoScaledFont; const key: PCairoUserDataKey): Pointer; cdecl; external libcairo;
function cairo_scaled_font_set_user_data(scaled_font: PCairoScaledFont; const key: PCairoUserDataKey; user_data: Pointer; destroy: TCairoDestroyFunc): TCairoStatus; cdecl; external libcairo;
procedure cairo_scaled_font_extents(scaled_font: PCairoScaledFont; extents: PCairoFontExtents); cdecl; external libcairo;
procedure cairo_scaled_font_text_extents(scaled_font: PCairoScaledFont; const utf8: PAnsiChar; extents: PCairoTextExtents); cdecl; external libcairo;
procedure cairo_scaled_font_glyph_extents(scaled_font: PCairoScaledFont; const glyphs: PCairoGlyph; num_glyphs: Integer; extents: PCairoTextExtents); cdecl; external libcairo;
function cairo_scaled_font_text_to_glyphs(scaled_font: PCairoScaledFont; x: Double; y: Double; const utf8: PAnsiChar; utf8_len: Integer; glyphs: PPCairoGlyph; num_glyphs: PInteger; clusters: PPCairoTextCluster; num_clusters: PInteger; cluster_flags: PCairoTextClusterFlags): TCairoStatus; cdecl; external libcairo;
function cairo_scaled_font_get_font_face(scaled_font: PCairoScaledFont): PCairoFontFace; cdecl; external libcairo;
procedure cairo_scaled_font_get_font_matrix(scaled_font: PCairoScaledFont; font_matrix: PCairoMatrix); cdecl; external libcairo;
procedure cairo_scaled_font_get_ctm(scaled_font: PCairoScaledFont; ctm: PCairoMatrix); cdecl; external libcairo;
procedure cairo_scaled_font_get_scale_matrix(scaled_font: PCairoScaledFont; scale_matrix: PCairoMatrix); cdecl; external libcairo;
procedure cairo_scaled_font_get_font_options(scaled_font: PCairoScaledFont; options: PCairoFontOptions); cdecl; external libcairo;
function cairo_toy_font_face_create(const family: PAnsiChar; slant: TCairoFontSlant; weight: TCairoFontWeight): PCairoFontFace; cdecl; external libcairo;
function cairo_toy_font_face_get_family(font_face: PCairoFontFace): PAnsiChar; cdecl; external libcairo;
function cairo_toy_font_face_get_slant(font_face: PCairoFontFace): TCairoFontSlant; cdecl; external libcairo;
function cairo_toy_font_face_get_weight(font_face: PCairoFontFace): TCairoFontWeight; cdecl; external libcairo;
function cairo_user_font_face_create: PCairoFontFace; cdecl; external libcairo;
procedure cairo_user_font_face_set_init_func(font_face: PCairoFontFace; init_func: TCairoUserScaledFontInitFunc); cdecl; external libcairo;
procedure cairo_user_font_face_set_render_glyph_func(font_face: PCairoFontFace; render_glyph_func: TCairoUserScaledFontRenderGlyphFunc); cdecl; external libcairo;
procedure cairo_user_font_face_set_text_to_glyphs_func(font_face: PCairoFontFace; text_to_glyphs_func: TCairoUserScaledFontTextToGlyphsFunc); cdecl; external libcairo;
procedure cairo_user_font_face_set_unicode_to_glyph_func(font_face: PCairoFontFace; unicode_to_glyph_func: TCairoUserScaledFontUnicodeToGlyphFunc); cdecl; external libcairo;
function cairo_user_font_face_get_init_func(font_face: PCairoFontFace): TCairoUserScaledFontInitFunc; cdecl; external libcairo;
function cairo_user_font_face_get_render_glyph_func(font_face: PCairoFontFace): TCairoUserScaledFontRenderGlyphFunc; cdecl; external libcairo;
function cairo_user_font_face_get_text_to_glyphs_func(font_face: PCairoFontFace): TCairoUserScaledFontTextToGlyphsFunc; cdecl; external libcairo;
function cairo_user_font_face_get_unicode_to_glyph_func(font_face: PCairoFontFace): TCairoUserScaledFontUnicodeToGlyphFunc; cdecl; external libcairo;
function cairo_get_operator(cr: PCairo): TCairoOperator; cdecl; external libcairo;
function cairo_get_source(cr: PCairo): PCairoPattern; cdecl; external libcairo;
function cairo_get_tolerance(cr: PCairo): Double; cdecl; external libcairo;
function cairo_get_antialias(cr: PCairo): TCairoAntialias; cdecl; external libcairo;
function cairo_has_current_point(cr: PCairo): TCairoBool; cdecl; external libcairo;
procedure cairo_get_current_point(cr: PCairo; x: PDouble; y: PDouble); cdecl; external libcairo;
function cairo_get_fill_rule(cr: PCairo): TCairoFillRule; cdecl; external libcairo;
function cairo_get_line_width(cr: PCairo): Double; cdecl; external libcairo;
function cairo_get_line_cap(cr: PCairo): TCairoLineCap; cdecl; external libcairo;
function cairo_get_line_join(cr: PCairo): TCairoLineJoin; cdecl; external libcairo;
function cairo_get_miter_limit(cr: PCairo): Double; cdecl; external libcairo;
function cairo_get_dash_count(cr: PCairo): Integer; cdecl; external libcairo;
procedure cairo_get_dash(cr: PCairo; dashes: PDouble; offset: PDouble); cdecl; external libcairo;
procedure cairo_get_matrix(cr: PCairo; matrix: PCairoMatrix); cdecl; external libcairo;
function cairo_get_target(cr: PCairo): PCairoSurface; cdecl; external libcairo;
function cairo_get_group_target(cr: PCairo): PCairoSurface; cdecl; external libcairo;
function cairo_copy_path(cr: PCairo): PCairoPath; cdecl; external libcairo;
function cairo_copy_path_flat(cr: PCairo): PCairoPath; cdecl; external libcairo;
procedure cairo_append_path(cr: PCairo; const path: PCairoPath); cdecl; external libcairo;
procedure cairo_path_destroy(path: PCairoPath); cdecl; external libcairo;
function cairo_status(cr: PCairo): TCairoStatus; cdecl; external libcairo;
function cairo_status_to_string(status: TCairoStatus): PAnsiChar; cdecl; external libcairo;
function cairo_device_reference(device: PCairoDevice): PCairoDevice; cdecl; external libcairo;
function cairo_device_get_type(device: PCairoDevice): TCairoDeviceType; cdecl; external libcairo;
function cairo_device_status(device: PCairoDevice): TCairoStatus; cdecl; external libcairo;
function cairo_device_acquire (device: PCairoDevice): TCairoStatus; cdecl; external libcairo;
procedure cairo_device_release(device: PCairoDevice); cdecl; external libcairo;
procedure cairo_device_flush(device: PCairoDevice); cdecl; external libcairo;
procedure cairo_device_finish(device: PCairoDevice); cdecl; external libcairo;
procedure cairo_device_destroy(device: PCairoDevice); cdecl; external libcairo;
function cairo_device_get_reference_count(device: PCairoDevice): Cardinal; cdecl; external libcairo;
function cairo_device_get_user_data(device: PCairoDevice; const key: PCairoUserDataKey): Pointer; cdecl; external libcairo;
function cairo_device_set_user_data (device: PCairoDevice; const key: PCairoUserDataKey; user_data: Pointer; destroy: TCairoDestroyFunc): TCairoStatus; cdecl; external libcairo;
function cairo_surface_create_similar(other: PCairoSurface; content: TCairoContent; width: Integer; height: Integer): PCairoSurface; cdecl; external libcairo;
function cairo_surface_create_for_rectangle(target: PCairoSurface; x, y, width, height: Double): PCairoSurface; cdecl; external libcairo;
function cairo_surface_reference(surface: PCairoSurface): PCairoSurface; cdecl; external libcairo;
procedure cairo_surface_finish(surface: PCairoSurface); cdecl; external libcairo;
procedure cairo_surface_destroy(surface: PCairoSurface); cdecl; external libcairo;
function cairo_surface_get_device(surface: PCairoSurface): PCairoDevice; cdecl; external libcairo;
function cairo_surface_get_reference_count(surface: PCairoSurface): Cardinal; cdecl; external libcairo;
function cairo_surface_status(surface: PCairoSurface): TCairoStatus; cdecl; external libcairo;
function cairo_surface_get_type(surface: PCairoSurface): TCairoSurfaceType; cdecl; external libcairo;
function cairo_surface_get_content(surface: PCairoSurface): TCairoContent; cdecl; external libcairo;
function cairo_surface_write_to_png(surface: PCairoSurface; const filename: PAnsiChar): TCairoStatus; cdecl; external libcairo;
function cairo_surface_write_to_png_stream(surface: PCairoSurface; write_func: TCairoWriteFunc; closure: Pointer): TCairoStatus; cdecl; external libcairo;
function cairo_surface_get_user_data(surface: PCairoSurface; const key: PCairoUserDataKey): Pointer; cdecl; external libcairo;
function cairo_surface_set_user_data(surface: PCairoSurface; const key: PCairoUserDataKey; user_data: Pointer; destroy: TCairoDestroyFunc): TCairoStatus; cdecl; external libcairo;
procedure cairo_surface_get_mime_data (surface: PCairoSurface; const mime_type: PAnsiChar; out data: Pointer; out length: Cardinal); cdecl; external libcairo;
function cairo_surface_set_mime_data (surface: PCairoSurface; const mime_type: PAnsiChar; const data : Pointer; length: Cardinal;	destroy: TCairoDestroyFunc; closure: Pointer): TCairoStatus; cdecl; external libcairo;
procedure cairo_surface_get_font_options(surface: PCairoSurface; options: PCairoFontOptions); cdecl; external libcairo;
procedure cairo_surface_flush(surface: PCairoSurface); cdecl; external libcairo;
procedure cairo_surface_mark_dirty(surface: PCairoSurface); cdecl; external libcairo;
procedure cairo_surface_mark_dirty_rectangle(surface: PCairoSurface; x, y, width, height: Integer); cdecl; external libcairo;
procedure cairo_surface_set_device_offset(surface: PCairoSurface; x_offset, y_offset: Double); cdecl; external libcairo;
procedure cairo_surface_get_device_offset(surface: PCairoSurface; x_offset, y_offset: PDouble); cdecl; external libcairo;
procedure cairo_surface_set_fallback_resolution(surface: PCairoSurface; x_pixels_per_inch, y_pixels_per_inch: Double); cdecl; external libcairo;
procedure cairo_surface_get_fallback_resolution(surface: PCairoSurface; x_pixels_per_inch, y_pixels_per_inch: PDouble); cdecl; external libcairo;
procedure cairo_surface_copy_page(surface: PCairoSurface); cdecl; external libcairo;
procedure cairo_surface_show_page(surface: PCairoSurface); cdecl; external libcairo;
function cairo_surface_has_show_text_glyphs(surface: PCairoSurface): TCairoBool; cdecl; external libcairo;
function cairo_image_surface_create(format: TCairoFormat; width: Integer; height: Integer): PCairoSurface; cdecl; external libcairo;
function cairo_format_stride_for_width(format: TCairoFormat; width: Integer): Integer; cdecl; external libcairo;
function cairo_image_surface_create_for_data( data: PByte; format: TCairoFormat; width: Integer; height: Integer; stride: Integer): PCairoSurface; cdecl; external libcairo;
function cairo_image_surface_get_data(surface: PCairoSurface): PByte; cdecl; external libcairo;
function cairo_image_surface_get_format(surface: PCairoSurface): TCairoFormat; cdecl; external libcairo;
function cairo_image_surface_get_width(surface: PCairoSurface): Integer; cdecl; external libcairo;
function cairo_image_surface_get_height(surface: PCairoSurface): Integer; cdecl; external libcairo;
function cairo_image_surface_get_stride(surface: PCairoSurface): Integer; cdecl; external libcairo;
{$ifdef CAIRO_HAS_PNG_FUNCTIONS}
function cairo_image_surface_create_from_png(const filename: PAnsiChar): PCairoSurface; cdecl; external libcairo;
function cairo_image_surface_create_from_png_stream(read_func: TCairoReadFunc; closure: Pointer): PCairoSurface; cdecl; external libcairo;
{$endif}
function cairo_recording_surface_create(content: TCairoContent; const extents: PCairoRectangle): PCairoSurface; cdecl; external libcairo;
procedure cairo_recording_surface_ink_extents(surface: PCairoSurface; x0, y0, width, height: PDouble); cdecl; external libcairo;
function cairo_pattern_create_rgb(red: Double; green: Double; blue: Double): PCairoPattern; cdecl; external libcairo;
function cairo_pattern_create_rgba(red: Double; green: Double; blue: Double; alpha: Double): PCairoPattern; cdecl; external libcairo;
function cairo_pattern_create_for_surface(surface: PCairoSurface): PCairoPattern; cdecl; external libcairo;
function cairo_pattern_create_linear(x0: Double; y0: Double; x1: Double; y1: Double): PCairoPattern; cdecl; external libcairo;
function cairo_pattern_create_radial(cx0: Double; cy0: Double; radius0: Double; cx1: Double; cy1: Double; radius1: Double): PCairoPattern; cdecl; external libcairo;
function cairo_pattern_reference(pattern: PCairoPattern): PCairoPattern; cdecl; external libcairo;
procedure cairo_pattern_destroy(pattern: PCairoPattern); cdecl; external libcairo;
function cairo_pattern_get_reference_count(pattern: PCairoPattern): Cardinal; cdecl; external libcairo;
function cairo_pattern_status(pattern: PCairoPattern): TCairoStatus; cdecl; external libcairo;
function cairo_pattern_get_user_data(pattern: PCairoPattern; const key: PCairoUserDataKey): Pointer; cdecl; external libcairo;
function cairo_pattern_set_user_data(pattern: PCairoPattern; const key: PCairoUserDataKey; user_data: Pointer; destroy: TCairoDestroyFunc): TCairoStatus; cdecl; external libcairo;
function cairo_pattern_get_type(pattern: PCairoPattern): TCairoPatternType; cdecl; external libcairo;
procedure cairo_pattern_add_color_stop_rgb(pattern: PCairoPattern; offset, red, green, blue: Double); cdecl; external libcairo;
procedure cairo_pattern_add_color_stop_rgba(pattern: PCairoPattern; offset, red, green, blue, alpha: Double); cdecl; external libcairo;
procedure cairo_pattern_set_matrix(pattern: PCairoPattern; const matrix: PCairoMatrix); cdecl; external libcairo;
procedure cairo_pattern_get_matrix(pattern: PCairoPattern; matrix: PCairoMatrix); cdecl; external libcairo;
procedure cairo_pattern_set_extend(pattern: PCairoPattern; extend: TCairoExtend); cdecl; external libcairo;
function cairo_pattern_get_extend(pattern: PCairoPattern): TCairoExtend; cdecl; external libcairo;
procedure cairo_debug_reset_static_data; cdecl; external libcairo;
procedure cairo_pattern_set_filter(pattern: PCairoPattern; filter: TCairoFilter); cdecl; external libcairo;
function cairo_pattern_get_filter(pattern: PCairoPattern): TCairoFilter; cdecl; external libcairo;
function cairo_pattern_get_rgba(pattern: PCairoPattern; red, green, blue, alpha: PDouble): TCairoStatus; cdecl; external libcairo;
function cairo_pattern_get_surface(pattern: PCairoPattern; var surface: PCairoSurface): TCairoStatus; cdecl; external libcairo;
function cairo_pattern_get_color_stop_rgba(pattern: PCairoPattern; index: Integer; offset, red, green, blue, alpha: PDouble): TCairoStatus; cdecl; external libcairo;
function cairo_pattern_get_color_stop_count(pattern: PCairoPattern; count: PInteger): TCairoStatus; cdecl; external libcairo;
function cairo_pattern_get_linear_points(pattern: PCairoPattern; x0, y0, x1, y1: PDouble): TCairoStatus; cdecl; external libcairo;
function cairo_pattern_get_radial_circles(pattern: PCairoPattern; x0, y0, r0, x1, y1, r1: PDouble): TCairoStatus; cdecl; external libcairo;
procedure cairo_matrix_init(matrix: PCairoMatrix; xx, yx, xy, yy, x0, y0: Double); cdecl; external libcairo;
procedure cairo_matrix_init_identity(matrix: PCairoMatrix); cdecl; external libcairo;
procedure cairo_matrix_init_translate(matrix: PCairoMatrix; tx, ty: Double); cdecl; external libcairo;
procedure cairo_matrix_init_scale(matrix: PCairoMatrix; sx, sy: Double); cdecl; external libcairo;
procedure cairo_matrix_init_rotate(matrix: PCairoMatrix; radians: Double); cdecl; external libcairo;
procedure cairo_matrix_translate(matrix: PCairoMatrix; tx, ty: Double); cdecl; external libcairo;
procedure cairo_matrix_scale(matrix: PCairoMatrix; sx, sy: Double); cdecl; external libcairo;
procedure cairo_matrix_rotate(matrix: PCairoMatrix; radians: Double); cdecl; external libcairo;
function cairo_matrix_invert(matrix: PCairoMatrix): TCairoStatus; cdecl; external libcairo;
procedure cairo_matrix_multiply(result: PCairoMatrix; const a, b: PCairoMatrix); cdecl; external libcairo;
procedure cairo_matrix_transform_distance(const matrix: PCairoMatrix; dx, dy: PDouble); cdecl; external libcairo;
procedure cairo_matrix_transform_point(const matrix: PCairoMatrix; x, y: PDouble); cdecl; external libcairo;

function cairo_region_create: PCairoRegion; cdecl; external libcairo;
function cairo_region_create_rectangle(const rectangle: PCairoRectangleInt): PCairoRegion; cdecl; external libcairo;
function cairo_region_create_rectangles(const rects: PCairoRectangleInt; count: Integer): PCairoRegion; cdecl; external libcairo;
function cairo_region_copy(const original: PCairoRegion): PCairoRegion; cdecl; external libcairo;
function cairo_region_reference(region: PCairoRegion): PCairoRegion; cdecl; external libcairo;
procedure cairo_region_destroy(region: PCairoRegion); cdecl; external libcairo;
function cairo_region_equal(const a, b: PCairoRegion): TCairoBool; cdecl; external libcairo;
function cairo_region_status(const region: PCairoRegion): TCairoStatus; cdecl; external libcairo;
procedure cairo_region_get_extents(const region: PCairoRegion; extents: PCairoRectangleInt); cdecl; external libcairo;
function cairo_region_num_rectangles (const region: PCairoRegion): Integer; cdecl; external libcairo;
procedure cairo_region_get_rectangle(const region: PCairoRegion; nth: Integer; rectangle: PCairoRectangleInt); cdecl; external libcairo;
function cairo_region_is_empty(const region: PCairoRegion): TCairoBool; cdecl; external libcairo;
function cairo_region_contains_rectangle (const region: PCairoRegion; const rectangle: PCairoRectangleInt): TCairoRegionOverlap; cdecl; external libcairo;
function cairo_region_contains_point(const region: PCairoRegion; x, y: Integer): TCairoBool; cdecl; external libcairo;
procedure cairo_region_translate(region: PCairoRegion; dx, dy: Integer); cdecl; external libcairo;
function cairo_region_subtract (dst: PCairoRegion; const other: PCairoRegion): TCairoStatus; cdecl; external libcairo;
function cairo_region_subtract_rectangle (dst: PCairoRegion; const rectangle: PCairoRectangleInt): TCairoStatus; cdecl; external libcairo;
function cairo_region_intersect(dst: PCairoRegion; const other: PCairoRegion): TCairoStatus; cdecl; external libcairo;
function cairo_region_intersect_rectangle (dst: PCairoRegion; const rectangle: PCairoRectangleInt): TCairoStatus; cdecl; external libcairo;
function cairo_region_union (dst: PCairoRegion; const other: PCairoRegion): TCairoStatus; cdecl; external libcairo;
function cairo_region_union_rectangle (dst: PCairoRegion; const rectangle: PCairoRectangleInt): TCairoStatus; cdecl; external libcairo;
function cairo_region_xor(dst: PCairoRegion; const other: PCairoRegion): TCairoStatus; cdecl; external libcairo;
function cairo_region_xor_rectangle (dst: PCairoRegion; const rectangle: PCairoRectangleInt): TCairoStatus; cdecl; external libcairo;

{$ifdef CAIRO_HAS_WIN32_SURFACE}
function cairo_win32_surface_create(hdc: HDC): PCairoSurface; cdecl; external libcairo;
function cairo_win32_printing_surface_create(hdc: HDC): PCairoSurface; cdecl; external libcairo;
function cairo_win32_surface_create_with_ddb(hdc: HDC; format: TCairoFormat; width: Integer; height: Integer): PCairoSurface; cdecl; external libcairo;
function cairo_win32_surface_create_with_dib(format: TCairoFormat; width: Integer; height: Integer): PCairoSurface; cdecl; external libcairo;
function cairo_win32_surface_get_dc(surface: PCairoSurface): HDC; cdecl; external libcairo;
function cairo_win32_surface_get_image(surface: PCairoSurface): PCairoSurface; cdecl; external libcairo;
{$ifdef CAIRO_HAS_WIN32_FONT}
function cairo_win32_font_face_create_for_logfontw(logfont: PLOGFONTW): PCairoFontFace; cdecl; external libcairo;
function cairo_win32_font_face_create_for_hfont(font: HFONT): PCairoFontFace; cdecl; external libcairo;
function cairo_win32_font_face_create_for_logfontw_hfont(logfont: PLOGFONTW; font: HFONT): PCairoFontFace; cdecl; external libcairo;
function cairo_win32_scaled_font_select_font(scaled_font: PCairoScaledFont; hdc: HDC): TCairoStatus; cdecl; external libcairo;
procedure cairo_win32_scaled_font_done_font(scaled_font: PCairoScaledFont); cdecl; external libcairo;
function cairo_win32_scaled_font_get_metrics_factor(scaled_font: PCairoScaledFont): Double; cdecl; external libcairo;
procedure cairo_win32_scaled_font_get_logical_to_device(scaled_font: PCairoScaledFont; logical_to_device: PCairoMatrix); cdecl; external libcairo;
procedure cairo_win32_scaled_font_get_device_to_logical(scaled_font: PCairoScaledFont; device_to_logical: PCairoMatrix); cdecl; external libcairo;
{$endif}
{$endif}

{$ifdef CAIRO_HAS_XLIB_SURFACE}
function cairo_xlib_surface_create(dpy: PDisplay; drawable: TDrawable; visual: PVisual; width: Integer; height: Integer): PCairoSurface; cdecl; external libcairo;
function cairo_xlib_surface_create_for_bitmap(dpy: PDisplay; bitmap: TPixmap; screen: PScreen; width: Integer; height: Integer): PCairoSurface; cdecl; external libcairo;
procedure cairo_xlib_surface_set_size(surface: PCairoSurface; width: Integer; height: Integer); cdecl; external libcairo;
procedure cairo_xlib_surface_set_drawable(surface: PCairoSurface; drawable: TDrawable; width: Integer; height: Integer); cdecl; external libcairo;
function cairo_xlib_surface_get_display(surface: PCairoSurface): PDisplay; cdecl; external libcairo;
function cairo_xlib_surface_get_drawable(surface: PCairoSurface): TDrawable; cdecl; external libcairo;
function cairo_xlib_surface_get_screen(surface: PCairoSurface): PScreen; cdecl; external libcairo;
function cairo_xlib_surface_get_visual(surface: PCairoSurface): PVisual; cdecl; external libcairo;
function cairo_xlib_surface_get_depth(surface: PCairoSurface): Integer; cdecl; external libcairo;
function cairo_xlib_surface_get_width(surface: PCairoSurface): Integer; cdecl; external libcairo;
function cairo_xlib_surface_get_height(surface: PCairoSurface): Integer; cdecl; external libcairo;
{$endif} (* CAIRO_HAS_XLIB_SURFACE *)

{$ifdef CAIRO_HAS_XLIB_XRENDER_SURFACE}
function cairo_xlib_surface_create_with_xrender_format(dpy: PDisplay; drawable: TDrawable; screen: PScreen; format: PXRenderPictFormat; width, height: Integer): PCairoSurface; cdecl; external libcairo;
function cairo_xlib_surface_get_xrender_format(surface: PCairoSurface): PXRenderPictFormat; cdecl; external libcairo;
{$endif} (* CAIRO_HAS_XLIB_XRENDER_SURFACE *)

{$ifdef CAIRO_HAS_FT_FONT}
function cairo_ft_font_face_create_for_pattern(pattern: PFcPattern): PCairoFontFace; cdecl; external libcairo;
procedure cairo_ft_font_options_substitute(const options: PCairoFontOptions; pattern: PFcPattern); cdecl; external libcairo;
function cairo_ft_font_face_create_for_ft_face(face: TFT_Face; load_flags: Integer): PCairoFontFace; cdecl; external libcairo;
function cairo_ft_scaled_font_lock_face(scaled_font: PCairoScaledFont): TFT_Face; cdecl; external libcairo;
procedure cairo_ft_scaled_font_unlock_face(scaled_font: PCairoScaledFont); cdecl; external libcairo;
{$endif} (* CAIRO_HAS_FT_FONT *)

{$ifdef CAIRO_HAS_SVG_SURFACE}
function cairo_svg_surface_create(const filename: PAnsiChar; width_in_points, height_in_points: Double): PCairoSurface; cdecl; external libcairo;
function cairo_svg_surface_create_for_stream(write_func: TCairoWriteFunc; closure: Pointer; width_in_points, height_in_points: Double): PCairoSurface; cdecl; external libcairo;
procedure cairo_svg_surface_restrict_to_version(surface: PCairoSurface; version: TCairoSVGVersion); cdecl; external libcairo;
procedure cairo_svg_get_versions(var versions: PCairoSVGVersion; num_versions: PInteger); cdecl; external libcairo;
function cairo_svg_version_to_string(version: TCairoSVGVersion): PAnsiChar; cdecl; external libcairo;
{$endif} (* CAIRO_HAS_SVG_SURFACE *)

{$ifdef CAIRO_HAS_PDF_SURFACE}

function cairo_pdf_surface_create(const filename: PAnsiChar; width_in_points, height_in_points: Double): PCairoSurface; cdecl; external libcairo;
function cairo_pdf_surface_create_for_stream(write_func: TCairoWriteFunc; closure: Pointer; width_in_points, height_in_points: Double): PCairoSurface; cdecl; external libcairo;
procedure cairo_pdf_surface_set_size(surface: PCairoSurface; width_in_points: Double; height_in_points: Double); cdecl; external libcairo;
procedure cairo_pdf_surface_restrict_to_version(surface: PCairoSurface; version: TCairoPdfVersion); cdecl; cdecl; external libcairo;
procedure cairo_pdf_get_versions(out versions: PCairoPdfVersion; out num_versions: Integer); cdecl; cdecl; external libcairo;
function cairo_pdf_version_to_string(version: TCairoPdfVersion): PAnsiChar; cdecl; cdecl; external libcairo;

{$endif} (* CAIRO_HAS_PDF_SURFACE *)

{$ifdef CAIRO_HAS_PS_SURFACE}

function cairo_ps_surface_create(const filename: PAnsiChar; width_in_points: Double; height_in_points: Double): PCairoSurface; cdecl; external libcairo;
function cairo_ps_surface_create_for_stream(write_func: TCairoWriteFunc; closure: Pointer; width_in_points: Double; height_in_points: Double): PCairoSurface; cdecl; external libcairo;
procedure cairo_ps_surface_restrict_to_level(surface: PCairoSurface; level: TCairoPSLevel); cdecl; external libcairo;
procedure cairo_ps_get_levels(var levels: PCairoPSLevel; num_levels: PInteger); cdecl; external libcairo;
function cairo_ps_level_to_string(level: TCairoPSLevel): PAnsiChar; cdecl; external libcairo;
procedure cairo_ps_surface_set_eps(surface: PCairoSurface; eps: TCairoBool); cdecl; external libcairo;
function cairo_ps_surface_get_eps(surface: PCairoSurface): TCairoBool; cdecl; external libcairo;
procedure cairo_ps_surface_set_size(surface: PCairoSurface; width_in_points: Double; height_in_points: Double); cdecl; external libcairo;
procedure cairo_ps_surface_dsc_comment(surface: PCairoSurface; const comment: PAnsiChar); cdecl; external libcairo;
procedure cairo_ps_surface_dsc_begin_setup(surface: PCairoSurface); cdecl; external libcairo;
procedure cairo_ps_surface_dsc_begin_page_setup(surface: PCairoSurface); cdecl; external libcairo;

{$endif} (* CAIRO_HAS_PS_SURFACE *)

end.

