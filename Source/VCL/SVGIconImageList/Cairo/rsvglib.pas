unit rsvglib;
{$ALIGN ON}
{$MINENUMSIZE 4}
interface

uses cairolib;

(*
   rsvg.h: SAX-based renderer for SVG files into a GdkPixbuf.

   Copyright (C) 2000 Eazel, Inc.

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this program; if not, write to the
   Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.

   Author: Raph Levien <raph@artofcode.com>
*)

//#include <gdk-pixbuf/gdk-pixbuf.h>


//#define RSVG_TYPE_HANDLE                  (rsvg_handle_get_type ())
//#define RSVG_HANDLE(obj)                  (G_TYPE_CHECK_INSTANCE_CAST ((obj), RSVG_TYPE_HANDLE, RsvgHandle))
//#define RSVG_HANDLE_CLASS(klass)          (G_TYPE_CHECK_CLASS_CAST ((klass), RSVG_TYPE_HANDLE, RsvgHandleClass))
//#define RSVG_IS_HANDLE(obj)               (G_TYPE_CHECK_INSTANCE_TYPE ((obj), RSVG_TYPE_HANDLE))
//#define RSVG_IS_HANDLE_CLASS(klass)       (G_TYPE_CHECK_CLASS_TYPE ((klass), RSVG_TYPE_HANDLE))
//#define RSVG_HANDLE_GET_CLASS(obj)        (G_TYPE_INSTANCE_GET_CLASS ((obj), RSVG_TYPE_HANDLE, RsvgHandleClass))

function rsvg_handle_get_type: Cardinal; cdecl;

(**
 * An enumeration representing possible error domains
 *)
type
  RsvgError = (
    RSVG_ERROR_FAILED
  );

(**
 *
 *)
//#define RSVG_ERROR (rsvg_error_quark ())
function rsvg_error_quark: Cardinal; cdecl;

(**
 * The RsvgHandle is an object representing the parsed form of a SVG
 *)

type
  PRsvgHandle = ^TRsvgHandle;
  TRsvgHandle = record
//    GObject parent;
//    RsvgHandlePrivate *priv;
//    gpointer _abi_padding[15];
  end;

  PRsvgHandlePrivate = ^TRsvgHandlePrivate;
  TRsvgHandlePrivate = record end;

  PRsvgHandleClass = ^TRsvgHandleClass;
  TRsvgHandleClass = record
//    GObjectClass parent;
//    gpointer _abi_padding[15];
  end;


  PRsvgDimensionData = ^TRsvgDimensionData;
  TRsvgDimensionData = record
	  // SVG's width, in pixels
    width: Integer;
	  // SVG's height, in pixels
    height: Integer;
	  // em
    em: Double;
	  // ex
    ex: Double;
  end;

(**
 * Position of an SVG fragment.
 **)

  PRsvgPositionData = ^TRsvgPositionData;
  TRsvgPositionData = record
    x, y: integer;
  end;

PPGError = ^PGError;
PGError = ^GError;
GError = record
  domain: Cardinal;
  code: Integer;
  message:  PAnsiChar;
end;

procedure rsvg_init; cdecl;
procedure rsvg_term; cdecl;

procedure rsvg_set_default_dpi(dpi: double); cdecl;
procedure rsvg_set_default_dpi_x_y(dpi_x, dpi_y: Double); cdecl;

procedure rsvg_handle_set_dpi	(handle: PRsvgHandle; dpi: double); cdecl;
procedure rsvg_handle_set_dpi_x_y	(handle: PRsvgHandle; dpi_x, dpi_y: Double); cdecl;

function rsvg_handle_new: PRsvgHandle; cdecl;
function rsvg_handle_write(handle: PRsvgHandle; const buf: PAnsiChar; count: Cardinal; error: PPGError): LongBool; cdecl;
function rsvg_handle_close(handle: PRsvgHandle; error: PPGError): LongBool; cdecl;
//GdkPixbuf   *rsvg_handle_get_pixbuf	(RsvgHandle * handle);
//GdkPixbuf   *rsvg_handle_get_pixbuf_sub (RsvgHandle * handle, const char *id);

function rsvg_handle_get_base_uri(handle: PRsvgHandle): PAnsiChar; cdecl;
procedure rsvg_handle_set_base_uri(handle: PRsvgHandle; const base_uri: PAnsiChar); cdecl;

procedure rsvg_handle_get_dimensions(handle: PRsvgHandle; dimension_data: PRsvgDimensionData); cdecl;

function rsvg_handle_get_dimensions_sub(handle: PRsvgHandle; dimension_data: PRsvgDimensionData; const id: PAnsiChar): LongBool; cdecl;
function rsvg_handle_get_position_sub (handle: PRsvgHandle; position_data: PRsvgPositionData; const id: PAnsiChar): LongBool; cdecl;

function rsvg_handle_has_sub(handle: PRsvgHandle; const id: PAnsiChar): LongBool; cdecl;

(* Accessibility API *)

function rsvg_handle_get_title(handle: PRsvgHandle): PAnsiChar; cdecl;
function rsvg_handle_get_desc	(handle: PRsvgHandle): PAnsiChar; cdecl;
function rsvg_handle_get_metadata	(handle: PRsvgHandle): PAnsiChar; cdecl;

function rsvg_handle_new_from_data(const data: PByte; data_len: Cardinal; error: PPGError): PRsvgHandle; cdecl;
function rsvg_handle_new_from_file(const file_name: PAnsiChar; error: PPGError): PRsvgHandle; cdecl;

//#ifndef RSVG_DISABLE_DEPRECATED

procedure rsvg_handle_free(handle: PRsvgHandle); cdecl;

(**
 * RsvgSizeFunc ():
 * @width: Pointer to where to set/store the width
 * @height: Pointer to where to set/store the height
 * @user_data: User data pointer
 *
 * Function to let a user of the library specify the SVG's dimensions
 * @width: the ouput width the SVG should be
 * @height: the output height the SVG should be
 * @user_data: user data
 *
 * Deprecated: Set up a cairo matrix and use rsvg_handle_render_cairo() instead.
 *)
type
  RsvgSizeFunc = procedure(width, height: PInteger; user_data: Pointer); cdecl;
  GDestroyNotify = procedure(data: Pointer); cdecl;
procedure rsvg_handle_set_size_callback (handle: PRsvgHandle; size_func: RsvgSizeFunc;
				    user_data: Pointer; user_data_destroy: GDestroyNotify); cdecl;

(* GdkPixbuf convenience API *)

//GdkPixbuf *rsvg_pixbuf_from_file		(const gchar * file_name, GError ** error);
//GdkPixbuf *rsvg_pixbuf_from_file_at_zoom	(const gchar * file_name,
//						 double x_zoom, double y_zoom, GError ** error);
//GdkPixbuf *rsvg_pixbuf_from_file_at_size	(const gchar * file_name, gint width, gint height,
//						 GError ** error);
//GdkPixbuf *rsvg_pixbuf_from_file_at_max_size	(const gchar * file_name,
//						 gint max_width, gint max_height, GError ** error);
//GdkPixbuf *rsvg_pixbuf_from_file_at_zoom_with_max (const gchar * file_name,
//						   double x_zoom,
//						   double y_zoom,
//						   gint max_width, gint max_height, GError ** error);

//#endif                          (* RSVG_DISABLE_DEPRECATED *)
//
//G_END_DECLS
//
//#endif                          (* RSVG_H *)


(*
   rsvg-cairo.h: SAX-based renderer for SVG files using cairo

   Copyright (C) 2005 Red Hat, Inc.

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this program; if not, write to the
   Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.

   Author: Carl Worth <cworth@cworth.org>
*)

//#ifndef RSVG_CAIRO_H
//#define RSVG_CAIRO_H
//
//#include <cairo.h>
//
//G_BEGIN_DECLS

function rsvg_handle_render_cairo(handle: PRsvgHandle; cr: PCairo): LongBool; cdecl;
function rsvg_handle_render_cairo_sub(handle: PRsvgHandle; cr: PCairo; const id: PAnsiChar): LongBool; cdecl;

//G_END_DECLS
//
//#endif

implementation
const
{$ifdef UNIX}
  librsvg = 'rsvg';
{$else}
  librsvg = 'librsvg-2-2.dll';
{$endif}

function rsvg_handle_get_type: Cardinal; cdecl; external librsvg;
function rsvg_error_quark: Cardinal; cdecl; external librsvg;
procedure rsvg_init; cdecl; external librsvg;
procedure rsvg_term; cdecl; external librsvg;
procedure rsvg_set_default_dpi(dpi: double); cdecl; external librsvg;
procedure rsvg_set_default_dpi_x_y(dpi_x, dpi_y: Double); cdecl; external librsvg;
procedure rsvg_handle_set_dpi	(handle: PRsvgHandle; dpi: double); cdecl; external librsvg;
procedure rsvg_handle_set_dpi_x_y	(handle: PRsvgHandle; dpi_x, dpi_y: Double); cdecl; external librsvg;
function rsvg_handle_new: PRsvgHandle; cdecl; external librsvg;
function rsvg_handle_write(handle: PRsvgHandle; const buf: PAnsiChar; count: Cardinal; error: PPGError): LongBool; cdecl; external librsvg;
function rsvg_handle_close(handle: PRsvgHandle; error: PPGError): LongBool; cdecl; external librsvg;
function rsvg_handle_get_base_uri(handle: PRsvgHandle): PAnsiChar; cdecl; external librsvg;
procedure rsvg_handle_set_base_uri(handle: PRsvgHandle; const base_uri: PAnsiChar); cdecl; external librsvg;
procedure rsvg_handle_get_dimensions(handle: PRsvgHandle; dimension_data: PRsvgDimensionData); cdecl; external librsvg;
function rsvg_handle_get_dimensions_sub(handle: PRsvgHandle; dimension_data: PRsvgDimensionData; const id: PAnsiChar): LongBool; cdecl; external librsvg;
function rsvg_handle_get_position_sub (handle: PRsvgHandle; position_data: PRsvgPositionData; const id: PAnsiChar): LongBool; cdecl; external librsvg;
function rsvg_handle_has_sub(handle: PRsvgHandle; const id: PAnsiChar): LongBool; cdecl; external librsvg;
function rsvg_handle_get_title(handle: PRsvgHandle): PAnsiChar; cdecl; external librsvg;
function rsvg_handle_get_desc	(handle: PRsvgHandle): PAnsiChar; cdecl; external librsvg;
function rsvg_handle_get_metadata	(handle: PRsvgHandle): PAnsiChar; cdecl; external librsvg;
function rsvg_handle_new_from_data(const data: PByte; data_len: Cardinal; error: PPGError): PRsvgHandle; cdecl; external librsvg;
function rsvg_handle_new_from_file(const file_name: PAnsiChar; error: PPGError): PRsvgHandle; cdecl; external librsvg;
procedure rsvg_handle_free(handle: PRsvgHandle); cdecl; external librsvg;
function rsvg_handle_render_cairo(handle: PRsvgHandle; cr: PCairo): LongBool; cdecl; external librsvg;
function rsvg_handle_render_cairo_sub(handle: PRsvgHandle; cr: PCairo; const id: PAnsiChar): LongBool; cdecl; external librsvg;
procedure rsvg_handle_set_size_callback (handle: PRsvgHandle; size_func: RsvgSizeFunc; user_data: Pointer; user_data_destroy: GDestroyNotify); cdecl; external librsvg;


end.