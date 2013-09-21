{----------------------------------------------------------------------------------

  The contents of this file are subject to the GNU General Public License
  Version 1.1 or later (the "License"); you may not use this file except in
  compliance with the License. You may obtain a copy of the License at
  http://www.gnu.org/copyleft/gpl.html

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
  the specific language governing rights and limitations under the License.

  The Initial Developer of the Original Code is Peter Schraut.
  http://www.console-dev.de

  
  Portions created by Peter Schraut are Copyright 
  (C) 2003, 2003 by Peter Schraut (http://www.console-dev.de) 
  All Rights Reserved.
  
----------------------------------------------------------------------------------}

//
//  TCustomXPToolTip features:  
//
//    Alphablending under windows 2000 and xp
//    Drops a shadow under windows xp
//    draws a black border around the hint, like the system hints from win2k and xp
//

unit XPToolTip;

interface

uses
{$IFDEF WIN32}
  SysUtils, Dialogs, Classes, Windows, Messages, Graphics, Controls, Menus, Forms, StdCtrls;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, QDialogs, Classes, QGraphics, QControls, QMenus, QForms, QStdCtrls, Types;
{$ENDIF}

  
type
  TCustomToolTip = class(THintWindow)
  private
    FActivated: Boolean;  
  protected
    property Activated: Boolean read FActivated;    
  public
    constructor Create(AOwner: TComponent); override;
{$IFDEF WIN32}
    procedure ActivateHint(Rect: TRect; const AHint: string); override;
{$ENDIF}
{$IFDEF LINUX}
    procedure ActivateHint(Rect: TRect; const AHint: WideString); override;
{$ENDIF}
    procedure ReleaseHandle; virtual;
  end;
  
  
  TToolTip = class(TCustomToolTip)
  public
    property Activated;
  end;
  
  
  TCustomXPToolTip = class(TCustomToolTip)
  private
    FAlphaBlend: Boolean;
    FAlphaBlendValue: Byte;
    FDropShadow: Boolean;
    procedure SetAlphaBlend(Value: Boolean);
    procedure SetAlphaBlendValue(Value: Byte);
    procedure SetDropShadow(Value: Boolean);
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMNCPaint(var msg: TMessage); message WM_NCPAINT;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    property AlphaBlend: Boolean read FAlphaBlend write SetAlphaBlend default False;
    property AlphaBlendValue: Byte read FAlphaBlendValue write SetAlphaBlendValue default 255;
    property DropShadow: Boolean read FDropShadow write SetDropShadow default True;
  public
    constructor Create(AOwner: TComponent); override;
{$IFDEF WIN32}
    procedure ActivateHint(Rect: TRect; const AHint: string); override;
{$ENDIF}
{$IFDEF LINUX}
    procedure ActivateHint(Rect: TRect; const AHint: WideString); override;
{$ENDIF}
  end;


  TXPToolTip = class(TCustomXPToolTip)
  public
    property Activated;
  published
    property AlphaBlend;
    property AlphaBlendValue;
    property DropShadow;
  end;
  
const
  clXPToolTipBk: TColor = $E1FFFF;
  
implementation
var
  SetLayeredWindowAttributesProc : function (hWnd : HWND; crKey: TColor; bAlpha: byte; dwFlags: DWORD): BOOL; stdcall;

//----------------- local helper functions -----------------------------------------------------------------------------

  function IsWin2kOrLater: Boolean;
  // returns true when the operating system is windows 2000 or newer  
  begin
{$IFDEF WIN32}
    Result := (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 5);
{$ENDIF}
{$IFDEF LINUX}
    Result := False;
{$ENDIF}
  end;
  
  function IsWinXP: Boolean;
  // returns true when the operating system is windows XP or newer  
  begin
{$IFDEF WIN32}
    Result := (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 5) and (Win32MinorVersion >= 1);
{$ENDIF}
{$IFDEF LINUX}
    Result := False;
{$ENDIF}
  end;

//----------------- TCustomToolTip -------------------------------------------------------------------------------------

constructor TCustomToolTip.Create(AOwner: TComponent); 
begin
  inherited;

  FActivated := False;
  Color := clXPToolTipBk;
end;

//----------------------------------------------------------------------------------------------------------------------

{$IFDEF WIN32}
procedure TCustomToolTip.ActivateHint(Rect: TRect; const AHint: string);
{$ENDIF}
{$IFDEF LINUX}
procedure TCustomToolTip.ActivateHint(Rect: TRect; const AHint: WideString);
{$ENDIF}
begin
  inherited;
  FActivated := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomToolTip.ReleaseHandle;
begin
  FActivated := False;
  DestroyHandle;
end;

//----------------- TCustomXPToolTip -----------------------------------------------------------------------------------

constructor TCustomXPToolTip.Create(AOwner: TComponent); 
begin
  inherited;

  Color := clXPToolTipBk;
  FAlphaBlend := False;
  FAlphaBlendValue := 255;
  FDropShadow := True;
  
  SetAlphaBlend(FAlphaBlend);
  SetAlphaBlendValue(FAlphaBlendValue);
end;

//----------------------------------------------------------------------------------------------------------------------

{$IFDEF WIN32}
procedure TCustomXPToolTip.ActivateHint(Rect: TRect; const AHint: string);
{$ENDIF}
{$IFDEF LINUX}
procedure TCustomXPToolTip.ActivateHint(Rect: TRect; const AHint: WideString);
{$ENDIF}
const
  CS_DROPSHADOW = $00020000;
begin     
  if IsWinXP then
  begin
    if FDropShadow then SetClassLong(Handle,GCL_STYLE, GetClassLong(Handle, GCL_STYLE) or CS_DROPSHADOW) 
    else SetClassLong(Handle,GCL_STYLE, GetClassLong(Handle, GCL_STYLE) and not CS_DROPSHADOW);
  end;
  
  //SetAlphaBlend(FAlphaBlend); 
  SetAlphaBlendValue(FAlphaBlendValue);      
  
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomXPToolTip.CreateParams(var Params: TCreateParams);
begin
 inherited;
 Params.ExStyle := Params.ExStyle or WS_EX_LAYERED;
end;

//----------------------------------------------------------------------------------------------------------------------
 
procedure TCustomXPToolTip.SetAlphaBlend(Value: Boolean);
const
 WS_EX_LAYERED = $80000;
begin
  FAlphaBlend := Value;
  
  if IsWin2kOrLater then
  begin
    if FAlphaBlend then
    begin
      SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) or WS_EX_LAYERED);
      SetAlphaBlendValue(FAlphaBlendValue);
    end
    else
    begin
      if (GetWindowLong(Handle, GWL_EXSTYLE) and WS_EX_LAYERED) = WS_EX_LAYERED then
        SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) and not WS_EX_LAYERED) ;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomXPToolTip.SetAlphaBlendValue(Value: Byte);
const
 LWA_COLORKEY  = $1;
 LWA_ALPHA     = $2;
begin
  FAlphaBlendValue := Value;
  
  if IsWin2kOrLater and Assigned(SetLayeredWindowAttributesProc) then
  begin
    if FAlphaBlend then SetLayeredWindowAttributesProc(Handle, 0, Value, LWA_ALPHA)
    else SetLayeredWindowAttributesProc(Handle, 0, Value, 0);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomXPToolTip.SetDropShadow(Value: Boolean);
begin
  FDropShadow := Value;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomXPToolTip.WMNCHitTest(var Message: TWMNCHitTest);
begin
  Message.Result := HTTRANSPARENT;
  //Message.Result := HTCLIENT;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomXPToolTip.WMNCPaint(var msg: TMessage);
var
  R: TRect;
  DC: HDC;
begin
  DC := GetWindowDC(Handle);
  try
    R := Rect(0, 0, Width, Height);
    DrawEdge(DC, R, EDGE_ETCHED, BF_RECT or BF_MONO);
  finally
    ReleaseDC(Handle, DC);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

initialization
  SetLayeredWindowAttributesProc := nil;
  if IsWin2kOrLater then 
    SetLayeredWindowAttributesProc := GetProcAddress(GetModulehandle(user32), 'SetLayeredWindowAttributes');
  
finalization
  SetLayeredWindowAttributesProc := nil;
  
end.
