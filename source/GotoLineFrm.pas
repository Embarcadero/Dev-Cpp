{
    This file is part of Dev-C++
    Copyright (c) 2004 Bloodshed Software

    Dev-C++ is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Dev-C++ is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Dev-C++; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

//
//  History:
//
//  26 March 2004, Peter Schraut
//    Pretty much a complete rewrite of this unit :P
//

unit GotoLineFrm;

interface

uses
{$IFDEF WIN32}
  Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, 
  Buttons, ExtCtrls, Spin, SynEdit;
{$ENDIF}
{$IFDEF LINUX}
  Classes, QGraphics, QForms, QControls, QStdCtrls, 
  QButtons, QExtCtrls, QComCtrls, QSynEdit;
{$ENDIF}

type
  TGotoLineForm = class(TForm)
    GotoLabel: TLabel;
    Line: TSpinEdit;
    BtnOK: TButton;
    BtnCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  public
    procedure LoadText;
  end;

implementation

uses
{$IFDEF WIN32}
  MultiLangSupport, devcfg;
{$ENDIF}
{$IFDEF LINUX}
  Xlib, MultiLangSupport, devcfg;
{$ENDIF}

{$R *.dfm}

procedure TGotoLineForm.FormCreate(Sender: TObject);
begin
	LoadText;
end;

procedure TGotoLineForm.LoadText;
begin
	// Set interface font
	Font.Name := devData.InterfaceFont;
	Font.Size := devData.InterfaceFontSize;

  Caption := Lang[ID_GOTO_CAPTION];
  GotoLabel.Caption := Lang[ID_GOTO_TEXT];
  BtnOk.Caption := Lang[ID_BTN_OK];
  BtnCancel.Caption := Lang[ID_BTN_CANCEL];
end;

procedure TGotoLineForm.FormKeyUp(Sender: TObject; var Key: Word;Shift: TShiftState);
begin
{$IFDEF WIN32}
  if Key = VK_ESCAPE then Close;
{$ENDIF}
{$IFDEF LINUX}
  if Key = XK_ESCAPE then Close;
{$ENDIF}
end;

procedure TGotoLineForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	Action := caFree;
end;

procedure TGotoLineForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
	// Alternative method to listen to VK_RETURN key presses of the Line component
	// This is needed to be able to mute the beep that plays when the enter key is hit
	// VK_RETURN isn't trapped by KeyPress of spinedits, and can't be deleted by KeyDown of it
	if Line.Focused then begin
		if Key = Chr(VK_RETURN) then begin
			Key := #0;
			BtnOK.Click;
		end;
	end;
end;

end.

