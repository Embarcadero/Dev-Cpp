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

unit CodeCompletionForm;

interface

uses
{$IFDEF WIN32}
  Windows, SysUtils, Variants, Classes, Graphics, Forms, StdCtrls, Controls,
  CodeCompletion, CppParser, Grids, Dialogs;
{$ENDIF}
{$IFDEF LINUX}
  Xlib, SysUtils, Variants, Classes, QGraphics, QForms, QStdCtrls, QControls,
  CodeCompletion, CppParser, QGrids, QDialogs, Types;
{$ENDIF}

type
  TCodeComplForm = class(TForm)
    lbCompletion: TListBox;
    procedure FormShow(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure lbCompletionDblClick(Sender: TObject);
    procedure lbCompletionDrawItem(Control: TWinControl; Index: Integer;Rect: TRect; State: TOwnerDrawState);
    procedure lbCompletionKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
    fOwner: TCodeCompletion;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateParams(var Params: TCreateParams); override;
  end;

var
  CodeComplForm: TCodeComplForm;

implementation

{$R *.dfm}

procedure TCodeComplForm.FormShow(Sender: TObject);
begin
	Width := fOwner.Width;
	Height := fOwner.Height;
	lbCompletion.DoubleBuffered := true; // reduces flicker
end;

procedure TCodeComplForm.FormDeactivate(Sender: TObject);
begin
	fOwner.Hide;
end;

procedure TCodeComplForm.CreateParams(var Params: TCreateParams);
begin
	inherited CreateParams(Params);

	Params.Style := Params.Style or WS_SIZEBOX;
end;

constructor TCodeComplForm.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);

	fOwner := TCodeCompletion(AOwner);
end;

procedure TCodeComplForm.lbCompletionDblClick(Sender: TObject);
var
	Key: Char;
begin
	// Send command to TEditor
	if Assigned(fOwner.OnKeyPress) then begin
		Key := Char(VK_RETURN);
		fOwner.OnKeyPress(self,Key);
	end;
end;

procedure TCodeComplForm.lbCompletionDrawItem(Control: TWinControl;Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
	Offset: integer;
	statement : PStatement;
begin
	Offset := 4;

	with lbCompletion do begin

		statement := PStatement(Items.Objects[Index]);

		if not Assigned(statement) then Exit; // TODO: why is this needed?

		// Draw statement kind string, like 'Preprocessor'
		if odSelected in State then begin
			Canvas.Brush.Color := clHighlight;
			Canvas.FillRect(Rect);
			Canvas.Font.Color := clHighlightText;
		end else begin
			Canvas.Brush.Color := fOwner.Color;
			Canvas.FillRect(Rect);
			case statement^._Kind of
				skFunction: Canvas.Font.Color := clGreen;
				skClass: Canvas.Font.Color := clMaroon;
				skVariable: Canvas.Font.Color := clBlue;
				skTypedef: Canvas.Font.Color := clOlive;
				skPreprocessor: Canvas.Font.Color := clPurple;
				skEnum: Canvas.Font.Color := clNavy;
			else
				Canvas.Font.Color := clGray;
			end;
		end;
		Canvas.TextOut(Offset, Rect.Top, fOwner.Parser.StatementKindStr(statement^._Kind));
		Offset := Offset + Canvas.TextWidth('Preprocessor '); // worst case width + spacing
		if not (odSelected in State) then
			Canvas.Font.Color := clWindowText;

		// Draw data type string, like 'int', hide for defines/others that don't have this property
		if Length(statement^._Type) > 0 then begin
			Canvas.TextOut(Offset, Rect.Top, statement^._Type);
			Offset := Offset + Canvas.TextWidth(statement^._Type + ' ');
		end;

		// draw statement name, like 'foo'
		Canvas.Font.Style := [fsBold];
		Canvas.TextOut(Offset, Rect.Top, statement^._ScopelessCmd);
		Offset := Offset + Canvas.TextWidth(statement^._ScopelessCmd + ' ');

		// if applicable, draw arguments
		if statement^._Kind in [skFunction,skConstructor,skDestructor] then begin
			Canvas.Font.Style := [];
			Canvas.TextOut(Offset, Rect.Top, statement^._Args);
		end;
	end;
end;

procedure TCodeComplForm.lbCompletionKeyPress(Sender: TObject;var Key: Char);
begin
	case Key of
		Char(VK_ESCAPE), '.', '>': begin
			fOwner.Hide;
		end;
	end;

	if Assigned(fOwner.OnKeyPress) then
		fOwner.OnKeyPress(self, Key);
end;

end.

