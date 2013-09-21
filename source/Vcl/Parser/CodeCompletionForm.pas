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
  {** Modified by Peter **}
  TCompletionEvent = procedure(Sender: TObject; const AStatement: TStatement; const AIndex: Integer) of object;
  
  TCodeComplForm = class(TForm)
    lbCompletion: TListBox;
    procedure FormShow(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure lbCompletionDblClick(Sender: TObject);
    procedure lbCompletionDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure lbCompletionKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    fOwner: TCodeCompletion;
    fColor: TColor;
    FOnCompletion: TCompletionEvent; {** Modified by Peter **}
  protected
    procedure DoCompletion; virtual;
  public
    { Public declarations }
    fCompletionStatementList: TList;
    fParser: TCppParser;
    constructor Create(AOwner: TComponent); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure SetColor(Value: TColor);
  published
    property OnCompletion: TCompletionEvent read FOnCompletion write FOnCompletion; {** Modified by Peter **}
  end;

var
  CodeComplForm: TCodeComplForm;

implementation

{$R *.dfm}

procedure TCodeComplForm.FormShow(Sender: TObject);
begin       
  Width := fOwner.Width;
  Height := fOwner.Height;
  lbCompletion.Font.Name := 'Tahoma';  {** Modified by Peter **}
  lbCompletion.Font.Size := 8; {** Modified by Peter **}
  lbCompletion.DoubleBuffered := True;
  lbCompletion.SetFocus;
  if lbCompletion.Items.Count > 0 then
    lbCompletion.ItemIndex := 0;
end;

procedure TCodeComplForm.FormDeactivate(Sender: TObject);
begin
  Hide;
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

{** Modified by Peter **}
procedure TCodeComplForm.DoCompletion;
begin
  Hide;
  Application.ProcessMessages;
   
  with lbCompletion do
  begin
    if (FCompletionStatementList.Count > ItemIndex) and (ItemIndex > -1) then
    begin
      if Assigned(FOnCompletion) then
        FOnCompletion(FOwner, PStatement(FCompletionStatementList[ItemIndex])^, ItemIndex); 
    end;
  end;
end;

procedure TCodeComplForm.SetColor(Value: TColor);
begin
  if Value <> fColor then begin
    fColor := Value;
    lbCompletion.Color := fColor;
    Color := fColor;
  end;
end;

procedure TCodeComplForm.lbCompletionDblClick(Sender: TObject);
var
  Key: Char;
begin
  if Assigned(OnKeyPress) then 
  begin
    Key := #13;
    OnKeyPress(Self, Key);
  end;
  {** Modified by Peter **}
  DoCompletion;
  //Hide;
end;

procedure TCodeComplForm.lbCompletionDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  Offset: integer;
  C, BC: TColor;
begin
  if fCompletionStatementList = nil then
    Exit;
  if not lbCompletion.Visible then
    Exit;
  if fCompletionStatementList.Count <= Index {+ 1  ??? }then
    Exit;
  if fCompletionStatementList[Index] = nil then
    Exit;

    
  with lbCompletion do begin
    if fCompletionStatementList.Count > 0 then begin
      if odSelected in State then begin
        Canvas.Brush.Color := clHighlight;
        Canvas.FillRect(Rect);
        Canvas.Font.Color := clHighlightText;
      end
      else begin
        Canvas.Brush.Color := fColor;
        Canvas.FillRect(Rect);
        case PStatement(fCompletionStatementList[Index])^._Kind of
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

      Offset := Rect.Bottom - Rect.Top;
      C := Canvas.Font.Color;
      BC := Canvas.Brush.Color;
      Canvas.Font.Color := clWhite;
      Canvas.Pen.Style := psClear;
      Canvas.Rectangle(Rect.Left, Rect.Top, Rect.Left + Offset, Rect.Bottom);
      case PStatement(fCompletionStatementList[Index])^._ClassScope of
        scsPrivate: Canvas.Brush.Color := clRed;
        scsProtected: Canvas.Brush.Color := clMaroon;
        scsPublic, scsPublished: Canvas.Brush.Color := clGreen;
      end;
      Canvas.Rectangle(Rect.Left + 4, Rect.Top + 4, Rect.Left + Offset - 4, Rect.Bottom - 4);
      Inc(Offset, 8);

      Canvas.Brush.Color := BC;
      Canvas.Font.Color := C;
      Canvas.TextOut(Rect.Left + Offset, Rect.Top, fParser.StatementKindStr(PStatement(fCompletionStatementList[Index])^._Kind));
      if not (odSelected in State) then
        Canvas.Font.Color := clWindowText;
      Canvas.Font.Style := [];

      Canvas.TextOut(64 + Rect.Left + Offset, Rect.Top, PStatement(fCompletionStatementList[Index])^._Type);
      Offset := Offset + Canvas.TextWidth(PStatement(fCompletionStatementList[Index])^._Type + ' ');
      Canvas.Font.Style := [fsBold];
      Canvas.TextOut(64 + Rect.Left + Offset, Rect.Top, PStatement(fCompletionStatementList[Index])^._ScopelessCmd);
      Offset := Offset + Canvas.TextWidth(PStatement(fCompletionStatementList[Index])^._ScopelessCmd + ' ');
      Canvas.Font.Style := [];
      Canvas.TextOut(64 + Rect.Left + Offset, Rect.Top, PStatement(fCompletionStatementList[Index])^._Args);
    end;
  end;
end;

procedure TCodeComplForm.lbCompletionKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  {** Modified by Peter **}
  case Key of
{$IFDEF WIN32}
    VK_TAB,
    VK_RETURN:     
{$ENDIF}
{$IFDEF LINUX}
    XK_TAB,
    XK_RETURN:     
{$ENDIF}
      begin
        DoCompletion;  
      end;
  end;
end;



end.

