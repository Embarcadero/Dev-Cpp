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

unit AddToDoFm;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, SynEditTextBuffer, SynEditTypes, XPMenu;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Variants, Classes, QGraphics, QControls, QForms,
  QDialogs, QStdCtrls, QComCtrls, QSynEditTextBuffer, QSynEditTypes;
{$ENDIF}

type
  TAddToDoForm = class(TForm)
    Label1: TLabel;
    memDescr: TMemo;
    Label2: TLabel;
    Label3: TLabel;
    spnPri: TSpinEdit;
    btnOK: TButton;
    btnCancel: TButton;
    txtUser: TEdit;
    XPMenu: TXPMenu;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure txtUserKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
    procedure LoadText;
  public
    { Public declarations }
  end;

var
  AddToDoForm: TAddToDoForm;

implementation

uses 
  main, editor, MultiLangSupport, devcfg;

{$R *.dfm}

procedure TAddToDoForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TAddToDoForm.btnOKClick(Sender: TObject);
var
  e: TEditor;
  I: integer;
  st: TBufferCoord;
  Line: integer;
  LineText: string;
  Hdr: string;
  Prepend: string;
begin
  e := MainForm.GetEditor;
  if not Assigned(e) then begin
    Close;
    Exit;
  end;

  Line := e.Text.CaretY - 1;
  LineText := e.Text.Lines[Line];
  st.Line := Line + 1;
  st.Char := 1;

  I := 1;
  while (I <= Length(LineText)) and (LineText[I] in [#9, ' ']) do
    Inc(I);
  Prepend := Copy(LineText, 1, I - 1);

  Hdr := '/* TODO (';
  if txtUser.Text <> '' then
    Hdr := Hdr + txtUser.Text;
  Hdr := Hdr + '#' + IntToStr(spnPri.Value) + '#): ';

  if memDescr.Lines.Count = 1 then
    e.Text.Lines.Insert(Line, Prepend + Hdr + memDescr.Text + ' */')
  else begin
    e.Text.Lines.Insert(Line, Prepend + Hdr + memDescr.Lines[0]);
    Prepend := Prepend + StringOfChar(#32, Length(Hdr));
    for I := 1 to memDescr.Lines.Count - 1 do begin
      if I = memDescr.Lines.Count - 1 then
        e.Text.Lines.Insert(Line + I, Prepend + memDescr.Lines[I] + ' */')
      else
        e.Text.Lines.Insert(Line + I, Prepend + memDescr.Lines[I]);
    end;
  end;
  e.Text.UndoList.AddChange(crInsert, st, BufferCoord(st.Char, st.Line + memDescr.Lines.Count), '', smNormal);
  e.Modified := True;
  Close;
end;

procedure TAddToDoForm.FormShow(Sender: TObject);
begin
  LoadText;
  memDescr.Clear;
  spnPri.Value := 0;
  txtUser.Clear;
end;

procedure TAddToDoForm.txtUserKeyPress(Sender: TObject; var Key: Char);
begin
  if not (Key in ['a'..'z', 'A'..'Z', '0'..'9', '_', #8, #13, #27]) then begin
    Key := #0;
    Exit;
  end;
end;

procedure TAddToDoForm.LoadText;
begin
  if devData.XPTheme then
    XPMenu.Active := true
  else
    XPMenu.Active := false;
  Caption := Lang[ID_ADDTODO_MENUITEM];
  Label1.Caption := Lang[ID_ADDTODO_DESCRIPTION] + ':';
  Label2.Caption := Lang[ID_ADDTODO_PRIORITY] + ':';
  Label3.Caption := Lang[ID_ADDTODO_USER] + ':';
  btnOk.Caption := Lang[ID_BTN_OK];
  btnCancel.Caption := Lang[ID_BTN_CANCEL];
end;

end.

