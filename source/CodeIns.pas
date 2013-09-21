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

unit CodeIns;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, Variants, Classes, Graphics, Controls, Forms,
  Spin, ExtCtrls, Buttons, StdCtrls, XPMenu;
{$ENDIF}
{$IFDEF LINUX}
  Variants, Classes, QGraphics, QControls, QForms,
  QComCtrls, QExtCtrls, QButtons, QStdCtrls;
{$ENDIF}

type
 PCodeIns = ^TCodeIns;
 TCodeIns = record
  Caption: string;
  Line: string;
  Desc: string;
  Sep: integer;
 end;

 TCodeInsList = class(TObject)
  private
   fFile: string;
   fList: TList;
   procedure SetItem(index: integer; Value: PCodeIns);
   function GetItem(index: integer): PCodeIns;
   function GetCount: integer;
  public
   constructor Create;
   destructor Destroy; override;

   procedure LoadCode;
   procedure SaveCode;
   function Indexof(const Value: String): integer;
   function AddItem(Value: PCodeIns): integer;
   procedure Delete(index: integer);
   procedure Clear;
   property Items[index: integer]: PCodeins read GetItem write SetItem; default;
   property Count: integer read GetCount;
 end;

  TfrmCodeEdit = class(TForm)
    Bevel1: TBevel;
    lblMenu: TLabel;
    lblSec: TLabel;
    edMenuText: TEdit;
    seSection: TSpinEdit;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    lblDesc: TLabel;
    edDesc: TEdit;
    XPMenu: TXPMenu;
    procedure FormCreate(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure edMenuTextChange(Sender: TObject);
  private
    fEdit: boolean;
    fEntry: PCodeIns;
    procedure SetEntry(Value: PCodeIns);
    procedure LoadText;
  public
    property Edit: boolean read fEdit write fEdit;
    property Entry: PCodeIns read fEntry write SetEntry;
  end;


implementation

uses
 SysUtils, IniFiles, devCFG, Utils, version, MultiLangSupport;

{$R *.dfm}
 { TCodeInsList }

constructor TCodeInsList.Create;
begin
  inherited Create;
  fList:= TList.Create;
end;

destructor TCodeInsList.Destroy;
var
 idx: integer;
begin
  for idx:= 0 to pred(fList.Count) do dispose(fList[idx]);
  fList.Free;
  inherited Destroy;
end;

function TCodeInsList.Indexof(const Value: string): integer;
begin
  for result:= 0 to pred(fList.Count) do
   if AnsiCompareText(PCodeIns(fList[result])^.Caption, Value) = 0 then exit;
  result:= -1;
end;

function TCodeInsList.AddItem(Value: PCodeIns): integer;
begin
  result:= fList.Add(Value);
end;

procedure TCodeInsList.Clear;
begin
  fList.Clear;
  fList.Pack;
  fList.Capacity:= fList.Count;
end;

procedure TCodeInsList.Delete(index: integer);
begin
  if (index <0) or (index > fList.Count -1) then exit;

  fList.Delete(index);
  fList.Pack;
  fList.Capacity:= fList.Count;
end;

function TCodeInsList.GetCount: integer;
begin
  result:= fList.Count;
end;

function TCodeInsList.GetItem(index: integer): PCodeIns;
begin
  if (index < 0) or (index > fList.Count -1) then
   result:= nil
  else
   result:= PCodeIns(fList[index]);
end;

procedure TCodeInsList.SetItem(index: integer; Value: PCodeIns);
begin
  fList[index]:= Value;
end;

procedure TCodeInsList.LoadCode;
var
 Item: PCodeIns;
 tmp: TStringList;
 idx: integer;
begin
  if not FileExists(fFile) then
    fFile:=devDirs.Config + DEV_CODEINS_FILE;

  if FileExists(fFile) then
  with TINIFile.Create(fFile) do
   try
    tmp:= TStringList.Create;
    Clear;
    try
     ReadSections(tmp);
     if tmp.Count = 0 then exit;
     for idx:= 0 to pred(tmp.Count) do
      begin
        new(Item);
        Item^.Caption:= StringReplace(tmp[idx], '_', ' ', [rfReplaceAll]);
        Item^.Desc:= ReadString(tmp[idx], 'Desc', '');
        Item^.Line:= StrtoCodeIns(ReadString(tmp[idx], 'Line', ''));
        Item^.Sep:= ReadInteger(tmp[idx], 'Sep', 0);
        AddItem(Item);
      end;
    finally
     tmp.free;
    end;
   finally
    free;
   end;
end;

procedure TCodeInsList.SaveCode;
var
 idx: integer;
 section: string;
 CI: TCodeIns;
begin
  fList.Pack;
  fList.Capacity:= fList.Count;
  DeleteFile(fFile);
  if fList.Count= 0 then exit;
  with TINIFile.Create(fFile) do
   try
    for idx:= 0 to pred(fList.Count) do
     begin
       CI:= PCodeIns(fList[idx])^;
       section:= StringReplace(CI.Caption, ' ', '_', [rfReplaceAll]);
       EraseSection(section);  // may be redundent
       WriteString(section, 'Desc', CI.Desc);
       WriteString(section, 'Line', CodeInstoStr(CI.Line));
       WriteInteger(section, 'Sep', CI.Sep);
     end;
   finally
    free;
   end;
end;


 { TfrmCodeEdit }

procedure TfrmCodeEdit.SetEntry(Value: PCodeIns);
begin
  edMenuText.Text:= Value^.Caption;
  edDesc.Text:= Value^.Desc;
  seSection.Value:= Value^.Sep;
end;

procedure TfrmCodeEdit.FormCreate(Sender: TObject);
begin
  LoadText;
end;

procedure TfrmCodeEdit.LoadText;
begin
  if devData.XPTheme then
    XPMenu.Active := true
  else
    XPMenu.Active := false;
  if Edit then
   Caption:= Lang[ID_CIE_EDCAPTION]
  else
   Caption:= Lang[ID_CIE_ADDCAPTION];

  lblMenu.Caption:= Lang[ID_CIE_MENU];
  lblDesc.Caption:= Lang[ID_CIE_DESC];
  lblSec.Caption:=  Lang[ID_CIE_SEC];
  btnOk.Caption:= Lang[ID_BTN_OK];
  btnCancel.Caption:= Lang[ID_BTN_CANCEL];
end;

procedure TfrmCodeEdit.btnOkClick(Sender: TObject);
begin
  if edMenuText.Text = '' then begin
    ModalResult := mrNone;
  end;
end;

procedure TfrmCodeEdit.edMenuTextChange(Sender: TObject);
begin
  btnOK.Enabled := edMenuText.Text <> '';
end;

end.
