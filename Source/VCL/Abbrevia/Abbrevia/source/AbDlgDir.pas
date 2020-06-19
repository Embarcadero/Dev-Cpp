(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower Abbrevia
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* ABBREVIA: AbDlgDir.pas                                *}
{*********************************************************}
{* ABBREVIA: Dialog - Directory                          *}
{*   Use AbQDgDir.pas for CLX                            *}
{*********************************************************}

unit AbDlgDir;

{$I AbDefine.inc}

interface

uses
  Windows, Messages, ShlObj, ActiveX, SysUtils, Classes, Buttons, ExtCtrls, Graphics,
  Forms, Controls, StdCtrls,
  {$WARN UNIT_PLATFORM OFF}
  FileCtrl,
  {$WARN UNIT_PLATFORM ON}
  AbResString;

type
  TDirDlg = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Bevel1: TBevel;
    DriveComboBox1: TDriveComboBox;
    DirectoryListBox1: TDirectoryListBox;
    Panel1: TPanel;
    procedure DirectoryListBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    SelectedFolder: string;
  end;

type
  TAbDirDlg = class(TComponent)
  protected {private}
    FAdditionalText      : string;
    FCaption             : string;
    FHandle              : Integer;
    FIDList              : PItemIDList;
    FSelectedFolder      : string;

    procedure SetSelectedFolder(const Value : string);
    procedure FreeIDList;

  public {properties}
    property AdditionalText : string
      read FAdditionalText
      write FAdditionalText;
    property Caption : string
      read FCaption
      write FCaption;
    property Handle : Integer
      read FHandle;
    property IDList : PItemIDList
      read FIDList;
    property SelectedFolder : string
      read FSelectedFolder
      write SetSelectedFolder;

  public {methods}
    constructor Create(AOwner : TComponent);
      override;
    destructor Destroy;
      override;
    function Execute : Boolean;
  end;

var
  DirDlg: TDirDlg;

implementation

{$R *.dfm}

{== TAbDirDlg ========================================================}
function AbDirDlgCallbackProc(hWnd : HWND; Msg : UINT; lParam : LPARAM;
                              Data : LPARAM): Integer; stdcall;
var
  X, Y : Integer;
  R    : TRect;
  Buf    : array[0..MAX_PATH-1] of Char;
begin
  Result := 0;
  with TAbDirDlg(Data) do begin
    case Msg of
      BFFM_INITIALIZED :
        begin
          FHandle := hWnd;
          if (FCaption <> '') then
            SendMessage(hWnd, WM_SETTEXT, 0, Integer(PChar(FCaption)));
          SendMessage(hWnd, BFFM_SETSELECTION, 1, Integer(PChar(SelectedFolder)));
          GetWindowRect(hWnd, R);
          X := (Screen.Width div 2) - ((R.Right - R.Left) div 2);
          Y := (Screen.Height div 2) - ((R.Bottom - R.Top) div 2);
          SetWindowPos(hWnd, 0, X, Y, 0, 0, SWP_NOSIZE or SWP_NOZORDER);
        end;
      BFFM_SELCHANGED :
        if (FHandle <> 0) then begin
          FIDList := PItemIDList(lParam);
          SHGetPathFromIDList(IDList, Buf);
          SelectedFolder := Buf;
        end;
    end;
  end;
end;
{ -------------------------------------------------------------------------- }
constructor TAbDirDlg.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
end;
{ -------------------------------------------------------------------------- }
destructor TAbDirDlg.Destroy;
begin
  if FIDList <> nil then
    FreeIDList;
  inherited Destroy;
end;
{ -------------------------------------------------------------------------- }
function TAbDirDlg.Execute : Boolean;
var
  Info   : TBrowseInfo;
  Buf    : array[0..MAX_PATH-1] of Char;
begin
  if (FIDList <> nil) then
    FreeIDList;

  if (Owner is TWinControl) then
    Info.hwndOwner := (Owner as TWinControl).Handle
  else if Owner is TApplication then
    Info.hwndOwner := (Owner as TApplication).Handle
  else
    Info.hwndOwner := 0;
  Info.pidlRoot := nil;
  Info.pszDisplayName := Buf;
  Info.lpszTitle := PChar(FAdditionalText);
  Info.ulFlags := BIF_RETURNONLYFSDIRS;
  Info.lpfn := AbDirDlgCallbackProc;
  Info.lParam := Integer(Self);
  Info.iImage := 0;

  FIDList := SHBrowseForFolder(Info);
  FHandle := 0;
  Result := (FIDList <> nil);
end;
{ -------------------------------------------------------------------------- }
procedure TAbDirDlg.FreeIDList;
var
  Malloc : IMalloc;
begin
  if coGetMalloc(MEMCTX_TASK, Malloc) = NOERROR then begin
    Malloc.Free(FIDList);
    FIDList := nil;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbDirDlg.SetSelectedFolder(const Value : string);
begin
  FSelectedFolder := Value;
  if FSelectedFolder <> '' then
    if FSelectedFolder[Length(FSelectedFolder)] = '\' then
      Delete(FSelectedFolder, Length(FSelectedFolder), 1);
  if (Length(FSelectedFolder) = 2) then
    FSelectedFolder := FSelectedFolder + '\';
end;

{== TDirDlg ========================================================}
{ TDirDlg }
procedure TDirDlg.FormCreate(Sender: TObject);
begin
  DirectoryListBox1Change(nil);
  OKBtn.Caption := AbOKS;
  CancelBtn.Caption := AbCancelS;
  Caption := AbSelectDirectoryS;
end;
{ -------------------------------------------------------------------------- }
procedure TDirDlg.DirectoryListBox1Change(Sender: TObject);
begin
  SelectedFolder := DirectoryListBox1.Directory;
  Panel1.Caption := SelectedFolder;
end;

end.
