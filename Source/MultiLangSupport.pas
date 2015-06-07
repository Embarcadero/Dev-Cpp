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

unit MultiLangSupport;

interface

uses
  Windows, Dialogs, SysUtils, Classes;

{$I LangIDs.inc}

type
  TdevMultiLangSupport = class(TObject)
  private
    fLangList: TStringList;
    fLangListLoaded: Boolean;
    fLangFile: AnsiString;
    fCurLang: AnsiString;
    fStrings: TStringList;
    fDefaultLangStrings: TStringList;
    function GetString(ID: integer): AnsiString;
    function GetLangName: AnsiString;
    constructor Create;
  public
    destructor Destroy; override;
    procedure SelectLanguage;
    procedure CheckLanguageFiles;
    procedure Open(const FileName: AnsiString);
    procedure SetLang(const Lang: AnsiString);
    function GetLangList: TStringList;
    function FileFromDescription(const Desc: AnsiString): AnsiString;
    property Strings[index: integer]: AnsiString read GetString; default;
    property CurrentLanguage: AnsiString read GetLangName;
    property Langs: TStringList read GetLangList;
  end;

function Lang: TdevMultiLangSupport;

implementation

uses
  LangFrm, Forms, Utils, Version, Controls, devCFG;

var
  fLangSingleton: TdevMultiLangSupport = nil;

function Lang: TdevMultiLangSupport;
begin
  // TODO: for some reason, devDirs is not always assigned here
  // before the first TdevMultiLangSupport call is made
  if Assigned(devDirs) and not Assigned(fLangSingleton) and not Application.Terminated then
    fLangSingleton := TdevMultiLangSupport.Create;
  Result := fLangSingleton;
end;

constructor TdevMultiLangSupport.Create;
var
  DefaultLangFile: AnsiString;
begin
  inherited;
  fLangList := TStringList.Create;
  fLangListLoaded := False; // only load when needed
  fStrings := TStringList.Create;
  fDefaultLangStrings := TStringList.Create;

  // Load default english file from languages directory
  DefaultLangFile := ValidateFile('English.lng', devDirs.Lang);
  if DefaultLangFile = '' then begin
    MessageDlg('Could not open language file English.lng', mtError, [mbOK], 0);
  end else begin
    fDefaultLangStrings.LoadFromFile(DefaultLangFile);
  end;
end;

destructor TdevMultiLangSupport.Destroy;
begin
  FreeAndNil(fLangList);
  FreeAndNil(fStrings);
  FreeAndNil(fDefaultLangStrings);
  fLangSingleton := nil;
  inherited;
end;

procedure TdevMultiLangSupport.Open(const Filename: AnsiString);
var
  LangFile: AnsiString;
begin
  // Load file from languages directory
  LangFile := ValidateFile(Filename, devDirs.Lang);
  if LangFile = '' then begin
    MessageDlg('Could not open language file ' + Filename, mtError, [mbOK], 0);
    Exit;
  end;

  // Load file into fStrings
  fStrings.LoadFromFile(LangFile);
  fLangFile := LangFile;

  // Get languange
  fCurLang := fStrings.Values['Lang'];
  if fCurLang = '' then
    fCurLang := ChangeFileExt(ExtractFileName(LangFile), '');
  devData.Language := ExtractFileName(LangFile);
end;

procedure TdevMultiLangSupport.CheckLanguageFiles;
var
  I: integer;
  s: AnsiString;
  sl: TStringList;
begin
  try
    fLangList.Clear;
    if devDirs.Lang = '' then
      Exit;

    // Load list of all files
    FilesFromWildcard(devDirs.Lang, '*.lng', fLangList, False, False, True);
    if fLangList.Count > 0 then begin
      fLangList.Sort;
      sl := TStringList.Create;
      try
        for I := 0 to fLangList.Count - 1 do begin
          sl.LoadFromFile(fLangList[I]);
          s := sl.Values['Lang'];
          if SameText(ExtractFileName(fLangList[I]), 'English.lng') and (devData.Language = '') then
            fCurLang := s;
          if s = '' then
            fLangList[I] := format('%s=%s', [fLangList[I], ChangeFileExt(ExtractFileName(fLangList[I]), '')])
          else
            fLangList[I] := format('%s=%s', [fLangList[I], s]);
        end;
      finally
        sl.Free;
      end;
    end;
    if fCurLang = '' then
      fCurLang := devData.Language;
  finally
    fLangListLoaded := True;
  end;
end;

function TdevMultiLangSupport.GetString(ID: integer): AnsiString;
begin
  result := fStrings.Values[IntToStr(ID)];
  if Result = '' then
    Result := fDefaultLangStrings.Values[IntToStr(ID)];
  if result = '' then
    result := format('<ID %d translation missing>', [ID])
  else
    result := StringReplace(result, '<CR>', #13#10, [rfReplaceAll]);
end;

function TdevMultiLangSupport.GetLangName: AnsiString;
begin
  result := fCurLang;
end;

procedure TdevMultiLangSupport.SelectLanguage;
begin
  with TLangForm.Create(nil) do try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TdevMultiLangSupport.SetLang(const Lang: AnsiString);
var
  I: integer;
begin
  if SameText(Lang, fCurLang) then
    exit;
  if not fLangListLoaded then
    CheckLanguageFiles; // can be slow, so load on demand
  for I := 0 to fLangList.Count - 1 do
    if SameText(ExtractFileName(fLangList.Names[I]), Lang) then begin
      Open(fLangList.Names[I]);
      break;
    end;
end;

function TdevMultiLangSupport.FileFromDescription(const Desc: AnsiString): AnsiString;
var
  i: integer;
begin
  // returns the filename of the lang file described as Desc
  // for example with Desc="English (Original)", returns "English.lng"
  Result := Desc;
  if not fLangListLoaded then
    CheckLanguageFiles; // can be slow, so load on demand
  for i := 0 to fLangList.Count - 1 do
    if SameText(fLangList.ValueFromIndex[i], Desc) then begin
      Result := ExtractFilename(fLangList.Names[i]);
      Break;
    end;
end;

function TdevMultiLangSupport.GetLangList: TStringList;
begin
  if not fLangListLoaded then
    CheckLanguageFiles; // can be slow, so load on demand
  Result := fLangList;
end;

end.

