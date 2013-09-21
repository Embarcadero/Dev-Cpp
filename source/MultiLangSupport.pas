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

(*
 Changes: 12.5.1
 Programmer: Mike Berg
 Desc:  Modified to be a singleton pattern.  Any time the object is called
        through the Lang function the object is auto-created if need be
        and returned. works like devcfg.
 ToUse: The Strings prop is default so just call like Lang[ID_value];
          - it works like a string list.
*)

unit MultiLangSupport;

interface

uses
{$IFDEF WIN32}
  Windows, Dialogs, SysUtils, Classes, oysUtils;
{$ENDIF}
{$IFDEF LINUX}
  QDialogs, SysUtils, Classes, oysUtils;
{$ENDIF}

{$I LangIDs.inc}

type
  TdevMultiLangSupport = class(TObject)
   private
    fLangList   : ToysStringList;
    fLangFile   : string;
    fCurLang    : string;
    fStrings    : TStringList;
    fDefaultLang: TStringList;
    fSelect     : boolean;
    function GetString(ID: integer): string;
    function GetLangName: string;
    constructor Create;
   public
    destructor Destroy; override;
    class function Lang: TdevMultiLangSupport;

    procedure CheckLanguageFiles;
    procedure SelectLanguage;

    function Open(const FileName: string): boolean;
    procedure SetLang(const Lang: string);

    function FileFromDescription(Desc: string): string;

    property Strings[index: integer]: string read GetString; default;//write SetString;
    property CurrentLanguage: string read GetLangName;
    property Langs: ToysStringList read fLangList write fLangList;
  end;

function Lang: TdevMultiLangSupport;

implementation

uses
{$IFDEF WIN32}
  LangFrm, Forms, utils, version, Controls, devcfg;
{$ENDIF}
{$IFDEF LINUX}
  LangFrm, QForms, utils, version, QControls, devcfg;
{$ENDIF}

var
 fLang: TdevMultiLangSupport = nil;
 fExternal: boolean = true;

function Lang: TdevMultiLangSupport;
begin
  if not assigned(fLang) then
   begin
     fExternal:= false;
     try
      fLang:= TdevMultiLangSupport.Create;
     finally
      fExternal:= true;
     end;
   end;
  result:= fLang;
end;

class function TdevMultiLangSupport.Lang: TdevMultiLangSupport;
begin
  result:= MultiLangSupport.Lang;
end;

constructor TdevMultiLangSupport.Create;
var
 ms: TMemoryStream;
begin
  if assigned(fLang) then
   raise Exception.Create('Language Support loaded');
  if fExternal then
   raise Exception.Create('Language Support Externally Created');

  fLangList:= ToysStringList.Create;
  fStrings:= TStringList.Create;
  fDefaultLang := TStringList.Create;
  ms:= TMemoryStream.Create;
  try
   LoadFilefromResource(DEFAULT_LANG_FILE, ms);
   fStrings.LoadFromStream(ms);
   ms.Seek(0, soFromBeginning);
   fDefaultLang.LoadFromStream(ms);
  finally
   ms.free;
  end;

  CheckLanguageFiles;
end;

destructor TdevMultiLangSupport.Destroy;
begin
  fLangList.Free;
  fStrings.Free;
  fDefaultLang.Free;
  fLang:= nil;
  inherited;
end;

function TdevMultiLangSupport.Open(const Filename : string): boolean;
var
 s,
 aFile: string;
 ver: Integer;
 NewStrs: TStringList;
begin
  result:= false;
  aFile:= ValidateFile(FileName, devDirs.Lang);
  if aFile = '' then
   begin
     if fSelect then
      MessageDlg('Could not open language file ' + filename, mtError, [mbOK], 0);
     exit;
   end;

  try // handle overall errors
   NewStrs:= TStringList.Create;

   try // handle newstr freeing
    NewStrs.LoadFromFile(aFile);
    s:= NewStrs.Values['Ver'];

    try // handle invalid ver entry
     ver:= strtoint(s);
    except
     if MessageDlg('The selected language file has an invalid, or is missing a version entry.'#13#10
                  +'You may not have all the required strings for your current Dev-C++ interface.'#13#10
                  +'Please check the Dev-C++ Update or Bloadshed.net for new language files, Continue Opening?',
          mtWarning, [mbYes, mbNo], 0) = mrNo then Exit else ver:= 1;
    end; // end invalid ver test

    fLangFile:= aFile;
    fStrings.Clear;
    fStrings.AddStrings(NewStrs);
   finally
    NewStrs.Free;
   end; // need for NewStrs object

   if ver>=1 then result:= true;

   fCurLang:= fStrings.Values['Lang'];
   if fCurLang = '' then
    fCurLang:= ChangeFileExt(ExtractFileName(aFile), '');
   devData.Language:= ExtractFileName(aFile);
  except
   result:= false;
  end;
end;

procedure TdevMultiLangSupport.CheckLanguageFiles;
var
 idx: integer;
 s: string;
 tmp: TStringList;
begin
  fLangList.Clear;
  if devDirs.Lang = '' then exit;

  FilesFromWildcard(devDirs.Lang , '*.lng',
                     TStringList(fLangList), False,   False, True);
  fLangList.Sort;
  if fLangList.Count> 0 then
   begin
     tmp:= TStringList.Create;
     try
      for idx:= 0 to pred(fLangList.Count) do
       begin
         tmp.Clear;
         tmp.LoadFromFile(fLangList[idx]);
         s:= tmp.Values['Lang'];
         if (Lowercase(ExtractFileName(fLangList[idx]))=LowerCase(DEFAULT_LANG_FILE)) and
           (devData.Language='') then
           fCurLang:=s;
         if s =  '' then
          fLangList[idx]:= format('%s=%s', [fLangList[idx], ChangeFileExt(ExtractFileName(fLangList[idx]), '')])
         else
          fLangList[idx]:= format('%s=%s', [fLangList[idx], s]);
       end;
     finally
      tmp.Free;
     end;
   end;
   if fCurLang='' then
     fCurLang:=devData.Language;
end;

function TdevMultiLangSupport.GetString(ID : integer) : string;
begin
  result:= fStrings.Values[inttostr(ID)];
  if Result = '' then Result := fDefaultLang.Values[inttostr(ID)];
  if result = '' then result:= format('<ERR: %d>', [ID])
    else result:=StringReplace(result, '<CR>', #13#10, [rfReplaceAll]);
end;

function TdevMultiLangSupport.GetLangName: string;
begin
  result:= fCurLang;
end;

procedure TdevMultiLangSupport.SelectLanguage;
begin
  fSelect:= TRUE;
  if fLangList.Count> 0 then
  with TLangForm.Create(Application.Mainform) do
   try
    UpdateList(fLangList);
    if (ShowModal = mrOK) then
     if Selected> -1 then
      begin
        Open(fLangList.Names[Selected]);
        devData.Language:= FileFromDescription(fLangList.Names[Selected]);
      end
     else begin

      Open(DEFAULT_LANG_FILE);
     end;
  finally
    Free;
    fSelect:= FALSE;
  end
  else
   begin
     Open(DEFAULT_LANG_FILE);
     fSelect:= FALSE;
   end;
end;

procedure TdevMultiLangSupport.SetLang(const Lang: string);
var
 idx: integer;
begin
  if Lang = fCurLang then exit;
  for idx := 0 to fLangList.Count - 1 do
    if AnsiSameText(ExtractFileName(fLangList.Names[idx]), Lang) then begin
      Open(fLangList.Names[idx]);
      break;
    end;
end;

function TdevMultiLangSupport.FileFromDescription(Desc: string): string;
var
  idx: integer;
begin
  // returns the filename of the lang file described as Desc
  // for example with Desc="English (Original)", returns "English.lng"
  Result:=Desc;
  for idx:=0 to fLangList.Count-1 do
    if CompareText(fLangList.Values[idx], Desc)=0 then begin
      Result:=ExtractFilename(fLangList.Names[idx]);
      Break;
    end;
end;

end.
