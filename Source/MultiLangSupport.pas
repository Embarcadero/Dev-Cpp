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
  TCPData = record
    CPID: Integer;
    CPName: String;
    CPLang: String;
  end;

  TdevMultiLangSupport = class(TObject)
  private
    fLangList: TStringList;
    fLangListLoaded: Boolean;
    fLangFile: String;
    fCurLang: String;
    fCurCodePage: Integer;
    fStrings: TStringList;
    fDefaultLangStrings: TStringList;
    function GetString(ID: integer): String;
    function GetLangName: String;
    constructor Create;
  public
    destructor Destroy; override;
    procedure SelectLanguage;
    procedure CheckLanguageFiles;
    procedure Open(const FileName: String);
    procedure SetLang(const Lang: String);
    function GetLangList: TStringList;
    function FileFromDescription(const Desc: String): String;
    property Strings[index: integer]: String read GetString; default;
    property CurrentLanguage: String read GetLangName;
    property Langs: TStringList read GetLangList;
  end;
  // https://stackoverflow.com/questions/2574289/codepage-id-to-codepage-name-getencoding-equivalent-in-delphi
  const
    MaxEncodings = 20;//148;

    Encodings: Array[0..MaxEncodings - 1] of TCPData =
    (
      //(CPID: 37; CPName: 'IBM037'),
      //(CPID: 437; CPName: 'IBM437'),
      //(CPID: 500; CPName: 'IBM500'),
      //(CPID: 708; CPName: 'ASMO-708'),
      //(CPID: 720; CPName: 'DOS-720'),
      //(CPID: 737; CPName: 'ibm737'),
      //(CPID: 775; CPName: 'ibm775'),
      //(CPID: 850; CPName: 'ibm850'),
      //(CPID: 852; CPName: 'ibm852'),
      //(CPID: 855; CPName: 'IBM855'),
      //(CPID: 857; CPName: 'ibm857'),
      //(CPID: 858; CPName: 'IBM00858'),
      //(CPID: 860; CPName: 'IBM860'),
      //(CPID: 861; CPName: 'ibm861'),
      //(CPID: 862; CPName: 'DOS-862'),
      //(CPID: 863; CPName: 'IBM863'),
      //(CPID: 864; CPName: 'IBM864'),
      //(CPID: 865; CPName: 'IBM865'),
      //(CPID: 866; CPName: 'cp866'),
      //(CPID: 869; CPName: 'ibm869'),
      //(CPID: 870; CPName: 'IBM870'),
      //(CPID: 874; CPName: 'windows-874'),
      //(CPID: 875; CPName: 'cp875'),
      (CPID: 932; CPName: 'shift_jis'; CPLang: 'Japanese';),
      (CPID: 936; CPName: 'gb2312'; CPLang: 'Chinese';),
      (CPID: 949; CPName: 'ks_c_5601-1987'; CPLang: 'Korean';),
      //(CPID: 950; CPName: 'big5'),
      //(CPID: 1026; CPName: 'IBM1026'),
      //(CPID: 1047; CPName: 'IBM01047'),
      //(CPID: 1140; CPName: 'IBM01140'),
      //(CPID: 1141; CPName: 'IBM01141'),
      //(CPID: 1142; CPName: 'IBM01142'),
      //(CPID: 1143; CPName: 'IBM01143'),
      //(CPID: 1144; CPName: 'IBM01144'),
      //(CPID: 1145; CPName: 'IBM01145'),
      //(CPID: 1146; CPName: 'IBM01146'),
      //(CPID: 1147; CPName: 'IBM01147'),
      //(CPID: 1148; CPName: 'IBM01148'),
      //(CPID: 1149; CPName: 'IBM01149'),
      //(CPID: 1200; CPName: 'utf-16'),
      //(CPID: 1201; CPName: 'unicodeFFFE'),
      (CPID: 1250; CPName: 'windows-1250'; CPLang: 'Croatian';),
      (CPID: 1250; CPName: 'windows-1250'; CPLang: 'Czech';),
      (CPID: 1250; CPName: 'windows-1250'; CPLang: 'Hungarian';),
      (CPID: 1250; CPName: 'windows-1250'; CPLang: 'Polish';),
      (CPID: 1250; CPName: 'windows-1250'; CPLang: 'Romanian';),
      (CPID: 1250; CPName: 'windows-1250'; CPLang: 'Slovak';),
      (CPID: 1250; CPName: 'windows-1250'; CPLang: 'Slovenian';),
      (CPID: 1251; CPName: 'windows-1251'; CPLang: 'Russian';),
      (CPID: 1251; CPName: 'windows-1251'; CPLang: 'Ukrainian';),
      (CPID: 1252; CPName: 'Windows-1252'; CPLang: 'English';),
      (CPID: 1253; CPName: 'windows-1253'; CPLang: 'Greek';),
      (CPID: 1254; CPName: 'windows-1254'; CPLang: 'Turkish';),
      (CPID: 1255; CPName: 'windows-1255'; CPLang: 'Hebrew';),
      //(CPID: 1256; CPName: 'windows-1256'),
      //(CPID: 1257; CPName: 'windows-1257'),
      //(CPID: 1258; CPName: 'windows-1258'),
      //(CPID: 1361; CPName: 'Johab'),
      //(CPID: 10000; CPName: 'macintosh'),
      //(CPID: 10001; CPName: 'x-mac-japanese'),
      //(CPID: 10002; CPName: 'x-mac-chinesetrad'),
      //(CPID: 10003; CPName: 'x-mac-korean'),
      //(CPID: 10004; CPName: 'x-mac-arabic'),
      //(CPID: 10005; CPName: 'x-mac-hebrew'),
      //(CPID: 10006; CPName: 'x-mac-greek'),
      //(CPID: 10007; CPName: 'x-mac-cyrillic'),
      //(CPID: 10008; CPName: 'x-mac-chinesesimp'),
      //(CPID: 10010; CPName: 'x-mac-romanian'),
      //(CPID: 10017; CPName: 'x-mac-ukrainian'),
      //(CPID: 10021; CPName: 'x-mac-thai'),
      //(CPID: 10029; CPName: 'x-mac-ce'),
      //(CPID: 10079; CPName: 'x-mac-icelandic'),
      //(CPID: 10081; CPName: 'x-mac-turkish'),
      //(CPID: 10082; CPName: 'x-mac-croatian'),
      //(CPID: 12000; CPName: 'utf-32'),
      //(CPID: 12001; CPName: 'utf-32BE'),
      //(CPID: 20000; CPName: 'x-Chinese-CNS'),
      //(CPID: 20001; CPName: 'x-cp20001'),
      //(CPID: 20002; CPName: 'x-Chinese-Eten'),
      //(CPID: 20003; CPName: 'x-cp20003'),
      //(CPID: 20004; CPName: 'x-cp20004'),
      //(CPID: 20005; CPName: 'x-cp20005'),
      //(CPID: 20105; CPName: 'x-IA5'),
      //(CPID: 20106; CPName: 'x-IA5-German'),
      //(CPID: 20107; CPName: 'x-IA5-Swedish'),
      //(CPID: 20108; CPName: 'x-IA5-Norwegian'),
      //(CPID: 20127; CPName: 'us-ascii'),
      //(CPID: 20261; CPName: 'x-cp20261'),
      //(CPID: 20269; CPName: 'x-cp20269'),
      //(CPID: 20273; CPName: 'IBM273'),
      //(CPID: 20277; CPName: 'IBM277'),
      //(CPID: 20278; CPName: 'IBM278'),
      //(CPID: 20280; CPName: 'IBM280'),
      //(CPID: 20284; CPName: 'IBM284'),
      //(CPID: 20285; CPName: 'IBM285'),
      //(CPID: 20290; CPName: 'IBM290'),
      //(CPID: 20297; CPName: 'IBM297'),
      //(CPID: 20420; CPName: 'IBM420'),
      //(CPID: 20423; CPName: 'IBM423'),
      //(CPID: 20424; CPName: 'IBM424'),
      //(CPID: 20833; CPName: 'x-EBCDIC-KoreanExtended'),
      //(CPID: 20838; CPName: 'IBM-Thai'),
      //(CPID: 20866; CPName: 'koi8-r'),
      //(CPID: 20871; CPName: 'IBM871'),
      //(CPID: 20880; CPName: 'IBM880'),
      //(CPID: 20905; CPName: 'IBM905'),
      //(CPID: 20924; CPName: 'IBM00924'),
      //(CPID: 20932; CPName: 'EUC-JP'),
      //(CPID: 20936; CPName: 'x-cp20936'),
      //(CPID: 20949; CPName: 'x-cp20949'),
      (CPID: 21025; CPName: 'cp1025'; CPLang: 'Bulgarian';),
      //(CPID: 21866; CPName: 'koi8-u'),
      //(CPID: 28591; CPName: 'iso-8859-1'),
      //(CPID: 28592; CPName: 'iso-8859-2'),
      //(CPID: 28593; CPName: 'iso-8859-3'),
      (CPID: 28594; CPName: 'iso-8859-4'; CPLang: 'Estonian';),
      (CPID: 28594; CPName: 'iso-8859-4'; CPLang: 'Latvian';),
      //(CPID: 28595; CPName: 'iso-8859-5'),
      //(CPID: 28596; CPName: 'iso-8859-6'),
      //(CPID: 28597; CPName: 'iso-8859-7'),
      //(CPID: 28598; CPName: 'iso-8859-8'),
      //(CPID: 28599; CPName: 'iso-8859-9'),
      //(CPID: 28603; CPName: 'iso-8859-13'),
      //(CPID: 28605; CPName: 'iso-8859-15'),
      //(CPID: 29001; CPName: 'x-Europa'),
      //(CPID: 38598; CPName: 'iso-8859-8-i'),
      //(CPID: 50220; CPName: 'iso-2022-jp'),
      //(CPID: 50221; CPName: 'csISO2022JP'),
      //(CPID: 50222; CPName: 'iso-2022-jp'),
      //(CPID: 50225; CPName: 'iso-2022-kr'),
      //(CPID: 50227; CPName: 'x-cp50227'),
      //(CPID: 51932; CPName: 'euc-jp'),
      //(CPID: 51936; CPName: 'EUC-CN'),
      //(CPID: 51949; CPName: 'euc-kr'),
      //(CPID: 52936; CPName: 'hz-gb-2312'),
      //(CPID: 54936; CPName: 'GB18030'),
      //(CPID: 57002; CPName: 'x-iscii-de'),
      //(CPID: 57003; CPName: 'x-iscii-be'),
      //(CPID: 57004; CPName: 'x-iscii-ta'),
      //(CPID: 57005; CPName: 'x-iscii-te'),
      //(CPID: 57006; CPName: 'x-iscii-as'),
      //(CPID: 57007; CPName: 'x-iscii-or'),
      //(CPID: 57008; CPName: 'x-iscii-ka'),
      //(CPID: 57009; CPName: 'x-iscii-ma'),
      //(CPID: 57010; CPName: 'x-iscii-gu'),
      //(CPID: 57011; CPName: 'x-iscii-pa'),
      //(CPID: 65000; CPName: 'utf-7'),
      (CPID: 65001; CPName: 'utf-8')
    );

function Lang: TdevMultiLangSupport;

implementation

uses
  System.WideStrUtils, System.UITypes, LangFrm, Forms, Utils, Controls, devCFG;

var
  fLangSingleton: TdevMultiLangSupport = nil;

function GetEncoding(CPID: Integer): String;
var
  I: Integer;
begin
  Result := 'iso-8859-2'; //put the default encoding here

  for I := 0 to MaxEncodings - 1 do
    if Encodings[I].CPID = CPID then
    begin
      Result := Encodings[I].CPName;
      break;
    end;
end;

function GetCodePage(const ALanguage: String): Integer;
var
  I: Integer;
begin
  Result := 1252; //put the default encoding here

  for I := 0 to MaxEncodings - 1 do
    if ALanguage.IndexOf(Encodings[I].CPLang)>-1 then
    begin
      Result := Encodings[I].CPID;
      break;
    end;
end;


function Lang: TdevMultiLangSupport;
begin
  if not Assigned(fLangSingleton) and not Application.Terminated then
    fLangSingleton := TdevMultiLangSupport.Create;
  Result := fLangSingleton;
end;

constructor TdevMultiLangSupport.Create;
var
  DefaultLangFile: String;
begin
  inherited;
  fLangList := TStringList.Create;
  fLangListLoaded := False; // only load when needed
  fStrings := TStringList.Create;
  fDefaultLangStrings := TStringList.Create;
  fCurCodePage := 1252;

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

procedure TdevMultiLangSupport.Open(const Filename: String);
var
  LangFile: String;
begin
  // Load file from languages directory
  LangFile := ValidateFile(Filename, devDirs.Lang);
  if LangFile = '' then begin
    MessageDlg('Could not open language file ' + Filename, mtError, [mbOK], 0);
    Exit;
  end;

  // Load file into fStrings
  fStrings.LoadFromFile(LangFile,TEncoding.ANSI);
  fLangFile := LangFile;

  // Get languange
  fCurLang := fStrings.Values['Lang'];
  if fCurLang = '' then
    fCurLang := ChangeFileExt(ExtractFileName(LangFile), '');

  fCurCodePage := GetCodePage(fCurLang);
  devData.Language := ExtractFileName(LangFile);
end;

procedure TdevMultiLangSupport.CheckLanguageFiles;
var
  I: integer;
  s: String;
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

function TdevMultiLangSupport.GetString(ID: integer): String;
begin
  result := AnsiToUtf8Ex(fStrings.Values[IntToStr(ID)],fCurCodePage);
  if Result = '' then
    Result := fDefaultLangStrings.Values[IntToStr(ID)];
  if result = '' then
    result := format('<ID %d translation missing>', [ID])
  else
    result := StringReplace(result, '<CR>', #13#10, [rfReplaceAll]);
end;

function TdevMultiLangSupport.GetLangName: String;
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

procedure TdevMultiLangSupport.SetLang(const Lang: String);
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

function TdevMultiLangSupport.FileFromDescription(const Desc: String): String;
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

