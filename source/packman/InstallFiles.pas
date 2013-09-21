unit InstallFiles;

interface

uses
{$IFDEF WIN32}
  Windows, Classes, IniFiles, SysUtils, Dialogs, PackmanUtils;
{$ENDIF}
{$IFDEF LINUX}
  Classes, IniFiles, SysUtils, QDialogs, PackmanUtils;
{$ENDIF}

type
  TIconFile = Class
  public
    FileName: String;
    Target: String;
    Icon: String;
  end;

  TInstallFile = Class
  public
    Source: String;
    Dest: String;
  end;

  TInstallFiles = class
  private
    FAppDir: String;
    FCount: Integer;
    FIconCount: Integer;
    List, IconList: TList;
    FInfoFile: String;
    FMenuName: String;
    function GetFiles(IIndex: Integer): TInstallFile;
    function GetIcons(IIndex: Integer): TIconFile;
    procedure SetAppDir(Dir: String);
  protected
    procedure Clear;
    procedure ApplyConstants(var Str: String);
    function ParseLine(Line: String; var Src, Dest, Flags: String): Boolean;
  public
    constructor Create(InfoFile: String; AppDir: String; MenuName: String);
    destructor Destroy; override;
    procedure Parse;

    property InfoFile: String read FInfoFile;

    property AppDir: String read FAppDir write SetAppDir;
    property Count: Integer read FCount;
    property IconCount: Integer read FIconCount;
    property Files[IIndex: Integer]: TInstallFile read GetFiles;
    property Icons[IIndex: Integer]: TIconFile read GetIcons;
  end;

  TInstallInfo = Class
  private
    FFileName: String;
    FAppName: String;
    FAppVerName: String;
    FAppVersion: String;
    FDependencies: TStringList;
    FDescription: String;
    FLicense: String;
    FLicenseFile: String;
    FMenuName: String;
    FReadme: String;
    FReadmeFile: String;
    FReboot: Boolean;
    FFiles: TInstallFiles;
    FURL: String;
    FVersion: Integer;
  public
    constructor Create(AFileName: String);
    destructor Destroy; override;
    function Parse: Boolean;

    function Files(AppDir: String): TInstallFiles;
    property FileName: String read FFileName;

    property AppName: String read FAppName;
    property AppVerName: String read FAppVerName;
    property AppVersion: String read FAppVersion;
    property Dependencies: TStringList read FDependencies;
    property Description: String read FDescription;
    property License: String read FLicense;
    property LicenseFile: String read FLicenseFile;
    property MenuName: String read FMenuName;
    property Readme: String read FReadme;
    property ReadmeFile: String read FReadmeFile;
    property Reboot: Boolean read FReboot;
    property URL: String read FURL;
    property Version: Integer read FVersion;
  end;

const
  SupportedVersion = 2;

implementation

constructor TInstallInfo.Create(AFileName: String);
begin
  inherited Create;
  FFileName := AFileName;
  FFiles := nil;
  if not Parse then
      raise Exception.Create('Not a DevPackage file.');
end;

destructor TInstallInfo.Destroy;
begin
  if Assigned(FFiles) then
      FFiles.Free;
  FDependencies.Free;
  inherited Destroy;
end;

function TInstallInfo.Parse: Boolean;
var
  Ini: TIniFile;
  StrList: TStringList;
  BaseName: String;
begin
  Result := False;

  Ini := TIniFile.Create(FFileName);
  FAppName := Ini.ReadString('Setup', 'AppName', 'ThIsNaMeDoEsNoTeXiStS-6.1GoLd');
  // this may not be a ini file...
  if CompareStr(FAppName, 'ThIsNaMeDoEsNoTeXiStS-6.1GoLd') = 0 then
  begin
      FAppName := Ini.ReadString('Setup', 'AppName', 'ThIsIsNoTaNiNi');
      if CompareStr(FAppName, 'ThIsIsNoTaNiNi') = 0 then
          Exit;
  end;

  FAppVerName := Ini.ReadString('Setup', 'AppVerName', 'MyPackage version 1.0');
  FAppVersion := Ini.ReadString('Setup', 'AppVersion', '1.0');
  FDescription := Ini.ReadString('Setup', 'Description', '');
  FMenuName := Ini.ReadString('Setup', 'MenuName', '');
  FReboot := Ini.ReadBool('Setup', 'Reboot', False);
  FURL := Ini.ReadString('Setup', 'Url', '');
  FVersion := Ini.ReadInteger('Setup', 'Version', 1);

  FDependencies := TStringList.Create;
  FDependencies.Delimiter := ';';
  FDependencies.DelimitedText := Ini.ReadString('Setup', 'Dependencies', '');

  BaseName := ExtractFilePath(FFileName);
  StrList := TStringList.Create;
  FLicenseFile := Ini.ReadString('Setup', 'License', '');
  if Length(FLicenseFile) > 0 then
  begin
      FLicenseFile := ExpandFileName(BaseName + FLicenseFile);
      StrList.LoadFromFile(FLicenseFile);
      FLicense := StrList.Text;
  end else
      FLicense := '';

  FReadmeFile := Ini.ReadString('Setup', 'Readme', '');
  if Length(FReadmeFile) > 0 then
  begin
      FReadmeFile := ExpandFileName(BaseName + FReadmeFile);
      StrList.LoadFromFile(FReadmeFile);
      FReadme := StrList.Text;
  end else
      FReadme := '';

  StrList.Free;
  Ini.Free;

  Result := True;
end;

function TInstallInfo.Files(AppDir: String): TInstallFiles;
begin
  if not Assigned(FFiles) then
      FFiles := TInstallFiles.Create(FFileName, AppDir, MenuName)
  else if CompareText(FFiles.AppDir, AppDir) <> 0 then
      FFiles.AppDir := AppDir;

  Result := FFiles;
end;

constructor TInstallFiles.Create(InfoFile: String; AppDir: String; MenuName: String);
begin
  inherited Create;
  List := TList.Create;
  IconList := TList.Create;
  FInfoFile := InfoFile;
  FMenuName := MenuName;
  SetAppDir(AppDir);
end;

destructor TInstallFiles.Destroy;
begin
  Clear;
  List.Free;
  IconList.Free;
  inherited Destroy;
end;

function TInstallFiles.GetFiles(IIndex: Integer): TInstallFile;
var
  TheFile: TInstallFile;
begin
  TheFile := List.Items[IIndex];
  Result := TheFile;
end;

function TInstallFiles.GetIcons(IIndex: Integer): TIconFile;
var
  TheIcon: TIconFile;
begin
  TheIcon := IconList.Items[IIndex];
  Result := TheIcon;
end;

procedure TInstallFiles.SetAppDir(Dir: String);
begin
  FAppDir := Dir;
  Parse;
end;

procedure TInstallFiles.Clear;
var
  i: Integer;
  TheFile: TInstallFile;
  TheIcon: TIconFile;
begin
  for i := 0 to List.Count - 1 do
  begin
      TheFile := TInstallFile(List.Items[i]);
      TheFile.Free;
  end;

  for i := 0 to IconList.Count - 1 do
  begin
      TheIcon := TIconFile(IconList.Items[i]);
      TheIcon.Free;
  end;

  FCount := 0;
  FIconCount := 0;
  List.Clear;
  IconList.Clear;
end;

function TInstallFiles.ParseLine(Line: String; var Src, Dest,
  Flags: String): Boolean;
var
  ALine: String;
  Sep, Sep2: Integer;
begin
  ALine := Line;
  { Seperate the text before and after the '=' sign }
  Sep := Pos('=', ALine);
  { An error occured }
  if Sep = 0 then
  begin
      Result := False;
      Exit;
  end;

  Src := ExtractRelativePath(ExtractFilePath(FInfoFile),
    Copy(ALine, 0, Sep - 1));

  ALine := Copy(Line, Sep + 1, Length(Line) - Sep);
  { Find out wether there are flags }
  Sep2 := Pos(';', ALine);
  if Sep2 <> 0 then
  begin
      Dest := Copy(ALine, 0, Sep2);
      Flags := Copy(ALine, Sep2 + 1, Length(ALine) - Sep2);
  end else
      { If not, just copy everything after '=' }
      Dest := ALine;

  Result := True;
end;

procedure ReplaceAll(var Str: String; SFrom, STo: String);
var
  i: Integer;
begin
  { Pos is case sensitive; workaround }
  i := Pos(LowerCase(SFrom), LowerCase(Str));
  while i <> 0 do
  begin
      Delete(Str, i, Length(SFrom));
      Insert(STo, Str, i);
      i := Pos(LowerCase(SFrom), LowerCase(Str));
  end;
end;

procedure TInstallFiles.ApplyConstants(var Str: String);
var
  WinDir, SysDir: array[0..1024] of Char;
begin
  GetWindowsDirectory(WinDir, SizeOf(WinDir));
  GetSystemDirectory(SysDir, SizeOf(WinDir));

  ReplaceAll(Str, '<app>', FAppDir);
  ReplaceAll(Str, '<src>', ExtractFileDir(FInfoFile));
  ReplaceAll(Str, '<win>', WinDir);
  ReplaceAll(Str, '<sys>', SysDir);
end;

procedure TInstallFiles.Parse;
var
  FileSection, IconSection: TStringList;
  Icon: TIconFile;
  i, j: Integer;
  OrgSrc, Src, Dest, Flags: String;
  TheFile: TInstallFile;
  Files: TStringList;
label
  LoadFiles;
begin
  Clear;

  { Load all keys from the [Icons] section }
  if Length(FMenuName) = 0 then
      goto LoadFiles;

  IconSection := TStringList.Create;
  IconSection.LoadFromFile(FInfoFile);
  for i := 0 to IconSection.Count - 1 do
  begin
      if CompareText(IconSection.Strings[0], '[Icons]') = 0 then
      begin
          IconSection.Delete(0);
          Break;
      end else
          IconSection.Delete(0);
  end;

  { Parse the icon filenames }
  for i := 0 to IconSection.Count - 1 do
  begin
      if (Length(IconSection.Strings[i]) = 0) or
        (IconSection.Strings[i][1] = '[') then
          Break;

      Icon := TIconFile.Create;
      Icon.FileName := GetDevcppMenu + '\' + FMenuName + '\' +
        IconSection.Names[i];
      Icon.Target := IconSection.Values[IconSection.Names[i]];

      j := Pos(',', Icon.Target);
      if j > 0 then
      begin
          Icon.Icon := Copy(Icon.Target, j + 1, Length(Icon.Target) - j);
          Delete(Icon.Target, j, Length(Icon.Target) - j + 1);
      end;

      if CompareText(Copy(Icon.Target, 1, 7), 'http://') = 0 then
          Icon.FileName := Icon.FileName + '.url'
      else
          Icon.FileName := Icon.FileName + '.lnk';

      ApplyConstants(Icon.Target);
      ApplyConstants(Icon.Icon);
      IconList.Add(Icon);
  end;


LoadFiles:
  { Load all keys from the [Files] section }
  FileSection := TStringList.Create;
  FileSection.LoadFromFile(FInfoFile);
  for i := 0 to FileSection.Count - 1 do
  begin
      if CompareText(FileSection.Strings[0], '[Files]') = 0 then
      begin
          FileSection.Delete(0);
          Break;
      end else
          FileSection.Delete(0);
  end;

  { Parse the filenames }
  for i := 0 to FileSection.Count - 1 do
  begin
      if (Length(FileSection.Strings[i]) = 0) or
        (FileSection.Strings[i][1] = '[') then
          Break;

      if not ParseLine(FileSection.Strings[i], Src, Dest, Flags) then
          Continue;
      ApplyConstants(Dest);
      OrgSrc := Src;
      Src := ExtractFilePath(FInfoFile) + Src;

      if DirectoryExists(Src) then
      begin
          Files := TStringList.Create;
          FilesFromWildcard(Src, '*', Files, True, False);
          for j := 0 to Files.Count - 1 do
          begin
              TheFile := TInstallFile.Create;
              TheFile.Source := Files.Strings[j];
              TheFile.Dest := Dest + Copy(Files.Strings[j], Length(Src) + 2,
                Length(Files.Strings[j]) - Length(Src));
              List.Add(TheFile);
          end;
          Files.Free;
      end else
      begin
          TheFile := TInstallFile.Create;
          TheFile.Source := Src;
          if Dest[Length(Dest)] = '\' then
              Dest := Dest + ExtractFileName(Src);
          TheFile.Dest := Dest;
          List.Add(TheFile);
      end;
  end;
  FCount := List.Count;
  FIconCount := IconList.Count;
  FileSection.Free;
end;

end.
