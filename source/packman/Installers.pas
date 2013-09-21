unit Installers;

interface

uses
{$IFDEF WIN32}
  SysUtils, Classes, InstallFiles, Forms, Windows, Dialogs;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Classes, InstallFiles, QForms, QDialogs;
{$ENDIF}

type
  TProgressEvent = procedure(Sender: TObject; CurrentFile: TInstallFile;
    Progress, Max: Integer) of Object;

  TInstaller = Class
  private
    FAbort, FInstalling: Boolean;
    FInfo: TInstallInfo;
    FOnAbort: TNotifyEvent;
    FOnProgress: TProgressEvent;
  public
    constructor Create(Info: TInstallInfo);
    procedure Abort;
    function Install: Boolean;

    property OnAbort: TNotifyEvent read FOnAbort write FOnAbort;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
  end;

implementation

uses
  RemoveForms, PackmanUtils;

constructor TInstaller.Create(Info: TInstallInfo);
begin
  inherited Create;
  FAbort := False;
  FInstalling := False;
  FInfo := Info;
end;

procedure TInstaller.Abort;
begin
  if FInstalling then
      FAbort := True;
end;

procedure Mkdir(const DirName: String);
var
  Dirs: TStringList;
  i: Integer;
begin
  Dirs := TStringList.Create;
  for i := 1 to Length(DirName) do
      if DirName[i] = '\' then
          Dirs.Add(Copy(DirName, 0, i - 1));
  Dirs.Add(DirName);

  for i := 0 to Dirs.Count - 1 do
      if not DirectoryExists(Dirs.Strings[i]) then
          CreateDirectory(PChar(Dirs.Strings[i]), nil);
  Dirs.Free;
end;

function TInstaller.Install: Boolean;
var
  i: Integer;
  Files: TInstallFiles;
  TheFile: TInstallFile;
  EntryName: String;
  F: TextFile;
  IMod: Integer;
  strDevRoot: String;
begin
  Result := True;
  FInstalling := True;

  Files := FInfo.Files(ExtractFileDir(ParamStr(0)));

  EntryName := ChangeFileExt(ExtractFileName(Files.InfoFile), '.entry');
  EntryName := Files.AppDir + '\Packages\' + EntryName;

  { First uninstall old package, if it exists }
  if FileExists(EntryName) then
  begin
      RemoveForm := TRemoveForm.Create(Application);
      with RemoveForm do
      try
         Entry := EntryName;
         CloseWhenDone := True;
         ShowModal;
      finally
         Free;
      end;
  end;

  { Copy the files }
  IMod := CalcMod(Files.Count);

  for i := 0 to Files.Count - 1 do
  begin
      if FAbort then
          Break;

      TheFile := Files.Files[i];
      if (IMod = 0) or (i mod IMod = 0) then
          if Assigned(FOnProgress) then
              FOnProgress(Self, TheFile, i + 1, Files.Count);

      if not DirectoryExists(ExtractFileDir(TheFile.Dest)) then
          Mkdir(ExtractFileDir(TheFile.Dest));
      CopyFile(PChar(TheFile.Source), PChar(TheFile.Dest), False);

      if (IMod = 0) or (i mod IMod = 0) then
          Application.ProcessMessages;
  end;


  { Create icons }
  for i := 0 to Files.IconCount - 1 do
  begin
      if not DirectoryExists(ExtractFileDir(Files.Icons[i].FileName)) then
          MkDir(ExtractFileDir(Files.Icons[i].FileName));
      CreateShortcut(Files.Icons[i].FileName, Files.Icons[i].Target,
        Files.Icons[i].Icon);
  end;

  { Create a package entry and write basic information }
  Mkdir(Files.AppDir + '\Packages');
  AssignFile(F, EntryName);
  Rewrite(F);

  Writeln(F, '[Setup]');
  Writeln(F, 'AppName=' + FInfo.AppName);
  Writeln(F, 'AppVersion=' + FInfo.AppVersion);
  Writeln(F, 'Description=' + FInfo.Description);
  Writeln(F, 'Url=' + FInfo.URL);

  strDevRoot := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));

  { Write the file logs }
  Writeln(F, '');
  Writeln(F, '[Files]');
  with Files do
  begin
    for i := 0 to Count - 1 do
      if Pos(strDevRoot, Files[i].Dest) = 1 then
        //if in root of dev-c++, write just relative path to the root
        Writeln(F, Copy(Files[i].Dest, Length(strDevRoot) + 1,
          Length(Files[i].Dest) - Length(strDevRoot)))
      else
        Writeln(F, Files[i].Dest);
    for i := 0 to IconCount - 1 do
        Writeln(F, Icons[i].FileName);
  end;

  Flush(F);
  CloseFile(F);

  { Finish }
  FInstalling := False;
  if FAbort then
  begin
      FAbort := False;
      if Assigned(FOnAbort) then
          FOnAbort(Self);
      Result := False;
  end;
end;

end.
