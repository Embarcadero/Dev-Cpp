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

unit project;

interface

uses 
{$IFDEF WIN32}
  IniFiles, SysUtils, Dialogs, ComCtrls, Editor, Contnrs,
  Classes, Controls, version, prjtypes, Templates, Forms,
  Windows;
{$ENDIF}
{$IFDEF LINUX}
  IniFiles, SysUtils, QDialogs, QComCtrls, Editor, Contnrs,
  Classes, QControls, version, prjtypes, Templates, QForms
  ;
{$ENDIF}

type
 TdevINI = class(TObject)
  private
   fINIFile: TmemINIFile;
    fSection: string;
    fFileName: string;
    procedure SetFileName(const Value: string);
    procedure SetSection(const Value: string);
  public
   destructor Destroy; override;

   procedure Write(Name, value: string); overload;
   procedure Write(Name: string; value: boolean); overload;
   procedure Write(Name: string; value: integer); overload;
   procedure Write(index: integer; value: string); overload;
   procedure Write(index: integer; Item: string; Value: string); overload;
   procedure Write(index: integer; Item: string; Value: boolean); overload;
   procedure Write(index: integer; Item: string; Value: integer); overload;

   function Read(Name, Default: string): string; overload;
   function Read(Name: string; Default: boolean): boolean; overload;
   function Read(Name: string; Default: integer): integer; overload;
   function Read(index: integer): string; overload; // read unit entry
   function Read(index: integer; Item: string; default: string): string; overload;
   function Read(index: integer; Item: string; default: boolean): boolean; overload;
   function Read(index: integer; Item: string; default: integer): integer; overload;

   function ValueExists(const value: string): boolean;
   procedure DeleteKey(const value: string);
   // uses fsection if section = ''
   procedure ClearSection(const Section: string = '');
   procedure EraseUnit(const index: integer);
   procedure UpdateFile;
   property FileName: string read fFileName write SetFileName;
   property Section: string read fSection write SetSection;
 end;

 TProjUnit = class;

 TUnitList = class
  private
   fList: TObjectList;
   function GetCount: integer;
   function  GetItem(index: integer): TProjUnit;
   procedure SetItem(index: integer; value: TProjUnit);
  public
   constructor Create;
   destructor Destroy; override;
   function Add(aunit: TProjUnit): integer;
   procedure Remove(index: integer);
   function Indexof(FileName: string): integer; overload;
   function Indexof(Node: TTreeNode): integer; overload;
   function Indexof(Editor: TEditor): integer; overload;

   property Items[index: integer]: Tprojunit read GetItem write SetItem; default;
   property Count: integer read GetCount;
 end;

 TProject = class;
 TProjUnit = class
  private
   fParent   : TProject;
   fEditor   : TEditor;
   fFileName : string;
   fNew      : boolean;
   fNode     : TTreeNode;
   fFolder   : string;
   fCompile  : boolean;
   fCompileCpp: boolean;
   fOverrideBuildCmd: boolean;
   fBuildCmd : string;
   fLink  : boolean;
   fPriority: integer;
   function GetDirty: boolean;
   procedure SetDirty(value: boolean);
  public
   constructor Create(aOwner: TProject);
   destructor Destroy; override;
   function Save: boolean;
   function SaveAs: boolean;
   property Editor: TEditor read fEditor write fEditor;
   property FileName: string read fFileName write fFileName;
   property New: boolean read fNew write fNew;
   property Dirty: boolean read GetDirty write SetDirty;
   property Node: TTreeNode read fNode write fNode;
   property Parent: TProject read fParent write fParent;
   property Folder: string read fFolder write fFolder;
   property Compile: boolean read fCompile write fCompile;
   property CompileCpp: boolean read fCompileCpp write fCompileCpp;
   property OverrideBuildCmd: boolean read fOverrideBuildCmd write fOverrideBuildCmd;
   property BuildCmd: string read fBuildCmd write fBuildCmd;
   property Link: boolean read fLink write fLink;
   property Priority: integer read fPriority write fPriority;
   procedure Assign(Source: TProjUnit);
 end;

 TProject = class
  private
   fUnits: TUnitList;
   fOptions: TProjOptions;
   finiFile: Tdevini;
   fName: string;
   fFileName: string;
   fNode: TTreeNode;
   fModified: boolean;
   fFolders: TStringList;
   { begin XXXKF }
   fFolderNodes: TObjectList;
   { end XXXKF }
   fCmdLineArgs: string;
   fUseCustomMakefile: boolean;
   fCustomMakefile: string;
   function GetDirectory : string;
   function GetExecutableName : string;
   procedure SetFileName(value: string);
   procedure SetNode(value: TTreeNode);
   function GetModified: boolean;
   procedure SetModified(value: boolean);
   procedure SortUnitsByPriority;
    procedure SetCmdLineArgs(const Value: string);
    procedure SetCustomMakefile(const Value: string);
    procedure SetUseCustomMakefile(const Value: boolean);
  public
   property Options: TProjOptions read fOptions write fOptions;
   property Name: string read fName write fName;
   property FileName: string read fFileName write SetFileName;
   property Node: TTreeNode read fNode write SetNode;

   property Directory : string read GetDirectory;
   property Executable : string read GetExecutableName;

   property Units: TUnitList read fUnits write fUnits;
   property INIFile: TdevINI read fINIFile write fINIFile;
   property Modified: boolean read GetModified write SetModified;

   property CmdLineArgs: string read fCmdLineArgs write SetCmdLineArgs;

   property UseCustomMakefile: boolean read fUseCustomMakefile write SetUseCustomMakefile;
   property CustomMakefile: string read fCustomMakefile write SetCustomMakefile;
  public
   constructor Create(nFileName, nName : string);
   destructor Destroy; override;
   function NewUnit(NewProject : boolean; CustomFileName: string = ''): integer;
   { begin XXXKF changed }
   function AddUnit(s : string; var pFolder: TTreeNode; Rebuild: Boolean) : TProjUnit;
   { end XXXKF changed }
   function GetFolderPath(Node: TTreeNode): string;
   procedure UpdateFolders;
   procedure AddFolder(s: string);
   function OpenUnit(index : integer): TEditor;
   procedure CloseUnit(index: integer);
   procedure SaveUnitAs(i : integer; sFileName : string);
   procedure Save;
   procedure LoadLayout;
   procedure LoadUnitLayout(e: TEditor; Index: integer);
   procedure SaveLayout;
   procedure SaveUnitLayout(e: TEditor; Index: integer);
   function MakeProjectNode : TTreeNode;
   { begin XXXKF changed }
   function MakeNewFileNode(s : string; IsFolder: boolean; NewParent: TTreeNode) : TTreeNode;
   { end XXXKF changed }
   procedure BuildPrivateResource(ForceSave: boolean = False);
   procedure Update;
   procedure UpdateFile;
   function UpdateUnits: Boolean;
   procedure Open;
   function FileAlreadyExists(s : string) : boolean;
   function Remove(index : integer; DoClose : boolean) : boolean;
   function GetUnitFromEditor(ed : TEditor) : integer;
   function GetUnitFromString(s : string) : integer;
   function GetUnitFromNode(t : TTreeNode) : integer;
   function GetFullUnitFileName(const index: integer): string;
   function DoesEditorExists(e : TEditor) : boolean;
   procedure AddLibrary(s : string);
   procedure AddInclude(s : string);
   procedure RemoveLibrary(index : integer);
   procedure RemoveInclude(index : integer);
   procedure RebuildNodes;
   function ListUnitStr(const sep: char): string;
   procedure Exportto(const HTML: boolean);
   procedure ShowOptions;
   function AssignTemplate(const aFileName: string; const aTemplate: TTemplate): boolean;
   procedure SetHostApplication(s : string);
   { begin XXXKF }
   function FolderNodeFromName(name: string):TTreeNode;
   procedure CreateFolderNodes;
   procedure UpdateNodeIndexes;
   procedure SetNodeValue(value: TTreeNode);
   { end XXXKF }
   procedure CheckProjectFileForUpdate;
   procedure IncrementBuildNumber;
 end;

implementation

uses
  main, MultiLangSupport, devcfg, ProjectOptionsFrm, datamod, utils,
  RemoveUnitFrm, ResourceSelector;

{ TProjUnit }

constructor TProjUnit.Create(aOwner: TProject);
begin
  fEditor:= nil;
  fNode:= nil;
  fParent:= aOwner;
end;

destructor TProjUnit.Destroy;
begin
  if Assigned(fEditor) then
    FreeAndNil(fEditor);
  fNode:=nil;
  inherited;
end;

function TProjUnit.Save: boolean;
begin
  if (fFileName = '') or (fNew) then
   result:= SaveAs
  else
   try
    // if no editor created open one save file and close
    // creates a blank file.
    if (not assigned(fEditor) and not FileExists(fFileName)) then
     begin
       fEditor:= TEditor.Create;
       fEditor.Init(TRUE, ExtractFileName(fFileName), fFileName, FALSE);
       if devEditor.AppendNewline then
         with fEditor.Text do
           if Lines.Count > 0 then
             if Lines[Lines.Count -1] <> '' then
               Lines.Add('');
       fEditor.Text.Lines.SavetoFile(fFileName);
       fEditor.New := False;
       fEditor.Modified := False;
       FreeAndNil(fEditor);
     end
    else
    if assigned(fEditor) and fEditor.Modified then
     begin
       if devEditor.AppendNewline then
         with fEditor.Text do
           if Lines.Count > 0 then
             if Lines[Lines.Count -1] <> '' then
               Lines.Add('');
       fEditor.Text.Lines.SaveToFile(fEditor.FileName);
       if FileExists(fEditor.FileName) then
         FileSetDate(fEditor.FileName, DateTimeToFileDate(Now)); // fix the "Clock skew detected" warning ;)
       fEditor.New := False;
       fEditor.Modified:= FALSE;
     end;
    if assigned(fNode) then
     fNode.Text:= ExtractfileName(fFileName);
    result:= TRUE;
   except
    result:= FALSE;
   end;
end;

function TProjUnit.SaveAs: boolean;
var
 flt: string;
 CFilter, CppFilter, HFilter: Integer;
begin
  with dmMain.SaveDialog do
   begin
     if fFileName = '' then
      FileName:= fEditor.TabSheet.Caption
     else
      FileName:= ExtractFileName(fFileName);

     if fParent.Options.useGPP then
      begin
        BuildFilter(flt, [FLT_CPPS, FLT_CS, FLT_HEADS]);
        DefaultExt:= CPP_EXT;
        CFilter := 3;
        CppFilter := 2;
        HFilter := 4;
      end
     else
      begin
        BuildFilter(flt, [FLT_CS, FLT_CPPS, FLT_HEADS]);
        DefaultExt:= C_EXT;
        CFilter := 2;
        CppFilter := 3;
        HFilter := 4;
      end;

     Filter:= flt;
     if (CompareText(ExtractFileExt(FileName), '.h') = 0) or
       (CompareText(ExtractFileExt(FileName), '.hpp') = 0) or
       (CompareText(ExtractFileExt(FileName), '.hh') = 0) then
     begin
         FilterIndex := HFilter;
     end else
     begin
         if fParent.Options.useGPP then
             FilterIndex := CppFilter
         else
             FilterIndex := CFilter;
     end;

     InitialDir:= ExtractFilePath(fFileName);
     Title:= Lang[ID_NV_SAVEFILE];
     if Execute then
      try
       if FileExists(FileName) and
           (MessageDlg(Lang[ID_MSG_FILEEXISTS],
             mtWarning, [mbYes, mbNo], 0) = mrNo) then
       begin
           Result := False;
           Exit;
       end;

       fNew:= FALSE;
       fFileName:= FileName;
       if assigned(fEditor) then
        fEditor.FileName:= fFileName;
       result:= Save;
      except
       result:= FALSE;
      end
     else
      result:= FALSE;
   end;
end;

function TProjUnit.GetDirty: boolean;
begin
  if assigned(fEditor) then
   result:= fEditor.Modified
  else
   result:= FALSE;
end;

procedure TProjUnit.SetDirty(value: boolean);
begin
  if assigned(fEditor) then
   fEditor.Modified:= value;
end;

procedure TProjUnit.Assign(Source: TProjUnit);
begin
  fEditor:=Source.fEditor;
  fFileName:=Source.fFileName;
  fNew:=Source.fNew;
  Dirty:=Source.Dirty;
  fNode:=Source.fNode;
  fParent:=Source.fParent;
  fFolder:=Source.fFolder;
  fCompile:=Source.fCompile;
  fCompileCpp:=Source.fCompileCpp;
  fOverrideBuildCmd:=Source.fOverrideBuildCmd;
  fBuildCmd:=Source.fBuildCmd;
  fLink:=Source.fLink;
  fPriority:=Source.fPriority;
end;

{ TProject }

constructor TProject.Create(nFileName, nName: string);
begin
  inherited Create;
  fNode := nil;
  fFolders:=TStringList.Create;
  fFolders.Duplicates:=dupIgnore;
  fFolders.Sorted:=True;
  { begin XXXKF }
  fFolderNodes:=TObjectList.Create;
  fFolderNodes.OwnsObjects:=false;
  { end XXXKF }
  fUnits:= TUnitList.Create;
  fFileName := nFileName;
  finiFile:= TdevINI.Create;
  try
    finiFile.FileName:= fFileName;
  except
    fFileName := '';
    MessageDlg('Could not read project file, make sure you have the correct permissions to read it.', mtError, [mbOK], 0);
    exit;
  end;
  finiFile.Section:= 'Project';

  InitOptionsRec(fOptions);
  if nName = DEV_INTERNAL_OPEN then
    Open
  else
   begin
     fName := nName;
     fIniFile.Write('filename', nFileName);
     fIniFile.Write('name', nName);
     fNode := MakeProjectNode;
   end;
end;

destructor TProject.Destroy;
begin
  if fModified then Save;
  fFolders.Free;
  fFolderNodes.Free;
  fIniFile.Free;
  fUnits.Free;
  if (fNode <> nil) and (not fNode.Deleting) then
    fNode.Free;
  fOptions.Includes.Free;
  fOptions.Libs.Free;
  fOptions.ResourceIncludes.Free;
  fOptions.MakeIncludes.Free;
  fOptions.ObjFiles.Free;
  inherited;
end;

function TProject.MakeProjectNode : TTreeNode;
begin
  MakeProjectNode := MainForm.ProjectView.Items.Add(nil, Name);
  MakeProjectNode.SelectedIndex:=0;
  MakeProjectNode.ImageIndex:=0;
  MainForm.ProjectView.FullExpand;
end;

{ begin XXXKF changed }
function TProject.MakeNewFileNode(s : string; IsFolder: boolean; NewParent: TTreeNode) : TTreeNode;
begin
  MakeNewFileNode := MainForm.ProjectView.Items.AddChild(NewParent, s);

  if IsFolder then begin
    MakeNewFileNode.SelectedIndex := 4;
    MakeNewFileNode.ImageIndex := 4;
  end
  else begin
    MakeNewFileNode.SelectedIndex := 1;
    MakeNewFileNode.ImageIndex := 1;
  end;

{
  KF: in my opinion, such a blanket expand is a bit too much for large prjs
  MainForm.ProjectView.FullExpand;
  moved to RebuildTreeNodes now
  }
end;
{ end XXXKF changed }

procedure TProject.BuildPrivateResource(ForceSave: boolean = False);
var
  ResFile, Original: TStringList;
  Res, Def, Icon: String;
  comp, i: Integer;
begin
  comp:=0;
  for i:=0 to Units.Count-1 do
    if GetFileTyp(Units[i].FileName)=utRes then
      if Units[i].Compile then
        Inc(comp);

  // if project has no other resources included
  // and does not have an icon
  // and does not include the XP style manifest
  // and does not include version info
  // then do not create a private resource file
  if (comp=0) and (not fOptions.SupportXPThemes) and (not fOptions.IncludeVersionInfo) and (fOptions.Icon = '') then begin
    fOptions.PrivateResource:='';
    exit;
  end;


  // change private resource from <project_filename>.res
  // to <project_filename>_private.res
  //
  // in many cases (like in importing a MSVC project)
  // the project's resource file has already the
  // <project_filename>.res filename.
  if Length(Options.PrivateResource) > 0 then begin
    Res := GetRealPath(Options.PrivateResource, Directory);
    if ChangeFileExt(Res, DEV_EXT) = FileName then
      Res:=ChangeFileExt(FileName, '_private'+RC_EXT);
  end
  else
      Res := ChangeFileExt(FileName, '_private'+RC_EXT);
  Res:= StringReplace(ExtractRelativePath(FileName, Res), ' ', '_', [rfReplaceAll]);

  // don't run the private resource file and header if not modified,
  // unless ForceSave is true
  if (not ForceSave) and FileExists(Res) and FileExists(ChangeFileExt(Res, H_EXT)) and not Modified then
     Exit;

  ResFile := TStringList.Create;
  ResFile.Add('/* THIS FILE WILL BE OVERWRITTEN BY DEV-C++ */');
  ResFile.Add('/* DO NOT EDIT! */');
  ResFile.Add('');

  if fOptions.IncludeVersionInfo then begin
    ResFile.Add('#include <windows.h> // include for version info constants');
    ResFile.Add('');
  end;

  for i:=0 to Units.Count-1 do
    if GetFileTyp(Units[i].FileName)=utRes then
      if Units[i].Compile then
        ResFile.Add('#include "' + GenMakePath(ExtractRelativePath(Directory, Units[i].FileName), False, False) + '"');

  if Length(Options.Icon) > 0 then
  begin
      ResFile.Add('');
      Icon := GetRealPath(Options.Icon, Directory);
      if FileExists(Icon) then
      begin
          Icon := ExtractRelativePath(FileName, Icon);
          Icon := StringReplace(Icon, '\', '/', [rfReplaceAll]);
          ResFile.Add('A ICON MOVEABLE PURE LOADONCALL DISCARDABLE "' + Icon + '"')
      end
      else
          fOptions.Icon := '';
  end;

  if fOptions.SupportXPThemes then begin
    ResFile.Add('');
    ResFile.Add('//');
    ResFile.Add('// SUPPORT FOR WINDOWS XP THEMES:');
    ResFile.Add('// THIS WILL MAKE THE PROGRAM USE THE COMMON CONTROLS');
    ResFile.Add('// LIBRARY VERSION 6.0 (IF IT IS AVAILABLE)');
    ResFile.Add('//');
    if (Options.ExeOutput <> '') then
      ResFile.Add('1 24 "'+GenMakePath2(IncludeTrailingPathDelimiter(Options.ExeOutput) + ExtractFileName(Executable))+'.Manifest"')
    else
      ResFile.Add('1 24 "'+ExtractFileName(Executable)+'.Manifest"');
  end;

  if fOptions.IncludeVersionInfo then begin
    ResFile.Add('');
    ResFile.Add('//');
    ResFile.Add('// TO CHANGE VERSION INFORMATION, EDIT PROJECT OPTIONS...');
    ResFile.Add('//');
    ResFile.Add('1 VERSIONINFO');
    ResFile.Add('FILEVERSION '+Format('%d,%d,%d,%d', [fOptions.VersionInfo.Major, fOptions.VersionInfo.Minor, fOptions.VersionInfo.Release, fOptions.VersionInfo.Build]));
    ResFile.Add('PRODUCTVERSION '+Format('%d,%d,%d,%d', [fOptions.VersionInfo.Major, fOptions.VersionInfo.Minor, fOptions.VersionInfo.Release, fOptions.VersionInfo.Build]));
    case fOptions.typ of
      dptGUI,
      dptCon:  ResFile.Add('FILETYPE VFT_APP');
      dptStat: ResFile.Add('FILETYPE VFT_STATIC_LIB');
      dptDyn:  ResFile.Add('FILETYPE VFT_DLL');
    end;
    ResFile.Add('{');
    ResFile.Add('  BLOCK "StringFileInfo"');
    ResFile.Add('	 {');
    ResFile.Add('		 BLOCK "'+Format('%4.4x%4.4x', [fOptions.VersionInfo.LanguageID, fOptions.VersionInfo.CharsetID])+'"');
    ResFile.Add('		 {');
    ResFile.Add('			 VALUE "CompanyName", "'+fOptions.VersionInfo.CompanyName+'"');
    ResFile.Add('			 VALUE "FileVersion", "'+fOptions.VersionInfo.FileVersion+'"');
    ResFile.Add('			 VALUE "FileDescription", "'+fOptions.VersionInfo.FileDescription+'"');
    ResFile.Add('			 VALUE "InternalName", "'+fOptions.VersionInfo.InternalName+'"');
    ResFile.Add('			 VALUE "LegalCopyright", "'+fOptions.VersionInfo.LegalCopyright+'"');
    ResFile.Add('			 VALUE "LegalTrademarks", "'+fOptions.VersionInfo.LegalTrademarks+'"');
    ResFile.Add('			 VALUE "OriginalFilename", "'+fOptions.VersionInfo.OriginalFilename+'"');
    ResFile.Add('			 VALUE "ProductName", "'+fOptions.VersionInfo.ProductName+'"');
    ResFile.Add('			 VALUE "ProductVersion", "'+fOptions.VersionInfo.ProductVersion+'"');
    ResFile.Add('		 }');
    ResFile.Add('	 }');

    // additional block for windows 95->NT
    ResFile.Add('  BLOCK "VarFileInfo"');
    ResFile.Add('	 {');
    ResFile.Add('		 VALUE "Translation", '+Format('0x%4.4x, %4.4d', [fOptions.VersionInfo.LanguageID, fOptions.VersionInfo.CharsetID]));
    ResFile.Add('	 }');

    ResFile.Add('}');
  end;

  Res := GetRealPath(Res, Directory);
  if ResFile.Count > 3 then
  begin
      if FileExists(Res) and not ForceSave then
      begin
          Original := TStringList.Create;
          Original.LoadFromFile(Res);
          if CompareStr(Original.Text, ResFile.Text) <> 0 then
          begin
            if devEditor.AppendNewline then
              if ResFile.Count > 0 then
                if ResFile[ResFile.Count -1] <> '' then
                  ResFile.Add('');
            ResFile.SaveToFile(Res);
          end;
          Original.Free;
      end
      else
      begin
        if devEditor.AppendNewline then
          if ResFile.Count > 0 then
            if ResFile[ResFile.Count -1] <> '' then
              ResFile.Add('');
        ResFile.SaveToFile(Res);
      end;
      fOptions.PrivateResource := ExtractRelativePath(Directory, Res);
  end
  else
  begin
      if FileExists(Res) then
          DeleteFile(PChar(Res));
      Res := ChangeFileExt(Res, RES_EXT);
      if FileExists(Res) then
          DeleteFile(PChar(Res));
      fOptions.PrivateResource := '';
  end;
  if FileExists(Res) then
    FileSetDate(Res, DateTimeToFileDate(Now)); // fix the "Clock skew detected" warning ;)

  // create XP manifest
  if fOptions.SupportXPThemes then begin
    ResFile.Clear;
    ResFile.Add('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>');
    ResFile.Add('<assembly');
    ResFile.Add('  xmlns="urn:schemas-microsoft-com:asm.v1"');
    ResFile.Add('  manifestVersion="1.0">');
    ResFile.Add('<assemblyIdentity');
    ResFile.Add('    name="DevCpp.Apps.'+StringReplace(Name, ' ', '_', [rfReplaceAll])+'"');
    ResFile.Add('    processorArchitecture="x86"');
    ResFile.Add('    version="1.0.0.0"');
    ResFile.Add('    type="win32"/>');
    ResFile.Add('<description>'+Name+'</description>');
    ResFile.Add('<dependency>');
    ResFile.Add('    <dependentAssembly>');
    ResFile.Add('        <assemblyIdentity');
    ResFile.Add('            type="win32"');
    ResFile.Add('            name="Microsoft.Windows.Common-Controls"');
    ResFile.Add('            version="6.0.0.0"');
    ResFile.Add('            processorArchitecture="x86"');
    ResFile.Add('            publicKeyToken="6595b64144ccf1df"');
    ResFile.Add('            language="*"');
    ResFile.Add('        />');
    ResFile.Add('    </dependentAssembly>');
    ResFile.Add('</dependency>');
    ResFile.Add('</assembly>');
    ResFile.SaveToFile(Executable+'.Manifest');
    FileSetDate(Executable+'.Manifest', DateTimeToFileDate(Now)); // fix the "Clock skew detected" warning ;)
  end
  else
    if FileExists(Executable+'.Manifest') then
      DeleteFile(PChar(Executable+'.Manifest'));

  // create private header file
  Res:=ChangeFileExt(Res, H_EXT);
  ResFile.Clear;
  Def:=StringReplace(ExtractFilename(UpperCase(Res)), '.', '_', [rfReplaceAll]);
  ResFile.Add('/* THIS FILE WILL BE OVERWRITTEN BY DEV-C++ */');
  ResFile.Add('/* DO NOT EDIT ! */');
  ResFile.Add('');
  ResFile.Add('#ifndef '+Def);
  ResFile.Add('#define '+Def);
  ResFile.Add('');
  ResFile.Add('/* VERSION DEFINITIONS */');
  ResFile.Add('#define VER_STRING'#9+Format('"%d.%d.%d.%d"', [fOptions.VersionInfo.Major, fOptions.VersionInfo.Minor, fOptions.VersionInfo.Release, fOptions.VersionInfo.Build]));
  ResFile.Add('#define VER_MAJOR'#9+IntToStr(fOptions.VersionInfo.Major));
  ResFile.Add('#define VER_MINOR'#9+IntToStr(fOptions.VersionInfo.Minor));
  ResFile.Add('#define VER_RELEASE'#9+IntToStr(fOptions.VersionInfo.Release));
  ResFile.Add('#define VER_BUILD'#9+IntToStr(fOptions.VersionInfo.Build));
  ResFile.Add('#define COMPANY_NAME'#9'"'+fOptions.VersionInfo.CompanyName+'"');
  ResFile.Add('#define FILE_VERSION'#9'"'+fOptions.VersionInfo.FileVersion+'"');
  ResFile.Add('#define FILE_DESCRIPTION'#9'"'+fOptions.VersionInfo.FileDescription+'"');
  ResFile.Add('#define INTERNAL_NAME'#9'"'+fOptions.VersionInfo.InternalName+'"');
  ResFile.Add('#define LEGAL_COPYRIGHT'#9'"'+fOptions.VersionInfo.LegalCopyright+'"');
  ResFile.Add('#define LEGAL_TRADEMARKS'#9'"'+fOptions.VersionInfo.LegalTrademarks+'"');
  ResFile.Add('#define ORIGINAL_FILENAME'#9'"'+fOptions.VersionInfo.OriginalFilename+'"');
  ResFile.Add('#define PRODUCT_NAME'#9'"'+fOptions.VersionInfo.ProductName+'"');
  ResFile.Add('#define PRODUCT_VERSION'#9'"'+fOptions.VersionInfo.ProductVersion+'"');
  ResFile.Add('');
  ResFile.Add('#endif /*'+Def +'*/');
  ResFile.SaveToFile(Res);

  if FileExists(Res) then
    FileSetDate(Res, DateTimeToFileDate(Now)); // fix the "Clock skew detected" warning ;)

  ResFile.Free;
end;

function TProject.NewUnit(NewProject : boolean; CustomFileName: String): integer;
var
 newunit: TProjUnit;
 s: string;
{ begin XXXKF changed }
 ParentNode, CurNode: TTreeNode;
{ end XXXKF changed }
begin
  NewUnit:= TProjUnit.Create(Self);
  ParentNode:=Node;
  with NewUnit do
   try
    if Length(CustomFileName) = 0 then
        s:= Directory +Lang[ID_Untitled] +inttostr(dmMain.GetNum)
    else begin
        if ExtractFilePath(CustomFileName)='' then // just filename, no path
            // make it full path filename, so that the save dialog, starts at the right directory ;)
            s:= Directory+ CustomFileName
        else
            s:= CustomFileName;
    end;
    if FileAlreadyExists(s) then
     repeat
      s:= Directory +Lang[ID_Untitled] +inttostr(dmMain.GetNum);
     until not FileAlreadyExists(s);
    Filename := s;
    New := True;
    Editor:= nil;
{ begin XXXKF changed }
    CurNode := MakeNewFileNode(ExtractFileName(FileName),false,ParentNode);
    NewUnit.Node := CurNode;
{ end XXXKF changed }
    result:= fUnits.Add(NewUnit);
    CurNode.Data:= pointer(result);
    Dirty:= TRUE;
    Compile:=True;
    CompileCpp:=Self.Options.useGPP;
    Link:=True;
    Priority:=1000;
    OverrideBuildCmd:=False;
    BuildCmd:='';
    SetModified(TRUE);
   except
    result:= -1;
    NewUnit.Free;
   end;
end;

{ begin XXXKF changed }
function TProject.AddUnit(s : string; var pFolder: TTreeNode; Rebuild: Boolean) : TProjUnit;
var
 NewUnit: TProjUnit;
 s2: string;
 TmpNode : TTreeNode;
begin
  result := nil;
  if s[length(s)] = '.' then     // correct filename if the user gives an alone dot to force the no extension
    s[length(s)] := #0;
  NewUnit:= TProjUnit.Create(Self);
  with NewUnit do
    try
      if FileAlreadyExists(s) then
      begin
        if fname = '' then
          s2:= fFileName
        else
          s2:= fName;
        MessageDlg(format(Lang[ID_MSG_FILEINPROJECT], [s, s2])
                   +#13#10 +Lang[ID_SPECIFY], mtError, [mbok], 0);
        NewUnit.Free;
        Exit;
      end;
      FileName:= s;
      New:= False;
      Editor:= nil;
      Node:= MakeNewFileNode(ExtractFileName(FileName), false, pFolder);
      Node.Data:= pointer(fUnits.Add(NewUnit));
      Folder:=pFolder.Text;
      TmpNode := pFolder.Parent;
      while Assigned(TmpNode) and (TmpNode <> self.fNode) do begin
        Folder := TmpNode.Text + '/' + Folder;
        TmpNode := TmpNode.Parent;
      end;

      case GetFileTyp(s) of
        utSrc, utHead: begin
           Compile:=True;
           CompileCpp:=Self.Options.useGPP;
           Link:=True;
        end;
        utRes: begin
           Compile:=True;
           CompileCpp:=Self.Options.useGPP;
           Link:=False;
           // if a resource was added, force (re)creation of private resource...
           BuildPrivateResource(True);
        end;
        else begin
           Compile:=False;
           CompileCpp:=False;
           Link:=False;
         end;
      end;
      Priority:=1000;
      OverrideBuildCmd:=False;
      BuildCmd:='';
      if Rebuild then
        RebuildNodes;
      SetModified(TRUE);
      Result := NewUnit;
    except
      result := nil;
      NewUnit.Free;
    end;
end;
{ end XXXKF changed }

procedure TProject.Update;
begin
  with finifile do
   begin
     Section:= 'Project';
     fName:= Read('name', '');
     fOptions.Icon:= Read('icon', '');
     if (Read('Ver', 0)> 0) then //ver> 0 is at least a v5 project
      begin
        fOptions.typ:= Read('type', 0);
        fOptions.cmdLines.Compiler:= Read('Compiler', '');
        fOptions.cmdLines.CppCompiler:= Read('CppCompiler', '');
        fOptions.cmdLines.Linker:= Read('Linker', '');
        fOptions.ObjFiles.DelimitedText:= Read('ObjFiles', '');
        fOptions.Libs.DelimitedText:= Read('Libs', '');
        fOptions.Includes.DelimitedText:= Read('Includes', '');
        fOptions.PrivateResource := Read('PrivateResource', '');
        fOptions.ResourceIncludes.DelimitedText:= Read('ResourceIncludes', '');
        fOptions.MakeIncludes.DelimitedText:= Read('MakeIncludes', '');
        fOptions.UseGpp:= Read('IsCpp', FALSE);
        fOptions.ExeOutput := Read('ExeOutput', '');
        fOptions.ObjectOutput := Read('ObjectOutput', '');
        fOptions.OverrideOutput := Read('OverrideOutput', FALSE);
        fOptions.OverridenOutput := Read('OverrideOutputName', '');
        fOptions.HostApplication := Read('HostApplication', '');

        fFolders.CommaText := Read('Folders', '');
        fCmdLineArgs:=Read('CommandLine', '');

        fUseCustomMakefile := Read('UseCustomMakefile', FALSE);
        fCustomMakefile := Read('CustomMakefile', '');

        fOptions.IncludeVersionInfo:=Read('IncludeVersionInfo', False);
        fOptions.SupportXPThemes:=Read('SupportXPThemes', False);
        fOptions.CompilerSet:=Read('CompilerSet', devCompiler.CompilerSet);
        fOptions.CompilerOptions:=Read('CompilerSettings', devCompiler.OptionStr);
        if fOptions.CompilerSet>devCompilerSet.Sets.Count-1 then begin
          fOptions.CompilerSet:=devCompiler.CompilerSet;
          MessageDlg('The compiler set you have selected for this project, no longer '+
                     'exists.'#10'It will be substituted by the global compiler set...',
                     mtError, [mbOk], 0);
        end;

        Section:= 'VersionInfo';
        fOptions.VersionInfo.Major:=            Read('Major',             0);
        fOptions.VersionInfo.Minor:=            Read('Minor',             1);
        fOptions.VersionInfo.Release:=          Read('Release',           1);
        fOptions.VersionInfo.Build:=            Read('Build',             1);
        fOptions.VersionInfo.LanguageID:=       Read('LanguageID',        $0409);
        fOptions.VersionInfo.CharsetID:=        Read('CharsetID',         $04E4);
        fOptions.VersionInfo.CompanyName:=      Read('CompanyName',       '');
        fOptions.VersionInfo.FileVersion:=      Read('FileVersion',       '0.1');
        fOptions.VersionInfo.FileDescription:=  Read('FileDescription',   'Developed using the Dev-C++ IDE');
        fOptions.VersionInfo.InternalName:=     Read('InternalName',      '');
        fOptions.VersionInfo.LegalCopyright:=   Read('LegalCopyright',    '');
        fOptions.VersionInfo.LegalTrademarks:=  Read('LegalTrademarks',   '');
        fOptions.VersionInfo.OriginalFilename:= Read('OriginalFilename',  ExtractFilename(Executable));
        fOptions.VersionInfo.ProductName:=      Read('ProductName',       Name);
        fOptions.VersionInfo.ProductVersion:=   Read('ProductVersion',    '0.1');
        fOptions.VersionInfo.AutoIncBuildNr:=   Read('AutoIncBuildNr',    False);
      end
     else
      begin // dev-c < 4
        fOptions.Ver:= -1;
        if not Read('NoConsole', TRUE) then
         fOptions.typ:= dptCon
        else
         if Read('IsDLL', FALSE) then
          fOptions.Typ:= dptDyn
         else
          fOptions.typ:= dptGUI;

        fOptions.PrivateResource := Read('PrivateResource', '');
        fOptions.ResourceIncludes.DelimitedText:= Read('ResourceIncludes', '');
        fOptions.ObjFiles.Add(read('ObjFiles', ''));
        fOptions.Includes.Add(Read('IncludeDirs', ''));
        fOPtions.cmdLines.Compiler:= Read('CompilerOptions', '');
        fOptions.usegpp:= Read('Use_GPP', FALSE);
        fOptions.ExeOutput := Read('ExeOutput', '');
        fOptions.ObjectOutput := Read('ObjectOutput', '');
        fOptions.OverrideOutput := Read('OverrideOutput', FALSE);
        fOptions.OverridenOutput := Read('OverrideOutputName', '');
        fOptions.HostApplication := Read('HostApplication', '');
      end;
   end;
end;

procedure TProject.UpdateFile;
begin
  with finifile do
   begin
     Section:= 'Project';

     Write('FileName', ExtractRelativePath(Directory, fFileName));
     Write('Name', fName);
     Write('Type', fOptions.typ);
     Write('Ver', 1);
     Write('ObjFiles', fOptions.ObjFiles.DelimitedText);
     Write('Includes', fOptions.Includes.DelimitedText);
     Write('Libs', fOptions.Libs.DelimitedText);
     Write('PrivateResource', fOptions.PrivateResource);
     Write('ResourceIncludes', fOptions.ResourceIncludes.DelimitedText);
     Write('MakeIncludes', fOptions.MakeIncludes.DelimitedText);
     Write('Compiler', fOptions.cmdLines.Compiler);
     Write('CppCompiler', fOptions.cmdLines.CppCompiler);
     Write('Linker', fOptions.cmdLines.Linker);
     Write('IsCpp', fOptions.UseGpp);
     Write('Icon', ExtractRelativePath(Directory, fOptions.Icon));
     Write('ExeOutput', fOptions.ExeOutput);
     Write('ObjectOutput', fOptions.ObjectOutput);
     Write('OverrideOutput', fOptions.OverrideOutput);
     Write('OverrideOutputName', fOptions.OverridenOutput);
     Write('HostApplication', fOptions.HostApplication);

     Write('Folders', fFolders.CommaText);
     Write('CommandLine', fCmdLineArgs);

     Write('UseCustomMakefile', fUseCustomMakefile);
     Write('CustomMakefile', fCustomMakefile);

     Write('IncludeVersionInfo', fOptions.IncludeVersionInfo);
     Write('SupportXPThemes', fOptions.SupportXPThemes);
     Write('CompilerSet', fOptions.CompilerSet);
     Write('CompilerSettings', fOptions.CompilerOptions);

     Section:= 'VersionInfo';
     Write('Major',             fOptions.VersionInfo.Major);
     Write('Minor',             fOptions.VersionInfo.Minor);
     Write('Release',           fOptions.VersionInfo.Release);
     Write('Build',             fOptions.VersionInfo.Build);
     Write('LanguageID',        fOptions.VersionInfo.LanguageID);
     Write('CharsetID',         fOptions.VersionInfo.CharsetID);
     Write('CompanyName',       fOptions.VersionInfo.CompanyName);
     Write('FileVersion',       fOptions.VersionInfo.FileVersion);
     Write('FileDescription',   fOptions.VersionInfo.FileDescription);
     Write('InternalName',      fOptions.VersionInfo.InternalName);
     Write('LegalCopyright',    fOptions.VersionInfo.LegalCopyright);
     Write('LegalTrademarks',   fOptions.VersionInfo.LegalTrademarks);
     Write('OriginalFilename',  fOptions.VersionInfo.OriginalFilename);
     Write('ProductName',       fOptions.VersionInfo.ProductName);
     Write('ProductVersion',    fOptions.VersionInfo.ProductVersion);
     Write('AutoIncBuildNr',    fOptions.VersionInfo.AutoIncBuildNr);

     Section:= 'Project';

     if fOptions.Ver <= 0 then
      begin
        //delete outdated dev4 project options
        DeleteKey('NoConsole');
        DeleteKey('IsDLL');
        DeleteKey('ResFiles');
        DeleteKey('IncludeDirs');
        DeleteKey('CompilerOptions');
        DeleteKey('Use_GPP');
      end;
   end;
end;

function TProject.UpdateUnits: Boolean;
var
 Count: integer;
 idx: integer;
 rd_only : boolean;
begin
  Result := False;
  Count:= 0;
  idx:= 0;
  rd_only := false;
  while idx <= pred(fUnits.Count) do
  begin
     with fUnits[idx] do
     begin
        {$WARN SYMBOL_PLATFORM OFF}
         if fUnits[idx].Dirty and FileExists(fUnits[idx].FileName) and (FileGetAttr(fUnits[idx].FileName) and faReadOnly <> 0) then begin
          // file is read-only
          if MessageDlg(Format(Lang[ID_MSG_FILEISREADONLY], [fUnits[idx].FileName]), mtConfirmation, [mbYes, mbNo], 0)=mrNo then
            rd_only := false
          else if FileSetAttr(fUnits[idx].FileName, FileGetAttr(fUnits[idx].FileName)-faReadOnly) <> 0 then begin
            MessageDlg(Format(Lang[ID_MSG_FILEREADONLYERROR], [fUnits[idx].FileName]), mtError, [mbOk], 0);
            rd_only := false;
          end;
        end;
        {$WARN SYMBOL_PLATFORM ON}

        if (not rd_only) and (not fUnits[idx].Save) and New then
           Exit;

        // saved new file or an existing file add to project file
        if (New and not Dirty) or not New then
        begin
           finifile.Write(Count, ExtractRelativePath(Directory, fUnits[idx].FileName));
           inc(Count);
        end;
        case GetFileTyp(fUnits[idx].FileName) of
          utHead,
            utSrc: finifile.Write(idx, 'CompileCpp', CompileCpp);
          utRes: if Folder='' then Folder:='Resources';
        end;
        finifile.Write(idx, 'Folder', Folder);
        finifile.Write(idx, 'Compile', Compile);
        finifile.Write(idx, 'Link', Link);
        finifile.Write(idx, 'Priority', Priority);
        finifile.Write(idx, 'OverrideBuildCmd', OverrideBuildCmd);
        finifile.Write(idx, 'BuildCmd', BuildCmd);
     end;
     inc(idx);
  end;
  finifile.Write('UnitCount', Count);
  Result := True;
end;

{ begin XXXKF }

function TProject.FolderNodeFromName(name: string):TTreeNode;
var
  i:integer;
begin
  FolderNodeFromName:=fNode;
  If name<>'' then
    for i:=0 to pred(fFolders.Count) do
    begin
      if AnsiCompareText(AnsiDequotedStr(fFolders[i], '"'), AnsiDequotedStr(name, '"'))=0 then
      begin
        FolderNodeFromName:=TTreeNode(fFolderNodes[i]);
        break;
      end;
    end;
end;

procedure TProject.CreateFolderNodes;
var idx:integer;
    findnode, node:TTreeNode;
    s: string;
    I, C: integer;
begin
  fFolderNodes.Clear;
  for idx:=0 to pred(fFolders.Count) do
  begin
    node:=fNode;
    S:=fFolders[idx];
    I:=Pos('/', S);
    while I > 0 do begin
      findnode:=nil;
      for C:=0 to Node.Count-1 do
        if node.Item[C].Text=Copy(S, 1, I-1) then begin
          findnode:=node.Item[C];
          Break;
        end;
      if not Assigned(findnode) then
        node:=MakeNewFileNode(Copy(S, 1, I-1), True, node)
      else
        node:=findnode;
      node.Data:=Pointer(-1);
      Delete(S, 1, I);
      I:=Pos('/', S);
    end;
    node:=MakeNewFileNode(S, True, Node);
    node.Data:=Pointer(-1);
    fFolderNodes.Add(node);
  end;
end;

procedure TProject.UpdateNodeIndexes;
var idx: integer;
begin
  for idx:= 0 to pred(fUnits.Count) do
    fUnits[idx].Node.Data:=pointer(idx);
end;

{ end XXXKF }

{ begin XXXKF changed }

// open is used to determine if layout info is present in the file.
// if it is present the users AutoOpen settings are ignored,
// perhaps we should make it another option?
procedure TProject.Open;
var
 ucount,
 i : integer;
 NewUnit: TProjUnit;
begin
  {$WARN SYMBOL_PLATFORM OFF}
  if FileExists(FileName) and (FileGetAttr(FileName) and faReadOnly <> 0) then begin
    // file is read-only
    if MessageDlg(Format(Lang[ID_MSG_FILEISREADONLY], [FileName]), mtConfirmation, [mbYes, mbNo], 0)=mrYes then
      if FileSetAttr(FileName, FileGetAttr(FileName)-faReadOnly) <> 0 then begin
        MessageDlg(Format(Lang[ID_MSG_FILEREADONLYERROR], [FileName]), mtError, [mbOk], 0);
      end;
  end;
  {$WARN SYMBOL_PLATFORM ON}

  Update;
  fNode := MakeProjectNode;

  CheckProjectFileForUpdate;

  finifile.Section:= 'Project';
  uCount := fIniFile.Read('UnitCount', 0);
  CreateFolderNodes;
  for i:= 0 to pred(uCount) do
   begin
     NewUnit:= TProjUnit.Create(Self);
     with NewUnit do
      begin
        FileName:= ExpandFileto(finifile.Read(i), Directory);
        if not FileExists(FileName) then
        begin
            Application.MessageBox(PChar(Format(Lang[ID_ERR_FILENOTFOUND],
              [FileName])), 'Error', MB_ICONHAND);
            SetModified(TRUE);
            Continue;
        end;
        Folder:=finifile.Read(i, 'Folder', '');

        Compile:=finifile.Read(i, 'Compile', True);
        if finifile.Read(i, 'CompileCpp', 2)=2 then // check if feature not present in this file
          CompileCpp:=Self.Options.useGPP
        else
          CompileCpp:=finifile.Read(i, 'CompileCpp', False);
        Link:=finifile.Read(i, 'Link', True);
        Priority:=finifile.Read(i, 'Priority', 1000);
        OverrideBuildCmd:=finifile.Read(i, 'OverrideBuildCmd', False);
        BuildCmd:=finifile.Read(i, 'BuildCmd', '');

        Editor:= nil;
        New:= FALSE;
        fParent:=self;

        Node:= MakeNewFileNode(ExtractFileName(FileName), False, FolderNodeFromName(Folder));
        Node.Data:= pointer(fUnits.Add(NewUnit));

      end;
   end;

   case devData.AutoOpen of
    0:                              // Open All
     for i:= 0 to pred(fUnits.Count) do
      OpenUnit(i).Activate;
    1:                              // Open First
     OpenUnit(0).Activate;
     else
       LoadLayout;
   end;
   RebuildNodes;
//   MainForm.ProjectView.FullExpand;
end;

{ end XXXKF changed }

procedure TProject.LoadLayout;
var
 layIni: TIniFile;
 top : integer;
 sl: TStringList;
 idx, currIdx: integer;
begin
  sl:=TStringList.Create;
  try
    layIni:=TIniFile.Create(ChangeFileExt(Filename, '.layout'));
    try
      top:=layIni.ReadInteger('Editors', 'Focused', -1);
      // read order of open files and open them accordingly
      sl.CommaText:=layIni.ReadString('Editors', 'Order', '');
    finally
      layIni.Free;
    end;

    for idx:=0 to sl.Count-1 do begin
      currIdx:=StrToIntDef(sl[idx], -1);
      LoadUnitLayout(OpenUnit(currIdx), currIdx);
    end;
  finally
    sl.Free;
  end;

  if (Top> -1) and (fUnits.Count > 0) and (top<fUnits.Count) and Assigned(fUnits[top].Editor) then
   fUnits[top].Editor.Activate;
end;

procedure TProject.LoadUnitLayout(e: TEditor; Index: integer);
var
 layIni: TIniFile;
begin
  layIni:=TIniFile.Create(ChangeFileExt(Filename, '.layout'));
  try
    if Assigned(e) then begin
      e.Text.CaretY:=layIni.ReadInteger('Editor_'+IntToStr(Index), 'CursorRow', 1);
      e.Text.CaretX:=layIni.ReadInteger('Editor_'+IntToStr(Index), 'CursorCol', 1);
      e.Text.TopLine:=layIni.ReadInteger('Editor_'+IntToStr(Index), 'TopLine', 1);
      e.Text.LeftChar:=layIni.ReadInteger('Editor_'+IntToStr(Index), 'LeftChar', 1);
    end;
  finally
    layIni.Free;
  end;
end;

procedure TProject.SaveLayout;
var
 layIni: TIniFile;
 idx: Integer;
 aset: boolean;
 sl: TStringList;
 S: string;
begin
  s := ChangeFileExt(Filename, '.layout');
  if FileIsReadOnly(s) then
    exit;
  layIni:=TIniFile.Create(s);
  try
//  finifile.Section:= 'Views';
//  finifile.Write('ProjectView', devData.ProjectView);
  finifile.Section:= 'Project';

  sl:=TStringList.Create;
  try
    // write order of open files
    sl.Clear;
    for idx:=0 to MainForm.PageControl.PageCount-1 do begin
      S:=MainForm.PageControl.Pages[idx].Caption;
      if (Length(S)>4) and
        (Copy(S, 1, 4)='[*] ') then
        // the file is modified and the tabsheet's caption starts with '[*] ' - delete it
        S:=Copy(S, 5, Length(S)-4);
      if sl.IndexOf(IntToStr(fUnits.Indexof(S)))=-1 then
        sl.Add(IntToStr(fUnits.Indexof(S)));
      if MainForm.PageControl.ActivePageIndex=idx then
        layIni.WriteInteger('Editors', 'Focused', fUnits.Indexof(S));
    end;
    layIni.WriteString('Editors', 'Order', sl.CommaText);
  finally
    sl.Free;
  end;

  // save editor info
  for idx:= 0 to pred(fUnits.Count) do
   with fUnits[idx] do
    begin
      // save info on open state
      aset:= Assigned(editor);
      layIni.WriteBool('Editor_'+IntToStr(idx), 'Open', aset);
      layIni.WriteBool('Editor_'+IntToStr(idx), 'Top', aset and
         (Editor.TabSheet = Editor.TabSheet.PageControl.ActivePage));
      if aset then begin
        layIni.WriteInteger('Editor_'+IntToStr(idx), 'CursorCol', Editor.Text.CaretX);
        layIni.WriteInteger('Editor_'+IntToStr(idx), 'CursorRow', Editor.Text.CaretY);
        layIni.WriteInteger('Editor_'+IntToStr(idx), 'TopLine', Editor.Text.TopLine);
        layIni.WriteInteger('Editor_'+IntToStr(idx), 'LeftChar', Editor.Text.LeftChar);
      end;

      // remove old data from project file
      fIniFile.Section:='Unit'+IntToStr(idx+1);
      fIniFile.DeleteKey('Open');
      fIniFile.DeleteKey('Top');
      fIniFile.DeleteKey('CursorCol');
      fIniFile.DeleteKey('CursorRow');
      fIniFile.DeleteKey('TopLine');
      fIniFile.DeleteKey('LeftChar');
    end;
 {   ** not good !!** if fModified then
      finiFile.UpdateFile; }

  finally
    layIni.Free;
  end;
  fIniFile.Section:='Project';
end;

procedure TProject.SaveUnitLayout(e: TEditor; Index: integer);
var
 layIni: TIniFile;
begin
  layIni:=TIniFile.Create(ChangeFileExt(Filename, '.layout'));
  try
    if Assigned(e) then begin
      layIni.WriteInteger('Editor_'+IntToStr(Index), 'CursorCol', e.Text.CaretX);
      layIni.WriteInteger('Editor_'+IntToStr(Index), 'CursorRow', e.Text.CaretY);
      layIni.WriteInteger('Editor_'+IntToStr(Index), 'TopLine', e.Text.TopLine);
      layIni.WriteInteger('Editor_'+IntToStr(Index), 'LeftChar', e.Text.LeftChar);
    end;
  finally
    layIni.Free;
  end;
end;

procedure TProject.Save;
begin
  if not UpdateUnits then
      Exit;
  UpdateFile;  // so data is current before going to disk
  SaveLayout; // save current opened files, and which is "active".
  if fModified then
    finiFile.UpdateFile;
  SetModified(FALSE);
end;

function TProject.Remove(index : integer; DoClose : boolean) : boolean;
var
 i: integer;
begin
  result := false;
  if index > -1 then
   begin
     // if a resource was removed, force (re)creation of private resource...
     if GetFileTyp(fUnits.GetItem(index).FileName)=utRes then
       BuildPrivateResource(True);
     if DoClose and Assigned(fUnits.GetItem(index).fEditor) then begin
       if not MainForm.CloseEditor(fUnits.GetItem(index).fEditor.TabSheet.PageIndex, False) then
         exit;
     end;
     result := true;
    { this causes problems if the project isn't saved after this, since the erase happens phisically at this moment }
    //if not fUnits.GetItem(index).fNew then
     finifile.EraseUnit(index);

     fUnits.GetItem(index).fNode.Delete;
     fUnits.Remove(index);

     UpdateNodeIndexes();
     SetModified(TRUE);
   end
  else // pick from list
   with TRemoveUnitForm.Create(MainForm) do
    try
     for i:= 0 to pred(fUnits.Count) do
      UnitList.Items.Append(fUnits[i].FileName);
     if (ShowModal = mrOk) and (UnitList.ItemIndex <> -1) then
      Remove(UnitList.ItemIndex, true);
    finally
     Free;
    end;
end;

function TProject.FileAlreadyExists(s : string) : boolean;
begin
  result := TRUE;
  if fUnits.Indexof(s) > -1 then exit;
  result:= FALSE;
end;

function TProject.OpenUnit(index : integer): TEditor;
begin
  result:= nil;
  if (index < 0) or (index > pred(fUnits.Count)) then exit;

  with fUnits[index] do
   begin
     fEditor := TEditor.Create;
     if FileName <> ''then
      try
       chdir(Directory);
       fEditor.Init(TRUE, ExtractFileName(FileName), ExpandFileName(FileName), not New);
       if New then
         if devEditor.DefaulttoPrj then
           fEditor.InsertDefaultText;
       LoadUnitLayout(fEditor, index);
       result:= fEditor;
      except
       MessageDlg(format(Lang[ID_ERR_OPENFILE], [Filename]), mtError, [mbOK], 0);
       FreeAndNil(fEditor);
      end
     else
       FreeAndNil(fEditor);
   end;
end;

{ begin XXXKF changed, doesn't help much though :-( }
procedure TProject.CloseUnit(index: integer);
begin
  if (index< 0) or (index> pred(fUnits.Count)) then exit;
  with fUnits[index] do
   begin
     if assigned(fEditor) then
     begin
       SaveUnitLayout(fEditor, index);
       FreeAndNil(fEditor);
     end;
   end;
end;
{ end XXXKF changed }

procedure TProject.SaveUnitAs(i : integer; sFileName : string);
begin
  if (i< 0) or (i> pred(fUnits.Count)) then exit;

  with fUnits[i] do
   begin
     if FileExists(FileName) then
      New:= FALSE;
     FileName:= sFileName; // fix bug #559694
     if Editor <> nil then
      begin
        Editor.UpdateCaption(ExtractFileName(sFileName));
        // project units are referenced with relative paths.
        Editor.FileName:= GetRealPath(sFileName, Directory);
      end;
     Node.Text := ExtractFileName(sFileName);
     New := False;
     fInifile.Write(i, ExtractRelativePath(Directory, sFileName));
   end;
   Modified := true;
end;

function TProject.GetUnitFromEditor(ed : TEditor) : integer;
begin
  result:= fUnits.Indexof(Ed);
end;

function TProject.GetUnitFromString(s : string) : integer;
begin
  result:= fUnits.Indexof(ExpandFileto(s, Directory));
end;

function TProject.GetUnitFromNode(t : TTreeNode) : integer;
begin
  result:= fUnits.Indexof(t);
end;

function TProject.GetExecutableName : string;
var
  Base: string;
begin
  if fOptions.OverrideOutput and (fOptions.OverridenOutput<>'') then
    Base:=ExtractFilePath(Filename)+fOptions.OverridenOutput
  else
    Base:=ChangeFileExt(Filename, '');

  // only mess with file extension if not supplied by the user
  // if he supplied one, then we assume he knows what he's doing...
  if ExtractFileExt(Base)='' then begin
    if fOptions.typ = dptStat then
      result := ChangeFileExt(Base, LIB_EXT)
    else if fOptions.typ = dptDyn then
      result := ChangeFileExt(Base, DLL_EXT)
    else
      result := ChangeFileExt(Base, EXE_EXT);
  end
  else
    result := Base;

  if Length(Options.ExeOutput) > 0 then begin
      if not DirectoryExists(Options.ExeOutput) then
        try
          SysUtils.ForceDirectories(Options.ExeOutput);
        except
          MessageDlg('Could not create executable output directory: "'
            + Options.ExeOutput + '". Please check your settings', mtWarning, [mbOK], 0);
          exit;
        end;
      Result := GetRealPath(IncludeTrailingPathDelimiter(Options.ExeOutput) +
        ExtractFileName(Result));
  end;
end;

function TProject.GetFullUnitFileName(const index: integer): string;
begin
  result:= ExpandFileto(fUnits[index].FileName, Directory);
end;

function TProject.DoesEditorExists(e : TEditor) : boolean;
var i : integer;
begin
  result:= FALSE;
  for i := 0 to pred(fUnits.Count) do
   if fUnits[i].Editor = e then
    result:= TRUE;
end;

function TProject.GetDirectory : string;
begin
  result := ExtractFilePath(FileName);
end;

procedure TProject.AddLibrary(s : string);
begin
  fOptions.Libs.Add(s);
end;

procedure TProject.AddInclude(s : string);
begin
  fOptions.Includes.Add(s);
end;

procedure TProject.RemoveLibrary(index : integer);
begin
  fOptions.Libs.Delete(index);
end;

procedure TProject.RemoveInclude(index : integer);
begin
  fOptions.Includes.Delete(index);
end;

function TProject.ListUnitStr(const sep: char): string;
var
 idx: integer;
  sDir: string;
begin
  Result:='';
  sDir:=Directory;
  if not CheckChangeDir(sDir) then
    Exit;
  for idx:= 0 to pred(fUnits.Count) do
   result:= result +sep +'"'+ExpandFileName(fUnits[idx].FileName)+'"';
end;

procedure TProject.SetFileName(value: string);
begin
  if fFileName<>value then begin
    fFileName:= value;
    SetModified(True);
    finifile.finifile.Rename(value, FALSE);
  end;
end;

function TProject.GetModified: boolean;
var
 idx: integer;
 ismod: boolean;
begin
  ismod:= FALSE;
  for idx:= 0 to pred(fUnits.Count) do
   if fUnits[idx].Dirty then ismod:= TRUE;

  result:= fModified or ismod;
end;

procedure TProject.SetModified(value: boolean);
begin
  // only mark modified if *not* read-only
  {$WARN SYMBOL_PLATFORM OFF}
  if not FileExists(FileName) or (FileExists(FileName) and (FileGetAttr(FileName) and faReadOnly = 0)) then
  {$WARN SYMBOL_PLATFORM ON}
    fModified:= value;
end;

procedure TProject.SetNode(value: TTreeNode);
begin
  if assigned(fNode) then
   begin
     fNode.DeleteChildren;
     fNode:= Value;
   end;
end;

procedure TProject.SetNodeValue(value: TTreeNode);
begin
   fNode:= Value;
end;

procedure TProject.Exportto(const HTML: boolean);
  function ConvertFilename(Filename, FinalPath, Extension: string): string;
  begin
    Result:=ExtractRelativePath(Directory, Filename);
    Result:=StringReplace(Result, '.', '_', [rfReplaceAll]);
    Result:=StringReplace(Result, '\', '_', [rfReplaceAll]);
    Result:=StringReplace(Result, '/', '_', [rfReplaceAll]);
    Result:=IncludeTrailingPathDelimiter(FinalPath)+Result+Extension;
  end;
var
  idx: integer;
  sl: TStringList;
  fname: string;
  Size: integer;
  SizeStr: string;
  link: string;
  BaseDir: string;
  hFile: integer;
begin
   with dmMain.SaveDialog do begin
      Filter:= dmMain.SynExporterHTML.DefaultFilter;
      DefaultExt := HTML_EXT;
      Title:= Lang[ID_NV_EXPORT];
      if not Execute then
        Abort;
      fname:=Filename;
   end;

  BaseDir:=ExtractFilePath(fname);
  CreateDir(IncludeTrailingPathDelimiter(BaseDir)+'files');
  sl:=TStringList.Create;
  try
    // create index file
    sl.Add('<HTML>');
    sl.Add('<HEADE><TITLE>Dev-C++ project: '+Name+'</TITLE></HEAD>');
    sl.Add('<BODY BGCOLOR=#FFFFFF>');
    sl.Add('<H2>Project: '+Name+'</H2>');
    sl.Add('<B>Index of files:</B>');
    sl.Add('<HR WIDTH="80%">');
    sl.Add('<TABLE ALIGN="CENTER" CELLSPACING=20>');
    sl.Add('<TR><TD><B><U>Filename</U></B></TD><TD><B><U>Location</U></B></TD><TD><B><U>Size</U></B></TD></TR>');
    for idx:=0 to Units.Count-1 do begin
      hFile:=FileOpen(Units[idx].FileName, fmOpenRead);
      if hFile>0 then begin
        Size:=FileSeek(hFile, 0, 2);
        if Size>=1024 then
          SizeStr:=IntToStr(Size div 1024)+' Kb'
        else
          SizeStr:=IntToStr(Size)+' bytes';
        FileClose(hFile);
      end
      else
        SizeStr:='0 bytes';
      link:=ExtractFilename(ConvertFilename(ExtractRelativePath(Directory, Units[idx].FileName), BaseDir, HTML_EXT));
      sl.Add('<TR><TD><A HREF="files/'+link+'">'+ExtractFilename(Units[idx].FileName)+'</A></TD><TD>'+ExpandFilename(Units[idx].FileName)+'</TD><TD>'+SizeStr+'</TD></TR>');
    end;
    sl.Add('</TABLE>');
    sl.Add('<HR WIDTH="80%">');
    sl.Add('<P ALIGN="CENTER"><FONT SIZE=1>Exported by <A HREF="http://www.bloodshed.net/dev">'+DEVCPP+'</A> v'+DEVCPP_VERSION+'</FONT></P>');
    sl.Add('</BODY>');
    sl.Add('</HTML>');
    sl.SaveToFile(fname);

    // export project files
    for idx:=0 to Units.Count-1 do begin
      fname:=Units[idx].FileName;
      sl.LoadFromFile(fname);
      fname:=ConvertFilename(ExtractRelativePath(Directory, Units[idx].FileName), IncludeTrailingPathDelimiter(BaseDir)+'files', HTML_EXT);
      if HTML then
        dmMain.ExportToHtml(sl, fname)
      else
        dmMain.ExportToRtf(sl, fname);
    end;
  finally
    sl.Free;
  end;
end;

procedure TProject.ShowOptions;
var
  IconFileName: String;
  L, I : TStrings;
  R : TStringList;
begin
  L := TStringList.Create;
  I := TStringList.Create;
  R := TStringList.Create;
  with TfrmProjectOptions.Create(MainForm) do
   try
    Project:= Self;

    L.AddStrings(fOptions.Libs);
    I.AddStrings(fOptions.Includes);
    R.AddStrings(fOptions.ResourceIncludes);

    Options:= fOptions;
    btnRemoveIcon.Enabled := Length(Options.Icon) > 0;
    if ShowModal = mrOk then
     begin
       SetModified(TRUE);
       SortUnitsByPriority;
       RebuildNodes;

       fOptions:= Options;

       IconFileName := ChangeFileExt(ExtractFileName(FileName), '.ico');
       {** why deleting the icon ? *
       if Length(fOptions.Icon) = 0 then
       begin
           DeleteFile(PChar(IconFileName));
       end else}
       if (CompareText(IconFileName, fOptions.Icon) <> 0) and (fOptions.Icon <> '') then
       begin
           CopyFile(PChar(fOptions.Icon), PChar(ExpandFileto(IconFileName,
             Directory)), False);
           fOptions.Icon := IconFileName;
           // force save of private resource to force rebuild, since icon has changed...
           BuildPrivateResource(True);
       end
       else
         BuildPrivateResource;

       // update the projects main node caption
       if edProjectName.Text <> '' then
        begin
          fName:= edProjectName.Text;
          fNode.Text:= fName;
        end;
    end
    else begin
       fOptions.Libs.Clear;
       fOptions.Libs.AddStrings(L);
       fOptions.Includes.Clear;
       fOptions.Includes.AddStrings(I);
       fOptions.ResourceIncludes.Clear;
       fOptions.ResourceIncludes.AddStrings(R);
    end;
  finally
    L.Free;
    I.Free;
    R.Free;
    Free;
  end;
end;

function TProject.AssignTemplate(const aFileName: string; const aTemplate: TTemplate): boolean;
var
 Options: TProjOptions;
 idx: integer;
 s, s2: string;
 OriginalIcon, DestIcon: String;
begin
  result:= TRUE;
  try
   if aTemplate.Version = -1 then
    begin
      fName:= format(Lang[ID_NEWPROJECT], [dmmain.GetNumber]);
      fNode.Text:= fName;
      finiFile.FileName:= aFileName;
      NewUnit(FALSE);
      with fUnits[fUnits.Count -1] do
       begin
         Editor:= TEditor.Create;
         Editor.init(TRUE, ExtractFileName(FileName), FileName, FALSE);
         Editor.InsertDefaultText;
         Editor.Activate;
       end;
      exit;
    end;

   fName:= aTemplate.ProjectName;
   finifile.FileName:= aFileName;

   Options:= aTemplate.OptionsRec;
   AssignOptionsRec(Options, fOptions);

   if Length(aTemplate.ProjectIcon) > 0 then
   begin
       OriginalIcon := ExtractFilePath(aTemplate.FileName) +
         aTemplate.ProjectIcon;
       DestIcon := ExpandFileTo(ExtractFileName(ChangeFileExt(FileName,
         '.ico')), Directory);
       CopyFile(PChar(OriginalIcon), PChar(DestIcon), False);
       fOptions.Icon := ExtractFileName(DestIcon);
   end;

   if aTemplate.Version> 0 then // new multi units
    for idx:= 0 to pred(aTemplate.UnitCount) do
     begin
       if aTemplate.OptionsRec.useGPP then
        s:= aTemplate.Units[idx].CppText
       else
        s:= aTemplate.Units[idx].CText;

       if aTemplate.OptionsRec.useGPP then
           NewUnit(FALSE, aTemplate.Units[idx].CppName)
       else
           NewUnit(FALSE, aTemplate.Units[idx].CName);

       with fUnits[fUnits.Count -1] do
        begin
          Editor:= TEditor.Create;
          try
           Editor.Init(TRUE, ExtractFileName(filename), FileName, FALSE);
           if (Length(aTemplate.Units[idx].CppName) > 0) and
              (aTemplate.OptionsRec.useGPP) then
           begin
               Editor.FileName := aTemplate.Units[idx].CppName;
               fUnits[fUnits.Count - 1].FileName := aTemplate.Units[idx].CppName;
           end else if Length(aTemplate.Units[idx].CName) > 0 then
           begin
               Editor.FileName := aTemplate.Units[idx].CName;
               fUnits[fUnits.Count - 1].FileName := aTemplate.Units[idx].CName;
           end;
           // ** if file isn't found blindly inserts text of unit
           s2:= validateFile(s, devDirs.Templates);
           if s2 <> '' then
            begin
              Editor.Text.Lines.LoadFromFile(s2);
              Editor.Modified:= TRUE;
            end
           else
            if s <> '' then
             begin
               s:= StringReplace(s, '#13#10', #13#10, [rfReplaceAll]);
               Editor.InsertString(s, FALSE);
               Editor.Modified:= TRUE;
             end;
           Editor.Activate;
          except
           Editor.Free;
          end;
        end;
     end
    else
     begin
       NewUnit(FALSE);
       with fUnits[fUnits.Count -1] do
        begin
          Editor:= TEditor.Create;
          Editor.init(TRUE, FileName, FileName, FALSE);
          if fOptions.useGPP then
           s:= aTemplate.OldData.CppText
          else
           s:= aTemplate.OldData.CText;
          s:= ValidateFile(s, ExpandFileto(devDirs.Templates, devDirs.Exec));
          if s <> '' then
           begin
             Editor.Text.Lines.LoadFromFile(s);
             Editor.Modified:= TRUE;
           end;
          Editor.Activate;
        end;
     end;
   except
    result:= FALSE;
   end;
end;

{ begin XXXKF changed }

procedure TProject.RebuildNodes;
var
  idx: integer;
  oldPaths: TStrings;
  tempnode: TTreeNode;

begin
  MainForm.ProjectView.Items.BeginUpdate;

  //remember if folder nodes were expanded or collapsed
  //create a list of expanded folder nodes
  oldPaths := TStringList.Create;
  with MainForm.ProjectView do
    for idx := 0 to Items.Count -1 do
    begin
      tempnode := Items[idx];
      if tempnode.Expanded AND (tempnode.Data=Pointer(-1)) then //data=pointer(-1) - it's folder
        oldPaths.Add(GetFolderPath(tempnode));
    end;

  fNode.DeleteChildren;

  CreateFolderNodes;
{
  for idx:=0 to pred(fFolders.Count) do
    MakeNewFileNode(fFolders[idx], True).Data:=Pointer(-1);}
  for idx:= 0 to pred(fUnits.Count) do
   begin
     fUnits[idx].Node:= MakeNewFileNode(ExtractFileName(fUnits[idx].FileName), False, FolderNodeFromName(fUnits[idx].Folder));
     fUnits[idx].Node.Data:= pointer(idx);
   end;
  for idx:=0 to pred(fFolders.Count) do
    TTreeNode(fFolderNodes[idx]).AlphaSort(False);
  Node.AlphaSort(False);

  //expand nodes expanded before recreating the project tree
  fNode.Collapse(True);
  with MainForm.ProjectView do
    for idx := 0 to Items.Count -1 do
    begin
      tempnode := Items[idx];
      if (tempnode.Data=Pointer(-1)) then //it's a folder
        if oldPaths.IndexOf(GetFolderPath(tempnode)) >= 0 then
          tempnode.Expand(False);
    end;
  FreeAndNil(oldPaths);

  fNode.Expand(False);
  MainForm.ProjectView.Items.EndUpdate;
end;
{ end XXXKF changed }

procedure TProject.UpdateFolders;
  procedure RunNode(Node: TTreeNode);
  var
    I: integer;
  begin
    for I:=0 to Node.Count-1 do
      if Node.Item[I].Data=Pointer(-1) then begin
        fFolders.Add(GetFolderPath(Node.Item[I]));
        if Node.Item[I].HasChildren then
          RunNode(Node.Item[I]);
      end;
  end;
  var
    idx: integer;
begin
  fFolders.Clear;
  RunNode(fNode);
  for idx:=0 to Units.Count-1 do
    Units[idx].Folder:=GetFolderPath(Units[idx].Node.Parent);
  SetModified(TRUE);
end;

function TProject.GetFolderPath(Node: TTreeNode): string;
begin
  Result:='';
  if not Assigned(Node) then
    Exit;

  if Node.Data<>Pointer(-1) then // not a folder
    Exit;

  while Node.Data=Pointer(-1) do begin
    Result:=Format('%s/%s', [Node.Text, Result]);
    Node:=Node.Parent;
  end;
  Delete(Result, Length(Result), 1); // remove last '/'
end;

procedure TProject.AddFolder(s: string);
begin
  if fFolders.IndexOf(s)=-1 then begin
    fFolders.Add(s);
    RebuildNodes;
    MainForm.ProjectView.Select(FolderNodeFromName(s));
    FolderNodeFromName(s).MakeVisible;
    SetModified(TRUE);
  end;
end;

procedure TProject.SetHostApplication(s : string);
begin
  fOptions.HostApplication := s;
end;

procedure TProject.CheckProjectFileForUpdate;
var
  oldRes: string;
  sl: TStringList;
  i, uCount: integer;
  cnvt: boolean;
begin
  cnvt:=False;
  finifile.Section:= 'Project';
  uCount := fIniFile.Read('UnitCount', 0);

  // check if using old way to store resources and fix it
  oldRes:=finifile.Read('Resources', '');
  if oldRes<>'' then begin
    CopyFile(PChar(Filename), PChar(FileName+'.bak'), False);
    sl:=TStringList.Create;
    try
      sl.Delimiter:=';';
      sl.DelimitedText:=oldRes;
      for i:=0 to sl.Count-1 do begin
        finifile.Write(uCount+i, 'Filename', sl[i]);
        finifile.Write(uCount+i, 'Folder', 'Resources');
        finifile.Write(uCount+i, 'Compile', True);
      end;
      fIniFile.Write('UnitCount', uCount+sl.Count);
      oldRes:=finifile.Read('Folders', '');
      if oldRes<>'' then
        oldRes:=oldRes+',Resources'
      else
        oldRes:='Resources';
      fIniFile.Write('Folders', oldRes);
      fFolders.Add('Resources');
    finally
      sl.Free;
    end;
    cnvt:=True;
  end;

  finifile.DeleteKey('Resources');
  finifile.DeleteKey('Focused');
  finifile.DeleteKey('Order');
  finifile.DeleteKey('DebugInfo');
  finifile.DeleteKey('ProfileInfo');

  if cnvt then
    MessageDlg('Your project was succesfully updated to a newer file format!'#13#10+
               'If something has gone wrong, we kept a backup-file: "'+
               FileName+'.bak"...', mtInformation, [mbOk], 0);
end;

procedure TProject.SortUnitsByPriority;
var
  I: integer;
  tmpU: TProjUnit;
  Again: boolean;
begin
  repeat
    I:=0;
    Again:=False;
    while I < Units.Count-1 do begin
      if Units[I+1].Priority < Units[I].Priority then begin
        tmpU:=TProjUnit.Create(Self);
        tmpU.Assign(Units[I]);
        Units[I].Assign(Units[I+1]);
        Units[I+1].Assign(tmpU);
        tmpU.Free;
        Again:=True;
      end;
      Inc(I);
    end;
  until not Again;
end;

procedure TProject.SetCmdLineArgs(const Value: string);
begin
  if (Value<>fCmdLineArgs) then begin
    fCmdLineArgs := Value;
    SetModified(TRUE);
  end;
end;

procedure TProject.IncrementBuildNumber;
begin
  Inc(fOptions.VersionInfo.Build);
  SetModified(True);
end;

procedure TProject.SetCustomMakefile(const Value: string);
begin
  if (Value<>fCustomMakefile) then begin
    fCustomMakefile := Value;
    SetModified(true);
  end;
end;

procedure TProject.SetUseCustomMakefile(const Value: boolean);
begin
  if (Value<>fUseCustomMakefile) then begin
    fUseCustomMakefile := Value;
    SetModified(true);
  end;
end;

{ TUnitList }

constructor TUnitList.Create;
begin
  inherited Create;
  fList:= TObjectList.Create;
end;

destructor TUnitList.Destroy;
var
 idx: integer;
begin
  for idx:= pred(fList.Count) downto 0 do Remove(0);
  fList.Free;
  inherited;
end;

function TUnitList.Add(aunit: TProjUnit): integer;
begin
  result:= fList.Add(aunit);
end;

procedure TUnitList.Remove(index: integer);
begin
  fList.Delete(index);
  fList.Pack;
  fList.Capacity:= fList.Count;
end;

function TUnitList.GetCount: integer;
begin
  result:= fList.Count;
end;

function TUnitList.GetItem(index: integer): TProjUnit;
begin
  result:= TProjUnit(fList[index]);
end;

procedure TUnitList.SetItem(index: integer; value: TProjUnit);
begin
  fList[index]:= value;
end;

function TUnitList.Indexof(Editor: TEditor): integer;
begin
  result:= Indexof(editor.FileName);
end;

function TUnitList.Indexof(FileName: string): integer;
var
  s1, s2: String;
begin
  for result:= 0 to pred(fList.Count) do
  begin
     s1 := GetRealPath(TProjUnit(fList[result]).FileName,
       TProjUnit(fList[result]).fParent.Directory);
     s2 := GetRealPath(FileName, TProjUnit(fList[result]).fParent.Directory);
     if CompareText(s1, s2) = 0 then exit;
  end;
  result:= -1;
end;

function TUnitList.Indexof(Node: TTreeNode): integer;
begin
  for result:= 0 to pred(fList.Count) do
   if TProjUnit(fList[result]).Node = Node then exit;
  result:= -1;
end;


{ TdevINI }

destructor TdevINI.Destroy;
begin
  if assigned(fIniFile) then
    fIniFile.Free;
  inherited;
end;

procedure TdevINI.SetFileName(const Value: string);
begin
  fFileName := Value;
  if not assigned(fINIFile) then
   fINIFile:= TmemINIFile.Create(fFileName)
  else
   fINIFile.ReName(fFileName, FALSE);
end;

procedure TdevINI.SetSection(const Value: string);
begin
  fSection := Value;
end;

// reads a boolean value from fsection
function TdevINI.Read(Name: string; Default: boolean): boolean;
begin
  result:= fINIFile.ReadBool(fSection, Name, Default);
end;

// reads a integer value from fsection
function TdevINI.Read(Name: string; Default: integer): integer;
begin
  result:= fINIFile.ReadInteger(fSection, Name, Default);
end;

// reads unit filename for passed index
function TdevINI.Read(index: integer): string;
begin
  result:= fINIFile.ReadString('Unit'+inttostr(index +1), 'FileName', '');
end;

// reads a string subitem from a unit entry
function TdevINI.Read(index: integer; Item: string; default: string): string;
begin
  result:= fINIFile.ReadString('Unit' +inttostr(index +1), Item, default);
end;

// reads a boolean subitem from a unit entry
function TdevINI.Read(index: integer; Item: string; default: boolean): boolean;
begin
  result:= fINIFile.ReadBool('Unit' +inttostr(index +1), Item, default);
end;

// reads an integer subitem from a unit entry
function TdevINI.Read(index: integer; Item: string; default: integer): integer;
begin
  result:= fINIFile.ReadInteger('Unit' +inttostr(index +1), Item, default);
end;

// reads string value from fsection
function TdevINI.Read(Name, Default: string): string;
begin
  result:= fINIFile.ReadString(fSection, Name, Default);
end;

// write unit entry for passed index
procedure TdevINI.Write(index: integer; value: string);
begin
  finifile.WriteString('Unit' +inttostr(index +1), 'FileName', value);
end;

// write a string subitem in a unit entry
procedure TdevINI.Write(index: integer; Item: string; Value: string);
begin
  finifile.WriteString('Unit' +inttostr(index +1), Item, Value);
end;

// write a boolean subitem in a unit entry
procedure TdevINI.Write(index: integer; Item: string; Value: boolean);
begin
  finifile.WriteBool('Unit' +inttostr(index +1), Item, Value);
end;

// write an integer subitem in a unit entry
procedure TdevINI.Write(index: integer; Item: string; Value: integer);
begin
  finifile.WriteInteger('Unit' +inttostr(index +1), Item, Value);
end;

// write string value to fsection
procedure TdevINI.Write(Name, value: string);
begin
  finifile.WriteString(fSection, Name, Value);
end;

// write boolean value to fsection
procedure TdevINI.Write(Name: string; value: boolean);
begin
  fINIFile.WriteBool(fSection, Name, Value);
end;

// write integer value to fsection
procedure TdevINI.Write(Name: string; value: integer);
begin
  fINIFile.WriteInteger(fSection, Name, Value);
end;

procedure TdevINI.UpdateFile;
begin
  {$WARN SYMBOL_PLATFORM OFF}
  if not FileExists(FileName) or (FileExists(FileName) and (FileGetAttr(FileName) and faReadOnly = 0)) then
  {$WARN SYMBOL_PLATFORM ON}
    fINIFile.UpdateFile;
end;

procedure TdevINI.ClearSection(const Section: string = '');
var
 s: string;
 tmp: TStringList;
 idx: integer;
begin
  if Section = '' then
   s:= fSection
  else
   s:= Section;

  if not finifile.SectionExists(s) then exit;
  tmp:= TStringList.Create;
  try
   finifile.ReadSectionValues(s, tmp);
   if tmp.Count = 0 then exit;
   for idx:= 0 to pred(tmp.Count) do
    finifile.DeleteKey(s, tmp[idx]);
  finally
   tmp.Free;
  end;
end;

procedure TdevINI.EraseUnit(const index: integer);
var
 s: string;
begin
  s:= 'Unit' +inttostr(index +1);
  if finifile.SectionExists(s) then
   finifile.EraseSection(s);
end;

procedure TdevINI.DeleteKey(const value: string);
begin
  if ValueExists(value) then
   finifile.DeleteKey(fSection, value);
end;

function TdevINI.ValueExists(const value: string): boolean;
begin
  result:= finifile.ValueExists(fSection, value);
end;

end.
