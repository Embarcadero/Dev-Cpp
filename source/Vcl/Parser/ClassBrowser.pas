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

unit ClassBrowser;

interface

uses 
{$IFDEF WIN32}
  Windows, Messages, Classes, SysUtils, Controls, ComCtrls, Forms, Graphics,
  CppParser;
{$ENDIF}
{$IFDEF LINUX}
  Classes, SysUtils, QControls, QComCtrls, QForms, QGraphics,
  CppParser;
{$ENDIF}

const
  MAX_CUSTOM_FOLDERS = 250;

type
  TMemberSelectEvent = procedure(Sender: TObject; Filename: TFilename; Line: integer) of object;

  PFolders = ^TFolders;
  TFolders = record
    Index: Char;
    Name: string[32];
    Under: string[164];
    Node: TTreeNode;
  end;

  PFolderAssocs = ^TFolderAssocs;
  TFolderAssocs = record
    FolderID: integer;
    Folder: string[32];
    Command: string[164];
  end;

  TImagesRecord = class(TPersistent)
  private
    fGlobalsImg: integer;
    fClassesImg: integer;
    fVariablePrivateImg: integer;
    fVariableProtectedImg: integer;
    fVariablePublicImg: integer;
    fVariablePublishedImg: integer;
    fMethodPrivateImg: integer;
    fMethodProtectedImg: integer;
    fMethodPublicImg: integer;
    fMethodPublishedImg: integer;
    fInhMethodProtectedImg: integer;
    fInhMethodPublicImg: integer;
    fInhVariableProtectedImg: integer;
    fInhVariablePublicImg: integer;
  published
    property Globals: integer read fGlobalsImg write fGlobalsImg;
    property Classes: integer read fClassesImg write fClassesImg;
    property VariablePrivate: integer read fVariablePrivateImg write fVariablePrivateImg;
    property VariableProtected: integer read fVariableProtectedImg write fVariableProtectedImg;
    property VariablePublic: integer read fVariablePublicImg write fVariablePublicImg;
    property VariablePublished: integer read fVariablePublishedImg write fVariablePublishedImg;
    property MethodPrivate: integer read fMethodPrivateImg write fMethodPrivateImg;
    property MethodProtected: integer read fMethodProtectedImg write fMethodProtectedImg;
    property MethodPublic: integer read fMethodPublicImg write fMethodPublicImg;
    property MethodPublished: integer read fMethodPublishedImg write fMethodPublishedImg;
    property InheritedMethodProtected: integer read fInhMethodProtectedImg write fInhMethodProtectedImg;
    property InheritedMethodPublic: integer read fInhMethodPublicImg write fInhMethodPublicImg;
    property InheritedVariableProtected: integer read fInhVariableProtectedImg write fInhVariableProtectedImg;
    property InheritedVariablePublic: integer read fInhVariablePublicImg write fInhVariablePublicImg;
  end;

  TShowFilter = (sfAll, sfProject, sfCurrent);

  TClassBrowser = class(TCustomTreeView)
  private
    fParser: TCppParser;
    fOnSelect: TMemberSelectEvent;
    fImagesRecord: TImagesRecord;
    fShowFilter: TShowFilter;
    fCurrentFile: string;
    fCurrentFileHeader: string;
    fCurrentFileImpl: string;
    fProjectDir: TFileName;
    fClassFoldersFile: TFileName;
    fFolders: array of TFolders;
    fFolderAssocs: array of TFolderAssocs;
    fLastSelection: string;
    fCnv: TControlCanvas;
    fUseColors: boolean;
    fParserBusy: boolean;
    fShowInheritedMembers: boolean;
    procedure CustomPaintMe(var Msg: TMessage); message WM_PAINT;
    procedure SetParser(Value: TCppParser);
    procedure AddMembers(Node: TTreeNode; ParentIndex, ParentID: integer);
    procedure OnNodeChange(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnNodeChanging(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure myDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure myDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure myMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure OnParserUpdate(Sender: TObject);
    procedure OnParserBusy(Sender: TObject);
    procedure SetNodeImages(Node: TTreeNode; Statement: PStatement);
    procedure Sort;
    procedure SetCurrentFile(Value: string);
    procedure SetShowFilter(const Value: TShowFilter);
    procedure ReadClassFolders; // read folders from disk
    procedure WriteClassFolders; // write folders to disk
    function HasSubFolder(Cmd: string): boolean; // if Command has subfolders, returns true
    procedure CreateFolders(Cmd: string; Node: TTreeNode); // creates folders under Command
    function BelongsToFolder(Cmd: string): integer; // returns the index to fFolders it belongs or -1 if does not
    function GetNodeOfFolder(Index: integer): TTreeNode; overload;
    function GetNodeOfFolder(Folder: string): TTreeNode; overload;
    procedure AddFolderAssociation(Fld, Cmd: string);
    procedure RemoveFolderAssociation(Fld, Cmd: string);
    function IndexOfFolder(Fld: string): integer;
    procedure ReSelect;
    procedure SetUseColors(const Value: boolean);
    procedure SetShowInheritedMembers(const Value: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateView;
    procedure ShowSampleData;
    procedure Clear;
    procedure AddFolder(S: string; Node: TTreeNode);
    procedure RemoveFolder(S: string);
    procedure RenameFolder(Old, New: string);
    function FolderCount: integer;
    procedure SetUpdateOn;
    procedure SetUpdateOff;
  published
    property Align;
    property Font;
    property Color;
    property Images;
    property ReadOnly;
    property Indent;
    property TabOrder;
    property PopupMenu;
    property ShowFilter: TShowFilter read fShowFilter write SetShowFilter;
    property OnSelect: TMemberSelectEvent read fOnSelect write fOnSelect;
    property Parser: TCppParser read fParser write SetParser;
    property ItemImages: TImagesRecord read fImagesRecord write fImagesRecord;
    property CurrentFile: string read fCurrentFile write SetCurrentFile;
    property ProjectDir: TFileName read fProjectDir write fProjectDir;
    property ClassFoldersFile: TFileName read fClassFoldersFile write fClassFoldersFile;
    property UseColors: boolean read fUseColors write SetUseColors;
    property ShowInheritedMembers: boolean read fShowInheritedMembers write SetShowInheritedMembers;
  end;

const
  CLASS_FOLDERS_MAGIC = 'DEVCF_1_0';

implementation

{ TClassBrowser }

procedure TClassBrowser.SetNodeImages(Node: TTreeNode; Statement: PStatement);
var
  bInherited: boolean;
begin
  bInherited := fShowInheritedMembers and Assigned(Node.Parent) and (PStatement(Node.Parent.Data)^._ID <> PStatement(Node.Data)^._ParentID);

  case Statement^._Kind of
    skClass: begin
        Node.ImageIndex := fImagesRecord.Classes;
      end;
    skVariable, skEnum: case Statement^._ClassScope of
        scsPrivate: Node.ImageIndex := fImagesRecord.VariablePrivate;
        scsProtected: if not bInherited then Node.ImageIndex := fImagesRecord.VariableProtected else Node.ImageIndex := fImagesRecord.InheritedVariableProtected;
        scsPublic: if not bInherited then Node.ImageIndex := fImagesRecord.VariablePublic else Node.ImageIndex := fImagesRecord.InheritedVariablePublic;
        scsPublished: if not bInherited then Node.ImageIndex := fImagesRecord.VariablePublished else Node.ImageIndex := fImagesRecord.InheritedVariablePublic;
      else
        Node.ImageIndex := fImagesRecord.VariablePublished;
      end;
    skFunction, skConstructor, skDestructor: case Statement^._ClassScope of
        scsPrivate: Node.ImageIndex := fImagesRecord.MethodPrivate;
        scsProtected: if not bInherited then Node.ImageIndex := fImagesRecord.MethodProtected else Node.ImageIndex := fImagesRecord.InheritedMethodProtected;
        scsPublic: if not bInherited then Node.ImageIndex := fImagesRecord.MethodPublic else Node.ImageIndex := fImagesRecord.InheritedMethodPublic;
        scsPublished: if not bInherited then Node.ImageIndex := fImagesRecord.MethodPublished else Node.ImageIndex := fImagesRecord.InheritedMethodPublic;
      else
        Node.ImageIndex := fImagesRecord.MethodPublished;
      end;
  end;
  Node.SelectedIndex := Node.ImageIndex;
  Node.StateIndex := Node.ImageIndex;
end;

procedure TClassBrowser.AddMembers(Node: TTreeNode; ParentIndex, ParentID: integer);
var
  I, iFrom, tmp, tmpI: integer;
  ParNode, NewNode: TTreeNode;
  F: integer;
  Sl: TStringList;
  tmpS: string;
  bInherited: boolean;
begin
  if (not fShowInheritedMembers) and (ParentIndex >= 0) then
    iFrom := ParentIndex // amazing speed-up
  else
    iFrom := 0; // if showing inheritance, a big speed penalty

  // create folders that have this branch as parent
  if ParentIndex <> -1 then with PStatement(fParser.Statements[ParentIndex])^ do begin
      if HasSubFolder(ExtractFileName(_Filename) + ':' + IntToStr(_Line) + ':' + _FullText) then
        CreateFolders(ExtractFileName(_Filename) + ':' + IntToStr(_Line) + ':' + _FullText, Node);
    end
  else begin
    if HasSubFolder('') then
      CreateFolders('', Node);
  end;

  sl := TStringList.Create;
  try

    // allow inheritance propagation
    if fShowInheritedMembers and (ParentIndex <> -1) and (PStatement(fParser.Statements[ParentIndex])^._Kind = skClass) then begin
      // follow the inheritance tree all the way up.
      // this code does not work for C++ polymorphic classes
      tmp := ParentIndex;
      tmpI := tmp;
      tmpS := '';
      sl.Clear;
      while (tmp <> -1) do begin
        tmpS := PStatement(fParser.Statements[tmpI])^._InheritsFromIDs;
        tmp := StrToIntDef(tmpS, -1);
        tmpI := fParser.IndexOfStatement(tmp);
        if sl.IndexOf(tmpS) <> -1 then
          tmp := -1;
        if (tmp <> -1) then
          sl.CommaText := sl.CommaText + tmpS + ',';
      end;
    end
    else
      sl.Clear;

    bInherited := False;
    for I := iFrom to fParser.Statements.Count - 1 do begin
      with PStatement(fParser.Statements[I])^ do begin
        if not _Visible then
          Continue;
        if _ParentID <> ParentID then begin
          bInherited := fShowInheritedMembers and (sl.IndexOf(IntToStr(_ParentID)) > -1);
          if not bInherited then
            Continue;
        end;

        if (fShowFilter = sfAll) or
          ((fShowFilter = sfProject) and (_InProject or bInherited)) or
          ((fShowFilter = sfCurrent) and ((_Filename = fCurrentFileHeader) or (_Filename = fCurrentFileImpl) or bInherited)) then begin

          // check if belongs to folder
          F := BelongsToFolder(ExtractFileName(_Filename) + ':' + IntToStr(_Line) + ':' + _FullText);
          if F <> -1 then
            ParNode := GetNodeOfFolder(F)
          else
            ParNode := Node;

          if fUseColors then
            NewNode := Items.AddChildObject(ParNode, '  ' + _FullText + '  ', PStatement(fParser.Statements[I]))
          else
            NewNode := Items.AddChildObject(ParNode, _Command, PStatement(fParser.Statements[I]));
          SetNodeImages(NewNode, PStatement(fParser.Statements[I]));
          if (PStatement(fParser.Statements[I])^._Kind = skClass) and (I <> ParentIndex) then  // CL: fixed potential infinite loop bug
            AddMembers(NewNode, I, _ID);
        end;
      end;
    end;
  finally
    sl.Free;
  end;
end;

constructor TClassBrowser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnMouseUp := OnNodeChange;
  OnMouseDown := OnNodeChanging;
  DragMode := dmAutomatic;
  OnDragOver := myDragOver;
  OnDragDrop := myDragDrop;
  SetLength(fFolders, 0);
  SetLength(fFolderAssocs, 0);
  fImagesRecord := TImagesRecord.Create;
  fCurrentFile := '';
  fCurrentFileHeader := '';
  fCurrentFileImpl := '';
  fShowFilter := sfAll;
  fParserBusy := False;
  fProjectDir := '';
  fClassFoldersFile := '';
  ShowHint := True;
  HideSelection := False;
  RightClickSelect := True;
  fShowInheritedMembers := False;
  SetUseColors(True);
end;

destructor TClassBrowser.Destroy;
begin
  SetLength(fFolderAssocs, 0);
  SetLength(fFolders, 0);
  FreeAndNil(fImagesRecord);

  if fUseColors then
    SetUseColors(False);

  inherited Destroy;
end;

procedure TClassBrowser.UpdateView;
begin
  if fParser = nil then
    Exit;

  fParserBusy := True;
  Items.BeginUpdate;
  Clear;
  ReadClassFolders;
  AddMembers(nil, -1, -1);
  Sort;
  if fLastSelection <> '' then
    ReSelect;
  WriteClassFolders;
  Items.EndUpdate;
  fParserBusy := False;
  Repaint;
end;

procedure TClassBrowser.OnNodeChanging(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: TTreeNode;
begin
  if htOnItem in GetHitTestInfoAt(X, Y) then
    Node := GetNodeAt(X, Y)
  else
    Node := nil;
  Selected := Node;
end;

procedure TClassBrowser.OnNodeChange(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: TTreeNode;
begin
  inherited;

  if htOnItem in GetHitTestInfoAt(X, Y) then
    Node := GetNodeAt(X, Y)
  else
    Node := nil;

  if not Assigned(Node) then
  begin
    fLastSelection := '';
    Exit;
  end
  else if not Assigned(Node.Data) then
  begin
    fLastSelection := '';
    Exit;
  end
  else if fParser = nil then
  begin
    Node.Data := nil;
    fLastSelection := '';
    Exit;
  end
  else if fParser.Statements.IndexOf(Node.Data) = -1 then
  begin
    Node.Data := nil;
    fLastSelection := '';
    Exit;
  end;

  if Node.ImageIndex = fImagesRecord.fGlobalsImg then begin
    fLastSelection := PFolders(Node.Data)^.Under;
    Exit;
  end;

  with PStatement(Node.Data)^ do
  begin
    fLastSelection := ExtractFileName(_Filename) + ':' + IntToStr(_Line) + ':' + _FullText;

    if Assigned(fOnSelect) then
      if (Button = mbLeft) and not (ssShift in Shift) then // need implementation
        if _IsDeclaration then
          fOnSelect(Self, _DeclImplFileName, _DeclImplLine)
        else
          fOnSelect(Self, _FileName, _Line)
      else if (Button = mbLeft) and (ssShift in Shift) then // // need declaration
        if _IsDeclaration then
          fOnSelect(Self, _FileName, _Line)
        else
          fOnSelect(Self, _DeclImplFileName, _DeclImplLine);
  end;
end;

procedure TClassBrowser.SetParser(Value: TCppParser);
begin
  if Value <> fParser then begin
    fParser := Value;
    if Assigned(fParser) then begin
      fParser.OnUpdate := OnParserUpdate;
      fParser.OnBusy := OnParserBusy;
    end;
    UpdateView;
  end;
end;

procedure TClassBrowser.SetUpdateOn;
begin
  if Assigned(fParser) then
    fParser.OnUpdate := OnParserUpdate;
end;

procedure TClassBrowser.SetUpdateOff;
begin
  if Assigned(fParser) then
    fParser.OnUpdate := nil;
end;

procedure TClassBrowser.OnParserUpdate(Sender: TObject);
begin
  fParserBusy := False;
  UpdateView;
end;

procedure TClassBrowser.OnParserBusy(Sender: TObject);
begin
  fParserBusy := True;
end;

procedure TClassBrowser.ShowSampleData;
  function CreateTempStatement(Full, Typ, Cmd, Args: string; K: TStatementKind; S: TStatementClassScope): PStatement;
  begin
    Result := New(PStatement);
    with Result^ do begin
      _FullText := Full;
      _Type := Typ;
      _ScopelessCmd := Cmd;
      _Command := _ScopelessCmd;
      _Args := Args;
      _Visible := True;
      _Valid := True;
      _Kind := K;
      _ClassScope := S;
    end;
  end;
var
  Node, SubNode: TTreeNode;
  Statement: PStatement;
begin
  Items.Clear;
  with Items do begin
    Statement := CreateTempStatement('class Class1', 'class', 'Class1', '', skClass, scsNone);
    Node := AddChildObject(nil, '  ' + Statement^._Command + '  ', Statement);
    SetNodeImages(Node, Statement);

    Statement := CreateTempStatement('Class1()', '', 'Class1', '', skConstructor, scsPublic);
    SubNode := AddChildObject(Node, '  ' + Statement^._Command + '  ', Statement);
    SetNodeImages(SubNode, Statement);

    Statement := CreateTempStatement('~Class1()', '', '~Class1', '', skDestructor, scsPublic);
    SubNode := AddChildObject(Node, '  ' + Statement^._Command + '  ', Statement);
    SetNodeImages(SubNode, Statement);

    Statement := CreateTempStatement('int private_Var1', 'int', 'private_Var1', '', skVariable, scsPrivate);
    SubNode := AddChildObject(Node, '  ' + Statement^._Command + '  ', Statement);
    SetNodeImages(SubNode, Statement);

    Statement := CreateTempStatement('int private_Var2', 'int', 'private_Var2', '', skVariable, scsPrivate);
    SubNode := AddChildObject(Node, '  ' + Statement^._Command + '  ', Statement);
    SetNodeImages(SubNode, Statement);

    Statement := CreateTempStatement('void protected_Func1(int x, int y)', 'void', 'protected_Func1', '(int x, int y)', skFunction, scsProtected);
    SubNode := AddChildObject(Node, '  ' + Statement^._Command + '  ', Statement);
    SetNodeImages(SubNode, Statement);

    Statement := CreateTempStatement('double public_Var1', 'double', 'public_Var1', '', skVariable, scsPublic);
    SubNode := AddChildObject(Node, '  ' + Statement^._Command + '  ', Statement);
    SetNodeImages(SubNode, Statement);

    Statement := CreateTempStatement('bool published_Func1(char* temp)', 'bool', 'published_Func1', '(char* temp)', skFunction, scsPublished);
    SubNode := AddChildObject(Node, '  ' + Statement^._Command + '  ', Statement);
    SetNodeImages(SubNode, Statement);
    Expand(Node);

    Statement := CreateTempStatement('class Class2', 'class', 'Class2', '', skClass, scsNone);
    Node := AddChildObject(nil, '  ' + Statement^._Command + '  ', Statement);
    SetNodeImages(Node, Statement);

    Statement := CreateTempStatement('Class2()', '', 'Class2', '', skConstructor, scsPublic);
    SubNode := AddChildObject(Node, '  ' + Statement^._Command + '  ', Statement);
    SetNodeImages(SubNode, Statement);

    Statement := CreateTempStatement('~Class2()', '', '~Class2', '', skDestructor, scsPublic);
    SubNode := AddChildObject(Node, '  ' + Statement^._Command + '  ', Statement);
    SetNodeImages(SubNode, Statement);

    Statement := CreateTempStatement('int private_Var1', 'int', 'private_Var1', '', skVariable, scsPrivate);
    SubNode := AddChildObject(Node, '  ' + Statement^._Command + '  ', Statement);
    SetNodeImages(SubNode, Statement);

    Statement := CreateTempStatement('int private_Var2', 'int', 'private_Var2', '', skVariable, scsPrivate);
    SubNode := AddChildObject(Node, '  ' + Statement^._Command + '  ', Statement);
    SetNodeImages(SubNode, Statement);

    Statement := CreateTempStatement('void protected_Func1(int x, int y)', 'void', 'protected_Func1', '(int x, int y)', skFunction, scsProtected);
    SubNode := AddChildObject(Node, '  ' + Statement^._Command + '  ', Statement);
    SetNodeImages(SubNode, Statement);

    Statement := CreateTempStatement('double public_Var1', 'double', 'public_Var1', '', skVariable, scsPublic);
    SubNode := AddChildObject(Node, '  ' + Statement^._Command + '  ', Statement);
    SetNodeImages(SubNode, Statement);

    Statement := CreateTempStatement('bool published_Func1(char* temp)', 'bool', 'published_Func1', '(char* temp)', skFunction, scsPublished);
    SubNode := AddChildObject(Node, '  ' + Statement^._Command + '  ', Statement);
    SetNodeImages(SubNode, Statement);
    Expand(Node);
  end;
end;

function CustomSortProc(Node1, Node2: TTreeNode; Data: Integer): Integer; stdcall;
begin
  if (Node1.ImageIndex = 0) or (Node2.ImageIndex = 0) then
    Result := Node1.ImageIndex - Node2.ImageIndex
  else
    Result := Ord(PStatement(Node1.Data)^._Kind) - Ord(PStatement(Node2.Data)^._Kind);
  if Result = 0 then
    Result := AnsiStrIComp(PChar(Node1.Text), PChar(Node2.Text));
end;

procedure TClassBrowser.Sort;
begin
  CustomSort(@CustomSortProc, 0);
end;

procedure TClassBrowser.SetCurrentFile(Value: string);
begin
  if fShowFilter <> sfCurrent then
    Exit;
  fCurrentFile := Value;
  fCurrentFileHeader := ChangeFileExt(fCurrentFile, '.h');
  if not FileExists(fCurrentFileHeader) then
    fCurrentFileHeader := ChangeFileExt(fCurrentFile, '.hpp');
  if not FileExists(fCurrentFileHeader) then
    fCurrentFileHeader := ChangeFileExt(fCurrentFile, '.hh');
  fCurrentFileImpl := ChangeFileExt(fCurrentFile, '.c');
  if not FileExists(fCurrentFileImpl) then
    fCurrentFileImpl := ChangeFileExt(fCurrentFile, '.cpp');
  if not FileExists(fCurrentFileImpl) then
    fCurrentFileImpl := ChangeFileExt(fCurrentFile, '.cc');
  fCurrentFileHeader := LowerCase(fCurrentFileHeader);
  fCurrentFileImpl := LowerCase(fCurrentFileImpl);
  UpdateView;
end;

procedure TClassBrowser.Clear;
begin
  Items.Clear;
  SetLength(fFolders, 0);
  SetLength(fFolderAssocs, 0);
end;

procedure TClassBrowser.SetShowFilter(const Value: TShowFilter);
begin
  if fShowFilter <> Value then begin
    fShowFilter := Value;
    UpdateView;
  end;
end;

// returns the index to fFolders it belongs or -1 if does not

function TClassBrowser.BelongsToFolder(Cmd: string): integer;
var
  I: integer;
begin
  Result := -1;
  for I := Low(fFolderAssocs) to High(fFolderAssocs) do
    if AnsiCompareText(fFolderAssocs[I].Command, Cmd) = 0 then
      Result := fFolderAssocs[I].FolderID;
end;

// creates folders under Command

procedure TClassBrowser.CreateFolders(Cmd: string; Node: TTreeNode);
var
  I: integer;
begin
  for I := Low(fFolders) to High(fFolders) do
    if AnsiCompareText(fFolders[I].Under, Cmd) = 0 then begin
      fFolders[I].Node := Items.AddChildObjectFirst(Node, fFolders[I].Name, @fFolders[I]);
//      if AnsiCompareStr(fFolders[I].Under, #01#02+Char(I)) = 0 then
      CreateFolders(#01#02 + Char(I), fFolders[I].Node);
    end;
end;

function TClassBrowser.HasSubFolder(Cmd: string): boolean;
var
  I: integer;
begin
  Result := False;
  for I := Low(fFolders) to High(fFolders) do
    if AnsiCompareText(fFolders[I].Under, Cmd) = 0 then begin
      Result := True;
      Break;
    end;
end;

procedure TClassBrowser.ReadClassFolders;
var
  Magic: array[0..8] of Char;
  iNumEntries: integer;
  hFile: integer;
  I: integer;
begin
  if fProjectDir = '' then
    Exit;

  hFile := FileOpen(fProjectDir + '\' + fClassFoldersFile, fmOpenRead);
  if hFile <= 0 then
    Exit; // file not open

  FileRead(hFile, Magic, SizeOf(Magic));
  if Magic <> CLASS_FOLDERS_MAGIC then begin
    FileClose(hFile);
    Exit; // magic different
  end;

  // folders
  FileRead(hFile, iNumEntries, SizeOf(integer));
  SetLength(fFolders, iNumEntries);
  for I := Low(fFolders) to High(fFolders) do begin
    fFolders[I].Index := Char(I);
    FileRead(hFile, fFolders[I].Name, SizeOf(fFolders[I].Name));
    FileRead(hFile, fFolders[I].Under, SizeOf(fFolders[I].Under));
  end;

  // associations
  FileRead(hFile, iNumEntries, SizeOf(integer));
  SetLength(fFolderAssocs, iNumEntries);
  for I := Low(fFolderAssocs) to High(fFolderAssocs) do begin
    FileRead(hFile, fFolderAssocs[I].FolderID, SizeOf(fFolderAssocs[I].FolderID));
    fFolderAssocs[I].Folder := fFolders[fFolderAssocs[I].FolderID].Name;
    FileRead(hFile, fFolderAssocs[I].Command, SizeOf(fFolderAssocs[I].Command));
  end;

  FileClose(hFile);
end;

procedure TClassBrowser.WriteClassFolders;
var
  Magic: array[0..8] of Char;
  iNumEntries: integer;
  hFile: integer;
  I: integer;
begin
  if fProjectDir = '' then
    Exit;

  if High(fFolders) = -1 then begin
    DeleteFile(fProjectDir + '\' + fClassFoldersFile);
    Exit;
  end;

  hFile := FileCreate(fProjectDir + '\' + fClassFoldersFile);
  if hFile <= 0 then
    Exit; // file not open

  Magic := CLASS_FOLDERS_MAGIC;
  FileWrite(hFile, Magic, SizeOf(Magic));

  // folders
  iNumEntries := High(fFolders) + 1;
  FileWrite(hFile, iNumEntries, SizeOf(integer));
  for I := Low(fFolders) to High(fFolders) do begin
    FileWrite(hFile, fFolders[I].Name, SizeOf(fFolders[I].Name));
    FileWrite(hFile, fFolders[I].Under, SizeOf(fFolders[I].Under));
  end;

  // associations
  iNumEntries := High(fFolderAssocs) + 1;
  FileWrite(hFile, iNumEntries, SizeOf(integer));
  for I := Low(fFolderAssocs) to High(fFolderAssocs) do begin
    FileWrite(hFile, fFolderAssocs[I].FolderID, SizeOf(fFolderAssocs[I].FolderID));
    FileWrite(hFile, fFolderAssocs[I].Command, SizeOf(fFolderAssocs[I].Command));
  end;

  FileClose(hFile);
end;

procedure TClassBrowser.AddFolder(S: string; Node: TTreeNode);
begin
  if High(fFolders) >= MAX_CUSTOM_FOLDERS then
    Exit;

  if S = '' then
    Exit;

  if Length(S) > 32 then
    S := Copy(S, 1, 32);

  SetLength(fFolders, High(fFolders) + 2);
  fFolders[High(fFolders)].Index := Char(High(fFolders));
  fFolders[High(fFolders)].Name := S;
  if Assigned(Node) and (Node.ImageIndex <> fImagesRecord.fGlobalsImg) and not Node.HasChildren then
    Node := Node.Parent;
  if Assigned(Node) then begin
    if Node.ImageIndex <> fImagesRecord.fGlobalsImg then
      with PStatement(Node.Data)^ do
        fFolders[High(fFolders)].Under := ExtractFileName(_Filename) + ':' + IntToStr(_Line) + ':' + _FullText
    else
      fFolders[High(fFolders)].Under := #01#02 + Char(PFolders(Node.Data)^.Index);
  end;
  fFolders[High(fFolders)].Node := Items.AddChildObjectFirst(Node, fFolders[High(fFolders)].Name, @fFolders[High(fFolders)]);
  WriteClassFolders;
end;

procedure TClassBrowser.RemoveFolder(S: string);
var
  I: integer;
  C: integer;
begin
  for I := Low(fFolders) to High(fFolders) do
    if AnsiCompareText(fFolders[I].Name, S) = 0 then begin
      if Assigned(fFolders[I].Node) then begin
        while fFolders[I].Node.Count > 0 do
          fFolders[I].Node[0].MoveTo(fFolders[I].Node.Parent, naAddChild);
        fFolders[I].Node.Delete;
      end;
      RemoveFolderAssociation(fFolders[I].Name, '');
      for C := I + 1 to High(fFolders) do
        fFolders[C - 1] := fFolders[C];
      SetLength(fFolders, High(fFolders));
      Break;
    end;
  Items.BeginUpdate;
  Sort;
  Items.EndUpdate;
  WriteClassFolders;
  Refresh;
end;

procedure TClassBrowser.AddFolderAssociation(Fld, Cmd: string);
var
  Index: integer;
begin
  if (Fld = '') or (Cmd = '') then
    Exit;

  if Length(Fld) > 32 then
    Fld := Copy(Fld, 1, 32);
  if Length(Cmd) > 128 then
    Cmd := Copy(Cmd, 1, 128);

  Index := IndexOfFolder(Fld);
  if Index <> -1 then begin
    SetLength(fFolderAssocs, High(fFolderAssocs) + 2);
    fFolderAssocs[High(fFolderAssocs)].FolderID := Index;
    fFolderAssocs[High(fFolderAssocs)].Folder := Fld;
    fFolderAssocs[High(fFolderAssocs)].Command := Cmd;
  end;
end;

procedure TClassBrowser.RemoveFolderAssociation(Fld, Cmd: string);
var
  I: integer;
  C: integer;
  Index: integer;
begin
  Index := IndexOfFolder(Fld);
  if (Index <> -1) or (Fld = '') then begin
    I := Low(fFolderAssocs);
    while I <= High(fFolderAssocs) do
      if ((Fld = '') or (fFolderAssocs[I].FolderID = Index)) and
        ((Cmd = '') or (AnsiCompareText(fFolderAssocs[I].Command, Cmd) = 0)) then begin
        for C := I + 1 to High(fFolderAssocs) do
          fFolderAssocs[C - 1] := fFolderAssocs[C];
        SetLength(fFolderAssocs, High(fFolderAssocs));
      end
      else
        Inc(I);
  end;
end;

function TClassBrowser.GetNodeOfFolder(Index: integer): TTreeNode;
begin
  Result := nil;
  if Index <= High(fFolders) then
    Result := fFolders[Index].Node;
end;

function TClassBrowser.GetNodeOfFolder(Folder: string): TTreeNode;
var
  I: integer;
begin
  Result := nil;
  for I := Low(fFolders) to High(fFolders) do
    if AnsiCompareText(fFolders[I].Name, Folder) = 0 then begin
      Result := fFolders[I].Node;
      Break;
    end;
end;

procedure TClassBrowser.myDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  Node: TTreeNode;
begin
  if htOnItem in GetHitTestInfoAt(X, Y) then
    Node := GetNodeAt(X, Y)
  else
    Node := nil;

  // if drag node is a folder
  if Selected.ImageIndex = fImagesRecord.fGlobalsImg then begin
    if Assigned(Selected.Data) then
      if Assigned(Node) then begin
        if Selected.ImageIndex <> fImagesRecord.fGlobalsImg then
          with PStatement(Node.Data)^ do
            PFolders(Selected.Data)^.Under := ExtractFileName(_Filename) + ':' + IntToStr(_Line) + ':' + _FullText
        else
          PFolders(Selected.Data)^.Under := #01#02 + Char(PFolders(Node.Data)^.Index);
      end
      else
        PFolders(Selected.Data)^.Under := '';
  end
  // drag node is statement
  else with PStatement(Selected.Data)^ do begin // dragged node is Statement, so Node is folder
      RemoveFolderAssociation('', ExtractFileName(_Filename) + ':' + IntToStr(_Line) + ':' + _FullText);
      if Assigned(Node) then
        AddFolderAssociation(Node.Text, ExtractFileName(_Filename) + ':' + IntToStr(_Line) + ':' + _FullText);
    end;

  if Assigned(Selected) then
    Selected.MoveTo(Node, naAddChildFirst);

  Items.BeginUpdate;
  Sort;
  Items.EndUpdate;
  WriteClassFolders;
  Refresh;
end;

procedure TClassBrowser.myDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  Node: TTreeNode;
begin
  if htOnItem in GetHitTestInfoAt(X, Y) then
    Node := GetNodeAt(X, Y)
  else
    Node := nil;
  Accept := (Source is TClassBrowser) and
    (
    (
    // drag node is folder, drop node is not and drop node has children
    Assigned(Node) and (Selected.ImageIndex = fImagesRecord.fGlobalsImg) {and (Node.ImageIndex <> fImagesRecord.fGlobalsImg)} and Node.HasChildren
    ) or
    (
    // drag node is folder and drop node is folder
    Assigned(Node) and (Selected.ImageIndex = fImagesRecord.fGlobalsImg) and (Node.ImageIndex = fImagesRecord.fGlobalsImg)
    ) or
    (
    // drag node is not folder, drop node is folder
    Assigned(Node) and (Selected.ImageIndex <> fImagesRecord.fGlobalsImg) and (Node.ImageIndex = fImagesRecord.fGlobalsImg)
    ) or
    // not drop node
    not Assigned(Node)
    ) and
    (Node <> Selected);
end;

procedure TClassBrowser.myMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  Node: TTreeNode;
begin
  if fParserBusy then
    Exit;
  Node := GetNodeAt(X, Y);
  if Assigned(Node) and Assigned(Node.Data) and (Node.ImageIndex <> fImagesRecord.fGlobalsImg) then begin
    Hint := PStatement(Node.Data)^._FullText;
    Application.ActivateHint(ClientToScreen(Point(X, Y)));
  end
  else
    Application.HideHint;
end;

procedure TClassBrowser.RenameFolder(Old, New: string);
var
  I: integer;
  Index: integer;
begin
  Index := IndexOfFolder(Old);

  if Index <> -1 then begin
    fFolders[Index].Name := New;

    for I := Low(fFolderAssocs) to High(fFolderAssocs) do
      if fFolderAssocs[I].FolderID = Index then
        fFolderAssocs[I].Folder := New;

    fFolders[Index].Node.Text := New;
    WriteClassFolders;
    Refresh;
  end;
end;

function TClassBrowser.IndexOfFolder(Fld: string): integer;
var
  I: integer;
begin
  Result := -1;
  for I := Low(fFolders) to High(fFolders) do
    if AnsiCompareText(Fld, fFolders[I].Name) = 0 then begin
      Result := I;
      Break;
    end;
end;

procedure TClassBrowser.ReSelect;
  function DoSelect(Node: TTreeNode): boolean;
  var
    I: integer;
    OldSelection: string;
  begin
    Result := False;
    for I := 0 to Node.Count - 1 do begin
      if Node[I].ImageIndex <> fImagesRecord.fGlobalsImg then
        with PStatement(Node[I].Data)^ do
          OldSelection := ExtractFileName(_Filename) + ':' + IntToStr(_Line) + ':' + _FullText
      else
        OldSelection := PFolders(Node[I].Data)^.Under;
      if AnsiCompareStr(OldSelection, fLastSelection) = 0 then begin
        Selected := Node[I];
        Result := True;
        Break;
      end
      else
        if Node[I].HasChildren then begin
          Result := DoSelect(Node[I]);
          if Result then
            Break;
        end;
    end;
  end;
var
  I: integer;
  OldSelection: string;
begin
  for I := 0 to Items.Count - 1 do begin
    if Items[I].ImageIndex <> fImagesRecord.fGlobalsImg then
      with PStatement(Items[I].Data)^ do
        OldSelection := ExtractFileName(_Filename) + ':' + IntToStr(_Line) + ':' + _FullText
    else
      OldSelection := PFolders(Items[I].Data)^.Under;
    if AnsiCompareStr(OldSelection, fLastSelection) = 0 then begin
      Selected := Items[I];
      Break;
    end
    else
      if Items[I].HasChildren then
        if DoSelect(Items[I]) then
          Break;
  end;
end;

function TClassBrowser.FolderCount: integer;
begin
  Result := High(fFolders) + 1;
end;

procedure TClassBrowser.CustomPaintMe(var Msg: TMessage);
var
  I: integer;
  NodeRect, tmp: TRect;
  st: PStatement;
  bInherited: boolean;
  currItem: TTreeNode;
begin
  inherited;

  if fParserBusy then
    Exit;

  if not Assigned(fCnv) or not fUseColors then
    Exit;

  for I := 0 to Items.Count - 1 do begin
    currItem := Items[I];

    if currItem.IsVisible then begin
      NodeRect := currItem.DisplayRect(true);

      if currItem.ImageIndex <> fImagesRecord.fGlobalsImg then begin
        fCnv.Font.Color := Self.Font.Color;
        fCnv.Brush.Color := Color;
        fCnv.FillRect(NodeRect);
        st := currItem.Data;
        if Assigned(fParser) then
          if Assigned(st) then
            if (fParser.Statements.IndexOf(st) <> -1) then begin
              fCnv.Font.Style := [fsBold];
              if Selected = currItem then begin
                fCnv.Font.Color := clHighlightText;
                fCnv.Brush.Color := clHighlight;
                tmp := NodeRect;
                tmp.Right := tmp.Left + fCnv.TextWidth(st^._ScopelessCmd) + 4;
                fCnv.FillRect(tmp);
              end;

              bInherited := fShowInheritedMembers and
                Assigned(currItem.Parent) and
                (currItem.Parent.ImageIndex <> fImagesRecord.Globals) and
                Assigned(currItem.Parent.Data) and
                (fParser.Statements.IndexOf(currItem.Parent.Data) <> -1) and
                (PStatement(currItem.Parent.Data)^._ID <> st^._ParentID);

              if bInherited then
                fCnv.Font.Color := clGray;

              fCnv.TextOut(NodeRect.Left + 2, NodeRect.Top + 1, st^._ScopelessCmd);
              if bInherited then
                fCnv.Font.Color := clGray
              else
                fCnv.Font.Color := Self.Font.Color;
              fCnv.Brush.Color := Color;

              NodeRect.Left := NodeRect.Left + fCnv.TextWidth(st^._ScopelessCmd) + 2;
              fCnv.Font.Style := [];
              if bInherited then
                fCnv.Font.Color := clGray
              else
                fCnv.Font.Color := clMaroon;
              fCnv.TextOut(NodeRect.Left + 2, NodeRect.Top + 1, st^._Args);

              if st^._Type <> '' then begin
                fCnv.Font.Color := clGray;
                NodeRect.Left := NodeRect.Left + fCnv.TextWidth(st^._Args) + 2;
                fCnv.TextOut(NodeRect.Left + 2, NodeRect.Top + 1, ': ' + st^._Type);
              end
              else begin
                if st^._Kind in [skConstructor, skDestructor] then begin
                  fCnv.Font.Color := clGray;
                  NodeRect.Left := NodeRect.Left + fCnv.TextWidth(st^._Args) + 2;
                  fCnv.TextOut(NodeRect.Left + 2, NodeRect.Top + 1, ': ' + fParser.StatementKindStr(st^._Kind));
                end;
              end;
            end
            else
              currItem.Data := nil;
      end
      else begin
        fCnv.Font.Style := [fsBold];
        if Selected = currItem then begin
          fCnv.Font.Color := clHighlightText;
          fCnv.Brush.Color := clHighlight;
          tmp := NodeRect;
          tmp.Right := tmp.Left + fCnv.TextWidth(currItem.Text) + 4;
          fCnv.FillRect(tmp);
        end
        else begin
          fCnv.Font.Color := clNavy;
          fCnv.Brush.Color := Color;
        end;
        fCnv.FillRect(NodeRect);
        fCnv.TextOut(NodeRect.Left + 2, NodeRect.Top + 1, currItem.Text);
      end;
    end;
  end;
end;

procedure TClassBrowser.SetUseColors(const Value: boolean);
begin
  fUseColors := Value;
  if not fUseColors then begin
    OnMouseMove := myMouseMove;
    if Assigned(fCnv) then begin
      fCnv.Free;
      fCnv := nil;
    end;
  end
  else begin
    OnMouseMove := nil;
    if not Assigned(fCnv) then begin
      fCnv := TControlCanvas.Create;
      fCnv.Control := Self;
      fCnv.Font.Assign(Self.Font);
      fCnv.Brush.Style := bsClear;
    end;
  end;
end;

procedure TClassBrowser.SetShowInheritedMembers(const Value: boolean);
begin
  if Value <> fShowInheritedMembers then begin
    fShowInheritedMembers := Value;
    if not fParserBusy then
      UpdateView;
  end;
end;

end.

