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
  Windows, Messages, Classes, SysUtils, U_IntList, Controls, ComCtrls, Forms, Graphics,
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
    fMethodPrivateImg: integer;
    fMethodProtectedImg: integer;
    fMethodPublicImg: integer;
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
    property MethodPrivate: integer read fMethodPrivateImg write fMethodPrivateImg;
    property MethodProtected: integer read fMethodProtectedImg write fMethodProtectedImg;
    property MethodPublic: integer read fMethodPublicImg write fMethodPublicImg;
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
    fCurrentFile: AnsiString;
    fCurrentFileHeader: AnsiString;
    fCurrentFileImpl: AnsiString;
    fProjectDir: AnsiString;
    fClassFoldersFile: AnsiString;
    fFolders: array of TFolders;
    fFolderAssocs: array of TFolderAssocs;
    fLastSelection: AnsiString;
    fCnv: TControlCanvas;
    fParserBusy: boolean;
    fShowInheritedMembers: boolean;
    procedure SetParser(Value: TCppParser);
    procedure AddMembers(Node: TTreeNode; ParentIndex, ParentID: integer);
    procedure AdvancedCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages,
      DefaultDraw: Boolean);
    procedure OnNodeChange(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnNodeChanging(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure myDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure myDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure OnParserUpdate(Sender: TObject);
    procedure OnParserBusy(Sender: TObject);
    procedure SetNodeImages(Node: TTreeNode; Statement: PStatement);
    procedure Sort;
    procedure SetCurrentFile(const Value: AnsiString);
    procedure SetShowFilter(const Value: TShowFilter);
    procedure ReadClassFolders; // read folders from disk
    procedure WriteClassFolders; // write folders to disk
    function HasSubFolder(const Cmd: AnsiString): boolean; // if Command has subfolders, returns true
    procedure CreateFolders(const Cmd: AnsiString; Node: TTreeNode); // creates folders under Command
    function BelongsToFolder(const Cmd: AnsiString): integer; // returns the index to fFolders it belongs or -1 if does not
    function GetNodeOfFolder(Index: integer): TTreeNode; overload;
    function GetNodeOfFolder(const Folder: AnsiString): TTreeNode; overload;
    procedure AddFolderAssociation(Fld, Cmd: AnsiString);
    procedure RemoveFolderAssociation(Fld, Cmd: AnsiString);
    function IndexOfFolder(const Fld: AnsiString): integer;
    procedure ReSelect;
    procedure SetShowInheritedMembers(const Value: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateView;
    procedure Clear;
    procedure AddFolder(S: AnsiString; Node: TTreeNode);
    procedure RemoveFolder(S: AnsiString);
    procedure RenameFolder(Old, New: AnsiString);
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
    property BorderStyle;
    property MultiSelect;
    property MultiSelectStyle;
    property ShowFilter: TShowFilter read fShowFilter write SetShowFilter;
    property OnSelect: TMemberSelectEvent read fOnSelect write fOnSelect;
    property Parser: TCppParser read fParser write SetParser;
    property ItemImages: TImagesRecord read fImagesRecord write fImagesRecord;
    property CurrentFile: AnsiString read fCurrentFile write SetCurrentFile;
    property ProjectDir: AnsiString read fProjectDir write fProjectDir;
    property ClassFoldersFile: AnsiString read fClassFoldersFile write fClassFoldersFile;
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
        scsNone: Node.ImageIndex := fImagesRecord.VariablePublic;
      end;
    skFunction, skConstructor, skDestructor: case Statement^._ClassScope of
        scsPrivate: Node.ImageIndex := fImagesRecord.MethodPrivate;
        scsProtected: if not bInherited then Node.ImageIndex := fImagesRecord.MethodProtected else Node.ImageIndex := fImagesRecord.InheritedMethodProtected;
        scsPublic: if not bInherited then Node.ImageIndex := fImagesRecord.MethodPublic else Node.ImageIndex := fImagesRecord.InheritedMethodPublic;
        scsNone: Node.ImageIndex := fImagesRecord.MethodPublic;
      end;
  end;
  Node.SelectedIndex := Node.ImageIndex;
  Node.StateIndex := Node.ImageIndex;
end;

procedure TClassBrowser.AddMembers(Node: TTreeNode; ParentIndex, ParentID: integer);
var
  I, iFrom, tmp, donecount: integer;
  ParNode, NewNode: TTreeNode;
  bInherited: boolean;
  inheritanceids : TIntList;
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

	inheritanceids := TIntList.Create;
	try
		// allow inheritance propagation
		if fShowInheritedMembers and (ParentIndex <> -1) and (PStatement(fParser.Statements[ParentIndex])^._Kind = skClass) then begin

			// Add inheritance of current node
			fParser.AddInheritance(PStatement(fParser.Statements[ParentIndex])^._InheritsFromIDs,inheritanceids);
			donecount := 0; // done means found last base class

			// then process inheritance of new items
			while donecount < inheritanceids.Count do begin
				tmp := fParser.IndexOfStatement(inheritanceids[donecount]); // slooow
				fParser.AddInheritance(PStatement(fParser.Statements[tmp])^._InheritsFromIDs,inheritanceids);
				Inc(donecount);
			end;
		end;

		bInherited := False;
		for I := iFrom to fParser.Statements.Count - 1 do begin
			with PStatement(fParser.Statements[I])^ do begin
				if not _Visible then
					Continue;
				if _ParentID <> ParentID then begin
					bInherited := fShowInheritedMembers and (inheritanceids.IndexOf(_ParentID) <> -1);
					if not bInherited then
						Continue;
				end;

				if (fShowFilter = sfAll) or
				((fShowFilter = sfProject) and (_InProject or bInherited)) or
				((fShowFilter = sfCurrent) and (SameText(_Filename,fCurrentFileHeader) or SameText(_Filename,fCurrentFileImpl) or bInherited)) then begin

					// check if belongs to folder
					tmp := BelongsToFolder(ExtractFileName(_Filename) + ':' + IntToStr(_Line) + ':' + _FullText);
					if tmp <> -1 then
						ParNode := GetNodeOfFolder(tmp)
					else
						ParNode := Node;

					NewNode := Items.AddChildObject(ParNode, _ScopelessCmd, PStatement(fParser.Statements[I]));
					SetNodeImages(NewNode, PStatement(fParser.Statements[I]));
					if (PStatement(fParser.Statements[I])^._Kind = skClass) and (I <> ParentIndex) then  // CL: fixed potential infinite loop bug
						AddMembers(NewNode, I, _ID);
				end;
			end;
		end;
	finally
		inheritanceids.Free;
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
  fCnv := TControlCanvas.Create;
  fCnv.Control := Self;
  fCnv.Font.Assign(Self.Font);
  OnAdvancedCustomDrawItem := AdvancedCustomDrawItem;
end;

destructor TClassBrowser.Destroy;
begin
  SetLength(fFolderAssocs, 0);
  SetLength(fFolders, 0);
  FreeAndNil(fImagesRecord);
  FreeAndNil(fCnv);
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

function CustomSortProc(Node1, Node2: TTreeNode; Data: Integer): Integer; stdcall;
begin
  if (Node1.ImageIndex = 0) or (Node2.ImageIndex = 0) then
    Result := Node1.ImageIndex - Node2.ImageIndex
  else
    Result := Ord(PStatement(Node1.Data)^._Kind) - Ord(PStatement(Node2.Data)^._Kind);
  if Result = 0 then
    Result := StrIComp(PAnsiChar(Node1.Text), PAnsiChar(Node2.Text));
end;

procedure TClassBrowser.Sort;
begin
  CustomSort(@CustomSortProc, 0);
end;

procedure TClassBrowser.SetCurrentFile(const Value: AnsiString);
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

function TClassBrowser.BelongsToFolder(const Cmd: AnsiString): integer;
var
	I: integer;
begin
	Result := -1;
	for I := Low(fFolderAssocs) to High(fFolderAssocs) do
		if CompareText(fFolderAssocs[I].Command, Cmd) = 0 then begin
			Result := fFolderAssocs[I].FolderID;
			break;
		end;
end;

// creates folders under Command

procedure TClassBrowser.CreateFolders(const Cmd: AnsiString; Node: TTreeNode);
var
	I: integer;
begin
	for I := Low(fFolders) to High(fFolders) do
		if CompareText(fFolders[I].Under, Cmd) = 0 then begin
			fFolders[I].Node := Items.AddChildObjectFirst(Node, fFolders[I].Name, @fFolders[I]);
			CreateFolders(#01#02 + Char(I), fFolders[I].Node);
		end;
end;

function TClassBrowser.HasSubFolder(const Cmd: AnsiString): boolean;
var
	I: integer;
begin
	Result := False;
	for I := Low(fFolders) to High(fFolders) do
		if CompareText(fFolders[I].Under, Cmd) = 0 then begin
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

procedure TClassBrowser.AddFolder(S: AnsiString; Node: TTreeNode);
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

procedure TClassBrowser.RemoveFolder(S: AnsiString);
var
  I: integer;
  C: integer;
begin
  for I := Low(fFolders) to High(fFolders) do
    if CompareText(fFolders[I].Name, S) = 0 then begin
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

procedure TClassBrowser.AddFolderAssociation(Fld, Cmd: AnsiString);
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

procedure TClassBrowser.RemoveFolderAssociation(Fld, Cmd: AnsiString);
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
        ((Cmd = '') or (CompareText(fFolderAssocs[I].Command, Cmd) = 0)) then begin
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

function TClassBrowser.GetNodeOfFolder(const Folder: AnsiString): TTreeNode;
var
  I: integer;
begin
  Result := nil;
  for I := Low(fFolders) to High(fFolders) do
    if CompareText(fFolders[I].Name, Folder) = 0 then begin
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

procedure TClassBrowser.RenameFolder(Old, New: AnsiString);
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

function TClassBrowser.IndexOfFolder(const Fld: AnsiString): integer;
var
  I: integer;
begin
  Result := -1;
  for I := Low(fFolders) to High(fFolders) do
    if CompareText(Fld, fFolders[I].Name) = 0 then begin
      Result := I;
      Break;
    end;
end;

procedure TClassBrowser.ReSelect;
  function DoSelect(Node: TTreeNode): boolean;
  var
    I: integer;
    OldSelection: AnsiString;
  begin
    Result := False;
    for I := 0 to Node.Count - 1 do begin
      if Node[I].ImageIndex <> fImagesRecord.fGlobalsImg then
        with PStatement(Node[I].Data)^ do
          OldSelection := ExtractFileName(_Filename) + ':' + IntToStr(_Line) + ':' + _FullText
      else
        OldSelection := PFolders(Node[I].Data)^.Under;
      if CompareStr(OldSelection, fLastSelection) = 0 then begin
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
  OldSelection: AnsiString;
begin
  for I := 0 to Items.Count - 1 do begin
    if Items[I].ImageIndex <> fImagesRecord.fGlobalsImg then
      with PStatement(Items[I].Data)^ do
        OldSelection := ExtractFileName(_Filename) + ':' + IntToStr(_Line) + ':' + _FullText
    else
      OldSelection := PFolders(Items[I].Data)^.Under;
    if CompareStr(OldSelection, fLastSelection) = 0 then begin
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

procedure TClassBrowser.AdvancedCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages,DefaultDraw: Boolean);
var
	NodeRect: TRect;
	st: PStatement;
	bInherited: boolean;
	typetext : AnsiString;
begin
	if fParserBusy then
		Exit;

	// Assume the node image is correct
	bInherited := fShowInheritedMembers and (Node.ImageIndex in [
		fImagesRecord.fInhMethodProtectedImg,
		fImagesRecord.fInhMethodPublicImg,
		fImagesRecord.fInhVariableProtectedImg,
		fImagesRecord.fInhVariablePublicImg]);

	if Stage = cdPrePaint then begin
		Sender.Canvas.Font.Style := [fsBold];
		if bInherited then
			Sender.Canvas.Font.Color := clGray;
	end else if Stage = cdPostPaint then begin
		st := Node.Data;
		if Assigned(st) then begin // useless check really
			if bInherited then
				fCnv.Font.Color := clGray
			else
				fCnv.Font.Color := clMaroon;

			// draw function arguments to the right of the already drawn text
			NodeRect := Node.DisplayRect(true);
			NodeRect.Left := NodeRect.Left + Sender.Canvas.TextWidth(st^._ScopelessCmd) + 2;
			fCnv.TextOut(NodeRect.Left + 2, NodeRect.Top + 2, st^._Args);

			fCnv.Font.Color := clGray;
			if st^._Type <> '' then
				typetext := st^._Type
			else if st^._Kind in [skConstructor, skDestructor] then
				typetext := fParser.StatementKindStr(st^._Kind)
			else
				Exit; // done

			// Then draw node type to the right of the arguments
			NodeRect.Left := NodeRect.Left + fCnv.TextWidth(st^._Args) + 2;
			fCnv.TextOut(NodeRect.Left + 2, NodeRect.Top + 2, ': ' + typetext);
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
