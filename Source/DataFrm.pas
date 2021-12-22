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

unit DataFrm;

interface

uses
  SysUtils, Classes, Menus, Controls, SynEditHighlighter, SynHighlighterCpp,
  CodeInsList, SynHighlighterRC, ImgList, System.ImageList,
  Vcl.BaseImageCollection, Vcl.ImageCollection, Vcl.VirtualImageList,
  SVGIconImageListBase, SVGIconImageList;

type
  PMRUItem = ^TMRUItem;
  TMRUItem = record
    FileName: String;
    MenuItem: TMenuItem;
    Visible: boolean; // not all items in the list are shown
  end;

  TdmMain = class(TDataModule)
    Cpp: TSynCppSyn;
    ProjectImage_Gnome: TImageList;
    MenuImages_Gnome: TImageList;
    Res: TSynRCSyn;
    MenuImages_NewLook: TImageList;
    ProjectImage_NewLook: TImageList;
    GutterImages: TImageList;
    MenuImages_Blue: TImageList;
    ProjectImage_Blue: TImageList;
    ClassImages: TImageList;
    ImageThemeColection: TImageCollection;
    SVGImageListMenuStyle: TSVGIconImageList;
    SVGImageListProjectStyle: TSVGIconImageList;
    SVGImageListClassStyle: TSVGIconImageList;
    SVGImageListMessageStyle: TSVGIconImageList;
    SVGIconImageWelcomeScreen: TSVGIconImageList;
    EMBTImageCollection: TImageCollection;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    fUnitCount: integer;
    fProjectCount: integer;
    fCodeList: TCodeInsList;
    fCodeMenu: TMenuItem;
    fCodePop: TMenuItem;
    fCodeEvent: TNotifyEvent;
    fCodeOffset: byte;
    procedure LoadCodeIns;
  public
    property CodeMenu: TMenuItem read fCodeMenu write fCodeMenu;
    property CodePop: TMenuItem read fCodePop write fCodePop;
    property CodeClick: TNotifyEvent read fCodeEvent write fCodeEvent;
    property CodeInserts: TCodeInsList read fCodeList write fCodeList;
    property CodeOffset: byte read fCodeOffset write fCodeOffset;

    { MRU List }
  private
    fMRU: TList; // let them store their own location
    fMRUMenu: TMenuItem;
    fMRUMenuParent: TMenuItem;
    fMRUMenuStartIndex: integer;
    fMRUTopSep: TMenuItem;
    fMRUMiddleSep: TMenuItem;
    fMRUBottomSep: TMenuItem;
    fMRUClick: TNotifyEvent;
    procedure FilterHistory; // remove deleted
    procedure LoadHistory;
    procedure SaveHistory;
    procedure SetMRUMenu(value: TMenuItem);
  public
    procedure RebuildMRU;
    procedure AddtoHistory(const s: String);
    procedure RemoveFromHistory(const s: String);
    procedure ClearHistory;
    property MRUMenu: TMenuItem read fMRUMenu write SetMRUMenu;
    property MRUClick: TNotifyEvent read fMRUClick write fMRUClick;
    property MRU: TList read fMRU;
  public
    procedure LoadDataMod;
    function GetNewProjectNumber: integer;
    function GetNewFileNumber: integer;
    procedure InitHighlighterFirstTime(index: integer);
    procedure UpdateHighlighter;
    function GetHighlighter(const FileName: String): TSynCustomHighlighter;
  end;

var
  dmMain: TdmMain;

const
  cDelphiStyle: array[0..8] of string = ('Windows','Windows10','Windows10 SlateGray','Windows10 Blue Whale','Windows10 BlackPearl','Glossy','Calypso','Flat UI Light','Material Patterns Blue');
  cSVGColor: array[0..8] of string = ('None','clGrayText','clMoneyGreen','clSkyBlue','clGrayText','clGrayText','clGrayText','clGrayText','clGrayText');
implementation

uses
  devcfg, utils, version, math, MultiLangSupport;

{$R *.dfm}

{ TdmMain }

procedure TdmMain.DataModuleCreate(Sender: TObject);
begin
  fMRU := TList.Create;
  fCodeList := TCodeInsList.Create;
end;

procedure TdmMain.DataModuleDestroy(Sender: TObject);
var
  I: integer;
begin
  SaveHistory;
  for I := 0 to fMRU.Count - 1 do
    Dispose(PMRUItem(fMRU[i]));
  fMRU.Free;
  fCodeList.Free;
end;

procedure TdmMain.InitHighlighterFirstTime(index: integer);
  procedure AddSpecial(AttrName: String; Offset: integer);
  var
    a: integer;
  begin
    a := devEditor.Syntax.IndexofName(AttrName);
    if a = -1 then
      devEditor.Syntax.Append(format('%s=%s', [AttrName, LoadStr(offset)]))
    else
      devEditor.Syntax.Values[AttrName] := LoadStr(offset);
  end;
var
  i, a, offset: integer;
  Attr: TSynHighlighterAttributes;
begin
  offset := index * 1000;
  for i := 0 to pred(cpp.AttrCount) do begin
    attr := TSynHighlighterAttributes.Create(cpp.Attribute[i].Name,cpp.Attribute[i].Name);
    try
      StrtoAttr(Attr, LoadStr(i + offset + 1));
      cpp.Attribute[i].Assign(Attr);
      a := devEditor.Syntax.IndexOfName(cpp.Attribute[i].Name);
      if a = -1 then
        devEditor.Syntax.Append(format('%s=%s', [cpp.Attribute[i].Name, AttrtoStr(Attr)]))
      else
        devEditor.Syntax.Values[cpp.Attribute[i].Name] := AttrtoStr(Attr);
    finally
      Attr.Free;
    end;
  end;
  AddSpecial(cBP, offset + 17); // breakpoint
  AddSpecial(cErr, offset + 18); // error line
  AddSpecial(cABP, offset + 19); // active breakpoint
  AddSpecial(cGut, offset + 20); // gutter
  AddSpecial(cSel, offset + 21); // selected text
  AddSpecial(cFld, offset + 22); // fold bar lines
end;

procedure TdmMain.UpdateHighlighter;
var
  Attr: TSynHighlighterAttributes;
  aName: String;
  a,
    idx: integer;
begin
  for idx := 0 to pred(cpp.AttrCount) do begin
    aName := cpp.Attribute[idx].Name;
    a := devEditor.Syntax.IndexOfName(aName);
    if a <> -1 then begin
      Attr := TSynHighlighterAttributes.Create(aName,aName);
      try
        StrtoAttr(Attr, devEditor.Syntax.Values[aname]);
        cpp.Attribute[idx].Assign(attr);
      finally
        Attr.Free;
      end;
    end;
  end;
  // update res highlighter
  with Res do begin
    CommentAttri.Assign(cpp.CommentAttri);
    DirecAttri.Assign(cpp.DirecAttri);
    IdentifierAttri.Assign(cpp.IdentifierAttri);
    KeyAttri.Assign(cpp.KeyAttri);
    NumberAttri.Assign(cpp.NumberAttri);
    SpaceAttri.Assign(cpp.SpaceAttri);
    StringAttri.Assign(cpp.StringAttri);
    SymbolAttri.Assign(cpp.SymbolAttri);
  end;
end;

function TdmMain.GetHighlighter(const FileName: String): TSynCustomHighlighter;
var
  ext: String;
  idx: integer;
  tmp: TStrings;
begin
  UpdateHighlighter;
  result := nil;
  if devEditor.UseSyntax then begin
    if (FileName = '') or (Pos(Lang[ID_UNTITLED], FileName) = 1) then
      result := cpp
    else begin
      ext := ExtractFileExt(FileName);
      if CompareText(ext, RC_EXT) = 0 then
        result := Res
      else begin
        tmp := TStringList.Create;
        try
          delete(ext, 1, 1);
          tmp.Delimiter := ';';
          tmp.DelimitedText := devEditor.SyntaxExt;
          if tmp.Count > 0 then
            for idx := 0 to pred(tmp.Count) do
              if CompareText(Ext, tmp[idx]) = 0 then begin
                result := cpp;
                Exit;
              end;
        finally
          tmp.Free;
        end;
      end;
    end;
  end;
end;

function TdmMain.GetNewFileNumber: integer;
begin
  Inc(fUnitCount);
  result := fUnitCount;
end;

function TdmMain.GetNewProjectNumber: integer;
begin
  Inc(fProjectCount);
  result := fProjectCount;
end;

procedure TdmMain.LoadDataMod;
begin
  LoadHistory;
  LoadCodeIns;
  UpdateHighlighter;
end;

{ ---------- MRU ---------- }

procedure TdmMain.SetMRUMenu(value: TMenuItem);
begin
  fMRUMenu := value;
  fMRUMenuParent := fMRUMenu.Parent;
  if Assigned(fMRUMenuParent) then begin
    // Assume there are three separators above this item
    fMRUMenuStartIndex := fMRUMenu.MenuIndex - 2;
    fMRUTopSep := fMRUMenuParent[fMRUMenuStartIndex - 1];
    fMRUMiddleSep := fMRUMenuParent[fMRUMenuStartIndex];
    fMRUBottomSep := fMRUMenuParent[fMRUMenuStartIndex + 1];
  end;
end;

procedure TdmMain.AddtoHistory(const s: String);
var
  I: integer;
  newitem: PMRUItem;
begin
  if (s = '') or not FileExists(s) then
    exit;

  // Don't add duplicates!
  for I := 0 to fMRU.Count - 1 do
    if SameText(s, PMRUItem(fMRU[i])^.filename) then
      Exit;

  newitem := new(PMRUItem);
  newitem^.FileName := s;
  newitem^.MenuItem := nil; // to be filled by RebuildMRU
  newitem^.Visible := false; // idem

  if GetFileTyp(s) = utPrj then begin
    fMRU.Insert(0, newitem); // insert first
  end else begin // find last project
    I := 0;
    while (i < fMRU.Count) and (GetFileTyp(PMRUItem(fMRU[I])^.filename) = utPrj) do
      Inc(I);
    fMRU.Insert(I, newitem); // insert after last project
  end;

  RebuildMRU;
end;

procedure TdmMain.RemoveFromHistory(const s: String);
var
  I: integer;
begin
  // Remove one, duplicates simply aren't present
  for I := 0 to fMRU.Count - 1 do
    if SameText(s, PMRUItem(fMRU[i])^.filename) then begin

      // remove menu item now
      PMRUItem(fMRU[i])^.MenuItem.Free;

      // delete pointed memory
      Dispose(PMRUItem(fMRU[i]));
      fMRU.Delete(i);

      // Rebuild whole list
      RebuildMRU;
      break;
    end;
end;

procedure TdmMain.ClearHistory;
var
  I: integer;
begin
  for I := 0 to fMRU.Count - 1 do begin

    // remove menu item now
    PMRUItem(fMRU[i])^.MenuItem.Free;

    // delete pointed memory
    Dispose(PMRUItem(fMRU[i]));
  end;
  fMRU.Clear;
  RebuildMRU;
end;

procedure TdmMain.FilterHistory; // remove deleted files
var
  I: integer;
begin
  for I := MRU.Count - 1 downto 0 do
    if not FileExists(PMRUItem(fMRU[i])^.filename) then begin
      Dispose(PMRUItem(fMRU[i]));
      fMRU.Delete(i);
    end;
end;

procedure TdmMain.LoadHistory;
var
  I, J: integer;
  sl: TStringList;
  newitem: PMRUItem;
begin
  sl := TStringList.Create;
  try
    // Use already open file handle
    devData.ReadStrings('History', sl);

    // Delete files that don't exist anymore
    for I := sl.Count - 1 downto 0 do
      if not FileExists(sl.ValueFromIndex[I]) then
        sl.Delete(I);

    // Remove duplicates
    for I := 0 to sl.Count - 1 do begin
      J := I + 1;
      while J < sl.Count do begin
        if (sl.ValueFromIndex[i] = sl.ValueFromIndex[j]) then
          sl.Delete(j)
        else
          Inc(j);
      end;
    end;

    // Create struct list
    for I := 0 to sl.Count - 1 do begin
      newitem := new(PMRUItem);
      newitem^.FileName := sl.ValueFromIndex[i];
      newitem^.MenuItem := nil; // to be filled by RebuildMRU
      newitem^.Visible := false;
      fMRU.Add(newitem);
    end;
  finally
    sl.Free;
  end;

  RebuildMRU; // rebuild once
end;

procedure TdmMain.SaveHistory;
var
  I: integer;
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    // Read struct list
    for I := 0 to fMRU.Count - 1 do
      sl.Add(PMRUItem(fMRU[i])^.filename);

    // Use already open file handle
    devData.WriteStrings('History', sl);
  finally
    sl.Free;
  end;
end;

procedure TdmMain.RebuildMRU;
var
  i, AllCount, ProjCount, FileCount: integer;
  Item: TMenuItem;
begin
  // Delete all menu items
  for I := 0 to fMRU.Count - 1 do begin
    FreeAndNil(PMRUItem(fMRU[i])^.MenuItem);
    PMRUItem(fMRU[i])^.visible := false;
  end;

  // Remove deleted files
  FilterHistory;

  // Add menu items up to MRUmax
  AllCount := 0;
  ProjCount := 0;
  FileCount := 0; // other files

  // First add projects
  for I := 0 to min(devData.MRUMax, fMRU.Count) - 1 do begin
    if GetFileTyp(PMRUItem(fMRU[I])^.filename) = utPrj then begin

      // Add item to main menu
      Item := TMenuItem.Create(fMRUMenuParent);
      Item.Caption := Format('&%1x %s', [AllCount, PMRUItem(fMRU[I])^.filename]);
      Item.OnClick := fMRUClick;
      Item.Tag := I;
      fMRUMenuParent.Insert(fMRUMenuStartIndex + AllCount, Item);

      // Hand a pointer to the MRU item, so it can remove it itself
      PMRUItem(fMRU[I])^.MenuItem := Item;
      PMRUItem(fMRU[I])^.Visible := true;

      // Keep count...
      Inc(AllCount);
      Inc(ProjCount);
      if AllCount = devData.MRUMax then
        break;
    end;
  end;

  // Then add other stuff
  if AllCount <> devData.MRUMax then begin
    for I := 0 to min(devData.MRUMax, fMRU.Count) - 1 do begin
      if GetFileTyp(PMRUItem(fMRU[I])^.filename) <> utPrj then begin

        // Add item to main menu
        Item := TMenuItem.Create(fMRUMenuParent);
        Item.Caption := Format('&%1x %s', [AllCount, PMRUItem(fMRU[I])^.filename]);
        Item.OnClick := fMRUClick;
        Item.Tag := I;
        fMRUMenuParent.Insert(fMRUMenuStartIndex + AllCount + 1, Item); // add AFTER middle separator

        // Hand a pointer to the MRU item, so it can remove it itself
        PMRUItem(fMRU[I])^.MenuItem := Item;
        PMRUItem(fMRU[I])^.Visible := true;

        // Keep count...
        Inc(AllCount);
        Inc(FileCount);
        if AllCount = devData.MRUMax then
          break;
      end;
    end;
  end;

  // Hide unneeded separators and clear history button
  fMRUTopSep.Visible := (AllCount > 0);
  fMRUMiddleSep.Visible := (FileCount > 0) and (ProjCount > 0);
  fMRUBottomSep.Visible := (AllCount > 0);
  fMRUMenu.Visible := (AllCount > 0);

  // Remove invisible items...
  for I := fMRU.Count - 1 downto 0 do begin
    if not PMRUItem(fMRU[I])^.visible then begin
      Dispose(PMRUItem(fMRU[I]));
      fMRU.Delete(I);
    end;
  end;
end;

{ ---------- Code Insert Methods ---------- }

// Loads code inserts, when sep value changes a separator is
// insert only if sep is a higher value then previous sep value.

procedure TdmMain.LoadCodeIns;
var
  cdx,
    idx: integer;
  Item: TMenuItem;
begin
  if not assigned(fCodeMenu) then
    exit;
  fCodeList.LoadCode;

  for idx := pred(fCodeMenu.Count) downto fCodeOffset do
    fCodeMenu[idx].Free;

  if assigned(fCodePop) then
    fCodePop.Clear;

  cdx := 0;
  for idx := 0 to pred(fCodeList.Count) do begin
    Item := TMenuItem.Create(fCodeMenu);
    Item.Caption := fCodeList[idx]^.Caption;
    Item.OnClick := fCodeEvent;
    Item.Tag := idx;
    if fCodeList[idx]^.Sep <= cdx then
      fCodeMenu.Add(Item)
    else begin
      cdx := fCodeList[idx]^.Sep;
      fCodeMenu.NewBottomLine;
      fCodeMenu.Add(Item);
    end;
  end;
  fCodeMenu.Visible := fCodeMenu.Count > 0;
  if assigned(fCodePop) then
    CloneMenu(fCodeMenu, fCodePop);
end;

end.

