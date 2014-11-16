{
Mystix Text Editor
Copyright (C) 2005 Piotr Jura

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

You can contact with me by e-mail: pjura@o2.pl
}
unit SynEditCodeFolding;

interface

uses
  Graphics, Types, Classes, SysUtils, SynEditHighlighter;

type
  TSynEditFoldRange = class;
  TFoldRegions = class;

  TSynCodeFolding = class
  private
    fIndentGuides: Boolean;
    fShowCollapsedLine: Boolean;
    fCollapsedLineColor: TColor;
    fFolderBarLinesColor: TColor;
    fIndentGuidesColor: TColor;
    fFoldRegions: TFoldRegions;
  public
    constructor Create;
    destructor Destroy; override;

    property CollapsedLineColor: TColor read fCollapsedLineColor write fCollapsedLineColor;
    property FolderBarLinesColor: TColor read fFolderBarLinesColor write fFolderBarLinesColor;
    property IndentGuidesColor: TColor read fIndentGuidesColor write fIndentGuidesColor;
    property IndentGuides: Boolean read fIndentGuides write fIndentGuides;
    property ShowCollapsedLine: Boolean read fShowCollapsedLine write fShowCollapsedLine;
    property FoldRegions: TFoldRegions read fFoldRegions write fFoldRegions;
  end;

  TFoldRegionItem = class(TCollectionItem)
  private
    fAddEnding: Boolean;
    fSubFoldRegions: TFoldRegions;
    fOpen: Char;
    fClose: Char;
    fHighlight: AnsiString;
    fOpenLength: Integer;
    fCloseLength: Integer;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property AddEnding: Boolean read fAddEnding write fAddEnding;
    property SubFoldRegions: TFoldRegions read fSubFoldRegions;
    property Open: Char read fOpen write fOpen;
    property Close: Char read fClose write fClose;
    property OpenLength: Integer read fOpenLength;
    property CloseLength: Integer read fCloseLength;
    property Highlight: AnsiString read fHighlight write fHighlight;
  end;

  TFoldRegions = class(TCollection)
  private
    function GetItem(Index: Integer): TFoldRegionItem;
  public
    constructor Create(ItemClass: TCollectionItemClass);
    destructor Destroy; override;
    function Add(AAddEnding: boolean; AOpen, AClose: Char; AHighlight: AnsiString): TFoldRegionItem;

    property Items[Index: Integer]: TFoldRegionItem read GetItem; default;
  end;

  // A parent fold which owns fold ranges (branch)
  TSynEditFoldRanges = class(TObject)
  private
    fRanges: TList;
    fOwnsObjects: boolean;
    function Get(Index: Integer): TSynEditFoldRange;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function AddByParts(AParent: TSynEditFoldRange; AAllFold: TSynEditFoldRanges; AFromLine, AIndent: Integer;
      AFoldRegion: TFoldRegionItem; AToLine: Integer = 0): TSynEditFoldRange;
    procedure AddObject(FoldRange: TSynEditFoldRange);

    procedure Delete(Index: Integer);

    property OwnsObjects: boolean read fOwnsObjects write fOwnsObjects;
    property Count: Integer read GetCount;
    property FoldRanges[Index: Integer]: TSynEditFoldRange read Get; default;
    property Ranges: TList read fRanges;
  end;

  // A single fold
  TSynEditFoldRange = class(TObject)
  private
    fFromLine: Integer; // Beginning line
    fToLine: Integer; // End line
    fIndent: Integer; // Indent level (physcial)
    fLinesCollapsed: Integer; // Number of collapsed lines
    fSubFoldRanges: TSynEditFoldRanges; // Sub fold ranges
    fCollapsed: Boolean; // Is collapsed?
    fAllFoldRanges: TSynEditFoldRanges; // TAllFoldRanges pointer
    fFoldRegion: TFoldRegionItem; // FoldRegion pointer
    fHintMarkLeft: Integer;
    fParent: TSynEditFoldRange;
    function GetParentCollapsed: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    property AllFolds: TSynEditFoldRanges read fAllFoldRanges;
    property SubFoldRanges: TSynEditFoldRanges read fSubFoldRanges;
    property FromLine: Integer read fFromLine write fFromLine;
    property ToLine: Integer read fToLine write fToLine;
    property Indent: Integer read fIndent write fIndent;
    property LinesCollapsed: Integer read fLinesCollapsed write fLinesCollapsed;
    property Collapsed: Boolean read fCollapsed write fCollapsed;
    property ParentCollapsed: Boolean read GetParentCollapsed;
    property FoldRegion: TFoldRegionItem read fFoldRegion write fFoldRegion;
    property HintMarkLeft: Integer read fHintMarkLeft write fHintMarkLeft;
    property Parent: TSynEditFoldRange read fParent write fParent;
    procedure Move(Count: Integer);
  end;

implementation

{ TSynEditFoldRanges }

constructor TSynEditFoldRanges.Create;
begin
  inherited;
  fRanges := TList.Create;
end;

destructor TSynEditFoldRanges.Destroy;
var
  I: integer;
begin
  if OwnsObjects then
    for I := 0 to fRanges.Count - 1 do begin
      TObject(fRanges[i]).Free;
      fRanges[i] := nil;
    end;
  fRanges.Free;
  inherited;
end;

function TSynEditFoldRanges.AddByParts(AParent: TSynEditFoldRange; AAllFold: TSynEditFoldRanges; AFromLine, AIndent:
  Integer; AFoldRegion: TFoldRegionItem; AToLine: Integer): TSynEditFoldRange;
begin
  Result := TSynEditFoldRange.Create;
  with Result do begin
    fParent := AParent;
    fFromLine := AFromLine;
    fToLine := AToLine;
    fIndent := AIndent;
    fAllFoldRanges := AAllFold;
    fFoldRegion := AFoldRegion;
  end;

  // Add pointers to our parent
  fRanges.Add(Result);

  // Don't add double pointers to the parent-of-all
  if Self <> AAllFold then
    AAllFold.fRanges.Add(Result);
end;

procedure TSynEditFoldRanges.AddObject(FoldRange: TSynEditFoldRange);
begin
  fRanges.Add(FoldRange);
end;

procedure TSynEditFoldRanges.Delete(Index: Integer);
begin
  if OwnsObjects then
    TObject(fRanges[Index]).Free;
  fRanges.Delete(Index);
end;

function TSynEditFoldRanges.Get(Index: Integer): TSynEditFoldRange;
begin
  Result := TSynEditFoldRange(fRanges[Index]);
end;

function TSynEditFoldRanges.GetCount: Integer;
begin
  Result := fRanges.Count;
end;

{ TSynEditFoldRange }

constructor TSynEditFoldRange.Create;
begin
  inherited;
  fSubFoldRanges := TSynEditFoldRanges.Create;
  fSubFoldRanges.OwnsObjects := false;
end;

destructor TSynEditFoldRange.Destroy;
begin
  fSubFoldRanges.Free;
  inherited;
end;

function TSynEditFoldRange.GetParentCollapsed: Boolean;
var
  ParentFold: TSynEditFoldRange;
begin
  // Find first parent that is collapsed
  ParentFold := fParent;
  while Assigned(ParentFold) do begin
    if ParentFold.Collapsed then begin
      Result := True;
      Exit;
    end;
    ParentFold := ParentFold.Parent;
  end;
  Result := False;
end;

procedure TSynEditFoldRange.Move(Count: Integer);
begin
  Inc(fFromLine, Count);
  Inc(fToLine, Count);
end;

{ TFoldRegion }

constructor TFoldRegionItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  fSubFoldRegions := TFoldRegions.Create(TFoldRegionItem);
end;

destructor TFoldRegionItem.Destroy;
begin
  fSubFoldRegions.Free;
  inherited;
end;

{ TFoldRegions }

function TFoldRegions.Add(AAddEnding: boolean; AOpen, AClose: Char; AHighlight: AnsiString): TFoldRegionItem;
begin
  Result := TFoldRegionItem(inherited Add);
  with Result do begin
    fAddEnding := AAddEnding;
    Open := AOPen;
    Close := AClose;
    fHighlight := AHighlight;
  end;
end;

constructor TFoldRegions.Create(ItemClass: TCollectionItemClass);
begin
  inherited Create(ItemClass);
end;

destructor TFoldRegions.Destroy;
begin
  inherited;
end;

function TFoldRegions.GetItem(Index: Integer): TFoldRegionItem;
begin
  Result := TFoldRegionItem(inherited Items[Index]);
end;

constructor TSynCodeFolding.Create;
begin
  fIndentGuides := True;
  fShowCollapsedLine := True;
  fCollapsedLineColor := clBlack;
  fFolderBarLinesColor := clBlack;
  fIndentGuidesColor := clGray;

  fFoldRegions := TFoldRegions.Create(TFoldRegionItem);
  with fFoldRegions do begin
    Add(True, '{', '}', 'Symbol');
    //Add(True, 'BEGIN','END','Identifier'); // too slow :(
  end;
end;

destructor TSynCodeFolding.Destroy;
begin
  fFoldRegions.Free;
  inherited;
end;

end.

