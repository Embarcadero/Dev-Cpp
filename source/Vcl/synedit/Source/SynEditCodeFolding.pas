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
	Graphics, Types, Classes, SysUtils;

type
	TSynEditFoldRange = class;
  TSynEditAllFoldRanges = class;
  TFoldRegions = class;

  TSkipRegionItemType = (itString, itMultiLineComment, itSingleLineComment);

  TSkipRegionItem = class(TCollectionItem)
  private
    fClose: PChar;
    fOpen: PChar;
    fEscape: PChar;
    fType: TSkipRegionItemType;
    procedure SetClose(const Value: PChar);
    procedure SetOpen(const Value: PChar);
    procedure SetEscape(const Value: PChar);
  public
  	property Open: PChar read fOpen write SetOpen;
    property Close: PChar read fClose write SetClose;
    property Escape: PChar read fEscape write SetEscape;
    property RegionType: TSkipRegionItemType read fType write fType;
  end;

  TSkipRegions = class(TCollection)
  private
    function GetSkipRegionItem(Index: Integer): TSkipRegionItem;
  public
    function Add(const aOpen, aClose, aEscape: String;aType: TSkipRegionItemType): TSkipRegionItem;
    property SkipRegions[Index: Integer]: TSkipRegionItem read GetSkipRegionItem; default;
  end;

	TFoldRegionType = (rtChar, rtKeyWord);

	// Folditem
	TFoldRegionItem = class(TCollectionItem)
	private
		fType: TFoldRegionType;
		fAddEnding: Boolean;
		fNoSubFoldRegions: Boolean;
		fSubFoldRegions: TFoldRegions;
		fOpen: PChar;
		fClose: PChar;
		fParentRegion: TFoldRegionItem;
		fWholeWords: Boolean;
		fRegExpUseOpen: Boolean;
		fRegExpOpen: PChar;
		fRegExpUseClose: Boolean;
		fRegExpClose: PChar;
		fName: String;
		
		procedure SetClose(const Value: PChar);
		procedure SetOpen(const Value: PChar);
		procedure SetRegExpClose(const Value: PChar);
		procedure SetRegExpOpen(const Value: PChar);
	public
		constructor Create(Collection: TCollection); override;
		destructor Destroy; override;
		property FoldRegionType: TFoldRegionType read fType write fType;
		property AddEnding: Boolean read fAddEnding write fAddEnding;
		property NoSubFoldRegions: Boolean read fNoSubFoldRegions write fNoSubFoldRegions;
		property SubFoldRegions: TFoldRegions read fSubFoldRegions;
		property Open: PChar read fOpen write SetOpen;
		property Close: PChar read fClose write SetClose;
		property ParentRegion: TFoldRegionItem read fParentRegion write fParentRegion;
		property WholeWords: Boolean read fWholeWords write fWholeWords;
		property RegExpOpen: PChar read fRegExpOpen write SetRegExpOpen;
		property RegExpClose: PChar read fRegExpClose write SetRegExpClose;
		property RegExpUseOpen: Boolean read fRegExpUseOpen write fRegExpUseOpen;
		property RegExpUseClose: Boolean read fRegExpUseClose write fRegExpUseClose;
		property Name: String read fName write fName;
	end;

	// Folditemlist (zoals { } en /* */)
	TFoldRegions = class(TCollection)
	private
		fSkipRegions: TSkipRegions;

		function GetItem(Index: Integer): TFoldRegionItem;
	public
		constructor Create(ItemClass: TCollectionItemClass);
		destructor Destroy; override;
		function Add(AType: TFoldRegionType; AAddEnding, ANoSubFoldRegions,AWholeWords: Boolean; AOpen, AClose: PChar;AParentRegion: TFoldRegionItem = nil): TFoldRegionItem;

		property Items[Index: Integer]: TFoldRegionItem read GetItem; default;
		property SkipRegions: TSkipRegions read fSkipRegions;
	end;

	// Alle folds, baseclass
	TSynEditFoldRanges = class(TPersistent)
	private
		fRanges: TList;
		function GetSynEditFoldRange(Index: Integer): TSynEditFoldRange;
		function GetCount: Integer;
	public
		constructor Create;
		destructor Destroy; override;

		function Add(AAllFold: TSynEditAllFoldRanges; AFromLine,ALevel, ARealLevel: Integer; AFoldRegion: TFoldRegionItem;AToLine: Integer = 0): TSynEditFoldRange;
		procedure AddF(FoldRange: TSynEditFoldRange);
		procedure Clear;

		property Count: Integer read GetCount;
		property FoldRanges[Index: Integer]: TSynEditFoldRange read GetSynEditFoldRange; default;
		property Ranges: TList read fRanges;
	end;

	// Alle folds, mainclass, wordt ook voor parents gebruikt
	TSynEditAllFoldRanges = class(TSynEditFoldRanges)
	private
		fAllRanges: TList;
		function GetAllCount: Integer;
		function GetAllFoldRange(Index: Integer): TSynEditFoldRange;
	public
		constructor Create;
		destructor Destroy; override;

		procedure ClearAll;
		procedure Delete(Index: Integer);
		procedure AddFold(FoldRange: TSynEditFoldRange);
		procedure Assign(Source: TPersistent); override;

		property AllCount: Integer read GetAllCount;
		property AllFoldRanges[Index: Integer]: TSynEditFoldRange read GetAllFoldRange; default;
		property AllRanges: TList read fAllRanges;
	end;

	// Een enkele fold
	TSynEditFoldRange = class
	private
		fFromLine, // Beginning line
		fToLine, // End line
		fLevel, // Indent level (physcial)
		fLinesCollapsed, // Number of collapsed lines
		fCollapsedBy: Integer; // Parent fold range index
		fRealLevel: Integer; // Fold range level
		fSubFoldRanges: TSynEditFoldRanges; // Sub fold ranges
		fCollapsed, // Is collapsed?
		fParentCollapsed: Boolean; // Is collapsed together with it's parent?
		fCollapsedLines: TStringList; // Collapsed lines
		fAllFoldRanges: TSynEditAllFoldRanges; // TAllFoldRanges pointer
		fFoldRegion: TFoldRegionItem; // FoldRegion pointer
		fHintMarkLeft: Integer;
		procedure SetRealLevel(const Value: Integer);
	public
		constructor Create;
		destructor Destroy; override;

		procedure SetPCOfSubFoldRanges(AParentCollapsed: Boolean;ACollapsedBy: Integer);
		function RealLinesCollapsed: Integer;
		procedure MoveBy(LineCount: Integer);
		procedure MoveChildren(By: Integer);
		procedure Widen(LineCount: Integer);
		function Collapsable: Boolean;

		property RealLevel: Integer read fRealLevel write SetRealLevel;
		property SubFoldRanges: TSynEditFoldRanges read fSubFoldRanges;
		property FromLine: Integer read fFromLine write fFromLine;
		property ToLine: Integer read fToLine write fToLine;
		property Level: Integer read fLevel write fLevel;
		property LinesCollapsed: Integer read fLinesCollapsed write fLinesCollapsed;
		property CollapsedBy: Integer read fCollapsedBy write fCollapsedBy;
		property Collapsed: Boolean read fCollapsed write fCollapsed;
		property ParentCollapsed: Boolean read fParentCollapsed write fParentCollapsed;
		property CollapsedLines: TStringList read fCollapsedLines;
		property FoldRegion: TFoldRegionItem read fFoldRegion write fFoldRegion;
		property HintMarkLeft: Integer read fHintMarkLeft write fHintMarkLeft;
	end;

	TNode = class
	private
		fValue:      Pointer;
		fChildNode:  TNode;
		fParentNode: TNode;
	public
		constructor Create(AValue: Pointer);

		property ChildNode: TNode read fChildNode;
		property ParentNode: TNode read fParentNode;
		property Value: Pointer read fValue;
	end;

	TStack = class
	private
		fFirstNode: TNode;
	public
		constructor Create;
		destructor Destroy; override;

		function IsEmpty: Boolean;
		function Peak: Pointer;
		function Pop: Pointer;
		procedure Push(Value: Pointer);
	end;

	THSLColor = record
		Hue,
		Saturation,
		Luminace: Integer;
	end;

function RGB2HSL(Color: TColor): THSLColor;
function HSL2RGB(Color: THSLColor): TColor;
function RoundUp(Number: Single): Integer;

implementation

uses utils;

function RoundUp(Number: Single): Integer;
begin
	if Number - Trunc(Number) > 0 then
		Result := Trunc(Number) + 1
	else
		Result := Trunc(Number);
end;

function RGB2HSL(Color: TColor): THSLColor;
var
	R, G, B, Max, Min, Diff, RGB, Sum: Integer;
  q: Single;
begin
  RGB := ColorToRGB(Color);
  R := RGB and $0000FF;
  G := (RGB and $00FF00) shr 8;
  B := (RGB and $FF0000) shr 16;

  if R > G then begin
  	Max := R;
    Min := G;
  end else begin
  	Max := G;
    Min := R;
  end;

  if B > Max then
  	Max := B
  else if B < Min then
  	Min := B;

  Sum := Max + Min;
  Diff := Max - Min;

  Result.Hue := 0;
  Result.Saturation := 0;
  Result.Luminace := (100 * Sum) div 510;

  if Diff > 0 then begin
  	if Result.Luminace <= 50 then
  		Result.Saturation := 100 * Diff div Sum
    else
      Result.Saturation := 100 * Diff div (510 - Sum);

    q := 60 / Diff;

    if Max = R then begin
    	if G < B then
        Result.Hue := Round(360 + q * (G - B))
    	else
        Result.Hue := Round(q * (G - B));
    end
    else if Max = G then
      Result.Hue := Round(q * (B - R) + 120)
    else if Max = B then
      Result.Hue := Round(q * (R - G) + 240);
  end;
end;

function HSL2RGB(Color: THSLColor): TColor;
var
	R, G, B, H, S, L, Min, Max, Diff: Single;
begin
	if Color.Saturation = 0 then
 		Result := (Round(2.55 * Color.Luminace) shl 16)
    or (Round(2.55 * Color.Luminace) shl 8) or (Round(2.55 * Color.Luminace))
  else begin
  	H := Color.Hue / 60;
    S := Color.Saturation / 100;
    L := Color.Luminace / 100;

    if L <= 0.5 then
    	Min := L * (1 - S)
    else
    	Min := L - S * (1 - L);
      
    Max := 2 * L - Min;
    Diff := Max - Min;

    case (Color.Hue div 60) of
    	0:
      begin
      	R := Max;
        B := Min;
        G := H * Diff + Min;
      end;
      1:
      begin
      	G := Max;
        B := Min;
        R := Min - (H - 2) * Diff;
      end;
      2:
      begin
      	G := Max;
        R := Min;
        B := (H - 2) * Diff + Min;
			end;
      3:
      begin
      	B := Max;
        R := Min;
        G := Min - (H - 4) * Diff;
      end;
      4:
      begin
      	B := Max;
        G := Min;
        R := (H - 4) * Diff + Min;
      end;
    else
    	R := Max;
      G := Min;
      B := Min - (H - 6) * Diff;
    end;

    Result := (RoundUp(B * 255) shl 16) + (RoundUp(G * 255) shl 8) + RoundUp(R * 255);
  end;
end;

{ TSynEditAllFoldRanges }

procedure TSynEditAllFoldRanges.AddFold(FoldRange: TSynEditFoldRange);
begin
	fAllRanges.Add(FoldRange);
end;

procedure TSynEditAllFoldRanges.Assign(Source: TPersistent);
var
	Src: TSynEditAllFoldRanges;

	procedure RangeCopy(FoldRanges, SrcFoldRanges: TSynEditFoldRanges);
	var
		i: Integer;
		FoldRange: TSynEditFoldRange;
	begin
		for i := 0 to SrcFoldRanges.GetCount - 1 do begin
			with SrcFoldRanges[i] do begin
				FoldRange := FoldRanges.Add(Self, fFromLine, fLevel, fRealLevel, fFoldRegion, fToLine);
				FoldRange.fLinesCollapsed := fLinesCollapsed;
				FoldRange.fCollapsedBy := fCollapsedBy;
				FoldRange.fCollapsed := fCollapsed;
				FoldRange.fParentCollapsed := fParentCollapsed;
				FoldRange.fCollapsedLines.Assign(fCollapsedLines);
				FoldRange.fFoldRegion := fFoldRegion;
			end;
			RangeCopy(FoldRange.SubFoldRanges, SrcFoldRanges[i].SubFoldRanges);
		end;
	end;

begin
	Src := TSynEditAllFoldRanges(Source);
	RangeCopy(Self, Src);
end;

procedure TSynEditAllFoldRanges.ClearAll;
var
	I : integer;
begin
	for I:= 0 to fAllRanges.Count - 1 do begin
		TObject(fAllRanges[i]).Free;
	end;
	fAllRanges.Clear;
end;

constructor TSynEditAllFoldRanges.Create;
begin
	inherited;
	fAllRanges := TList.Create;
end;

procedure TSynEditAllFoldRanges.Delete(Index: Integer);
begin
	fAllRanges.Delete(Index);
end;

destructor TSynEditAllFoldRanges.Destroy;
begin
	fAllRanges.Free;
	inherited;
end;

function TSynEditAllFoldRanges.GetAllCount: Integer;
begin
	Result := fAllRanges.Count;
end;

function TSynEditAllFoldRanges.GetAllFoldRange(Index: Integer): TSynEditFoldRange;
begin
	Result := fAllRanges[Index];
end;

{ TSynEditFoldRanges }

function TSynEditFoldRanges.Add(AAllFold: TSynEditAllFoldRanges; AFromLine,ALevel, ARealLevel: Integer; AFoldRegion: TFoldRegionItem;AToLine: Integer): TSynEditFoldRange;
begin
	Result := TSynEditFoldRange.Create;
	with Result do begin
		fFromLine := AFromLine;
		fToLine := AToLine;
		fLevel := ALevel;
		fRealLevel := ARealLevel;
		fAllFoldRanges := AAllFold;
		fFoldRegion := AFoldRegion;
	end;
	fRanges.Add(Result);
	AAllFold.fAllRanges.Add(Result);
end;

procedure TSynEditFoldRanges.AddF(FoldRange: TSynEditFoldRange);
begin
	fRanges.Add(FoldRange);
end;

procedure TSynEditFoldRanges.Clear;
begin
	fRanges.Clear;
end;

constructor TSynEditFoldRanges.Create;
begin
	fRanges := TList.Create;
end;

destructor TSynEditFoldRanges.Destroy;
begin
	fRanges.Free;
	inherited;
end;

function TSynEditFoldRanges.GetCount: Integer;
begin
	Result := fRanges.Count;
end;

function TSynEditFoldRanges.GetSynEditFoldRange(
  Index: Integer): TSynEditFoldRange;
begin
	Result := fRanges[Index];
end;

{ TSynEditFoldRange }

function TSynEditFoldRange.Collapsable: Boolean;
begin
	Result := fFromLine <> fToLine;
end;

constructor TSynEditFoldRange.Create;
begin
	fSubFoldRanges := TSynEditFoldRanges.Create;
	fCollapsedLines := TStringList.Create;
	fCollapsedBy := -1;
end;

destructor TSynEditFoldRange.Destroy;
begin
  fSubFoldRanges.Free;
  fCollapsedLines.Free;
  fFoldRegion.Free;
  inherited;
end;

procedure TSynEditFoldRange.MoveBy(LineCount: Integer);
begin
	Inc(fFromLine, LineCount);
	Inc(fToLine, LineCount);
end;

procedure TSynEditFoldRange.MoveChildren(By: Integer);
var
	i: Integer;
begin
	for i := 0 to fSubFoldRanges.Count - 1 do
  begin
  	fSubFoldRanges[i].MoveChildren(By);
    
  	with fAllFoldRanges.FAllRanges do
    	if fSubFoldRanges[i].fParentCollapsed then
    		Move(IndexOf(fSubFoldRanges[i]), IndexOf(fSubFoldRanges[i]) + By);
  end;
end;

function TSynEditFoldRange.RealLinesCollapsed: Integer;
	function RealLinesCollapsedEx(FoldRange: TSynEditFoldRange): Integer;
  var
  	i: Integer;
  begin
  	Result := 0;

    with FoldRange do
  		for i := 0 to fSubFoldRanges.Count - 1 do
      begin
    		Inc(Result, RealLinesCollapsedEx(fSubFoldRanges[i]));
        
    		if fSubFoldRanges[i].fCollapsed then
      		Inc(Result, fSubFoldRanges[i].fLinesCollapsed + 1);
    	end;
  end;
begin
	Result := fLinesCollapsed + RealLinesCollapsedEx(Self);
end;

procedure TSynEditFoldRange.SetPCOfSubFoldRanges(AParentCollapsed: Boolean;
  ACollapsedBy: Integer);
var
	i: Integer;
begin
	for i := 0 to fSubFoldRanges.Count - 1 do begin
		fSubFoldRanges[i].SetPCOfSubFoldRanges(AParentCollapsed, ACollapsedBy);

		if (fSubFoldRanges[i].fCollapsedBy = -1) or (fSubFoldRanges[i].fCollapsedBy = ACollapsedBy) then begin
			fSubFoldRanges[i].fParentCollapsed := AParentCollapsed;

			if not AParentCollapsed then
				fSubFoldRanges[i].fCollapsedBy := -1
			else
				fSubFoldRanges[i].fCollapsedBy := ACollapsedBy;
		end;
	end;
end;

procedure TSynEditFoldRange.SetRealLevel(const Value: Integer);
var
	i: Integer;
begin
	if fParentCollapsed then
  	fCollapsedBy := Value - 1;

  fRealLevel := Value;

  for i := 0 to fSubFoldRanges.Count - 1 do
  	fSubFoldRanges[i].RealLevel := fRealLevel + 1;
end;

procedure TSynEditFoldRange.Widen(LineCount: Integer);
begin
	Inc(fToLine, LineCount);
end;

{ TStack }

constructor TStack.Create;
begin
	fFirstNode := nil;
end;

destructor TStack.Destroy;
begin
	while not IsEmpty
  	do Pop;

  inherited;
end;

function TStack.IsEmpty: Boolean;
begin
	Result := fFirstNode = nil;
end;

function TStack.Peak: Pointer;
begin
	if IsEmpty then
		Result := nil
	else
		Result := fFirstNode.fValue;
end;

function TStack.Pop: Pointer;
var
	TmpNode: TNode;
begin
	if IsEmpty then
		Result := nil
	else
  begin
		TmpNode := fFirstNode;
		Result := fFirstNode.Value;
		fFirstNode := fFirstNode.fChildNode;
		TmpNode.Free;
	end;
end;

procedure TStack.Push(Value: Pointer);
var
	Node: TNode;
begin
	Node := TNode.Create(Value);

	if not IsEmpty then
  begin
		Node.fChildNode := fFirstNode;
		fFirstNode.fParentNode := Node;
	end;

	fFirstNode := Node;
end;

{ TNode }

constructor TNode.Create(AValue: Pointer);
begin
	fValue := AValue;
	fChildNode := nil;
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

function TFoldRegions.Add(AType: TFoldRegionType; AAddEnding,
	ANoSubFoldRegions, AWholeWords: Boolean; AOpen, AClose: PChar;
  AParentRegion: TFoldRegionItem): TFoldRegionItem;
begin
	Result := TFoldRegionItem(inherited Add);
	with Result do begin
		fType := AType;
		fAddEnding := AAddEnding;
		fNoSubFoldRegions := ANoSubFoldRegions;
		fWholeWords := AWholeWords;
		Result.Open := AOPen;
		Result.Close := AClose;
		fParentRegion := AParentRegion;
	end;
end;

constructor TFoldRegions.Create(ItemClass: TCollectionItemClass);
begin
	inherited Create(ItemClass);
	fSkipRegions := TSkipRegions.Create(TSkipRegionItem);
end;

destructor TFoldRegions.Destroy;
begin
	fSkipRegions.Free;
	inherited;
end;

function TFoldRegions.GetItem(Index: Integer): TFoldRegionItem;
begin
	Result := TFoldRegionItem(inherited Items[Index]);
end;

procedure TFoldRegionItem.SetClose(const Value: PChar);
begin
  GetMem(fClose, StrLen(Value) + 1);
  StrCopy(fClose, Value);
end;

procedure TFoldRegionItem.SetOpen(const Value: PChar);
begin
  GetMem(fOpen, StrLen(Value) + 1);
  StrCopy(fOpen, Value);
end;

procedure TFoldRegionItem.SetRegExpClose(const Value: PChar);
begin
  GetMem(fRegExpClose, StrLen(Value) + 1);
  StrCopy(fRegExpClose, Value);
end;

procedure TFoldRegionItem.SetRegExpOpen(const Value: PChar);
begin
  GetMem(fRegExpOpen, StrLen(Value) + 1);
  StrCopy(fRegExpOpen, Value);
end;

{ TSkipRegions }

function TSkipRegions.Add(const aOpen, aClose, aEscape: String;aType: TSkipRegionItemType): TSkipRegionItem;
begin
	Result := TSkipRegionItem(inherited Add);
	with Result do begin
		Open := PChar(aOpen);
		Close := PChar(aClose);
		Escape := PChar(aEscape);
		RegionType := aType;
	end;
end;

function TSkipRegions.GetSkipRegionItem(Index: Integer): TSkipRegionItem;
begin
	Result := TSkipRegionItem(inherited Items[Index]);
end;

{ TSkipRegionItem }

procedure TSkipRegionItem.SetClose(const Value: PChar);
begin
  if fClose <> nil then
    FreeMem(fClose);

  GetMem(fClose, StrLen(Value) + 1);
  StrCopy(fClose, Value);
end;

procedure TSkipRegionItem.SetEscape(const Value: PChar);
begin
  if fEscape <> nil then // Orwell Mod: fOpen should be fEscape
    FreeMem(fEscape);

  GetMem(fEscape, StrLen(Value) + 1);
  StrCopy(fEscape, Value);
end;

procedure TSkipRegionItem.SetOpen(const Value: PChar);
begin
  if fOpen <> nil then
    FreeMem(fOpen);

  GetMem(fOpen, StrLen(Value) + 1);
  StrCopy(fOpen, Value);
end;

end.
