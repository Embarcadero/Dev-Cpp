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
	TFoldRegionType = (rtChar, rtKeyWord);
	TSynCollapsingMarkStyle = (msSquare, msEllipse);
	TSynCodeFoldingChanges = (fcEnabled, fcRefresh, fcRescan);
	TCodeFoldingChangeEvent = procedure(Event: TSynCodeFoldingChanges) of object;

	TSynEditFoldRange = class;
	TSynEditAllFoldRanges = class;
	TFoldRegions = class;

	TSynCodeFolding = class(TPersistent)
	private
		fIndentGuides: Boolean;
		fShowCollapsedLine: Boolean;
		fCollapsedLineColor: TColor;
		fEnabled: Boolean;
		fHighlightIndentGuides: Boolean;
		fFolderBarColor: TColor;
		fFolderBarLinesColor: TColor;
		fCollapsingMarkStyle: TSynCollapsingMarkStyle;
		fFoldRegions: TFoldRegions;
		fCaseSensitive: Boolean;
		fOnChange: TCodeFoldingChangeEvent;

		procedure SetFolderBarColor(const Value: TColor);
		procedure SetFolderBarLinesColor(const Value: TColor);
		procedure SetEnabled(const Value: Boolean);
		procedure SetCollapsedLineColor(const Value: TColor);
		procedure SetCollapsingMarkStyle(const Value: TSynCollapsingMarkStyle);
		procedure SetHighlightIndentGuides(const Value: Boolean);
		procedure SetIndentGuides(const Value: Boolean);
		procedure SetShowCollapsedLine(const Value: Boolean);
		procedure SetCaseSensitive(const Value: Boolean);
	public
		constructor Create;
		destructor Destroy; override;

		procedure Assign(Source: TPersistent); override;

		property CaseSensitive: Boolean read fCaseSensitive write SetCaseSensitive;
		property CollapsedLineColor: TColor read fCollapsedLineColor write SetCollapsedLineColor default clDefault;
		property CollapsingMarkStyle: TSynCollapsingMarkStyle read fCollapsingMarkStyle write SetCollapsingMarkStyle default msSquare;
		property Enabled: Boolean read fEnabled write SetEnabled default False;
		property FoldRegions: TFoldRegions read fFoldRegions;
		property FolderBarColor: TColor read fFolderBarColor write SetFolderBarColor default clDefault;
		property FolderBarLinesColor: TColor read fFolderBarLinesColor write SetFolderBarLinesColor default clDefault;
		property HighlightIndentGuides: Boolean read fHighlightIndentGuides write SetHighlightIndentGuides default True;
		property IndentGuides: Boolean read fIndentGuides write SetIndentGuides default True;
		property ShowCollapsedLine: Boolean read fShowCollapsedLine write SetShowCollapsedLine default True;
		property OnChange: TCodeFoldingChangeEvent read fOnChange write fOnChange;
	end;

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
		fName: String;

		procedure SetClose(const Value: PChar);
		procedure SetOpen(const Value: PChar);
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
		property Name: String read fName write fName;
	end;

	TFoldRegions = class(TCollection)
	private
		function GetItem(Index: Integer): TFoldRegionItem;
	public
		constructor Create(ItemClass: TCollectionItemClass);
		destructor Destroy; override;
		function Add(AType: TFoldRegionType; AAddEnding, ANoSubFoldRegions,AWholeWords: Boolean; AOpen, AClose: PChar;AParentRegion: TFoldRegionItem = nil): TFoldRegionItem;

		property Items[Index: Integer]: TFoldRegionItem read GetItem; default;
	end;

	// A parent fold which owns fold ranges
	TSynEditFoldRanges = class(TObject)
	private
		fRanges: TList;
		function GetSynEditFoldRange(Index: Integer): TSynEditFoldRange;
		function GetCount: Integer;
	public
		constructor Create;
		destructor Destroy; override;

		function AddByParts(AAllFold: TSynEditAllFoldRanges; AFromLine,ALevel, ARealLevel: Integer; AFoldRegion: TFoldRegionItem;AToLine: Integer = 0): TSynEditFoldRange;
		procedure AddObject(FoldRange: TSynEditFoldRange);

		property Count: Integer read GetCount;
		property FoldRanges[Index: Integer]: TSynEditFoldRange read GetSynEditFoldRange; default;
		property Ranges: TList read fRanges;
	end;

	// Top-level folds
	TSynEditAllFoldRanges = class(TSynEditFoldRanges)
	private
		fAllRanges: TList;
		function GetAllCount: Integer;
		function GetAllFoldRange(Index: Integer): TSynEditFoldRange;
	public
		constructor Create;
		destructor Destroy; override;

		procedure Delete(Index: Integer);

		property AllCount: Integer read GetAllCount;
		property AllFoldRanges[Index: Integer]: TSynEditFoldRange read GetAllFoldRange; default;
		property AllRanges: TList read fAllRanges;
	end;

	// A single fold
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

implementation

{ TSynEditAllFoldRanges }

constructor TSynEditAllFoldRanges.Create;
begin
	inherited;
	fAllRanges := TList.Create;
end;

destructor TSynEditAllFoldRanges.Destroy;
var
	I : integer;
begin
	for I:=0 to fAllRanges.Count - 1 do begin
		TObject(fAllRanges[i]).Free;
		fAllRanges[i] := nil;
	end;
	fAllRanges.Free;
	inherited;
end;

procedure TSynEditAllFoldRanges.Delete(Index: Integer);
begin
	TObject(fAllRanges[Index]).Free;
	fAllRanges.Delete(Index);
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

function TSynEditFoldRanges.AddByParts(AAllFold: TSynEditAllFoldRanges; AFromLine,ALevel, ARealLevel: Integer; AFoldRegion: TFoldRegionItem;AToLine: Integer): TSynEditFoldRange;
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

	// Add pointers
	fRanges.Add(Result);
	AAllFold.fAllRanges.Add(Result);
end;

procedure TSynEditFoldRanges.AddObject(FoldRange: TSynEditFoldRange);
begin
	fRanges.Add(FoldRange);
end;

constructor TSynEditFoldRanges.Create;
begin
	inherited;
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

function TSynEditFoldRanges.GetSynEditFoldRange(Index: Integer): TSynEditFoldRange;
begin
	Result := TSynEditFoldRange(fRanges[Index]);
end;

{ TSynEditFoldRange }

function TSynEditFoldRange.Collapsable: Boolean;
begin
	Result := fFromLine <> fToLine;
end;

constructor TSynEditFoldRange.Create;
begin
	inherited;
	fSubFoldRanges := TSynEditFoldRanges.Create;
	fCollapsedLines := TStringList.Create;
	fCollapsedBy := -1;
end;

destructor TSynEditFoldRange.Destroy;
begin
	fSubFoldRanges.Free;
	fCollapsedLines.Free;
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
	for i := 0 to fSubFoldRanges.Count - 1 do begin
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
			for i := 0 to fSubFoldRanges.Count - 1 do begin
				Inc(Result, RealLinesCollapsedEx(fSubFoldRanges[i]));
				if fSubFoldRanges[i].fCollapsed then
					Inc(Result, fSubFoldRanges[i].fLinesCollapsed + 1);
			end;
	end;
begin
	Result := fLinesCollapsed + RealLinesCollapsedEx(Self);
end;

procedure TSynEditFoldRange.SetPCOfSubFoldRanges(AParentCollapsed: Boolean;ACollapsedBy: Integer);
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

{ TFoldRegion }

constructor TFoldRegionItem.Create(Collection: TCollection);
begin
	inherited Create(Collection);
	fSubFoldRegions := TFoldRegions.Create(TFoldRegionItem);
end;

destructor TFoldRegionItem.Destroy;
begin
	fSubFoldRegions.Free;
	if Assigned(fOpen) then
		FreeMem(fOpen);
	if Assigned(fClose) then
		FreeMem(fClose);
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
		Open := AOPen;
		Close := AClose;
		fParentRegion := AParentRegion;
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

procedure TFoldRegionItem.SetClose(const Value: PChar);
begin
	if fClose <> nil then
		FreeMem(fClose);

	GetMem(fClose, StrLen(Value) + 1);
	StrCopy(fClose, Value);
end;

procedure TFoldRegionItem.SetOpen(const Value: PChar);
begin
	if fOpen <> nil then
		FreeMem(fOpen);

	GetMem(fOpen, StrLen(Value) + 1);
	StrCopy(fOpen, Value);
end;

procedure TSynCodeFolding.Assign(Source: TPersistent);
begin
	inherited
end;

constructor TSynCodeFolding.Create;
begin
	fCollapsingMarkStyle := msSquare;
	fShowCollapsedLine := True;
	fFoldRegions := TFoldRegions.Create(TFoldRegionItem); // Leaky
end;

destructor TSynCodeFolding.Destroy;
begin
	fFoldRegions.Free;
	inherited;
end;

procedure TSynCodeFolding.SetEnabled(const Value: Boolean);
begin
	fEnabled := Value;

	if Assigned(fOnChange) then fOnChange(fcEnabled);
end;

procedure TSynCodeFolding.SetFolderBarColor(const Value: TColor);
begin
	fFolderBarColor := Value;

	if Assigned(fOnChange) then fOnChange(fcRefresh);
end;

procedure TSynCodeFolding.SetFolderBarLinesColor(const Value: TColor);
begin
	fFolderBarLinesColor := Value;

	if Assigned(fOnChange) then fOnChange(fcRefresh);
end;

procedure TSynCodeFolding.SetCollapsedLineColor(const Value: TColor);
begin
	fCollapsedLineColor := Value;

	if Assigned(fOnChange) then fOnChange(fcRefresh);
end;

procedure TSynCodeFolding.SetCollapsingMarkStyle(const Value: TSynCollapsingMarkStyle);
begin
	fCollapsingMarkStyle := Value;
  
  if Assigned(fOnChange) then fOnChange(fcRefresh);
end;

procedure TSynCodeFolding.SetHighlightIndentGuides(const Value: Boolean);
begin
	fHighlightIndentGuides := Value;
  
  if Assigned(fOnChange) then fOnChange(fcRefresh);
end;

procedure TSynCodeFolding.SetIndentGuides(const Value: Boolean);
begin
	fIndentGuides := Value;
  
  if Assigned(fOnChange) then fOnChange(fcRefresh);
end;

procedure TSynCodeFolding.SetShowCollapsedLine(const Value: Boolean);
begin
	fShowCollapsedLine := Value;

  if Assigned(fOnChange) then fOnChange(fcRefresh);
end;

procedure TSynCodeFolding.SetCaseSensitive(const Value: Boolean);
begin
	fCaseSensitive := Value;

	if Assigned(fOnChange) then fOnChange(fcRescan);
end;

end.
