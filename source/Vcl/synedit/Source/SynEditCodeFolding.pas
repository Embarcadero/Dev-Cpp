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

	TSynEditFoldRange = class;
	TFoldRegions = class;

	TSynCodeFolding = class
	private
		fIndentGuides: Boolean;
		fShowCollapsedLine: Boolean;
		fCollapsedLineColor: TColor;
		fFolderBarLinesColor: TColor;
		fCollapsingMarkStyle: TSynCollapsingMarkStyle;
		fFoldRegions: TFoldRegions;
		fEnabled : boolean;

		procedure SetEnabled(value : boolean);
	public
		constructor Create(enable : boolean);
		destructor Destroy; override;

		property CollapsedLineColor: TColor read fCollapsedLineColor write fCollapsedLineColor;
		property CollapsingMarkStyle: TSynCollapsingMarkStyle read fCollapsingMarkStyle write fCollapsingMarkStyle;
		property FolderBarLinesColor: TColor read fFolderBarLinesColor write fFolderBarLinesColor;
		property IndentGuides: Boolean read fIndentGuides write fIndentGuides;
		property ShowCollapsedLine: Boolean read fShowCollapsedLine write fShowCollapsedLine;
		property FoldRegions: TFoldRegions read fFoldRegions write fFoldRegions;
		property Enabled : Boolean read fEnabled write SetEnabled;
	end;

	TFoldRegionItem = class(TCollectionItem)
	private
		fType: TFoldRegionType;
		fAddEnding: Boolean;
		fSubFoldRegions: TFoldRegions;
		fOpen: PChar;
		fClose: PChar;

		procedure SetClose(const Value: PChar);
		procedure SetOpen(const Value: PChar);
	public
		constructor Create(Collection: TCollection); override;
		destructor Destroy; override;
		property FoldRegionType: TFoldRegionType read fType write fType;
		property AddEnding: Boolean read fAddEnding write fAddEnding;
		property SubFoldRegions: TFoldRegions read fSubFoldRegions;
		property Open: PChar read fOpen write SetOpen;
		property Close: PChar read fClose write SetClose;
	end;

	TFoldRegions = class(TCollection)
	private
		function GetItem(Index: Integer): TFoldRegionItem;
	public
		constructor Create(ItemClass: TCollectionItemClass);
		destructor Destroy; override;
		function Add(AType: TFoldRegionType; AAddEnding: boolean;AOpen, AClose: PChar): TFoldRegionItem;

		property Items[Index: Integer]: TFoldRegionItem read GetItem; default;
	end;

	// A parent fold which owns fold ranges (branch)
	TSynEditFoldRanges = class(TObject)
	private
		fRanges: TList;
		fOwnsObjects : boolean;
		function Get(Index: Integer): TSynEditFoldRange;
		function GetCount: Integer;
	public
		constructor Create;
		destructor Destroy; override;

		function AddByParts(AAllFold: TSynEditFoldRanges; AFromLine,AIndent, ARealLevel: Integer; AFoldRegion: TFoldRegionItem;AToLine: Integer = 0): TSynEditFoldRange;
		procedure AddObject(FoldRange: TSynEditFoldRange);

		procedure Delete(Index: Integer);

		property OwnsObjects : boolean read fOwnsObjects write fOwnsObjects;
		property Count: Integer read GetCount;
		property FoldRanges[Index: Integer]: TSynEditFoldRange read Get; default;
		property Ranges: TList read fRanges;
	end;

	// A single fold
	TSynEditFoldRange = class(TObject)
	private
		fFromLine, // Beginning line
		fToLine, // End line
		fIndent, // Indent level (physcial)
		fLinesCollapsed, // Number of collapsed lines
		fCollapsedBy: Integer; // Parent fold range index
		fRealLevel: Integer; // Fold range level
		fSubFoldRanges: TSynEditFoldRanges; // Sub fold ranges
		fCollapsed, // Is collapsed?
		fParentCollapsed: Boolean; // Is collapsed together with it's parent?
		fCollapsedLines: TStringList; // Collapsed lines
		fAllFoldRanges: TSynEditFoldRanges; // TAllFoldRanges pointer
		fFoldRegion: TFoldRegionItem; // FoldRegion pointer
		fHintMarkLeft: Integer;

		// For GetUncollapsedLines
		fFromLineBackup : integer;
		fToLineBackup : integer;
		fCollapsedByBackup: Integer;
		fParentCollapsedBackup: Boolean;
		fCollapsedBackup : boolean;
		fModified: Boolean;
		procedure SetRealLevel(const Value: Integer);
	public
		constructor Create;
		destructor Destroy; override;

		procedure SetPCOfSubFoldRanges(AParentCollapsed: Boolean;ACollapsedBy: Integer);
		function RealLinesCollapsed: Integer;
		procedure MoveBy(LineCount: Integer);
		procedure MoveChildren(By: Integer);
		procedure Widen(LineCount: Integer);

		procedure Backup;
		procedure Fix;
		procedure BackupPCOfSubFoldRanges;
		procedure FixPCOfSubFoldRanges;

		property AllFolds : TSynEditFoldRanges read fAllFoldRanges;
		property RealLevel: Integer read fRealLevel write SetRealLevel;
		property SubFoldRanges: TSynEditFoldRanges read fSubFoldRanges;
		property FromLine: Integer read fFromLine write fFromLine;
		property ToLine: Integer read fToLine write fToLine;
		property Indent: Integer read fIndent write fIndent;
		property LinesCollapsed: Integer read fLinesCollapsed write fLinesCollapsed;
		property CollapsedBy: Integer read fCollapsedBy write fCollapsedBy;
		property Collapsed: Boolean read fCollapsed write fCollapsed;
		property ParentCollapsed: Boolean read fParentCollapsed write fParentCollapsed;
		property CollapsedLines: TStringList read fCollapsedLines;
		property FoldRegion: TFoldRegionItem read fFoldRegion write fFoldRegion;
		property HintMarkLeft: Integer read fHintMarkLeft write fHintMarkLeft;
		property Modified : Boolean read fModified write fModified;
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
	I : integer;
begin
	if OwnsObjects then
		for I:=0 to fRanges.Count - 1 do begin
			TObject(fRanges[i]).Free;
			fRanges[i] := nil;
		end;
	fRanges.Free;
	inherited;
end;

function TSynEditFoldRanges.AddByParts(AAllFold: TSynEditFoldRanges; AFromLine,AIndent, ARealLevel: Integer; AFoldRegion: TFoldRegionItem;AToLine: Integer): TSynEditFoldRange;
begin
	Result := TSynEditFoldRange.Create;
	with Result do begin
		fFromLine := AFromLine;
		fToLine := AToLine;
		fIndent := AIndent;
		fRealLevel := ARealLevel;
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
		with fAllFoldRanges.fRanges do
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

procedure TSynEditFoldRange.BackupPCOfSubFoldRanges;
var
	i: Integer;
begin
	for i := 0 to fSubFoldRanges.Count - 1 do begin
		fSubFoldRanges[i].BackupPCOfSubFoldRanges;

		// Always backup for GetUncollapsedLines
		fSubFoldRanges[i].fParentCollapsedBackup := fSubFoldRanges[i].fParentCollapsed;
		fSubFoldRanges[i].fCollapsedByBackup := fSubFoldRanges[i].fCollapsedBy;
	end;
end;

procedure TSynEditFoldRange.FixPCOfSubFoldRanges;
var
	i: Integer;
begin
	for i := 0 to fSubFoldRanges.Count - 1 do begin
		fSubFoldRanges[i].FixPCOfSubFoldRanges;

		// And revert to old settings
		fSubFoldRanges[i].fParentCollapsed := fSubFoldRanges[i].fParentCollapsedBackup;
		fSubFoldRanges[i].fCollapsedBy := fSubFoldRanges[i].fCollapsedByBackup;
	end;
end;

procedure TSynEditFoldRange.Backup;
begin
	fCollapsedBackup := fCollapsed;
	fFromLineBackup := fFromLine;
	fToLineBackup := fToLine;
end;

procedure TSynEditFoldRange.Fix;
begin
	fCollapsed := fCollapsedBackup;
	fFromLine := fFromLineBackup;
	fToLine := fToLineBackup;
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

function TFoldRegions.Add(AType: TFoldRegionType; AAddEnding: boolean;AOpen, AClose: PChar): TFoldRegionItem;
begin
	Result := TFoldRegionItem(inherited Add);
	with Result do begin
		fType := AType;
		fAddEnding := AAddEnding;
		Open := AOPen;
		Close := AClose;
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

constructor TSynCodeFolding.Create(enable : boolean);
begin
	fIndentGuides := True;
	fShowCollapsedLine := True;
	fCollapsedLineColor := clBlack;
	fFolderBarLinesColor := clBlack;
	fCollapsingMarkStyle := msSquare;
	fEnabled := false;

	if fEnabled then begin
		fFoldRegions := TFoldRegions.Create(TFoldRegionItem);
		with fFoldRegions do begin
			Add(rtChar, False, '{', '}');
			Add(rtKeyword, False, 'BEGIN', 'END');
		end;
	end;
end;

destructor TSynCodeFolding.Destroy;
begin
	fFoldRegions.Free;
	inherited;
end;

procedure TSynCodeFolding.SetEnabled(value : boolean);
begin
	if value <> fEnabled then begin
		fEnabled := value;

		// Disabling? Free fold regions
		if not fEnabled then begin
			fFoldRegions.Free;

		// Enabling? Other way round
		end else begin
			fFoldRegions := TFoldRegions.Create(TFoldRegionItem);
			with fFoldRegions do begin
				Add(rtChar, False, '{', '}');
				Add(rtKeyword, False, 'BEGIN', 'END');
			end;
		end;
	end;
end;


end.
