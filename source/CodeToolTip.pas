{----------------------------------------------------------------------------------

  The contents of this file are subject to the GNU General Public License
  Version 1.1 or later (the "License"); you may not use this file except in
  compliance with the License. You may obtain a copy of the License at
  http://www.gnu.org/copyleft/gpl.html

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
  the specific language governing rights and limitations under the License.

  The Initial Developer of the Original Code is Peter Schraut.
  http://www.console-dev.de

  
  Portions created by Peter Schraut are Copyright 
  (C) 2002, 2003, 2004 by Peter Schraut (http://www.console-dev.de) 
  All Rights Reserved.

  
----------------------------------------------------------------------------------}

//
//
//  History:
//
//    xx/xx/2002
//      Initial release
//
//    31/10/2003
//      Complete rewrite of first version
//
//    01/11/2003
//      Beautified code
//
//    02/11/2003
//      Improved active parameter highlighting
//      Added "SkipString" function
//
//    21/03/2004
//      Added compatibility for latest synedit with wordwrap
//
//    22/03/2004
//      Added 'overloaded' feature. when more than 1 prototype
//      with the same name is in the list, the hintwindow displays
//      two buttons (up/down) where the user can walk through
//      all the same named functions. (like in VC++ .NET)
//      
//      added highlighting for hints. the hints use the same
//      highlighter-attributes as the editor does :)
//
//      the codehint is now displayed below the currentline and at the
//      same x position where the token starts. (like in VC++ .NET)
//
//      now looks the file very rubbish and needs some serious cleanup :P
//
//
//    2004-12-05
//      NEW_SYNEDIT is no longer need as we use "new' synedit
//      and never going back to "old". removing all defines and ifdefs
//
//    24/03/2004
//      Made more changes for downwards compatibility with old SynEdit.
//      Check the 'NEW_SYNEDIT' define.
//      Outsourced XPToolTip code to its own unit
//      Sorted functions in alphabetical order
//
//    25/03/2004
//      Some minor fixes regarding overloaded tooltips
//      Turned of DropShadow by default, since it's a bit annoying for code tooltips
//      Fixed 'Select' function. It now sets FCustomSelection to True
//
//    26/03/2004
//      Fixed a bug where no hint appeared when the cursor was directly before a '('
//
//    2011/2012
//      Rewritten the code that tries to find out which function needs to be displayed.
//      Completely restyled the tooltip font. Now only highlights the currently needed argument.
//      The tooltip now also shows when placing the cursor inside completed arglists between ().
//      The tooltip now updates automatically when hopping from function to function.

unit CodeToolTip;

interface
uses
{$IFDEF WIN32}
  SysUtils, Dialogs, Classes, Windows, Messages, Graphics, Controls, Menus, Forms, StdCtrls,
  SynEditKbdHandler, SynEdit, SynEditHighlighter, datamod, utils, CppParser;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, QDialogs, Classes, Xlib, QGraphics, QControls, QMenus, QForms, QStdCtrls,
  QSynEditKbdHandler, QSynEdit, QSynEditHighlighter, Types;
{$ENDIF}

type
  TCustomCodeToolTipButton = class(TPersistent)
  private
    FLeft: Integer;
    FTop: Integer;
    FWidth: Integer;
    FHeight: Integer;
  protected
    procedure Paint(const TargetCanvas: TCanvas); virtual; abstract;
    property Left: Integer read FLeft write FLeft;
    property Top: Integer read FTop write FTop;
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
  public
    constructor Create; virtual;
    function ClientRect: TRect;
  end;


  TCodeToolTipUpButton = class(TCustomCodeToolTipButton)
  private
    FBitmap: TBitmap;
  protected
    procedure Paint(const TargetCanvas: TCanvas); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TCodeToolTipDownButton = class(TCustomCodeToolTipButton)
  private
    FBitmap: TBitmap;
  protected
    procedure Paint(const TargetCanvas: TCanvas); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  { Options to customize the CodeToolTip behavior }
  TToolTipOptions = set of
  (
    ttoHideOnEsc,                 // hides the tooltip when ESC has been pressed
    ttoCurrentArgumentExtra,      // force the current argument to attract extra attention
    shoFindBestMatchingToolTip    // automatically find the best matching tooltip (for overloaded functions)
  );

  TCodeToolTip = class(THintWindow)
  private
    FActivated: Boolean;
    FParser: TCppParser;
    FTokenPos: Integer;
    FFunctionEnd : Integer;
    FFunctionStart : Integer;
    FUpButton: TCustomCodeToolTipButton;
    FDownButton: TCustomCodeToolTipButton;
    FSelIndex: Integer;
    FBmp: TBitmap;
    FOptions: TToolTipOptions;
    FEditor: TCustomSynEdit;
    FKeyDownProc: TKeyEvent;
    FToolTips: TStringList;
    FCurParamIndex: Integer;
    FLookupEditor: TCustomSynEdit;
    FMaxScanLength: Integer;
    FOldFunction : AnsiString;
    FCustomSelIndex: Boolean; // user clicked up/down
    procedure SetSelIndex(Value: Integer);
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
  protected
    function GetCommaIndex(P: PAnsiChar; Start, CurPos: Integer):Integer;
    procedure EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    function FindClosestToolTip(const ToolTip: AnsiString; CommaIndex: Integer): AnsiString;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function DummyPaint : integer; // obtain tooltip width
    procedure Paint; override;
    procedure RemoveEditor(AEditor: TCustomSynEdit);
    procedure RethinkCoordAndActivate;
    procedure SetEditor(Value: TCustomSynEdit);
    property MaxScanLength: Integer read FMaxScanLength write FMaxScanLength;
    property Options: TToolTipOptions read FOptions write FOptions;
    property SelIndex: Integer read FSelIndex write SetSelIndex;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Show;
    procedure ActivateHint(Rect: TRect; const AHint: AnsiString); override;
    procedure ReleaseHandle;
    property Parser: TCppParser read FParser write FParser;
    property Activated: Boolean read FActivated write FActivated;
    property Editor: TCustomSynEdit read FEditor write SetEditor;
  end;

implementation

uses
	devcfg;

// contains the up/down buttons
// I tried to draw them using DrawFrameControl first,
// but it looked very bad, so I use a bitmap instead.
{$R CodeToolTip.res}

resourcestring
	SCodeToolTipIndexXOfX = '%2d/%2d';

var
	Identifiers : array[#0..#255] of boolean;

procedure MakeIdentTable;
var
	c: char;
begin
	FillChar(Identifiers, SizeOf(Identifiers), 0);

	for c := 'a' to 'z' do
		Identifiers[c] := True;

	for c := 'A' to 'Z' do
		Identifiers[c] := True;

	for c := '0' to '9' do
		Identifiers[c] := True;

	Identifiers['_'] := True;
	Identifiers['~'] := True;
end;

// Returns name of function, so passing "foo(bar,bar,bar)" will return "foo"
function GetPrototypeName(const S: AnsiString) : AnsiString;
var
	WordEnd, WordStart: Integer;
begin
	WordEnd := Pos('(', S);

	// If we found the starting ( of a function
	if WordEnd > 0 then begin

		// Skip blanks
		repeat
			Dec(WordEnd);
		until (WordEnd = 0) or not (S[WordEnd] in [#32,#9,#13,#10]);

		// Then save the next word
		WordStart := WordEnd;
		repeat
			Dec(WordStart);
		until (WordStart = 0) or (S[WordStart] in [#0..#32]); // This fixes an unsigned 0 - 1 range error

		Result := Copy(S, WordStart+1, WordEnd-WordStart);
	end else
		Result := '';
end;

// Count commas in the parameter list
function CountCommas(const S: AnsiString): Integer;
var
	J: Integer;
begin
	Result := 0;
	for J := 1 to Length(S) do
		if S[J] = ',' then Inc(Result);
end;

//----------------- TCustomCodeToolTipButton ---------------------------------------------------------------------------

constructor TCustomCodeToolTipButton.Create;
begin
	FLeft := 0;
	FTop := 0;
	FWidth := 9;
	FHeight := 11;
end;

function TCustomCodeToolTipButton.ClientRect: TRect;
begin
	Result := Rect(FLeft, FTop, FLeft+FWidth, FTop+FHeight);
end;

//----------------- TCodeToolTipUpButton -------------------------------------------------------------------------------

constructor TCodeToolTipUpButton.Create;
begin
	inherited;

	FBitmap := TBitmap.Create;
	FBitmap.LoadFromResourceName(HInstance, 'UPBUTTON');
end;

destructor TCodeToolTipUpButton.Destroy;
begin
	FBitmap.Free;
	inherited;
end;

procedure TCodeToolTipUpButton.Paint(const TargetCanvas: TCanvas);
begin
	StretchBlt(TargetCanvas.Handle, FLeft, FTop, FWidth, FHeight, FBitmap.Canvas.Handle, 0, 0, FBitmap.Width, FBitmap.Height, SRCCOPY);
end;

//----------------- TCodeToolTipDownButton -----------------------------------------------------------------------------

constructor TCodeToolTipDownButton.Create;
begin
	inherited;

	FBitmap := TBitmap.Create;
	FBitmap.LoadFromResourceName(HInstance, 'DOWNBUTTON');
end;

destructor TCodeToolTipDownButton.Destroy;
begin
	FBitmap.Free;
	inherited;
end;

procedure TCodeToolTipDownButton.Paint(const TargetCanvas: TCanvas);
begin
	StretchBlt(TargetCanvas.Handle, FLeft, FTop, FWidth, FHeight, FBitmap.Canvas.Handle, 0, 0, FBitmap.Width, FBitmap.Height, SRCCOPY);
end;

//----------------- TCodeToolTip ---------------------------------------------------------------------------------------

constructor TCodeToolTip.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);

	FLookupEditor := TSynEdit.Create(Self);

	FOptions := [ttoHideOnEsc,ttoCurrentArgumentExtra,shoFindBestMatchingToolTip];

	FBmp := TBitmap.Create;
	with FBmp do begin
		PixelFormat := pf24Bit;
		Width := Screen.Width; // worst case hintwidth
		Height := 32;
	end;

	FToolTips := TStringList.Create;
	with FToolTips do begin
		Sorted := False;
		CaseSensitive := True;
		Duplicates := dupAccept;
	end;

	FMaxScanLength := 256; // Number of character to walk through trying to find ( and )
	FKeyDownProc  := EditorKeyDown;

	FUpButton := TCodeToolTipUpButton.Create;
	FUpButton.Left := 4;
	FUpButton.Top := 3;

	FDownButton := TCodeToolTipDownButton.Create;
	FDownButton.Left := 20; // Edited at runtime
	FDownButton.Top := 3;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCodeToolTip.ActivateHint(Rect: TRect; const AHint: AnsiString);
begin
	inherited;
	FActivated := True;
end;

procedure TCodeToolTip.ReleaseHandle;
begin
	FActivated := False;
	DestroyHandle;
end;

destructor TCodeToolTip.Destroy;
begin
	if Activated then ReleaseHandle;

	FKeyDownProc := nil;
	FEditor := nil;

	FreeAndNil(FUpButton);
	FreeAndNil(FDownButton);

	FBmp.Free;
	FToolTips.Free;
	FLookupEditor.Free;

	inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

// Handles input of the tooltip...
procedure TCodeToolTip.EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
	if Activated then begin
		case Key of
			VK_ESCAPE: begin
				if (ttoHideOnESC in FOptions) then ReleaseHandle;
			end;
		end;
	end;
end;

function TCodeToolTip.FindClosestToolTip(const ToolTip: AnsiString; CommaIndex: Integer): AnsiString;
var
	I,K: Integer;
	NewIndex: Integer;
	Str: AnsiString;
	LastCommaCnt: Integer;
begin

	// Don't change selection if the current tooltip is correct too
	if ToolTip = FToolTips.Strings[FSelIndex] then
		if CountCommas(FToolTips.Strings[FSelIndex]) <= CommaIndex then Exit;

	LastCommaCnt := 9999;
	NewIndex := 0;

	// Pick the one with the count being as close as possible to the act
	for I := 0 to FToolTips.Count-1 do begin
		Str := GetPrototypeName(FToolTips.Strings[I]);
		if SameStr(Str,ToolTip) then begin
			K := CountCommas(FToolTips.Strings[I]);
			if K >= CommaIndex then begin
				if K < LastCommaCnt then begin
					NewIndex := I;
					LastCommaCnt := K;
				end;
			end;
		end;
	end;

	Result := FToolTips.Strings[NewIndex];
	SelIndex := NewIndex;
end;

// Return the comma count the cursor is placed *after*, so foo(aaa,bbb,ccc) with the cursor somewhere in b will return 1
function TCodeToolTip.GetCommaIndex(P: PAnsiChar; Start, CurPos: Integer):Integer;
var
  I: Integer;

  // skip to EOL
  procedure SkipLine;
  begin
    repeat
      Inc(i);
      if i > CurPos then
        Break;
    until P[i] in [#0,#10,#13];
  end;

  // skip c/c++ commentblocks
  procedure SkipCommentBlock;
  begin
    repeat
      case P[i] of
        '*':
          if P[i+1] = '/' then
          begin
            Break;
          end;
      end;
      Inc(i);
      if i > CurPos then
        Break;
    until P[i] in [#0];
  end;

  // skip strings! since it not unusual to
  // have commas in AnsiString we MUST ignore them!
  procedure SkipStrings;
  begin
    Inc(i);

    repeat
      case P[i] of
        // make sure not to skip inline AnsiString "'s
        // for example, a c AnsiString: "Hello \"Bond, James\"..."
        // This is ONE AnsiString only and we dont want to count commas in it
        '\':
          if P[i+1] = '"' then
            Inc(i);

        '"': Break;
      end;
      Inc(i);
      if i > CurPos then
        Break;
    until P[i] in [#0];
  end;

begin
	Result := 0;
	I := Start;

	while I <= CurPos do begin
		if P[i] = '"' then begin
			SkipStrings;
		end else if P[i] = '(' then begin
			repeat
				Inc(I);
			until (P[i] = #0) or (P[i] = ')');
		end else if P[i] = '/' then begin
			if P[i+1] = '/' then
				SkipLine
			else if P[i+1] = '*' then
				SkipCommentBlock;
		end else if P[i] = ',' then
			Inc(Result);

		Inc(i);
	end;
end;

procedure TCodeToolTip.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
	Pt: TPoint;
	NeedRefresh: Boolean;
begin
	inherited;

	if Button = mbLeft then begin
		NeedRefresh := False;

		Pt := ScreenToClient(Mouse.CursorPos);
		if PtInRect(FUpButton.ClientRect, Pt) then begin
			if FSelIndex < FToolTips.Count-1 then
				Inc(FSelIndex, 1)
			else
				FSelIndex := 0;

			NeedRefresh := True;
			FCustomSelIndex := True;
		end else if PtInRect(FDownButton.ClientRect, Pt) then begin
			if FSelIndex > 0 then
				Dec(FSelIndex, 1)
			else
				FSelIndex := FToolTips.Count-1;

			NeedRefresh := True;
			FCustomSelIndex := True;
		end;

		FEditor.SetFocus;
		if NeedRefresh then
			Show;
	end;
end;

// Determine how much space we need...
function TCodeToolTip.DummyPaint : integer;
var
	ArgumentIndex,HighlightStart: Integer;
	CurPos : integer;
	s : AnsiString;
	InsideString : boolean;
	HLAttr: TSynHighlighterAttributes;
begin
	Result := 4; // left offset in pixels

	// when more than one tooltip is in the list
	// we must draw the buttons as well ...
	if FToolTips.Count > 1 then begin

		// paint the UP button
		Inc(Result, FUpButton.Left + FUpButton.Width);

		// output text between the buttons
		Inc(Result, FBmp.Canvas.TextWidth(Format(SCodeToolTipIndexXOfX, [FSelIndex+1, FToolTips.Count])) + 3);

		// paint the DOWN button
		Inc(Result, 3 + FDownButton.Width + FUpButton.Left);
	end;

	ArgumentIndex := -1;
	HighlightStart := 0;

	// now loop through the hint and draw each letter
	for CurPos := 1 to Length(Caption) do begin

		// we use a lookup editor to get the syntax coloring for our tooltips and check for strings
		FLookupEditor.CaretX := CurPos;
		FLookupEditor.GetHighlighterAttriAtRowCol(FLookupEditor.CaretXY, S, HLAttr);
		InsideString := (HLAttr = FLookupEditor.Highlighter.StringAttribute);

		// Argument delimiters
		if not InsideString then begin
			if (Caption[CurPos] = ',') or (Caption[CurPos] = '(') then begin
				Inc(ArgumentIndex);

				// Don't draw this one red yet, but start at the next char...
				HighlightStart := CurPos + 1;
			end else if (Caption[CurPos] = ')') then begin

				// Stop highlighting
				Inc(ArgumentIndex,999);
			end;
		end;

		with FBmp.Canvas do begin
			Font.Color := clBlack; //don't use clCaptionText: can be white too, making stuff unreadable
			if (ArgumentIndex = FCurParamIndex) and (CurPos >= HighlightStart) then begin
				Font.Style := [fsBold];
				if (ttoCurrentArgumentExtra in FOptions) then
					Font.Color := clRed;
			end else
				Font.Style := HLAttr.Style;

			Inc(Result, TextWidth(Caption[CurPos]));
		end;
	end;
end;

// This function paints the tooltip to FBmp and copies it to the tooltip client surface
procedure TCodeToolTip.Paint;
var
	ArgumentIndex,HighlightStart,WidthParam : Integer;
	S : AnsiString;
	CurPos : integer;
	InsideString : boolean;
	HLAttr : TSynHighlighterAttributes;
begin
	// Clear the backbuffer and set options
	with FBmp.Canvas do begin
		Brush.Color := TColor($E1FFFF);
		FillRect(ClientRect);
		Brush.Style := bsClear;
	end;

	WidthParam := 4; // left offset in pixels

	// when more than one tooltip is in the list
	// we must draw the buttons as well ...
	if FToolTips.Count > 1 then begin

		// paint the UP button
		FUpButton.Paint(FBmp.Canvas);
		Inc(WidthParam, FUpButton.Left + FUpButton.Width);

		// output text between the buttons
		S := Format(SCodeToolTipIndexXOfX, [FSelIndex+1, FToolTips.Count]);
		FBmp.Canvas.TextOut(WidthParam, 1, S);
		Inc(WidthParam, FBmp.Canvas.TextWidth(S) + 3);

		// paint the DOWN button
		FDownButton.Left := WidthParam;
		FDownButton.Paint(FBmp.Canvas);
		Inc(WidthParam, 3 + FDownButton.Width + FUpButton.Left);
	end;

	ArgumentIndex := -1;
	HighlightStart := 0;

	// now loop through the hint and draw each letter
	for CurPos := 1 to Length(Caption) do begin

		// we use a lookup editor to get the syntax coloring for our tooltips and check for strings
		FLookupEditor.CaretX := CurPos;
		FLookupEditor.GetHighlighterAttriAtRowCol(FLookupEditor.CaretXY, S, HLAttr);
		InsideString := (HLAttr = FLookupEditor.Highlighter.StringAttribute);

		// Argument delimiters
		if not InsideString then begin
			if (Caption[CurPos] = ',') or (Caption[CurPos] = '(') then begin
				Inc(ArgumentIndex);

				// Don't draw this one red yet, but start at the next char...
				HighlightStart := CurPos + 1;
			end else if (Caption[CurPos] = ')') then begin

				// Stop highlighting
				Inc(ArgumentIndex,999);
			end;
		end;

		with FBmp.Canvas do begin
			Font.Color := clBlack; //don't use clCaptionText can be white too, making stuff unreadable
			if (ArgumentIndex = FCurParamIndex) and (CurPos >= HighlightStart) then begin
				Font.Style := [fsBold];
				if (ttoCurrentArgumentExtra in FOptions) then
					Font.Color := clRed;
			end else
				Font.Style := HLAttr.Style;

			TextOut(WidthParam, 1, Caption[CurPos]);
			Inc(WidthParam, TextWidth(Caption[CurPos]));
		end;
	end;
	Canvas.CopyRect(ClientRect, FBmp.Canvas, ClientRect);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCodeToolTip.RemoveEditor(aEditor: TCustomSynEdit);
begin
	if Assigned(aEditor) then begin
		aEditor.RemoveKeyDownHandler(fKeyDownProc);
		if aEditor = fEditor then
			fEditor := nil;
		{$IFDEF SYN_COMPILER_5_UP}
		RemoveFreeNotification(aEditor);
		{$ENDIF}
	end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCodeToolTip.RethinkCoordAndActivate;
var
	Pt: TPoint;
begin
	// Clear the backbuffer and set options
	with FBmp.Canvas do begin

		// Paint the bg
		Brush.Color := TColor($E1FFFF);
		FillRect(ClientRect);
		Brush.Style := bsClear;

		// Set font properties before estimating height using it
		Font.Name := devEditor.Font.Name;
		Font.Size := 10;
		Font.Style := [];
	end;

	// this displays the rect below the current line and at the same position where the token begins
	Pt.X := FEditor.ClientToScreen(FEditor.RowColumnToPixels(FEditor.BufferToDisplayPos(FEditor.CharIndexToRowCol(FTokenPos,true)))).X;
	Pt.Y := FEditor.ClientToScreen(FEditor.RowColumnToPixels(FEditor.BufferToDisplayPos(FEditor.CharIndexToRowCol(FFunctionEnd,true)))).Y;

	ActivateHint(Rect(Pt.X,
                    Pt.Y+FEditor.LineHeight+2,
                    Pt.X+DummyPaint,
                    Pt.Y+FBmp.Canvas.TextHeight('Wg')+FEditor.LineHeight+4),
                    Caption);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCodeToolTip.SetEditor(Value: TCustomSynEdit);
begin
	if Assigned(FEditor) then
		RemoveEditor(fEditor);

	FEditor := Value;

	if Assigned(FEditor) then begin
		FEditor.AddKeyDownHandler(FKeyDownProc);
		FEditor.FreeNotification(FEditor);
	end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCodeToolTip.SetSelIndex(Value: Integer);
// sets the selection index and repaints the hint when it is activated
begin
	if Value <> FSelIndex then begin
		FSelIndex := Value;
		if Activated then begin
			FCustomSelIndex := True;
			Show;
		end;
	end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCodeToolTip.Show;
var
	CurPos : Integer;
	P : PAnsiChar;
	I : Integer;
	nBraces : Integer;
	S : AnsiString;
	CaretPos : Integer;
	nCommas : Integer;
	token : AnsiString;
	HLAttr : TSynHighlighterAttributes;
	FuncStartXY : TBufferCoord;
begin

	// get the current position in the collapsed text
	CaretPos := FEditor.RowColToCharIndex(FEditor.CaretXY,true);
	CurPos := CaretPos;

	// get a pointer to the collapsed text
	P := PAnsiChar(FEditor.Lines.Text);

	nBraces := 0;
	nCommas := 0;

	// Find out where the function ends...
	for I := 1 to FMaxScanLength do begin

		// Stopping characters...
		if (P[CurPos] = #0) or (P[CurPos] = ';') then begin
			ReleaseHandle;
			Exit;

		// Opening brace, increase count
		end else if (P[CurPos] = '(') then begin
			Inc(nBraces);

		// Ending brace, decrease count or success (found ending)!
		end else if (P[CurPos] = ')') then begin
			Dec(nBraces);
			if nBraces = -1 then
				break;

		// Single line comments
		end else if (P[CurPos] = '/') and (P[CurPos+1] = '/') then begin

			// Walk up to an enter sequence
			while (P[CurPos] <> #0) and not (P[CurPos] in [#13, #10]) do
				Inc(curpos);

			// Skip ONE enter sequence (CRLF, CR, LF, etc.)
			if (P[CurPos] = #13) and (P[CurPos+1] <> #0) and (P[CurPos+1] = #13) then // DOS
				Inc(curpos,2)
			else if (P[CurPos] = #13) then // UNIX
				Inc(curpos)
			else if (P[CurPos] = #10) then // MAC
				Inc(curpos);

		// Multiline comments
		end else if (P[CurPos] = '/') and (P[CurPos+1] <> #0) and (P[CurPos+1] = '*') then begin

			// Walk up to an */
			while (P[CurPos] <> #0) and (P[CurPos+1] <> #0) and not ((P[CurPos] = '*') and (P[CurPos+1] = '/')) do
				Inc(curpos);

			// Step over
			Inc(curpos);
		end;

		Inc(CurPos);
	end;

	// If we couldn't find the closing brace or reached the FMaxScanLength...
	if (nBraces <> -1) then begin // -1 means found it
		ReleaseHandle;
		Exit;
	end;

	FFunctionEnd := CurPos;

	// We've stopped at the ending ), start walking backwards )*here* with nBraces = -1
	for I := 1 to FMaxScanLength do begin

		if (P[CurPos] = '/') and (P[CurPos-1] = '*') then begin
			repeat
				Dec(CurPos);
			until (CurPos < 2) or ((P[CurPos] = '/') and (P[CurPos+1] = '*'));

		end else if (P[CurPos] = ')') then begin
			Inc(nBraces);

		end else if (P[CurPos] = '(') then begin
			Dec(nBraces);
			if nBraces = -1 then // Found it!
				Break;

		end else if (P[CurPos] = ',') then begin
			if nBraces = 0 then
				Inc(nCommas);
		end;

		Dec(CurPos);
		if CurPos < 1 then break;
	end;

	// If we couldn't find the opening brace or reached the FMaxScanLength
	if (nBraces <> -1) then begin // -1 means found it
		ReleaseHandle;
		Exit;
	end;

	FFunctionStart := CurPos;

	// Skip blanks
	while (CurPos > 1) and (P[CurPos-1] in [#32,#9,#13,#10]) do
		Dec(CurPos);

	// Get the name of the function we're about to show
	FuncStartXY := FEditor.CharIndexToRowCol(CurPos-1,true);
	S := FEditor.GetWordAtRowCol(FuncStartXY);

	// Don't bother scanning the database when there's no identifier to scan for
	FEditor.GetHighlighterAttriAtRowCol(FuncStartXY,token,HLAttr);
	if not (HLAttr = FEditor.Highlighter.IdentifierAttribute) then begin
		ReleaseHandle;
		Exit;
	end;

	// Only do the cumbersome list filling when showing a new tooltip...
	if (S <> FOldFunction) or not Activated then begin
		FSelIndex := 0;
		FCustomSelIndex := False;

		// Fill a cache of known functions...
		FToolTips.BeginUpdate;
		FToolTips.Clear;
		FParser.FillListOfFunctions(S, FToolTips);
		FToolTips.EndUpdate;
	end;

	// If we can't find it in our database, hide
	if FToolTips.Count = 0 then begin
		ReleaseHandle;
		Exit;
	end;

	FOldFunction := S;

	// get the current token position in the text
	// this is where the prototype name usually starts
	FTokenPos := CurPos - Length(S);

	// Search for the best possible overload match according to comma count
	if (shoFindBestMatchingToolTip in FOptions) then

		// Only do so when the user didn't select his own
		if not FCustomSelIndex then
			S := FindClosestToolTip(S, nCommas);

	// Select the current one
	if (FSelIndex < FToolTips.Count) then
		S := FToolTips.Strings[FSelIndex];

	// set the hint caption
	Caption := S;

	// we use the LookupEditor to get the highlighter-attributes
	// from. check the DrawAdvanced method!
	FLookupEditor.Text := S;
	FLookupEditor.Highlighter := FEditor.Highlighter;

	// get the index of the current argument (where the cursor is)
	FCurParamIndex := GetCommaIndex(P, FFunctionStart + 1, CaretPos - 1);
	RethinkCoordAndActivate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCodeToolTip.WMNCHitTest(var Message: TWMNCHitTest);
var
	Pt: TPoint;
begin
	Message.Result := HTTRANSPARENT;
	Pt := ScreenToClient(Point(Message.XPos,Message.YPos));

	// hitcheck against the position of our both buttons
	if PtInRect(FUpButton.ClientRect, Pt) or PtInRect(FDownButton.ClientRect, Pt) then
		Message.Result := HTCLIENT;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCodeToolTip.WMEraseBkgnd(var Message: TWMEraseBkgnd);
// override WMEraseBkgnd to avoid flickering
begin
	Message.Result := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

initialization
	MakeIdentTable;

end.

