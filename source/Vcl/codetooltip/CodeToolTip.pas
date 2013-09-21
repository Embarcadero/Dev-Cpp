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
//      Complete rewrote of first version
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
//    26/03/2204
//      Fixed a bug where no hint appeared when the cursor was directly before a '('
//
//

unit CodeToolTip;

// the xptooltip looks nicer than the original THintWindow
// from delphi and yeah, it supports alphablending under win2k
// and shadowing under win xp. it is is downwards compatible.
// ttould run on win98 etc too!
{$DEFINE USE_XPTOOLTIP}


interface
uses
{$IFDEF WIN32}
  SysUtils, Dialogs, Classes, Windows, Messages, Graphics, Controls, Menus, Forms, StdCtrls,
  SynEditKbdHandler, SynEdit, SynEditHighlighter, XPToolTip;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, QDialogs, Classes, Xlib, QGraphics, QControls, QMenus, QForms, QStdCtrls,
  QSynEditKbdHandler, QSynEdit, QSynEditHighlighter, Types, XPToolTip;
{$ENDIF}

         
type
  TCustomCodeToolTipButton = class(TPersistent)
  private
    FLeft,
    FTop,
    FWidth,
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
    ttoHideOnEnter,               // hides the tooltip when Return has been pressed
    ttoHideOnEsc,                 // hides the tooltip when ESC has been pressed
    ttoPaintStipples,             // paints stippled under the current highlighted argument 
    ttoCurrentArgumentBlackOnly,  // force the current argument to be black only
    shoFindBestMatchingToolTip    // automatically find the best matching tooltip (for overloaded functions)
  );
      
      
{$IFDEF USE_XPTOOLTIP}
  TBaseCodeToolTip = class(TXPToolTip)
{$ELSE}
  TBaseCodeToolTip = class(TToolTip)
{$ENDIF}
  private
    FTokenPos: Integer;
    FUpButton: TCustomCodeToolTipButton;
    FDownButton: TCustomCodeToolTipButton;
    FSelIndex: Integer;
    FBmp: TBitmap;
    FOptions: TToolTipOptions;
    FEditor: TCustomSynEdit;
    FKeyDownProc: TKeyEvent;  
    FEndWhenChr: String;
    FStartWhenChr: String;
    FToolTips: TStringList;
    FCurCharW: Word;
    FCurShift: TShiftState;
    FActivateKey: TShortCut;
    FCurParamIndex: Integer;
    FLookupEditor: TCustomSynEdit;
    FDelimiters: string;
    FMaxScanLength: Integer;
    FCustomSelIndex: Boolean; // user clicked up/down
    procedure SetSelIndex(Value: Integer);
    procedure SetToolTips(const Strings: TStringList);
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
  protected
    procedure DoBeforeShow(const AToolTips: TStringList; const APrototypeName: string); virtual;
    function GetCommaIndex(P: PChar; BraceStart, CurPos: Integer):Integer; virtual;
    procedure EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    function FindClosestToolTip(ToolTip: string; CommaIndex: Integer): string; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    function PaintToolTip: Integer; virtual;
    function RemoveEditor(AEditor: TCustomSynEdit): boolean; virtual;
    procedure RethinkCoordAndActivate; virtual;
    procedure SetEditor(const Value: TCustomSynEdit); virtual; 
    property ActivateKey: TShortCut read FActivateKey write FActivateKey; 
    property Editor: TCustomSynEdit read FEditor write SetEditor;
    property EndWhenChr: String read FEndWhenChr write FEndWhenChr;
    property Hints: TStringList read FToolTips write SetToolTips;
    property MaxScanLength: Integer read FMaxScanLength write FMaxScanLength default 1024;
    property Options: TToolTipOptions read FOptions write FOptions;
    property SelIndex: Integer read FSelIndex write SetSelIndex;
    property StartWhenChr: String read FStartWhenChr write FStartWhenChr;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Select(ToolTip: string): Integer; virtual;
    procedure Show; virtual;
  end;

  
  TCodeToolTip = class(TBaseCodeToolTip)
  public
    property Activated;
    property SelIndex;
  published
    property ActivateKey;
    property Color;
    property Editor;
    property EndWhenChr;
    property Hints;
    property MaxScanLength;
    property Options;    
    property StartWhenChr;
  end;

  
implementation

// contains the up/down buttons
// i tried to draw them using DrawFrameControl first,
// but it looked very bad, so I use a bitmap instead.
{$R CodeToolTip.res}

resourcestring
  SCodeToolTipIndexXOfX = '%d / %d';

  
var
 Identifiers : array[#0..#255] of ByteBool;

  
//----------------------------------------------------------------------------------------------------------------------

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
  end;

  //--------------------------------------------------------------------------------------------------------------------
  
  function PreviousWordString(S: string; Index: Integer):string;
  var
    I: Integer;
  begin
 
    Result := '';

    if (Index <= 1) or (S = '') then
      Exit;

    I := Index;

    // Skip blanks and TAB's
    repeat
      Dec(Index);
    until not (S[Index] in [#32,#9]);
    Inc(Index);

    repeat
      Dec(Index);
      if Index < 2 then
        Break;
    until not Identifiers[S[Index]];

    if not Identifiers[S[Index]] then
      Inc(Index);

    Result := Copy(S, Index, I-Index);
  end;

  function GetPrototypeName(const S: string): string;
  var
    iStart, iLen: Integer;
  begin
    Result := '';
    iStart := AnsiPos('(', S);

    if iStart > 0 then
    begin

      // Skip blanks and TAB's
      repeat
        Dec(iStart);
      until not (S[iStart] in [#32,#9]);
      Inc(iStart);
    
      Dec(iStart);
      iLen := 0;
      repeat
        Dec(iStart);
        Inc(iLen);
      until (S[iStart] in [#0..#32]);
      Result := Copy(S, iStart+1, iLen);
    end;
  end;
  
  function CountCommas(const S: string): Integer;
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

//----------------------------------------------------------------------------------------------------------------------

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

//----------------------------------------------------------------------------------------------------------------------

destructor TCodeToolTipUpButton.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCodeToolTipUpButton.Paint(const TargetCanvas: TCanvas);
begin
  StretchBlt(TargetCanvas.Handle, FLeft, FTop, FWidth, FHeight, 
    FBitmap.Canvas.Handle, 0, 0, FBitmap.Width, FBitmap.Height, SRCCOPY);
end;

//----------------- TCodeToolTipDownButton -----------------------------------------------------------------------------

constructor TCodeToolTipDownButton.Create;
begin
  inherited;

  FBitmap := TBitmap.Create;
  FBitmap.LoadFromResourceName(HInstance, 'DOWNBUTTON');
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TCodeToolTipDownButton.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCodeToolTipDownButton.Paint(const TargetCanvas: TCanvas);
begin
  StretchBlt(TargetCanvas.Handle, FLeft, FTop, FWidth, FHeight, 
    FBitmap.Canvas.Handle, 0, 0, FBitmap.Width, FBitmap.Height, SRCCOPY);
end;

//----------------- TBaseCodeToolTip ---------------------------------------------------------------------------------------

constructor TBaseCodeToolTip.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

{$IFDEF USE_XPTOOLTIP}
  DropShadow := False;
{$ENDIF}
  
  FLookupEditor := TSynEdit.Create(Self);

  FOptions := [ttoHideOnEsc,ttoCurrentArgumentBlackOnly,shoFindBestMatchingToolTip];
  
  FBmp := TBitmap.Create;
  with FBmp do
  begin
    PixelFormat := pf24Bit;
    Width := Screen.Width; // worst case hintwidth
    Height := 32;
    Canvas.Font := Screen.HintFont;
  end; 

  FToolTips := TStringList.Create;
  with FToolTips do
  begin
    //Sorted := True;
    CaseSensitive := True;
    //Duplicates := dupIgnore;
  end;

  // FDelimiters is used for the prototype-scanning.
  // for example when your prototypes look like this:
  //  procedure foo(a: Integer; b: Byte);
  // then use FDelimiters := ';'
  // If they are C stylish like
  // void foo(int a, unsigned char b);
  // then use FDelimiters := ',' 
  FDelimiters := ',';
  
  FStartWhenChr := '(';          // Start to check, when one of this char is pressed
  EndWhenChr := ';\';
  FActivateKey := ShortCut(Ord(#32), [ssCtrl,ssShift]);
  FCurCharW := 0;
  FCurShift := [];

  // since we supports scanning over multiple lines,
  // we have a limit of how many chars we scan maximal...
  FMaxScanLength := 1024;

  FKeyDownProc  := EditorKeyDown;

  FUpButton := TCodeToolTipUpButton.Create;
  FUpButton.Left := 4;
  FUpButton.Top := 3;

  FDownButton := TCodeToolTipDownButton.Create;
  FDownButton.Left := 20; // unknown, it's calculated at runtime
  FDownButton.Top := 3;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TBaseCodeToolTip.Destroy;
begin
  if Activated then ReleaseHandle;

  FKeyDownProc  := nil;

  FEditor := nil;

  FreeAndNil(FUpButton);
  FreeAndNil(FDownButton);

  FToolTips.Free;
  FLookupEditor.Free;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseCodeToolTip.DoBeforeShow(const AToolTips: TStringList; const APrototypeName: string);
begin
  // descents override this function to be able to fill the FToolTips list with
  // more tooltips ...
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseCodeToolTip.EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  FCurCharW := Key;
  FCurShift := Shift;

  if FActivateKey = Shortcut(Key, Shift) then Show;
  
  if Activated then
  begin
    case Key of
{$IFDEF WIN32}
      VK_ESCAPE:
        begin
          if (ttoHideOnESC in FOptions) then ReleaseHandle;
        end;
        
      VK_RETURN:
        begin
          if (ttoHideOnEnter in FOptions) then ReleaseHandle
          else Show;
        end; 
{$ENDIF}
{$IFDEF LINUX}
      XK_ESCAPE:
        begin
          if (ttoHideOnESC in FOptions) then ReleaseHandle;
        end;
        
      XK_RETURN:
        begin
          if (ttoHideOnEnter in FOptions) then ReleaseHandle
          else Show;
        end; 
{$ENDIF}
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseCodeToolTip.FindClosestToolTip(ToolTip: string; CommaIndex: Integer): string;
var
  I,K: Integer;
  NewIndex: Integer;
  Str: string;
  LastCommaCnt: Integer;
begin
  LastCommaCnt := 4096;
  NewIndex := 0;
  Result := GetPrototypeName(FToolTips.Strings[FSelIndex]);
         
  // current commacount fits still in the selected tooltip and just do nothing
  if ToolTip = FToolTips.Strings[FSelIndex] then
    if CountCommas(FToolTips.Strings[FSelIndex]) <= CommaIndex then Exit;


  // loop through the list and find the closest matching tooltip
  // we compare the comma counts
  for I := 0 to FToolTips.Count-1 do
  begin
    Str := GetPrototypeName(FToolTips.Strings[I]);
    if Str = ToolTip then
    begin     
      K := CountCommas(FToolTips.Strings[I]);
      if K >= CommaIndex then
      begin
        if K < LastCommaCnt then
        begin
          NewIndex := I;
          LastCommaCnt := K;
        end;
      end; 
    end;
  end;
  
  Result := FToolTips.Strings[NewIndex];
  SelIndex := NewIndex;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseCodeToolTip.GetCommaIndex(P: PChar; BraceStart, CurPos: Integer):Integer;
//  to highlight the current prototype argument, we need
//  to know where the cursor in the prototype is.
//  this functions returns the count of commas from the beginning
//  prototype.
//  for example: 
//  definition is -> void foo(int a, int b, int c);
//  we write this -> foo(1, 2|
//  The '|' represents the cursor. In this example this function returns 1, since
//  it progressed one comma.
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
  // have commas in string we MUST ignore them!
  procedure SkipStrings;
  begin
    Inc(i);

    repeat
      case P[i] of
        // make sure not to skip inline string "'s
        // for example, a c string: "Hello \"Bond, James\"..."
        // This is ONE string only and we dont want to count commas in it
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
  I := BraceStart;

  while I < CurPos do
  begin
    case P[i] of
      // strings
      '"': 
        SkipStrings;

      // comments
      '/':
        if P[i+1] = '/' then
          SkipLine
        else
        if P[i+1] = '*' then
          SkipCommentBlock;

      // commas
      ',':
        Inc(Result);
    end;

    Inc(i);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseCodeToolTip.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Pt: TPoint;
  NeedRefresh: Boolean;
begin  
  inherited;
  
  if mbLeft = Button then
  begin
    NeedRefresh := False;
  
    try
      Pt := ScreenToClient(Mouse.CursorPos);
    
      // check if we clicked in the UpButton
      if PtInRect(FUpButton.ClientRect, Pt) then
      begin
        if FSelIndex < FToolTips.Count-1 then Inc(FSelIndex, 1)
        else FSelIndex := 0;
        NeedRefresh := True;
        FCustomSelIndex := True;
      end;

      // check if we clicked in the DownButton
      if PtInRect(FDownButton.ClientRect, Pt) then
      begin
        if FSelIndex > 0 then Dec(FSelIndex, 1)
        else FSelIndex := FToolTips.Count-1;
        NeedRefresh := True;
        FCustomSelIndex := True;
      end; 

    finally
      FEditor.SetFocus;
      if NeedRefresh then Show;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseCodeToolTip.Paint;
begin
  PaintToolTip;
  Canvas.CopyRect(ClientRect, FBmp.Canvas, ClientRect);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseCodeToolTip.PaintToolTip:Integer;
const
  cStipple : array [0..3] of Integer = (0,1,2,1);
var
 BracePos: Integer;
 WidthParam: Integer;
 I: Integer;
 CurParam: Integer;
 StrToken,S: string;
{$IFDEF WIN32}
 CurChar: Char;
{$ENDIF}
{$IFDEF LINUX}
 CurChar: WideChar;
{$ENDIF}

  procedure DrawParamLetterEx(Index: Integer; CurrentParam: Boolean=False);
  var
    J: Integer;
    CharW: Integer;
    CharH: Integer;
    HLAttr: TSynHighlighterAttributes;
  begin
    // we use a lookup editor to get the syntax coloring
    // for out tooltips :)
    FLookupEditor.CaretX := Index;
    FLookupEditor.GetHighlighterAttriAtRowCol(FLookupEditor.CaretXY, StrToken, HLAttr);
    
    with FBmp.Canvas do
    begin
      Brush.Style := bsClear;
      if HLAttr <> nil then
        Font.Color := HLAttr.Foreground;

      // if it is the word where the cursor currently is
      // we draw it in bold and also check for further drawing options
      if CurrentParam then
      begin
        Font.Style := [fsBold];
        if (ttoCurrentArgumentBlackOnly in FOptions) then
          Font.Color := clBlack;        
      end
      else if HLAttr <> nil then
        Font.Style := HLAttr.Style;
      
      CharH := TextHeight('Wg');
      CharW := TextWidth(FLookupEditor.Text[Index]);

      // draw the stipple line under the current param-char
      if CurrentParam and (ttoPaintStipples in FOptions) then
      begin
        for J := 0 to CharW do
          Pixels[WidthParam+J, CharH-cStipple[J and 3]] := clRed;
      end;

      // draw the char and then increase the current width by the
      // width of the just drawn char ...
      TextOut(WidthParam, 1, FLookupEditor.Text[Index]);
      Inc(WidthParam, CharW);      
    end;
  end;
begin

  BracePos := AnsiPos('(', Caption);
  if BracePos > 0 then
  begin

    CurParam := 0;  
    WidthParam := 4; // left start position in pixels
      
    // clear the backbuffer
    with FBmp.Canvas do
    begin    
      Brush.Color := Self.Color; 
      FillRect(ClientRect); 
    end;

    // when more than one tooltip is in the list
    // we must draw the buttons as well ...
    if FToolTips.Count > 1 then
    begin
      // paint the UP button
      FUpButton.Paint(FBmp.Canvas);
      Inc(WidthParam, FUpButton.Left + FUpButton.Width);

      // output text between the buttons
      FBmp.Canvas.Font.Style := [];
      S := Format(SCodeToolTipIndexXOfX, [FSelIndex+1, FToolTips.Count]);
      FBmp.Canvas.TextOut(WidthParam, 1, S);
      Inc(WidthParam, FBmp.Canvas.TextWidth(S)+3);

      // paint the DOWN button
      FDownButton.Paint(FBmp.Canvas);
      FDownButton.Left := WidthParam;
      Inc(WidthParam, 3 + FDownButton.Width+FUpButton.Left);
    end;
  

    // now loop through the hint and draw each letter
    for i := 1 to Length(Caption) do
    begin
      CurChar := Caption[I];
      
      // if the current char is one of our delimiters
      // we must increase the CurParam variable which indicates
      // at which comma index our cursor is.
      if AnsiPos(CurChar, FDelimiters) > 0 then
        Inc(CurParam);

      if (CurParam = FCurParamIndex) and (AnsiPos(CurChar, FDelimiters)=0) and (I > BracePos) and (CurChar <> ')') and (CurChar <> ' ') then
        DrawParamLetterEx(I, True) // at current comma index
      else
        DrawParamLetterEx(I); // normal
    end;
  end;

  Result := WidthParam+6;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseCodeToolTip.RemoveEditor(aEditor: TCustomSynEdit): boolean;
begin
  Result := Assigned (aEditor);

  if Result then
  begin         
    aEditor.RemoveKeyDownHandler(fKeyDownProc);
    if aEditor = fEditor then
      fEditor := nil;
  {$IFDEF SYN_COMPILER_5_UP}
    RemoveFreeNotification(aEditor);
  {$ENDIF}
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseCodeToolTip.RethinkCoordAndActivate;
const
  cHorzFix = 30;
var
  Pt: TPoint;
  NewWidth: Integer;
  CaretXYPix: TPoint;
  YPos: Integer;
begin
  CaretXYPix := FEditor.RowColumnToPixels(FEditor.DisplayXY);

  Pt := FEditor.ClientToScreen(Point(CaretXYPix.X, CaretXYPix.Y));
  Dec(Pt.X, cHorzFix);

  Dec(Pt.Y, FEditor.LineHeight);

  YPos := Pt.Y;
  Inc(YPos, FEditor.LineHeight);

  // draw the tooltop on the offscreen bitmap
  // and return the length of the drawn text  
  NewWidth := PaintToolTip;

  // this displays the rect below the current line and at the
  // same position where the token begins
  Pt := FEditor.ClientToScreen(FEditor.RowColumnToPixels(FEditor.BufferToDisplayPos(FEditor.CharIndexToRowCol(FTokenPos))));

  ActivateHint(Rect(Pt.X, 
                    YPos+2+FEditor.LineHeight, 
                    Pt.X+NewWidth, 
                    YPos+4+Canvas.TextHeight('Wg')+FEditor.LineHeight), 
                Caption);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseCodeToolTip.Select(ToolTip: string): Integer;

//  selects the tooltip specified by ToolTip and returns
//  the index from it, in the list. 
//  the tooltip must be already added to the tooltiplist,
//  otherwise it cannot find it and returns -1
//
//  on success it returns the index of the tooltip in the list
//  otherwise it returns -1

var
  I: Integer;
begin
  Result := -1;
  
  if FToolTips.Count <> -1 then
  begin
    for I := 0 to FToolTips.Count-1 do
    begin
      if FToolTips.Strings[I] = ToolTip then
      begin
        SelIndex := I;  // set the current index
        Result := I;  // return the index
        Break;
      end;
    end;
  end;  
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseCodeToolTip.SetEditor(const Value: TCustomSynEdit);
begin
  if (FEditor <> nil) then
    RemoveEditor (fEditor);

  FEditor := Value;

  if (FEditor <> nil) then
  begin   
    FEditor.AddKeyDownHandler(FKeyDownProc);
    FEditor.FreeNotification(FEditor);    
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseCodeToolTip.SetSelIndex(Value: Integer);
// sets the selection index and repaints the hint when it is activated
begin
  if (Value < 0) or (Value > FToolTips.Count) then
    raise Exception.Create('ToolTip selection index is out of list range!');

  if Value <> FSelIndex then
  begin
    FSelIndex := Value;
    if Activated then 
    begin
      FCustomSelIndex := True;
      Show;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseCodeToolTip.SetToolTips(const Strings: TStringList);
begin
  FToolTips.Clear;
  FToolTips.Assign(Strings);
end;


//----------------------------------------------------------------------------------------------------------------------

procedure TBaseCodeToolTip.Show;
var
  CurPos: Integer;
  P: PChar;
  I: Integer;
  nBraces: Integer;
  S: string;
  Idx: Integer;
  S1: string;
  nCommas: Integer;
  ProtoFound: Boolean;
  attr: TSynHighlighterAttributes;
  attrstr: String; //dummy string
      
  // skip c/c++ commentblocks 
  procedure SkipCommentBlock;
  begin
    repeat
      case P[CurPos] of
        '*':
          if P[CurPos-1] = '/' then
          begin
            Dec(CurPos);
            Break;
          end
          else
            Dec(CurPos);
      else
        Dec(CurPos);
      end;

      Inc(I);
      if i > FMaxScanLength then
        Break;
    until P[CurPos] in [#0];
  end;
begin
  
  ASSERT(nil <> FToolTips, 'FToolTips must not be nil');
  ASSERT(nil <> FEditor, 'FEditor must not be nil');
  ASSERT(nil <> FEditor.Highlighter, 'FEditor.Highlighter must not be nil');

  //get highlighter attribute and check if the cursor is not within
  //string or comment etc.
  FEditor.GetHighlighterAttriAtRowCol(FEditor.CaretXY, attrstr, attr);
  with FEditor.Highlighter do
    if (attr <> nil) then
      if (attr = StringAttribute) or (attr = CommentAttribute) then
        Exit;

  // get the current position in the text
  Idx := FEditor.SelStart;
  CurPos := FEditor.SelStart;
  
  // get a pointer to the text
  P := PChar(FEditor.Lines.Text);

  // set the braced count to its initial value
  // when a closing brace is right behind the cursor
  // we increase it by 1
  nBraces := -1;

  if P[CurPos-1] = ')' then Inc(nBraces,1);
  if P[CurPos] = ')' then Inc(nBraces,1);
    
  I := 1;
  nCommas := 0;
  
  // this loop is used to find an open function like
  // foo(i, ...
  while I < FMaxScanLength do
  begin
    case P[CurPos] of
       
      '/':
        if P[CurPos-1] = '*' then
          SkipCommentBlock;

      ')':
        begin
          Dec(nBraces);
          if nBraces = 0 then
          begin
            Inc(CurPos);
            Break;
          end;
        end;

      '(':
        begin
          Inc(nBraces);
          if nBraces = 0 then
          begin
            Inc(CurPos);
            Break;
          end;
        end;

      ',':
        Inc(nCommas);
        
      #0: Exit;
    end;

    Dec(CurPos);
    if CurPos <= 1 then
    begin
      CurPos := FEditor.SelStart;
      Inc(CurPos);
      Break;
    end;

    Inc(I);
  end;

  // when the tooltip is not activated yet
  // we don't have an index, so we set it to zero
  // by index i mean the index of an overloaded function
  if not Activated then 
  begin
    FSelIndex := 0;
    FCustomSelIndex := False;
  end;

  // get the previous word, this is the word infron
  // of the brace. for example from
  // foo(int a, int b
  // the previous word would be 'foo'  
  S := PreviousWordString(P, CurPos);

  DoBeforeShow(FToolTips, S);

  // get the current token position in the text
  // this is where the prototypename usually starts
  FTokenPos := CurPos - Length(S)-1;


  // check if the token is added to the list
  ProtoFound := False;
  for I := 0 to FToolTips.Count-1 do
  begin
    S1 := GetPrototypeName(FToolTips.Strings[I]);
    if S1 = S then
    begin
      ProtoFound := True;
      if not Activated then FSelIndex := I;      
      Break;
    enD;
  end;

  // if it is not found then empty the string sp
  // the tooltip just hides then
  if not ProtoFound then S := '';
  
  
  if (S <> '') then
  begin  
    // when the user has choosen an own index, ermm when he clicked
    // either the UP or DOWN button, we don't try to find the closest
    // tooltip anymore, we simply use the current selection index
    if (shoFindBestMatchingToolTip in FOptions) then
      if not FCustomSelIndex then
        S := FindClosestToolTip(S, nCommas);   

    if (FToolTips.Count > 0) and (FSelIndex < FToolTips.Count) then
      S := FToolTips.Strings[FSelIndex];  
  end;

      
  if S <> '' then
  begin       
    // set the hint caption
    Caption := S;
    
    // we use the LookupEditor to get the highlighter-attributes
    // from. check the DrawAdvanced method!
    FLookupEditor.Text := S;
    FLookupEditor.Highlighter := FEditor.Highlighter;

    // get the index of the current bracket where the cursor it
    FCurParamIndex := GetCommaIndex(P, CurPos, Idx-1);
    RethinkCoordAndActivate;    
  end
  else
  if (AnsiPos(Chr(FCurCharW), FEndWhenChr) > 0) then
    ReleaseHandle
  else
  // Make sure when we are before a brace, to release the hint.
  // hel_function(
  //            ^-- If we are here, we must hide it!
  case FCurCharW of
{$IFDEF WIN32}
    VK_LEFT,
    VK_RIGHT,
    VK_UP,
    VK_DOWN,
    VK_BACK: ReleaseHandle;
{$ENDIF}
{$IFDEF LINUX}
    XK_LEFT,
    XK_RIGHT,
    XK_UP,
    XK_DOWN,
    XK_BackSpace: ReleaseHandle;
{$ENDIF}
  else
    // Braces dont match? Then hide ..
    if nBraces <> 0 then
      ReleaseHandle;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseCodeToolTip.WMNCHitTest(var Message: TWMNCHitTest);
var
  Pt: TPoint;
begin    
  Message.Result := HTTRANSPARENT;
  Pt := ScreenToClient(Point(Message.XPos,Message.YPos));

  // hitcheck against the position of our both buttons
  if PtInRect(FUpButton.ClientRect, Pt) or PtInRect(FDownButton.ClientRect, Pt) then
  begin
    Message.Result := HTCLIENT;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseCodeToolTip.WMEraseBkgnd(var Message: TWMEraseBkgnd);
// override WMEraseBkgnd to avoid flickering
begin
  Message.Result := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

initialization
  MakeIdentTable;  
  
finalization

  
end.
