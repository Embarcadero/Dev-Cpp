unit Unit1;
(*
   Unit documentation
*)


interface
{$REGION interface}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, SynEdit, Vcl.Menus,
  Vcl.StdActns, Vcl.ActnList, System.Actions, Vcl.ActnPopup, Vcl.ToolWin,
{$IFDEF CPPU64}
  Vcl.ActnMan, Vcl.ActnCtrls, Vcl.ActnMenus, Vcl.PlatformDefaultStyleActnCtrls,
  SynEditPrint, SynEditPythonBehaviour, SynHighlighterPython,
{$ENDIF CPPU64}
  SynEditCodeFolding, SynHighlighterJScript, SynEditHighlighter,
  SynHighlighterCpp, SynHighlighterDWS;

type

  TPerson = record
    Name: string;
    SurName: string;
  end;

  TForm1 = class(TForm)
    ActionManager1: TActionManager;
    ActionMainMenuBar1: TActionMainMenuBar;
    PopupActionBar1: TPopupActionBar;
    FileOpen1: TFileOpen;
    FileSaveAs1: TFileSaveAs;
    FilePrintSetup1: TFilePrintSetup;
    FilePageSetup1: TFilePageSetup;
    FileExit1: TFileExit;
    DialogPrintDlg1: TPrintDlg;
    EditCut1: TEditCut;
    EditCopy1: TEditCopy;
    EditPaste1: TEditPaste;
    EditSelectAll1: TEditSelectAll;
    EditUndo1: TEditUndo;
    EditDelete1: TEditDelete;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    SynEdit1: TSynEdit;
    DialogFontEdit1: TFontEdit;
    ActSave: TAction;
    SynEditPrint1: TSynEditPrint;
    actGutterLines: TAction;
    SynCppSyn1: TSynCppSyn;
    SynJScriptSyn1: TSynJScriptSyn;
    SynPythonSyn1: TSynPythonSyn;
    PythonBehaviour: TSynEditPythonBehaviour;
    actCPP: TAction;
    actJavaScript: TAction;
    actPython: TAction;
    actCodeFolding: TAction;
    actFoldAll: TAction;
    actUnFoldAll: TAction;
    actFoldNearest: TAction;
    actFoldRegions: TAction;
    actFoldLevel1: TAction;
    actFoldLevel2: TAction;
    actFoldLevel3: TAction;
    actUnfoldNearest: TAction;
    actUnfoldRegions: TAction;
    actUnfoldLevel1: TAction;
    actUnfoldLevel2: TAction;
    actUnfoldLevel3: TAction;
    N1: TMenuItem;
    N2: TMenuItem;
    Fold1: TMenuItem;
    All1: TMenuItem;
    Nearest1: TMenuItem;
    Ranges1: TMenuItem;
    N3: TMenuItem;
    Level11: TMenuItem;
    Level21: TMenuItem;
    Level31: TMenuItem;
    Unfold1: TMenuItem;
    All2: TMenuItem;
    Nearest2: TMenuItem;
    Ranges2: TMenuItem;
    N4: TMenuItem;
    Level12: TMenuItem;
    Level22: TMenuItem;
    Level32: TMenuItem;
    actShowCollapsedMarks: TAction;
    actShowCollapsedLines: TAction;
    actFoldShapeSize: TAction;
    SynDWSSyn1: TSynDWSSyn;
    actDWS: TAction;
    procedure FileOpen1Accept(Sender: TObject);
    procedure FileSaveAs1Accept(Sender: TObject);
    procedure ActSaveExecute(Sender: TObject);
    procedure DialogPrintDlg1Accept(Sender: TObject);
    procedure actGutterLinesExecute(Sender: TObject);
    procedure actCPPExecute(Sender: TObject);
    procedure actJavaScriptExecute(Sender: TObject);
    procedure actPythonExecute(Sender: TObject);
    procedure actCodeFoldingExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ScanForFoldRanges(Sender: TObject; TopFoldRanges:
      TSynFoldRanges; LinesToScan: TStrings; FromLine: Integer;
      ToLine: Integer);
    procedure DialogFontEdit1BeforeExecute(Sender: TObject);
    procedure actFoldExecute(Sender: TObject);
    procedure actFoldUpdate(Sender: TObject);
    procedure ActionManager1Update(Action: TBasicAction; var Handled: Boolean);
    procedure actShowCollapsedLinesExecute(Sender: TObject);
    procedure actShowCollapsedMarksExecute(Sender: TObject);
    procedure actFoldShapeSizeExecute(Sender: TObject);
    procedure actDWSExecute(Sender: TObject);
  private
    { Private declarations }
    Highlighters : TStringList;
  public
    { Public declarations }
    FileName : String;
  end;

var
  Form1: TForm1;
{$ENDREGION interface}
implementation

uses
  SynEditTextBuffer,
  SynEditTypes,
  uHighlighterProcs, SynEditKeyCmds;

{$R *.dfm}

function CountBits(const AValue: Longword): Byte;
asm
  mov  ecx, eax
  xor  al, al
  test ecx, ecx
  jz   @@ending
 @@counting:
  shr  ecx, 1
  adc  al, 0
  test ecx, ecx
  jnz  @@counting
 @@ending:
end;

procedure TForm1.actCodeFoldingExecute(Sender: TObject);
begin
  SynEdit1.UseCodeFolding := actCodeFolding.Checked;
end;

procedure TForm1.actCPPExecute(Sender: TObject);
begin
  PythonBehaviour.Editor := nil;
  SynEdit1.OnScanForFoldRanges := ScanForFoldRanges;
  SynEdit1.Highlighter := SynCppSyn1;
end;

procedure TForm1.actDWSExecute(Sender: TObject);
begin
  PythonBehaviour.Editor := nil;
  SynEdit1.OnScanForFoldRanges := nil;
  SynEdit1.Highlighter := SynDwsSyn1;
end;

procedure TForm1.actFoldExecute(Sender: TObject);
begin
  SynEdit1.ExecuteCommand(TAction(Sender).Tag, ' ', nil);
end;

procedure TForm1.actFoldShapeSizeExecute(Sender: TObject);
var
  S : String;
  Size : Integer;
begin
  Size := SynEdit1.CodeFolding.GutterShapeSize;
  S := InputBox('New Gutter Square Size', 'New size in pixels (odd number):', IntToStr(Size));
  if TryStrToInt(S, Size) then
    SynEdit1.CodeFolding.GutterShapeSize := Size;
end;

procedure TForm1.actFoldUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := SynEdit1.UseCodeFolding;
end;

procedure TForm1.actGutterLinesExecute(Sender: TObject);
begin
  Synedit1.Gutter.ShowLineNumbers := actGutterLines.Checked;
end;

procedure TForm1.ActionManager1Update(Action: TBasicAction;
  var Handled: Boolean);
begin
  actCodeFolding.Checked := SynEdit1.UseCodeFolding;
  actShowCollapsedMarks.Checked := SynEdit1.CodeFolding.ShowCollapsedLine;
  actShowCollapsedMarks.Checked := SynEdit1.CodeFolding.ShowHintMark;
end;

procedure TForm1.actJavaScriptExecute(Sender: TObject);
begin
  PythonBehaviour.Editor := nil;
  SynEdit1.OnScanForFoldRanges := nil;
  SynEdit1.Highlighter := SynJScriptSyn1;
end;

procedure TForm1.actPythonExecute(Sender: TObject);
begin
  PythonBehaviour.Editor := Synedit1;
  PythonBehaviour.Editor := nil;
  SynEdit1.Highlighter := SynPythonSyn1;
end;

procedure TForm1.ActSaveExecute(Sender: TObject);
begin
  if FileName = '' then
    FileSaveAs1.Execute
  else
    SynEdit1.Lines.SaveToFile(FileName);
end;

procedure TForm1.actShowCollapsedLinesExecute(Sender: TObject);
begin
  SynEdit1.CodeFolding.ShowCollapsedLine := TAction(Sender).Checked;
end;

procedure TForm1.DialogFontEdit1BeforeExecute(Sender: TObject);
begin
  DialogFontEdit1.Dialog.Font := SynEdit1.Font;
end;

procedure TForm1.DialogPrintDlg1Accept(Sender: TObject);
begin
  SynEditPrint1.SynEdit := SynEdit1;
  SynEditPrint1.Print;
end;

procedure TForm1.FileOpen1Accept(Sender: TObject);
begin
  FileName := FileOpen1.Dialog.FileName;
  SynEdit1.Lines.LoadFromFile(FileName);
  SynEdit1.Highlighter := GetHighlighterFromFileExt(Highlighters, ExtractFileExt(FileName));
  if SynEdit1.Highlighter = SynPythonSyn1 then
    PythonBehaviour.Editor := SynEdit1
  else
    PythonBehaviour.Editor := nil;
  if SynEdit1.Highlighter = SynCppSyn1 then
    SynEdit1.OnScanForFoldRanges := ScanForFoldRanges
  else
    SynEdit1.OnScanForFoldRanges := nil;
  SynEdit1.UseCodeFolding := actCodeFolding.Checked;
end;

procedure TForm1.FileSaveAs1Accept(Sender: TObject);
begin
  FileName := FileSaveAs1.Dialog.FileName;
  SynEdit1.Lines.SaveToFile(FileName);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Highlighters := TStringList.Create;
  GetHighlighters(Self, Highlighters, False);
  FileOpen1.Dialog.Filter :=  GetHighlightersFilter(Highlighters);
  FileOpen1.Dialog.InitialDir := ExtractFilePath(Application.ExeName);
  FileSaveAs1.Dialog.Filter :=  FileOpen1.Dialog.Filter;

  actFoldAll.Tag := ecFoldAll;
  actFoldNearest.Tag := ecFoldNearest;
  actFoldRegions.Tag := ecFoldRegions;
  actFoldLevel1.Tag := ecFoldLevel1;
  actFoldLevel2.Tag := ecFoldLevel2;
  actFoldLevel3.Tag := ecFoldLevel3;
  actUnFoldAll.Tag := ecUnfoldAll;
  actUnfoldNearest.Tag := ecUnfoldNearest;
  actUnfoldRegions.Tag := ecUnfoldRegions;
  actUnfoldLevel1.Tag := ecUnfoldLevel1;
  actUnfoldLevel2.Tag := ecUnfoldLevel2;
  actUnfoldLevel3.Tag :=  ecUnfoldLevel3;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
 Highlighters.Free;
end;

procedure TForm1.ScanForFoldRanges(Sender: TObject;
  TopFoldRanges: TSynFoldRanges; LinesToScan: TStrings; FromLine,
  ToLine: Integer);
var
  CurLine: String;
  Line: Integer;

  function InsideComment(Line : Integer; Col : Integer): Boolean;
  var
    Token : string;
    Attr : TSynHighlighterAttributes;
  begin
    Result := SynEdit1.GetHighlighterAttriAtRowCol(BufferCoord(Col, Line + 1), Token, Attr) and
      (Attr = SynCppSyn1.CommentAttribute);
  end;

  function LineHasChar(Line: Integer; character: char;
  StartCol : Integer): boolean; // faster than Pos!
  var
    i: Integer;
  begin
    result := false;
    for I := StartCol to Length(CurLine) do begin
      if CurLine[i] = character then begin
        // Char must have proper highlighting (ignore stuff inside comments...)
        if not InsideComment(Line, I) then begin
          result := true;
          break;
        end;
      end;
    end;
  end;

  function FindBraces(Line: Integer) : Boolean;
  var
    Col : Integer;
  begin
    Result := False;

    for Col := 1 to Length(CurLine) do
    begin
      // We've found a starting character
      if CurLine[col] = '{' then
      begin
        // Char must have proper highlighting (ignore stuff inside comments...)
        if not InsideComment(Line, Col) then
        begin
          // And ignore lines with both opening and closing chars in them
          if not LineHasChar(Line, '}', col + 1) then begin
            TopFoldRanges.StartFoldRange(Line + 1, 1);
            Result := True;
          end;
          // Skip until a newline
          break;
        end;
      end else if CurLine[col] = '}' then
      begin
        // Char must have symbol attri too
        if not InsideComment(Line, Col) then
        begin
          // And ignore lines with both opening and closing chars in them
          if not LineHasChar(Line, '{', col + 1) then begin
            TopFoldRanges.StopFoldRange(Line + 1, 1);
            Result := True;
          end;
          // Skip until a newline
          break;
        end;
      end;
    end; // for Col
  end;

function FoldRegion(Line: Integer): Boolean;
var
  S : string;
begin
  Result := False;
  S := TrimLeft(CurLine);
  if Uppercase(Copy(S, 1, 14)) = '#PRAGMA REGION' then
  begin
    TopFoldRanges.StartFoldRange(Line + 1, FoldRegionType);
    Result := True;
  end
  else if Uppercase(Copy(S, 1, 17)) = '#PRAGMA ENDREGION' then
  begin
    TopFoldRanges.StopFoldRange(Line + 1, FoldRegionType);
    Result := True;
  end;
end;

begin
  for Line := FromLine to ToLine do
  begin
    // Deal first with Multiline comments (Fold Type 2)
    if SynHighlighterCpp.TRangeState(TSynEditStringList(LinesToScan).Ranges[Line]) =
      SynHighlighterCpp.rsANSIc
    then
    begin
      if (Line = 0) or (SynHighlighterCpp.TRangeState(TSynEditStringList(LinesToScan).Ranges[Line-1]) <>
        SynHighlighterCpp.rsANSIc)
      then
        TopFoldRanges.StartFoldRange(Line + 1, 2)
      else
        TopFoldRanges.NoFoldInfo(Line + 1);
      Continue;
    end
    else if (Line > 0) and (SynHighlighterCpp.TRangeState(TSynEditStringList(LinesToScan).Ranges[Line-1]) =
      SynHighlighterCpp.rsANSIc)
    then
    begin
      TopFoldRanges.StopFoldRange(Line + 1, 2);
      Continue;
    end;

    // Find Fold regions
    CurLine := LinesToScan[Line];
    if FoldRegion(Line) then
      Continue;

    // Find an braces on this line  (Fold Type 1)
    CurLine := LinesToScan[Line];
    if not FindBraces(Line) then
      TopFoldRanges.NoFoldInfo(Line + 1);
  end; // while Line
end;

procedure TForm1.actShowCollapsedMarksExecute(Sender: TObject);
begin
  SynEdit1.CodeFolding.ShowHintMark := TAction(Sender).Checked;
end;

end.
