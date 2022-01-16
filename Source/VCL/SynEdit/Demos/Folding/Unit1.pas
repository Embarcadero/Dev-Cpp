unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, SynEdit, Vcl.Menus,
  Vcl.StdActns, Vcl.ActnList, System.Actions, Vcl.ActnPopup, Vcl.ToolWin,
  Vcl.ActnMan, Vcl.ActnCtrls, Vcl.ActnMenus, Vcl.PlatformDefaultStyleActnCtrls,
  SynEditPrint, SynEditPythonBehaviour, SynHighlighterPython,
  SynEditCodeFolding, SynHighlighterJScript, SynEditHighlighter,
  SynHighlighterCpp, SynHighlighterDWS, SynHighlighterPas, SynHighlighterXML;

type
  TFormFoldingDemo = class(TForm)
    ActionCodeFolding: TAction;
    ActionCPP: TAction;
    ActionDialogFontEdit: TFontEdit;
    ActionDialogPrintDlg: TPrintDlg;
    ActionDWS: TAction;
    ActionEditCopy: TEditCopy;
    ActionEditCut: TEditCut;
    ActionEditDelete: TEditDelete;
    ActionEditPaste: TEditPaste;
    ActionEditSelectAll: TEditSelectAll;
    ActionEditUndo: TEditUndo;
    ActionFileExit: TFileExit;
    ActionFileOpen: TFileOpen;
    ActionFilePageSetup: TFilePageSetup;
    ActionFilePrintSetup: TFilePrintSetup;
    ActionFileSaveAs: TFileSaveAs;
    ActionFoldAll: TAction;
    ActionFoldLevel1: TAction;
    ActionFoldLevel2: TAction;
    ActionFoldLevel3: TAction;
    ActionFoldNearest: TAction;
    ActionFoldRegions: TAction;
    ActionFoldShapeSize: TAction;
    ActionGutterLines: TAction;
    ActionJavaScript: TAction;
    ActionMainMenuBar: TActionMainMenuBar;
    ActionManager: TActionManager;
    ActionPascal: TAction;
    ActionPython: TAction;
    ActionSave: TAction;
    ActionShowCollapsedLines: TAction;
    ActionShowCollapsedMarks: TAction;
    ActionUnFoldAll: TAction;
    ActionUnfoldLevel1: TAction;
    ActionUnfoldLevel2: TAction;
    ActionUnfoldLevel3: TAction;
    ActionUnfoldNearest: TAction;
    ActionUnfoldRegions: TAction;
    ActionXML: TAction;
    MenuItemCopy: TMenuItem;
    MenuItemCut: TMenuItem;
    MenuItemFold: TMenuItem;
    MenuItemFoldAll: TMenuItem;
    MenuItemFoldLevel1: TMenuItem;
    MenuItemFoldLevel2: TMenuItem;
    MenuItemFoldLevel3: TMenuItem;
    MenuItemFoldNearest: TMenuItem;
    MenuItemFoldRanges: TMenuItem;
    MenuItemPaste: TMenuItem;
    MenuItemUnfold: TMenuItem;
    MenuItemUnfoldAll: TMenuItem;
    MenuItemUnfoldLevel1: TMenuItem;
    MenuItemUnfoldLevel2: TMenuItem;
    MenuItemUnfoldLevel3: TMenuItem;
    MenuItemUnfoldNearest: TMenuItem;
    MenuItemUnfoldRanges: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    PopupActionBar: TPopupActionBar;
    PythonBehaviour: TSynEditPythonBehaviour;
    SynCppSyn: TSynCppSyn;
    SynDWSSyn: TSynDWSSyn;
    SynEdit: TSynEdit;
    SynEditPrint: TSynEditPrint;
    SynJScriptSyn: TSynJScriptSyn;
    SynPasSyn: TSynPasSyn;
    SynPythonSyn: TSynPythonSyn;
    SynXMLSyn: TSynXMLSyn;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ActionCodeFoldingExecute(Sender: TObject);
    procedure ActionCPPExecute(Sender: TObject);
    procedure ActionDialogFontEditBeforeExecute(Sender: TObject);
    procedure ActionDialogPrintDlgAccept(Sender: TObject);
    procedure ActionDWSExecute(Sender: TObject);
    procedure ActionFileOpenAccept(Sender: TObject);
    procedure ActionFileSaveAsAccept(Sender: TObject);
    procedure ActionFoldExecute(Sender: TObject);
    procedure ActionFoldShapeSizeExecute(Sender: TObject);
    procedure ActionFoldUpdate(Sender: TObject);
    procedure ActionGutterLinesExecute(Sender: TObject);
    procedure ActionJavaScriptExecute(Sender: TObject);
    procedure ActionManagerUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure ActionPascalExecute(Sender: TObject);
    procedure ActionPythonExecute(Sender: TObject);
    procedure ActionSaveExecute(Sender: TObject);
    procedure ActionShowCollapsedLinesExecute(Sender: TObject);
    procedure ActionShowCollapsedMarksExecute(Sender: TObject);
    procedure ActionXMLExecute(Sender: TObject);
    procedure SynEditGutterGetText(Sender: TObject; aLine: Integer;
      var aText: string);
    procedure ScanForFoldRanges(Sender: TObject; TopFoldRanges:
      TSynFoldRanges; LinesToScan: TStrings; FromLine: Integer;
      ToLine: Integer);
    procedure SynEditStatusChange(Sender: TObject; Changes: TSynStatusChanges);
  private
    FHighlighters: TStringList;
    FOldCaretY: Integer;
  public
    FileName: string;
  end;

var
  FormFoldingDemo: TFormFoldingDemo;

implementation

uses
  SynEditTextBuffer,
  SynEditTypes,
  SynEditKeyCmds,
  uHighlighterProcs;

{$R *.dfm}

procedure TFormFoldingDemo.ActionCodeFoldingExecute(Sender: TObject);
begin
  SynEdit.UseCodeFolding := ActionCodeFolding.Checked;
end;

procedure TFormFoldingDemo.ActionCPPExecute(Sender: TObject);
begin
  PythonBehaviour.Editor := nil;
  SynEdit.OnScanForFoldRanges := ScanForFoldRanges;
  SynEdit.Highlighter := SynCppSyn;
end;

procedure TFormFoldingDemo.ActionDWSExecute(Sender: TObject);
begin
  PythonBehaviour.Editor := nil;
  SynEdit.OnScanForFoldRanges := nil;
  SynEdit.Highlighter := SynDwsSyn;
end;

procedure TFormFoldingDemo.ActionFoldExecute(Sender: TObject);
begin
  SynEdit.ExecuteCommand(TAction(Sender).Tag, ' ', nil);
end;

procedure TFormFoldingDemo.ActionFoldShapeSizeExecute(Sender: TObject);
var
  S: string;
  Size : Integer;
begin
  Size := SynEdit.CodeFolding.GutterShapeSize;
  S := InputBox('New Gutter Square Size', 'New size in pixels (odd number):', IntToStr(Size));
  if TryStrToInt(S, Size) then
    SynEdit.CodeFolding.GutterShapeSize := Size;
end;

procedure TFormFoldingDemo.ActionFoldUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := SynEdit.UseCodeFolding;
end;

procedure TFormFoldingDemo.ActionGutterLinesExecute(Sender: TObject);
begin
  SynEdit.Gutter.ShowLineNumbers := ActionGutterLines.Checked;
end;

procedure TFormFoldingDemo.ActionManagerUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  ActionCodeFolding.Checked := SynEdit.UseCodeFolding;
  ActionShowCollapsedMarks.Checked := SynEdit.CodeFolding.ShowCollapsedLine;
  ActionShowCollapsedMarks.Checked := SynEdit.CodeFolding.ShowHintMark;
end;

procedure TFormFoldingDemo.ActionXMLExecute(Sender: TObject);
begin
  PythonBehaviour.Editor := nil;
  SynEdit.OnScanForFoldRanges := nil;
  SynEdit.Highlighter := SynXMLSyn;
end;

procedure TFormFoldingDemo.ActionJavaScriptExecute(Sender: TObject);
begin
  PythonBehaviour.Editor := nil;
  SynEdit.OnScanForFoldRanges := nil;
  SynEdit.Highlighter := SynJScriptSyn;
end;

procedure TFormFoldingDemo.ActionPythonExecute(Sender: TObject);
begin
  PythonBehaviour.Editor := SynEdit;
  PythonBehaviour.Editor := nil;
  SynEdit.Highlighter := SynPythonSyn;
end;

procedure TFormFoldingDemo.ActionSaveExecute(Sender: TObject);
begin
  if FileName = '' then
    ActionFileSaveAs.Execute
  else
    SynEdit.Lines.SaveToFile(FileName);
end;

procedure TFormFoldingDemo.ActionShowCollapsedLinesExecute(Sender: TObject);
begin
  SynEdit.CodeFolding.ShowCollapsedLine := TAction(Sender).Checked;
end;

procedure TFormFoldingDemo.ActionDialogFontEditBeforeExecute(Sender: TObject);
begin
  ActionDialogFontEdit.Dialog.Font := SynEdit.Font;
end;

procedure TFormFoldingDemo.ActionDialogPrintDlgAccept(Sender: TObject);
begin
  SynEditPrint.SynEdit := SynEdit;
  SynEditPrint.Print;
end;

procedure TFormFoldingDemo.ActionFileOpenAccept(Sender: TObject);
begin
  FileName := ActionFileOpen.Dialog.FileName;
  SynEdit.Lines.LoadFromFile(FileName);
  SynEdit.Highlighter := GetHighlighterFromFileExt(FHighlighters, ExtractFileExt(FileName));
  if SynEdit.Highlighter = SynPythonSyn then
    PythonBehaviour.Editor := SynEdit
  else
    PythonBehaviour.Editor := nil;
  if SynEdit.Highlighter = SynCppSyn then
    SynEdit.OnScanForFoldRanges := ScanForFoldRanges
  else
    SynEdit.OnScanForFoldRanges := nil;
  SynEdit.UseCodeFolding := ActionCodeFolding.Checked;

  if (SynEdit.Highlighter = SynPythonSyn) or (SynEdit.Highlighter = SynCppSyn) then
    SynEdit.TabWidth := 4
  else
    SynEdit.TabWidth := 2;
end;

procedure TFormFoldingDemo.ActionFileSaveAsAccept(Sender: TObject);
begin
  FileName := ActionFileSaveAs.Dialog.FileName;
  SynEdit.Lines.SaveToFile(FileName);
end;

procedure TFormFoldingDemo.FormCreate(Sender: TObject);
begin
  FHighlighters := TStringList.Create;
  GetHighlighters(Self, FHighlighters, False);
  ActionFileOpen.Dialog.Filter :=  GetHighlightersFilter(FHighlighters);
  ActionFileOpen.Dialog.InitialDir := ExtractFilePath(Application.ExeName);
  ActionFileSaveAs.Dialog.Filter := ActionFileOpen.Dialog.Filter;

  ActionFoldAll.Tag := ecFoldAll;
  ActionFoldNearest.Tag := ecFoldNearest;
  ActionFoldRegions.Tag := ecFoldRegions;
  ActionFoldLevel1.Tag := ecFoldLevel1;
  ActionFoldLevel2.Tag := ecFoldLevel2;
  ActionFoldLevel3.Tag := ecFoldLevel3;
  ActionUnFoldAll.Tag := ecUnfoldAll;
  ActionUnfoldNearest.Tag := ecUnfoldNearest;
  ActionUnfoldRegions.Tag := ecUnfoldRegions;
  ActionUnfoldLevel1.Tag := ecUnfoldLevel1;
  ActionUnfoldLevel2.Tag := ecUnfoldLevel2;
  ActionUnfoldLevel3.Tag :=  ecUnfoldLevel3;
end;

procedure TFormFoldingDemo.FormDestroy(Sender: TObject);
begin
  FHighlighters.Free;
end;

procedure TFormFoldingDemo.ScanForFoldRanges(Sender: TObject;
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
    Result := SynEdit.GetHighlighterAttriAtRowCol(BufferCoord(Col, Line + 1), Token, Attr) and
      (Attr = SynCppSyn.CommentAttribute);
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

procedure TFormFoldingDemo.SynEditGutterGetText(Sender: TObject; aLine: Integer;
  var aText: string);
begin
  if aLine = TSynEdit(Sender).CaretY then
    Exit;

  if aLine mod 10 <> 0 then
    if aLine mod 5 <> 0 then
      aText := '·'
    else
      aText := '-';
end;

procedure TFormFoldingDemo.SynEditStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
Var
  NewCaretY: Integer;
begin
  if (scCaretY in Changes) and SynEdit.Gutter.Visible
    and SynEdit.Gutter.ShowLineNumbers then
  begin
    NewCaretY := SynEdit.CaretY;
    SynEdit.InvalidateGutterLine(FOldCaretY);
    SynEdit.InvalidateGutterLine(NewCaretY);
    FOldCaretY := NewCaretY;
  end;
end;

procedure TFormFoldingDemo.ActionShowCollapsedMarksExecute(Sender: TObject);
begin
  SynEdit.CodeFolding.ShowHintMark := TAction(Sender).Checked;
end;

procedure TFormFoldingDemo.ActionPascalExecute(Sender: TObject);
begin
  PythonBehaviour.Editor := nil;
  SynEdit.OnScanForFoldRanges := nil;
  SynEdit.Highlighter := SynPasSyn;
end;

end.
