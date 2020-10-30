{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: frmMain.pas, released 2000-06-23.

The Original Code is part of the SearchReplaceDemo project, written by
Michael Hieke for the SynEdit component suite.
All Rights Reserved.

Contributors to the SynEdit project are listed in the Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: frmMain.pas,v 1.5.2.3 2009/09/29 00:13:16 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit frmMain;

{$I SynEdit.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ToolWin, ImgList, ActnList, SynEdit, SynEditRegexSearch,
  SynEditMiscClasses, SynEditSearch, SynUnicode;

type
  TSearchReplaceDemoForm = class(TForm)
    ActionFileOpen: TAction;
    ActionListMain: TActionList;
    ActionSearch: TAction;
    ActionSearchNext: TAction;
    ActionSearchPrev: TAction;
    ActionSearchReplace: TAction;
    OpenDialogFile: TOpenDialog;
    ImageListMain: TImageList;
    Statusbar: TStatusBar;
    SynEditor: TSynEdit;
    SynEditRegexSearch: TSynEditRegexSearch;
    SynEditSearch: TSynEditSearch;
    ToolBarMain: TToolBar;
    ToolButtonFileOpen: TToolButton;
    ToolButtonSearch: TToolButton;
    ToolButtonSearchNext: TToolButton;
    ToolButtonSeparator1: TToolButton;
    ToolButtonSeparator2: TToolButton;
    ToolButtonSearchPrev: TToolButton;
    ToolButtonSearchReplace: TToolButton;
    procedure ActionFileOpenExecute(Sender: TObject);
    procedure ActionSearchExecute(Sender: TObject);
    procedure ActionSearchNextExecute(Sender: TObject);
    procedure ActionSearchPrevExecute(Sender: TObject);
    procedure ActionSearchReplaceExecute(Sender: TObject);
    procedure actSearchUpdate(Sender: TObject);
    procedure ActionSearchReplaceUpdate(Sender: TObject);
    procedure SynEditorReplaceText(Sender: TObject; const ASearch,
      AReplace: UnicodeString; Line, Column: Integer;
      var Action: TSynReplaceAction);
    procedure FormCreate(Sender: TObject);
  private
    FSearchFromCaret: Boolean;
    procedure DoSearchReplaceText(AReplace: boolean; ABackwards: boolean);
    procedure ShowSearchReplaceDialog(AReplace: boolean);
  end;

var
  SearchReplaceDemoForm: TSearchReplaceDemoForm;

implementation

{$R *.DFM}

uses
  dlgSearchText, dlgReplaceText, dlgConfirmReplace, plgSearchHighlighter,
  SynEditTypes, SynEditMiscProcs;

  // options - to be saved to the registry
var
  gbSearchBackwards: Boolean;
  gbSearchCaseSensitive: Boolean;
  gbSearchFromCaret: Boolean;
  gbSearchSelectionOnly: Boolean;
  gbSearchTextAtCaret: Boolean;
  gbSearchWholeWords: Boolean;
  gbSearchRegex: Boolean;

  gsSearchText: string;
  gsSearchTextHistory: string;
  gsReplaceText: string;
  gsReplaceTextHistory: string;

resourcestring
  STextNotFound = 'Text not found';

{ TSearchReplaceDemoForm }

procedure TSearchReplaceDemoForm.FormCreate(Sender: TObject);
begin
  with TSearchTextHightlighterSynEditPlugin.Create(SynEditor) do
  begin
    Attribute.Background := $0078AAFF;
  end;
end;

procedure TSearchReplaceDemoForm.DoSearchReplaceText(AReplace: Boolean;
  ABackwards: Boolean);
var
  Options: TSynSearchOptions;
begin
  Statusbar.SimpleText := '';
  if AReplace then
    Options := [ssoPrompt, ssoReplace, ssoReplaceAll]
  else
    Options := [];
  if ABackwards then
    Include(Options, ssoBackwards);
  if gbSearchCaseSensitive then
    Include(Options, ssoMatchCase);
  if not FSearchFromCaret then
    Include(Options, ssoEntireScope);
  if gbSearchSelectionOnly then
    Include(Options, ssoSelectedOnly);
  if gbSearchWholeWords then
    Include(Options, ssoWholeWord);
  if gbSearchRegex then
    SynEditor.SearchEngine := SynEditRegexSearch
  else
    SynEditor.SearchEngine := SynEditSearch;
  if SynEditor.SearchReplace(gsSearchText, gsReplaceText, Options) = 0 then
  begin
    MessageBeep(MB_ICONASTERISK);
    Statusbar.SimpleText := STextNotFound;
    if ssoBackwards in Options then
      SynEditor.BlockEnd := SynEditor.BlockBegin
    else
      SynEditor.BlockBegin := SynEditor.BlockEnd;
    SynEditor.CaretXY := SynEditor.BlockBegin;
  end;

  if ConfirmReplaceDialog <> nil then
    ConfirmReplaceDialog.Free;
end;

procedure TSearchReplaceDemoForm.ShowSearchReplaceDialog(AReplace: Boolean);
var
  dlg: TTextSearchDialog;
begin
  Statusbar.SimpleText := '';
  if AReplace then
    dlg := TTextReplaceDialog.Create(Self)
  else
    dlg := TTextSearchDialog.Create(Self);
  with dlg do try
    // assign search options
    SearchBackwards := gbSearchBackwards;
    SearchCaseSensitive := gbSearchCaseSensitive;
    SearchFromCursor := gbSearchFromCaret;
    SearchInSelectionOnly := gbSearchSelectionOnly;
    // start with last search text
    SearchText := gsSearchText;
    if gbSearchTextAtCaret then begin
      // if something is selected search for that text
      if SynEditor.SelAvail and (SynEditor.BlockBegin.Line = SynEditor.BlockEnd.Line)
      then
        SearchText := SynEditor.SelText
      else
        SearchText := SynEditor.GetWordAtRowCol(SynEditor.CaretXY);
    end;
    SearchTextHistory := gsSearchTextHistory;
    if AReplace then with dlg as TTextReplaceDialog do begin
      ReplaceText := gsReplaceText;
      ReplaceTextHistory := gsReplaceTextHistory;
    end;
    SearchWholeWords := gbSearchWholeWords;
    if ShowModal = mrOK then begin
      gbSearchBackwards := SearchBackwards;
      gbSearchCaseSensitive := SearchCaseSensitive;
      gbSearchFromCaret := SearchFromCursor;
      gbSearchSelectionOnly := SearchInSelectionOnly;
      gbSearchWholeWords := SearchWholeWords;
      gbSearchRegex := SearchRegularExpression;
      gsSearchText := SearchText;
      gsSearchTextHistory := SearchTextHistory;
      if AReplace then with dlg as TTextReplaceDialog do begin
        gsReplaceText := ReplaceText;
        gsReplaceTextHistory := ReplaceTextHistory;
      end;
      FSearchFromCaret := gbSearchFromCaret;
      if gsSearchText <> '' then begin
        DoSearchReplaceText(AReplace, gbSearchBackwards);
        FSearchFromCaret := TRUE;
      end;
    end;
  finally
    dlg.Free;
  end;
end;

{ event handler }

procedure TSearchReplaceDemoForm.ActionFileOpenExecute(Sender: TObject);
begin
  if OpenDialogFile.Execute then begin
    SynEditor.Lines.LoadFromFile(OpenDialogFile.FileName);
    SynEditor.ReadOnly := ofReadOnly in OpenDialogFile.Options;
  end;
end;

procedure TSearchReplaceDemoForm.ActionSearchExecute(Sender: TObject);
begin
  ShowSearchReplaceDialog(FALSE);
end;

procedure TSearchReplaceDemoForm.ActionSearchNextExecute(Sender: TObject);
begin
  DoSearchReplaceText(FALSE, FALSE);
end;

procedure TSearchReplaceDemoForm.ActionSearchPrevExecute(Sender: TObject);
begin
  DoSearchReplaceText(FALSE, TRUE);
end;

procedure TSearchReplaceDemoForm.ActionSearchReplaceExecute(Sender: TObject);
begin
  ShowSearchReplaceDialog(TRUE);
end;

procedure TSearchReplaceDemoForm.actSearchUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := gsSearchText <> '';
end;

procedure TSearchReplaceDemoForm.ActionSearchReplaceUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (gsSearchText <> '')
    and not SynEditor.ReadOnly;
end;

procedure TSearchReplaceDemoForm.SynEditorReplaceText(Sender: TObject;
  const ASearch, AReplace: UnicodeString; Line, Column: Integer;
  var Action: TSynReplaceAction);
var
  APos: TPoint;
  EditRect: TRect;
begin
  if ASearch = AReplace then
    Action := raSkip
  else begin
    APos := SynEditor.ClientToScreen(
      SynEditor.RowColumnToPixels(
      SynEditor.BufferToDisplayPos(
        BufferCoord(Column, Line) ) ) );
    EditRect := ClientRect;
    EditRect.TopLeft := ClientToScreen(EditRect.TopLeft);
    EditRect.BottomRight := ClientToScreen(EditRect.BottomRight);

    if ConfirmReplaceDialog = nil then
      ConfirmReplaceDialog := TConfirmReplaceDialog.Create(Application);
    ConfirmReplaceDialog.PrepareShow(EditRect, APos.X, APos.Y,
      APos.Y + SynEditor.LineHeight, ASearch);
    case ConfirmReplaceDialog.ShowModal of
      mrYes: Action := raReplace;
      mrYesToAll: Action := raReplaceAll;
      mrNo: Action := raSkip;
      else Action := raCancel;
    end;
  end;
end;

end.
