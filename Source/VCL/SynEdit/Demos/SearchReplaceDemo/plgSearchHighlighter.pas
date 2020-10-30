unit plgSearchHighlighter;

interface

uses
  Windows, Classes, Graphics, SynEdit, SynEditTypes, SynEditHighlighter,
  SynEditPlugins;

type
  TSearchTextHightlighterSynEditPlugin = class(TSynEditPlugin)
  private
    FAttribute: TSynHighlighterAttributes;
  protected
    procedure AfterPaint(ACanvas: TCanvas; const AClip: TRect;
      FirstLine, LastLine: Integer); override;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    property Attribute: TSynHighlighterAttributes read FAttribute write FAttribute;
  end;

implementation

{ TEditorFrameSynEditPlugin }

procedure TSearchTextHightlighterSynEditPlugin.AfterConstruction;
begin
  inherited;
  FAttribute := TSynHighlighterAttributes.Create('SearchText', 'Search Text Highlighter');
end;

destructor TSearchTextHightlighterSynEditPlugin.Destroy;
begin
  FAttribute.Free;
  inherited;
end;

procedure TSearchTextHightlighterSynEditPlugin.AfterPaint(ACanvas: TCanvas;
  const AClip: TRect; FirstLine, LastLine: Integer);
var
  SearchText, SearchResultText: string;
  Pt: TPoint;
  Rct: TRect;
  OldFont: TFont;
  LineIndex, Count, ItemIndex: Integer;
  CurrCoord: TBufferCoord;
begin
  inherited;

  if not Assigned(Editor.SearchEngine) then
    Exit;

  SearchText := Editor.SearchEngine.Pattern;
  if SearchText = '' then
    Exit;

  OldFont := TFont.Create;
  try
    OldFont.Assign(ACanvas.Font);

    if Attribute.Background <> clNone then
    begin
      ACanvas.Brush.Color := Attribute.Background;
      ACanvas.Brush.Style := bsSolid
    end
    else
      ACanvas.Brush.Style := bsClear;

    if Attribute.Foreground <> clNone then
    begin
      ACanvas.Pen.Color := Attribute.Foreground;
      ACanvas.Pen.Style := psSolid;
    end
    else
      ACanvas.Pen.Style := psClear;

    for LineIndex := FirstLine to LastLine do
    begin
      Count := Editor.SearchEngine.FindAll(Editor.Lines[LineIndex - 1]);
      for ItemIndex := 0 to Count - 1 do
      begin
        CurrCoord := BufferCoord(Editor.SearchEngine.Results[ItemIndex], LineIndex);
        if CurrCoord = Editor.BlockBegin then
          Continue;

        SearchResultText := Copy(Editor.Lines[LineIndex - 1], Editor.SearchEngine.Results[ItemIndex],
          Editor.SearchEngine.Lengths[ItemIndex]);

        Pt := Editor.RowColumnToPixels(Editor.BufferToDisplayPos(CurrCoord));
        Rct := Rect(Pt.X, Pt.Y, Pt.X + Editor.CharWidth * Length(SearchResultText),
          Pt.Y + Editor.LineHeight);

        ACanvas.FillRect(Rct);
        ACanvas.TextRect(Rct, Pt.X, Pt.Y, SearchResultText);
      end
    end;

    ACanvas.Font.Assign(OldFont);
  finally
    OldFont.Free;
  end;
end;

end.

