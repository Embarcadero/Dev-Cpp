unit SynMiniMap;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Math, ExtCtrls, SynEdit, SynEditHighlighter, SynHighlighterDWS;

type
  TFormSynEditMinimap = class(TForm)
    SynEdit: TSynEdit;
    SynEditMiniMap: TSynEdit;
    Splitter: TSplitter;
    SynDWSSyn: TSynDWSSyn;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure SynEditStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure SynEditMiniMapSpecialLineColors(Sender: TObject; Line: Integer;
      var Special: Boolean; var FG, BG: TColor);
    procedure SynEditMiniMapMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SynEditMiniMapEnter(Sender: TObject);
  end;

var
  FormSynEditMinimap: TFormSynEditMinimap;

implementation

{$R *.dfm}

{ TFormSynEditMinimap }

procedure TFormSynEditMinimap.FormCreate(Sender: TObject);
begin
  // double buffer both SynEdit instances
  SynEditMiniMap.DoubleBuffered := True;
  SynEdit.DoubleBuffered := True;
  SynEditMiniMap.SetLinesPointer(SynEdit);
end;

procedure TFormSynEditMinimap.FormResize(Sender: TObject);
begin
  SynEditStatusChange(Self, []);
end;

procedure TFormSynEditMinimap.SynEditMiniMapEnter(Sender: TObject);
begin
  SynEdit.SetFocus;
end;

procedure TFormSynEditMinimap.SynEditMiniMapMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Coord: TDisplayCoord;
begin
  Coord := SynEditMiniMap.PixelsToNearestRowColumn(X, Y);
  SynEdit.CaretXY := SynEdit.DisplayToBufferPos(Coord);
  SynEdit.Invalidate;
  SynEdit.TopLine := Max(1, Coord.Row - (SynEdit.LinesInWindow div 2));
end;

procedure TFormSynEditMinimap.SynEditMiniMapSpecialLineColors(Sender: TObject; Line: Integer;
  var Special: Boolean; var FG, BG: TColor);
begin
  Special := (Cardinal(Line - SynEdit.TopLine) <= Cardinal(SynEdit.LinesInWindow));
  BG := clBtnFace;
end;

procedure TFormSynEditMinimap.SynEditStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  if SynEditMiniMap.Tag = SynEdit.TopLine then
    Exit;
  SynEditMiniMap.Tag := SynEdit.TopLine;
  SynEditMiniMap.TopLine :=
    Max(1, SynEdit.TopLine - (SynEditMiniMap.LinesInWindow -
    SynEdit.LinesInWindow) div 2);
  SynEditMiniMap.Invalidate;
end;

end.
