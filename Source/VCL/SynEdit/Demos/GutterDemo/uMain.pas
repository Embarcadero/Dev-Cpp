unit uMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdActns, ActnList, StdCtrls, ExtCtrls,
  SynEdit, SynEditHighlighter, SynHighlighterDWS;

type
  TFormMain = class(TForm)
    CheckBoxAutoSize: TCheckBox;
    CheckBoxCustomLineNumbers: TCheckBox;
    CheckBoxCustomPaint: TCheckBox;
    CheckBoxDirect2D: TCheckBox;
    CheckBoxGradient: TCheckBox;
    CheckBoxLeadingZeroes: TCheckBox;
    CheckBoxShowLineNumbers: TCheckBox;
    CheckBoxShowModifications: TCheckBox;
    CheckBoxUseFontStyle: TCheckBox;
    CheckBoxVisible: TCheckBox;
    CheckBoxZeroStart: TCheckBox;
    ColorDialog: TColorDialog;
    LabelBorderColor: TLabel;
    LabelColor: TLabel;
    LabelGradientStart: TLabel;
    LabelGradientStop: TLabel;
    Panel: TPanel;
    ShapeColorBackground: TShape;
    ShapeColorBorder: TShape;
    ShapeGradientStartColor: TShape;
    ShapeGradientStopColor: TShape;
    SynDWSSyn: TSynDWSSyn;
    SynEdit: TSynEdit;
    procedure FormCreate(Sender: TObject);
    procedure CheckBoxAutoSizeClick(Sender: TObject);
    procedure CheckBoxCustomLineNumbersClick(Sender: TObject);
    procedure CheckBoxCustomPaintClick(Sender: TObject);
    procedure CheckBoxGradientClick(Sender: TObject);
    procedure CheckBoxLeadingZeroesClick(Sender: TObject);
    procedure CheckBoxShowLineNumbersClick(Sender: TObject);
    procedure CheckBoxShowModificationsClick(Sender: TObject);
    procedure CheckBoxUseFontStyleClick(Sender: TObject);
    procedure CheckBoxVisibleClick(Sender: TObject);
    procedure CheckBoxZeroStartClick(Sender: TObject);
    procedure ShapeColorBackgroundMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ShapeColorBorderMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ShapeGradientStartColorMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ShapeGradientStopColorMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SynEditGutterGetText(Sender: TObject; aLine: Integer;
      var aText: string);
    procedure SynEditGutterPaint(Sender: TObject; aLine, X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure CheckBoxDirect2DClick(Sender: TObject);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.DFM}

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  ShapeColorBackground.Brush.Color := SynEdit.Gutter.Color;
  ShapeColorBorder.Brush.Color := SynEdit.Gutter.BorderColor;
  ShapeGradientStartColor.Brush.Color := SynEdit.Gutter.GradientStartColor;
  ShapeGradientStopColor.Brush.Color := SynEdit.Gutter.GradientEndColor;
  CheckBoxAutoSize.Checked := SynEdit.Gutter.AutoSize;
  CheckBoxLeadingZeroes.Checked := SynEdit.Gutter.LeadingZeros;
  CheckBoxShowLineNumbers.Checked := SynEdit.Gutter.ShowLineNumbers;
  CheckBoxShowModifications.Checked := SynEdit.Gutter.ShowModification;
  CheckBoxZeroStart.Checked := SynEdit.Gutter.ZeroStart;
  CheckBoxUseFontStyle.Checked := SynEdit.Gutter.UseFontStyle;
  CheckBoxDirect2D.Checked := SynEdit.UseDirect2D;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  SynEdit.UseDirect2D := False;
end;

procedure TFormMain.CheckBoxAutoSizeClick(Sender: TObject);
begin
  SynEdit.Gutter.AutoSize := TCheckBox(Sender).Checked;
end;

procedure TFormMain.CheckBoxCustomLineNumbersClick(Sender: TObject);
begin
  if CheckBoxCustomLineNumbers.Checked then
    SynEdit.OnGutterGetText := SynEditGutterGetText
  else
    SynEdit.OnGutterGetText := nil;
  SynEdit.InvalidateGutter;
end;

procedure TFormMain.CheckBoxCustomPaintClick(Sender: TObject);
begin
  if CheckBoxCustomPaint.Checked then
    SynEdit.OnGutterPaint := SynEditGutterPaint
  else
    SynEdit.OnGutterPaint := nil;
  SynEdit.InvalidateGutter;
end;

procedure TFormMain.CheckBoxDirect2DClick(Sender: TObject);
begin
  SynEdit.UseDirect2D := TCheckBox(Sender).Checked;
end;

procedure TFormMain.CheckBoxGradientClick(Sender: TObject);
begin
  SynEdit.Gutter.Gradient := TCheckBox(Sender).Checked;
end;

procedure TFormMain.CheckBoxLeadingZeroesClick(Sender: TObject);
begin
  SynEdit.Gutter.LeadingZeros := TCheckBox(Sender).Checked;
end;

procedure TFormMain.CheckBoxShowLineNumbersClick(Sender: TObject);
begin
  SynEdit.Gutter.ShowLineNumbers := TCheckBox(Sender).Checked;
end;

procedure TFormMain.CheckBoxShowModificationsClick(Sender: TObject);
begin
  SynEdit.Gutter.ShowModification := TCheckBox(Sender).Checked;
end;

procedure TFormMain.CheckBoxUseFontStyleClick(Sender: TObject);
begin
  SynEdit.Gutter.UseFontStyle := TCheckBox(Sender).Checked;
end;

procedure TFormMain.CheckBoxVisibleClick(Sender: TObject);
begin
  SynEdit.Gutter.Visible := CheckBoxVisible.Checked;
end;

procedure TFormMain.CheckBoxZeroStartClick(Sender: TObject);
begin
  SynEdit.Gutter.ZeroStart := TCheckBox(Sender).Checked;
end;

procedure TFormMain.ShapeColorBackgroundMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ColorDialog.Color := ShapeColorBackground.Brush.Color;
  if ColorDialog.Execute then
  begin
    ShapeColorBackground.Brush.Color := ColorDialog.Color;
    SynEdit.Gutter.Color := ColorDialog.Color;
  end;
end;

procedure TFormMain.ShapeColorBorderMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ColorDialog.Color := ShapeColorBorder.Brush.Color;
  if ColorDialog.Execute then
  begin
    ShapeColorBorder.Brush.Color := ColorDialog.Color;
    SynEdit.Gutter.BorderColor := ColorDialog.Color;
  end;
end;

procedure TFormMain.ShapeGradientStartColorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ColorDialog.Color := ShapeGradientStartColor.Brush.Color;
  if ColorDialog.Execute then
  begin
    ShapeGradientStartColor.Brush.Color := ColorDialog.Color;
    SynEdit.Gutter.GradientStartColor := ColorDialog.Color;
  end;
end;

procedure TFormMain.ShapeGradientStopColorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ColorDialog.Color := ShapeGradientStopColor.Brush.Color;
  if ColorDialog.Execute then
  begin
    ShapeGradientStopColor.Brush.Color := ColorDialog.Color;
    SynEdit.Gutter.GradientEndColor := ColorDialog.Color;
  end;
end;

procedure TFormMain.SynEditGutterGetText(Sender: TObject; aLine: Integer;
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

procedure TFormMain.SynEditGutterPaint(Sender: TObject; aLine, X, Y: Integer);
var
  StrLineNumber: string;
  LineNumberRect: TRect;
  GutterWidth, Offset: Integer;
  OldFont: TFont;
begin
  with TSynEdit(Sender), Canvas do
  begin
    Brush.Style := bsClear;
    GutterWidth := Gutter.Width - 5;
    if (ALine = 1) or (ALine = CaretY) or ((ALine mod 10) = 0) then
    begin
      StrLineNumber := IntToStr(ALine);
      LineNumberRect := Rect(x, y, GutterWidth, y + LineHeight);
      OldFont := TFont.Create;
      try
        OldFont.Assign(Canvas.Font);
        Canvas.Font := Gutter.Font;
        Canvas.TextRect(LineNumberRect, StrLineNumber, [tfVerticalCenter,
          tfSingleLine, tfRight]);
        Canvas.Font := OldFont;
      finally
        OldFont.Free;
      end;
    end
    else
    begin
      Canvas.Pen.Color := Gutter.Font.Color;
      if (ALine mod 5) = 0 then
        Offset := 5
      else
        Offset := 2;
      Inc(y, LineHeight div 2);
      Canvas.MoveTo(GutterWidth - Offset, y);
      Canvas.LineTo(GutterWidth, y);
    end;
  end;
end;

end.
