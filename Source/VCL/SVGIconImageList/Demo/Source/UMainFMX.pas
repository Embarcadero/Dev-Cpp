unit UMainFMX;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, System.ImageList, FMX.ImgList,
  FMX.Objects, FMX.MultiresBitmap, System.Rtti, System.Messaging,
  FMX.ListBox, FMX.Colors, FMX.Layouts,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.ListView, FMX.Edit, FMX.EditBox, FMX.SpinBox,
  FMX.SVGIconImageList, FMX.SVGIconImage;

type
  TSVGIconImageListForm = class(TForm)
    NextButton: TButton;
    Panel1: TPanel;
    RandomButton: TButton;
    IconsLabel: TLabel;
    CurrentLabel: TLabel;
    AutoSizeCheckBox: TCheckBox;
    PrevButton: TButton;
    ShowEditorButton: TButton;
    ImageView: TListBox;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    SpinBox1: TSpinBox;
    ListBoxItem3: TListBoxItem;
    TopPanel: TPanel;
    Glyph2: TGlyph;
    Glyph1: TGlyph;
    Glyph: TGlyph;
    SVGIconImageList: TSVGIconImageList;
    OpenDialog: TOpenDialog;
    GrayScaleCheckBox: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure NextButtonClick(Sender: TObject);
    procedure RandomButtonClick(Sender: TObject);
    procedure AutoSizeCheckBoxChange(Sender: TObject);
    procedure PrevButtonClick(Sender: TObject);
    procedure ShowEditorButtonClick(Sender: TObject);
    procedure SpinBox1Change(Sender: TObject);
    procedure GrayScaleCheckBoxChange(Sender: TObject);
  private
    procedure UpdateGUI;
  public
    { Public declarations }
  end;

var
  SVGIconImageListForm: TSVGIconImageListForm;

implementation

uses
  System.Math
  {$IFDEF MSWINDOWS}, FMX.SVGIconImageListEditorUnit{$ENDIF}
  , FMX.Consts
  ;

{$R *.fmx}

procedure TSVGIconImageListForm.NextButtonClick(Sender: TObject);
begin
  if SVGIconImageList.Count-1 = Glyph.ImageIndex  then
    Glyph.ImageIndex := 0
  else
    Glyph.ImageIndex := Glyph.ImageIndex +1;
  UpdateGUI;
end;

procedure TSVGIconImageListForm.PrevButtonClick(Sender: TObject);
begin
  if Glyph.ImageIndex = 0 then
    Glyph.ImageIndex := SVGIconImageList.Count-1
  else
    Glyph.ImageIndex := Glyph.ImageIndex -1;
  UpdateGUI;
end;

procedure TSVGIconImageListForm.RandomButtonClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    //Screen.Cursor := crHourGlass;
    try
      SVGIconImageList.LoadFromFiles(OpenDialog.Files);
    finally
      UpdateGUI;
      //Screen.Cursor := crDefault;
    end;
  end;
  Glyph.ImageIndex := SVGIconImageList.Count-1;
  UpdateGUI;
end;

procedure TSVGIconImageListForm.ShowEditorButtonClick(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}EditSVGIconImageList(SVGIconImageList);{$ENDIF}
end;

procedure TSVGIconImageListForm.SpinBox1Change(Sender: TObject);
begin
  SVGIconImageList.Size := Round(SpinBox1.Value);
end;

procedure TSVGIconImageListForm.UpdateGUI;
begin
  IconsLabel.Text := Format('Total icons: %d', [SVGIconImageList.Count]);
  CurrentLabel.Text := Format('Current: %d', [Glyph.ImageIndex]);
end;

procedure TSVGIconImageListForm.AutoSizeCheckBoxChange(Sender: TObject);
begin
  SVGIconImageList.AutoSizeBitmaps := AutoSizeCheckBox.IsChecked;
end;

procedure TSVGIconImageListForm.FormCreate(Sender: TObject);
begin
  {$IFNDEF MSWINDOWS}ShowEditorButton.Visible := False;{$ENDIF}
  UpdateGUI;
end;

procedure TSVGIconImageListForm.GrayScaleCheckBoxChange(Sender: TObject);
begin
  SVGIconImageList.GrayScale := GrayScaleCheckBox.IsChecked;
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
