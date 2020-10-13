{
    This file is part of Dev-C++
    Copyright (c) 2004 Bloodshed Software

    Dev-C++ is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Dev-C++ is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Dev-C++; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit LangFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, Menus, FileCtrl, SynEdit, ToolWin, ComCtrls, Themes,
  svgColor, Vcl.VirtualImage;

type
  TLangForm = class(TForm)
    OkBtn: TBitBtn;
    LangPanel: TPanel;
    lbLanguages: TListBox;
    grpLanguages: TGroupBox;
    lblLangInfo: TLabel;
    grpThemes: TGroupBox;
    cmbTheme: TComboBox;
    FinishPanel: TPanel;
    Finish2: TLabel;
    Finish3: TLabel;
    cmbColors: TComboBox;
    lblTheme: TLabel;
    lblColor: TLabel;
    Finish1: TLabel;
    synExample: TSynEdit;
    EditPanel: TPanel;
    lblEditInfo: TLabel;
    lblFont: TLabel;
    cmbFont: TComboBox;
    VirtualImageTheme: TVirtualImage;
    procedure OkBtnClick(Sender: TObject);
    procedure ColorChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FontChange(Sender: TObject);
    procedure cmbThemeChange(Sender: TObject);
    procedure cmbFontDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure HandleLangPanel;
    procedure HandleEditPanel;
    procedure UpdateLangList(List: TStrings);
    procedure LoadText;
    procedure FormCreate(Sender: TObject); // call after selecting a language of course
  end;

implementation

uses
  MultiLangSupport, DataFrm, devcfg, utils, main, version, ImageTheme, SynEditTypes;

{$R *.dfm}

procedure TLangForm.LoadText;
begin
  grpThemes.Caption := Lang[ID_LANGFORM_SELECTTHEME];
  lblFont.Caption := Lang[ID_LANGFORM_FONT];
  lblColor.Caption := Lang[ID_LANGFORM_COLOR];
  lblTheme.Caption := Lang[ID_LANGFORM_ICONS];
  lblEditInfo.Caption := Lang[ID_LANGFORM_THEMCHANGEHINT];
  Finish1.Caption := Lang[ID_LANGFORM_FINISH1];
  Finish2.Caption := Lang[ID_LANGFORM_FINISH2];
  Finish3.Caption := Lang[ID_LANGFORM_FINISH3];
  OkBtn.Caption := Lang[ID_LANGFORM_NEXT];
end;

procedure TLangForm.UpdateLangList(List: TStrings);
var
  I, sel: integer;
begin
  lbLanguages.Items.BeginUpdate;
  try
    lbLanguages.Clear;
    for I := 0 to List.Count - 1 do begin
      sel := lbLanguages.Items.Add(List.ValueFromIndex[I]);
      if StartsText('english', lbLanguages.Items[sel]) then
        lbLanguages.Selected[sel] := True;
    end;
  finally
    lbLanguages.Items.EndUpdate;
  end;
end;

procedure TLangForm.HandleLangPanel;
var
  SelectedLang: String;
begin
  OkBtn.Tag := 1;
  LangPanel.Visible := false;

  // Update translation
  if lbLanguages.ItemIndex <> -1 then begin
    SelectedLang := Lang.Langs.Names[lbLanguages.ItemIndex];
    Lang.Open(SelectedLang);
    devData.Language := Lang.FileFromDescription(SelectedLang);
  end else begin
    Lang.Open('English.lng');
  end;
  LoadText;

  EditPanel.Visible := true;
end;

procedure TLangForm.HandleEditPanel;
begin
  OkBtn.Tag := 2;
  OkBtn.Kind := bkOK;
  OkBtn.ModalResult := mrOK;
  EditPanel.Visible := false;
  FinishPanel.Visible := true;
  //devData.ThemeChange := true;
  //devData.Theme := cmbIcons.Items[cmbIcons.ItemIndex];
  devData.Theme := 'NewLook';
  devData.Style := cmbTheme.ItemIndex;
  devData.StyleChange := True;
end;

procedure TLangForm.OkBtnClick(Sender: TObject);
begin
  case OkBtn.Tag of
    0: HandleLangPanel;
    1: HandleEditPanel;
  end;
end;

procedure TLangForm.FormShow(Sender: TObject);
var
  FontIndex: Integer;
begin
  // Set interface font
  Font.Name := devData.InterfaceFont;
  Font.Size := devData.InterfaceFontSize;

  // Set demo caret
  synExample.CaretXY := BufferCoord(11, 5);

  // Interface themes
  cmbTheme.ItemIndex := devData.Style;
  VirtualImageTheme.ImageIndex := devData.Style;

  // Editor colors
  cmbColors.ItemIndex := 1; // Classic Plus
  dmMain.InitHighlighterFirstTime(cmbColors.ItemIndex);
  devEditor.AssignEditor(synExample, 'main.cpp');

  // Font options
  cmbFont.Items.Assign(Screen.Fonts);
  FontIndex := cmbFont.Items.IndexOf('Source Code Pro');
  if FontIndex = -1 then
    FontIndex := cmbFont.Items.IndexOf('Consolas');
  if FontIndex = -1 then
    FontIndex := cmbFont.Items.IndexOf('Courier');
  cmbFont.ItemIndex := FontIndex; // set ItemIndex once

  // Populate language list
  UpdateLangList(Lang.GetLangList);
  lbLanguages.SetFocus;
end;

procedure TLangForm.ColorChange(Sender: TObject);
begin
  dmMain.InitHighlighterFirstTime(cmbColors.ItemIndex);

  // Pick a proper current line color (choice is up for debate...)
  if cmbColors.Text = 'Obsidian' then
    devEditor.HighColor := clBlack
  else if cmbColors.Text = 'Twilight' then
    devEditor.HighColor := $202020
  else if cmbColors.Text = 'Borland' then
    devEditor.HighColor := $202020
  else if cmbColors.Text = 'Matrix' then
    devEditor.HighColor := $202020 // dark brown
  else if cmbColors.Text = 'GSS Hacker' then
    devEditor.HighColor := clBlack
  else if cmbColors.Text = 'Obvilion' then
    devEditor.HighColor := clBlack
  else if cmbColors.Text = 'PlasticCodeWrap' then
    devEditor.HighColor := clBlack
  else if cmbColors.Text = 'Monokai' then
    devEditor.HighColor := clBlack
  else if cmbColors.Text = 'Monokai Fresh' then
    devEditor.HighColor := clBlack
  else
    devEditor.HighColor := $FFFFCC; // Light Turquoise

  devEditor.AssignEditor(synExample, 'main.cpp');
end;

procedure TLangForm.FontChange(Sender: TObject);
begin
  devEditor.Font.Name := cmbFont.Text;
  devEditor.Gutterfont.Name := cmbFont.Text;
  devEditor.AssignEditor(synExample, 'main.cpp');
end;

procedure TLangForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TLangForm.FormCreate(Sender: TObject);
begin
  //if devData.Style <> 0 then
    TStyleManager.TrySetStyle(cDelphiStyle[devData.Style]);
end;

procedure TLangForm.cmbThemeChange(Sender: TObject);
begin
  {if cmbIcons.ItemIndex = 1 then
    tbExample.Images := dmMain.MenuImages_Gnome
  else if cmbIcons.ItemIndex = 2 then
    tbExample.Images := dmMain.MenuImages_Blue
  else
    tbExample.Images := dmMain.MenuImages_NewLook;}
  VirtualImageTheme.ImageIndex := cmbTheme.ItemIndex;
end;

procedure TLangForm.cmbFontDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  alignleft: integer;
  aligntop: integer;
begin
  with TComboBox(Control) do begin
    Canvas.Font.Name := Items.Strings[Index];
    Canvas.Font.Size := devEditor.Font.Size;
    Canvas.FillRect(Rect);
    alignleft := (Rect.Right - Rect.Left) div 2 - Canvas.TextWidth(Canvas.Font.Name) div 2;
    aligntop := Rect.Top + (Rect.Bottom - Rect.Top) div 2 - Canvas.TextHeight(Canvas.Font.Name) div 2;
    Canvas.TextOut(alignleft, aligntop, Canvas.Font.Name);
  end;
end;

end.

