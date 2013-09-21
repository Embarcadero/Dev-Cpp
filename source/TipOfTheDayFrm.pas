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

unit TipOfTheDayFrm;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, ShellAPI;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Variants, Classes, QGraphics, QControls, QForms,
  QDialogs, QStdCtrls, QComCtrls, QExtCtrls, XPMenu;
{$ENDIF}

type
  TTipOfTheDayForm = class(TForm)
    btnPrev: TButton;
    btnNext: TButton;
    btnClose: TButton;
    chkNotAgain: TCheckBox;
    Panel1: TPanel;
    Image: TImage;
    lblTitle: TLabel;
    lblTip: TLabel;
    Bevel1: TBevel;
    lblUrl: TLabel;
    btnRandom: TButton;
    procedure btnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnPrevClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lblUrlClick(Sender: TObject);
    procedure btnRandomClick(Sender: TObject);
  private
    { Private declarations }
    sl: TStringList;
    TipsCounter: integer;
    HiddenUrl: AnsiString;
    function ConvertMacros(const Str: AnsiString): AnsiString;
    procedure LoadFromFile(const Filename: AnsiString);
    function CurrentTip: AnsiString;
    function NextTip: AnsiString;
    function PreviousTip: AnsiString;
    function RandomTip: AnsiString;
    procedure LoadText;
  end;

implementation

uses 
  devcfg, MultiLangSupport;

{$R *.dfm}

procedure TTipOfTheDayForm.btnCloseClick(Sender: TObject);
begin
	Close;
end;

function TTipOfTheDayForm.ConvertMacros(const Str: AnsiString): AnsiString;
var
  idx: integer;
  url: AnsiString;
  urldesc: AnsiString;
begin
  // <CR> macro
  Result := StringReplace(Str, '<CR>', #10, [rfReplaceAll]);

  // <URL> and <UDESC> macros
  url := '';
  urldesc := '';
  idx := Pos('<URL>', Result);
  if idx > 0 then begin
    url := Copy(Result, idx + 5, MaxInt);
    Delete(Result, idx, MaxInt);
    idx := Pos('<UDESC>', url);
    lblUrl.Visible := True;
    if idx > 0 then begin
      urldesc := Copy(url, idx + 7, MaxInt);
      Delete(url, idx, MaxInt);
    end;
    if urldesc = '' then
      urldesc := url;
    lblUrl.Caption := urldesc;
    HiddenUrl := url;
    lblUrl.Visible := True;
  end
  else
    lblUrl.Visible := False;
end;

procedure TTipOfTheDayForm.FormCreate(Sender: TObject);
var
  S: AnsiString;
  LangNoExt: AnsiString;
  ExtPos: integer;
begin
	LoadText;
	chkNotAgain.Checked:=not devData.ShowTipsOnStart;
	TipsCounter := devData.LastTip;
	sl := TStringList.Create;

	lblUrl.Visible := False;
	LangNoExt := Lang.FileFromDescription(devData.Language);
	ExtPos := Pos(ExtractFileExt(LangNoExt), LangNoExt);
	Delete(LangNoExt, ExtPos, MaxInt);
	S := devDirs.Lang + ExtractFileName(LangNoExt) + '.tips';
	if not FileExists(S) then
		S := devDirs.Lang + 'English.tips';
	if not FileExists(S) then begin
		btnNext.Enabled := False;
		btnPrev.Enabled := False;
	end else begin
		LoadFromFile(S);
		if (TipsCounter < 0) or (TipsCounter >= sl.Count) then
			TipsCounter := 0;
		if sl.Count > 0 then
			lblTip.Caption := CurrentTip
		else begin
			btnNext.Enabled := False;
			btnPrev.Enabled := False;
		end;
	end;
end;

procedure TTipOfTheDayForm.FormDestroy(Sender: TObject);
begin
	sl.Free;
end;

function TTipOfTheDayForm.CurrentTip: AnsiString;
begin
  Result := ConvertMacros(sl[TipsCounter]);
end;

function TTipOfTheDayForm.NextTip: AnsiString;
begin
  if TipsCounter < sl.Count - 1 then
    Inc(TipsCounter)
  else
    TipsCounter := 0;
  Result := ConvertMacros(sl[TipsCounter]);
end;

function TTipOfTheDayForm.PreviousTip: AnsiString;
begin
  if TipsCounter > 0 then
    Dec(TipsCounter)
  else
    TipsCounter := sl.Count - 1;
  Result := ConvertMacros(sl[TipsCounter]);
end;

function TTipOfTheDayForm.RandomTip: AnsiString;
var
	newval : integer;
begin
	Randomize;
	repeat
		// Make sure the same tip is never shown twice in a row
		newval := 1 + Random(sl.Count - 1);
	until newval <> TipsCounter;
	TipsCounter := newval;
	Result := ConvertMacros(sl[TipsCounter]);
end;

procedure TTipOfTheDayForm.btnNextClick(Sender: TObject);
begin
  lblTip.Caption := NextTip;
end;

procedure TTipOfTheDayForm.btnPrevClick(Sender: TObject);
begin
  lblTip.Caption := PreviousTip;
end;

procedure TTipOfTheDayForm.btnRandomClick(Sender: TObject);
begin
  lblTip.Caption := RandomTip;
end;

procedure TTipOfTheDayForm.LoadFromFile(const Filename: AnsiString);
var
  I: integer;
begin
  try
    sl.LoadFromFile(Filename);
    I := 0;
    while I < sl.Count do begin
      if Trim(sl[I]) = '' then
        sl.Delete(I) // delete empty lines
      else if Trim(sl[I])[1] = '#' then
        sl.Delete(I) // delete lines starting with '#' (comments)
      else
        Inc(I);
    end;
  finally
  end;
end;

procedure TTipOfTheDayForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
	devData.LastTip:=TipsCounter+1;
	devData.ShowTipsOnStart:=not chkNotAgain.Checked;
	Action := caFree;
end;

procedure TTipOfTheDayForm.lblUrlClick(Sender: TObject);
begin
  ShellExecute(0, 'open', PAnsiChar(HiddenUrl), '', '', SW_SHOWNORMAL);
end;

procedure TTipOfTheDayForm.LoadText;
begin

	Font.Name := devData.InterfaceFont;
	Font.Size := devData.InterfaceFontSize;

  Caption := Lang[ID_TIPS_CAPTION];
  lblTitle.Caption := Lang[ID_TIPS_DIDYOUKNOW];
  lblTip.Caption := Lang[ID_TIPS_NOTIPSTODISPLAY];
  chkNotAgain.Caption := Lang[ID_TIPS_DONTSHOWTIPS];
  btnNext.Caption := Lang[ID_TIPS_NEXTTIP];
  btnPrev.Caption := Lang[ID_TIPS_PREVIOUSTIP];
  btnClose.Caption := Lang[ID_BTN_CLOSE];
end;

end.

