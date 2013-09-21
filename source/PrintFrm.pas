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

unit PrintFrm;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, MultiLangSupport, Spin;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QButtons, MultiLangSupport, QComCtrls;
{$ENDIF}

type
  TPrintForm = class(TForm)
    btnCancel: TBitBtn;
    btnOk: TBitBtn;
    grpParams: TGroupBox;
    cbColors: TCheckBox;
    cbHighlight: TCheckBox;
    rbLN: TRadioButton;
    rbLNMargin: TRadioButton;
    cbWordWrap: TCheckBox;
    grpPages: TGroupBox;
    lblCopies: TLabel;
    seCopies: TSpinEdit;
    cbSelection: TCheckBox;
    rbNoLN: TRadioButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  public
    procedure LoadText;
  end;

//var
//  PrintForm: TPrintForm;

implementation

uses 
  devcfg;

{$R *.dfm}

procedure TPrintForm.LoadText;
begin
	// Set interface font
	Font.Name := devData.InterfaceFont;
	Font.Size := devData.InterfaceFontSize;

  Caption:=                 Lang[ID_PRT];
  grpParams.Caption:=       '  '+Lang[ID_PRT_GRP_PARAMS] +'  ';
  grpPages.Caption:=        '  '+Lang[ID_PRT_GRP_PAGES]+'  ';
  cbColors.Caption:=        Lang[ID_PRT_COLORS];
  cbHighlight.Caption:=     Lang[ID_PRT_HIGHLIGHT];
  cbWordWrap.Caption:=      Lang[ID_PRT_WORDWRAP];
  rbLN.Caption:=            Lang[ID_PRT_PRTLINENUM];
  rbLNMargin.Caption:=      Lang[ID_PRT_PRTLINENUMMAR];
  rbNoLN.Caption:=          Lang[ID_PRT_NOPRTLINENUMMAR];
  lblCopies.Caption:=       Lang[ID_PRT_COPIES];
  cbSelection.Caption:=     Lang[ID_PRT_SELONLY];

  btnOk.Caption:=           Lang[ID_BTN_OK];
  btnCancel.Caption:=       Lang[ID_BTN_CANCEL];
end;

procedure TPrintForm.FormCreate(Sender: TObject);
begin
  LoadText;
  cbColors.Checked := devData.PrintColors;
  cbHighlight.Checked := devData.PrintHighlight;
  cbWordWrap.Checked := devData.PrintWordWrap;
  rbLN.Checked := devData.PrintLineNumbers;
  rbNoLN.Checked := not devData.PrintLineNumbers;
  rbLNMargin.Checked := devData.PrintLineNumbersMargins;
end;

procedure TPrintForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	Action := caFree;
end;

end.
