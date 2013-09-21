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

unit CompileProgressFrm;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Variants, Classes, QGraphics, QControls, QForms,
  QDialogs, QStdCtrls, QExtCtrls, QComCtrls;
{$ENDIF}

type
  TCompileProgressForm = class(TForm)
    btnClose: TButton;
    memoMiniLog: TMemo;
    infoCompiler: TLabel;
    lblCompiler: TLabel;
    infoStatus: TLabel;
    lblStatus: TLabel;
    infoFile: TLabel;
    lblFile: TLabel;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    infoErrors: TLabel;
    infoWarnings: TLabel;
    lblErr: TLabel;
    lblWarn: TLabel;
    Bevel5: TBevel;
    Bevel6: TBevel;
    pb: TProgressBar;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
  public
    starttime : cardinal;
    procedure LoadText;
  end;

var
  CompileProgressForm: TCompileProgressForm;

implementation

uses
	devcfg, MultiLangSupport;

{$R *.dfm}

procedure TCompileProgressForm.FormClose(Sender: TObject;var Action: TCloseAction);
begin
	Action := caFree;
end;

procedure TCompileProgressForm.LoadText;
begin
	// Set interface font
	Font.Name := devData.InterfaceFont;
	Font.Size := devData.InterfaceFontSize;

	Caption:=              Lang[ID_CMPPRG_CAPTION];

	btnClose.Caption:=     Lang[ID_BTN_CANCEL];
	infoCompiler.Caption:= Lang[ID_CMPRPG_COMPILER];
	infoStatus.Caption:=   Lang[ID_CMPPRG_STATUS];
	infoFile.Caption:=     Lang[ID_CMPPRG_FILE];
	infoErrors.Caption:=   Lang[ID_CMPPRG_ERRORS];
	infoWarnings.Caption:= Lang[ID_CMPPRG_WARNINGS];
end;

procedure TCompileProgressForm.FormCreate(Sender: TObject);
begin
	LoadText;
end;

end.

