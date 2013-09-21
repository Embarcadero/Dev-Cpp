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

unit CPUFrm;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, SynEdit, XPMenu;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Variants, Classes, QGraphics, QControls, QForms,
  QDialogs, QStdCtrls, QButtons, QSynEdit;
{$ENDIF}

type
  TCPUForm = class(TForm)
    gbAsm: TGroupBox;
    gbSyntax: TGroupBox;
    rbIntel: TRadioButton;
    rbATT: TRadioButton;
    CloseBtn: TBitBtn;
    edFunc: TEdit;
    lblFunc: TLabel;
    CodeList: TSynEdit;
    gbRegisters: TGroupBox;
    lblEIP: TLabel;
    EIPText: TEdit;
    EAXText: TEdit;
    lblEAX: TLabel;
    EBXText: TEdit;
    lblEBX: TLabel;
    lblECX: TLabel;
    ECXText: TEdit;
    lblEDX: TLabel;
    EDXText: TEdit;
    lblESI: TLabel;
    ESIText: TEdit;
    lblEDI: TLabel;
    EDIText: TEdit;
    lblEBP: TLabel;
    EBPText: TEdit;
    lblESP: TLabel;
    ESPText: TEdit;
    lblCS: TLabel;
    CSText: TEdit;
    lblDS: TLabel;
    DSText: TEdit;
    lblSS: TLabel;
    SSText: TEdit;
    lblES: TLabel;
    ESText: TEdit;
    XPMenu: TXPMenu;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure edFuncKeyPress(Sender: TObject; var Key: Char);
    procedure rbSyntaxClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    ActiveLine : integer;

    procedure LoadText;
    procedure OnRegistersReady;
    procedure OnActiveLine(Sender: TObject; Line: Integer;
                           var Special: Boolean; var FG, BG: TColor);

    { Private declarations }
  public
    { Public declarations }
  end;

var
  CPUForm: TCPUForm;

implementation

uses 
  main, version, MultiLangSupport, debugger, utils,
  devcfg, debugwait, Types; 

{$R *.dfm}

procedure TCPUForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  MainForm.fDebugger.OnRegistersReady := nil;
  CPUForm := nil;
end;

procedure TCPUForm.edFuncKeyPress(Sender: TObject; var Key: Char);
begin
  if key = #13 then begin
    if (MainForm.fDebugger.Executing) then
      CodeList.Lines.Clear;
      MainForm.fDebugger.SendCommand(GDB_DISASSEMBLE, edFunc.Text);
  end;
end;

procedure TCPUForm.rbSyntaxClick(Sender: TObject);
var cb : TCheckBox;
begin
  cb := TCheckBox(sender);
  while (MainForm.fDebugger.InAssembler) do
    sleep(20);
  if (MainForm.fDebugger.Executing) then begin
    CodeList.Lines.Clear;
    if cb.Tag = 0 then
      MainForm.fDebugger.SendCommand(GDB_SETFLAVOR, GDB_ATT)
    else
      MainForm.fDebugger.SendCommand(GDB_SETFLAVOR, GDB_INTEL);
    MainForm.fDebugger.Idle;
    MainForm.fDebugger.SendCommand(GDB_DISASSEMBLE, edFunc.Text);
    MainForm.fDebugger.Idle;
  end;
end;

procedure TCPUForm.LoadText;
begin
  if devData.XPTheme then
    XPMenu.Active := true
  else
    XPMenu.Active := false;
  with Lang do begin
    Caption := Strings[ID_CPU_CAPTION];
    gbAsm.Caption := Strings[ID_CPU_ASMCODE];
    gbSyntax.Caption := Strings[ID_CPU_SYNTAX];
    gbRegisters.Caption := Strings[ID_CPU_REGISTERS];
    lblFunc.Caption := Strings[ID_CPU_FUNC];
    CloseBtn.Caption := Strings[ID_BTN_CLOSE];
  end;
end;

procedure TCPUForm.FormCreate(Sender: TObject);
begin
  ActiveLine := -1;
  CodeList.OnSpecialLineColors := OnActiveLine;
  MainForm.fDebugger.OnRegistersReady := OnRegistersReady;
  LoadText;
end;

procedure TCPUForm.OnRegistersReady;
var i : integer;
begin
  EAXText.Text := MainForm.fDebugger.Registers[EAX];
  EBXText.Text := MainForm.fDebugger.Registers[EBX];
  ECXText.Text := MainForm.fDebugger.Registers[ECX];
  EDXText.Text := MainForm.fDebugger.Registers[EDX];
  ESIText.Text := MainForm.fDebugger.Registers[ESI];
  EDIText.Text := MainForm.fDebugger.Registers[EDI];
  EBPText.Text := MainForm.fDebugger.Registers[EBP];
  ESPText.Text := MainForm.fDebugger.Registers[ESP];
  EIPText.Text := MainForm.fDebugger.Registers[EIP];
  CSText.Text := MainForm.fDebugger.Registers[CS];
  DSText.Text := MainForm.fDebugger.Registers[DS];
  SSText.Text := MainForm.fDebugger.Registers[SS];
  ESText.Text := MainForm.fDebugger.Registers[ES];
  for i := 0 to CodeList.Lines.Count - 1 do
    if pos(EIPText.Text, CodeList.Lines[i]) <> 0 then begin
      if (ActiveLine <> i) and (ActiveLine <> -1) then
        CodeList.InvalidateLine(ActiveLine);
      ActiveLine := i + 1;
      CodeList.InvalidateLine(ActiveLine);
      CodeList.CaretY := ActiveLine;
      CodeList.EnsureCursorPosVisible;
      break;
    end;
end;

procedure TCPUForm.OnActiveLine(Sender: TObject; Line: Integer;
                                var Special: Boolean; var FG, BG: TColor);
var pt : TPoint;
begin
   if (Line = ActiveLine) then begin
     StrtoPoint(pt, devEditor.Syntax.Values[cABP]);
     BG:= pt.X;
     FG:= pt.Y;
     Special:= TRUE;
   end;
end;

end.
