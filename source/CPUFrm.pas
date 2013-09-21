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
  Dialogs, StdCtrls, Buttons, SynEdit, StrUtils, ComCtrls, ExtCtrls;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Variants, Classes, QGraphics, QControls, QForms,
  QDialogs, QStdCtrls, QButtons, QSynEdit;
{$ENDIF}

type

  PRegister = ^TRegister;
  TRegister = record
    name : AnsiString;
    value : AnsiString;
  end;

  TCPUForm = class(TForm)
    edFunc: TEdit;
    lblFunc: TLabel;
    CodeList: TSynEdit;
    RegisterListbox: TListView;
    DisasPanel: TPanel;
    StackTrace: TListView;
    RadioATT: TRadioButton;
    RadioIntel: TRadioButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure edFuncKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure gbSyntaxClick(Sender: TObject);
  private
    fActiveLine : integer;
    fRegisters : TList;
    fAssembler : TStringList;
    fBacktrace : TList;

    procedure LoadText;
  public
    procedure OnAssemblerReady;
    procedure OnRegistersReady;
    procedure OnBacktraceReady;
  end;

var
  CPUForm: TCPUForm = nil;

implementation

uses
  main, version, MultiLangSupport, debugger, debugreader, datamod, utils,
  devcfg, Types;

{$R *.dfm}

procedure TCPUForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
	I : integer;
begin
	for I := 0 to fRegisters.Count - 1 do
		Dispose(PRegister(fRegisters.Items[I]));
	fRegisters.Free;

	fAssembler.Free;

	for I := 0 to fBacktrace.Count - 1 do
		Dispose(PTrace(fBacktrace.Items[I]));
	fBackTrace.Free;

	action := caFree;
	CPUForm := nil;
end;

procedure TCPUForm.edFuncKeyPress(Sender: TObject; var Key: Char);
begin
	if MainForm.fDebugger.Executing then begin
		if Key = Chr(VK_RETURN) then begin
			Key := #0;
			MainForm.fDebugger.SendCommand('disassemble',TEdit(Sender).Text);
		end;
	end;
end;

procedure TCPUForm.LoadText;
begin
	// Set interface font
	Font.Name := devData.InterfaceFont;
	Font.Size := devData.InterfaceFontSize;

	Caption := Lang[ID_CPU_CAPTION];
	lblFunc.Caption := Lang[ID_CPU_FUNC];
end;

procedure TCPUForm.OnBacktraceReady;
var
	I : integer;
	item : TListItem;
begin
	StackTrace.Items.BeginUpdate;
	StackTrace.Clear;
	for I := 0 to fBacktrace.Count - 1 do begin
		item := StackTrace.Items.Add;
		item.Caption := PTrace(fBacktrace.Items[I])^.funcname;
		item.SubItems.Add(PTrace(fBacktrace.Items[I])^.filename);
		item.SubItems.Add(PTrace(fBacktrace.Items[I])^.line);
	end;
	StackTrace.Items.EndUpdate;

	// Free list for reuse
	for I := 0 to fBacktrace.Count - 1 do
		Dispose(PTrace(fBacktrace.Items[I]));
	fBacktrace.Clear;
end;

procedure TCPUForm.OnAssemblerReady;
var
	I : integer;
begin
	edFunc.Text := fAssembler.Strings[0];

	CodeList.BeginUpdate;
	CodeList.Clear;
	for I := 1 to fAssembler.Count - 1 do
		CodeList.Lines.Add(fAssembler.Strings[i]);
	CodeList.EndUpdate;

	// Free list for reuse
	fAssembler.Clear;
end;

procedure TCPUForm.OnRegistersReady;
var
	item : TListItem;
	I : integer;
begin
	RegisterListbox.Items.BeginUpdate;
	RegisterListBox.Clear;
	for I := 0 to fRegisters.Count - 1 do begin
		item := RegisterListbox.Items.Add;
		item.Caption := UpperCase(PRegister(fRegisters.Items[I])^.name);
		item.SubItems.Add(PRegister(fRegisters.Items[I])^.value);
	end;
	RegisterListBox.Items.EndUpdate;

	// Free list for reuse
	for I := 0 to fRegisters.Count - 1 do
		Dispose(PRegister(fRegisters.Items[I]));
	fRegisters.Clear;
end;

procedure TCPUForm.FormCreate(Sender: TObject);
begin
	fActiveLine := -1;
	LoadText;

	fRegisters := TList.Create;
	fAssembler := TStringList.Create;
	fBacktrace := TList.Create;

	if MainForm.fDebugger.Executing then begin

		// Load the registers...
		MainForm.fDebugger.SetRegisters(fRegisters);
		MainForm.fDebugger.SendCommand('info','registers');

		// Set disassembly flavor and load the current function
		MainForm.fDebugger.SetDisassembly(fAssembler);
		gbSyntaxClick(nil);

		// Obtain stack trace too
		MainForm.fDebugger.SetBacktrace(fBacktrace);
		MainForm.fDebugger.SendCommand('backtrace','');
	end;
end;

procedure TCPUForm.gbSyntaxClick(Sender: TObject);
begin
	// Set disassembly flavor
	if RadioAtt.Checked then begin
		MainForm.fDebugger.SendCommand('set disassembly-flavor','att');
		RadioIntel.Checked := false;
	end else if RadioIntel.Checked then begin
		MainForm.fDebugger.SendCommand('set disassembly-flavor','intel');
		RadioAtt.Checked := false;
	end;

	// Reload the current function
	MainForm.fDebugger.SendCommand('disassemble',edFunc.Text);
end;

end.
