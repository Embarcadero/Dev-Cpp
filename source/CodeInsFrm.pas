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

unit CodeInsFrm;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, Variants, Classes, Graphics, Controls, Forms,
  Spin, ExtCtrls, Buttons, StdCtrls;
{$ENDIF}
{$IFDEF LINUX}
  Variants, Classes, QGraphics, QControls, QForms,
  QComCtrls, QExtCtrls, QButtons, QStdCtrls;
{$ENDIF}

type
 PCodeIns = ^TCodeIns;
 TCodeIns = record
  Caption: AnsiString;
  Line: AnsiString;
  Desc: AnsiString;
  Sep: integer;
 end;

 TCodeInsList = class(TObject)
  private
   fFile: AnsiString;
   fList: TList;
   procedure SetItem(index: integer; Value: PCodeIns);
   function GetItem(index: integer): PCodeIns;
   function GetCount: integer;
  public
   constructor Create;
   destructor Destroy; override;

   procedure LoadCode;
   procedure SaveCode;
   function Indexof(const Value: AnsiString): integer;
   function AddItem(Value: PCodeIns): integer;
   procedure AddItemByValues(const menutext, description, code : AnsiString; section : integer);
   procedure Delete(index: integer);
   procedure Clear;
   property Items[index: integer]: PCodeins read GetItem write SetItem; default;
   property Count: integer read GetCount;
 end;

  TfrmCodeEdit = class(TForm)
    Bevel1: TBevel;
    lblMenu: TLabel;
    lblSec: TLabel;
    edMenuText: TEdit;
    seSection: TSpinEdit;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    lblDesc: TLabel;
    edDesc: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure edMenuTextChange(Sender: TObject);
  private
    fEdit: boolean;
    fEntry: PCodeIns;
    procedure SetEntry(Value: PCodeIns);
    procedure LoadText;
  public
    property Edit: boolean read fEdit write fEdit;
    property Entry: PCodeIns read fEntry write SetEntry;
  end;

implementation

uses
 SysUtils, IniFiles, devCFG, Utils, version, MultiLangSupport;

{$R *.dfm}
 { TCodeInsList }

constructor TCodeInsList.Create;
begin
	inherited Create;
	fList := TList.Create;
end;

destructor TCodeInsList.Destroy;
var
	I: integer;
begin
	for I := 0 to fList.Count - 1 do
		Dispose(PCodeIns(fList[I]));
	fList.Free;
	inherited Destroy;
end;

function TCodeInsList.Indexof(const Value: AnsiString): integer;
begin
  for result:= 0 to pred(fList.Count) do
   if CompareText(PCodeIns(fList[result])^.Caption, Value) = 0 then exit;
  result:= -1;
end;

function TCodeInsList.AddItem(Value: PCodeIns): integer;
begin
	result:= fList.Add(Value);
end;

procedure TCodeInsList.AddItemByValues(const menutext, description, code : AnsiString; section : integer);
var
	assembleditem : PCodeIns;
begin
	new(assembleditem);
	assembleditem^.Caption:=menutext;
	assembleditem^.Line:=code;
	assembleditem^.Desc:=description;
	assembleditem^.Sep:=section;
	fList.Add(assembleditem);
end;

procedure TCodeInsList.Clear;
begin
  fList.Clear;
  fList.Pack;
  fList.Capacity:= fList.Count;
end;

procedure TCodeInsList.Delete(index: integer);
begin
  if (index <0) or (index > fList.Count -1) then exit;

  fList.Delete(index);
  fList.Pack;
  fList.Capacity:= fList.Count;
end;

function TCodeInsList.GetCount: integer;
begin
  result:= fList.Count;
end;

function TCodeInsList.GetItem(index: integer): PCodeIns;
begin
  if (index < 0) or (index > fList.Count -1) then
   result:= nil
  else
   result:= PCodeIns(fList[index]);
end;

procedure TCodeInsList.SetItem(index: integer; Value: PCodeIns);
begin
  fList[index]:= Value;
end;

procedure TCodeInsList.LoadCode;
var
 Item: PCodeIns;
 tmp: TStringList;
 idx: integer;
begin
	if not FileExists(fFile) then
		fFile:=devDirs.Config + DEV_CODEINS_FILE;

	if FileExists(fFile) then begin
		with TINIFile.Create(fFile) do
			try
				tmp:= TStringList.Create;
				Clear;
				try
					ReadSections(tmp);
					if tmp.Count = 0 then
						exit;

						for idx:= 0 to pred(tmp.Count) do begin
							new(Item);
							Item^.Caption:= StringReplace(tmp[idx], '_', ' ', [rfReplaceAll]);
							Item^.Desc:= ReadString(tmp[idx], 'Desc', '');
							Item^.Line:= StrtoCodeIns(ReadString(tmp[idx], 'Line', ''));
							Item^.Sep:= ReadInteger(tmp[idx], 'Sep', 0);
							AddItem(Item);
						end;
				finally
					tmp.free;
				end;
			finally
				free;
			end;
	end else begin
		// Win32
		AddItemByValues('MessageBox','Win32 MessageBox','MessageBox(*|*,"Hello","Caption",MB_OK);',1);
		AddItemByValues('WinMain','Win32 Main Function',

	'int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow) {'+#13#10+
	'	WNDCLASSEX wc;'+#13#10+
	'	HWND hwnd;'+#13#10+
	'	MSG Msg;'+#13#10+
	''+#13#10+
	'	memset(&wc,0,sizeof(wc));'+#13#10+
	'	wc.cbSize		 = sizeof(WNDCLASSEX);'+#13#10+
	'	wc.lpfnWndProc	 = *|*; /* insert window procedure function here */;'+#13#10+
	'	wc.hInstance	 = hInstance;'+#13#10+
	'	wc.hCursor		 = LoadCursor(NULL, IDC_ARROW);'+#13#10+
	'	wc.hbrBackground = (HBRUSH)(COLOR_WINDOW+1);'+#13#10+
	'	wc.lpszClassName = "WindowClass";'+#13#10+
	'	wc.hIcon		 = LoadIcon(NULL, IDI_APPLICATION);'+#13#10+
	'	wc.hIconSm		 = LoadIcon(NULL, IDI_APPLICATION);'+#13#10+
	''+#13#10+
	'	if(!RegisterClassEx(&wc)) {'+#13#10+
	'		MessageBox(NULL, "Window Registration Failed!","Error!",MB_ICONEXCLAMATION|MB_OK);'+#13#10+
	'		return 0;'+#13#10+
	'	}'+#13#10+
	''+#13#10+
	'	hwnd = CreateWindowEx(WS_EX_CLIENTEDGE,"WindowClass","Caption",WS_VISIBLE|WS_OVERLAPPEDWINDOW,CW_USEDEFAULT,CW_USEDEFAULT,640,480,NULL,NULL,hInstance,NULL);'+#13#10+
	'	if(hwnd == NULL) {'+#13#10+
	'		MessageBox(NULL, "Window Creation Failed!","Error!",MB_ICONEXCLAMATION|MB_OK);'+#13#10+
	'		return 0;'+#13#10+
	'	}'+#13#10+
	''+#13#10+
	'	while(GetMessage(&Msg, NULL, 0, 0) > 0) {'+#13#10+
	'		TranslateMessage(&Msg);'+#13#10+
	'		DispatchMessage(&Msg);'+#13#10+
	'	}'+#13#10+
	'	return Msg.wParam;'+#13#10+
	'}',1);

		AddItemByValues('Main Window Proc','Win32 Main Proc Function',

	'LRESULT CALLBACK WndProc(HWND hwnd, UINT Message, WPARAM wParam, LPARAM lParam) {'+#13#10+
	'	switch(Message) {'+#13#10+
	'		case *|*: {'+#13#10+
	'			break;'+#13#10+
	'		case WM_DESTROY: {'+#13#10+
	'			PostQuitMessage(0);'+#13#10+
	'			break;'+#13#10+
	'		}'+#13#10+
	'		default:'+#13#10+
	'			return DefWindowProc(hwnd, Message, wParam, lParam);'+#13#10+
	'	}'+#13#10+
	'	return 0;'+#13#10+
	'}',1);

		AddItemByValues('Child Window Proc','Win32 Child Proc Function',

	'BOOL CALLBACK ChildProc(HWND hwnd, UINT Message, WPARAM wParam, LPARAM lParam) {'+#13#10+
	'	switch(Message) {'+#13#10+
	'		case *|*: {'+#13#10+
	'			break;'+#13#10+
	'		}'+#13#10+
	'		default:'+#13#10+
	'			return false;'+#13#10+
	'	}'+#13#10+
	'	return true;'+#13#10+
	'}',1);

		// Generic C
		AddItemByValues('for()','for loop','for(*|*;;) {'+#13#10+'}',2);
		AddItemByValues('while()','while loop','while(*|*) {'+#13#10+'}',2);
		AddItemByValues('do-while()','do-while loop','do {'+#13#10+'} while(*|*);',2);
		AddItemByValues('if()','if statement','if(*|*) {'+#13#10+'}',2);
		AddItemByValues('switch()','switch statement','switch(*|*) {'+#13#10+'	default:'+#13#10+'}',2);

		// C++
		AddItemByValues('Class','Class',

	'class *|* {'+#13#10+
	'	// Private section'+#13#10+
	'	public:'+#13#10+
	'		// Public Declarations'+#13#10+
	'	protected:'+#13#10+
	'		// Protected Declarations'+#13#10+
	'};',2);

		AddItemByValues('Class Header Template','Class',

	'#ifndef SOMETHING_INCLUDED'+#13#10+
	'#define SOMETHING_INCLUDED'+#13#10#13#10+
	'class *|* {'+#13#10+
	'	// Private section'+#13#10+
	'	public:'+#13#10+
	'		// Public Declarations'+#13#10+
	'	protected:'+#13#10+
	'		// Protected Declarations'+#13#10+
	'};'+#13#10+#13#10+
	'#endif',2);

		// Preprocessor
		AddItemByValues('#ifdef','Preprocessor if','#ifdef *|*'+#13#10#13#10+'#endif',3);
		AddItemByValues('#ifndef','Preprocessor !if','#ifndef *|*'+#13#10#13#10+'#endif',3);
		AddItemByValues('#ifdef/else','Preprocessor if-else','#ifdef *|*'+#13#10#13#10+'#elif'+#13#10#13#10+'#endif',3);
		AddItemByValues('#ifndef/else','Preprocessor !if-else','#ifndef *|*'+#13#10#13#10+'#elif'+#13#10#13#10+'#endif',3);
	end;
end;

procedure TCodeInsList.SaveCode;
var
	idx: integer;
	section: AnsiString;
	CI: TCodeIns;
begin
	fList.Pack;
	fList.Capacity := fList.Count;
	DeleteFile(fFile);
	if fList.Count = 0 then exit;
	with TINIFile.Create(fFile) do
		try
			for idx:= 0 to pred(fList.Count) do begin
				CI:= PCodeIns(fList[idx])^;
				section:= StringReplace(CI.Caption, ' ', '_', [rfReplaceAll]);
				EraseSection(section);  // may be redundant
				WriteString(section, 'Desc', CI.Desc);
				WriteString(section, 'Line', CodeInstoStr(CI.Line));
				WriteInteger(section, 'Sep', CI.Sep);
			end;
		finally
			Free;
		end;
end;


 { TfrmCodeEdit }

procedure TfrmCodeEdit.SetEntry(Value: PCodeIns);
begin
  edMenuText.Text:= Value^.Caption;
  edDesc.Text:= Value^.Desc;
  seSection.Value:= Value^.Sep;
end;

procedure TfrmCodeEdit.FormCreate(Sender: TObject);
begin
  LoadText;
end;

procedure TfrmCodeEdit.LoadText;
begin
	// Set interface font
	Font.Name := devData.InterfaceFont;
	Font.Size := devData.InterfaceFontSize;

  if Edit then
   Caption:= Lang[ID_CIE_EDCAPTION]
  else
   Caption:= Lang[ID_CIE_ADDCAPTION];

  lblMenu.Caption:= Lang[ID_CIE_MENU];
  lblDesc.Caption:= Lang[ID_CIE_DESC];
  lblSec.Caption:=  Lang[ID_CIE_SEC];
  btnOk.Caption:= Lang[ID_BTN_OK];
  btnCancel.Caption:= Lang[ID_BTN_CANCEL];
end;

procedure TfrmCodeEdit.btnOkClick(Sender: TObject);
begin
  if edMenuText.Text = '' then begin
    ModalResult := mrNone;
  end;
end;

procedure TfrmCodeEdit.edMenuTextChange(Sender: TObject);
begin
  btnOK.Enabled := edMenuText.Text <> '';
end;

end.
