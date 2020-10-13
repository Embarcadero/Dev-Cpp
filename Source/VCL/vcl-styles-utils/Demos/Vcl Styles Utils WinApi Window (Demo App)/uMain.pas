unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.Buttons, Vcl.Styles, Vcl.Themes;

type
  TForm1 = class(TForm)
    SpeedButton1: TSpeedButton;
    CheckBox1: TCheckBox;
    procedure SpeedButton1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  Vcl.Styles.Utils.SysControls,
  Vcl.Styles.Utils.Forms;
{$R *.dfm}

var
  wc: TWndClass;
  hWindow, hBtn, hLst: HWND;
  Msg: TMsg;

function MainWndProc(hWindow: HWND; Msg: UINT; wParam: wParam; lParam: lParam)
  : LRESULT; stdcall; export;
begin
  Result := 0;
  case Msg of
    WM_COMMAND:
      begin
        case wParam of
          0:
            ShowMessage('Hi ...');
        end;
      end;
    WM_DESTROY:
      PostQuitMessage(0);
  else
    begin
      Result := DefWindowProc(hWindow, Msg, wParam, lParam);
      Exit;
    end;
  end;
end;

procedure CreateMyWindow;
var
  i : Integer;
begin
  wc.lpszClassName := 'CustomSkinedWindowClass';
  wc.lpfnWndProc := @MainWndProc;
  wc.Style := CS_VREDRAW or CS_HREDRAW;
  wc.hInstance := hInstance;
  wc.hIcon := LoadIcon(0, IDI_APPLICATION);
  wc.hCursor := LoadCursor(0, IDC_ARROW);
  wc.hbrBackground := COLOR_BTNFACE;
  wc.lpszMenuName := nil;
  wc.cbClsExtra := 0;
  wc.cbWndExtra := 0;
  Winapi.Windows.RegisterClass(wc);

  hWindow := CreateWindowEx(WS_EX_CONTROLPARENT or WS_EX_WINDOWEDGE or
    WS_EX_OVERLAPPEDWINDOW, wc.lpszClassName, 'Custom Skined Window',
    WS_POPUP or WS_VISIBLE or WS_SIZEBOX or WS_OVERLAPPEDWINDOW or
    WS_OVERLAPPED or WS_SYSMENU or WS_VSCROLL, CW_USEDEFAULT, 0, 400, 300, 0, 0,
    hInstance, nil);

  hBtn := CreateWindowEx(0, 'Button', 'Click Me !!', WS_CHILD or WS_VISIBLE, 50,
    10, 200, 30, hWindow, 0, hInstance, nil);


  hLst := CreateWindowEx(WS_EX_WINDOWEDGE or WS_EX_CLIENTEDGE, 'ListBox', '', WS_CHILD or WS_VISIBLE or WS_VSCROLL or ES_AUTOVSCROLL, 50,
    60, 200, 200, hWindow, 0, hInstance, nil);

  for i:=1 to 30 do
   SendMessage(hLst, LB_ADDSTRING, 0, LPARAM(PChar('Item '+IntToStr(i))));

  ShowWindow(hWindow, SW_SHOWNORMAL);
  UpDateWindow(hWindow);

  while GetMessage(Msg, 0, 0, 0) do
  begin
    TranslateMessage(Msg);
    DispatchMessage(Msg);
  end;

end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  TSysStyleManager.Enabled := TCheckBox(Sender).Checked;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  CreateMyWindow;
end;

initialization

TSysStyleManager.RegisterSysStyleHook('CustomSkinedWindowClass',
  TSysDialogStyleHook);


finalization

TSysStyleManager.UnRegisterSysStyleHook('CustomSkinedWindowClass',
  TSysDialogStyleHook);

end.
