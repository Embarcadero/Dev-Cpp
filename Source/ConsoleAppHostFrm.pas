unit ConsoleAppHostFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls;

type
  TConsoleAppHost = class(TForm)
  private
  protected
    { Private declarations }
    FCommand: string;
    FCallBack: TProc<TConsoleAppHost, Boolean>;
    FProcess: THandle;
    FProcessWindow: THandle;
    FWaitInitialization: Integer;

    procedure Resize; override;
    procedure ApplyHost; virtual;

    function StartCommand: Boolean;
    procedure ShowAppEmbedded(WindowHandle: THandle; Container: TWinControl);
    procedure AssignTo(Dest: TPersistent); override;
    procedure DoTabShow(Sender: TObject);

    procedure DestroyWindowHandle; override;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
  public
    { Public declarations }

    class procedure NewHost(const ACommand, ACaption: string; const ACallBack: TProc<TConsoleAppHost, Boolean>; const AWaitInitialization: Integer = 0);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  System.Generics.Collections, System.DateUtils,

  Winapi.ShellAPI;

{$R *.dfm}

{ TfmConsoleHost }
 class procedure TConsoleAppHost.NewHost(const ACommand, ACaption: string; const ACallBack: TProc<TConsoleAppHost, Boolean>; const AWaitInitialization: Integer = 0);
begin
  if not Assigned(ACallBack) then Exit;
  if ACommand.IsEmpty then Exit;

  var NewHost := TConsoleAppHost.Create(Application);
  NewHost.FCommand := ACommand;

  NewHost.Caption := ACaption;
  if ACaption.IsEmpty then
    NewHost.Caption := ACommand;
  NewHost.FWaitInitialization := AWaitInitialization;
  NewHost.FCallBack := ACallBack;
  NewHost.ApplyHost;
end;

constructor TConsoleAppHost.Create(AOwner: TComponent);
begin
  FCommand := '';
  FCallBack := nil;
  FProcess := INVALID_HANDLE_VALUE;
  FProcessWindow := INVALID_HANDLE_VALUE;
  FWaitInitialization := 0;

  inherited;
end;

destructor TConsoleAppHost.Destroy;
begin
  FProcessWindow := INVALID_HANDLE_VALUE;
  if FProcess <> INVALID_HANDLE_VALUE then
  begin
    var TmpProcess := FProcess;
    FProcess := INVALID_HANDLE_VALUE;
    TerminateProcess(TmpProcess, 0);
  end;

  inherited;
end;

procedure TConsoleAppHost.CreateWindowHandle(const Params: TCreateParams);
begin
  inherited;

  ShowAppEmbedded(FProcessWindow, Self);
end;

procedure TConsoleAppHost.DestroyWindowHandle;
begin
  if FProcessWindow <> INVALID_HANDLE_VALUE then
  begin
    ShowWindow(FProcessWindow, SW_HIDE);
    Winapi.Windows.SetParent(FProcessWindow, 0);
  end;

  inherited;
end;

procedure TConsoleAppHost.DoTabShow(Sender: TObject);
begin
  Resize;

  if FProcessWindow <> INVALID_HANDLE_VALUE then
    Winapi.Windows.RedrawWindow(FProcessWindow, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW);
end;

function OpenThread(dwDesiredAccess: DWORD; bInheritHandle: BOOL; dwThreadId: DWORD): DWORD; stdcall; external 'kernel32.dll';

function EnumWindowsProc(AWnd: THandle; ASearchRec: Pointer): Bool; stdcall;
begin
  var WindowPlacement: PWindowPlacement := nil;
  if (GetWindowPlacement(AWnd, WindowPlacement) and (WindowPlacement.showCmd <> SW_HIDE)) then Exit(True);

  var ThreadHandle: DWORD := 0;
  var ThreadProcessID: Cardinal := 0;

  try
    ThreadHandle := OpenThread($0800, FALSE, GetWindowThreadProcessId(AWnd));
    try
      ThreadProcessID := GetProcessIdOfThread(ThreadHandle);
    finally
      CloseHandle(ThreadHandle);
    end;
  except
    ThreadProcessID := 0;
  end;

  Result := (ThreadProcessID <> TPair<THandle, THandle>(ASearchRec^).Key);
  if not Result then
    TPair<THandle, THandle>(ASearchRec^).Value := AWnd;
end;

procedure TConsoleAppHost.ApplyHost;
begin
  if FCommand.IsEmpty then Exit;
  if FProcess <> INVALID_HANDLE_VALUE then Exit;

  TThread.CreateAnonymousThread(
    procedure
    begin
      var IsSuccess := StartCommand;

      TThread.Synchronize(nil,
        procedure
        begin
          FCallBack(Self, IsSuccess);
        end);

      if not IsSuccess then Exit;

      TThread.CreateAnonymousThread(
        procedure
        begin
          while True do
          begin
            while (FProcess <> INVALID_HANDLE_VALUE) and (WaitForSingleObject(FProcess, 10) = WAIT_TIMEOUT) do
              TThread.Sleep(10);

            if FProcess = INVALID_HANDLE_VALUE then Break;

            if StartCommand then
              ShowAppEmbedded(FProcessWindow, Self);
          end;
        end).Start;
    end).Start;
end;

procedure TConsoleAppHost.AssignTo(Dest: TPersistent);
begin
  if Dest is TTabSheet then
  begin
    TTabSheet(Dest).Caption := Caption;
    TTabSheet(Dest).OnShow := DoTabShow;
    Parent := TTabSheet(Dest);
    TTabSheet(Dest).Tag := NativeInt(Self);
    Visible := True;
  end
  else
    inherited;
end;


procedure TConsoleAppHost.Resize;
begin
  inherited;

  if FProcessWindow <> INVALID_HANDLE_VALUE then
    MoveWindow(FProcessWindow, 0, 0, ClientWidth, ClientHeight, True);
end;

procedure TConsoleAppHost.ShowAppEmbedded(WindowHandle: THandle; Container: TWinControl);
var
  WindowStyle : Integer;
begin
  if WindowHandle = INVALID_HANDLE_VALUE then Exit;

  FProcessWindow := WindowHandle;
  WindowStyle := GetWindowLong(WindowHandle, GWL_STYLE);
  WindowStyle := WindowStyle
                 and (not WS_CAPTION)
                 and (not WS_BORDER)
                 and (not WS_OVERLAPPED)
                 and (not WS_THICKFRAME)
                 and (not WS_SYSMENU)
                 and (not WS_MINIMIZE)
                 and (not WS_MAXIMIZEBOX)
                 or WS_DLGFRAME;

  SetWindowLong(WindowHandle, GWL_STYLE, WindowStyle);
  SetWindowLong(WindowHandle, GWL_EXSTYLE, GetWindowLong(WindowHandle, GWL_EXSTYLE) or WS_EX_DLGMODALFRAME);

  AttachThreadInput(MainThreadID, GetWindowThreadProcessId(WindowHandle, nil), True);

  Container.HandleNeeded;
  Winapi.Windows.SetParent(WindowHandle, Container.Handle);
  ShowWindow(WindowHandle, SW_SHOWMAXIMIZED);
  SetWindowPos(WindowHandle, 0, 0, 0, Container.ClientWidth, Container.ClientHeight, SWP_NOMOVE or SWP_NOSIZE or SWP_FRAMECHANGED or SWP_NOZORDER);

  SendMessage(Container.Handle, WM_UPDATEUISTATE, UIS_INITIALIZE, 0);
  Winapi.Windows.RedrawWindow(FProcessWindow, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW);

  SetWindowLong(Container.Handle, GWL_STYLE, GetWindowLong(Container.Handle,GWL_STYLE) or WS_CLIPCHILDREN);

  SetForegroundWindow(WindowHandle);

  keybd_event(VK_HOME, $24, 0, 0);
  keybd_event(VK_HOME, $24, KEYEVENTF_KEYUP, 0);
end;

function TConsoleAppHost.StartCommand: Boolean;
begin
  FProcess := INVALID_HANDLE_VALUE;
  var SEI: TShellExecuteInfo;
  FillChar(SEI, SizeOf(SEI), 0);
  SEI.cbSize := SizeOf(SEI);
  SEI.wnd := Self.Handle;
  SEI.fMask := SEE_MASK_FLAG_NO_UI or SEE_MASK_NOCLOSEPROCESS or SEE_MASK_DOENVSUBST or SEE_MASK_WAITFORINPUTIDLE;
  SEI.nShow := SW_HIDE;
  SEI.lpFile := @FCommand[1];

  Result := ShellExecuteEx(@SEI);
  if Result then
  begin
    FProcess := SEI.hProcess;
    WaitForInputIdle(FProcess, INFINITE);

    var SearchRec: TPair<THandle, THandle>;
    SearchRec.Key := GetProcessId(FProcess);
    SearchRec.Value := 0;

    TThread.Sleep(FWaitInitialization);

    var StartWait := Now;
    while SearchRec.Value = 0 do
    begin
      EnumWindows(@EnumWindowsProc, LPARAM(@SearchRec));
      TThread.Sleep(100);
      if SecondsBetween(StartWait, Now) > 10 then
        Exit(False);
    end;

    if SearchRec.Value <> 0 then
    begin
      ShowWindow(SearchRec.Value, SW_HIDE);
      FProcessWindow := SearchRec.Value;
    end;
  end;
end;

end.
