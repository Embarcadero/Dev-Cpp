unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TFrmMain = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmMain: TFrmMain;

implementation

uses
  ActiveX, ObjSel, ADODB;

{$R *.dfm}


function InitObjectPicker(Picker: IDsObjectPicker): HRESULT;
var
  ScopeInit: array [0..0] of TDSOPScopeInitInfo;
  InitInfo: TDSOPInitInfo;
begin
  if nil = Picker then
    Result := E_INVALIDARG
  else
  begin
    ZeroMemory(@ScopeInit, SizeOf(ScopeInit));
    ScopeInit[0].cbSize := SizeOf(TDSOPScopeInitInfo);
    ScopeInit[0].flType := DSOP_SCOPE_TYPE_TARGET_COMPUTER;
    ScopeInit[0].flScope := DSOP_SCOPE_TYPE_USER_ENTERED_DOWNLEVEL_SCOPE;
    ScopeInit[0].FilterFlags.Uplevel.flBothModes := DSOP_FILTER_USERS;
    ScopeInit[0].FilterFlags.flDownlevel := DSOP_DOWNLEVEL_FILTER_USERS;

    ZeroMemory(@InitInfo, SizeOf(InitInfo));
    InitInfo.cbSize := SizeOf(InitInfo);
    InitInfo.cDsScopeInfos := SizeOf(ScopeInit) div SizeOf(TDSOPScopeInitInfo);
    InitInfo.aDsScopeInfos := @ScopeInit;
    InitInfo.flOptions := DSOP_FLAG_SKIP_TARGET_COMPUTER_DC_CHECK;

    Result := Picker.Initialize(InitInfo);
  end;
end;

function ProcessSelectedObjects(DatObj: IDataObject): HRESULT;
var
  StgMed: TStgMedium;
  FmtEtc: TFormatEtc;
  SelLst: PDSSelectionList;
  Index: ULONG;
  Text: string;
begin
  if nil = DatObj then
    Result := E_INVALIDARG
  else
  begin
    with FmtEtc do
    begin
      cfFormat := RegisterClipboardFormat(CFSTR_DSOP_DS_SELECTION_LIST);
      ptd := nil;
      dwAspect := DVASPECT_CONTENT;
      lindex := -1;
      tymed := TYMED_HGLOBAL;
    end;
    Result := DatObj.GetData(FmtEtc, StgMed);
    if Succeeded(Result) then
    begin
      SelLst := PDS_SELECTION_LIST(GlobalLock(StgMed.hGlobal));
      if SelLst <> nil then
      try
        Text := '';
        for Index := 0 to SelLst.cItems - 1 do
        begin
          Text := Text + Format(
            'Object : %u'#13#10 +
            ' Name : %s'#13#10 +
            ' Class: %s'#13#10 +
            ' Path : %s'#13#10 +
            ' UPN : %s'#13#10, [
            Index,
            WideCharToString(SelLst.aDsSelection[Index].pwzName),
            WideCharToString(SelLst.aDsSelection[Index].pwzClass),
            WideCharToString(SelLst.aDsSelection[Index].pwzADsPath),
            WideCharToString(SelLst.aDsSelection[Index].pwzUPN)]);
        end;
        ShowMessage(Text);
      finally
        GlobalUnlock(StgMed.hGlobal);
      end
      else
        Result := E_POINTER;

      ReleaseStgMedium(StgMed);
    end;
  end;
end;


procedure TFrmMain.Button1Click(Sender: TObject);
begin
   PromptDataSource(Self.Handle, '');
end;

procedure TFrmMain.Button2Click(Sender: TObject);
begin
   PromptDataLinkFile(Self.Handle, '');
end;

procedure TFrmMain.Button3Click(Sender: TObject);
begin
   WNetConnectionDialog(0, RESOURCETYPE_DISK);
end;

procedure TFrmMain.Button4Click(Sender: TObject);
begin
   WNetDisconnectDialog(0, RESOURCETYPE_DISK);
end;

procedure TFrmMain.Button5Click(Sender: TObject);
var
  Picker: IDsObjectPicker;
  DatObj: IDataObject;
begin
  if Succeeded(CoInitialize(nil)) then
  try
    if Succeeded(CoCreateInstance(CLSID_DsObjectPicker, nil,
      CLSCTX_INPROC_SERVER, IID_IDsObjectPicker, Picker)) then
    try
      if Succeeded(InitObjectPicker(Picker)) then
        case Picker.InvokeDialog(Self.Handle, DatObj) of
          S_OK:
            try
              ProcessSelectedObjects(DatObj);
            finally
              DatObj := nil;
            end;
          S_FALSE:
            ShowMessage('Dialog canceled by the user.');
        end;
    finally
      Picker := nil;
    end;
  finally
    CoUninitialize;
  end;
end;

end.
