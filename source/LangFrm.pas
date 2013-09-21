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
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, Menus, XPMenu, ComCtrls, FileCtrl;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QButtons, QExtCtrls, QMenus, QComCtrls;
{$ENDIF}

type
  TLangForm = class(TForm)
    OkBtn: TBitBtn;
    PicPanel: TPanel;
    PopupMenu: TPopupMenu;
    N1: TMenuItem;
    Image2: TImage;
    XPMenu: TXPMenu;
    FirstPanel: TPanel;
    ListBox: TListBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    ThemeGroupBox: TGroupBox;
    ThemeBox: TComboBox;
    PreviewBtn: TBitBtn;
    XPCheckBox: TCheckBox;
    CachePanel: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    BuildPanel: TPanel;
    YesCache: TRadioButton;
    NoCache: TRadioButton;
    DirCheckBox: TCheckBox;
    DirEdit: TEdit;
    LoadBtn: TSpeedButton;
    ProgressPanel: TPanel;
    pbCCCache: TProgressBar;
    ParseLabel: TLabel;
    SecondPanel: TPanel;
    SecondLabel: TLabel;
    Label5: TLabel;
    YesClassBrowser: TRadioButton;
    NoClassBrowser: TRadioButton;
    FinishPanel: TPanel;
    Label6: TLabel;
    Label4: TLabel;
    Label7: TLabel;
    procedure PreviewBtnClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure XPCheckBoxClick(Sender: TObject);
    procedure DirCheckBoxClick(Sender: TObject);
    procedure LoadBtnClick(Sender: TObject);
    procedure ThemeBoxChange(Sender: TObject);
  private
    HasProgressStarted : boolean;

    function GetSelected: integer;
    procedure CppParserTotalProgress(Sender: TObject; FileName: String; Total, Current: Integer);
    procedure CppParserStartParsing(Sender: TObject);
    procedure CppParserEndParsing(Sender: TObject);

  public
    procedure UpdateList(const List: TStrings);
    property Selected: integer read GetSelected;
  end;

implementation

uses 
  MultiLangSupport, datamod, DevThemes, devcfg, utils, main, version;

{$R *.dfm}

procedure TLangForm.UpdateList;
var
 idx: integer;
 sel: integer;
begin
  ListBox.Clear;
  for idx:= 0 to pred(List.Count) do
  begin
   sel := ListBox.Items.Add(List.Values[List.Names[idx]]);
   if Pos('english', LowerCase(ListBox.Items[sel])) > 0 then
     ListBox.Selected[sel] := True;
  end;
end;

function TLangForm.GetSelected: integer;
begin
  result:= ListBox.ItemIndex;
end;

procedure TLangForm.PreviewBtnClick(Sender: TObject);
begin
  if ThemeBox.ItemIndex =  1 then
    PopupMenu.Images := dmMain.MenuImages_Gnome
  else if ThemeBox.ItemIndex = 2 then
    PopupMenu.Images := dmMain.MenuImages_Blue
  else
    PopupMenu.Images := dmMain.MenuImages_NewLook;
  PopupMenu.Popup(Left + PreviewBtn.Left + ThemeGroupBox.Left +
                  PreviewBtn.Width + 15, Top + PreviewBtn.Top + ThemeGroupBox.Top);
end;

procedure TLangForm.FormActivate(Sender: TObject);
var s : array [0..255] of char;
    d : DWORD;
    sl : TStrings;
begin
  HasProgressStarted := false;
  sl := devTheme.ThemeList;
  ThemeBox.Items.AddStrings(sl);
  sl.Free;
  ThemeBox.ItemIndex := 0;
  Image2.Picture.Bitmap.LoadFromResourceName(HInstance, 'THEMENEWLOOK');
  GetUserName(s, d);
  {$IFDEF BETAVERSION}
   MessageBox(Self.Handle,
              PChar('This is a beta version of Dev-C++.'
              +'Please report bugs at http://bloodshed.net/bugs. We provide updates often, so be sure to check for them in Tools menu, Check for Updates/Packages.'+ #13#13
              +'Your config files will be stored in ' +  ExtractFileDir(devData.INIFile) + #13
              +'Otherwise, you can pass the following parameter to Dev-C++ : -c c:\config_file_directory'),
              PChar('Beta version Notice'), MB_OK);
  {$ENDIF}
end;

procedure TLangForm.CppParserStartParsing(Sender: TObject);
begin
  pbCCCache.Visible := True;
end;

procedure TLangForm.CppParserEndParsing(Sender: TObject);
begin
  pbCCCache.Visible := False;
end;

procedure TLangForm.CppParserTotalProgress(Sender: TObject; FileName: String; Total, Current: Integer);
begin
  if not HasProgressStarted then begin
    pbCCCache.Max := Total;
    HasProgressStarted := true;
  end;
  pbCCCache.Position := pbCCCache.Position + Current;
  if FileName <> '' then
    ParseLabel.Caption :=  'Parsing file: ' + FileName
  else
    ParseLabel.Caption := 'Finalizing... Please wait';
  ParseLabel.Width := 236;
  Application.ProcessMessages;
end;

procedure TLangForm.OkBtnClick(Sender: TObject);
var s, f : TStringList;
    i, j : integer;
begin
  if OkBtn.Tag = 0 then begin
    OkBtn.Tag := 1;
    SecondPanel.Visible := true;
    FirstPanel.Visible := false;
    devData.ThemeChange := true;
    devData.Theme := ThemeBox.Items[ThemeBox.ItemIndex];
    devData.XPTheme := XPCheckBox.Checked;
  end
  else if OkBtn.Tag = 1 then begin
    if YesClassBrowser.Checked then begin
      OkBtn.Tag := 2;
      CachePanel.Visible := true;
      SecondPanel.Visible := false;
    end
    else begin
      OkBtn.Tag := 3;
      OkBtn.Kind := bkOK;
      OkBtn.ModalResult := mrOK;
      FinishPanel.Visible := true;
      SecondPanel.Visible := false;
      devCodeCompletion.Enabled := false;
      devCodeCompletion.UseCacheFiles := false;
      devClassBrowsing.Enabled := false;
      devClassBrowsing.ParseLocalHeaders := false;
      devClassBrowsing.ParseGlobalHeaders := false;
      SaveOptions;
    end;
  end
  else if OkBtn.Tag = 2 then begin
    if YesCache.Checked then begin
       YesCache.Enabled := false;
       NoCache.Enabled := false;
       OkBtn.Enabled := false;
       DirEdit.Enabled := false;
       DirCheckBox.Enabled := false;
       LoadBtn.Enabled := false;
       BuildPanel.Visible := False;
       ProgressPanel.Visible := True;
       OkBtn.Caption := 'Please wait...';
       MainForm.CacheCreated := true;
       Application.ProcessMessages;
       devCodeCompletion.Enabled := true;
       devCodeCompletion.UseCacheFiles := true;
       devClassBrowsing.Enabled := true;
       devClassBrowsing.ParseLocalHeaders := true;
       devClassBrowsing.ParseGlobalHeaders := false;
       SaveOptions;

       MainForm.CppParser1.ParseLocalHeaders := True;
       MainForm.CppParser1.ParseGlobalHeaders := True;
       MainForm.CppParser1.OnStartParsing := CppParserStartParsing;
       MainForm.CppParser1.OnEndParsing := CppParserEndParsing;
       MainForm.CppParser1.OnTotalProgress := CppParserTotalProgress;
       MainForm.CppParser1.Tokenizer:= MainForm.CppTokenizer1;
       MainForm.CppParser1.Enabled := true;

       MainForm.ClassBrowser1.SetUpdateOff;

       s := TStringList.Create;
       if (DirCheckBox.Checked) then
         StrToList(DirEdit.Text, s)
       else
         StrToList(devDirs.Cpp, s);

       f := TStringList.Create;
       for i := 0 to s.Count - 1 do begin
         if DirectoryExists(s[i]) then begin
           FilesFromWildcard(s[i], '*.*', f, false, false, false);
           Screen.Cursor:=crHourglass;
           Application.ProcessMessages;
           for j := 0 to f.Count - 1 do
             MainForm.CppParser1.AddFileToScan(f[j]);
         end
         else
           MessageDlg('Directory "' + s[i] + '" does not exists', mtWarning, [mbOK], 0);
       end;
       MainForm.CppParser1.ParseList;
       ParseLabel.Caption := 'Finalizing... Please wait';
       Application.ProcessMessages;
       MainForm.CppParser1.Save(devDirs.Config+DEV_COMPLETION_CACHE);

       MainForm.CppParser1.OnStartParsing := MainForm.CppParser1StartParsing;;
       MainForm.CppParser1.OnEndParsing := MainForm.CppParser1EndParsing;
       MainForm.CppParser1.OnTotalProgress := MainForm.CppParser1TotalProgress;

       MainForm.ClassBrowser1.SetUpdateOn;

     { MainForm.ClassBrowser1.Parser := MainForm.CppParser1;
       MainForm.CodeCompletion1.Parser := MainForm.CppParser1;}

       Application.ProcessMessages;
       Screen.Cursor:=crDefault;
       s.Free;
       f.Free;
    end
    else begin
      devClassBrowsing.Enabled := true;
      devClassBrowsing.ParseLocalHeaders := true;
      devClassBrowsing.ParseGlobalHeaders := false;
    end;
    OkBtn.Tag := 3;
    OkBtn.Kind := bkOK;
    OkBtn.ModalResult := mrOK;
    OkBtn.Enabled := true;
    FinishPanel.Visible := true;
    CachePanel.Visible := false;
  end
end;

procedure TLangForm.XPCheckBoxClick(Sender: TObject);
begin
  if XPCheckBox.Checked then
    XPMenu.Active := true
  else
    XPMenu.Active := false;
end;

procedure TLangForm.DirCheckBoxClick(Sender: TObject);
begin
  DirEdit.Enabled := DirCheckBox.Checked;
  if DirEdit.Enabled then
    DirEdit.Color := clCaptionText
  else
    DirEdit.Color := clInactiveCaptionText;
end;

procedure TLangForm.LoadBtnClick(Sender: TObject);
var
{$IFDEF WIN32}
  s: string;
{$ENDIF}
{$IFDEF LINUX}
  s: WideString;
{$ENDIF}
begin
  if SelectDirectory('Include Directory', '', s) then
    DirEdit.Text := s;
end;

procedure TLangForm.ThemeBoxChange(Sender: TObject);
begin
  case ThemeBox.ItemIndex of
  1:
    Image2.Picture.Bitmap.LoadFromResourceName(HInstance, 'THEMEGNOME');
  2:
    Image2.Picture.Bitmap.LoadFromResourceName(HInstance, 'THEMEBLUE');
  else
    Image2.Picture.Bitmap.LoadFromResourceName(HInstance, 'THEMENEWLOOK');
  end;
end;

end.
