unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.PlatformDefaultStyleActnCtrls,
  Vcl.ActnList, Vcl.ActnMan, Vcl.ToolWin, Vcl.ActnCtrls,
  Vcl.ActnMenus, Vcl.StdActns, Vcl.ExtActns, Vcl.XPStyleActnCtrls,
  Vcl.StdStyleActnCtrls, Vcl.StdCtrls, Vcl.Menus, Vcl.ActnPopup;

type
  TForm4 = class(TForm)
    ActionMainMenuBar1: TActionMainMenuBar;
    ActionManager1: TActionManager;
    EditCut1: TEditCut;
    EditCopy1: TEditCopy;
    EditPaste1: TEditPaste;
    EditSelectAll1: TEditSelectAll;
    EditUndo1: TEditUndo;
    EditDelete1: TEditDelete;
    FormatRichEditBold1: TRichEditBold;
    FormatRichEditItalic1: TRichEditItalic;
    FormatRichEditUnderline1: TRichEditUnderline;
    FormatRichEditStrikeOut1: TRichEditStrikeOut;
    FormatRichEditBullets1: TRichEditBullets;
    FormatRichEditAlignLeft1: TRichEditAlignLeft;
    FormatRichEditAlignRight1: TRichEditAlignRight;
    FormatRichEditAlignCenter1: TRichEditAlignCenter;
    HelpContents1: THelpContents;
    HelpTopicSearch1: THelpTopicSearch;
    HelpOnHelp1: THelpOnHelp;
    HelpContextAction1: THelpContextAction;
    WindowClose1: TWindowClose;
    WindowCascade1: TWindowCascade;
    WindowTileHorizontal1: TWindowTileHorizontal;
    WindowTileVertical1: TWindowTileVertical;
    WindowMinimizeAll1: TWindowMinimizeAll;
    WindowArrange1: TWindowArrange;
    FileOpen1: TFileOpen;
    FileOpenWith1: TFileOpenWith;
    FileSaveAs1: TFileSaveAs;
    FilePrintSetup1: TFilePrintSetup;
    FilePageSetup1: TFilePageSetup;
    FileRun1: TFileRun;
    FileExit1: TFileExit;
    BrowseForFolder1: TBrowseForFolder;
    Button1: TButton;
    PopupActionBar1: TPopupActionBar;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.dfm}

uses
  Vcl.PlatformVclStylesActnCtrls;

procedure TForm4.Button1Click(Sender: TObject);
begin
  ActionManager1.Style:=PlatformVclStylesStyle;
end;

procedure TForm4.FormCreate(Sender: TObject);
var
  MenuItem: TMenuItem;
begin
  MenuItem := TMenuItem.Create(PopupActionBar1);
  MenuItem.Action := FileOpen1;
  PopupActionBar1.Items.Add(MenuItem);

  MenuItem := TMenuItem.Create(PopupActionBar1);
  MenuItem.Action := FileOpenWith1;
  PopupActionBar1.Items.Add(MenuItem);

  MenuItem := TMenuItem.Create(PopupActionBar1);
  MenuItem.Action := FileSaveAs1;
  PopupActionBar1.Items.Add(MenuItem);

  MenuItem := TMenuItem.Create(PopupActionBar1);
  MenuItem.Action := FilePrintSetup1;
  PopupActionBar1.Items.Add(MenuItem);

  MenuItem := TMenuItem.Create(PopupActionBar1);
  MenuItem.Action := FilePageSetup1;
  PopupActionBar1.Items.Add(MenuItem);

  MenuItem := TMenuItem.Create(PopupActionBar1);
  MenuItem.Action := FileRun1;
  PopupActionBar1.Items.Add(MenuItem);

  MenuItem := TMenuItem.Create(PopupActionBar1);
  MenuItem.Action := FileExit1;
  PopupActionBar1.Items.Add(MenuItem);

  Screen.MenuFont.Name := 'Impact';
  Screen.MenuFont.Size := 12;
end;


end.
