unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.Styles.Ext,
  Vcl.ActnList, Vcl.PlatformDefaultStyleActnCtrls, Vcl.ActnMan, Vcl.ToolWin;

type
  TFrmMain = class(TForm)
    ListView1: TListView;
    Label1: TLabel;
    Button1: TButton;
    ActionManager1: TActionManager;
    ActionApplyStyle: TAction;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ActionApplyStyleUpdate(Sender: TObject);
    procedure ActionApplyStyleExecute(Sender: TObject);
    procedure ListView1SelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    Loading : Boolean;
    FStylesPath : string;
    FPreview:TVclStylesPreview;
    procedure FillVclStylesList;
    procedure ClearVclStylesList;
  public
  end;



var
  FrmMain: TFrmMain;

implementation
uses
  IOUtils,
  Vcl.Themes,
  Vcl.Styles;

type
 TVclStylesPreviewClass = class(TVclStylesPreview);

{$R *.dfm}

procedure TFrmMain.FormCreate(Sender: TObject);
begin
   Loading:=False;
   FStylesPath:= IncludeTrailingPathDelimiter(ExpandFileName(ExtractFilePath(ParamStr(0))  + '\..\Styles'));
   ReportMemoryLeaksOnShutdown:=True;
   FPreview:=TVclStylesPreview.Create(Self);
   FPreview.Parent:=Panel1;
   FPreview.BoundsRect := Panel1.ClientRect;
   FillVclStylesList;
end;

procedure TFrmMain.FormDestroy(Sender: TObject);
begin
  ClearVclStylesList;
  FPreview.Free;
end;

procedure TFrmMain.FormShow(Sender: TObject);
begin
   if ListView1.Items.Count>0 then
    ListView1.Selected:=ListView1.Items.Item[0];
end;

procedure TFrmMain.ListView1SelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
   LStyle : TCustomStyle;
begin
  if Selected then
  begin
   LStyle:=nil;
   if Assigned(Item.Data) then
     LStyle:=TCustomStyleExt(Item.Data)
   else
   if not Loading then
   begin
     LStyle := TCustomStyleExt.Create(FStylesPath+Item.SubItems[0]);
     Item.Data:= LStyle;
   end;

   if Assigned(LStyle) and not Loading  then
   begin
     FPreview.Caption:=Item.SubItems[1];
     FPreview.Style:=LStyle;
     TVclStylesPreviewClass(FPreview).Paint;
   end;
  end;
end;

procedure TFrmMain.ActionApplyStyleExecute(Sender: TObject);
begin
  if (ListView1.Selected<>nil) and (ListView1.Selected.Caption='Resource') then
   TStyleManager.SetStyle(ListView1.Selected.SubItems[1]);
end;

procedure TFrmMain.ActionApplyStyleUpdate(Sender: TObject);
begin
 TCustomAction(Sender).Enabled:=(ListView1.Selected<>nil) and (ListView1.Selected.Caption='Resource');
end;

procedure TFrmMain.ClearVclStylesList;
var
 i : integer;
begin
 for i:=0 to ListView1.Items.Count-1 do
  if Assigned(ListView1.Items[i].Data) then
    TCustomStyleExt(ListView1.Items[i].Data).Free;
 ListView1.Items.Clear;
end;

procedure TFrmMain.FillVclStylesList;
Var
 StyleName: string;
 Item     : TListItem;
 StyleInfo:  TStyleInfo;
 SourceInfo: TSourceInfo;
 VCLStyleExt:TCustomStyleServices;
begin
   Loading:=True;
               {
   for FileName in TDirectory.GetFiles(FStylesPath,'*.vsf') do
   begin
      Item:=ListView1.Items.Add;
      Item.Caption:='File';
      Item.SubItems.Add(ExtractFileName(FileName));
      TStyleManager.IsValidStyle(FileName, StyleInfo);
      Item.SubItems.Add(StyleInfo.Name);
      Item.SubItems.Add(StyleInfo.Author);
      Item.SubItems.Add(StyleInfo.AuthorURL);
      Item.SubItems.Add(StyleInfo.Version);
   end;
            }

   for StyleName in  TStyleManager.StyleNames do
     if not SameText(StyleName,'Windows') then
     begin
        Item:=ListView1.Items.Add;
        Item.Caption:='Resource';
        Item.SubItems.Add('');

        SourceInfo:=TStyleManager.StyleSourceInfo[StyleName];
        VCLStyleExt:=TCustomStyleExt.Create(TStream(SourceInfo.Data));

        Item.Data  :=VCLStyleExt;
        StyleInfo  :=TCustomStyleExt(VCLStyleExt).StyleInfo;
        Item.SubItems.Add(StyleInfo.Name);
        Item.SubItems.Add(StyleInfo.Author);
        Item.SubItems.Add(StyleInfo.AuthorURL);
        Item.SubItems.Add(StyleInfo.Version);
     end;

   Loading:=False;
end;



end.
