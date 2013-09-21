{----------------------------------------------------------------------------------

  The contents of this file are subject to the GNU General Public License
  Version 1.1 or later (the "License"); you may not use this file except in
  compliance with the License. You may obtain a copy of the License at
  http://www.gnu.org/copyleft/gpl.html

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
  the specific language governing rights and limitations under the License.

  The Initial Developer of the Original Code is Peter Schraut.
  http://www.console-dev.de

  Portions created by Peter Schraut are Copyright
  (C) 2004 by Peter Schraut (http://www.console-dev.de) 
  All Rights Reserved.
  
  
  History:
    11th April 2004 - Initial release

    
  Known bugs:
  
----------------------------------------------------------------------------------}

unit ImageTheme;

interface

uses 
{$IFDEF WIN32}
  Windows, SysUtils, Classes, Controls, Graphics, Contnrs;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Classes, QControls, QGraphics, Contnrs, QImgList;
{$ENDIF}

type
  TImageThemeClass = class of TCustomImageTheme;

  TCustomImageTheme = class
  private
    FBitmap: TBitmap;
    FFilename: AnsiString;
    FOnChange: TNotifyEvent;
    FTitle: AnsiString;
    procedure SetBitmap(const Bmp: TBitmap);
    procedure OnChangeEvent(Sender: TObject);
  protected
    procedure Changed; virtual;
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property Filename: AnsiString read FFilename;
    property Title: AnsiString read FTitle;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure LoadFromFile(AFilename: AnsiString); virtual;
  end;

  TDevImageTheme = class(TCustomImageTheme)
  private
    FMenuImages,
    FHelpImages,
    FProjectImages,
    FSpecialImages,
    FBrowserImages: TImageList;
    procedure SetMenuImages(const Img: TImageList);
    procedure SetHelpImages(const Img: TImageList);
    procedure SetProjectImages(const Img: TImageList);
    procedure SetSpecialImages(const Img: TImageList);
    procedure SetBrowserImages(const Img: TImageList);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure LoadFromFile(AFilename: AnsiString); override;
    procedure SaveToFile(AFilename: AnsiString);

    property MenuImages: TImageList read FMenuImages write SetMenuImages;
    property HelpImages: TImageList read FHelpImages write SetHelpImages;
    property ProjectImages: TImageList read FProjectImages write SetProjectImages;
    property SpecialImages: TImageList read FSpecialImages write SetSpecialImages;
    property BrowserImages: TImageList read FBrowserImages write SetBrowserImages;

    property Filename;
    property Title;
  end;


  TGnomeImageTheme = class(TDevImageTheme)
  public
    constructor Create; override;
  end;
  

  TNewLookImageTheme = class(TDevImageTheme)
  public
    constructor Create; override;
  end;

  
  TBlueImageTheme = class(TDevImageTheme)
  public
    constructor Create; override;
  end;


  TCustomImageThemeFactory = class
  private
    FCurrentTheme: TCustomImageTheme;
    FThemes: TObjectList;
    function GetTheme(Index: Integer): TCustomImageTheme;
    function GetThemeFilename(Index: Integer): AnsiString;
    function GetThemeTitle(Index: Integer): AnsiString;
    procedure SetCurrentTheme(const ATheme: TCustomImageTheme);
  protected
    procedure DoCreateThemeFromFile(AFilename: AnsiString); virtual;
    property CurrentTheme: TCustomImageTheme read FCurrentTheme write SetCurrentTheme;
    property Themes[Index: Integer]: TCustomImageTheme read GetTheme;
    property ThemeFilename[Index: Integer]: AnsiString read GetThemeFilename;
    property ThemeTitle[Index: Integer]: AnsiString read GetThemeTitle;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function ActivateTheme(ATheme: TCustomImageTheme): Boolean; overload;
    function ActivateTheme(AThemeTitle: AnsiString): Boolean; overload;
    procedure AddTheme(const ATheme: TCustomImageTheme);
    function Count: Integer;
    function IndexOf(const AThemeTitle: AnsiString): Integer;
    procedure GetThemeTitles(ADest: TStrings);
    procedure LoadFromDirectory(ADirectory: AnsiString);
    procedure RegisterTheme(const ThemeClass: TImageThemeClass);
  end;

  TDevImageThemeChanged = procedure(Sender: TObject; const OldTheme: TDevImageTheme; const NewTheme: TDevImageTheme);

  TDevImageThemeFactory = class(TCustomImageThemeFactory)
  private
    FOnThemeChanged: TDevImageThemeChanged;
    function GetTheme(Index: Integer): TDevImageTheme;
    function GetCurrentTheme: TDevImageTheme;
    procedure SetCurrentTheme(const ATheme: TDevImageTheme);
  protected
    procedure DoCreateThemeFromFile(AFilename: AnsiString); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property CurrentTheme: TDevImageTheme read GetCurrentTheme write SetCurrentTheme;
    property Themes[Index: Integer]: TDevImageTheme read GetTheme;
    property ThemeFilename;
    property ThemeTitle;
    property OnThemeChanged: TDevImageThemeChanged read FOnThemeChanged write FOnThemeChanged;
  end;
  
var
  devImageThemes: TDevImageThemeFactory;
  
implementation

uses
  DataMod;
  
//----------------- TCustomImageTheme ----------------------------------------------------------------------------------

constructor TCustomImageTheme.Create;
begin
  inherited;

  FBitmap := TBitmap.Create;
  FBitmap.OnChange := OnChangeEvent;
  FFilename := '';
  FTitle := '';
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TCustomImageTheme.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomImageTheme.OnChangeEvent(Sender: TObject);
begin
  Changed;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomImageTheme.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomImageTheme.LoadFromFile(AFilename: AnsiString);
begin
  FFilename := AFilename;
  FBitmap.LoadFromFile(AFilename);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomImageTheme.SetBitmap(const Bmp: TBitmap);
begin
  FBitmap.Assign(Bmp);
end;

//----------------- TDevImageTheme -------------------------------------------------------------------------------

constructor TDevImageTheme.Create;

	function _CreateImageList: TImageList;
	begin
		Result := TImageList.Create(nil);
		//Result.OnChange := OnChangeEvent;
		Result.Masked := True;
	end;

begin
  inherited;

  FMenuImages := _CreateImageList;
  FHelpImages := _CreateImageList;
  FProjectImages := _CreateImageList;
  FSpecialImages := _CreateImageList;
  FBrowserImages := _CreateImageList;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TDevImageTheme.Destroy;
begin
  FMenuImages.Free;
  FHelpImages.Free;
  FProjectImages.Free;
  FSpecialImages.Free;
  FBrowserImages.Free;
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

const
  cTileW=16;
  cTileH=16;
  
procedure TDevImageTheme.LoadFromFile(AFilename: AnsiString);
var
  Bmp: TBitmap;

  procedure LoadImageLine(ALine: Integer);
  begin
    Bmp.FreeImage;
    Bmp.PixelFormat := pf24bit;
    Bmp.Width := Bitmap.Width;
    Bmp.Height := Bitmap.Height;
    
    if (ALine*cTileH+cTileH) <= Bitmap.Height then
      Bmp.Canvas.CopyRect(Rect(0,0,FBitmap.Width,cTileH), Bitmap.Canvas, Rect(0,ALine*cTileH,Bitmap.Width,ALine*cTileH+cTileH))
    else
    begin
      Bmp.Canvas.Brush.Color := clBtnFace;
      Bmp.Canvas.FillRect(Rect(0,0,Bmp.Width,Bmp.Height));
    end;
  end;

  procedure MakeImageList(List: TImageList; ALine: Integer);
  begin
    LoadImageLine(ALine);
    List.AddMasked(Bmp, Bmp.Canvas.Pixels[0,cTileH-1]);
  end;
begin

  inherited;
  
  Bmp := TBitmap.Create;
  try
    MakeImageList(FMenuImages,    0);
    MakeImageList(FHelpImages,    1);
    MakeImageList(FProjectImages, 2);
    MakeImageList(FSpecialImages, 3);
    MakeImageList(FBrowserImages, 4);
  finally
    Bmp.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDevImageTheme.SaveToFile(AFilename: AnsiString);
var
  I: Integer;
  X,Y: Integer;
  MaxW: Integer;
  ImgLists: array [0..4] of TImageList;
  Bmp: TBitmap;
  SingleBmp: TBitmap;
begin
  
  ImgLists[0] := FMenuImages;
  ImgLists[1] := FHelpImages;
  ImgLists[2] := FProjectImages;
  ImgLists[3] := FSpecialImages;
  ImgLists[4] := FBrowserImages;

  MaxW := 0;
  for I := Low(ImgLists) to High(ImgLists) do
    if (ImgLists[I].Count*cTileW)+cTileW > MaxW then
      MaxW := ImgLists[I].Count*cTileW;

  Bmp := TBitmap.Create;
  SingleBmp := TBitmap.Create;
  try
    SingleBmp.Transparent := True;
    SingleBmp.TransparentMode := tmAuto;
    
    Bmp.Height := cTileH * High(ImgLists) + cTileH;
    Bmp.Width := MaxW;
    Bmp.Canvas.Brush.Color := clFuchsia;
    Bmp.Canvas.FillRect(Rect(0,0,Bmp.Width,Bmp.Height));

    for Y := Low(ImgLists) to High(ImgLists) do
    begin
      for X := 0 to ImgLists[Y].Count-1 do
      begin
        if ImgLists[Y].GetBitmap(X, SingleBmp) then
        begin
          //SingleBmp.TransparentColor := SingleBmp.Canvas.Pixels[0,cTileH-1];
          Bmp.Canvas.Draw(X*cTileW, Y*cTileW, SingleBmp);
        end;
      end; 
    end;

    Bmp.SaveToFile(AFilename);
  finally
    Bmp.Free;
    SingleBmp.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDevImageTheme.SetMenuImages(const Img: TImageList);
begin
  if Img <> FMenuImages then
  begin
    FMenuImages.Assign(Img);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDevImageTheme.SetHelpImages(const Img: TImageList);
begin
  if Img <> FHelpImages then
  begin
    FHelpImages.Assign(Img);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDevImageTheme.SetProjectImages(const Img: TImageList);
begin
  if Img <> FProjectImages then
  begin
    FProjectImages.Assign(Img);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDevImageTheme.SetSpecialImages(const Img: TImageList);
begin
  if Img <> FSpecialImages then
  begin
    FSpecialImages.Assign(Img);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDevImageTheme.SetBrowserImages(const Img: TImageList);
begin
  if Img <> FBrowserImages then
  begin
    FBrowserImages.Assign(Img);
  end;
end;

//----------------- TGnomeImageTheme -----------------------------------------------------------------------------------

constructor TGnomeImageTheme.Create;
begin
  inherited;

  MenuImages    := dmMain.MenuImages_Gnome;
  HelpImages    := dmMain.HelpImages_Gnome;
  ProjectImages := dmMain.ProjectImage_Gnome;
  SpecialImages := dmMain.SpecialImages_Gnome;
  BrowserImages := dmMain.ClassImages;

  FTitle := 'Gnome';
end;

//----------------- TNewLookImageTheme ---------------------------------------------------------------------------------

constructor TNewLookImageTheme.Create;
begin
  inherited;

  MenuImages    := dmMain.MenuImages_NewLook;
  HelpImages    := dmMain.HelpImages_NewLook;
  ProjectImages := dmMain.ProjectImage_NewLook;
  SpecialImages := dmMain.SpecialImages_NewLook;
  BrowserImages := dmMain.ClassImages;

  FTitle := 'New Look';
end;

//----------------- TBlueImageTheme ------------------------------------------------------------------------------------

constructor TBlueImageTheme.Create;
begin
  inherited;

  MenuImages    := dmMain.MenuImages_Blue;
  HelpImages    := dmMain.HelpImages_Blue;
  ProjectImages := dmMain.ProjectImage_Blue;
  SpecialImages := dmMain.SpecialImages_Blue;
  BrowserImages := dmMain.ClassImages;

  FTitle := 'Blue';
end;

//----------------- TCustomImageThemeFactory ---------------------------------------------------------------------------

constructor TCustomImageThemeFactory.Create;
begin
	inherited;
	FThemes := TObjectList.Create(True);
end;

destructor TCustomImageThemeFactory.Destroy;
begin
	FThemes.Free;
	inherited;
end;

function TCustomImageThemeFactory.ActivateTheme(ATheme: TCustomImageTheme): Boolean;
begin
  CurrentTheme := ATheme;
  Result := True;
end;

function TCustomImageThemeFactory.ActivateTheme(AThemeTitle: AnsiString): Boolean;
var
  I: Integer;
begin
  Result := False;
  
  I := IndexOf(AThemeTitle);

  if I > -1 then
    Result := ActivateTheme(Themes[I]);
end;

procedure TCustomImageThemeFactory.AddTheme(const ATheme: TCustomImageTheme);
begin
  FThemes.Add(ATheme);
end;

function TCustomImageThemeFactory.Count: Integer;
begin
  Result := FThemes.Count;
end;

procedure TCustomImageThemeFactory.DoCreateThemeFromFile(AFilename: AnsiString);
begin
end;

function TCustomImageThemeFactory.GetTheme(Index: Integer): TCustomImageTheme;
begin
  Result := TCustomImageTheme(FThemes[Index]);
end;

function TCustomImageThemeFactory.GetThemeFilename(Index: Integer): AnsiString;
begin
  Result := TCustomImageTheme(FThemes[Index]).Filename;
end;

procedure TCustomImageThemeFactory.GetThemeTitles(ADest: TStrings);
var
  I: Integer;
begin
  Assert(ADest <> nil, 'ADest must not be nil');

  ADest.BeginUpdate;
  try
    for I := 0 to Count-1 do
      ADest.Add(Themes[I].Title)
  finally
    ADest.EndUpdate;
  end;
end;

function TCustomImageThemeFactory.IndexOf(const AThemeTitle: AnsiString): Integer;
var
  I: Integer;
begin
  Result := -1;

  for I := 0 to Count-1 do
    if SameText(AThemeTitle, Themes[I].Title) then
    begin
      Result := I;
      Break;
    end;
end;

function TCustomImageThemeFactory.GetThemeTitle(Index: Integer): AnsiString;
begin
  Result := TCustomImageTheme(FThemes[Index]).Title;
end; 

procedure TCustomImageThemeFactory.LoadFromDirectory(ADirectory: AnsiString);
var
  F: TSearchRec;
begin
  if not DirectoryExists(ADirectory) then Exit;

  if ADirectory[Length(ADirectory)] <> '\' then
    ADirectory := ADirectory + '\';
  
  if FindFirst(ADirectory + '*.bmp', faAnyFile, F) = 0 then
    repeat
      DoCreateThemeFromFile(ADirectory + F.Name);
    until FindNext(F) <> 0;

  FindClose(F);
end;

procedure TCustomImageThemeFactory.RegisterTheme(const ThemeClass: TImageThemeClass);
var
  NewTheme: TCustomImageTheme;
begin
  NewTheme := ThemeClass.Create;
  FThemes.Add(NewTheme);
end;

procedure TCustomImageThemeFactory.SetCurrentTheme(const ATheme: TCustomImageTheme);
begin
  if FCurrentTheme <> ATheme then
  begin
    
    if Assigned(ATheme) and (FThemes.IndexOf(ATheme) < 0) then
      FThemes.Add(ATheme);

    FCurrentTheme := ATheme;
  end;
end;

//----------------- TDevImageThemeFactory ------------------------------------------------------------------------------

constructor TDevImageThemeFactory.Create;
begin
  inherited;

  FOnThemeChanged := nil;
  RegisterTheme(TNewLookImageTheme);
  RegisterTheme(TGnomeImageTheme);
  RegisterTheme(TBlueImageTheme);

  FCurrentTheme := Themes[0];
end;

destructor TDevImageThemeFactory.Destroy;
begin
	FOnThemeChanged := nil;
	inherited;
end;

procedure TDevImageThemeFactory.DoCreateThemeFromFile(AFilename: AnsiString);
var
	NewTheme: TDevImageTheme;
begin
	if FileExists(AFilename) then begin
		NewTheme := TDevImageTheme.Create;
		NewTheme.LoadFromFile(AFilename);
		NewTheme.FTitle := ChangeFileExt(ExtractFileName(AFilename), '');

	AddTheme(NewTheme);
	end;
end;

function TDevImageThemeFactory.GetCurrentTheme: TDevImageTheme;
begin
	Result := TDevImageTheme(inherited CurrentTheme);
end;

procedure TDevImageThemeFactory.SetCurrentTheme(const ATheme: TDevImageTheme);
var
	OldTheme: TDevImageTheme;
begin
	OldTheme := CurrentTheme;

	inherited CurrentTheme := ATheme;

	if Assigned(FOnThemeChanged) then
		FOnThemeChanged(Self, OldTheme, ATheme);
end;

function TDevImageThemeFactory.GetTheme(Index: Integer): TDevImageTheme;
begin
	Result := TDevImageTheme(inherited Themes[Index]);
end;

end.
