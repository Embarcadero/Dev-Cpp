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

unit DevThemes;

interface

uses
{$IFDEF WIN32}
  Classes, Controls;
{$ENDIF}
{$IFDEF LINUX}
  Classes, QControls, QImgList;
{$ENDIF}

type
 TdevTheme = class(TObject)
  private
   fThemes: TStringList;
   fFile: AnsiString;
   fName: AnsiString;
   fMenus: TImageList;
   fHelp: TImageList;
   fProjects: TImageList;
   fSpecials: TImageList;
   fBrowser: TImageList;
   fImgfiles: TStringList;
   procedure ClearLists;
   function GetImage(const Index: Integer; var imglst: TImageList): boolean;
  public
   constructor Create;
   destructor Destroy; override;

   procedure ScanThemes;
   function ThemeList: TStrings;
   function SetTheme(const theme: AnsiString): boolean;
   function LoadTheme(const FileName: AnsiString): boolean;

   property Name: AnsiString read fName;
   property Menus: TImageList read fMenus;
   property Help: TImageList read fHelp;
   property Projects: TImageList read fProjects;
   property Specials: TImageList read fSpecials;
   property Browser: TImageList read fBrowser;
 end;

var
 devTheme: TdevTheme = nil;

implementation

uses 
{$IFDEF WIN32}
  SysUtils, Forms, Graphics, devcfg, utils, datamod, version;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, QForms, QGraphics, devcfg, utils, datamod, version;
{$ENDIF}

{ TdevTheme }

constructor TdevTheme.Create;
begin
  fThemes:= TStringList.Create;
  fMenus:= TImageList.Create(nil);
  fHelp:= TImageList.Create(nil);
  fProjects:= TImageList.Create(nil);
  fSpecials:= TImageList.Create(nil);
  fBrowser:= TImageList.Create(nil);
  fMenus.Masked:= True;
  fHelp.Masked:= True;
  fProjects.Masked:= True;
  fSpecials.Masked:= True;
  fBrowser.Masked:= True;

  ScanThemes;
end;

destructor TdevTheme.Destroy;
begin
  fThemes.Free;

  fMenus.Clear;
  fHelp.Clear;
  fProjects.Clear;
  fSpecials.Clear;
  fBrowser.Clear;

  fMenus.Free;
  fHelp.Free;
  fProjects.Free;
  fSpecials.Free;
  fBrowser.Free;
  inherited;
end;

procedure TdevTheme.ClearLists;
begin
  fMenus.Clear;
  fHelp.Clear;
  fProjects.Clear;
  fSpecials.Clear;
  fBrowser.Clear;
end;

function TdevTheme.GetImage(const Index: Integer; var imglst: TImageList): boolean;
var
 idx: Integer;
 aFile: AnsiString;
 img: TBitmap;
 clr: TColor;
begin
  try
   idx:= fimgfiles.IndexofName(IntToStr(index));
   if idx <> -1 then
    aFile:= ExpandFileto(fimgFiles.ValueFromIndex[idx], ExtractFilePath(fFile))
   else
    aFile:= '';

   img:= TBitmap.Create;
   try
    if (aFile <> '') and (FileExists(aFile)) then
     img.LoadFromFile(aFile)
    else
     img.LoadFromResourceName(HInstance, 'NOIMG');

    clr:= img.Canvas.Pixels[0, 15];
    imglst.AddMasked(img, clr);
   finally
    img.Free;
   end;
   Result:= True;
  except
   Result:= False;
  end;
end;

function TdevTheme.SetTheme(const theme: AnsiString): boolean;
var
	idx: Integer;
begin
	Result:= False;
	if theme = fName then Exit;

	// yes, I'm duplicating stuff three times, but it's of little use creating a function for this...
	if SameStr(theme,DEV_NEWLOOK_THEME) then begin
		fMenus.Clear;
		fHelp.Clear;
		fProjects.Clear;
		fSpecials.Clear;
		fBrowser.Clear;
		fMenus.AddImages(dmMain.MenuImages_NewLook);
		fHelp.AddImages(dmMain.HelpImages_NewLook);
		fProjects.AddImages(dmMain.ProjectImage_NewLook);
		fSpecials.AddImages(dmMain.SpecialImages_NewLook);
		fBrowser.AddImages(dmMain.ClassImages);
		result:= True;
		fFile:= DEV_INTERNAL_THEME;
	end else if SameStr(theme,DEV_GNOME_THEME) then begin
		fMenus.Clear;
		fHelp.Clear;
		fProjects.Clear;
		fSpecials.Clear;
		fBrowser.Clear;
		fMenus.AddImages(dmMain.MenuImages_Gnome);
		fHelp.AddImages(dmMain.HelpImages_Gnome);
		fProjects.AddImages(dmMain.ProjectImage_Gnome);
		fSpecials.AddImages(dmMain.SpecialImages_Gnome);
		fBrowser.AddImages(dmMain.ClassImages);
		result:= True;
		fFile:= DEV_INTERNAL_THEME;
	end else if SameStr(theme,DEV_BLUE_THEME) then begin
		fMenus.Clear;
		fHelp.Clear;
		fProjects.Clear;
		fSpecials.Clear;
		fBrowser.Clear;
		fMenus.AddImages(dmMain.MenuImages_Blue);
		fHelp.AddImages(dmMain.HelpImages_Blue);
		fProjects.AddImages(dmMain.ProjectImage_Blue);
		fSpecials.AddImages(dmMain.SpecialImages_Blue);
		fBrowser.AddImages(dmMain.ClassImages);
		result:= True;
		fFile:= DEV_INTERNAL_THEME;
	end else begin  // load theme from file
		for idx := 0 to fThemes.Count - 1 do
			if SameStr(Theme,fThemes.ValueFromIndex[idx]) then begin
				Result := LoadTheme(fThemes.Names[idx]);
				break;
			end;
	end;
end;

function TdevTheme.LoadTheme(const FileName: AnsiString): boolean;
const
 MNU_CNT = 47;
 MNU_OFF = 1000;
 HLP_CNT = 7;
 HLP_OFF = 1100;
 PRJ_CNT = 4;
 PRJ_OFF = 1200;
 SPL_CNT = 4;
 SPL_OFF = 1300;
 BRW_CNT = 8;
 BRW_OFF = 1400;
var
 I: Integer;
 fName: AnsiString;
begin
//  Open file and load images into lists
//  if image isn't found load "NOIMG" bitmap from resources
	Result:= False;
	fName:= ValidateFile(FileName, devDirs.Themes);
	if fName = '' then begin
//		MessageDlg('Could not open Theme File ', +FileName, mtErrorm [mbOk], 0);
		Exit;
	end;

	fFile:= fName;
	ClearLists;
	fimgFiles:= TStringList.Create;
	with fimgfiles do
		try
			LoadFromFile(FName);

			fName:= fimgFiles.Values['Name'];

			// fill menu
			for I:= 0 to pred(MNU_CNT) do
				GetImage(I +MNU_OFF, fMenus);

			// fill Help
			for I:= 0 to pred(HLP_CNT) do
				GetImage(I +HLP_OFF, fHelp);

			// fill Projects
			for I:= 0 to pred(PRJ_CNT) do
				GetImage(I +PRJ_OFF, fProjects);

			// fill Specials
			for I:= 0 to pred(SPL_CNT) do
				GetImage(I +SPL_OFF, fSpecials);

			// fill Browser
			for I:= 0 to pred(BRW_CNT) do
				GetImage(I +BRW_OFF, fBrowser);
		finally
			Free;
		end;
	Result:= True;
end;

function TdevTheme.ThemeList: TStrings;
var
	I : integer;
begin
	Result := TStringList.Create;
	for I := 0 to fthemes.Count - 1 do
		Result.Add(fThemes.ValueFromIndex[I]);

	// Caller needs to free our crap
end;

procedure TdevTheme.ScanThemes;
var
	tmp: TStringList;
	idx: Integer;
begin
	fThemes.Clear;
	fThemes.Add(DEV_NEWLOOK_THEME+'='+DEV_NEWLOOK_THEME);
	fThemes.Add(DEV_GNOME_THEME+'='+DEV_GNOME_THEME);
	fThemes.Add(DEV_BLUE_THEME+'='+DEV_BLUE_THEME);

	if devDirs.Themes = '' then Exit;

	FilesFromWildCard(devDirs.Themes, '*.thm',TStringList(fThemes), True, False, True);

	// Found more themes in the Themes folder?
	if fThemes.Count > 2 then begin
		tmp:= TStringList.Create;
		try
			for idx:= 3 to pred(fThemes.Count) do begin
				tmp.LoadFromfile(fThemes[idx]);
				if tmp.Values['Name'] = '' then
					fThemes[idx]:= Format('%s=%s', [fThemes[idx],ChangefileExt(ExtractFileName(fThemes[idx]), '')])
				else
					fThemes[idx]:= Format('%s=%s', [fThemes[idx], tmp.Values['Name']]);
			end;
		finally
			tmp.Free;
		end;
	end;
end;

end.
