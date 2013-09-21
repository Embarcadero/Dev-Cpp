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

unit Splash;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, Version;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Classes, QGraphics, QControls, QForms, QDialogs,
  QExtCtrls, QComCtrls, Version;
{$ENDIF}

type
  TSplashForm = class(TForm)
    Panel: TPanel;
    Image: TImage;
    StatusBar: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure ImageClick(Sender: TObject);
  end;

var
  SplashForm: TSplashForm;

implementation

uses 
  devcfg;

{$R *.dfm}

procedure TSplashForm.FormCreate(Sender: TObject);
begin
  if (devData.Splash <> '') and FileExists(devData.Splash) then
   begin
     Image.Picture.LoadFromFile(devData.Splash);
     ClientWidth:= Image.Width;
     ClientHeight:= Image.Height + StatusBar.Height;
   end;
  StatusBar.SimpleText := 'Bloodshed Dev-C++ '+ DEVCPP_VERSION +'. Loading...';
end;

procedure TSplashForm.ImageClick(Sender: TObject);
begin
  close;
end;

end.
