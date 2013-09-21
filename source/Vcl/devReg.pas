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

unit devReg;

interface

uses
{$IFDEF WIN32}
 Classes, Controls, devTabs, ColorPickerButton, devFileMonitor,
{$ENDIF}
{$IFDEF LINUX}
 Classes, QControls, devTabs, ColorPickerButton, devFileMonitor,
{$ENDIF}

{$IFDEF VER130}
 DsgnIntf
{$ELSE}
 DesignEditors,
 DesignIntf
{$ENDIF};

type
 TdevPageEditor = class(TComponentEditor)
  function GetVerb(index: integer): string; override;
  function GetVerbCount: integer; override;
  procedure ExecuteVerb(index: integer); override;
 end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('dev-c++',
    [TdevTabs, TdevPages, TColorPickerButton, TdevFileMonitor]);
  RegisterClasses([TdevPage]);
  RegisterComponentEditor(TdevPages, TdevPageEditor);
  RegisterComponentEditor(TdevPage, TdevPageEditor);
end;


{ TdevPageEditor }

procedure TdevPageEditor.ExecuteVerb(index: integer);
var
 Pages: TdevCustomPages;
begin
  if Component is TdevPages then
   Pages:= TdevPages(Component)
  else
   Pages:= TdevPage(Component).Pages;

  if index = 0 then
   begin
     Pages.ControlStyle:= Pages.ControlStyle +[csAcceptsControls];
     try
      Designer.CreateComponent(TdevPage, Pages, 0, 0, 0, 0);
     finally
      Pages.ControlStyle:= Pages.ControlStyle -[csAcceptsControls];
     end;
   end
{$IFDEF VER130}
  else
   Designer.DeleteSelection;
{$ENDIF}
end;

function TdevPageEditor.GetVerb(index: integer): string;
begin
  result:= 'New Page';
  if index = 1 then
   result:= 'Delete Page';
end;

function TdevPageEditor.GetVerbCount: integer;
begin
{$IFDEF VER130}
  result:= 1;
{$ELSE}
  result:= 2;
{$ENDIF}
end;

end.
