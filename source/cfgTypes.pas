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

unit cfgTypes;

interface

uses
{$IFDEF WIN32}
  Windows, Classes;
{$ENDIF}
{$IFDEF LINUX}
  Classes;
{$ENDIF}
 
//  abstract class for additional windows
//  i.e. a sub form
type
 TCFGOptions = class(TPersistent)
  private
   fName: string;
   fWinPlace: TWindowPlacement;
  public
   procedure SettoDefaults; virtual; abstract;
   procedure SaveSettings; virtual; abstract;
   procedure LoadSettings; virtual; abstract;
   property Name: string read fName write fName;
   property WindowPlacement: TWindowPlacement read fWinPlace write fWinPlace;
 end;


implementation

end.
 
