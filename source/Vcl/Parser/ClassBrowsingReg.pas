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

unit ClassBrowsingReg;

interface

uses 
{$IFDEF WIN32}
  Windows, Classes, CppParser, CppTokenizer, CodeCompletion, ClassBrowser;
{$ENDIF}
{$IFDEF LINUX}
  Classes, CppParser, CppTokenizer, CodeCompletion, ClassBrowser;
{$ENDIF}

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('dev-c++', [TCppTokenizer, TCppParser, TCodeCompletion, TClassBrowser]);
end;

end.
 