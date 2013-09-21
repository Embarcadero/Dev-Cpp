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

  
  To obtain the latest version of this file, please visit 
  http://www.bloodshed.net
  
----------------------------------------------------------------------------------}

//
//  History:
//
//    25 March 2004
//      Initial release
//
//    28 march 2004
//      DeBeforeShow optimized
//

unit DevCodeToolTip;

interface
uses
  SysUtils, Classes, CodeToolTip, CppParser;

type        
  TDevCodeToolTipError = class(Exception);
  
  
  TDevCodeToolTip = class(TBaseCodeToolTip)
  private
    FList: TList;
    FParser: TCppParser;
  protected
    procedure DoBeforeShow(const AToolTips: TStringList; const APrototypeName: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Activated;
    property SelIndex;
  published
    property ActivateKey;
    property Color;
    property Editor;
    property EndWhenChr;
    property Hints;
    property MaxScanLength;
    property Options;    
    property Parser: TCppParser read FParser write FParser;
    property StartWhenChr;
  end;
  
implementation

//----------------------------------------------------------------------------------------------------------------------

constructor TDevCodeToolTip.Create(AOwner: TComponent);
begin
  inherited;

  FList := TList.Create;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TDevCodeToolTip.Destroy;
begin
  FList.Free;
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDevCodeToolTip.DoBeforeShow(const AToolTips: TStringList; const APrototypeName: string);
var
  I: Integer;
begin
  // added on 28march 2004
  // we dont need to go further when the hint is already
  // active, BECAUSE we already got all neccessary prototimes!
  if Activated then Exit;
  
  AToolTips.Clear;

  if Parser = nil then
    raise TDevCodeToolTipError.Create('No parser available!');
  
  Parser.FillListOf(APrototypeName, False, FList);
  AToolTips.BeginUpdate;
  try
    for I := 0 to FList.Count-1 do
    begin
      AToolTips.Add(PStatement(FList.Items[I])^._FullText);
    end;
  finally
    AToolTips.EndUpdate;
  end;
end;


end.
