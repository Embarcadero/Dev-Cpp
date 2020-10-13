{******************************************************************}
{ Parse of SVG properties                                          }
{                                                                  }
{ home page : http://www.mwcs.de                                   }
{ email     : martin.walter@mwcs.de                                }
{                                                                  }
{ date      : 05-06-2020                                           }
{                                                                  }
{ version   : 0.69c                                                }
{                                                                  }
{ Use of this file is permitted for commercial and non-commercial  }
{ use, as long as the author is credited.                          }
{ This file (c) 2005, 2008 Martin Walter                           }
{                                                                  }
{ Thanks to:                                                       }
{ Carlo Barazzetta (parsing errors)                                }
{ Kiriakos Vlahos (exception handling)                             }
{                                                                  }
{ This Software is distributed on an "AS IS" basis, WITHOUT        }
{ WARRANTY OF ANY KIND, either express or implied.                 }
{                                                                  }
{ *****************************************************************}

unit SVGProperties;

interface

uses
  System.Math.Vectors,
  Xml.XmlIntf,
  SVGTypes;

procedure LoadLength(const Node: IXMLNode; const S: string; var X: TFloat);

procedure LoadTFloat(const Node: IXMLNode; const S: string; var X: TFloat);

procedure LoadString(const Node: IXMLNode; const S: string; var X: string);

procedure LoadTransform(const Node: IXMLNode; const S: string; var Matrix: TMatrix);

procedure LoadPercent(const Node: IXMLNode; const S: string; var X: TFloat); overload;
procedure LoadPercent(const Node: IXMLNode; const S: string; Max: Integer; var X: TFloat); overload;
procedure LoadBytePercent(const Node: IXMLNode; const S: string; var X: Integer);

procedure LoadBoolean(const Node: IXMLNode; const S: string; var X: Boolean);

procedure LoadDisplay(const Node: IXMLNode; var X: Integer);

procedure LoadVisible(const Node: IXMLNode; var X: Integer);

procedure LoadGradientUnits(const Node: IXMLNode; var Units: TGradientUnits);

implementation

uses
  SVGCommon, SVGParse, System.Variants;

procedure LoadLength(const Node: IXMLNode; const S: string;
  var X: TFloat);
var
  Attribute: IXMLNode;
begin
  Attribute := Node.AttributeNodes.FindNode(S);
  if Assigned(Attribute) then
  begin
    X := ParseLength(VarToStr(Attribute.nodeValue));
  end;
end;

procedure LoadTFloat(const Node: IXMLNode; const S: string;
  var X: TFloat);
var
  Attribute: IXMLNode;
begin
  Attribute := Node.AttributeNodes.FindNode(S);
  if Assigned(Attribute) then
  begin
    X := StrToTFloat(Attribute.nodeValue);
  end;
end;

procedure LoadString(const Node: IXMLNode; const S: string;
  var X: string);
var
  Attribute: IXMLNode;
  V: Variant;
begin
  Attribute := Node.AttributeNodes.FindNode(S);
  if Assigned(Attribute) then
  begin
    V := Attribute.nodeValue;
    if VarType(V) = varNull then
      X := ''
    else
      X := V;
  end;
end;

procedure LoadTransform(const Node: IXMLNode; const S: string;
  var Matrix: TMatrix);
var
  Attribute: IXMLNode;
begin
  Attribute := Node.AttributeNodes.FindNode(S);
  if Assigned(Attribute) then
  begin
    Matrix := ParseTransform(Attribute.nodeValue);
  end;
end;

procedure LoadPercent(const Node: IXMLNode; const S: string;
  var X: TFloat);
var
  Attribute: IXMLNode;
begin
  Attribute := Node.AttributeNodes.FindNode(S);
  if Assigned(Attribute) then
    X := ParsePercent(Attribute.nodeValue);
end;

procedure LoadPercent(const Node: IXMLNode; const S: string;
  Max: Integer; var X: TFloat);
var
  Attribute: IXMLNode;
begin
  Attribute := Node.AttributeNodes.FindNode(S);
  if Assigned(Attribute) then
  begin
    X := Max * ParsePercent(Attribute.nodeValue);
  end;
end;

procedure LoadBytePercent(const Node: IXMLNode; const S: string;
  var X: Integer);
var
  Attribute: IXMLNode;
begin
  Attribute := Node.AttributeNodes.FindNode(S);
  if Assigned(Attribute) then
  begin
    X := Round(255 * ParsePercent(Attribute.nodeValue));
  end;
end;

procedure LoadBoolean(const Node: IXMLNode; const S: string;
  var X: Boolean);
var
  Attribute: IXMLNode;
begin
  Attribute := Node.AttributeNodes.FindNode(S);
  if Assigned(Attribute) then
    X := Boolean(ParseInteger(Attribute.nodeValue));
end;

procedure LoadDisplay(const Node: IXMLNode; var X: Integer);
var
  S: string;
  Attribute: IXMLNode;
begin
  Attribute := Node.AttributeNodes.FindNode('display');
  if Assigned(Attribute) then
  begin
    S := Attribute.nodeValue;
    if S = 'inherit' then
      X := -1
    else
      if S = 'none' then
        X := 0
      else
        X := 1;
  end;
end;

procedure LoadVisible(const Node: IXMLNode; var X: Integer);
var
  S: string;
  Attribute: IXMLNode;
begin
  Attribute := Node.AttributeNodes.FindNode('visibility');
  if Assigned(Attribute) then
  begin
    S := Attribute.nodeValue;
    if S = 'inherit' then
      X := -1
    else
      if S = 'visible' then
        X := 1
      else
        X := 0;
  end;
end;

procedure LoadGradientUnits(const Node: IXMLNode; var Units: TGradientUnits);
var
  S: string;
  Attribute: IXMLNode;
begin
  Units := guObjectBoundingBox;
  Attribute := Node.AttributeNodes.FindNode('gradientUnits');
  if Assigned(Attribute) then
  begin
    S := Attribute.nodeValue;
    if S = 'userSpaceOnUse' then
      Units := guUserSpaceOnUse
    else
      if S = 'objectBoundingBox' then
        Units := guObjectBoundingBox;
  end;
end;

end.
