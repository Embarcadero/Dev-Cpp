      { *****************************************************************}
      { Added Support for RoundRect (GraphicsPath + TGPGraphics)         }
      {                                                                  }
      { date  : 05-11-2006                                               }
      {                                                                  }
      { email : martin.walter@winningcubed.de                            }
      {                                                                  }
      { *****************************************************************}
unit GDIPOBJ2;

interface

uses
  Winapi.GDIPAPI, Winapi.GDIPOBJ;

type
  TGPGraphicsPath2 = class(TGPGraphicsPath)
  public
    function AddRoundRect(Rect: TGPRectF; RX, RY: Single): TStatus; overload;
    function AddRoundRect(X, Y, Width, Height, RX, RY: Single): TStatus; overload;
    function Clone: TGPGraphicsPath2;
  end;

implementation

{ TGPGraphicsPath2 }

function TGPGraphicsPath2.AddRoundRect(Rect: TGPRectF; RX, RY: Single): TStatus;
begin
  Result := AddRoundRect(Rect.X, Rect.Y, Rect.Width, Rect.Height, RX, RY);
end;

function TGPGraphicsPath2.AddRoundRect(X, Y, Width, Height, RX, RY: Single) : TStatus;
begin
  Result := AddLine(X + RX, Y, X + Width - RX, Y);
  if Result <> OK then
    Exit;
  Result := AddArc(X + Width - 2 * RX, Y, 2 * RX, 2 * RY, 270, 90);
  if Result <> OK then
    Exit;

  Result := AddLine(X + Width, Y + RY,X + Width, Y + Height - RY);
  if Result <> OK then
    Exit;
  Result := AddArc(X + Width - 2 * RX, Y + Height - 2 * RY, 2 * RX, 2 * RY, 0, 90);
  if Result <> OK then
    Exit;

  Result := AddLine(X + Width - RX, Y + Height, X + RX, Y + Height);
  if Result <> OK then
    Exit;
  Result := AddArc(X, Y + Height - 2 * RY, 2 * RX, 2 * RY, 90, 90);
  if Result <> OK then
    Exit;

  Result := AddLine(X, Y + Height - RY, X, Y + RY);
  if Result <> OK then
    Exit;
  Result := AddArc(X, Y, 2 * RX, 2 * RY, 180, 90);
  if Result <> OK then
    Exit;
  Result := CloseFigure;
end;

function TGPGraphicsPath2.Clone: TGPGraphicsPath2;
var
  ClonePath: GpPath;
begin
  Clonepath := nil;
  SetStatus(GdipClonePath(nativePath, Clonepath));
  result := TGPGraphicsPath2.Create(ClonePath);
end;

end.
