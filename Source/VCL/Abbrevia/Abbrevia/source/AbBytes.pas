unit AbBytes;

interface

uses
  System.SysUtils;

type
  TAbBytes = record
  public
    class function AsString(ASource: Pointer): string; overload; static;
    class function AsString(ASource: Pointer; ALen: Integer): string; overload; static;
    class function Equals(const ALeft: string; ARight: Pointer): Boolean; overload; static;
    class function Equals(const ALeft: Pointer; ARight: string): Boolean; overload; static;
    class procedure FromString(const ASource: string; ADest: Pointer); static;
    class function StrLCopy(ADest: PByte; const ASource: string; AMaxLen: Cardinal): PByte; static;
    class function StrLen(ABuffer: Pointer): Cardinal; static;
    class function StrPCopy(ADest: Pointer; const ASource: string): PByte; static;
    class function StrPLCopy(ADest: Pointer; const ASource: string; AMaxLen: Cardinal): PByte; static;
  end;

implementation

{ TAbBytes }

class function TAbBytes.AsString(ASource: Pointer): string;
begin
  Result := AsString(ASource, StrLen(ASource));
end;

class function TAbBytes.AsString(ASource: Pointer; ALen: Integer): string;
var
  pBytes: TBytes;
begin
  if ALen = 0 then
    Exit('');

  SetLength(pBytes, ALen);
  Move(ASource^, pBytes[0], ALen);
  Result := TEncoding.ANSI.GetString(pBytes);
end;

class function TAbBytes.Equals(const ALeft: Pointer; ARight: string): Boolean;
begin
  Result := Equals(ARight, ALeft);
end;

class function TAbBytes.Equals(const ALeft: string; ARight: Pointer): Boolean;
var
  iByte: Integer;
  pBuffer: PByte;
begin
  pBuffer := PByte(ARight);
  for iByte := 1 to Length(ALeft) do
  begin
    if pBuffer[iByte - 1] <> Ord(ALeft[iByte]) then
      Exit(False);
  end;
  Exit(True);
end;

class procedure TAbBytes.FromString(const ASource: string; ADest: Pointer);
var
  iByte: Integer;
  pBuffer: PByte;
begin
  pBuffer := PByte(ADest);
  for iByte := 1 to Length(ASource) do
    pBuffer[iByte - 1] := Ord(ASource[iByte]);
end;

class function TAbBytes.StrLCopy(ADest: PByte; const ASource: string; AMaxLen: Cardinal): PByte;
var
  iLen: Cardinal;
  pBytes: TBytes;
begin
  Result := ADest;
  pBytes := TEncoding.ANSI.GetBytes(ASource);
  iLen := Length(pBytes);
  if iLen > AMaxLen then
    iLen := AMaxLen;
  if iLen > 0 then
    Move(pBytes[0], ADest^, iLen);
  ADest[iLen] := 0;
end;

class function TAbBytes.StrLen(ABuffer: Pointer): Cardinal;
var
  pBuffer: PByte;
begin
  pBuffer := PByte(ABuffer);
  for Result := 0 to MaxInt do
  begin
    if pBuffer^ <> 0 then
      Inc(pBuffer)
    else
      Break;
  end;
end;

class function TAbBytes.StrPCopy(ADest: Pointer; const ASource: string): PByte;
begin
  Result := StrLCopy(ADest, ASource, Length(ASource));
end;

class function TAbBytes.StrPLCopy(ADest: Pointer; const ASource: string; AMaxLen: Cardinal): PByte;
begin
  Result := StrLCopy(ADest, ASource, AMaxLen);
end;

end.
