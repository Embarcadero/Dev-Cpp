{******************************************************************}
{ SVG style class                                                  }
{                                                                  }
{ home page : http://www.mwcs.de                                   }
{ email     : martin.walter@mwcs.de                                }
{                                                                  }
{ date      : 26-04-2005                                           }
{                                                                  }
{ Use of this file is permitted for commercial and non-commercial  }
{ use, as long as the author is credited.                          }
{ This file (c) 2005 Martin Walter                                 }
{                                                                  }
{ Thanks to:                                                       }
{ Kiriakos Vlahos (Process Stylesheet)                             }
{                                                                  }
{ This Software is distributed on an "AS IS" basis, WITHOUT        }
{ WARRANTY OF ANY KIND, either express or implied.                 }
{                                                                  }
{ *****************************************************************}

unit SVGStyle;

interface

uses
  System.Classes, System.Contnrs;

type
  TStyle = class(TObject)
  strict private
    FValues: TStrings;
    function GetCount: Integer;
    procedure PutValues(const Key: string; const Value: string);
    function GetValues(const Key: string): string;

    procedure PutValuesByNum(const Index: Integer; const Value: string);
    function GetValuesByNum(const Index: Integer): string;

    procedure PutKey(const Index: Integer; const Key: string);
    function GetKey(const Index: Integer): string;

    function Dequote(const Value: string): string;
  private
    FName: string;
  strict private
    FOnChange: TNotifyEvent;
    procedure DoOnChange;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Clone: TStyle;
    procedure SetValues(const Values: string);

    function AddStyle(const Key, Value: string): Integer;
    function IndexOf(const Key: string): Integer;
    procedure Delete(Index: Integer);
    function Remove(const Key: string): Integer;

    property Count: Integer read GetCount;
    property Values[const Key: string]: string read GetValues write PutValues; default;
    property ValuesByNum[const Index: Integer]: string read GetValuesByNum write PutValuesByNum;
    property Keys[const Index: Integer]: string read GetKey write PutKey;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TStyleList = class(TObject)
  strict private
    FList: TObjectList;

    function GetCount: Integer;
    function GetStyle(const Index: Integer): TStyle;
    procedure PutStyle(const Index: Integer; Style: TStyle);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Clone: TStyleList;

    procedure Delete(Index: Integer);
    function Remove(const Style: TStyle): Integer;
    function Add(const AStyle: TStyle): Integer; overload;
    function Add(const Name, Values: string): Integer; overload;
    function Add(const AStyle: string): Integer; overload;

    procedure Insert(Index: Integer; Style: TStyle); overload;
    procedure Insert(Index: Integer; const Name, Values: string); overload;
    procedure Exchange(Index1, Index2: Integer);
    procedure Move(CurIndex, NewIndex: Integer);
    function IndexOf(Style: TStyle): Integer;
    function GetStyleByName(const Name: string): TStyle;

    property Style[const Index: Integer]: TStyle read GetStyle write PutStyle; default;
    property Count: Integer read GetCount;
  end;

  procedure ProcessStyleSheet(Var S: string);

implementation

uses
  System.SysUtils, System.StrUtils, System.Character;

{$REGION 'TStyle'}

constructor TStyle.Create;
begin
  inherited;
  FValues := TStringList.Create;
  FValues.NameValueSeparator := '"';
end;

destructor TStyle.Destroy;
begin
  FreeAndNil(FValues);
  inherited;
end;

procedure TStyle.Clear;
begin
  if FValues <> nil then
  begin
    FValues.Clear;
  end;
end;

function TStyle.Clone: TStyle;
begin
  Result := TStyle.Create;
  Result.FName := FName;
  Result.FValues.Assign(FValues);
end;

function TStyle.GetCount: Integer;
begin
  Result := FValues.Count;
end;

procedure TStyle.DoOnChange;
begin
  if Assigned(FOnChange) then
  begin
    FOnChange(Self);
  end;
end;

procedure TStyle.PutValues(const Key: string; const Value: string);
var
  Index: Integer;
begin
  Index := IndexOf(Key);
  if Index > 0 then
    PutValuesByNum(Index, Value)
  else
    AddStyle(Key, Value);
end;

function TStyle.GetValues(const Key: string): string;
begin
  Result := GetValuesByNum(IndexOf(Key));
end;

procedure TStyle.PutValuesByNum(const Index: Integer; const Value: string);
begin
  if (Index >= 0) and (Index < FValues.Count) then
    FValues.ValueFromIndex[Index] := DeQuote(Value);
end;

function TStyle.GetValuesByNum(const Index: Integer): string;
begin
  if (Index >= 0) and (Index < FValues.Count) then
    Result := FValues.ValueFromIndex[Index]
  else
    Result := '';
end;

procedure TStyle.PutKey(const Index: Integer; const Key: string);
begin
  if (Index >= 0) and (Index < FValues.Count) then
    FValues[Index] := Key + FValues.NameValueSeparator + FValues.ValueFromIndex[Index];
end;

function TStyle.GetKey(const Index: Integer): string;
begin
  if (Index >= 0) and (Index < FValues.Count) then
    Result := FValues.Names[Index]
  else
    Result := '';
end;

function TStyle.Dequote(const Value: string): string;
begin
  if Value <> '' then
  begin
    if (Value[1] = '''') and (Value[Length(Value)] = '''') then
      Result := Copy(Value, 2, Length(Value) - 2)
    else
      if (Value[1] = '"') and (Value[Length(Value)] = '"') then
        Result := Copy(Value, 2, Length(Value) - 2)
      else
        Result := Value;
  end else
    Result := Value;
end;

procedure TStyle.SetValues(const Values: string);
var
  C: Integer;
  Key: string;
  Value: string;
  Help: string;
begin
  Help := Trim(Values);

  while Help <> '' do
  begin
    C := Pos(';', Help);
    if C = 0 then
      C := Length(Help) + 1;
    Key := Copy(Help, 1, C - 1);
    Help := Trim(Copy(Help, C + 1, MaxInt));
    C := Pos(':', Key);
    if C <> 0 then
    begin
      Value := Trim(Copy(Key, C + 1, MaxInt));
      Key := Trim(Copy(Key, 1, C - 1));

      C := IndexOf(Key);
      if C = -1 then
        FValues.Add(Key + FValues.NameValueSeparator + DeQuote(Value))
      else
        PutValuesByNum(C, Value);
    end;
  end;
end;

function TStyle.AddStyle(const Key, Value: string): Integer;
begin
  Result := IndexOf(Key);
  if Result = -1 then
    Result := FValues.Add(Key + FValues.NameValueSeparator + DeQuote(Value))
  else
    PutValuesByNum(Result, Value);
  DoOnChange;
end;

function TStyle.IndexOf(const Key: string): Integer;
begin
  for Result := 0 to FValues.Count - 1 do
  begin
    if FValues.Names[Result] = Key then
      Exit;
  end;
  Result := -1;
end;

procedure TStyle.Delete(Index: Integer);
begin
  if (Index >= 0) and (Index < FValues.Count) then
  begin
    FValues.Delete(Index);
  end;
end;

function TStyle.Remove(const Key: string): Integer;
begin
  Result := IndexOf(Key);
  Delete(Result);
end;
{$ENDREGION}

{$REGION 'TStyleList'}
constructor TStyleList.Create;
begin
  inherited;
  FList := TObjectList.Create(False);
end;

destructor TStyleList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

procedure TStyleList.Clear;
begin
  while FList.Count > 0 do
  begin
    TStyle(FList[0]).Free;
    FList.Delete(0);
  end;
end;

function TStyleList.Clone: TStyleList;
var
  C: Integer;
begin
  Result := TStyleList.Create;
  for C := 0 to FList.Count - 1 do
    Result.Add(GetStyle(C).Clone);
end;

function TStyleList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TStyleList.GetStyle(const Index: Integer): TStyle;
begin
  if (Index >= 0) and (Index < FList.Count) then
    Result := TStyle(FList[Index])
  else
    Result := nil;
end;

procedure TStyleList.PutStyle(const Index: Integer; Style: TStyle);
begin
  if (Index >= 0) and (Index < FList.Count) then
  begin
    FList[Index].Free;
    FList[Index] := Style;
  end;
end;

procedure TStyleList.Delete(Index: Integer);
begin
  if (Index >= 0) and (Index < FList.Count) then
  begin
    FList[Index].Free;
    FList.Delete(Index);
  end;
end;

function TStyleList.Remove(const Style: TStyle): Integer;
begin
  Result := IndexOf(Style);
  Delete(Result);
end;

function TStyleList.Add(const AStyle: TStyle): Integer;
begin
  Result := FList.Add(AStyle);
end;

function TStyleList.Add(const Name, Values: string): Integer;
var
  S: TStyle;
begin
  S := TStyle.Create;
  S.FName := Name;
  S.SetValues(Values);
  Result := Add(S);
end;

function TStyleList.Add(const AStyle: string): Integer;
var
  Name: string;
  StyleStr: string;
  Values: string;
  C: Integer;
  D: Integer;
begin
  Result := -1;
  StyleStr := Trim(AStyle);
  for C := Low(StyleStr) to High(StyleStr) do
  begin
    if StyleStr[C] = '{' then
    begin
      for D := High(StyleStr) downto C + 1 do
      begin
        if StyleStr[D] = '}' then
        begin
          Name := Trim(Copy(StyleStr, 1, C - 1));

          Values := Copy(StyleStr, C + 1, D - C - 1);
          Result := Add(Name, Values);
        end;
      end;
    end;
  end;
end;

procedure TStyleList.Insert(Index: Integer; Style: TStyle);
begin
  if (Index >= 0) and (Index < FList.Count) then
    FList.Insert(Index, Style);
end;

procedure TStyleList.Insert(Index: Integer; const Name, Values: string);
var
  S: TStyle;
begin
  if (Index >= 0) and (Index < FList.Count) then
  begin
    S := TStyle.Create;
    S.FName := Name;
    S.SetValues(Values);
    Insert(Index, S);
  end;
end;

procedure TStyleList.Exchange(Index1, Index2: Integer);
begin
  if (Index1 >= 0) and (Index1 < FList.Count) and
     (Index2 >= 0) and (Index2 < FList.Count) then
    FList.Exchange(Index1, Index2);
end;

procedure TStyleList.Move(CurIndex, NewIndex: Integer);
begin
  if (CurIndex >= 0) and (CurIndex < FList.Count) and
     (NewIndex >= 0) and (NewIndex < FList.Count) then
    FList.Move(CurIndex, NewIndex);
end;

function TStyleList.IndexOf(Style: TStyle): Integer;
begin
  Result := FList.IndexOf(Style);
end;

function TStyleList.GetStyleByName(const Name: string): TStyle;
var
  C: Integer;
begin
  for C := 0 to FList.Count - 1 do
  begin
    Result := TStyle(FList[C]);
    if Result.FName = Name then
      Exit;
  end;

  Result := nil;
end;

procedure ProcessStyleSheet(Var S: string);
Var
  OutS: string;
  C: Char;
begin
  OutS := '';
  for C in S do
  begin
    if C.IsWhiteSpace then Continue;
    if C = '}' then
      OutS := OutS + C + SLineBreak
    else
      OutS := OutS + C;
  end;
  S := OutS;
end;

{$ENDREGION}

end.
