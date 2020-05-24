unit CharUtils;

interface

uses
  System.SysUtils;


const
  Operators: array[0..16] of string = ('*', '/', '+', '-', '<', '<=', '>', '>=', '==', '!=', '&', '^', '|', '&&', '||', 'and', 'or');

type
  TSetOfChar = record
  private
    FSet: TArray<Char>;
  public
    constructor New(const AAnsiSet: TSysCharSet);
    procedure Add(const AAnsiChar: AnsiChar); inline;
  public
    class operator In(const AChar: Char; const ASet: TSetOfChar): Boolean; inline;
    class operator Add(const ASet1, ASet2: TSetOfChar): TSetOfChar; inline;
    class operator Implicit(const AAnsiSet: TSysCharSet): TSetOfChar; inline;
  end;


var
  LineChars: TSetOfChar;
  SpaceChars: TSetOfChar;
  OperatorChars: TSetOfChar;
  IdentChars: TSetOfChar;
  MacroIdentChars: TSetOfChar;
  DigitChars: TSetOfChar;
  LetterChars: TSetOfChar;
  HexChars: TSetOfChar;
  BlankChars: TSetOfChar;


implementation

const
  DefLineChars: TSysCharSet = [#13, #10];
  DefSpaceChars: TSysCharSet = [#32, #9];
  DefDigitChars: TSysCharSet = ['0'..'9'];
//  DefOperatorChars: TSysCharSet = ['+', '-', '*', '/', '!', '=', '<', '>', '&', '|', '^'];
  DefOperatorChars: TSysCharSet = ['+', '-', '/', '*', '[', ']', '=', '%', '!', '&', '|', '>', '<', '^', '!']; //is they must be different?

  DefIdentChars: TSysCharSet = ['A'..'Z', '0'..'9', 'a'..'z', '_', '*', '&', '~'];
  DefMacroIdentChars: TSysCharSet = ['A'..'Z', 'a'..'z', '_'];
  DefLetterChars: TSysCharSet = ['A'..'Z', 'a'..'z', '_', '*', '&', '~'];
  DefHexChars: TSysCharSet = ['A'..'F', 'a'..'f', 'x', 'L'];
  DefBlankChars: TSysCharSet = [#0..#32];

{ TSetOfChar }


procedure TSetOfChar.Add(const AAnsiChar: AnsiChar);
begin
  SetLength(FSet, Length(FSet) + 1);
  FSet[High(FSet)] := Char(AAnsiChar);
end;

class operator TSetOfChar.Add(const ASet1, ASet2: TSetOfChar): TSetOfChar;
begin
  Result.FSet := Concat(ASet1.FSet, ASet2.FSet);
end;

class operator TSetOfChar.Implicit(const AAnsiSet: TSysCharSet): TSetOfChar;
begin
  Result := TSetOfChar.New(AAnsiSet);
end;

class operator TSetOfChar.In(const AChar: Char; const ASet: TSetOfChar): Boolean;
begin
  for var SetChar in ASet.FSet do
    if SetChar = AChar then
      Exit(True);

  Result := False;
end;

constructor TSetOfChar.New(const AAnsiSet: TSysCharSet);
begin
  FSet := [];
  for var AChar in AAnsiSet do
    Add(AChar);
end;

initialization
  LineChars := TSetOfChar(DefLineChars);
  SpaceChars := TSetOfChar(DefSpaceChars);
  OperatorChars := TSetOfChar(DefOperatorChars);
  IdentChars := TSetOfChar(DefIdentChars);
  MacroIdentChars := TSetOfChar(DefMacroIdentChars);
  DigitChars := TSetOfChar(DefDigitChars);
  LetterChars := TSetOfChar(DefLetterChars);
  HexChars := TSetOfChar(DefHexChars);
  BlankChars := TSetOfChar(DefBlankChars);

end.
