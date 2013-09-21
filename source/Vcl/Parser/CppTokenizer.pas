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

unit CppTokenizer;

interface

uses 
{$IFDEF WIN32}
  Windows, Classes, SysUtils, StrUtils, ComCtrls;
{$ENDIF}
{$IFDEF LINUX}
  Classes, SysUtils, StrUtils, QComCtrls;
{$ENDIF}

const
  LetterChars: set of Char = ['A'..'Z', 'a'..'z', '_', '*', '&', '~'];
  DigitChars: set of Char = ['0'..'9'];
  HexChars: set of Char = ['A'..'F', 'a'..'f', 'x', 'L'];
  SpaceChars: set of Char = [' ', #9];
  LineChars: set of Char = [#13, #10];

  MAX_TOKEN_SIZE = 32768;

type
  TSetOfChars = set of Char;

  TLogTokenEvent = procedure(Sender: TObject; Msg: string) of object;
  TProgressEvent = procedure(Sender: TObject; FileName: string; Total, Current: integer) of object;

  PToken = ^TToken;
  TToken = record
    Text: string[255];
    Line: integer;
  end;

  TCppTokenizer = class(TComponent)
  private
    pStart: PChar;
    pCurrent: PChar;
    pLineCount: PChar;
    fEnd: integer;
    fCurrLine: integer;
    fTokenList: TList;
    fLogTokens: boolean;
    fOnLogToken: TLogTokenEvent;
    fOnProgress: TProgressEvent;
    fTmpOutput: PChar;
    procedure AddToken(sText: string; iLine: integer);
    procedure CountLines;
    procedure MatchChar(C: Char);
    procedure SkipCStyleComment;
    procedure SkipSplitLine;
    procedure SkipToEOL;
    procedure SkipToNextToken;
    procedure SkipDoubleQuotes;
    procedure SkipSingleQuote;
    procedure SkipPair(cStart, cEnd: Char);
    procedure SkipAssignment;
    function GetNumber: string;
    function GetWord(bSkipParenthesis: boolean = False; bSkipArray: boolean = False; bSkipBlock: boolean = False): string;
    function GetPreprocessor: string;
    function GetArguments: string;
    function IsWord: boolean;
    function IsNumber: boolean;
    function IsPreprocessor: boolean;
    function IsArguments: boolean;
    function GetNextToken(bSkipParenthesis: boolean = False; bSkipArray: boolean = False; bSkipBlock: boolean = False): string;
    function Simplify(Str: string): string;
    procedure PostProcessToken(var Str: string);
    procedure Advance(bPerformChecks: boolean = True);
    function OpenFile(FileName: string): boolean;
    function OpenStream(Stream: TStream): boolean;
    procedure ReleaseFileMemory;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Reset;
    procedure Tokenize(StartAt: PChar); overload;
    procedure Tokenize(FileName: TFilename); overload;
    procedure Tokenize(Stream: TStream); overload;
  published
    property LogTokens: boolean read fLogTokens write fLogTokens;
    property OnLogToken: TLogTokenEvent read fOnLogToken write fOnLogToken;
    property OnProgress: TProgressEvent read fOnProgress write fOnProgress;
    property Tokens: TList read fTokenList;
  end;

implementation

constructor TCppTokenizer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fTokenList := TList.Create;
  fLogTokens := False;
end;

destructor TCppTokenizer.Destroy;
begin
  try
    Reset;
  finally
    FreeAndNil(fTokenList);
  end;
  inherited Destroy;
end;

procedure TCppTokenizer.Reset;
begin
  if fTokenList <> nil then begin
    while fTokenList.Count > 0 do
      if Assigned(PToken(fTokenList[fTokenList.Count - 1])) then begin
        Dispose(PToken(fTokenList[fTokenList.Count - 1]));
        fTokenList.Delete(fTokenList.Count - 1);
      end
      else
        fTokenList.Delete(fTokenList.Count - 1);
    fTokenList.Clear;
  end;
end;

function TCppTokenizer.OpenFile(FileName: string): boolean;
var
  hFile: integer;
  iLength, iRead: integer;
begin
  Result := False;
  if FileExists(FileName) then begin
    hFile := FileOpen(FileName, fmOpenRead);
    if hFile > 0 then begin
      iLength := FileSeek(hFile, 0, 2);
      FileSeek(hFile, 0, 0);
      if iLength > 0 then begin
        GetMem(pStart, iLength + 1);
        iRead := FileRead(hFile, pStart^, iLength);
        (pStart + iLength)^ := #0;
        Result := iRead = iLength;
      end;
      FileClose(hFile);
      if not Result then begin
        if fLogTokens then
          if Assigned(fOnLogToken) then
            fOnLogToken(Self, '[tokenizer]: Could not read file.');
      end
    end
    else begin
      if fLogTokens then
        if Assigned(fOnLogToken) then
          fOnLogToken(Self, '[tokenizer]: Could not open file.');
    end;
  end
  else begin
    if fLogTokens then
      if Assigned(fOnLogToken) then
        fOnLogToken(Self, '[tokenizer]: File not found.');
  end;
end;

function TCppTokenizer.OpenStream(Stream: TStream): boolean;
begin
  Result := False;
  if Assigned(Stream) then begin
    Stream.Position := 0;
    GetMem(pStart, Stream.Size + 1);
    Stream.Read(pStart^, Stream.Size);
    (pStart + Stream.Size)^ := #0;
    Result := True;
  end
  else begin
    if fLogTokens then
      if Assigned(fOnLogToken) then
        fOnLogToken(Self, '[tokenizer]: Non-existent stream.');
  end;
end;

procedure TCppTokenizer.AddToken(sText: string; iLine: integer);
var
  Token: PToken;
begin
  Token := New(PToken);
  FillChar(Token^.Text, sizeof(Token^.Text), 0);
  Token^.Text := sText;
  Token^.Line := iLine;
  fTokenList.Add(Token);
end;

procedure TCppTokenizer.CountLines;
begin
  while (pLineCount^ <> #0) and (pLineCount < pCurrent) do begin
    if pLineCount^ = #10 then
      Inc(fCurrLine, 1);
    Inc(pLineCount, 1);
  end;
end;

procedure TCppTokenizer.MatchChar(C: Char);
begin
  while not (pCurrent^ in [C, #0]) do begin
    Inc(pCurrent);
  end;
  Inc(pCurrent);
end;

procedure TCppTokenizer.SkipCStyleComment;
begin
  repeat
    Inc(pCurrent);
  until (pCurrent^ = #0) or ((pCurrent^ = '*') and ((pCurrent + 1)^ = '/'));
  if pCurrent^ <> #0 then
    Inc(pCurrent, 2); //skip '*/'
end;

procedure TCppTokenizer.SkipSplitLine;
begin
  Inc(pCurrent); // skip '\'
  while pCurrent^ in LineChars do // skip newline
    Inc(pCurrent);
end;

procedure TCppTokenizer.SkipToEOL;
var
  SplitLine: boolean;
  Last : char;
begin
  Last := #0;
  while (not (pCurrent^ in LineChars)) and (pCurrent^ <> #0) do begin
    if (pCurrent^ = '/') and ((pCurrent + 1)^ = '*') and (Last <> '/') then
      SkipCStyleComment
    else begin
      Last := pCurrent^;
      Inc(pCurrent);
    end;
  end;

  SplitLine := ((pCurrent - 1)^ = '\') and (pCurrent^ in LineChars);

  while pCurrent^ in LineChars do
    Inc(pCurrent);

  if SplitLine then
    SkipToEOL; //recurse
end;

procedure TCppTokenizer.SkipToNextToken;
begin
  while pCurrent^ in SpaceChars + LineChars do
    Advance;
end;

procedure TCppTokenizer.SkipDoubleQuotes;
begin
  repeat
    Inc(pCurrent);
    if pCurrent^ = '\' then
      Inc(pCurrent, 2); // skip escaped char
  until pCurrent^ in ['"', #0];
end;

procedure TCppTokenizer.SkipSingleQuote;
begin
  repeat
    Inc(pCurrent);
    if pCurrent^ = '\' then
      Inc(pCurrent, 2); // skip escaped quote
  until pCurrent^ in ['''', #0];
end;

procedure TCppTokenizer.SkipPair(cStart, cEnd: Char); // e.g.: SkipPair('[', ']');
begin
  repeat
    Inc(pCurrent);
    if pCurrent^ = #0 then
      Break;
    if pCurrent^ = cStart then
      SkipPair(cStart, cEnd); //recurse
    if pCurrent^ = cEnd then
      Break;
    case pCurrent^ of
      '"': if cStart <> '''' then
          SkipDoubleQuotes; // don't do it inside string!
      '''': SkipSingleQuote;
      '/': if (pCurrent + 1)^ = '/' then
          SkipToEOL
        else if (pCurrent + 1)^ = '*' then
          SkipCStyleComment;
    end;
  until (pCurrent^ = cEnd) or (pCurrent^ = #0);
  Advance;
end;

procedure TCppTokenizer.SkipAssignment;
begin
  repeat
    Inc(pCurrent);
    case pCurrent^ of
      '(': SkipPair('(', ')');
      '"': SkipDoubleQuotes;
      '''': SkipSingleQuote;
      '/': if (pCurrent + 1)^ = '/' then
          SkipToEOL
        else if (pCurrent + 1)^ = '*' then
          SkipCStyleComment;
    end;
//    if pCurrent^ = '(' then
//      SkipPair('(', ')');
  until pCurrent^ in [',', ';', ')', '{', '}', #0]; // + LineChars;
end;

procedure TCppTokenizer.Advance(bPerformChecks: boolean = True);
begin
  Inc(pCurrent);
  if (pCurrent^ <> #0) and bPerformChecks then
    case pCurrent^ of
      '''', '"': begin
          Inc(pCurrent);
          MatchChar((pCurrent - 1)^);
        end;
//      '"': SkipDoubleQuotes;
//      '''': SkipSingleQuote;
      '/': case (pCurrent + 1)^ of
          '*': SkipCStyleComment;
          '/': SkipToEOL;
        end;
      '=': SkipAssignment;
      '&', '*', '!', '|', '+', '-': if (pCurrent + 1)^ = '=' then
          SkipAssignment;
      '\': if (pCurrent + 1)^ in LineChars then
          SkipSplitLine;
    end;
end;

function TCppTokenizer.GetNumber: string;
var
  Offset: PChar;
begin
  fTmpOutput^ := #0;
  Offset := pCurrent;

  if pCurrent^ in DigitChars then
    while pCurrent^ in DigitChars + HexChars do
      Advance;

  if Offset <> pCurrent then begin
    StrLCopy(fTmpOutput, Offset, pCurrent - Offset);
    if pCurrent^ = '.' then // keep '.' for decimal
      StrLCat(StrEnd(fTmpOutput), pCurrent, 1);
    Result := StrPas(fTmpOutput);
  end
  else
    Result := '';
end;

function TCppTokenizer.GetWord(bSkipParenthesis: boolean = False; bSkipArray: boolean = False; bSkipBlock: boolean = False): string;
var
  Offset: PChar;
  Backup: PChar;
  tmp: integer;
  Done: boolean;
  AssignPos: PChar;
  localOutput: PChar;
begin
  localOutput := StrAlloc(MAX_TOKEN_SIZE);
  localOutput^ := #0;
  SkipToNextToken;
  Offset := pCurrent;

  repeat
    while pCurrent^ in LetterChars + DigitChars do
      Advance;
    // check for operator functions (look below too)
    if (pCurrent - Offset >= 8) and (StrLComp('operator', Offset, pCurrent - Offset) = 0) then begin
      if pCurrent^ in ['+', '-', '/', '*', '['] then begin
        Inc(pCurrent);
        if pCurrent^ = '(' then
          SkipPair('(', ')');
      end;
      Advance;
      Done := False;
    end
    else
      Done := True;
  until Done;

  // check for '<<' or '>>' operator
  Backup := pCurrent;
  while Backup^ in SpaceChars do
    Inc(Backup);
  if ((Backup^ = '<') and ((Backup + 1)^ = '<')) or
    ((Backup^ = '>') and ((Backup + 1)^ = '>')) then begin
    //skip to ';'
    repeat
      Inc(Backup);
    until Backup^ in [';', #0];
    pCurrent := Backup;
  end

  // check for <xxx> values (templates, lists etc)
  else if pCurrent^ = '<' then begin
    Backup := pCurrent;
    repeat
      Inc(Backup);
    until Backup^ in ['>', ';', '{', '}', '(', ')', '.', #0];
    if Backup^ = '>' then // got it!
      pCurrent := Backup + 1;
  end;

  if Offset <> pCurrent then begin
    StrLCopy(localOutput, Offset, pCurrent - Offset);
    // check for operator functions (look above too)
    // we separate the args from the func name (only for operators - for other funcs it's done automatically)
    AssignPos := StrPos(localOutput, '(');
    if (AssignPos <> nil) and (pCurrent - Offset >= 8) and (StrLComp('operator', Offset, 8) = 0) then begin
      AssignPos^ := #0;
      pCurrent := Offset + (AssignPos - localOutput);
      bSkipArray := False;
    end
    // if it contains assignment, remove it
    else begin
      AssignPos := StrPos(localOutput, '=');
      if AssignPos <> nil then
        AssignPos^ := #0;
    end;

    // we want it
    SkipToNextToken;
    if bSkipArray and (pCurrent^ = '[') then begin
      repeat
        Offset := pCurrent;
        tmp := 1;
        repeat
          repeat
            Inc(pCurrent);
            if pCurrent^ = '[' then
              Inc(tmp);
          until pCurrent^ in [#0, ']'] + LineChars;
          Dec(tmp);
        until tmp = 0;
        Inc(pCurrent);
//        SkipPair('[', ']');
        StrLCat(StrEnd(localOutput), Offset, pCurrent - Offset);
        SkipToNextToken;
      until pCurrent^ <> '['; // maybe multi-dimension array
    end
    else if bSkipBlock and (pCurrent^ = '{') then begin
      SkipPair('{', '}');
      SkipToNextToken;
    end;
    if pCurrent^ = '.' then // keep '.' for class-members
      StrLCat(StrEnd(localOutput), pCurrent, 1)
    else if (pCurrent^ = '-') and ((pCurrent + 1)^ = '>') then begin // keep '->' for members
      StrLCat(StrEnd(localOutput), pCurrent, 2);
      Inc(pCurrent, 2);
    end
    else if (pCurrent^ = ':') and ((pCurrent + 1)^ = ':') then begin // keep '::'
      StrLCat(StrEnd(localOutput), pCurrent, 2);
      Inc(pCurrent, 2); // there are 2 ':'!
      StrCat(localOutput, PChar(GetWord(bSkipParenthesis, bSkipArray, bSkipBlock)));
    end;
    Result := StrPas(localOutput);
  end
  else
    Result := '';
  StrDispose(localOutput);
end;

function TCppTokenizer.GetPreprocessor: string;
var
  Offset: PChar;
begin
  Offset := pCurrent;
  SkipToEOL;
  fTmpOutput^ := #0;
  StrLCopy(fTmpOutput, Offset, pCurrent - Offset);
  Result := StrPas(fTmpOutput);
end;

function TCppTokenizer.GetArguments: string;
var
  Offset: PChar;
begin
  fTmpOutput^ := #0;
  Result := '';
  Offset := pCurrent;
  SkipPair('(', ')');
  if pCurrent - Offset > MAX_TOKEN_SIZE then
    Exit;
  StrLCopy(fTmpOutput, Offset, pCurrent - Offset);
  if (pCurrent^ = '.') or ((pCurrent^ = '-') and ((pCurrent + 1)^ = '>')) then // skip '.' and '->'
    while not (pCurrent^ in [#0, '(', ';', '{', '}', ')'] + LineChars + SpaceChars) do
      Inc(pCurrent);
  SkipToNextToken;
  Result := StrPas(fTmpOutput);
end;

function TCppTokenizer.IsWord: boolean;
begin
  Result := pCurrent^ in LetterChars;
end;

function TCppTokenizer.IsNumber: boolean;
begin
  Result := pCurrent^ in DigitChars;
end;

function TCppTokenizer.IsPreprocessor: boolean;
begin
  Result := pCurrent^ = '#';
end;

function TCppTokenizer.IsArguments: boolean;
begin
  Result := pCurrent^ = '(';
end;

function TCppTokenizer.GetNextToken(bSkipParenthesis: boolean = False; bSkipArray: boolean = False; bSkipBlock: boolean = False): string;
var
  Done: boolean;
begin
  Result := '';
  Done := False;
  repeat
    SkipToNextToken;
    if pCurrent^ = #0 then
      Break;
    if IsPreprocessor then begin
      CountLines;
      Result := GetPreprocessor;
      Done := Result <> '';
    end
    else if IsArguments then begin
      CountLines;
      Result := GetArguments;
      Done := Result <> '';
    end
    else if IsWord then begin
      CountLines;
      Result := GetWord(False, bSkipArray, bSkipBlock);
      Done := Result <> '';
    end
    else if IsNumber then begin
      CountLines;
      Result := GetNumber;
      Done := Result <> '';
    end
    else begin
      case pCurrent^ of
        #0: Done := True;
        '/': case (pCurrent + 1)^ of
            '*': SkipCStyleComment;
            '/': SkipToEOL;
          else
            Advance;
          end;
        '{', '}', ';', ',', ':': begin //just return the brace or the ';'
            CountLines;
            Result := pCurrent^;
            Advance;
            Done := True;
          end;
      else
        Advance;
      end;
    end;
  until Done;
end;

function TCppTokenizer.Simplify(Str: string): string;
var
  I: integer;
  len: integer;
begin
  Result := Str;
  if Str = '' then
    Exit;

  len := Length(Result);
  fTmpOutput^ := #0;
  I := 1;
  while I <= len do begin
    // simplify spaces
    if Result[I] in [' ', #9] then begin
      while (Result[I] in [' ', #9]) and (I <= len) do
        Inc(I);
      StrLCat(StrEnd(fTmpOutput), ' ', 1);
    end;
    // remove comments
    if (I < len) and (Result[I] = '/') then begin
      case Result[I + 1] of
        '*': begin // C style
            repeat
              Inc(I);
            until (I >= len - 1) or ((Result[I] = '*') and (Result[I + 1] = '/'));
            if Result[I] = '*' then
              Inc(I, 2);
          end;
        '/': begin // C++ style
            repeat
              Inc(I);
            until (I >= len - 1) or ((Result[I] = '/') and (Result[I + 1] = '/'));
            if Result[I] = '/' then
              Inc(I, 2);
          end;
      end;
    end;
    if I <= len then
      if not (Result[I] in [' ', #9]) then
        StrLCat(StrEnd(fTmpOutput), @Result[I], 1);
    Inc(I);
  end;
  Result := StrPas(fTmpOutput);
end;

procedure TCppTokenizer.PostProcessToken(var Str: string);
begin
  Str := StringReplace(Str, '\'#13, '', [rfReplaceAll]);
  Str := StringReplace(Str, '\'#10, '', [rfReplaceAll]);
  Str := StringReplace(Str, #13, '', [rfReplaceAll]);
  Str := StringReplace(Str, #10, '', [rfReplaceAll]);
  Str := Simplify(Str);
end;

procedure TCppTokenizer.Tokenize(StartAt: PChar);
var
  S: string;
  LastToken: string;
  Command: string;
  bSkipBlocks: boolean;
  I: integer;
begin
  if StartAt = nil then
    Exit;

  Reset;
  fTmpOutput := StrAlloc(MAX_TOKEN_SIZE);
  pStart := StartAt;
  fEnd := Length(StrPas(pStart));
  pCurrent := pStart;
  pLineCount := pStart;

  S := '';
  bSkipBlocks := False;
  Command := '';
  fCurrLine := 1;
  if Assigned(fOnProgress) then
    fOnProgress(Self, '', fEnd, 0);
  repeat
    LastToken := S;
    S := GetNextToken(True, True, bSkipBlocks);
    PostProcessToken(S);
    if S <> '' then
      AddToken(S, fCurrLine);
    if Assigned(fOnProgress) then
      fOnProgress(Self, '', fEnd, pCurrent - pStart);
  until S = '';
  AddToken(#0, fCurrLine);
  if Assigned(fOnProgress) then
    fOnProgress(Self, '', fEnd, 0);
  if fLogTokens then
    if Assigned(fOnLogToken) then
      for I := 0 to fTokenList.Count - 1 do
        fOnLogToken(Self, Format('[tokenizer]: Idx: %4d Line: %4d Token: %s', [I, PToken(fTokenList[I])^.Line, PToken(fTokenList[I])^.Text]));
  StrDispose(fTmpOutput);
end;

procedure TCppTokenizer.Tokenize(FileName: TFilename);
begin
  if OpenFile(FileName) then
    Tokenize(pStart);
  ReleaseFileMemory;
end;

procedure TCppTokenizer.Tokenize(Stream: TStream);
begin
  if OpenStream(Stream) then
    Tokenize(pStart);
  ReleaseFileMemory;
end;

procedure TCppTokenizer.ReleaseFileMemory;
begin
  if pStart <> nil then
    FreeMem(pStart);
  pStart := nil;
end;

end.

