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
  Windows, Classes, SysUtils, StrUtils, ComCtrls, Math, cbutils;
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
  BlankChars: set of Char = [#0..#32];
  OperatorChars: set of Char = ['+', '-', '/', '*', '[', ']', '=', '%', '!', '&', '|', '>', '<', '^', '!'];

type
  PToken = ^TToken;
  TToken = record
    Text: string[255];
    Line: integer;
  end;

  TCppTokenizer = class(TComponent)
  private
    pStart: PAnsiChar;
    pCurrent: PAnsiChar;
    pLineCount: PAnsiChar;
    fLastToken: AnsiString;
    fEnd: integer;
    fCurrLine: integer;
    fTokenList: TList;
    fFileName: AnsiString;
    procedure AddToken(const sText: AnsiString; iLine: integer);
    function GetToken(index : integer) : PToken;
    procedure CountLines;
    procedure SkipCStyleComment;
    procedure SkipSplitLine;
    procedure SkipToEOL;
    procedure SkipToNextToken;
    procedure SkipDoubleQuotes;
    procedure SkipSingleQuote;
    procedure SkipPair(cStart, cEnd: Char; FailChars : TSysCharSet = []);
    procedure SkipAssignment;
    procedure SkipTemplate;
    function GetNumber: AnsiString;
    function GetWord(bSkipParenthesis: boolean = False; bSkipArray: boolean = False; bSkipBlock: boolean = False): AnsiString;
    function GetPreprocessor: AnsiString;
    function GetArguments: AnsiString;
    function GetForInit: AnsiString;
    function IsWord: boolean;
    function IsNumber: boolean;
    function IsPreprocessor: boolean;
    function IsArguments: boolean;
    function IsForInit: boolean;
    function GetNextToken(bSkipParenthesis: boolean = False; bSkipArray: boolean = False; bSkipBlock: boolean = False): AnsiString;
    procedure Simplify(var Output: AnsiString);
    procedure SimplifyArgs(var Output : AnsiString);
    procedure Advance;
    function OpenFile(const FileName: AnsiString): boolean;
    function OpenStream(Stream: TStream): boolean;
    // no manual buffer madness including Schlemiel algorithms anymore
    procedure CatString(var Dest : AnsiString; Source : PAnsiChar; Count : integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Reset;
    procedure TokenizeBuffer(StartAt: PAnsiChar);
    procedure TokenizeStream(const FileName: AnsiString; Stream: TStream);
    procedure TokenizeFile(const FileName: AnsiString);
    property TokenList[index : integer]: PToken read GetToken; default;
    property Tokens: TList read fTokenList;
  end;

procedure Register;

implementation

uses
  DateUtils;

procedure Register;
begin
  RegisterComponents('Dev-C++', [TCppTokenizer]);
end;

constructor TCppTokenizer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fTokenList := TList.Create;
end;

destructor TCppTokenizer.Destroy;
var
	I : integer;
begin
	for I := 0 to fTokenList.Count - 1 do
		Dispose(PToken(fTokenList[I]));
	fTokenList.Free;
	inherited Destroy;
end;

procedure TCppTokenizer.Reset;
var
	I : integer;
begin
	for I := 0 to fTokenList.Count - 1 do
		Dispose(PToken(fTokenList[I]));
	fTokenList.Clear;
end;

function TCppTokenizer.OpenFile(const FileName: AnsiString): boolean;
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
    end;
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
  end;
end;

procedure TCppTokenizer.AddToken(const sText: AnsiString; iLine: integer);
var
  Token: PToken;
begin
  Token := New(PToken);
  FillChar(Token^.Text, sizeof(Token^.Text), 0);
  Token^.Text := sText;
  Token^.Line := iLine;
  fTokenList.Add(Token);
end;

function TCppTokenizer.GetToken(index : integer) : PToken;
begin
	result := PToken(fTokenList[index]); // don't dereference, but pass pointer
end;

procedure TCppTokenizer.CountLines;
begin
  while (pLineCount^ <> #0) and (pLineCount < pCurrent) do begin
    if pLineCount^ = #10 then
      Inc(fCurrLine, 1);
    Inc(pLineCount, 1);
  end;
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
begin
  while (not (pCurrent^ in LineChars)) and (pCurrent^ <> #0) do begin
    if (pCurrent^ = '/') and ((pCurrent + 1)^ = '*') then
      SkipCStyleComment
    else begin
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
  Inc(pCurrent);
end;

procedure TCppTokenizer.SkipSingleQuote;
begin
  repeat
    Inc(pCurrent);
    if pCurrent^ = '\' then
      Inc(pCurrent, 2); // skip escaped quote
  until pCurrent^ in ['''', #0];
  Inc(pCurrent);
end;

procedure TCppTokenizer.SkipPair(cStart, cEnd: Char; FailChars : TSysCharSet); // e.g.: SkipPair('[', ']');
begin
	Inc(pCurrent);
	while pCurrent^ <> #0 do begin
		if pCurrent^ = cStart then begin
			SkipPair(cStart,cEnd,FailChars);
		end else if pCurrent^ = cEnd then begin
			Inc(pCurrent); // skip over end
			break;
		end else if pCurrent^ = '"' then begin
			if cStart <> '''' then
				SkipDoubleQuotes // don't do it inside AnsiString!
			else
				Inc(pCurrent);
		end else if pCurrent^ = '''' then begin
			SkipSingleQuote;
		end else if pCurrent^ = '/' then begin
			if (pCurrent + 1)^ = '/' then
				SkipToEOL
			else if (pCurrent + 1)^ = '*' then
				SkipCStyleComment // skips over */
			else
				Inc(pCurrent);
		end else if pCurrent^ in FailChars then begin
			Exit;
		end else
			Inc(pCurrent);
	end;
end;

procedure TCppTokenizer.SkipAssignment;
begin
  repeat
    Inc(pCurrent);
    case pCurrent^ of
      '(': SkipPair('(', ')');
      '"': SkipDoubleQuotes;
      '''': SkipSingleQuote;
      '{': SkipPair('{', '}'); // support struct initializers
      '/': if (pCurrent + 1)^ = '/' then
          SkipToEOL
        else if (pCurrent + 1)^ = '*' then
          SkipCStyleComment;
    end;
  until pCurrent^ in [',', ';', ')', '}', #0];
end;

procedure TCppTokenizer.SkipTemplate;
{var
	tmp : integer;}
var
	Start : PAnsiChar;
begin
	// Skip template contents. Do not blindy do a pair skip from < to >,
	// as there can be assignments within the part we have to skip
	// Take, for example, this masterpiece from bits\random.h, line 69, GCC 4.7.x":
	//template<typename _UIntType, size_t __w, bool = __w < static_cast<size_t>std::numeric_limits<_UIntType>::digits)>
	//                                                    ^ problem
	{if pCurrent^ = '<' then begin
		tmp := 1;
		repeat
			Inc(pCurrent);
			if pCurrent^ = '<' then
				Inc(tmp)
			else if pCurrent^ = '>' then
				Dec(tmp)
			else if pCurrent^ = '=' then begin
				Inc(pCurrent);
				// Do a dumb check for the case above, as I do not know of a more general way...

				// Skip spaces after =
				while pCurrent^ in SpaceChars do
				Inc(pCurrent);

				// Skip identifier (if there is one)
				while pCurrent^ in LetterChars do // don't accept numbers
					Inc(pCurrent);

				// Skip spaces after identifier
				while pCurrent^ in SpaceChars do
					Inc(pCurrent);

				// We found some boolean comparison. Don't count it
				if pCurrent^ in ['<','>'] then
					Inc(pCurrent^);
			end;
		until tmp = 0;
	end;}
	Start := pCurrent;
	if pCurrent^ <> '<' then
		Exit;

	SkipPair('<','>',['{','}',';']);

	// if we failed, return to where we came from
	if (pCurrent-1)^ <> '>' then
		pCurrent := Start;
end;

procedure TCppTokenizer.Advance;
begin
	case pCurrent^ of
		'"': SkipDoubleQuotes;
		'''': SkipSingleQuote;
		'/':
			case (pCurrent + 1)^ of
				'*': SkipCStyleComment;
				'/': SkipToEOL;
				'=': SkipAssignment;
				else
					Inc(pCurrent);
			end;
		'=': SkipAssignment;
		'&', '*', '!', '|', '+', '-', '~':
			if (pCurrent + 1)^ = '=' then
				SkipAssignment
			else
				Inc(pCurrent);
		'\':
			if (pCurrent + 1)^ in LineChars then
				SkipSplitLine
			else
				Inc(pCurrent);
		else
			Inc(pCurrent);
	end;
end;

function TCppTokenizer.GetNumber: AnsiString;
var
  Offset: PAnsiChar;
begin
	Offset := pCurrent;

	if pCurrent^ in DigitChars then
		while pCurrent^ in DigitChars + HexChars do
			Advance;

	if Offset <> pCurrent then begin
		SetString(Result,Offset,pCurrent - Offset);
		if pCurrent^ = '.' then // keep '.' for decimal
			CatString(Result, pCurrent, 1);
	end else
		Result := '';
end;

function TCppTokenizer.GetWord(bSkipParenthesis: boolean = False; bSkipArray: boolean = False; bSkipBlock: boolean = False): AnsiString;
var
	Offset: PAnsiChar;
	S: AnsiString;
	tmp: integer;
	bFoundTemplate : boolean;
begin
	bFoundTemplate := false;

	// Skip spaces
	SkipToNextToken;

	// Get next word...
	Offset := pCurrent;

	// Copy the word ahead of us
	while pCurrent^ in LetterChars + DigitChars do
		Inc(pCurrent);

	// Append the operator characters and argument list to the operator word
	if (pCurrent - Offset = Length('operator')) and (StrLComp('operator', Offset, pCurrent - Offset) = 0) then begin

		// Spaces between 'operator' and the operator itself are allowed
		while pCurrent^ in SpaceChars do
			Inc(pCurrent);

		// Find end of operator
		while pCurrent^ in OperatorChars do
			Inc(pCurrent);
	end else if (pCurrent - Offset = Length('template')) and (StrLComp('template', Offset, pCurrent - Offset) = 0) then begin
		bFoundTemplate := true;
	end;

	// We found a word...
	if Offset <> pCurrent then begin
		SetString(Result, Offset, pCurrent - Offset);

		// Skip whitespace
		SkipToNextToken;

		// Skip template contents, but keep template variable types
		if pCurrent^ = '<' then begin
			Offset := pCurrent;
			SkipTemplate;
			if not bFoundTemplate then
				CatString(Result, Offset, pCurrent - Offset);

		// Append array stuff
		end else if bSkipArray and (pCurrent^ = '[') then begin
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
				CatString(Result, Offset, pCurrent - Offset);
				SkipToNextToken;
			until pCurrent^ <> '['; // maybe multi-dimension array
		end else if bSkipBlock and (pCurrent^ = '{') then begin
			SkipPair('{', '}');
			SkipToNextToken;
		end;

		// Keep parent/child operators
		if pCurrent^ = '.' then begin
			CatString(Result, pCurrent, 1);
			Inc(pCurrent);
		end else if (pCurrent^ = '-') and ((pCurrent + 1)^ = '>') then begin
			CatString(Result, pCurrent, 2);
			Inc(pCurrent,2);
		end else if (pCurrent^ = ':') and ((pCurrent + 1)^ = ':') then begin
			CatString(Result, pCurrent, 2);
			Inc(pCurrent,2);

			// Append next token to this one
			S := GetWord(bSkipParenthesis, bSkipArray, bSkipBlock);
			CatString(Result,PAnsiChar(S),Length(S));
		end;
	end else
		Result := '';
end;

function TCppTokenizer.GetPreprocessor: AnsiString;
var
  Offset: PAnsiChar;
begin
  Offset := pCurrent;
  SkipToEOL;
  SetString(Result, Offset, pCurrent - Offset);
end;

function TCppTokenizer.GetArguments: AnsiString;
var
  Offset: PAnsiChar;
begin
  Offset := pCurrent;
  SkipPair('(', ')');
  SetString(Result, Offset, pCurrent - Offset);
  SimplifyArgs(Result);
  if (pCurrent^ = '.') or ((pCurrent^ = '-') and ((pCurrent + 1)^ = '>')) then // skip '.' and '->'
    while not (pCurrent^ in [#0, '(', ';', '{', '}', ')'] + LineChars + SpaceChars) do
      Inc(pCurrent);
  SkipToNextToken;
end;

function TCppTokenizer.GetForInit: AnsiString;
var
  StartOffset: PAnsiChar;
  S: AnsiString;
begin
	StartOffset := pCurrent;

	// Step into the init statement
	Inc(pCurrent);

	// Process until ; or end of file
	repeat
		S := GetNextToken(True, True, False);
		Simplify(S);
		if S <> '' then
			AddToken(S, fCurrLine);
	until (S = '') or (S = ';');

	// Skip to end of for loop
	pCurrent := StartOffset;
	SkipPair('(',')');
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

function TCppTokenizer.IsForInit: boolean;
begin
  Result := (pCurrent^ = '(') and (fLastToken = 'for');
end;

function TCppTokenizer.GetNextToken(bSkipParenthesis: boolean = False; bSkipArray: boolean = False; bSkipBlock: boolean = False): AnsiString;
var
  Done: boolean;
  DelimPos : integer;
begin
  Result := '';
  Done := False;
  repeat
    SkipToNextToken;
    if pCurrent^ = #0 then
      Break;
    if IsPreprocessor then begin
      CountLines;
      Result := GetPreprocessor; // don't count preprocessor lines
      if StartsStr('#include',Result) then begin
        DelimPos := LastPos(':',Result);
        if DelimPos > 0 then
          fCurrLine := StrToInt(TrimRight(Copy(Result,DelimPos + 1,MaxInt))) - 1; // fCurrLine is 0 based
      end;
      Done := Result <> '';
    end
    else if IsForInit then begin
      CountLines;
      Result := GetForInit;
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
        '>': begin // keep stream operators
          if (pCurrent + 1)^ = '>' then begin
            CountLines;
            Result := '>>';
            Advance;
            Done := True;
          end else
            Advance;
        end;
        '<': begin
          if (pCurrent + 1)^ = '<' then begin
            CountLines;
            Result := '<<';
            Advance;
            Done := True;
          end else
            Advance;
        end;
      else
        Advance;
      end;
    end;
  until Done;
end;

procedure TCppTokenizer.Simplify(var Output: AnsiString);
var
	DelimPosFrom, DelimPosTo: integer;
begin
	// Remove C-style comments
	while true do begin
		DelimPosFrom := Pos('/*',Output);
		if DelimPosFrom > 0 then begin
			DelimPosTo := PosEx('*/',Output,DelimPosFrom);
			if DelimPosTo > 0 then
				Delete(Output,DelimPosFrom,DelimPosTo - DelimPosFrom + Length('*/'))
			else
				break; // invalid syntax. ignore
		end else
			break;
	end;

	// Remove C++ style comments
	while true do begin
		DelimPosFrom := Pos('//',Output);
		if DelimPosFrom > 0 then begin
			DelimPosTo := PosEx(#10,Output,DelimPosFrom);
			if DelimPosTo > 0 then
				Delete(Output,DelimPosFrom,DelimPosTo - DelimPosFrom + Length(#10))
			else
				break; // invalid syntax. ignore
		end else
			break;
	end;

	Output := FastStringReplace(Output, '\'#13, '', [rfReplaceAll]);
	Output := FastStringReplace(Output, '\'#10, '', [rfReplaceAll]);
	Output := FastStringReplace(Output, #13, '', [rfReplaceAll]);
	Output := FastStringReplace(Output, #10, '', [rfReplaceAll]);
	Output := Trim(Output);
end;

procedure TCppTokenizer.SimplifyArgs(var Output : AnsiString);
var
	SearchStart, CommaPos : integer;

	procedure FormatSpacesAround(Index : integer;InsertCount : integer);
	var
		Head, InsertIndex : integer;
	begin
		InsertIndex := Index + 1;

		// Remove all before
		if Index > 1 then begin
			Head := Index;
			while (Head > 0) and (Output[Head] in BlankChars) do
				Dec(Head);
			if (Head-1 > 0) and (Output[Head+1] in BlankChars) then begin
				Delete(Output,Head,Index - Head);
				// update insert index due to removal
				Dec(InsertIndex,Index - Head);
			end;
		end;

		// Remove all after
		if Index+1 < Length(Output) then begin
			Head := Index + 1;
			while (Head <= Length(Output)) and (Output[Head] in BlankChars) do
				Inc(Head);
			if (Head-1 <= Length(Output)) and (Output[Head-1] in BlankChars) then
				Delete(Output,Index+1,Head-Index-1);
		end;

		// Insert at starting position
		if InsertCount > 0 then
			Insert(StringOfChar(' ',InsertCount),Output,InsertIndex);
	end;
begin
	// Format so that string that comes out is formatted as (int a, int b, int c)
	if (Length(Output) = 0) then
		Exit;

	// Remove at starting brace
	FormatSpacesAround(1,0);

	SearchStart := 1;
	while true do begin
		CommaPos := PosEx(',',Output,SearchStart);
		if CommaPos > 0 then begin
			FormatSpacesAround(CommaPos,1);
			SearchStart := CommaPos + 1;
		end else
			break;
	end;

	// Remove at ending brace
	FormatSpacesAround(Length(Output),0);
end;

procedure TCppTokenizer.TokenizeBuffer(StartAt: PAnsiChar);
var
  S: AnsiString;
  Command: AnsiString;
  bSkipBlocks: boolean;
begin
  if StartAt = nil then
    Exit;

  Reset;
  pStart := StartAt;
  fEnd := Length(StrPas(pStart));
  pCurrent := pStart;
  pLineCount := pStart;

  S := '';
  bSkipBlocks := False;
  Command := '';
  fCurrLine := 1;
  repeat
    fLastToken := S;
    S := GetNextToken(True, True, bSkipBlocks);
    Simplify(S);
    if S <> '' then
      AddToken(S, fCurrLine);
  until S = '';
  AddToken(#0, fCurrLine);
end;

procedure TCppTokenizer.TokenizeStream(const FileName: AnsiString; Stream: TStream);
begin
	fFileName := FileName;
	if OpenStream(Stream) then
		TokenizeBuffer(pStart);
end;

procedure TCppTokenizer.TokenizeFile(const FileName: AnsiString);
begin
	fFileName := FileName;
	if OpenFile(FileName) then
		TokenizeBuffer(pStart);
end;

procedure TCppTokenizer.CatString(var Dest : AnsiString; Source : PAnsiChar; Count : integer);
var
	OldLength : integer;
begin
	OldLength := Length(Dest);
	SetLength(Dest,Length(Dest) + Count);
	StrLCopy(@Dest[OldLength + 1],Source,Count);
end;

end.

