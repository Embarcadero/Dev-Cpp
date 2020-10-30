{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditRegexSearch.pas, released 2002-07-26.

Original Code by Eduardo Mauro, Gerald Nunn and Flávio Etrusco.
Unicode translation by Maël Hörz.
All Rights Reserved.

Contributors to the SynEdit project are listed in the Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SynEditRegexSearch.pas,v 1.5.2.2 2008/09/14 16:24:59 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynEditRegexSearch;

{$I SynEdit.inc}

interface

uses
  SynEditTypes,
  RegularExpressions,
  RegularExpressionsCore,
  SynEditMiscClasses,
  SynUnicode,
  Classes;

type
  TSynEditRegexSearch = class(TSynEditSearchCustom)
  private
    RegEx : TRegEx;
    fMatchCollection: TMatchCollection;
    fOptions : TRegExOptions;
    fPattern: String;
  protected
    function GetPattern: string; override;
    procedure SetPattern(const Value: string); override;
    procedure SetOptions(const Value: TSynSearchOptions); override;
    function GetLength(Index: Integer): Integer; override;
    function GetResult(Index: Integer): Integer; override;
    function GetResultCount: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    function FindAll(const NewText: string): Integer; override;
    function Replace(const aOccurrence, aReplacement: string): string; override;
  end;

  ESynRegEx = ERegularExpressionError;

implementation

uses
  RegularExpressionsAPI,
  Consts;


type
  { TPerlRegExHelper }

  TPerlRegExHelper = class helper for TPerlRegEx
    procedure SetAdditionalPCREOptions(PCREOptions : Integer);
  end;

procedure TPerlRegExHelper.SetAdditionalPCREOptions(PCREOptions: Integer);
begin
  with Self do FPCREOptions := FPCREOptions or PCREOptions;
end;

type
  TRegExHelper = record helper for TRegEx
  public
    procedure SetAdditionalPCREOptions(PCREOptions : Integer);
  end;

procedure TRegExHelper.SetAdditionalPCREOptions(PCREOptions: Integer);
begin
  with Self do FRegEx.SetAdditionalPCREOptions(PCREOptions);
end;

{ TSynEditRegexSearch }

constructor TSynEditRegexSearch.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fOptions := [roNotEmpty];
end;

function TSynEditRegexSearch.FindAll(const NewText: string): Integer;
begin
  fMatchCollection :=  RegEx.Matches(NewText);
  Result := fMatchCollection.Count;
end;

function TSynEditRegexSearch.Replace(const aOccurrence, aReplacement: string): string;
begin
  Result := RegEx.Replace(aOccurrence, aReplacement);
end;

function TSynEditRegexSearch.GetLength(Index: Integer): Integer;
begin
  Result := fMatchCollection[Index].Length;
end;

function TSynEditRegexSearch.GetPattern: string;
begin
  Result := fPattern;
end;

function TSynEditRegexSearch.GetResult(Index: Integer): Integer;
begin
  Result := fMatchCollection[Index].Index;
end;

function TSynEditRegexSearch.GetResultCount: Integer;
begin
  Result := fMatchCollection.Count;
end;

procedure TSynEditRegexSearch.SetOptions(const Value: TSynSearchOptions);
begin
  if ssoMatchCase in Value then
    fOptions := [roNotEmpty]
  else
    fOptions := [roNotEmpty, roIgnoreCase];
  RegEx := TRegEx.Create(fPattern, fOptions);
  RegEx.SetAdditionalPCREOptions(PCRE_UCP);
end;

procedure TSynEditRegexSearch.SetPattern(const Value: string);
begin
  fPattern := Value;
  RegEx := TRegEx.Create(fPattern, fOptions);
  RegEx.SetAdditionalPCREOptions(PCRE_UCP);
end;

end.

