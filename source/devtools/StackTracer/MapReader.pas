unit MapReader;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StrUtils, StdCtrls, Buttons, ExtCtrls, ShellAPI, ComCtrls;

procedure ClearModules;
procedure ReadMapFile(Fname: string);
function AddressInfo(Address: dword): string;

implementation

type
  PUnitEntry = ^TUnitEntry;
  TUnitEntry = packed record
    Name: string;
    Start: dword;
    Len: integer;
  end;

  PFuncsEntry = ^TFuncsEntry;
  TFuncsEntry = packed record
    Name: string;
    Address: dword;
  end;

  PLineEntry = ^TLineEntry;
  TLineEntry = packed record
    Line: string;
    Address: dword;
    UnitIndex: dword;
  end;

  Long = record
    LoWord: Word;
    HiWord: Word;
  end;

var
  fLines: TList;
  fUnits: TList;
  fFuncs: TList;

/// EXCEPTIONS ////////////////////////////
{ Exceptions code created by Yiannis Mandravellos (mandrav@supergoal.gr)
  Used various resources to gather information }

procedure ReadMapFile(Fname: string);
var
  pUn: PUnitEntry;
  pFun: PFuncsEntry;
  pLin: PLineEntry;
  CurrUnit: integer;
  sl: TStringList;
  I: integer;
  idx: integer;
  iStart, iLen: longint;
  sUnitName: string;
begin
  if not FileExists(Fname) then
    Exit;

  sl := TStringList.Create;
  try
    sl.LoadFromFile(Fname);
    if sl.Count > 0 then begin

      // find "Detailed map of segments"
      I := 0;
      while I < sl.Count - 1 do begin
        if sl[I] = 'Detailed map of segments' then
          Break;
        Inc(I);
      end;

      // if not found, show error and abort
      if I >= sl.Count then
        Exit;

      // look for specific address' unit
      Inc(I);
      while (I < sl.Count - 1) and (sl[I] = '') do
        Inc(I);
      while I < sl.Count - 1 do begin
        if sl[I] = '' then
          Break;
        if Copy(sl[I], 1, 6) = ' 0001:' then begin
          iStart := StrToIntDef('$' + Copy(sl[I], 7, 8), 0);
          iLen := StrToIntDef('$' + Copy(sl[I], 16, 8), 0);
          pUn := New(PUnitEntry);
          idx := Pos('ACBP=', sl[I]);
          if idx > 0 then
            pUn^.Name := Trim(Copy(sl[I], 60, idx - 60 - 1));
          pUn^.Start := iStart;
          pUn^.Len := iLen;
          fUnits.Add(pUn);
        end;
        Inc(I);
      end;

      // find "  Address         Publics by Value"
      while I < sl.Count - 1 do begin
        if sl[I] = '  Address         Publics by Value' then
          Break;
        Inc(I);
      end;

      // locate function name
      Inc(I);
      while (I < sl.Count - 1) and (sl[I] = '') do
        Inc(I);
      while I < sl.Count - 1 do begin
        if sl[I] = '' then
          Break;
        if Copy(sl[I], 1, 6) = ' 0001:' then begin
          iStart := StrToIntDef('$' + Copy(sl[I], 7, 8), 0);
          pFun := New(PFuncsEntry);
          pFun^.Name := Trim(Copy(sl[I], 22, MaxInt));
          pFun^.Address := iStart;
          fFuncs.Add(pFun);
        end;
        Inc(I);
      end;

      CurrUnit := -1;
      while I < sl.Count - 1 do begin
        // find "Line numbers for"
        while I < sl.Count - 1 do begin
          if AnsiStartsStr('Line numbers for ', sl[I]) then begin
            idx := Pos('(', sl[I]);
            if idx > 0 then begin
              sUnitName := Copy(sl[I], 18, idx - 18);
              for idx := 0 to fUnits.Count - 1 do
                if CompareStr(sUnitName, PUnitEntry(fUnits[idx])^.Name) = 0 then begin
                  CurrUnit := idx;
                  Break;
                end;
            end;
            Break;
          end;
          Inc(I);
        end;

        // locate error line
        Inc(I);
        while (I < sl.Count - 1) and (sl[I] = '') do
          Inc(I);
        while (I < sl.Count - 1) and (sl[I] <> '') do begin
          pLin := New(PLineEntry);
          pLin^.Line := Trim(Copy(sl[I], 1, 6));
          pLin^.Address := StrToIntDef('$' + Copy(sl[I], 13, 8), 0);
          pLin^.UnitIndex := CurrUnit;
          fLines.Add(pLin);
          pLin := New(PLineEntry);
          pLin^.Line := Trim(Copy(sl[I], 21, 6));
          pLin^.Address := StrToIntDef('$' + Copy(sl[I], 33, 8), 0);
          pLin^.UnitIndex := CurrUnit;
          fLines.Add(pLin);
          pLin := New(PLineEntry);
          pLin^.Line := Trim(Copy(sl[I], 41, 6));
          pLin^.Address := StrToIntDef('$' + Copy(sl[I], 53, 8), 0);
          pLin^.UnitIndex := CurrUnit;
          fLines.Add(pLin);
          pLin := New(PLineEntry);
          pLin^.Line := Trim(Copy(sl[I], 61, 6));
          pLin^.Address := StrToIntDef('$' + Copy(sl[I], 73, 8), 0);
          pLin^.UnitIndex := CurrUnit;
          fLines.Add(pLin);
          Inc(I);
        end;
        Inc(I);
        if sl[I] = 'Bound resource files' then
          Break;
      end;
    end;
  finally
    sl.Free;
  end;
end;

procedure ClearModules;
begin
  while fLines.Count > 0 do begin
    Dispose(fLines[0]);
    fLines.Delete(0);
  end;
  while fFuncs.Count > 0 do begin
    Dispose(fFuncs[0]);
    fFuncs.Delete(0);
  end;
  while fUnits.Count > 0 do begin
    Dispose(fUnits[0]);
    fUnits.Delete(0);
  end;
end;

function AddressInfo(Address: dword): string;
var
  I: integer;
  MapAddress: dword;
  sUnitName: string;
  sProcName: string;
  sLineNum: string;
  UnitIdx: integer;
begin
  sUnitName := '';
  sProcName := '';
  sLineNum := '';
  Result := '';
  UnitIdx := -1;
  dword(MapAddress) := dword(Address) - (dword(hInstance) + $1000);
  if MapAddress >= $FF000000 then // out of scope
    Exit;

  // find unit
  for I := 0 to fUnits.Count - 1 do
    if (MapAddress >= pUnitEntry(fUnits[I])^.Start) and
      (dword(MapAddress) <= (dword(pUnitEntry(fUnits[I])^.Start) + dword(pUnitEntry(fUnits[I])^.Len))) then begin
      sUnitName := pUnitEntry(fUnits[I])^.Name;
      UnitIdx := I;
      Break;
    end;

  // find function
  for I := 0 to fFuncs.Count - 1 do
    if MapAddress < PFuncsEntry(fFuncs[I])^.Address then begin
      if I > 0 then
        sProcName := PFuncsEntry(fFuncs[I - 1])^.Name;
      Break;
    end;

  // find line
  for I := 0 to fLines.Count - 1 do
    if integer(PLineEntry(fLines[I])^.UnitIndex) = UnitIdx then
      if PLineEntry(fLines[I])^.Address > MapAddress then begin
        if (I > 0) and (integer(PLineEntry(fLines[I - 1])^.UnitIndex) = UnitIdx) then
          sLineNum := PLineEntry(fLines[I - 1])^.Line;
        Break;
      end;
  if sLineNum <> '' then
    Result := Format('%s.%s - %s', [sUnitName, sProcName, sLineNum])
  else begin
    if sUnitName <> '' then
      Result := sUnitName;
    if sProcName <> '' then begin
      if Result <> '' then
        Result := Result + '.';
      Result := Result + sProcName;
    end;
  end;
end;

initialization
  fLines := TList.Create;
  fFuncs := TList.Create;
  fUnits := TList.Create;

finalization
  ClearModules;
  fUnits.Free;
  fFuncs.Free;
  fLines.Free;

end.

