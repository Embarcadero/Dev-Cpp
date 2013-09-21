{   Unzips deflated, imploded, shrunk and stored files
  ** COMPATIBLE WITH
        * Turbo Pascal v7.x     (DOS Real Mode)
        * Borland Pascal v7.x   (Dos Real Mode, DOS DPMI, and Win16)
        * Delphi v1.x           (Win16)
        * Delphi v2.x           (Win32)
        * Delphi v3.x           (Win32)
        * Delphi v4.x           (Win32)
        * Delphi v6.x           (Win32)
        * Delphi v7.x           (Win32)
        * Kylix  v3.0           (Linux)
        * Virtual Pascal v2.x   (Win32,
                                 OS/2,
                                 Linux)
        * Free Pascal v1.x      (Win32)
        * GNU Pascal (v2.1):
                                (Dos32,
                                 Win32,
                                 Linux,
                                 Solaris,
                                 Irix,
                                 Ultrix, etc)
        * TMT Pascal v3.50+     (Win32, Dos32)
}

UNIT unzip;
{
  Original version (1.x): Christian Ghisler
   C code by info-zip group, translated to pascal by Christian Ghisler
   based on unz51g.zip;
   Special thanks go to Mark Adler,who wrote the main inflate and
   explode code, and did NOT copyright it!!!

 March 1998: Dr Abimbola Olowofoyeku (The African Chief)
            http:www.bigfoot.com/~African_Chief/
   * modified to compile for Delphi v2.x and Delphi v3.x

 April 1998: Dr Abimbola Olowofoyeku (The African Chief)
   * source files merged into a single source (this) file
   * several high level functions added - i.e.,
              FileUnzip()
              FileUnzipEx()
              ViewZip()
              UnzipSize()
              SetUnzipReportProc()
              SetUnzipQuestionProc()
              ChfUnzip_Init()
   * callbacks added
   * modified to support Virtual Pascal v2.0 (Win32)
   * Delphi component added (chfunzip.pas)

  May 1998: Dr Abimbola Olowofoyeku
   * Functionality to make subdirectory recursion optional added
        "SetNoRecurseDirs"
   * Support for Free Pascal (FPC) added (by Peter Vreman)

  June 1998: Dr Abimbola Olowofoyeku
   * STDCALL added for all Win32 platforms

  June 1998: Dr Abimbola Olowofoyeku
   * support for OS/2 added (Virtual Pascal 2.0)
   * several amendments to UNZIP.INC

  June 1998: Dr Abimbola Olowofoyeku
   * fixed to support OS/2 DLLs
   * minor enhancements to MATCH.PAS

  March 1999: Dr Abimbola Olowofoyeku
   * fixed to support GNU Pascal
   * fixed to avoid moving file memory images (by Dr Peter Gerwinski)
   * declared some new data types so that we can be sure as to
     the exact types and sizes of variables being used.
   * new CRC-32 routines (all in Pascal)

  Sept. 1999: Dr Abimbola Olowofoyeku
   * fixed to avoid aborting on the first error in the zip file

  Dec. 1999: Dr Abimbola Olowofoyeku
   * can now process *some* self-extracting archives

  April 2000: Dr Abimbola Olowofoyeku
   * change to parameter list of IsZip
   * fixed a number of bugs

  Sept. 2000: Prof. Abimbola Olowofoyeku
   * tidied up some data structures
   * endian stuff added: the library is now endian-neutral
   * "View" method in chfunzip.pas renamed to "List"
   * TMT Pascal support added

  Oct. 2001: Prof. Abimbola Olowofoyeku
   * fixed to compile for Delphi 6

  Dec. 2003: Prof. Abimbola Olowofoyeku
   * fixed to compile for Delphi 7, and new GPC releases
   * fixed some bugs
   * now supports Kylix (3.0)
   * now supports Linux under Virtual Pascal
}

{$I unzip.inc}    {include file for conditional definitions}

{ * we want our data files to be compatible across differing endian systems * }
{$define __endian_neutral_data__}

INTERFACE

{$ifndef fpc}
  {$F+}
  {$R-}         {No range checking}
{$endif}

USES
{$ifdef __GPC__}
  gpc,
 {$ifdef GPC_Win32}
 Windows,
 {$endif}
{$endif}

{$ifdef UseWin}
  {$ifdef __TMT__}
     windows,
     messages,
     {$ifdef __CON__} Dos, {$ifndef NOCRT} crt, {$endif} {$endif}
  {$else __TMT__}
    {$ifdef Win16}
     wintypes,
     winprocs,
     {$else}
     Windows,
     {$endif} {Win16}
  {$endif __TMT__}
  {$ifdef Delphi}
    Messages,
    Sysutils,
  {$else Delphi}
    strings,
    windos,
  {$endif Delphi}
{$else UseWin}
   {$ifdef kylix}
   SysUtils, Libc,
   {$else}
   strings, dos,
   {$ifndef NOCRT} crt, {$endif}
   {$endif}
{$endif UseWin}
   ziptypes;

{**********************************************************************}
{**********************************************************************}
{****** HIGH LEVEL FUNCTIONS: BY THE AFRICAN CHIEF ********************}
{**********************************************************************}
{**********************************************************************}

FUNCTION FileUnzip
( SourceZipFile, TargetDirectory, FileSpecs : pChar;
 Report : UnzipReportProc;Question : UnzipQuestionProc ) : integer;
{$ifdef USE_STDCALL}STDCALL;{$else}{$ifndef NO_EXPORTS}EXPORT;{$endif NO_EXPORTS}{$endif} {USE_STDCALL}

{
high level unzip
usage:
SourceZipFile: source zip file;
TargetDirectory: target directory
FileSpecs: "*.*", etc.
Report: Report callback or Nil;
Question: Question callback (for confirmation of whether to replace existing
          files) or Nil;

* REFER to ZIPTYPES.PAS for information on callback functions

e.g.,
   Count := FileUnzip ('test.zip', 'c:\temp', '*.*', MyReportProc, Nil);

}

FUNCTION FileUnzipEx
( SourceZipFile, TargetDirectory, FileSpecs : pChar ) : integer;
{$ifdef USE_STDCALL}STDCALL;{$else USE_STDCALL}{$ifndef NO_EXPORTS}EXPORT;{$endif NO_EXPORTS}{$endif USE_STDCALL}
{
high level unzip with no callback parameters;
passes ZipReport & ZipQuestion internally, so you
can use SetZipReportProc and SetZipQuestionProc before calling this;

e.g.,
   Count := FileUnzipEx ('test.zip', 'c:\temp', '*.*');
}

FUNCTION ViewZip
( SourceZipFile, FileSpecs : pChar; Report : UnzipReportProc ) : integer;
{$ifdef USE_STDCALL}STDCALL;{$else USE_STDCALL}{$ifndef NO_EXPORTS}EXPORT;{$endif NO_EXPORTS}{$endif USE_STDCALL}
{
view contents of zip file
usage:
SourceZipFile: source zip file;
FileSpecs: "*.*", etc.
Report: callback procedure to process the reported contents of ZIP file;

* REFER to ZIPTYPES.PAS for information on callback functions

e.g.,
  ViewZip ('test.zip', '*.*', MyReportProc);
}

FUNCTION  SetUnZipReportProc ( aProc : UnzipReportProc ) : Pointer;
{$ifdef USE_STDCALL}STDCALL;{$else USE_STDCALL}{$ifndef NO_EXPORTS}EXPORT;{$endif NO_EXPORTS}{$endif USE_STDCALL}
{
sets the internal unzip report procedure to aproc
Returns: pointer to the original report procedure
(return value should normally be ignored)

e.g.,
   SetUnZipReportProc (MyReportProc);
}

FUNCTION  SetUnZipQuestionProc ( aProc : UnzipQuestionProc ) : Pointer;
{$ifdef USE_STDCALL}STDCALL;{$else USE_STDCALL}{$ifndef NO_EXPORTS}EXPORT;{$endif NO_EXPORTS}{$endif USE_STDCALL}
{
sets the internal unzip question procedure to aproc
Returns: pointer to the original "question" procedure
(return value should normally be ignored)

e.g.,
SetUnZipQuestionProc (QueryFileExistProc);
}

FUNCTION UnzipSize
( SourceZipFile : pChar; VAR Compressed : Word64 ) : Word64;
{$ifdef USE_STDCALL}STDCALL;{$else USE_STDCALL}{$ifndef NO_EXPORTS}EXPORT;{$endif NO_EXPORTS}{$endif USE_STDCALL}
{ uncompressed and compressed zip size
 usage:
 SourceZipFile  = the zip file
 Compressed     = the compressed size of the files in the archive
 Returns:         the uncompressed size of the ZIP archive

e.g.,
  Var
  Size,CSize:longint;
  begin
     Size := UnzipSize ('test.zip', CSize);
  end;
}

PROCEDURE ChfUnzip_Init;
{$ifdef USE_STDCALL}STDCALL;{$else USE_STDCALL}{$ifndef NO_EXPORTS}EXPORT;{$endif NO_EXPORTS}{$endif USE_STDCALL}
{
initialise or reinitialise the shared data: !!! use with care !!!
}

FUNCTION SetNoRecurseDirs ( DontRecurse : Boolean ) : Boolean;
{$ifdef USE_STDCALL}STDCALL;{$else USE_STDCALL}{$ifndef NO_EXPORTS}EXPORT;{$endif NO_EXPORTS}{$endif USE_STDCALL}
{
determine whether the UNZIP function should recreate
the subdirectory structure;
 DontRecurse = TRUE :  don't recurse
 DontRecurse = FALSE : recurse (default)
}

FUNCTION RecursiveMkDir ( aPath :  pChar ) : Integer;
{$ifdef USE_STDCALL}STDCALL;{$else USE_STDCALL}{$ifndef NO_EXPORTS}EXPORT;{$endif NO_EXPORTS}{$endif USE_STDCALL}
{
Iteratively create a directory path

 e.g.,
   i := RecursiveMkDir ( 'c:\fred\smith\jones\and\sons' );
   if i = 0 then Writeln('Success!') else Writeln('Error!');
}

FUNCTION GetHeaderOffset ( FileName : pChar; pEndoffSet : pLongint ) : Longint;
{$ifdef USE_STDCALL}STDCALL;{$else USE_STDCALL}{$ifndef NO_EXPORTS}EXPORT;{$endif NO_EXPORTS}{$endif USE_STDCALL}
{
  return the offset to the PK signature; with normal zip files, this
  will be zero - but SFX archives will start elsewhere
  * if pEndOffSet <> NIL then the size of the physical file is
    returned in it

  e.g.,
    StartOffSet := GetHeaderOffSet ( 'test.zip', Nil );
    OffSet := GetHeaderOffSet ( 'test.zip', @Size );
}

{**********************************************************************}
{**********************************************************************}
{************ LOW LEVEL FUNCTIONS: BY CHRISTIAN GHISLER ***************}
{***********  several amendments by Prof. Abimbola A Olowofoyeku ******}
{**********************************************************************}
{**********************************************************************}
{**********************************************************************}
FUNCTION GetSupportedMethods : longint;
{$ifdef USE_STDCALL}STDCALL;{$else USE_STDCALL}{$ifndef NO_EXPORTS}EXPORT;{$endif NO_EXPORTS}{$endif USE_STDCALL}
{Checks which pack methods are supported by the dll}
{bit 8=1 -> Format 8 supported, etc.}

FUNCTION UnzipFile
( in_name : pchar;out_name : pchar;offset : longint;hFileAction : word;cm_index : integer ) : integer;
{$ifdef USE_STDCALL}STDCALL;{$else USE_STDCALL}{$ifndef NO_EXPORTS}EXPORT;{$endif NO_EXPORTS}{$endif USE_STDCALL}
{usage:
 in_name:      name of zip file with full path
 out_name:     desired name for out file
 offset:       header position of desired file in zipfile
 hFileAction:  handle to dialog box showing advance of decompression (optional)
 cm_index:     notification code sent in a wm_command message to the dialog
               to update percent-bar
 Return value: one of the above unzip_xxx codes

 Example for handling the cm_index message in a progress dialog:

 unzipfile(......,cm_showpercent);

 ...

 procedure TFileActionDialog.wmcommand(var msg:tmessage);
 var ppercent:^word;
 begin
   TDialog.WMCommand(msg);
   if msg.wparam=cm_showpercent then begin
     ppercent:=pointer(lparam);
     if ppercent<>nil then begin
       if (ppercent^>=0) and (ppercent^<=100) then
         SetProgressBar(ppercent^);
       if UserPressedAbort then
         ppercent^:=$ffff
       else
         ppercent^:=0;
       end;
     end;
   end;
 end;
}

FUNCTION  GetFirstInZip ( zipfilename : pchar;VAR zprec : tZipRec ) : integer;
{$ifdef USE_STDCALL}STDCALL;{$else USE_STDCALL}{$ifndef NO_EXPORTS}EXPORT;{$endif NO_EXPORTS}{$endif USE_STDCALL}
{
 Get first entry from ZIP file
 e.g.,
   rc := GetFirstInZip ('test.zip', myZipRec);
}

FUNCTION  GetNextInZip ( VAR Zprec : tZiprec ) : integer;
{$ifdef USE_STDCALL}STDCALL;{$else USE_STDCALL}{$ifndef NO_EXPORTS}EXPORT;{$endif NO_EXPORTS}{$endif USE_STDCALL}
{
  Get next entry from ZIP file

 e.g.,
   rc := GetNextInZip (myZipRec);
}

FUNCTION  IsZip ( filename : pchar; pStartOffSet : pLongint ) : boolean;
{$ifdef USE_STDCALL}STDCALL;{$else USE_STDCALL}{$ifndef NO_EXPORTS}EXPORT;{$endif NO_EXPORTS}{$endif USE_STDCALL}
{
  test for zip file
  parameters:
    [1] zip file name
    [2] pointer to longint to hold the offset to the PK signature
        (this will normally zero, except for SFX files); pass NIL
        if you don't need the offset

  e.g.,
   ItsaZipFile := IsZip ('test.zip', @Offset);
   ItsaZipFile := IsZip ('test2.zip', Nil);
}

PROCEDURE CloseZipFile ( VAR Zprec : tZiprec );
{Only free buffer, file only open in Getfirstinzip}
{$ifdef USE_STDCALL}STDCALL;{$else USE_STDCALL}{$ifndef NO_EXPORTS}EXPORT;{$endif NO_EXPORTS}{$endif USE_STDCALL}
{
  free ZIP buffers

  e.g.,
  CloseZipFile (myZipRec);
}

FUNCTION FromOEM ( CONST s : String ) : String;
{ convert OEM chars to ANSI chars }

FUNCTION FromOEMToDisplay ( CONST s : String ) : String;
{ convert OEM chars to ANSI chars - for display purposes only }

IMPLEMENTATION

VAR
ZipReport     : UnzipReportProc;      {Global Status Report Callback}
ZipQuestion   : UnzipQuestionProc;    {Global "Question" Callback}
ZipRec        : TReportRec;           {Global ZIP record for callbacks}
NoRecurseDirs : Boolean;              {Global Recurse variable}

CONST fmOpenRead       = $0000;
CONST fmOpenWrite      = $0001;
CONST fmOpenReadWrite  = $0002;
CONST fmShareCompat    = $0000;
CONST fmShareExclusive = $0010;
CONST fmShareDenyWrite = $0020;
CONST fmShareDenyRead  = $0030;
CONST fmShareDenyNone  = $0040;

CONST { file sharing mode }
FileModeCompat = fmOpenRead {$ifdef __OS_DOS__} OR fmShareDenyNone {$endif};

{*** endian dummy functions for non-GPC platforms ***}
{ the calls to these functions will (under big endian systems
  like the Sun Sparcs) convert the compression data to and from
  small endian ordering; this makes the compression data compatible
  across big endian and small endian systems
}
{$ifndef __GPC__}
PROCEDURE ConvertFromLittleEndian ( VAR Buf; ElementSize, Count : Integer );
BEGIN
    { dummy }
END;
{$endif} {__GPC__}

PROCEDURE QuickEndianConversion ( VAR Buf; ElementSize : Integer );
BEGIN
  ConvertFromLittleEndian ( Buf, ElementSize, 1 );
END;

{$ifndef MSWINDOWS}
{$ifndef GPC_Win32}
PROCEDURE OemToChar ( p1, p2 : pChar );
BEGIN
 { dummy }
END;
{$endif}
{$endif}

{ convert from "oem" character sets to "ansi" }
Function FromOEM ( Const s : String ) : String;
Var
p : Array [0..{$ifdef __OS_DOS__}259{$else}2048{$endif}] of char;
Begin
  FromOEM := s;

  {$ifdef VirtualPascal}
     Exit;   { VP does the conversions for itself }
  {$endif}

  {$ifdef CanUseWinApi}
     Strpcopy (p, s);
     {$ifdef Win16}
     OEMToANSI (p, p);
     {$else}
     OEMToChar (p, p);
     {$endif}
     FromOEM := Strpas (p);
  {$else}  { CanUseWinApi }
     Exit;
  {$endif} { CanUseWinApi }
End;

FUNCTION FromOemP ( p : pChar ) : pChar;
BEGIN
   Strpcopy (p, FromOEM (strpas (p)));
   FromOemP := p;
END;

FUNCTION FromOEMToDisplay ( CONST s : String ) : String;
Begin
   FromOEMToDisplay :=
   {$ifdef Win16}
      FromOEM ( s )
   {$else}
      s   { do nothing }
   {$endif}
End;

{ some utility functions }
{$ifndef Delphi}
{$ifdef __GPC__}
FUNCTION ExtractFileDir ( CONST aName : String ) : TString;
BEGIN
  Result := DirFromPath  ( aName );
END;
{$else} {__GPC__}
FUNCTION CurrentDirectory : String;
{$ifndef FPC}
VAR
Result : String;
{$endif}
BEGIN
    GetDir ( 0, Result );
    CurrentDirectory := Result;
END;

{ return file attributes }
FUNCTION FileGetAttr ( CONST s : String ) : Word;
VAR
  f : FILE;
  Attr : Word;
BEGIN
  FileGetAttr := 0;
  Assign ( f, FromOEM ( s ) );
  GetFAttr ( f, Attr );
  IF DosError = 0 THEN FileGetAttr := Attr;
END;

{ return the pathname only - strip filename out }
FUNCTION ExtractFileDir ( CONST aName : String ) : String;
VAR
i : Word;
BEGIN
  i := Length ( aName );
  WHILE NOT ( aName [ i ] IN [ OS_Path_Separator, ':' ] ) AND ( i <> 0 ) DO
     Dec ( i );
  IF i = 0 THEN
    ExtractFileDir := ''
  ELSE IF i = 1 THEN
    ExtractFileDir := Copy ( CurrentDirectory, 1, 3 )
  ELSE
    ExtractFileDir := Copy ( aName, 1, i )
END;
{$endif} {__GPC__}
{$endif} {Delphi}

{$ifndef __GPC__}{$ifndef kylix}
FUNCTION DirectoryExists ( {$ifndef Win32}CONST{$endif}s : String ) : Boolean;
{does a directory exist?}
VAR
{$ifdef Win32}
Attr : DWORD;
{$else Win32}
{$ifdef Delphi}
Attr : Integer;
{$else Delphi}
f   : FILE;
Attr : word;
{$endif Delphi}
{$endif Win32}
BEGIN
{$ifdef Win32}
  {$ifndef FPC}
  s := s + #0;
  Attr := GetFileAttributes ( PChar ( @s [1] ) );
  Result := ( Attr <> $FFFFFFFF ) AND                  // Success ...
            ( Attr AND FILE_ATTRIBUTE_DIRECTORY <> 0 ) // Directory...
  {$else FPC}
   Attr := FileGetAttr ( s );
   DirectoryExists :=
   ( Attr >= 0 ) AND
   ( Attr AND {$ifdef UseWin}faDirectory{$else}Directory{$endif} <> 0 );
  {$endif FPC}
{$else Win32}
   Attr := FileGetAttr ( s );
   DirectoryExists :=
   ( Attr >= 0 ) AND
   ( Attr AND {$ifdef UseWin}faDirectory{$else}{$ifdef kylix}faDirectory{$else}Directory{$endif}{$endif} <> 0 );
{$endif Win32}
END;
{$endif} {__GPC__} {$endif} { kylix }


FUNCTION RecursiveMkDir {( aPath :  pChar ) : Integer};
VAR
  i  : Word;
  NewDir : String;
  Path   : ARRAY [0..255] OF Char;
BEGIN
 RecursiveMkDir := - 1;
 IF StrLen ( aPath ) = 0 THEN Exit;
 {$ifdef Delphi}
   StrPCopy ( Path, ExpandFileName ( StrPas ( aPath ) ) );
 {$else Delphi}
  {$ifdef MSWINDOWS}
   FileExpand ( Path, aPath );
  {$else MSWINDOWS}
   StrPCopy ( Path, FExpand ( StrPas ( aPath ) ) );
  {$endif MSWINDOWS}
 {$endif Delphi}
 i := 3;
 REPEAT
    REPEAT
      Inc ( i )
    UNTIL ( i > StrLen ( Path ) ) OR ( Path [ Pred ( i ) ] = OS_Path_Separator );
    NewDir := Copy ( StrPas ( Path ), 1, Pred ( i ) );
    IF NOT DirectoryExists ( NewDir )
    THEN BEGIN
        {$ifndef Win32}
        IF NOT DirectoryExists ( NewDir ) THEN
        {$endif Win32}
        {$i-}MkDir ( NewDir );{$ifdef Win32}{$i+}{$endif}
        IF IOResult <> 0 THEN Exit;
    END;
  UNTIL i > StrLen ( Path );
  RecursiveMkDir := 0; { success }
END;
{/////////////////////////////////////////////////////////}

{*************************************************************************}
{$ifdef Delphi}
FUNCTION SetFTime ( VAR f : FILE; CONST l : longint ) : integer;
BEGIN
 {$ifdef OS_BigMem}Result := {$endif}FileSetDate ( TFileRec ( f ) .{$ifdef kylix}Name{$else}Handle{$endif}, l );
END;
{$endif}{Delphi}
{/////////////////////////////////////////////////////////}
PROCEDURE SetFileAttrs ( {$ifndef kylix}CONST {$endif}s : String; CONST l : longint );
{$ifndef Delphi}
VAR
f : FILE;
old : integer;
{$endif}
{$ifdef kylix} Var k : integer;{$endif}
BEGIN
  {$ifdef Delphi}
    {$ifdef kylix}
    s := s + #0;
    k := S_IREAD or S_IWRITE;
    if (l and faReadOnly <> 0) then k := S_IREAD;
    chmod (@s[1], k);
    {$else}
    FileSetAttr ( s, l );
    {$endif}
  {$else}
  old := filemode;
  filemode := FileModeCompat;
  Assign ( f, FromOEM ( s ) );
  {$i-}Reset ( f, 1 );{$i+}
  filemode := old;
  IF ioresult = 0
  THEN BEGIN
    {$ifdef __GPC__}
    SetFattr ( Gpc_AnyFile ( f ), l );
    {$else}
    SetFattr ( f, l );
    {$endif}
    {$i-}Close ( f );{$i+} IF ioresult <> 0 THEN;
  END;
  {$endif}
END;

{$ifdef __GPC__}
PROCEDURE GetDir ( Drive : Byte; VAR s : String );
BEGIN
  IF Drive = 0
    THEN s := FExpand ( DirSelf )
    ELSE s := FExpand ( Succ ( 'a', Drive - 1 ) + ':' )
END;
{$endif}

{.$I z_global.pas}  {global constants, types and variables}
{Include file for unzip.pas: global constants, types and variables}

{C code by info-zip group, translated to pascal by Christian Ghisler}
{based on unz51g.zip}

CONST   {Error codes returned by huft_build}
  huft_complete   = 0;   {Complete tree}
  huft_incomplete = 1;   {Incomplete tree <- sufficient in some cases!}
  huft_error      = 2;   {bad tree constructed}
  huft_outofmem   = 3;   {not enough memory}

CONST
  MaxMax   = 31 * 1024;
  wsize    = $8000;       {Size of sliding dictionary}
  INBUFSIZ = 1024 * 4;    {Size of input buffer (4kb) }
  lbits    : integer = 9;
  dbits    : integer = 6;
  b_max    = 16;
  n_max    = 288;
  BMAX     = 16;

TYPE push     = ^ush;
     ush      = Word16;
     {pbyte    = ^byte;}
     pword16    = ^Word16;
     pushlist = ^ushlist;
     ushlist  = ARRAY [ 0..maxmax ] OF ush;  {only pseudo-size!!}
     piobuf   = ^iobuf;
     iobuf    = ARRAY [ 0..inbufsiz - 1 ] OF byte;

TYPE pphuft    = ^phuft;
     phuft     = ^huft;
     phuftlist = ^huftlist;
     huft      = RECORD
       e,             {# of extra bits}
       b : byte;      {# of bits in code}
       v_n : ush;
       v_t : phuftlist; {Linked List}
     END;
     huftlist = ARRAY [ 0..8190 ] OF huft;

TYPE li = RECORD
     {$ifdef __BYTES_BIG_ENDIAN__}
      Hi, Lo : Word16;
     {$else}
      Lo, Hi : Word16;
     {$endif}
     END;

{ pkzip header in front of every file in archive }
{$ifdef __GPC__}{$pack-struct, maximum-field-alignment 8}{$endif}
TYPE
  plocalheader = ^tlocalheader;
  tlocalheader = {$ifndef __GPC__}PACKED {$endif}RECORD {* must be packed *}
    signature : TSignatureType;  {'PK'#1#2}
    extract_ver,
    bit_flag,
    zip_type        : Word16;
    file_timedate,
    crc_32          : Int32;
    compress_size,
    uncompress_size : Word32;
    filename_len,
    extra_field_len : Word16;
  END;
{$ifdef __GPC__}{$no-pack-struct, maximum-field-alignment 0}{$endif}

{$ifdef __endian_neutral_data__}
PROCEDURE LocalHeader2BigEndian ( VAR aHead : tlocalheader ) ;
{ convert the whole local header from little to big endian }

{$ifdef __BYTES_BIG_ENDIAN__}
TYPE
 mtlocalheader = RECORD
    signature : TSignatureType;  {'PK'#1#2}
    extract_ver,
    bit_flag,
    zip_type        : Word16;
    file_timedate,
    crc_32          : Int32;
    compress_size,
    uncompress_size : Word32;
    filename_len,
    extra_field_len : Word16;
  END;
VAR
Head : mtlocalheader;
{$endif}
BEGIN
  {$ifdef __BYTES_BIG_ENDIAN__}
  WITH Head DO BEGIN
    extract_ver := aHead.extract_ver;
    bit_flag := aHead.bit_flag;
    zip_type := aHead.zip_type;
    file_timedate := aHead.file_timedate;
    crc_32 := aHead.crc_32;
    compress_size := aHead.compress_size;
    uncompress_size := aHead.uncompress_size;
    filename_len := aHead.filename_len;
    extra_field_len := aHead.extra_field_len;

    QuickEndianConversion ( extract_ver, Sizeof ( extract_ver ) );
    QuickEndianConversion ( bit_flag, Sizeof ( bit_flag ) );
    QuickEndianConversion ( zip_type, Sizeof ( zip_type ) );
    QuickEndianConversion ( file_timedate, Sizeof ( file_timedate ) );
    QuickEndianConversion ( crc_32, Sizeof ( crc_32 ) );
    QuickEndianConversion ( compress_size, Sizeof ( compress_size ) );
    QuickEndianConversion ( uncompress_size, Sizeof ( uncompress_size ) );
    QuickEndianConversion ( filename_len, Sizeof ( filename_len ) );
    QuickEndianConversion ( extra_field_len, Sizeof ( extra_field_len ) );
  END;

  WITH aHead DO BEGIN
    extract_ver := Head.extract_ver;
    bit_flag := Head.bit_flag;
    zip_type := Head.zip_type;
    file_timedate := Head.file_timedate;
    crc_32 := Head.crc_32;
    compress_size := Head.compress_size;
    uncompress_size := Head.uncompress_size;
    filename_len := Head.filename_len;
    extra_field_len := Head.extra_field_len;
  END;

  {$endif}
END;
{$endif}


VAR
    slide : pchar;            {Sliding dictionary for unzipping}
    inbuf : iobuf;            {input buffer}
    inpos, readpos : integer; {position in input buffer, position read from file}
    dlghandle : word;         {optional: handle of a cancel and "%-done"-dialog}
    dlgnotify : integer;      {notification code to tell dialog how far the decompression is}

VAR w : word;                 {Current Position in slide}
    b : longint;              {Bit Buffer}
    k : byte;                 {Bits in bit buffer}
    infile,                   {handle to zipfile}
    outfile : FILE;           {handle to extracted file}
    compsize,                 {comressed size of file}
    reachedsize,              {number of bytes read from zipfile}
    uncompsize : Word64;      {uncompressed size of file}
    oldpercent : integer;     {last percent value shown}
    crc32val   : longint;     {crc calculated from data}
    hufttype   : word;        {coding type=bit_flag from header}
    totalabort,               {User pressed abort button, set in showpercent!}
    zipeof : boolean;         {read over end of zip section for this file}
    inuse : boolean;          {is unit already in use -> don't call it again!!!}
{$ifdef MSWINDOWS}
    lastusedtime : longint;   {Time of last usage in timer ticks for timeout!}
{$endif}


(***************************************************************************)
{.$I z_tables.pas}  {Tables for bit masking, huffman codes and CRC checking}

{include file for unzip.pas: Tables for bit masking, huffman codes and CRC checking}

{C code by info-zip group, translated to Pascal by Christian Ghisler}
{based on unz51g.zip}

{b and mask_bits[i] gets lower i bits out of i}
CONST mask_bits : ARRAY [ 0..16 ] OF word16 =
   ( $0000,
    $0001, $0003, $0007, $000f, $001f, $003f, $007f, $00ff,
    $01ff, $03ff, $07ff, $0fff, $1fff, $3fff, $7fff, $ffff );

{ Tables for deflate from PKZIP's appnote.txt. }

CONST border : ARRAY [ 0..18 ] OF byte =   { Order of the bit length code lengths }
        ( 16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15 );
CONST cplens : ARRAY [ 0..30 ] OF word16 =    { Copy lengths for literal codes 257..285 }
        ( 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31,
        35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258, 0, 0 );
        { note: see note #13 above about the 258 in this list.}
CONST cplext : ARRAY [ 0..30 ] OF word16 =    { Extra bits for literal codes 257..285 }
        ( 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2,
        3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0, 99, 99 ); { 99==invalid }
CONST cpdist : ARRAY [ 0..29 ] OF word16 =     { Copy offsets for distance codes 0..29 }
        ( 1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193,
        257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145,
        8193, 12289, 16385, 24577 );
CONST cpdext : ARRAY [ 0..29 ] OF word16 =    { Extra bits for distance codes }
        ( 0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6,
        7, 7, 8, 8, 9, 9, 10, 10, 11, 11,
        12, 12, 13, 13 );

{ Tables for explode }

CONST cplen2 : ARRAY [ 0..63 ] OF word16 = ( 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,
        18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34,
        35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51,
        52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65 );

CONST cplen3 : ARRAY [ 0..63 ] OF word16 = ( 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18,
        19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35,
        36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52,
        53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66 );

CONST extra : ARRAY [ 0..63 ] OF word16 = ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        8 );

CONST cpdist4 : ARRAY [ 0..63 ] OF word16 = ( 1, 65, 129, 193, 257, 321, 385, 449, 513, 577, 641, 705,
        769, 833, 897, 961, 1025, 1089, 1153, 1217, 1281, 1345, 1409, 1473,
        1537, 1601, 1665, 1729, 1793, 1857, 1921, 1985, 2049, 2113, 2177,
        2241, 2305, 2369, 2433, 2497, 2561, 2625, 2689, 2753, 2817, 2881,
        2945, 3009, 3073, 3137, 3201, 3265, 3329, 3393, 3457, 3521, 3585,
        3649, 3713, 3777, 3841, 3905, 3969, 4033 );

CONST cpdist8 : ARRAY [ 0..63 ] OF word16 = ( 1, 129, 257, 385, 513, 641, 769, 897, 1025, 1153, 1281,
        1409, 1537, 1665, 1793, 1921, 2049, 2177, 2305, 2433, 2561, 2689,
        2817, 2945, 3073, 3201, 3329, 3457, 3585, 3713, 3841, 3969, 4097,
        4225, 4353, 4481, 4609, 4737, 4865, 4993, 5121, 5249, 5377, 5505,
        5633, 5761, 5889, 6017, 6145, 6273, 6401, 6529, 6657, 6785, 6913,
        7041, 7169, 7297, 7425, 7553, 7681, 7809, 7937, 8065 );

(***************************************************************************)
{.$I z_generl.pas}  {General functions used by both inflate and explode}
{include for unzip.pas: General functions used by both inflate and explode}

{C code by info-zip group, translated to Pascal by Christian Ghisler}
{based on unz51g.zip}

{***************** CRC Checking **************************}
{////// by the African Chief  ////////////////////////////}
PROCEDURE UpdateCRC32 ( VAR CRC : Longint ; VAR InBuf ; InLen : Word32 );
CONST
CRC32Table :
ARRAY [0..255] OF ZipBitInt = (
   $00000000, $77073096, $ee0e612c, $990951ba, $076dc419, $706af48f, $e963a535, $9e6495a3,
   $0edb8832, $79dcb8a4, $e0d5e91e, $97d2d988, $09b64c2b, $7eb17cbd, $e7b82d07, $90bf1d91,
   $1db71064, $6ab020f2, $f3b97148, $84be41de, $1adad47d, $6ddde4eb, $f4d4b551, $83d385c7,
   $136c9856, $646ba8c0, $fd62f97a, $8a65c9ec, $14015c4f, $63066cd9, $fa0f3d63, $8d080df5,
   $3b6e20c8, $4c69105e, $d56041e4, $a2677172, $3c03e4d1, $4b04d447, $d20d85fd, $a50ab56b,
   $35b5a8fa, $42b2986c, $dbbbc9d6, $acbcf940, $32d86ce3, $45df5c75, $dcd60dcf, $abd13d59,
   $26d930ac, $51de003a, $c8d75180, $bfd06116, $21b4f4b5, $56b3c423, $cfba9599, $b8bda50f,
   $2802b89e, $5f058808, $c60cd9b2, $b10be924, $2f6f7c87, $58684c11, $c1611dab, $b6662d3d,
   $76dc4190, $01db7106, $98d220bc, $efd5102a, $71b18589, $06b6b51f, $9fbfe4a5, $e8b8d433,
   $7807c9a2, $0f00f934, $9609a88e, $e10e9818, $7f6a0dbb, $086d3d2d, $91646c97, $e6635c01,
   $6b6b51f4, $1c6c6162, $856530d8, $f262004e, $6c0695ed, $1b01a57b, $8208f4c1, $f50fc457,
   $65b0d9c6, $12b7e950, $8bbeb8ea, $fcb9887c, $62dd1ddf, $15da2d49, $8cd37cf3, $fbd44c65,
   $4db26158, $3ab551ce, $a3bc0074, $d4bb30e2, $4adfa541, $3dd895d7, $a4d1c46d, $d3d6f4fb,
   $4369e96a, $346ed9fc, $ad678846, $da60b8d0, $44042d73, $33031de5, $aa0a4c5f, $dd0d7cc9,
   $5005713c, $270241aa, $be0b1010, $c90c2086, $5768b525, $206f85b3, $b966d409, $ce61e49f,
   $5edef90e, $29d9c998, $b0d09822, $c7d7a8b4, $59b33d17, $2eb40d81, $b7bd5c3b, $c0ba6cad,
   $edb88320, $9abfb3b6, $03b6e20c, $74b1d29a, $ead54739, $9dd277af, $04db2615, $73dc1683,
   $e3630b12, $94643b84, $0d6d6a3e, $7a6a5aa8, $e40ecf0b, $9309ff9d, $0a00ae27, $7d079eb1,
   $f00f9344, $8708a3d2, $1e01f268, $6906c2fe, $f762575d, $806567cb, $196c3671, $6e6b06e7,
   $fed41b76, $89d32be0, $10da7a5a, $67dd4acc, $f9b9df6f, $8ebeeff9, $17b7be43, $60b08ed5,
   $d6d6a3e8, $a1d1937e, $38d8c2c4, $4fdff252, $d1bb67f1, $a6bc5767, $3fb506dd, $48b2364b,
   $d80d2bda, $af0a1b4c, $36034af6, $41047a60, $df60efc3, $a867df55, $316e8eef, $4669be79,
   $cb61b38c, $bc66831a, $256fd2a0, $5268e236, $cc0c7795, $bb0b4703, $220216b9, $5505262f,
   $c5ba3bbe, $b2bd0b28, $2bb45a92, $5cb36a04, $c2d7ffa7, $b5d0cf31, $2cd99e8b, $5bdeae1d,
   $9b64c2b0, $ec63f226, $756aa39c, $026d930a, $9c0906a9, $eb0e363f, $72076785, $05005713,
   $95bf4a82, $e2b87a14, $7bb12bae, $0cb61b38, $92d28e9b, $e5d5be0d, $7cdcefb7, $0bdbdf21,
   $86d3d2d4, $f1d4e242, $68ddb3f8, $1fda836e, $81be16cd, $f6b9265b, $6fb077e1, $18b74777,
   $88085ae6, $ff0f6a70, $66063bca, $11010b5c, $8f659eff, $f862ae69, $616bffd3, $166ccf45,
   $a00ae278, $d70dd2ee, $4e048354, $3903b3c2, $a7672661, $d06016f7, $4969474d, $3e6e77db,
   $aed16a4a, $d9d65adc, $40df0b66, $37d83bf0, $a9bcae53, $debb9ec5, $47b2cf7f, $30b5ffe9,
   $bdbdf21c, $cabac28a, $53b39330, $24b4a3a6, $bad03605, $cdd70693, $54de5729, $23d967bf,
   $b3667a2e, $c4614ab8, $5d681b02, $2a6f2b94, $b40bbe37, $c30c8ea1, $5a05df1b, $2d02ef8d
);

VAR
BytePtr : ^Byte;
wcount : Word;
aCRC   : Longint;
BEGIN
     aCRC := CRC ;
     BytePtr := Addr ( InBuf ) ;
     FOR wcount := 1 TO InLen
     DO BEGIN
         aCRC := CRC32Table [Byte ( aCRC XOR Longint ( BytePtr^ ) ) ]
                     XOR ( ( aCRC SHR 8 ) AND $00ffffff ) ;
         Inc ( BytePtr );
     END ;
     CRC := aCRC;
END;
{/////////////////////////////////////////////////////////}
PROCEDURE InitCRC32 ( VAR CRC : Longint );
BEGIN
  CRC := - 1;
END;
{/////////////////////////////////////////////////////////}
FUNCTION FinalCRC32 ( CRC : Longint ) : Longint;
BEGIN
  FinalCRC32 := NOT CRC;
END;
{/////////////////////////////////////////////////////////}
PROCEDURE UpdateCRC ( VAR s : iobuf;len : integer );
BEGIN
   UpdateCRC32 ( crc32val, s , len );
END;
{/////////////////////////////////////////////////////////}
{/////////////////////////////////////////////////////////}
{/////////////////////////////////////////////////////////}
{************************ keep other programs running ***************************}
PROCEDURE messageloop;
{$ifdef MSWINDOWS}
VAR msg : tmsg;
BEGIN
  lastusedtime := gettickcount;
  WHILE PeekMessage ( {$ifdef fpc}@{$endif}Msg, 0, 0, 0, PM_Remove ) DO
    IF ( dlghandle = 0 ) OR
    NOT IsDialogMessage ( dlghandle, {$ifdef fpc}@{$endif}msg )
    THEN BEGIN
      TranslateMessage ( {$ifdef fpc}@{$endif}Msg );
      DispatchMessage ( {$ifdef fpc}@{$endif}Msg );
    END;
END;
{$else MSWINDOWS}
VAR ch : word;
BEGIN
 {$ifndef __GPC__}
 {$ifndef fpc}
 {$ifndef OS2}
 {$ifndef NOCRT}
  IF keypressed THEN BEGIN
    ch := byte ( readkey );
    IF ch = 0 THEN ch := 256 + byte ( readkey );  {Extended code}
    IF ch = dlgnotify THEN totalabort := TRUE;
  END
 {$endif}
 {$endif}
 {$endif}
 {$endif}
END;
{$endif MSWINDOWS}

{************************* tell dialog to show % ******************************}
{$ifdef MSWINDOWS}
PROCEDURE showpercent; {use this with the low level functions only !!!}
VAR percent : word;
BEGIN
  IF compsize <> 0 THEN BEGIN
    percent := reachedsize * 100 DIV compsize;
    IF percent > 100 THEN percent := 100;
    IF ( percent <> oldpercent )
    THEN BEGIN
      oldpercent := percent;
      IF dlghandle <> 0 THEN BEGIN     {Use dialog box for aborting}
        {Sendmessage returns directly -> ppercent contains result}
        sendmessage ( dlghandle, wm_command, dlgnotify, longint ( @percent ) );
        totalabort := ( percent = $FFFF );   {Abort pressed!}
      END ELSE
        IF dlgnotify <> 0 THEN
          totalabort := getasynckeystate ( dlgnotify ) < 0;  {break Key pressed!}
    END;
  END;
END;
{$endif}
{************************** fill inbuf from infile *********************}
PROCEDURE readbuf;
BEGIN
  IF reachedsize > compsize + 2
  THEN BEGIN {+2: last code is smaller than requested!}
    readpos := sizeof ( inbuf ); {Simulates reading -> no blocking}
    zipeof := TRUE
  END
  ELSE BEGIN
    messageloop;      {Other programs, or in DOS: keypressed?}
    {$ifdef MSWINDOWS}
    showpercent;      {Before, because it shows the data processed, not read!}
    {$endif}

    {$I-}
    blockread ( infile, inbuf, sizeof ( inbuf ), readpos );
    {$I+}

    IF ( ioresult <> 0 ) OR ( readpos = 0 )
    THEN BEGIN  {readpos=0: kein Fehler gemeldet!!!}
      readpos := sizeof ( inbuf ); {Simulates reading -> CRC error}
      zipeof := TRUE;
    END;
    inc ( reachedsize, readpos );
    dec ( readpos );    {Reason: index of inbuf starts at 0}
  END;
  inpos := 0;
END;

{**** read byte, only used by explode ****}

PROCEDURE READBYTE ( VAR bt : byte );
BEGIN
  IF inpos > readpos THEN readbuf;
  bt := inbuf [ inpos ];
  inc ( inpos );
END;

{*********** read at least n bits into the global variable b *************}

PROCEDURE NEEDBITS ( n : byte );
VAR nb : longint;
BEGIN
{$ifndef assembler}
  WHILE k < n
  DO BEGIN
    IF inpos > readpos THEN readbuf;
    nb := inbuf [ inpos ];
    inc ( inpos );
    b := b OR nb SHL k;
    inc ( k, 8 );
  END;
{$else}
  ASM
    mov si, offset inbuf
    mov ch, n
    mov cl, k
    mov bx, inpos    {bx=inpos}
@again :
    cmp cl, ch
    JAE @finished   {k>=n -> finished}
    cmp bx, readpos
    jg @readbuf
@fullbuf :
    mov al, [ si + bx ]  {dx:ax=nb}
    XOR ah, ah
    XOR dx, dx
    cmp cl, 8      {cl>=8 -> shift into DX or directly by 1 byte}
    JAE @bigger8
    SHL ax, cl     {Normal shifting!}
    jmp @continue
@bigger8 :
    mov di, cx     {save cx}
    mov ah, al     {shift by 8}
    XOR al, al
    sub cl, 8      {8 bits shifted}
@rotate :
    OR cl, cl
    jz @continue1 {all shifted -> finished}
    SHL ah, 1      {al ist empty!}
    rcl dx, 1
    dec cl
    jmp @rotate
@continue1 :
    mov cx, di
@continue :
    OR li.hi ( b ), dx {b=b or nb shl k}
    OR li.lo ( b ), ax
    inc bx         {inpos}
    add cl, 8       {inc k by 8 Bits}
    jmp @again

@readbuf :
    push si
    push cx
    call readbuf   {readbuf not critical, called only every 2000 bytes}
    pop cx
    pop si
    mov bx, inpos   {New inpos}
    jmp @fullbuf

@finished :
    mov k, cl
    mov inpos, bx
  END;
{$endif}
END;

{***************** dump n bits no longer needed from global variable b *************}

PROCEDURE DUMPBITS ( n : byte );
BEGIN
{$ifndef assembler}
  b := b SHR n;
  k := k - n;
{$else}
  ASM
    mov cl, n
    mov ax, li.lo ( b )
    mov dx, li.hi ( b )

    mov ch, cl
    OR ch, ch
    jz @finished
@rotate :
    SHR dx, 1           {Lower Bit in Carry}
    rcr ax, 1
    dec ch
    jnz @rotate
@finished :
    mov li.lo ( b ), ax
    mov li.hi ( b ), dx
    sub k, cl
  END;
{$endif}
END;

{********************* Flush w bytes directly from slide to file ******************}
FUNCTION flush ( w : word32 ) : boolean;
VAR n : {$ifdef ver120}word32{$else}nword{$endif};
b : boolean;
BEGIN
  {$I-}
  blockwrite ( outfile, slide [ 0 ], w, n );
  {$I+}
  b := ( n = w ) AND ( ioresult = 0 );  {True-> alles ok}
  UpdateCRC ( iobuf ( piobuf ( @slide [ 0 ] ) ^ ), w );
  {--}
  IF ( b = TRUE ) AND ( @ZipReport <> NIL )  {callback report for high level functions}
  THEN BEGIN
      WITH ZipRec
      DO BEGIN
           Status := file_unzipping;
           ZipReport{$ifdef __TMT__}^{$endif} ( n, @ZipRec );  {report the actual bytes written}
      END;
  END; {report}
  flush := b;
END;

{******************************* Break string into tokens ****************************}

VAR
  _Token : PChar;

FUNCTION StrTok ( Source : PChar; Token : CHAR ) : PChar;
  VAR P : PChar;
BEGIN
  IF Source <> NIL THEN _Token := Source;
  IF _Token = NIL THEN BEGIN
    strTok := NIL;
    exit
  END;
  P := StrScan ( _Token, Token );
  StrTok := _Token;
  IF P <> NIL THEN BEGIN
    P^ := #0;
    Inc ( P );
  END;
  _Token := P;
END;

(***************************************************************************)
{.$I z_huft.pas}    {Huffman tree generating and destroying}
{include for unzip.pas: Huffman tree generating and destroying}

{C code by info-zip group, translated to Pascal by Christian Ghisler}
{based on unz51g.zip}

{*************** free huffman tables starting with table where t points to ************}

PROCEDURE huft_free ( t : phuftlist );

VAR p, q : phuftlist;
    z : integer;

BEGIN
  p := pointer ( t );
  WHILE p <> NIL DO BEGIN
    dec ( longint ( p ), sizeof ( huft ) );
    q := p^ [ 0 ].v_t;
    z := p^ [ 0 ].v_n;   {Size in Bytes, required by TP ***}
    freemem ( p, ( z + 1 ) * sizeof ( huft ) );
    p := q
  END;
END;

{*********** build huffman table from code lengths given by array b^ *******************}

FUNCTION huft_build ( b : pword16; n : word; s : word; d, e : pushlist; t : pphuft;VAR m : integer ) : integer;
VAR a : word;                        {counter for codes of length k}
    c : ARRAY [ 0..b_max + 1 ] OF word;   {bit length count table}
    f : word;                        {i repeats in table every f entries}
    g,                             {max. code length}
    h : integer;                     {table level}
    i,                             {counter, current code}
    j : word;                        {counter}
    k : integer;                     {number of bits in current code}
    p : pword16;                       {pointer into c, b and v}
    q : phuftlist;                   {points to current table}
    r : huft;                        {table entry for structure assignment}
    u : ARRAY [ 0..b_max ] OF phuftlist;{table stack}
    v : ARRAY [ 0..n_max ] OF word16;     {values in order of bit length}
    w : integer;                     {bits before this table}
    x : ARRAY [ 0..b_max + 1 ] OF word16;   {bit offsets, then code stack}
    l : ARRAY [  - 1..b_max + 1 ] OF word16;  {l[h] bits in table of level h}
    xp : pword16;                       {pointer into x}
    y : integer;                     {number of dummy codes added}
    z : word;                        {number of entries in current table}
    tryagain : boolean;              {bool for loop}
    pt : phuft;                      {for test against bad input}
    el : word16;                     {length of eob code=code 256}

BEGIN
  IF n > 256 THEN el := pword16 ( longint ( b ) + 256 * sizeof ( word16 ) ) ^
           ELSE el := BMAX;
  {generate counts for each bit length}
  fillchar ( c, sizeof ( c ), #0 );
  p := b; i := n;                      {p points to array of word}
  REPEAT
    IF p^ > b_max THEN BEGIN
      t^ := NIL;
      m := 0;
      huft_build := huft_error;
      exit
    END;
    inc ( c [ p^ ] );
    inc ( longint ( p ), sizeof ( word16 ) );   {point to next item}
    dec ( i );
  UNTIL i = 0;
  IF c [ 0 ] = n THEN BEGIN
    t^ := NIL;
    m := 0;
    huft_build := huft_complete;
    exit
  END;

  {find minimum and maximum length, bound m by those}
  j := 1;
  WHILE ( j <= b_max ) AND ( c [ j ] = 0 ) DO inc ( j );
  k := j;
  IF m < j THEN m := j;
  i := b_max;
  WHILE ( i > 0 ) AND ( c [ i ] = 0 ) DO dec ( i );
  g := i;
  IF m > i THEN m := i;

  {adjust last length count to fill out codes, if needed}
  y := 1 SHL j;
  WHILE j < i DO BEGIN
    y := y - c [ j ];
    IF y < 0 THEN BEGIN
      huft_build := huft_error;
      exit
    END;
    y := y SHL 1;
    inc ( j );
  END;
  dec ( y, c [ i ] );
  IF y < 0 THEN BEGIN
    huft_build := huft_error;
    exit
  END;
  inc ( c [ i ], y );

  {generate starting offsets into the value table for each length}
  x [ 1 ] := 0;
  j := 0;
  p := pword16 ( @c ); {@@}
  inc ( longint ( p ), sizeof ( word16 ) );
  xp := pword16 ( @x ); {@@}
  inc ( longint ( xp ), 2 * sizeof ( word16 ) );
  dec ( i );
  WHILE i <> 0 DO BEGIN
    inc ( j, p^ );
    xp^ := j;
    inc ( longint ( p ), 2 );
    inc ( longint ( xp ), 2 );
    dec ( i );
  END;

  {make table of values in order of bit length}
  p := b; i := 0;
  REPEAT
    j := p^;
    inc ( longint ( p ), sizeof ( word16 ) );
    IF j <> 0 THEN BEGIN
      v [ x [ j ] ] := i;
      inc ( x [ j ] );
    END;
    inc ( i );
  UNTIL i >= n;

  {generate huffman codes and for each, make the table entries}
  x [ 0 ] := 0; i := 0;
  p := pword16 ( @v ); {@@}
  h := - 1;
  l [  - 1 ] := 0;
  w := 0;
  u [ 0 ] := NIL;
  q := NIL;
  z := 0;

  {go through the bit lengths (k already is bits in shortest code)}
  FOR k := k TO g DO BEGIN
    FOR a := c [ k ] DOWNTO 1 DO BEGIN
      {here i is the huffman code of length k bits for value p^}
      WHILE k > w + l [ h ] DO BEGIN
        inc ( w, l [ h ] ); {Length of tables to this position}
        inc ( h );
        z := g - w;
        IF z > m THEN z := m;
        j := k - w;
        f := 1 SHL j;
        IF f > a + 1 THEN BEGIN
          dec ( f, a + 1 );
          xp := @c [ k ];
          inc ( j );
          tryagain := TRUE;
          WHILE ( j < z ) AND tryagain DO BEGIN
            f := f SHL 1;
            inc ( longint ( xp ), sizeof ( word16 ) );
            IF f <= xp^ THEN tryagain := FALSE
                      ELSE BEGIN
                        dec ( f, xp^ );
                        inc ( j );
                      END;
          END;
        END;
        IF ( w + j > el ) AND ( w < el ) THEN
          j := el - w;       {Make eob code end at table}
        IF w = 0 THEN BEGIN
          j := m;  {*** Fix: main table always m bits!}
        END;
        z := 1 SHL j;
        l [ h ] := j;

        {allocate and link new table}
        getmem ( q, ( z + 1 ) * sizeof ( huft ) );
        IF q = NIL THEN BEGIN
          IF h <> 0 THEN huft_free ( pointer ( u [ 0 ] ) );
          huft_build := huft_outofmem;
          exit
        END;
        fillchar ( q^, ( z + 1 ) * sizeof ( huft ), #0 );
        q^ [ 0 ].v_n := z;  {Size of table, needed in freemem ***}
        t^ := @q^ [ 1 ];     {first item starts at 1}
        t :=  pphuft ( @q^ [ 0 ].v_t );  {@@}
        t^ := NIL;
        q := phuftlist ( @q^ [ 1 ] ); {@@}  {pointer(longint(q)+sizeof(huft));} {???}
        u [ h ] := q;
        {connect to last table, if there is one}
        IF h <> 0 THEN BEGIN
          x [ h ] := i;
          r.b := l [ h - 1 ];
          r.e := 16 + j;
          r.v_t := q;
          j := ( i AND ( ( 1 SHL w ) - 1 ) ) SHR ( w - l [ h - 1 ] );

          {test against bad input!}
          pt := phuft ( longint ( u [ h - 1 ] ) - sizeof ( huft ) );
          IF j > pt^.v_n THEN BEGIN
            huft_free ( pointer ( u [ 0 ] ) );
            huft_build := huft_error;
            exit
          END;

          pt := @u [ h - 1 ]^ [ j ];
          pt^ := r;
        END;
      END;

      {set up table entry in r}
      r.b := word ( k - w );
      r.v_t := NIL;   {Unused}   {***********}
      IF longint ( p ) >= longint ( @v [ n ] ) THEN r.e := 99
      ELSE IF p^ < s THEN BEGIN
        IF p^ < 256 THEN r.e := 16 ELSE r.e := 15;
        r.v_n := p^;
        inc ( longint ( p ), sizeof ( word16 ) );
      END ELSE BEGIN
        IF ( d = NIL ) OR ( e = NIL ) THEN BEGIN
          huft_free ( pointer ( u [ 0 ] ) );
          huft_build := huft_error;
          exit
        END;
        r.e := word ( e^ [ p^ - s ] );
        r.v_n := d^ [ p^ - s ];
        inc ( longint ( p ), sizeof ( word16 ) );
      END;

      {fill code like entries with r}
      f := 1 SHL ( k - w );
      j := i SHR w;
      WHILE j < z DO BEGIN
        q^ [ j ] := r;
        inc ( j, f );
      END;

      {backwards increment the k-bit code i}
      j := 1 SHL ( k - 1 );
      WHILE ( i AND j ) <> 0 DO BEGIN
        {i:=i^j;}
        i := i XOR j;
        j := j SHR 1;
      END;
      i := i XOR j;

      {backup over finished tables}
      WHILE ( ( i AND ( ( 1 SHL w ) - 1 ) ) <> x [ h ] ) DO BEGIN
        dec ( h );
        dec ( w, l [ h ] ); {Size of previous table!}
      END;
    END;
  END;
  IF ( y <> 0 ) AND ( g <> 1 ) THEN huft_build := huft_incomplete
                       ELSE huft_build := huft_complete;
END;

(***************************************************************************)
{.$I z_inflat.pas}  {Inflate deflated file}
{include for unzip.pas: Inflate deflated file}

{C code by info-zip group, translated to Pascal by Christian Ghisler}
{based on unz51g.zip}

FUNCTION inflate_codes ( tl, td : phuftlist;bl, bd : integer ) : integer;
VAR
    n, d, e1,          {length and index for copy}
    ml, md : word;      {masks for bl and bd bits}
    t : phuft;         {pointer to table entry}
    e : byte;          {table entry flag/number of extra bits}

BEGIN

  { inflate the coded data }
  ml := mask_bits [ bl ];          {precompute masks for speed}
  md := mask_bits [ bd ];
  WHILE NOT ( totalabort OR zipeof ) DO BEGIN
    NEEDBITS ( bl );
    t := @tl^ [ b AND ml ];
    e := t^.e;
    IF e > 16 THEN REPEAT       {then it's a literal}
      IF e = 99 THEN BEGIN
        inflate_codes := unzip_ZipFileErr;
        exit
      END;
      DUMPBITS ( t^.b );
      dec ( e, 16 );
      NEEDBITS ( e );
      t := @t^.v_t^ [ b AND mask_bits [ e ] ];
      e := t^.e;
    UNTIL e <= 16;
    DUMPBITS ( t^.b );
    IF e = 16 THEN BEGIN
      slide [ w ] := char ( t^.v_n );
      inc ( w );
      IF w = WSIZE THEN BEGIN
        IF NOT flush ( w ) THEN BEGIN
          inflate_codes := unzip_WriteErr;
          exit;
        END;
        w := 0
      END;
    END ELSE BEGIN                {it's an EOB or a length}
      IF e = 15 THEN BEGIN {Ende}   {exit if end of block}
        inflate_codes := unzip_Ok;
        exit;
      END;
      NEEDBITS ( e );                 {get length of block to copy}
      n := t^.v_n + ( b AND mask_bits [ e ] );
      DUMPBITS ( e );

      NEEDBITS ( bd );                {decode distance of block to copy}
      t := @td^ [ b AND md ];
      e := t^.e;
      IF e > 16 THEN REPEAT
        IF e = 99 THEN BEGIN
          inflate_codes := unzip_ZipFileErr;
          exit
        END;
        DUMPBITS ( t^.b );
        dec ( e, 16 );
        NEEDBITS ( e );
        t := @t^.v_t^ [ b AND mask_bits [ e ] ];
        e := t^.e;
      UNTIL e <= 16;
      DUMPBITS ( t^.b );
      NEEDBITS ( e );
      d := w - t^.v_n - b AND mask_bits [ e ];
      DUMPBITS ( e );
      {do the copy}
      REPEAT
        d := d AND ( WSIZE - 1 );
        IF d > w THEN e1 := WSIZE - d
               ELSE e1 := WSIZE - w;
        IF e1 > n THEN e1 := n;
        dec ( n, e1 );
        IF ( w - d >= e1 ) THEN BEGIN
          move ( slide [ d ], slide [ w ], e1 );
          inc ( w, e1 );
          inc ( d, e1 );
        END ELSE REPEAT
          slide [ w ] := slide [ d ];
          inc ( w );
          inc ( d );
          dec ( e1 );
        UNTIL ( e1 = 0 );
        IF w = WSIZE
        THEN BEGIN
          IF NOT flush ( w )
          THEN BEGIN
            inflate_codes := unzip_WriteErr;
            exit;
          END;
          w := 0;
        END;
      UNTIL n = 0;
    END;
  END;
  IF totalabort THEN
    inflate_codes := unzip_userabort
  ELSE
    inflate_codes := unzip_readErr;
END;

{**************************** "decompress" stored block **************************}

FUNCTION inflate_stored : integer;
VAR n : word;            {number of bytes in block}

BEGIN
  {go to byte boundary}
  n := k AND 7;
  dumpbits ( n );
  {get the length and its complement}
  NEEDBITS ( 16 );
  n := b AND $ffff;
  DUMPBITS ( 16 );
  NEEDBITS ( 16 );
  IF ( n <> ( NOT b ) AND $ffff ) THEN BEGIN
    inflate_stored := unzip_zipFileErr;
    exit
  END;
  DUMPBITS ( 16 );
  WHILE ( n > 0 ) AND NOT ( totalabort OR zipeof ) DO BEGIN {read and output the compressed data}
    dec ( n );
    NEEDBITS ( 8 );
    slide [ w ] := char ( b );
    inc ( w );
    IF w = WSIZE THEN BEGIN
      IF NOT flush ( w ) THEN BEGIN
        inflate_stored := unzip_WriteErr;
        exit
      END;
      w := 0;
    END;
    DUMPBITS ( 8 );
  END;
  IF totalabort THEN inflate_stored := unzip_UserAbort
    ELSE IF zipeof THEN inflate_stored := unzip_readErr
      ELSE inflate_stored := unzip_Ok;
END;

{**************************** decompress fixed block **************************}

FUNCTION inflate_fixed : integer;
VAR i : integer;               {temporary variable}
    tl,                      {literal/length code table}
    td : phuftlist;                {distance code table}
    bl, bd : integer;           {lookup bits for tl/bd}
    l : ARRAY [ 0..287 ] OF word16; {length list for huft_build}

BEGIN
  {set up literal table}
  FOR i := 0 TO 143 DO l [ i ] := 8;
  FOR i := 144 TO 255 DO l [ i ] := 9;
  FOR i := 256 TO 279 DO l [ i ] := 7;
  FOR i := 280 TO 287 DO l [ i ] := 8; {make a complete, but wrong code set}
  bl := 7;
  i := huft_build ( pword16 ( @l ), 288, 257, pushlist ( @cplens ), pushlist ( @cplext ), pphuft ( @tl ), bl ); {@@}
  IF i <> huft_complete THEN BEGIN
    inflate_fixed := i;
    exit
  END;
  FOR i := 0 TO 29 DO l [ i ] := 5;    {make an incomplete code set}
  bd := 5;
  i := huft_build ( pword16 ( @l ), 30, 0, pushlist ( @cpdist ), pushlist ( @cpdext ), pphuft ( @td ), bd );  {@@}
  IF i > huft_incomplete THEN BEGIN
    huft_free ( tl );
    inflate_fixed := unzip_ZipFileErr;
    exit
  END;
  inflate_fixed := inflate_codes ( tl, td, bl, bd );
  huft_free ( tl );
  huft_free ( td );
END;

{**************************** decompress dynamic block **************************}

FUNCTION inflate_dynamic : integer;
VAR i : integer;                      {temporary variables}
    j,
    l,                              {last length}
    m,                              {mask for bit length table}
    n : word;                         {number of lengths to get}
    tl,                             {literal/length code table}
    td : phuftlist;                   {distance code table}
    bl, bd : integer;                  {lookup bits for tl/bd}
    nb, nl, nd : word;                  {number of bit length/literal length/distance codes}
    ll : ARRAY [ 0..288 + 32 - 1 ] OF word16;  {literal/length and distance code lengths}

BEGIN
  {read in table lengths}
  NEEDBITS ( 5 );
  nl := 257 + word ( b ) AND $1f;
  DUMPBITS ( 5 );
  NEEDBITS ( 5 );
  nd := 1 + word ( b ) AND $1f;
  DUMPBITS ( 5 );
  NEEDBITS ( 4 );
  nb := 4 + word ( b ) AND $f;
  DUMPBITS ( 4 );
  IF ( nl > 288 ) OR ( nd > 32 ) THEN BEGIN
    inflate_dynamic := 1;
    exit
  END;
  fillchar ( ll, sizeof ( ll ), #0 );
  {read in bit-length-code lengths}
  FOR j := 0 TO nb - 1 DO BEGIN
    NEEDBITS ( 3 );
    ll [ border [ j ] ] := b AND 7;
    DUMPBITS ( 3 );
  END;
  FOR j := nb TO 18 DO ll [ border [ j ] ] := 0;

  {build decoding table for trees--single level, 7 bit lookup}
  bl := 7;
  i := huft_build ( pword16 ( @ll ), 19, 19, NIL, NIL, pphuft ( @tl ), bl ); {@@}
  IF i <> huft_complete THEN BEGIN
    IF i = huft_incomplete THEN huft_free ( tl ); {other errors: already freed}
    inflate_dynamic := unzip_ZipFileErr;
    exit
  END;

  {read in literal and distance code lengths}
  n := nl + nd;
  m := mask_bits [ bl ];
  i := 0; l := 0;
  WHILE word ( i ) < n DO BEGIN
    NEEDBITS ( bl );
    td := phuftlist ( @tl^ [ b AND m ] ); {@@}
    j := phuft ( td ) ^.b;
    DUMPBITS ( j );
    j := phuft ( td ) ^.v_n;
    IF j < 16 THEN BEGIN            {length of code in bits (0..15)}
      l := j;                       {ave last length in l}
      ll [ i ] := l;
      inc ( i )
    END ELSE IF j = 16 THEN BEGIN   {repeat last length 3 to 6 times}
      NEEDBITS ( 2 );
      j := 3 + b AND 3;
      DUMPBITS ( 2 );
      IF i + j > n THEN BEGIN
        inflate_dynamic := 1;
        exit
      END;
      WHILE j > 0 DO BEGIN
        ll [ i ] := l;
        dec ( j );
        inc ( i );
      END;
    END ELSE IF j = 17 THEN BEGIN   {3 to 10 zero length codes}
      NEEDBITS ( 3 );
      j := 3 + b AND 7;
      DUMPBITS ( 3 );
      IF i + j > n THEN BEGIN
        inflate_dynamic := 1;
        exit
      END;
      WHILE j > 0 DO BEGIN
        ll [ i ] := 0;
        inc ( i );
        dec ( j );
      END;
      l := 0;
    END ELSE BEGIN                {j == 18: 11 to 138 zero length codes}
      NEEDBITS ( 7 );
      j := 11 + b AND $7f;
      DUMPBITS ( 7 );
      IF i + j > n THEN BEGIN
        inflate_dynamic := unzip_zipfileErr;
        exit
      END;
      WHILE j > 0 DO BEGIN
        ll [ i ] := 0;
        dec ( j );
        inc ( i );
      END;
      l := 0;
    END;
  END;
  huft_free ( tl );        {free decoding table for trees}

  {build the decoding tables for literal/length and distance codes}
  bl := lbits;
  i := huft_build ( pword16 ( @ll ), nl, 257, pushlist ( @cplens ), pushlist ( @cplext ), pphuft ( @tl ), bl );
  IF i <> huft_complete THEN BEGIN
    IF i = huft_incomplete THEN huft_free ( tl );
    inflate_dynamic := unzip_ZipFileErr;
    exit
  END;
  bd := dbits;
  i := huft_build ( pword16 ( @ll [ nl ] ), nd, 0, pushlist ( @cpdist ), pushlist ( @cpdext ), pphuft ( @td ), bd );
  IF i > huft_incomplete THEN BEGIN {pkzip bug workaround}
    IF i = huft_incomplete THEN huft_free ( td );
    huft_free ( tl );
    inflate_dynamic := unzip_ZipFileErr;
    exit
  END;
  {decompress until an end-of-block code}
  inflate_dynamic := inflate_codes ( tl, td, bl, bd );
  huft_free ( tl );
  huft_free ( td );
END;

{**************************** decompress a block ******************************}

FUNCTION inflate_block ( VAR e : integer ) : integer;
VAR t : word16;           {block type}

BEGIN
  NEEDBITS ( 1 );
  e := b AND 1;
  DUMPBITS ( 1 );

  NEEDBITS ( 2 );
  t := b AND 3;
  DUMPBITS ( 2 );

  CASE t OF
    0 : BEGIN inflate_block := inflate_stored; END;
    1 : BEGIN inflate_block := inflate_fixed; END;
    2 : BEGIN inflate_block := inflate_dynamic; END;
  ELSE
    inflate_block := unzip_ZipFileErr;  {bad block type}
  END;
END;

{**************************** decompress an inflated entry **************************}

FUNCTION inflate : integer;
VAR e,                 {last block flag}
    r : integer;         {result code}

BEGIN
  inpos := 0;            {Input buffer position}
  readpos := - 1;         {Nothing read}

  {initialize window, bit buffer}
  w := 0;
  k := 0;
  b := 0;

  {decompress until the last block}
  REPEAT
    r := inflate_block ( e );
    IF r <> 0
    THEN BEGIN
      inflate := r;
      exit
    END;
  UNTIL e <> 0;

  {flush out slide}
  IF NOT flush ( w )
  THEN inflate := unzip_WriteErr
  ELSE inflate := unzip_Ok;
END;
(***************************************************************************)
{.$I z_copyst.pas}  {Copy stored file}
{include for unzip.pas: Copy stored file}

{C code by info-zip group, translated to Pascal by Christian Ghisler}
{based on unz51g.zip}

{************************* copy stored file ************************************}
FUNCTION copystored : integer;
VAR readin : longint;
    outcnt : nword;
BEGIN
  WHILE ( reachedsize < compsize ) AND NOT totalabort
  DO BEGIN
    readin := compsize - reachedsize;
    IF readin > wsize THEN readin := wsize;

    {$I-}
    blockread ( infile, slide [ 0 ], readin, outcnt );  {Use slide as buffer}
    {$I+}

    IF ( outcnt <> readin ) OR ( ioresult <> 0 )
    THEN BEGIN
      copystored := unzip_ReadErr;
      exit
    END;

    IF NOT flush ( outcnt )
    THEN BEGIN  {Flushoutput takes care of CRC too}
      copystored := unzip_WriteErr;
      exit
    END;

    inc ( reachedsize, outcnt );

    messageloop;      {Other programs, or in DOS: keypressed?}
    {$ifdef MSWINDOWS}
    showpercent;
    {$endif}
  END;
  IF NOT totalabort THEN
    copystored := unzip_Ok
  ELSE
    copystored := unzip_Userabort;
END;
(***************************************************************************)
{.$I z_explod.pas}  {Explode imploded file}
{include for unzip.pas: Explode imploded file}

{C code by info-zip group, translated to Pascal by Christian Ghisler}
{based on unz51g.zip}

{************************************* explode ********************************}

{*********************************** read in tree *****************************}
FUNCTION get_tree ( l : pword16;n : word ) : integer;
VAR i, k, j, b : word;
    bytebuf : byte;

BEGIN
  READBYTE ( bytebuf );
  i := bytebuf;
  inc ( i );
  k := 0;
  REPEAT
    READBYTE ( bytebuf );
    j := bytebuf;
    b := ( j AND $F ) + 1;
    j := ( ( j AND $F0 ) SHR 4 ) + 1;
    IF ( k + j ) > n THEN BEGIN
      get_tree := 4;
      exit
    END;
    REPEAT
      l^ := b;
      inc ( longint ( l ), sizeof ( word16 ) );
      inc ( k );
      dec ( j );
    UNTIL j = 0;
    dec ( i );
  UNTIL i = 0;
  IF k <> n THEN get_tree := 4 ELSE get_tree := 0;
END;

{******************exploding, method: 8k slide, 3 trees ***********************}

FUNCTION explode_lit8 ( tb, tl, td : phuftlist;bb, bl, bd : integer ) : integer;
VAR s : longint;
    e : word;
    n, d : word;
    w : word;
    t : phuft;
    mb, ml, md : word;
    u : word;

BEGIN
  b := 0; k := 0; w := 0;
  u := 1;
  mb := mask_bits [ bb ];
  ml := mask_bits [ bl ];
  md := mask_bits [ bd ];
  s := uncompsize;
  WHILE ( s > 0 ) AND NOT ( totalabort OR zipeof ) DO BEGIN
    NEEDBITS ( 1 );
    IF ( b AND 1 ) <> 0 THEN BEGIN  {Litteral}
      DUMPBITS ( 1 );
      dec ( s );
      NEEDBITS ( bb );
      t := @tb^ [  ( NOT b ) AND mb ];
      e := t^.e;
      IF e > 16 THEN REPEAT
        IF e = 99 THEN BEGIN
          explode_lit8 := unzip_ZipFileErr;
          exit
        END;
        DUMPBITS ( t^.b );
        dec ( e, 16 );
        NEEDBITS ( e );
        t := @t^.v_t^ [  ( NOT b ) AND mask_bits [ e ] ];
        e := t^.e;
      UNTIL e <= 16;
      DUMPBITS ( t^.b );
      slide [ w ] := char ( t^.v_n );
      inc ( w );
      IF w = WSIZE THEN BEGIN
        IF NOT flush ( w ) THEN BEGIN
          explode_lit8 := unzip_WriteErr;
          exit
        END;
        w := 0; u := 0;
      END;
    END ELSE BEGIN
      DUMPBITS ( 1 );
      NEEDBITS ( 7 );
      d := b AND $7F;
      DUMPBITS ( 7 );
      NEEDBITS ( bd );
      t := @td^ [  ( NOT b ) AND md ];
      e := t^.e;
      IF e > 16 THEN REPEAT
        IF e = 99 THEN BEGIN
          explode_lit8 := unzip_ZipFileErr;
          exit
        END;
        DUMPBITS ( t^.b );
        dec ( e, 16 );
        NEEDBITS ( e );
        t := @t^.v_t^ [  ( NOT b ) AND mask_bits [ e ] ];
        e := t^.e;
      UNTIL e <= 16;
      DUMPBITS ( t^.b );

      d := w - d - t^.v_n;
      NEEDBITS ( bl );
      t := @tl^ [  ( NOT b ) AND ml ];
      e := t^.e;
      IF e > 16 THEN REPEAT
        IF e = 99 THEN BEGIN
          explode_lit8 := unzip_ZipFileErr;
          exit
        END;
        DUMPBITS ( t^.b );
        dec ( e, 16 );
        NEEDBITS ( e );
        t := @t^.v_t^ [  ( NOT b ) AND mask_bits [ e ] ];
        e := t^.e;
      UNTIL e <= 16;

      DUMPBITS ( t^.b );

      n := t^.v_n;
      IF e <> 0 THEN BEGIN
        NEEDBITS ( 8 );
        inc ( n, byte ( b ) AND $ff );
        DUMPBITS ( 8 );
      END;
      dec ( s, n );
      REPEAT
        d := d AND pred ( WSIZE );
        IF d > w THEN e := WSIZE - d ELSE e := WSIZE - w;
        IF e > n THEN e := n;
        dec ( n, e );
        IF ( u <> 0 ) AND ( w <= d ) THEN BEGIN
          fillchar ( slide [ w ], e, #0 );
          inc ( w, e );
          inc ( d, e );
        END ELSE IF ( w - d >= e ) THEN BEGIN
          move ( slide [ d ], slide [ w ], e );
          inc ( w, e );
          inc ( d, e );
        END ELSE REPEAT
          slide [ w ] := slide [ d ];
          inc ( w );
          inc ( d );
          dec ( e );
        UNTIL e = 0;
        IF w = WSIZE THEN BEGIN
          IF NOT flush ( w ) THEN BEGIN
            explode_lit8 := unzip_WriteErr;
            exit
          END;
          w := 0; u := 0;
        END;
      UNTIL n = 0;
    END;
  END;
  IF totalabort THEN explode_lit8 := unzip_userabort
  ELSE
    IF NOT flush ( w ) THEN explode_lit8 := unzip_WriteErr
  ELSE
    IF zipeof THEN explode_lit8 := unzip_readErr
  ELSE
    explode_lit8 := unzip_Ok;
END;

{******************exploding, method: 4k slide, 3 trees ***********************}

FUNCTION explode_lit4 ( tb, tl, td : phuftlist;bb, bl, bd : integer ) : integer;
VAR s : longint;
    e : word;
    n, d : word;
    w : word;
    t : phuft;
    mb, ml, md : word;
    u : word;

BEGIN
  b := 0; k := 0; w := 0;
  u := 1;
  mb := mask_bits [ bb ];
  ml := mask_bits [ bl ];
  md := mask_bits [ bd ];
  s := uncompsize;
  WHILE ( s > 0 ) AND NOT ( totalabort OR zipeof ) DO BEGIN
    NEEDBITS ( 1 );
    IF ( b AND 1 ) <> 0 THEN BEGIN  {Litteral}
      DUMPBITS ( 1 );
      dec ( s );
      NEEDBITS ( bb );
      t := @tb^ [  ( NOT b ) AND mb ];
      e := t^.e;
      IF e > 16 THEN REPEAT
        IF e = 99 THEN BEGIN
          explode_lit4 := unzip_ZipFileErr;
          exit
        END;
        DUMPBITS ( t^.b );
        dec ( e, 16 );
        NEEDBITS ( e );
        t := @t^.v_t^ [  ( NOT b ) AND mask_bits [ e ] ];
        e := t^.e;
      UNTIL e <= 16;
      DUMPBITS ( t^.b );
      slide [ w ] := char ( t^.v_n );
      inc ( w );
      IF w = WSIZE THEN BEGIN
        IF NOT flush ( w ) THEN BEGIN
          explode_lit4 := unzip_WriteErr;
          exit
        END;
        w := 0; u := 0;
      END;
    END ELSE BEGIN
      DUMPBITS ( 1 );
      NEEDBITS ( 6 );
      d := b AND $3F;
      DUMPBITS ( 6 );
      NEEDBITS ( bd );
      t := @td^ [  ( NOT b ) AND md ];
      e := t^.e;
      IF e > 16 THEN REPEAT
        IF e = 99 THEN BEGIN
          explode_lit4 := unzip_ZipFileErr;
          exit
        END;
        DUMPBITS ( t^.b );
        dec ( e, 16 );
        NEEDBITS ( e );
        t := @t^.v_t^ [  ( NOT b ) AND mask_bits [ e ] ];
        e := t^.e;
      UNTIL e <= 16;
      DUMPBITS ( t^.b );
      d := w - d - t^.v_n;
      NEEDBITS ( bl );
      t := @tl^ [  ( NOT b ) AND ml ];
      e := t^.e;
      IF e > 16 THEN REPEAT
        IF e = 99 THEN BEGIN
          explode_lit4 := unzip_ZipFileErr;
          exit
        END;
        DUMPBITS ( t^.b );
        dec ( e, 16 );
        NEEDBITS ( e );
        t := @t^.v_t^ [  ( NOT b ) AND mask_bits [ e ] ];
        e := t^.e;
      UNTIL e <= 16;

      DUMPBITS ( t^.b );
      n := t^.v_n;
      IF e <> 0 THEN BEGIN
        NEEDBITS ( 8 );
        inc ( n, b AND $ff );
        DUMPBITS ( 8 );
      END;
      dec ( s, n );
      REPEAT
        d := d AND pred ( WSIZE );
        IF d > w THEN e := WSIZE - d ELSE e := WSIZE - w;
        IF e > n THEN e := n;
        dec ( n, e );
        IF ( u <> 0 ) AND ( w <= d ) THEN BEGIN
          fillchar ( slide [ w ], e, #0 );
          inc ( w, e );
          inc ( d, e );
        END ELSE IF ( w - d >= e ) THEN BEGIN
          move ( slide [ d ], slide [ w ], e );
          inc ( w, e );
          inc ( d, e );
        END ELSE REPEAT
          slide [ w ] := slide [ d ];
          inc ( w );
          inc ( d );
          dec ( e );
        UNTIL e = 0;
        IF w = WSIZE THEN BEGIN
          IF NOT flush ( w ) THEN BEGIN
            explode_lit4 := unzip_WriteErr;
            exit
          END;
          w := 0; u := 0;
        END;
      UNTIL n = 0;
    END;
  END;
  IF totalabort THEN explode_lit4 := unzip_userabort
  ELSE
  IF NOT flush ( w ) THEN explode_lit4 := unzip_WriteErr
  ELSE
    IF zipeof THEN explode_lit4 := unzip_readErr
  ELSE explode_lit4 := unzip_Ok;
END;

{******************exploding, method: 8k slide, 2 trees ***********************}

FUNCTION explode_nolit8 ( tl, td : phuftlist;bl, bd : integer ) : integer;
VAR s : longint;
    e : word;
    n, d : word;
    w : word;
    t : phuft;
    ml, md : word;
    u : word;

BEGIN
  b := 0; k := 0; w := 0;
  u := 1;
  ml := mask_bits [ bl ];
  md := mask_bits [ bd ];
  s := uncompsize;
  WHILE ( s > 0 ) AND NOT ( totalabort OR zipeof ) DO BEGIN
    NEEDBITS ( 1 );
    IF ( b AND 1 ) <> 0 THEN BEGIN  {Litteral}
      DUMPBITS ( 1 );
      dec ( s );
      NEEDBITS ( 8 );
      slide [ w ] := char ( b );
      inc ( w );
      IF w = WSIZE THEN BEGIN
        IF NOT flush ( w ) THEN BEGIN
          explode_nolit8 := unzip_WriteErr;
          exit
        END;
        w := 0; u := 0;
      END;
      DUMPBITS ( 8 );
    END ELSE BEGIN
      DUMPBITS ( 1 );
      NEEDBITS ( 7 );
      d := b AND $7F;
      DUMPBITS ( 7 );
      NEEDBITS ( bd );
      t := @td^ [  ( NOT b ) AND md ];
      e := t^.e;
      IF e > 16 THEN REPEAT
        IF e = 99 THEN BEGIN
          explode_nolit8 := unzip_ZipFileErr;
          exit
        END;
        DUMPBITS ( t^.b );
        dec ( e, 16 );
        NEEDBITS ( e );
        t := @t^.v_t^ [  ( NOT b ) AND mask_bits [ e ] ];
        e := t^.e;
      UNTIL e <= 16;
      DUMPBITS ( t^.b );

      d := w - d - t^.v_n;
      NEEDBITS ( bl );
      t := @tl^ [  ( NOT b ) AND ml ];
      e := t^.e;
      IF e > 16 THEN REPEAT
        IF e = 99 THEN BEGIN
          explode_nolit8 := unzip_ZipFileErr;
          exit
        END;
        DUMPBITS ( t^.b );
        dec ( e, 16 );
        NEEDBITS ( e );
        t := @t^.v_t^ [  ( NOT b ) AND mask_bits [ e ] ];
        e := t^.e;
      UNTIL e <= 16;

      DUMPBITS ( t^.b );

      n := t^.v_n;
      IF e <> 0 THEN BEGIN
        NEEDBITS ( 8 );
        inc ( n, b AND $ff );
        DUMPBITS ( 8 );
      END;
      dec ( s, n );
      REPEAT
        d := d AND pred ( WSIZE );
        IF d > w THEN e := WSIZE - d ELSE e := WSIZE - w;
        IF e > n THEN e := n;
        dec ( n, e );
        IF ( u <> 0 ) AND ( w <= d ) THEN BEGIN
          fillchar ( slide [ w ], e, #0 );
          inc ( w, e );
          inc ( d, e );
        END ELSE IF ( w - d >= e ) THEN BEGIN
          move ( slide [ d ], slide [ w ], e );
          inc ( w, e );
          inc ( d, e );
        END ELSE REPEAT
          slide [ w ] := slide [ d ];
          inc ( w );
          inc ( d );
          dec ( e );
        UNTIL e = 0;
        IF w = WSIZE THEN BEGIN
          IF NOT flush ( w ) THEN BEGIN
            explode_nolit8 := unzip_WriteErr;
            exit
          END;
          w := 0; u := 0;
        END;
      UNTIL n = 0;
    END;
  END;
  IF totalabort THEN explode_nolit8 := unzip_userabort
  ELSE
  IF NOT flush ( w ) THEN explode_nolit8 := unzip_WriteErr
  ELSE
    IF zipeof THEN explode_nolit8 := unzip_readErr
  ELSE explode_nolit8 := unzip_Ok;
END;

{******************exploding, method: 4k slide, 2 trees ***********************}

FUNCTION explode_nolit4 ( tl, td : phuftlist;bl, bd : integer ) : integer;
VAR s : longint;
    e : word;
    n, d : word;
    w : word;
    t : phuft;
    ml, md : word;
    u : word;

BEGIN
  b := 0; k := 0; w := 0;
  u := 1;
  ml := mask_bits [ bl ];
  md := mask_bits [ bd ];
  s := uncompsize;
  WHILE ( s > 0 ) AND NOT ( totalabort OR zipeof ) DO BEGIN
    NEEDBITS ( 1 );
    IF ( b AND 1 ) <> 0 THEN BEGIN  {Litteral}
      DUMPBITS ( 1 );
      dec ( s );
      NEEDBITS ( 8 );
      slide [ w ] := char ( b );
      inc ( w );
      IF w = WSIZE THEN BEGIN
        IF NOT flush ( w ) THEN BEGIN
          explode_nolit4 := unzip_WriteErr;
          exit
        END;
        w := 0; u := 0;
      END;
      DUMPBITS ( 8 );
    END ELSE BEGIN
      DUMPBITS ( 1 );
      NEEDBITS ( 6 );
      d := b AND $3F;
      DUMPBITS ( 6 );
      NEEDBITS ( bd );
      t := @td^ [  ( NOT b ) AND md ];
      e := t^.e;
      IF e > 16 THEN REPEAT
        IF e = 99 THEN BEGIN
          explode_nolit4 := unzip_ZipFileErr;
          exit
        END;
        DUMPBITS ( t^.b );
        dec ( e, 16 );
        NEEDBITS ( e );
        t := @t^.v_t^ [  ( NOT b ) AND mask_bits [ e ] ];
        e := t^.e;
      UNTIL e <= 16;
      DUMPBITS ( t^.b );
      d := w - d - t^.v_n;
      NEEDBITS ( bl );
      t := @tl^ [  ( NOT b ) AND ml ];
      e := t^.e;
      IF e > 16 THEN REPEAT
        IF e = 99 THEN BEGIN
          explode_nolit4 := unzip_ZipFileErr;
          exit
        END;
        DUMPBITS ( t^.b );
        dec ( e, 16 );
        NEEDBITS ( e );
        t := @t^.v_t^ [  ( NOT b ) AND mask_bits [ e ] ];
        e := t^.e;
      UNTIL e <= 16;

      DUMPBITS ( t^.b );
      n := t^.v_n;
      IF e <> 0 THEN BEGIN
        NEEDBITS ( 8 );
        inc ( n, b AND $ff );
        DUMPBITS ( 8 );
      END;
      dec ( s, n );
      REPEAT
        d := d AND pred ( WSIZE );
        IF d > w THEN e := WSIZE - d ELSE e := WSIZE - w;
        IF e > n THEN e := n;
        dec ( n, e );
        IF ( u <> 0 ) AND ( w <= d ) THEN BEGIN
          fillchar ( slide [ w ], e, #0 );
          inc ( w, e );
          inc ( d, e );
        END ELSE IF ( w - d >= e ) THEN BEGIN
          move ( slide [ d ], slide [ w ], e );
          inc ( w, e );
          inc ( d, e );
        END ELSE REPEAT
          slide [ w ] := slide [ d ];
          inc ( w );
          inc ( d );
          dec ( e );
        UNTIL e = 0;
        IF w = WSIZE THEN BEGIN
          IF NOT flush ( w ) THEN BEGIN
            explode_nolit4 := unzip_WriteErr;
            exit
          END;
          w := 0; u := 0;
        END;
      UNTIL n = 0;
    END;
  END;
  IF totalabort THEN explode_nolit4 := unzip_userabort
  ELSE
  IF NOT flush ( w ) THEN explode_nolit4 := unzip_WriteErr
  ELSE
    IF zipeof THEN explode_nolit4 := unzip_readErr
  ELSE explode_nolit4 := unzip_Ok;
END;

{****************************** explode *********************************}

FUNCTION explode : integer;
VAR r : integer;
    tb, tl, td : phuftlist;
    bb, bl, bd : integer;
    l : ARRAY [ 0..255 ] OF word;

BEGIN
  inpos := 0;
  readpos := - 1;  {Nothing read in}
  bl := 7;
  IF compsize > 200000 THEN bd := 8 ELSE bd := 7;
  IF hufttype AND 4 <> 0 THEN BEGIN
    bb := 9;
    r := get_tree ( @l [ 0 ], 256 );
    IF r <> 0 THEN BEGIN
      explode := unzip_ZipFileErr;
      exit
    END;
    r := huft_build ( pword16 ( @l ), 256, 256, NIL, NIL, pphuft ( @tb ), bb );
    IF r <> 0 THEN BEGIN
      IF r = huft_incomplete THEN huft_free ( tb );
      explode := unzip_ZipFileErr;
      exit
    END;
    r := get_tree ( @l [ 0 ], 64 );
    IF r <> 0 THEN BEGIN
      huft_free ( tb );
      explode := unzip_ZipFileErr;
      exit
    END;
    r := huft_build ( pword16 ( @l ), 64, 0, pushlist ( @cplen3 ), pushlist ( @extra ), pphuft ( @tl ), bl );
    IF r <> 0 THEN BEGIN
      IF r = huft_incomplete THEN huft_free ( tl );
      huft_free ( tb );
      explode := unzip_ZipFileErr;
      exit
    END;
    r := get_tree ( @l [ 0 ], 64 );
    IF r <> 0 THEN BEGIN
      huft_free ( tb );
      huft_free ( tl );
      explode := unzip_ZipFileErr;
      exit
    END;
    IF hufttype AND 2 <> 0 THEN BEGIN {8k}
      r := huft_build ( pword16 ( @l ), 64, 0, pushlist ( @cpdist8 ), pushlist ( @extra ), pphuft ( @td ), bd );
      IF r <> 0 THEN BEGIN
        IF r = huft_incomplete THEN huft_free ( td );
        huft_free ( tb );
        huft_free ( tl );
        explode := unzip_ZipFileErr;
        exit
      END;
      r := explode_lit8 ( tb, tl, td, bb, bl, bd );
    END ELSE BEGIN
      r := huft_build ( pword16 ( @l ), 64, 0, pushlist ( @cpdist4 ), pushlist ( @extra ), pphuft ( @td ), bd );
      IF r <> 0 THEN BEGIN
        IF r = huft_incomplete THEN huft_free ( td );
        huft_free ( tb );
        huft_free ( tl );
        explode := unzip_ZipFileErr;
        exit
      END;
      r := explode_lit4 ( tb, tl, td, bb, bl, bd );
    END;
    huft_free ( td );
    huft_free ( tl );
    huft_free ( tb );
  END ELSE BEGIN       {No literal tree}
    r := get_tree ( @l [ 0 ], 64 );
    IF r <> 0 THEN BEGIN
      explode := unzip_ZipFileErr;
      exit
    END;
    r := huft_build ( pword16 ( @l ), 64, 0, pushlist ( @cplen2 ), pushlist ( @extra ), pphuft ( @tl ), bl );
    IF r <> 0 THEN BEGIN
      IF r = huft_incomplete THEN huft_free ( tl );
      explode := unzip_ZipFileErr;
      exit
    END;

    r := get_tree ( @l [ 0 ], 64 );
    IF r <> 0 THEN BEGIN
      huft_free ( tl );
      explode := unzip_ZipFileErr;
      exit
    END;
    IF hufttype AND 2 <> 0 THEN BEGIN {8k}
      r := huft_build ( pword16 ( @l ), 64, 0, pushlist ( @cpdist8 ), pushlist ( @extra ), pphuft ( @td ), bd );
      IF r <> 0 THEN BEGIN
        IF r = huft_incomplete THEN huft_free ( td );
        huft_free ( tl );
        explode := unzip_ZipFileErr;
        exit
      END;
      r := explode_nolit8 ( tl, td, bl, bd );
    END ELSE BEGIN
      r := huft_build ( pword16 ( @l ), 64, 0, pushlist ( @cpdist4 ), pushlist ( @extra ), pphuft ( @td ), bd );
      IF r <> 0 THEN BEGIN
        IF r = huft_incomplete THEN huft_free ( td );
        huft_free ( tl );
        explode := unzip_ZipFileErr;
        exit
      END;
      r := explode_nolit4 ( tl, td, bl, bd );
    END;
    huft_free ( td );
    huft_free ( tl );
  END;
  explode := r;
END;
(***************************************************************************)
{.$I z_shrunk.pas}  {Unshrink function}
{*************************** unshrink **********************************}
{Written and NOT copyrighted by Christian Ghisler.
 I have rewritten unshrink because the original
 function was copyrighted by Mr. Smith of Info-zip
 This funtion here is now completely FREE!!!!
 The only right I claim on this code is that
 noone else claims a copyright on it!}


CONST max_code = 8192;
      max_stack = 8192;
      initial_code_size = 9;
      final_code_size = 13;
      write_max = wsize - 3 * ( max_code - 256 ) - max_stack - 2;  {Rest of slide=write buffer}
                                                     {=766 bytes}

TYPE prev = ARRAY [ 257..max_code ] OF integer;
     pprev = ^prev;
     cds = ARRAY [ 257..max_code ] OF char;
     pcds = ^cds;
     stacktype = ARRAY [ 0..max_stack ] OF char;
     pstacktype = ^stacktype;
     writebuftype = ARRAY [ 0..write_max ] OF char;   {write buffer}
     pwritebuftype = ^writebuftype;

VAR previous_code : pprev;       {previous code trie}
    actual_code : pcds;          {actual code trie}
    stack : pstacktype;          {Stack for output}
    writebuf : pwritebuftype;    {Write buffer}
    next_free,                 {Next free code in trie}
    write_ptr : integer;         {Pointer to output buffer}

FUNCTION unshrink_flush : boolean;
VAR
n : nword;
b : boolean;
BEGIN
  {$I-}
  blockwrite ( outfile, writebuf^ [ 0 ], write_ptr, n );
  {$I+}
  b := ( n = write_ptr ) AND ( ioresult = 0 );  {True-> alles ok}
  UpdateCRC ( iobuf ( piobuf ( @writebuf^ [ 0 ] ) ^ ), write_ptr );
  {--}
  IF ( b = TRUE ) AND ( @ZipReport <> NIL )  {callback report for high level functions}
  THEN BEGIN
      WITH ZipRec DO BEGIN
           Status := file_unzipping;
           ZipReport{$ifdef __TMT__}^{$endif} ( n, @ZipRec );    {report the actual bytes written}
      END;
  END; {report}
  unshrink_flush := b;
END;

FUNCTION write_char ( c : char ) : boolean;
BEGIN
  writebuf^ [ write_ptr ] := c;
  inc ( write_ptr );
  IF write_ptr > write_max THEN BEGIN
    write_char := unshrink_flush;
    write_ptr := 0;
  END ELSE write_char := TRUE;
END;

PROCEDURE ClearLeafNodes;
VAR pc,                    {previous code}
    i,                     {index}
    act_max_code : integer;  {max code to be searched for leaf nodes}
    previous : pprev;   {previous code trie}

BEGIN
  previous := previous_code;
  act_max_code := next_free - 1;
  FOR i := 257 TO act_max_code DO
    previous^ [ i ] := previous^ [ i ] OR $8000;
  FOR i := 257 TO act_max_code DO BEGIN
    pc := previous^ [ i ] AND NOT $8000;
    IF pc > 256 THEN
      previous^ [ pc ] := previous^ [ pc ] AND ( NOT $8000 );
  END;
  {Build new free list}
  pc := - 1;
  next_free := - 1;
  FOR i := 257 TO act_max_code DO
    IF previous^ [ i ] AND $C000 <> 0 THEN BEGIN {Either free before or marked now}
      IF pc <> - 1 THEN previous^ [ pc ] := - i     {Link last item to this item}
                ELSE next_free := i;
      pc := i;
    END;
  IF pc <> - 1 THEN
    previous^ [ pc ] := - act_max_code - 1;
END;

FUNCTION unshrink : integer;

VAR incode : integer;            {code read in}
    lastincode : integer;        {last code read in}
    lastoutcode : char;          {last code emitted}
    code_size : byte;            {Actual code size}
    stack_ptr,                   {Stackpointer}
    new_code,                    {Save new code read}
    code_mask,                   {mask for coding}
    i : integer;                 {Index}
    a1, a2, a3,
    bits_to_read : longint;

BEGIN
  IF compsize = maxlongint THEN BEGIN   {Compressed Size was not in header!}
    unshrink := unzip_NotSupported;
    exit
  END;
  inpos := 0;            {Input buffer position}
  readpos := - 1;         {Nothing read}

  {initialize window, bit buffer}
  w := 0;
  k := 0;
  b := 0;

  {Initialize pointers for various buffers}
  a1 := sizeof ( prev );
  a2 := sizeof ( prev ) + sizeof ( cds );
  a3 := sizeof ( prev ) + sizeof ( cds ) + sizeof ( stacktype );
  previous_code := pprev ( @slide [ 0 ] );
  actual_code := pcds ( @slide [ a1 ] );
  stack := pstacktype ( @slide [ a2 ] );
  writebuf := pwritebuftype ( @slide [ a3 ] );
  fillchar ( slide^, wsize, #0 );

  {initialize free codes list}
  FOR i := 257 TO max_code DO
    previous_code^ [ i ] := - ( i + 1 );
  next_free := 257;
  stack_ptr := max_stack;
  write_ptr := 0;
  code_size := initial_code_size;
  code_mask := mask_bits [ code_size ];

  NEEDBITS ( code_size );
  incode := b AND code_mask;
  DUMPBITS ( code_size );

  lastincode := incode;
  lastoutcode := char ( incode );
  IF NOT write_char ( lastoutcode ) THEN BEGIN
    unshrink := unzip_writeErr;
    exit
  END;

  bits_to_read := 8 * compsize - code_size;   {Bits to be read}

  WHILE NOT totalabort AND ( bits_to_read >= code_size ) DO BEGIN
    NEEDBITS ( code_size );
    incode := b AND code_mask;
    DUMPBITS ( code_size );
    dec ( bits_to_read, code_size );
    IF incode = 256 THEN BEGIN            {Special code}
      NEEDBITS ( code_size );
      incode := b AND code_mask;
      DUMPBITS ( code_size );
      dec ( bits_to_read, code_size );
      CASE incode OF
        1 : BEGIN
          inc ( code_size );
          IF code_size > final_code_size THEN BEGIN
            unshrink := unzip_ZipFileErr;
            exit
          END;
          code_mask := mask_bits [ code_size ];
        END;
        2 : BEGIN
          ClearLeafNodes;
        END;
      ELSE
        unshrink := unzip_ZipFileErr;
        exit
      END;
    END ELSE BEGIN
      new_code := incode;
      IF incode < 256 THEN BEGIN          {Simple char}
        lastoutcode := char ( incode );
        IF NOT write_char ( lastoutcode ) THEN BEGIN
          unshrink := unzip_writeErr;
          exit
        END;
      END ELSE BEGIN
        IF previous_code^ [ incode ] < 0 THEN BEGIN
          stack^ [ stack_ptr ] := lastoutcode;
          dec ( stack_ptr );
          incode := lastincode;
        END;
        WHILE incode > 256 DO BEGIN
          stack^ [ stack_ptr ] := actual_code^ [ incode ];
          dec ( stack_ptr );
          incode := previous_code^ [ incode ];
        END;
        lastoutcode := char ( incode );
        IF NOT write_char ( lastoutcode ) THEN BEGIN
          unshrink := unzip_writeErr;
          exit
        END;
        FOR i := stack_ptr + 1 TO max_stack DO
          IF NOT write_char ( stack^ [ i ] ) THEN BEGIN
            unshrink := unzip_writeErr;
            exit
          END;
        stack_ptr := max_stack;
      END;
      incode := next_free;
      IF incode <= max_code THEN BEGIN
        next_free := - previous_code^ [ incode ];   {Next node in free list}
        previous_code^ [ incode ] := lastincode;
        actual_code^ [ incode ] := lastoutcode;
      END;
      lastincode := new_code;
    END;
  END;
  IF totalabort THEN
    unshrink := unzip_UserAbort
  ELSE IF unshrink_flush THEN
    unshrink := unzip_ok
  ELSE
    unshrink := unzip_WriteErr;
END;
(***************************************************************************)
{***************************************************************************}
FUNCTION GetSupportedMethods{ : longint};
BEGIN
  GetSupportedMethods := 1 + ( 1 SHL 1 ) + ( 1 SHL 6 ) + ( 1 SHL 8 );
  {stored, shrunk, imploded and deflated}
END;

{******************** main low level function: unzipfile ********************}
{written and not copyrighted by Christian Ghisler}
FUNCTION unzipfile
{ ( in_name : pchar;
   out_name : pchar;
   offset : longint;
   hFileAction : word;
   cm_index : integer ) : integer};

VAR
   err : integer;
   header : plocalheader;
   buf : TDirType;
{$ifdef __OS_DOS__}
    buf0 : ARRAY [ 0..3 ] OF char;
{$endif}
    timedate : longint;
    originalcrc : longint;    {crc from zip-header}
    ziptype, aResult : integer;
    p, p1 : pchar;
    isadir : boolean;
    Tmp,
    oldcurdir : TString;
BEGIN

  {$ifdef CanUseWinApi}
  FromOemP ( in_name );   { converts filename for disk file }
  FromOemP ( out_name );  { converts filename for disk file }
  {$endif}

  {$ifdef MSWINDOWS}
  IF inuse THEN BEGIN
    {take care of crashed applications!}
    IF  ( lastusedtime <> 0 )
    AND ( abs ( longint ( gettickcount ) - lastusedtime ) > 30000 )
      THEN BEGIN {1/2 minute timeout!!!}
      {do not close files or free slide, they were already freed when application crashed!}
      inuse := FALSE;
      {memory for huffman trees is lost}
    END ELSE BEGIN
      unzipfile := unzip_inuse;
      exit
    END;
  END;{inuse}
  inuse := TRUE;
  {$endif MSWINDOWS}

  Tmp := StrPas ( out_Name );

  getmem ( slide, wsize );
  fillchar ( slide [ 0 ], wsize, #0 );
  assign ( infile, StrPas ( in_name ) );
  err := filemode;
  filemode := FileModeCompat;
  {$I-}
  reset ( infile, 1 );
  filemode := err;
  {$I+}
  IF IOResult <> 0
  THEN BEGIN
    freemem ( slide, wsize );
    unzipfile := unzip_ZipFileOpenError;
    inuse := FALSE;
    exit
  END;
  {$I-}
  seek ( infile, offset );       {seek to header position}
  {$I+}
  IF ioresult <> 0
  THEN BEGIN
    freemem ( slide, wsize );
    close ( infile );
    unzipfile := unzip_ReadErr;
    inuse := FALSE;
    exit
  END;

  header := plocalheader ( @inbuf );

  {$I-}
  blockread ( infile, header^, sizeof ( header^ ) );  {read in local header}
  {$I+}

  IF ioresult <> 0
  THEN BEGIN
    freemem ( slide, wsize );
    close ( infile );
    unzipfile := unzip_ZipFileErr;
    inuse := FALSE;
    exit
  END;

  {$ifdef __endian_neutral_data__}
  LocalHeader2BigEndian ( header^ );
  {$endif}

  IF strlcomp ( header^.signature, 'PK'#3#4, 4 ) <> 0
  THEN BEGIN
    freemem ( slide, wsize );
    close ( infile );
    unzipfile := unzip_ZipFileErr;
    inuse := FALSE;
    exit
  END;

  {calculate offset of data}
  offset := offset + header^.filename_len + header^.extra_field_len + sizeof ( tlocalheader );
  timedate := header^.file_timedate;
  IF ( hufttype AND 8 ) = 0 THEN BEGIN  {Size and crc at the beginning}
    compsize := header^.compress_size;
    uncompsize := header^.uncompress_size;
    originalcrc := header^.crc_32;
  END ELSE BEGIN
  {$ifdef __GPC__}
    compsize := maxint;           {Don't get a sudden zipeof!}
    uncompsize := maxint;
  {$else}
    compsize := maxlongint;           {Don't get a sudden zipeof!}
    uncompsize := maxlongint;
   {$endif}
    originalcrc := 0
  END;

  ziptype := header^.zip_type;     {0=stored, 6=imploded, 8=deflated}

  IF ( ( 1 SHL ziptype ) AND GetSupportedMethods = 0 )
  THEN BEGIN  {Not Supported!!!}
    freemem ( slide, wsize );
    close ( infile );
    unzipfile := unzip_NotSupported;
    inuse := FALSE;
    exit;
  END;

  hufttype := header^.bit_flag;

  IF ( ( hufttype AND 1 ) <> 0 )
  THEN BEGIN {encrypted}
    freemem ( slide, wsize );
    close ( infile );
    unzipfile := unzip_Encrypted;
    inuse := FALSE;
    exit;
  END;

  reachedsize := 0;
  seek ( infile, offset );

  assign ( outfile, Tmp );
  {$I-}
  rewrite ( outfile, 1 );
  {$I+}
  err := ioresult;

  {create directories not yet in path}
  ConvertPath ( out_name );
  isadir := ( out_name [ strlen ( out_name ) - 1 ] = OS_Path_Separator );
  IF ( err <> 0 ) OR ( isadir )
  THEN BEGIN  {path not found}
    {$I-}
    getdir ( 0, oldcurdir );
    {$I+}

   {$ifdef __OS_DOS__}  {@@ remove when GPC's Getdir is fixed }
    IF  ( oldcurdir [length ( oldcurdir ) ] = OS_Path_Separator )
    AND ( length ( oldcurdir ) > 3 )
    THEN Delete ( oldcurdir, length ( oldcurdir ), 1 );
   {$endif}  {@@}

    IF ioresult <> 0 THEN;
    strcopy ( buf, out_name );
    p1 := strrscan ( buf, OS_Path_Separator );
    IF p1 <> NIL THEN inc ( p1 );  {pointer to filename}
    p := strtok ( buf, OS_Path_Separator );

    {$ifdef __OS_DOS__}
    IF ( p <> NIL ) AND ( p [ 1 ] = ':' )
    THEN BEGIN
      strcopy ( buf0, 'c:\' );    {set drive}
      buf0 [ 0 ] := p [ 0 ];
      {$I-}
      chdir ( strpas ( buf0 ) );
      {$I+}
      IF ioresult <> 0 THEN;
      p := strtok ( NIL, OS_Path_Separator );
    END;
    {$endif}

    WHILE ( p <> NIL ) AND ( p <> p1 )
    DO BEGIN
      Tmp := Strpas ( p );
      {$I-}
      chdir ( Tmp );
      {$I+}
      err := ioresult;
      IF err <> 0
      THEN BEGIN
        {$I-}
        mkdir ( Tmp );
        {$I+}
        err := ioresult;
        IF err = 0
        THEN BEGIN
         {$I-}
         chdir ( Tmp );
         {$I+}
         err := ioresult;
        END;
      END;

      IF err = 0 THEN
        p := strtok ( NIL, OS_Path_Separator )
      ELSE
        p := NIL;
    END;

    {$I-}
    chdir ( oldcurdir );
    {$I+}
    IF ioresult <> 0 THEN;

    IF isadir
    THEN BEGIN
      freemem ( slide, wsize );
      unzipfile := unzip_Ok;    {A directory -> ok}
      close ( infile );
      inuse := FALSE;
      exit;
    END;

    {$I-}
    rewrite ( outfile, 1 );
    {$I+}
    err := ioresult;
  END;

  IF err <> 0
  THEN BEGIN
 { can't create the file }
    Tmp := ExtractFileDir ( StrPas ( out_name ) );
    IF ( Tmp <> '' ) AND ( NOT DirectoryExists ( Tmp ) )
    THEN BEGIN
       Tmp := Tmp + #0;
       err := RecursiveMkDir ( @Tmp [1] );
       IF err = 0 THEN BEGIN
         {$I-}rewrite ( outfile, 1 );{$I+}
         err := ioresult;
       END;
   END;

   IF err <> 0
   THEN BEGIN
   { we can't create file, or directory }
      freemem ( slide, wsize );
      unzipfile := unzip_WriteErr;
      close ( infile );
      inuse := FALSE;
      exit
   END;
  END;

  totalabort := FALSE;
  zipeof := FALSE;
  dlghandle := hFileAction;
  dlgnotify := cm_index;

  {$ifdef MSWINDOWS}
  messageloop;
  oldpercent := 0;
  {$endif}

  InitCrc32 ( crc32val );

  {Unzip correct type}
  CASE ziptype OF
    0 : aResult := copystored;
    1 : aResult := unshrink;
    6 : aResult := explode;
    8 : aResult := inflate;
  ELSE BEGIN
    aResult := unzip_NotSupported;
   END;
  END;
  unzipfile := aResult;

  IF ( aResult = unzip_ok ) AND ( ( hufttype AND 8 ) <> 0 )
  THEN BEGIN {CRC at the end}
    dumpbits ( k AND 7 );
    needbits ( 16 );
    {originalcrc := b AND $FFFF;}
    dumpbits ( 16 );
    needbits ( 16 );
    originalcrc := ( b AND $FFFF ) SHL 16;
    dumpbits ( 16 );
  END;
  {$i-}
  close ( infile ); IF ioresult <> 0 THEN;
  close ( outfile ); IF ioresult <> 0 THEN;
  {$i+}

  crc32val := FinalCrc32 ( crc32val );

  IF aResult <> 0
  THEN BEGIN
    {$i-}erase ( outfile ); {$i+}
    IF ioresult <> 0 THEN;
  END
  ELSE IF ( originalcrc <> crc32val )
  THEN BEGIN
    unzipfile := unzip_CRCErr;
    {$i-}erase ( outfile ); {$i+}
    IF ioresult <> 0 THEN;
  END ELSE BEGIN
    oldpercent := 100;       {100 percent}
    {$ifdef MSWINDOWS}
    IF dlghandle <> 0 THEN
      sendmessage ( dlghandle, wm_command, dlgnotify, longint ( @oldpercent ) );
    {$endif}
    reset ( outfile {$ifdef __GPC__}, 1{$endif} );
    {$ifdef __GPC__} {@@}
    setftime ( GPC_AnyFile ( outfile ), timedate ); {set zipped time and date of oufile}
    {$else}
    setftime ( outfile, timedate ); {set zipped time and date of oufile}
    {$endif}
    close ( outfile ); IF ioresult <> 0 THEN;
  END;

  freemem ( slide, wsize );
  inuse := FALSE;
END; { unzipfile }
{***************************************************************************}
{***************************************************************************}
{***************************************************************************}
{ other functions; zipread.pas }
CONST mainheader : pchar = 'PK'#5#6;
      maxbufsize = TFileBufferSize; { Can be as low as 500 Bytes; however, }
                                    { this would lead to extensive disk reading!}
                                    { If one entry (including Extra field) is bigger}
                                    { than maxbufsize, you cannot read it :-( }

{$ifdef __GPC__}{$pack-struct, maximum-field-alignment 8}{$endif}
TYPE
  pmainheader = ^tmainheader;
  tmainheader = {$ifndef __GPC__}PACKED {$endif}RECORD     { !!! }
    signature : TSignatureType;  {'PK'#5#6}
    thisdisk,
    centralstartdisk,
    entries_this_disk,
    entries_central_dir : word16;
    headsize,
    headstart,
    comment_len : Int32;
    unknown : word16;
  END;

  pheader = ^theader;
  theader = {$ifndef __GPC__}PACKED {$endif}RECORD  {* must be packed *}
    signature : TSignatureType;  { 'PK'#1#2 }
    OSversion,        { Operating system version }
    OSmadeby : byte;  { MSDOS (FAT): 0 }
    extract_ver,
    bit_flag,
    zip_type : word16;
    file_timedate,
    crc_32,
    compress_size,
    uncompress_size : Int32;
    filename_len,
    extra_field_len,
    file_comment_len,
    disk_number_start,
    internal_attr : word16;
    external_attr : ARRAY [ 0..3 ] OF byte;
    offset_local_header : Int32;
  END;
{$ifdef __GPC__}{$no-pack-struct, maximum-field-alignment 0}{$endif}

{$ifdef __endian_neutral_data__}
PROCEDURE MainHeader2BigEndian ( VAR aHead : tmainheader ) ;
{ convert mainheader variable to big endian format }

{$ifdef __BYTES_BIG_ENDIAN__}
TYPE
  mtmainheader = RECORD
    signature : TSignatureType;
    thisdisk,
    centralstartdisk,
    entries_this_disk,
    entries_central_dir : word16;
    headsize,
    headstart,
    comment_len : Int32;
    unknown : word16;
  END;

 VAR
 Head : mtmainheader;
{$endif}

BEGIN
  {$ifdef __BYTES_BIG_ENDIAN__}
  WITH Head DO BEGIN
    thisdisk := aHead.thisdisk;
    centralstartdisk := aHead.centralstartdisk;
    entries_this_disk := aHead.entries_this_disk;
    entries_central_dir := aHead.entries_central_dir;
    headsize := aHead.headsize;
    headstart := aHead.headstart;
    comment_len := aHead.comment_len;
    unknown := aHead.unknown;

    QuickEndianConversion ( thisdisk, Sizeof ( thisdisk ) );
    QuickEndianConversion ( centralstartdisk, Sizeof ( centralstartdisk ) );
    QuickEndianConversion ( entries_this_disk, Sizeof ( entries_this_disk ) );
    QuickEndianConversion ( entries_central_dir, Sizeof ( entries_central_dir ) );
    QuickEndianConversion ( headsize, Sizeof ( headsize ) );
    QuickEndianConversion ( headstart, Sizeof ( headstart ) );
    QuickEndianConversion ( comment_len, Sizeof ( comment_len ) );
    QuickEndianConversion ( unknown, Sizeof ( unknown ) );
  END;

  WITH aHead DO BEGIN
    thisdisk := Head.thisdisk;
    centralstartdisk := Head.centralstartdisk;
    entries_this_disk := Head.entries_this_disk;
    entries_central_dir := Head.entries_central_dir;
    headsize := Head.headsize;
    headstart := Head.headstart;
    comment_len := Head.comment_len;
    unknown := Head.unknown;
  END;
  {$endif}
END;

PROCEDURE THeader2BigEndian ( VAR aHead : theader ) ;
{ convert theader variable to big endian format }

{$ifdef __BYTES_BIG_ENDIAN__}
TYPE
 mtheader = RECORD
    signature : TSignatureType;  { 'PK'#1#2 }
    OSversion,        { Operating system version }
    OSmadeby : byte;  { MSDOS (FAT): 0 }
    extract_ver,
    bit_flag,
    zip_type : word16;
    file_timedate,
    crc_32,
    compress_size,
    uncompress_size : Int32;
    filename_len,
    extra_field_len,
    file_comment_len,
    disk_number_start,
    internal_attr : word16;
    external_attr : ARRAY [ 0..3 ] OF byte;
    offset_local_header : Int32;
  END;
VAR
Head : mtheader;
{$endif}
BEGIN
  {$ifdef __BYTES_BIG_ENDIAN__}
  WITH Head DO BEGIN
    extract_ver := aHead.extract_ver;
    bit_flag := aHead.bit_flag;
    zip_type := aHead.zip_type;
    file_timedate := aHead.file_timedate;
    crc_32 := aHead.crc_32;
    compress_size := aHead.compress_size;
    uncompress_size := aHead.uncompress_size;
    filename_len := aHead.filename_len;
    extra_field_len := aHead.extra_field_len;
    file_comment_len := aHead.file_comment_len;
    disk_number_start := aHead.disk_number_start;
    internal_attr := aHead.internal_attr;
    offset_local_header := aHead.offset_local_header;

    QuickEndianConversion ( extract_ver, Sizeof ( extract_ver ) );
    QuickEndianConversion ( bit_flag, Sizeof ( bit_flag ) );
    QuickEndianConversion ( zip_type, Sizeof ( zip_type ) );
    QuickEndianConversion ( file_timedate, Sizeof ( file_timedate ) );
    QuickEndianConversion ( crc_32, Sizeof ( crc_32 ) );
    QuickEndianConversion ( compress_size, Sizeof ( compress_size ) );
    QuickEndianConversion ( uncompress_size, Sizeof ( uncompress_size ) );
    QuickEndianConversion ( filename_len, Sizeof ( filename_len ) );
    QuickEndianConversion ( extra_field_len, Sizeof ( extra_field_len ) );
    QuickEndianConversion ( file_comment_len, Sizeof ( file_comment_len ) );
    QuickEndianConversion ( disk_number_start, Sizeof ( disk_number_start ) );
    QuickEndianConversion ( internal_attr, Sizeof ( internal_attr ) );
    QuickEndianConversion ( offset_local_header, Sizeof ( offset_local_header ) );
  END;

  WITH aHead DO BEGIN
    extract_ver := Head.extract_ver;
    bit_flag := Head.bit_flag;
    zip_type := Head.zip_type;
    file_timedate := Head.file_timedate;
    crc_32 := Head.crc_32;
    compress_size := Head.compress_size;
    uncompress_size := Head.uncompress_size;
    filename_len := Head.filename_len;
    extra_field_len := Head.extra_field_len;
    file_comment_len := Head.file_comment_len;
    disk_number_start := Head.disk_number_start;
    internal_attr := Head.internal_attr;
    offset_local_header := Head.offset_local_header;
  END;
  {$endif}
END;
{$endif}


PROCEDURE Fill_Array ( VAR Dest : TDirType; Src : TDirType );
BEGIN
  {IF LowcaseFileNames THEN StrLower ( Src );}
  Strcopy ( Dest, Src );
END;

{*********** Fill out tZipRec structure with next entry *************}
FUNCTION FillOutRec ( VAR zprec : tZipRec ) : integer;
VAR
    incr   : longint;
    header : pheader;
    offs   : nword;
    old    : char;
    err    : nword;
    {$ifdef __GPC__}
    p : TString;
    {$endif}
BEGIN
 WITH zprec
 DO BEGIN
  header := pheader ( @buf^ [ localstart ] );

  {$ifdef __endian_neutral_data__}
  THeader2BigEndian ( header^ );
  {$endif}

  IF ( bufsize >= maxbufsize )
  THEN BEGIN       {Caution: header bigger than buffer!}
    IF ( ( localstart + sizeof ( theader ) ) > bufsize )
    OR ( localstart + header^.filename_len + header^.extra_field_len +
         header^.file_comment_len + sizeof ( theader ) > bufsize )
     {
    IF localstart + header^.filename_len + header^.extra_field_len +
      header^.file_comment_len + sizeof ( theader ) > bufsize
    }
      THEN BEGIN     {Read over end of header}
        move ( buf^ [ localstart ], buf^ [ 0 ], bufsize - localstart );  {Move end to beginning in buffer}

        {$I-}
        {Read in full central dir, up to maxbufsize Bytes}
        blockread ( DirFile, buf^ [ bufsize - localstart ], localstart, err );
        {$I+}

        IF ( ioresult <> 0 ) OR ( err + localstart < sizeof ( theader ) )
        THEN BEGIN
          filloutrec := unzip_nomoreitems;
          exit
        END;
        localstart := 0;
        header := pheader ( @buf^ [ localstart ] );

        {$ifdef __endian_neutral_data__}
        THeader2BigEndian ( header^ );
        {$endif}

      END;
  END;

  IF ( localstart + 4 <= bufsize ) AND   {Here is the ONLY correct finish!}
    ( strlcomp ( header^.signature, mainheader, 4 ) = 0 )
  THEN BEGIN  {Main header}
    filloutrec := unzip_nomoreitems;
    exit
  END;

  IF ( localstart + sizeof ( header ) > bufsize ) OR
    ( localstart + header^.filename_len + header^.extra_field_len +
    header^.file_comment_len + sizeof ( theader ) > bufsize ) OR
    ( strlcomp ( header^.signature, 'PK'#1#2, 4 ) <> 0 ) THEN BEGIN
    filloutrec := unzip_nomoreitems;
    exit
  END;

  size := header^.uncompress_size;
  compressSize := header^.compress_size;

  IF header^.osmadeby = 0 THEN
    attr := header^.external_attr [ 0 ]
  ELSE
    attr := 0;

  time := header^.file_timedate;
  headeroffset := header^.offset_local_header; {Other header size}
  Packmethod := header^.zip_type;
  offs := localstart + header^.filename_len + sizeof ( header^ );
  old := buf^ [ offs ];
  buf^ [ offs ] := #0;  {Repair signature of next block!}
  {$ifdef __GPC__}
  p := strpas ( pchar ( @buf^ [ localstart + sizeof ( header^ ) ] ) );
  Move ( p [1], Filename [0], length ( p ) );
  filename [length ( p ) ] := #0;
  {$else}
  strlcopy ( filename, pchar ( @buf^ [ localstart + sizeof ( header^ ) ] ), sizeof ( filename ) - 1 );
  {$endif}
  buf^ [ offs ] := old;
  ConvertPath ( filename );
  incr := header^.filename_len + header^.extra_field_len +
          header^.file_comment_len + sizeof ( header^ );
  IF incr <= 0 THEN BEGIN
    filloutrec := unzip_InternalError;
    exit
  END;
  localstart := localstart + incr;
  filloutrec := unzip_ok;
 END;
END; { filloutRec }


{**************** Get first entry from ZIP file ********************}
FUNCTION GetFirstInZip
{ ( zipfilename : pchar; VAR zprec : tZipRec ) : integer};
VAR
    bufstart, headerstart, start : longint;
    err, i : nWord;
    mainh  : tmainheader;

BEGIN
 WITH zprec
 DO BEGIN
  Buf := NIL;
  assign ( DirFile, FromOEM ( StrPas ( zipfilename ) ) );
  err := filemode;
  filemode := FileModeCompat;
  {$I-}
  reset ( DirFile, 1 );
  filemode := err;
  {$I+}
  IF ioresult <> 0 THEN BEGIN
    GetFirstInZip := unzip_FileError;
    exit
  END;

  size := filesize ( DirFile );

  IF size = 0 THEN BEGIN
    GetFirstInZip := unzip_FileError;
    {$I-}
    close ( DirFile );
    {$I+}
    exit
  END;

  bufsize := 4096;     {in 4k-blocks}
  IF size > bufsize
  THEN BEGIN
    bufstart := size - bufsize;
  END
  ELSE BEGIN
    bufstart := 0;
    bufsize := size;
  END;

  getmem ( buf, bufsize + 1 );     {#0 at the end of filename}

  {Search from back of file to central directory start}
  start := - 1;    {Nothing found}
  REPEAT
    {$I-}
    seek ( DirFile, bufstart );
    {$I+}
    IF ioresult <> 0 THEN BEGIN
      GetFirstInZip := unzip_FileError;
      freeMem ( buf, bufsize + 1 );
      buf := NIL;
      {$I-}
      close ( DirFile );
      {$I+}
      exit
    END;

    {$I-}
    blockread ( DirFile, buf^, bufsize, err );
    {$I+}

   IF ( ioresult <> 0 ) OR ( err <> bufsize )
    THEN BEGIN
      GetFirstInZip := unzip_FileError;
      freeMem ( buf, bufsize + 1 );
      buf := NIL;
      {$I-}
      close ( DirFile );
      {$I+}
      exit
    END;

    IF bufstart = 0
     THEN start := {$ifdef __GPC__}maxint{$else}maxlongint{$endif};{Break}

    FOR i := bufsize - 22 DOWNTO 0
    DO BEGIN  {Search buffer backwards}
      IF ( ( buf^ [ i ] = 'P' )
       AND ( buf^ [ i + 1 ] = 'K' )
       AND ( buf^ [ i + 2 ] = #5 )
       AND ( buf^ [ i + 3 ] = #6 ) )
       THEN BEGIN                         {Header found!!!}
        start := bufstart + i;
        break;
       END;
    END;

    IF start = - 1
    THEN BEGIN               {Nothing found yet}
      dec ( bufstart, bufsize - 22 );       {Full header in buffer!}
      IF bufstart < 0 THEN bufstart := 0;
    END;

  UNTIL start >= 0;

  IF ( start = {$ifdef __GPC__}maxint{$else}maxlongint{$endif} )
  THEN BEGIN       {Nothing found}
    GetFirstInZip := unzip_FileError;
    freeMem ( buf, bufsize + 1 );
    buf := NIL;
    {$I-}
    close ( DirFile );
    {$I+}
    exit
  END;

  {mainh := pmainheader ( @buf^ [ start - bufstart ] ); }
  Move ( buf^ [ start - bufstart ], mainh,
        SizeOf ( mainh ) {$ifdef BP_DPMI} - 4 {$endif} );

  {$ifdef __endian_neutral_data__}
  MainHeader2BigEndian ( mainh );
  {$endif}

  headerstart := mainh.headstart;
  localstart := 0;
  freeMem ( buf, bufsize + 1 );
  IF ( localstart + sizeof ( theader ) > start )
  THEN BEGIN
    buf := NIL;
    GetFirstInZip := unzip_InternalError;
    {$I-}
    close ( DirFile );
    {$I+}
    exit
  END;

  bufstart := headerstart;
  start := start - headerstart + 4; {size for central dir,Including main header signature}

  IF start >= maxbufsize  { Max buffer size, limit of around 1000 items (for 16-bit)}
  THEN bufsize := maxbufsize
  ELSE bufsize := start;

  getmem ( buf, bufsize + 1 );
  {$I-}
  seek ( DirFile, bufstart );
  {$I+}
  IF ioresult <> 0
  THEN BEGIN
    GetFirstInZip := unzip_FileError;
    freeMem ( buf, bufsize + 1 );
    buf := NIL;
    {$I-}
    close ( DirFile );
    {$I+}
    exit
  END;

  {$I-}
  blockread ( DirFile, buf^, bufsize, err ); {Read in full central dir, up to maxbufsize Bytes}
  {$I+}

  IF ioresult <> 0
  THEN BEGIN
    GetFirstInZip := unzip_FileError;
    freeMem ( buf, bufsize + 1 );
    buf := NIL;
    {$I-}
    close ( DirFile );
    {$I+}
    exit
  END;

  err := filloutRec ( zprec );
  IF err <> unzip_ok
  THEN BEGIN
    CloseZipFile ( zprec );
    GetFirstInZip := err;
    exit
  END;
  GetFirstInZip := err;
 END;
END; { GetFirstInZip }

{**************** Get next entry from ZIP file ********************}
FUNCTION GetNextInZip {( VAR Zprec : tZiprec ) : integer};
VAR err : integer;
BEGIN
 WITH zprec
 DO BEGIN
  IF ( buf <> NIL )
  THEN BEGIN  {Main Header at the end}
    err := filloutRec ( zprec );
    IF err <> unzip_ok
    THEN BEGIN
      CloseZipFile ( ZPRec );
    END;
    GetNextInZip := err;
  END ELSE GetNextInZip := unzip_NoMoreItems;
 END
END; { GetNextInZip }

{**************** free ZIP buffers ********************}
PROCEDURE CloseZipFile {( VAR Zprec : tZiprec )};
 {Only free buffer, file only open in Getfirstinzip}
BEGIN
 WITH zprec
 DO BEGIN
  IF buf <> NIL
  THEN BEGIN
    {$I-}
    close ( DirFile ); IF Ioresult <> 0 THEN;
    {$I+}
    freemem ( buf, bufsize + 1 );     {#0 at the end of filename}
    buf := NIL
  END;
 END;
END; { CloseZipFile }

{**************** test for zip file ********************}
FUNCTION isZip
{ ( filename : pchar; pStartOffSet : pLongint ) : boolean};
VAR
    myname : tdirtype;
    l, err : integer;
    f : FILE;
    buf : ARRAY [ 0..4 ] OF char;
    oldcurdir : TString;
    j : longint;
BEGIN
    IF Assigned ( pStartOffSet ) THEN pStartOffSet^ := - 1;
    {$i-}getdir ( 0, oldcurdir );{$i+}
    err := ioresult;
    isZip := FALSE;
    strcopy ( myname, filename );
    ConvertPath ( myname );
    l := strlen ( myname );
    IF myname [ l - 1 ] = OS_Path_Separator THEN myname [ l - 1 ] := #0;
    {$I-}
    chdir ( Strpas ( myname ) );
    {$I+}
    IF ioresult <> 0
    THEN BEGIN
      assign ( f, FromOEM ( Strpas ( myname ) ) );
      l := filemode;
      filemode := FileModeCompat;
      {$I-}
      reset ( f, 1 );
      filemode := l;
      {$I+}
      IF ioresult <> 0
      THEN BEGIN
        {$I-} chdir ( oldcurdir ); {$I+} err := ioresult;
        Exit;
      END;

      {$I-} blockread ( f, buf, 4, err ); {$I+}
      IF ( ioresult = 0 )
      THEN BEGIN
         {$I-}
         close ( f ); IF ioresult <> 0 THEN;
         chdir ( oldcurdir ); IF ioresult <> 0 THEN;
         {$I+}
         IF ( err = 4 ) AND ( buf [ 0 ] = 'P' ) AND ( buf [ 1 ] = 'K' )
         AND ( buf [ 2 ] = #3 ) AND ( buf [ 3 ] = #4 )
         THEN BEGIN
            isZip := TRUE;
            IF Assigned ( pStartOffSet ) THEN pStartOffSet^ := 0;
         END
         ELSE BEGIN
            j := GetHeaderOffSet ( filename, NIL );
            IsZip := j > 0;
            IF Assigned ( pStartOffSet ) THEN pStartOffSet^ := j;
         END;
         Exit;
      END;
      {$I-} close ( f ); err := ioresult;
      chdir ( oldcurdir ); {$I+} err := ioresult;
    END;
END; { IsZip }

{***************************************************************************}
{***************************************************************************}
{********** routines by the African Chief **********************************}
{***************************************************************************}
{***************************************************************************}
FUNCTION GetHeaderOffset
 {( FileName : pChar; pEndoffSet : pLongint ) : Longint};

CONST
maxbuf = {$ifdef ver70}63{$else}256{$endif} * 1024;

TYPE
buft = ARRAY [0..maxbuf] OF char;

VAR
buffer : ^buft;
f : FILE;
j, k, l : longint;
i : nWord;
oldmode : integer;

BEGIN
  GetHeaderOffSet := - 1;
  IF Assigned ( pEndoffSet ) THEN pEndoffSet^ := - 1;
  IF ( NOT Assigned ( FileName ) ) OR ( StrLen ( FileName ) = 0 ) THEN exit;
  assign ( f, FromOEM ( StrPas ( FileName ) ) );
  oldmode := filemode;
  filemode := FileModeCompat;
  {$i-}
  reset ( f, 1 );
  {$i+}
  filemode := oldmode;
  IF ioresult <> 0 THEN Exit;

  {$i-}
  l := filesize ( f ); IF ioresult <> 0 THEN;
  IF Assigned ( pEndoffSet ) THEN pEndoffSet^ := l;
  IF l < 5  { can't be zip file }
  THEN BEGIN
     close ( f ); IF ioresult <> 0 THEN;
   {$i+}
     Exit;
  END
  ELSE IF l >= maxbuf THEN l := pred ( maxbuf );

  dec ( l, l div 4 );
  getmem ( buffer, l );
  blockread ( f, buffer^, l, i ); IF ioresult <> 0 THEN;
  close ( f ); IF ioresult <> 0 THEN;
  {$i+}
  k := - 1;
  IF  ( buffer^ [0] = 'P' ) AND ( buffer^ [1] = 'K' )
  AND ( buffer^ [2] = #3 )  AND ( buffer^ [3] = #4 )
  THEN BEGIN
     k := 0;
     IF Assigned ( pEndoffSet ) THEN pEndoffSet^ := 0;
  END
  ELSE BEGIN
     FOR j := 0 TO ( i - 5 ) DO
      IF  ( buffer^ [j]   = 'P' )
      AND ( buffer^ [j + 1] = 'K' )
      AND ( buffer^ [j + 2] = #3 )
      AND ( buffer^ [j + 3] = #4 )
      THEN BEGIN
         k := j;
         break;
      END;
  END;
  IF k < 0 THEN IF Assigned ( pEndoffSet ) THEN pEndoffSet^ := - 1;
  GetHeaderOffSet := k;
  freemem ( buffer, l );
END;


FUNCTION Matches ( CONST FileSpec, StringToSearch : String ) : Boolean;
{ does filename: StringtoSearch match the file specification: FileSpec? }
VAR
pStringToSearch,
pFileSpec : pChar;
b1 : boolean;

FUNCTION MatchPattern ( Element, FileSpec : PChar ) : Boolean;

FUNCTION IsPatternWild ( FileSpec : PChar ) : Boolean;
VAR
  b : Boolean;
BEGIN
  b := StrScan ( FileSpec, '*' ) <> NIL;
  IF NOT b THEN b := StrScan ( FileSpec, '?' ) <> NIL;
  IsPatternWild := b;
END;

BEGIN
  b1 := False;
  IF 0 = StrComp ( FileSpec, '*' ) THEN
    b1 := True
  ELSE IF ( element^ = Chr ( 0 ) ) AND ( FileSpec^ <> Chr ( 0 ) ) THEN
    b1 := False
  ELSE IF element^ = Chr ( 0 ) THEN
    b1 := True
  ELSE BEGIN
    CASE FileSpec^ OF
    '*' : IF MatchPattern ( element, @FileSpec [1] ) THEN
           b1 := True
         ELSE
           b1 := MatchPattern ( @element [1], FileSpec );
    '?' : b1 := MatchPattern ( @element [1], @FileSpec [1] );

    ELSE
      IF element^ = FileSpec^ THEN
        b1 := MatchPattern ( @element [1], @FileSpec [1] )
      ELSE
        b1 := False;
    END;  { case }
  END; { else begin }
  MatchPattern := b1;
END; { MatchPattern }

CONST StrMaxLen = {$ifdef __GPC__} 512 {$else} 255 {$endif};

BEGIN{ Matches }
  Matches := TRUE;
  IF ( FileSpec = '*.*' ) OR ( FileSpec = '' ) THEN exit; {'' or '*.*' = all files match}
  Getmem ( pStringToSearch, StrMaxLen );
  Getmem ( pFileSpec, StrMaxLen );
  StrPCopy ( pStringToSearch, StringToSearch );
  StrPCopy ( pFileSpec, FileSpec );
  {$ifdef __OS_DOS__}  { dos, windows, os/2, djgpp - case-insensitive search }
                       { linux, unix - case-sensitive search }
  StrUpper ( pStringToSearch );
  StrUpper ( pFileSpec );
  {$endif}
  Matches := MatchPattern ( pStringToSearch, pFileSpec );
  Freemem ( pFileSpec, StrMaxLen );
  Freemem ( pStringToSearch, StrMaxLen );
END; { Matches }
{****************************************************}


{$ifndef Delphi}
{$ifndef __GPC__}
FUNCTION FileExists ( CONST fname : string ) : boolean; {simple fileexist function}
VAR
f : FILE;
i : byte;
BEGIN
    i := filemode;
    filemode := FileModeCompat;
    assign ( f, FromOEM ( fname ) );
    {$i-}
    Reset ( f, 1 );
    filemode := i;
    FileExists := ioresult = 0;
    Close ( f ); IF ioresult <> 0 THEN;
    {$i+}
END;
{$endif} {__GPC__}
{$endif Delphi}

PROCEDURE DummyReport ( Retcode : longint;Rec : pReportRec );
{$ifdef USE_STDCALL}STDCALL;{$else USE_STDCALL}{$ifdef MSWINDOWS}EXPORT;{$endif MSWINDOWS}
{$endif USE_STDCALL}
{dummy report procedure}
BEGIN
END;
{****************************************************}
FUNCTION FileUnzip
{ ( SourceZipFile, TargetDirectory, FileSpecs : pChar;
 Report : UnzipReportProc;Question : UnzipQuestionProc ) : integer};

VAR
    r  : tziprec;
    rc : integer;
    buf,
    thename,
    target : TDirType;
    StartOffSet,
    Count  : Int32;
    rSize,
    cSize  : Word64;
    s      : TString;
    b      : boolean;
BEGIN
  Count := 0;
  rSize := 0;
  cSize := 0;
  StartOffSet := 0;
  FileUnzip := unzip_MissingParameter;
  IF ( StrPas ( SourceZipFile ) = '' ) OR ( StrPas ( TargetDirectory ) = '' ) THEN
    Exit;
  Strpcopy ( thename, FromOEM ( Strpas ( SourceZipFile ) ) );
  Strpcopy ( target, FromOEM ( Strpas ( TargetDirectory ) ) );
  ConvertPath ( target );
  IF ( target [ 0 ] <> #0 ) AND ( target [ strlen ( target ) - 1 ] <> OS_Path_Separator ) THEN
    strcat ( target, OS_Path_Separator );

  FileUnzip := unzip_NotZipFile;
  IF NOT iszip ( thename, @StartOffset ) THEN exit;

  FillChar ( ZipRec, Sizeof ( ZipRec ), #0 );
  FileUnzip := unzip_InternalError;

  rc := getfirstinzip ( thename, r );

  IF rc <> unzip_ok THEN exit;

  IF @Report <> NIL THEN {start of ZIP file}
  WITH ZipRec DO BEGIN
       IsaDir := FALSE;
       Fill_Array ( FileName, thename );
       Size   := UnZipSize ( SourceZipFile, CompressSize );
       Ratio  := CalcRatio ( CompressSize, Size );
       Status := unzip_starting;
       Report{$ifdef __TMT__}^{$endif} ( Status, @ZipRec );
  END; {start of ZIP file}

  IF {$ifndef __TMT__}@{$endif}Report = NIL THEN Report := {$ifdef UseAT}@{$endif}DummyReport;
  ZipReport := Report;

  WHILE ( rc = unzip_ok )
  DO BEGIN
   IF ( Matches ( StrPas ( FileSpecs ), StrPas ( R.FileName ) ) )
   THEN BEGIN
      Inc ( rSize, r.Size );
      Inc ( cSize, r.CompressSize );
      Inc ( R.Headeroffset, StartOffSet );

      IF LowcaseFileNames THEN StrLower ( R.FileName ); {!!!}
      {FromOemP (R.FileName); }

      strcopy ( buf, target );
      IF NoRecurseDirs  { no recursion }
      THEN BEGIN
          s := StripPath ( Strpas ( r.filename ) ) + #0;
          Strcat ( buf, @s [ 1 ] );
      END ELSE strcat ( buf, r.filename );

      WITH ZipRec DO BEGIN { report start of file }
           s := StrPas ( Buf );
           IsaDir := s [ length ( s ) ] IN ['\', '/'];
           Time := r.Time;
           Size := r.Size;
           CompressSize := r.CompressSize;
           Fill_Array ( FileName, buf );
           PackMethod := r.PackMethod;
           Attr := r.Attr;
           Ratio := CalcRatio ( CompressSize, Size );
           Status := file_starting;
           IF ( IsaDir ) AND ( NoRecurseDirs )
           THEN {} ELSE
           ZipReport{$ifdef __TMT__}^{$endif} ( Status, @ZipRec );
      END;  { start of file }

      b := FileExists ( StrPas ( buf ) ); { sigsegv here !!! }

      IF  ( {$ifndef __TMT__}@{$endif}Question <> NIL )
      AND ( b = True )
      AND ( Question{$ifdef __TMT__}^{$endif} ( @ZipRec ) = FALSE )
      THEN BEGIN
         rc := unzip_ok;              { we are okay }
         WITH ZipRec DO BEGIN
           Status := file_unzipping;
           PackMethod := 9;           { skipped }
           ZipReport{$ifdef __TMT__}^{$endif} ( Size, @ZipRec );  { report uncompressed size }
         END;
      END ELSE BEGIN
          IF b THEN SetFileAttrs ( StrPas ( Buf ), 0 );
          rc := unzipfile ( thename, buf, r.headeroffset, 0,
          {$ifdef MSWINDOWS}vk_escape{$else}27{$endif} ); {Escape interrupts}
          IF rc = unzip_ok { reset the file attributes }
          THEN BEGIN
             SetFileAttrs ( StrPas ( Buf ), ZipRec.Attr );
          END;
      END;

      IF rc = unzip_ok
      THEN BEGIN
         Inc ( Count );
         WITH ZipRec DO BEGIN   { report end of file }
            Status := file_completed;
            IF ( IsaDir ) AND ( NoRecurseDirs )
            THEN {} ELSE
            ZipReport{$ifdef __TMT__}^{$endif} ( Status, @ZipRec );
         END; { end of file }
      END
      ELSE BEGIN { some error }
           ZipRec.Status := file_failure; {error}
           CASE rc OF
                unzip_CRCErr,
                unzip_WriteErr,
                unzip_Encrypted,
                unzip_NotSupported : ZipReport{$ifdef __TMT__}^{$endif} ( rc, @ZipRec );

                unzip_ZipFileOpenError,
                unzip_ReadErr, unzip_Userabort,
                unzip_FileError, unzip_InternalError,
                unzip_InUse, unzip_ZipFileErr :
                BEGIN
                    ZipRec.Status := rc{unzip_SeriousError};
                    FileUnzip := rc{unzip_SeriousError};   {Serious error, force abort}
                    ZipReport{$ifdef __TMT__}^{$endif} ( unzip_SeriousError, @ZipRec );
                    closezipfile ( r );
                    SetUnZipReportProc ( NIL );
                    SetUnZipQuestionProc ( NIL );
                    exit;
                END;
           END; {case rc}
           rc := unzip_ok; { to continue if we fail with one file }
           {Continue;}
        END; {else}
   END; { if Matches }
   rc := getnextinzip ( r );
  END; {while }

  closezipfile ( r );               {Free memory used for central directory info}

  WITH ZipRec DO BEGIN { report end of ZIP file }
       Time := - 1;
       Attr := - 1;
       PackMethod := 0;
       Size := rSize;
       CompressSize := cSize;
       Fill_Array ( FileName, thename );
       Ratio := CalcRatio ( CompressSize, Size );
       Status := unzip_completed;
       ZipReport{$ifdef __TMT__}^{$endif} ( Status, @ZipRec );
  END; { end of ZIP file }

  SetUnZipReportProc ( NIL );
  SetUnZipQuestionProc ( NIL );
  FileUnzip := Count;
END; { FileUnzip }
{***************************************************************************}
FUNCTION FileUnzipEx
{( SourceZipFile, TargetDirectory, FileSpecs : pChar ) : integer};
BEGIN
  FileUnzipEx :=
  FileUnzip ( SourceZipFile, TargetDirectory, FileSpecs, ZipReport, ZipQuestion );
END; { FileUnzipEx }
{***************************************************************************}
FUNCTION Viewzip
{( SourceZipFile, FileSpecs : pChar; Report : UnzipReportProc ) : integer};
VAR
    rc : integer;
    r  : tziprec;
    thename : TDirType;
    Count : integer;
    rSize, cSize : Word64;
    l : longint;

BEGIN
  Count := 0;
  rSize := 0;
  cSize := 0;
  l     := 0;
  Viewzip := unzip_MissingParameter;
  IF ( StrPas ( SourceZipFile ) = '' ) OR ( @Report = NIL ) THEN Exit;

  Strcopy ( thename, SourceZipFile );
  ViewZip := unzip_NotZipFile;

  IF NOT iszip ( thename, @l ) THEN exit;
  FillChar ( ZipRec, Sizeof ( ZipRec ), #0 );

  ViewZip := unzip_InternalError;
  rc := getfirstinzip ( thename, r );
  IF ( rc <> unzip_ok ) THEN exit;

  IF {$ifndef __TMT__}@{$endif}Report = NIL THEN Report := {$ifdef UseAT}@{$endif}DummyReport;

  WHILE ( rc = unzip_ok )
  DO BEGIN
   IF ( Matches ( StrPas ( FileSpecs ), StrPas ( R.FileName ) ) )
   THEN BEGIN
      Inc ( rSize, r.Size );
      Inc ( cSize, r.CompressSize );
      {
      Inc ( R.Headeroffset, l );
      }
      IF LowcaseFileNames THEN StrLower ( R.FileName ); {!!!}
      WITH ZipRec DO BEGIN
           Time := r.Time;
           Size := r.Size;
           CompressSize := r.CompressSize;
           Fill_Array ( FileName, r.Filename );
           PackMethod := r.PackMethod;
           Attr := r.Attr;
           Ratio := CalcRatio ( CompressSize, Size );
           Status := file_unzipping;
      END;
      Inc ( Count );
      Report{$ifdef __TMT__}^{$endif} ( rc, @ZipRec );
   END; {matches}
      rc := getnextinzip ( r );
  END; {while }
  closezipfile ( r );

  WITH ZipRec DO BEGIN
       Time := - 1;
       Attr := - 1;
       PackMethod := 0;
       Size := rSize;
       CompressSize := cSize;
       Fill_Array ( FileName, thename );
       Ratio := CalcRatio ( CompressSize, Size );
       Status := unzip_completed;
  END;
  Report{$ifdef __TMT__}^{$endif} ( Count, @ZipRec );
  ViewZip := Count;
END; { ViewZip }
{***************************************************************************}
FUNCTION UnZipSize
{( SourceZipFile : pChar; VAR Compressed : Word64 ) : Word64};
VAR
    rc : integer;
    r  : tziprec;
    thename : TDirType;
    Count : Word64;
    f : FILE;

BEGIN
  Compressed := 0;
  UnZipSize := 0;
  IF ( StrPas ( SourceZipFile ) = '' ) THEN Exit;
  Assign ( f, FromOEM ( StrPas ( SourceZipFile ) ) );
  count := filemode;
  filemode := FileModeCompat;
  {$i-}Reset ( f, 1 );{$i+}
  filemode := count;
  IF ioresult <> 0 THEN exit;
  Count := filesize ( f );
  close ( f );
  UnZipSize := count;
  Compressed := count;
  Strcopy ( thename, SourceZipFile );
  IF NOT iszip ( thename, NIL ) THEN exit;
  Count := 0;
  Compressed := 0;
  rc := getfirstinzip ( thename, r );
  IF ( rc <> unzip_ok ) THEN exit;
  WHILE ( rc = unzip_ok )
  DO BEGIN
      Inc ( Count, r.Size );
      Inc ( Compressed, r.CompressSize );
      rc := getnextinzip ( r );
  END; {while }
  closezipfile ( r );
  UnZipSize := Count;
END; { UnZipSize }
{***************************************************************************}
FUNCTION  SetUnZipReportProc;
BEGIN
   SetUnZipReportProc := @ZipReport; {save and return original}
   ZipReport := aProc;
END; { SetUnZipReportProc }
{***************************************************************************}
FUNCTION  SetUnZipQuestionProc;
BEGIN
  SetUnZipQuestionProc := @ZipQuestion;  {save and return original}
  ZipQuestion := aProc;
END; { SetUnZipQuestionProc }
{***************************************************************************}
FUNCTION SetNoRecurseDirs;
BEGIN
   SetNoRecurseDirs := NoRecurseDirs;
   NoRecurseDirs := DontRecurse;
END; { SetNoRecurseDirs }
{***************************************************************************}
{***************************************************************************}
PROCEDURE ChfUnzip_Init;
BEGIN
   slide := NIL;       {unused}

  {$ifdef MSWINDOWS}
   inuse := FALSE;    {Not yet in use!}
   lastusedtime := 0; {Not yet used}
  {$endif}

   SetUnZipReportProc ( NIL );
   SetUnZipQuestionProc ( NIL );

   SetNoRecurseDirs ( FALSE );
END;
{***************************************************************************}
{***************************************************************************}

BEGIN
  ChfUnzip_Init;
END.

