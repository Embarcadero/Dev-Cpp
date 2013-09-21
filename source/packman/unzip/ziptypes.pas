{
Type definitions for UNZIP
  * original version by Christian Ghisler
  * extended
    and
    amended for Win32 and OS/2 (Virtual Pascal) by Prof. Abimbola Olowofoyeku (The African Chief)
  * amended for FreePascal by Peter Vreman
  * ConvertPath and CalcRatio routines by Peter Vreman
  * amended for GNU Pascal (GPC) by Prof. Abimbola Olowofoyeku (The African Chief)
            http://www.bigfoot.com/~African_Chief/
}

UNIT ziptypes;
{$i unzip.inc}

INTERFACE

{$ifdef __GPC__}
USES
Strings,
GPC;

TYPE
  Word    = Cardinal attribute ( Size = 16 );  { GPC's normal Word is 32-bit }
  Word16  = Cardinal attribute ( Size = 16 );
  Word32  = Cardinal attribute ( Size = 32 );
  Int32   = Integer  attribute ( Size = 32 );
  Longint = Integer  attribute ( Size = 32 );   { GPC's normal Longint is 64-bit }
  Word64  = Cardinal attribute ( Size = 64 );
//  Int64   = Integer  attribute ( Size = 64 );
{$else}
TYPE
  {$ifndef Has_Int64}
  Int64   = Longint;
  {$endif}
  Int32   = Longint;
  Word64  = Int64;
  Word32  = {$ifdef Has_Int64}Cardinal{$else}Longint{$endif};
  Word16  = Word;
{$endif}

{$ifdef Has_Int64}
ZipBitInt = Cardinal;
{$else}
ZipBitInt = Int64;
{$endif}

{$ifndef OS_16_BIT}
{$ifndef __GPC__}TYPE Integer = Longint;{$endif}
{$endif}

TYPE
nWord = {$ifdef OS_BigMem}Integer;{$else}Word;{$endif}
pLongint = ^Longint;

{$ifndef __GPC__}
TString = String;
{$endif}

CONST
{ file buffer size; the bigger the better - if installed RAM is big enough }
TFileBufferSize = High ( nword ) - 16;

{ filename length }
TFileNameSize   = {$ifdef OS_BigMem}259{$else}79{$endif};

{ file stuff }
TYPE
TFileType = FILE;

{ Record for UNZIP }
TYPE
TSignatureType = ARRAY [ 0..3 ] OF char;
BufType  = ARRAY [ 0..TFileBufferSize ] OF char;
TDirType = ARRAY [ 0..TFileNameSize ] OF char;
TZipRec  = RECORD
       buf : ^Buftype;          {please}         {buffer containing central dir}
       bufsize,                 {do not}         {size of buffer}
       localstart   : nword;    {change these!}  {start pos in buffer}
       Time         : Int32;
       Size,
       CompressSize : Word64;
       headeroffset : Word32;
       FileName     : TDirType;
       PackMethod   : Word16;
       Attr         : Byte;
       DirFile      : FILE;     { used internally: do not access directly! }
     END; { TZipRec }


{ record for callback progress Reports, etc. }
TYPE
pReportRec = ^TReportRec;     {passed to callback functions}
TReportRec = RECORD
       FileName : TDirType;   {name of individual file}
       Time     : longint;    {date and time stamp of individual file}
       Size,                  {uncompressed and time stamp of individual file}
       CompressSize : Word64; {compressed and time stamp of individual file}
       Attr : integer;        {file attribute of individual file}
       PackMethod : Word16;   {compression method of individual file}
       Ratio  : byte;         {compression ratio of individual file}
       Status : longint;      {callback status code to show where we are}
       IsaDir : Boolean;      {is this file a directory?}
END; { TReportRec }

{ callback status codes }
CONST
file_starting    = - 1000;  {beginning the unzip process; file}
file_unzipping   = - 1001;  {continuing the unzip process; file}
file_completed   = - 1002;  {completed the unzip process; file}
file_Failure     = - 1003;  {failure in unzipping file}
unzip_starting   = - 1004;  {starting with a new ZIP file}
unzip_completed  = - 1005;  {completed this ZIP file}


{ procedural types for callbacks }
TYPE
pUnzipReportProc = ^UnzipReportProc;
{$ifdef __TMT__}
{$W-}
UnzipReportProc  = ^PROCEDURE conv arg_stdcall ( Retcode : longint;Rec : pReportRec );
{$W+}
{$else}
UnzipReportProc  = PROCEDURE ( Retcode : longint; Rec : pReportRec )
{$ifdef USE_STDCALL}STDCALL{$endif};
{$endif} {TMT}

{ procedural type for "Report" callback: the callback function
  (if any) is called several times during the unzip process

  Error codes are sent to the callback in "Retcode". Other
  details are sent in the record pointed to by "Rec".
  * Note particularly Rec^.Status - this contains information about
  the current status or stage of the unzip process. It can have
  any of the following values;
  (archive status)
    unzip_starting   = starting with a new ZIP archive (rec^.filename)
    unzip_completed  = finished with the ZIP archive (rec^.filename)

  (file status)
    file_starting    = starting to unzip (extract) a file (from archive)
    file_unzipping   = continuing to unzip a file (from archive)
        (when this status value is reported, the actual number of
         bytes written to the file are reported in "Retcode"; this is
         valuable for updating any progress bar)

    file_completed   = finshed  unzip a file (from archive)
    file_Failure     = could not extract the file (from archive)
}

pUnzipQuestionProc = ^UnzipQuestionProc;
{$ifdef __TMT__}
{$W-}
UnzipQuestionProc = ^FUNCTION conv arg_stdcall ( Rec : pReportRec ) : Boolean;
{$W+}
{$else}
UnzipQuestionProc = FUNCTION ( Rec : pReportRec ) : Boolean
{$ifdef USE_STDCALL}STDCALL{$endif};
{$endif}{TMT}


{ procedural type for "Question" callback:if a file already
  exists, the callback (if any) will be called to ask whether
  the file should be overwritten by the one in the ZIP file;

  the details of the file in the ZIP archive are supplied in the
  record pointed to by "Rec"

 in your callback function, you should;
   return TRUE  if you want the existing file to be overwritten
   return FALSE is you want the existing file to be skipped
}


{ Separator for Directory paths }
CONST
{$ifdef __GPC__}
  OS_Path_Separator = DirSeparator;
  OtherOsSeparator  = {$ifdef __OS_DOS__} '/' {$else} '\' {$endif};
{$else} {__GPC__}
  {$ifdef linux}
    OS_Path_Separator = '/';
  {$else}
    OS_Path_Separator = '\';
  {$endif}

  { the path separator used by another OS which we will need to convert
   to our own path separator }
  OtherOsSeparator = {$ifdef linux} '\' {$else} '/' {$endif};
{$endif}{__GPC__}

{Error codes returned by the main unzip functions}
CONST
  unzip_Ok             =  0;
  unzip_CRCErr         = - 1;
  unzip_WriteErr       = - 2;
  unzip_ReadErr        = - 3;
  unzip_ZipFileErr     = - 4;
  unzip_UserAbort      = - 5;
  unzip_NotSupported   = - 6;
  unzip_Encrypted      = - 7;
  unzip_InUse          = - 8;
  unzip_InternalError  = - 9;    {Error in zip format}
  unzip_NoMoreItems    = - 10;
  unzip_FileError      = - 11;   {Error Accessing file}
  unzip_NotZipfile     = - 12;   {not a zip file}
  unzip_HeaderTooLarge = - 13;   {can't handle such a big ZIP header}
  unzip_ZipFileOpenError = - 14; { can't open zip file }
  unzip_SeriousError   = - 100;  {serious error}
  unzip_MissingParameter = - 500; {missing parameter}


{ the various unzip methods }
CONST
Unzipmethods : ARRAY [ 0..9 ] OF pchar =
  ( 'stored', 'shrunk', 'reduced 1', 'reduced 2', 'reduced 3',
   'reduced 4', 'imploded', 'tokenized', 'deflated', 'skipped' );

{ unzip actions being undertaken }
CONST
UnzipActions : ARRAY [ 0..9 ] OF pchar =
  ( 'copying', 'unshrinking', 'unreducing 1', 'unreducing 2', 'unreducing 3',
   'unreducing 4', 'exploding', 'un-tokenizing', 'inflating', 'skipping' );

{ rudimentary "uppercase" function }
FUNCTION Upper ( s : String ) : TString;

{ rudimentary "lowercase" function }
FUNCTION Lower ( s : String ) : TString;

{ remove path and return filename only }
FUNCTION StripPath ( CONST s : String ) : TString;

{ Calculate the ratio between newsize and orgsize }
FUNCTION CalcRatio ( newsize, orgsize : longint ) : Longint;

{ Convert Path separators to correct ones for operating system }
PROCEDURE ConvertPath ( p : pchar );

VAR
LowcaseFileNames : Boolean; { set to TRUE to convert the names of files (not
                             directories) in the ZIP archive to lowercase when extracting them
                             }

IMPLEMENTATION

{$ifndef __GPC__}
 USES
 {$ifdef Delphi}
 SysUtils;
 {$else Delphi}
 Strings;
 {$endif Delphi}
{$endif}

FUNCTION Upper ( s : String ) : TString;
VAR i : integer;
BEGIN
   FOR i := 1 TO length ( s ) DO s [ i ] := Upcase ( s [ i ] );
   Upper := s;
END;

FUNCTION Lower ( s : String ) : TString;
VAR i, j : integer;
BEGIN
   FOR i := 1 TO length ( s )
   DO BEGIN
       j := Ord ( s [i] );
       IF j IN [65..90]  { convert only 'A' .. 'Z' }
        THEN s [i] := Chr ( j + 32 );
   END;
   Lower := s;
END;

FUNCTION StripPath ( CONST s : String ) : TString;
VAR
i, j : integer;
BEGIN
   StripPath := s;
   j := length ( s );
   FOR i := j DOWNTO 1 DO BEGIN
       IF s [ i ] IN [ '\', ':', '/' ] THEN BEGIN
          StripPath := Copy ( s, succ ( i ), j - i );
          exit;
       END;
   END;
END;

FUNCTION CalcRatio ( newsize, orgsize : longint ) : Longint;
BEGIN
{ When the size is so large there is no difference in accuracy }
  IF newsize > ( MaxLongint div 100 ) THEN
   BEGIN
     newsize := newsize div 100;
     orgsize := orgsize div 100;
   END;
  IF orgsize = 0 THEN
   CalcRatio := 0
  ELSE
   CalcRatio := 100 - ( ( newsize * 100 ) div orgsize );
END;

PROCEDURE ConvertPath ( p : pchar );
VAR
  i, Len : longint;
BEGIN
  Len := StrLen ( p );
  FOR i := 1 TO Len DO
  IF p [ i ] = OtherOsSeparator THEN p [ i ] := OS_Path_Separator;
END;

BEGIN
   LowcaseFileNames := False;
END.

