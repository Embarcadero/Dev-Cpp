(**
===============================================================================================
Name    : LibTar
===============================================================================================
Subject : Handling of "tar" files
===============================================================================================
Author  : Stefan Heymann
          Eschenweg 3
          72076 Tübingen
          GERMANY

E-Mail:   stefan@destructor.de
Web:      www.destructor.de

===============================================================================================
TTarArchive Usage
-----------------
- Choose a constructor
- Make an instance of TTarArchive                  TA := TTarArchive.Create (Filename);
- Scan through the archive                         TA.Reset;
                                                   WHILE TA.FindNext (DirRec) DO BEGIN
- Evaluate the DirRec for each file                  ListBox.Items.Add (DirRec.Name);
- Read out the current file                          TA.ReadFile (DestFilename);
  (You can ommit this if you want to
  read in the directory only)                        END;      
- You're done                                      TA.Free;


TTarWriter Usage
----------------
- Choose a constructor
- Make an instance of TTarWriter                   TW := TTarWriter.Create ('my.tar');
- Add a file to the tar archive                    TW.AddFile ('foobar.txt');
- Add a string as a file                           TW.AddString (SL.Text, 'joe.txt', Now);
- Destroy TarWriter instance                       TW.Free;
- Now your tar file is ready.


Source, Legals ("Licence")
--------------------------
The official site to get this code is http://www.destructor.de/

Usage and Distribution of this Source Code is ruled by the
"Destructor.de Source code Licence" (DSL) which comes with this file or
can be downloaded at http://www.destructor.de/

IN SHORT: Usage and distribution of this source code is free.
          You use it completely on your own risk.

Donateware
----------
If you like this code, you are free to donate
http://www.destructor.de/donateware.htm

===============================================================================================
!!!  All parts of this code which are not finished or known to be buggy
     are marked with three exclamation marks
===============================================================================================
Date        Author Changes
-----------------------------------------------------------------------------------------------
2001-04-26  HeySt  0.0.1 Start
2001-04-28  HeySt  1.0.0 First Release
2001-06-19  HeySt  2.0.0 Finished TTarWriter
2001-09-06  HeySt  2.0.1 Bugfix in TTarArchive.FindNext: FBytesToGo must sometimes be 0
2001-10-25  HeySt  2.0.2 Introduced the ClearDirRec procedure
2001-11-13  HeySt  2.0.3 Bugfix: Take out ClearDirRec call from WriteTarHeader
                         Bug Reported by Tony BenBrahim
2001-12-25  HeySt  2.0.4 WriteTarHeader: Fill Rec with zero bytes before filling it
2002-05-18  HeySt  2.0.5 Kylix awareness: Thanks to Kerry L. Davison for the canges
2005-09-03  HeySt  2.0.6 TTarArchive.FindNext: Don't access SourceStream.Size
                         (for compressed streams, which don't know their .Size)
2006-03-13  HeySt  2.0.7 Bugfix in ReadFile (Buffer : POINTER)
2007-05-16  HeySt  2.0.8 Bugfix in TTarWriter.AddFile (Convertfilename in the ELSE branch)
                         Bug Reported by Chris Rorden
2010-11-29  HeySt  2.1.0 WriteTarHeader: Mode values for ftNormal/ftLink/ftSymbolicLink/ftDirectory
                         Thanks to Iouri Kharon for the fix.
                         Still no support for filenames > 100 bytes. Sorry.
                         Support for Unicode Delphi versions (2009, 2010, XE, etc.)
2011-05-23  HeySt  2.1.1 New IFDEF WIN32 in the USES clause
*)

UNIT LibTar;

INTERFACE

USES
(*$IFDEF LINUX*)
   Libc,
(*$ENDIF *)
{$IFDEF WIN32}
  {$DEFINE MSWINDOWS} // predefined for D6+/BCB6+    // because in Delphi 5  MSWINDOWS is not defined
{$ENDIF}
(*$IFDEF MSWINDOWS *)
   Windows,
(*$ENDIF *)
  SysUtils, Classes;

TYPE
  (*$IFNDEF UNICODE *)
  RawByteString = AnsiString;
  (*$ENDIF *)

  // --- File Access Permissions
  TTarPermission  = (tpReadByOwner, tpWriteByOwner, tpExecuteByOwner,
                     tpReadByGroup, tpWriteByGroup, tpExecuteByGroup,
                     tpReadByOther, tpWriteByOther, tpExecuteByOther);
  TTarPermissions = SET OF TTarPermission;

  // --- Type of File
  TFileType = (ftNormal,          // Regular file
               ftLink,            // Link to another, previously archived, file (LinkName)
               ftSymbolicLink,    // Symbolic link to another file              (LinkName)
               ftCharacter,       // Character special files
               ftBlock,           // Block special files
               ftDirectory,       // Directory entry. Size is zero (unlimited) or max. number of bytes
               ftFifo,            // FIFO special file. No data stored in the archive.
               ftContiguous,      // Contiguous file, if supported by OS
               ftDumpDir,         // List of files
               ftMultiVolume,     // Multi-volume file part
               ftVolumeHeader);   // Volume header. Can appear only as first record in the archive

  // --- Mode
  TTarMode  = (tmSetUid, tmSetGid, tmSaveText);
  TTarModes = SET OF TTarMode;

  // --- Record for a Directory Entry
  //     Adjust the ClearDirRec procedure when this record changes!
  TTarDirRec  = RECORD
                  Name        : AnsiString;        // File path and name
                  Size        : INT64;             // File size in Bytes
                  DateTime    : TDateTime;         // Last modification date and time
                  Permissions : TTarPermissions;   // Access permissions
                  FileType    : TFileType;         // Type of file
                  LinkName    : AnsiString;        // Name of linked file (for ftLink, ftSymbolicLink)
                  UID         : INTEGER;           // User ID
                  GID         : INTEGER;           // Group ID
                  UserName    : AnsiString;        // User name
                  GroupName   : AnsiString;        // Group name
                  ChecksumOK  : BOOLEAN;           // Checksum was OK
                  Mode        : TTarModes;         // Mode
                  Magic       : AnsiString;        // Contents of the "Magic" field
                  MajorDevNo  : INTEGER;           // Major Device No. for ftCharacter and ftBlock
                  MinorDevNo  : INTEGER;           // Minor Device No. for ftCharacter and ftBlock
                  FilePos     : INT64;             // Position in TAR file
                END;

  // --- The TAR Archive CLASS
  TTarArchive = CLASS
                PROTECTED
                  FStream     : TStream;   // Internal Stream
                  FOwnsStream : BOOLEAN;   // True if FStream is owned by the TTarArchive instance
                  FBytesToGo  : INT64;     // Bytes until the next Header Record
                PUBLIC
                  CONSTRUCTOR Create (Stream   : TStream);                                OVERLOAD;
                  CONSTRUCTOR Create (Filename : STRING;
                                      FileMode : WORD = fmOpenRead OR fmShareDenyWrite);  OVERLOAD;
                  DESTRUCTOR Destroy;                                       OVERRIDE;
                  PROCEDURE Reset;                                         // Reset File Pointer
                  FUNCTION  FindNext (VAR DirRec : TTarDirRec) : BOOLEAN;  // Reads next Directory Info Record. FALSE if EOF reached
                  PROCEDURE ReadFile (Buffer   : POINTER); OVERLOAD;       // Reads file data for last Directory Record
                  PROCEDURE ReadFile (Stream   : TStream); OVERLOAD;       // -;-
                  PROCEDURE ReadFile (Filename : STRING);  OVERLOAD;       // -;-
                  FUNCTION  ReadFile : RawByteString;      OVERLOAD;       // -;-

                  PROCEDURE GetFilePos (VAR Current, Size : INT64);        // Current File Position
                  PROCEDURE SetFilePos (NewPos : INT64);                   // Set new Current File Position
                END;

  // --- The TAR Archive Writer CLASS
  TTarWriter = CLASS
               PROTECTED
                 FStream      : TStream;
                 FOwnsStream  : BOOLEAN;
                 FFinalized   : BOOLEAN;
                                                   // --- Used at the next "Add" method call: ---
                 FPermissions : TTarPermissions;   // Access permissions
                 FUID         : INTEGER;           // User ID
                 FGID         : INTEGER;           // Group ID
                 FUserName    : AnsiString;        // User name
                 FGroupName   : AnsiString;        // Group name
                 FMode        : TTarModes;         // Mode
                 FMagic       : AnsiString;        // Contents of the "Magic" field
                 CONSTRUCTOR CreateEmpty;
               PUBLIC
                 CONSTRUCTOR Create (TargetStream   : TStream);                            OVERLOAD;
                 CONSTRUCTOR Create (TargetFilename : STRING; Mode : INTEGER = fmCreate);  OVERLOAD;
                 DESTRUCTOR Destroy; OVERRIDE;                   // Writes End-Of-File Tag
                 PROCEDURE AddFile   (Filename : STRING;        TarFilename : AnsiString = '');
                 PROCEDURE AddStream (Stream   : TStream;       TarFilename : AnsiString; FileDateGmt : TDateTime);
                 PROCEDURE AddString (Contents : RawByteString; TarFilename : AnsiString; FileDateGmt : TDateTime);
                 PROCEDURE AddDir          (Dirname            : AnsiString; DateGmt : TDateTime; MaxDirSize : INT64 = 0);
                 PROCEDURE AddSymbolicLink (Filename, Linkname : AnsiString; DateGmt : TDateTime);
                 PROCEDURE AddLink         (Filename, Linkname : AnsiString; DateGmt : TDateTime);
                 PROCEDURE AddVolumeHeader (VolumeId           : AnsiString; DateGmt : TDateTime);
                 PROCEDURE Finalize;
                 PROPERTY Permissions : TTarPermissions READ FPermissions WRITE FPermissions;   // Access permissions
                 PROPERTY UID         : INTEGER         READ FUID         WRITE FUID;           // User ID
                 PROPERTY GID         : INTEGER         READ FGID         WRITE FGID;           // Group ID
                 PROPERTY UserName    : AnsiString      READ FUserName    WRITE FUserName;      // User name
                 PROPERTY GroupName   : AnsiString      READ FGroupName   WRITE FGroupName;     // Group name
                 PROPERTY Mode        : TTarModes       READ FMode        WRITE FMode;          // Mode
                 PROPERTY Magic       : AnsiString      READ FMagic       WRITE FMagic;         // Contents of the "Magic" field
               END;

// --- Some useful constants
CONST
  FILETYPE_NAME : ARRAY [TFileType] OF STRING =
                  ('Regular', 'Link', 'Symbolic Link', 'Char File', 'Block File',
                   'Directory', 'FIFO File', 'Contiguous', 'Dir Dump', 'Multivol', 'Volume Header');

  ALL_PERMISSIONS     = [tpReadByOwner, tpWriteByOwner, tpExecuteByOwner,
                         tpReadByGroup, tpWriteByGroup, tpExecuteByGroup,
                         tpReadByOther, tpWriteByOther, tpExecuteByOther];
  READ_PERMISSIONS    = [tpReadByOwner, tpReadByGroup,  tpReadByOther];
  WRITE_PERMISSIONS   = [tpWriteByOwner, tpWriteByGroup, tpWriteByOther];
  EXECUTE_PERMISSIONS = [tpExecuteByOwner, tpExecuteByGroup, tpExecuteByOther];


FUNCTION  PermissionString      (Permissions : TTarPermissions) : STRING;
FUNCTION  ConvertFilename       (Filename    : STRING)          : STRING;
FUNCTION  FileTimeGMT           (FileName    : STRING)          : TDateTime;  OVERLOAD;
FUNCTION  FileTimeGMT           (SearchRec   : TSearchRec)      : TDateTime;  OVERLOAD;
PROCEDURE ClearDirRec           (VAR DirRec  : TTarDirRec);


(*
===============================================================================================
IMPLEMENTATION
===============================================================================================
*)

IMPLEMENTATION

FUNCTION PermissionString (Permissions : TTarPermissions) : STRING;
BEGIN
  Result := '';
  IF tpReadByOwner    IN Permissions THEN Result := Result + 'r' ELSE Result := Result + '-';
  IF tpWriteByOwner   IN Permissions THEN Result := Result + 'w' ELSE Result := Result + '-';
  IF tpExecuteByOwner IN Permissions THEN Result := Result + 'x' ELSE Result := Result + '-';
  IF tpReadByGroup    IN Permissions THEN Result := Result + 'r' ELSE Result := Result + '-';
  IF tpWriteByGroup   IN Permissions THEN Result := Result + 'w' ELSE Result := Result + '-';
  IF tpExecuteByGroup IN Permissions THEN Result := Result + 'x' ELSE Result := Result + '-';
  IF tpReadByOther    IN Permissions THEN Result := Result + 'r' ELSE Result := Result + '-';
  IF tpWriteByOther   IN Permissions THEN Result := Result + 'w' ELSE Result := Result + '-';
  IF tpExecuteByOther IN Permissions THEN Result := Result + 'x' ELSE Result := Result + '-';
END;


FUNCTION ConvertFilename  (Filename : STRING) : STRING;
         // Converts the filename to Unix conventions
BEGIN
  (*$IFDEF LINUX *)
  Result := Filename;
  (*$ELSE *)
  Result := StringReplace (Filename, '\', '/', [rfReplaceAll]);
  (*$ENDIF *)
END;


FUNCTION FileTimeGMT (FileName: STRING): TDateTime;
         // Returns the Date and Time of the last modification of the given File
         // The Result is zero if the file could not be found
         // The Result is given in UTC (GMT) time zone
VAR
  SR : TSearchRec;
BEGIN
  Result := 0.0;
  IF FindFirst (FileName, faAnyFile, SR) = 0 THEN
    Result := FileTimeGMT (SR);
  FindClose (SR);
END;


FUNCTION FileTimeGMT (SearchRec : TSearchRec) : TDateTime;
(*$IFDEF MSWINDOWS *)
VAR
  SystemFileTime: TSystemTime;
(*$ENDIF *)
(*$IFDEF LINUX *)
VAR
  TimeVal  : TTimeVal;
  TimeZone : TTimeZone;
(*$ENDIF *)
BEGIN
  Result := 0.0;
  (*$IFDEF MSWINDOWS *) (*$WARNINGS OFF *)
    IF (SearchRec.FindData.dwFileAttributes AND faDirectory) = 0 THEN
      IF FileTimeToSystemTime (SearchRec.FindData.ftLastWriteTime, SystemFileTime) THEN
        Result := EncodeDate (SystemFileTime.wYear, SystemFileTime.wMonth, SystemFileTime.wDay)
                + EncodeTime (SystemFileTime.wHour, SystemFileTime.wMinute, SystemFileTime.wSecond, SystemFileTime.wMilliseconds);
  (*$ENDIF *) (*$WARNINGS ON *)
  (*$IFDEF LINUX *)
     IF SearchRec.Attr AND faDirectory = 0 THEN BEGIN
       Result := FileDateToDateTime (SearchRec.Time);
       GetTimeOfDay (TimeVal, TimeZone);
       Result := Result + TimeZone.tz_minuteswest / (60 * 24);
       END;
  (*$ENDIF *)
end;


PROCEDURE ClearDirRec (VAR DirRec : TTarDirRec);
          // This is included because a FillChar (DirRec, SizeOf (DirRec), 0)
          // will destroy the long string pointers, leading to strange bugs
BEGIN
  WITH DirRec DO BEGIN
    Name        := '';
    Size        := 0;
    DateTime    := 0.0;
    Permissions := [];
    FileType    := TFileType (0);
    LinkName    := '';
    UID         := 0;
    GID         := 0;
    UserName    := '';
    GroupName   := '';
    ChecksumOK  := FALSE;
    Mode        := [];
    Magic       := '';
    MajorDevNo  := 0;
    MinorDevNo  := 0;
    FilePos     := 0;
    END;
END;

(*
===============================================================================================
TAR format
===============================================================================================
*)

CONST
  RECORDSIZE = 512;
  NAMSIZ     = 100;
  TUNMLEN    =  32;
  TGNMLEN    =  32;
  CHKBLANKS  = #32#32#32#32#32#32#32#32;

TYPE
  TTarHeader = PACKED RECORD
                 Name     : ARRAY [0..NAMSIZ-1] OF AnsiChar;
                 Mode     : ARRAY [0..7]  OF AnsiChar;
                 UID      : ARRAY [0..7]  OF AnsiChar;
                 GID      : ARRAY [0..7]  OF AnsiChar;
                 Size     : ARRAY [0..11] OF AnsiChar;
                 MTime    : ARRAY [0..11] OF AnsiChar;
                 ChkSum   : ARRAY [0..7]  OF AnsiChar;
                 LinkFlag : AnsiChar;
                 LinkName : ARRAY [0..NAMSIZ-1] OF AnsiChar;
                 Magic    : ARRAY [0..7] OF AnsiChar;
                 UName    : ARRAY [0..TUNMLEN-1] OF AnsiChar;
                 GName    : ARRAY [0..TGNMLEN-1] OF AnsiChar;
                 DevMajor : ARRAY [0..7] OF AnsiChar;
                 DevMinor : ARRAY [0..7] OF AnsiChar;
               END;

FUNCTION ExtractText (P : PAnsiChar) : AnsiString;
BEGIN
  Result := AnsiString (P);
END;


FUNCTION ExtractNumber (P : PAnsiChar) : INTEGER; OVERLOAD;
VAR
  Strg : AnsiString;
BEGIN
  Strg := AnsiString (Trim (string (P)));
  P := PAnsiChar (Strg);
  Result := 0;
  WHILE (P^ <> #32) AND (P^ <> #0) DO BEGIN
    Result := (ORD (P^) - ORD ('0')) OR (Result SHL 3);
    INC (P);
    END;
END;


FUNCTION ExtractNumber64 (P : PAnsiChar) : INT64; OVERLOAD;
VAR
  Strg : AnsiString;
BEGIN
  Strg := AnsiString (Trim (string (P)));
  P := PAnsiChar (Strg);
  Result := 0;
  WHILE (P^ <> #32) AND (P^ <> #0) DO BEGIN
    Result := (ORD (P^) - ORD ('0')) OR (Result SHL 3);
    INC (P);
    END;
END;



FUNCTION ExtractNumber (P : PAnsiChar; MaxLen : INTEGER) : INTEGER; OVERLOAD;
VAR
  S0   : ARRAY [0..255] OF AnsiChar;
  Strg : AnsiString;
BEGIN
  StrLCopy (S0, P, MaxLen);
  Strg := AnsiString (Trim (string (S0)));
  P := PAnsiChar (Strg);
  Result := 0;
  WHILE (P^ <> #32) AND (P^ <> #0) DO BEGIN
    Result := (ORD (P^) - ORD ('0')) OR (Result SHL 3);
    INC (P);
    END;
END;


FUNCTION ExtractNumber64 (P : PAnsiChar; MaxLen : INTEGER) : INT64; OVERLOAD;
VAR
  S0   : ARRAY [0..255] OF AnsiChar;
  Strg : AnsiString;
BEGIN
  StrLCopy (S0, P, MaxLen);
  Strg := AnsiString (Trim (string (S0)));
  P := PAnsiChar (Strg);
  Result := 0;
  WHILE (P^ <> #32) AND (P^ <> #0) DO BEGIN
    Result := (ORD (P^) - ORD ('0')) OR (Result SHL 3);
    INC (P);
    END;
END;


FUNCTION Records (Bytes : INT64) : INT64;
BEGIN
  Result := Bytes DIV RECORDSIZE;
  IF Bytes MOD RECORDSIZE > 0 THEN
    INC (Result);
END;


PROCEDURE Octal (N : INTEGER; P : PAnsiChar; Len : INTEGER);
         // Makes a string of octal digits
         // The string will always be "Len" characters long
VAR
  I : INTEGER;
BEGIN
  FOR I := Len-2 DOWNTO 0 DO BEGIN
    (P+I)^ := AnsiChar (ORD ('0') + ORD (N AND $07));
    N := N SHR 3;
    END;
  FOR I := 0 TO Len-3 DO
    IF (P+I)^ = '0'
      THEN (P+I)^ := #32
      ELSE BREAK;
  (P+Len-1)^ := #32;
END;


PROCEDURE Octal64 (N : INT64; P : PAnsiChar; Len : INTEGER);
         // Makes a string of octal digits
         // The string will always be "Len" characters long
VAR
  I     : INTEGER;
BEGIN
  FOR I := Len-2 DOWNTO 0 DO BEGIN
    (P+I)^ := AnsiChar (ORD ('0') + ORD (N AND $07));
    N := N SHR 3;
    END;
  FOR I := 0 TO Len-3 DO
    IF (P+I)^ = '0'
      THEN (P+I)^ := #32
      ELSE BREAK;
  (P+Len-1)^ := #32;
END;


PROCEDURE OctalN (N : INTEGER; P : PAnsiChar; Len : INTEGER);
BEGIN
  Octal (N, P, Len-1);
  (P+Len-1)^ := #0;
END;


PROCEDURE WriteTarHeader (Dest : TStream; DirRec : TTarDirRec);
VAR
  Rec      : ARRAY [0..RECORDSIZE-1] OF AnsiChar;
  TH       : TTarHeader ABSOLUTE Rec;
  Mode     : INTEGER;
  NullDate : TDateTime;
  Checksum : CARDINAL;
  I        : INTEGER;
BEGIN
  FillChar (Rec, RECORDSIZE, 0);
  StrLCopy (TH.Name, PAnsiChar (DirRec.Name), NAMSIZ);
  CASE DirRec.FileType OF
    ftNormal, ftLink  : Mode := $08000;
    ftSymbolicLink    : Mode := $0A000;
    ftDirectory       : Mode := $04000;
    ELSE                Mode := 0;
    END;
  IF tmSaveText IN DirRec.Mode THEN Mode := Mode OR $0200;
  IF tmSetGid   IN DirRec.Mode THEN Mode := Mode OR $0400;
  IF tmSetUid   IN DirRec.Mode THEN Mode := Mode OR $0800;
  IF tpReadByOwner    IN DirRec.Permissions THEN Mode := Mode OR $0100;
  IF tpWriteByOwner   IN DirRec.Permissions THEN Mode := Mode OR $0080;
  IF tpExecuteByOwner IN DirRec.Permissions THEN Mode := Mode OR $0040;
  IF tpReadByGroup    IN DirRec.Permissions THEN Mode := Mode OR $0020;
  IF tpWriteByGroup   IN DirRec.Permissions THEN Mode := Mode OR $0010;
  IF tpExecuteByGroup IN DirRec.Permissions THEN Mode := Mode OR $0008;
  IF tpReadByOther    IN DirRec.Permissions THEN Mode := Mode OR $0004;
  IF tpWriteByOther   IN DirRec.Permissions THEN Mode := Mode OR $0002;
  IF tpExecuteByOther IN DirRec.Permissions THEN Mode := Mode OR $0001;
  OctalN (Mode, @TH.Mode, 8);
  OctalN (DirRec.UID, @TH.UID, 8);
  OctalN (DirRec.GID, @TH.GID, 8);
  Octal64 (DirRec.Size, @TH.Size, 12);
  NullDate := EncodeDate (1970, 1, 1);
  IF DirRec.DateTime >= NullDate
    THEN Octal (Trunc ((DirRec.DateTime - NullDate) * 86400.0), @TH.MTime, 12)
    ELSE Octal (Trunc (                   NullDate  * 86400.0), @TH.MTime, 12);
  CASE DirRec.FileType OF
    ftNormal       : TH.LinkFlag := '0';
    ftLink         : TH.LinkFlag := '1';
    ftSymbolicLink : TH.LinkFlag := '2';
    ftCharacter    : TH.LinkFlag := '3';
    ftBlock        : TH.LinkFlag := '4';
    ftDirectory    : TH.LinkFlag := '5';
    ftFifo         : TH.LinkFlag := '6';
    ftContiguous   : TH.LinkFlag := '7';
    ftDumpDir      : TH.LinkFlag := 'D';
    ftMultiVolume  : TH.LinkFlag := 'M';
    ftVolumeHeader : TH.LinkFlag := 'V';
    END;
  StrLCopy (TH.LinkName, PAnsiChar (DirRec.LinkName), NAMSIZ);
  StrLCopy (TH.Magic, PAnsiChar (DirRec.Magic + #32#32#32#32#32#32#32#32), 8);
  StrLCopy (TH.UName, PAnsiChar (DirRec.UserName), TUNMLEN);
  StrLCopy (TH.GName, PAnsiChar (DirRec.GroupName), TGNMLEN);
  OctalN (DirRec.MajorDevNo, @TH.DevMajor, 8);
  OctalN (DirRec.MinorDevNo, @TH.DevMinor, 8);
  StrMove (TH.ChkSum, CHKBLANKS, 8);

  CheckSum := 0;
  FOR I := 0 TO SizeOf (TTarHeader)-1 DO
    INC (CheckSum, INTEGER (ORD (Rec [I])));
  OctalN (CheckSum, @TH.ChkSum, 8);

  Dest.Write (TH, RECORDSIZE);
END;



(*
===============================================================================================
TTarArchive
===============================================================================================
*)

CONSTRUCTOR TTarArchive.Create (Stream : TStream);
BEGIN
  INHERITED Create;
  FStream     := Stream;
  FOwnsStream := FALSE;
  Reset;
END;


CONSTRUCTOR TTarArchive.Create (Filename : STRING; FileMode : WORD);
BEGIN
  INHERITED Create;
  FStream     := TFileStream.Create (Filename, FileMode);
  FOwnsStream := TRUE;
  Reset;
END;


DESTRUCTOR TTarArchive.Destroy;
BEGIN
  IF FOwnsStream THEN
    FStream.Free;
  INHERITED Destroy;
END;


PROCEDURE TTarArchive.Reset;
          // Reset File Pointer
BEGIN
  FStream.Position := 0;
  FBytesToGo       := 0;
END;


FUNCTION  TTarArchive.FindNext (VAR DirRec : TTarDirRec) : BOOLEAN;
          // Reads next Directory Info Record
          // The Stream pointer must point to the first byte of the tar header
VAR
  Rec          : ARRAY [0..RECORDSIZE-1] OF CHAR;
  CurFilePos   : INTEGER;
  Header       : TTarHeader ABSOLUTE Rec;
  I            : INTEGER;
  HeaderChkSum : WORD;
  Checksum     : CARDINAL;
BEGIN
  // --- Scan until next pointer
  IF FBytesToGo > 0 THEN
    FStream.Seek (Records (FBytesToGo) * RECORDSIZE, soFromCurrent);

  // --- EOF reached?
  Result := FALSE;
  CurFilePos := FStream.Position;
  TRY
    FStream.ReadBuffer (Rec, RECORDSIZE);
    if Rec [0] = #0 THEN EXIT;   // EOF reached
  EXCEPT
    EXIT;   // EOF reached, too
    END;
  Result := TRUE;

  ClearDirRec (DirRec);

  DirRec.FilePos := CurFilePos;
  DirRec.Name := ExtractText (Header.Name);
  DirRec.Size := ExtractNumber64 (@Header.Size, 12);
  DirRec.DateTime := EncodeDate (1970, 1, 1) + (ExtractNumber (@Header.MTime, 12) / 86400.0);
  I := ExtractNumber (@Header.Mode);
  IF I AND $0100 <> 0 THEN Include (DirRec.Permissions, tpReadByOwner);
  IF I AND $0080 <> 0 THEN Include (DirRec.Permissions, tpWriteByOwner);
  IF I AND $0040 <> 0 THEN Include (DirRec.Permissions, tpExecuteByOwner);
  IF I AND $0020 <> 0 THEN Include (DirRec.Permissions, tpReadByGroup);
  IF I AND $0010 <> 0 THEN Include (DirRec.Permissions, tpWriteByGroup);
  IF I AND $0008 <> 0 THEN Include (DirRec.Permissions, tpExecuteByGroup);
  IF I AND $0004 <> 0 THEN Include (DirRec.Permissions, tpReadByOther);
  IF I AND $0002 <> 0 THEN Include (DirRec.Permissions, tpWriteByOther);
  IF I AND $0001 <> 0 THEN Include (DirRec.Permissions, tpExecuteByOther);
  IF I AND $0200 <> 0 THEN Include (DirRec.Mode, tmSaveText);
  IF I AND $0400 <> 0 THEN Include (DirRec.Mode, tmSetGid);
  IF I AND $0800 <> 0 THEN Include (DirRec.Mode, tmSetUid);
  CASE Header.LinkFlag OF
    #0, '0' : DirRec.FileType := ftNormal;
    '1'     : DirRec.FileType := ftLink;
    '2'     : DirRec.FileType := ftSymbolicLink;
    '3'     : DirRec.FileType := ftCharacter;
    '4'     : DirRec.FileType := ftBlock;
    '5'     : DirRec.FileType := ftDirectory;
    '6'     : DirRec.FileType := ftFifo;
    '7'     : DirRec.FileType := ftContiguous;
    'D'     : DirRec.FileType := ftDumpDir;
    'M'     : DirRec.FileType := ftMultiVolume;
    'V'     : DirRec.FileType := ftVolumeHeader;
    END;
  DirRec.LinkName   := ExtractText (Header.LinkName);
  DirRec.UID        := ExtractNumber (@Header.UID);
  DirRec.GID        := ExtractNumber (@Header.GID);
  DirRec.UserName   := ExtractText (Header.UName);
  DirRec.GroupName  := ExtractText (Header.GName);
  DirRec.Magic      := AnsiString (Trim (string (Header.Magic)));
  DirRec.MajorDevNo := ExtractNumber (@Header.DevMajor);
  DirRec.MinorDevNo := ExtractNumber (@Header.DevMinor);

  HeaderChkSum := ExtractNumber (@Header.ChkSum);   // Calc Checksum
  CheckSum := 0;
  StrMove (Header.ChkSum, CHKBLANKS, 8);
  FOR I := 0 TO SizeOf (TTarHeader)-1 DO
    INC (CheckSum, INTEGER (ORD (Rec [I])));
  DirRec.CheckSumOK := WORD (CheckSum) = WORD (HeaderChkSum);

  IF DirRec.FileType in [ftLink, ftSymbolicLink, ftDirectory, ftFifo, ftVolumeHeader]
    THEN FBytesToGo := 0
    ELSE FBytesToGo := DirRec.Size;
END;


PROCEDURE TTarArchive.ReadFile (Buffer : POINTER);
          // Reads file data for the last Directory Record. The entire file is read into the buffer.
          // The buffer must be large enough to take up the whole file.
VAR
  RestBytes : INTEGER;
BEGIN
  IF FBytesToGo = 0 THEN EXIT;
  RestBytes := Records (FBytesToGo) * RECORDSIZE - FBytesToGo;
  FStream.ReadBuffer (Buffer^, FBytesToGo);
  FStream.Seek (RestBytes, soFromCurrent);
  FBytesToGo := 0;
END;


PROCEDURE TTarArchive.ReadFile (Stream : TStream);
          // Reads file data for the last Directory Record.
          // The entire file is written out to the stream.
          // The stream is left at its current position prior to writing
VAR
  RestBytes : INTEGER;
BEGIN
  IF FBytesToGo = 0 THEN EXIT;
  RestBytes := Records (FBytesToGo) * RECORDSIZE - FBytesToGo;
  Stream.CopyFrom (FStream, FBytesToGo);
  FStream.Seek (RestBytes, soFromCurrent);
  FBytesToGo := 0;
END;


PROCEDURE TTarArchive.ReadFile (Filename : STRING);
          // Reads file data for the last Directory Record.
          // The entire file is saved in the given Filename
VAR
  FS : TFileStream;
BEGIN
  FS := TFileStream.Create (Filename, fmCreate);
  TRY
    ReadFile (FS);
  FINALLY
    FS.Free;
    END;
END;


FUNCTION  TTarArchive.ReadFile : RawByteString;
          // Reads file data for the last Directory Record. The entire file is returned
          // as a large ANSI string.
VAR
  RestBytes : INTEGER;
BEGIN
  IF FBytesToGo = 0 THEN EXIT;
  RestBytes := Records (FBytesToGo) * RECORDSIZE - FBytesToGo;
  SetLength (Result, FBytesToGo);
  FStream.ReadBuffer (PAnsiChar (Result)^, FBytesToGo);
  FStream.Seek (RestBytes, soFromCurrent);
  FBytesToGo := 0;
END;


PROCEDURE TTarArchive.GetFilePos (VAR Current, Size : INT64);
          // Returns the Current Position in the TAR stream
BEGIN
  Current := FStream.Position;
  Size    := FStream.Size;
END;


PROCEDURE TTarArchive.SetFilePos (NewPos : INT64);                   // Set new Current File Position
BEGIN
  IF NewPos < FStream.Size THEN
    FStream.Seek (NewPos, soFromBeginning);
END;


(*
===============================================================================================
TTarWriter
===============================================================================================
*)


CONSTRUCTOR TTarWriter.CreateEmpty;
VAR
  TP : TTarPermission;
BEGIN
  INHERITED Create;
  FOwnsStream  := FALSE;
  FFinalized   := FALSE;
  FPermissions := [];
  FOR TP := Low (TP) TO High (TP) DO
    Include (FPermissions, TP);
  FUID       := 0;
  FGID       := 0;
  FUserName  := '';
  FGroupName := '';
  FMode      := [];
  FMagic     := 'ustar';
END;

CONSTRUCTOR TTarWriter.Create (TargetStream   : TStream);
BEGIN
  CreateEmpty;
  FStream     := TargetStream;
  FOwnsStream := FALSE;
END;


CONSTRUCTOR TTarWriter.Create (TargetFilename : STRING; Mode : INTEGER = fmCreate);
BEGIN
  CreateEmpty;
  FStream     := TFileStream.Create (TargetFilename, Mode);
  FOwnsStream := TRUE;
END;


DESTRUCTOR TTarWriter.Destroy;
BEGIN
  IF NOT FFinalized THEN BEGIN
    Finalize;
    FFinalized := TRUE;
    END;
  IF FOwnsStream THEN
    FStream.Free;
  INHERITED Destroy;
END;


PROCEDURE TTarWriter.AddFile   (Filename : STRING;  TarFilename : AnsiString = '');
VAR
  S    : TFileStream;
  Date : TDateTime;
BEGIN
  Date := FileTimeGMT (Filename);
  IF TarFilename = ''
    THEN TarFilename := AnsiString (ConvertFilename (Filename))
    ELSE TarFilename := AnsiString (ConvertFilename (string (TarFilename)));
  S := TFileStream.Create (Filename, fmOpenRead OR fmShareDenyWrite);
  TRY
    AddStream (S, TarFilename, Date);
  FINALLY
    S.Free
    END;
END;


PROCEDURE TTarWriter.AddStream (Stream : TStream; TarFilename : AnsiString; FileDateGmt : TDateTime);
VAR
  DirRec      : TTarDirRec;
  Rec         : ARRAY [0..RECORDSIZE-1] OF CHAR;
  BytesToRead : INT64;      // Bytes to read from the Source Stream
  BlockSize   : INT64;      // Bytes to write out for the current record
BEGIN
  ClearDirRec (DirRec);
  DirRec.Name        := TarFilename;
  DirRec.Size        := Stream.Size - Stream.Position;
  DirRec.DateTime    := FileDateGmt;
  DirRec.Permissions := FPermissions;
  DirRec.FileType    := ftNormal;
  DirRec.LinkName    := '';
  DirRec.UID         := FUID;
  DirRec.GID         := FGID;
  DirRec.UserName    := FUserName;
  DirRec.GroupName   := FGroupName;
  DirRec.ChecksumOK  := TRUE;
  DirRec.Mode        := FMode;
  DirRec.Magic       := FMagic;
  DirRec.MajorDevNo  := 0;
  DirRec.MinorDevNo  := 0;

  WriteTarHeader (FStream, DirRec);
  BytesToRead := DirRec.Size;
  WHILE BytesToRead > 0 DO BEGIN
    BlockSize := BytesToRead;
    IF BlockSize > RECORDSIZE THEN BlockSize := RECORDSIZE;
    FillChar (Rec, RECORDSIZE, 0);
    Stream.Read (Rec, BlockSize);
    FStream.Write (Rec, RECORDSIZE);
    DEC (BytesToRead, BlockSize);
    END;
END;


PROCEDURE TTarWriter.AddString (Contents : RawByteString; TarFilename : AnsiString; FileDateGmt : TDateTime);
VAR
  S : TStringStream;
BEGIN
  S := TStringStream.Create (Contents);
  TRY
    AddStream (S, TarFilename, FileDateGmt);
  FINALLY
    S.Free
    END
END;


PROCEDURE TTarWriter.AddDir (Dirname : AnsiString; DateGmt : TDateTime; MaxDirSize : INT64 = 0);
VAR
  DirRec      : TTarDirRec;
BEGIN
  ClearDirRec (DirRec);
  DirRec.Name        := Dirname;
  DirRec.Size        := MaxDirSize;
  DirRec.DateTime    := DateGmt;
  DirRec.Permissions := FPermissions;
  DirRec.FileType    := ftDirectory;
  DirRec.LinkName    := '';
  DirRec.UID         := FUID;
  DirRec.GID         := FGID;
  DirRec.UserName    := FUserName;
  DirRec.GroupName   := FGroupName;
  DirRec.ChecksumOK  := TRUE;
  DirRec.Mode        := FMode;
  DirRec.Magic       := FMagic;
  DirRec.MajorDevNo  := 0;
  DirRec.MinorDevNo  := 0;

  WriteTarHeader (FStream, DirRec);
END;


PROCEDURE TTarWriter.AddSymbolicLink (Filename, Linkname : AnsiString; DateGmt : TDateTime);
VAR
  DirRec : TTarDirRec;
BEGIN
  ClearDirRec (DirRec);
  DirRec.Name        := Filename;
  DirRec.Size        := 0;
  DirRec.DateTime    := DateGmt;
  DirRec.Permissions := FPermissions;
  DirRec.FileType    := ftSymbolicLink;
  DirRec.LinkName    := Linkname;
  DirRec.UID         := FUID;
  DirRec.GID         := FGID;
  DirRec.UserName    := FUserName;
  DirRec.GroupName   := FGroupName;
  DirRec.ChecksumOK  := TRUE;
  DirRec.Mode        := FMode;
  DirRec.Magic       := FMagic;
  DirRec.MajorDevNo  := 0;
  DirRec.MinorDevNo  := 0;

  WriteTarHeader (FStream, DirRec);
END;


PROCEDURE TTarWriter.AddLink (Filename, Linkname : AnsiString; DateGmt : TDateTime);
VAR
  DirRec : TTarDirRec;
BEGIN
  ClearDirRec (DirRec);
  DirRec.Name        := Filename;
  DirRec.Size        := 0;
  DirRec.DateTime    := DateGmt;
  DirRec.Permissions := FPermissions;
  DirRec.FileType    := ftLink;
  DirRec.LinkName    := Linkname;
  DirRec.UID         := FUID;
  DirRec.GID         := FGID;
  DirRec.UserName    := FUserName;
  DirRec.GroupName   := FGroupName;
  DirRec.ChecksumOK  := TRUE;
  DirRec.Mode        := FMode;
  DirRec.Magic       := FMagic;
  DirRec.MajorDevNo  := 0;
  DirRec.MinorDevNo  := 0;

  WriteTarHeader (FStream, DirRec);
END;


PROCEDURE TTarWriter.AddVolumeHeader (VolumeId : AnsiString; DateGmt : TDateTime);
VAR
  DirRec : TTarDirRec;
BEGIN
  ClearDirRec (DirRec);
  DirRec.Name        := VolumeId;
  DirRec.Size        := 0;
  DirRec.DateTime    := DateGmt;
  DirRec.Permissions := FPermissions;
  DirRec.FileType    := ftVolumeHeader;
  DirRec.LinkName    := '';
  DirRec.UID         := FUID;
  DirRec.GID         := FGID;
  DirRec.UserName    := FUserName;
  DirRec.GroupName   := FGroupName;
  DirRec.ChecksumOK  := TRUE;
  DirRec.Mode        := FMode;
  DirRec.Magic       := FMagic;
  DirRec.MajorDevNo  := 0;
  DirRec.MinorDevNo  := 0;

  WriteTarHeader (FStream, DirRec);
END;


PROCEDURE TTarWriter.Finalize;
          // Writes the End-Of-File Tag
          // Data after this tag will be ignored
          // The destructor calls this automatically if you didn't do it before
VAR
  Rec : ARRAY [0..RECORDSIZE-1] OF CHAR;
BEGIN
  FillChar (Rec, SizeOf (Rec), 0);
  FStream.Write (Rec, RECORDSIZE);
  FFinalized := TRUE;
END;


END.

