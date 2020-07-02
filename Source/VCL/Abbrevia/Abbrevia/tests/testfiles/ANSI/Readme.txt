These test files use a subset of characters that are supported by both OEM 437 and ANSI 1252, in order to test .gz/.tar filename decoding.  They will work on Delphi 2007 and earlier.  The filenames are encoded in each archive as indicated by their names (OEM 437, ANSI 1252, UTF-8), which are the ANSI/OEM codepages for US English.

I've looked into filename encoding behavior in various Windows applications, and there isn't any agreement, so when writing new archives Abbrevia always uses UTF-8 on Windows and OS X.

RFC 1952 says gzips should use ISO 8859-1 (Latin-1), but no one actually does that.

GNU gzip/tar (Unix):
  Filenames are encoded/decoded using the system "C" encoding.  On Unix this is usually UTF-8 now, but up through the late 90's it was generally one of the other ISO encodings.

Cygwin:
  Filenames are encoded/decoded as UTF-8.  This only changed in Cygwin 1.7, released Dec 2009.  Prior to that it used the Windows ANSI encoding.

GnuWin32:
  Filenames are encoded/decoded as using the Windows ANSI encoding.

7-zip:
  Gzips are encoded/decoded using the Windows ANSI encoding.
  Tars are encoded/decoded using the Windows OEM encoding.

IZArc:
  Filenames are encoded using the Windows OEM encoding, or the Windows ANSI encoding, or UTF-8, on a filename-by-filename basis depending on which encoding (in that order) can store the filename losslessly.

  Filenames are decoded by using UTF-8 for any filenames that are valid UTF-8 and the Windows ANSI encoding otherwise.  Since it doesn't detect OEM strings, it can mis-read files it's written itself.

PowerArchiver:
  Filenames are encoded/decoded using the Windows OEM encoding.  It will detect valid UTF-8 when decoding gzips, but not tars.

WinRAR:
  Filenames are decoded using the Windows ANSI encoding.

WinZip:
  Gzips are decoded using the Windows OEM encoding.
  Tars are decoded using the Windows ANSI encoding.
  Note that this is the opposite of 7-zip. 