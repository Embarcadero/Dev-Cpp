This directory includes Unicode filename examples.  To run the tests create a "source" directory and extract any of the "OEM & ANSI" zips into it using Winzip 12 or newer.

The "OEM & ANSI" versions were created using WinZip 12 running on various system encodings.  They use a mix of OEM, ANSI, and UTF-8 encodings, and the Info-Zip Unicode path extended header.  The WinRar one was created with WinRar and uses UTF-8 for all extended filenames, with the language encoding bit of the general purpose bit flag set.  The Xceed one was created using the Xceed zip compression library and uses a Unicode extended header that is different than the documented Info-Zip one.

These tests are not run on Delphi 2007 and earlier, because they only support ANSI characters.  The filenames are decoded correctly, but the ANSI conversion makes the comparisons in the test fail.  To enable them we would need separate directories for any ANSI codepages we wanted to run.

Filenames:
Á (ANSI 1250, ANSI 1252)
À (ANSI 1252)
a (ASCII)
â (OEM 437, ANSI 1250, ANSI 1252)
ÀŁ (ANSI 1252 + ANSI 1250)
âŁ (OEM 437 + ANSI 1250)
Ł (ANSI 1250)
本 (ANSI 932)