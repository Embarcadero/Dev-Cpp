LZMA SDK v9.20

Original download available from http://downloads.sourceforge.net/sevenzip/lzma920.tar.bz2

Added #pragma pack(push, 1) / #pragma pack(pop) to LzmaDec.h, LzmaEnc.h, Types.h
Added windows.h function redefinitions in Threads.h for 64-bit compatibility

Compile 32-bit with C++Builder:
  bcc32 -q -c -u- -w-8004 -w-8065 *.c

Compile 64-bit with Visual Studio:
  cl -c -nologo -GS- -Z7 -Gs32768 -Ox *.c