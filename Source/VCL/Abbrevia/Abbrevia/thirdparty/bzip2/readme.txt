bzip2 SDK v1.0.6

Original download available from http://bzip.org/1.0.6/bzip2-1.0.6.tar.gz

Compile 32-bit with
  bcc32 -q -c -u- -w-8004 -w-8008 -w-8057 -w-8066 -w-8068 -DBZ_NO_STDIO *.c

Compile 64-bit with
  cl -c -nologo -GS- -Z7 -wd4068 -Gs32768 -DBZ_NO_STDIO *.c