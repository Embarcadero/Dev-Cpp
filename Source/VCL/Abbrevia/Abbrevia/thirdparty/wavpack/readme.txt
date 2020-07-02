WavPack v4.60.1

Compile using
   bcc32 -DWIN32 -DNO_USE_FSTREAMS -c -u- -w-8004 -w-8012 -w-8017 -w-8057 -w-8065 *.c

In wavpack_local.h remove the line "#define FASTCALL __fastcall"
