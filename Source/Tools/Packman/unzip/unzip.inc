{*********************************************************************
 *
 * Global Conditional Definitions for Chief's UNZIP package
 * By Prof. Abimbola Olowofoyeku (The African Chief)
 *          http://www.bigfoot.com/~African_Chief/
 *
 **********************************************************************
 }

{.$define Debug}           { uncomment for debugging }

{$ifdef Win32}
   {$define MSWindows}
{$endif}

{$ifdef Windows}
   {$define MSWindows}
{$endif}


{$ifndef fpc}
    {$I-}
    {$W-}
    {$X+}
 {$ifndef __GPC__}
  {$IFDEF Debug}
    {$F+,D+,Q-,L+,R+,I-,S+,Y+,A+}
  {$ELSE}
    {$F+,Q-,R-,S-,I-,A+}
  {$ENDIF}
 {$endif} {__GPC__}
{$endif} {fpc}


{$ifdef ver70}
   {$ifdef Windows} {$define Win16}      {$endif Windows}
   {$ifdef MSDOS}   {$define NO_EXPORTS} {$endif MSDOS}
   {$ifdef DPMI}    {$define BP_DPMI}    {$endif}
   {$define OS_16_BIT}
   {$define __OS_DOS__}
{$endif ver70}

{$ifdef ver80}
   {$define Delphi}      {Delphi 1.x}
   {$define Delphi16}
   {$define Win16}
   {$define OS_16_BIT}
   {$define __OS_DOS__}
{$endif ver80}

{$ifdef ver90}
   {$define Delphi}      {Delphi 2.x}
   {$define Delphi32}
{$endif ver90}

{$ifdef ver93}
   {$define Delphi}      {C++ Builder 1.x}
   {$define Delphi32}
{$endif ver93}

{$ifdef ver100}
   {$define Delphi}      {Delphi 3.x}
   {$define Delphi32}
{$endif ver100}

{$ifdef ver110}
   {$define Delphi}      {C++ Builder 3.x}
   {$define Delphi32}
{$endif ver110}

{$ifdef ver120}
   {$define Delphi}      {Delphi 4.x}
   {$define Delphi32}
   {$define Has_Int64}
{$endif ver120}

{$ifdef ver130}
   {$define Delphi}      {Delphi 5.x}
   {$define Delphi32}
   {$define Has_Int64}
{$endif ver130}

{$ifdef ver140}
   {$define Delphi}      {Delphi 6.x}
   {$define Delphi32}
   {$define Has_Int64}
   {$define Has_Longword}
{$endif ver140}

{$ifdef ver150}
   {$define Delphi}      {Delphi 7.x}
   {$define Delphi32}
   {$define Has_Int64}
   {$define Has_Longword}
{$endif ver150}

{$ifdef ver160}
   {$define Delphi}      {Delphi 8.x}
   {$define Delphi32}
   {$define Has_Int64}
   {$define Has_Longword}
{$endif ver150}

{$ifdef VirtualPascal} {  Virtual Pascal 2.x }
   {$define Delphi}    {  Use Delphi Syntax }
   {$define VP2}
   {&Delphi+}
   {$define OS_BigMem}
   {$undef assembler}
{$endif VirtualPascal}

{$ifdef Delphi}
  {$define Windows}
  {$define USE_STDCALL}
{$endif Delphi}

{$ifdef FPC}
  {$MODE Delphi}          { use Delphi compatibility mode }
  {$define Delphi}
  {$define UseAT}
  {$undef USE_STDCALL}
  {$define OS_BigMem}
  {$define NO_EXPORTS}
  {$define NOCRT}
  {$define Has_Int64}
  {$ifdef linux}
     {$define fpc_linux}
  {$else}
     {$define UseWin}
     {$define __OS_DOS__}
  {$endif}
{$endif FPC}

{$ifdef Win16}
  {$K+}   {smart callbacks}
{$endif Win16}

 {$ifdef OS2}
    {$undef Windows}
    {$undef MSWINDOWS}
    {$define UseWin}
    {$define OS_BigMem}
 {$endif OS2}

{$ifdef __GPC__}
  {$undef UseWin}
  {$undef USE_STDCALL}
  {$define OS_BigMem}
  {$define NO_EXPORTS}
  {$define NOCRT}
  {$ifdef __MSYS__}
     {$define _WIN32}
  {$endif}
  {$ifdef __CYGWIN__}
     {$define _WIN32}
  {$endif}

  {$ifdef _WIN32}
    {$define Stdcall attribute(stdcall)}
    {$define WINAPI(X) external name X}
    {$define GPC_Win32}
    {$define CanUseWinApi}
  {$endif}
{$endif}{ __GPC__}

{$ifdef __TMT__}
  {$define OS_BigMem}
  {$define NO_EXPORTS}
  {$define __OS_DOS__}
  {$define UseAT}
  {$ifndef MSDOS}{$define USE_STDCALL}{$endif}

   {$ifdef __WIN32__}
     {$define Win32}
     {$define UseWin}
     {$define NOCRT}
     {$define Win32}
     {$ifndef __CON__}
       {$define Windows}
     {$endif}
   {$endif}
{$endif}

{$ifdef Win32}
  {$H-}                    { no long strings, please! }
  {$define OS_BigMem}
  {$define CanUseWinApi}
{$else Win32}
  {$ifdef ver70}{$define assembler}{$endif} { use 16-bit assembler! }
{$endif Win32}

{ ************************** dos/dos-like platforms **************}
{$ifdef Windows}
   {$define __OS_DOS__}
   {$define UseWin}
   {$define MSWINDOWS}
{$endif Delphi}

{$ifdef OS2}
   {$define __OS_DOS__}
   {$define Can_Use_DLL}
{$endif Delphi}

{$ifdef UseWin}
   {$define Can_Use_DLL}
{$endif}

{$ifdef Win16}
   {$define Can_Use_DLL}
   {$define CanUseWinApi}
{$endif}

{$ifdef BP_DPMI}
   {$define Can_Use_DLL}
{$endif}

{$ifdef USE_STDCALL}
   {$ifndef __TMT__}
     {$define BY_NAME}
   {$endif}
{$endif}

{$ifdef Delphi32}
  {$H-}
  {$ifdef Linux}
     {$define kylix}
  {$endif}
  {$define OS_BigMem}
  {$ifdef kylix}
   {$undef __OS_DOS__}
   {$undef UseWin}
   {$undef MSWINDOWS}
   {$define NOCRT}
  {$else}
    {$define Win32}
  {$endif}
  {$undef assembler}
{$endif}


{ *********** charset conversions *************** }
{ no conversion under BP/TP7 for DOS and DPMI }

{$ifdef MSWindows}
  {$define Charset_Display_Conv}      { convert for displaying }
  {$ifndef VirtualPascal}
    {$define Charset_FileName_Conv}   { convert for file creation }
  {$endif}
{$endif}


{$ifdef GPC_Win32}
  {$define Charset_FileName_Conv}
  {$ifndef __MINGW32__}
    {$define Charset_Display_Conv}
  {$endif}
{$endif}


{$ifdef VirtualPascal}
  {$ifdef linux}
    {$undef MSWINDOWS}
    {$define NOCRT}
  {$endif}
{$endif}
