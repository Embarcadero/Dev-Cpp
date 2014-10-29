(*

Fast Memory Manager 4.991

Description:
 A fast replacement memory manager for Embarcadero Delphi Win32 applications
 that scales well under multi-threaded usage, is not prone to memory
 fragmentation, and supports shared memory without the use of external .DLL
 files.

Homepage:
 http://fastmm.sourceforge.net

Advantages:
 - Fast
 - Low overhead. FastMM is designed for an average of 5% and maximum of 10%
   overhead per block.
 - Supports up to 3GB of user mode address space under Windows 32-bit and 4GB
   under Windows 64-bit. Add the "$SetPEFlags $20" option (in curly braces)
   to your .dpr to enable this.
 - Highly aligned memory blocks. Can be configured for either 8-byte or 16-byte
   alignment.
 - Good scaling under multi-threaded applications
 - Intelligent reallocations. Avoids slow memory move operations through
   not performing unneccesary downsizes and by having a minimum percentage
   block size growth factor when an in-place block upsize is not possible.
 - Resistant to address space fragmentation
 - No external DLL required when sharing memory between the application and
   external libraries (provided both use this memory manager)
 - Optionally reports memory leaks on program shutdown. (This check can be set
   to be performed only if Delphi is currently running on the machine, so end
   users won't be bothered by the error message.)
 - Supports Delphi 4 (or later), C++ Builder 4 (or later), Kylix 3.

Usage:
 Delphi:
  Place this unit as the very first unit under the "uses" section in your
  project's .dpr file. When sharing memory between an application and a DLL
  (e.g. when passing a long string or dynamic array to a DLL function), both the
  main application and the DLL must be compiled using this memory manager (with
  the required conditional defines set). There are some conditional defines
  (inside FastMM4Options.inc) that may be used to tweak the memory manager. To
  enable support for a user mode address space greater than 2GB you will have to
  use the EditBin* tool to set the LARGE_ADDRESS_AWARE flag in the EXE header.
  This informs Windows x64 or Windows 32-bit (with the /3GB option set) that the
  application supports an address space larger than 2GB (up to 4GB). In Delphi 6
  and later you can also specify this flag through the compiler directive
  {$SetPEFlags $20}
  *The EditBin tool ships with the MS Visual C compiler.
 C++ Builder 6:
  Refer to the instructions inside FastMM4BCB.cpp.

License:
 This work is copyright Professional Software Development / Pierre le Riche. It
 is released under a dual license, and you may choose to use it under either the
 Mozilla Public License 1.1 (MPL 1.1, available from
 http://www.mozilla.org/MPL/MPL-1.1.html) or the GNU Lesser General Public
 License 2.1 (LGPL 2.1, available from
 http://www.opensource.org/licenses/lgpl-license.php). If you find FastMM useful
 or you would like to support further development, a donation would be much
 appreciated. My banking details are:
   Country: South Africa
   Bank: ABSA Bank Ltd
   Branch: Somerset West
   Branch Code: 334-712
   Account Name: PSD (Distribution)
   Account No.: 4041827693
   Swift Code: ABSAZAJJ
 My PayPal account is:
   bof@psd.co.za

Contact Details:
 My contact details are shown below if you would like to get in touch with me.
 If you use this memory manager I would like to hear from you: please e-mail me
 your comments - good and bad.
 Snailmail:
   PO Box 2514
   Somerset West
   7129
   South Africa
 E-mail:
   plr@psd.co.za

Support:
 If you have trouble using FastMM, you are welcome to drop me an e-mail at the
 address above, or you may post your questions in the BASM newsgroup on the
 Embarcadero news server (which is where I hang out quite frequently).

Disclaimer:
 FastMM has been tested extensively with both single and multithreaded
 applications on various hardware platforms, but unfortunately I am not in a
 position to make any guarantees. Use it at your own risk.

Acknowledgements (for version 4):
 - Eric Grange for his RecyclerMM on which the earlier versions of FastMM were
   based. RecyclerMM was what inspired me to try and write my own memory
   manager back in early 2004.
 - Primoz Gabrijelcic for helping to track down various bugs.
 - Dennis Christensen for his tireless efforts with the Fastcode project:
   helping to develop, optimize and debug the growing Fastcode library.
 - JiYuan Xie for implementing the leak reporting code for C++ Builder.
 - Sebastian Zierer for implementing the OS X support.
 - Pierre Y. for his suggestions regarding the extension of the memory leak
   checking options.
 - Hanspeter Widmer for his suggestion to have an option to display install and
   uninstall debug messages and moving options to a separate file, as well as
   the new usage tracker.
 - Anders Isaksson and Greg for finding and identifying the "DelphiIsRunning"
   bug under Delphi 5.
 - Francois Malan for various suggestions and bug reports.
 - Craig Peterson for helping me identify the cache associativity issues that
   could arise due to medium blocks always being an exact multiple of 256 bytes.
   Also for various other bug reports and enhancement suggestions.
 - Jarek Karciarz, Vladimir Ulchenko (Vavan) and Bob Gonder for their help in
   implementing the BCB support.
 - Ben Taylor for his suggestion to display the object class of all memory
   leaks.
 - Jean Marc Eber and Vincent Mahon (the Memcheck guys) for the call stack
   trace code and also the method used to catch virtual method calls on freed
   objects.
 - Nahan Hyn for the suggestion to be able to enable or disable memory leak
   reporting through a global variable (the "ManualLeakReportingControl"
   option.)
 - Leonel Togniolli for various suggestions with regard to enhancing the bug
   tracking features of FastMM and other helpful advice.
 - Joe Bain and Leonel Togniolli for the workaround to QC#10922 affecting
   compilation under Delphi 2005.
 - Robert Marquardt for the suggestion to make localisation of FastMM easier by
   having all string constants together.
 - Simon Kissel and Fikret Hasovic for their help in implementing Kylix support.
 - Matthias Thoma, Petr Vones, Robert Rossmair and the rest of the JCL team for
   their debug info library used in the debug info support DLL and also the
   code used to check for a valid call site in the "raw" stack trace code.
 - Andreas Hausladen for the suggestion to use an external DLL to enable the
   reporting of debug information.
 - Alexander Tabakov for various good suggestions regarding the debugging
   facilities of FastMM.
 - M. Skloff for some useful suggestions and bringing to my attention some
   compiler warnings.
 - Martin Aignesberger for the code to use madExcept instead of the JCL library
   inside the debug info support DLL.
 - Diederik and Dennis Passmore for the suggestion to be able to register
   expected leaks.
 - Dario Tiraboschi and Mark Gebauer for pointing out the problems that occur
   when range checking and complete boolean evaluation is turned on.
 - Arthur Hoornweg for notifying me of the image base being incorrect for
   borlndmm.dll.
 - Theo Carr-Brion and Hanspeter Widmer for finding the false alarm error
   message "Block Header Has Been Corrupted" bug in FullDebugMode.
 - Danny Heijl for reporting the compiler error in "release" mode.
 - Omar Zelaya for reporting the BCB support regression bug.
 - Dan Miser for various good suggestions, e.g. not logging expected leaks to
   file, enhancements the stack trace and messagebox functionality, etc.
 - Arjen de Ruijter for fixing the bug in GetMemoryLeakType that caused it
   to not properly detect expected leaks registered by class when in
   "FullDebugMode".
 - Aleksander Oven for reporting the installation problem when trying to use
   FastMM in an application together with libraries that all use runtime
   packages.
 - Kristofer Skaug for reporting the bug that sometimes causes the leak report
   to be shown, even when all the leaks have been registered as expected leaks.
   Also for some useful enhancement suggestions.
 - Günther Schoch for the "RequireDebuggerPresenceForLeakReporting" option.
 - Jan Schlüter for the "ForceMMX" option.
 - Hallvard Vassbotn for various good enhancement suggestions.
 - Mark Edington for some good suggestions and bug reports.
 - Paul Ishenin for reporting the compilation error when the NoMessageBoxes
   option is set and also the missing call stack entries issue when "raw" stack
   traces are enabled, as well as for the Russian translation.
 - Cristian Nicola for reporting the compilation bug when the
   CatchUseOfFreedInterfaces option was enabled (4.40).
 - Mathias Rauen (madshi) for improving the support for madExcept in the debug
   info support DLL.
 - Roddy Pratt for the BCB5 support code.
 - Rene Mihula for the Czech translation and the suggestion to have dynamic
   loading of the FullDebugMode DLL as an option.
 - Artur Redzko for the Polish translation.
 - Bart van der Werf for helping me solve the DLL unload order problem when
   using the debug mode borlndmm.dll library, as well as various other
   suggestions.
 - JRG ("The Delphi Guy") for the Spanish translation.
 - Justus Janssen for Delphi 4 support.
 - Vadim Lopushansky and Charles Vinal for reporting the Delphi 5 compiler
   error in version 4.50.
 - Johni Jeferson Capeletto for the Brazilian Portuguese translation.
 - Kurt Fitzner for reporting the BCB6 compiler error in 4.52.
 - Michal Niklas for reporting the Kylix compiler error in 4.54.
 - Thomas Speck and Uwe Queisser for German translations.
 - Zaenal Mutaqin for the Indonesian translation.
 - Carlos Macao for the Portuguese translation.
 - Michael Winter for catching the performance issue when reallocating certain
   block sizes.
 - dzmitry[li] for the Belarussian translation.
 - Marcelo Montenegro for the updated Spanish translation.
 - Jud Cole for finding and reporting the bug which may trigger a read access
   violation when upsizing certain small block sizes together with the
   "UseCustomVariableSizeMoveRoutines" option.
 - Zdenek Vasku for reporting and fixing the memory manager sharing bug
   affecting Windows 95/98/Me.
 - RB Winston for suggesting the improvement to GExperts "backup" support.
 - Thomas Schulz for reporting the bug affecting large address space support
   under FullDebugMode, as well as the recursive call bug when attempting to
   report memory leaks when EnableMemoryLeakReporting is disabled.
 - Luigi Sandon for the Italian translation.
 - Werner Bochtler for various suggestions and bug reports.
 - Markus Beth for suggesting the "NeverSleepOnThreadContention" option.
 - JiYuan Xie for the Simplified Chinese translation.
 - Andrey Shtukaturov for the updated Russian translation, as well as the
   Ukrainian translation.
 - Dimitry Timokhov for finding two elusive bugs in the memory leak class
   detection code.
 - Paulo Moreno for fixing the AllocMem bug in FullDebugMode that prevented
   large blocks from being cleared.
 - Vladimir Bochkarev for the suggestion to remove some unnecessary code if the
   MM sharing mechanism is disabled.
 - Loris Luise for the version constant suggestion.
 - J.W. de Bokx for the MessageBox bugfix.
 - Igor Lindunen for reporting the bug that caused the Align16Bytes option to
   not work in FullDebugMode.
 - Ionut Muntean for the Romanian translation.
 - Florent Ouchet for the French translation.
 - Marcus Mönnig for the ScanMemoryPoolForCorruptions suggestion and the
   suggestion to have the option to scan the memory pool before every
   operation when in FullDebugMode.
 - Francois Piette for bringing under my attention that
   ScanMemoryPoolForCorruption was not thread safe.
 - Michael Rabatscher for reporting some compiler warnings.
 - QianYuan Wang for the Simplified Chinese translation of FastMM4Options.inc.
 - Maurizio Lotauro and Christian-W. Budde for reporting some Delphi 5
   compiler errors.
 - Patrick van Logchem for the DisableLoggingOfMemoryDumps option.
 - Norbert Spiegel for the BCB4 support code.
 - Uwe Schuster for the improved string leak detection code.
 - Murray McGowan for improvements to the usage tracker.
 - Michael Hieke for the SuppressFreeMemErrorsInsideException option as well
   as a bugfix to GetMemoryMap.
 - Richard Bradbrook for fixing the Windows 95 FullDebugMode support that was
   broken in version 4.94.
 - Zach Saw for the suggestion to (optionally) use SwitchToThread when
   waiting for a lock on a shared resource to be released.
 - Everyone who have made donations. Thanks!
 - Any other Fastcoders or supporters that I have forgotten, and also everyone
   that helped with the older versions.

Change log:
 Version 1.00 (28 June 2004):
  - First version (called PSDMemoryManager). Based on RecyclerMM (free block
    stack approach) by Eric Grange.
 Version 2.00 (3 November 2004):
  - Complete redesign and rewrite from scratch. Name changed to FastMM to
    reflect this fact. Uses a linked-list approach. Is faster, has less memory
    overhead, and will now catch most bad pointers on FreeMem calls.
 Version 3.00 (1 March 2005):
  - Another rewrite. Reduced the memory overhead by: (a) not having a separate
    memory area for the linked list of free blocks (uses space inside free
    blocks themselves) (b) batch managers are allocated as part of chunks (c)
    block size lookup table size reduced. This should make FastMM more CPU
    cache friendly.
 Version 4.00 (7 June 2005):
  - Yet another rewrite. FastMM4 is in fact three memory managers in one: Small
    blocks (up to a few KB) are managed through the binning model in the same
    way as previous versions, medium blocks (from a few KB up to approximately
    256K) are allocated in a linked-list fashion, and large blocks are grabbed
    directly from the system through VirtualAlloc. This 3-layered design allows
    very fast operation with the most frequently used block sizes (small
    blocks), while also minimizing fragmentation and imparting significant
    overhead savings with blocks larger than a few KB.
 Version 4.01 (8 June 2005):
  - Added the options "RequireDebugInfoForLeakReporting" and
    "RequireIDEPresenceForLeakReporting" as suggested by Pierre Y.
  - Fixed the "DelphiIsRunning" function not working under Delphi 5, and
    consequently no leak checking. (Reported by Anders Isaksson and Greg.)
 Version 4.02 (8 June 2005):
  - Fixed the compilation error when both the "AssumeMultiThreaded" and
    "CheckHeapForCorruption options were set. (Reported by Francois Malan.)
 Version 4.03 (9 June 2005):
  - Added descriptive error messages when FastMM4 cannot be installed because
    another MM has already been installed or memory has already been allocated.
 Version 4.04 (13 June 2005):
  - Added a small fixed offset to the size of medium blocks (previously always
    exact multiples of 256 bytes). This makes performance problems due to CPU
    cache associativity limitations much less likely. (Reported by Craig
    Peterson.)
 Version 4.05 (17 June 2005):
  - Added the Align16Bytes option. Disable this option to drop the 16 byte
    alignment restriction and reduce alignment to 8 bytes for the smallest
    block sizes. Disabling Align16Bytes should lower memory consumption at the
    cost of complicating the use of aligned SSE move instructions. (Suggested
    by Craig Peterson.)
  - Added a support unit for C++ Builder 6 - Add FastMM4BCB.cpp and
    FastMM4.pas to your BCB project to use FastMM instead of the RTL MM. Memory
    leak checking is not supported because (unfortunately) once an MM is
    installed under BCB you cannot uninstall it... at least not without
    modifying the RTL code in exit.c or patching the RTL code runtime. (Thanks
    to Jarek Karciarz, Vladimir Ulchenko and Bob Gonder.)
 Version 4.06 (22 June 2005):
  - Displays the class of all leaked objects on the memory leak report and also
    tries to identify leaked long strings. Previously it only displayed the
    sizes of all leaked blocks. (Suggested by Ben Taylor.)
  - Added support for displaying the sizes of medium and large block memory
    leaks. Previously it only displayed details for small block leaks.
 Version 4.07 (22 June 2005):
  - Fixed the detection of the class of leaked objects not working under
    Windows 98/Me.
 Version 4.08 (27 June 2005):
  - Added a BorlndMM.dpr project to allow you to build a borlndmm.dll that uses
    FastMM4 instead of the default memory manager. You may replace the old
    DLL in the Delphi \Bin directory to make the IDE use this memory manager
    instead.
 Version 4.09 (30 June 2005):
  - Included a patch fix for the bug affecting replacement borlndmm.dll files
    with Delphi 2005 (QC#14007). Compile the patch, close Delphi, and run it
    once to patch your vclide90.bpl. You will now be able to use the
    replacement borlndmm.dll to speed up the Delphi 2005 IDE as well.
 Version 4.10 (7 July 2005):
  - Due to QC#14070 ("Delphi IDE attempts to free memory after the shutdown
    code of borlndmm.dll has been called"), FastMM cannot be uninstalled
    safely when used inside a replacement borlndmm.dll for the IDE. Added a
    conditional define "NeverUninstall" for this purpose.
  - Added the "FullDebugMode" option to pad all blocks with a header and footer
    to help you catch memory overwrite bugs in your applications. All blocks
    returned to freemem are also zeroed out to help catch bugs involving the
    use of previously freed blocks. Also catches attempts at calling virtual
    methods of freed objects provided the block in question has not been reused
    since the object was freed. Displays stack traces on error to aid debugging.
  - Added the "LogErrorsToFile" option to log all errors to a text file in the
    same folder as the application.
  - Added the "ManualLeakReportingControl" option (suggested by Nahan Hyn) to
    enable control over whether the memory leak report should be done or not
    via a global variable.
 Version 4.11 (7 July 2005):
  - Fixed a compilation error under Delphi 2005 due to QC#10922. (Thanks to Joe
    Bain and Leonel Togniolli.)
  - Fixed leaked object classes not displaying in the leak report in
    "FullDebugMode".
  Version 4.12 (8 July 2005):
  - Moved all the string constants to one place to make it easier to do
    translations into other languages. (Thanks to Robert Marquardt.)
  - Added support for Kylix. Some functionality is currently missing: No
    support for detecting the object class on leaks and also no MM sharing.
    (Thanks to Simon Kissel and Fikret Hasovic).
  Version 4.13 (11 July 2005):
  - Added the FastMM_DebugInfo.dll support library to display debug info for
    stack traces.
  - Stack traces for the memory leak report is now logged to the log file in
    "FullDebugMode".
  Version 4.14 (14 July 2005):
  - Fixed string leaks not being detected as such in "FullDebugMode". (Thanks
    to Leonel Togniolli.)
  - Fixed the compilation error in "FullDebugMode" when "LogErrorsToFile" is
    not set. (Thanks to Leonel Togniolli.)
  - Added a "Release" option to allow the grouping of various options and to
    make it easier to make debug and release builds. (Thanks to Alexander
    Tabakov.)
  - Added a "HideMemoryLeakHintMessage" option to not display the hint below
    the memory leak message. (Thanks to Alexander Tabakov.)
  - Changed the fill character for "FullDebugMode" from zero to $80 to be able
    to differentiate between invalid memory accesses using nil pointers to
    invalid memory accesses using fields of freed objects. FastMM tries to
    reserve the 64K block starting at $80800000 at startup to ensure that an
    A/V will occur when this block is accessed. (Thanks to Alexander Tabakov.)
  - Fixed some compiler warnings. (Thanks to M. Skloff)
  - Fixed some display bugs in the memory leak report. (Thanks to Leonel
    Togniolli.)
  - Added a "LogMemoryLeakDetailToFile" option. Some applications leak a lot of
    memory and can make the log file grow very large very quickly.
  - Added the option to use madExcept instead of the JCL Debug library in the
    debug info support DLL. (Thanks to Martin Aignesberger.)
  - Added procedures "GetMemoryManagerState" and "GetMemoryMap" to retrieve
    statistics about the current state of the memory manager and memory pool.
    (A usage tracker form together with a demo is also available.)
  Version 4.15 (14 July 2005):
  - Fixed a false 4GB(!) memory leak reported in some instances.
  Version 4.16 (15 July 2005):
  - Added the "CatchUseOfFreedInterfaces" option to catch the use of interfaces
    of freed objects. This option is not compatible with checking that a freed
    block has not been modified, so enable this option only when hunting an
    invalid interface reference. (Only relevant if "FullDebugMode" is set.)
  - During shutdown FastMM now checks that all free blocks have not been
    modified since being freed. (Only when "FullDebugMode" is set and
    "CatchUseOfFreedInterfaces" is disabled.)
  Version 4.17 (15 July 2005):
 - Added the AddExpectedMemoryLeaks and RemoveExpectedMemoryLeaks procedures to
   register/unregister expected leaks, thus preventing the leak report from
   displaying if only expected leaks occurred. (Thanks to Diederik and Dennis
   Passmore for the suggestion.) (Note: these functions were renamed in later
   versions.)
 - Fixed the "LogMemoryLeakDetailToFile" not logging memory leak detail to file
   as it is supposed to. (Thanks to Leonel Togniolli.)
 Version 4.18 (18 July 2005):
 - Fixed some issues when range checking or complete boolean evaluation is
   switched on. (Thanks to Dario Tiraboschi and Mark Gebauer.)
 - Added the "OutputInstallUninstallDebugString" option to display a message when
   FastMM is installed or uninstalled. (Thanks to Hanspeter Widmer.)
 - Moved the options to a separate include file. (Thanks to Hanspeter Widmer.)
 - Moved message strings to a separate file for easy translation.
 Version 4.19 (19 July 2005):
 - Fixed Kylix support that was broken in 4.14.
 Version 4.20 (20 July 2005):
 - Fixed a false memory overwrite report at shutdown in "FullDebugMode". If you
   consistently got a "Block Header Has Been Corrupted" error message during
   shutdown at address $xxxx0070 then it was probably a false alarm. (Thanks to
   Theo Carr-Brion and Hanspeter Widmer.}
 Version 4.21 (27 July 2005):
 - Minor change to the block header flags to make it possible to immediately
   tell whether a medium block is being used as a small block pool or not.
   (Simplifies the leak checking and status reporting code.)
 - Expanded the functionality around the management of expected memory leaks.
 - Added the "ClearLogFileOnStartup" option. Deletes the log file during
   initialization. (Thanks to M. Skloff.)
 - Changed "OutputInstallUninstallDebugString" to use OutputDebugString instead
   of MessageBox. (Thanks to Hanspeter Widmer.)
 Version 4.22 (1 August 2005):
 - Added a FastAllocMem function that avoids an unnecessary FillChar call with
   large blocks.
 - Changed large block resizing behavior to be a bit more conservative. Large
   blocks will be downsized if the new size is less than half of the old size
   (the threshold was a quarter previously).
 Version 4.23 (6 August 2005):
 - Fixed BCB6 support (Thanks to Omar Zelaya).
 - Renamed "OutputInstallUninstallDebugString" to "UseOutputDebugString", and
   added debug string output on memory leak or error detection.
 Version 4.24 (11 August 2005):
 - Added the "NoMessageBoxes" option to suppress the display of message boxes,
   which is useful for services that should not be interrupted. (Thanks to Dan
   Miser).
 - Changed the stack trace code to return the line number of the caller and not
   the line number of the return address. (Thanks to Dan Miser).
 Version 4.25 (15 August 2005):
 - Fixed GetMemoryLeakType not detecting expected leaks registered by class
   when in "FullDebugMode". (Thanks to Arjen de Ruijter).
 Version 4.26 (18 August 2005):
 - Added a "UseRuntimePackages" option that allows FastMM to be used in a main
   application together with DLLs that all use runtime packages. (Thanks to
   Aleksander Oven.)
 Version 4.27 (24 August 2005):
 - Fixed a bug that sometimes caused the leak report to be shown even though all
   leaks were registered as expected leaks. (Thanks to Kristofer Skaug.)
 Version 4.29 (30 September 2005):
 - Added the "RequireDebuggerPresenceForLeakReporting" option to only display
   the leak report if the application is run inside the IDE. (Thanks to Günther
   Schoch.)
 - Added the "ForceMMX" option, which when disabled will check the CPU for
   MMX compatibility before using MMX. (Thanks to Jan Schlüter.)
 - Added the module name to the title of error dialogs to more easily identify
   which application caused the error. (Thanks to Kristofer Skaug.)
 - Added an ASCII dump to the "FullDebugMode" memory dumps. (Thanks to Hallvard
   Vassbotn.)
 - Added the option "HideExpectedLeaksRegisteredByPointer" to suppress the
   display and logging of expected memory leaks that were registered by pointer.
   (Thanks to Dan Miser.) Leaks registered by size or class are often ambiguous,
   so these expected leaks are always logged to file (in FullDebugMode) and are
   never hidden from the leak display (only displayed if there is at least one
   unexpected leak).
 - Added a procedure "GetRegisteredMemoryLeaks" to return a list of all
   registered memory leaks. (Thanks to Dan Miser.)
 - Added the "RawStackTraces" option to perform "raw" stack traces, negating
   the need for stack frames. This will usually result in more complete stack
   traces in FullDebugMode error reports, but it is significantly slower.
   (Thanks to Hallvard Vassbotn, Dan Miser and the JCL team.)
 Version 4.31 (2 October 2005):
 - Fixed the crash bug when both "RawStackTraces" and "FullDebugMode" were
   enabled. (Thanks to Dan Miser and Mark Edington.)
 Version 4.33 (6 October 2005):
 - Added a header corruption check to all memory blocks that are identified as
   leaks in FullDebugMode. This allows better differentiation between memory
   pool corruption bugs and actual memory leaks.
 - Fixed the stack overflow bug when using "RawStackTraces".
 Version 4.35 (6 October 2005):
 - Fixed a compilation error when the "NoMessageBoxes" option is set. (Thanks
   to Paul Ishenin.)
 - Before performing a "raw" stack trace, FastMM now checks whether exception
   handling is in place. If exception handling is not in place FastMM falls
   back to stack frame tracing. (Exception handling is required to handle the
   possible A/Vs when reading invalid call addresses. Exception handling is
   usually always available except when SysUtils hasn't been initialized yet or
   after SysUtils has been finalized.)
 Version 4.37 (8 October 2005):
 - Fixed the missing call stack trace entry issue when dynamically loading DLLs.
   (Thanks to Paul Ishenin.)
 Version 4.39 (12 October 2005):
 - Restored the performance with "RawStackTraces" enabled back to the level it
   was in 4.35.
 - Fixed the stack overflow error when using "RawStackTraces" that I thought I
   had fixed in 4.31, but unfortunately didn't. (Thanks to Craig Peterson.)
 Version 4.40 (13 October 2005):
 - Improved "RawStackTraces" to have less incorrect extra entries. (Thanks to
   Craig Peterson.)
 - Added the Russian (by Paul Ishenin) and Afrikaans translations of
   FastMM4Messages.pas.
 Version 4.42 (13 October 2005):
 - Fixed the compilation error when "CatchUseOfFreedInterfaces" is enabled.
   (Thanks to Cristian Nicola.)
 Version 4.44 (25 October 2005):
 - Implemented a FastGetHeapStatus function in analogy with GetHeapStatus.
   (Suggested by Cristian Nicola.)
 - Shifted more of the stack trace code over to the support dll to allow third
   party vendors to make available their own stack tracing and stack trace
   logging facilities.
 - Mathias Rauen (madshi) improved the support for madExcept in the debug info
   support DLL. Thanks!
 - Added support for BCB5. (Thanks to Roddy Pratt.)
 - Added the Czech translation by Rene Mihula.
 - Added the "DetectMMOperationsAfterUninstall" option. This will catch
   attempts to use the MM after FastMM has been uninstalled, and is useful for
   debugging.
 Version 4.46 (26 October 2005):
 - Renamed FastMM_DebugInfo.dll to FastMM_FullDebugMode.dll and made the
   dependency on this library a static one. This solves a DLL unload order
   problem when using FullDebugMode together with the replacement
   borlndmm.dll. (Thanks to Bart van der Werf.)
 - Added the Polish translation by Artur Redzko.
 Version 4.48 (10 November 2005):
 - Fixed class detection for objects leaked in dynamically loaded DLLs that
   were relocated.
 - Fabio Dell'Aria implemented support for EurekaLog in the FullDebugMode
   support DLL. Thanks!
 - Added the Spanish translation by JRG ("The Delphi Guy").
 Version 4.49 (10 November 2005):
 - Implemented support for installing replacement AllocMem and leak
   registration mechanisms for Delphi/BCB versions that support it.
 - Added support for Delphi 4. (Thanks to Justus Janssen.)
 Version 4.50 (5 December 2005):
 - Renamed the ReportMemoryLeaks global variable to ReportMemoryLeaksOnShutdown
   to be more consistent with the Delphi 2006 memory manager.
 - Improved the handling of large blocks. Large blocks can now consist of
   several consecutive segments allocated through VirtualAlloc. This
   significantly improves speed when frequently resizing large blocks, since
   these blocks can now often be upsized in-place.
 Version 4.52 (7 December 2005):
 - Fixed the compilation error with Delphi 5. (Thanks to Vadim Lopushansky and
   Charles Vinal for reporting the error.)
 Version 4.54 (15 December 2005):
 - Added the Brazilian Portuguese translation by Johni Jeferson Capeletto.
 - Fixed the compilation error with BCB6. (Thanks to Kurt Fitzner.)
 Version 4.56 (20 December 2005):
 - Fixed the Kylix compilation problem. (Thanks to Michal Niklas.)
 Version 4.58 (1 February 2006):
 - Added the German translations by Thomas Speck and Uwe Queisser.
 - Added the Indonesian translation by Zaenal Mutaqin.
 - Added the Portuguese translation by Carlos Macao.
 Version 4.60 (21 February 2006):
 - Fixed a performance issue due to an unnecessary block move operation when
   allocating a block in the range 1261-1372 bytes and then reallocating it in
   the range 1373-1429 bytes twice. (Thanks to Michael Winter.)
 - Added the Belarussian translation by dzmitry[li].
 - Added the updated Spanish translation by Marcelo Montenegro.
 - Added a new option "EnableSharingWithDefaultMM". This option allows FastMM
   to be shared with the default MM of Delphi 2006. It is on by default, but
   MM sharing has to be enabled otherwise it has no effect (refer to the
   documentation for the "ShareMM" and "AttemptToUseSharedMM" options).
 Version 4.62 (22 February 2006):
 - Fixed a possible read access violation in the MoveX16LP routine when the
   UseCustomVariableSizeMoveRoutines option is enabled. (Thanks to Jud Cole for
   some great detective work in finding this bug.)
 - Improved the downsizing behaviour of medium blocks to better correlate with
   the reallocation behaviour of small blocks. This change reduces the number
   of transitions between small and medium block types when reallocating blocks
   in the 0.7K to 2.6K range. It cuts down on the number of memory move
   operations and improves performance.
 Version 4.64 (31 March 2006):
 - Added the following functions for use with FullDebugMode (and added the
   exports to the replacement BorlndMM.dll): SetMMLogFileName,
   GetCurrentAllocationGroup, PushAllocationGroup, PopAllocationGroup and
   LogAllocatedBlocksToFile. The purpose of these functions are to allow you to
   identify and log related memory leaks while your application is still
   running.
 - Fixed a bug in the memory manager sharing mechanism affecting Windows
   95/98/ME. (Thanks to Zdenek Vasku.)
 Version 4.66 (9 May 2006):
 - Added a hint comment in this file so that FastMM4Messages.pas will also be
   backed up by GExperts. (Thanks to RB Winston.)
 - Fixed a bug affecting large address space (> 2GB) support under
   FullDebugMode. (Thanks to Thomas Schulz.)
 Version 4.68 (3 July 2006):
 - Added the Italian translation by Luigi Sandon.
 - If FastMM is used inside a DLL it will now use the name of the DLL as base
   for the log file name. (Previously it always used the name of the main
   application executable file.)
 - Fixed a rare A/V when both the FullDebugMode and RawStackTraces options were
   enabled. (Thanks to Primoz Gabrijelcic.)
 - Added the "NeverSleepOnThreadContention" option. This option may improve
   performance if the ratio of the the number of active threads to the number
   of CPU cores is low (typically < 2). This option is only useful for 4+ CPU
   systems, it almost always hurts performance on single and dual CPU systems.
   (Thanks to Werner Bochtler and Markus Beth.)
 Version 4.70 (4 August 2006):
  - Added the Simplified Chinese translation by JiYuan Xie.
  - Added the updated Russian as well as the Ukrainian translation by Andrey
    Shtukaturov.
  - Fixed two bugs in the leak class detection code that would sometimes fail
    to detect the class of leaked objects and strings, and report them as
    'unknown'. (Thanks to Dimitry Timokhov)
  Version 4.72 (24 September 2006):
  - Fixed a bug that caused AllocMem to not clear blocks > 256K in
    FullDebugMode. (Thanks to Paulo Moreno.)
  Version 4.74 (9 November 2006):
  - Fixed a bug in the segmented large block functionality that could lead to
    an application freeze when upsizing blocks greater than 256K in a
    multithreaded application (one of those "what the heck was I thinking?"
    type bugs).
  Version 4.76 (12 January 2007):
  - Changed the RawStackTraces code in the FullDebugMode DLL
    to prevent it from modifying the Windows "GetLastError" error code.
    (Thanks to Primoz Gabrijelcic.)
  - Fixed a threading issue when the "CheckHeapForCorruption" option was
    enabled, but the "FullDebugMode" option was disabled. (Thanks to Primoz
    Gabrijelcic.)
  - Removed some unnecessary startup code when the MM sharing mechanism is
    disabled. (Thanks to Vladimir Bochkarev.)
  - In FullDebugMode leaked blocks would sometimes be reported as belonging to
    the class "TFreedObject" if they were allocated but never used. Such blocks
    will now be reported as "unknown". (Thanks to Francois Malan.)
  - In recent versions the replacement borlndmm.dll created a log file (when
    enabled) that used the "borlndmm" prefix instead of the application name.
    It is now fixed to use the application name, however if FastMM is used
    inside other DLLs the name of those DLLs will be used. (Thanks to Bart van
    der Werf.)
  - Added a "FastMMVersion" constant. (Suggested by Loris Luise.)
  - Fixed an issue with error message boxes not displaying under certain
    configurations. (Thanks to J.W. de Bokx.)
  - FastMM will now display only one error message at a time. If many errors
    occur in quick succession, only the first error will be shown (but all will
    be logged). This avoids a stack overflow with badly misbehaved programs.
    (Thanks to Bart van der Werf.)
  - Added a LoadDebugDLLDynamically option to be used in conjunction with
    FullDebugMode. In this mode FastMM_FullDebugMode.dll is loaded dynamically.
    If the DLL cannot be found, stack traces will not be available. (Thanks to
    Rene Mihula.)
  Version 4.78 (1 March 2007):
  - The MB_DEFAULT_DESKTOP_ONLY constant that is used when displaying messages
    boxes since 4.76 is not defined under Kylix, and the source would thus not
    compile. That constant is now defined. (Thanks to Werner Bochtler.)
  - Moved the medium block locking code that was duplicated in several places
    to a subroutine to reduce code size. (Thanks to Hallvard Vassbotn.)
  - Fixed a bug in the leak registration code that sometimes caused registered
    leaks to be reported erroneously. (Thanks to Primoz Gabrijelcic.)
  - Added the NoDebugInfo option (on by default) that suppresses the generation
    of debug info for the FastMM4.pas unit. This will prevent the integrated
    debugger from stepping into the memory manager. (Thanks to Primoz
    Gabrijelcic.)
  - Increased the default stack trace depth in FullDebugMode from 9 to 10 to
    ensure that the Align16Bytes setting works in FullDebugMode. (Thanks to
    Igor Lindunen.)
  - Updated the Czech translation. (Thanks to Rene Mihula.)
  Version 4.84 (7 July 2008):
  - Added the Romanian translation. (Thanks to Ionut Muntean.)
  - Optimized the GetMemoryMap procedure to improve speed.
  - Added the GetMemoryManagerUsageSummary function that returns a summary of
    the GetMemoryManagerState call. (Thanks to Hallvard Vassbotn.)
  - Added the French translation. (Thanks to Florent Ouchet.)
  - Added the "AlwaysAllocateTopDown" FullDebugMode option to help with
    catching bad pointer arithmetic code in an address space > 2GB. This option
    is enabled by default.
  - Added the "InstallOnlyIfRunningInIDE" option. Enable this option to
    only install FastMM as the memory manager when the application is run
    inside the Delphi IDE. This is useful when you want to deploy the same EXE
    that you use for testing, but only want the debugging features active on
    development machines. When this option is enabled and the application is
    not being run inside the IDE, then the default Delphi memory manager will
    be used (which, since Delphi 2006, is FastMM without FullDebugMode.) This
    option is off by default.
  - Added the "FullDebugModeInIDE" option. This is a convenient shorthand for
    enabling FullDebugMode, InstallOnlyIfRunningInIDE and
    LoadDebugDLLDynamically. This causes FastMM to be used in FullDebugMode
    when the application is being debugged on development machines, and the
    default memory manager when the same executable is deployed. This allows
    the debugging and deployment of an application without having to compile
    separate executables. This option is off by default.
  - Added a ScanMemoryPoolForCorruptions procedure that checks the entire
    memory pool for corruptions and raises an exception if one is found. It can
    be called at any time, but is only available in FullDebugMode. (Thanks to
    Marcus Mönnig.)
  - Added a global variable "FullDebugModeScanMemoryPoolBeforeEveryOperation".
    When this variable is set to true and FullDebugMode is enabled, then the
    entire memory pool is checked for consistency before every GetMem, FreeMem
    and ReallocMem operation. An "Out of Memory" error is raised if a
    corruption is found (and this variable is set to false to prevent recursive
    errors). This obviously incurs a massive performance hit, so enable it only
    when hunting for elusive memory corruption bugs. (Thanks to Marcus Mönnig.)
  - Fixed a bug in AllocMem that caused the FPU stack to be shifted by one
    position.
  - Changed the default for option "EnableMMX" to false, since using MMX may
    cause unexpected behaviour in code that passes parameters on the FPU stack
    (like some "compiler magic" routines, e.g. VarFromReal).
  - Removed the "EnableSharingWithDefaultMM" option. This is now the default
    behaviour and cannot be disabled. (FastMM will always try to share memory
    managers between itself and the default memory manager when memory manager
    sharing is enabled.)
  - Introduced a new memory manager sharing mechanism based on memory mapped
    files. This solves compatibility issues with console and service
    applications. This sharing mechanism currently runs in parallel with the
    old mechanism, but the old mechanism can be disabled by undefining
    "EnableBackwardCompatibleMMSharing" in FastMM4Options.inc.
  - Fixed the recursive call error when the EnableMemoryLeakReporting option
    is disabled and an attempt is made to register a memory leak under Delphi
    2006 or later. (Thanks to Thomas Schulz.)
  - Added a global variable "SuppressMessageBoxes" to enable or disable
    messageboxes at runtime. (Thanks to Craig Peterson.)
  - Added the leak reporting code for C++ Builder, as well as various other
    C++ Builder bits written by JiYuan Xie. (Thank you!)
  - Added the new Usage Tracker written by Hanspeter Widmer. (Thank you!)
  Version 4.86 (31 July 2008):
  - Tweaked the string detection algorithm somewhat to be less strict, and
    allow non-class leaks to be more often categorized as strings.
  - Fixed a compilation error under Delphi 5.
  - Made LogAllocatedBlocksToFile and ScanMemoryPoolForCorruptions thread
    safe. (Thanks to Francois Piette.)
  Version 4.88 (13 August 2008):
  - Fixed compiler warnings in NoOpRegisterExpectedMemoryLeak and
    NoOpUnRegisterExpectedMemoryLeak. (Thanks to Michael Rabatscher.)
  - Added the Simplified Chinese translation of FastMM4Options.inc by
    QianYuan Wang. (Thank you!)
  - Included the updated C++ Builder files with support for BCB6 without
    update 4 applied. (Submitted by JiYuan Xie. Thanks!)
  - Fixed a compilation error under Delphi 5.
  - Made LogAllocatedBlocksToFile and ScanMemoryPoolForCorruptions thread
    safe - for real this time. (Thanks to Francois Piette.)
  Version 4.90 (9 September 2008):
  - Added logging of the thread ID when capturing and displaying stack
    traces. (Suggested by Allen Bauer and Mark Edington.)
  - Fixed a Delphi 5 compiler error under FullDebugMode. (Thanks to Maurizio
    Lotauro and Christian-W. Budde.)
  - Changed a default setting in FastMM4Options.inc: RawStackTraces is now
    off by default due to the high number of support requests I receive with
    regards to the false postives it may cause. I recommend compiling debug
    builds of applications with the "Stack Frames" option enabled.
  - Fixed a compilation error under Kylix. (Thanks to Werner Bochtler.)
  - Official support for Delphi 2009.
  Version 4.92 (25 November 2008):
  - Added the DisableLoggingOfMemoryDumps option under FullDebugMode. When
    this option is set, memory dumps will not be logged for memory leaks or
    errors. (Thanks to Patrick van Logchem.)
  - Exposed the class and string type detection code in the interface section
    for use in application code (if required). (Requested by Patrick van
    Logchem.)
  - Fixed a bug in SetMMLogFileName that could cause the log file name to be
    set incorrectly.
  - Added BCB4 support. (Thanks to Norbert Spiegel.)
  - Included the updated Czech translation by Rene Mihula.
  - When FastMM raises an error due to a freed block being modified, it now
    logs detail about which bytes in the block were modified.
  Version 4.94 (28 August 2009):
  - Added the DoNotInstallIfDLLMissing option that prevents FastMM from
    installing itself if the FastMM_FullDebugMode.dll library is not
    available. (Only applicable when FullDebugMode and LoadDebugDLLDynamically
    are both enabled.) This is useful when the same executable will be used for
    both debugging and deployment - when the debug support DLL is available
    FastMM will be installed in FullDebugMode, and otherwise the default memory
    manager will be used.
  - Added the FullDebugModeWhenDLLAvailable option that combines the
    FullDebugMode, LoadDebugDLLDynamically and DoNotInstallIfDLLMissing options.
  - Re-enabled RawStackTraces by default. The frame based stack traces (even
    when compiling with stack frames enabled) are generally too incomplete.
  - Improved the speed of large block operations under FullDebugMode: Since
    large blocks are never reused, there is no point in clearing them before
    and after use (so it does not do that anymore).
  - If an error occurs in FullDebugMode and FastMM is unable to append to the
    log file, it will attempt to write to a log file of the same name in the
    "My Documents" folder. This feature is helpful when the executable resides
    in a read-only location and the default log file, which is derived from the
    executable name, would thus not be writeable.
  - Added support for controlling the error log file location through an
    environment variable. If the 'FastMMLogFilePath' environment variable is
    set then any generated error logs will be written to the specified folder
    instead of the default location (which is the same folder as the
    application).
  - Improved the call instruction detection code in the FastMM_FullDebugMode
    library. (Thanks to the JCL team.)
  - Improved the string leak detection and reporting code. (Thanks to Uwe
    Schuster.)
  - New FullDebugMode feature: Whenever FreeMem or ReallocMem is called, FastMM
    will check that the block was actually allocated through the same FastMM
    instance. This is useful for tracking down memory manager sharing issues.
  - Compatible with Delphi 2010.
  Version 4.96 (31 August 2010):
  - Reduced the minimum block size to 4 bytes from the previous value of 12
    bytes (only applicable to 8 byte alignment). This reduces memory usage if
    the application allocates many blocks <= 4 bytes in size.
  - Added colour-coded change indication to the FastMM usage tracker, making
    it easier to spot changes in the memory usage grid. (Thanks to Murray
    McGowan.)
  - Added the SuppressFreeMemErrorsInsideException FullDebugMode option: If
    FastMM encounters a problem with a memory block inside the FullDebugMode
    FreeMem handler then an "invalid pointer operation" exception will usually
    be raised. If the FreeMem occurs while another exception is being handled
    (perhaps in the try.. finally code) then the original exception will be
    lost. With this option set FastMM will ignore errors inside FreeMem when an
    exception is being handled, thus allowing the original exception to
    propagate. This option is on by default. (Thanks to Michael Hieke.)
  - Fixed Windows 95 FullDebugMode support that was broken in 4.94. (Thanks to
    Richard Bradbrook.)
  - Fixed a bug affecting GetMemoryMap performance and accuracy of measurements
    above 2GB if a large address space is not enabled for the project. (Thanks
    to Michael Hieke.)
  - Added the FullDebugModeRegisterAllAllocsAsExpectedMemoryLeak boolean flag.
    When set, all allocations are automatically registered as expected memory
    leaks. Only available in FullDebugMode. (Thanks to Brian Cook.)
  - Compatible with Delphi XE.
  Version 4.97 (30 September 2010):
  - Fixed a crash bug (that crept in in 4.96) that may manifest itself when
    resizing a block to 4 bytes or less.
  - Added the UseSwitchToThread option. Set this option to call SwitchToThread
    instead of sitting in a "busy waiting" loop when a thread contention
    occurs. This is used in conjunction with the NeverSleepOnThreadContention
    option, and has no effect unless NeverSleepOnThreadContention is also
    defined. This option may improve performance with many CPU cores and/or
    threads of different priorities. Note that the SwitchToThread API call is
    only available on Windows 2000 and later. (Thanks to Zach Saw.)
  Version 4.98 (23 September 2011):
  - Added the FullDebugModeCallBacks define which adds support for memory
    manager event callbacks. This allows the application to be notified of
    memory allocations, frees and reallocations as they occur. (Thanks to
    Jeroen Pluimers.)
  - Added security options ClearMemoryBeforeReturningToOS and
    AlwaysClearFreedMemory to force the clearing of memory blocks after being
    freed. This could possibly provide some protection against information
    theft, but at a significant performance penalty. (Thanks to Andrey
    Sozonov.)
  - Shifted the code in the initialization section to a procedure
    RunInitializationCode. This allows the startup code to be called before
    InitUnits, which is required by some software protection tools.
  - Added support for Delphi XE2 (Windows 32-bit and Windows 64-bit platforms
    only).
  Version 4.99 (6 November 2011):
  - Fixed crashes in the 64-bit BASM codepath when more than 4GB of memory is
    allocated.
  - Fixed bad record alignment under 64-bit that affected performance.
  - Fixed compilation errors with some older compilers.
  Version 4.991 (3 September 2012)
  - Added the LogMemoryManagerStateToFile call. This call logs a summary of
    the memory manager state to file: The total allocated memory, overhead,
    efficiency, and a breakdown of allocated memory by class and string type.
    This call may be useful to catch objects that do not necessarily leak, but
    do linger longer than they should.
  - OS X support added by Sebastian Zierer
  - Compatible with Delphi XE3

*)

unit FastMM4;

interface

{$Include FastMM4Options.inc}

{$RANGECHECKS OFF}
{$BOOLEVAL OFF}
{$OVERFLOWCHECKS OFF}
{$OPTIMIZATION ON}
{$TYPEDADDRESS OFF}
{$LONGSTRINGS ON}

{Compiler version defines}
{$ifndef BCB}
  {$ifdef ver120}
    {$define Delphi4or5}
  {$endif}
  {$ifdef ver130}
    {$define Delphi4or5}
  {$endif}
  {$ifdef ver140}
    {$define Delphi6}
  {$endif}
  {$ifdef ver150}
    {$define Delphi7}
  {$endif}
  {$ifdef ver170}
    {$define Delphi2005}
  {$endif}
{$else}
  {for BCB4, use the Delphi 5 codepath}
  {$ifdef ver120}
    {$define Delphi4or5}
    {$define BCB4}
  {$endif}
  {for BCB5, use the Delphi 5 codepath}
  {$ifdef ver130}
    {$define Delphi4or5}
  {$endif}
{$endif}
{$ifdef ver180}
  {$define BDS2006}
{$endif}
{$define 32Bit}
{$ifndef Delphi4or5}
  {$if SizeOf(Pointer) = 8}
    {$define 64Bit}
    {$undef 32Bit}
  {$ifend}
  {$if CompilerVersion >= 23}
    {$define XE2AndUp}
  {$ifend}
  {$define BCB6OrDelphi6AndUp}
  {$ifndef BCB}
    {$define Delphi6AndUp}
  {$endif}
  {$ifndef Delphi6}
    {$define BCB6OrDelphi7AndUp}
    {$ifndef BCB}
      {$define Delphi7AndUp}
    {$endif}
    {$ifndef BCB}
      {$ifndef Delphi7}
        {$ifndef Delphi2005}
          {$define BDS2006AndUp}
        {$endif}
      {$endif}
    {$endif}
  {$endif}
{$endif}

{$ifdef 64Bit}
  {Under 64 bit memory blocks must always be 16-byte aligned}
  {$define Align16Bytes}
  {No need for MMX under 64-bit, since SSE2 is available}
  {$undef EnableMMX}
  {There is little need for raw stack traces under 64-bit, since frame based
   stack traces are much more accurate than under 32-bit. (And frame based
   stack tracing is much faster.)}
  {$undef RawStackTraces}
{$endif}

{IDE debug mode always enables FullDebugMode and dynamic loading of the FullDebugMode DLL.}
{$ifdef FullDebugModeInIDE}
  {$define InstallOnlyIfRunningInIDE}
  {$define FullDebugMode}
  {$define LoadDebugDLLDynamically}
{$endif}

{Install in FullDebugMode only when the DLL is available?}
{$ifdef FullDebugModeWhenDLLAvailable}
  {$define FullDebugMode}
  {$define LoadDebugDLLDynamically}
  {$define DoNotInstallIfDLLMissing}
{$endif}

{$ifdef Linux}
  {$define POSIX}
{$endif}

{Some features not currently supported under Kylix / OS X}
{$ifdef POSIX}
  {$undef FullDebugMode}
  {$undef LogErrorsToFile}
  {$undef LogMemoryLeakDetailToFile}
  {$undef ShareMM}
  {$undef AttemptToUseSharedMM}
  {$undef RequireIDEPresenceForLeakReporting}
  {$undef UseOutputDebugString}
  {$ifdef PIC}
    {BASM version does not support position independent code}
    {$undef ASMVersion}
  {$endif}
{$endif}

{Do we require debug info for leak checking?}
{$ifdef RequireDebugInfoForLeakReporting}
  {$ifopt D-}
    {$undef EnableMemoryLeakReporting}
  {$endif}
{$endif}

{Enable heap checking and leak reporting in full debug mode}
{$ifdef FullDebugMode}
  {$STACKFRAMES ON}
  {$define CheckHeapForCorruption}
  {$ifndef CatchUseOfFreedInterfaces}
    {$define CheckUseOfFreedBlocksOnShutdown}
  {$endif}
{$else}
  {Error logging requires FullDebugMode}
  {$undef LogErrorsToFile}
  {$undef CatchUseOfFreedInterfaces}
  {$undef RawStackTraces}
  {$undef AlwaysAllocateTopDown}
{$endif}

{Set defines for security options}
{$ifdef FullDebugMode}
  {In FullDebugMode small and medium blocks are always cleared when calling
   FreeMem. Large blocks are always returned to the OS immediately.}
  {$ifdef ClearMemoryBeforeReturningToOS}
    {$define ClearLargeBlocksBeforeReturningToOS}
  {$endif}
  {$ifdef AlwaysClearFreedMemory}
    {$define ClearLargeBlocksBeforeReturningToOS}
  {$endif}
{$else}
  {If memory blocks are cleared in FreeMem then they do not need to be cleared
   before returning the memory to the OS.}
  {$ifdef AlwaysClearFreedMemory}
    {$define ClearSmallAndMediumBlocksInFreeMem}
    {$define ClearLargeBlocksBeforeReturningToOS}
  {$else}
    {$ifdef ClearMemoryBeforeReturningToOS}
      {$define ClearMediumBlockPoolsBeforeReturningToOS}
      {$define ClearLargeBlocksBeforeReturningToOS}
    {$endif}
  {$endif}
{$endif}

{Only the Pascal version supports extended heap corruption checking.}
{$ifdef CheckHeapForCorruption}
  {$undef ASMVersion}
{$endif}

{For BASM bits that are not implemented in 64-bit.}
{$ifdef 32Bit}
  {$ifdef ASMVersion}
    {$define Use32BitAsm}
  {$endif}
{$endif}

{$ifdef UseRuntimePackages}
  {$define AssumeMultiThreaded}
{$endif}

{$ifdef BCB6OrDelphi6AndUp}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN SYMBOL_DEPRECATED OFF}
{$endif}

{Leak detail logging requires error logging}
{$ifndef LogErrorsToFile}
  {$undef LogMemoryLeakDetailToFile}
  {$undef ClearLogFileOnStartup}
{$endif}

{$ifndef EnableMemoryLeakReporting}
  {Manual leak reporting control requires leak reporting to be enabled}
  {$undef ManualLeakReportingControl}
{$endif}

{$ifndef EnableMMX}
  {$undef ForceMMX}
{$endif}

{Are any of the MM sharing options enabled?}
{$ifdef ShareMM}
  {$define MMSharingEnabled}
{$endif}
{$ifdef AttemptToUseSharedMM}
  {$define MMSharingEnabled}
{$endif}

{Instruct GExperts to back up the messages file as well.}
{#BACKUP FastMM4Messages.pas}

{Should debug info be disabled?}
{$ifdef NoDebugInfo}
  {$DEBUGINFO OFF}
{$endif}

{$ifdef BCB}
  {$ifdef borlndmmdll}
    {$OBJEXPORTALL OFF}
  {$endif}
  {$ifndef PatchBCBTerminate}
    {Cannot uninstall safely under BCB}
    {$define NeverUninstall}
    {Disable memory leak reporting}
    {$undef EnableMemoryLeakReporting}
  {$endif}
{$endif}

{-------------------------Public constants-----------------------------}
const
  {The current version of FastMM}
  FastMMVersion = '4.991';
  {The number of small block types}
{$ifdef Align16Bytes}
  NumSmallBlockTypes = 46;
{$else}
  NumSmallBlockTypes = 56;
{$endif}

{----------------------------Public types------------------------------}
type

  {Make sure all the required types are available}
{$ifdef BCB6OrDelphi6AndUp}
  {$if CompilerVersion < 20}
  PByte = PAnsiChar;
  {NativeInt didn't exist or was broken before Delphi 2009.}
  NativeInt = Integer;
  {$ifend}
  {$if CompilerVersion < 21}
  {NativeUInt didn't exist or was broken before Delphi 2010.}
  NativeUInt = Cardinal;
  {$ifend}
  {$if CompilerVersion < 22}
  {PNativeUInt didn't exist before Delphi XE.}
  PNativeUInt = ^Cardinal;
  {$ifend}
  {$if CompilerVersion < 23}
  {IntPtr and UIntPtr didn't exist before Delphi XE2.}
  IntPtr = Integer;
  UIntPtr = Cardinal;
  {$ifend}
{$else}
  PByte = PAnsiChar;
  NativeInt = Integer;
  NativeUInt = Cardinal;
  PNativeUInt = ^Cardinal;
  IntPtr = Integer;
  UIntPtr = Cardinal;
{$endif}

  TSmallBlockTypeState = record
    {The internal size of the block type}
    InternalBlockSize: Cardinal;
    {Useable block size: The number of non-reserved bytes inside the block.}
    UseableBlockSize: Cardinal;
    {The number of allocated blocks}
    AllocatedBlockCount: NativeUInt;
    {The total address space reserved for this block type (both allocated and
     free blocks)}
    ReservedAddressSpace: NativeUInt;
  end;
  TSmallBlockTypeStates = array[0..NumSmallBlockTypes - 1] of TSmallBlockTypeState;

  TMemoryManagerState = record
    {Small block type states}
    SmallBlockTypeStates: TSmallBlockTypeStates;
    {Medium block stats}
    AllocatedMediumBlockCount: Cardinal;
    TotalAllocatedMediumBlockSize: NativeUInt;
    ReservedMediumBlockAddressSpace: NativeUInt;
    {Large block stats}
    AllocatedLargeBlockCount: Cardinal;
    TotalAllocatedLargeBlockSize: NativeUInt;
    ReservedLargeBlockAddressSpace: NativeUInt;
  end;

  TMemoryManagerUsageSummary = record
    {The total number of bytes allocated by the application.}
    AllocatedBytes: NativeUInt;
    {The total number of address space bytes used by control structures, or
     lost due to fragmentation and other overhead.}
    OverheadBytes: NativeUInt;
    {The efficiency of the memory manager expressed as a percentage. This is
     100 * AllocatedBytes / (AllocatedBytes + OverheadBytes).}
    EfficiencyPercentage: Double;
  end;

  {Memory map}
  TChunkStatus = (csUnallocated, csAllocated, csReserved, csSysAllocated,
    csSysReserved);
  TMemoryMap = array[0..65535] of TChunkStatus;

{$ifdef EnableMemoryLeakReporting}
  {List of registered leaks}
  TRegisteredMemoryLeak = record
    LeakAddress: Pointer;
    LeakedClass: TClass;
    {$ifdef CheckCppObjectTypeEnabled}
    LeakedCppTypeIdPtr: Pointer;
    {$endif}
    LeakSize: NativeInt;
    LeakCount: Integer;
  end;
  TRegisteredMemoryLeaks = array of TRegisteredMemoryLeak;
{$endif}

  {Used by the DetectStringData routine to detect whether a leaked block
   contains string data.}
  TStringDataType = (stUnknown, stAnsiString, stUnicodeString);

  {The callback procedure for WalkAllocatedBlocks.}
  TWalkAllocatedBlocksCallback = procedure(APBlock: Pointer; ABlockSize: NativeInt; AUserData: Pointer);

{--------------------------Public variables----------------------------}
var
  {If this variable is set to true and FullDebugMode is enabled, then the
   entire memory pool is checked for consistency before every memory
   operation. Note that this incurs a massive performance hit on top of
   the already significant FullDebugMode overhead, so enable this option
   only when absolutely necessary.}
  FullDebugModeScanMemoryPoolBeforeEveryOperation: Boolean = False;
  FullDebugModeRegisterAllAllocsAsExpectedMemoryLeak: Boolean = False;
{$ifdef ManualLeakReportingControl}
  {Variable is declared in system.pas in newer Delphi versions.}
  {$ifndef BDS2006AndUp}
  ReportMemoryLeaksOnShutdown: Boolean;
  {$endif}
{$endif}
  {If set to True, disables the display of all messageboxes}
  SuppressMessageBoxes: Boolean;

{-------------------------Public procedures----------------------------}
{Executes the code normally run in the initialization section. Running it
 earlier may be required with e.g. some software protection tools.}
procedure RunInitializationCode;
{Installation procedures must be exposed for the BCB helper unit FastMM4BCB.cpp}
{$ifdef BCB}
procedure InitializeMemoryManager;
function CheckCanInstallMemoryManager: Boolean;
procedure InstallMemoryManager;

{$ifdef FullDebugMode}
(*$HPPEMIT '#define FullDebugMode' *)

{$ifdef ClearLogFileOnStartup}
(*$HPPEMIT '  #define ClearLogFileOnStartup' *)
procedure DeleteEventLog;
{$endif}

{$ifdef LoadDebugDLLDynamically}
(*$HPPEMIT '  #define LoadDebugDLLDynamically' *)
{$endif}

{$ifdef RawStackTraces}
(*$HPPEMIT '  #define RawStackTraces' *)
{$endif}

{$endif}

{$ifdef PatchBCBTerminate}
(*$HPPEMIT ''#13#10 *)
(*$HPPEMIT '#define PatchBCBTerminate' *)

{$ifdef EnableMemoryLeakReporting}
(*$HPPEMIT ''#13#10 *)
(*$HPPEMIT '#define EnableMemoryLeakReporting' *)
{$endif}

{$ifdef DetectMMOperationsAfterUninstall}
(*$HPPEMIT ''#13#10 *)
(*$HPPEMIT '#define DetectMMOperationsAfterUninstall' *)
{$endif}

{Called in FastMM4BCB.cpp, should contain codes of original "finalization" section}
procedure FinalizeMemoryManager;

{For completion of "RequireDebuggerPresenceForLeakReporting" checking in "FinalizeMemoryManager"}
var
  pCppDebugHook: ^Integer = nil; //PInteger not defined in BCB5

{$ifdef CheckCppObjectTypeEnabled}
(*$HPPEMIT ''#13#10 *)
(*$HPPEMIT '#define CheckCppObjectTypeEnabled' *)

type
  TGetCppVirtObjSizeByTypeIdPtrFunc = function(APointer: Pointer): Cardinal;
  TGetCppVirtObjTypeIdPtrFunc = function(APointer: Pointer; ASize: Cardinal): Pointer;
  TGetCppVirtObjTypeNameFunc = function(APointer: Pointer; ASize: Cardinal): PAnsiChar;
  TGetCppVirtObjTypeNameByTypeIdPtrFunc = function (APointer: Pointer): PAnsiChar;
  TGetCppVirtObjTypeNameByVTablePtrFunc = function(AVTablePtr: Pointer; AVTablePtrOffset: Cardinal): PAnsiChar;
var
  {Return virtual object's size from typeId pointer}
  GetCppVirtObjSizeByTypeIdPtrFunc: TGetCppVirtObjSizeByTypeIdPtrFunc = nil;
  {Retrieve virtual object's typeId pointer}
  GetCppVirtObjTypeIdPtrFunc: TGetCppVirtObjTypeIdPtrFunc = nil;
  {Retrieve virtual object's type name}
  GetCppVirtObjTypeNameFunc: TGetCppVirtObjTypeNameFunc = nil;
  {Return virtual object's type name from typeId pointer}
  GetCppVirtObjTypeNameByTypeIdPtrFunc: TGetCppVirtObjTypeNameByTypeIdPtrFunc = nil;
  {Retrieve virtual object's typeId pointer from it's virtual table pointer}
  GetCppVirtObjTypeNameByVTablePtrFunc: TGetCppVirtObjTypeNameByVTablePtrFunc = nil;
{$endif}
{$endif}
{$endif}

{$ifndef FullDebugMode}
{The standard memory manager functions}
function FastGetMem(ASize: {$ifdef XE2AndUp}NativeInt{$else}Integer{$endif}): Pointer;
function FastFreeMem(APointer: Pointer): Integer;
function FastReallocMem(APointer: Pointer; ANewSize: {$ifdef XE2AndUp}NativeInt{$else}Integer{$endif}): Pointer;
function FastAllocMem(ASize: {$ifdef XE2AndUp}NativeInt{$else}Cardinal{$endif}): Pointer;
{$else}
{The FullDebugMode memory manager functions}
function DebugGetMem(ASize: {$ifdef XE2AndUp}NativeInt{$else}Integer{$endif}): Pointer;
function DebugFreeMem(APointer: Pointer): Integer;
function DebugReallocMem(APointer: Pointer; ANewSize: {$ifdef XE2AndUp}NativeInt{$else}Integer{$endif}): Pointer;
function DebugAllocMem(ASize: {$ifdef XE2AndUp}NativeInt{$else}Cardinal{$endif}): Pointer;
{Scans the memory pool for any corruptions. If a corruption is encountered an "Out of Memory" exception is
 raised.}
procedure ScanMemoryPoolForCorruptions;
{Specify the full path and name for the filename to be used for logging memory
 errors, etc. If ALogFileName is nil or points to an empty string it will
 revert to the default log file name.}
procedure SetMMLogFileName(ALogFileName: PAnsiChar = nil);
{Returns the current "allocation group". Whenever a GetMem request is serviced
 in FullDebugMode, the current "allocation group" is stored in the block header.
 This may help with debugging. Note that if a block is subsequently reallocated
 that it keeps its original "allocation group" and "allocation number" (all
 allocations are also numbered sequentially).}
function GetCurrentAllocationGroup: Cardinal;
{Allocation groups work in a stack like fashion. Group numbers are pushed onto
 and popped off the stack. Note that the stack size is limited, so every push
 should have a matching pop.}
procedure PushAllocationGroup(ANewCurrentAllocationGroup: Cardinal);
procedure PopAllocationGroup;
{Logs detail about currently allocated memory blocks for the specified range of
 allocation groups. if ALastAllocationGroupToLog is less than
 AFirstAllocationGroupToLog or it is zero, then all allocation groups are
 logged. This routine also checks the memory pool for consistency at the same
 time, raising an "Out of Memory" error if the check fails.}
procedure LogAllocatedBlocksToFile(AFirstAllocationGroupToLog, ALastAllocationGroupToLog: Cardinal);
{$endif}

{Releases all allocated memory (use with extreme care)}
procedure FreeAllMemory;

{Returns summarised information about the state of the memory manager. (For
 backward compatibility.)}
function FastGetHeapStatus: THeapStatus;
{Returns statistics about the current state of the memory manager}
procedure GetMemoryManagerState(var AMemoryManagerState: TMemoryManagerState);
{Returns a summary of the information returned by GetMemoryManagerState}
procedure GetMemoryManagerUsageSummary(
  var AMemoryManagerUsageSummary: TMemoryManagerUsageSummary);
{$ifndef POSIX}
{Gets the state of every 64K block in the 4GB address space}
procedure GetMemoryMap(var AMemoryMap: TMemoryMap);
{$endif}

{$ifdef EnableMemoryLeakReporting}
{Registers expected memory leaks. Returns true on success. The list of leaked
 blocks is limited, so failure is possible if the list is full.}
function RegisterExpectedMemoryLeak(ALeakedPointer: Pointer): Boolean; overload;
function RegisterExpectedMemoryLeak(ALeakedObjectClass: TClass; ACount: Integer = 1): Boolean; overload;
function RegisterExpectedMemoryLeak(ALeakedBlockSize: NativeInt; ACount: Integer = 1): Boolean; overload;
{$ifdef CheckCppObjectTypeEnabled}
{Registers expected memory leaks by virtual object's typeId pointer.
 Usage: RegisterExpectedMemoryLeak(typeid(ACppObject).tpp, Count);}
function RegisterExpectedMemoryLeak(ALeakedCppVirtObjTypeIdPtr: Pointer; ACount: Integer): boolean; overload;
{$endif}
{Removes expected memory leaks. Returns true on success.}
function UnregisterExpectedMemoryLeak(ALeakedPointer: Pointer): Boolean; overload;
function UnregisterExpectedMemoryLeak(ALeakedObjectClass: TClass; ACount: Integer = 1): Boolean; overload;
function UnregisterExpectedMemoryLeak(ALeakedBlockSize: NativeInt; ACount: Integer = 1): Boolean; overload;
{$ifdef CheckCppObjectTypeEnabled}
{Usage: UnregisterExpectedMemoryLeak(typeid(ACppObject).tpp, Count);}
function UnregisterExpectedMemoryLeak(ALeakedCppVirtObjTypeIdPtr: Pointer; ACount: Integer): boolean; overload;
{$endif}
{Returns a list of all expected memory leaks}
function GetRegisteredMemoryLeaks: TRegisteredMemoryLeaks;
{$endif}

{Returns the class for a memory block. Returns nil if it is not a valid class.
 Used by the leak detection code.}
function DetectClassInstance(APointer: Pointer): TClass;
{Detects the probable string data type for a memory block. Used by the leak
 classification code when a block cannot be identified as a known class
 instance.}
function DetectStringData(APMemoryBlock: Pointer;
  AAvailableSpaceInBlock: NativeInt): TStringDataType;
{Walks all allocated blocks, calling ACallBack for each. Passes the user block size and AUserData to the callback.
 Important note: All block types will be locked during the callback, so the memory manager cannot be used inside it.}
procedure WalkAllocatedBlocks(ACallBack: TWalkAllocatedBlocksCallback; AUserData: Pointer);
{Writes a log file containing a summary of the memory mananger state and a summary of allocated blocks grouped by
 class. The file will be saved in UTF-8 encoding (in supported Delphi versions). Returns True on success. }
function LogMemoryManagerStateToFile(const AFileName: string; const AAdditionalDetails: string = ''): Boolean;

{$ifdef FullDebugMode}
{-------------FullDebugMode constants---------------}
const
  {The stack trace depth. (Must be an *uneven* number to ensure that the
   Align16Bytes option works in FullDebugMode.)}
  StackTraceDepth = 11;
  {The number of entries in the allocation group stack}
  AllocationGroupStackSize = 1000;
  {The number of fake VMT entries - used to track virtual method calls on
   freed objects. Do not change this value without also updating TFreedObject.GetVirtualMethodIndex}
  MaxFakeVMTEntries = 200;
  {The pattern used to fill unused memory}
  DebugFillByte = $80;
{$ifdef 32Bit}
  DebugFillPattern = $01010101 * Cardinal(DebugFillByte);
  {The address that is reserved so that accesses to the address of the fill
   pattern will result in an A/V. (Not used under 64-bit, since the upper half
   of the address space is always reserved by the OS.)}
  DebugReservedAddress = $01010000 * Cardinal(DebugFillByte);
{$else}
  DebugFillPattern = $8080808080808080;
{$endif}

{-------------------------FullDebugMode structures--------------------}
type
  PStackTrace = ^TStackTrace;
  TStackTrace = array[0..StackTraceDepth - 1] of NativeUInt;

  TBlockOperation = (boBlockCheck, boGetMem, boFreeMem, boReallocMem);

  {The header placed in front of blocks in FullDebugMode (just after the
   standard header). Must be a multiple of 16 bytes in size otherwise the
   Align16Bytes option will not work. Current size = 128 bytes under 32-bit,
   and 240 bytes under 64-bit.}
  PFullDebugBlockHeader = ^TFullDebugBlockHeader;
  TFullDebugBlockHeader = record
    {Space used by the medium block manager for previous/next block management.
     If a medium block is binned then these two fields will be modified.}
    Reserved1: Pointer;
    Reserved2: Pointer;
    {Is the block currently allocated? If it is allocated this will be the
     address of the getmem routine through which it was allocated, otherwise it
     will be nil.}
    AllocatedByRoutine: Pointer;
    {The allocation group: Can be used in the debugging process to group
     related memory leaks together}
    AllocationGroup: Cardinal;
    {The allocation number: All new allocations are numbered sequentially. This
     number may be useful in memory leak analysis. If it reaches 4G it wraps
     back to 0.}
    AllocationNumber: Cardinal;
    {The call stack when the block was allocated}
    AllocationStackTrace: TStackTrace;
    {The thread that allocated the block}
    AllocatedByThread: Cardinal;
    {The thread that freed the block}
    FreedByThread: Cardinal;
    {The call stack when the block was freed}
    FreeStackTrace: TStackTrace;
    {The user requested size for the block. 0 if this is the first time the
     block is used.}
    UserSize: NativeUInt;
    {The object class this block was used for the previous time it was
     allocated. When a block is freed, the pointer that would normally be in the
     space of the class pointer is copied here, so if it is detected that
     the block was used after being freed we have an idea what class it is.}
    PreviouslyUsedByClass: NativeUInt;
    {The sum of all the dwords(32-bit)/qwords(64-bit) in this structure
     excluding the initial two reserved fields and this field.}
    HeaderCheckSum: NativeUInt;
  end;
  {The NativeUInt following the user area of the block is the inverse of
   HeaderCheckSum. This is used to catch buffer overrun errors.}

  {The class used to catch attempts to execute a virtual method of a freed
   object}
  TFreedObject = class
  public
    procedure GetVirtualMethodIndex;
    procedure VirtualMethodError;
{$ifdef CatchUseOfFreedInterfaces}
    procedure InterfaceError;
{$endif}
  end;

{$ifdef FullDebugModeCallBacks}
  {FullDebugMode memory manager event callbacks. Note that APHeaderFreedBlock in the TOnDebugFreeMemFinish
   will not be valid for large (>260K) blocks.}
  TOnDebugGetMemFinish = procedure(APHeaderNewBlock: PFullDebugBlockHeader; ASize: NativeInt);
  TOnDebugFreeMemStart = procedure(APHeaderBlockToFree: PFullDebugBlockHeader);
  TOnDebugFreeMemFinish = procedure(APHeaderFreedBlock: PFullDebugBlockHeader; AResult: Integer);
  TOnDebugReallocMemStart = procedure(APHeaderBlockToReallocate: PFullDebugBlockHeader; ANewSize: NativeInt);
  TOnDebugReallocMemFinish = procedure(APHeaderReallocatedBlock: PFullDebugBlockHeader; ANewSize: NativeInt);

var
  {Note: FastMM will not catch exceptions inside these hooks, so make sure your hook code runs without
   exceptions.}
  OnDebugGetMemFinish: TOnDebugGetMemFinish = nil;
  OnDebugFreeMemStart: TOnDebugFreeMemStart = nil;
  OnDebugFreeMemFinish: TOnDebugFreeMemFinish = nil;
  OnDebugReallocMemStart: TOnDebugReallocMemStart = nil;
  OnDebugReallocMemFinish: TOnDebugReallocMemFinish = nil;
{$endif}
{$endif}

implementation

uses
{$ifndef POSIX}
  Windows,
  {$ifdef FullDebugMode}
    {$ifdef Delphi4or5}
  ShlObj,
    {$else}
  SHFolder,
    {$endif}
  {$endif}
{$else}
  {$ifdef MACOS}
  Posix.Stdlib, Posix.Unistd, Posix.Fcntl,
  {$ELSE}
  Libc,
  {$endif}
{$endif}
  FastMM4Messages;

{Fixed size move procedures. The 64-bit versions assume 16-byte alignment.}
procedure Move4(const ASource; var ADest; ACount: NativeInt); forward;
procedure Move12(const ASource; var ADest; ACount: NativeInt); forward;
procedure Move20(const ASource; var ADest; ACount: NativeInt); forward;
procedure Move28(const ASource; var ADest; ACount: NativeInt); forward;
procedure Move36(const ASource; var ADest; ACount: NativeInt); forward;
procedure Move44(const ASource; var ADest; ACount: NativeInt); forward;
procedure Move52(const ASource; var ADest; ACount: NativeInt); forward;
procedure Move60(const ASource; var ADest; ACount: NativeInt); forward;
procedure Move68(const ASource; var ADest; ACount: NativeInt); forward;
{$ifdef 64Bit}
{These are not needed and thus unimplemented under 32-bit}
procedure Move8(const ASource; var ADest; ACount: NativeInt); forward;
procedure Move24(const ASource; var ADest; ACount: NativeInt); forward;
procedure Move40(const ASource; var ADest; ACount: NativeInt); forward;
procedure Move56(const ASource; var ADest; ACount: NativeInt); forward;
{$endif}

{$ifdef DetectMMOperationsAfterUninstall}
{Invalid handlers to catch MM operations after uninstall}
function InvalidFreeMem(APointer: Pointer): Integer; forward;
function InvalidGetMem(ASize: {$ifdef XE2AndUp}NativeInt{$else}Integer{$endif}): Pointer; forward;
function InvalidReallocMem(APointer: Pointer; ANewSize: {$ifdef XE2AndUp}NativeInt{$else}Integer{$endif}): Pointer; forward;
function InvalidAllocMem(ASize: {$ifdef XE2AndUp}NativeInt{$else}Cardinal{$endif}): Pointer; forward;
function InvalidRegisterAndUnRegisterMemoryLeak(APointer: Pointer): Boolean; forward;
{$endif}

{-------------------------Private constants----------------------------}
const
  {The size of a medium block pool. This is allocated through VirtualAlloc and
   is used to serve medium blocks. The size must be a multiple of 16 and at
   least 4 bytes less than a multiple of 4K (the page size) to prevent a
   possible read access violation when reading past the end of a memory block
   in the optimized move routine (MoveX16LP). In Full Debug mode we leave a
   trailing 256 bytes to be able to safely do a memory dump.}
  MediumBlockPoolSize = 20 * 64 * 1024{$ifndef FullDebugMode} - 16{$else} - 256{$endif};
  {The granularity of small blocks}
{$ifdef Align16Bytes}
  SmallBlockGranularity = 16;
{$else}
  SmallBlockGranularity = 8;
{$endif}
  {The granularity of medium blocks. Newly allocated medium blocks are
   a multiple of this size plus MediumBlockSizeOffset, to avoid cache line
   conflicts}
  MediumBlockGranularity = 256;
  MediumBlockSizeOffset = 48;
  {The granularity of large blocks}
  LargeBlockGranularity = 65536;
  {The maximum size of a small block. Blocks Larger than this are either
   medium or large blocks.}
  MaximumSmallBlockSize = 2608;
  {The smallest medium block size. (Medium blocks are rounded up to the nearest
   multiple of MediumBlockGranularity plus MediumBlockSizeOffset)}
  MinimumMediumBlockSize = 11 * 256 + MediumBlockSizeOffset;
  {The number of bins reserved for medium blocks}
  MediumBlockBinsPerGroup = 32;
  MediumBlockBinGroupCount = 32;
  MediumBlockBinCount = MediumBlockBinGroupCount * MediumBlockBinsPerGroup;
  {The maximum size allocatable through medium blocks. Blocks larger than this
   fall through to VirtualAlloc ( = large blocks).}
  MaximumMediumBlockSize = MinimumMediumBlockSize + (MediumBlockBinCount - 1) * MediumBlockGranularity;
  {The target number of small blocks per pool. The actual number of blocks per
   pool may be much greater for very small sizes and less for larger sizes. The
   cost of allocating the small block pool is amortized across all the small
   blocks in the pool, however the blocks may not all end up being used so they
   may be lying idle.}
  TargetSmallBlocksPerPool = 48;
  {The minimum number of small blocks per pool. Any available medium block must
   have space for roughly this many small blocks (or more) to be useable as a
   small block pool.}
  MinimumSmallBlocksPerPool = 12;
  {The lower and upper limits for the optimal small block pool size}
  OptimalSmallBlockPoolSizeLowerLimit = 29 * 1024 - MediumBlockGranularity + MediumBlockSizeOffset;
  OptimalSmallBlockPoolSizeUpperLimit = 64 * 1024 - MediumBlockGranularity + MediumBlockSizeOffset;
  {The maximum small block pool size. If a free block is this size or larger
   then it will be split.}
  MaximumSmallBlockPoolSize = OptimalSmallBlockPoolSizeUpperLimit + MinimumMediumBlockSize;
  {-------------Block type flags--------------}
  {The lower 3 bits in the dword header of small blocks (4 bits in medium and
   large blocks) are used as flags to indicate the state of the block}
  {Set if the block is not in use}
  IsFreeBlockFlag = 1;
  {Set if this is a medium block}
  IsMediumBlockFlag = 2;
  {Set if it is a medium block being used as a small block pool. Only valid if
   IsMediumBlockFlag is set.}
  IsSmallBlockPoolInUseFlag = 4;
  {Set if it is a large block. Only valid if IsMediumBlockFlag is not set.}
  IsLargeBlockFlag = 4;
  {Is the medium block preceding this block available? (Only used by medium
   blocks)}
  PreviousMediumBlockIsFreeFlag = 8;
  {Is this large block segmented? I.e. is it actually built up from more than
   one chunk allocated through VirtualAlloc? (Only used by large blocks.)}
  LargeBlockIsSegmented = 8;
  {The flags masks for small blocks}
  DropSmallFlagsMask = -8;
  ExtractSmallFlagsMask = 7;
  {The flags masks for medium and large blocks}
  DropMediumAndLargeFlagsMask = -16;
  ExtractMediumAndLargeFlagsMask = 15;
  {-------------Block resizing constants---------------}
  SmallBlockDownsizeCheckAdder = 64;
  SmallBlockUpsizeAdder = 32;
  {When a medium block is reallocated to a size smaller than this, then it must
   be reallocated to a small block and the data moved. If not, then it is
   shrunk in place down to MinimumMediumBlockSize. Currently the limit is set
   at a quarter of the minimum medium block size.}
  MediumInPlaceDownsizeLimit = MinimumMediumBlockSize div 4;
  {-------------Memory leak reporting constants---------------}
  ExpectedMemoryLeaksListSize = 64 * 1024;
  {-------------Other constants---------------}
{$ifndef NeverSleepOnThreadContention}
  {Sleep time when a resource (small/medium/large block manager) is in use}
  InitialSleepTime = 0;
  {Used when the resource is still in use after the first sleep}
  AdditionalSleepTime = 1;
{$endif}
  {Hexadecimal characters}
  HexTable: array[0..15] of AnsiChar = ('0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
  {Copyright message - not used anywhere in the code}
  Copyright: AnsiString = 'FastMM4 (c) 2004 - 2011 Pierre le Riche / Professional Software Development';
{$ifdef FullDebugMode}
  {Virtual Method Called On Freed Object Errors}
  StandardVirtualMethodNames: array[1 + vmtParent div SizeOf(Pointer) .. vmtDestroy div SizeOf(Pointer)] of PAnsiChar = (
{$ifdef BCB6OrDelphi6AndUp}
  {$if RTLVersion >= 20}
    'Equals',
    'GetHashCode',
    'ToString',
  {$ifend}
{$endif}
    'SafeCallException',
    'AfterConstruction',
    'BeforeDestruction',
    'Dispatch',
    'DefaultHandler',
    'NewInstance',
    'FreeInstance',
    'Destroy');
  {The name of the FullDebugMode support DLL. The support DLL implements stack
   tracing and the conversion of addresses to unit and line number information.}
{$ifdef 32Bit}
  FullDebugModeLibraryName = FullDebugModeLibraryName32Bit;
{$else}
  FullDebugModeLibraryName = FullDebugModeLibraryName64Bit;
{$endif}
{$endif}

{-------------------------Private types----------------------------}
type

{$ifdef Delphi4or5}
  {Delphi 5 Compatibility}
  PCardinal = ^Cardinal;
  PPointer = ^Pointer;
{$endif}
{$ifdef BCB4}
  {Define some additional types for BCB4}
  PInteger  = ^Integer;
{$endif}

  {Move procedure type}
  TMoveProc = procedure(const ASource; var ADest; ACount: NativeInt);

  {Registers structure (for GetCPUID)}
  TRegisters = record
    RegEAX, RegEBX, RegECX, RegEDX: Integer;
  end;

  {The layout of a string allocation. Used to detect string leaks.}
  PStrRec = ^StrRec;
  StrRec = packed record
{$ifdef 64Bit}
    _Padding: Integer;
{$endif}
{$ifdef BCB6OrDelphi6AndUp}
  {$if RTLVersion >= 20}
    codePage: Word;
    elemSize: Word;
  {$ifend}
{$endif}
    refCnt: Integer;
    length: Integer;
  end;

{$ifdef EnableMemoryLeakReporting}
  {Different kinds of memory leaks}
  TMemoryLeakType = (mltUnexpectedLeak, mltExpectedLeakRegisteredByPointer,
    mltExpectedLeakRegisteredByClass, mltExpectedLeakRegisteredBySize);
{$endif}

  {---------------Small block structures-------------}

  {Pointer to the header of a small block pool}
  PSmallBlockPoolHeader = ^TSmallBlockPoolHeader;

  {Small block type (Size = 32 bytes for 32-bit, 64 bytes for 64-bit).}
  PSmallBlockType = ^TSmallBlockType;
  TSmallBlockType = record
    {True = Block type is locked}
    BlockTypeLocked: Boolean;
    {Bitmap indicating which of the first 8 medium block groups contain blocks
     of a suitable size for a block pool.}
    AllowedGroupsForBlockPoolBitmap: Byte;
    {The block size for this block type}
    BlockSize: Word;
    {The minimum and optimal size of a small block pool for this block type}
    MinimumBlockPoolSize: Word;
    OptimalBlockPoolSize: Word;
    {The first partially free pool for the given small block. This field must
     be at the same offset as TSmallBlockPoolHeader.NextPartiallyFreePool.}
    NextPartiallyFreePool: PSmallBlockPoolHeader;
    {The last partially free pool for the small block type. This field must
     be at the same offset as TSmallBlockPoolHeader.PreviousPartiallyFreePool.}
    PreviousPartiallyFreePool: PSmallBlockPoolHeader;
    {The offset of the last block that was served sequentially. The field must
     be at the same offset as TSmallBlockPoolHeader.FirstFreeBlock.}
    NextSequentialFeedBlockAddress: Pointer;
    {The last block that can be served sequentially.}
    MaxSequentialFeedBlockAddress: Pointer;
    {The pool that is current being used to serve blocks in sequential order}
    CurrentSequentialFeedPool: PSmallBlockPoolHeader;
{$ifdef UseCustomFixedSizeMoveRoutines}
    {The fixed size move procedure used to move data for this block size when
     it is upsized. When a block is downsized (which usually does not occur
     that often) the variable size move routine is used.}
    UpsizeMoveProcedure: TMoveProc;
{$else}
    Reserved1: Pointer;
{$endif}
{$ifdef 64Bit}
    {Pad to 64 bytes for 64-bit}
    Reserved2: Pointer;
{$endif}
  end;

  {Small block pool (Size = 32 bytes for 32-bit, 48 bytes for 64-bit).}
  TSmallBlockPoolHeader = record
    {BlockType}
    BlockType: PSmallBlockType;
{$ifdef 32Bit}
    {Align the next fields to the same fields in TSmallBlockType and pad this
     structure to 32 bytes for 32-bit}
    Reserved1: Cardinal;
{$endif}
    {The next and previous pool that has free blocks of this size. Do not
     change the position of these two fields: They must be at the same offsets
     as the fields in TSmallBlockType of the same name.}
    NextPartiallyFreePool: PSmallBlockPoolHeader;
    PreviousPartiallyFreePool: PSmallBlockPoolHeader;
    {Pointer to the first free block inside this pool. This field must be at
     the same offset as TSmallBlockType.NextSequentialFeedBlockAddress.}
    FirstFreeBlock: Pointer;
    {The number of blocks allocated in this pool.}
    BlocksInUse: Cardinal;
    {Padding}
    Reserved2: Cardinal;
    {The pool pointer and flags of the first block}
    FirstBlockPoolPointerAndFlags: NativeUInt;
  end;

  {Small block layout:
   At offset -SizeOf(Pointer) = Flags + address of the small block pool.
   At offset BlockSize - SizeOf(Pointer) = Flags + address of the small block
   pool for the next small block.
  }

  {------------------------Medium block structures------------------------}

  {The medium block pool from which medium blocks are drawn. Size = 16 bytes
   for 32-bit and 32 bytes for 64-bit.}
  PMediumBlockPoolHeader = ^TMediumBlockPoolHeader;
  TMediumBlockPoolHeader = record
    {Points to the previous and next medium block pools. This circular linked
     list is used to track memory leaks on program shutdown.}
    PreviousMediumBlockPoolHeader: PMediumBlockPoolHeader;
    NextMediumBlockPoolHeader: PMediumBlockPoolHeader;
    {Padding}
    Reserved1: NativeUInt;
    {The block size and flags of the first medium block in the block pool}
    FirstMediumBlockSizeAndFlags: NativeUInt;
  end;

  {Medium block layout:
   Offset: -2 * SizeOf(Pointer) = Previous Block Size (only if the previous block is free)
   Offset: -SizeOf(Pointer) = This block size and flags
   Offset: 0 = User data / Previous Free Block (if this block is free)
   Offset: SizeOf(Pointer) = Next Free Block (if this block is free)
   Offset: BlockSize - 2*SizeOf(Pointer) = Size of this block (if this block is free)
   Offset: BlockSize - SizeOf(Pointer) = Size of the next block and flags

  {A medium block that is unused}
  PMediumFreeBlock = ^TMediumFreeBlock;
  TMediumFreeBlock = record
    PreviousFreeBlock: PMediumFreeBlock;
    NextFreeBlock: PMediumFreeBlock;
  end;

  {-------------------------Large block structures------------------------}

  {Large block header record (Size = 16 for 32-bit, 32 for 64-bit)}
  PLargeBlockHeader = ^TLargeBlockHeader;
  TLargeBlockHeader = record
    {Points to the previous and next large blocks. This circular linked
     list is used to track memory leaks on program shutdown.}
    PreviousLargeBlockHeader: PLargeBlockHeader;
    NextLargeBlockHeader: PLargeBlockHeader;
    {The user allocated size of the Large block}
    UserAllocatedSize: NativeUInt;
    {The size of this block plus the flags}
    BlockSizeAndFlags: NativeUInt;
  end;

  {-------------------------Expected Memory Leak Structures--------------------}
{$ifdef EnableMemoryLeakReporting}

  {The layout of an expected leak. All fields may not be specified, in which
   case it may be harder to determine which leaks are expected and which are
   not.}
  PExpectedMemoryLeak = ^TExpectedMemoryLeak;
  PPExpectedMemoryLeak = ^PExpectedMemoryLeak;
  TExpectedMemoryLeak = record
    {Linked list pointers}
    PreviousLeak, NextLeak: PExpectedMemoryLeak;
    {Information about the expected leak}
    LeakAddress: Pointer;
    LeakedClass: TClass;
    {$ifdef CheckCppObjectTypeEnabled}
    LeakedCppTypeIdPtr: Pointer;
    {$endif}
    LeakSize: NativeInt;
    LeakCount: Integer;
  end;

  TExpectedMemoryLeaks = record
    {The number of entries used in the expected leaks buffer}
    EntriesUsed: Integer;
    {Freed entries}
    FirstFreeSlot: PExpectedMemoryLeak;
    {Entries with the address specified}
    FirstEntryByAddress: PExpectedMemoryLeak;
    {Entries with no address specified, but with the class specified}
    FirstEntryByClass: PExpectedMemoryLeak;
    {Entries with only size specified}
    FirstEntryBySizeOnly: PExpectedMemoryLeak;
    {The expected leaks buffer (Need to leave space for this header)}
    ExpectedLeaks: array[0..(ExpectedMemoryLeaksListSize - 64) div SizeOf(TExpectedMemoryLeak) - 1] of TExpectedMemoryLeak;
  end;
  PExpectedMemoryLeaks = ^TExpectedMemoryLeaks;

{$endif}

{-------------------------Private constants----------------------------}
const
{$ifndef BCB6OrDelphi7AndUp}
  reOutOfMemory = 1;
  reInvalidPtr = 2;
{$endif}
  {The size of the block header in front of small and medium blocks}
  BlockHeaderSize = SizeOf(Pointer);
  {The size of a small block pool header}
  SmallBlockPoolHeaderSize = SizeOf(TSmallBlockPoolHeader);
  {The size of a medium block pool header}
  MediumBlockPoolHeaderSize = SizeOf(TMediumBlockPoolHeader);
  {The size of the header in front of Large blocks}
  LargeBlockHeaderSize = SizeOf(TLargeBlockHeader);
{$ifdef FullDebugMode}
  {We need space for the header, the trailer checksum and the trailing block
   size (only used by freed medium blocks).}
  FullDebugBlockOverhead = SizeOf(TFullDebugBlockHeader) + SizeOf(NativeUInt) + SizeOf(Pointer);
{$endif}

{-------------------------Private variables----------------------------}
var
  {-----------------Small block management------------------}
  {The small block types. Sizes include the leading header. Sizes are
   picked to limit maximum wastage to about 10% or 256 bytes (whichever is
   less) where possible.}
  SmallBlockTypes: array[0..NumSmallBlockTypes - 1] of TSmallBlockType =(
    {8/16 byte jumps}
{$ifndef Align16Bytes}
    (BlockSize: 8 {$ifdef UseCustomFixedSizeMoveRoutines}; UpsizeMoveProcedure: Move4{$endif}),
{$endif}
    (BlockSize: 16 {$ifdef UseCustomFixedSizeMoveRoutines}; UpsizeMoveProcedure: {$ifdef 32Bit}Move12{$else}Move8{$endif}{$endif}),
{$ifndef Align16Bytes}
    (BlockSize: 24 {$ifdef UseCustomFixedSizeMoveRoutines}; UpsizeMoveProcedure: Move20{$endif}),
{$endif}
    (BlockSize: 32 {$ifdef UseCustomFixedSizeMoveRoutines}; UpsizeMoveProcedure: {$ifdef 32Bit}Move28{$else}Move24{$endif}{$endif}),
{$ifndef Align16Bytes}
    (BlockSize: 40 {$ifdef UseCustomFixedSizeMoveRoutines}; UpsizeMoveProcedure: Move36{$endif}),
{$endif}
    (BlockSize: 48 {$ifdef UseCustomFixedSizeMoveRoutines}; UpsizeMoveProcedure: {$ifdef 32Bit}Move44{$else}Move40{$endif}{$endif}),
{$ifndef Align16Bytes}
    (BlockSize: 56 {$ifdef UseCustomFixedSizeMoveRoutines}; UpsizeMoveProcedure: Move52{$endif}),
{$endif}
    (BlockSize: 64 {$ifdef UseCustomFixedSizeMoveRoutines}; UpsizeMoveProcedure: {$ifdef 32Bit}Move60{$else}Move56{$endif}{$endif}),
{$ifndef Align16Bytes}
    (BlockSize: 72 {$ifdef UseCustomFixedSizeMoveRoutines}; UpsizeMoveProcedure: Move68{$endif}),
{$endif}
    (BlockSize: 80),
{$ifndef Align16Bytes}
    (BlockSize: 88),
{$endif}
    (BlockSize: 96),
{$ifndef Align16Bytes}
    (BlockSize: 104),
{$endif}
    (BlockSize: 112),
{$ifndef Align16Bytes}
    (BlockSize: 120),
{$endif}
    (BlockSize: 128),
{$ifndef Align16Bytes}
    (BlockSize: 136),
{$endif}
    (BlockSize: 144),
{$ifndef Align16Bytes}
    (BlockSize: 152),
{$endif}
    (BlockSize: 160),
    {16 byte jumps}
    (BlockSize: 176),
    (BlockSize: 192),
    (BlockSize: 208),
    (BlockSize: 224),
    (BlockSize: 240),
    (BlockSize: 256),
    (BlockSize: 272),
    (BlockSize: 288),
    (BlockSize: 304),
    (BlockSize: 320),
    {32 byte jumps}
    (BlockSize: 352),
    (BlockSize: 384),
    (BlockSize: 416),
    (BlockSize: 448),
    (BlockSize: 480),
    {48 byte jumps}
    (BlockSize: 528),
    (BlockSize: 576),
    (BlockSize: 624),
    (BlockSize: 672),
    {64 byte jumps}
    (BlockSize: 736),
    (BlockSize: 800),
    {80 byte jumps}
    (BlockSize: 880),
    (BlockSize: 960),
    {96 byte jumps}
    (BlockSize: 1056),
    (BlockSize: 1152),
    {112 byte jumps}
    (BlockSize: 1264),
    (BlockSize: 1376),
    {128 byte jumps}
    (BlockSize: 1504),
    {144 byte jumps}
    (BlockSize: 1648),
    {160 byte jumps}
    (BlockSize: 1808),
    {176 byte jumps}
    (BlockSize: 1984),
    {192 byte jumps}
    (BlockSize: 2176),
    {208 byte jumps}
    (BlockSize: 2384),
    {224 byte jumps}
    (BlockSize: MaximumSmallBlockSize),
    {The last block size occurs three times. If, during a GetMem call, the
     requested block size is already locked by another thread then up to two
     larger block sizes may be used instead. Having the last block size occur
     three times avoids the need to have a size overflow check.}
    (BlockSize: MaximumSmallBlockSize),
    (BlockSize: MaximumSmallBlockSize));
  {Size to small block type translation table}
  AllocSize2SmallBlockTypeIndX4: array[0..(MaximumSmallBlockSize - 1) div SmallBlockGranularity] of Byte;
  {-----------------Medium block management------------------}
  {A dummy medium block pool header: Maintains a circular list of all medium
   block pools to enable memory leak detection on program shutdown.}
  MediumBlockPoolsCircularList: TMediumBlockPoolHeader;
  {Are medium blocks locked?}
  MediumBlocksLocked: Boolean;
  {The sequential feed medium block pool.}
  LastSequentiallyFedMediumBlock: Pointer;
  MediumSequentialFeedBytesLeft: Cardinal;
  {The medium block bins are divided into groups of 32 bins. If a bit
   is set in this group bitmap, then at least one bin in the group has free
   blocks.}
  MediumBlockBinGroupBitmap: Cardinal;
  {The medium block bins: total of 32 * 32 = 1024 bins of a certain
   minimum size.}
  MediumBlockBinBitmaps: array[0..MediumBlockBinGroupCount - 1] of Cardinal;
  {The medium block bins. There are 1024 LIFO circular linked lists each
   holding blocks of a specified minimum size. The sizes vary in size from
   MinimumMediumBlockSize to MaximumMediumBlockSize. The bins are treated as
   type TMediumFreeBlock to avoid pointer checks.}
  MediumBlockBins: array[0..MediumBlockBinCount - 1] of TMediumFreeBlock;
  {-----------------Large block management------------------}
  {Are large blocks locked?}
  LargeBlocksLocked: Boolean;
  {A dummy large block header: Maintains a list of all allocated large blocks
   to enable memory leak detection on program shutdown.}
  LargeBlocksCircularList: TLargeBlockHeader;
  {-------------------------Expected Memory Leak Structures--------------------}
{$ifdef EnableMemoryLeakReporting}
  {The expected memory leaks}
  ExpectedMemoryLeaks: PExpectedMemoryLeaks;
  ExpectedMemoryLeaksListLocked: Boolean;
{$endif}
  {---------------------Full Debug Mode structures--------------------}
{$ifdef FullDebugMode}
  {The allocation group stack}
  AllocationGroupStack: array[0..AllocationGroupStackSize - 1] of Cardinal;
  {The allocation group stack top (it is an index into AllocationGroupStack)}
  AllocationGroupStackTop: Cardinal;
  {The last allocation number used}
  CurrentAllocationNumber: Cardinal;
  {This is a count of the number of threads currently inside any of the
   FullDebugMode GetMem, Freemem or ReallocMem handlers. If this value
   is negative then a block scan is in progress and no thread may
   allocate, free or reallocate any block or modify any FullDebugMode
   block header or footer.}
  ThreadsInFullDebugModeRoutine: Integer;
  {The current log file name}
  MMLogFileName: array[0..1023] of AnsiChar;
  {The 64K block of reserved memory used to trap invalid memory accesses using
   fields in a freed object.}
  ReservedBlock: Pointer;
  {The virtual method index count - used to get the virtual method index for a
   virtual method call on a freed object.}
  VMIndex: Integer;
  {The fake VMT used to catch virtual method calls on freed objects.}
  FreedObjectVMT: packed record
    VMTData: array[vmtSelfPtr .. vmtParent + SizeOf(Pointer) - 1] of byte;
    VMTMethods: array[SizeOf(Pointer) + vmtParent .. vmtParent + MaxFakeVMTEntries * SizeOf(Pointer) + SizeOf(Pointer) - 1] of Byte;
  end;
  {$ifdef CatchUseOfFreedInterfaces}
  VMTBadInterface: array[0..MaxFakeVMTEntries - 1] of Pointer;
  {$endif}
{$endif}
  {--------------Other info--------------}
  {The memory manager that was replaced}
  OldMemoryManager: {$ifndef BDS2006AndUp}TMemoryManager{$else}TMemoryManagerEx{$endif};
  {The replacement memory manager}
  NewMemoryManager: {$ifndef BDS2006AndUp}TMemoryManager{$else}TMemoryManagerEx{$endif};
{$ifdef DetectMMOperationsAfterUninstall}
  {Invalid handlers to catch MM operations after uninstall}
  InvalidMemoryManager: {$ifndef BDS2006AndUp}TMemoryManager{$else}TMemoryManagerEx{$endif} = (
    GetMem: InvalidGetMem;
    FreeMem: InvalidFreeMem;
    ReallocMem: InvalidReallocMem
  {$ifdef BDS2006AndUp};
    AllocMem: InvalidAllocMem;
    RegisterExpectedMemoryLeak: InvalidRegisterAndUnRegisterMemoryLeak;
    UnRegisterExpectedMemoryLeak: InvalidRegisterAndUnRegisterMemoryLeak;
  {$endif}
  );
{$endif}

{$ifdef MMSharingEnabled}
  {A string uniquely identifying the current process (for sharing the memory
   manager between DLLs and the main application)}
  MappingObjectName: array[0..25] of AnsiChar = ('L', 'o', 'c', 'a', 'l', '\',
    'F', 'a', 's', 't', 'M', 'M', '_', 'P', 'I', 'D', '_', '?', '?', '?', '?',
    '?', '?', '?', '?', #0);
{$ifdef EnableBackwardCompatibleMMSharing}
  UniqueProcessIDString: array[1..20] of AnsiChar = ('?', '?', '?', '?', '?',
    '?', '?', '?', '_', 'P', 'I', 'D', '_', 'F', 'a', 's', 't', 'M', 'M', #0);
  UniqueProcessIDStringBE: array[1..23] of AnsiChar = ('?', '?', '?', '?', '?',
    '?', '?', '?', '_', 'P', 'I', 'D', '_', 'F', 'a', 's', 't', 'M', 'M', '_',
    'B', 'E', #0);
  {The handle of the MM window}
  MMWindow: HWND;
  {The handle of the MM window (for default MM of Delphi 2006 compatibility)}
  MMWindowBE: HWND;
{$endif}
  {The handle of the memory mapped file}
  MappingObjectHandle: NativeUInt;
{$endif}
  {Has FastMM been installed?}
  FastMMIsInstalled: Boolean;
  {Is the MM in place a shared memory manager?}
  IsMemoryManagerOwner: Boolean;
  {Must MMX be used for move operations?}
{$ifdef EnableMMX}
  {$ifndef ForceMMX}
  UseMMX: Boolean;
  {$endif}
{$endif}
  {Is a MessageBox currently showing? If so, do not show another one.}
  ShowingMessageBox: Boolean;
  {True if RunInitializationCode has been called already.}
  InitializationCodeHasRun: Boolean = False;

{----------------Utility Functions------------------}

{A copy of StrLen in order to avoid the SysUtils unit, which would have
 introduced overhead like exception handling code.}
function StrLen(const AStr: PAnsiChar): NativeUInt;
{$ifndef Use32BitAsm}
begin
  Result := 0;
  while AStr[Result] <> #0 do
    Inc(Result);
end;
{$else}
asm
  {Check the first byte}
  cmp byte ptr [eax], 0
  je @ZeroLength
  {Get the negative of the string start in edx}
  mov edx, eax
  neg edx
  {Word align}
  add eax, 1
  and eax, -2
@ScanLoop:
  mov cx, [eax]
  add eax, 2
  test cl, ch
  jnz @ScanLoop
  test cl, cl
  jz @ReturnLess2
  test ch, ch
  jnz @ScanLoop
  lea eax, [eax + edx - 1]
  ret
@ReturnLess2:
  lea eax, [eax + edx - 2]
  ret
@ZeroLength:
  xor eax, eax
end;
{$endif}

{$ifdef EnableMMX}
{$ifndef ForceMMX}
{Returns true if the CPUID instruction is supported}
function CPUID_Supported: Boolean;
asm
  pushfd
  pop eax
  mov edx, eax
  xor eax, $200000
  push eax
  popfd
  pushfd
  pop eax
  xor eax, edx
  setnz al
end;

{Gets the CPUID}
function GetCPUID(AInfoRequired: Integer): TRegisters;
asm
  push ebx
  push esi
  mov esi, edx
  {cpuid instruction}
{$ifdef Delphi4or5}
  db $0f, $a2
{$else}
  cpuid
{$endif}
  {Save registers}
  mov TRegisters[esi].RegEAX, eax
  mov TRegisters[esi].RegEBX, ebx
  mov TRegisters[esi].RegECX, ecx
  mov TRegisters[esi].RegEDX, edx
  pop esi
  pop ebx
end;

{Returns true if the CPU supports MMX}
function MMX_Supported: Boolean;
var
  LReg: TRegisters;
begin
  if CPUID_Supported then
  begin
    {Get the CPUID}
    LReg := GetCPUID(1);
    {Bit 23 must be set for MMX support}
    Result := LReg.RegEDX and $800000 <> 0;
  end
  else
    Result := False;
end;
{$endif}
{$endif}

{Compare [AAddress], CompareVal:
 If Equal: [AAddress] := NewVal and result = CompareVal
 If Unequal: Result := [AAddress]}
function LockCmpxchg(CompareVal, NewVal: Byte; AAddress: PByte): Byte;
asm
{$ifdef 32Bit}
  {On entry:
    al = CompareVal,
    dl = NewVal,
    ecx = AAddress}
  {$ifndef LINUX}
  lock cmpxchg [ecx], dl
  {$else}
  {Workaround for Kylix compiler bug}
  db $F0, $0F, $B0, $11
  {$endif}
{$else}
  {On entry:
    cl = CompareVal
    dl = NewVal
    r8 = AAddress}
  .noframe
  mov rax, rcx
  lock cmpxchg [r8], dl
{$endif}
end;

{$ifndef ASMVersion}
{Gets the first set bit in the 32-bit number, returning the bit index}
function FindFirstSetBit(ACardinal: Cardinal): Cardinal;
asm
{$ifdef 64Bit}
  .noframe
  mov rax, rcx
{$endif}
  bsf eax, eax
end;
{$endif}

{$ifdef MACOS}

function StrLCopy(Dest: PAnsiChar; const Source: PAnsiChar; MaxLen: Cardinal): PAnsiChar;
var
  Len: Cardinal;
begin
  Result := Dest;
  Len := StrLen(Source);
  if Len > MaxLen then
    Len := MaxLen;
  Move(Source^, Dest^, Len * SizeOf(AnsiChar));
  Dest[Len] := #0;
end;

function GetModuleFileName(Module: HMODULE; Buffer: PAnsiChar; BufLen: Integer): Integer;
const
  CUnknown: AnsiString = 'unknown';
var
  tmp: array[0..512] of Char;
begin
  if FastMMIsInstalled then
  begin
    Result := System.GetModuleFileName(Module, tmp, BufLen);
    StrLCopy(Buffer, PAnsiChar(AnsiString(tmp)), BufLen);
  end
  else
  begin
    Result := Length(CUnknown);
    StrLCopy(Buffer, Pointer(CUnknown), Result + 1);
  end;
end;

const
  INVALID_HANDLE_VALUE = THandle(-1);

function FileCreate(const FileName: string): THandle;
begin
  Result := THandle(__open(PAnsiChar(UTF8String(FileName)), O_RDWR or O_CREAT or O_TRUNC or O_EXCL, FileAccessRights));
end;

{$endif}

{Writes the module filename to the specified buffer and returns the number of
 characters written.}
function AppendModuleFileName(ABuffer: PAnsiChar): Integer;
var
  LModuleHandle: HModule;
begin
  {Get the module handle}
{$ifndef borlndmmdll}
  if IsLibrary then
    LModuleHandle := HInstance
  else
{$endif}
    LModuleHandle := 0;
  {Get the module name}
{$ifndef POSIX}
  Result := GetModuleFileNameA(LModuleHandle, ABuffer, 512);
{$else}
  Result := GetModuleFileName(LModuleHandle, ABuffer, 512);
{$endif}
end;

{Copies the name of the module followed by the given string to the buffer,
 returning the pointer following the buffer.}
function AppendStringToModuleName(AString, ABuffer: PAnsiChar): PAnsiChar;
var
  LModuleNameLength: Cardinal;
  LCopyStart: PAnsiChar;
begin
  {Get the name of the application}
  LModuleNameLength := AppendModuleFileName(ABuffer);
  {Replace the last few characters}
  if LModuleNameLength > 0 then
  begin
    {Find the last backslash}
    LCopyStart := PAnsiChar(PByte(ABuffer) + LModuleNameLength - 1);
    LModuleNameLength := 0;
    while (UIntPtr(LCopyStart) >= UIntPtr(ABuffer))
      and (LCopyStart^ <> '\') do
    begin
      Inc(LModuleNameLength);
      Dec(LCopyStart);
    end;
    {Copy the name to the start of the buffer}
    Inc(LCopyStart);
    System.Move(LCopyStart^, ABuffer^, LModuleNameLength);
    Inc(ABuffer, LModuleNameLength);
    ABuffer^ := ':';
    Inc(ABuffer);
    ABuffer^ := ' ';
    Inc(ABuffer);
  end;
  {Append the string}
  while AString^ <> #0 do
  begin
    ABuffer^ := AString^;
    Inc(ABuffer);
    {Next char}
    Inc(AString);
  end;
  ABuffer^ := #0;
  Result := ABuffer;
end;

{----------------Faster Move Procedures-------------------}

{Fixed size move operations ignore the size parameter. All moves are assumed to
 be non-overlapping.}

procedure Move4(const ASource; var ADest; ACount: NativeInt);
asm
{$ifdef 32Bit}
  mov eax, [eax]
  mov [edx], eax
{$else}
.noframe
  mov eax, [rcx]
  mov [rdx], eax
{$endif}
end;

{$ifdef 64Bit}
procedure Move8(const ASource; var ADest; ACount: NativeInt);
asm
  mov rax, [rcx]
  mov [rdx], rax
end;
{$endif}

procedure Move12(const ASource; var ADest; ACount: NativeInt);
asm
{$ifdef 32Bit}
  mov ecx, [eax]
  mov [edx], ecx
  mov ecx, [eax + 4]
  mov eax, [eax + 8]
  mov [edx + 4], ecx
  mov [edx + 8], eax
{$else}
.noframe
  mov rax, [rcx]
  mov ecx, [rcx + 8]
  mov [rdx], rax
  mov [rdx + 8], ecx
{$endif}
end;

procedure Move20(const ASource; var ADest; ACount: NativeInt);
asm
{$ifdef 32Bit}
  mov ecx, [eax]
  mov [edx], ecx
  mov ecx, [eax + 4]
  mov [edx + 4], ecx
  mov ecx, [eax + 8]
  mov [edx + 8], ecx
  mov ecx, [eax + 12]
  mov eax, [eax + 16]
  mov [edx + 12], ecx
  mov [edx + 16], eax
{$else}
.noframe
  movdqa xmm0, [rcx]
  mov ecx, [rcx + 16]
  movdqa [rdx], xmm0
  mov [rdx + 16], ecx
{$endif}
end;

{$ifdef 64Bit}
procedure Move24(const ASource; var ADest; ACount: NativeInt);
asm
  movdqa xmm0, [rcx]
  mov r8, [rcx + 16]
  movdqa [rdx], xmm0
  mov [rdx + 16], r8
end;
{$endif}

procedure Move28(const ASource; var ADest; ACount: NativeInt);
asm
{$ifdef 32Bit}
  mov ecx, [eax]
  mov [edx], ecx
  mov ecx, [eax + 4]
  mov [edx + 4], ecx
  mov ecx, [eax + 8]
  mov [edx + 8], ecx
  mov ecx, [eax + 12]
  mov [edx + 12], ecx
  mov ecx, [eax + 16]
  mov [edx + 16], ecx
  mov ecx, [eax + 20]
  mov eax, [eax + 24]
  mov [edx + 20], ecx
  mov [edx + 24], eax
{$else}
.noframe
  movdqa xmm0, [rcx]
  mov r8, [rcx + 16]
  mov ecx, [rcx + 24]
  movdqa [rdx], xmm0
  mov [rdx + 16], r8
  mov [rdx + 24], ecx
{$endif}
end;

procedure Move36(const ASource; var ADest; ACount: NativeInt);
asm
{$ifdef 32Bit}
  fild qword ptr [eax]
  fild qword ptr [eax + 8]
  fild qword ptr [eax + 16]
  fild qword ptr [eax + 24]
  mov ecx, [eax + 32]
  mov [edx + 32], ecx
  fistp qword ptr [edx + 24]
  fistp qword ptr [edx + 16]
  fistp qword ptr [edx + 8]
  fistp qword ptr [edx]
{$else}
.noframe
  movdqa xmm0, [rcx]
  movdqa xmm1, [rcx + 16]
  mov ecx, [rcx + 32]
  movdqa [rdx], xmm0
  movdqa [rdx + 16], xmm1
  mov [rdx + 32], ecx
{$endif}
end;

{$ifdef 64Bit}
procedure Move40(const ASource; var ADest; ACount: NativeInt);
asm
  movdqa xmm0, [rcx]
  movdqa xmm1, [rcx + 16]
  mov r8, [rcx + 32]
  movdqa [rdx], xmm0
  movdqa [rdx + 16], xmm1
  mov [rdx + 32], r8
end;
{$endif}

procedure Move44(const ASource; var ADest; ACount: NativeInt);
asm
{$ifdef 32Bit}
  fild qword ptr [eax]
  fild qword ptr [eax + 8]
  fild qword ptr [eax + 16]
  fild qword ptr [eax + 24]
  fild qword ptr [eax + 32]
  mov ecx, [eax + 40]
  mov [edx + 40], ecx
  fistp qword ptr [edx + 32]
  fistp qword ptr [edx + 24]
  fistp qword ptr [edx + 16]
  fistp qword ptr [edx + 8]
  fistp qword ptr [edx]
{$else}
.noframe
  movdqa xmm0, [rcx]
  movdqa xmm1, [rcx + 16]
  mov r8, [rcx + 32]
  mov ecx, [rcx + 40]
  movdqa [rdx], xmm0
  movdqa [rdx + 16], xmm1
  mov [rdx + 32], r8
  mov [rdx + 40], ecx
{$endif}
end;

procedure Move52(const ASource; var ADest; ACount: NativeInt);
asm
{$ifdef 32Bit}
  fild qword ptr [eax]
  fild qword ptr [eax + 8]
  fild qword ptr [eax + 16]
  fild qword ptr [eax + 24]
  fild qword ptr [eax + 32]
  fild qword ptr [eax + 40]
  mov ecx, [eax + 48]
  mov [edx + 48], ecx
  fistp qword ptr [edx + 40]
  fistp qword ptr [edx + 32]
  fistp qword ptr [edx + 24]
  fistp qword ptr [edx + 16]
  fistp qword ptr [edx + 8]
  fistp qword ptr [edx]
{$else}
.noframe
  movdqa xmm0, [rcx]
  movdqa xmm1, [rcx + 16]
  movdqa xmm2, [rcx + 32]
  mov ecx, [rcx + 48]
  movdqa [rdx], xmm0
  movdqa [rdx + 16], xmm1
  movdqa [rdx + 32], xmm2
  mov [rdx + 48], ecx
{$endif}
end;

{$ifdef 64Bit}
procedure Move56(const ASource; var ADest; ACount: NativeInt);
asm
  movdqa xmm0, [rcx]
  movdqa xmm1, [rcx + 16]
  movdqa xmm2, [rcx + 32]
  mov r8, [rcx + 48]
  movdqa [rdx], xmm0
  movdqa [rdx + 16], xmm1
  movdqa [rdx + 32], xmm2
  mov [rdx + 48], r8
end;
{$endif}

procedure Move60(const ASource; var ADest; ACount: NativeInt);
asm
{$ifdef 32Bit}
  fild qword ptr [eax]
  fild qword ptr [eax + 8]
  fild qword ptr [eax + 16]
  fild qword ptr [eax + 24]
  fild qword ptr [eax + 32]
  fild qword ptr [eax + 40]
  fild qword ptr [eax + 48]
  mov ecx, [eax + 56]
  mov [edx + 56], ecx
  fistp qword ptr [edx + 48]
  fistp qword ptr [edx + 40]
  fistp qword ptr [edx + 32]
  fistp qword ptr [edx + 24]
  fistp qword ptr [edx + 16]
  fistp qword ptr [edx + 8]
  fistp qword ptr [edx]
{$else}
.noframe
  movdqa xmm0, [rcx]
  movdqa xmm1, [rcx + 16]
  movdqa xmm2, [rcx + 32]
  mov r8, [rcx + 48]
  mov ecx, [rcx + 56]
  movdqa [rdx], xmm0
  movdqa [rdx + 16], xmm1
  movdqa [rdx + 32], xmm2
  mov [rdx + 48], r8
  mov [rdx + 56], ecx
{$endif}
end;

procedure Move68(const ASource; var ADest; ACount: NativeInt);
asm
{$ifdef 32Bit}
  fild qword ptr [eax]
  fild qword ptr [eax + 8]
  fild qword ptr [eax + 16]
  fild qword ptr [eax + 24]
  fild qword ptr [eax + 32]
  fild qword ptr [eax + 40]
  fild qword ptr [eax + 48]
  fild qword ptr [eax + 56]
  mov ecx, [eax + 64]
  mov [edx + 64], ecx
  fistp qword ptr [edx + 56]
  fistp qword ptr [edx + 48]
  fistp qword ptr [edx + 40]
  fistp qword ptr [edx + 32]
  fistp qword ptr [edx + 24]
  fistp qword ptr [edx + 16]
  fistp qword ptr [edx + 8]
  fistp qword ptr [edx]
{$else}
.noframe
  movdqa xmm0, [rcx]
  movdqa xmm1, [rcx + 16]
  movdqa xmm2, [rcx + 32]
  movdqa xmm3, [rcx + 48]
  mov ecx, [rcx + 64]
  movdqa [rdx], xmm0
  movdqa [rdx + 16], xmm1
  movdqa [rdx + 32], xmm2
  movdqa [rdx + 48], xmm3
  mov [rdx + 64], ecx
{$endif}
end;

{Variable size move procedure: Rounds ACount up to the next multiple of 16 less
 SizeOf(Pointer). Important note: Always moves at least 16 - SizeOf(Pointer)
 bytes (the minimum small block size with 16 byte alignment), irrespective of
 ACount.}
procedure MoveX16LP(const ASource; var ADest; ACount: NativeInt);
asm
{$ifdef 32Bit}
  {Make the counter negative based: The last 12 bytes are moved separately}
  sub ecx, 12
  add eax, ecx
  add edx, ecx
{$ifdef EnableMMX}
  {$ifndef ForceMMX}
  cmp UseMMX, True
  jne @FPUMove
  {$endif}
  {Make the counter negative based: The last 12 bytes are moved separately}
  neg ecx
  jns @MMXMoveLast12
@MMXMoveLoop:
  {Move a 16 byte block}
  {$ifdef Delphi4or5}
  {Delphi 5 compatibility}
  db $0f, $6f, $04, $01
  db $0f, $6f, $4c, $01, $08
  db $0f, $7f, $04, $11
  db $0f, $7f, $4c, $11, $08
  {$else}
  movq mm0, [eax + ecx]
  movq mm1, [eax + ecx + 8]
  movq [edx + ecx], mm0
  movq [edx + ecx + 8], mm1
  {$endif}
  {Are there another 16 bytes to move?}
  add ecx, 16
  js @MMXMoveLoop
@MMXMoveLast12:
  {Do the last 12 bytes}
  {$ifdef Delphi4or5}
  {Delphi 5 compatibility}
  db $0f, $6f, $04, $01
  {$else}
  movq mm0, [eax + ecx]
  {$endif}
  mov eax, [eax + ecx + 8]
  {$ifdef Delphi4or5}
  {Delphi 5 compatibility}
  db $0f, $7f, $04, $11
  {$else}
  movq [edx + ecx], mm0
  {$endif}
  mov [edx + ecx + 8], eax
  {Exit MMX state}
  {$ifdef Delphi4or5}
  {Delphi 5 compatibility}
  db $0f, $77
  {$else}
  emms
  {$endif}
  {$ifndef ForceMMX}
  ret
  {$endif}
{$endif}
{FPU code is only used if MMX is not forced}
{$ifndef ForceMMX}
@FPUMove:
  neg ecx
  jns @FPUMoveLast12
@FPUMoveLoop:
  {Move a 16 byte block}
  fild qword ptr [eax + ecx]
  fild qword ptr [eax + ecx + 8]
  fistp qword ptr [edx + ecx + 8]
  fistp qword ptr [edx + ecx]
  {Are there another 16 bytes to move?}
  add ecx, 16
  js @FPUMoveLoop
@FPUMoveLast12:
  {Do the last 12 bytes}
  fild qword ptr [eax + ecx]
  fistp qword ptr [edx + ecx]
  mov eax, [eax + ecx + 8]
  mov [edx + ecx + 8], eax
{$endif}
{$else}
.noframe
  {Make the counter negative based: The last 8 bytes are moved separately}
  sub r8, 8
  add rcx, r8
  add rdx, r8
  neg r8
  jns @MoveLast12
@MoveLoop:
  {Move a 16 byte block}
  movdqa xmm0, [rcx + r8]
  movdqa [rdx + r8], xmm0
  {Are there another 16 bytes to move?}
  add r8, 16
  js @MoveLoop
@MoveLast12:
  {Do the last 8 bytes}
  mov r9, [rcx + r8]
  mov [rdx + r8], r9
{$endif}
end;

{Variable size move procedure: Rounds ACount up to the next multiple of 8 less
 SizeOf(Pointer). Important note: Always moves at least 8 - SizeOf(Pointer)
 bytes (the minimum small block size with 8 byte alignment), irrespective of
 ACount.}
procedure MoveX8LP(const ASource; var ADest; ACount: NativeInt);
asm
{$ifdef 32Bit}
  {Make the counter negative based: The last 4 bytes are moved separately}
  sub ecx, 4
  {4 bytes or less? -> Use the Move4 routine.}
  jle @FourBytesOrLess
  add eax, ecx
  add edx, ecx
  neg ecx
{$ifdef EnableMMX}
  {$ifndef ForceMMX}
  cmp UseMMX, True
  jne @FPUMoveLoop
  {$endif}
@MMXMoveLoop:
  {Move an 8 byte block}
{$ifdef Delphi4or5}
  {Delphi 5 compatibility}
  db $0f, $6f, $04, $01
  db $0f, $7f, $04, $11
{$else}
  movq mm0, [eax + ecx]
  movq [edx + ecx], mm0
{$endif}
  {Are there another 8 bytes to move?}
  add ecx, 8
  js @MMXMoveLoop
  {Exit MMX state}
{$ifdef Delphi4or5}
  {Delphi 5 compatibility}
  db $0f, $77
{$else}
  emms
{$endif}
  {Do the last 4 bytes}
  mov eax, [eax + ecx]
  mov [edx + ecx], eax
  ret
{$endif}
{FPU code is only used if MMX is not forced}
{$ifndef ForceMMX}
@FPUMoveLoop:
  {Move an 8 byte block}
  fild qword ptr [eax + ecx]
  fistp qword ptr [edx + ecx]
  {Are there another 8 bytes to move?}
  add ecx, 8
  js @FPUMoveLoop
  {Do the last 4 bytes}
  mov eax, [eax + ecx]
  mov [edx + ecx], eax
  ret
{$endif}
@FourBytesOrLess:
  {Four or less bytes to move}
  mov eax, [eax]
  mov [edx], eax
{$else}
.noframe
  {Make the counter negative based}
  add rcx, r8
  add rdx, r8
  neg r8
@MoveLoop:
  {Move an 8 byte block}
  mov r9, [rcx + r8]
  mov [rdx + r8], r9
  {Are there another 8 bytes to move?}
  add r8, 8
  js @MoveLoop
{$endif}
end;

{----------------Windows Emulation Functions for Kylix / OS X Support-----------------}

{$ifdef POSIX}

const
  {Messagebox constants}
  MB_OK = 0;
  MB_ICONERROR = $10;
  MB_TASKMODAL = $2000;
  MB_DEFAULT_DESKTOP_ONLY = $20000;
  {Virtual memory constants}
  MEM_COMMIT = $1000;
  MEM_RELEASE = $8000;
  MEM_TOP_DOWN = $100000;
  PAGE_READWRITE = 4;

procedure MessageBoxA(hWnd: Cardinal; AMessageText, AMessageTitle: PAnsiChar; uType: Cardinal); stdcall;
begin
  if FastMMIsInstalled then
    writeln(AMessageText)
  else
    __write(STDERR_FILENO, AMessageText, StrLen(AMessageText));
end;

function VirtualAlloc(lpvAddress: Pointer; dwSize, flAllocationType, flProtect: Cardinal): Pointer; stdcall;
begin
  Result := valloc(dwSize);
end;

function VirtualFree(lpAddress: Pointer; dwSize, dwFreeType: Cardinal): LongBool; stdcall;
begin
  free(lpAddress);
  Result := True;
end;

function WriteFile(hFile: THandle; const Buffer; nNumberOfBytesToWrite: Cardinal;
  var lpNumberOfBytesWritten: Cardinal; lpOverlapped: Pointer): Boolean; stdcall;
begin
  lpNumberOfBytesWritten := __write(hFile, @Buffer, nNumberOfBytesToWrite);
  if lpNumberOfBytesWritten = Cardinal(-1) then
  begin
    lpNumberOfBytesWritten := 0;
    Result := False;
  end
  else
    Result := True;
end;

{$ifndef NeverSleepOnThreadContention}
procedure Sleep(dwMilliseconds: Cardinal); stdcall;
begin
  {Convert to microseconds (more or less)}
  usleep(dwMilliseconds shl 10);
end;
{$endif}
{$endif}

{-----------------Debugging Support Functions and Procedures------------------}

{$ifdef FullDebugMode}

{Returns the current thread ID}
function GetThreadID: Cardinal;
{$ifdef 32Bit}
asm
  mov eax, FS:[$24]
end;
{$else}
begin
  Result := GetCurrentThreadId;
end;
{$endif}

{Fills a block of memory with the given dword (32-bit) or qword (64-bit).
 Always fills a multiple of SizeOf(Pointer) bytes}
procedure DebugFillMem(var AAddress; AByteCount: NativeInt; AFillValue: NativeUInt);
asm
{$ifdef 32Bit}
  {On Entry:
   eax = AAddress
   edx = AByteCount
   ecx = AFillValue}
  add eax, edx
  neg edx
  jns @Done
@FillLoop:
  mov [eax + edx], ecx
  add edx, 4
  js @FillLoop
@Done:
{$else}
  {On Entry:
   rcx = AAddress
   rdx = AByteCount
   r8 = AFillValue}
  add rcx, rdx
  neg rdx
  jns @Done
@FillLoop:
  mov [rcx + rdx], r8
  add rdx, 8
  js @FillLoop
@Done:
{$endif}
end;

  {$ifndef LoadDebugDLLDynamically}

{The stack trace procedure. The stack trace module is external since it may
 raise handled access violations that result in the creation of exception
 objects and the stack trace code is not re-entrant.}
procedure GetStackTrace(AReturnAddresses: PNativeUInt;
  AMaxDepth, ASkipFrames: Cardinal); external FullDebugModeLibraryName
  name {$ifdef RawStackTraces}'GetRawStackTrace'{$else}'GetFrameBasedStackTrace'{$endif};

{The exported procedure in the FastMM_FullDebugMode.dll library used to convert
 the return addresses of a stack trace to a text string.}
function LogStackTrace(AReturnAddresses: PNativeUInt;
  AMaxDepth: Cardinal; ABuffer: PAnsiChar): PAnsiChar; external FullDebugModeLibraryName
  name 'LogStackTrace';

  {$else}

  {Default no-op stack trace and logging handlers}
  procedure NoOpGetStackTrace(AReturnAddresses: PNativeUInt;
    AMaxDepth, ASkipFrames: Cardinal);
  begin
    DebugFillMem(AReturnAddresses^, AMaxDepth * SizeOf(Pointer), 0);
  end;

  function NoOpLogStackTrace(AReturnAddresses: PNativeUInt;
    AMaxDepth: Cardinal; ABuffer: PAnsiChar): PAnsiChar;
  begin
    Result := ABuffer;
  end;

var

  {Handle to the FullDebugMode DLL}
  FullDebugModeDLL: HMODULE;

  GetStackTrace: procedure (AReturnAddresses: PNativeUInt;
    AMaxDepth, ASkipFrames: Cardinal) = NoOpGetStackTrace;

  LogStackTrace: function (AReturnAddresses: PNativeUInt;
    AMaxDepth: Cardinal; ABuffer: PAnsiChar): PAnsiChar = NoOpLogStackTrace;

  {$endif}

{$endif}

{$ifndef POSIX}
function DelphiIsRunning: Boolean;
begin
  Result := FindWindowA('TAppBuilder', nil) <> 0;
end;
{$endif}

{Converts an unsigned integer to string at the buffer location, returning the
 new buffer position. Note: The 32-bit asm version only supports numbers up to
 2^31 - 1.}
function NativeUIntToStrBuf(ANum: NativeUInt; APBuffer: PAnsiChar): PAnsiChar;
{$ifndef Use32BitAsm}
const
  MaxDigits = 20;
var
  LDigitBuffer: array[0..MaxDigits - 1] of AnsiChar;
  LCount: Cardinal;
  LDigit: NativeUInt;
begin
  {Generate the digits in the local buffer}
  LCount := 0;
  repeat
    LDigit := ANum;
    ANum := ANum div 10;
    LDigit := LDigit - ANum * 10;
    Inc(LCount);
    LDigitBuffer[MaxDigits - LCount] := AnsiChar(Ord('0') + LDigit);
  until ANum = 0;
  {Copy the digits to the output buffer and advance it}
  System.Move(LDigitBuffer[MaxDigits - LCount], APBuffer^, LCount);
  Result := APBuffer + LCount;
end;
{$else}
asm
  {On entry: eax = ANum, edx = ABuffer}
  push edi
  mov edi, edx                //Pointer to the first character in edi
  {Calculate leading digit: divide the number by 1e9}
  add eax, 1                  //Increment the number
  mov edx, $89705F41          //1e9 reciprocal
  mul edx                     //Multplying with reciprocal
  shr eax, 30                 //Save fraction bits
  mov ecx, edx                //First digit in bits <31:29>
  and edx, $1FFFFFFF          //Filter fraction part edx<28:0>
  shr ecx, 29                 //Get leading digit into accumulator
  lea edx, [edx + 4 * edx]    //Calculate ...
  add edx, eax                //... 5*fraction
  mov eax, ecx                //Copy leading digit
  or eax, '0'                 //Convert digit to ASCII
  mov [edi], al               //Store digit out to memory
  {Calculate digit #2}
  mov eax, edx                //Point format such that 1.0 = 2^28
  cmp ecx, 1                  //Any non-zero digit yet ?
  sbb edi, -1                 //Yes->increment ptr, No->keep old ptr
  shr eax, 28                 //Next digit
  and edx, $0fffffff          //Fraction part edx<27:0>
  or ecx, eax                 //Accumulate next digit
  or eax, '0'                 //Convert digit to ASCII
  mov [edi], al               //Store digit out to memory
  {Calculate digit #3}
  lea eax, [edx * 4 + edx]    //5*fraction, new digit eax<31:27>
  lea edx, [edx * 4 + edx]    //5*fraction, new fraction edx<26:0>
  cmp ecx, 1                  //Any non-zero digit yet ?
  sbb edi, -1                 //Yes->increment ptr, No->keep old ptr
  shr eax, 27                 //Next digit
  and edx, $07ffffff          //Fraction part
  or ecx, eax                 //Accumulate next digit
  or eax, '0'                 //Convert digit to ASCII
  mov [edi], al               //Store digit out to memory
  {Calculate digit #4}
  lea eax, [edx * 4 + edx]    //5*fraction, new digit eax<31:26>
  lea edx, [edx * 4 + edx]    //5*fraction, new fraction edx<25:0>
  cmp ecx, 1                  //Any non-zero digit yet ?
  sbb edi, -1                 //Yes->increment ptr, No->keep old ptr
  shr eax, 26                 //Next digit
  and edx, $03ffffff          //Fraction part
  or ecx, eax                 //Accumulate next digit
  or eax, '0'                 //Convert digit to ASCII
  mov [edi], al               //Store digit out to memory
  {Calculate digit #5}
  lea eax, [edx * 4 + edx]    //5*fraction, new digit eax<31:25>
  lea edx, [edx * 4 + edx]    //5*fraction, new fraction edx<24:0>
  cmp ecx, 1                  //Any non-zero digit yet ?
  sbb edi, -1                 //Yes->increment ptr, No->keep old ptr
  shr eax, 25                 //Next digit
  and edx, $01ffffff          //Fraction part
  or ecx, eax                 //Accumulate next digit
  or eax, '0'                 //Convert digit to ASCII
  mov [edi], al               //Store digit out to memory
  {Calculate digit #6}
  lea eax, [edx * 4 + edx]    //5*fraction, new digit eax<31:24>
  lea edx, [edx * 4 + edx]    //5*fraction, new fraction edx<23:0>
  cmp ecx, 1                  //Any non-zero digit yet ?
  sbb edi, -1                 //Yes->increment ptr, No->keep old ptr
  shr eax, 24                 //Next digit
  and edx, $00ffffff          //Fraction part
  or ecx, eax                 //Accumulate next digit
  or eax, '0'                 //Convert digit to ASCII
  mov [edi], al               //Store digit out to memory
  {Calculate digit #7}
  lea eax, [edx * 4 + edx]    //5*fraction, new digit eax<31:23>
  lea edx, [edx * 4 + edx]    //5*fraction, new fraction edx<31:23>
  cmp ecx, 1                  //Any non-zero digit yet ?
  sbb edi, -1                 //Yes->increment ptr, No->keep old ptr
  shr eax, 23                 //Next digit
  and edx, $007fffff          //Fraction part
  or ecx, eax                 //Accumulate next digit
  or eax, '0'                 //Convert digit to ASCII
  mov [edi], al               //Store digit out to memory
  {Calculate digit #8}
  lea eax, [edx * 4 + edx]    //5*fraction, new digit eax<31:22>
  lea edx, [edx * 4 + edx]    //5*fraction, new fraction edx<22:0>
  cmp ecx, 1                  //Any non-zero digit yet ?
  sbb edi, -1                 //Yes->increment ptr, No->keep old ptr
  shr eax, 22                 //Next digit
  and edx, $003fffff          //Fraction part
  or ecx, eax                 //Accumulate next digit
  or eax, '0'                 //Convert digit to ASCII
  mov [edi], al               //Store digit out to memory
  {Calculate digit #9}
  lea eax, [edx * 4 + edx]    //5*fraction, new digit eax<31:21>
  lea edx, [edx * 4 + edx]    //5*fraction, new fraction edx<21:0>
  cmp ecx, 1                  //Any non-zero digit yet ?
  sbb edi, -1                 //Yes->increment ptr, No->keep old ptr
  shr eax, 21                 //Next digit
  and edx, $001fffff          //Fraction part
  or ecx, eax                 //Accumulate next digit
  or eax, '0'                 //Convert digit to ASCII
  mov [edi], al               //Store digit out to memory
  {Calculate digit #10}
  lea eax, [edx * 4 + edx]    //5*fraction, new digit eax<31:20>
  cmp ecx, 1                  //Any-non-zero digit yet ?
  sbb edi, -1                 //Yes->increment ptr, No->keep old ptr
  shr eax, 20                 //Next digit
  or eax, '0'                 //Convert digit to ASCII
  mov [edi], al               //Store last digit and end marker out to memory
  {Return a pointer to the next character}
  lea eax, [edi + 1]
  {Restore edi}
  pop edi
end;
{$endif}

{Converts an unsigned integer to a hexadecimal string at the buffer location,
 returning the new buffer position.}
function NativeUIntToHexBuf(ANum: NativeUInt; APBuffer: PAnsiChar): PAnsiChar;
{$ifndef Use32BitAsm}
const
  MaxDigits = 16;
var
  LDigitBuffer: array[0..MaxDigits - 1] of AnsiChar;
  LCount: Cardinal;
  LDigit: NativeUInt;
begin
  {Generate the digits in the local buffer}
  LCount := 0;
  repeat
    LDigit := ANum;
    ANum := ANum div 16;
    LDigit := LDigit - ANum * 16;
    Inc(LCount);
    LDigitBuffer[MaxDigits - LCount] := HexTable[LDigit];
  until ANum = 0;
  {Copy the digits to the output buffer and advance it}
  System.Move(LDigitBuffer[MaxDigits - LCount], APBuffer^, LCount);
  Result := APBuffer + LCount;
end;
{$else}
asm
  {On entry:
    eax = ANum
    edx = ABuffer}
  push ebx
  push edi
  {Save ANum in ebx}
  mov ebx, eax
  {Get a pointer to the first character in edi}
  mov edi, edx
  {Get the number in ecx as well}
  mov ecx, eax
  {Keep the low nibbles in ebx and the high nibbles in ecx}
  and ebx, $0f0f0f0f
  and ecx, $f0f0f0f0
  {Swap the bytes into the right order}
  ror ebx, 16
  ror ecx, 20
  {Get nibble 7}
  movzx eax, ch
  mov dl, ch
  mov al, byte ptr HexTable[eax]
  mov [edi], al
  cmp dl, 1
  sbb edi, -1
  {Get nibble 6}
  movzx eax, bh
  or dl, bh
  mov al, byte ptr HexTable[eax]
  mov [edi], al
  cmp dl, 1
  sbb edi, -1
  {Get nibble 5}
  movzx eax, cl
  or dl, cl
  mov al, byte ptr HexTable[eax]
  mov [edi], al
  cmp dl, 1
  sbb edi, -1
  {Get nibble 4}
  movzx eax, bl
  or dl, bl
  mov al, byte ptr HexTable[eax]
  mov [edi], al
  cmp dl, 1
  sbb edi, -1
  {Rotate ecx and ebx so we get access to the rest}
  shr ebx, 16
  shr ecx, 16
  {Get nibble 3}
  movzx eax, ch
  or dl, ch
  mov al, byte ptr HexTable[eax]
  mov [edi], al
  cmp dl, 1
  sbb edi, -1
  {Get nibble 2}
  movzx eax, bh
  or dl, bh
  mov al, byte ptr HexTable[eax]
  mov [edi], al
  cmp dl, 1
  sbb edi, -1
  {Get nibble 1}
  movzx eax, cl
  or dl, cl
  mov al, byte ptr HexTable[eax]
  mov [edi], al
  cmp dl, 1
  sbb edi, -1
  {Get nibble 0}
  movzx eax, bl
  mov al, byte ptr HexTable[eax]
  mov [edi], al
  {Return a pointer to the end of the string}
  lea eax, [edi + 1]
  {Restore registers}
  pop edi
  pop ebx
end;
{$endif}

{Appends the source text to the destination and returns the new destination
 position}
function AppendStringToBuffer(const ASource, ADestination: PAnsiChar; ACount: Cardinal): PAnsiChar;
begin
  System.Move(ASource^, ADestination^, ACount);
  Result := Pointer(PByte(ADestination) + ACount);
end;

{Appends the name of the class to the destination buffer and returns the new
 destination position}
function AppendClassNameToBuffer(AClass: TClass; ADestination: PAnsiChar): PAnsiChar;
var
  LPClassName: PShortString;
begin
  {Get a pointer to the class name}
  if AClass <> nil then
  begin
    LPClassName := PShortString(PPointer(PByte(AClass) + vmtClassName)^);
    {Append the class name}
    Result := AppendStringToBuffer(@LPClassName^[1], ADestination, Length(LPClassName^));
  end
  else
  begin
    Result := AppendStringToBuffer(UnknownClassNameMsg, ADestination, Length(UnknownClassNameMsg));
  end;
end;

{Shows a message box if the program is not showing one already.}
procedure ShowMessageBox(AText, ACaption: PAnsiChar);
begin
  if (not ShowingMessageBox) and (not SuppressMessageBoxes) then
  begin
    ShowingMessageBox := True;
    MessageBoxA(0, AText, ACaption,
      MB_OK or MB_ICONERROR or MB_TASKMODAL or MB_DEFAULT_DESKTOP_ONLY);
    ShowingMessageBox := False;
  end;
end;

{Returns the class for a memory block. Returns nil if it is not a valid class}
function DetectClassInstance(APointer: Pointer): TClass;
{$ifndef POSIX}
var
  LMemInfo: TMemoryBasicInformation;

  {Checks whether the given address is a valid address for a VMT entry.}
  function IsValidVMTAddress(APAddress: Pointer): Boolean;
  begin
    {Do some basic pointer checks: Must be dword aligned and beyond 64K}
    if (UIntPtr(APAddress) > 65535)
      and (UIntPtr(APAddress) and 3 = 0) then
    begin
      {Do we need to recheck the virtual memory?}
      if (UIntPtr(LMemInfo.BaseAddress) > UIntPtr(APAddress))
        or ((UIntPtr(LMemInfo.BaseAddress) + LMemInfo.RegionSize) < (UIntPtr(APAddress) + 4)) then
      begin
        {Get the VM status for the pointer}
        LMemInfo.RegionSize := 0;
        VirtualQuery(APAddress,  LMemInfo, SizeOf(LMemInfo));
      end;
      {Check the readability of the memory address}
      Result := (LMemInfo.RegionSize >= 4)
        and (LMemInfo.State = MEM_COMMIT)
        and (LMemInfo.Protect and (PAGE_READONLY or PAGE_READWRITE or PAGE_EXECUTE or PAGE_EXECUTE_READ or PAGE_EXECUTE_READWRITE or PAGE_EXECUTE_WRITECOPY) <> 0)
        and (LMemInfo.Protect and PAGE_GUARD = 0);
    end
    else
      Result := False;
  end;

  {Returns true if AClassPointer points to a class VMT}
  function InternalIsValidClass(AClassPointer: Pointer; ADepth: Integer = 0): Boolean;
  var
    LParentClassSelfPointer: PPointer;
  begin
    {Check that the self pointer as well as parent class self pointer addresses
     are valid}
    if (ADepth < 1000)
      and IsValidVMTAddress(Pointer(PByte(AClassPointer) + vmtSelfPtr))
      and IsValidVMTAddress(Pointer(PByte(AClassPointer) + vmtParent)) then
    begin
      {Get a pointer to the parent class' self pointer}
      LParentClassSelfPointer := PPointer(PByte(AClassPointer) + vmtParent)^;
      {Check that the self pointer as well as the parent class is valid}
      Result := (PPointer(PByte(AClassPointer) + vmtSelfPtr)^ = AClassPointer)
        and ((LParentClassSelfPointer = nil)
          or (IsValidVMTAddress(LParentClassSelfPointer)
            and InternalIsValidClass(LParentClassSelfPointer^, ADepth + 1)));
    end
    else
      Result := False;
  end;

begin
  {Get the class pointer from the (suspected) object}
  Result := TClass(PPointer(APointer)^);
  {No VM info yet}
  LMemInfo.RegionSize := 0;
  {Check the block}
  if (not InternalIsValidClass(Pointer(Result), 0))
{$ifdef FullDebugMode}
    or (Result = @FreedObjectVMT.VMTMethods[0])
{$endif}
  then
    Result := nil;
end;
{$else}
begin
  {Not currently supported under Linux / OS X}
  Result := nil;
end;
{$endif}

{Gets the available size inside a block}
function GetAvailableSpaceInBlock(APointer: Pointer): NativeUInt;
var
  LBlockHeader: NativeUInt;
  LPSmallBlockPool: PSmallBlockPoolHeader;
begin
  LBlockHeader := PNativeUInt(PByte(APointer) - BlockHeaderSize)^;
  if LBlockHeader and (IsMediumBlockFlag or IsLargeBlockFlag) = 0 then
  begin
    LPSmallBlockPool := PSmallBlockPoolHeader(LBlockHeader and DropSmallFlagsMask);
    Result := LPSmallBlockPool.BlockType.BlockSize - BlockHeaderSize;
  end
  else
  begin
    Result := (LBlockHeader and DropMediumAndLargeFlagsMask) - BlockHeaderSize;
    if (LBlockHeader and IsMediumBlockFlag) = 0 then
      Dec(Result, LargeBlockHeaderSize);
  end;
end;

{-----------------Small Block Management------------------}

{Locks all small block types}
procedure LockAllSmallBlockTypes;
var
  LInd: Cardinal;
begin
  {Lock the medium blocks}
{$ifndef AssumeMultiThreaded}
  if IsMultiThread then
{$endif}
  begin
    for LInd := 0 to NumSmallBlockTypes - 1 do
    begin
      while LockCmpxchg(0, 1, @SmallBlockTypes[LInd].BlockTypeLocked) <> 0 do
      begin
{$ifdef NeverSleepOnThreadContention}
  {$ifdef UseSwitchToThread}
        SwitchToThread;
  {$endif}
{$else}
        Sleep(InitialSleepTime);
        if LockCmpxchg(0, 1, @SmallBlockTypes[LInd].BlockTypeLocked) = 0 then
          Break;
        Sleep(AdditionalSleepTime);
{$endif}
      end;
    end;
  end;
end;

{Gets the first and last block pointer for a small block pool}
procedure GetFirstAndLastSmallBlockInPool(APSmallBlockPool: PSmallBlockPoolHeader;
  var AFirstPtr, ALastPtr: Pointer);
var
  LBlockSize: NativeUInt;
begin
  {Get the pointer to the first block}
  AFirstPtr := Pointer(PByte(APSmallBlockPool) + SmallBlockPoolHeaderSize);
  {Get a pointer to the last block}
  if (APSmallBlockPool.BlockType.CurrentSequentialFeedPool <> APSmallBlockPool)
    or (UIntPtr(APSmallBlockPool.BlockType.NextSequentialFeedBlockAddress) > UIntPtr(APSmallBlockPool.BlockType.MaxSequentialFeedBlockAddress)) then
  begin
    {Not the sequential feed - point to the end of the block}
    LBlockSize := PNativeUInt(PByte(APSmallBlockPool) - BlockHeaderSize)^ and DropMediumAndLargeFlagsMask;
    ALastPtr := Pointer(PByte(APSmallBlockPool) + LBlockSize - APSmallBlockPool.BlockType.BlockSize);
  end
  else
  begin
    {The sequential feed pool - point to before the next sequential feed block}
    ALastPtr := Pointer(PByte(APSmallBlockPool.BlockType.NextSequentialFeedBlockAddress) - 1);
  end;
end;

{-----------------Medium Block Management------------------}

{Advances to the next medium block. Returns nil if the end of the medium block
 pool has been reached}
function NextMediumBlock(APMediumBlock: Pointer): Pointer;
var
  LBlockSize: NativeUInt;
begin
  {Get the size of this block}
  LBlockSize := PNativeUInt(PByte(APMediumBlock) - BlockHeaderSize)^ and DropMediumAndLargeFlagsMask;
  {Advance the pointer}
  Result := Pointer(PByte(APMediumBlock) + LBlockSize);
  {Is the next block the end of medium pool marker?}
  LBlockSize := PNativeUInt(PByte(Result) - BlockHeaderSize)^ and DropMediumAndLargeFlagsMask;
  if LBlockSize = 0 then
    Result := nil;
end;

{Gets the first medium block in the medium block pool}
function GetFirstMediumBlockInPool(APMediumBlockPoolHeader: PMediumBlockPoolHeader): Pointer;
begin
  if (MediumSequentialFeedBytesLeft = 0)
    or (UIntPtr(LastSequentiallyFedMediumBlock) < UIntPtr(APMediumBlockPoolHeader))
    or (UIntPtr(LastSequentiallyFedMediumBlock) > UIntPtr(APMediumBlockPoolHeader) + MediumBlockPoolSize) then
  begin
    Result := Pointer(PByte(APMediumBlockPoolHeader) + MediumBlockPoolHeaderSize);
  end
  else
  begin
    {Is the sequential feed pool empty?}
    if MediumSequentialFeedBytesLeft <> MediumBlockPoolSize - MediumBlockPoolHeaderSize then
      Result := LastSequentiallyFedMediumBlock
    else
      Result := nil;
  end;
end;

{Locks the medium blocks. Note that the 32-bit asm version is assumed to
 preserve all registers except eax.}
{$ifndef Use32BitAsm}
procedure LockMediumBlocks;
begin
  {Lock the medium blocks}
{$ifndef AssumeMultiThreaded}
  if IsMultiThread then
{$endif}
  begin
    while LockCmpxchg(0, 1, @MediumBlocksLocked) <> 0 do
    begin
{$ifdef NeverSleepOnThreadContention}
  {$ifdef UseSwitchToThread}
      SwitchToThread;
  {$endif}
{$else}
      Sleep(InitialSleepTime);
      if LockCmpxchg(0, 1, @MediumBlocksLocked) = 0 then
        Break;
      Sleep(AdditionalSleepTime);
{$endif}
    end;
  end;
end;
{$else}
procedure LockMediumBlocks;
asm
  {Note: This routine is assumed to preserve all registers except eax}
@MediumBlockLockLoop:
  mov eax, $100
  {Attempt to lock the medium blocks}
  lock cmpxchg MediumBlocksLocked, ah
  je @Done
{$ifdef NeverSleepOnThreadContention}
  {Pause instruction (improves performance on P4)}
  rep nop
  {$ifdef UseSwitchToThread}
  push ecx
  push edx
  call SwitchToThread
  pop edx
  pop ecx
  {$endif}
  {Try again}
  jmp @MediumBlockLockLoop
{$else}
  {Couldn't lock the medium blocks - sleep and try again}
  push ecx
  push edx
  push InitialSleepTime
  call Sleep
  pop edx
  pop ecx
  {Try again}
  mov eax, $100
  {Attempt to grab the block type}
  lock cmpxchg MediumBlocksLocked, ah
  je @Done
  {Couldn't lock the medium blocks - sleep and try again}
  push ecx
  push edx
  push AdditionalSleepTime
  call Sleep
  pop edx
  pop ecx
  {Try again}
  jmp @MediumBlockLockLoop
{$endif}
@Done:
end;
{$endif}

{Removes a medium block from the circular linked list of free blocks.
 Does not change any header flags. Medium blocks should be locked
 before calling this procedure.}
procedure RemoveMediumFreeBlock(APMediumFreeBlock: PMediumFreeBlock);
{$ifndef ASMVersion}
var
  LPreviousFreeBlock, LNextFreeBlock: PMediumFreeBlock;
  LBinNumber, LBinGroupNumber: Cardinal;
begin
  {Get the current previous and next blocks}
  LNextFreeBlock := APMediumFreeBlock.NextFreeBlock;
  LPreviousFreeBlock := APMediumFreeBlock.PreviousFreeBlock;
  {Remove this block from the linked list}
  LPreviousFreeBlock.NextFreeBlock := LNextFreeBlock;
  LNextFreeBlock.PreviousFreeBlock := LPreviousFreeBlock;
  {Is this bin now empty? If the previous and next free block pointers are
   equal, they must point to the bin.}
  if LPreviousFreeBlock = LNextFreeBlock then
  begin
    {Get the bin number for this block size}
    LBinNumber := (UIntPtr(LNextFreeBlock) - UIntPtr(@MediumBlockBins)) div SizeOf(TMediumFreeBlock);
    LBinGroupNumber := LBinNumber div 32;
    {Flag this bin as empty}
    MediumBlockBinBitmaps[LBinGroupNumber] := MediumBlockBinBitmaps[LBinGroupNumber]
      and (not (1 shl (LBinNumber and 31)));
    {Is the group now entirely empty?}
    if MediumBlockBinBitmaps[LBinGroupNumber] = 0 then
    begin
      {Flag this group as empty}
      MediumBlockBinGroupBitmap := MediumBlockBinGroupBitmap
        and (not (1 shl LBinGroupNumber));
    end;
  end;
end;
{$else}
{$ifdef 32Bit}
asm
  {On entry: eax = APMediumFreeBlock}
  {Get the current previous and next blocks}
  mov ecx, TMediumFreeBlock[eax].NextFreeBlock
  mov edx, TMediumFreeBlock[eax].PreviousFreeBlock
  {Is this bin now empty? If the previous and next free block pointers are
   equal, they must point to the bin.}
  cmp ecx, edx
  {Remove this block from the linked list}
  mov TMediumFreeBlock[ecx].PreviousFreeBlock, edx
  mov TMediumFreeBlock[edx].NextFreeBlock, ecx
  {Is this bin now empty? If the previous and next free block pointers are
   equal, they must point to the bin.}
  je @BinIsNowEmpty
@Done:
  ret
  {Align branch target}
  nop
@BinIsNowEmpty:
  {Get the bin number for this block size in ecx}
  sub ecx, offset MediumBlockBins
  mov edx, ecx
  shr ecx, 3
  {Get the group number in edx}
  movzx edx, dh
  {Flag this bin as empty}
  mov eax, -2
  rol eax, cl
  and dword ptr [MediumBlockBinBitmaps + edx * 4], eax
  jnz @Done
  {Flag this group as empty}
  mov eax, -2
  mov ecx, edx
  rol eax, cl
  and MediumBlockBinGroupBitmap, eax
end;
{$else}
asm
  {On entry: rcx = APMediumFreeBlock}
  mov rax, rcx
  {Get the current previous and next blocks}
  mov rcx, TMediumFreeBlock[rax].NextFreeBlock
  mov rdx, TMediumFreeBlock[rax].PreviousFreeBlock
  {Is this bin now empty? If the previous and next free block pointers are
   equal, they must point to the bin.}
  cmp rcx, rdx
  {Remove this block from the linked list}
  mov TMediumFreeBlock[rcx].PreviousFreeBlock, rdx
  mov TMediumFreeBlock[rdx].NextFreeBlock, rcx
  {Is this bin now empty? If the previous and next free block pointers are
   equal, they must point to the bin.}
  jne @Done
  {Get the bin number for this block size in rcx}
  lea r8, MediumBlockBins
  sub rcx, r8
  mov edx, ecx
  shr ecx, 4
  {Get the group number in edx}
  shr edx, 9
  {Flag this bin as empty}
  mov eax, -2
  rol eax, cl
  lea r8, MediumBlockBinBitmaps
  and dword ptr [r8 + rdx * 4], eax
  jnz @Done
  {Flag this group as empty}
  mov eax, -2
  mov ecx, edx
  rol eax, cl
  and MediumBlockBinGroupBitmap, eax
@Done:
end;
{$endif}
{$endif}

{Inserts a medium block into the appropriate medium block bin.}
procedure InsertMediumBlockIntoBin(APMediumFreeBlock: PMediumFreeBlock; AMediumBlockSize: Cardinal);
{$ifndef ASMVersion}
var
  LBinNumber, LBinGroupNumber: Cardinal;
  LPBin, LPFirstFreeBlock: PMediumFreeBlock;
begin
  {Get the bin number for this block size. Get the bin that holds blocks of at
   least this size.}
  LBinNumber := (AMediumBlockSize - MinimumMediumBlockSize) div MediumBlockGranularity;
  if LBinNumber >= MediumBlockBinCount then
    LBinNumber := MediumBlockBinCount - 1;
  {Get the bin}
  LPBin := @MediumBlockBins[LBinNumber];
  {Bins are LIFO, se we insert this block as the first free block in the bin}
  LPFirstFreeBlock := LPBin.NextFreeBlock;
  APMediumFreeBlock.PreviousFreeBlock := LPBin;
  APMediumFreeBlock.NextFreeBlock := LPFirstFreeBlock;
  LPFirstFreeBlock.PreviousFreeBlock := APMediumFreeBlock;
  LPBin.NextFreeBlock := APMediumFreeBlock;
  {Was this bin empty?}
  if LPFirstFreeBlock = LPBin then
  begin
    {Get the group number}
    LBinGroupNumber := LBinNumber div 32;
    {Flag this bin as used}
    MediumBlockBinBitmaps[LBinGroupNumber] := MediumBlockBinBitmaps[LBinGroupNumber]
      or (1 shl (LBinNumber and 31));
    {Flag the group as used}
    MediumBlockBinGroupBitmap := MediumBlockBinGroupBitmap
      or (1 shl LBinGroupNumber);
  end;
end;
{$else}
{$ifdef 32Bit}
asm
  {On entry: eax = APMediumFreeBlock, edx = AMediumBlockSize}
  {Get the bin number for this block size. Get the bin that holds blocks of at
   least this size.}
  sub edx, MinimumMediumBlockSize
  shr edx, 8
  {Validate the bin number}
  sub edx, MediumBlockBinCount - 1
  sbb ecx, ecx
  and edx, ecx
  add edx, MediumBlockBinCount - 1
  {Get the bin in ecx}
  lea ecx, [MediumBlockBins + edx * 8]
  {Bins are LIFO, se we insert this block as the first free block in the bin}
  mov edx, TMediumFreeBlock[ecx].NextFreeBlock
  {Was this bin empty?}
  cmp edx, ecx
  mov TMediumFreeBlock[eax].PreviousFreeBlock, ecx
  mov TMediumFreeBlock[eax].NextFreeBlock, edx
  mov TMediumFreeBlock[edx].PreviousFreeBlock, eax
  mov TMediumFreeBlock[ecx].NextFreeBlock, eax
  {Was this bin empty?}
  je @BinWasEmpty
  ret
  {Align branch target}
  nop
  nop
@BinWasEmpty:
  {Get the bin number in ecx}
  sub ecx, offset MediumBlockBins
  mov edx, ecx
  shr ecx, 3
  {Get the group number in edx}
  movzx edx, dh
  {Flag this bin as not empty}
  mov eax, 1
  shl eax, cl
  or dword ptr [MediumBlockBinBitmaps + edx * 4], eax
  {Flag the group as not empty}
  mov eax, 1
  mov ecx, edx
  shl eax, cl
  or MediumBlockBinGroupBitmap, eax
end;
{$else}
asm
  {On entry: rax = APMediumFreeBlock, edx = AMediumBlockSize}
  mov rax, rcx
  {Get the bin number for this block size. Get the bin that holds blocks of at
   least this size.}
  sub edx, MinimumMediumBlockSize
  shr edx, 8
  {Validate the bin number}
  sub edx, MediumBlockBinCount - 1
  sbb ecx, ecx
  and edx, ecx
  add edx, MediumBlockBinCount - 1
  mov r9, rdx
  {Get the bin address in rcx}
  lea rcx, MediumBlockBins
  shl edx, 4
  add rcx, rdx
  {Bins are LIFO, se we insert this block as the first free block in the bin}
  mov rdx, TMediumFreeBlock[rcx].NextFreeBlock
  {Was this bin empty?}
  cmp rdx, rcx
  mov TMediumFreeBlock[rax].PreviousFreeBlock, rcx
  mov TMediumFreeBlock[rax].NextFreeBlock, rdx
  mov TMediumFreeBlock[rdx].PreviousFreeBlock, rax
  mov TMediumFreeBlock[rcx].NextFreeBlock, rax
  {Was this bin empty?}
  jne @Done
  {Get the bin number in ecx}
  mov rcx, r9
  {Get the group number in edx}
  mov rdx, r9
  shr edx, 5
  {Flag this bin as not empty}
  mov eax, 1
  shl eax, cl
  lea r8, MediumBlockBinBitmaps
  or dword ptr [r8 + rdx * 4], eax
  {Flag the group as not empty}
  mov eax, 1
  mov ecx, edx
  shl eax, cl
  or MediumBlockBinGroupBitmap, eax
@Done:
end;
{$endif}
{$endif}

{Bins what remains in the current sequential feed medium block pool. Medium
 blocks must be locked.}
procedure BinMediumSequentialFeedRemainder;
{$ifndef ASMVersion}
var
  LSequentialFeedFreeSize, LNextBlockSizeAndFlags: NativeUInt;
  LPRemainderBlock, LNextMediumBlock: Pointer;
begin
  LSequentialFeedFreeSize := MediumSequentialFeedBytesLeft;
  if LSequentialFeedFreeSize > 0 then
  begin
    {Get the block after the open space}
    LNextMediumBlock := LastSequentiallyFedMediumBlock;
    LNextBlockSizeAndFlags := PNativeUInt(PByte(LNextMediumBlock) - BlockHeaderSize)^;
    {Point to the remainder}
    LPRemainderBlock := Pointer(PByte(LNextMediumBlock) - LSequentialFeedFreeSize);
{$ifndef FullDebugMode}
    {Can the next block be combined with the remainder?}
    if (LNextBlockSizeAndFlags and IsFreeBlockFlag) <> 0 then
    begin
      {Increase the size of this block}
      Inc(LSequentialFeedFreeSize, LNextBlockSizeAndFlags and DropMediumAndLargeFlagsMask);
      {Remove the next block as well}
      if (LNextBlockSizeAndFlags and DropMediumAndLargeFlagsMask) >= MinimumMediumBlockSize then
        RemoveMediumFreeBlock(LNextMediumBlock);
    end
    else
    begin
{$endif}
      {Set the "previous block is free" flag of the next block}
      PNativeUInt(PByte(LNextMediumBlock) - BlockHeaderSize)^ := LNextBlockSizeAndFlags or PreviousMediumBlockIsFreeFlag;
{$ifndef FullDebugMode}
    end;
{$endif}
    {Store the size of the block as well as the flags}
    PNativeUInt(PByte(LPRemainderBlock) - BlockHeaderSize)^ := LSequentialFeedFreeSize or IsMediumBlockFlag or IsFreeBlockFlag;
    {Store the trailing size marker}
    PNativeUInt(PByte(LPRemainderBlock) + LSequentialFeedFreeSize - BlockHeaderSize * 2)^ := LSequentialFeedFreeSize;
{$ifdef FullDebugMode}
    {In full debug mode the sequential feed remainder will never be too small to
     fit a full debug header.}
    {Clear the user area of the block}
    DebugFillMem(Pointer(PByte(LPRemainderBlock) + SizeOf(TFullDebugBlockHeader) + SizeOf(NativeUInt))^,
      LSequentialFeedFreeSize - FullDebugBlockOverhead - SizeOf(NativeUInt),
      {$ifndef CatchUseOfFreedInterfaces}DebugFillPattern{$else}NativeUInt(@VMTBadInterface){$endif});
    {We need to set a valid debug header and footer in the remainder}
    PFullDebugBlockHeader(LPRemainderBlock).HeaderCheckSum := NativeUInt(LPRemainderBlock);
    PNativeUInt(PByte(LPRemainderBlock) + SizeOf(TFullDebugBlockHeader))^ := not NativeUInt(LPRemainderBlock);
{$endif}
    {Bin this medium block}
    if LSequentialFeedFreeSize >= MinimumMediumBlockSize then
      InsertMediumBlockIntoBin(LPRemainderBlock, LSequentialFeedFreeSize);
  end;
end;
{$else}
{$ifdef 32Bit}
asm
  cmp MediumSequentialFeedBytesLeft, 0
  jne @MustBinMedium
  {Nothing to bin}
  ret
  {Align branch target}
  nop
  nop
@MustBinMedium:
  {Get a pointer to the last sequentially allocated medium block}
  mov eax, LastSequentiallyFedMediumBlock
  {Is the block that was last fed sequentially free?}
  test byte ptr [eax - 4], IsFreeBlockFlag
  jnz @LastBlockFedIsFree
  {Set the "previous block is free" flag in the last block fed}
  or dword ptr [eax - 4], PreviousMediumBlockIsFreeFlag
  {Get the remainder in edx}
  mov edx, MediumSequentialFeedBytesLeft
  {Point eax to the start of the remainder}
  sub eax, edx
@BinTheRemainder:
  {Status: eax = start of remainder, edx = size of remainder}
  {Store the size of the block as well as the flags}
  lea ecx, [edx + IsMediumBlockFlag + IsFreeBlockFlag]
  mov [eax - 4], ecx
  {Store the trailing size marker}
  mov [eax + edx - 8], edx
  {Bin this medium block}
  cmp edx, MinimumMediumBlockSize
  jnb InsertMediumBlockIntoBin
  ret
  {Align branch target}
  nop
  nop
@LastBlockFedIsFree:
  {Drop the flags}
  mov edx, DropMediumAndLargeFlagsMask
  and edx, [eax - 4]
  {Free the last block fed}
  cmp edx, MinimumMediumBlockSize
  jb @DontRemoveLastFed
  {Last fed block is free - remove it from its size bin}
  call RemoveMediumFreeBlock
  {Re-read eax and edx}
  mov eax, LastSequentiallyFedMediumBlock
  mov edx, DropMediumAndLargeFlagsMask
  and edx, [eax - 4]
@DontRemoveLastFed:
  {Get the number of bytes left in ecx}
  mov ecx, MediumSequentialFeedBytesLeft
  {Point eax to the start of the remainder}
  sub eax, ecx
  {edx = total size of the remainder}
  add edx, ecx
  jmp @BinTheRemainder
@Done:
end;
{$else}
asm
  .params 2
  xor eax, eax
  cmp MediumSequentialFeedBytesLeft, eax
  je @Done
  {Get a pointer to the last sequentially allocated medium block}
  mov rax, LastSequentiallyFedMediumBlock
  {Is the block that was last fed sequentially free?}
  test byte ptr [rax - BlockHeaderSize], IsFreeBlockFlag
  jnz @LastBlockFedIsFree
  {Set the "previous block is free" flag in the last block fed}
  or qword ptr [rax - BlockHeaderSize], PreviousMediumBlockIsFreeFlag
  {Get the remainder in edx}
  mov edx, MediumSequentialFeedBytesLeft
  {Point eax to the start of the remainder}
  sub rax, rdx
@BinTheRemainder:
  {Status: rax = start of remainder, edx = size of remainder}
  {Store the size of the block as well as the flags}
  lea rcx, [rdx + IsMediumBlockFlag + IsFreeBlockFlag]
  mov [rax - BlockHeaderSize], rcx
  {Store the trailing size marker}
  mov [rax + rdx - 2 * BlockHeaderSize], rdx
  {Bin this medium block}
  cmp edx, MinimumMediumBlockSize
  jb @Done
  mov rcx, rax
  call InsertMediumBlockIntoBin
  jmp @Done
@LastBlockFedIsFree:
  {Drop the flags}
  mov rdx, DropMediumAndLargeFlagsMask
  and rdx, [rax - BlockHeaderSize]
  {Free the last block fed}
  cmp edx, MinimumMediumBlockSize
  jb @DontRemoveLastFed
  {Last fed block is free - remove it from its size bin}
  mov rcx, rax
  call RemoveMediumFreeBlock
  {Re-read rax and rdx}
  mov rax, LastSequentiallyFedMediumBlock
  mov rdx, DropMediumAndLargeFlagsMask
  and rdx, [rax - BlockHeaderSize]
@DontRemoveLastFed:
  {Get the number of bytes left in ecx}
  mov ecx, MediumSequentialFeedBytesLeft
  {Point rax to the start of the remainder}
  sub rax, rcx
  {edx = total size of the remainder}
  add edx, ecx
  jmp @BinTheRemainder
@Done:
end;
{$endif}
{$endif}

{Allocates a new sequential feed medium block pool and immediately splits off a
 block of the requested size. The block size must be a multiple of 16 and
 medium blocks must be locked.}
function AllocNewSequentialFeedMediumPool(AFirstBlockSize: Cardinal): Pointer;
var
  LOldFirstMediumBlockPool: PMediumBlockPoolHeader;
  LNewPool: Pointer;
begin
  {Bin the current sequential feed remainder}
  BinMediumSequentialFeedRemainder;
  {Allocate a new sequential feed block pool}
  LNewPool := VirtualAlloc(nil, MediumBlockPoolSize,
    MEM_COMMIT{$ifdef AlwaysAllocateTopDown} or MEM_TOP_DOWN{$endif}, PAGE_READWRITE);
  if LNewPool <> nil then
  begin
    {Insert this block pool into the list of block pools}
    LOldFirstMediumBlockPool := MediumBlockPoolsCircularList.NextMediumBlockPoolHeader;
    PMediumBlockPoolHeader(LNewPool).PreviousMediumBlockPoolHeader := @MediumBlockPoolsCircularList;
    MediumBlockPoolsCircularList.NextMediumBlockPoolHeader := LNewPool;
    PMediumBlockPoolHeader(LNewPool).NextMediumBlockPoolHeader := LOldFirstMediumBlockPool;
    LOldFirstMediumBlockPool.PreviousMediumBlockPoolHeader := LNewPool;
    {Store the sequential feed pool trailer}
    PNativeUInt(PByte(LNewPool) + MediumBlockPoolSize - BlockHeaderSize)^ := IsMediumBlockFlag;
    {Get the number of bytes still available}
    MediumSequentialFeedBytesLeft := (MediumBlockPoolSize - MediumBlockPoolHeaderSize) - AFirstBlockSize;
    {Get the result}
    Result := Pointer(PByte(LNewPool) + MediumBlockPoolSize - AFirstBlockSize);
    LastSequentiallyFedMediumBlock := Result;
    {Store the block header}
    PNativeUInt(PByte(Result) - BlockHeaderSize)^ := AFirstBlockSize or IsMediumBlockFlag;
  end
  else
  begin
    {Out of memory}
    MediumSequentialFeedBytesLeft := 0;
    Result := nil;
  end;
end;

{-----------------Large Block Management------------------}

{Locks the large blocks}
procedure LockLargeBlocks;
begin
  {Lock the large blocks}
{$ifndef AssumeMultiThreaded}
  if IsMultiThread then
{$endif}
  begin
    while LockCmpxchg(0, 1, @LargeBlocksLocked) <> 0 do
    begin
{$ifdef NeverSleepOnThreadContention}
  {$ifdef UseSwitchToThread}
      SwitchToThread;
  {$endif}
{$else}
      Sleep(InitialSleepTime);
      if LockCmpxchg(0, 1, @LargeBlocksLocked) = 0 then
        Break;
      Sleep(AdditionalSleepTime);
{$endif}
    end;
  end;
end;

{Allocates a Large block of at least ASize (actual size may be larger to
 allow for alignment etc.). ASize must be the actual user requested size. This
 procedure will pad it to the appropriate page boundary and also add the space
 required by the header.}
function AllocateLargeBlock(ASize: NativeUInt): Pointer;
var
  LLargeUsedBlockSize: NativeUInt;
  LOldFirstLargeBlock: PLargeBlockHeader;
begin
  {Pad the block size to include the header and granularity. We also add a
   SizeOf(Pointer) overhead so a huge block size is a multiple of 16 bytes less
   SizeOf(Pointer) (so we can use a single move function for reallocating all
   block types)}
  LLargeUsedBlockSize := (ASize + LargeBlockHeaderSize + LargeBlockGranularity - 1 + BlockHeaderSize)
    and -LargeBlockGranularity;
  {Get the Large block}
  Result := VirtualAlloc(nil, LLargeUsedBlockSize, MEM_COMMIT or MEM_TOP_DOWN,
    PAGE_READWRITE);
  {Set the Large block fields}
  if Result <> nil then
  begin
    {Set the large block size and flags}
    PLargeBlockHeader(Result).UserAllocatedSize := ASize;
    PLargeBlockHeader(Result).BlockSizeAndFlags := LLargeUsedBlockSize or IsLargeBlockFlag;
    {Insert the large block into the linked list of large blocks}
    LockLargeBlocks;
    LOldFirstLargeBlock := LargeBlocksCircularList.NextLargeBlockHeader;
    PLargeBlockHeader(Result).PreviousLargeBlockHeader := @LargeBlocksCircularList;
    LargeBlocksCircularList.NextLargeBlockHeader := Result;
    PLargeBlockHeader(Result).NextLargeBlockHeader := LOldFirstLargeBlock;
    LOldFirstLargeBlock.PreviousLargeBlockHeader := Result;
    LargeBlocksLocked := False;
    {Add the size of the header}
    Inc(PByte(Result), LargeBlockHeaderSize);
{$ifdef FullDebugMode}
    {Since large blocks are never reused, the user area is not initialized to
     the debug fill pattern, but the debug header and footer must be set.}
    PFullDebugBlockHeader(Result).HeaderCheckSum := NativeUInt(Result);
    PNativeUInt(PByte(Result) + SizeOf(TFullDebugBlockHeader))^ := not NativeUInt(Result);
{$endif}
  end;
end;

{Frees a large block, returning 0 on success, -1 otherwise}
function FreeLargeBlock(APointer: Pointer): Integer;
var
  LPreviousLargeBlockHeader, LNextLargeBlockHeader: PLargeBlockHeader;
{$ifndef POSIX}
  LRemainingSize: NativeUInt;
  LCurrentSegment: Pointer;
  LMemInfo: TMemoryBasicInformation;
{$endif}
begin
{$ifdef ClearLargeBlocksBeforeReturningToOS}
  FillChar(APointer^,
    (PLargeBlockHeader(PByte(APointer) - LargeBlockHeaderSize).BlockSizeAndFlags
      and DropMediumAndLargeFlagsMask) - LargeBlockHeaderSize, 0);
{$endif}
  {Point to the start of the large block}
  APointer := Pointer(PByte(APointer) - LargeBlockHeaderSize);
  {Get the previous and next large blocks}
  LockLargeBlocks;
  LPreviousLargeBlockHeader := PLargeBlockHeader(APointer).PreviousLargeBlockHeader;
  LNextLargeBlockHeader := PLargeBlockHeader(APointer).NextLargeBlockHeader;
{$ifndef POSIX}
  {Is the large block segmented?}
  if PLargeBlockHeader(APointer).BlockSizeAndFlags and LargeBlockIsSegmented = 0 then
  begin
{$endif}
    {Single segment large block: Try to free it}
    if VirtualFree(APointer, 0, MEM_RELEASE) then
      Result := 0
    else
      Result := -1;
{$ifndef POSIX}
  end
  else
  begin
    {The large block is segmented - free all segments}
    LCurrentSegment := APointer;
    LRemainingSize := PLargeBlockHeader(APointer).BlockSizeAndFlags and DropMediumAndLargeFlagsMask;
    Result := 0;
    while True do
    begin
      {Get the size of the current segment}
      VirtualQuery(LCurrentSegment, LMemInfo, SizeOf(LMemInfo));
      {Free the segment}
      if not VirtualFree(LCurrentSegment, 0, MEM_RELEASE) then
      begin
        Result := -1;
        Break;
      end;
      {Done?}
      if NativeUInt(LMemInfo.RegionSize) >= LRemainingSize then
        Break;
      {Decrement the remaining size}
      Dec(LRemainingSize, NativeUInt(LMemInfo.RegionSize));
      Inc(PByte(LCurrentSegment), NativeUInt(LMemInfo.RegionSize));
    end;
  end;
{$endif}
  {Success?}
  if Result = 0 then
  begin
    {Remove the large block from the linked list}
    LNextLargeBlockHeader.PreviousLargeBlockHeader := LPreviousLargeBlockHeader;
    LPreviousLargeBlockHeader.NextLargeBlockHeader := LNextLargeBlockHeader;
  end;
  {Unlock the large blocks}
  LargeBlocksLocked := False;
end;

{$ifndef FullDebugMode}
{Reallocates a large block to at least the requested size. Returns the new
 pointer, or nil on error}
function ReallocateLargeBlock(APointer: Pointer; ANewSize: NativeUInt): Pointer;
var
  LOldAvailableSize, LBlockHeader, LOldUserSize, LMinimumUpsize,
    LNewAllocSize: NativeUInt;
{$ifndef POSIX}
  LNewSegmentSize: NativeUInt;
  LNextSegmentPointer: Pointer;
  LMemInfo: TMemoryBasicInformation;
{$endif}
begin
  {Get the block header}
  LBlockHeader := PNativeUInt(PByte(APointer) - BlockHeaderSize)^;
  {Large block - size is (16 + 4) less than the allocated size}
  LOldAvailableSize := (LBlockHeader and DropMediumAndLargeFlagsMask) - (LargeBlockHeaderSize + BlockHeaderSize);
  {Is it an upsize or a downsize?}
  if ANewSize > LOldAvailableSize then
  begin
    {This pointer is being reallocated to a larger block and therefore it is
     logical to assume that it may be enlarged again. Since reallocations are
     expensive, there is a minimum upsize percentage to avoid unnecessary
     future move operations.}
    {Add 25% for large block upsizes}
    LMinimumUpsize := LOldAvailableSize + (LOldAvailableSize shr 2);
    if ANewSize < LMinimumUpsize then
      LNewAllocSize := LMinimumUpsize
    else
      LNewAllocSize := ANewSize;
{$ifndef POSIX}
    {Can another large block segment be allocated directly after this segment,
     thus negating the need to move the data?}
    LNextSegmentPointer := Pointer(PByte(APointer) - LargeBlockHeaderSize + (LBlockHeader and DropMediumAndLargeFlagsMask));
    VirtualQuery(LNextSegmentPointer, LMemInfo, SizeOf(LMemInfo));
    if LMemInfo.State = MEM_FREE then
    begin
      {Round the region size to the previous 64K}
      LMemInfo.RegionSize := LMemInfo.RegionSize and -LargeBlockGranularity;
      {Enough space to grow in place?}
      if NativeUInt(LMemInfo.RegionSize) > (ANewSize - LOldAvailableSize) then
      begin
        {There is enough space after the block to extend it - determine by how
         much}
        LNewSegmentSize := (LNewAllocSize - LOldAvailableSize + LargeBlockGranularity - 1) and -LargeBlockGranularity;
        if LNewSegmentSize > LMemInfo.RegionSize then
          LNewSegmentSize := LMemInfo.RegionSize;
        {Attempy to reserve the address range (which will fail if another
         thread has just reserved it) and commit it immediately afterwards.}
        if (VirtualAlloc(LNextSegmentPointer, LNewSegmentSize, MEM_RESERVE, PAGE_READWRITE) <> nil)
          and (VirtualAlloc(LNextSegmentPointer, LNewSegmentSize, MEM_COMMIT, PAGE_READWRITE) <> nil) then
        begin
          {Update the requested size}
          PLargeBlockHeader(PByte(APointer) - LargeBlockHeaderSize).UserAllocatedSize := ANewSize;
          PLargeBlockHeader(PByte(APointer) - LargeBlockHeaderSize).BlockSizeAndFlags :=
            (PLargeBlockHeader(PByte(APointer) - LargeBlockHeaderSize).BlockSizeAndFlags + LNewSegmentSize)
            or LargeBlockIsSegmented;
          {Success}
          Result := APointer;
          Exit;
        end;
      end;
    end;
{$endif}
    {Could not resize in place: Allocate the new block}
    Result := FastGetMem(LNewAllocSize);
    if Result <> nil then
    begin
      {If it's a large block - store the actual user requested size (it may
       not be if the block that is being reallocated from was previously
       downsized)}
      if LNewAllocSize > (MaximumMediumBlockSize - BlockHeaderSize) then
        PLargeBlockHeader(PByte(Result) - LargeBlockHeaderSize).UserAllocatedSize := ANewSize;
      {The user allocated size is stored for large blocks}
      LOldUserSize := PLargeBlockHeader(PByte(APointer) - LargeBlockHeaderSize).UserAllocatedSize;
      {The number of bytes to move is the old user size.}
{$ifdef UseCustomVariableSizeMoveRoutines}
      MoveX16LP(APointer^, Result^, LOldUserSize);
{$else}
      System.Move(APointer^, Result^, LOldUserSize);
{$endif}
      {Free the old block}
      FastFreeMem(APointer);
    end;
  end
  else
  begin
    {It's a downsize: do we need to reallocate? Only if the new size is less
     than half the old size}
    if ANewSize >= (LOldAvailableSize shr 1) then
    begin
      {No need to reallocate}
      Result := APointer;
      {Update the requested size}
      PLargeBlockHeader(PByte(APointer) - LargeBlockHeaderSize).UserAllocatedSize := ANewSize;
    end
    else
    begin
      {The block is less than half the old size, and the current size is
       greater than the minimum block size allowing a downsize: reallocate}
      Result := FastGetMem(ANewSize);
      if Result <> nil then
      begin
        {Still a large block? -> Set the user size}
        if ANewSize > (MaximumMediumBlockSize - BlockHeaderSize) then
          PLargeBlockHeader(PByte(APointer) - LargeBlockHeaderSize).UserAllocatedSize := ANewSize;
        {Move the data across}
{$ifdef UseCustomVariableSizeMoveRoutines}
{$ifdef Align16Bytes}
        MoveX16LP(APointer^, Result^, ANewSize);
{$else}
        MoveX8LP(APointer^, Result^, ANewSize);
{$endif}
{$else}
        System.Move(APointer^, Result^, ANewSize);
{$endif}
        {Free the old block}
        FastFreeMem(APointer);
      end;
    end;
  end;
end;
{$endif}

{---------------------Replacement Memory Manager Interface---------------------}

{Replacement for SysGetMem}

function FastGetMem(ASize: {$ifdef XE2AndUp}NativeInt{$else}Integer{$endif}): Pointer;
{$ifndef ASMVersion}
var
  LMediumBlock{$ifndef FullDebugMode}, LNextFreeBlock, LSecondSplit{$endif}: PMediumFreeBlock;
  LNextMediumBlockHeader: PNativeUInt;
  LBlockSize, LAvailableBlockSize{$ifndef FullDebugMode}, LSecondSplitSize{$endif},
    LSequentialFeedFreeSize: NativeUInt;
  LPSmallBlockType: PSmallBlockType;
  LPSmallBlockPool, LPNewFirstPool: PSmallBlockPoolHeader;
  LNewFirstFreeBlock: Pointer;
  LPMediumBin: PMediumFreeBlock;
  LBinNumber, {$ifndef FullDebugMode}LBinGroupsMasked, {$endif}LBinGroupMasked,
    LBinGroupNumber: Cardinal;
begin
  {Is it a small block? -> Take the header size into account when
   determining the required block size}
  if NativeUInt(ASize) <= (MaximumSmallBlockSize - BlockHeaderSize) then
  begin
    {-------------------------Allocate a small block---------------------------}
    {Get the block type from the size}
    LPSmallBlockType := PSmallBlockType(AllocSize2SmallBlockTypeIndX4[
      (NativeUInt(ASize) + (BlockHeaderSize - 1)) div SmallBlockGranularity]
      * (SizeOf(TSmallBlockType) div 4)
      + UIntPtr(@SmallBlockTypes));
    {Lock the block type}
{$ifndef AssumeMultiThreaded}
    if IsMultiThread then
{$endif}
    begin
      while True do
      begin
        {Try to lock the small block type}
        if LockCmpxchg(0, 1, @LPSmallBlockType.BlockTypeLocked) = 0 then
          Break;
        {Try the next block type}
        Inc(PByte(LPSmallBlockType), SizeOf(TSmallBlockType));
        if LockCmpxchg(0, 1, @LPSmallBlockType.BlockTypeLocked) = 0 then
          Break;
        {Try up to two sizes past the requested size}
        Inc(PByte(LPSmallBlockType), SizeOf(TSmallBlockType));
        if LockCmpxchg(0, 1, @LPSmallBlockType.BlockTypeLocked) = 0 then
          Break;
        {All three sizes locked - given up and sleep}
        Dec(PByte(LPSmallBlockType), 2 * SizeOf(TSmallBlockType));
{$ifdef NeverSleepOnThreadContention}
  {$ifdef UseSwitchToThread}
        SwitchToThread;
  {$endif}
{$else}
        {Both this block type and the next is in use: sleep}
        Sleep(InitialSleepTime);
        {Try the lock again}
        if LockCmpxchg(0, 1, @LPSmallBlockType.BlockTypeLocked) = 0 then
          Break;
        {Sleep longer}
        Sleep(AdditionalSleepTime);
{$endif}
      end;
    end;
    {Get the first pool with free blocks}
    LPSmallBlockPool := LPSmallBlockType.NextPartiallyFreePool;
    {Is the pool valid?}
    if UIntPtr(LPSmallBlockPool) <> UIntPtr(LPSmallBlockType) then
    begin
      {Get the first free offset}
      Result := LPSmallBlockPool.FirstFreeBlock;
      {Get the new first free block}
      LNewFirstFreeBlock := PPointer(PByte(Result) - BlockHeaderSize)^;
{$ifdef CheckHeapForCorruption}
      {The block should be free}
      if (NativeUInt(LNewFirstFreeBlock) and ExtractSmallFlagsMask) <> IsFreeBlockFlag then
  {$ifdef BCB6OrDelphi7AndUp}
        System.Error(reInvalidPtr);
  {$else}
        System.RunError(reInvalidPtr);
  {$endif}
{$endif}
      LNewFirstFreeBlock := Pointer(UIntPtr(LNewFirstFreeBlock) and DropSmallFlagsMask);
      {Increment the number of used blocks}
      Inc(LPSmallBlockPool.BlocksInUse);
      {Set the new first free block}
      LPSmallBlockPool.FirstFreeBlock := LNewFirstFreeBlock;
      {Is the pool now full?}
      if LNewFirstFreeBlock = nil then
      begin
        {Pool is full - remove it from the partially free list}
        LPNewFirstPool := LPSmallBlockPool.NextPartiallyFreePool;
        LPSmallBlockType.NextPartiallyFreePool := LPNewFirstPool;
        LPNewFirstPool.PreviousPartiallyFreePool := PSmallBlockPoolHeader(LPSmallBlockType);
      end;
    end
    else
    begin
      {Try to feed a small block sequentially}
      Result := LPSmallBlockType.NextSequentialFeedBlockAddress;
      {Can another block fit?}
      if UIntPtr(Result) <= UIntPtr(LPSmallBlockType.MaxSequentialFeedBlockAddress) then
      begin
        {Get the sequential feed block pool}
        LPSmallBlockPool := LPSmallBlockType.CurrentSequentialFeedPool;
        {Increment the number of used blocks in the sequential feed pool}
        Inc(LPSmallBlockPool.BlocksInUse);
        {Store the next sequential feed block address}
        LPSmallBlockType.NextSequentialFeedBlockAddress := Pointer(PByte(Result) + LPSmallBlockType.BlockSize);
      end
      else
      begin
        {Need to allocate a pool: Lock the medium blocks}
        LockMediumBlocks;
{$ifndef FullDebugMode}
        {Are there any available blocks of a suitable size?}
        LBinGroupsMasked := MediumBlockBinGroupBitmap and ($ffffff00 or LPSmallBlockType.AllowedGroupsForBlockPoolBitmap);
        if LBinGroupsMasked <> 0 then
        begin
          {Get the bin group with free blocks}
          LBinGroupNumber := FindFirstSetBit(LBinGroupsMasked);
          {Get the bin in the group with free blocks}
          LBinNumber := FindFirstSetBit(MediumBlockBinBitmaps[LBinGroupNumber])
            + LBinGroupNumber * 32;
          LPMediumBin := @MediumBlockBins[LBinNumber];
          {Get the first block in the bin}
          LMediumBlock := LPMediumBin.NextFreeBlock;
          {Remove the first block from the linked list (LIFO)}
          LNextFreeBlock := LMediumBlock.NextFreeBlock;
          LPMediumBin.NextFreeBlock := LNextFreeBlock;
          LNextFreeBlock.PreviousFreeBlock := LPMediumBin;
          {Is this bin now empty?}
          if LNextFreeBlock = LPMediumBin then
          begin
            {Flag this bin as empty}
            MediumBlockBinBitmaps[LBinGroupNumber] := MediumBlockBinBitmaps[LBinGroupNumber]
              and (not (1 shl (LBinNumber and 31)));
            {Is the group now entirely empty?}
            if MediumBlockBinBitmaps[LBinGroupNumber] = 0 then
            begin
              {Flag this group as empty}
              MediumBlockBinGroupBitmap := MediumBlockBinGroupBitmap
                and (not (1 shl LBinGroupNumber));
            end;
          end;
          {Get the size of the available medium block}
          LBlockSize := PNativeUInt(PByte(LMediumBlock) - BlockHeaderSize)^ and DropMediumAndLargeFlagsMask;
  {$ifdef CheckHeapForCorruption}
          {Check that this block is actually free and the next and previous blocks
           are both in use.}
          if ((PNativeUInt(PByte(LMediumBlock) - BlockHeaderSize)^ and ExtractMediumAndLargeFlagsMask) <> (IsMediumBlockFlag or IsFreeBlockFlag))
            or ((PNativeUInt(PByte(LMediumBlock) + (PNativeUInt(PByte(LMediumBlock) - BlockHeaderSize)^ and DropMediumAndLargeFlagsMask) - BlockHeaderSize)^ and IsFreeBlockFlag) <> 0)
          then
          begin
    {$ifdef BCB6OrDelphi7AndUp}
            System.Error(reInvalidPtr);
    {$else}
            System.RunError(reInvalidPtr);
    {$endif}
          end;
  {$endif}
          {Should the block be split?}
          if LBlockSize >= MaximumSmallBlockPoolSize then
          begin
            {Get the size of the second split}
            LSecondSplitSize := LBlockSize - LPSmallBlockType.OptimalBlockPoolSize;
            {Adjust the block size}
            LBlockSize := LPSmallBlockType.OptimalBlockPoolSize;
            {Split the block in two}
            LSecondSplit := PMediumFreeBlock(PByte(LMediumBlock) + LBlockSize);
            PNativeUInt(PByte(LSecondSplit) - BlockHeaderSize)^ := LSecondSplitSize or (IsMediumBlockFlag or IsFreeBlockFlag);
            {Store the size of the second split as the second last dword/qword}
            PNativeUInt(PByte(LSecondSplit) + LSecondSplitSize - 2 * BlockHeaderSize)^ := LSecondSplitSize;
            {Put the remainder in a bin (it will be big enough)}
            InsertMediumBlockIntoBin(LSecondSplit, LSecondSplitSize);
          end
          else
          begin
            {Mark this block as used in the block following it}
            LNextMediumBlockHeader := PNativeUInt(PByte(LMediumBlock) + LBlockSize - BlockHeaderSize);
            LNextMediumBlockHeader^ := LNextMediumBlockHeader^ and (not PreviousMediumBlockIsFreeFlag);
          end;
        end
        else
        begin
{$endif}
          {Check the sequential feed medium block pool for space}
          LSequentialFeedFreeSize := MediumSequentialFeedBytesLeft;
          if LSequentialFeedFreeSize >= LPSmallBlockType.MinimumBlockPoolSize then
          begin
            {Enough sequential feed space: Will the remainder be usable?}
            if LSequentialFeedFreeSize >= (LPSmallBlockType.OptimalBlockPoolSize + MinimumMediumBlockSize) then
            begin
              LBlockSize := LPSmallBlockType.OptimalBlockPoolSize;
            end
            else
              LBlockSize := LSequentialFeedFreeSize;
            {Get the block}
            LMediumBlock := Pointer(PByte(LastSequentiallyFedMediumBlock) - LBlockSize);
            {Update the sequential feed parameters}
            LastSequentiallyFedMediumBlock := LMediumBlock;
            MediumSequentialFeedBytesLeft := LSequentialFeedFreeSize - LBlockSize;
          end
          else
          begin
            {Need to allocate a new sequential feed medium block pool: use the
             optimal size for this small block pool}
            LBlockSize := LPSmallBlockType.OptimalBlockPoolSize;
            {Allocate the medium block pool}
            LMediumBlock := AllocNewSequentialFeedMediumPool(LBlockSize);
            if LMediumBlock = nil then
            begin
              {Out of memory}
              {Unlock the medium blocks}
              MediumBlocksLocked := False;
              {Unlock the block type}
              LPSmallBlockType.BlockTypeLocked := False;
              {Failed}
              Result := nil;
              {done}
              Exit;
            end;
          end;
{$ifndef FullDebugMode}
        end;
{$endif}
        {Mark this block as in use}
        {Set the size and flags for this block}
        PNativeUInt(PByte(LMediumBlock) - BlockHeaderSize)^ := LBlockSize or IsMediumBlockFlag or IsSmallBlockPoolInUseFlag;
        {Unlock medium blocks}
        MediumBlocksLocked := False;
        {Set up the block pool}
        LPSmallBlockPool := PSmallBlockPoolHeader(LMediumBlock);
        LPSmallBlockPool.BlockType := LPSmallBlockType;
        LPSmallBlockPool.FirstFreeBlock := nil;
        LPSmallBlockPool.BlocksInUse := 1;
        {Set it up for sequential block serving}
        LPSmallBlockType.CurrentSequentialFeedPool := LPSmallBlockPool;
        Result := Pointer(PByte(LPSmallBlockPool) + SmallBlockPoolHeaderSize);
        LPSmallBlockType.NextSequentialFeedBlockAddress := Pointer(PByte(Result) + LPSmallBlockType.BlockSize);
        LPSmallBlockType.MaxSequentialFeedBlockAddress := Pointer(PByte(LPSmallBlockPool) + LBlockSize - LPSmallBlockType.BlockSize);
      end;
{$ifdef FullDebugMode}
      {Clear the user area of the block}
      DebugFillMem(Pointer(PByte(Result) + (SizeOf(TFullDebugBlockHeader) + SizeOf(NativeUInt)))^,
        LPSmallBlockType.BlockSize - FullDebugBlockOverhead - SizeOf(NativeUInt),
        {$ifndef CatchUseOfFreedInterfaces}DebugFillPattern{$else}NativeUInt(@VMTBadInterface){$endif});
      {Block was fed sequentially - we need to set a valid debug header. Use
       the block address.}
      PFullDebugBlockHeader(Result).HeaderCheckSum := NativeUInt(Result);
      PNativeUInt(PByte(Result) + SizeOf(TFullDebugBlockHeader))^ := not NativeUInt(Result);
{$endif}
    end;
    {Unlock the block type}
    LPSmallBlockType.BlockTypeLocked := False;
    {Set the block header}
    PNativeUInt(PByte(Result) - BlockHeaderSize)^ := UIntPtr(LPSmallBlockPool);
  end
  else
  begin
    {Medium block or Large block?}
    if NativeUInt(ASize) <= (MaximumMediumBlockSize - BlockHeaderSize) then
    begin
      {------------------------Allocate a medium block--------------------------}
      {Get the block size and bin number for this block size. Block sizes are
       rounded up to the next bin size.}
      LBlockSize := ((NativeUInt(ASize) + (MediumBlockGranularity - 1 + BlockHeaderSize - MediumBlockSizeOffset))
        and -MediumBlockGranularity) + MediumBlockSizeOffset;
      {Get the bin number}
      LBinNumber := (LBlockSize - MinimumMediumBlockSize) div MediumBlockGranularity;
      {Lock the medium blocks}
      LockMediumBlocks;
      {Calculate the bin group}
      LBinGroupNumber := LBinNumber div 32;
      {Is there a suitable block inside this group?}
      LBinGroupMasked := MediumBlockBinBitmaps[LBinGroupNumber] and -(1 shl (LBinNumber and 31));
      if LBinGroupMasked <> 0 then
      begin
        {Get the actual bin number}
        LBinNumber := FindFirstSetBit(LBinGroupMasked) + LBinGroupNumber * 32;
      end
      else
      begin
{$ifndef FullDebugMode}
        {Try all groups greater than this group}
        LBinGroupsMasked := MediumBlockBinGroupBitmap and -(2 shl LBinGroupNumber);
        if LBinGroupsMasked <> 0 then
        begin
          {There is a suitable group with space: get the bin number}
          LBinGroupNumber := FindFirstSetBit(LBinGroupsMasked);
          {Get the bin in the group with free blocks}
          LBinNumber := FindFirstSetBit(MediumBlockBinBitmaps[LBinGroupNumber])
            + LBinGroupNumber * 32;
        end
        else
        begin
{$endif}
          {There are no bins with a suitable block: Sequentially feed the required block}
          LSequentialFeedFreeSize := MediumSequentialFeedBytesLeft;
          if LSequentialFeedFreeSize >= LBlockSize then
          begin
{$ifdef FullDebugMode}
            {In full debug mode a medium block must have enough bytes to fit
             all the debug info, so we must make sure there are no tiny medium
             blocks at the start of the pool.}
            if LSequentialFeedFreeSize - LBlockSize < (FullDebugBlockOverhead + BlockHeaderSize) then
              LBlockSize := LSequentialFeedFreeSize;
{$endif}
            {Block can be fed sequentially}
            Result := Pointer(PByte(LastSequentiallyFedMediumBlock) - LBlockSize);
            {Store the last sequentially fed block}
            LastSequentiallyFedMediumBlock := Result;
            {Store the remaining bytes}
            MediumSequentialFeedBytesLeft := LSequentialFeedFreeSize - LBlockSize;
            {Set the flags for the block}
            PNativeUInt(PByte(Result) - BlockHeaderSize)^ := LBlockSize or IsMediumBlockFlag;
          end
          else
          begin
            {Need to allocate a new sequential feed block}
            Result := AllocNewSequentialFeedMediumPool(LBlockSize);
          end;
{$ifdef FullDebugMode}
          {Block was fed sequentially - we need to set a valid debug header}
          if Result <> nil then
          begin
            PFullDebugBlockHeader(Result).HeaderCheckSum := NativeUInt(Result);
            PNativeUInt(PByte(Result) + SizeOf(TFullDebugBlockHeader))^ := not NativeUInt(Result);
            {Clear the user area of the block}
            DebugFillMem(Pointer(PByte(Result) + SizeOf(TFullDebugBlockHeader) + SizeOf(NativeUInt))^,
              LBlockSize - FullDebugBlockOverhead - SizeOf(NativeUInt),
              {$ifndef CatchUseOfFreedInterfaces}DebugFillPattern{$else}NativeUInt(@VMTBadInterface){$endif});
          end;
{$endif}
          {Done}
          MediumBlocksLocked := False;
          Exit;
{$ifndef FullDebugMode}
        end;
{$endif}
      end;
      {If we get here we have a valid LBinGroupNumber and LBinNumber:
       Use the first block in the bin, splitting it if necessary}
      {Get a pointer to the bin}
      LPMediumBin := @MediumBlockBins[LBinNumber];
      {Get the result}
      Result := LPMediumBin.NextFreeBlock;
{$ifdef CheckHeapForCorruption}
      {Check that this block is actually free and the next and previous blocks
       are both in use (except in full debug mode).}
      if ((PNativeUInt(PByte(Result) - BlockHeaderSize)^ and {$ifndef FullDebugMode}ExtractMediumAndLargeFlagsMask{$else}(IsMediumBlockFlag or IsFreeBlockFlag){$endif}) <> (IsFreeBlockFlag or IsMediumBlockFlag))
  {$ifndef FullDebugMode}
        or ((PNativeUInt(PByte(Result) + (PNativeUInt(PByte(Result) - BlockHeaderSize)^ and DropMediumAndLargeFlagsMask) - BlockHeaderSize)^ and (ExtractMediumAndLargeFlagsMask - IsSmallBlockPoolInUseFlag)) <> (IsMediumBlockFlag or PreviousMediumBlockIsFreeFlag))
  {$endif}
      then
      begin
  {$ifdef BCB6OrDelphi7AndUp}
        System.Error(reInvalidPtr);
  {$else}
        System.RunError(reInvalidPtr);
  {$endif}
      end;
{$endif}
      {Remove the block from the bin containing it}
      RemoveMediumFreeBlock(Result);
      {Get the block size}
      LAvailableBlockSize := PNativeUInt(PByte(Result) - BlockHeaderSize)^ and DropMediumAndLargeFlagsMask;
{$ifndef FullDebugMode}
      {Is it an exact fit or not?}
      LSecondSplitSize := LAvailableBlockSize - LBlockSize;
      if LSecondSplitSize <> 0 then
      begin
        {Split the block in two}
        LSecondSplit := PMediumFreeBlock(PByte(Result) + LBlockSize);
        {Set the size of the second split}
        PNativeUInt(PByte(LSecondSplit) - BlockHeaderSize)^ := LSecondSplitSize or (IsMediumBlockFlag or IsFreeBlockFlag);
        {Store the size of the second split}
        PNativeUInt(PByte(LSecondSplit) + LSecondSplitSize - 2 * BlockHeaderSize)^ := LSecondSplitSize;
        {Put the remainder in a bin if it is big enough}
        if LSecondSplitSize >= MinimumMediumBlockSize then
          InsertMediumBlockIntoBin(LSecondSplit, LSecondSplitSize);
      end
      else
      begin
{$else}
        {In full debug mode blocks are never split or coalesced}
        LBlockSize := LAvailableBlockSize;
{$endif}
        {Mark this block as used in the block following it}
        LNextMediumBlockHeader := Pointer(PByte(Result) + LBlockSize - BlockHeaderSize);
{$ifndef FullDebugMode}
  {$ifdef CheckHeapForCorruption}
        {The next block must be in use}
        if (LNextMediumBlockHeader^ and (ExtractMediumAndLargeFlagsMask - IsSmallBlockPoolInUseFlag)) <> (IsMediumBlockFlag or PreviousMediumBlockIsFreeFlag) then
    {$ifdef BCB6OrDelphi7AndUp}
        System.Error(reInvalidPtr);
    {$else}
        System.RunError(reInvalidPtr);
    {$endif}
  {$endif}
{$endif}
        LNextMediumBlockHeader^ :=
          LNextMediumBlockHeader^ and (not PreviousMediumBlockIsFreeFlag);
{$ifndef FullDebugMode}
      end;
      {Set the size and flags for this block}
      PNativeUInt(PByte(Result) - BlockHeaderSize)^ := LBlockSize or IsMediumBlockFlag;
{$else}
      {In full debug mode blocks are never split or coalesced}
      Dec(PNativeUInt(PByte(Result) - BlockHeaderSize)^, IsFreeBlockFlag);
{$endif}
      {Unlock the medium blocks}
      MediumBlocksLocked := False;
    end
    else
    begin
      {Allocate a Large block}
      if ASize > 0 then
        Result := AllocateLargeBlock(ASize)
      else
        Result := nil;
    end;
  end;
end;
{$else}
{$ifdef 32Bit}
asm
  {On entry:
    eax = ASize}
  {Since most allocations are for small blocks, determine the small block type
   index so long}
  lea edx, [eax + BlockHeaderSize - 1]
{$ifdef Align16Bytes}
  shr edx, 4
{$else}
  shr edx, 3
{$endif}
  {Is it a small block?}
  cmp eax, (MaximumSmallBlockSize - BlockHeaderSize)
  {Save ebx}
  push ebx
  {Get the IsMultiThread variable so long}
{$ifndef AssumeMultiThreaded}
  mov cl, IsMultiThread
{$endif}
  {Is it a small block?}
  ja @NotASmallBlock
  {Do we need to lock the block type?}
{$ifndef AssumeMultiThreaded}
  test cl, cl
{$endif}
  {Get the small block type in ebx}
  movzx eax, byte ptr [AllocSize2SmallBlockTypeIndX4 + edx]
  lea ebx, [SmallBlockTypes + eax * 8]
  {Do we need to lock the block type?}
{$ifndef AssumeMultiThreaded}
  jnz @LockBlockTypeLoop
{$else}
  jmp @LockBlockTypeLoop
  {Align branch target}
  nop
  nop
{$endif}
@GotLockOnSmallBlockType:
  {Find the next free block: Get the first pool with free blocks in edx}
  mov edx, TSmallBlockType[ebx].NextPartiallyFreePool
  {Get the first free block (or the next sequential feed address if edx = ebx)}
  mov eax, TSmallBlockPoolHeader[edx].FirstFreeBlock
  {Get the drop flags mask in ecx so long}
  mov ecx, DropSmallFlagsMask
  {Is there a pool with free blocks?}
  cmp edx, ebx
  je @TrySmallSequentialFeed
  {Increment the number of used blocks}
  add TSmallBlockPoolHeader[edx].BlocksInUse, 1
  {Get the new first free block}
  and ecx, [eax - 4]
  {Set the new first free block}
  mov TSmallBlockPoolHeader[edx].FirstFreeBlock, ecx
  {Set the block header}
  mov [eax - 4], edx
  {Is the chunk now full?}
  jz @RemoveSmallPool
  {Unlock the block type}
  mov TSmallBlockType[ebx].BlockTypeLocked, False
  {Restore ebx}
  pop ebx
  {All done}
  ret
  {Align branch target}
{$ifndef AssumeMultiThreaded}
  nop
  nop
{$endif}
  nop
@TrySmallSequentialFeed:
  {Try to feed a small block sequentially: Get the sequential feed block pool}
  mov edx, TSmallBlockType[ebx].CurrentSequentialFeedPool
  {Get the next sequential feed address so long}
  movzx ecx, TSmallBlockType[ebx].BlockSize
  add ecx, eax
  {Can another block fit?}
  cmp eax, TSmallBlockType[ebx].MaxSequentialFeedBlockAddress
  ja @AllocateSmallBlockPool
  {Increment the number of used blocks in the sequential feed pool}
  add TSmallBlockPoolHeader[edx].BlocksInUse, 1
  {Store the next sequential feed block address}
  mov TSmallBlockType[ebx].NextSequentialFeedBlockAddress, ecx
  {Unlock the block type}
  mov TSmallBlockType[ebx].BlockTypeLocked, False
  {Set the block header}
  mov [eax - 4], edx
  {Restore ebx}
  pop ebx
  {All done}
  ret
  {Align branch target}
  nop
  nop
  nop
@RemoveSmallPool:
  {Pool is full - remove it from the partially free list}
  mov ecx, TSmallBlockPoolHeader[edx].NextPartiallyFreePool
  mov TSmallBlockPoolHeader[ecx].PreviousPartiallyFreePool, ebx
  mov TSmallBlockType[ebx].NextPartiallyFreePool, ecx
  {Unlock the block type}
  mov TSmallBlockType[ebx].BlockTypeLocked, False
  {Restore ebx}
  pop ebx
  {All done}
  ret
  {Align branch target}
  nop
  nop
@LockBlockTypeLoop:
  mov eax, $100
  {Attempt to grab the block type}
  lock cmpxchg TSmallBlockType([ebx]).BlockTypeLocked, ah
  je @GotLockOnSmallBlockType
  {Try the next size}
  add ebx, Type(TSmallBlockType)
  mov eax, $100
  lock cmpxchg TSmallBlockType([ebx]).BlockTypeLocked, ah
  je @GotLockOnSmallBlockType
  {Try the next size (up to two sizes larger)}
  add ebx, Type(TSmallBlockType)
  mov eax, $100
  lock cmpxchg TSmallBlockType([ebx]).BlockTypeLocked, ah
  je @GotLockOnSmallBlockType
  {Block type and two sizes larger are all locked - give up and sleep}
  sub ebx, 2 * Type(TSmallBlockType)
{$ifdef NeverSleepOnThreadContention}
  {Pause instruction (improves performance on P4)}
  rep nop
  {$ifdef UseSwitchToThread}
  call SwitchToThread
  {$endif}
  {Try again}
  jmp @LockBlockTypeLoop
  {Align branch target}
  nop
  {$ifndef UseSwitchToThread}
  nop
  {$endif}
{$else}
  {Couldn't grab the block type - sleep and try again}
  push InitialSleepTime
  call Sleep
  {Try again}
  mov eax, $100
  {Attempt to grab the block type}
  lock cmpxchg TSmallBlockType([ebx]).BlockTypeLocked, ah
  je @GotLockOnSmallBlockType
  {Couldn't grab the block type - sleep and try again}
  push AdditionalSleepTime
  call Sleep
  {Try again}
  jmp @LockBlockTypeLoop
  {Align branch target}
  nop
  nop
  nop
{$endif}
@AllocateSmallBlockPool:
  {save additional registers}
  push esi
  push edi
  {Do we need to lock the medium blocks?}
{$ifndef AssumeMultiThreaded}
  cmp IsMultiThread, False
  je @MediumBlocksLockedForPool
{$endif}
  call LockMediumBlocks
@MediumBlocksLockedForPool:
  {Are there any available blocks of a suitable size?}
  movsx esi, TSmallBlockType[ebx].AllowedGroupsForBlockPoolBitmap
  and esi, MediumBlockBinGroupBitmap
  jz @NoSuitableMediumBlocks
  {Get the bin group number with free blocks in eax}
  bsf eax, esi
  {Get the bin number in ecx}
  lea esi, [eax * 8]
  mov ecx, dword ptr [MediumBlockBinBitmaps + eax * 4]
  bsf ecx, ecx
  lea ecx, [ecx + esi * 4]
  {Get a pointer to the bin in edi}
  lea edi, [MediumBlockBins + ecx * 8]
  {Get the free block in esi}
  mov esi, TMediumFreeBlock[edi].NextFreeBlock
  {Remove the first block from the linked list (LIFO)}
  mov edx, TMediumFreeBlock[esi].NextFreeBlock
  mov TMediumFreeBlock[edi].NextFreeBlock, edx
  mov TMediumFreeBlock[edx].PreviousFreeBlock, edi
  {Is this bin now empty?}
  cmp edi, edx
  jne @MediumBinNotEmpty
  {eax = bin group number, ecx = bin number, edi = @bin, esi = free block, ebx = block type}
  {Flag this bin as empty}
  mov edx, -2
  rol edx, cl
  and dword ptr [MediumBlockBinBitmaps + eax * 4], edx
  jnz @MediumBinNotEmpty
  {Flag the group as empty}
  btr MediumBlockBinGroupBitmap, eax
@MediumBinNotEmpty:
  {esi = free block, ebx = block type}
  {Get the size of the available medium block in edi}
  mov edi, DropMediumAndLargeFlagsMask
  and edi, [esi - 4]
  cmp edi, MaximumSmallBlockPoolSize
  jb @UseWholeBlock
  {Split the block: get the size of the second part, new block size is the
   optimal size}
  mov edx, edi
  movzx edi, TSmallBlockType[ebx].OptimalBlockPoolSize
  sub edx, edi
  {Split the block in two}
  lea eax, [esi + edi]
  lea ecx, [edx + IsMediumBlockFlag + IsFreeBlockFlag]
  mov [eax - 4], ecx
  {Store the size of the second split as the second last dword}
  mov [eax + edx - 8], edx
  {Put the remainder in a bin (it will be big enough)}
  call InsertMediumBlockIntoBin
  jmp @GotMediumBlock
  {Align branch target}
{$ifdef AssumeMultiThreaded}
  nop
{$endif}
@NoSuitableMediumBlocks:
  {Check the sequential feed medium block pool for space}
  movzx ecx, TSmallBlockType[ebx].MinimumBlockPoolSize
  mov edi, MediumSequentialFeedBytesLeft
  cmp edi, ecx
  jb @AllocateNewSequentialFeed
  {Get the address of the last block that was fed}
  mov esi, LastSequentiallyFedMediumBlock
  {Enough sequential feed space: Will the remainder be usable?}
  movzx ecx, TSmallBlockType[ebx].OptimalBlockPoolSize
  lea edx, [ecx + MinimumMediumBlockSize]
  cmp edi, edx
  jb @NotMuchSpace
  mov edi, ecx
@NotMuchSpace:
  sub esi, edi
  {Update the sequential feed parameters}
  sub MediumSequentialFeedBytesLeft, edi
  mov LastSequentiallyFedMediumBlock, esi
  {Get the block pointer}
  jmp @GotMediumBlock
  {Align branch target}
@AllocateNewSequentialFeed:
  {Need to allocate a new sequential feed medium block pool: use the
   optimal size for this small block pool}
  movzx eax, TSmallBlockType[ebx].OptimalBlockPoolSize
  mov edi, eax
  {Allocate the medium block pool}
  call AllocNewSequentialFeedMediumPool
  mov esi, eax
  test eax, eax
  jnz @GotMediumBlock
  mov MediumBlocksLocked, al
  mov TSmallBlockType[ebx].BlockTypeLocked, al
  pop edi
  pop esi
  pop ebx
  ret
  {Align branch target}
@UseWholeBlock:
  {esi = free block, ebx = block type, edi = block size}
  {Mark this block as used in the block following it}
  and byte ptr [esi + edi - 4], not PreviousMediumBlockIsFreeFlag
@GotMediumBlock:
  {esi = free block, ebx = block type, edi = block size}
  {Set the size and flags for this block}
  lea ecx, [edi + IsMediumBlockFlag + IsSmallBlockPoolInUseFlag]
  mov [esi - 4], ecx
  {Unlock medium blocks}
  xor eax, eax
  mov MediumBlocksLocked, al
  {Set up the block pool}
  mov TSmallBlockPoolHeader[esi].BlockType, ebx
  mov TSmallBlockPoolHeader[esi].FirstFreeBlock, eax
  mov TSmallBlockPoolHeader[esi].BlocksInUse, 1
  {Set it up for sequential block serving}
  mov TSmallBlockType[ebx].CurrentSequentialFeedPool, esi
  {Return the pointer to the first block}
  lea eax, [esi + SmallBlockPoolHeaderSize]
  movzx ecx, TSmallBlockType[ebx].BlockSize
  lea edx, [eax + ecx]
  mov TSmallBlockType[ebx].NextSequentialFeedBlockAddress, edx
  add edi, esi
  sub edi, ecx
  mov TSmallBlockType[ebx].MaxSequentialFeedBlockAddress, edi
  {Unlock the small block type}
  mov TSmallBlockType[ebx].BlockTypeLocked, False
  {Set the small block header}
  mov [eax - 4], esi
  {Restore registers}
  pop edi
  pop esi
  pop ebx
  {Done}
  ret
{-------------------Medium block allocation-------------------}
  {Align branch target}
  nop
@NotASmallBlock:
  cmp eax, (MaximumMediumBlockSize - BlockHeaderSize)
  ja @IsALargeBlockRequest
  {Get the bin size for this block size. Block sizes are
   rounded up to the next bin size.}
  lea ebx, [eax + MediumBlockGranularity - 1 + BlockHeaderSize - MediumBlockSizeOffset]
  and ebx, -MediumBlockGranularity
  add ebx, MediumBlockSizeOffset
  {Do we need to lock the medium blocks?}
{$ifndef AssumeMultiThreaded}
  test cl, cl
  jz @MediumBlocksLocked
{$endif}
  call LockMediumBlocks
@MediumBlocksLocked:
  {Get the bin number in ecx and the group number in edx}
  lea edx, [ebx - MinimumMediumBlockSize]
  mov ecx, edx
  shr edx, 8 + 5
  shr ecx, 8
  {Is there a suitable block inside this group?}
  mov eax, -1
  shl eax, cl
  and eax, dword ptr [MediumBlockBinBitmaps + edx * 4]
  jz @GroupIsEmpty
  {Get the actual bin number}
  and ecx, -32
  bsf eax, eax
  or ecx, eax
  jmp @GotBinAndGroup
  {Align branch target}
  nop
@GroupIsEmpty:
  {Try all groups greater than this group}
  mov eax, -2
  mov ecx, edx
  shl eax, cl
  and eax, MediumBlockBinGroupBitmap
  jz @TrySequentialFeedMedium
  {There is a suitable group with space: get the bin number}
  bsf edx, eax
  {Get the bin in the group with free blocks}
  mov eax, dword ptr [MediumBlockBinBitmaps + edx * 4]
  bsf ecx, eax
  mov eax, edx
  shl eax, 5
  or ecx, eax
  jmp @GotBinAndGroup
  {Align branch target}
  nop
@TrySequentialFeedMedium:
  mov ecx, MediumSequentialFeedBytesLeft
  {Block can be fed sequentially?}
  sub ecx, ebx
  jc @AllocateNewSequentialFeedForMedium
  {Get the block address}
  mov eax, LastSequentiallyFedMediumBlock
  sub eax, ebx
  mov LastSequentiallyFedMediumBlock, eax
  {Store the remaining bytes}
  mov MediumSequentialFeedBytesLeft, ecx
  {Set the flags for the block}
  or ebx, IsMediumBlockFlag
  mov [eax - 4], ebx
  jmp @MediumBlockGetDone
  {Align branch target}
@AllocateNewSequentialFeedForMedium:
  mov eax, ebx
  call AllocNewSequentialFeedMediumPool
@MediumBlockGetDone:
  mov MediumBlocksLocked, False
  pop ebx
  ret
  {Align branch target}
@GotBinAndGroup:
  {ebx = block size, ecx = bin number, edx = group number}
  push esi
  push edi
  {Get a pointer to the bin in edi}
  lea edi, [MediumBlockBins + ecx * 8]
  {Get the free block in esi}
  mov esi, TMediumFreeBlock[edi].NextFreeBlock
  {Remove the first block from the linked list (LIFO)}
  mov eax, TMediumFreeBlock[esi].NextFreeBlock
  mov TMediumFreeBlock[edi].NextFreeBlock, eax
  mov TMediumFreeBlock[eax].PreviousFreeBlock, edi
  {Is this bin now empty?}
  cmp edi, eax
  jne @MediumBinNotEmptyForMedium
  {eax = bin group number, ecx = bin number, edi = @bin, esi = free block, ebx = block size}
  {Flag this bin as empty}
  mov eax, -2
  rol eax, cl
  and dword ptr [MediumBlockBinBitmaps + edx * 4], eax
  jnz @MediumBinNotEmptyForMedium
  {Flag the group as empty}
  btr MediumBlockBinGroupBitmap, edx
@MediumBinNotEmptyForMedium:
  {esi = free block, ebx = block size}
  {Get the size of the available medium block in edi}
  mov edi, DropMediumAndLargeFlagsMask
  and edi, [esi - 4]
  {Get the size of the second split in edx}
  mov edx, edi
  sub edx, ebx
  jz @UseWholeBlockForMedium
  {Split the block in two}
  lea eax, [esi + ebx]
  lea ecx, [edx + IsMediumBlockFlag + IsFreeBlockFlag]
  mov [eax - 4], ecx
  {Store the size of the second split as the second last dword}
  mov [eax + edx - 8], edx
  {Put the remainder in a bin}
  cmp edx, MinimumMediumBlockSize
  jb @GotMediumBlockForMedium
  call InsertMediumBlockIntoBin
  jmp @GotMediumBlockForMedium
  {Align branch target}
  nop
  nop
  nop
@UseWholeBlockForMedium:
  {Mark this block as used in the block following it}
  and byte ptr [esi + edi - 4], not PreviousMediumBlockIsFreeFlag
@GotMediumBlockForMedium:
  {Set the size and flags for this block}
  lea ecx, [ebx + IsMediumBlockFlag]
  mov [esi - 4], ecx
  {Unlock medium blocks}
  mov MediumBlocksLocked, False
  mov eax, esi
  pop edi
  pop esi
  pop ebx
  ret
{-------------------Large block allocation-------------------}
  {Align branch target}
@IsALargeBlockRequest:
  pop ebx
  test eax, eax
  jns AllocateLargeBlock
  xor eax, eax
end;
{$else}
{64-bit BASM implementation}
asm
  {On entry:
    rcx = ASize}
  .params 2
  .pushnv rbx
  .pushnv rsi
  .pushnv rdi
  {Since most allocations are for small blocks, determine the small block type
   index so long}
  lea edx, [ecx + BlockHeaderSize - 1]
{$ifdef Align16Bytes}
  shr edx, 4
{$else}
  shr edx, 3
{$endif}
  {Preload the addresses of some small block structures}
  lea r8, AllocSize2SmallBlockTypeIndX4
  lea rbx, SmallBlockTypes
{$ifndef AssumeMultiThreaded}
  {Get the IsMultiThread variable so long}
  movzx esi, IsMultiThread
{$endif}
  {Is it a small block?}
  cmp rcx, (MaximumSmallBlockSize - BlockHeaderSize)
  ja @NotASmallBlock
  {Get the small block type pointer in rbx}
  movzx ecx, byte ptr [r8 + rdx]
  shl ecx, 4 //SizeOf(TSmallBlockType) = 64
  add rbx, rcx
  {Do we need to lock the block type?}
{$ifndef AssumeMultiThreaded}
  test esi, esi
  jnz @LockBlockTypeLoop
{$else}
  jmp @LockBlockTypeLoop
{$endif}
@GotLockOnSmallBlockType:
  {Find the next free block: Get the first pool with free blocks in rdx}
  mov rdx, TSmallBlockType[rbx].NextPartiallyFreePool
  {Get the first free block (or the next sequential feed address if rdx = rbx)}
  mov rax, TSmallBlockPoolHeader[rdx].FirstFreeBlock
  {Get the drop flags mask in rcx so long}
  mov rcx, DropSmallFlagsMask
  {Is there a pool with free blocks?}
  cmp rdx, rbx
  je @TrySmallSequentialFeed
  {Increment the number of used blocks}
  add TSmallBlockPoolHeader[rdx].BlocksInUse, 1
  {Get the new first free block}
  and rcx, [rax - BlockHeaderSize]
  {Set the new first free block}
  mov TSmallBlockPoolHeader[rdx].FirstFreeBlock, rcx
  {Set the block header}
  mov [rax - BlockHeaderSize], rdx
  {Is the chunk now full?}
  jz @RemoveSmallPool
  {Unlock the block type}
  mov TSmallBlockType[rbx].BlockTypeLocked, False
  jmp @Done
@TrySmallSequentialFeed:
  {Try to feed a small block sequentially: Get the sequential feed block pool}
  mov rdx, TSmallBlockType[rbx].CurrentSequentialFeedPool
  {Get the next sequential feed address so long}
  movzx ecx, TSmallBlockType[rbx].BlockSize
  add rcx, rax
  {Can another block fit?}
  cmp rax, TSmallBlockType[rbx].MaxSequentialFeedBlockAddress
  ja @AllocateSmallBlockPool
  {Increment the number of used blocks in the sequential feed pool}
  add TSmallBlockPoolHeader[rdx].BlocksInUse, 1
  {Store the next sequential feed block address}
  mov TSmallBlockType[rbx].NextSequentialFeedBlockAddress, rcx
  {Unlock the block type}
  mov TSmallBlockType[rbx].BlockTypeLocked, False
  {Set the block header}
  mov [rax - BlockHeaderSize], rdx
  jmp @Done
@RemoveSmallPool:
  {Pool is full - remove it from the partially free list}
  mov rcx, TSmallBlockPoolHeader[rdx].NextPartiallyFreePool
  mov TSmallBlockPoolHeader[rcx].PreviousPartiallyFreePool, rbx
  mov TSmallBlockType[rbx].NextPartiallyFreePool, rcx
  {Unlock the block type}
  mov TSmallBlockType[rbx].BlockTypeLocked, False
  jmp @Done
@LockBlockTypeLoop:
  mov eax, $100
  {Attempt to grab the block type}
  lock cmpxchg TSmallBlockType([rbx]).BlockTypeLocked, ah
  je @GotLockOnSmallBlockType
  {Try the next size}
  add rbx, Type(TSmallBlockType)
  mov eax, $100
  lock cmpxchg TSmallBlockType([rbx]).BlockTypeLocked, ah
  je @GotLockOnSmallBlockType
  {Try the next size (up to two sizes larger)}
  add rbx, Type(TSmallBlockType)
  mov eax, $100
  lock cmpxchg TSmallBlockType([rbx]).BlockTypeLocked, ah
  je @GotLockOnSmallBlockType
  {Block type and two sizes larger are all locked - give up and sleep}
  sub rbx, 2 * Type(TSmallBlockType)
{$ifdef NeverSleepOnThreadContention}
  {Pause instruction (improves performance on P4)}
  pause
  {$ifdef UseSwitchToThread}
  call SwitchToThread
  {$endif}
  {Try again}
  jmp @LockBlockTypeLoop
{$else}
  {Couldn't grab the block type - sleep and try again}
  mov ecx, InitialSleepTime
  call Sleep
  {Try again}
  mov eax, $100
  {Attempt to grab the block type}
  lock cmpxchg TSmallBlockType([rbx]).BlockTypeLocked, ah
  je @GotLockOnSmallBlockType
  {Couldn't grab the block type - sleep and try again}
  mov ecx, AdditionalSleepTime
  call Sleep
  {Try again}
  jmp @LockBlockTypeLoop
{$endif}
@AllocateSmallBlockPool:
  {Do we need to lock the medium blocks?}
{$ifndef AssumeMultiThreaded}
  test esi, esi
  jz @MediumBlocksLockedForPool
{$endif}
  call LockMediumBlocks
@MediumBlocksLockedForPool:
  {Are there any available blocks of a suitable size?}
  movsx esi, TSmallBlockType[rbx].AllowedGroupsForBlockPoolBitmap
  and esi, MediumBlockBinGroupBitmap
  jz @NoSuitableMediumBlocks
  {Get the bin group number with free blocks in eax}
  bsf eax, esi
  {Get the bin number in ecx}
  lea r8, MediumBlockBinBitmaps
  lea r9, [rax * 4]
  mov ecx, [r8 + r9]
  bsf ecx, ecx
  lea ecx, [ecx + r9d * 8]
  {Get a pointer to the bin in edi}
  lea rdi, MediumBlockBins
  lea esi, [ecx * 8]
  lea rdi, [rdi + rsi * 2] //SizeOf(TMediumBlockBin) = 16
  {Get the free block in rsi}
  mov rsi, TMediumFreeBlock[rdi].NextFreeBlock
  {Remove the first block from the linked list (LIFO)}
  mov rdx, TMediumFreeBlock[rsi].NextFreeBlock
  mov TMediumFreeBlock[rdi].NextFreeBlock, rdx
  mov TMediumFreeBlock[rdx].PreviousFreeBlock, rdi
  {Is this bin now empty?}
  cmp rdi, rdx
  jne @MediumBinNotEmpty
  {r8 = @MediumBlockBinBitmaps, eax = bin group number,
   r9 = bin group number * 4, ecx = bin number, edi = @bin, esi = free block,
   ebx = block type}
  {Flag this bin as empty}
  mov edx, -2
  rol edx, cl
  and [r8 + r9], edx
  jnz @MediumBinNotEmpty
  {Flag the group as empty}
  btr MediumBlockBinGroupBitmap, eax
@MediumBinNotEmpty:
  {esi = free block, ebx = block type}
  {Get the size of the available medium block in edi}
  mov rdi, DropMediumAndLargeFlagsMask
  and rdi, [rsi - BlockHeaderSize]
  cmp edi, MaximumSmallBlockPoolSize
  jb @UseWholeBlock
  {Split the block: get the size of the second part, new block size is the
   optimal size}
  mov edx, edi
  movzx edi, TSmallBlockType[rbx].OptimalBlockPoolSize
  sub edx, edi
  {Split the block in two}
  lea rcx, [rsi + rdi]
  lea rax, [rdx + IsMediumBlockFlag + IsFreeBlockFlag]
  mov [rcx - BlockHeaderSize], rax
  {Store the size of the second split as the second last qword}
  mov [rcx + rdx - BlockHeaderSize * 2], rdx
  {Put the remainder in a bin (it will be big enough)}
  call InsertMediumBlockIntoBin
  jmp @GotMediumBlock
@NoSuitableMediumBlocks:
  {Check the sequential feed medium block pool for space}
  movzx ecx, TSmallBlockType[rbx].MinimumBlockPoolSize
  mov edi, MediumSequentialFeedBytesLeft
  cmp edi, ecx
  jb @AllocateNewSequentialFeed
  {Get the address of the last block that was fed}
  mov rsi, LastSequentiallyFedMediumBlock
  {Enough sequential feed space: Will the remainder be usable?}
  movzx ecx, TSmallBlockType[rbx].OptimalBlockPoolSize
  lea edx, [ecx + MinimumMediumBlockSize]
  cmp edi, edx
  jb @NotMuchSpace
  mov edi, ecx
@NotMuchSpace:
  sub rsi, rdi
  {Update the sequential feed parameters}
  sub MediumSequentialFeedBytesLeft, edi
  mov LastSequentiallyFedMediumBlock, rsi
  {Get the block pointer}
  jmp @GotMediumBlock
  {Align branch target}
@AllocateNewSequentialFeed:
  {Need to allocate a new sequential feed medium block pool: use the
   optimal size for this small block pool}
  movzx ecx, TSmallBlockType[rbx].OptimalBlockPoolSize
  mov edi, ecx
  {Allocate the medium block pool}
  call AllocNewSequentialFeedMediumPool
  mov rsi, rax
  test rax, rax
  jnz @GotMediumBlock
  mov MediumBlocksLocked, al
  mov TSmallBlockType[rbx].BlockTypeLocked, al
  jmp @Done
@UseWholeBlock:
  {rsi = free block, rbx = block type, edi = block size}
  {Mark this block as used in the block following it}
  and byte ptr [rsi + rdi - BlockHeaderSize], not PreviousMediumBlockIsFreeFlag
@GotMediumBlock:
  {rsi = free block, rbx = block type, edi = block size}
  {Set the size and flags for this block}
  lea ecx, [edi + IsMediumBlockFlag + IsSmallBlockPoolInUseFlag]
  mov [rsi - BlockHeaderSize], rcx
  {Unlock medium blocks}
  xor eax, eax
  mov MediumBlocksLocked, al
  {Set up the block pool}
  mov TSmallBlockPoolHeader[rsi].BlockType, rbx
  mov TSmallBlockPoolHeader[rsi].FirstFreeBlock, rax
  mov TSmallBlockPoolHeader[rsi].BlocksInUse, 1
  {Set it up for sequential block serving}
  mov TSmallBlockType[rbx].CurrentSequentialFeedPool, rsi
  {Return the pointer to the first block}
  lea rax, [rsi + SmallBlockPoolHeaderSize]
  movzx ecx, TSmallBlockType[rbx].BlockSize
  lea rdx, [rax + rcx]
  mov TSmallBlockType[rbx].NextSequentialFeedBlockAddress, rdx
  add rdi, rsi
  sub rdi, rcx
  mov TSmallBlockType[rbx].MaxSequentialFeedBlockAddress, rdi
  {Unlock the small block type}
  mov TSmallBlockType[rbx].BlockTypeLocked, False
  {Set the small block header}
  mov [rax - BlockHeaderSize], rsi
  jmp @Done
{-------------------Medium block allocation-------------------}
@NotASmallBlock:
  cmp rcx, (MaximumMediumBlockSize - BlockHeaderSize)
  ja @IsALargeBlockRequest
  {Get the bin size for this block size. Block sizes are
   rounded up to the next bin size.}
  lea ebx, [ecx + MediumBlockGranularity - 1 + BlockHeaderSize - MediumBlockSizeOffset]
  and ebx, -MediumBlockGranularity
  add ebx, MediumBlockSizeOffset
  {Do we need to lock the medium blocks?}
{$ifndef AssumeMultiThreaded}
  test esi, esi
  jz @MediumBlocksLocked
{$endif}
  call LockMediumBlocks
@MediumBlocksLocked:
  {Get the bin number in ecx and the group number in edx}
  lea edx, [ebx - MinimumMediumBlockSize]
  mov ecx, edx
  shr edx, 8 + 5
  shr ecx, 8
  {Is there a suitable block inside this group?}
  mov eax, -1
  shl eax, cl
  lea r8, MediumBlockBinBitmaps
  and eax, [r8 + rdx * 4]
  jz @GroupIsEmpty
  {Get the actual bin number}
  and ecx, -32
  bsf eax, eax
  or ecx, eax
  jmp @GotBinAndGroup
@GroupIsEmpty:
  {Try all groups greater than this group}
  mov eax, -2
  mov ecx, edx
  shl eax, cl
  and eax, MediumBlockBinGroupBitmap
  jz @TrySequentialFeedMedium
  {There is a suitable group with space: get the bin number}
  bsf edx, eax
  {Get the bin in the group with free blocks}
  mov eax, [r8 + rdx * 4]
  bsf ecx, eax
  mov eax, edx
  shl eax, 5
  or ecx, eax
  jmp @GotBinAndGroup
@TrySequentialFeedMedium:
  mov ecx, MediumSequentialFeedBytesLeft
  {Block can be fed sequentially?}
  sub ecx, ebx
  jc @AllocateNewSequentialFeedForMedium
  {Get the block address}
  mov rax, LastSequentiallyFedMediumBlock
  sub rax, rbx
  mov LastSequentiallyFedMediumBlock, rax
  {Store the remaining bytes}
  mov MediumSequentialFeedBytesLeft, ecx
  {Set the flags for the block}
  or rbx, IsMediumBlockFlag
  mov [rax - BlockHeaderSize], rbx
  jmp @MediumBlockGetDone
@AllocateNewSequentialFeedForMedium:
  mov ecx, ebx
  call AllocNewSequentialFeedMediumPool
@MediumBlockGetDone:
  xor cl, cl
  mov MediumBlocksLocked, cl //workaround for QC99023
  jmp @Done
@GotBinAndGroup:
  {ebx = block size, ecx = bin number, edx = group number}
  {Get a pointer to the bin in edi}
  lea rdi, MediumBlockBins
  lea eax, [ecx + ecx]
  lea rdi, [rdi + rax * 8]
  {Get the free block in esi}
  mov rsi, TMediumFreeBlock[rdi].NextFreeBlock
  {Remove the first block from the linked list (LIFO)}
  mov rax, TMediumFreeBlock[rsi].NextFreeBlock
  mov TMediumFreeBlock[rdi].NextFreeBlock, rax
  mov TMediumFreeBlock[rax].PreviousFreeBlock, rdi
  {Is this bin now empty?}
  cmp rdi, rax
  jne @MediumBinNotEmptyForMedium
  {edx = bin group number, ecx = bin number, rdi = @bin, rsi = free block, ebx = block size}
  {Flag this bin as empty}
  mov eax, -2
  rol eax, cl
  lea r8, MediumBlockBinBitmaps
  and [r8 + rdx * 4], eax
  jnz @MediumBinNotEmptyForMedium
  {Flag the group as empty}
  btr MediumBlockBinGroupBitmap, edx
@MediumBinNotEmptyForMedium:
  {rsi = free block, ebx = block size}
  {Get the size of the available medium block in edi}
  mov rdi, DropMediumAndLargeFlagsMask
  and rdi, [rsi - BlockHeaderSize]
  {Get the size of the second split in edx}
  mov edx, edi
  sub edx, ebx
  jz @UseWholeBlockForMedium
  {Split the block in two}
  lea rcx, [rsi + rbx]
  lea rax, [rdx + IsMediumBlockFlag + IsFreeBlockFlag]
  mov [rcx - BlockHeaderSize], rax
  {Store the size of the second split as the second last dword}
  mov [rcx + rdx - BlockHeaderSize * 2], rdx
  {Put the remainder in a bin}
  cmp edx, MinimumMediumBlockSize
  jb @GotMediumBlockForMedium
  call InsertMediumBlockIntoBin
  jmp @GotMediumBlockForMedium
@UseWholeBlockForMedium:
  {Mark this block as used in the block following it}
  and byte ptr [rsi + rdi - BlockHeaderSize], not PreviousMediumBlockIsFreeFlag
@GotMediumBlockForMedium:
  {Set the size and flags for this block}
  lea rcx, [rbx + IsMediumBlockFlag]
  mov [rsi - BlockHeaderSize], rcx
  {Unlock medium blocks}
  xor cl, cl
  mov MediumBlocksLocked, cl //workaround for QC99023
  mov rax, rsi
  jmp @Done
{-------------------Large block allocation-------------------}
@IsALargeBlockRequest:
  xor rax, rax
  test rcx, rcx
  js @Done
  call AllocateLargeBlock
@Done:
end;
{$endif}
{$endif}

{$ifndef ASMVersion}
{Frees a medium block, returning 0 on success, -1 otherwise}
function FreeMediumBlock(APointer: Pointer): Integer;
var
  LNextMediumBlock{$ifndef FullDebugMode}, LPreviousMediumBlock{$endif}: PMediumFreeBlock;
  LNextMediumBlockSizeAndFlags: NativeUInt;
  LBlockSize{$ifndef FullDebugMode}, LPreviousMediumBlockSize{$endif}: Cardinal;
{$ifndef FullDebugMode}
  LPPreviousMediumBlockPoolHeader, LPNextMediumBlockPoolHeader: PMediumBlockPoolHeader;
{$endif}
  LBlockHeader: NativeUInt;
begin
  {Get the block header}
  LBlockHeader := PNativeUInt(PByte(APointer) - BlockHeaderSize)^;
  {Get the medium block size}
  LBlockSize := LBlockHeader and DropMediumAndLargeFlagsMask;
  {Lock the medium blocks}
  LockMediumBlocks;
  {Can we combine this block with the next free block?}
  LNextMediumBlock := PMediumFreeBlock(PByte(APointer) + LBlockSize);
  LNextMediumBlockSizeAndFlags := PNativeUInt(PByte(LNextMediumBlock) - BlockHeaderSize)^;
{$ifndef FullDebugMode}
{$ifdef CheckHeapForCorruption}
  {Check that this block was flagged as in use in the next block}
  if (LNextMediumBlockSizeAndFlags and PreviousMediumBlockIsFreeFlag) <> 0 then
{$ifdef BCB6OrDelphi7AndUp}
    System.Error(reInvalidPtr);
{$else}
    System.RunError(reInvalidPtr);
{$endif}
{$endif}
  if (LNextMediumBlockSizeAndFlags and IsFreeBlockFlag) <> 0 then
  begin
    {Increase the size of this block}
    Inc(LBlockSize, LNextMediumBlockSizeAndFlags and DropMediumAndLargeFlagsMask);
    {Remove the next block as well}
    if LNextMediumBlockSizeAndFlags >= MinimumMediumBlockSize then
      RemoveMediumFreeBlock(LNextMediumBlock);
  end
  else
  begin
{$endif}
    {Reset the "previous in use" flag of the next block}
    PNativeUInt(PByte(LNextMediumBlock) - BlockHeaderSize)^ := LNextMediumBlockSizeAndFlags or PreviousMediumBlockIsFreeFlag;
{$ifndef FullDebugMode}
  end;
  {Can we combine this block with the previous free block? We need to
   re-read the flags since it could have changed before we could lock the
   medium blocks.}
  if (PNativeUInt(PByte(APointer) - BlockHeaderSize)^ and PreviousMediumBlockIsFreeFlag) <> 0 then
  begin
    {Get the size of the free block just before this one}
    LPreviousMediumBlockSize := PNativeUInt(PByte(APointer) - 2 * BlockHeaderSize)^;
    {Get the start of the previous block}
    LPreviousMediumBlock := PMediumFreeBlock(PByte(APointer) - LPreviousMediumBlockSize);
{$ifdef CheckHeapForCorruption}
    {Check that the previous block is actually free}
    if (PNativeUInt(PByte(LPreviousMediumBlock) - BlockHeaderSize)^ and ExtractMediumAndLargeFlagsMask) <> (IsMediumBlockFlag or IsFreeBlockFlag) then
{$ifdef BCB6OrDelphi7AndUp}
    System.Error(reInvalidPtr);
{$else}
    System.RunError(reInvalidPtr);
{$endif}
{$endif}
    {Set the new block size}
    Inc(LBlockSize, LPreviousMediumBlockSize);
    {This is the new current block}
    APointer := LPreviousMediumBlock;
    {Remove the previous block from the linked list}
    if LPreviousMediumBlockSize >= MinimumMediumBlockSize then
      RemoveMediumFreeBlock(LPreviousMediumBlock);
  end;
{$ifdef CheckHeapForCorruption}
  {Check that the previous block is currently flagged as in use}
  if (PNativeUInt(PByte(APointer) - BlockHeaderSize)^ and PreviousMediumBlockIsFreeFlag) <> 0 then
{$ifdef BCB6OrDelphi7AndUp}
    System.Error(reInvalidPtr);
{$else}
    System.RunError(reInvalidPtr);
{$endif}
{$endif}
  {Is the entire medium block pool free, and there are other free blocks
   that can fit the largest possible medium block? -> free it. (Except in
   full debug mode where medium pools are never freed.)}
  if (LBlockSize <> (MediumBlockPoolSize - MediumBlockPoolHeaderSize)) then
  begin
    {Store the size of the block as well as the flags}
    PNativeUInt(PByte(APointer) - BlockHeaderSize)^ := LBlockSize or (IsMediumBlockFlag or IsFreeBlockFlag);
{$else}
    {Mark the block as free}
    Inc(PNativeUInt(PByte(APointer) - BlockHeaderSize)^, IsFreeBlockFlag);
{$endif}
    {Store the trailing size marker}
    PNativeUInt(PByte(APointer) + LBlockSize - 2 * BlockHeaderSize)^ := LBlockSize;
    {Insert this block back into the bins: Size check not required here,
     since medium blocks that are in use are not allowed to be
     shrunk smaller than MinimumMediumBlockSize}
    InsertMediumBlockIntoBin(APointer, LBlockSize);
{$ifndef FullDebugMode}
{$ifdef CheckHeapForCorruption}
    {Check that this block is actually free and the next and previous blocks are both in use.}
    if ((PNativeUInt(PByte(APointer) - BlockHeaderSize)^ and ExtractMediumAndLargeFlagsMask) <> (IsMediumBlockFlag or IsFreeBlockFlag))
      or ((PNativeUInt(PByte(APointer) + (PNativeUInt(PByte(APointer) - BlockHeaderSize)^ and DropMediumAndLargeFlagsMask) - BlockHeaderSize)^ and IsFreeBlockFlag) <> 0) then
    begin
{$ifdef BCB6OrDelphi7AndUp}
    System.Error(reInvalidPtr);
{$else}
    System.RunError(reInvalidPtr);
{$endif}
    end;
{$endif}
{$endif}
    {Unlock medium blocks}
    MediumBlocksLocked := False;
    {All OK}
    Result := 0;
{$ifndef FullDebugMode}
  end
  else
  begin
    {Should this become the new sequential feed?}
    if MediumSequentialFeedBytesLeft <> MediumBlockPoolSize - MediumBlockPoolHeaderSize then
    begin
      {Bin the current sequential feed}
      BinMediumSequentialFeedRemainder;
      {Set this medium pool up as the new sequential feed pool:
       Store the sequential feed pool trailer}
      PNativeUInt(PByte(APointer) + LBlockSize - BlockHeaderSize)^ := IsMediumBlockFlag;
      {Store the number of bytes available in the sequential feed chunk}
      MediumSequentialFeedBytesLeft := MediumBlockPoolSize - MediumBlockPoolHeaderSize;
      {Set the last sequentially fed block}
      LastSequentiallyFedMediumBlock := Pointer(PByte(APointer) + LBlockSize);
      {Unlock medium blocks}
      MediumBlocksLocked := False;
      {Success}
      Result := 0;
    end
    else
    begin
      {Remove this medium block pool from the linked list}
      Dec(PByte(APointer), MediumBlockPoolHeaderSize);
      LPPreviousMediumBlockPoolHeader := PMediumBlockPoolHeader(APointer).PreviousMediumBlockPoolHeader;
      LPNextMediumBlockPoolHeader := PMediumBlockPoolHeader(APointer).NextMediumBlockPoolHeader;
      LPPreviousMediumBlockPoolHeader.NextMediumBlockPoolHeader := LPNextMediumBlockPoolHeader;
      LPNextMediumBlockPoolHeader.PreviousMediumBlockPoolHeader := LPPreviousMediumBlockPoolHeader;
      {Unlock medium blocks}
      MediumBlocksLocked := False;
{$ifdef ClearMediumBlockPoolsBeforeReturningToOS}
      FillChar(APointer^, MediumBlockPoolSize, 0);
{$endif}
      {Free the medium block pool}
      if VirtualFree(APointer, 0, MEM_RELEASE) then
        Result := 0
      else
        Result := -1;
    end;
  end;
{$endif}
end;
{$endif}

{Replacement for SysFreeMem}
function FastFreeMem(APointer: Pointer): Integer;
{$ifndef ASMVersion}
var
  LPSmallBlockPool{$ifndef FullDebugMode}, LPPreviousPool, LPNextPool{$endif},
    LPOldFirstPool: PSmallBlockPoolHeader;
  LPSmallBlockType: PSmallBlockType;
  LOldFirstFreeBlock: Pointer;
  LBlockHeader: NativeUInt;
begin
  {Get the small block header: Is it actually a small block?}
  LBlockHeader := PNativeUInt(PByte(APointer) - BlockHeaderSize)^;
  {Is it a small block that is in use?}
  if LBlockHeader and (IsFreeBlockFlag or IsMediumBlockFlag or IsLargeBlockFlag) = 0 then
  begin
    {Get a pointer to the block pool}
    LPSmallBlockPool := PSmallBlockPoolHeader(LBlockHeader);
    {Get the block type}
    LPSmallBlockType := LPSmallBlockPool.BlockType;
{$ifdef ClearSmallAndMediumBlocksInFreeMem}
    FillChar(APointer^, LPSmallBlockType.BlockSize - BlockHeaderSize, 0);
{$endif}
    {Lock the block type}
{$ifndef AssumeMultiThreaded}
    if IsMultiThread then
{$endif}
    begin
      while (LockCmpxchg(0, 1, @LPSmallBlockType.BlockTypeLocked) <> 0) do
      begin
{$ifdef NeverSleepOnThreadContention}
  {$ifdef UseSwitchToThread}
        SwitchToThread;
  {$endif}
{$else}
        Sleep(InitialSleepTime);
        if LockCmpxchg(0, 1, @LPSmallBlockType.BlockTypeLocked) = 0 then
          Break;
        Sleep(AdditionalSleepTime);
{$endif}
      end;
    end;
    {Get the old first free block}
    LOldFirstFreeBlock := LPSmallBlockPool.FirstFreeBlock;
    {Was the pool manager previously full?}
    if LOldFirstFreeBlock = nil then
    begin
      {Insert this as the first partially free pool for the block size}
      LPOldFirstPool := LPSmallBlockType.NextPartiallyFreePool;
      LPSmallBlockPool.NextPartiallyFreePool := LPOldFirstPool;
      LPOldFirstPool.PreviousPartiallyFreePool := LPSmallBlockPool;
      LPSmallBlockPool.PreviousPartiallyFreePool := PSmallBlockPoolHeader(LPSmallBlockType);
      LPSmallBlockType.NextPartiallyFreePool := LPSmallBlockPool;
    end;
    {Store the old first free block}
    PNativeUInt(PByte(APointer) - BlockHeaderSize)^ := UIntPtr(LOldFirstFreeBlock) or IsFreeBlockFlag;
    {Store this as the new first free block}
    LPSmallBlockPool.FirstFreeBlock := APointer;
    {Decrement the number of allocated blocks}
    Dec(LPSmallBlockPool.BlocksInUse);
    {Small block pools are never freed in full debug mode. This increases the
     likehood of success in catching objects still being used after being
     destroyed.}
{$ifndef FullDebugMode}
    {Is the entire pool now free? -> Free it.}
    if LPSmallBlockPool.BlocksInUse = 0 then
    begin
      {Get the previous and next chunk managers}
      LPPreviousPool := LPSmallBlockPool.PreviousPartiallyFreePool;
      LPNextPool := LPSmallBlockPool.NextPartiallyFreePool;
      {Remove this manager}
      LPPreviousPool.NextPartiallyFreePool := LPNextPool;
      LPNextPool.PreviousPartiallyFreePool := LPPreviousPool;
      {Is this the sequential feed pool? If so, stop sequential feeding}
      if (LPSmallBlockType.CurrentSequentialFeedPool = LPSmallBlockPool) then
        LPSmallBlockType.MaxSequentialFeedBlockAddress := nil;
      {Unlock this block type}
      LPSmallBlockType.BlockTypeLocked := False;
      {Free the block pool}
      FreeMediumBlock(LPSmallBlockPool);
    end
    else
    begin
{$endif}
      {Unlock this block type}
      LPSmallBlockType.BlockTypeLocked := False;
{$ifndef FullDebugMode}
    end;
{$endif}
    {No error}
    Result := 0;
  end
  else
  begin
    {Is this a medium block or a large block?}
    if LBlockHeader and (IsFreeBlockFlag or IsLargeBlockFlag) = 0 then
    begin
{$ifdef ClearSmallAndMediumBlocksInFreeMem}
      {Get the block header, extract the block size and clear the block it.}
      LBlockHeader := PNativeUInt(PByte(APointer) - BlockHeaderSize)^;
      FillChar(APointer^,
        (LBlockHeader and DropMediumAndLargeFlagsMask) - BlockHeaderSize, 0);
{$endif}
      Result := FreeMediumBlock(APointer);
    end
    else
    begin
      {Validate: Is this actually a Large block, or is it an attempt to free an
       already freed small block?}
      if LBlockHeader and (IsFreeBlockFlag or IsMediumBlockFlag) = 0 then
        Result := FreeLargeBlock(APointer)
      else
        Result := -1;
    end;
  end;
end;
{$else}
{$ifdef 32Bit}
asm
  {Get the block header in edx}
  mov edx, [eax - 4]
  {Is it a small block in use?}
  test dl, IsFreeBlockFlag + IsMediumBlockFlag + IsLargeBlockFlag
  {Save the pointer in ecx}
  mov ecx, eax
  {Save ebx}
  push ebx
  {Get the IsMultiThread variable in bl}
{$ifndef AssumeMultiThreaded}
  mov bl, IsMultiThread
{$endif}
  {Is it a small block that is in use?}
  jnz @NotSmallBlockInUse
{$ifdef ClearSmallAndMediumBlocksInFreeMem}
  push edx
  push ecx
  mov edx, TSmallBlockPoolHeader[edx].BlockType
  movzx edx, TSmallBlockType(edx).BlockSize
  sub edx, BlockHeaderSize
  xor ecx, ecx
  call System.@FillChar
  pop ecx
  pop edx
{$endif}
  {Do we need to lock the block type?}
{$ifndef AssumeMultiThreaded}
  test bl, bl
{$endif}
  {Get the small block type in ebx}
  mov ebx, TSmallBlockPoolHeader[edx].BlockType
  {Do we need to lock the block type?}
{$ifndef AssumeMultiThreaded}
  jnz @LockBlockTypeLoop
{$else}
  jmp @LockBlockTypeLoop
  {Align branch target}
  nop
{$endif}
@GotLockOnSmallBlockType:
  {Current state: edx = @SmallBlockPoolHeader, ecx = APointer, ebx = @SmallBlockType}
  {Decrement the number of blocks in use}
  sub TSmallBlockPoolHeader[edx].BlocksInUse, 1
  {Get the old first free block}
  mov eax, TSmallBlockPoolHeader[edx].FirstFreeBlock
  {Is the pool now empty?}
  jz @PoolIsNowEmpty
  {Was the pool full?}
  test eax, eax
  {Store this as the new first free block}
  mov TSmallBlockPoolHeader[edx].FirstFreeBlock, ecx
  {Store the previous first free block as the block header}
  lea eax, [eax + IsFreeBlockFlag]
  mov [ecx - 4], eax
  {Insert the pool back into the linked list if it was full}
  jz @SmallPoolWasFull
  {All ok}
  xor eax, eax
  {Unlock the block type}
  mov TSmallBlockType[ebx].BlockTypeLocked, al
  {Restore registers}
  pop ebx
  {Done}
  ret
  {Align branch target}
{$ifndef AssumeMultiThreaded}
  nop
{$endif}
@SmallPoolWasFull:
  {Insert this as the first partially free pool for the block size}
  mov ecx, TSmallBlockType[ebx].NextPartiallyFreePool
  mov TSmallBlockPoolHeader[edx].PreviousPartiallyFreePool, ebx
  mov TSmallBlockPoolHeader[edx].NextPartiallyFreePool, ecx
  mov TSmallBlockPoolHeader[ecx].PreviousPartiallyFreePool, edx
  mov TSmallBlockType[ebx].NextPartiallyFreePool, edx
  {Unlock the block type}
  mov TSmallBlockType[ebx].BlockTypeLocked, False
  {All ok}
  xor eax, eax
  {Restore registers}
  pop ebx
  {Done}
  ret
  {Align branch target}
  nop
  nop
@PoolIsNowEmpty:
  {Was this pool actually in the linked list of pools with space? If not, it
   can only be the sequential feed pool (it is the only pool that may contain
   only one block, i.e. other blocks have not been split off yet)}
  test eax, eax
  jz @IsSequentialFeedPool
  {Pool is now empty: Remove it from the linked list and free it}
  mov eax, TSmallBlockPoolHeader[edx].PreviousPartiallyFreePool
  mov ecx, TSmallBlockPoolHeader[edx].NextPartiallyFreePool
  {Remove this manager}
  mov TSmallBlockPoolHeader[eax].NextPartiallyFreePool, ecx
  mov TSmallBlockPoolHeader[ecx].PreviousPartiallyFreePool, eax
  {Zero out eax}
  xor eax, eax
  {Is this the sequential feed pool? If so, stop sequential feeding}
  cmp TSmallBlockType[ebx].CurrentSequentialFeedPool, edx
  jne @NotSequentialFeedPool
@IsSequentialFeedPool:
  mov TSmallBlockType[ebx].MaxSequentialFeedBlockAddress, eax
@NotSequentialFeedPool:
  {Unlock the block type}
  mov TSmallBlockType[ebx].BlockTypeLocked, al
  {Release this pool}
  mov eax, edx
  mov edx, [edx - 4]
{$ifndef AssumeMultiThreaded}
  mov bl, IsMultiThread
{$endif}
  jmp @FreeMediumBlock
  {Align branch target}
{$ifndef AssumeMultiThreaded}
  nop
  nop
{$endif}
  nop
@LockBlockTypeLoop:
  mov eax, $100
  {Attempt to grab the block type}
  lock cmpxchg TSmallBlockType([ebx]).BlockTypeLocked, ah
  je @GotLockOnSmallBlockType
{$ifdef NeverSleepOnThreadContention}
  {Pause instruction (improves performance on P4)}
  rep nop
  {$ifdef UseSwitchToThread}
  push ecx
  push edx
  call SwitchToThread
  pop edx
  pop ecx
  {$endif}
  {Try again}
  jmp @LockBlockTypeLoop
  {Align branch target}
  {$ifndef UseSwitchToThread}
  nop
  {$endif}
{$else}
  {Couldn't grab the block type - sleep and try again}
  push ecx
  push edx
  push InitialSleepTime
  call Sleep
  pop edx
  pop ecx
  {Try again}
  mov eax, $100
  {Attempt to grab the block type}
  lock cmpxchg TSmallBlockType([ebx]).BlockTypeLocked, ah
  je @GotLockOnSmallBlockType
  {Couldn't grab the block type - sleep and try again}
  push ecx
  push edx
  push AdditionalSleepTime
  call Sleep
  pop edx
  pop ecx
  {Try again}
  jmp @LockBlockTypeLoop
  {Align branch target}
  nop
  nop
{$endif}
  {---------------------Medium blocks------------------------------}
  {Align branch target}
@NotSmallBlockInUse:
  {Not a small block in use: is it a medium or large block?}
  test dl, IsFreeBlockFlag + IsLargeBlockFlag
  jnz @NotASmallOrMediumBlock
@FreeMediumBlock:
{$ifdef ClearSmallAndMediumBlocksInFreeMem}
  push eax
  push edx
  and edx, DropMediumAndLargeFlagsMask
  sub edx, BlockHeaderSize
  xor ecx, ecx
  call System.@FillChar
  pop edx
  pop eax
{$endif}
  {Drop the flags}
  and edx, DropMediumAndLargeFlagsMask
  {Free the medium block pointed to by eax, header in edx, bl = IsMultiThread}
{$ifndef AssumeMultiThreaded}
  {Do we need to lock the medium blocks?}
  test bl, bl
{$endif}
  {Block size in ebx}
  mov ebx, edx
  {Save registers}
  push esi
  {Pointer in esi}
  mov esi, eax
  {Do we need to lock the medium blocks?}
{$ifndef AssumeMultiThreaded}
  jz @MediumBlocksLocked
{$endif}
  call LockMediumBlocks
@MediumBlocksLocked:
  {Can we combine this block with the next free block?}
  test dword ptr [esi + ebx - 4], IsFreeBlockFlag
  {Get the next block size and flags in ecx}
  mov ecx, [esi + ebx - 4]
  jnz @NextBlockIsFree
  {Set the "PreviousIsFree" flag in the next block}
  or ecx, PreviousMediumBlockIsFreeFlag
  mov [esi + ebx - 4], ecx
@NextBlockChecked:
  {Can we combine this block with the previous free block? We need to
   re-read the flags since it could have changed before we could lock the
   medium blocks.}
  test byte ptr [esi - 4], PreviousMediumBlockIsFreeFlag
  jnz @PreviousBlockIsFree
@PreviousBlockChecked:
  {Is the entire medium block pool free, and there are other free blocks
   that can fit the largest possible medium block -> free it.}
  cmp ebx, (MediumBlockPoolSize - MediumBlockPoolHeaderSize)
  je @EntireMediumPoolFree
@BinFreeMediumBlock:
  {Store the size of the block as well as the flags}
  lea eax, [ebx + IsMediumBlockFlag + IsFreeBlockFlag]
  mov [esi - 4], eax
  {Store the trailing size marker}
  mov [esi + ebx - 8], ebx
  {Insert this block back into the bins: Size check not required here,
   since medium blocks that are in use are not allowed to be
   shrunk smaller than MinimumMediumBlockSize}
  mov eax, esi
  mov edx, ebx
  {Insert into bin}
  call InsertMediumBlockIntoBin
  {Unlock medium blocks}
  mov MediumBlocksLocked, False;
  {All OK}
  xor eax, eax
  {Restore registers}
  pop esi
  pop ebx
  {Return}
  ret
  {Align branch target}
@NextBlockIsFree:
  {Get the next block address in eax}
  lea eax, [esi + ebx]
  {Increase the size of this block}
  and ecx, DropMediumAndLargeFlagsMask
  add ebx, ecx
  {Was the block binned?}
  cmp ecx, MinimumMediumBlockSize
  jb @NextBlockChecked
  call RemoveMediumFreeBlock
  jmp @NextBlockChecked
  {Align branch target}
  nop
@PreviousBlockIsFree:
  {Get the size of the free block just before this one}
  mov ecx, [esi - 8]
  {Include the previous block}
  sub esi, ecx
  {Set the new block size}
  add ebx, ecx
  {Remove the previous block from the linked list}
  cmp ecx, MinimumMediumBlockSize
  jb @PreviousBlockChecked
  mov eax, esi
  call RemoveMediumFreeBlock
  jmp @PreviousBlockChecked
  {Align branch target}
@EntireMediumPoolFree:
  {Should we make this the new sequential feed medium block pool? If the
   current sequential feed pool is not entirely free, we make this the new
   sequential feed pool.}
  cmp MediumSequentialFeedBytesLeft, MediumBlockPoolSize - MediumBlockPoolHeaderSize
  jne @MakeEmptyMediumPoolSequentialFeed
  {Point esi to the medium block pool header}
  sub esi, MediumBlockPoolHeaderSize
  {Remove this medium block pool from the linked list}
  mov eax, TMediumBlockPoolHeader[esi].PreviousMediumBlockPoolHeader
  mov edx, TMediumBlockPoolHeader[esi].NextMediumBlockPoolHeader
  mov TMediumBlockPoolHeader[eax].NextMediumBlockPoolHeader, edx
  mov TMediumBlockPoolHeader[edx].PreviousMediumBlockPoolHeader, eax
  {Unlock medium blocks}
  mov MediumBlocksLocked, False;
{$ifdef ClearMediumBlockPoolsBeforeReturningToOS}
  mov eax, esi
  mov edx, MediumBlockPoolSize
  xor ecx, ecx
  call System.@FillChar
{$endif}
  {Free the medium block pool}
  push MEM_RELEASE
  push 0
  push esi
  call VirtualFree
  {VirtualFree returns >0 if all is ok}
  cmp eax, 1
  {Return 0 on all ok}
  sbb eax, eax
  {Restore registers}
  pop esi
  pop ebx
  ret
  {Align branch target}
  nop
  nop
  nop
@MakeEmptyMediumPoolSequentialFeed:
  {Get a pointer to the end-marker block}
  lea ebx, [esi + MediumBlockPoolSize - MediumBlockPoolHeaderSize]
  {Bin the current sequential feed pool}
  call BinMediumSequentialFeedRemainder
  {Set this medium pool up as the new sequential feed pool:
   Store the sequential feed pool trailer}
  mov dword ptr [ebx - BlockHeaderSize], IsMediumBlockFlag
  {Store the number of bytes available in the sequential feed chunk}
  mov MediumSequentialFeedBytesLeft, MediumBlockPoolSize - MediumBlockPoolHeaderSize
  {Set the last sequentially fed block}
  mov LastSequentiallyFedMediumBlock, ebx
  {Unlock medium blocks}
  mov MediumBlocksLocked, False;
  {Success}
  xor eax, eax
  {Restore registers}
  pop esi
  pop ebx
  ret
  {Align branch target}
  nop
  nop
@NotASmallOrMediumBlock:
  {Restore ebx}
  pop ebx
  {Is it in fact a large block?}
  test dl, IsFreeBlockFlag + IsMediumBlockFlag
  jz FreeLargeBlock
  {Attempt to free an already free block}
  mov eax, -1
end;

{$else}

{---------------64-bit BASM FastFreeMem---------------}
asm
  .params 3
  .pushnv rbx
  .pushnv rsi
  {Get the block header in rdx}
  mov rdx, [rcx - BlockHeaderSize]
  {Is it a small block in use?}
  test dl, IsFreeBlockFlag + IsMediumBlockFlag + IsLargeBlockFlag
  {Get the IsMultiThread variable in bl}
{$ifndef AssumeMultiThreaded}
  mov bl, IsMultiThread
{$endif}
  {Is it a small block that is in use?}
  jnz @NotSmallBlockInUse
{$ifdef ClearSmallAndMediumBlocksInFreeMem}
  mov rsi, rcx
  mov rdx, TSmallBlockPoolHeader[rdx].BlockType
  movzx edx, TSmallBlockType(rdx).BlockSize
  sub edx, BlockHeaderSize
  xor r8, r8
  call System.@FillChar
  mov rcx, rsi
  mov rdx, [rcx - BlockHeaderSize]
{$endif}
  {Do we need to lock the block type?}
{$ifndef AssumeMultiThreaded}
  test bl, bl
{$endif}
  {Get the small block type in rbx}
  mov rbx, TSmallBlockPoolHeader[rdx].BlockType
  {Do we need to lock the block type?}
{$ifndef AssumeMultiThreaded}
  jnz @LockBlockTypeLoop
{$else}
  jmp @LockBlockTypeLoop
{$endif}
@GotLockOnSmallBlockType:
  {Current state: rdx = @SmallBlockPoolHeader, rcx = APointer, rbx = @SmallBlockType}
  {Decrement the number of blocks in use}
  sub TSmallBlockPoolHeader[rdx].BlocksInUse, 1
  {Get the old first free block}
  mov rax, TSmallBlockPoolHeader[rdx].FirstFreeBlock
  {Is the pool now empty?}
  jz @PoolIsNowEmpty
  {Was the pool full?}
  test rax, rax
  {Store this as the new first free block}
  mov TSmallBlockPoolHeader[rdx].FirstFreeBlock, rcx
  {Store the previous first free block as the block header}
  lea rax, [rax + IsFreeBlockFlag]
  mov [rcx - BlockHeaderSize], rax
  {Insert the pool back into the linked list if it was full}
  jz @SmallPoolWasFull
  {All ok}
  xor eax, eax
  {Unlock the block type}
  mov TSmallBlockType[rbx].BlockTypeLocked, al
  jmp @Done
@SmallPoolWasFull:
  {Insert this as the first partially free pool for the block size}
  mov rcx, TSmallBlockType[rbx].NextPartiallyFreePool
  mov TSmallBlockPoolHeader[rdx].PreviousPartiallyFreePool, rbx
  mov TSmallBlockPoolHeader[rdx].NextPartiallyFreePool, rcx
  mov TSmallBlockPoolHeader[rcx].PreviousPartiallyFreePool, rdx
  mov TSmallBlockType[rbx].NextPartiallyFreePool, rdx
  {Unlock the block type}
  mov TSmallBlockType[rbx].BlockTypeLocked, False
  {All ok}
  xor eax, eax
  jmp @Done
@PoolIsNowEmpty:
  {Was this pool actually in the linked list of pools with space? If not, it
   can only be the sequential feed pool (it is the only pool that may contain
   only one block, i.e. other blocks have not been split off yet)}
  test rax, rax
  jz @IsSequentialFeedPool
  {Pool is now empty: Remove it from the linked list and free it}
  mov rax, TSmallBlockPoolHeader[rdx].PreviousPartiallyFreePool
  mov rcx, TSmallBlockPoolHeader[rdx].NextPartiallyFreePool
  {Remove this manager}
  mov TSmallBlockPoolHeader[rax].NextPartiallyFreePool, rcx
  mov TSmallBlockPoolHeader[rcx].PreviousPartiallyFreePool, rax
  {Zero out eax}
  xor rax, rax
  {Is this the sequential feed pool? If so, stop sequential feeding}
  cmp TSmallBlockType[rbx].CurrentSequentialFeedPool, rdx
  jne @NotSequentialFeedPool
@IsSequentialFeedPool:
  mov TSmallBlockType[rbx].MaxSequentialFeedBlockAddress, rax
@NotSequentialFeedPool:
  {Unlock the block type}
  mov TSmallBlockType[rbx].BlockTypeLocked, al
  {Release this pool}
  mov rcx, rdx
  mov rdx, [rdx - BlockHeaderSize]
{$ifndef AssumeMultiThreaded}
  mov bl, IsMultiThread
{$endif}
  jmp @FreeMediumBlock
@LockBlockTypeLoop:
  mov eax, $100
  {Attempt to grab the block type}
  lock cmpxchg TSmallBlockType([rbx]).BlockTypeLocked, ah
  je @GotLockOnSmallBlockType
{$ifdef NeverSleepOnThreadContention}
  {Pause instruction (improves performance on P4)}
  pause
  {$ifdef UseSwitchToThread}
  mov rsi, rcx
  call SwitchToThread
  mov rcx, rsi
  mov rdx, [rcx - BlockHeaderSize]
  {$endif}
  {Try again}
  jmp @LockBlockTypeLoop
{$else}
  {Couldn't grab the block type - sleep and try again}
  mov rsi, rcx
  mov ecx, InitialSleepTime
  call Sleep
  mov rcx, rsi
  mov rdx, [rcx - BlockHeaderSize]
  {Try again}
  mov eax, $100
  {Attempt to grab the block type}
  lock cmpxchg TSmallBlockType([rbx]).BlockTypeLocked, ah
  je @GotLockOnSmallBlockType
  {Couldn't grab the block type - sleep and try again}
  mov rsi, rcx
  mov ecx, AdditionalSleepTime
  call Sleep
  mov rcx, rsi
  mov rdx, [rcx - BlockHeaderSize]
  {Try again}
  jmp @LockBlockTypeLoop
{$endif}
  {---------------------Medium blocks------------------------------}
@NotSmallBlockInUse:
  {Not a small block in use: is it a medium or large block?}
  test dl, IsFreeBlockFlag + IsLargeBlockFlag
  jnz @NotASmallOrMediumBlock
@FreeMediumBlock:
{$ifdef ClearSmallAndMediumBlocksInFreeMem}
  mov rsi, rcx
  and rdx, DropMediumAndLargeFlagsMask
  sub rdx, BlockHeaderSize
  xor r8, r8
  call System.@FillChar
  mov rcx, rsi
  mov rdx, [rcx - BlockHeaderSize]
{$endif}
  {Drop the flags}
  and rdx, DropMediumAndLargeFlagsMask
  {Free the medium block pointed to by eax, header in edx, bl = IsMultiThread}
{$ifndef AssumeMultiThreaded}
  {Do we need to lock the medium blocks?}
  test bl, bl
{$endif}
  {Block size in rbx}
  mov rbx, rdx
  {Pointer in rsi}
  mov rsi, rcx
  {Do we need to lock the medium blocks?}
{$ifndef AssumeMultiThreaded}
  jz @MediumBlocksLocked
{$endif}
  call LockMediumBlocks
@MediumBlocksLocked:
  {Can we combine this block with the next free block?}
  test qword ptr [rsi + rbx - BlockHeaderSize], IsFreeBlockFlag
  {Get the next block size and flags in rcx}
  mov rcx, [rsi + rbx - BlockHeaderSize]
  jnz @NextBlockIsFree
  {Set the "PreviousIsFree" flag in the next block}
  or rcx, PreviousMediumBlockIsFreeFlag
  mov [rsi + rbx - BlockHeaderSize], rcx
@NextBlockChecked:
  {Can we combine this block with the previous free block? We need to
   re-read the flags since it could have changed before we could lock the
   medium blocks.}
  test byte ptr [rsi - BlockHeaderSize], PreviousMediumBlockIsFreeFlag
  jnz @PreviousBlockIsFree
@PreviousBlockChecked:
  {Is the entire medium block pool free, and there are other free blocks
   that can fit the largest possible medium block -> free it.}
  cmp ebx, (MediumBlockPoolSize - MediumBlockPoolHeaderSize)
  je @EntireMediumPoolFree
@BinFreeMediumBlock:
  {Store the size of the block as well as the flags}
  lea rax, [rbx + IsMediumBlockFlag + IsFreeBlockFlag]
  mov [rsi - BlockHeaderSize], rax
  {Store the trailing size marker}
  mov [rsi + rbx - 2 * BlockHeaderSize], rbx
  {Insert this block back into the bins: Size check not required here,
   since medium blocks that are in use are not allowed to be
   shrunk smaller than MinimumMediumBlockSize}
  mov rcx, rsi
  mov rdx, rbx
  {Insert into bin}
  call InsertMediumBlockIntoBin
  {All OK}
  xor eax, eax
  {Unlock medium blocks}
  mov MediumBlocksLocked, al
  jmp @Done
@NextBlockIsFree:
  {Get the next block address in rax}
  lea rax, [rsi + rbx]
  {Increase the size of this block}
  and rcx, DropMediumAndLargeFlagsMask
  add rbx, rcx
  {Was the block binned?}
  cmp rcx, MinimumMediumBlockSize
  jb @NextBlockChecked
  mov rcx, rax
  call RemoveMediumFreeBlock
  jmp @NextBlockChecked
@PreviousBlockIsFree:
  {Get the size of the free block just before this one}
  mov rcx, [rsi - 2 * BlockHeaderSize]
  {Include the previous block}
  sub rsi, rcx
  {Set the new block size}
  add rbx, rcx
  {Remove the previous block from the linked list}
  cmp ecx, MinimumMediumBlockSize
  jb @PreviousBlockChecked
  mov rcx, rsi
  call RemoveMediumFreeBlock
  jmp @PreviousBlockChecked
@EntireMediumPoolFree:
  {Should we make this the new sequential feed medium block pool? If the
   current sequential feed pool is not entirely free, we make this the new
   sequential feed pool.}
  lea r8, MediumSequentialFeedBytesLeft
  cmp dword ptr [r8], MediumBlockPoolSize - MediumBlockPoolHeaderSize //workaround for QC99023
  jne @MakeEmptyMediumPoolSequentialFeed
  {Point esi to the medium block pool header}
  sub rsi, MediumBlockPoolHeaderSize
  {Remove this medium block pool from the linked list}
  mov rax, TMediumBlockPoolHeader[rsi].PreviousMediumBlockPoolHeader
  mov rdx, TMediumBlockPoolHeader[rsi].NextMediumBlockPoolHeader
  mov TMediumBlockPoolHeader[rax].NextMediumBlockPoolHeader, rdx
  mov TMediumBlockPoolHeader[rdx].PreviousMediumBlockPoolHeader, rax
  {Unlock medium blocks}
  xor eax, eax
  mov MediumBlocksLocked, al
{$ifdef ClearMediumBlockPoolsBeforeReturningToOS}
  mov rcx, rsi
  mov edx, MediumBlockPoolSize
  xor r8, r8
  call System.@FillChar
{$endif}
  {Free the medium block pool}
  mov rcx, rsi
  xor edx, edx
  mov r8d, MEM_RELEASE
  call VirtualFree
  {VirtualFree returns >0 if all is ok}
  cmp eax, 1
  {Return 0 on all ok}
  sbb eax, eax
  jmp @Done
@MakeEmptyMediumPoolSequentialFeed:
  {Get a pointer to the end-marker block}
  lea rbx, [rsi + MediumBlockPoolSize - MediumBlockPoolHeaderSize]
  {Bin the current sequential feed pool}
  call BinMediumSequentialFeedRemainder
  {Set this medium pool up as the new sequential feed pool:
   Store the sequential feed pool trailer}
  mov qword ptr [rbx - BlockHeaderSize], IsMediumBlockFlag
  {Store the number of bytes available in the sequential feed chunk}
  lea rax, MediumSequentialFeedBytesLeft
  mov dword ptr [rax], MediumBlockPoolSize - MediumBlockPoolHeaderSize //QC99023 workaround
  {Set the last sequentially fed block}
  mov LastSequentiallyFedMediumBlock, rbx
  {Success}
  xor eax, eax
  {Unlock medium blocks}
  mov MediumBlocksLocked, al
  jmp @Done
@NotASmallOrMediumBlock:
  {Attempt to free an already free block?}
  mov eax, -1
  {Is it in fact a large block?}
  test dl, IsFreeBlockFlag + IsMediumBlockFlag
  jnz @Done
  call FreeLargeBlock
@Done:
end;
{$endif}
{$endif}

{$ifndef FullDebugMode}
{Replacement for SysReallocMem}
function FastReallocMem(APointer: Pointer; ANewSize: {$ifdef XE2AndUp}NativeInt{$else}Integer{$endif}): Pointer;
{$ifndef ASMVersion}
var
  LBlockHeader, LNextBlockSizeAndFlags, LNewAllocSize, LBlockFlags,
    LOldAvailableSize, LNextBlockSize, LNewAvailableSize, LMinimumUpsize,
    LSecondSplitSize, LNewBlockSize: NativeUInt;
  LPSmallBlockType: PSmallBlockType;
  LPNextBlock, LPNextBlockHeader: Pointer;

  {Upsizes a large block in-place. The following variables are assumed correct:
    LBlockFlags, LOldAvailableSize, LPNextBlock, LNextBlockSizeAndFlags,
    LNextBlockSize, LNewAvailableSize. Medium blocks must be locked on entry if
    required.}
  procedure MediumBlockInPlaceUpsize;
  begin
    {Remove the next block}
    if LNextBlockSizeAndFlags >= MinimumMediumBlockSize then
      RemoveMediumFreeBlock(LPNextBlock);
    {Add 25% for medium block in-place upsizes}
    LMinimumUpsize := LOldAvailableSize + (LOldAvailableSize shr 2);
    if NativeUInt(ANewSize) < LMinimumUpsize then
      LNewAllocSize := LMinimumUpsize
    else
      LNewAllocSize := NativeUInt(ANewSize);
    {Round up to the nearest block size granularity}
    LNewBlockSize := ((LNewAllocSize + (BlockHeaderSize + MediumBlockGranularity - 1 - MediumBlockSizeOffset))
      and -MediumBlockGranularity) + MediumBlockSizeOffset;
    {Calculate the size of the second split}
    LSecondSplitSize := LNewAvailableSize + BlockHeaderSize - LNewBlockSize;
    {Does it fit?}
    if NativeInt(LSecondSplitSize) <= 0 then
    begin
      {The block size is the full available size plus header}
      LNewBlockSize := LNewAvailableSize + BlockHeaderSize;
      {Grab the whole block: Mark it as used in the block following it}
      LPNextBlockHeader := Pointer(PByte(APointer) + LNewAvailableSize);
      PNativeUInt(LPNextBlockHeader)^ :=
        PNativeUInt(LPNextBlockHeader)^ and (not PreviousMediumBlockIsFreeFlag);
    end
    else
    begin
      {Split the block in two}
      LPNextBlock := PMediumFreeBlock(PByte(APointer) + LNewBlockSize);
      {Set the size of the second split}
      PNativeUInt(PByte(LPNextBlock) - BlockHeaderSize)^ := LSecondSplitSize or (IsMediumBlockFlag or IsFreeBlockFlag);
      {Store the size of the second split before the header of the next block}
      PNativeUInt(PByte(LPNextBlock) + LSecondSplitSize - 2 * BlockHeaderSize)^ := LSecondSplitSize;
      {Put the remainder in a bin if it is big enough}
      if LSecondSplitSize >= MinimumMediumBlockSize then
        InsertMediumBlockIntoBin(LPNextBlock, LSecondSplitSize);
    end;
    {Set the size and flags for this block}
    PNativeUInt(PByte(APointer) - BlockHeaderSize)^ := LNewBlockSize or LBlockFlags;
  end;

  {In-place downsize of a medium block. On entry Size must be less than half of
   LOldAvailableSize.}
  procedure MediumBlockInPlaceDownsize;
  begin
    {Round up to the next medium block size}
    LNewBlockSize := ((ANewSize + (BlockHeaderSize + MediumBlockGranularity - 1 - MediumBlockSizeOffset))
      and -MediumBlockGranularity) + MediumBlockSizeOffset;
    {Get the size of the second split}
    LSecondSplitSize := (LOldAvailableSize + BlockHeaderSize) - LNewBlockSize;
    {Lock the medium blocks}
    LockMediumBlocks;
    {Set the new size}
    PNativeUInt(PByte(APointer) - BlockHeaderSize)^ :=
      (PNativeUInt(PByte(APointer) - BlockHeaderSize)^ and ExtractMediumAndLargeFlagsMask)
      or LNewBlockSize;
    {Is the next block in use?}
    LPNextBlock := PNativeUInt(PByte(APointer) + LOldAvailableSize + BlockHeaderSize);
    LNextBlockSizeAndFlags := PNativeUInt(PByte(LPNextBlock) - BlockHeaderSize)^;
    if LNextBlockSizeAndFlags and IsFreeBlockFlag = 0 then
    begin
      {The next block is in use: flag its previous block as free}
      PNativeUInt(PByte(LPNextBlock) - BlockHeaderSize)^ :=
        LNextBlockSizeAndFlags or PreviousMediumBlockIsFreeFlag;
    end
    else
    begin
      {The next block is free: combine it}
      LNextBlockSizeAndFlags := LNextBlockSizeAndFlags and DropMediumAndLargeFlagsMask;
      Inc(LSecondSplitSize, LNextBlockSizeAndFlags);
      if LNextBlockSizeAndFlags >= MinimumMediumBlockSize then
        RemoveMediumFreeBlock(LPNextBlock);
    end;
    {Set the split}
    LPNextBlock := PNativeUInt(PByte(APointer) + LNewBlockSize);
    {Store the free part's header}
    PNativeUInt(PByte(LPNextBlock) - BlockHeaderSize)^ := LSecondSplitSize or (IsMediumBlockFlag or IsFreeBlockFlag);
    {Store the trailing size field}
    PNativeUInt(PByte(LPNextBlock) + LSecondSplitSize - 2 * BlockHeaderSize)^ := LSecondSplitSize;
    {Bin this free block}
    if LSecondSplitSize >= MinimumMediumBlockSize then
      InsertMediumBlockIntoBin(LPNextBlock, LSecondSplitSize);
    {Unlock the medium blocks}
    MediumBlocksLocked := False;
  end;

begin
  {Get the block header: Is it actually a small block?}
  LBlockHeader := PNativeUInt(PByte(APointer) - BlockHeaderSize)^;
  {Is it a small block that is in use?}
  if LBlockHeader and (IsFreeBlockFlag or IsMediumBlockFlag or IsLargeBlockFlag) = 0 then
  begin
    {-----------------------------------Small block-------------------------------------}
    {The block header is a pointer to the block pool: Get the block type}
    LPSmallBlockType := PSmallBlockPoolHeader(LBlockHeader).BlockType;
    {Get the available size inside blocks of this type.}
    LOldAvailableSize := LPSmallBlockType.BlockSize - BlockHeaderSize;
    {Is it an upsize or a downsize?}
    if LOldAvailableSize >= NativeUInt(ANewSize) then
    begin
      {It's a downsize. Do we need to allocate a smaller block? Only if the new
       block size is less than a quarter of the available size less
       SmallBlockDownsizeCheckAdder bytes}
      if (NativeUInt(ANewSize) * 4 + SmallBlockDownsizeCheckAdder) >= LOldAvailableSize then
      begin
        {In-place downsize - return the pointer}
        Result := APointer;
        Exit;
      end
      else
      begin
        {Allocate a smaller block}
        Result := FastGetMem(ANewSize);
        {Allocated OK?}
        if Result <> nil then
        begin
          {Move the data across}
{$ifdef UseCustomVariableSizeMoveRoutines}
  {$ifdef Align16Bytes}
          MoveX16LP(APointer^, Result^, ANewSize);
  {$else}
          MoveX8LP(APointer^, Result^, ANewSize);
  {$endif}
{$else}
          System.Move(APointer^, Result^, ANewSize);
{$endif}
          {Free the old pointer}
          FastFreeMem(APointer);
        end;
      end;
    end
    else
    begin
      {This pointer is being reallocated to a larger block and therefore it is
       logical to assume that it may be enlarged again. Since reallocations are
       expensive, there is a minimum upsize percentage to avoid unnecessary
       future move operations.}
      {Must grow with at least 100% + x bytes}
      LNewAllocSize := LOldAvailableSize * 2 + SmallBlockUpsizeAdder;
      {Still not large enough?}
      if LNewAllocSize < NativeUInt(ANewSize) then
        LNewAllocSize := NativeUInt(ANewSize);
      {Allocate the new block}
      Result := FastGetMem(LNewAllocSize);
      {Allocated OK?}
      if Result <> nil then
      begin
        {Do we need to store the requested size? Only large blocks store the
         requested size.}
        if LNewAllocSize > (MaximumMediumBlockSize - BlockHeaderSize) then
          PLargeBlockHeader(PByte(Result) - LargeBlockHeaderSize).UserAllocatedSize := ANewSize;
        {Move the data across}
{$ifdef UseCustomFixedSizeMoveRoutines}
        LPSmallBlockType.UpsizeMoveProcedure(APointer^, Result^, LOldAvailableSize);
{$else}
        System.Move(APointer^, Result^, LOldAvailableSize);
{$endif}
        {Free the old pointer}
        FastFreeMem(APointer);
      end;
    end;
  end
  else
  begin
    {Is this a medium block or a large block?}
    if LBlockHeader and (IsFreeBlockFlag or IsLargeBlockFlag) = 0 then
    begin
      {-------------------------------Medium block--------------------------------------}
      {What is the available size in the block being reallocated?}
      LOldAvailableSize := (LBlockHeader and DropMediumAndLargeFlagsMask);
      {Get a pointer to the next block}
      LPNextBlock := PNativeUInt(PByte(APointer) + LOldAvailableSize);
      {Subtract the block header size from the old available size}
      Dec(LOldAvailableSize, BlockHeaderSize);
      {Is it an upsize or a downsize?}
      if NativeUInt(ANewSize) > LOldAvailableSize then
      begin
        {Can we do an in-place upsize?}
        LNextBlockSizeAndFlags := PNativeUInt(PByte(LPNextBlock) - BlockHeaderSize)^;
        {Is the next block free?}
        if LNextBlockSizeAndFlags and IsFreeBlockFlag <> 0 then
        begin
          LNextBlockSize := LNextBlockSizeAndFlags and DropMediumAndLargeFlagsMask;
          {The available size including the next block}
          LNewAvailableSize := LOldAvailableSize + LNextBlockSize;
          {Can the block fit?}
          if NativeUInt(ANewSize) <= LNewAvailableSize then
          begin
            {The next block is free and there is enough space to grow this
             block in place.}
{$ifndef AssumeMultiThreaded}
            if IsMultiThread then
            begin
{$endif}
              {Multi-threaded application - lock medium blocks and re-read the
               information on the blocks.}
              LockMediumBlocks;
              {Re-read the info for this block}
              LBlockFlags := PNativeUInt(PByte(APointer) - BlockHeaderSize)^ and ExtractMediumAndLargeFlagsMask;
              {Re-read the info for the next block}
              LNextBlockSizeAndFlags := PNativeUInt(PByte(LPNextBlock) - BlockHeaderSize)^;
              {Recalculate the next block size}
              LNextBlockSize := LNextBlockSizeAndFlags and DropMediumAndLargeFlagsMask;
              {The available size including the next block}
              LNewAvailableSize := LOldAvailableSize + LNextBlockSize;
              {Is the next block still free and the size still sufficient?}
              if (LNextBlockSizeAndFlags and IsFreeBlockFlag <> 0)
                and (NativeUInt(ANewSize) <= LNewAvailableSize) then
              begin
                {Upsize the block in-place}
                MediumBlockInPlaceUpsize;
                {Unlock the medium blocks}
                MediumBlocksLocked := False;
                {Return the result}
                Result := APointer;
                {Done}
                Exit;
              end;
              {Couldn't use the block: Unlock the medium blocks}
              MediumBlocksLocked := False;
{$ifndef AssumeMultiThreaded}
            end
            else
            begin
              {Extract the block flags}
              LBlockFlags := ExtractMediumAndLargeFlagsMask and LBlockHeader;
              {Upsize the block in-place}
              MediumBlockInPlaceUpsize;
              {Return the result}
              Result := APointer;
              {Done}
              Exit;
            end;
{$endif}
          end;
        end;
        {Couldn't upsize in place. Grab a new block and move the data across:
         If we have to reallocate and move medium blocks, we grow by at
         least 25%}
        LMinimumUpsize := LOldAvailableSize + (LOldAvailableSize shr 2);
        if NativeUInt(ANewSize) < LMinimumUpsize then
          LNewAllocSize := LMinimumUpsize
        else
          LNewAllocSize := NativeUInt(ANewSize);
        {Allocate the new block}
        Result := FastGetMem(LNewAllocSize);
        if Result <> nil then
        begin
          {If it's a large block - store the actual user requested size}
          if LNewAllocSize > (MaximumMediumBlockSize - BlockHeaderSize) then
            PLargeBlockHeader(PByte(Result) - LargeBlockHeaderSize).UserAllocatedSize := ANewSize;
          {Move the data across}
{$ifdef UseCustomVariableSizeMoveRoutines}
          MoveX16LP(APointer^, Result^, LOldAvailableSize);
{$else}
          System.Move(APointer^, Result^, LOldAvailableSize);
{$endif}
          {Free the old block}
          FastFreeMem(APointer);
        end;
      end
      else
      begin
        {Must be less than half the current size or we don't bother resizing.}
        if NativeUInt(ANewSize * 2) >= LOldAvailableSize then
        begin
          Result := APointer;
        end
        else
        begin
          {In-place downsize? Balance the cost of moving the data vs. the cost
           of fragmenting the memory pool. Medium blocks in use may never be
           smaller than MinimumMediumBlockSize.}
          if NativeUInt(ANewSize) >= (MinimumMediumBlockSize - BlockHeaderSize) then
          begin
            MediumBlockInPlaceDownsize;
            Result := APointer;
          end
          else
          begin
            {The requested size is less than the minimum medium block size. If
             the requested size is less than the threshold value (currently a
             quarter of the minimum medium block size), move the data to a small
             block, otherwise shrink the medium block to the minimum allowable
             medium block size.}
            if NativeUInt(ANewSize) >= MediumInPlaceDownsizeLimit then
            begin
              {The request is for a size smaller than the minimum medium block
               size, but not small enough to justify moving data: Reduce the
               block size to the minimum medium block size}
              ANewSize := MinimumMediumBlockSize - BlockHeaderSize;
              {Is it already at the minimum medium block size?}
              if LOldAvailableSize > NativeUInt(ANewSize) then
                MediumBlockInPlaceDownsize;
              Result := APointer;
            end
            else
            begin
              {Allocate the new block}
              Result := FastGetMem(ANewSize);
              if Result <> nil then
              begin
                {Move the data across}
{$ifdef UseCustomVariableSizeMoveRoutines}
  {$ifdef Align16Bytes}
                MoveX16LP(APointer^, Result^, ANewSize);
  {$else}
                MoveX8LP(APointer^, Result^, ANewSize);
  {$endif}
{$else}
                System.Move(APointer^, Result^, ANewSize);
{$endif}
                {Free the old block}
                FastFreeMem(APointer);
              end;
            end;
          end;
        end;
      end;
    end
    else
    begin
      {Is this a valid large block?}
      if LBlockHeader and (IsFreeBlockFlag or IsMediumBlockFlag) = 0 then
      begin
        {-----------------------Large block------------------------------}
        Result := ReallocateLargeBlock(APointer, ANewSize);
      end
      else
      begin
        {-----------------------Invalid block------------------------------}
        {Bad pointer: probably an attempt to reallocate a free memory block.}
        Result := nil;
      end;
    end;
  end;
end;
{$else}
{$ifdef 32Bit}
asm
  {On entry: eax = APointer; edx = ANewSize}
  {Get the block header: Is it actually a small block?}
  mov ecx, [eax - 4]
  {Is it a small block?}
  test cl, IsFreeBlockFlag + IsMediumBlockFlag + IsLargeBlockFlag
  {Save ebx}
  push ebx
  {Save esi}
  push esi
  {Save the original pointer in esi}
  mov esi, eax
  {Is it a small block?}
  jnz @NotASmallBlock
  {-----------------------------------Small block-------------------------------------}
  {Get the block type in ebx}
  mov ebx, TSmallBlockPoolHeader[ecx].BlockType
  {Get the available size inside blocks of this type.}
  movzx ecx, TSmallBlockType[ebx].BlockSize
  sub ecx, 4
  {Is it an upsize or a downsize?}
  cmp ecx, edx
  jb @SmallUpsize
  {It's a downsize. Do we need to allocate a smaller block? Only if the new
   size is less than a quarter of the available size less
   SmallBlockDownsizeCheckAdder bytes}
  lea ebx, [edx * 4 + SmallBlockDownsizeCheckAdder]
  cmp ebx, ecx
  jb @NotSmallInPlaceDownsize
  {In-place downsize - return the original pointer}
  pop esi
  pop ebx
  ret
  {Align branch target}
  nop
@NotSmallInPlaceDownsize:
  {Save the requested size}
  mov ebx, edx
  {Allocate a smaller block}
  mov eax, edx
  call FastGetMem
  {Allocated OK?}
  test eax, eax
  jz @SmallDownsizeDone
  {Move data across: count in ecx}
  mov ecx, ebx
  {Destination in edx}
  mov edx, eax
  {Save the result in ebx}
  mov ebx, eax
  {Original pointer in eax}
  mov eax, esi
  {Move the data across}
{$ifdef UseCustomVariableSizeMoveRoutines}
  {$ifdef Align16Bytes}
  call MoveX16LP
  {$else}
  call MoveX8LP
  {$endif}
{$else}
  call System.Move
{$endif}
  {Free the original pointer}
  mov eax, esi
  call FastFreeMem
  {Return the pointer}
  mov eax, ebx
@SmallDownsizeDone:
  pop esi
  pop ebx
  ret
  {Align branch target}
  nop
  nop
@SmallUpsize:
  {State: esi = APointer, edx = ANewSize, ecx = Current Block Size, ebx = Current Block Type}
  {This pointer is being reallocated to a larger block and therefore it is
   logical to assume that it may be enlarged again. Since reallocations are
   expensive, there is a minimum upsize percentage to avoid unnecessary
   future move operations.}
  {Small blocks always grow with at least 100% + SmallBlockUpsizeAdder bytes}
  lea ecx, [ecx + ecx + SmallBlockUpsizeAdder]
  {save edi}
  push edi
  {Save the requested size in edi}
  mov edi, edx
  {New allocated size is the maximum of the requested size and the minimum
   upsize}
  xor eax, eax
  sub ecx, edx
  adc eax, -1
  and eax, ecx
  add eax, edx
  {Allocate the new block}
  call FastGetMem
  {Allocated OK?}
  test eax, eax
  jz @SmallUpsizeDone
  {Do we need to store the requested size? Only large blocks store the
   requested size.}
  cmp edi, MaximumMediumBlockSize - BlockHeaderSize
  jbe @NotSmallUpsizeToLargeBlock
  {Store the user requested size}
  mov [eax - 8], edi
@NotSmallUpsizeToLargeBlock:
  {Get the size to move across}
  movzx ecx, TSmallBlockType[ebx].BlockSize
  sub ecx, BlockHeaderSize
  {Move to the new block}
  mov edx, eax
  {Save the result in edi}
  mov edi, eax
  {Move from the old block}
  mov eax, esi
  {Move the data across}
{$ifdef UseCustomFixedSizeMoveRoutines}
  call TSmallBlockType[ebx].UpsizeMoveProcedure
{$else}
  call System.Move
{$endif}
  {Free the old pointer}
  mov eax, esi
  call FastFreeMem
  {Done}
  mov eax, edi
@SmallUpsizeDone:
  pop edi
  pop esi
  pop ebx
  ret
  {Align branch target}
  nop
@NotASmallBlock:
  {Is this a medium block or a large block?}
  test cl, IsFreeBlockFlag + IsLargeBlockFlag
  jnz @PossibleLargeBlock
  {-------------------------------Medium block--------------------------------------}
  {Status: ecx = Current Block Size + Flags, eax/esi = APointer,
   edx = Requested Size}
  mov ebx, ecx
  {Drop the flags from the header}
  and ecx, DropMediumAndLargeFlagsMask
  {Save edi}
  push edi
  {Get a pointer to the next block in edi}
  lea edi, [eax + ecx]
  {Subtract the block header size from the old available size}
  sub ecx, BlockHeaderSize
  {Get the complete flags in ebx}
  and ebx, ExtractMediumAndLargeFlagsMask
  {Is it an upsize or a downsize?}
  cmp edx, ecx
  {Save ebp}
  push ebp
  {Is it an upsize or a downsize?}
  ja @MediumBlockUpsize
  {Status: ecx = Current Block Size - 4, bl = Current Block Flags,
   edi = @Next Block, eax/esi = APointer, edx = Requested Size}
  {Must be less than half the current size or we don't bother resizing.}
  lea ebp, [edx + edx]
  cmp ebp, ecx
  jb @MediumMustDownsize
@MediumNoResize:
  {Restore registers}
  pop ebp
  pop edi
  pop esi
  pop ebx
  {Return}
  ret
  {Align branch target}
  nop
  nop
  nop
@MediumMustDownsize:
  {In-place downsize? Balance the cost of moving the data vs. the cost of
   fragmenting the memory pool. Medium blocks in use may never be smaller
   than MinimumMediumBlockSize.}
  cmp edx, MinimumMediumBlockSize - BlockHeaderSize
  jae @MediumBlockInPlaceDownsize
  {The requested size is less than the minimum medium block size. If the
  requested size is less than the threshold value (currently a quarter of the
  minimum medium block size), move the data to a small block, otherwise shrink
  the medium block to the minimum allowable medium block size.}
  cmp edx, MediumInPlaceDownsizeLimit
  jb @MediumDownsizeRealloc
  {The request is for a size smaller than the minimum medium block size, but
   not small enough to justify moving data: Reduce the block size to the
   minimum medium block size}
  mov edx, MinimumMediumBlockSize - BlockHeaderSize
  {Is it already at the minimum medium block size?}
  cmp ecx, edx
  jna @MediumNoResize
@MediumBlockInPlaceDownsize:
  {Round up to the next medium block size}
  lea ebp, [edx + BlockHeaderSize + MediumBlockGranularity - 1 - MediumBlockSizeOffset]
  and ebp, -MediumBlockGranularity;
  add ebp, MediumBlockSizeOffset
  {Get the size of the second split}
  add ecx, BlockHeaderSize
  sub ecx, ebp
  {Lock the medium blocks}
{$ifndef AssumeMultiThreaded}
  cmp IsMultiThread, False
  je @DoMediumInPlaceDownsize
{$endif}
@DoMediumLockForDownsize:
  {Lock the medium blocks (ecx *must* be preserved)}
  call LockMediumBlocks
  {Reread the flags - they may have changed before medium blocks could be
   locked.}
  mov ebx, ExtractMediumAndLargeFlagsMask
  and ebx, [esi - 4]
@DoMediumInPlaceDownsize:
  {Set the new size}
  or ebx, ebp
  mov [esi - 4], ebx
  {Get the second split size in ebx}
  mov ebx, ecx
  {Is the next block in use?}
  mov edx, [edi - 4]
  test dl, IsFreeBlockFlag
  jnz @MediumDownsizeNextBlockFree
  {The next block is in use: flag its previous block as free}
  or edx, PreviousMediumBlockIsFreeFlag
  mov [edi - 4], edx
  jmp @MediumDownsizeDoSplit
  {Align branch target}
  nop
  nop
{$ifdef AssumeMultiThreaded}
  nop
{$endif}
@MediumDownsizeNextBlockFree:
  {The next block is free: combine it}
  mov eax, edi
  and edx, DropMediumAndLargeFlagsMask
  add ebx, edx
  add edi, edx
  cmp edx, MinimumMediumBlockSize
  jb @MediumDownsizeDoSplit
  call RemoveMediumFreeBlock
@MediumDownsizeDoSplit:
  {Store the trailing size field}
  mov [edi - 8], ebx
  {Store the free part's header}
  lea eax, [ebx + IsMediumBlockFlag + IsFreeBlockFlag];
  mov [esi + ebp - 4], eax
  {Bin this free block}
  cmp ebx, MinimumMediumBlockSize
  jb @MediumBlockDownsizeDone
  lea eax, [esi + ebp]
  mov edx, ebx
  call InsertMediumBlockIntoBin
@MediumBlockDownsizeDone:
  {Unlock the medium blocks}
  mov MediumBlocksLocked, False
  {Result = old pointer}
  mov eax, esi
  {Restore registers}
  pop ebp
  pop edi
  pop esi
  pop ebx
  {Return}
  ret
  {Align branch target}
@MediumDownsizeRealloc:
  {Save the requested size}
  mov edi, edx
  mov eax, edx
  {Allocate the new block}
  call FastGetMem
  test eax, eax
  jz @MediumBlockDownsizeExit
  {Save the result}
  mov ebp, eax
  mov edx, eax
  mov eax, esi
  mov ecx, edi
  {Move the data across}
{$ifdef UseCustomVariableSizeMoveRoutines}
  {$ifdef Align16Bytes}
  call MoveX16LP
  {$else}
  call MoveX8LP
  {$endif}
{$else}
  call System.Move
{$endif}
  mov eax, esi
  call FastFreeMem
  {Return the result}
  mov eax, ebp
@MediumBlockDownsizeExit:
  pop ebp
  pop edi
  pop esi
  pop ebx
  ret
  {Align branch target}
@MediumBlockUpsize:
  {Status: ecx = Current Block Size - 4, bl = Current Block Flags,
   edi = @Next Block, eax/esi = APointer, edx = Requested Size}
  {Can we do an in-place upsize?}
  mov eax, [edi - 4]
  test al, IsFreeBlockFlag
  jz @CannotUpsizeMediumBlockInPlace
  {Get the total available size including the next block}
  and eax, DropMediumAndLargeFlagsMask
  {ebp = total available size including the next block (excluding the header)}
  lea ebp, [eax + ecx]
  {Can the block fit?}
  cmp edx, ebp
  ja @CannotUpsizeMediumBlockInPlace
  {The next block is free and there is enough space to grow this
   block in place.}
{$ifndef AssumeMultiThreaded}
  cmp IsMultiThread, False
  je @DoMediumInPlaceUpsize
{$endif}
@DoMediumLockForUpsize:
  {Lock the medium blocks (ecx and edx *must* be preserved}
  call LockMediumBlocks
  {Re-read the info for this block (since it may have changed before the medium
   blocks could be locked)}
  mov ebx, ExtractMediumAndLargeFlagsMask
  and ebx, [esi - 4]
  {Re-read the info for the next block}
  mov eax, [edi - 4]
  {Next block still free?}
  test al, IsFreeBlockFlag
  jz @NextMediumBlockChanged
  {Recalculate the next block size}
  and eax, DropMediumAndLargeFlagsMask
  {The available size including the next block}
  lea ebp, [eax + ecx]
  {Can the block still fit?}
  cmp edx, ebp
  ja @NextMediumBlockChanged
@DoMediumInPlaceUpsize:
  {Is the next block binnable?}
  cmp eax, MinimumMediumBlockSize
  {Remove the next block}
  jb @MediumInPlaceNoNextRemove
  mov eax, edi
  push ecx
  push edx
  call RemoveMediumFreeBlock
  pop edx
  pop ecx
@MediumInPlaceNoNextRemove:
  {Medium blocks grow a minimum of 25% in in-place upsizes}
  mov eax, ecx
  shr eax, 2
  add eax, ecx
  {Get the maximum of the requested size and the minimum growth size}
  xor edi, edi
  sub eax, edx
  adc edi, -1
  and eax, edi
  {Round up to the nearest block size granularity}
  lea eax, [eax + edx + BlockHeaderSize + MediumBlockGranularity - 1 - MediumBlockSizeOffset]
  and eax, -MediumBlockGranularity
  add eax, MediumBlockSizeOffset
  {Calculate the size of the second split}
  lea edx, [ebp + BlockHeaderSize]
  sub edx, eax
  {Does it fit?}
  ja @MediumInPlaceUpsizeSplit
  {Grab the whole block: Mark it as used in the block following it}
  and dword ptr [esi + ebp], not PreviousMediumBlockIsFreeFlag
  {The block size is the full available size plus header}
  add ebp, 4
  {Upsize done}
  jmp @MediumUpsizeInPlaceDone
  {Align branch target}
{$ifndef AssumeMultiThreaded}
  nop
  nop
  nop
{$endif}
@MediumInPlaceUpsizeSplit:
  {Store the size of the second split as the second last dword}
  mov [esi + ebp - 4], edx
  {Set the second split header}
  lea edi, [edx + IsMediumBlockFlag + IsFreeBlockFlag]
  mov [esi + eax - 4], edi
  mov ebp, eax
  cmp edx, MinimumMediumBlockSize
  jb @MediumUpsizeInPlaceDone
  add eax, esi
  call InsertMediumBlockIntoBin
@MediumUpsizeInPlaceDone:
  {Set the size and flags for this block}
  or ebp, ebx
  mov [esi - 4], ebp
  {Unlock the medium blocks}
  mov MediumBlocksLocked, False
  {Result = old pointer}
  mov eax, esi
@MediumBlockResizeDone2:
  {Restore registers}
  pop ebp
  pop edi
  pop esi
  pop ebx
  {Return}
  ret
  {Align branch target for "@CannotUpsizeMediumBlockInPlace"}
  nop
  nop
@NextMediumBlockChanged:
  {The next medium block changed while the medium blocks were being locked}
  mov MediumBlocksLocked, False
@CannotUpsizeMediumBlockInPlace:
  {Couldn't upsize in place. Grab a new block and move the data across:
   If we have to reallocate and move medium blocks, we grow by at
   least 25%}
  mov eax, ecx
  shr eax, 2
  add eax, ecx
  {Get the maximum of the requested size and the minimum growth size}
  xor edi, edi
  sub eax, edx
  adc edi, -1
  and eax, edi
  add eax, edx
  {Save the size to allocate}
  mov ebp, eax
  {Save the size to move across}
  mov edi, ecx
  {Get the block}
  push edx
  call FastGetMem
  pop edx
  {Success?}
  test eax, eax
  jz @MediumBlockResizeDone2
  {If it's a Large block - store the actual user requested size}
  cmp ebp, MaximumMediumBlockSize - BlockHeaderSize
  jbe @MediumUpsizeNotLarge
  mov [eax - 8], edx
@MediumUpsizeNotLarge:
  {Save the result}
  mov ebp, eax
  {Move the data across}
  mov edx, eax
  mov eax, esi
  mov ecx, edi
{$ifdef UseCustomVariableSizeMoveRoutines}
  call MoveX16LP
{$else}
  call System.Move
{$endif}
  {Free the old block}
  mov eax, esi
  call FastFreeMem
  {Restore the result}
  mov eax, ebp
  {Restore registers}
  pop ebp
  pop edi
  pop esi
  pop ebx
  {Return}
  ret
  {Align branch target}
  nop
@PossibleLargeBlock:
  {-----------------------Large block------------------------------}
  {Restore registers}
  pop esi
  pop ebx
  {Is this a valid large block?}
  test cl, IsFreeBlockFlag + IsMediumBlockFlag
  jz ReallocateLargeBlock
  {-----------------------Invalid block------------------------------}
  xor eax, eax
end;

{$else}

{-----------------64-bit BASM FastReallocMem-----------------}
asm
  .params 3
  .pushnv rbx
  .pushnv rsi
  .pushnv rdi
  .pushnv r14
  .pushnv r15
  {On entry: rcx = APointer; rdx = ANewSize}
  {Save the original pointer in rsi}
  mov rsi, rcx
  {Get the block header}
  mov rcx, [rcx - BlockHeaderSize]
  {Is it a small block?}
  test cl, IsFreeBlockFlag + IsMediumBlockFlag + IsLargeBlockFlag
  jnz @NotASmallBlock
  {-----------------------------------Small block-------------------------------------}
  {Get the block type in rbx}
  mov rbx, TSmallBlockPoolHeader[rcx].BlockType
  {Get the available size inside blocks of this type.}
  movzx ecx, TSmallBlockType[rbx].BlockSize
  sub ecx, BlockHeaderSize
  {Is it an upsize or a downsize?}
  cmp rcx, rdx
  jb @SmallUpsize
  {It's a downsize. Do we need to allocate a smaller block? Only if the new
   size is less than a quarter of the available size less
   SmallBlockDownsizeCheckAdder bytes}
  lea ebx, [edx * 4 + SmallBlockDownsizeCheckAdder]
  cmp ebx, ecx
  jb @NotSmallInPlaceDownsize
  {In-place downsize - return the original pointer}
  mov rax, rsi
  jmp @Done
@NotSmallInPlaceDownsize:
  {Save the requested size}
  mov rbx, rdx
  {Allocate a smaller block}
  mov rcx, rdx
  call FastGetMem
  {Allocated OK?}
  test rax, rax
  jz @Done
  {Move data across: count in r8}
  mov r8, rbx
  {Destination in edx}
  mov rdx, rax
  {Save the result in ebx}
  mov rbx, rax
  {Original pointer in ecx}
  mov rcx, rsi
  {Move the data across}
{$ifdef UseCustomVariableSizeMoveRoutines}
  {$ifdef Align16Bytes}
  call MoveX16LP
  {$else}
  call MoveX8LP
  {$endif}
{$else}
  call System.Move
{$endif}
  {Free the original pointer}
  mov rcx, rsi
  call FastFreeMem
  {Return the pointer}
  mov rax, rbx
  jmp @Done
@SmallUpsize:
  {State: rsi = APointer, rdx = ANewSize, rcx = Current Block Size, rbx = Current Block Type}
  {This pointer is being reallocated to a larger block and therefore it is
   logical to assume that it may be enlarged again. Since reallocations are
   expensive, there is a minimum upsize percentage to avoid unnecessary
   future move operations.}
  {Small blocks always grow with at least 100% + SmallBlockUpsizeAdder bytes}
  lea ecx, [ecx + ecx + SmallBlockUpsizeAdder]
  {Save the requested size in rdi}
  mov rdi, rdx
  {New allocated size is the maximum of the requested size and the minimum
   upsize}
  xor rax, rax
  sub rcx, rdx
  adc rax, -1
  and rcx, rax
  add rcx, rdx
  {Allocate the new block}
  call FastGetMem
  {Allocated OK?}
  test rax, rax
  jz @Done
  {Do we need to store the requested size? Only large blocks store the
   requested size.}
  cmp rdi, MaximumMediumBlockSize - BlockHeaderSize
  jbe @NotSmallUpsizeToLargeBlock
  {Store the user requested size}
  mov [rax - 2 * BlockHeaderSize], rdi
@NotSmallUpsizeToLargeBlock:
  {Get the size to move across}
  movzx r8d, TSmallBlockType[rbx].BlockSize
  sub r8d, BlockHeaderSize
  {Move to the new block}
  mov rdx, rax
  {Save the result in edi}
  mov rdi, rax
  {Move from the old block}
  mov rcx, rsi
  {Move the data across}
{$ifdef UseCustomFixedSizeMoveRoutines}
  call TSmallBlockType[rbx].UpsizeMoveProcedure
{$else}
  call System.Move
{$endif}
  {Free the old pointer}
  mov rcx, rsi
  call FastFreeMem
  {Done}
  mov rax, rdi
  jmp @Done
@NotASmallBlock:
  {Is this a medium block or a large block?}
  test cl, IsFreeBlockFlag + IsLargeBlockFlag
  jnz @PossibleLargeBlock
  {-------------------------------Medium block--------------------------------------}
  {Status: rcx = Current Block Size + Flags, rsi = APointer,
   rdx = Requested Size}
  mov rbx, rcx
  {Drop the flags from the header}
  and ecx, DropMediumAndLargeFlagsMask
  {Get a pointer to the next block in rdi}
  lea rdi, [rsi + rcx]
  {Subtract the block header size from the old available size}
  sub ecx, BlockHeaderSize
  {Get the complete flags in ebx}
  and ebx, ExtractMediumAndLargeFlagsMask
  {Is it an upsize or a downsize?}
  cmp rdx, rcx
  ja @MediumBlockUpsize
  {Status: ecx = Current Block Size - BlockHeaderSize, bl = Current Block Flags,
   rdi = @Next Block, rsi = APointer, rdx = Requested Size}
  {Must be less than half the current size or we don't bother resizing.}
  lea r15, [rdx + rdx]
  cmp r15, rcx
  jb @MediumMustDownsize
@MediumNoResize:
  mov rax, rsi
  jmp @Done
@MediumMustDownsize:
  {In-place downsize? Balance the cost of moving the data vs. the cost of
   fragmenting the memory pool. Medium blocks in use may never be smaller
   than MinimumMediumBlockSize.}
  cmp edx, MinimumMediumBlockSize - BlockHeaderSize
  jae @MediumBlockInPlaceDownsize
  {The requested size is less than the minimum medium block size. If the
  requested size is less than the threshold value (currently a quarter of the
  minimum medium block size), move the data to a small block, otherwise shrink
  the medium block to the minimum allowable medium block size.}
  cmp edx, MediumInPlaceDownsizeLimit
  jb @MediumDownsizeRealloc
  {The request is for a size smaller than the minimum medium block size, but
   not small enough to justify moving data: Reduce the block size to the
   minimum medium block size}
  mov edx, MinimumMediumBlockSize - BlockHeaderSize
  {Is it already at the minimum medium block size?}
  cmp ecx, edx
  jna @MediumNoResize
@MediumBlockInPlaceDownsize:
  {Round up to the next medium block size}
  lea r15, [rdx + BlockHeaderSize + MediumBlockGranularity - 1 - MediumBlockSizeOffset]
  and r15, -MediumBlockGranularity
  add r15, MediumBlockSizeOffset
  {Get the size of the second split}
  add ecx, BlockHeaderSize
  sub ecx, r15d
  {Lock the medium blocks}
{$ifndef AssumeMultiThreaded}
  lea r8, IsMultiThread
  cmp byte ptr [r8], False
  je @DoMediumInPlaceDownsize
{$endif}
@DoMediumLockForDownsize:
  {Lock the medium blocks}
  mov ebx, ecx
  call LockMediumBlocks
  mov ecx, ebx
  {Reread the flags - they may have changed before medium blocks could be
   locked.}
  mov rbx, ExtractMediumAndLargeFlagsMask
  and rbx, [rsi - BlockHeaderSize]
@DoMediumInPlaceDownsize:
  {Set the new size}
  or rbx, r15
  mov [rsi - BlockHeaderSize], rbx
  {Get the second split size in ebx}
  mov ebx, ecx
  {Is the next block in use?}
  mov rdx, [rdi - BlockHeaderSize]
  test dl, IsFreeBlockFlag
  jnz @MediumDownsizeNextBlockFree
  {The next block is in use: flag its previous block as free}
  or rdx, PreviousMediumBlockIsFreeFlag
  mov [rdi - BlockHeaderSize], rdx
  jmp @MediumDownsizeDoSplit
@MediumDownsizeNextBlockFree:
  {The next block is free: combine it}
  mov rcx, rdi
  and rdx, DropMediumAndLargeFlagsMask
  add rbx, rdx
  add rdi, rdx
  cmp edx, MinimumMediumBlockSize
  jb @MediumDownsizeDoSplit
  call RemoveMediumFreeBlock
@MediumDownsizeDoSplit:
  {Store the trailing size field}
  mov [rdi - 2 * BlockHeaderSize], rbx
  {Store the free part's header}
  lea rcx, [rbx + IsMediumBlockFlag + IsFreeBlockFlag];
  mov [rsi + r15 - BlockHeaderSize], rcx
  {Bin this free block}
  cmp rbx, MinimumMediumBlockSize
  jb @MediumBlockDownsizeDone
  lea rcx, [rsi + r15]
  mov rdx, rbx
  call InsertMediumBlockIntoBin
@MediumBlockDownsizeDone:
  {Unlock the medium blocks}
  lea rax, MediumBlocksLocked
  mov byte ptr [rax], False
  {Result = old pointer}
  mov rax, rsi
  jmp @Done
@MediumDownsizeRealloc:
  {Save the requested size}
  mov rdi, rdx
  mov rcx, rdx
  {Allocate the new block}
  call FastGetMem
  test rax, rax
  jz @Done
  {Save the result}
  mov r15, rax
  mov rdx, rax
  mov rcx, rsi
  mov r8, rdi
  {Move the data across}
{$ifdef UseCustomVariableSizeMoveRoutines}
  {$ifdef Align16Bytes}
  call MoveX16LP
  {$else}
  call MoveX8LP
  {$endif}
{$else}
  call System.Move
{$endif}
  mov rcx, rsi
  call FastFreeMem
  {Return the result}
  mov rax, r15
  jmp @Done
@MediumBlockUpsize:
  {Status: ecx = Current Block Size - BlockHeaderSize, bl = Current Block Flags,
   rdi = @Next Block, rsi = APointer, rdx = Requested Size}
  {Can we do an in-place upsize?}
  mov rax, [rdi - BlockHeaderSize]
  test al, IsFreeBlockFlag
  jz @CannotUpsizeMediumBlockInPlace
  {Get the total available size including the next block}
  and rax, DropMediumAndLargeFlagsMask
  {r15 = total available size including the next block (excluding the header)}
  lea r15, [rax + rcx]
  {Can the block fit?}
  cmp rdx, r15
  ja @CannotUpsizeMediumBlockInPlace
  {The next block is free and there is enough space to grow this
   block in place.}
{$ifndef AssumeMultiThreaded}
  lea r8, IsMultiThread
  cmp byte ptr [r8], False
  je @DoMediumInPlaceUpsize
{$endif}
@DoMediumLockForUpsize:
  {Lock the medium blocks.}
  mov rbx, rcx
  mov r15, rdx
  call LockMediumBlocks
  mov rcx, rbx
  mov rdx, r15
  {Re-read the info for this block (since it may have changed before the medium
   blocks could be locked)}
  mov rbx, ExtractMediumAndLargeFlagsMask
  and rbx, [rsi - BlockHeaderSize]
  {Re-read the info for the next block}
  mov rax, [rdi - BlockheaderSize]
  {Next block still free?}
  test al, IsFreeBlockFlag
  jz @NextMediumBlockChanged
  {Recalculate the next block size}
  and eax, DropMediumAndLargeFlagsMask
  {The available size including the next block}
  lea r15, [rax + rcx]
  {Can the block still fit?}
  cmp rdx, r15
  ja @NextMediumBlockChanged
@DoMediumInPlaceUpsize:
  {Is the next block binnable?}
  cmp eax, MinimumMediumBlockSize
  {Remove the next block}
  jb @MediumInPlaceNoNextRemove
  mov r14, rcx
  mov rcx, rdi
  mov rdi, rdx
  call RemoveMediumFreeBlock
  mov rcx, r14
  mov rdx, rdi
@MediumInPlaceNoNextRemove:
  {Medium blocks grow a minimum of 25% in in-place upsizes}
  mov eax, ecx
  shr eax, 2
  add eax, ecx
  {Get the maximum of the requested size and the minimum growth size}
  xor edi, edi
  sub eax, edx
  adc edi, -1
  and eax, edi
  {Round up to the nearest block size granularity}
  lea eax, [eax + edx + BlockHeaderSize + MediumBlockGranularity - 1 - MediumBlockSizeOffset]
  and eax, -MediumBlockGranularity
  add eax, MediumBlockSizeOffset
  {Calculate the size of the second split}
  lea rdx, [r15 + BlockHeaderSize]
  sub edx, eax
  {Does it fit?}
  ja @MediumInPlaceUpsizeSplit
  {Grab the whole block: Mark it as used in the block following it}
  and qword ptr [rsi + r15], not PreviousMediumBlockIsFreeFlag
  {The block size is the full available size plus header}
  add r15, BlockHeaderSize
  {Upsize done}
  jmp @MediumUpsizeInPlaceDone
@MediumInPlaceUpsizeSplit:
  {Store the size of the second split as the second last dword}
  mov [rsi + r15 - BlockHeaderSize], rdx
  {Set the second split header}
  lea edi, [edx + IsMediumBlockFlag + IsFreeBlockFlag]
  mov [rsi + rax - BlockHeaderSize], rdi
  mov r15, rax
  cmp edx, MinimumMediumBlockSize
  jb @MediumUpsizeInPlaceDone
  lea rcx, [rsi + rax]
  call InsertMediumBlockIntoBin
@MediumUpsizeInPlaceDone:
  {Set the size and flags for this block}
  or r15, rbx
  mov [rsi - BlockHeaderSize], r15
  {Unlock the medium blocks}
  lea rax, MediumBlocksLocked
  mov byte ptr [rax], False
  {Result = old pointer}
  mov rax, rsi
  jmp @Done
@NextMediumBlockChanged:
  {The next medium block changed while the medium blocks were being locked}
  lea rax, MediumBlocksLocked
  mov byte ptr [rax], False
@CannotUpsizeMediumBlockInPlace:
  {Couldn't upsize in place. Grab a new block and move the data across:
   If we have to reallocate and move medium blocks, we grow by at
   least 25%}
  mov eax, ecx
  shr eax, 2
  add eax, ecx
  {Get the maximum of the requested size and the minimum growth size}
  xor rdi, rdi
  sub rax, rdx
  adc rdi, -1
  and rax, rdi
  add rax, rdx
  {Save the size to allocate}
  mov r15, rax
  {Save the size to move across}
  mov edi, ecx
  {Save the requested size}
  mov rbx, rdx
  {Get the block}
  mov rcx, rax
  call FastGetMem
  mov rdx, rbx
  {Success?}
  test eax, eax
  jz @Done
  {If it's a Large block - store the actual user requested size}
  cmp r15, MaximumMediumBlockSize - BlockHeaderSize
  jbe @MediumUpsizeNotLarge
  mov [rax - 2 * BlockHeaderSize], rdx
@MediumUpsizeNotLarge:
  {Save the result}
  mov r15, rax
  {Move the data across}
  mov rdx, rax
  mov rcx, rsi
  mov r8, rdi
{$ifdef UseCustomVariableSizeMoveRoutines}
  call MoveX16LP
{$else}
  call System.Move
{$endif}
  {Free the old block}
  mov rcx, rsi
  call FastFreeMem
  {Restore the result}
  mov rax, r15
  jmp @Done
@PossibleLargeBlock:
  {-----------------------Large block------------------------------}
  {Is this a valid large block?}
  test cl, IsFreeBlockFlag + IsMediumBlockFlag
  jnz @Error
  mov rcx, rsi
  call ReallocateLargeBlock
  jmp @Done
  {-----------------------Invalid block------------------------------}
@Error:
  xor eax, eax
@Done:
end;
{$endif}
{$endif}
{$endif}

{Allocates a block and fills it with zeroes}
function FastAllocMem(ASize: {$ifdef XE2AndUp}NativeInt{$else}Cardinal{$endif}): Pointer;
{$ifndef ASMVersion}
begin
  Result := FastGetMem(ASize);
  {Large blocks are already zero filled}
  if (Result <> nil) and (ASize <= (MaximumMediumBlockSize - BlockHeaderSize)) then
    FillChar(Result^, ASize, 0);
end;
{$else}
{$ifdef 32Bit}
asm
  push ebx
  {Get the size rounded down to the previous multiple of 4 into ebx}
  lea ebx, [eax - 1]
  and ebx, -4
  {Get the block}
  call FastGetMem
  {Could a block be allocated? ecx = 0 if yes, $ffffffff if no}
  cmp eax, 1
  sbb ecx, ecx
  {Point edx to the last dword}
  lea edx, [eax + ebx]
  {ebx = $ffffffff if no block could be allocated, otherwise size rounded down
   to previous multiple of 4. If ebx = 0 then the block size is 1..4 bytes and
   the FPU based clearing loop should not be used (since it clears 8 bytes per
   iteration).}
  or ebx, ecx
  jz @ClearLastDWord
  {Large blocks are already zero filled}
  cmp ebx, MaximumMediumBlockSize - BlockHeaderSize
  jae @Done
  {Make the counter negative based}
  neg ebx
  {Load zero into st(0)}
  fldz
  {Clear groups of 8 bytes. Block sizes are always four less than a multiple
   of 8.}
@FillLoop:
  fst qword ptr [edx + ebx]
  add ebx, 8
  js @FillLoop
  {Clear st(0)}
  ffree st(0)
  {Correct the stack top}
  fincstp
  {Clear the last four bytes}
@ClearLastDWord:
  mov [edx], ecx
@Done:
  pop ebx
end;

{$else}

{---------------64-bit BASM FastAllocMem---------------}
asm
  .params 1
  .pushnv rbx
  {Get the size rounded down to the previous multiple of SizeOf(Pointer) into
   ebx}
  lea rbx, [rcx - 1]
  and rbx, -8
  {Get the block}
  call FastGetMem
  {Could a block be allocated? rcx = 0 if yes, -1 if no}
  cmp rax, 1
  sbb rcx, rcx
  {Point rdx to the last dword}
  lea rdx, [rax + rbx]
  {rbx = -1 if no block could be allocated, otherwise size rounded down
   to previous multiple of 8. If rbx = 0 then the block size is 1..8 bytes and
   the SSE2 based clearing loop should not be used (since it clears 16 bytes per
   iteration).}
  or rbx, rcx
  jz @ClearLastQWord
  {Large blocks are already zero filled}
  cmp rbx, MaximumMediumBlockSize - BlockHeaderSize
  jae @Done
  {Make the counter negative based}
  neg rbx
  {Load zero into xmm0}
  pxor xmm0, xmm0
  {Clear groups of 16 bytes. Block sizes are always 8 less than a multiple of
   16.}
@FillLoop:
  movdqa [rdx + rbx], xmm0
  add rbx, 16
  js @FillLoop
  {Clear the last 8 bytes}
@ClearLastQWord:
  xor rcx, rcx
  mov [rdx], rcx
@Done:
end;
{$endif}
{$endif}

{-----------------Post Uninstall GetMem/FreeMem/ReallocMem-------------------}

{$ifdef DetectMMOperationsAfterUninstall}

function InvalidGetMem(ASize: {$ifdef XE2AndUp}NativeInt{$else}Integer{$endif}): Pointer;
{$ifndef NoMessageBoxes}
var
  LErrorMessageTitle: array[0..1023] of AnsiChar;
{$endif}
begin
{$ifdef UseOutputDebugString}
  OutputDebugStringA(InvalidGetMemMsg);
{$endif}
{$ifndef NoMessageBoxes}
  AppendStringToModuleName(InvalidOperationTitle, LErrorMessageTitle);
  ShowMessageBox(InvalidGetMemMsg, LErrorMessageTitle);
{$endif}
  Result := nil;
end;

function InvalidFreeMem(APointer: Pointer): Integer;
{$ifndef NoMessageBoxes}
var
  LErrorMessageTitle: array[0..1023] of AnsiChar;
{$endif}
begin
{$ifdef UseOutputDebugString}
  OutputDebugStringA(InvalidFreeMemMsg);
{$endif}
{$ifndef NoMessageBoxes}
  AppendStringToModuleName(InvalidOperationTitle, LErrorMessageTitle);
  ShowMessageBox(InvalidFreeMemMsg, LErrorMessageTitle);
{$endif}
  Result := -1;
end;

function InvalidReallocMem(APointer: Pointer; ANewSize: {$ifdef XE2AndUp}NativeInt{$else}Integer{$endif}): Pointer;
{$ifndef NoMessageBoxes}
var
  LErrorMessageTitle: array[0..1023] of AnsiChar;
{$endif}
begin
{$ifdef UseOutputDebugString}
  OutputDebugStringA(InvalidReallocMemMsg);
{$endif}
{$ifndef NoMessageBoxes}
  AppendStringToModuleName(InvalidOperationTitle, LErrorMessageTitle);
  ShowMessageBox(InvalidReallocMemMsg, LErrorMessageTitle);
{$endif}
  Result := nil;
end;

function InvalidAllocMem(ASize: {$ifdef XE2AndUp}NativeInt{$else}Cardinal{$endif}): Pointer;
{$ifndef NoMessageBoxes}
var
  LErrorMessageTitle: array[0..1023] of AnsiChar;
{$endif}
begin
{$ifdef UseOutputDebugString}
  OutputDebugStringA(InvalidAllocMemMsg);
{$endif}
{$ifndef NoMessageBoxes}
  AppendStringToModuleName(InvalidOperationTitle, LErrorMessageTitle);
  ShowMessageBox(InvalidAllocMemMsg, LErrorMessageTitle);
{$endif}
  Result := nil;
end;

function InvalidRegisterAndUnRegisterMemoryLeak(APointer: Pointer): Boolean;
begin
  Result := False;
end;

{$endif}

{-----------------Full Debug Mode Memory Manager Interface--------------------}

{$ifdef FullDebugMode}

{Compare [AAddress], CompareVal:
 If Equal: [AAddress] := NewVal and result = CompareVal
 If Unequal: Result := [AAddress]}
function LockCmpxchg32(CompareVal, NewVal: Integer; AAddress: PInteger): Integer;
asm
{$ifdef 32Bit}
  {On entry:
    eax = CompareVal,
    edx = NewVal,
    ecx = AAddress}
  lock cmpxchg [ecx], edx
{$else}
.noframe
  {On entry:
    ecx = CompareVal,
    edx = NewVal,
    r8 = AAddress}
  mov eax, ecx
  lock cmpxchg [r8], edx
{$endif}
end;

{Called by DebugGetMem, DebugFreeMem and DebugReallocMem in order to block a
 free block scan operation while the memory pool is being modified.}
procedure StartChangingFullDebugModeBlock;
var
  LOldCount: Integer;
begin
  while True do
  begin
    {Get the old thread count}
    LOldCount := ThreadsInFullDebugModeRoutine;
    if (LOldCount >= 0)
      and (LockCmpxchg32(LOldCount, LOldCount + 1, @ThreadsInFullDebugModeRoutine) = LOldCount) then
    begin
      Break;
    end;
  {$ifdef NeverSleepOnThreadContention}
    {$ifdef UseSwitchToThread}
    SwitchToThread;
    {$endif}
  {$else}
    Sleep(InitialSleepTime);
    {Try again}
    LOldCount := ThreadsInFullDebugModeRoutine;
    if (LOldCount >= 0)
      and (LockCmpxchg32(LOldCount, LOldCount + 1, @ThreadsInFullDebugModeRoutine) = LOldCount) then
    begin
      Break;
    end;
    Sleep(AdditionalSleepTime);
  {$endif}
  end;
end;

procedure DoneChangingFullDebugModeBlock;
asm
{$ifdef 32Bit}
  lock dec ThreadsInFullDebugModeRoutine
{$else}
.noframe
  lea rax, ThreadsInFullDebugModeRoutine
  lock dec dword ptr [rax]
{$endif}
end;

{Increments the allocation number}
procedure IncrementAllocationNumber;
asm
{$ifdef 32Bit}
  lock inc CurrentAllocationNumber
{$else}
.noframe
  lea rax, CurrentAllocationNumber
  lock inc dword ptr [rax]
{$endif}
end;

{Called by a routine wanting to lock the entire memory pool in FullDebugMode, e.g. before scanning the memory
 pool for corruptions.}
procedure BlockFullDebugModeMMRoutines;
begin
  while True do
  begin
    {Get the old thread count}
    if LockCmpxchg32(0, -1, @ThreadsInFullDebugModeRoutine) = 0 then
      Break;
{$ifdef NeverSleepOnThreadContention}
  {$ifdef UseSwitchToThread}
    SwitchToThread;
  {$endif}
{$else}
    Sleep(InitialSleepTime);
    {Try again}
    if LockCmpxchg32(0, -1, @ThreadsInFullDebugModeRoutine) = 0 then
      Break;
    Sleep(AdditionalSleepTime);
{$endif}
  end;
end;

procedure UnblockFullDebugModeMMRoutines;
begin
  {Currently blocked? If so, unblock the FullDebugMode routines.}
  if ThreadsInFullDebugModeRoutine = -1 then
    ThreadsInFullDebugModeRoutine := 0;
end;

procedure DeleteEventLog;
begin
  {Delete the file}
  DeleteFileA(MMLogFileName);
end;

{Finds the start and length of the file name given a full path.}
procedure ExtractFileName(APFullPath: PAnsiChar; var APFileNameStart: PAnsiChar; var AFileNameLength: Integer);
var
  LChar: AnsiChar;
begin
  {Initialize}
  APFileNameStart := APFullPath;
  AFileNameLength := 0;
  {Find the file }
  while True do
  begin
    {Get the next character}
    LChar := APFullPath^;
    {End of the path string?}
    if LChar = #0 then
      Break;
    {Advance the buffer position}
    Inc(APFullPath);
    {Found a backslash? -> May be the start of the file name}
    if LChar = '\' then
      APFileNameStart := APFullPath;
  end;
  {Calculate the length of the file name}
  AFileNameLength := IntPtr(APFullPath) - IntPtr(APFileNameStart);
end;

procedure AppendEventLog(ABuffer: Pointer; ACount: Cardinal);
const
  {Declared here, because it is not declared in the SHFolder.pas unit of some older Delphi versions.}
  SHGFP_TYPE_CURRENT = 0;
var
  LFileHandle, LBytesWritten: Cardinal;
  LEventHeader: array[0..1023] of AnsiChar;
  LAlternateLogFileName: array[0..2047] of AnsiChar;
  LPathLen, LNameLength: Integer;
  LMsgPtr, LPFileName: PAnsiChar;
  LSystemTime: TSystemTime;
begin
  {Try to open the log file in read/write mode.}
  LFileHandle := CreateFileA(MMLogFileName, GENERIC_READ or GENERIC_WRITE,
    0, nil, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
  {Did log file creation fail? If so, the destination folder is perhaps read-only:
   Try to redirect logging to a file in the user's "My Documents" folder.}
  if (LFileHandle = INVALID_HANDLE_VALUE)
{$ifdef Delphi4or5}
    and SHGetSpecialFolderPathA(0, @LAlternateLogFileName, CSIDL_PERSONAL, True) then
{$else}
    and (SHGetFolderPathA(0, CSIDL_PERSONAL or CSIDL_FLAG_CREATE, 0,
      SHGFP_TYPE_CURRENT, @LAlternateLogFileName) = S_OK) then
{$endif}
  begin
    {Extract the filename part from MMLogFileName and append it to the path of
     the "My Documents" folder.}
    LPathLen := StrLen(LAlternateLogFileName);
    {Ensure that there is a trailing backslash in the path}
    if (LPathLen = 0) or (LAlternateLogFileName[LPathLen - 1] <> '\') then
    begin
      LAlternateLogFileName[LPathLen] := '\';
      Inc(LPathLen);
    end;
    {Add the filename to the path}
    ExtractFileName(@MMLogFileName, LPFileName, LNameLength);
    System.Move(LPFileName^, LAlternateLogFileName[LPathLen], LNameLength + 1);
    {Try to open the alternate log file}
    LFileHandle := CreateFileA(LAlternateLogFileName, GENERIC_READ or GENERIC_WRITE,
      0, nil, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
  end;
  {Was the log file opened/created successfully?}
  if LFileHandle <> INVALID_HANDLE_VALUE then
  begin
    {Seek to the end of the file}
    SetFilePointer(LFileHandle, 0, nil, FILE_END);
    {Set the separator}
    LMsgPtr := AppendStringToBuffer(CRLF, @LEventHeader[0], Length(CRLF));
    LMsgPtr := AppendStringToBuffer(EventSeparator, LMsgPtr, Length(EventSeparator));
    {Set the date & time}
    GetLocalTime(LSystemTime);
    LMsgPtr := NativeUIntToStrBuf(LSystemTime.wYear, LMsgPtr);
    LMsgPtr^ := '/';
    Inc(LMsgPtr);
    LMsgPtr := NativeUIntToStrBuf(LSystemTime.wMonth, LMsgPtr);
    LMsgPtr^ := '/';
    Inc(LMsgPtr);
    LMsgPtr := NativeUIntToStrBuf(LSystemTime.wDay, LMsgPtr);
    LMsgPtr^ := ' ';
    Inc(LMsgPtr);
    LMsgPtr := NativeUIntToStrBuf(LSystemTime.wHour, LMsgPtr);
    LMsgPtr^ := ':';
    Inc(LMsgPtr);
    if LSystemTime.wMinute < 10 then
    begin
      LMsgPtr^ := '0';
      Inc(LMsgPtr);
    end;
    LMsgPtr := NativeUIntToStrBuf(LSystemTime.wMinute, LMsgPtr);
    LMsgPtr^ := ':';
    Inc(LMsgPtr);
    if LSystemTime.wSecond < 10 then
    begin
      LMsgPtr^ := '0';
      Inc(LMsgPtr);
    end;
    LMsgPtr := NativeUIntToStrBuf(LSystemTime.WSecond, LMsgPtr);
    {Write the header}
    LMsgPtr := AppendStringToBuffer(EventSeparator, LMsgPtr, Length(EventSeparator));
    LMsgPtr := AppendStringToBuffer(CRLF, LMsgPtr, Length(CRLF));
    WriteFile(LFileHandle, LEventHeader[0], NativeUInt(LMsgPtr) - NativeUInt(@LEventHeader[0]), LBytesWritten, nil);
    {Write the data}
    WriteFile(LFileHandle, ABuffer^, ACount, LBytesWritten, nil);
    {Close the file}
    CloseHandle(LFileHandle);
  end;
end;

{Sets the default log filename}
procedure SetDefaultMMLogFileName;
const
  LogFileExtAnsi: PAnsiChar = LogFileExtension;
var
  LEnvVarLength, LModuleNameLength: Cardinal;
  LPathOverride: array[0..2047] of AnsiChar;
  LPFileName: PAnsiChar;
  LFileNameLength: Integer;
begin
  {Get the name of the application}
  LModuleNameLength := AppendModuleFileName(@MMLogFileName[0]);
  {Replace the last few characters of the module name, and optionally override
   the path.}
  if LModuleNameLength > 0 then
  begin
    {Change the filename}
    System.Move(LogFileExtAnsi^, MMLogFileName[LModuleNameLength - 4],
      StrLen(LogFileExtAnsi) + 1);
    {Try to read the FastMMLogFilePath environment variable}
    LEnvVarLength := GetEnvironmentVariableA('FastMMLogFilePath',
      @LPathOverride, 1023);
    {Does the environment variable exist? If so, override the log file path.}
    if LEnvVarLength > 0 then
    begin
      {Ensure that there's a trailing backslash.}
      if LPathOverride[LEnvVarLength - 1] <> '\' then
      begin
        LPathOverride[LEnvVarLength] := '\';
        Inc(LEnvVarLength);
      end;
      {Add the filename to the path override}
      ExtractFileName(@MMLogFileName[0], LPFileName, LFileNameLength);
      System.Move(LPFileName^, LPathOverride[LEnvVarLength], LFileNameLength + 1);
      {Copy the override path back to the filename buffer}
      System.Move(LPathOverride, MMLogFileName, SizeOf(MMLogFileName) - 1);
    end;
  end;
end;

{Specify the full path and name for the filename to be used for logging memory
 errors, etc. If ALogFileName is nil or points to an empty string it will
 revert to the default log file name.}
procedure SetMMLogFileName(ALogFileName: PAnsiChar = nil);
var
  LLogFileNameLen: Integer;
begin
  {Is ALogFileName valid?}
  if (ALogFileName <> nil) and (ALogFileName^ <> #0) then
  begin
    LLogFileNameLen := StrLen(ALogFileName);
    if LLogFileNameLen < Length(MMLogFileName) then
    begin
      {Set the log file name}
      System.Move(ALogFileName^, MMLogFileName, LLogFileNameLen + 1);
      Exit;
    end;
  end;
  {Invalid log file name}
  SetDefaultMMLogFileName;
end;

{Returns the current "allocation group". Whenever a GetMem request is serviced
 in FullDebugMode, the current "allocation group" is stored in the block header.
 This may help with debugging. Note that if a block is subsequently reallocated
 that it keeps its original "allocation group" and "allocation number" (all
 allocations are also numbered sequentially).}
function GetCurrentAllocationGroup: Cardinal;
begin
  Result := AllocationGroupStack[AllocationGroupStackTop];
end;

{Allocation groups work in a stack like fashion. Group numbers are pushed onto
 and popped off the stack. Note that the stack size is limited, so every push
 should have a matching pop.}
procedure PushAllocationGroup(ANewCurrentAllocationGroup: Cardinal);
begin
  if AllocationGroupStackTop < AllocationGroupStackSize - 1 then
  begin
    Inc(AllocationGroupStackTop);
    AllocationGroupStack[AllocationGroupStackTop] := ANewCurrentAllocationGroup;
  end
  else
  begin
    {Raise a runtime error if the stack overflows}
  {$ifdef BCB6OrDelphi7AndUp}
    System.Error(reInvalidPtr);
  {$else}
    System.RunError(reInvalidPtr);
  {$endif}
  end;
end;

procedure PopAllocationGroup;
begin
  if AllocationGroupStackTop > 0 then
  begin
    Dec(AllocationGroupStackTop);
  end
  else
  begin
    {Raise a runtime error if the stack underflows}
  {$ifdef BCB6OrDelphi7AndUp}
    System.Error(reInvalidPtr);
  {$else}
    System.RunError(reInvalidPtr);
  {$endif}
  end;
end;

{Sums all the dwords starting at the given address. ACount must be > 0 and a
 multiple of SizeOf(Pointer).}
function SumNativeUInts(AStartValue: NativeUInt; APointer: PNativeUInt;
  ACount: NativeUInt): NativeUInt;
asm
{$ifdef 32Bit}
  {On entry: eax = AStartValue, edx = APointer; ecx = ACount}
  add edx, ecx
  neg ecx
@AddLoop:
  add eax, [edx + ecx]
  add ecx, 4
  js @AddLoop
{$else}
  {On entry: rcx = AStartValue, rdx = APointer; r8 = ACount}
  add rdx, r8
  neg r8
  mov rax, rcx
@AddLoop:
  add rax, [rdx + r8]
  add r8, 8
  js @AddLoop
{$endif}
end;

{Checks the memory starting at the given address for the fill pattern.
 Returns True if all bytes are all valid. ACount must be >0 and a multiple of
 SizeOf(Pointer).}
function CheckFillPattern(APointer: Pointer; ACount: NativeUInt;
  AFillPattern: NativeUInt): Boolean;
asm
{$ifdef 32Bit}
  {On entry: eax = APointer; edx = ACount; ecx = AFillPattern}
  add eax, edx
  neg edx
@CheckLoop:
  cmp [eax + edx], ecx
  jne @Done
  add edx, 4
  js @CheckLoop
@Done:
  sete al
{$else}
  {On entry: rcx = APointer; rdx = ACount; r8 = AFillPattern}
  add rcx, rdx
  neg rdx
@CheckLoop:
  cmp [rcx + rdx], r8
  jne @Done
  add rdx, 8
  js @CheckLoop
@Done:
  sete al
{$endif}
end;

{Calculates the checksum for the debug header. Adds all dwords in the debug
 header to the start address of the block.}
function CalculateHeaderCheckSum(APointer: PFullDebugBlockHeader): NativeUInt;
begin
  Result := SumNativeUInts(
    NativeUInt(APointer),
    PNativeUInt(PByte(APointer) + 2 * SizeOf(Pointer)),
    SizeOf(TFullDebugBlockHeader) - 2 * SizeOf(Pointer) - SizeOf(NativeUInt));
end;

procedure UpdateHeaderAndFooterCheckSums(APointer: PFullDebugBlockHeader);
var
  LHeaderCheckSum: NativeUInt;
begin
  LHeaderCheckSum := CalculateHeaderCheckSum(APointer);
  APointer.HeaderCheckSum := LHeaderCheckSum;
  PNativeUInt(PByte(APointer) + SizeOf(TFullDebugBlockHeader) + APointer.UserSize)^ := not LHeaderCheckSum;
end;

function LogCurrentThreadAndStackTrace(ASkipFrames: Cardinal; ABuffer: PAnsiChar): PAnsiChar;
var
  LCurrentStackTrace: TStackTrace;
begin
  {Get the current call stack}
  GetStackTrace(@LCurrentStackTrace[0], StackTraceDepth, ASkipFrames);
  {Log the thread ID}
  Result := AppendStringToBuffer(CurrentThreadIDMsg, ABuffer, Length(CurrentThreadIDMsg));
  Result := NativeUIntToHexBuf(GetThreadID, Result);
  {List the stack trace}
  Result := AppendStringToBuffer(CurrentStackTraceMsg, Result, Length(CurrentStackTraceMsg));
  Result := LogStackTrace(@LCurrentStackTrace, StackTraceDepth, Result);
end;

{$ifndef DisableLoggingOfMemoryDumps}
function LogMemoryDump(APointer: PFullDebugBlockHeader; ABuffer: PAnsiChar): PAnsiChar;
var
  LByteNum, LVal: Cardinal;
  LDataPtr: PByte;
begin
  Result := AppendStringToBuffer(MemoryDumpMsg, ABuffer, Length(MemoryDumpMsg));
  Result := NativeUIntToHexBuf(NativeUInt(APointer) + SizeOf(TFullDebugBlockHeader), Result);
  Result^ := ':';
  Inc(Result);
  {Add the bytes}
  LDataPtr := PByte(PByte(APointer) + SizeOf(TFullDebugBlockHeader));
  for LByteNum := 0 to 255 do
  begin
    if LByteNum and 31 = 0 then
    begin
      Result^ := #13;
      Inc(Result);
      Result^ := #10;
      Inc(Result);
    end
    else
    begin
      Result^ := ' ';
      Inc(Result);
    end;
    {Set the hex data}
    LVal := Byte(LDataPtr^);
    Result^ := HexTable[LVal shr 4];
    Inc(Result);
    Result^ := HexTable[LVal and $f];
    Inc(Result);
    {Next byte}
    Inc(LDataPtr);
  end;
  {Dump ASCII}
  LDataPtr := PByte(PByte(APointer) + SizeOf(TFullDebugBlockHeader));
  for LByteNum := 0 to 255 do
  begin
    if LByteNum and 31 = 0 then
    begin
      Result^ := #13;
      Inc(Result);
      Result^ := #10;
      Inc(Result);
    end
    else
    begin
      Result^ := ' ';
      Inc(Result);
      Result^ := ' ';
      Inc(Result);
    end;
    {Set the hex data}
    LVal := Byte(LDataPtr^);
    if LVal < 32 then
      Result^ := '.'
    else
      Result^ := AnsiChar(LVal);
    Inc(Result);
    {Next byte}
    Inc(LDataPtr);
  end;
end;
{$endif}

{Rotates AValue ABitCount bits to the right}
function RotateRight(AValue, ABitCount: NativeUInt): NativeUInt;
asm
{$ifdef 32Bit}
  mov ecx, edx
  ror eax, cl
{$else}
  mov rax, rcx
  mov rcx, rdx
  ror rax, cl
{$endif}
end;

{Determines whether a byte in the user portion of the freed block has been modified. Does not work beyond
 the end of the user portion (i.e. footer and beyond).}
function FreeBlockByteWasModified(APointer: PFullDebugBlockHeader; AUserOffset: NativeUInt): Boolean;
var
  LFillPattern: NativeUInt;
begin
  {Get the expected fill pattern}
  if AUserOffset < SizeOf(Pointer) then
  begin
    LFillPattern := NativeUInt(@FreedObjectVMT.VMTMethods[0]);
  end
  else
  begin
{$ifndef CatchUseOfFreedInterfaces}
    LFillPattern := DebugFillPattern;
{$else}
    LFillPattern := NativeUInt(@VMTBadInterface);
{$endif}
  end;
  {Compare the byte value}
  Result := Byte(PByte(PByte(APointer) + SizeOf(TFullDebugBlockHeader) + AUserOffset)^) <>
    Byte(RotateRight(LFillPattern, (AUserOffset and (SizeOf(Pointer) - 1)) * 8));
end;

function LogBlockChanges(APointer: PFullDebugBlockHeader; ABuffer: PAnsiChar): PAnsiChar;
var
  LOffset, LChangeStart, LCount: NativeUInt;
  LLogCount: Integer;
begin
  {No errors logged so far}
  LLogCount := 0;
  {Log a maximum of 32 changes}
  LOffset := 0;
  while (LOffset < APointer.UserSize) and (LLogCount < 32) do
  begin
    {Has the byte been modified?}
    if FreeBlockByteWasModified(APointer, LOffset) then
    begin
      {Found the start of a changed block, now find the length}
      LChangeStart := LOffset;
      LCount := 0;
      while True do
      begin
        Inc(LCount);
        Inc(LOffset);
        if (LOffset >= APointer.UserSize)
          or (not FreeBlockByteWasModified(APointer, LOffset)) then
        begin
          Break;
        end;
      end;
      {Got the offset and length, now log it.}
      if LLogCount = 0 then
      begin
        ABuffer := AppendStringToBuffer(FreeModifiedDetailMsg, ABuffer, Length(FreeModifiedDetailMsg));
      end
      else
      begin
        ABuffer^ := ',';
        Inc(ABuffer);
        ABuffer^ := ' ';
        Inc(ABuffer);
      end;
      ABuffer := NativeUIntToStrBuf(LChangeStart, ABuffer);
      ABuffer^ := '(';
      Inc(ABuffer);
      ABuffer := NativeUIntToStrBuf(LCount, ABuffer);
      ABuffer^ := ')';
      Inc(ABuffer);
      {Increment the log count}
      Inc(LLogCount);
    end;
    {Next byte}
    Inc(LOffset);
  end;
  {Return the current buffer position}
  Result := ABuffer;
end;

procedure LogBlockError(APointer: PFullDebugBlockHeader; AOperation: TBlockOperation; LHeaderValid, LFooterValid: Boolean);
var
  LMsgPtr: PAnsiChar;
  LErrorMessage: array[0..32767] of AnsiChar;
{$ifndef NoMessageBoxes}
  LErrorMessageTitle: array[0..1023] of AnsiChar;
{$endif}
  LClass: TClass;
  {$ifdef CheckCppObjectTypeEnabled}
  LCppObjectTypeName: PAnsiChar;
  {$endif}
begin
  {Display the error header and the operation type.}
  LMsgPtr := AppendStringToBuffer(ErrorMsgHeader, @LErrorMessage[0], Length(ErrorMsgHeader));
  case AOperation of
    boGetMem: LMsgPtr := AppendStringToBuffer(GetMemMsg, LMsgPtr, Length(GetMemMsg));
    boFreeMem: LMsgPtr := AppendStringToBuffer(FreeMemMsg, LMsgPtr, Length(FreeMemMsg));
    boReallocMem: LMsgPtr := AppendStringToBuffer(ReallocMemMsg, LMsgPtr, Length(ReallocMemMsg));
    boBlockCheck: LMsgPtr := AppendStringToBuffer(BlockCheckMsg, LMsgPtr, Length(BlockCheckMsg));
  end;
  LMsgPtr := AppendStringToBuffer(OperationMsg, LMsgPtr, Length(OperationMsg));
  {Is the header still intact?}
  if LHeaderValid then
  begin
    {Is the footer still valid?}
    if LFooterValid then
    begin
      {A freed block has been modified, a double free has occurred, or an
       attempt was made to free a memory block allocated by a different
       instance of FastMM.}
      if AOperation <= boGetMem then
      begin
        LMsgPtr := AppendStringToBuffer(FreeModifiedErrorMsg, LMsgPtr, Length(FreeModifiedErrorMsg));
        {Log the exact changes that caused the error.}
        LMsgPtr := LogBlockChanges(APointer, LMsgPtr);
      end
      else
      begin
        {It is either a double free, or an attempt was made to free a block
         that was allocated via a different memory manager.}
        if APointer.AllocatedByRoutine = nil then
          LMsgPtr := AppendStringToBuffer(DoubleFreeErrorMsg, LMsgPtr, Length(DoubleFreeErrorMsg))
        else
          LMsgPtr := AppendStringToBuffer(WrongMMFreeErrorMsg, LMsgPtr, Length(WrongMMFreeErrorMsg));
      end;
    end
    else
    begin
      LMsgPtr := AppendStringToBuffer(BlockFooterCorruptedMsg, LMsgPtr, Length(BlockFooterCorruptedMsg))
    end;
    {Set the block size message}
    if AOperation <= boGetMem then
      LMsgPtr := AppendStringToBuffer(PreviousBlockSizeMsg, LMsgPtr, Length(PreviousBlockSizeMsg))
    else
      LMsgPtr := AppendStringToBuffer(CurrentBlockSizeMsg, LMsgPtr, Length(CurrentBlockSizeMsg));
    LMsgPtr := NativeUIntToStrBuf(APointer.UserSize, LMsgPtr);
    {The header is still intact - display info about the this/previous allocation}
    if APointer.AllocationStackTrace[0] <> 0 then
    begin
      if AOperation <= boGetMem then
        LMsgPtr := AppendStringToBuffer(ThreadIDPrevAllocMsg, LMsgPtr, Length(ThreadIDPrevAllocMsg))
      else
        LMsgPtr := AppendStringToBuffer(ThreadIDAtAllocMsg, LMsgPtr, Length(ThreadIDAtAllocMsg));
      LMsgPtr := NativeUIntToHexBuf(APointer.AllocatedByThread, LMsgPtr);
      LMsgPtr := AppendStringToBuffer(StackTraceMsg, LMsgPtr, Length(StackTraceMsg));
      LMsgPtr := LogStackTrace(@APointer.AllocationStackTrace, StackTraceDepth, LMsgPtr);
    end;
    {Get the class this block was used for previously}
    LClass := DetectClassInstance(@APointer.PreviouslyUsedByClass);
    if (LClass <> nil) and (IntPtr(LClass) <> IntPtr(@FreedObjectVMT.VMTMethods[0])) then
    begin
      LMsgPtr := AppendStringToBuffer(PreviousObjectClassMsg, LMsgPtr, Length(PreviousObjectClassMsg));
      LMsgPtr := AppendClassNameToBuffer(LClass, LMsgPtr);
    end;
    {$ifdef CheckCppObjectTypeEnabled}
    if (LClass = nil) and Assigned(GetCppVirtObjTypeNameByVTablePtrFunc) then
    begin
      LCppObjectTypeName := GetCppVirtObjTypeNameByVTablePtrFunc(Pointer(APointer.PreviouslyUsedByClass), 0);
      if Assigned(LCppObjectTypeName) then
      begin
        LMsgPtr := AppendStringToBuffer(PreviousObjectClassMsg, LMsgPtr, Length(PreviousObjectClassMsg));
        LMsgPtr := AppendStringToBuffer(LCppObjectTypeName, LMsgPtr, StrLen(LCppObjectTypeName));
      end;
    end;
    {$endif}
    {Get the current class for this block}
    if (AOperation > boGetMem) and (APointer.AllocatedByRoutine <> nil) then
    begin
      LMsgPtr := AppendStringToBuffer(CurrentObjectClassMsg, LMsgPtr, Length(CurrentObjectClassMsg));
      LClass := DetectClassInstance(Pointer(PByte(APointer) + SizeOf(TFullDebugBlockHeader)));
      if IntPtr(LClass) = IntPtr(@FreedObjectVMT.VMTMethods[0]) then
        LClass := nil;
      {$ifndef CheckCppObjectTypeEnabled}
      LMsgPtr := AppendClassNameToBuffer(LClass, LMsgPtr);
      {$else}
      if (LClass = nil) and Assigned(GetCppVirtObjTypeNameFunc) then
      begin
        LCppObjectTypeName := GetCppVirtObjTypeNameFunc(Pointer(PByte(APointer) + SizeOf(TFullDebugBlockHeader)),
          APointer.UserSize);
        if LCppObjectTypeName <> nil then
          LMsgPtr := AppendStringToBuffer(LCppObjectTypeName, LMsgPtr, StrLen(LCppObjectTypeName))
        else
          LMsgPtr := AppendClassNameToBuffer(LClass, LMsgPtr);
      end
      else
      begin
        LMsgPtr := AppendClassNameToBuffer(LClass, LMsgPtr);
      end;
      {$endif}
      {Log the allocation group}
      if APointer.AllocationGroup > 0 then
      begin
        LMsgPtr := AppendStringToBuffer(CurrentAllocationGroupMsg, LMsgPtr, Length(CurrentAllocationGroupMsg));
        LMsgPtr := NativeUIntToStrBuf(APointer.AllocationGroup, LMsgPtr);
      end;
      {Log the allocation number}
      LMsgPtr := AppendStringToBuffer(CurrentAllocationNumberMsg, LMsgPtr, Length(CurrentAllocationNumberMsg));
      LMsgPtr := NativeUIntToStrBuf(APointer.AllocationNumber, LMsgPtr);
    end
    else
    begin
      {Log the allocation group}
      if APointer.AllocationGroup > 0 then
      begin
        LMsgPtr := AppendStringToBuffer(PreviousAllocationGroupMsg, LMsgPtr, Length(PreviousAllocationGroupMsg));
        LMsgPtr := NativeUIntToStrBuf(APointer.AllocationGroup, LMsgPtr);
      end;
      {Log the allocation number}
      LMsgPtr := AppendStringToBuffer(PreviousAllocationNumberMsg, LMsgPtr, Length(PreviousAllocationNumberMsg));
      LMsgPtr := NativeUIntToStrBuf(APointer.AllocationNumber, LMsgPtr);
    end;
    {Get the call stack for the previous free}
    if APointer.FreeStackTrace[0] <> 0 then
    begin
      LMsgPtr := AppendStringToBuffer(ThreadIDAtFreeMsg, LMsgPtr, Length(ThreadIDAtFreeMsg));
      LMsgPtr := NativeUIntToHexBuf(APointer.FreedByThread, LMsgPtr);
      LMsgPtr := AppendStringToBuffer(StackTraceMsg, LMsgPtr, Length(StackTraceMsg));
      LMsgPtr := LogStackTrace(@APointer.FreeStackTrace, StackTraceDepth, LMsgPtr);
    end;
  end
  else
  begin
    {Header has been corrupted}
    LMsgPtr := AppendStringToBuffer(BlockHeaderCorruptedMsg, LMsgPtr, Length(BlockHeaderCorruptedMsg));
  end;
  {Add the current stack trace}
  LMsgPtr := LogCurrentThreadAndStackTrace(3 + Ord(AOperation <> boGetMem) + Ord(AOperation = boReallocMem), LMsgPtr);
{$ifndef DisableLoggingOfMemoryDumps}
  {Add the memory dump}
  LMsgPtr := LogMemoryDump(APointer, LMsgPtr);
{$endif}
  {Trailing CRLF}
  LMsgPtr^ := #13;
  Inc(LMsgPtr);
  LMsgPtr^ := #10;
  Inc(LMsgPtr);
  {Trailing #0}
  LMsgPtr^ := #0;
{$ifdef LogErrorsToFile}
  {Log the error}
  AppendEventLog(@LErrorMessage[0], NativeUInt(LMsgPtr) - NativeUInt(@LErrorMessage[0]));
{$endif}
{$ifdef UseOutputDebugString}
  OutputDebugStringA(LErrorMessage);
{$endif}
  {Show the message}
{$ifndef NoMessageBoxes}
  AppendStringToModuleName(BlockErrorMsgTitle, LErrorMessageTitle);
  ShowMessageBox(LErrorMessage, LErrorMessageTitle);
{$endif}
end;

{Logs the stack traces for a memory leak to file}
procedure LogMemoryLeakOrAllocatedBlock(APointer: PFullDebugBlockHeader; IsALeak: Boolean);
var
  LHeaderValid: Boolean;
  LMsgPtr: PAnsiChar;
  LErrorMessage: array[0..32767] of AnsiChar;
  LClass: TClass;
  {$ifdef CheckCppObjectTypeEnabled}
  LCppObjectTypeName: PAnsiChar;
  {$endif}
begin
  {Display the error header and the operation type.}
  if IsALeak then
    LMsgPtr := AppendStringToBuffer(LeakLogHeader, @LErrorMessage[0], Length(LeakLogHeader))
  else
    LMsgPtr := AppendStringToBuffer(BlockScanLogHeader, @LErrorMessage[0], Length(BlockScanLogHeader));
  LMsgPtr := NativeUIntToStrBuf(GetAvailableSpaceInBlock(APointer) - FullDebugBlockOverhead, LMsgPtr);
  {Is the debug info surrounding the block valid?}
  LHeaderValid := CalculateHeaderCheckSum(APointer) = APointer.HeaderCheckSum;
  {Is the header still intact?}
  if LHeaderValid then
  begin
    {The header is still intact - display info about this/previous allocation}
    if APointer.AllocationStackTrace[0] <> 0 then
    begin
      LMsgPtr := AppendStringToBuffer(ThreadIDAtAllocMsg, LMsgPtr, Length(ThreadIDAtAllocMsg));
      LMsgPtr := NativeUIntToHexBuf(APointer.AllocatedByThread, LMsgPtr);
      LMsgPtr := AppendStringToBuffer(StackTraceMsg, LMsgPtr, Length(StackTraceMsg));
      LMsgPtr := LogStackTrace(@APointer.AllocationStackTrace, StackTraceDepth, LMsgPtr);
    end;
    LMsgPtr := AppendStringToBuffer(CurrentObjectClassMsg, LMsgPtr, Length(CurrentObjectClassMsg));
    {Get the current class for this block}
    LClass := DetectClassInstance(Pointer(PByte(APointer) + SizeOf(TFullDebugBlockHeader)));
    if IntPtr(LClass) = IntPtr(@FreedObjectVMT.VMTMethods[0]) then
      LClass := nil;
    {$ifndef CheckCppObjectTypeEnabled}
    if LClass <> nil then
    begin
      LMsgPtr := AppendClassNameToBuffer(LClass, LMsgPtr);
    end
    else
    begin
      case DetectStringData(Pointer(PByte(APointer) + SizeOf(TFullDebugBlockHeader)), APointer.UserSize) of
        stUnknown: LMsgPtr := AppendClassNameToBuffer(nil, LMsgPtr);
        stAnsiString: LMsgPtr := AppendStringToBuffer(AnsiStringBlockMessage, LMsgPtr, Length(AnsiStringBlockMessage));
        stUnicodeString: LMsgPtr := AppendStringToBuffer(UnicodeStringBlockMessage, LMsgPtr, Length(UnicodeStringBlockMessage));
      end;
    end;
    {$else}
    if (LClass = nil) and Assigned(GetCppVirtObjTypeNameFunc) then
    begin
      LCppObjectTypeName := GetCppVirtObjTypeNameFunc(Pointer(PByte(APointer) + SizeOf(TFullDebugBlockHeader)),
        APointer.UserSize);
      if LCppObjectTypeName <> nil then
        LMsgPtr := AppendStringToBuffer(LCppObjectTypeName, LMsgPtr, StrLen(LCppObjectTypeName))
      else
      begin
        case DetectStringData(Pointer(PByte(APointer) + SizeOf(TFullDebugBlockHeader)), APointer.UserSize) of
          stUnknown: LMsgPtr := AppendClassNameToBuffer(nil, LMsgPtr);
          stAnsiString: LMsgPtr := AppendStringToBuffer(AnsiStringBlockMessage, LMsgPtr, Length(AnsiStringBlockMessage));
          stUnicodeString: LMsgPtr := AppendStringToBuffer(UnicodeStringBlockMessage, LMsgPtr, Length(UnicodeStringBlockMessage));
        end;
      end;
    end
    else
      LMsgPtr := AppendClassNameToBuffer(LClass, LMsgPtr);
    {$endif}
    {Log the allocation group}
    if APointer.AllocationGroup > 0 then
    begin
      LMsgPtr := AppendStringToBuffer(CurrentAllocationGroupMsg, LMsgPtr, Length(CurrentAllocationGroupMsg));
      LMsgPtr := NativeUIntToStrBuf(APointer.AllocationGroup, LMsgPtr);
    end;
    {Log the allocation number}
    LMsgPtr := AppendStringToBuffer(CurrentAllocationNumberMsg, LMsgPtr, Length(CurrentAllocationNumberMsg));
    LMsgPtr := NativeUIntToStrBuf(APointer.AllocationNumber, LMsgPtr);
  end
  else
  begin
    {Header has been corrupted}
    LMsgPtr^ := '.';
    Inc(LMsgPtr);
    LMsgPtr^ := ' ';
    Inc(LMsgPtr);
    LMsgPtr := AppendStringToBuffer(BlockHeaderCorruptedMsg, LMsgPtr, Length(BlockHeaderCorruptedMsg));
  end;
{$ifndef DisableLoggingOfMemoryDumps}
  {Add the memory dump}
  LMsgPtr := LogMemoryDump(APointer, LMsgPtr);
{$endif}
  {Trailing CRLF}
  LMsgPtr^ := #13;
  Inc(LMsgPtr);
  LMsgPtr^ := #10;
  Inc(LMsgPtr);
  {Trailing #0}
  LMsgPtr^ := #0;
  {Log the error}
  AppendEventLog(@LErrorMessage[0], NativeUInt(LMsgPtr) - NativeUInt(@LErrorMessage[0]));
end;

{Checks that a free block is unmodified}
function CheckFreeBlockUnmodified(APBlock: PFullDebugBlockHeader; ABlockSize: NativeUInt;
  AOperation: TBlockOperation): Boolean;
var
  LHeaderCheckSum: NativeUInt;
  LHeaderValid, LFooterValid, LBlockUnmodified: Boolean;
begin
  LHeaderCheckSum := CalculateHeaderCheckSum(APBlock);
  LHeaderValid := LHeaderCheckSum = APBlock.HeaderCheckSum;
  {Is the footer itself still in place}
  LFooterValid := LHeaderValid
    and (PNativeUInt(PByte(APBlock) + SizeOf(TFullDebugBlockHeader) + APBlock.UserSize)^ = (not LHeaderCheckSum));
  {Is the footer and debug VMT in place? The debug VMT is only valid if the user size is greater than the size of a pointer.}
  if LFooterValid
    and (APBlock.UserSize < SizeOf(Pointer)) or (PNativeUInt(PByte(APBlock) + SizeOf(TFullDebugBlockHeader))^ = NativeUInt(@FreedObjectVMT.VMTMethods[0])) then
  begin
    {Store the debug fill pattern in place of the footer in order to simplify
     checking for block modifications.}
    PNativeUInt(PByte(APBlock) + SizeOf(TFullDebugBlockHeader) + APBlock.UserSize)^ :=
    {$ifndef CatchUseOfFreedInterfaces}
      DebugFillPattern;
    {$else}
      RotateRight(NativeUInt(@VMTBadInterface), (APBlock.UserSize and (SizeOf(Pointer) - 1)) * 8);
    {$endif}
    {Check that all the filler bytes are valid inside the block, except for
     the "dummy" class header}
    LBlockUnmodified := CheckFillPattern(PNativeUInt(PByte(APBlock) + (SizeOf(TFullDebugBlockHeader) + SizeOf(Pointer))),
      ABlockSize - (FullDebugBlockOverhead + SizeOf(Pointer)),
      {$ifndef CatchUseOfFreedInterfaces}DebugFillPattern{$else}NativeUInt(@VMTBadInterface){$endif});
    {Reset the old footer}
    PNativeUInt(PByte(APBlock) + SizeOf(TFullDebugBlockHeader) + APBlock.UserSize)^ := not LHeaderCheckSum;
  end
  else
    LBlockUnmodified := False;
  if (not LHeaderValid) or (not LFooterValid) or (not LBlockUnmodified) then
  begin
    LogBlockError(APBlock, AOperation, LHeaderValid, LFooterValid);
    Result := False;
  end
  else
    Result := True;
end;

function DebugGetMem(ASize: {$ifdef XE2AndUp}NativeInt{$else}Integer{$endif}): Pointer;
begin
  {Scan the entire memory pool first?}
  if FullDebugModeScanMemoryPoolBeforeEveryOperation then
    ScanMemoryPoolForCorruptions;
  {Enter the memory manager: block scans may not be performed now}
  StartChangingFullDebugModeBlock;
  try
    {We need extra space for (a) The debug header, (b) the block debug trailer
     and (c) the trailing block size pointer for free blocks}
    Result := FastGetMem(ASize + FullDebugBlockOverhead);
    if Result <> nil then
    begin
      {Large blocks are always newly allocated (and never reused), so checking
       for a modify-after-free is not necessary.}
      if (ASize > (MaximumMediumBlockSize - BlockHeaderSize - FullDebugBlockOverhead))
        or CheckFreeBlockUnmodified(Result, GetAvailableSpaceInBlock(Result) + BlockHeaderSize, boGetMem) then
      begin
        {Set the allocation call stack}
        GetStackTrace(@PFullDebugBlockHeader(Result).AllocationStackTrace, StackTraceDepth, 1);
        {Set the thread ID of the thread that allocated the block}
        PFullDebugBlockHeader(Result).AllocatedByThread := GetThreadID;
        {Block is now in use: It was allocated by this routine}
        PFullDebugBlockHeader(Result).AllocatedByRoutine := @DebugGetMem;
        {Set the group number}
        PFullDebugBlockHeader(Result).AllocationGroup := AllocationGroupStack[AllocationGroupStackTop];
        {Set the allocation number}
        IncrementAllocationNumber;
        PFullDebugBlockHeader(Result).AllocationNumber := CurrentAllocationNumber;
        {Clear the previous block trailer}
        PNativeUInt(PByte(Result) + SizeOf(TFullDebugBlockHeader) + PFullDebugBlockHeader(Result).UserSize)^ :=
        {$ifndef CatchUseOfFreedInterfaces}
          DebugFillPattern;
        {$else}
          RotateRight(NativeUInt(@VMTBadInterface), (PFullDebugBlockHeader(Result).UserSize and (SizeOf(Pointer) - 1)) * 8);
        {$endif}
        {Set the user size for the block}
        PFullDebugBlockHeader(Result).UserSize := ASize;
        {Set the checksums}
        UpdateHeaderAndFooterCheckSums(Result);
        {$ifdef FullDebugModeCallBacks}
        if Assigned(OnDebugGetMemFinish) then
          OnDebugGetMemFinish(PFullDebugBlockHeader(Result), ASize);
        {$endif}
        {Return the start of the actual block}
        Result := Pointer(PByte(Result) + SizeOf(TFullDebugBlockHeader));
{$ifdef EnableMemoryLeakReporting}
        {Should this block be marked as an expected leak automatically?}
        if FullDebugModeRegisterAllAllocsAsExpectedMemoryLeak then
          RegisterExpectedMemoryLeak(Result);
{$endif}
      end
      else
      begin
        Result := nil;
      end;
    end;
  finally
    {Leaving the memory manager routine: Block scans may be performed again.}
    DoneChangingFullDebugModeBlock;
  end;
end;

function CheckBlockBeforeFreeOrRealloc(APBlock: PFullDebugBlockHeader;
  AOperation: TBlockOperation): Boolean;
var
  LHeaderValid, LFooterValid: Boolean;
  LPFooter: PNativeUInt;
{$ifndef CatchUseOfFreedInterfaces}
  LBlockSize: NativeUInt;
  LPTrailingByte, LPFillPatternEnd: PByte;
{$endif}
begin
  {Is the checksum for the block header valid?}
  LHeaderValid := CalculateHeaderCheckSum(APBlock) = APBlock.HeaderCheckSum;
  {If the header is corrupted then the footer is assumed to be corrupt too.}
  if LHeaderValid then
  begin
    {Check the footer checksum: The footer checksum should equal the header
     checksum with all bits inverted.}
    LPFooter := PNativeUInt(PByte(APBlock) + SizeOf(TFullDebugBlockHeader) + PFullDebugBlockHeader(APBlock).UserSize);
    if APBlock.HeaderCheckSum = (not (LPFooter^)) then
    begin
      LFooterValid := True;
{$ifndef CatchUseOfFreedInterfaces}
      {Large blocks do not have the debug fill pattern, since they are never reused.}
      if PNativeUInt(PByte(APBlock) - BlockHeaderSize)^ and (IsMediumBlockFlag or IsLargeBlockFlag) <> IsLargeBlockFlag then
      begin
        {Check that the application has not modified bytes beyond the block
         footer. The $80 fill pattern should extend up to 2 nativeints before
         the start of the next block (leaving space for the free block size and
         next block header.)}
        LBlockSize := GetAvailableSpaceInBlock(APBlock);
        LPFillPatternEnd := PByte(PByte(APBlock) + LBlockSize - SizeOf(Pointer));
        LPTrailingByte := PByte(PByte(LPFooter) + SizeOf(NativeUInt));
        while UIntPtr(LPTrailingByte) < UIntPtr(LPFillPatternEnd) do
        begin
          if Byte(LPTrailingByte^) <> DebugFillByte then
          begin
            LFooterValid := False;
            Break;
          end;
          Inc(LPTrailingByte);
        end;
      end;
{$endif}
    end
    else
      LFooterValid := False;
  end
  else
    LFooterValid := False;
  {The header and footer must be intact and the block must have been allocated
   by this memory manager instance.}
  if LFooterValid and (APBlock.AllocatedByRoutine = @DebugGetMem) then
  begin
    Result := True;
  end
  else
  begin
    {Log the error}
    LogBlockError(APBlock, AOperation, LHeaderValid, LFooterValid);
    {Return an error}
    Result := False;
  end;
end;

function DebugFreeMem(APointer: Pointer): Integer;
var
  LActualBlock: PFullDebugBlockHeader;
  LBlockHeader: NativeUInt;
begin
  {Scan the entire memory pool first?}
  if FullDebugModeScanMemoryPoolBeforeEveryOperation then
    ScanMemoryPoolForCorruptions;
  {Get a pointer to the start of the actual block}
  LActualBlock := PFullDebugBlockHeader(PByte(APointer)
    - SizeOf(TFullDebugBlockHeader));
  {Is the debug info surrounding the block valid?}
  if CheckBlockBeforeFreeOrRealloc(LActualBlock, boFreeMem) then
  begin
    {Enter the memory manager: block scans may not be performed now}
    StartChangingFullDebugModeBlock;
    try
      {$ifdef FullDebugModeCallBacks}
      if Assigned(OnDebugFreeMemStart) then
        OnDebugFreeMemStart(LActualBlock);
      {$endif}
      {Large blocks are never reused, so there is no point in updating their
       headers and fill pattern.}
      LBlockHeader := PNativeUInt(PByte(LActualBlock) - BlockHeaderSize)^;
      if LBlockHeader and (IsFreeBlockFlag or IsMediumBlockFlag or IsLargeBlockFlag) <> IsLargeBlockFlag then
      begin
        {Get the class the block was used for}
        LActualBlock.PreviouslyUsedByClass := PNativeUInt(APointer)^;
        {Set the free call stack}
        GetStackTrace(@LActualBlock.FreeStackTrace, StackTraceDepth, 1);
        {Set the thread ID of the thread that freed the block}
        LActualBlock.FreedByThread := GetThreadID;
        {Block is now free}
        LActualBlock.AllocatedByRoutine := nil;
        {Clear the user area of the block}
        DebugFillMem(APointer^, LActualBlock.UserSize,
          {$ifndef CatchUseOfFreedInterfaces}DebugFillPattern{$else}NativeUInt(@VMTBadInterface){$endif});
        {Set a pointer to the dummy VMT}
        PNativeUInt(APointer)^ := NativeUInt(@FreedObjectVMT.VMTMethods[0]);
        {Recalculate the checksums}
        UpdateHeaderAndFooterCheckSums(LActualBlock);
      end;
{$ifdef EnableMemoryLeakReporting}
      {Automatically deregister the expected memory leak?}
      if FullDebugModeRegisterAllAllocsAsExpectedMemoryLeak then
        UnregisterExpectedMemoryLeak(APointer);
{$endif}
      {Free the actual block}
      Result := FastFreeMem(LActualBlock);
      {$ifdef FullDebugModeCallBacks}
      if Assigned(OnDebugFreeMemFinish) then
        OnDebugFreeMemFinish(LActualBlock, Result);
      {$endif}
    finally
      {Leaving the memory manager routine: Block scans may be performed again.}
      DoneChangingFullDebugModeBlock;
    end;
  end
  else
  begin
{$ifdef SuppressFreeMemErrorsInsideException}
    if {$ifdef BDS2006AndUp}ExceptObject{$else}RaiseList{$endif} <> nil then
      Result := 0
    else
{$endif}
      Result := -1;
  end;
end;

function DebugReallocMem(APointer: Pointer; ANewSize: {$ifdef XE2AndUp}NativeInt{$else}Integer{$endif}): Pointer;
var
  LMoveSize, LBlockSpace: NativeUInt;
  LActualBlock, LNewActualBlock: PFullDebugBlockHeader;
begin
  {Scan the entire memory pool first?}
  if FullDebugModeScanMemoryPoolBeforeEveryOperation then
    ScanMemoryPoolForCorruptions;
  {Get a pointer to the start of the actual block}
  LActualBlock := PFullDebugBlockHeader(PByte(APointer)
    - SizeOf(TFullDebugBlockHeader));
  {Is the debug info surrounding the block valid?}
  if CheckBlockBeforeFreeOrRealloc(LActualBlock, boReallocMem) then
  begin
    {Get the current block size}
    LBlockSpace := GetAvailableSpaceInBlock(LActualBlock);
    {Can the block fit? We need space for the debug overhead and the block header
     of the next block}
    if LBlockSpace < (NativeUInt(ANewSize) + FullDebugBlockOverhead) then
    begin
      {Get a new block of the requested size.}
      Result := DebugGetMem(ANewSize);
      if Result <> nil then
      begin
        {Block scans may not be performed now}
        StartChangingFullDebugModeBlock;
        try
          {$ifdef FullDebugModeCallBacks}
          if Assigned(OnDebugReallocMemStart) then
            OnDebugReallocMemStart(LActualBlock, ANewSize);
          {$endif}
          {We reuse the old allocation number. Since DebugGetMem always bumps
           CurrentAllocationGroup, there may be gaps in the sequence of
           allocation numbers.}
          LNewActualBlock := PFullDebugBlockHeader(PByte(Result)
            - SizeOf(TFullDebugBlockHeader));
          LNewActualBlock.AllocationGroup := LActualBlock.AllocationGroup;
          LNewActualBlock.AllocationNumber := LActualBlock.AllocationNumber;
          {Recalculate the header and footer checksums}
          UpdateHeaderAndFooterCheckSums(LNewActualBlock);
          {$ifdef FullDebugModeCallBacks}
          if Assigned(OnDebugReallocMemFinish) then
            OnDebugReallocMemFinish(LNewActualBlock, ANewSize);
          {$endif}
        finally
          {Block scans can again be performed safely}
          DoneChangingFullDebugModeBlock;
        end;
        {How many bytes to move?}
        LMoveSize := LActualBlock.UserSize;
        if LMoveSize > NativeUInt(ANewSize) then
          LMoveSize := ANewSize;
        {Move the data across}
        System.Move(APointer^, Result^, LMoveSize);
        {Free the old block}
        DebugFreeMem(APointer);
      end
      else
      begin
        Result := nil;
      end;
    end
    else
    begin
      {Block scans may not be performed now}
      StartChangingFullDebugModeBlock;
      try
        {$ifdef FullDebugModeCallBacks}
        if Assigned(OnDebugReallocMemStart) then
          OnDebugReallocMemStart(LActualBlock, ANewSize);
        {$endif}
        {Clear all data after the new end of the block up to the old end of the
         block, including the trailer.}
        DebugFillMem(Pointer(PByte(APointer) + NativeUInt(ANewSize) + SizeOf(NativeUInt))^,
          NativeInt(LActualBlock.UserSize) - ANewSize,
{$ifndef CatchUseOfFreedInterfaces}
          DebugFillPattern);
{$else}
          RotateRight(NativeUInt(@VMTBadInterface), (ANewSize and (SizeOf(Pointer) - 1)) * 8));
{$endif}
        {Update the user size}
        LActualBlock.UserSize := ANewSize;
        {Set the new checksums}
        UpdateHeaderAndFooterCheckSums(LActualBlock);
        {$ifdef FullDebugModeCallBacks}
        if Assigned(OnDebugReallocMemFinish) then
          OnDebugReallocMemFinish(LActualBlock, ANewSize);
        {$endif}
      finally
        {Block scans can again be performed safely}
        DoneChangingFullDebugModeBlock;
      end;
      {Return the old pointer}
      Result := APointer;
    end;
  end
  else
  begin
    Result := nil;
  end;
end;

{Allocates a block and fills it with zeroes}
function DebugAllocMem(ASize: {$ifdef XE2AndUp}NativeInt{$else}Cardinal{$endif}): Pointer;
begin
  Result := DebugGetMem(ASize);
  {Clear the block}
  if Result <>  nil then
    FillChar(Result^, ASize, 0);
end;

{Raises a runtime error if a memory corruption was encountered. Subroutine for
 InternalScanMemoryPool and InternalScanSmallBlockPool.}
procedure RaiseMemoryCorruptionError;
begin
  {Disable exhaustive checking in order to prevent recursive exceptions.}
  FullDebugModeScanMemoryPoolBeforeEveryOperation := False;
  {Unblock the memory manager in case the creation of the exception below
   causes an attempt to be made to allocate memory.}
  UnblockFullDebugModeMMRoutines;
  {Raise the runtime error}
{$ifdef BCB6OrDelphi7AndUp}
  System.Error(reOutOfMemory);
{$else}
  System.RunError(reOutOfMemory);
{$endif}
end;

{Subroutine for InternalScanMemoryPool: Checks the given small block pool for
 allocated blocks}
procedure InternalScanSmallBlockPool(APSmallBlockPool: PSmallBlockPoolHeader;
  AFirstAllocationGroupToLog, ALastAllocationGroupToLog: Cardinal);
var
  LCurPtr, LEndPtr: Pointer;
begin
  {Get the first and last pointer for the pool}
  GetFirstAndLastSmallBlockInPool(APSmallBlockPool, LCurPtr, LEndPtr);
  {Step through all blocks}
  while UIntPtr(LCurPtr) <= UIntPtr(LEndPtr) do
  begin
    {Is this block in use? If so, is the debug info intact?}
    if ((PNativeUInt(PByte(LCurPtr) - BlockHeaderSize)^ and IsFreeBlockFlag) = 0) then
    begin
      if CheckBlockBeforeFreeOrRealloc(LCurPtr, boBlockCheck) then
      begin
        if (PFullDebugBlockHeader(LCurPtr).AllocationGroup >= AFirstAllocationGroupToLog)
          and (PFullDebugBlockHeader(LCurPtr).AllocationGroup <= ALastAllocationGroupToLog) then
        begin
          LogMemoryLeakOrAllocatedBlock(LCurPtr, False);
        end;
      end
      else
        RaiseMemoryCorruptionError;
    end
    else
    begin
      {Check that the block has not been modified since being freed}
      if not CheckFreeBlockUnmodified(LCurPtr, APSmallBlockPool.BlockType.BlockSize, boBlockCheck) then
        RaiseMemoryCorruptionError;
    end;
    {Next block}
    Inc(PByte(LCurPtr), APSmallBlockPool.BlockType.BlockSize);
  end;
end;

{Subroutine for LogAllocatedBlocksToFile and ScanMemoryPoolForCorruptions:
 Scans the memory pool for corruptions and optionally logs allocated blocks
 in the allocation group range.}
procedure InternalScanMemoryPool(AFirstAllocationGroupToLog, ALastAllocationGroupToLog: Cardinal);
var
  LPLargeBlock: PLargeBlockHeader;
  LPMediumBlock: Pointer;
  LPMediumBlockPoolHeader: PMediumBlockPoolHeader;
  LMediumBlockHeader: NativeUInt;
begin
  {Block all the memory manager routines while performing the scan. No memory
   block may be allocated or freed, and no FullDebugMode block header or
   footer may be modified, while the scan is in progress.}
  BlockFullDebugModeMMRoutines;
  try
    {Step through all the medium block pools}
    LPMediumBlockPoolHeader := MediumBlockPoolsCircularList.NextMediumBlockPoolHeader;
    while LPMediumBlockPoolHeader <> @MediumBlockPoolsCircularList do
    begin
      LPMediumBlock := GetFirstMediumBlockInPool(LPMediumBlockPoolHeader);
      while LPMediumBlock <> nil do
      begin
        LMediumBlockHeader := PNativeUInt(PByte(LPMediumBlock) - BlockHeaderSize)^;
        {Is the block in use?}
        if LMediumBlockHeader and IsFreeBlockFlag = 0 then
        begin
          {Block is in use: Is it a medium block or small block pool?}
          if (LMediumBlockHeader and IsSmallBlockPoolInUseFlag) <> 0 then
          begin
            {Get all the leaks for the small block pool}
            InternalScanSmallBlockPool(LPMediumBlock, AFirstAllocationGroupToLog, ALastAllocationGroupToLog);
          end
          else
          begin
            if CheckBlockBeforeFreeOrRealloc(LPMediumBlock, boBlockCheck) then
            begin
              if (PFullDebugBlockHeader(LPMediumBlock).AllocationGroup >= AFirstAllocationGroupToLog)
                and (PFullDebugBlockHeader(LPMediumBlock).AllocationGroup <= ALastAllocationGroupToLog) then
              begin
                LogMemoryLeakOrAllocatedBlock(LPMediumBlock, False);
              end;
            end
            else
              RaiseMemoryCorruptionError;
          end;
        end
        else
        begin
          {Check that the block has not been modified since being freed}
          if not CheckFreeBlockUnmodified(LPMediumBlock, LMediumBlockHeader and DropMediumAndLargeFlagsMask, boBlockCheck) then
            RaiseMemoryCorruptionError;
        end;
        {Next medium block}
        LPMediumBlock := NextMediumBlock(LPMediumBlock);
      end;
      {Get the next medium block pool}
      LPMediumBlockPoolHeader := LPMediumBlockPoolHeader.NextMediumBlockPoolHeader;
    end;
    {Scan large blocks}
    LPLargeBlock := LargeBlocksCircularList.NextLargeBlockHeader;
    while LPLargeBlock <> @LargeBlocksCircularList do
    begin
      if CheckBlockBeforeFreeOrRealloc(Pointer(PByte(LPLargeBlock) + LargeBlockHeaderSize), boBlockCheck) then
      begin
        if (PFullDebugBlockHeader(PByte(LPLargeBlock) + LargeBlockHeaderSize).AllocationGroup >= AFirstAllocationGroupToLog)
          and (PFullDebugBlockHeader(PByte(LPLargeBlock) + LargeBlockHeaderSize).AllocationGroup <= ALastAllocationGroupToLog) then
        begin
          LogMemoryLeakOrAllocatedBlock(Pointer(PByte(LPLargeBlock) + LargeBlockHeaderSize), False);
        end;
      end
      else
        RaiseMemoryCorruptionError;
      {Get the next large block}
      LPLargeBlock := LPLargeBlock.NextLargeBlockHeader;
    end;
  finally
    {Unblock the FullDebugMode memory manager routines.}
    UnblockFullDebugModeMMRoutines;
  end;
end;

{Logs detail about currently allocated memory blocks for the specified range of
 allocation groups. if ALastAllocationGroupToLog is less than
 AFirstAllocationGroupToLog or it is zero, then all allocation groups are
 logged. This routine also checks the memory pool for consistency at the same
 time, raising an "Out of Memory" error if the check fails.}
procedure LogAllocatedBlocksToFile(AFirstAllocationGroupToLog, ALastAllocationGroupToLog: Cardinal);
begin
  {Validate input}
  if (ALastAllocationGroupToLog = 0) or (ALastAllocationGroupToLog < AFirstAllocationGroupToLog) then
  begin
    {Bad input: log all groups}
    AFirstAllocationGroupToLog := 0;
    ALastAllocationGroupToLog := $ffffffff;
  end;
  {Scan the memory pool, logging allocated blocks in the requested range.}
  InternalScanMemoryPool(AFirstAllocationGroupToLog, ALastAllocationGroupToLog);
end;

{Scans the memory pool for any corruptions. If a corruption is encountered an "Out of Memory" exception is
 raised.}
procedure ScanMemoryPoolForCorruptions;
begin
  {Scan the memory pool for corruptions, but don't log any allocated blocks}
  InternalScanMemoryPool($ffffffff, 0);
end;

{-----------------------Invalid Virtual Method Calls-------------------------}

{ TFreedObject }

{Used to determine the index of the virtual method call on the freed object.
 Do not change this without updating MaxFakeVMTEntries. Currently 200.}
procedure TFreedObject.GetVirtualMethodIndex;
asm
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);

  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);

  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);

  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);

  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);

  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);

  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);

  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);

  jmp TFreedObject.VirtualMethodError
end;

procedure TFreedObject.VirtualMethodError;
var
  LVMOffset: Integer;
  LMsgPtr: PAnsiChar;
  LErrorMessage: array[0..32767] of AnsiChar;
{$ifndef NoMessageBoxes}
  LErrorMessageTitle: array[0..1023] of AnsiChar;
{$endif}
  LClass: TClass;
  LActualBlock: PFullDebugBlockHeader;
begin
  {Get the offset of the virtual method}
  LVMOffset := (MaxFakeVMTEntries - VMIndex) * SizeOf(Pointer) + vmtParent + SizeOf(Pointer);
  {Reset the index for the next error}
  VMIndex := 0;
  {Get the address of the actual block}
  LActualBlock := PFullDebugBlockHeader(PByte(Self) - SizeOf(TFullDebugBlockHeader));
  {Display the error header}
  LMsgPtr := AppendStringToBuffer(VirtualMethodErrorHeader, @LErrorMessage[0], Length(VirtualMethodErrorHeader));
  {Is the debug info surrounding the block valid?}
  if CalculateHeaderCheckSum(LActualBlock) = LActualBlock.HeaderCheckSum then
  begin
    {Get the class this block was used for previously}
    LClass := DetectClassInstance(@LActualBlock.PreviouslyUsedByClass);
    if (LClass <> nil) and (IntPtr(LClass) <> IntPtr(@FreedObjectVMT.VMTMethods[0])) then
    begin
      LMsgPtr := AppendStringToBuffer(FreedObjectClassMsg, LMsgPtr, Length(FreedObjectClassMsg));
      LMsgPtr := AppendClassNameToBuffer(LClass, LMsgPtr);
    end;
    {Get the virtual method name}
    LMsgPtr := AppendStringToBuffer(VirtualMethodName, LMsgPtr, Length(VirtualMethodName));
    if LVMOffset < 0 then
    begin
      LMsgPtr := AppendStringToBuffer(StandardVirtualMethodNames[LVMOffset div SizeOf(Pointer)], LMsgPtr, Length(StandardVirtualMethodNames[LVMOffset div SizeOf(Pointer)]));
    end
    else
    begin
      LMsgPtr := AppendStringToBuffer(VirtualMethodOffset, LMsgPtr, Length(VirtualMethodOffset));
      LMsgPtr := NativeUIntToStrBuf(LVMOffset, LMsgPtr);
    end;
    {Virtual method address}
    if (LClass <> nil) and (IntPtr(LClass) <> IntPtr(@FreedObjectVMT.VMTMethods[0])) then
    begin
      LMsgPtr := AppendStringToBuffer(VirtualMethodAddress, LMsgPtr, Length(VirtualMethodAddress));
      LMsgPtr := NativeUIntToHexBuf(PNativeUInt(PByte(LClass) + LVMOffset)^, LMsgPtr);
    end;
    {Log the allocation group}
    if LActualBlock.AllocationGroup > 0 then
    begin
      LMsgPtr := AppendStringToBuffer(PreviousAllocationGroupMsg, LMsgPtr, Length(PreviousAllocationGroupMsg));
      LMsgPtr := NativeUIntToStrBuf(LActualBlock.AllocationGroup, LMsgPtr);
    end;
    {Log the allocation number}
    LMsgPtr := AppendStringToBuffer(PreviousAllocationNumberMsg, LMsgPtr, Length(PreviousAllocationNumberMsg));
    LMsgPtr := NativeUIntToStrBuf(LActualBlock.AllocationNumber, LMsgPtr);
    {The header is still intact - display info about the this/previous allocation}
    if LActualBlock.AllocationStackTrace[0] <> 0 then
    begin
      LMsgPtr := AppendStringToBuffer(ThreadIDAtObjectAllocMsg, LMsgPtr, Length(ThreadIDAtObjectAllocMsg));
      LMsgPtr := NativeUIntToHexBuf(LActualBlock.AllocatedByThread, LMsgPtr);
      LMsgPtr := AppendStringToBuffer(StackTraceMsg, LMsgPtr, Length(StackTraceMsg));
      LMsgPtr := LogStackTrace(@LActualBlock.AllocationStackTrace, StackTraceDepth, LMsgPtr);
    end;
    {Get the call stack for the previous free}
    if LActualBlock.FreeStackTrace[0] <> 0 then
    begin
      LMsgPtr := AppendStringToBuffer(ThreadIDAtObjectFreeMsg, LMsgPtr, Length(ThreadIDAtObjectFreeMsg));
      LMsgPtr := NativeUIntToHexBuf(LActualBlock.FreedByThread, LMsgPtr);
      LMsgPtr := AppendStringToBuffer(StackTraceMsg, LMsgPtr, Length(StackTraceMsg));
      LMsgPtr := LogStackTrace(@LActualBlock.FreeStackTrace, StackTraceDepth, LMsgPtr);
    end;
  end
  else
  begin
    {Header has been corrupted}
    LMsgPtr := AppendStringToBuffer(BlockHeaderCorruptedNoHistoryMsg, LMsgPtr, Length(BlockHeaderCorruptedNoHistoryMsg));
  end;
  {Add the current stack trace}
  LMsgPtr := LogCurrentThreadAndStackTrace(2, LMsgPtr);
{$ifndef DisableLoggingOfMemoryDumps}
  {Add the pointer address}
  LMsgPtr := LogMemoryDump(LActualBlock, LMsgPtr);
{$endif}
  {Trailing CRLF}
  LMsgPtr^ := #13;
  Inc(LMsgPtr);
  LMsgPtr^ := #10;
  Inc(LMsgPtr);
  {Trailing #0}
  LMsgPtr^ := #0;
{$ifdef LogErrorsToFile}
  {Log the error}
  AppendEventLog(@LErrorMessage[0], NativeUInt(LMsgPtr) - NativeUInt(@LErrorMessage[0]));
{$endif}
{$ifdef UseOutputDebugString}
  OutputDebugStringA(LErrorMessage);
{$endif}
{$ifndef NoMessageBoxes}
  {Show the message}
  AppendStringToModuleName(BlockErrorMsgTitle, LErrorMessageTitle);
  ShowMessageBox(LErrorMessage, LErrorMessageTitle);
{$endif}
  {Raise an access violation}
  RaiseException(EXCEPTION_ACCESS_VIOLATION, 0, 0, nil);
end;

{$ifdef CatchUseOfFreedInterfaces}
procedure TFreedObject.InterfaceError;
var
  LMsgPtr: PAnsiChar;
{$ifndef NoMessageBoxes}
  LErrorMessageTitle: array[0..1023] of AnsiChar;
{$endif}
  LErrorMessage: array[0..4000] of AnsiChar;
begin
  {Display the error header}
  LMsgPtr := AppendStringToBuffer(InterfaceErrorHeader, @LErrorMessage[0], Length(InterfaceErrorHeader));
  {Add the current stack trace}
  LMsgPtr := LogCurrentThreadAndStackTrace(2, LMsgPtr);
  {Trailing CRLF}
  LMsgPtr^ := #13;
  Inc(LMsgPtr);
  LMsgPtr^ := #10;
  Inc(LMsgPtr);
  {Trailing #0}
  LMsgPtr^ := #0;
{$ifdef LogErrorsToFile}
  {Log the error}
  AppendEventLog(@LErrorMessage[0], NativeUInt(LMsgPtr) - NativeUInt(@LErrorMessage[0]));
{$endif}
{$ifdef UseOutputDebugString}
  OutputDebugStringA(LErrorMessage);
{$endif}
{$ifndef NoMessageBoxes}
  {Show the message}
  AppendStringToModuleName(BlockErrorMsgTitle, LErrorMessageTitle);
  ShowMessageBox(LErrorMessage, LErrorMessageTitle);
{$endif}
  {Raise an access violation}
  RaiseException(EXCEPTION_ACCESS_VIOLATION, 0, 0, nil);
end;
{$endif}

{$endif}

{----------------------------Memory Leak Checking-----------------------------}

{$ifdef EnableMemoryLeakReporting}

{Adds a leak to the specified list}
function UpdateExpectedLeakList(APLeakList: PPExpectedMemoryLeak;
  APNewEntry: PExpectedMemoryLeak; AExactSizeMatch: Boolean = True): Boolean;
var
  LPInsertAfter, LPNewEntry: PExpectedMemoryLeak;
begin
  {Default to error}
  Result := False;
  {Find the insertion spot}
  LPInsertAfter := APLeakList^;
  while LPInsertAfter <> nil do
  begin
    {Too big?}
    if LPInsertAfter.LeakSize > APNewEntry.LeakSize then
    begin
      LPInsertAfter := LPInsertAfter.PreviousLeak;
      Break;
    end;
    {Find a matching entry. If an exact size match is not required and the leak
     is larger than the current entry, use it if the expected size of the next
     entry is too large.}
    if (IntPtr(LPInsertAfter.LeakAddress) = IntPtr(APNewEntry.LeakAddress))
      and ((IntPtr(LPInsertAfter.LeakedClass) = IntPtr(APNewEntry.LeakedClass))
      {$ifdef CheckCppObjectTypeEnabled}
       or (LPInsertAfter.LeakedCppTypeIdPtr = APNewEntry.LeakedCppTypeIdPtr)
      {$endif}
      )
      and ((LPInsertAfter.LeakSize = APNewEntry.LeakSize)
        or ((not AExactSizeMatch)
          and (LPInsertAfter.LeakSize < APNewEntry.LeakSize)
          and ((LPInsertAfter.NextLeak = nil)
            or (LPInsertAfter.NextLeak.LeakSize > APNewEntry.LeakSize))
          )) then
    begin
      if (LPInsertAfter.LeakCount + APNewEntry.LeakCount) >= 0 then
      begin
        Inc(LPInsertAfter.LeakCount, APNewEntry.LeakCount);
        {Is the count now 0?}
        if LPInsertAfter.LeakCount = 0 then
        begin
          {Delete the entry}
          if LPInsertAfter.NextLeak <> nil then
            LPInsertAfter.NextLeak.PreviousLeak := LPInsertAfter.PreviousLeak;
          if LPInsertAfter.PreviousLeak <> nil then
            LPInsertAfter.PreviousLeak.NextLeak := LPInsertAfter.NextLeak
          else
            APLeakList^ := LPInsertAfter.NextLeak;
          {Insert it as the first free slot}
          LPInsertAfter.NextLeak := ExpectedMemoryLeaks.FirstFreeSlot;
          ExpectedMemoryLeaks.FirstFreeSlot := LPInsertAfter;
        end;
        Result := True;
      end;
      Exit;
    end;
    {Next entry}
    if LPInsertAfter.NextLeak <> nil then
      LPInsertAfter := LPInsertAfter.NextLeak
    else
      Break;
  end;
  if APNewEntry.LeakCount > 0 then
  begin
    {Get a position for the entry}
    LPNewEntry := ExpectedMemoryLeaks.FirstFreeSlot;
    if LPNewEntry <> nil then
    begin
      ExpectedMemoryLeaks.FirstFreeSlot := LPNewEntry.NextLeak;
    end
    else
    begin
      if ExpectedMemoryLeaks.EntriesUsed < Length(ExpectedMemoryLeaks.ExpectedLeaks) then
      begin
        LPNewEntry := @ExpectedMemoryLeaks.ExpectedLeaks[ExpectedMemoryLeaks.EntriesUsed];
        Inc(ExpectedMemoryLeaks.EntriesUsed);
      end
      else
      begin
        {No more space}
        Exit;
      end;
    end;
    {Set the entry}
    LPNewEntry^ := APNewEntry^;
    {Insert it into the list}
    LPNewEntry.PreviousLeak := LPInsertAfter;
    if LPInsertAfter <> nil then
    begin
      LPNewEntry.NextLeak := LPInsertAfter.NextLeak;
      if LPNewEntry.NextLeak <> nil then
        LPNewEntry.NextLeak.PreviousLeak := LPNewEntry;
      LPInsertAfter.NextLeak := LPNewEntry;
    end
    else
    begin
      LPNewEntry.NextLeak := APLeakList^;
      if LPNewEntry.NextLeak <> nil then
        LPNewEntry.NextLeak.PreviousLeak := LPNewEntry;
      APLeakList^ := LPNewEntry;
    end;
    Result := True;
  end;
end;

{Locks the expected leaks. Returns false if the list could not be allocated.}
function LockExpectedMemoryLeaksList: Boolean;
begin
  {Lock the expected leaks list}
{$ifndef AssumeMultiThreaded}
  if IsMultiThread then
{$endif}
  begin
    while LockCmpxchg(0, 1, @ExpectedMemoryLeaksListLocked) <> 0 do
    begin
{$ifdef NeverSleepOnThreadContention}
  {$ifdef UseSwitchToThread}
      SwitchToThread;
  {$endif}
{$else}
      Sleep(InitialSleepTime);
      if LockCmpxchg(0, 1, @ExpectedMemoryLeaksListLocked) = 0 then
        Break;
      Sleep(AdditionalSleepTime);
{$endif}
    end;
  end;
  {Allocate the list if it does not exist}
  if ExpectedMemoryLeaks = nil then
    ExpectedMemoryLeaks := VirtualAlloc(nil, ExpectedMemoryLeaksListSize, MEM_COMMIT, PAGE_READWRITE);
  {Done}
  Result := ExpectedMemoryLeaks <> nil;
end;

{Registers expected memory leaks. Returns true on success. The list of leaked
 blocks is limited, so failure is possible if the list is full.}
function RegisterExpectedMemoryLeak(ALeakedPointer: Pointer): Boolean; overload;
var
  LNewEntry: TExpectedMemoryLeak;
begin
  {Fill out the structure}
{$ifndef FullDebugMode}
  LNewEntry.LeakAddress := ALeakedPointer;
{$else}
  LNewEntry.LeakAddress := Pointer(PByte(ALeakedPointer) - SizeOf(TFullDebugBlockHeader));
{$endif}
  LNewEntry.LeakedClass := nil;
  {$ifdef CheckCppObjectTypeEnabled}
  LNewEntry.LeakedCppTypeIdPtr := nil;
  {$endif}
  LNewEntry.LeakSize := 0;
  LNewEntry.LeakCount := 1;
  {Add it to the correct list}
  Result := LockExpectedMemoryLeaksList
    and UpdateExpectedLeakList(@ExpectedMemoryLeaks.FirstEntryByAddress, @LNewEntry);
  ExpectedMemoryLeaksListLocked := False;
end;

function RegisterExpectedMemoryLeak(ALeakedObjectClass: TClass; ACount: Integer = 1): Boolean; overload;
var
  LNewEntry: TExpectedMemoryLeak;
begin
  {Fill out the structure}
  LNewEntry.LeakAddress := nil;
  LNewEntry.LeakedClass := ALeakedObjectClass;
  {$ifdef CheckCppObjectTypeEnabled}
  LNewEntry.LeakedCppTypeIdPtr := nil;
  {$endif}
  LNewEntry.LeakSize := ALeakedObjectClass.InstanceSize;
  LNewEntry.LeakCount := ACount;
  {Add it to the correct list}
  Result := LockExpectedMemoryLeaksList
    and UpdateExpectedLeakList(@ExpectedMemoryLeaks.FirstEntryByClass, @LNewEntry);
  ExpectedMemoryLeaksListLocked := False;
end;

{$ifdef CheckCppObjectTypeEnabled}
function RegisterExpectedMemoryLeak(ALeakedCppVirtObjTypeIdPtr: Pointer; ACount: Integer): Boolean; overload;
var
  LNewEntry: TExpectedMemoryLeak;
begin
  {Fill out the structure}
  if Assigned(GetCppVirtObjSizeByTypeIdPtrFunc) then
  begin
    //Return 0 if not a proper type
    LNewEntry.LeakSize := GetCppVirtObjSizeByTypeIdPtrFunc(ALeakedCppVirtObjTypeIdPtr);
    if LNewEntry.LeakSize > 0 then
    begin
      LNewEntry.LeakAddress := nil;
      LNewEntry.LeakedClass := nil;
      LNewEntry.LeakedCppTypeIdPtr := ALeakedCppVirtObjTypeIdPtr;
      LNewEntry.LeakCount := ACount;
      {Add it to the correct list}
      Result := LockExpectedMemoryLeaksList
        and UpdateExpectedLeakList(@ExpectedMemoryLeaks.FirstEntryByClass, @LNewEntry);
      ExpectedMemoryLeaksListLocked := False;
    end
    else
    begin
      Result := False;
    end;
  end
  else
  begin
    Result := False;
  end;
end;
{$endif}

function RegisterExpectedMemoryLeak(ALeakedBlockSize: NativeInt; ACount: Integer = 1): Boolean; overload;
var
  LNewEntry: TExpectedMemoryLeak;
begin
  {Fill out the structure}
  LNewEntry.LeakAddress := nil;
  LNewEntry.LeakedClass := nil;
  {$ifdef CheckCppObjectTypeEnabled}
  LNewEntry.LeakedCppTypeIdPtr := nil;
  {$endif}
  LNewEntry.LeakSize := ALeakedBlockSize;
  LNewEntry.LeakCount := ACount;
  {Add it to the correct list}
  Result := LockExpectedMemoryLeaksList
    and UpdateExpectedLeakList(@ExpectedMemoryLeaks.FirstEntryBySizeOnly, @LNewEntry);
  ExpectedMemoryLeaksListLocked := False;
end;

function UnregisterExpectedMemoryLeak(ALeakedPointer: Pointer): Boolean; overload;
var
  LNewEntry: TExpectedMemoryLeak;
begin
  {Fill out the structure}
{$ifndef FullDebugMode}
  LNewEntry.LeakAddress := ALeakedPointer;
{$else}
  LNewEntry.LeakAddress := Pointer(PByte(ALeakedPointer) - SizeOf(TFullDebugBlockHeader));
{$endif}
  LNewEntry.LeakedClass := nil;
  {$ifdef CheckCppObjectTypeEnabled}
  LNewEntry.LeakedCppTypeIdPtr := nil;
  {$endif}
  LNewEntry.LeakSize := 0;
  LNewEntry.LeakCount := -1;
  {Remove it from the list}
  Result := LockExpectedMemoryLeaksList
    and UpdateExpectedLeakList(@ExpectedMemoryLeaks.FirstEntryByAddress, @LNewEntry);
  ExpectedMemoryLeaksListLocked := False;
end;

function UnregisterExpectedMemoryLeak(ALeakedObjectClass: TClass; ACount: Integer = 1): Boolean; overload;
begin
  Result := RegisterExpectedMemoryLeak(ALeakedObjectClass, - ACount);
end;

{$ifdef CheckCppObjectTypeEnabled}
function UnregisterExpectedMemoryLeak(ALeakedCppVirtObjTypeIdPtr: Pointer; ACount: Integer): Boolean; overload;
begin
  Result := RegisterExpectedMemoryLeak(ALeakedCppVirtObjTypeIdPtr, - ACount);
end;
{$endif}

function UnregisterExpectedMemoryLeak(ALeakedBlockSize: NativeInt; ACount: Integer = 1): Boolean; overload;
begin
  Result := RegisterExpectedMemoryLeak(ALeakedBlockSize, - ACount);
end;

{Returns a list of all expected memory leaks}
function GetRegisteredMemoryLeaks: TRegisteredMemoryLeaks;

  procedure AddEntries(AEntry: PExpectedMemoryLeak);
  var
    LInd: Integer;
  begin
    while AEntry <> nil do
    begin
      LInd := Length(Result);
      SetLength(Result, LInd + 1);
      {Add the entry}
{$ifndef FullDebugMode}
      Result[LInd].LeakAddress := AEntry.LeakAddress;
{$else}
      Result[LInd].LeakAddress := Pointer(PByte(AEntry.LeakAddress) + SizeOf(TFullDebugBlockHeader));
{$endif}
      Result[LInd].LeakedClass := AEntry.LeakedClass;
{$ifdef CheckCppObjectTypeEnabled}
      Result[LInd].LeakedCppTypeIdPtr := AEntry.LeakedCppTypeIdPtr;
{$endif}
      Result[LInd].LeakSize := AEntry.LeakSize;
      Result[LInd].LeakCount := AEntry.LeakCount;
      {Next entry}
      AEntry := AEntry.NextLeak;
    end;
  end;

begin
  SetLength(Result, 0);
  if (ExpectedMemoryLeaks <> nil) and LockExpectedMemoryLeaksList then
  begin
    {Add all entries}
    AddEntries(ExpectedMemoryLeaks.FirstEntryByAddress);
    AddEntries(ExpectedMemoryLeaks.FirstEntryByClass);
    AddEntries(ExpectedMemoryLeaks.FirstEntryBySizeOnly);
    {Unlock the list}
    ExpectedMemoryLeaksListLocked := False;
  end;
end;

{$else}
  {$ifdef BDS2006AndUp}
function NoOpRegisterExpectedMemoryLeak(ALeakedPointer: Pointer): Boolean;
begin
  {Do nothing. Used when memory leak reporting is disabled under Delphi 2006 and later.}
  Result := False;
end;

function NoOpUnregisterExpectedMemoryLeak(ALeakedPointer: Pointer): Boolean;
begin
  {Do nothing. Used when memory leak reporting is disabled under Delphi 2006 and later.}
  Result := False;
end;
  {$endif}
{$endif}

{Detects the probable string data type for a memory block.}
function DetectStringData(APMemoryBlock: Pointer;
  AAvailableSpaceInBlock: NativeInt): TStringDataType;
const
  {If the string reference count field contains a value greater than this,
   then it is assumed that the block is not a string.}
  MaxRefCount = 255;
  {The lowest ASCII character code considered valid string data. If there are
   any characters below this code point then the data is assumed not to be a
   string. #9 = Tab.}
  MinCharCode = #9;
var
  LStringLength, LElemSize, LCharInd: Integer;
  LPAnsiStr: PAnsiChar;
  LPUniStr: PWideChar;
begin
  {Check that the reference count is within a reasonable range}
  if PStrRec(APMemoryBlock).refCnt > MaxRefCount then
  begin
    Result := stUnknown;
    Exit;
  end;
{$ifdef BCB6OrDelphi6AndUp}
  {$if RTLVersion >= 20}
  LElemSize := PStrRec(APMemoryBlock).elemSize;
  {Element size must be either 1 (Ansi) or 2 (Unicode)}
  if (LElemSize <> 1) and (LElemSize <> 2) then
  begin
    Result := stUnknown;
    Exit;
  end;
  {$ifend}
  {$if RTLVersion < 20}
  LElemSize := 1;
  {$ifend}
{$else}
  LElemSize := 1;
{$endif}
  {Get the string length}
  LStringLength := PStrRec(APMemoryBlock).length;
  {Does the string fit?}
  if (LStringLength <= 0)
    or (LStringLength >= (AAvailableSpaceInBlock - SizeOf(StrRec)) div LElemSize) then
  begin
    Result := stUnknown;
    Exit;
  end;
  {Check for no characters outside the expected range. If there are,
   then it is probably not a string.}
  if LElemSize = 1 then
  begin
    {Check that all characters are in the range considered valid.}
    LPAnsiStr := PAnsiChar(PByte(APMemoryBlock) + SizeOf(StrRec));
    for LCharInd := 1 to LStringLength do
    begin
      if LPAnsiStr^ < MinCharCode then
      begin
        Result := stUnknown;
        Exit;
      end;
      Inc(LPAnsiStr);
    end;
    {Must have a trailing #0}
    if LPAnsiStr^ = #0 then
      Result := stAnsiString
    else
      Result := stUnknown;
  end
  else
  begin
    {Check that all characters are in the range considered valid.}
    LPUniStr := PWideChar(PByte(APMemoryBlock) + SizeOf(StrRec));
    for LCharInd := 1 to LStringLength do
    begin
      if LPUniStr^ < MinCharCode then
      begin
        Result := stUnknown;
        Exit;
      end;
      Inc(LPUniStr);
    end;
    {Must have a trailing #0}
    if LPUniStr^ = #0 then
      Result := stUnicodeString
    else
      Result := stUnknown;
  end;
end;

{Walks all allocated blocks, calling ACallBack for each. Passes the user block size and AUserData to the callback.
 Important note: All block types will be locked during the callback, so the memory manager cannot be used inside it.}
procedure WalkAllocatedBlocks(ACallBack: TWalkAllocatedBlocksCallback; AUserData: Pointer);
const
  DebugHeaderSize = {$ifdef FullDebugMode}SizeOf(TFullDebugBlockHeader){$else}0{$endif};
  TotalDebugOverhead = {$ifdef FullDebugMode}FullDebugBlockOverhead{$else}0{$endif};
var
  LPMediumBlock: Pointer;
  LPMediumBlockPoolHeader: PMediumBlockPoolHeader;
  LMediumBlockHeader: NativeUInt;
  LPLargeBlock: PLargeBlockHeader;
  LBlockSize: NativeInt;
  LPSmallBlockPool: PSmallBlockPoolHeader;
  LCurPtr, LEndPtr: Pointer;
  LInd: Integer;
begin
  {Lock all small block types}
  LockAllSmallBlockTypes;
  {Lock the medium blocks}
  LockMediumBlocks;
  try
    {Step through all the medium block pools}
    LPMediumBlockPoolHeader := MediumBlockPoolsCircularList.NextMediumBlockPoolHeader;
    while LPMediumBlockPoolHeader <> @MediumBlockPoolsCircularList do
    begin
      LPMediumBlock := GetFirstMediumBlockInPool(LPMediumBlockPoolHeader);
      while LPMediumBlock <> nil do
      begin
        LMediumBlockHeader := PNativeUInt(PByte(LPMediumBlock) - BlockHeaderSize)^;
        {Is the block in use?}
        if LMediumBlockHeader and IsFreeBlockFlag = 0 then
        begin
          if (LMediumBlockHeader and IsSmallBlockPoolInUseFlag) <> 0 then
          begin
            {Step through all the blocks in the small block pool}
            LPSmallBlockPool := LPMediumBlock;
            {Get the useable size inside a block}
            LBlockSize := LPSmallBlockPool.BlockType.BlockSize - BlockHeaderSize - TotalDebugOverhead;
            {Get the first and last pointer for the pool}
            GetFirstAndLastSmallBlockInPool(LPSmallBlockPool, LCurPtr, LEndPtr);
            {Step through all blocks}
            while UIntPtr(LCurPtr) <= UIntPtr(LEndPtr) do
            begin
              {Is this block in use?}
              if (PNativeUInt(PByte(LCurPtr) - BlockHeaderSize)^ and IsFreeBlockFlag) = 0 then
              begin
                ACallBack(PByte(LCurPtr) + DebugHeaderSize, LBlockSize, AUserData);
              end;
              {Next block}
              Inc(PByte(LCurPtr), LPSmallBlockPool.BlockType.BlockSize);
            end;
          end
          else
          begin
            LBlockSize := (LMediumBlockHeader and DropMediumAndLargeFlagsMask) - BlockHeaderSize - TotalDebugOverhead;
            ACallBack(PByte(LPMediumBlock) + DebugHeaderSize, LBlockSize, AUserData);
          end;
        end;
        {Next medium block}
        LPMediumBlock := NextMediumBlock(LPMediumBlock);
      end;
      {Get the next medium block pool}
      LPMediumBlockPoolHeader := LPMediumBlockPoolHeader.NextMediumBlockPoolHeader;
    end;
  finally
    {Unlock medium blocks}
    MediumBlocksLocked := False;
    {Unlock all the small block types}
    for LInd := 0 to NumSmallBlockTypes - 1 do
      SmallBlockTypes[LInd].BlockTypeLocked := False;
  end;
  {Step through all the large blocks}
  LockLargeBlocks;
  try
    {Get all leaked large blocks}
    LPLargeBlock := LargeBlocksCircularList.NextLargeBlockHeader;
    while LPLargeBlock <> @LargeBlocksCircularList do
    begin
      LBlockSize := (LPLargeBlock.BlockSizeAndFlags and DropMediumAndLargeFlagsMask) - BlockHeaderSize - LargeBlockHeaderSize - TotalDebugOverhead;
      ACallBack(PByte(LPLargeBlock) + LargeBlockHeaderSize + DebugHeaderSize, LBlockSize, AUserData);
      {Get the next large block}
      LPLargeBlock := LPLargeBlock.NextLargeBlockHeader;
    end;
  finally
    LargeBlocksLocked := False;
  end;
end;

{-----------LogMemoryManagerStateToFile implementation------------}
const
  MaxMemoryLogNodes = 100000;
  QuickSortMinimumItemsInPartition = 4;

type
  {While scanning the memory pool the list of classes is built up in a binary search tree.}
  PMemoryLogNode = ^TMemoryLogNode;
  TMemoryLogNode = record
    {The left and right child nodes}
    LeftAndRightNodePointers: array[Boolean] of PMemoryLogNode;
    {The class this node belongs to}
    ClassPtr: Pointer;
    {The number of instances of the class}
    InstanceCount: NativeInt;
    {The total memory usage for this class}
    TotalMemoryUsage: NativeInt;
  end;
  TMemoryLogNodes = array[0..MaxMemoryLogNodes - 1] of TMemoryLogNode;
  PMemoryLogNodes = ^TMemoryLogNodes;

  TMemoryLogInfo = record
    {The number of nodes in "Nodes" that are used.}
    NodeCount: Integer;
    {The root node of the binary search tree. The content of this node is not actually used, it just simplifies the
     binary search code.}
    RootNode: TMemoryLogNode;
    Nodes: TMemoryLogNodes;
  end;
  PMemoryLogInfo = ^TMemoryLogInfo;

{LogMemoryManagerStateToFile callback subroutine}
procedure LogMemoryManagerStateCallBack(APBlock: Pointer; ABlockSize: NativeInt; AUserData: Pointer);
var
  LClass, LClassHashBits: NativeUInt;
  LPLogInfo: PMemoryLogInfo;
  LPParentNode, LPClassNode: PMemoryLogNode;
  LChildNodeDirection: Boolean;
begin
  LPLogInfo := AUserData;
  {Detecting an object is very expensive (due to the VirtualQuery call), so we do some basic checks and try to find
   the "class" in the tree first.}
  LClass := PNativeUInt(APBlock)^;
  {Do some basic pointer checks: The "class" must be dword aligned and beyond 64K}
  if (LClass > 65535)
    and (LClass and 3 = 0) then
  begin
    LPParentNode := @LPLogInfo.RootNode;
    LClassHashBits := LClass;
    repeat
      LChildNodeDirection := Boolean(LClassHashBits and 1);
      {Split off the next bit of the class pointer and traverse in the appropriate direction.}
      LPClassNode := LPParentNode.LeftAndRightNodePointers[LChildNodeDirection];
      {Is this child node the node the class we're looking for?}
      if (LPClassNode = nil) or (NativeUInt(LPClassNode.ClassPtr) = LClass) then
        Break;
      {The node was not found: Keep on traversing the tree.}
      LClassHashBits := LClassHashBits shr 1;
      LPParentNode := LPClassNode;
    until False;
  end
  else
    LPClassNode := nil;
  {Was the "class" found?}
  if LPClassNode = nil then
  begin
    {The "class" is not yet in the tree: Determine if it is actually a class.}
    LClass := NativeUInt(DetectClassInstance(APBlock));
    {If it is not a class, try to detect the string type.}
    if LClass = 0 then
      LClass := Ord(DetectStringData(APBlock, ABlockSize));
    {Is this class already in the tree?}
    LPParentNode := @LPLogInfo.RootNode;
    LClassHashBits := LClass;
    repeat
      LChildNodeDirection := Boolean(LClassHashBits and 1);
      {Split off the next bit of the class pointer and traverse in the appropriate direction.}
      LPClassNode := LPParentNode.LeftAndRightNodePointers[LChildNodeDirection];
      {Is this child node the node the class we're looking for?}
      if LPClassNode = nil then
      begin
        {The end of the tree was reached: Add a new child node.}
        LPClassNode := @LPLogInfo.Nodes[LPLogInfo.NodeCount];
        Inc(LPLogInfo.NodeCount);
        LPParentNode.LeftAndRightNodePointers[LChildNodeDirection] := LPClassNode;
        LPClassNode.ClassPtr := Pointer(LClass);
        Break;
      end
      else
      begin
        if NativeUInt(LPClassNode.ClassPtr) = LClass then
          Break;
      end;
      {The node was not found: Keep on traversing the tree.}
      LClassHashBits := LClassHashBits shr 1;
      LPParentNode := LPClassNode;
    until False;
  end;
  {Update the statistics for the class}
  Inc(LPClassNode.InstanceCount);
  Inc(LPClassNode.TotalMemoryUsage, ABlockSize);
end;

{LogMemoryManagerStateToFile subroutine: A median-of-3 quicksort routine for sorting a TMemoryLogNodes array.}
procedure QuickSortLogNodes(APLeftItem: PMemoryLogNodes; ARightIndex: Integer);
var
  M, I, J: Integer;
  LPivot, LTempItem: TMemoryLogNode;
begin
  while True do
  begin
    {Order the left, middle and right items in ascending order}
    M := ARightIndex shr 1;
    {Is the middle item larger than the left item?}
    if APLeftItem[0].TotalMemoryUsage > APLeftItem[M].TotalMemoryUsage then
    begin
      {Swap items 0 and M}
      LTempItem := APLeftItem[0];
      APLeftItem[0] := APLeftItem[M];
      APLeftItem[M] := LTempItem;
    end;
    {Is the middle item larger than the right?}
    if APLeftItem[M].TotalMemoryUsage > APLeftItem[ARightIndex].TotalMemoryUsage then
    begin
      {The right-hand item is not larger - swap it with the middle}
      LTempItem := APLeftItem[ARightIndex];
      APLeftItem[ARightIndex] := APLeftItem[M];
      APLeftItem[M] := LTempItem;
      {Is the left larger than the new middle?}
      if APLeftItem[0].TotalMemoryUsage > APLeftItem[M].TotalMemoryUsage then
      begin
        {Swap items 0 and M}
        LTempItem := APLeftItem[0];
        APLeftItem[0] := APLeftItem[M];
        APLeftItem[M] := LTempItem;
      end;
    end;
    {Move the pivot item out of the way by swapping M with R - 1}
    LPivot := APLeftItem[M];
    APLeftItem[M] := APLeftItem[ARightIndex - 1];
    APLeftItem[ARightIndex - 1] := LPivot;
    {Set up the loop counters}
    I := 0;
    J := ARightIndex - 1;
    while true do
    begin
      {Find the first item from the left that is not smaller than the pivot}
      repeat
        Inc(I);
      until APLeftItem[I].TotalMemoryUsage >= LPivot.TotalMemoryUsage;
      {Find the first item from the right that is not larger than the pivot}
      repeat
        Dec(J);
      until APLeftItem[J].TotalMemoryUsage <= LPivot.TotalMemoryUsage;
      {Stop the loop when the two indexes cross}
      if J < I then
        Break;
      {Swap item I and J}
      LTempItem := APLeftItem[I];
      APLeftItem[I] := APLeftItem[J];
      APLeftItem[J] := LTempItem;
    end;
    {Put the pivot item back in the correct position by swapping I with R - 1}
    APLeftItem[ARightIndex - 1] := APLeftItem[I];
    APLeftItem[I] := LPivot;
    {Sort the left-hand partition}
    if J >= (QuickSortMinimumItemsInPartition - 1) then
      QuickSortLogNodes(APLeftItem, J);
    {Sort the right-hand partition}
    APLeftItem := @APLeftItem[I + 1];
    ARightIndex := ARightIndex - I - 1;
    if ARightIndex < (QuickSortMinimumItemsInPartition - 1) then
      Break;
  end;
end;

{LogMemoryManagerStateToFile subroutine: An InsertionSort routine for sorting a TMemoryLogNodes array.}
procedure InsertionSortLogNodes(APLeftItem: PMemoryLogNodes; ARightIndex: Integer);
var
  I, J: Integer;
  LCurNode: TMemoryLogNode;
begin
  for I := 1 to ARightIndex do
  begin
    LCurNode := APLeftItem[I];
    {Scan backwards to find the best insertion spot}
    J := I;
    while (J > 0) and (APLeftItem[J - 1].TotalMemoryUsage > LCurNode.TotalMemoryUsage) do
    begin
      APLeftItem[J] := APLeftItem[J - 1];
      Dec(J);
    end;
    APLeftItem[J] := LCurNode;
  end;
end;

{Writes a log file containing a summary of the memory mananger state and a summary of allocated blocks grouped by
 class. The file will be saved in UTF-8 encoding (in supported Delphi versions). Returns True on success. }
function LogMemoryManagerStateToFile(const AFileName: string; const AAdditionalDetails: string): Boolean;
const
  MsgBufferSize = 65536;
  MaxLineLength = 512;
  {Write the UTF-8 BOM in Delphi versions that support UTF-8 conversion.}
  LogStateHeaderMsg = {$ifdef BCB6OrDelphi7AndUp}#$EF#$BB#$BF + {$endif}
    'FastMM State Capture:'#13#10'---------------------'#13#10#13#10;
  LogStateAllocatedMsg = 'K Allocated'#13#10;
  LogStateOverheadMsg = 'K Overhead'#13#10;
  LogStateEfficiencyMsg = '% Efficiency'#13#10#13#10'Usage Detail:'#13#10;
  LogStateAdditionalInfoMsg = #13#10'Additional Information:'#13#10'-----------------------'#13#10;
var
  LPLogInfo: PMemoryLogInfo;
  LInd: Integer;
  LPNode: PMemoryLogNode;
  LMsgBuffer: array[0..MsgBufferSize - 1] of AnsiChar;
  LPMsg: PAnsiChar;
  LBufferSpaceUsed, LBytesWritten: Cardinal;
  LFileHandle: NativeUInt;
  LMemoryManagerUsageSummary: TMemoryManagerUsageSummary;
  LUTF8Str: AnsiString;
begin
  {Get the current memory manager usage summary.}
  GetMemoryManagerUsageSummary(LMemoryManagerUsageSummary);
  {Allocate the memory required to capture detailed allocation information.}
  LPLogInfo := VirtualAlloc(nil, SizeOf(TMemoryLogInfo), MEM_COMMIT or MEM_TOP_DOWN, PAGE_READWRITE);
  if LPLogInfo <> nil then
  begin
    try
      {Log all allocated blocks by class.}
      WalkAllocatedBlocks(LogMemoryManagerStateCallBack, LPLogInfo);
      {Sort the classes by total memory usage: Do the initial QuickSort pass over the list to sort the list in groups
       of QuickSortMinimumItemsInPartition size.}
      if LPLogInfo.NodeCount >= QuickSortMinimumItemsInPartition then
        QuickSortLogNodes(@LPLogInfo.Nodes[0], LPLogInfo.NodeCount - 1);
      {Do the final InsertionSort pass.}
      InsertionSortLogNodes(@LPLogInfo.Nodes[0], LPLogInfo.NodeCount - 1);
      {Create the output file}
      {$ifdef POSIX}
      lFileHandle := FileCreate(AFilename);
      {$else}
      LFileHandle := CreateFile(PChar(AFilename), GENERIC_READ or GENERIC_WRITE, 0,
        nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
      {$endif}
      if LFileHandle <> INVALID_HANDLE_VALUE then
      begin
        try
          {Log the usage summary}
          LPMsg := @LMsgBuffer;
          LPMsg := AppendStringToBuffer(LogStateHeaderMsg, LPMsg, Length(LogStateHeaderMsg));
          LPMsg := NativeUIntToStrBuf(LMemoryManagerUsageSummary.AllocatedBytes shr 10, LPMsg);
          LPMsg := AppendStringToBuffer(LogStateAllocatedMsg, LPMsg, Length(LogStateAllocatedMsg));
          LPMsg := NativeUIntToStrBuf(LMemoryManagerUsageSummary.OverheadBytes shr 10, LPMsg);
          LPMsg := AppendStringToBuffer(LogStateOverheadMsg, LPMsg, Length(LogStateOverheadMsg));
          LPMsg := NativeUIntToStrBuf(Round(LMemoryManagerUsageSummary.EfficiencyPercentage), LPMsg);
          LPMsg := AppendStringToBuffer(LogStateEfficiencyMsg, LPMsg, Length(LogStateEfficiencyMsg));
          {Log the allocation detail}
          for LInd := LPLogInfo.NodeCount - 1 downto 0 do
          begin
            LPNode := @LPLogInfo.Nodes[LInd];
            {Add the allocated size}
            LPMsg^ := ' ';
            Inc(LPMsg);
            LPMsg := NativeUIntToStrBuf(LPNode.TotalMemoryUsage, LPMsg);
            LPMsg := AppendStringToBuffer(BytesMessage, LPMsg, Length(BytesMessage));
            {Add the class type}
            case NativeInt(LPNode.ClassPtr) of
              {Unknown}
              0:
              begin
                LPMsg := AppendStringToBuffer(UnknownClassNameMsg, LPMsg, Length(UnknownClassNameMsg));
              end;
              {AnsiString}
              1:
              begin
                LPMsg := AppendStringToBuffer(AnsiStringBlockMessage, LPMsg, Length(AnsiStringBlockMessage));
              end;
              {UnicodeString}
              2:
              begin
                LPMsg := AppendStringToBuffer(UnicodeStringBlockMessage, LPMsg, Length(UnicodeStringBlockMessage));
              end;
              {Classes}
            else
              begin
                LPMsg := AppendClassNameToBuffer(LPNode.ClassPtr, LPMsg);
              end;
            end;
            {Add the count}
            LPMsg^ := ' ';
            Inc(LPMsg);
            LPMsg^ := 'x';
            Inc(LPMsg);
            LPMsg^ := ' ';
            Inc(LPMsg);
            LPMsg := NativeUIntToStrBuf(LPNode.InstanceCount, LPMsg);
            LPMsg^ := #13;
            Inc(LPMsg);
            LPMsg^ := #10;
            Inc(LPMsg);
            {Flush the buffer?}
            LBufferSpaceUsed := NativeInt(LPMsg) - NativeInt(@LMsgBuffer);
            if LBufferSpaceUsed > (MsgBufferSize - MaxLineLength) then
            begin
              WriteFile(LFileHandle, LMsgBuffer, LBufferSpaceUsed, LBytesWritten, nil);
              LPMsg := @LMsgBuffer;
            end;
          end;
          if AAdditionalDetails <> '' then
            LPMsg := AppendStringToBuffer(LogStateAdditionalInfoMsg, LPMsg, Length(LogStateAdditionalInfoMsg));
          {Flush any remaining bytes}
          LBufferSpaceUsed := NativeInt(LPMsg) - NativeInt(@LMsgBuffer);
          if LBufferSpaceUsed > 0 then
            WriteFile(LFileHandle, LMsgBuffer, LBufferSpaceUsed, LBytesWritten, nil);
          {Write the additional info}
          if AAdditionalDetails <> '' then
          begin
            {$ifdef BCB6OrDelphi7AndUp}
            LUTF8Str := UTF8Encode(AAdditionalDetails);
            {$else}
            LUTF8Str := AAdditionalDetails;
            {$endif}
            WriteFile(LFileHandle, LUTF8Str[1], Length(LUTF8Str), LBytesWritten, nil);
          end;
          {Success}
          Result := True;
        finally
          {Close the file}
          {$ifdef POSIX}
          __close(LFileHandle)
          {$else}
          CloseHandle(LFileHandle);
          {$endif}
        end;
      end
      else
        Result := False;
    finally
      VirtualFree(LPLogInfo, 0, MEM_RELEASE);
    end;
  end
  else
    Result := False;
end;

{-----------CheckBlocksOnShutdown implementation------------}

{Checks blocks for modification after free and also for memory leaks}
procedure CheckBlocksOnShutdown(ACheckForLeakedBlocks: Boolean);
{$ifdef EnableMemoryLeakReporting}
type
  {Leaked class type}
  TLeakedClass = record
    ClassPointer: TClass;
    {$ifdef CheckCppObjectTypeEnabled}
    CppTypeIdPtr: Pointer;
    {$endif}
    NumLeaks: Cardinal;
  end;
  TLeakedClasses = array[0..255] of TLeakedClass;
  PLeakedClasses = ^TLeakedClasses;
  {Leak statistics for a small block type}
  TSmallBlockLeaks = array[0..NumSmallBlockTypes - 1] of TLeakedClasses;
  {A leaked medium or large block}
  TMediumAndLargeBlockLeaks = array[0..4095] of NativeUInt;
{$endif}
var
{$ifdef EnableMemoryLeakReporting}
  {The leaked classes for small blocks}
  LSmallBlockLeaks: TSmallBlockLeaks;
  LLeakType: TMemoryLeakType;
  {$ifdef CheckCppObjectTypeEnabled}
  LLeakedCppTypeIdPtr: Pointer;
  LCppTypeName: PAnsiChar;
  {$endif}
  LMediumAndLargeBlockLeaks: TMediumAndLargeBlockLeaks;
  LNumMediumAndLargeLeaks: Integer;
  LPLargeBlock: PLargeBlockHeader;
  LLeakMessage: array[0..32767] of AnsiChar;
  {$ifndef NoMessageBoxes}
  LMessageTitleBuffer: array[0..1023] of AnsiChar;
  {$endif}
  LMsgPtr: PAnsiChar;
  LExpectedLeaksOnly, LSmallLeakHeaderAdded, LBlockSizeHeaderAdded: Boolean;
  LBlockTypeInd, LClassInd, LBlockInd: Cardinal;
  LMediumBlockSize, LPreviousBlockSize, LLargeBlockSize, LThisBlockSize: NativeUInt;
{$endif}
  LPMediumBlock: Pointer;
  LPMediumBlockPoolHeader: PMediumBlockPoolHeader;
  LMediumBlockHeader: NativeUInt;

{$ifdef EnableMemoryLeakReporting}
  {Tries to account for a memory leak. Returns true if the leak is expected and
   removes the leak from the list}
  function GetMemoryLeakType(AAddress: Pointer; ASpaceInsideBlock: NativeUInt): TMemoryLeakType;
  var
    LLeak: TExpectedMemoryLeak;
  begin
    {Default to not found}
    Result := mltUnexpectedLeak;
    if ExpectedMemoryLeaks <> nil then
    begin
      {Check by pointer address}
      LLeak.LeakAddress := AAddress;
      LLeak.LeakedClass := nil;
      {$ifdef CheckCppObjectTypeEnabled}
      LLeak.LeakedCppTypeIdPtr := nil;
      {$endif}
      LLeak.LeakSize := 0;
      LLeak.LeakCount := -1;
      if UpdateExpectedLeakList(@ExpectedMemoryLeaks.FirstEntryByAddress, @LLeak, False) then
      begin
        Result := mltExpectedLeakRegisteredByPointer;
        Exit;
      end;
      {Check by class}
      LLeak.LeakAddress := nil;
      {$ifdef FullDebugMode}
      LLeak.LeakedClass := TClass(PNativeUInt(PByte(AAddress)+ SizeOf(TFullDebugBlockHeader))^);
      {$else}
      LLeak.LeakedClass := TClass(PNativeUInt(AAddress)^);
      {$endif}
      {$ifdef CheckCppObjectTypeEnabled}
      if Assigned(GetCppVirtObjTypeIdPtrFunc) then
      begin
        {$ifdef FullDebugMode}
        LLeak.LeakedCppTypeIdPtr := GetCppVirtObjTypeIdPtrFunc(Pointer(PByte(AAddress)
          + SizeOf(TFullDebugBlockHeader)), ASpaceInsideBlock);
        {$else}
        LLeak.LeakedCppTypeIdPtr := GetCppVirtObjTypeIdPtrFunc(AAddress, ASpaceInsideBlock);
        {$endif}
      end;
      LLeakedCppTypeIdPtr := LLeak.LeakedCppTypeIdPtr;
      {$endif}
      LLeak.LeakSize := ASpaceInsideBlock;
      if UpdateExpectedLeakList(@ExpectedMemoryLeaks.FirstEntryByClass, @LLeak, False) then
      begin
        Result := mltExpectedLeakRegisteredByClass;
        Exit;
      end;
      {Check by size: the block must be large enough to hold the leak}
      LLeak.LeakedClass := nil;
      if UpdateExpectedLeakList(@ExpectedMemoryLeaks.FirstEntryBySizeOnly, @LLeak, False) then
        Result := mltExpectedLeakRegisteredBySize;
    end;
  end;

  {Checks the small block pool for leaks.}
  procedure CheckSmallBlockPoolForLeaks(APSmallBlockPool: PSmallBlockPoolHeader);
  var
    LLeakedClass: TClass;
    {$ifdef CheckCppObjectTypeEnabled}
    LLeakedCppObjectTypeId: Pointer;
    {$endif}
    LSmallBlockLeakType: TMemoryLeakType;
    LClassIndex: Integer;
    LCurPtr, LEndPtr, LDataPtr: Pointer;
    LBlockTypeIndex: Cardinal;
    LPLeakedClasses: PLeakedClasses;
    LSmallBlockSize: Cardinal;
  begin
    {Get the useable size inside a block}
    LSmallBlockSize := APSmallBlockPool.BlockType.BlockSize - BlockHeaderSize;
  {$ifdef FullDebugMode}
    Dec(LSmallBlockSize, FullDebugBlockOverhead);
  {$endif}
    {Get the block type index}
    LBlockTypeIndex := (UIntPtr(APSmallBlockPool.BlockType) - UIntPtr(@SmallBlockTypes[0])) div SizeOf(TSmallBlockType);
    LPLeakedClasses := @LSmallBlockLeaks[LBlockTypeIndex];
    {Get the first and last pointer for the pool}
    GetFirstAndLastSmallBlockInPool(APSmallBlockPool, LCurPtr, LEndPtr);
    {Step through all blocks}
    while UIntPtr(LCurPtr) <= UIntPtr(LEndPtr) do
    begin
      {Is this block in use? If so, is the debug info intact?}
      if ((PNativeUInt(PByte(LCurPtr) - BlockHeaderSize)^ and IsFreeBlockFlag) = 0) then
      begin
  {$ifdef FullDebugMode}
        if CheckBlockBeforeFreeOrRealloc(LCurPtr, boBlockCheck) then
  {$endif}
        begin
          {$ifdef CheckCppObjectTypeEnabled}
          LLeakedCppTypeIdPtr := nil;
          {$endif}
          {Get the leak type}
          LSmallBlockLeakType := GetMemoryLeakType(LCurPtr, LSmallBlockSize);
    {$ifdef LogMemoryLeakDetailToFile}
      {$ifdef HideExpectedLeaksRegisteredByPointer}
          if LSmallBlockLeakType <> mltExpectedLeakRegisteredByPointer then
      {$endif}
            LogMemoryLeakOrAllocatedBlock(LCurPtr, True);
    {$endif}
          {Only expected leaks?}
          LExpectedLeaksOnly := LExpectedLeaksOnly and (LSmallBlockLeakType <> mltUnexpectedLeak);
    {$ifdef HideExpectedLeaksRegisteredByPointer}
          if LSmallBlockLeakType <> mltExpectedLeakRegisteredByPointer then
    {$endif}
          begin
            {Get a pointer to the user data}
    {$ifndef FullDebugMode}
            LDataPtr := LCurPtr;
    {$else}
            LDataPtr := Pointer(PByte(LCurPtr) + SizeOf(TFullDebugBlockHeader));
    {$endif}
            {Default to an unknown block}
            LClassIndex := 0;
            {Get the class contained by the block}
            LLeakedClass := DetectClassInstance(LDataPtr);
            {Not a Delphi class? -> is it perhaps a string or C++ object type?}
            if LLeakedClass = nil then
            begin
              {$ifdef CheckCppObjectTypeEnabled}
              LLeakedCppObjectTypeId := LLeakedCppTypeIdPtr;
              if (LLeakedCppObjectTypeId = nil) and (ExpectedMemoryLeaks = nil) then
              begin
                if Assigned(GetCppVirtObjTypeIdPtrFunc) then
                begin
                  LLeakedCppObjectTypeId := GetCppVirtObjTypeIdPtrFunc(LDataPtr, LSmallBlockSize);
                end;
              end;
              if Assigned(LLeakedCppObjectTypeId) then
              begin
                LClassIndex := 3;
                while LClassIndex <= High(TLeakedClasses) do
                begin
                  if (Pointer(LPLeakedClasses[LClassIndex].CppTypeIdPtr) = LLeakedCppObjectTypeId)
                    or ((LPLeakedClasses[LClassIndex].CppTypeIdPtr = nil)
                    and (LPLeakedClasses[LClassIndex].ClassPointer = nil)) then
                  begin
                    Break;
                  end;
                  Inc(LClassIndex);
                end;
                if LClassIndex <= High(TLeakedClasses) then
                  Pointer(LPLeakedClasses[LClassIndex].CppTypeIdPtr) := LLeakedCppObjectTypeId
                else
                  LClassIndex := 0;
              end
              else
              begin
              {$endif}
                {Not a known class: Is it perhaps string data?}
                case DetectStringData(LDataPtr, APSmallBlockPool.BlockType.BlockSize - (BlockHeaderSize {$ifdef FullDebugMode} + FullDebugBlockOverhead{$endif})) of
                  stAnsiString: LClassIndex := 1;
                  stUnicodeString: LClassIndex := 2;
                end;
              {$ifdef CheckCppObjectTypeEnabled}
              end;
              {$endif}
            end
            else
            begin
              LClassIndex := 3;
              while LClassIndex <= High(TLeakedClasses) do
              begin
                if (LPLeakedClasses[LClassIndex].ClassPointer = LLeakedClass)
                  or ((LPLeakedClasses[LClassIndex].ClassPointer = nil)
                  {$ifdef CheckCppObjectTypeEnabled}
                  and (LPLeakedClasses[LClassIndex].CppTypeIdPtr = nil)
                  {$endif}
                  ) then
                begin
                  Break;
                end;
                Inc(LClassIndex);
              end;
              if LClassIndex <= High(TLeakedClasses) then
                LPLeakedClasses[LClassIndex].ClassPointer := LLeakedClass
              else
                LClassIndex := 0;
            end;
            {Add to the number of leaks for the class}
            Inc(LPLeakedClasses[LClassIndex].NumLeaks);
          end;
        end;
      end
      else
      begin
  {$ifdef CheckUseOfFreedBlocksOnShutdown}
        {Check that the block has not been modified since being freed}
        CheckFreeBlockUnmodified(LCurPtr, APSmallBlockPool.BlockType.BlockSize, boBlockCheck);
  {$endif}
      end;
      {Next block}
      Inc(PByte(LCurPtr), APSmallBlockPool.BlockType.BlockSize);
    end;
  end;
{$endif}

begin
{$ifdef EnableMemoryLeakReporting}
  {Clear the leak arrays}
  FillChar(LSmallBlockLeaks, SizeOf(LSmallBlockLeaks), 0);
  FillChar(LMediumAndLargeBlockLeaks, SizeOf(LMediumAndLargeBlockLeaks), 0);
  {Step through all the medium block pools}
  LNumMediumAndLargeLeaks := 0;
  {No unexpected leaks so far}
  LExpectedLeaksOnly := True;
{$endif}
  {Step through all the medium block pools}
  LPMediumBlockPoolHeader := MediumBlockPoolsCircularList.NextMediumBlockPoolHeader;
  while LPMediumBlockPoolHeader <> @MediumBlockPoolsCircularList do
  begin
    LPMediumBlock := GetFirstMediumBlockInPool(LPMediumBlockPoolHeader);
    while LPMediumBlock <> nil do
    begin
      LMediumBlockHeader := PNativeUInt(PByte(LPMediumBlock) - BlockHeaderSize)^;
      {Is the block in use?}
      if LMediumBlockHeader and IsFreeBlockFlag = 0 then
      begin
{$ifdef EnableMemoryLeakReporting}
        if ACheckForLeakedBlocks then
        begin
          if (LMediumBlockHeader and IsSmallBlockPoolInUseFlag) <> 0 then
          begin
            {Get all the leaks for the small block pool}
            CheckSmallBlockPoolForLeaks(LPMediumBlock);
          end
          else
          begin
            if (LNumMediumAndLargeLeaks < Length(LMediumAndLargeBlockLeaks))
  {$ifdef FullDebugMode}
              and CheckBlockBeforeFreeOrRealloc(LPMediumBlock, boBlockCheck)
  {$endif}
            then
            begin
              LMediumBlockSize := (LMediumBlockHeader and DropMediumAndLargeFlagsMask) - BlockHeaderSize;
  {$ifdef FullDebugMode}
              Dec(LMediumBlockSize, FullDebugBlockOverhead);
  {$endif}
              {Get the leak type}
              LLeakType := GetMemoryLeakType(LPMediumBlock, LMediumBlockSize);
              {Is it an expected leak?}
              LExpectedLeaksOnly := LExpectedLeaksOnly and (LLeakType <> mltUnexpectedLeak);
  {$ifdef LogMemoryLeakDetailToFile}
    {$ifdef HideExpectedLeaksRegisteredByPointer}
              if LLeakType <> mltExpectedLeakRegisteredByPointer then
    {$endif}
                LogMemoryLeakOrAllocatedBlock(LPMediumBlock, True);
  {$endif}
  {$ifdef HideExpectedLeaksRegisteredByPointer}
              if LLeakType <> mltExpectedLeakRegisteredByPointer then
  {$endif}
              begin
                {Add the leak to the list}
                LMediumAndLargeBlockLeaks[LNumMediumAndLargeLeaks] := LMediumBlockSize;
                Inc(LNumMediumAndLargeLeaks);
              end;
            end;
          end;
        end;
{$endif}
      end
      else
      begin
{$ifdef CheckUseOfFreedBlocksOnShutdown}
        {Check that the block has not been modified since being freed}
        CheckFreeBlockUnmodified(LPMediumBlock, LMediumBlockHeader and DropMediumAndLargeFlagsMask, boBlockCheck);
{$endif}
      end;
      {Next medium block}
      LPMediumBlock := NextMediumBlock(LPMediumBlock);
    end;
    {Get the next medium block pool}
    LPMediumBlockPoolHeader := LPMediumBlockPoolHeader.NextMediumBlockPoolHeader;
  end;
{$ifdef EnableMemoryLeakReporting}
  if ACheckForLeakedBlocks then
  begin
    {Get all leaked large blocks}
    LPLargeBlock := LargeBlocksCircularList.NextLargeBlockHeader;
    while LPLargeBlock <> @LargeBlocksCircularList do
    begin
      if (LNumMediumAndLargeLeaks < length(LMediumAndLargeBlockLeaks))
  {$ifdef FullDebugMode}
        and CheckBlockBeforeFreeOrRealloc(Pointer(PByte(LPLargeBlock) + LargeBlockHeaderSize), boBlockCheck)
  {$endif}
      then
      begin
        LLargeBlockSize := (LPLargeBlock.BlockSizeAndFlags and DropMediumAndLargeFlagsMask) - BlockHeaderSize - LargeBlockHeaderSize;
  {$ifdef FullDebugMode}
        Dec(LLargeBlockSize, FullDebugBlockOverhead);
  {$endif}
        {Get the leak type}
        LLeakType := GetMemoryLeakType(Pointer(PByte(LPLargeBlock) + LargeBlockHeaderSize), LLargeBlockSize);
        {Is it an expected leak?}
        LExpectedLeaksOnly := LExpectedLeaksOnly and (LLeakType <> mltUnexpectedLeak);
  {$ifdef LogMemoryLeakDetailToFile}
    {$ifdef HideExpectedLeaksRegisteredByPointer}
        if LLeakType <> mltExpectedLeakRegisteredByPointer then
    {$endif}
          LogMemoryLeakOrAllocatedBlock(Pointer(PByte(LPLargeBlock) + LargeBlockHeaderSize), True);
  {$endif}
  {$ifdef HideExpectedLeaksRegisteredByPointer}
        if LLeakType <> mltExpectedLeakRegisteredByPointer then
  {$endif}
        begin
          {Add the leak}
          LMediumAndLargeBlockLeaks[LNumMediumAndLargeLeaks] := LLargeBlockSize;
          Inc(LNumMediumAndLargeLeaks);
        end;
      end;
      {Get the next large block}
      LPLargeBlock := LPLargeBlock.NextLargeBlockHeader;
    end;
    {Display the leak message if required}
    if not LExpectedLeaksOnly then
    begin
      {Small leak header has not been added}
      LSmallLeakHeaderAdded := False;
      LPreviousBlockSize := 0;
      {Set up the leak message header so long}
      LMsgPtr := AppendStringToBuffer(LeakMessageHeader, @LLeakMessage[0], length(LeakMessageHeader));
      {Step through all the small block types}
      for LBlockTypeInd := 0 to NumSmallBlockTypes - 1 do
      begin
        LThisBlockSize := SmallBlockTypes[LBlockTypeInd].BlockSize - BlockHeaderSize;
  {$ifdef FullDebugMode}
        Dec(LThisBlockSize, FullDebugBlockOverhead);
        if NativeInt(LThisBlockSize) < 0 then
          LThisBlockSize := 0;
  {$endif}
        LBlockSizeHeaderAdded := False;
        {Any leaks?}
        for LClassInd := High(LSmallBlockLeaks[LBlockTypeInd]) downto 0 do
        begin
          {Is there still space in the message buffer? Reserve space for the message
           footer.}
          if LMsgPtr > @LLeakMessage[High(LLeakMessage) - 2048] then
            Break;
          {Check the count}
          if LSmallBlockLeaks[LBlockTypeInd][LClassInd].NumLeaks > 0 then
          begin
            {Need to add the header?}
            if not LSmallLeakHeaderAdded then
            begin
              LMsgPtr := AppendStringToBuffer(SmallLeakDetail, LMsgPtr, Length(SmallLeakDetail));
              LSmallLeakHeaderAdded := True;
            end;
            {Need to add the size header?}
            if not LBlockSizeHeaderAdded then
            begin
              LMsgPtr^ := #13;
              Inc(LMsgPtr);
              LMsgPtr^ := #10;
              Inc(LMsgPtr);
              LMsgPtr := NativeUIntToStrBuf(LPreviousBlockSize + 1, LMsgPtr);
              LMsgPtr^ := ' ';
              Inc(LMsgPtr);
              LMsgPtr^ := '-';
              Inc(LMsgPtr);
              LMsgPtr^ := ' ';
              Inc(LMsgPtr);
              LMsgPtr := NativeUIntToStrBuf(LThisBlockSize, LMsgPtr);
              LMsgPtr := AppendStringToBuffer(BytesMessage, LMsgPtr, Length(BytesMessage));
              LBlockSizeHeaderAdded := True;
            end
            else
            begin
              LMsgPtr^ := ',';
              Inc(LMsgPtr);
              LMsgPtr^ := ' ';
              Inc(LMsgPtr);
            end;
            {Show the count}
            case LClassInd of
              {Unknown}
              0:
              begin
                LMsgPtr := AppendStringToBuffer(UnknownClassNameMsg, LMsgPtr, Length(UnknownClassNameMsg));
              end;
              {AnsiString}
              1:
              begin
                LMsgPtr := AppendStringToBuffer(AnsiStringBlockMessage, LMsgPtr, Length(AnsiStringBlockMessage));
              end;
              {UnicodeString}
              2:
              begin
                LMsgPtr := AppendStringToBuffer(UnicodeStringBlockMessage, LMsgPtr, Length(UnicodeStringBlockMessage));
              end;
              {Classes}
            else
              begin
                {$ifdef CheckCppObjectTypeEnabled}
                if LSmallBlockLeaks[LBlockTypeInd][LClassInd].CppTypeIdPtr <> nil then
                begin
                  if Assigned(GetCppVirtObjTypeNameByTypeIdPtrFunc) then
                  begin
                    LCppTypeName := GetCppVirtObjTypeNameByTypeIdPtrFunc(LSmallBlockLeaks[LBlockTypeInd][LClassInd].CppTypeIdPtr);
                    LMsgPtr := AppendStringToBuffer(LCppTypeName, LMsgPtr, StrLen(LCppTypeName));
                  end
                  else
                    LMsgPtr := AppendClassNameToBuffer(nil, LMsgPtr);
                end
                else
                begin
                {$endif}
                  LMsgPtr := AppendClassNameToBuffer(LSmallBlockLeaks[LBlockTypeInd][LClassInd].ClassPointer, LMsgPtr);
                {$ifdef CheckCppObjectTypeEnabled}
                end;
                {$endif}
              end;
            end;
            {Add the count}
            LMsgPtr^ := ' ';
            Inc(LMsgPtr);
            LMsgPtr^ := 'x';
            Inc(LMsgPtr);
            LMsgPtr^ := ' ';
            Inc(LMsgPtr);
            LMsgPtr := NativeUIntToStrBuf(LSmallBlockLeaks[LBlockTypeInd][LClassInd].NumLeaks, LMsgPtr);
          end;
        end;
        LPreviousBlockSize := LThisBlockSize;
      end;
      {Add the medium/large block leak message}
      if LNumMediumAndLargeLeaks > 0 then
      begin
        {Any non-small leaks?}
        if LSmallLeakHeaderAdded then
        begin
          LMsgPtr^ := #13;
          Inc(LMsgPtr);
          LMsgPtr^ := #10;
          Inc(LMsgPtr);
          LMsgPtr^ := #13;
          Inc(LMsgPtr);
          LMsgPtr^ := #10;
          Inc(LMsgPtr);
        end;
        {Add the medium/large block leak message}
        LMsgPtr := AppendStringToBuffer(LargeLeakDetail, LMsgPtr, Length(LargeLeakDetail));
        {List all the blocks}
        for LBlockInd := 0 to LNumMediumAndLargeLeaks - 1 do
        begin
          if LBlockInd <> 0 then
          begin
            LMsgPtr^ := ',';
            Inc(LMsgPtr);
            LMsgPtr^ :=  ' ';
            Inc(LMsgPtr);
          end;
          LMsgPtr := NativeUIntToStrBuf(LMediumAndLargeBlockLeaks[LBlockInd], LMsgPtr);
          {Is there still space in the message buffer? Reserve space for the
           message footer.}
          if LMsgPtr > @LLeakMessage[High(LLeakMessage) - 2048] then
            Break;
        end;
      end;
  {$ifdef LogErrorsToFile}
     {Set the message footer}
      LMsgPtr := AppendStringToBuffer(LeakMessageFooter, LMsgPtr, Length(LeakMessageFooter));
      {Append the message to the memory errors file}
      AppendEventLog(@LLeakMessage[0], UIntPtr(LMsgPtr) - UIntPtr(@LLeakMessage[1]));
  {$else}
      {Set the message footer}
      AppendStringToBuffer(LeakMessageFooter, LMsgPtr, Length(LeakMessageFooter));
  {$endif}
  {$ifdef UseOutputDebugString}
      OutputDebugStringA(LLeakMessage);
  {$endif}
  {$ifndef NoMessageBoxes}
      {Show the message}
      AppendStringToModuleName(LeakMessageTitle, LMessageTitleBuffer);
      ShowMessageBox(LLeakMessage, LMessageTitleBuffer);
  {$endif}
    end;
  end;
{$endif}
end;

{Returns statistics about the current state of the memory manager}
procedure GetMemoryManagerState(var AMemoryManagerState: TMemoryManagerState);
var
  LPMediumBlockPoolHeader: PMediumBlockPoolHeader;
  LPMediumBlock: Pointer;
  LInd: Integer;
  LBlockTypeIndex, LMediumBlockSize: Cardinal;
  LMediumBlockHeader, LLargeBlockSize: NativeUInt;
  LPLargeBlock: PLargeBlockHeader;
begin
  {Clear the structure}
  FillChar(AMemoryManagerState, SizeOf(AMemoryManagerState), 0);
  {Set the small block size stats}
  for LInd := 0 to NumSmallBlockTypes - 1 do
  begin
    AMemoryManagerState.SmallBlockTypeStates[LInd].InternalBlockSize :=
      SmallBlockTypes[LInd].BlockSize;
    AMemoryManagerState.SmallBlockTypeStates[LInd].UseableBlockSize :=
      SmallBlockTypes[LInd].BlockSize - BlockHeaderSize{$ifdef FullDebugMode} - FullDebugBlockOverhead{$endif};
    if NativeInt(AMemoryManagerState.SmallBlockTypeStates[LInd].UseableBlockSize) < 0 then
      AMemoryManagerState.SmallBlockTypeStates[LInd].UseableBlockSize := 0;
  end;
  {Lock all small block types}
  LockAllSmallBlockTypes;
  {Lock the medium blocks}
  LockMediumBlocks;
  {Step through all the medium block pools}
  LPMediumBlockPoolHeader := MediumBlockPoolsCircularList.NextMediumBlockPoolHeader;
  while LPMediumBlockPoolHeader <> @MediumBlockPoolsCircularList do
  begin
    {Add to the medium block used space}
    Inc(AMemoryManagerState.ReservedMediumBlockAddressSpace, MediumBlockPoolSize);
    LPMediumBlock := GetFirstMediumBlockInPool(LPMediumBlockPoolHeader);
    while LPMediumBlock <> nil do
    begin
      LMediumBlockHeader := PNativeUInt(PByte(LPMediumBlock) - BlockHeaderSize)^;
      {Is the block in use?}
      if LMediumBlockHeader and IsFreeBlockFlag = 0 then
      begin
        {Get the block size}
        LMediumBlockSize := LMediumBlockHeader and DropMediumAndLargeFlagsMask;
        if (LMediumBlockHeader and IsSmallBlockPoolInUseFlag) <> 0 then
        begin
          {Get the block type index}
          LBlockTypeIndex := (UIntPtr(PSmallBlockPoolHeader(LPMediumBlock).BlockType) - UIntPtr(@SmallBlockTypes[0])) div SizeOf(TSmallBlockType);
          {Subtract from medium block usage}
          Dec(AMemoryManagerState.ReservedMediumBlockAddressSpace, LMediumBlockSize);
          {Add it to the reserved space for the block size}
          Inc(AMemoryManagerState.SmallBlockTypeStates[LBlockTypeIndex].ReservedAddressSpace, LMediumBlockSize);
          {Add the usage for the pool}
          Inc(AMemoryManagerState.SmallBlockTypeStates[LBlockTypeIndex].AllocatedBlockCount,
            PSmallBlockPoolHeader(LPMediumBlock).BlocksInUse);
        end
        else
        begin
{$ifdef FullDebugMode}
          Dec(LMediumBlockSize, FullDebugBlockOverhead);
{$endif}
          Inc(AMemoryManagerState.AllocatedMediumBlockCount);
          Inc(AMemoryManagerState.TotalAllocatedMediumBlockSize, LMediumBlockSize - BlockHeaderSize);
        end;
      end;
      {Next medium block}
      LPMediumBlock := NextMediumBlock(LPMediumBlock);
    end;
    {Get the next medium block pool}
    LPMediumBlockPoolHeader := LPMediumBlockPoolHeader.NextMediumBlockPoolHeader;
  end;
  {Unlock medium blocks}
  MediumBlocksLocked := False;
  {Unlock all the small block types}
  for LInd := 0 to NumSmallBlockTypes - 1 do
    SmallBlockTypes[LInd].BlockTypeLocked := False;
  {Step through all the large blocks}
  LockLargeBlocks;
  LPLargeBlock := LargeBlocksCircularList.NextLargeBlockHeader;
  while LPLargeBlock <> @LargeBlocksCircularList do
  begin
    LLargeBlockSize := LPLargeBlock.BlockSizeAndFlags and DropMediumAndLargeFlagsMask;
    Inc(AMemoryManagerState.AllocatedLargeBlockCount);
    Inc(AMemoryManagerState.ReservedLargeBlockAddressSpace, LLargeBlockSize);
    Inc(AMemoryManagerState.TotalAllocatedLargeBlockSize, LPLargeBlock.UserAllocatedSize);
    {Get the next large block}
    LPLargeBlock := LPLargeBlock.NextLargeBlockHeader;
  end;
  LargeBlocksLocked := False;
end;

{Returns a summary of the information returned by GetMemoryManagerState}
procedure GetMemoryManagerUsageSummary(
  var AMemoryManagerUsageSummary: TMemoryManagerUsageSummary);
var
  LMMS: TMemoryManagerState;
  LAllocatedBytes, LReservedBytes: NativeUInt;
  LSBTIndex: Integer;
begin
  {Get the memory manager state}
  GetMemoryManagerState(LMMS);
  {Add up the totals}
  LAllocatedBytes := LMMS.TotalAllocatedMediumBlockSize
    + LMMS.TotalAllocatedLargeBlockSize;
  LReservedBytes := LMMS.ReservedMediumBlockAddressSpace
    + LMMS.ReservedLargeBlockAddressSpace;
  for LSBTIndex := 0 to NumSmallBlockTypes - 1 do
  begin
    Inc(LAllocatedBytes, LMMS.SmallBlockTypeStates[LSBTIndex].UseableBlockSize
      * LMMS.SmallBlockTypeStates[LSBTIndex].AllocatedBlockCount);
    Inc(LReservedBytes, LMMS.SmallBlockTypeStates[LSBTIndex].ReservedAddressSpace);
  end;
  {Set the structure values}
  AMemoryManagerUsageSummary.AllocatedBytes := LAllocatedBytes;
  AMemoryManagerUsageSummary.OverheadBytes := LReservedBytes - LAllocatedBytes;
  if LReservedBytes > 0 then
  begin
    AMemoryManagerUsageSummary.EfficiencyPercentage :=
      LAllocatedBytes / LReservedBytes * 100;
  end
  else
    AMemoryManagerUsageSummary.EfficiencyPercentage := 100;
end;

{$ifndef POSIX}
{Gets the state of every 64K block in the 4GB address space. Under 64-bit this
 returns only the state for the low 4GB.}
procedure GetMemoryMap(var AMemoryMap: TMemoryMap);
var
  LPMediumBlockPoolHeader: PMediumBlockPoolHeader;
  LPLargeBlock: PLargeBlockHeader;
  LInd, LChunkIndex, LNextChunk, LLargeBlockSize: NativeUInt;
  LMBI: TMemoryBasicInformation;
begin
  {Clear the map}
  FillChar(AMemoryMap, SizeOf(AMemoryMap), Ord(csUnallocated));
  {Step through all the medium block pools}
  LockMediumBlocks;
  LPMediumBlockPoolHeader := MediumBlockPoolsCircularList.NextMediumBlockPoolHeader;
  while LPMediumBlockPoolHeader <> @MediumBlockPoolsCircularList do
  begin
    {Add to the medium block used space}
    LChunkIndex := NativeUInt(LPMediumBlockPoolHeader) shr 16;
    for LInd := 0 to (MediumBlockPoolSize - 1) shr 16 do
    begin
      if (LChunkIndex + LInd) > High(AMemoryMap) then
        Break;
      AMemoryMap[LChunkIndex + LInd] := csAllocated;
    end;
    {Get the next medium block pool}
    LPMediumBlockPoolHeader := LPMediumBlockPoolHeader.NextMediumBlockPoolHeader;
  end;
  MediumBlocksLocked := False;
  {Step through all the large blocks}
  LockLargeBlocks;
  LPLargeBlock := LargeBlocksCircularList.NextLargeBlockHeader;
  while LPLargeBlock <> @LargeBlocksCircularList do
  begin
    LChunkIndex := UIntPtr(LPLargeBlock) shr 16;
    LLargeBlockSize := LPLargeBlock.BlockSizeAndFlags and DropMediumAndLargeFlagsMask;
    for LInd := 0 to (LLargeBlockSize - 1) shr 16 do
    begin
      if (LChunkIndex + LInd) > High(AMemoryMap) then
        Break;
      AMemoryMap[LChunkIndex + LInd] := csAllocated;
    end;
    {Get the next large block}
    LPLargeBlock := LPLargeBlock.NextLargeBlockHeader;
  end;
  LargeBlocksLocked := False;
  {Fill in the rest of the map}
  LInd := 0;
  while LInd <= 65535 do
  begin
    {If the chunk is not allocated by this MM, what is its status?}
    if AMemoryMap[LInd] = csUnallocated then
    begin
      {Query the address space starting at the chunk boundary}
      if VirtualQuery(Pointer(LInd * 65536), LMBI, SizeOf(LMBI)) = 0 then
      begin
        {VirtualQuery may fail for addresses >2GB if a large address space is
         not enabled.}
        FillChar(AMemoryMap[LInd], 65536 - LInd, csSysReserved);
        Break;
      end;
      {Get the chunk number after the region}
      LNextChunk := (LMBI.RegionSize - 1) shr 16 + LInd + 1;
      {Validate}
      if LNextChunk > 65536 then
        LNextChunk := 65536;
      {Set the status of all the chunks in the region}
      if LMBI.State = MEM_COMMIT then
      begin
        FillChar(AMemoryMap[LInd], LNextChunk - LInd, csSysAllocated);
      end
      else
      begin
        if LMBI.State = MEM_RESERVE then
          FillChar(AMemoryMap[LInd], LNextChunk - LInd, csSysReserved);
      end;
      {Point to the start of the next chunk}
      LInd := LNextChunk;
    end
    else
    begin
      {Next chunk}
      Inc(LInd);
    end;
  end;
end;
{$endif}

{Returns summarised information about the state of the memory manager. (For
 backward compatibility.)}
function FastGetHeapStatus: THeapStatus;
var
  LPMediumBlockPoolHeader: PMediumBlockPoolHeader;
  LPMediumBlock: Pointer;
  LBlockTypeIndex, LMediumBlockSize: Cardinal;
  LSmallBlockUsage, LSmallBlockOverhead, LMediumBlockHeader, LLargeBlockSize: NativeUInt;
  LInd: Integer;
  LPLargeBlock: PLargeBlockHeader;
begin
  {Clear the structure}
  FillChar(Result, SizeOf(Result), 0);
  {Lock all small block types}
  LockAllSmallBlockTypes;
  {Lock the medium blocks}
  LockMediumBlocks;
  {Step through all the medium block pools}
  LPMediumBlockPoolHeader := MediumBlockPoolsCircularList.NextMediumBlockPoolHeader;
  while LPMediumBlockPoolHeader <> @MediumBlockPoolsCircularList do
  begin
    {Add to the total and committed address space}
    Inc(Result.TotalAddrSpace, ((MediumBlockPoolSize + $ffff) and $ffff0000));
    Inc(Result.TotalCommitted, ((MediumBlockPoolSize + $ffff) and $ffff0000));
    {Add the medium block pool overhead}
    Inc(Result.Overhead, (((MediumBlockPoolSize + $ffff) and $ffff0000)
      - MediumBlockPoolSize + MediumBlockPoolHeaderSize));
    {Get the first medium block in the pool}
    LPMediumBlock := GetFirstMediumBlockInPool(LPMediumBlockPoolHeader);
    while LPMediumBlock <> nil do
    begin
      {Get the block header}
      LMediumBlockHeader := PNativeUInt(PByte(LPMediumBlock) - BlockHeaderSize)^;
      {Get the block size}
      LMediumBlockSize := LMediumBlockHeader and DropMediumAndLargeFlagsMask;
      {Is the block in use?}
      if LMediumBlockHeader and IsFreeBlockFlag = 0 then
      begin
        if (LMediumBlockHeader and IsSmallBlockPoolInUseFlag) <> 0 then
        begin
          {Get the block type index}
          LBlockTypeIndex := (UIntPtr(PSmallBlockPoolHeader(LPMediumBlock).BlockType) - UIntPtr(@SmallBlockTypes[0])) div SizeOf(TSmallBlockType);
          {Get the usage in the block}
          LSmallBlockUsage := PSmallBlockPoolHeader(LPMediumBlock).BlocksInUse
            * SmallBlockTypes[LBlockTypeIndex].BlockSize;
          {Get the total overhead for all the small blocks}
          LSmallBlockOverhead := PSmallBlockPoolHeader(LPMediumBlock).BlocksInUse
              * (BlockHeaderSize{$ifdef FullDebugMode} + FullDebugBlockOverhead{$endif});
          {Add to the totals}
          Inc(Result.FreeSmall, LMediumBlockSize - LSmallBlockUsage - BlockHeaderSize);
          Inc(Result.Overhead, LSmallBlockOverhead + BlockHeaderSize);
          Inc(Result.TotalAllocated, LSmallBlockUsage - LSmallBlockOverhead);
        end
        else
        begin
{$ifdef FullDebugMode}
          Dec(LMediumBlockSize, FullDebugBlockOverhead);
          Inc(Result.Overhead, FullDebugBlockOverhead);
{$endif}
          {Add to the result}
          Inc(Result.TotalAllocated, LMediumBlockSize - BlockHeaderSize);
          Inc(Result.Overhead, BlockHeaderSize);
        end;
      end
      else
      begin
        {The medium block is free}
        Inc(Result.FreeBig, LMediumBlockSize);
      end;
      {Next medium block}
      LPMediumBlock := NextMediumBlock(LPMediumBlock);
    end;
    {Get the next medium block pool}
    LPMediumBlockPoolHeader := LPMediumBlockPoolHeader.NextMediumBlockPoolHeader;
  end;
  {Add the sequential feed unused space}
  Inc(Result.Unused, MediumSequentialFeedBytesLeft);
  {Unlock the medium blocks}
  MediumBlocksLocked := False;
  {Unlock all the small block types}
  for LInd := 0 to NumSmallBlockTypes - 1 do
    SmallBlockTypes[LInd].BlockTypeLocked := False;
  {Step through all the large blocks}
  LockLargeBlocks;
  LPLargeBlock := LargeBlocksCircularList.NextLargeBlockHeader;
  while LPLargeBlock <> @LargeBlocksCircularList do
  begin
    LLargeBlockSize := LPLargeBlock.BlockSizeAndFlags and DropMediumAndLargeFlagsMask;
    Inc(Result.TotalAddrSpace, LLargeBlockSize);
    Inc(Result.TotalCommitted, LLargeBlockSize);
    Inc(Result.TotalAllocated, LPLargeBlock.UserAllocatedSize
      {$ifdef FullDebugMode} - FullDebugBlockOverhead{$endif});
    Inc(Result.Overhead, LLargeBlockSize - LPLargeBlock.UserAllocatedSize
      {$ifdef FullDebugMode} + FullDebugBlockOverhead{$endif});
    {Get the next large block}
    LPLargeBlock := LPLargeBlock.NextLargeBlockHeader;
  end;
  LargeBlocksLocked := False;
  {Set the total number of free bytes}
  Result.TotalFree := Result.FreeSmall + Result.FreeBig + Result.Unused;
end;

{Frees all allocated memory. Does not support segmented large blocks (yet).}
procedure FreeAllMemory;
var
  LPMediumBlockPoolHeader, LPNextMediumBlockPoolHeader: PMediumBlockPoolHeader;
  LPMediumFreeBlock: PMediumFreeBlock;
  LPLargeBlock, LPNextLargeBlock: PLargeBlockHeader;
  LInd: Integer;
begin
  {Free all block pools}
  LPMediumBlockPoolHeader := MediumBlockPoolsCircularList.NextMediumBlockPoolHeader;
  while LPMediumBlockPoolHeader <> @MediumBlockPoolsCircularList do
  begin
    {Get the next medium block pool so long}
    LPNextMediumBlockPoolHeader := LPMediumBlockPoolHeader.NextMediumBlockPoolHeader;
{$ifdef ClearMediumBlockPoolsBeforeReturningToOS}
    FillChar(LPMediumBlockPoolHeader^, MediumBlockPoolSize, 0);
{$else}
    {$ifdef ClearSmallAndMediumBlocksInFreeMem}
    FillChar(LPMediumBlockPoolHeader^, MediumBlockPoolSize, 0);
    {$endif}
{$endif}
    {Free this pool}
    VirtualFree(LPMediumBlockPoolHeader, 0, MEM_RELEASE);
    {Next pool}
    LPMediumBlockPoolHeader := LPNextMediumBlockPoolHeader;
  end;
  {Clear all small block types}
  for LInd := 0 to High(SmallBlockTypes) do
  begin
    SmallBlockTypes[Lind].PreviousPartiallyFreePool := @SmallBlockTypes[Lind];
    SmallBlockTypes[Lind].NextPartiallyFreePool := @SmallBlockTypes[Lind];
    SmallBlockTypes[Lind].NextSequentialFeedBlockAddress := Pointer(1);
    SmallBlockTypes[Lind].MaxSequentialFeedBlockAddress := nil;
  end;
  {Clear all medium block pools}
  MediumBlockPoolsCircularList.PreviousMediumBlockPoolHeader := @MediumBlockPoolsCircularList;
  MediumBlockPoolsCircularList.NextMediumBlockPoolHeader := @MediumBlockPoolsCircularList;
  {All medium bins are empty}
  for LInd := 0 to High(MediumBlockBins) do
  begin
    LPMediumFreeBlock := @MediumBlockBins[LInd];
    LPMediumFreeBlock.PreviousFreeBlock := LPMediumFreeBlock;
    LPMediumFreeBlock.NextFreeBlock := LPMediumFreeBlock;
  end;
  MediumBlockBinGroupBitmap := 0;
  FillChar(MediumBlockBinBitmaps, SizeOf(MediumBlockBinBitmaps), 0);
  MediumSequentialFeedBytesLeft := 0;
  {Free all large blocks}
  LPLargeBlock := LargeBlocksCircularList.NextLargeBlockHeader;
  while LPLargeBlock <> @LargeBlocksCircularList do
  begin
    {Get the next large block}
    LPNextLargeBlock := LPLargeBlock.NextLargeBlockHeader;
{$ifdef ClearLargeBlocksBeforeReturningToOS}
    FillChar(LPLargeBlock^,
      LPLargeBlock.BlockSizeAndFlags and DropMediumAndLargeFlagsMask, 0);
{$endif}
    {Free this large block}
    VirtualFree(LPLargeBlock, 0, MEM_RELEASE);
    {Next large block}
    LPLargeBlock := LPNextLargeBlock;
  end;
  {There are no large blocks allocated}
  LargeBlocksCircularList.PreviousLargeBlockHeader := @LargeBlocksCircularList;
  LargeBlocksCircularList.NextLargeBlockHeader := @LargeBlocksCircularList;
end;

{----------------------------Memory Manager Setup-----------------------------}

{Checks that no other memory manager has been installed after the RTL MM and
 that there are currently no live pointers allocated through the RTL MM.}
function CheckCanInstallMemoryManager: Boolean;
{$ifndef NoMessageBoxes}
var
  LErrorMessageTitle: array[0..1023] of AnsiChar;
{$endif}
begin
  {Default to error}
  Result := False;
{$ifdef FullDebugMode}
  {$ifdef LoadDebugDLLDynamically}
    {$ifdef DoNotInstallIfDLLMissing}
  {Should FastMM be installed only if the FastMM_FullDebugMode.dll file is
   available?}
  if FullDebugModeDLL = 0 then
    Exit;
    {$endif}
  {$endif}
{$endif}
  {Is FastMM already installed?}
  if FastMMIsInstalled then
  begin
{$ifdef UseOutputDebugString}
    OutputDebugStringA(AlreadyInstalledMsg);
{$endif}
{$ifndef NoMessageBoxes}
    AppendStringToModuleName(AlreadyInstalledTitle, LErrorMessageTitle);
    ShowMessageBox(AlreadyInstalledMsg, LErrorMessageTitle);
{$endif}
    Exit;
  end;
  {Has another MM been set, or has the Embarcadero MM been used? If so, this
   file is not the first unit in the uses clause of the project's .dpr file.}
  if IsMemoryManagerSet then
  begin
    {When using runtime packages, another library may already have installed
     FastMM: Silently ignore the installation request.}
{$ifndef UseRuntimePackages}
    {Another memory manager has been set.}
  {$ifdef UseOutputDebugString}
    OutputDebugStringA(OtherMMInstalledMsg);
  {$endif}
  {$ifndef NoMessageBoxes}
    AppendStringToModuleName(OtherMMInstalledTitle, LErrorMessageTitle);
    ShowMessageBox(OtherMMInstalledMsg, LErrorMessageTitle);
  {$endif}
{$endif}
    Exit;
  end;
{$ifndef POSIX}
  if GetHeapStatus.TotalAllocated <> 0 then
  begin
    {Memory has been already been allocated with the RTL MM}
{$ifdef UseOutputDebugString}
    OutputDebugStringA(MemoryAllocatedMsg);
{$endif}
  {$ifndef NoMessageBoxes}
    AppendStringToModuleName(MemoryAllocatedTitle, LErrorMessageTitle);
    ShowMessageBox(MemoryAllocatedMsg, LErrorMessageTitle);
  {$endif}
    Exit;
  end;
{$endif}
  {All OK}
  Result := True;
end;

{Initializes the lookup tables for the memory manager}
procedure InitializeMemoryManager;
const
  {The size of the Inc(VMTIndex) code in TFreedObject.GetVirtualMethodIndex}
  VMTIndexIncCodeSize = 6;
var
  LInd, LSizeInd, LMinimumPoolSize, LOptimalPoolSize, LGroupNumber,
    LBlocksPerPool, LPreviousBlockSize: Cardinal;
  LPMediumFreeBlock: PMediumFreeBlock;
begin
{$ifdef FullDebugMode}
  {$ifdef LoadDebugDLLDynamically}
  {Attempt to load the FullDebugMode DLL dynamically.}
  FullDebugModeDLL := LoadLibrary(FullDebugModeLibraryName);
  if FullDebugModeDLL <> 0 then
  begin
    GetStackTrace := GetProcAddress(FullDebugModeDLL,
      {$ifdef RawStackTraces}'GetRawStackTrace'{$else}'GetFrameBasedStackTrace'{$endif});
    LogStackTrace := GetProcAddress(FullDebugModeDLL, 'LogStackTrace');
  end;
  {$endif}
{$endif}
{$ifdef EnableMMX}
  {$ifndef ForceMMX}
  UseMMX := MMX_Supported;
  {$endif}
{$endif}
  {Initialize the memory manager}
  {-------------Set up the small block types-------------}
  LPreviousBlockSize := 0;
  for LInd := 0 to High(SmallBlockTypes) do
  begin
    {Set the move procedure}
{$ifdef UseCustomFixedSizeMoveRoutines}
    {The upsize move procedure may move chunks in 16 bytes even with 8-byte
    alignment, since the new size will always be at least 8 bytes bigger than
    the old size.}
    if not Assigned(SmallBlockTypes[LInd].UpsizeMoveProcedure) then
  {$ifdef UseCustomVariableSizeMoveRoutines}
      SmallBlockTypes[LInd].UpsizeMoveProcedure := MoveX16LP;
  {$else}
      SmallBlockTypes[LInd].UpsizeMoveProcedure := @System.Move;
  {$endif}
{$endif}
    {Set the first "available pool" to the block type itself, so that the
     allocation routines know that there are currently no pools with free
     blocks of this size.}
    SmallBlockTypes[LInd].PreviousPartiallyFreePool := @SmallBlockTypes[LInd];
    SmallBlockTypes[LInd].NextPartiallyFreePool := @SmallBlockTypes[LInd];
    {Set the block size to block type index translation table}
    for LSizeInd := (LPreviousBlockSize div SmallBlockGranularity) to ((SmallBlockTypes[LInd].BlockSize - 1) div SmallBlockGranularity) do
      AllocSize2SmallBlockTypeIndX4[LSizeInd] := LInd * 4;
    {Cannot sequential feed yet: Ensure that the next address is greater than
     the maximum address}
    SmallBlockTypes[LInd].MaxSequentialFeedBlockAddress := Pointer(0);
    SmallBlockTypes[LInd].NextSequentialFeedBlockAddress := Pointer(1);
    {Get the mask to use for finding a medium block suitable for a block pool}
    LMinimumPoolSize :=
      ((SmallBlockTypes[LInd].BlockSize * MinimumSmallBlocksPerPool
        + SmallBlockPoolHeaderSize + MediumBlockGranularity - 1 - MediumBlockSizeOffset)
      and -MediumBlockGranularity) + MediumBlockSizeOffset;
    if LMinimumPoolSize < MinimumMediumBlockSize then
      LMinimumPoolSize := MinimumMediumBlockSize;
    {Get the closest group number for the minimum pool size}
    LGroupNumber := (LMinimumPoolSize - MinimumMediumBlockSize + MediumBlockBinsPerGroup * MediumBlockGranularity div 2)
      div (MediumBlockBinsPerGroup * MediumBlockGranularity);
    {Too large?}
    if LGroupNumber > 7 then
      LGroupNumber := 7;
    {Set the bitmap}
    SmallBlockTypes[LInd].AllowedGroupsForBlockPoolBitmap := Byte(-(1 shl LGroupNumber));
    {Set the minimum pool size}
    SmallBlockTypes[LInd].MinimumBlockPoolSize := MinimumMediumBlockSize + LGroupNumber * (MediumBlockBinsPerGroup * MediumBlockGranularity);
    {Get the optimal block pool size}
    LOptimalPoolSize := ((SmallBlockTypes[LInd].BlockSize * TargetSmallBlocksPerPool
        + SmallBlockPoolHeaderSize + MediumBlockGranularity - 1 - MediumBlockSizeOffset)
      and -MediumBlockGranularity) + MediumBlockSizeOffset;
    {Limit the optimal pool size to within range}
    if LOptimalPoolSize < OptimalSmallBlockPoolSizeLowerLimit then
      LOptimalPoolSize := OptimalSmallBlockPoolSizeLowerLimit;
    if LOptimalPoolSize > OptimalSmallBlockPoolSizeUpperLimit then
      LOptimalPoolSize := OptimalSmallBlockPoolSizeUpperLimit;
    {How many blocks will fit in the adjusted optimal size?}
    LBlocksPerPool := (LOptimalPoolSize - SmallBlockPoolHeaderSize) div SmallBlockTypes[LInd].BlockSize;
    {Recalculate the optimal pool size to minimize wastage due to a partial
     last block.}
    SmallBlockTypes[LInd].OptimalBlockPoolSize :=
      ((LBlocksPerPool * SmallBlockTypes[LInd].BlockSize + SmallBlockPoolHeaderSize + MediumBlockGranularity - 1 - MediumBlockSizeOffset) and -MediumBlockGranularity) + MediumBlockSizeOffset;
{$ifdef CheckHeapForCorruption}
    {Debug checks}
    if (SmallBlockTypes[LInd].OptimalBlockPoolSize < MinimumMediumBlockSize)
      or (SmallBlockTypes[LInd].BlockSize div SmallBlockGranularity * SmallBlockGranularity <> SmallBlockTypes[LInd].BlockSize) then
    begin
  {$ifdef BCB6OrDelphi7AndUp}
      System.Error(reInvalidPtr);
  {$else}
      System.RunError(reInvalidPtr);
  {$endif}
    end;
{$endif}
    {Set the previous small block size}
    LPreviousBlockSize := SmallBlockTypes[LInd].BlockSize;
  end;
  {-------------------Set up the medium blocks-------------------}
{$ifdef CheckHeapForCorruption}
  {Check that there are no gaps between where the small blocks end and the
   medium blocks start}
  if (((MaximumSmallBlockSize - 3) + (MediumBlockGranularity - 1 + BlockHeaderSize - MediumBlockSizeOffset))
    and -MediumBlockGranularity) + MediumBlockSizeOffset < MinimumMediumBlockSize then
  begin
  {$ifdef BCB6OrDelphi7AndUp}
    System.Error(reInvalidPtr);
  {$else}
    System.RunError(reInvalidPtr);
  {$endif}
  end;
{$endif}
  {There are currently no medium block pools}
  MediumBlockPoolsCircularList.PreviousMediumBlockPoolHeader := @MediumBlockPoolsCircularList;
  MediumBlockPoolsCircularList.NextMediumBlockPoolHeader := @MediumBlockPoolsCircularList;
  {All medium bins are empty}
  for LInd := 0 to High(MediumBlockBins) do
  begin
    LPMediumFreeBlock := @MediumBlockBins[LInd];
    LPMediumFreeBlock.PreviousFreeBlock := LPMediumFreeBlock;
    LPMediumFreeBlock.NextFreeBlock := LPMediumFreeBlock;
  end;
  {------------------Set up the large blocks---------------------}
  LargeBlocksCircularList.PreviousLargeBlockHeader := @LargeBlocksCircularList;
  LargeBlocksCircularList.NextLargeBlockHeader := @LargeBlocksCircularList;
  {------------------Set up the debugging structures---------------------}
{$ifdef FullDebugMode}
  {Set up the fake VMT}
  {Copy the basic info from the TFreedObject class}
  System.Move(Pointer(PByte(TFreedObject) + vmtSelfPtr + SizeOf(Pointer))^,
    FreedObjectVMT.VMTData[vmtSelfPtr + SizeOf(Pointer)], vmtParent - vmtSelfPtr);
  PNativeUInt(@FreedObjectVMT.VMTData[vmtSelfPtr])^ := NativeUInt(@FreedObjectVMT.VMTMethods[0]);
  {Set up the virtual method table}
  for LInd := 0 to MaxFakeVMTEntries - 1 do
  begin
    PNativeUInt(@FreedObjectVMT.VMTMethods[Low(FreedObjectVMT.VMTMethods) + Integer(LInd * SizeOf(Pointer))])^ :=
      NativeUInt(@TFreedObject.GetVirtualMethodIndex) + LInd * VMTIndexIncCodeSize;
  {$ifdef CatchUseOfFreedInterfaces}
    VMTBadInterface[LInd] := @TFreedObject.InterfaceError;
  {$endif}
  end;
  {Set up the default log file name}
  SetDefaultMMLogFileName;
{$endif}
end;

{Installs the memory manager (InitializeMemoryManager should be called first)}
procedure InstallMemoryManager;
{$ifdef MMSharingEnabled}
var
  i, LCurrentProcessID: Cardinal;
  LPMapAddress: PPointer;
  LChar: AnsiChar;
{$endif}
begin
  if not FastMMIsInstalled then
  begin
{$ifdef FullDebugMode}
  {$ifdef 32Bit}
    {Try to reserve the 64K block covering address $80808080}
    ReservedBlock := VirtualAlloc(Pointer(DebugReservedAddress), 65536, MEM_RESERVE, PAGE_NOACCESS);
  {$endif}
{$endif}
{$ifdef MMSharingEnabled}
    {Build a string identifying the current process}
    LCurrentProcessID := GetCurrentProcessId;
    for i := 0 to 7 do
    begin
      LChar := HexTable[((LCurrentProcessID shr (i * 4)) and $F)];
      MappingObjectName[(High(MappingObjectName) - 1) - i] := LChar;
  {$ifdef EnableBackwardCompatibleMMSharing}
      UniqueProcessIDString[8 - i] := LChar;
      UniqueProcessIDStringBE[8 - i] := LChar;
  {$endif}
    end;
{$endif}
{$ifdef AttemptToUseSharedMM}
    {Is the replacement memory manager already installed for this process?}
{$ifdef EnableBackwardCompatibleMMSharing}
    MMWindow := FindWindowA('STATIC', PAnsiChar(@UniqueProcessIDString[1]));
    MMWindowBE := FindWindowA('STATIC', PAnsiChar(@UniqueProcessIDStringBE[1]));
{$endif}
    MappingObjectHandle := OpenFileMappingA(FILE_MAP_READ, False, MappingObjectName);
    {Is no MM being shared?}
{$ifdef EnableBackwardCompatibleMMSharing}
    if (MMWindow or MMWindowBE or MappingObjectHandle) = 0 then
{$else}
    if MappingObjectHandle = 0 then
{$endif}
    begin
{$endif}
{$ifdef ShareMM}
      {Share the MM with other DLLs? - if this DLL is unloaded, then
       dependent DLLs will cause a crash.}
  {$ifndef ShareMMIfLibrary}
      if not IsLibrary then
  {$endif}
      begin
  {$ifdef EnableBackwardCompatibleMMSharing}
        {No memory manager installed yet - create the invisible window}
        MMWindow := CreateWindowA('STATIC', PAnsiChar(@UniqueProcessIDString[1]),
          WS_POPUP, 0, 0, 0, 0, 0, 0, hInstance, nil);
        MMWindowBE := CreateWindowA('STATIC', PAnsiChar(@UniqueProcessIDStringBE[1]),
          WS_POPUP, 0, 0, 0, 0, 0, 0, hInstance, nil);
        {The window data is a pointer to this memory manager}
        if MMWindow <> 0 then
          SetWindowLongA(MMWindow, GWL_USERDATA, NativeInt(@NewMemoryManager));
        if MMWindowBE <> 0 then
          SetWindowLongA(MMWindowBE, GWL_USERDATA, NativeInt(@NewMemoryManager));
  {$endif}
        {Create the memory mapped file}
        MappingObjectHandle := CreateFileMappingA(INVALID_HANDLE_VALUE, nil,
          PAGE_READWRITE, 0, SizeOf(Pointer), MappingObjectName);
        {Map a view of the memory}
        LPMapAddress := MapViewOfFile(MappingObjectHandle, FILE_MAP_WRITE, 0, 0, 0);
        {Set a pointer to the new memory manager}
        LPMapAddress^ := @NewMemoryManager;
        {Unmap the file}
        UnmapViewOfFile(LPMapAddress);
      end;
{$endif}
      {We will be using this memory manager}
{$ifndef FullDebugMode}
      NewMemoryManager.GetMem := FastGetMem;
      NewMemoryManager.FreeMem := FastFreeMem;
      NewMemoryManager.ReallocMem := FastReallocMem;
{$else}
      NewMemoryManager.GetMem := DebugGetMem;
      NewMemoryManager.FreeMem := DebugFreeMem;
      NewMemoryManager.ReallocMem := DebugReallocMem;
{$endif}
{$ifdef BDS2006AndUp}
  {$ifndef FullDebugMode}
      NewMemoryManager.AllocMem := FastAllocMem;
  {$else}
      NewMemoryManager.AllocMem := DebugAllocMem;
  {$endif}
  {$ifdef EnableMemoryLeakReporting}
      NewMemoryManager.RegisterExpectedMemoryLeak := RegisterExpectedMemoryLeak;
      NewMemoryManager.UnRegisterExpectedMemoryLeak := UnRegisterExpectedMemoryLeak;
  {$else}
      NewMemoryManager.RegisterExpectedMemoryLeak := NoOpRegisterExpectedMemoryLeak;
      NewMemoryManager.UnRegisterExpectedMemoryLeak := NoOpUnRegisterExpectedMemoryLeak;
  {$endif}
{$endif}
      {Owns the memory manager}
      IsMemoryManagerOwner := True;
{$ifdef AttemptToUseSharedMM}
    end
    else
    begin
      {Get the address of the shared memory manager}
  {$ifndef BDS2006AndUp}
    {$ifdef EnableBackwardCompatibleMMSharing}
      if MappingObjectHandle <> 0 then
      begin
    {$endif}
        {Map a view of the memory}
        LPMapAddress := MapViewOfFile(MappingObjectHandle, FILE_MAP_READ, 0, 0, 0);
        {Set the new memory manager}
        NewMemoryManager := PMemoryManager(LPMapAddress^)^;
        {Unmap the file}
        UnmapViewOfFile(LPMapAddress);
    {$ifdef EnableBackwardCompatibleMMSharing}
      end
      else
      begin
        if MMWindow <> 0 then
        begin
          NewMemoryManager := PMemoryManager(GetWindowLong(MMWindow, GWL_USERDATA))^;
        end
        else
        begin
          NewMemoryManager := PMemoryManager(GetWindowLong(MMWindowBE, GWL_USERDATA))^;
        end;
      end;
    {$endif}
  {$else}
    {$ifdef EnableBackwardCompatibleMMSharing}
      if MappingObjectHandle <> 0 then
      begin
    {$endif}
        {Map a view of the memory}
        LPMapAddress := MapViewOfFile(MappingObjectHandle, FILE_MAP_READ, 0, 0, 0);
        {Set the new memory manager}
        NewMemoryManager := PMemoryManagerEx(LPMapAddress^)^;
        {Unmap the file}
        UnmapViewOfFile(LPMapAddress);
    {$ifdef EnableBackwardCompatibleMMSharing}
      end
      else
      begin
        if MMWindow <> 0 then
        begin
          NewMemoryManager := PMemoryManagerEx(GetWindowLong(MMWindow, GWL_USERDATA))^;
        end
        else
        begin
          NewMemoryManager := PMemoryManagerEx(GetWindowLong(MMWindowBE, GWL_USERDATA))^;
        end;
      end;
    {$endif}
  {$endif}
      {Close the file mapping handle}
      CloseHandle(MappingObjectHandle);
      MappingObjectHandle := 0;
      {The memory manager is not owned by this module}
      IsMemoryManagerOwner := False;
    end;
{$endif}
    {Save the old memory manager}
    GetMemoryManager(OldMemoryManager);
    {Replace the memory manager with either this one or the shared one.}
    SetMemoryManager(NewMemoryManager);
    {FastMM is now installed}
    FastMMIsInstalled := True;
{$ifdef UseOutputDebugString}
    if IsMemoryManagerOwner then
      OutputDebugStringA(FastMMInstallMsg)
    else
      OutputDebugStringA(FastMMInstallSharedMsg);
{$endif}
  end;
end;

procedure UninstallMemoryManager;
begin
  {Is this the owner of the shared MM window?}
  if IsMemoryManagerOwner then
  begin
{$ifdef ShareMM}
  {$ifdef EnableBackwardCompatibleMMSharing}
    {Destroy the window}
    if MMWindow <> 0 then
    begin
      DestroyWindow(MMWindow);
      MMWindow := 0;
    end;
    if MMWindowBE <> 0 then
    begin
      DestroyWindow(MMWindowBE);
      MMWindowBE := 0;
    end;
  {$endif}
    {Destroy the memory mapped file handle}
    if MappingObjectHandle <> 0 then
    begin
      CloseHandle(MappingObjectHandle);
      MappingObjectHandle := 0;
    end;
{$endif}
{$ifdef FullDebugMode}
    {Release the reserved block}
    if ReservedBlock <> nil then
    begin
      VirtualFree(ReservedBlock, 0, MEM_RELEASE);
      ReservedBlock := nil;
    end;
{$endif}
  end;
{$ifndef DetectMMOperationsAfterUninstall}
  {Restore the old memory manager}
  SetMemoryManager(OldMemoryManager);
{$else}
  {Set the invalid memory manager: no more MM operations allowed}
  SetMemoryManager(InvalidMemoryManager);
{$endif}
  {Memory manager has been uninstalled}
  FastMMIsInstalled := False;
{$ifdef UseOutputDebugString}
  if IsMemoryManagerOwner then
    OutputDebugStringA(FastMMUninstallMsg)
  else
    OutputDebugStringA(FastMMUninstallSharedMsg);
{$endif}
end;

procedure FinalizeMemoryManager;
begin
  {Restore the old memory manager if FastMM has been installed}
  if FastMMIsInstalled then
  begin
{$ifndef NeverUninstall}
    {Uninstall FastMM}
    UninstallMemoryManager;
{$endif}
    {Do we own the memory manager, or are we just sharing it?}
    if IsMemoryManagerOwner then
    begin
{$ifdef CheckUseOfFreedBlocksOnShutdown}
      CheckBlocksOnShutdown(
  {$ifdef EnableMemoryLeakReporting}
        True
    {$ifdef RequireIDEPresenceForLeakReporting}
        and DelphiIsRunning
    {$endif}
    {$ifdef RequireDebuggerPresenceForLeakReporting}
        and ((DebugHook <> 0)
        {$ifdef PatchBCBTerminate}
        or (Assigned(pCppDebugHook) and (pCppDebugHook^ <> 0))
        {$endif PatchBCBTerminate}
        )
    {$endif}
    {$ifdef ManualLeakReportingControl}
        and ReportMemoryLeaksOnShutdown
    {$endif}
  {$else}
        False
  {$endif}
      );
{$else}
  {$ifdef EnableMemoryLeakReporting}
      if True
    {$ifdef RequireIDEPresenceForLeakReporting}
        and DelphiIsRunning
    {$endif}
    {$ifdef RequireDebuggerPresenceForLeakReporting}
        and ((DebugHook <> 0)
        {$ifdef PatchBCBTerminate}
        or (Assigned(pCppDebugHook) and (pCppDebugHook^ <> 0))
        {$endif PatchBCBTerminate}
        )
    {$endif}
    {$ifdef ManualLeakReportingControl}
        and ReportMemoryLeaksOnShutdown
    {$endif}
      then
        CheckBlocksOnShutdown(True);
  {$endif}
{$endif}
{$ifdef EnableMemoryLeakReporting}
      {Free the expected memory leaks list}
      if ExpectedMemoryLeaks <> nil then
      begin
        VirtualFree(ExpectedMemoryLeaks, 0, MEM_RELEASE);
        ExpectedMemoryLeaks := nil;
      end;
{$endif}
{$ifndef NeverUninstall}
      {Clean up: Free all memory. If this is a .DLL that owns its own MM, then
       it is necessary to prevent the main application from running out of
       address space.}
      FreeAllMemory;
{$endif}
    end;
  end;
end;

procedure RunInitializationCode;
begin
  {Only run this code once during startup.}
  if InitializationCodeHasRun then
    Exit;
  InitializationCodeHasRun := True;
{$ifndef BCB}
  {$ifdef InstallOnlyIfRunningInIDE}
  if (DebugHook <> 0) and DelphiIsRunning then
  {$endif}
  begin
    {Initialize all the lookup tables, etc. for the memory manager}
    InitializeMemoryManager;
    {Has another MM been set, or has the Embarcadero MM been used? If so, this
     file is not the first unit in the uses clause of the project's .dpr
     file.}
    if CheckCanInstallMemoryManager then
    begin
    {$ifdef ClearLogFileOnStartup}
      DeleteEventLog;
    {$endif}
      InstallMemoryManager;
    end;
  end;
{$endif}
end;

initialization
  RunInitializationCode;

finalization
{$ifndef PatchBCBTerminate}
  FinalizeMemoryManager;
{$endif}

end.