{

FastMM 5.02

Description:
  A fast replacement memory manager for Embarcadero Delphi applications that scales well across multiple threads and CPU
  cores, is not prone to memory fragmentation, and supports shared memory without the use of external .DLL files.

Developed by:
  Pierre le Riche

Sponsored by:
  gs-soft AG

Homepage:
  https://github.com/pleriche/FastMM5

Licence:
  FastMM 5 is dual-licensed.  You may choose to use it under the restrictions of the GPL v3 licence at no cost to you,
  or you may purchase a commercial licence.  A commercial licence grants you the right to use FastMM5 in your own
  applications, royalty free, and without any requirement to disclose your source code nor any modifications to FastMM
  to any other party.  A commercial licence lasts into perpetuity, and entitles you to all future updates, free of
  charge.  A commercial licence is sold per developer developing applications that use FastMM, as follows:
    1 developer = $99
    2 developers = $189
    3 developers = $269
    4 developers = $339
    5 developers = $399
    >5 developers = $399 + $50 per developer from the 6th onwards
    site licence = $999 (unlimited number of developers affiliated with the owner of the licence, i.e. employees,
    co-workers and contractors)

  Please send an e-mail to fastmm@leriche.org to request an invoice before or after payment is made.  Payment may be
  made via PayPal at https://www.paypal.me/fastmm (paypal@leriche.org), or via bank transfer.  Bank details will be
  provided on the invoice.

  Support (via e-mail) is available for users with a commercial licence.  Enhancement requests submitted by users with a
  commercial licence will be prioritized.


Usage Instructions:
  Add FastMM5.pas as the first unit in your project's DPR file.  It will install itself automatically during startup,
  replacing the default memory manager.

  In order to share the memory manager between the main application and libraries call
  FastMM_AttemptToUseSharedMemoryManager (in order to use the memory manager of another module in the process) or
  FastMM_ShareMemoryManager (to share the memory manager instance of the current module with other modules).  It is
  important to share the memory manager between modules where memory allocated in the one module may be freed by the
  other.

  If the application requires memory alignment greater than the default, call FastMM_EnterMinimumAddressAlignment and
  once the greater alignment is no longer required call FastMM_ExitMinimumAddressAlignment.  Calls may be nested.  The
  coarsest memory alignment requested takes precedence.

  At the cost of performance and increased memory usage FastMM can log additional metadata together with every block.
  In order to enable this mode call FastMM_EnterDebugMode and to exit debug mode call FastMM_ExitDebugMode.  Calls may
  be nested in which case debug mode will be active as long as the number of FastMM_EnterDebugMode calls exceed the
  number of FastMM_ExitDebugMode calls.  In debug mode freed memory blocks will be filled with the byte pattern
  $808080... so that usage of a freed memory block or object, as well as corruption of the block header and/or footer
  will likely be detected.  If the debug support library, FastMM_FullDebugMode.dll, is available and the application has
  not specified its own handlers for FastMM_GetStackTrace and FastMM_ConvertStackTraceToText then the support library
  will be loaded during the first call to FastMM_EnterDebugMode.

  Events (memory leaks, errors, etc.) may be logged to file, displayed on-screen, passed to the debugger or any
  combination of the three.  Specify how each event should be handled via the FastMM_LogToFileEvents,
  FastMM_MessageBoxEvents and FastMM_OutputDebugStringEvents variables.  The default event log filename will be built
  from the application filepath, but may be overridden via FastMM_SetEventLogFilename.  Messages are built from
  templates that may be changed/translated by the application.

  The optimization strategy of the memory manager may be tuned via FastMM_SetOptimizationStrategy.  It can be set to
  favour performance, low memory usage, or a blend of both.  The default strategy is to blend the performance and low
  memory usage goals.

Supported Compilers:
  Delphi XE3 and later

Supported Platforms:
  Windows, 32-bit and 64-bit

}

unit FastMM5;

interface

uses
  Winapi.Windows;

{$RangeChecks Off}
{$BoolEval Off}
{$OverflowChecks Off}
{$Optimization On}
{$StackFrames Off}
{$TypedAddress Off}
{$LongStrings On}
{$Align 8}

{Calling the deprecated GetHeapStatus is unavoidable, so suppress the warning.}
{$warn Symbol_Deprecated Off}
{$warn Symbol_Platform Off}

{$if SizeOf(Pointer) = 8}
  {$define 64Bit}
{$else}
  {$define 32Bit}
{$endif}

{$ifdef CPUX86}
  {$ifndef PurePascal}
    {$define X86ASM}
  {$endif}
{$else}
  {$ifdef CPUX64}
    {$ifndef PurePascal}
      {$define X64ASM}
    {$endif}
  {$else}
    {x86/x64 CPUs do not reorder writes, but ARM CPUs do.}
    {$define WeakMemoryOrdering}
    {$define PurePascal}
  {$endif}
{$endif}

const

  {The current version of FastMM.  The first digit is the major version, followed by a two digit minor version number.}
  CFastMM_Version = 502;

  {The number of arenas for small, medium and large blocks.  Increasing the number of arenas decreases the likelihood
  of thread contention happening (when the number of threads inside a GetMem call is greater than the number of arenas),
  at a slightly higher fixed cost per GetMem call.  Usually two threads can be served simultaneously from the same arena
  (a new block can be split off for one thread while a freed block can be recycled for the other), so the optimal number
  of arenas is usually somewhere between 0.5x and 1x the number of threads.  If you suspect that thread contention may
  be dragging down performance, inspect the FastMM_...BlockThreadContentionCount variables - if their numbers are high
  then an increase in the number of arenas will reduce thread contention.}
  CFastMM_SmallBlockArenaCount = 4;
  CFastMM_MediumBlockArenaCount = 4;
  CFastMM_LargeBlockArenaCount = 8;

  {The number of entries per stack trace differs between 32-bit and 64-bit in order to ensure that the debug header is
  always a multiple of 64 bytes.}
{$ifdef 32Bit}
  CFastMM_StackTraceEntryCount = 19; //8 stack trace entries per 64 bytes
{$else}
  CFastMM_StackTraceEntryCount = 20; //4 stack trace entries per 64 bytes
{$endif}

type

  {The optimization strategy for the memory manager.}
  TFastMM_MemoryManagerOptimizationStrategy = (mmosOptimizeForSpeed, mmosBalanced, mmosOptimizeForLowMemoryUsage);

  TFastMM_MemoryManagerEventType = (
    {Another third party memory manager has already been installed.}
    mmetAnotherThirdPartyMemoryManagerAlreadyInstalled,
    {FastMM cannot be installed, because memory has already been allocated through the default memory manager.}
    mmetCannotInstallAfterDefaultMemoryManagerHasBeenUsed,
    {When an attempt is made to install or use a shared memory manager, but the memory manager has already been used to
    allocate memory.}
    mmetCannotSwitchToSharedMemoryManagerWithLivePointers,
    {Details about an individual memory leak.}
    mmetUnexpectedMemoryLeakDetail,
    {Summary of memory leaks}
    mmetUnexpectedMemoryLeakSummary,
    {When an attempt to free or reallocate a debug block that has already been freed is detected.}
    mmetDebugBlockDoubleFree,
    mmetDebugBlockReallocOfFreedBlock,
    {When a corruption of the memory pool is detected.}
    mmetDebugBlockHeaderCorruption,
    mmetDebugBlockFooterCorruption,
    mmetDebugBlockModifiedAfterFree,
    {When a virtual method is called on a freed object.}
    mmetVirtualMethodCallOnFreedObject);
  TFastMM_MemoryManagerEventTypeSet = set of TFastMM_MemoryManagerEventType;

  TFastMM_MemoryManagerInstallationState = (
    {The default memory manager is currently in use.}
    mmisDefaultMemoryManagerInUse,
    {Another third party memory manager has been installed.}
    mmisOtherThirdPartyMemoryManagerInstalled,
    {A shared memory manager is being used.}
    mmisUsingSharedMemoryManager,
    {This memory manager has been installed.}
    mmisInstalled);

  TFastMM_StackTrace = array[0..CFastMM_StackTraceEntryCount - 1] of NativeUInt;

  {The debug block header.  Must be a multiple of 64 in order to guarantee that minimum block alignment restrictions
  are honoured.}
{$PointerMath On}
  PFastMM_DebugBlockHeader = ^TFastMM_DebugBlockHeader;
{$PointerMath Off}
  TFastMM_DebugBlockHeader = packed record
    {The first two pointer sized slots cannot be used by the debug block header.  The medium block manager uses the
    first two pointers in a free block for the free block linked list, and the small block manager uses the first
    pointer for the free block linked list.  This space is thus reserved.}
    Reserved1: Pointer;
    Reserved2: Pointer;
    {The user requested size for the block.}
    UserSize: NativeInt;
    {The object class this block was used for the previous time it was allocated.  When a block is freed, the pointer
    that would normally be in the space of the class pointer is copied here, so if it is detected that the block was
    used after being freed we have an idea what class it is.}
    PreviouslyUsedByClass: Pointer;
    {The call stack when the block was allocated}
    AllocationStackTrace: TFastMM_StackTrace;
    {The call stack when the block was freed}
    FreeStackTrace: TFastMM_StackTrace;
    {The value of the FastMM_CurrentAllocationGroup when the block was allocated.  Can be used in the debugging process
    to group related memory leaks together.}
    AllocationGroup: Cardinal;
    {The allocation number:  All debug mode allocations are numbered sequentially.  This number may be useful in memory
    leak analysis.  If it reaches 4G it wraps back to 0.}
    AllocationNumber: Cardinal;
    {The ID of the thread that allocated the block}
    AllocatedByThread: Cardinal;
    {The ID of the thread that freed the block}
    FreedByThread: Cardinal;
    {The sum of the dwords(32-bit)/qwords(64-bit) in this structure starting after the initial two reserved fields up
    to just before this field.}
    HeaderCheckSum: NativeUInt;
{$ifdef 64Bit}
    Padding1: Cardinal;
{$endif}
    Padding2: SmallInt;
    {The debug block signature.  This will always be CIsDebugBlockFlag.}
    DebugBlockFlags: SmallInt;
  end;

  TFastMM_WalkAllocatedBlocksBlockType = (
    btLargeBlock,
    btMediumBlockSpan,
    btMediumBlock,
    btSmallBlockSpan,
    btSmallBlock);
  TFastMM_WalkBlocksBlockTypes = set of TFastMM_WalkAllocatedBlocksBlockType;

  TFastMM_WalkAllocatedBlocks_BlockInfo = record
    BlockAddress: Pointer;
    {If there is additional debug information for the block, this will be a pointer to it.  (Will be nil if there is no
    additional debug information for the block.}
    DebugInformation: PFastMM_DebugBlockHeader;
    {The size of the block or span.  This includes the size of the block header, padding and internal fragmentation.}
    BlockSize: NativeInt;
    {The usable size of the block.  This is BlockSize less any headers, footers, other management structures and
    internal fragmentation.}
    UsableSize: NativeInt;
    {An arbitrary pointer value passed in to the WalkAllocatedBlocks routine, which is passed through to the callback.}
    UserData: Pointer;
    {The arena number for the block}
    ArenaIndex: Byte;
    {The type of block}
    BlockType: TFastMM_WalkAllocatedBlocksBlockType;
    {True if the block is free, False if it is in use}
    BlockIsFree: Boolean;
    {--------Medium block spans only-------}
    {If True this is the current sequential feed medium block span for ArenaIndex}
    IsSequentialFeedMediumBlockSpan: Boolean;
    {If this is the sequential feed span for the medium block arena then this will contain the number of bytes
    currently unused.}
    MediumBlockSequentialFeedSpanUnusedBytes: Integer;
    {----Small block spans only-----}
    {If True this is the current sequential feed small block span for ArenaIndex and the block size}
    IsSequentialFeedSmallBlockSpan: Boolean;
    {If IsSmallBlockSpan = True then this will contain the size of the small block.}
    SmallBlockSpanBlockSize: Word;
    {If this is a sequential feed small block span then this will contain the number of bytes currently unused.}
    SmallBlockSequentialFeedSpanUnusedBytes: Integer;
  end;

  TFastMM_WalkBlocksCallback = procedure(const ABlockInfo: TFastMM_WalkAllocatedBlocks_BlockInfo);

  TFastMM_MinimumAddressAlignment = (maa8Bytes, maa16Bytes, maa32Bytes, maa64Bytes);
  TFastMM_MinimumAddressAlignmentSet = set of TFastMM_MinimumAddressAlignment;

  {The formats in which text files (e.g. the event log) may be written.  Controlled via the FastMM_TextFileEncoding
  variable.}
  TFastMM_TextFileEncoding = (
    {UTF-8 with no byte-order mark}
    teUTF8,
    {UTF-8 with a byte-order mark}
    teUTF8_BOM,
    {UTF-16 little endian, with no byte-order mark}
    teUTF16LE,
    {UTF-16 little endian, with a byte-order mark}
    teUTF16LE_BOM);

  {A routine used to obtain the current stack trace up to AMaxDepth levels deep.  The first ASkipFrames frames in the
  stack trace are skipped.  Unused entries will be set to 0.}
  TFastMM_GetStackTrace = procedure(APReturnAddresses: PNativeUInt; AMaxDepth, ASkipFrames: Cardinal);

  {A routine used to convert a stack trace to a textual representation (typically unit and line information).
  APReturnAddresses points to a buffer with up to AMaxDepth return addresses (zero return addresses are ignored).  The
  textual representation is stored to APBufferPosition.  The routine will update both APBufferPosition and
  ARemainingBufferSpaceInWideChars.}
  TFastMM_ConvertStackTraceToText = function(APReturnAddresses: PNativeUInt; AMaxDepth: Cardinal;
    APBuffer, APBufferEnd: PWideChar): PWideChar;

  {List of registered leaks}
  TFastMM_RegisteredMemoryLeak = record
    LeakAddress: Pointer;
    LeakedClass: TClass;
    LeakSize: NativeInt;
    LeakCount: Integer;
  end;
  TFastMM_RegisteredMemoryLeaks = array of TFastMM_RegisteredMemoryLeak;

  TFastMM_UsageSummary = record
    {The total number of bytes allocated by the application.}
    AllocatedBytes: NativeUInt;
    {The committed virtual address space less AllocatedBytes:  The total number of address space bytes used by control
    structures, or lost due to fragmentation and other overhead.  Blocks that have been freed by the application but
    not yet released back to the operating system are included in this total.}
    OverheadBytes: NativeUInt;
    {The efficiency of the memory manager expressed as a percentage.  This is:
    100 * AllocatedBytes / (AllocatedBytes + OverheadBytes).}
    EfficiencyPercentage: Double;
  end;

{------------------------Core memory manager interface------------------------}
function FastMM_GetMem(ASize: NativeInt): Pointer;
function FastMM_FreeMem(APointer: Pointer): Integer;
function FastMM_ReallocMem(APointer: Pointer; ANewSize: NativeInt): Pointer;
function FastMM_AllocMem(ASize: NativeInt): Pointer;

{------------------------Expected memory leak management------------------------}

{Registers expected memory leaks.  Returns True on success.  The list of leaked blocks is limited, so failure is
possible if the list is full.}
function FastMM_RegisterExpectedMemoryLeak(ALeakedPointer: Pointer): Boolean; overload;
function FastMM_RegisterExpectedMemoryLeak(ALeakedObjectClass: TClass; ACount: Integer = 1): Boolean; overload;
function FastMM_RegisterExpectedMemoryLeak(ALeakedBlockSize: NativeInt; ACount: Integer = 1): Boolean; overload;
{Removes expected memory leaks.  Returns True on success.}
function FastMM_UnregisterExpectedMemoryLeak(ALeakedPointer: Pointer): Boolean; overload;
function FastMM_UnregisterExpectedMemoryLeak(ALeakedObjectClass: TClass; ACount: Integer = 1): Boolean; overload;
function FastMM_UnregisterExpectedMemoryLeak(ALeakedBlockSize: NativeInt; ACount: Integer = 1): Boolean; overload;
{Returns a list of all expected memory leaks}
function FastMM_GetRegisteredMemoryLeaks: TFastMM_RegisteredMemoryLeaks;

{------------------------Diagnostics------------------------}

{Returns the user size of the block, normally the number of bytes requested in the original GetMem or ReallocMem call.
Exception:  Outside of debug mode the requested size for small and medium blocks is not tracked, and in these instances
the value returned will be the same as the value returned by the FastMM_BlockMaximumUserBytes call.}
function FastMM_BlockCurrentUserBytes(APointer: Pointer): NativeInt;
{Returns the maximum number of bytes that may safely be used by the application for the block starting at APointer.
This will be greater or equal to the size requested in the original GetMem or ReallocMem call.  Note that using more
than the value returned by FastMM_BlockCurrentUserBytes is not recommended, since a reallocation request will only move
up to FastMM_BlockCurrentUserBytes bytes.}
function FastMM_BlockMaximumUserBytes(APointer: Pointer): NativeInt;

{Attempts to release all pending free blocks.  Returns True if there were no pending frees, or all pending frees could
be released.  Returns False if there were locked (currently in use) managers with pending frees.}
function FastMM_ProcessAllPendingFrees: Boolean;

{Walks the block types indicated by the AWalkBlockTypes set, calling ACallBack for each allocated block.  If
AWalkBlockTypes = [] then all block types is assumed.  Note that pending free blocks are treated as used blocks for the
purpose of the AWalkUsedBlocksOnly parameter.  Call FastMM_ProcessAllPendingFrees first in order to process all pending
frees if this is a concern.  ALockTimeoutMilliseconds is the maximum number of millseconds that FastMM_WalkBlocks will
wait to acquire a lock on an arena, skipping the arena if it is unable to do so.  Returns True if all blocks were walked
successfully, False if one or more arenas were skipped due to a lock timeout.}
function FastMM_WalkBlocks(ACallBack: TFastMM_WalkBlocksCallback; AWalkBlockTypes: TFastMM_WalkBlocksBlockTypes = [];
  AWalkUsedBlocksOnly: Boolean = True; AUserData: Pointer = nil; ALockTimeoutMilliseconds: Cardinal = 1000): Boolean;

{Walks all debug mode blocks (blocks that were allocated between a FastMM_EnterDebugMode and FastMM_ExitDebugMode call),
checking for corruption of the debug header, footer, and in the case of freed blocks whether the block content was
modified after the block was freed.  If a corruption is encountered an error message will be logged and/or displayed
(as per the error logging configuration) and an invalid pointer exception will be raised.}
procedure FastMM_ScanDebugBlocksForCorruption;

{Returns a THeapStatus structure with information about the current memory usage.}
function FastMM_GetHeapStatus: THeapStatus;

{Returns the number of allocated bytes, the number of overhead bytes (wastage due to management structures and internal
fragmentation), as well as the efficiency percentage.  The efficiency percentage is the total allocated bytes divided
by the total address space committed (whether in use or reserved for future use) multiplied by 100.  Note that freed
blocks not yet released to the operating system are included in the overhead, which differs from FastMM_GetHeapStatus
that exposes freed blocks in separate fields.}
function FastMM_GetUsageSummary: TFastMM_UsageSummary;

{Writes a log file containing a summary of the memory manager state and a list of allocated blocks grouped by class.
The file will be saved in the encoding specified by FastMM_TextFileEncoding.  ALockTimeoutMilliseconds is the maximum
amount of time to wait for a lock on a manager to be released, before it is skipped.  Returns True on success.}
function FastMM_LogStateToFile(const AFilename: string; const AAdditionalDetails: string = '';
  ALockTimeoutMilliseconds: Cardinal = 1000): Boolean;

{------------------------Memory Manager Sharing------------------------}

{Searches the current process for a shared memory manager.  If no memory has been allocated using this memory manager
it will switch to using the shared memory manager instead.  Returns True if another memory manager was found and it
could be shared.  If this memory manager instance *is* the shared memory manager, it will do nothing and return True.}
function FastMM_AttemptToUseSharedMemoryManager: Boolean;

{Starts sharing this memory manager with other modules in the current process.  Only one memory manager may be shared
per process, so this function may fail.}
function FastMM_ShareMemoryManager: Boolean;

{------------------------Configuration------------------------}

{Returns the current installation state of the memory manager.}
function FastMM_GetInstallationState: TFastMM_MemoryManagerInstallationState;

{Gets/sets the optimization strategy for the memory manager.  FastMM can be optimized for maximum performance, low
memory usage or a blend of the two.}
procedure FastMM_SetOptimizationStrategy(AStrategy: TFastMM_MemoryManagerOptimizationStrategy);
function FastMM_GetCurrentOptimizationStrategy: TFastMM_MemoryManagerOptimizationStrategy;

{Call FastMM_EnterMinimumAddressAlignment to request that all subsequent allocated blocks are aligned to the specified
minimum.  Call FastMM_ExitMinimumAddressAlignment to rescind a prior request.  Requests for coarser alignments have
precedence over requests for lesser alignments.  These calls are thread safe.  In the current implementation the
following minimum alignments are always in effect, regardless of any alignment requests:
  32-Bit applications: >= maa8Bytes
  64-bit applications: >= maa16Bytes
  Allocations greater than 150 bytes: >= maa16Bytes
  Allocations greater than 302 bytes: >= maa32Bytes
  Allocations greater than 606 bytes: maa64Bytes}
procedure FastMM_EnterMinimumAddressAlignment(AMinimumAddressAlignment: TFastMM_MinimumAddressAlignment);
procedure FastMM_ExitMinimumAddressAlignment(AMinimumAddressAlignment: TFastMM_MinimumAddressAlignment);
{Returns the current minimum address alignment in effect.}
function FastMM_GetCurrentMinimumAddressAlignment: TFastMM_MinimumAddressAlignment;

{Attempts to load the debug support library specified by FastMM_DebugSupportLibraryName.  On success it will set the
FastMM_GetStackTrace and FastMM_ConvertStackTraceToText handlers to point to the routines in the debug library, provided
alternate handlers have not yet been assigned by the application.  Returns True if the library was loaded successfully,
or was already loaded successfully prior to this call.  FastMM_EnterDebugMode will call FastMM_LoadDebugSupportLibrary
the first time it is called, unless the debug support library has already been loaded or handlers for both
FastMM_GetStackTrace and FastMM_ConvertStackTraceToText have been set by the application.}
function FastMM_LoadDebugSupportLibrary: Boolean;
{Frees the debug support library, pointing the stack trace handlers currently using the debug support library back to
the default no-op handlers.}
function FastMM_FreeDebugSupportLibrary: Boolean;

{Enters/exits debug mode.  Calls may be nested, in which case debug mode is only exited when the number of
FastMM_ExitDebugMode calls equal the number of FastMM_EnterDebugMode calls.  In debug mode extra metadata is logged
before and after the user data in the block, and extra checks are performed in order to catch common programming
errors.  Returns True on success, False if this memory manager instance is not currently installed or the installed
memory manager has changed.  Note that debug mode comes with a severe performance penalty, and due to the extra
metadata all blocks that are allocated while debug mode is active will use significantly more address space.}
function FastMM_EnterDebugMode: Boolean;
function FastMM_ExitDebugMode: Boolean;
{Returns True if debug mode is currently active, i.e. FastMM_EnterDebugMode has been called more times than
FastMM_ExitDebugMode.}
function FastMM_DebugModeActive: Boolean;

{No-op call stack routines.}
procedure FastMM_NoOpGetStackTrace(APReturnAddresses: PNativeUInt; AMaxDepth, ASkipFrames: Cardinal);
function FastMM_NoOpConvertStackTraceToText(APReturnAddresses: PNativeUInt; AMaxDepth: Cardinal;
  APBufferPosition, APBufferEnd: PWideChar): PWideChar;

{Sets the default event log path and filename.  If the FastMMLogFilePath environment variable is set then that will be
used as the path, otherwise the path to the application will be used.  The filename is built from the name of the
application.}
procedure FastMM_SetDefaultEventLogFilename;
{Sets the full path and filename for the event log.  if APEventLogFilename = nil then the default event log filename
will be set.}
procedure FastMM_SetEventLogFilename(APEventLogFilename: PWideChar);
{Returns the current full path and filename for the event log.}
function FastMM_GetEventLogFilename: PWideChar;
{Deletes the event log file.}
function FastMM_DeleteEventLogFile: Boolean;

var

  {-----------Stack trace support routines----------}
  {The active routines used to get a call stack and to convert it to a textual representation.  These will be set to
  the no-op routines during startup.  If either of these have not been assigned a different value when
  FastMM_EnterDebugMode is called for the first time then an attempt will be made to load the debug support DLL and
  any of these still set to the no-op routines will be rerouted to the handlers in the debug support DLL.}
  FastMM_GetStackTrace: TFastMM_GetStackTrace;
  FastMM_ConvertStackTraceToText: TFastMM_ConvertStackTraceToText;

  {---------Debug options---------}

  {The name of the library that contains the functionality used to obtain the current call stack, and also to convert a
  call stack to unit and line number information.  The first time EnterDebugMode is called an attempt will be made to
  load this library, unless handlers for both FastMM_GetStackTrace and FastMM_ConvertStackTraceToText have already been
  set.}
  FastMM_DebugSupportLibraryName: PWideChar = {$ifndef 64Bit}'FastMM_FullDebugMode.dll'{$else}'FastMM_FullDebugMode64.dll'{$endif};

  {The events that are passed to OutputDebugString.}
  FastMM_OutputDebugStringEvents: TFastMM_MemoryManagerEventTypeSet = [mmetDebugBlockDoubleFree,
    mmetDebugBlockReallocOfFreedBlock, mmetDebugBlockHeaderCorruption, mmetDebugBlockFooterCorruption,
    mmetDebugBlockModifiedAfterFree, mmetVirtualMethodCallOnFreedObject, mmetAnotherThirdPartyMemoryManagerAlreadyInstalled,
    mmetCannotInstallAfterDefaultMemoryManagerHasBeenUsed, mmetCannotSwitchToSharedMemoryManagerWithLivePointers];
  {The events that are logged to file.}
  FastMM_LogToFileEvents: TFastMM_MemoryManagerEventTypeSet = [mmetDebugBlockDoubleFree,
    mmetDebugBlockReallocOfFreedBlock, mmetDebugBlockHeaderCorruption, mmetDebugBlockFooterCorruption,
    mmetDebugBlockModifiedAfterFree, mmetVirtualMethodCallOnFreedObject, mmetAnotherThirdPartyMemoryManagerAlreadyInstalled,
    mmetCannotInstallAfterDefaultMemoryManagerHasBeenUsed, mmetCannotSwitchToSharedMemoryManagerWithLivePointers];
  {The events that are displayed in a message box.}
  FastMM_MessageBoxEvents: TFastMM_MemoryManagerEventTypeSet = [mmetDebugBlockDoubleFree,
    mmetDebugBlockReallocOfFreedBlock, mmetDebugBlockHeaderCorruption, mmetDebugBlockFooterCorruption,
    mmetDebugBlockModifiedAfterFree, mmetVirtualMethodCallOnFreedObject, mmetAnotherThirdPartyMemoryManagerAlreadyInstalled,
    mmetCannotInstallAfterDefaultMemoryManagerHasBeenUsed, mmetCannotSwitchToSharedMemoryManagerWithLivePointers];
  {All debug blocks are tagged with the current value of this variable when the block is allocated.  This may be used
  by the application to track memory issues.}
  FastMM_CurrentAllocationGroup: Cardinal;
  {This variable is incremented during every debug getmem call (wrapping to 0 once it hits 4G) and stored in the debug
  header.  It may be useful for debugging purposes.}
  FastMM_LastAllocationNumber: Cardinal;
  {These variables are incremented every time all the arenas for the block size are locked simultaneously and FastMM had
  to relinquish the thread's timeslice during a GetMem or ReallocMem call. (FreeMem frees can always be deferred, so
  will never cause a thread contention).  If these numbers are excessively high then it is an indication that the number
  of small, medium and/or large block arenas are insufficient for the number of application threads and should be
  increased.  (The CFastMM_SmallBlockArenaCount, CFastMM_MediumBlockArenaCount and CFastMM_LargeBlockArenaCount constants.)}
  FastMM_SmallBlockThreadContentionCount: Cardinal;
  FastMM_MediumBlockThreadContentionCount: Cardinal;
  FastMM_LargeBlockThreadContentionCount: Cardinal;

  {---------Message and log file text configuration--------}

  {The text encoding to use for the event log and other text file output.}
  FastMM_TextFileEncoding: TFastMM_TextFileEncoding;

  {Messages contain numeric tokens that will be substituted.  The available tokens are:
    0: A blank string (invalid token IDs will also translate to this)
    1: The current date in yyyy-mm-dd format.
    2: The current time in HH:nn:ss format.
    3: Block size in bytes
    4: The ID of the allocating thread (in hexadecimal).
    5: The ID of the freeing thread (in hexadecimal).
    6: The stack trace when the block was allocated.
    7: The stack trace when the block was freed.
    8: The object class for the block.  For freed blocks this will be the prior object class, otherwise it will be the
       current object class.
    9: The allocation number for the block (in decimal).
    10: Hex and ASCII dump size in bytes
    11: Block address (in hexadecimal).
    12: Hex dump of block (each line is followed by #13#10)
    13: ASCII dump of block (each line is followed by #13#10)
    14: Leak summary entries
    15: The size and offsets for modifications to a block after it was freed.
    16: The full path and filename of the event log.
    17: The virtual method name for a virtual method calls on a freed object
    18: The total kilobytes allocated (FastMM_LogStateToFile)
    19: The total kilobytes overhead (FastMM_LogStateToFile)
    20: The efficiency percentage (FastMM_LogStateToFile)
    21: The total number of bytes used by the class (FastMM_LogStateToFile)
    22: The number of instances of the class (FastMM_LogStateToFile)
    23: The average number of bytes per instance for the class (FastMM_LogStateToFile)
    24: The stack trace for a virtual method call on a freed object

  }

  {This entry precedes every entry in the event log.}
  FastMM_LogFileEntryHeader: PWideChar = '--------------------------------{1} {2}--------------------------------'#13#10;
  {Memory manager installation errors}
  FastMM_CannotInstallAfterDefaultMemoryManagerHasBeenUsedMessage: PWideChar = 'FastMM cannot be installed, because the '
    + 'default memory manager has already been used to allocate memory.';
  FastMM_CannotSwitchToSharedMemoryManagerWithLivePointersMessage: PWideChar = 'Cannot switch to the shared memory '
    + 'manager, because the local memory manager instance has already been used to allocate memory.';
  FastMM_AnotherMemoryManagerAlreadyInstalledMessage: PWideChar = 'FastMM cannot be installed, because another third '
    + 'party memory manager has already been installed.';
  FastMM_CannotSwitchMemoryManagerMessageBoxCaption: PWideChar = 'Cannot Switch Memory Managers';

  {Memory leak messages.}
  FastMM_MemoryLeakDetailMessage_NormalBlock: PWideChar = 'A memory block has been leaked. The size is: {3}'#13#10#13#10
    + 'The block is currently used for an object of class: {8}'#13#10#13#10
    + 'Current memory dump of {10} bytes starting at pointer address {11}:'#13#10
    + '{12}'#13#10'{13}'#13#10;
  FastMM_MemoryLeakDetailMessage_DebugBlock: PWideChar = 'A memory block has been leaked. The size is: {3}'#13#10#13#10
    + 'This block was allocated by thread 0x{4}, and the stack trace (return addresses) at the time was:'
    + '{6}'#13#10#13#10'The block is currently used for an object of class: {8}'#13#10#13#10
    + 'The allocation number is: {9}'#13#10#13#10
    + 'Current memory dump of {10} bytes starting at pointer address {11}:'#13#10
    + '{12}'#13#10'{13}'#13#10;
  FastMM_MemoryLeakSummaryMessage_LeakDetailNotLogged: PWideChar = 'This application has leaked memory. '
    + 'The leaks ordered by size are:'#13#10'{14}'#13#10;
  FastMM_MemoryLeakSummaryMessage_LeakDetailLoggedToEventLog: PWideChar = 'This application has leaked memory. '
    + 'The leaks ordered by size are:'#13#10'{14}'#13#10#13#10
    + 'Memory leak detail was logged to {16}'#13#10;
  FastMM_MemoryLeakMessageBoxCaption: PWideChar = 'Unexpected Memory Leak';
  {Attempts to free or reallocate a debug block that has alredy been freed.}
  FastMM_DebugBlockDoubleFree: PWideChar = 'An attempt was made to free a block that has already been freed.'#13#10#13#10
    + 'The block size is {3}.'#13#10#13#10
    + 'The block was allocated by thread 0x{4}, and the stack trace (return addresses) at the time was:'
    + '{6}'#13#10#13#10'This block was freed by thread 0x{5}, and the stack trace (return addresses) at the time was:'
    + '{7}'#13#10#13#10
    + 'The allocation number is: {9}'#13#10;
  FastMM_DebugBlockReallocOfFreedBlock: PWideChar = 'An attempt was made to resize a block that has already been freed.'#13#10#13#10
    + 'The block size is {3}.'#13#10#13#10
    + 'The block was allocated by thread 0x{4}, and the stack trace (return addresses) at the time was:'
    + '{6}'#13#10#13#10'This block was freed by thread 0x{5}, and the stack trace (return addresses) at the time was:'
    + '{7}'#13#10#13#10
    + 'The allocation number is: {9}'#13#10;

  {Memory pool corruption messages.}
  FastMM_BlockModifiedAfterFreeMessage: PWideChar = 'A memory block was modified after it was freed.'#13#10#13#10
    + 'The block size is {3}.'#13#10#13#10
    + 'Modifications were detected at offsets (with lengths in brackets): {15}.'#13#10#13#10
    + 'The block was allocated by thread 0x{4}, and the stack trace (return addresses) at the time was:'
    + '{6}'#13#10#13#10'This block was freed by thread 0x{5}, and the stack trace (return addresses) at the time was:'
    + '{7}'#13#10#13#10
    + 'The allocation number is: {9}'#13#10#13#10
    + 'Current memory dump of {10} bytes starting at pointer address {11}:'#13#10
    + '{12}'#13#10'{13}'#13#10;
  FastMM_BlockHeaderCorruptedMessage: PWideChar = 'A memory block header has been corrupted.'#13#10#13#10
    + 'Current memory dump of {10} bytes starting at pointer address {11}:'#13#10
    + '{12}'#13#10'{13}'#13#10;
  FastMM_BlockFooterCorruptedMessage_AllocatedBlock: PWideChar = 'A memory block footer has been corrupted.'#13#10#13#10
    + 'The block size is {3}.'#13#10#13#10
    + 'The block was allocated by thread 0x{4}, and the stack trace (return addresses) at the time was:'
    + '{6}'#13#10#13#10
    + 'The allocation number is: {9}'#13#10#13#10
    + 'Current memory dump of {10} bytes starting at pointer address {11}:'#13#10
    + '{12}'#13#10'{13}'#13#10;
  FastMM_BlockFooterCorruptedMessage_FreedBlock: PWideChar = 'A memory block footer has been corrupted.'#13#10#13#10
    + 'The block size is {3}.'#13#10#13#10
    + 'The block was allocated by thread 0x{4}, and the stack trace (return addresses) at the time was:'
    + '{6}'#13#10#13#10'This block was freed by thread 0x{5}, and the stack trace (return addresses) at the time was:'
    + '{7}'#13#10#13#10
    + 'The allocation number is: {9}'#13#10#13#10
    + 'Current memory dump of {10} bytes starting at pointer address {11}:'#13#10
    + '{12}'#13#10'{13}'#13#10;
  FastMM_MemoryCorruptionMessageBoxCaption: PWideChar = 'Memory Corruption Detected';

  {Virtual method call on a freed object.}
  FastMM_VirtualMethodCallOnFreedObjectMessage: PWideChar = 'A virtual method was called on a freed object.'#13#10#13#10
    + 'Freed object class: {8}'#13#10#13#10
    + 'Virtual method: {17}'#13#10#13#10
    + 'The block size is {3}.'#13#10#13#10
    + 'The block was allocated by thread 0x{4}, and the stack trace (return addresses) at the time was:'
    + '{6}'#13#10#13#10'This block was freed by thread 0x{5}, and the stack trace (return addresses) at the time was:'
    + '{7}'#13#10#13#10'The stack trace for the virtual call that lead to this error is:'
    + '{24}'#13#10#13#10
    + 'The allocation number is: {9}'#13#10#13#10
    + 'Current memory dump of {10} bytes starting at pointer address {11}:'#13#10
    + '{12}'#13#10'{13}'#13#10;
  FastMM_VirtualMethodCallOnFreedObjectMessageBoxCaption: PWideChar = 'Virtual Method Call On Freed Object';

  {Memory state logging messages}
  FastMM_LogStateToFileTemplate: PWideChar = 'FastMM State Capture:'#13#10
    + '---------------------'#13#10
    + '{18}K Allocated'#13#10
    + '{19}K Overhead'#13#10
    + '{20}% Efficiency'#13#10#13#10
    + 'Usage Detail:'#13#10;
  FastMM_LogStateToFileTemplate_UsageDetail: PWideChar = '{21} bytes: {8} x {22} ({23} bytes avg.)'#13#10;
  {Initialization error messages.}
  FastMM_DebugSupportLibraryNotAvailableError: PWideChar = 'The FastMM debug support library could not be loaded.';
  FastMM_DebugSupportLibraryNotAvailableError_Caption: PWideChar = 'Fatal Error';

implementation

{All blocks are preceded by a block header.  The block header varies in size according to the block type.  The block
type and state may be determined from the bits of the word preceding the block address, as follows:

  All block types:
  ----------------

  Bit 0: Block is free flag
    0 = Block is in use
    1 = Block is free

  Bit 1: Debug info flag
    0 = the block contains no additional debug information
    1 = the block contains a debug mode sub-block

  Bit 2: Block type 1
    0 = Is not a small block
    1 = Is a small block


  Small blocks only (bit 2 = 1):
  ------------------------------

  Bits 3..15: Offset to small block span header
    The offset of the block from the start of the small block span header, divided by 64.


  Medium, Large and Debug Blocks (bit 2 = 0):
  -------------------------------------------

  Bit 3: Block type 2
    0 = Is not a medium block
    1 = Is a medium block

  Bit 4: Block type 3
    0 = Is not a large block
    1 = Is a large block

  Bit 5: Block type 4
    0 = Is not a debug sub-block
    1 = Is a debug sub-block

  Bits 6..15: Reserved (always 0)

}

const

  {$ifdef 32Bit}
  CPointerSizeBitShift = 2; //1 shl 2 = 4
  CTObjectInstanceSize = 8;
  {$else}
  CPointerSizeBitShift = 3; //1 shl 3 = 8
  CTObjectInstanceSize = 16;
  {$endif}

  {Block status flags}
  CBlockIsFreeFlag = 1;
  CHasDebugInfoFlag = 2;
  CIsSmallBlockFlag = 4;
  CIsMediumBlockFlag = 8;
  CIsLargeBlockFlag = 16;
  CIsDebugBlockFlag = 32;

  {-----Small block constants-----}
{$ifdef 32Bit}
  CSmallBlockTypeCount = 61;
  CSmallBlockGranularityBits = 3;
{$else}
  CSmallBlockTypeCount = 51;
  CSmallBlockGranularityBits = 4;
{$endif}
  CSmallBlockGranularity = 1 shl CSmallBlockGranularityBits;
  CMaximumSmallBlockSize = 2624; //Must be a multiple of 64 for the 64-byte alignment option to work
  CSmallBlockFlagCount = 3;
  CDropSmallBlockFlagsMask = - (1 shl CSmallBlockFlagCount);
  CSmallBlockSpanOffsetBitShift = 6 - CSmallBlockFlagCount;

  {-----Medium block constants-----}
  {Medium blocks are always aligned to at least 64 bytes (which is the typical cache line size).  Spans must be a
  multiple of 64K (to make optimal use of the virtual address space), and offsets divided by the granularity must fit
  inside a 16-bit word.}
  CMediumBlockAlignmentBits = 6;
  CMediumBlockAlignment = 1 shl CMediumBlockAlignmentBits;
  CMaximumMediumBlockSpanSize = 64 * 1024 * CMediumBlockAlignment; // = 4MB

  {Medium blocks are binned in linked lists - one linked list for each size.}
  CMediumBlockBinsPerGroup = 32;
  CMediumBlockBinGroupCount = 32;
  CMediumBlockBinCount = CMediumBlockBinGroupCount * CMediumBlockBinsPerGroup;

  {The smallest medium block should be <= 10% greater than the largest small block.  It is an odd multiple
  of the typical cache line size in order to facilitate better cache line utilization.}
  CMinimumMediumBlockSize = CMaximumSmallBlockSize + 256; // = 2880

  {The spacing between medium block bins is not constant.  There are three groups: initial, middle and final.}
  CInitialBinCount = 384;
  CInitialBinSpacingBits = 8;
  CInitialBinSpacing = 1 shl CInitialBinSpacingBits; // = 256

  CMediumBlockMiddleBinsStart = CMinimumMediumBlockSize + CInitialBinSpacing * CInitialBinCount;
  CMiddleBinCount = 384;
  CMiddleBinSpacingBits = 9;
  CMiddleBinSpacing = 1 shl CMiddleBinSpacingBits; // = 512

  CMediumBlockFinalBinsStart = CMediumBlockMiddleBinsStart + CMiddleBinSpacing * CMiddleBinCount;
  CFinalBinCount = CMediumBlockBinCount - CMiddleBinCount - CInitialBinCount;
  CFinalBinSpacingBits = 10;
  CFinalBinSpacing = 1 shl CFinalBinSpacingBits; // = 1024

  {The maximum size allocatable through medium blocks.  Blocks larger than this are allocated via the OS from the
  virtual memory pool ( = large blocks).}
  CMaximumMediumBlockSize = CMediumBlockFinalBinsStart + (CFinalBinCount - 1) * CFinalBinSpacing;

  {-----Large block constants-----}
  CLargeBlockGranularity = 64 * 1024; //Address space obtained from VirtualAlloc is always aligned to a 64K boundary

  {-----Small block span constants-----}
  {Allocating and deallocating small block spans are expensive, so it is not something that should be done frequently.}
  CMinimumSmallBlocksPerSpan = 16;
  COptimalSmallBlocksPerSpan = 64;
  COptimalSmallBlockSpanSizeLowerLimit = CMinimumMediumBlockSize + 16 * 1024;
  COptimalSmallBlockSpanSizeUpperLimit = CMinimumMediumBlockSize + 96 * 1024;
  {The maximum amount by which a small block span may exceed the optimal size before the block will be split instead of
  using it as-is.}
  CSmallBlockSpanMaximumAmountWithWhichOptimalSizeMayBeExceeded = 4 * 1024;

  {-------------Block resizing constants---------------}
  CSmallBlockDownsizeCheckAdder = 64;
  CSmallBlockUpsizeAdder = 32;
  {When a medium block is reallocated to a size smaller than this, then it must be reallocated to a small block and the
  data moved.  If not, then it is shrunk in place.}
  CMediumInPlaceDownsizeLimit = CMinimumMediumBlockSize div 4;

  {------Debug constants-------}
{$ifdef 32Bit}
  {The number of bytes of address space that is reserved and only released once the first OS allocation request fails.
  This allows some subsequent memory allocation requests to succeed in order to allow the application to allocate some
  memory for error handling, etc. in response to the first EOutOfMemory exception.  This only applies to 32-bit
  applications.}
  CEmergencyReserveAddressSpace = CMaximumMediumBlockSpanSize;
{$endif}

  {Event and state log tokens}
  CEventLogTokenBlankString = 0;
  CEventLogTokenCurrentDate = 1;
  CEventLogTokenCurrentTime = 2;
  CEventLogTokenBlockSize = 3;
  CEventLogTokenAllocatedByThread = 4;
  CEventLogTokenFreedByThread = 5;
  CEventLogTokenAllocationStackTrace = 6;
  CEventLogTokenFreeStackTrace = 7;
  CEventLogTokenObjectClass = 8;
  CEventLogTokenAllocationNumber = 9;
  CEventLogTokenMemoryDumpSize = 10;
  CEventLogTokenBlockAddress = 11;
  CEventLogTokenHexDump = 12;
  CEventLogTokenASCIIDump = 13;
  CEventLogTokenLeakSummaryEntries = 14;
  CEventLogTokenModifyAfterFreeDetail = 15;
  CEventLogTokenEventLogFilename = 16;
  CEventLogTokenVirtualMethodName = 17;
  CEventLogTokenVirtualMethodCallOnFreedObject = 24;

  CStateLogTokenAllocatedKB = 18;
  CStateLogTokenOverheadKB = 19;
  CStateLogTokenEfficiencyPercentage = 20;
  CStateLogTokenClassTotalBytesUsed = 21;
  CStateLogTokenClassInstanceCount = 22;
  CStateLogTokenClassAverageBytesPerInstance = 23;

  {The highest ID of an event log token.}
  CEventLogMaxTokenID = 30;

  {The maximum size of an event message, in wide characters.}
  CEventMessageMaxWideChars = 32768;
  CTokenBufferMaxWideChars = 32768;

  CFilenameMaxLength = 1024;

  {The size of the memory block reserved for maintaining the list of registered memory leaks.}
  CExpectedMemoryLeaksListSize = 64 * 1024;

  CHexDigits: array[0..15] of Char = '0123456789ABCDEF';

  {The maximum size of hexadecimal and ASCII dumps.}
  CMemoryDumpMaxBytes = 256;
  CMemoryDumpMaxBytesPerLine = 32;

  {The debug block fill pattern, in several sizes.}
  CDebugFillPattern8B = $8080808080808080;
  CDebugFillPattern4B = $80808080;
  CDebugFillPattern2B = $8080;
  CDebugFillPattern1B = $80;

  {The first few frames of a GetMem or FreeMem stack trace are inside system.pas and this unit, so does not provide any
  useful information.  Specify how many of the initial frames should be skipped here.  Note that these are actual
  frames, so routines that do not have frames will also be skipped.}
  CFastMM_StackTrace_SkipFrames_GetMem = 0;
  CFastMM_StackTrace_SkipFrames_FreeMem = 0;

  {The number of bytes in a memory page.  It is assumed that pages are aligned at page size boundaries, and that memory
  protection is set at the page level.}
  CVirtualMemoryPageSize = 4096;

  CCopyrightMessage: PAnsiChar = 'FastMM (c) 2004 - 2020 Pierre le Riche';

type

  {Event log token values are pointers #0 terminated text strings.  The payload for the tokens is in TokenData.}
  TEventLogTokenValues = array[0..CEventLogMaxTokenID] of PWideChar;

  TMoveProc = procedure(const ASource; var ADest; ACount: NativeInt);

  TIntegerWithABACounter = record
    case Integer of
      0: (IntegerAndABACounter: Int64);
      1: (IntegerValue, ABACounter: Integer);
  end;

  TBlockStatusFlags = Word;

{$PointerMath On}
  PBlockStatusFlags = ^TBlockStatusFlags;
{$PointerMath Off}

  {------------------------Small block structures------------------------}

  {Small blocks have a 16-bit header.}
  TSmallBlockHeader = record
    {
    Bit 0: Block is free flag
      0 = Block is in use
      1 = Block is free

    Bit 1: Debug flag
      0 = the block contains no additional debug information
      1 = the block contains a debug mode sub-block

    Bit 2: Small block indicator
      Must be 1

    Bits 3..15 (0..8191):
      The offset of the block from the start of the small block span header, divided by 64.
    }
    BlockStatusFlagsAndSpanOffset: TBlockStatusFlags;
  end;
{$PointerMath On}
  PSmallBlockHeader = ^TSmallBlockHeader;
{$PointerMath Off}

  {Small block layout:
    Offset: -2 = This block's header
    Offset: 0 = User data / Pointer to next free block (if this block is free)}

  PSmallBlockSpanHeader = ^TSmallBlockSpanHeader;

  {Always 64 bytes in size in order to fit inside a cache line, under both 32-bit and 64-bit.  It should preferably be
  aligned to 64 bytes.}
  TSmallBlockManager = record
    {The first/last partially free span in the arena.  This field must be at the same offsets as
    TSmallBlockSpanHeader.NextPartiallyFreeSpan and TSmallBlockSpanHeader.PreviousPartiallyFreeSpan.}
    FirstPartiallyFreeSpan: PSmallBlockSpanHeader; //Do not change position
    LastPartiallyFreeSpan: PSmallBlockSpanHeader; //Do not change position

    {The offset from the start of SequentialFeedSmallBlockSpan of the last block that was fed sequentially, as well as
    an ABA counter to solve concurrency issues.}
    LastSmallBlockSequentialFeedOffset: TIntegerWithABACounter;

    {The span that is current being used to serve blocks in sequential order, from the last block down to the first.}
    SequentialFeedSmallBlockSpan: PSmallBlockSpanHeader;

    {Singly linked list of blocks in this arena that should be freed.  If a block must be freed but the arena is
    currently locked by another thread then the block is added to the head of this list.  It is the responsibility of
    the next thread that locks this arena to clean up this list.}
    PendingFreeList: Pointer;

    {The fixed size move procedure used to move data for this block size when it is upsized.  When a block is downsized
    (which typically occurs less often) the variable size move routine is used.}
    UpsizeMoveProcedure: TMoveProc;

    {0 = unlocked, 1 = locked, cannot be Boolean due to RSP-25672}
    SmallBlockManagerLocked: Integer;

    {The minimum and optimal size of a small block span for this block type}
    MinimumSpanSize: Integer;
    OptimalSpanSize: Integer;

    {The block size for this small block manager}
    BlockSize: Word;

{$ifdef 64Bit}
    Padding: array[0..1] of Byte;
{$else}
    Padding: array[0..21] of Byte;
{$endif}
  end;
  PSmallBlockManager = ^TSmallBlockManager;

  TSmallBlockArena = array[0..CSmallBlockTypeCount - 1] of TSmallBlockManager;
  PSmallBlockArena = ^TSmallBlockArena;

  TSmallBlockArenas = array[0..CFastMM_SmallBlockArenaCount - 1] of TSmallBlockArena;

  {This is always 64 bytes in size in order to ensure proper alignment of small blocks under all circumstances.}
  TSmallBlockSpanHeader = packed record
    {The next and previous spans in this arena that have free blocks of this size.  These fields must be at the same
    offsets as TSmallBlockManager.FirstPartiallyFreeSpan and TSmallBlockManager.LastPartiallyFreeSpan.}
    NextPartiallyFreeSpan: PSmallBlockSpanHeader; //Do not change position
    PreviousPartiallyFreeSpan: PSmallBlockSpanHeader; //Do not change position
    {Pointer to the first free block inside this span.}
    FirstFreeBlock: Pointer;
    {Pointer to the small block manager to which this span belongs.}
    SmallBlockManager: PSmallBlockManager;
    {The total number of blocks in this small block span.}
    TotalBlocksInSpan: Integer;
    {The number of blocks currently in use in this small block span.}
    BlocksInUse: Integer;
{$ifdef 64Bit}
    Padding: array[0..21] of Byte;
{$else}
    Padding: array[0..37] of Byte;
{$endif}
    {The header for the first block}
    FirstBlockHeader: TSmallBlockHeader;
  end;

  {------------------------Medium block structures------------------------}

  TMediumBlockHeader = packed record

    {Multiply with CMediumBlockAlignment in order to get the size of the block.}
    MediumBlockSizeMultiple: Word;

    {The offset from the start of medium block span header to the start of the block.  Multiply this with
    CMediumBlockAlignment and subtract the result from the pointer in order to obtain the address of the medium block
    span.}
    MediumBlockSpanOffsetMultiple: Word;

    {True if the previous medium block in the medium block span is free.  If this is True then the size of the previous
    block will be stored in the Integer immediately preceding this header.}
    PreviousBlockIsFree: Boolean;
    {True if this medium block is used as a small block span.}
    IsSmallBlockSpan: Boolean;
    {The block status and type}
    BlockStatusFlags: TBlockStatusFlags;
  end;
{$PointerMath On}
  PMediumBlockHeader = ^TMediumBlockHeader;
{$PointerMath Off}

  {Medium block layout:
   Offset: - SizeOf(TMediumBlockHeader) - 4 = Integer containing the previous block size (only if PreviousBlockIsFree = True)
   Offset: - SizeOf(TMediumBlockHeader) = This block's header
   Offset: 0 = User data / Pointer to previous free block (if this block is free)
   Offset: SizeOf(Pointer) = Next Free Block (if this block is free)
   Offset: BlockSize - SizeOf(TMediumBlockHeader) - 4 = Size of this block (if this block is free)
   Offset: BlockSize - SizeOf(TMediumBlockHeader) = Header for the next block}

  PMediumBlockManager = ^TMediumBlockManager;

  {The medium block span from which medium blocks are drawn.  This is always 64 bytes in size.}
  PMediumBlockSpanHeader = ^TMediumBlockSpanHeader;
  TMediumBlockSpanHeader = packed record
    {Points to the previous and next medium block spans.  This circular linked list is used to track memory leaks on
    program shutdown.  Must be at the same offsets as TMediumBlockManager.FirstMediumBlockSpanHeader and
    TMediumBlockManager.LastMediumBlockSpanHeader.}
    NextMediumBlockSpanHeader: PMediumBlockSpanHeader; //Do not change position
    PreviousMediumBlockSpanHeader: PMediumBlockSpanHeader; //Do not change position
    {The manager for the arena to which this medium block span belongs.}
    MediumBlockManager: PMediumBlockManager;
    {The size of this medium block span, in bytes.}
    SpanSize: Integer;
{$ifdef 64Bit}
    Padding: array[0..27] of Byte;
{$else}
    Padding: array[0..39] of Byte;
{$endif}
    {The header for the first block}
    FirstBlockHeader: TMediumBlockHeader;
  end;

  {The contents of a medium block that is unused.  This data follows the block header.}
  PMediumFreeBlockContent = ^TMediumFreeBlockContent;
  TMediumFreeBlockContent = record
    {This will point to the bin if this is the last free medium block in the bin.}
    NextFreeMediumBlock: PMediumFreeBlockContent;
    {This will point to the bin if this is the first free medium block in the bin.}
    PreviousFreeMediumBlock: PMediumFreeBlockContent;
  end;

  {Free medium blocks always store their size before the header of the next block}
  TMediumFreeBlockFooter = packed record
    MediumFreeBlockSize: Integer;
    NextBlockHeader: TMediumBlockHeader;
  end;
{$PointerMath On}
  PMediumFreeBlockFooter = ^TMediumFreeBlockFooter;
{$PointerMath Off}

  {Medium block manager.  It should preferably be aligned to 64 bytes.}
  TMediumBlockManager = record
    {Maintains a circular list of all medium block spans to enable memory leak detection on program shutdown.  These
    fields must be at the same position as the corresponding fields in TMediumBlockSpanHeader.}
    FirstMediumBlockSpanHeader: PMediumBlockSpanHeader; //Do not change position
    LastMediumBlockSpanHeader: PMediumBlockSpanHeader; //Do not change position

    {The sequential feed medium block span.}
    LastMediumBlockSequentialFeedOffset: TIntegerWithABACounter;
    SequentialFeedMediumBlockSpan: PMediumBlockSpanHeader;

    {Singly linked list of blocks in this arena that should be freed.  If a block must be freed but the arena is
    currently locked by another thread then the block is added to the head of this list.  It is the responsibility of
    the next thread that locks this arena to clean up this list.}
    PendingFreeList: Pointer;
    {0 = unlocked, 1 = locked, cannot be Boolean due to RSP-25672}
    MediumBlockManagerLocked: Integer;

    {The medium block bins are divided into groups of 32 bins.  If a bit is set in this group bitmap, then at least one
    bin in the group has free blocks.}
    MediumBlockBinGroupBitmap: Cardinal;
    {The medium block bins:  total of 32 * 32 = 1024 bins of a certain minimum size.  The minimum size of blocks in the
    first bin will be CMinimumMediumBlockSize.}
    MediumBlockBinBitmaps: array[0..CMediumBlockBinGroupCount - 1] of Cardinal;
    {The medium block bins.  There are 1024 LIFO circular linked lists each holding blocks of a specified minimum size.
    The bin sizes vary from CMinimumMediumBlockSize to CMaximumMediumBlockSize.  The value for each bin is a pointer to
    the first free medium block in the bin.  Will point to itself if the bin is empty.  The last block in the bin will
    point back to the bin.}
    FirstFreeBlockInBin: array[0..CMediumBlockBinCount - 1] of Pointer;
  end;

  TMediumBlockArenas = array[0..CFastMM_MediumBlockArenaCount - 1] of TMediumBlockManager;

  {-------------------------Large block structures------------------------}

  PLargeBlockManager = ^TLargeBlockManager;

  {Large block header.  Always 64 bytes in size.}
  {$PointerMath On}
  PLargeBlockHeader = ^TLargeBlockHeader;
  {$PointerMath Off}
  TLargeBlockHeader = packed record
    {Points to the previous and next large blocks.  This circular linked list is used to track memory leaks on program
    shutdown.}
    NextLargeBlockHeader: PLargeBlockHeader; //Do not change position
    PreviousLargeBlockHeader: PLargeBlockHeader; //Do not change position
    {The large block manager for the arena to which this block belongs.}
    LargeBlockManager: PLargeBlockManager;
    {The actual block size as obtained from the operating system.}
    ActualBlockSize: NativeInt;
    {The user allocated size of the large block}
    UserAllocatedSize: NativeInt;
    {If True then the large block is built up from more than one chunk allocated through VirtualAlloc}
    BlockIsSegmented: Boolean;
    {Alignment padding}
{$ifdef 64Bit}
    Padding: array[0..20] of Byte;
{$else}
    Padding: array[0..40] of Byte;
{$endif}
    {The block status and type}
    BlockStatusFlags: TBlockStatusFlags;
  end;

  TLargeBlockManager = record
    {Maintains a circular list of all large blocks to enable memory leak detection on program shutdown.}
    FirstLargeBlockHeader: PLargeBlockHeader; //Do not change position
    LastLargeBlockHeader: PLargeBlockHeader; //Do not change position
    {Singly linked list of blocks in this arena that should be freed.  If a block must be freed but the arena is
    currently locked by another thread then the block is added to the head of this list.  It is the responsibility of
    the next thread that locks this arena to clean up this list.}
    PendingFreeList: Pointer;
    {0 = unlocked, 1 = locked, cannot be Boolean due to RSP-25672}
    LargeBlockManagerLocked: Integer; //0 = unlocked, 1 = locked
{$ifdef 64Bit}
    Padding: array[0..35] of Byte;
{$else}
    Padding: array[0..47] of Byte;
{$endif}
  end;

  TLargeBlockArenas = array[0..CFastMM_LargeBlockArenaCount - 1] of TLargeBlockManager;

  {-------------------------Expected Memory Leak Structures--------------------}

  {The layout of an expected leak.  All fields may not be specified, in which case it may be harder to determine which
  leaks are expected and which are not.}
  PExpectedMemoryLeak = ^TExpectedMemoryLeak;
  PPExpectedMemoryLeak = ^PExpectedMemoryLeak;
  TExpectedMemoryLeak = record
    {Leaks are maintained in doubly linked list.}
    PreviousLeak, NextLeak: PExpectedMemoryLeak;
    LeakAddress: Pointer;
    LeakedClass: TClass;
    LeakSize: NativeInt;
    LeakCount: Integer;
  end;

  TExpectedMemoryLeaks = record
    {The number of entries used in the expected leaks buffer}
    EntriesUsed: Integer;
    {Freed entries that are available for reuse}
    FirstFreeSlot: PExpectedMemoryLeak;
    {Entries with the address specified}
    FirstEntryByAddress: PExpectedMemoryLeak;
    {Entries with no address specified, but with the class specified}
    FirstEntryByClass: PExpectedMemoryLeak;
    {Entries with only size specified}
    FirstEntryBySizeOnly: PExpectedMemoryLeak;
    {The expected leaks buffer (Need to leave space for this header)}
    ExpectedLeaks: array[0..(CExpectedMemoryLeaksListSize - 64) div SizeOf(TExpectedMemoryLeak) - 1] of TExpectedMemoryLeak;
  end;
  PExpectedMemoryLeaks = ^TExpectedMemoryLeaks;

  {-------Memory leak reporting structures--------}

  TMemoryLeakType = (mltUnexpectedLeak, mltExpectedLeakRegisteredByPointer, mltExpectedLeakRegisteredByClass,
    mltExpectedLeakRegisteredBySize);

  TMemoryAccessRight = (marExecute, marRead, marWrite);
  TMemoryAccessRights = set of TMemoryAccessRight;
  TMemoryRegionInfo = record
    RegionStartAddress: Pointer;
    RegionSize: NativeUInt;
    RegionIsFree: Boolean;
    AccessRights: TMemoryAccessRights;
  end;

  {Used by the DetectStringData routine to detect whether a leaked block contains string data.}
  TStringDataType = (stNotAString, stAnsiString, stUnicodeString);

  {An entry in the binary search tree of memory leaks.  Leaks are grouped by block size and class.}
  TMemoryLeakSummaryEntry = record
    {The user size of the block}
    BlockUsableSize: NativeInt;
    {The content of the leaked block.}
    BlockContentType: NativeUInt; //0 = unknown, 1 = AnsiString, 2 = UnicodeString, other values = class pointer
    {The number of leaks of this block size and content type.}
    NumLeaks: NativeInt;
    {The indexes of the left (False) and right (True) leaks in the binary search tree.}
    ChildIndexes: array[Boolean] of Integer;
  end;
  PMemoryLeakSummaryEntry = ^TMemoryLeakSummaryEntry;

  TMemoryLeakSummary = record
    MemoryLeakEntries: array[0..4095] of TMemoryLeakSummaryEntry;
    LeakCount: Integer;
  end;
  PMemoryLeakSummary = ^TMemoryLeakSummary;

  {-------Catching virtual calls on freed objects--------}

  {When a debug block is freed the header is set to point to this class in order to catch virtual method calls on a
  freed object.}
  TFastMM_FreedObject = class(TObject)
  protected
    class var FVirtualMethodStackTrace: TFastMM_StackTrace;
    procedure VirtualMethodOnFreedObject_LogEvent(APMethodName: PWideChar);
    procedure VirtualMethodOnFreedObject(APMethodName: PWideChar); overload;
    procedure VirtualMethodOnFreedObject(AIndex: Byte); overload;
  public
    {Virtual method calls that will redirect to VirtualMethodOnFreedObject}
    function Equals(Obj: TObject): Boolean; override;
    function GetHashCode: Integer; override;
    function ToString: string; override;
    function SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HResult; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Dispatch(var Message); override;
    procedure DefaultHandler(var Message); override;
    procedure FreeInstance; override;
    destructor Destroy; override;
    {Virtual method intercepts - these will redirect to VirtualMethodOnFreedObject}
    procedure VirtualMethod0; virtual; procedure VirtualMethod1; virtual; procedure VirtualMethod2; virtual;
    procedure VirtualMethod3; virtual; procedure VirtualMethod4; virtual; procedure VirtualMethod5; virtual;
    procedure VirtualMethod6; virtual; procedure VirtualMethod7; virtual; procedure VirtualMethod8; virtual;
    procedure VirtualMethod9; virtual; procedure VirtualMethod10; virtual; procedure VirtualMethod11; virtual;
    procedure VirtualMethod12; virtual; procedure VirtualMethod13; virtual; procedure VirtualMethod14; virtual;
    procedure VirtualMethod15; virtual; procedure VirtualMethod16; virtual; procedure VirtualMethod17; virtual;
    procedure VirtualMethod18; virtual; procedure VirtualMethod19; virtual; procedure VirtualMethod20; virtual;
    procedure VirtualMethod21; virtual; procedure VirtualMethod22; virtual; procedure VirtualMethod23; virtual;
    procedure VirtualMethod24; virtual; procedure VirtualMethod25; virtual; procedure VirtualMethod26; virtual;
    procedure VirtualMethod27; virtual; procedure VirtualMethod28; virtual; procedure VirtualMethod29; virtual;
    procedure VirtualMethod30; virtual; procedure VirtualMethod31; virtual; procedure VirtualMethod32; virtual;
    procedure VirtualMethod33; virtual; procedure VirtualMethod34; virtual; procedure VirtualMethod35; virtual;
    procedure VirtualMethod36; virtual; procedure VirtualMethod37; virtual; procedure VirtualMethod38; virtual;
    procedure VirtualMethod39; virtual; procedure VirtualMethod40; virtual; procedure VirtualMethod41; virtual;
    procedure VirtualMethod42; virtual; procedure VirtualMethod43; virtual; procedure VirtualMethod44; virtual;
    procedure VirtualMethod45; virtual; procedure VirtualMethod46; virtual; procedure VirtualMethod47; virtual;
    procedure VirtualMethod48; virtual; procedure VirtualMethod49; virtual; procedure VirtualMethod50; virtual;
    procedure VirtualMethod51; virtual; procedure VirtualMethod52; virtual; procedure VirtualMethod53; virtual;
    procedure VirtualMethod54; virtual; procedure VirtualMethod55; virtual; procedure VirtualMethod56; virtual;
    procedure VirtualMethod57; virtual; procedure VirtualMethod58; virtual; procedure VirtualMethod59; virtual;
    procedure VirtualMethod60; virtual; procedure VirtualMethod61; virtual; procedure VirtualMethod62; virtual;
    procedure VirtualMethod63; virtual; procedure VirtualMethod64; virtual; procedure VirtualMethod65; virtual;
    procedure VirtualMethod66; virtual; procedure VirtualMethod67; virtual; procedure VirtualMethod68; virtual;
    procedure VirtualMethod69; virtual; procedure VirtualMethod70; virtual; procedure VirtualMethod71; virtual;
    procedure VirtualMethod72; virtual; procedure VirtualMethod73; virtual; procedure VirtualMethod74; virtual;
  end;

  {-------Legacy debug support DLL interface--------}
  {The interface for the legacy (version 4) stack trace conversion routine in the FastMM_FullDebugMode library.}
  TFastMM_LegacyConvertStackTraceToText = function(APReturnAddresses: PNativeUInt; AMaxDepth: Cardinal;
    APBuffer: PAnsiChar): PAnsiChar;

const
  {Structure size constants}
  CBlockStatusFlagsSize = SizeOf(TBlockStatusFlags);
  CSmallBlockHeaderSize = SizeOf(TSmallBlockHeader);
  CMediumBlockHeaderSize = SizeOf(TMediumBlockHeader);
  CMediumFreeBlockFooterSize = SizeOf(TMediumFreeBlockFooter);
  CLargeBlockHeaderSize = SizeOf(TLargeBlockHeader);
  CDebugBlockHeaderSize = SizeOf(TFastMM_DebugBlockHeader);
  CDebugBlockFooterSize = SizeOf(NativeUInt);

  CSmallBlockSpanHeaderSize = SizeOf(TSmallBlockSpanHeader);
  CMediumBlockSpanHeaderSize = SizeOf(TMediumBlockSpanHeader);

  CSmallBlockManagerSize = SizeOf(TSmallBlockManager);
  CSmallBlockManagerSizeBits = 6;

  CMediumBlockManagerSize = SizeOf(TMediumBlockManager);

  CLargeBlockManagerSize = SizeOf(TLargeBlockManager);

  {Small block sizes (including the header).  The 8 byte aligned sizes are not available under 64-bit.}
  CSmallBlockSizes: array[0..CSmallBlockTypeCount - 1] of Word = (
    {8 byte jumps}
{$ifdef 32Bit}
    8,
{$endif}
    16,
{$ifdef 32Bit}
    24,
{$endif}
    32,
{$ifdef 32Bit}
    40,
{$endif}
    48,
{$ifdef 32Bit}
    56,
{$endif}
    64,
{$ifdef 32Bit}
    72,
{$endif}
    80,
{$ifdef 32Bit}
    88,
{$endif}
    96,
{$ifdef 32Bit}
    104,
{$endif}
    112,
{$ifdef 32Bit}
    120,
{$endif}
    128,
{$ifdef 32Bit}
    136,
{$endif}
    144,
{$ifdef 32Bit}
    152,
{$endif}
    160,
    {16 byte jumps}
    176,
    192,
    208,
    224,
    240,
    256,
    272,
    288,
    304,
    320,
    {32 byte jumps}
    352,
    384,
    416,
    448,
    480,
    512,
    544,
    576,
    608,
    640,
    {64 byte jumps}
    704,
    768,
    832,
    896,
    960,
    1024,
    1088,
    1152,
    1216,
    1280,
    1344,
    {128 byte jumps}
    1472,
    1600,
    1728,
    1856,
    1984,
    2112,
    2240,
    2368,
    2496,
    CMaximumSmallBlockSize // = 2624
  );

var
  {Lookup table for converting a block size to a small block type index from 0..CSmallBlockTypeCount - 1}
  SmallBlockTypeLookup: array[0.. CMaximumSmallBlockSize div CSmallBlockGranularity - 1] of Byte;

  {The small block managers.  Every arena has a separate manager for each small block size.  This should ideally be
  aligned on a 64-byte (cache line) boundary in order to prevent false dependencies between adjacent small block
  managers (RSP-28144).}
  SmallBlockManagers: TSmallBlockArenas;

  {The default size of new medium block spans.  Must be a multiple of 64K and may not exceed CMaximumMediumBlockSpanSize.}
  DefaultMediumBlockSpanSize: Integer;

  {The medium block manager for each medium block arena.}
  MediumBlockManagers: TMediumBlockArenas;

  {The large block manager for each large block arena.}
  LargeBlockManagers: TLargeBlockArenas;

  {Counts the number of time FastMM_EnterMinimumAddressAlignment less the number of times
  FastMM_ExitMinimumAddressAlignment has been called for each alignment type.}
  AlignmentRequestCounters: array[TFastMM_MinimumAddressAlignment] of Integer;

  {The current optimization stategy in effect.}
  OptimizationStrategy: TFastMM_MemoryManagerOptimizationStrategy;

{$ifdef 32Bit}
  {Pointer to the emergency reserve address space.  This allows some subsequent memory allocation requests to succeed
  in order to allow the application to allocate some memory for error handling, etc. in response to the first
  EOutOfMemory exception.  This only applies to 32-bit applications.}
  EmergencyReserveAddressSpace: Pointer;
{$endif}

  {The current installation state of FastMM.}
  CurrentInstallationState: TFastMM_MemoryManagerInstallationState;

  {The difference between the number of times EnterDebugMode has been called vs ExitDebugMode.}
  DebugModeCounter: Integer;

  {A lock that allows switching between debug and normal mode to be thread safe.}
  SettingMemoryManager: Integer; //0 = False, 1 = True;

  {The memory manager that was in place before this memory manager was installed.}
  PreviousMemoryManager: TMemoryManagerEx;
  {The memory manager that is currently set.  This is used to detect the installation of another 3rd party memory
  manager which would prevent the switching between debug and normal mode.}
  InstalledMemoryManager: TMemoryManagerEx;
  {The handle to the debug mode support DLL.}
  DebugSupportLibraryHandle: NativeUInt;
  DebugSupportConfigured: Boolean;
  {The stack trace routines from the FastMM_FullDebugMode support DLL.  These will only be set if the support DLL is
  loaded.}
  DebugLibrary_GetRawStackTrace: TFastMM_GetStackTrace;
  DebugLibrary_GetFrameBasedStackTrace: TFastMM_GetStackTrace;
  DebugLibrary_LogStackTrace_Legacy: TFastMM_LegacyConvertStackTraceToText;

  {The full path and filename for the event log.}
  EventLogFilename: array[0..CFilenameMaxLength] of WideChar;

  {The expected memory leaks list}
  ExpectedMemoryLeaks: PExpectedMemoryLeaks;
  ExpectedMemoryLeaksListLocked: Integer; //1 = Locked

{$ifdef MSWindows}
  {A string uniquely identifying the current process (for sharing the memory manager between DLLs and the main
  application).}
  SharingFileMappingObjectName: array[0..25] of AnsiChar = ('L', 'o', 'c', 'a', 'l', '\', 'F', 'a', 's', 't', 'M', 'M',
    '_', 'P', 'I', 'D', '_', '?', '?', '?', '?', '?', '?', '?', '?', #0);
  {The handle of the memory mapped file.}
  SharingFileMappingObjectHandle: NativeUInt;
{$endif}


{------------------------------------------}
{--------------Move routines---------------}
{------------------------------------------}

procedure Move16(const ASource; var ADest; ACount: NativeInt);
{$ifndef PurePascal}
asm
{$ifdef X86ASM}
  fild qword ptr [eax]
  fild qword ptr [eax + 8]
  fistp qword ptr [edx + 8]
  fistp qword ptr [edx]
{$else}
  .noframe
  movdqa xmm0, [rcx]
  movdqa [rdx], xmm0
{$endif}
{$else}
begin
  PInt64Array(@ADest)[0] := PInt64Array(@ASource)[0];
  PInt64Array(@ADest)[1] := PInt64Array(@ASource)[1];
{$endif}
end;

procedure Move32(const ASource; var ADest; ACount: NativeInt);
{$ifndef PurePascal}
asm
{$ifdef X86ASM}
  fild qword ptr [eax]
  fild qword ptr [eax + 8]
  fild qword ptr [eax + 16]
  fild qword ptr [eax + 24]
  fistp qword ptr [edx + 24]
  fistp qword ptr [edx + 16]
  fistp qword ptr [edx + 8]
  fistp qword ptr [edx]
{$else}
  .noframe
  movdqa xmm0, [rcx]
  movdqa xmm1, [rcx + 16]
  movdqa [rdx], xmm0
  movdqa [rdx + 16], xmm1
{$endif}
{$else}
begin
  PInt64Array(@ADest)[0] := PInt64Array(@ASource)[0];
  PInt64Array(@ADest)[1] := PInt64Array(@ASource)[1];
  PInt64Array(@ADest)[2] := PInt64Array(@ASource)[2];
  PInt64Array(@ADest)[3] := PInt64Array(@ASource)[3];
{$endif}
end;

procedure Move48(const ASource; var ADest; ACount: NativeInt);
{$ifndef PurePascal}
asm
{$ifdef X86ASM}
  fild qword ptr [eax]
  fild qword ptr [eax + 8]
  fild qword ptr [eax + 16]
  fild qword ptr [eax + 24]
  fild qword ptr [eax + 32]
  fild qword ptr [eax + 40]
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
  movdqa [rdx], xmm0
  movdqa [rdx + 16], xmm1
  movdqa [rdx + 32], xmm2
{$endif}
{$else}
begin
  PInt64Array(@ADest)[0] := PInt64Array(@ASource)[0];
  PInt64Array(@ADest)[1] := PInt64Array(@ASource)[1];
  PInt64Array(@ADest)[2] := PInt64Array(@ASource)[2];
  PInt64Array(@ADest)[3] := PInt64Array(@ASource)[3];
  PInt64Array(@ADest)[4] := PInt64Array(@ASource)[4];
  PInt64Array(@ADest)[5] := PInt64Array(@ASource)[5];
{$endif}
end;

procedure Move64(const ASource; var ADest; ACount: NativeInt);
{$ifndef PurePascal}
asm
{$ifdef X86ASM}
  fild qword ptr [eax]
  fild qword ptr [eax + 8]
  fild qword ptr [eax + 16]
  fild qword ptr [eax + 24]
  fild qword ptr [eax + 32]
  fild qword ptr [eax + 40]
  fild qword ptr [eax + 48]
  fild qword ptr [eax + 56]
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
  movdqa [rdx], xmm0
  movdqa [rdx + 16], xmm1
  movdqa [rdx + 32], xmm2
  movdqa [rdx + 48], xmm3
{$endif}
{$else}
begin
  PInt64Array(@ADest)[0] := PInt64Array(@ASource)[0];
  PInt64Array(@ADest)[1] := PInt64Array(@ASource)[1];
  PInt64Array(@ADest)[2] := PInt64Array(@ASource)[2];
  PInt64Array(@ADest)[3] := PInt64Array(@ASource)[3];
  PInt64Array(@ADest)[4] := PInt64Array(@ASource)[4];
  PInt64Array(@ADest)[5] := PInt64Array(@ASource)[5];
  PInt64Array(@ADest)[6] := PInt64Array(@ASource)[6];
  PInt64Array(@ADest)[7] := PInt64Array(@ASource)[7];
{$endif}
end;

{64-bit is always 16 byte aligned, so the 8 byte aligned moves are not needed under 64-bit.}
{$ifdef 32Bit}
procedure Move8(const ASource; var ADest; ACount: NativeInt);
{$ifdef X86ASM}
asm
  fild qword ptr [eax]
  fistp qword ptr [edx]
{$else}
begin
  PInt64Array(@ADest)[0] := PInt64Array(@ASource)[0];
{$endif}
end;

procedure Move24(const ASource; var ADest; ACount: NativeInt);
{$ifdef X86ASM}
asm
  fild qword ptr [eax]
  fild qword ptr [eax + 8]
  fild qword ptr [eax + 16]
  fistp qword ptr [edx + 16]
  fistp qword ptr [edx + 8]
  fistp qword ptr [edx]
{$else}
begin
  PInt64Array(@ADest)[0] := PInt64Array(@ASource)[0];
  PInt64Array(@ADest)[1] := PInt64Array(@ASource)[1];
  PInt64Array(@ADest)[2] := PInt64Array(@ASource)[2];
{$endif}
end;

procedure Move40(const ASource; var ADest; ACount: NativeInt);
{$ifdef X86ASM}
asm
  fild qword ptr [eax]
  fild qword ptr [eax + 8]
  fild qword ptr [eax + 16]
  fild qword ptr [eax + 24]
  fild qword ptr [eax + 32]
  fistp qword ptr [edx + 32]
  fistp qword ptr [edx + 24]
  fistp qword ptr [edx + 16]
  fistp qword ptr [edx + 8]
  fistp qword ptr [edx]
{$else}
begin
  PInt64Array(@ADest)[0] := PInt64Array(@ASource)[0];
  PInt64Array(@ADest)[1] := PInt64Array(@ASource)[1];
  PInt64Array(@ADest)[2] := PInt64Array(@ASource)[2];
  PInt64Array(@ADest)[3] := PInt64Array(@ASource)[3];
  PInt64Array(@ADest)[4] := PInt64Array(@ASource)[4];
{$endif}
end;

procedure Move56(const ASource; var ADest; ACount: NativeInt);
{$ifdef X86ASM}
asm
  fild qword ptr [eax]
  fild qword ptr [eax + 8]
  fild qword ptr [eax + 16]
  fild qword ptr [eax + 24]
  fild qword ptr [eax + 32]
  fild qword ptr [eax + 40]
  fild qword ptr [eax + 48]
  fistp qword ptr [edx + 48]
  fistp qword ptr [edx + 40]
  fistp qword ptr [edx + 32]
  fistp qword ptr [edx + 24]
  fistp qword ptr [edx + 16]
  fistp qword ptr [edx + 8]
  fistp qword ptr [edx]
{$else}
begin
  PInt64Array(@ADest)[0] := PInt64Array(@ASource)[0];
  PInt64Array(@ADest)[1] := PInt64Array(@ASource)[1];
  PInt64Array(@ADest)[2] := PInt64Array(@ASource)[2];
  PInt64Array(@ADest)[3] := PInt64Array(@ASource)[3];
  PInt64Array(@ADest)[4] := PInt64Array(@ASource)[4];
  PInt64Array(@ADest)[5] := PInt64Array(@ASource)[5];
  PInt64Array(@ADest)[6] := PInt64Array(@ASource)[6];
{$endif}
end;

{Moves 8x bytes from ASource to ADest, where x is an integer >= 1.  ASource and ADest are assumed to be aligned on a 8
byte boundary.  The source and destination buffers may not overlap.}
procedure MoveMultipleOf8(const ASource; var ADest; ACount: NativeInt);
{$ifdef X86ASM}
asm
  add eax, ecx
  add edx, ecx
  neg ecx
@MoveLoop:
  fild qword ptr [eax + ecx]
  fistp qword ptr [edx + ecx]
  add ecx, 8
  js @MoveLoop
{$else}
var
  LPSource, LPDest: PByte;
begin
  LPSource := @PByte(@ASource)[ACount];
  LPDest := @PByte(@ADest)[ACount];
  ACount := - ACount;

  while True do
  begin
    PInt64Array(@LPDest[ACount])[0] := PInt64Array(@LPSource[ACount])[0];

    Inc(ACount, 8);
    if ACount >= 0 then
      Break;
  end;
{$endif}
end;

{$ifdef X86ASM}
procedure MoveMultipleOf16_x86_SSE2(const ASource; var ADest; ACount: NativeInt);
asm
  add eax, ecx
  add edx, ecx
  neg ecx
@MoveLoop:
  movdqa xmm0, [eax + ecx]
  movdqa [edx + ecx], xmm0
  add ecx, 16
  js @MoveLoop
end;

procedure MoveMultipleOf32_x86_SSE2(const ASource; var ADest; ACount: NativeInt);
asm
  add eax, ecx
  add edx, ecx
  neg ecx
@MoveLoop:
  movdqa xmm0, [eax + ecx]
  movdqa xmm1, [eax + ecx + 16]
  movdqa [edx + ecx], xmm0
  movdqa [edx + ecx + 16], xmm1
  add ecx, 32
  js @MoveLoop
end;

procedure MoveMultipleOf64_Small_x86_SSE2(const ASource; var ADest; ACount: NativeInt);
asm
  add eax, ecx
  add edx, ecx
  neg ecx
@MoveLoop:
  movdqa xmm0, [eax + ecx]
  movdqa xmm1, [eax + ecx + 16]
  movdqa xmm2, [eax + ecx + 32]
  movdqa xmm3, [eax + ecx + 48]
  movdqa [edx + ecx], xmm0
  movdqa [edx + ecx + 16], xmm1
  movdqa [edx + ecx + 32], xmm2
  movdqa [edx + ecx + 48], xmm3
  add ecx, 64
  js @MoveLoop
end;
{$endif}

{$endif}

{Moves 16x bytes from ASource to ADest, where x is an integer >= 1.  ASource and ADest are assumed to be aligned on a
16 byte boundary.  The source and destination buffers may not overlap.}
procedure MoveMultipleOf16(const ASource; var ADest; ACount: NativeInt);
{$ifndef PurePascal}
asm
{$ifdef X86ASM}
  add eax, ecx
  add edx, ecx
  neg ecx
@MoveLoop:
  fild qword ptr [eax + ecx]
  fild qword ptr [eax + ecx + 8]
  fistp qword ptr [edx + ecx + 8]
  fistp qword ptr [edx + ecx]
  add ecx, 16
  js @MoveLoop
{$else}
  .noframe
  add rcx, r8
  add rdx, r8
  neg r8
@MoveLoop:
  movdqa xmm0, [rcx + r8]
  movdqa [rdx + r8], xmm0
  add r8, 16
  js @MoveLoop
{$endif}
{$else}
var
  LPSource, LPDest: PByte;
begin
  LPSource := @PByte(@ASource)[ACount];
  LPDest := @PByte(@ADest)[ACount];
  ACount := - ACount;

  while True do
  begin
    PInt64Array(@LPDest[ACount])[0] := PInt64Array(@LPSource[ACount])[0];
    PInt64Array(@LPDest[ACount])[1] := PInt64Array(@LPSource[ACount])[1];

    Inc(ACount, 16);
    if ACount >= 0 then
      Break;
  end;
{$endif}
end;

{Moves 32x bytes from ASource to ADest, where x is an integer >= 1.  ASource and ADest are assumed to be aligned on a
32 byte boundary.  The source and destination buffers may not overlap.}
procedure MoveMultipleOf32(const ASource; var ADest; ACount: NativeInt);
{$ifndef PurePascal}
asm
{$ifdef X86ASM}
  add eax, ecx
  add edx, ecx
  neg ecx
@MoveLoop:
  fild qword ptr [eax + ecx]
  fild qword ptr [eax + ecx + 8]
  fild qword ptr [eax + ecx + 16]
  fild qword ptr [eax + ecx + 24]
  fistp qword ptr [edx + ecx + 24]
  fistp qword ptr [edx + ecx + 16]
  fistp qword ptr [edx + ecx + 8]
  fistp qword ptr [edx + ecx]
  add ecx, 32
  js @MoveLoop
{$else}
  .noframe
  add rcx, r8
  add rdx, r8
  neg r8
@MoveLoop:
  movdqa xmm0, [rcx + r8]
  movdqa xmm1, [rcx + r8 + 16]
  movdqa [rdx + r8], xmm0
  movdqa [rdx + r8 + 16], xmm1
  add r8, 32
  js @MoveLoop
{$endif}
{$else}
var
  LPSource, LPDest: PByte;
begin
  LPSource := @PByte(@ASource)[ACount];
  LPDest := @PByte(@ADest)[ACount];
  ACount := - ACount;

  while True do
  begin
    PInt64Array(@LPDest[ACount])[0] := PInt64Array(@LPSource[ACount])[0];
    PInt64Array(@LPDest[ACount])[1] := PInt64Array(@LPSource[ACount])[1];
    PInt64Array(@LPDest[ACount])[2] := PInt64Array(@LPSource[ACount])[2];
    PInt64Array(@LPDest[ACount])[3] := PInt64Array(@LPSource[ACount])[3];

    Inc(ACount, 32);
    if ACount >= 0 then
      Break;
  end;
{$endif}
end;

{Moves 64x bytes from ASource to ADest, where x is an integer >= 1.  ASource and ADest are assumed to be aligned on a
64 byte boundary.  The source and destination buffers may not overlap.}
procedure MoveMultipleOf64_Small(const ASource; var ADest; ACount: NativeInt);
{$ifndef PurePascal}
asm
{$ifdef X86ASM}
  add eax, ecx
  add edx, ecx
  neg ecx
@MoveLoop:
  fild qword ptr [eax + ecx]
  fild qword ptr [eax + ecx + 8]
  fild qword ptr [eax + ecx + 16]
  fild qword ptr [eax + ecx + 24]
  fild qword ptr [eax + ecx + 32]
  fild qword ptr [eax + ecx + 40]
  fild qword ptr [eax + ecx + 48]
  fild qword ptr [eax + ecx + 56]
  fistp qword ptr [edx + ecx + 56]
  fistp qword ptr [edx + ecx + 48]
  fistp qword ptr [edx + ecx + 40]
  fistp qword ptr [edx + ecx + 32]
  fistp qword ptr [edx + ecx + 24]
  fistp qword ptr [edx + ecx + 16]
  fistp qword ptr [edx + ecx + 8]
  fistp qword ptr [edx + ecx]
  add ecx, 64
  js @MoveLoop
{$else}
  .noframe
  add rcx, r8
  add rdx, r8
  neg r8
@MoveLoop:
  movdqa xmm0, [rcx + r8]
  movdqa xmm1, [rcx + r8 + 16]
  movdqa xmm2, [rcx + r8 + 32]
  movdqa xmm3, [rcx + r8 + 48]
  movdqa [rdx + r8], xmm0
  movdqa [rdx + r8 + 16], xmm1
  movdqa [rdx + r8 + 32], xmm2
  movdqa [rdx + r8 + 48], xmm3
  add r8, 64
  js @MoveLoop
{$endif}
{$else}
var
  LPSource, LPDest: PByte;
begin
  LPSource := @PByte(@ASource)[ACount];
  LPDest := @PByte(@ADest)[ACount];
  ACount := - ACount;

  while True do
  begin
    PInt64Array(@LPDest[ACount])[0] := PInt64Array(@LPSource[ACount])[0];
    PInt64Array(@LPDest[ACount])[1] := PInt64Array(@LPSource[ACount])[1];
    PInt64Array(@LPDest[ACount])[2] := PInt64Array(@LPSource[ACount])[2];
    PInt64Array(@LPDest[ACount])[3] := PInt64Array(@LPSource[ACount])[3];
    PInt64Array(@LPDest[ACount])[4] := PInt64Array(@LPSource[ACount])[4];
    PInt64Array(@LPDest[ACount])[5] := PInt64Array(@LPSource[ACount])[5];
    PInt64Array(@LPDest[ACount])[6] := PInt64Array(@LPSource[ACount])[6];
    PInt64Array(@LPDest[ACount])[7] := PInt64Array(@LPSource[ACount])[7];

    Inc(ACount, 64);
    if ACount >= 0 then
      Break;
  end;
{$endif}
end;

{As above, but optimized for larger blocks.  The startup cost for REP MOVS is high, but it is significantly faster with
large blocks on modern CPUs.  If ACount is not a multiple of 64 then at least ACount bytes will be moved, possibly
more.}
procedure MoveMultipleOf64_Large(const ASource; var ADest; ACount: NativeInt);
{$ifndef PurePascal}
asm
{$ifdef X86ASM}
  cld
  add ecx, 3 //round up the number of dwords
  shr ecx, 2
  push esi
  push edi
  mov esi, eax
  mov edi, edx
  rep movsd
  pop edi
  pop esi
{$else}
  .noframe
  .pushnv rsi
  .pushnv rdi
  cld
  add r8, 7 //round up the number of qwords
  shr r8, 3
  mov rsi, rcx
  mov rdi, rdx
  mov rcx, r8
  rep movsq
{$endif}
{$else}
var
  LPSource, LPDest: PByte;
begin
  LPSource := @PByte(@ASource)[ACount];
  LPDest := @PByte(@ADest)[ACount];
  ACount := - ACount;

  while True do
  begin
    PInt64Array(@LPDest[ACount])[0] := PInt64Array(@LPSource[ACount])[0];
    PInt64Array(@LPDest[ACount])[1] := PInt64Array(@LPSource[ACount])[1];
    PInt64Array(@LPDest[ACount])[2] := PInt64Array(@LPSource[ACount])[2];
    PInt64Array(@LPDest[ACount])[3] := PInt64Array(@LPSource[ACount])[3];
    PInt64Array(@LPDest[ACount])[4] := PInt64Array(@LPSource[ACount])[4];
    PInt64Array(@LPDest[ACount])[5] := PInt64Array(@LPSource[ACount])[5];
    PInt64Array(@LPDest[ACount])[6] := PInt64Array(@LPSource[ACount])[6];
    PInt64Array(@LPDest[ACount])[7] := PInt64Array(@LPSource[ACount])[7];

    Inc(ACount, 64);
    if ACount >= 0 then
      Break;
  end;
{$endif}
end;


{------------------------------------------}
{---------Operating system calls-----------}
{------------------------------------------}

procedure ReleaseEmergencyReserveAddressSpace; forward;
function CharCount(APFirstFreeChar, APBufferStart: PWideChar): Integer; forward;

{Allocates a block of memory from the operating system.  The block is assumed to be aligned to at least a 64 byte
boundary, and is assumed to be zero initialized.  Returns nil on error.}
function OS_AllocateVirtualMemory(ABlockSize: NativeInt; AAllocateTopDown: Boolean;
  AReserveOnlyNoReadWriteAccess: Boolean): Pointer;
begin
  if AReserveOnlyNoReadWriteAccess then
  begin
    Result := Winapi.Windows.VirtualAlloc(nil, ABlockSize, MEM_RESERVE, PAGE_NOACCESS);
  end
  else
  begin
    Result := Winapi.Windows.VirtualAlloc(nil, ABlockSize, MEM_COMMIT, PAGE_READWRITE);
    {The emergency address space reserve is released when address space runs out for the first time.  This allows some
    subsequent memory allocation requests to succeed in order to allow the application to allocate some memory for error
    handling, etc. in response to the EOutOfMemory exception.  This only applies to 32-bit applications.}
    if Result = nil then
      ReleaseEmergencyReserveAddressSpace;
  end;
end;

function OS_AllocateVirtualMemoryAtAddress(APAddress: Pointer; ABlockSize: NativeInt;
  AReserveOnlyNoReadWriteAccess: Boolean): Boolean;
begin
  if AReserveOnlyNoReadWriteAccess then
  begin
    Result := Winapi.Windows.VirtualAlloc(APAddress, ABlockSize, MEM_RESERVE, PAGE_NOACCESS) <> nil;
  end
  else
  begin
    Result := (Winapi.Windows.VirtualAlloc(APAddress, ABlockSize, MEM_RESERVE, PAGE_READWRITE) <> nil)
      and (Winapi.Windows.VirtualAlloc(APAddress, ABlockSize, MEM_COMMIT, PAGE_READWRITE) <> nil);
  end;
end;

{Releases a block of memory back to the operating system.  Returns 0 on success, -1 on failure.}
function OS_FreeVirtualMemory(APointer: Pointer): Integer;
begin
  if Winapi.Windows.VirtualFree(APointer, 0, MEM_RELEASE) then
    Result := 0
  else
    Result := -1;
end;

{Determines the size and state of the virtual memory region starting at APRegionStart.}
procedure OS_GetVirtualMemoryRegionInfo(APRegionStart: Pointer; var AMemoryRegionInfo: TMemoryRegionInfo);
var
  LMemInfo: TMemoryBasicInformation;
begin
  {VirtualQuery might fail if the address is not aligned on a 4K boundary, e.g. it fails when called on Pointer(-1).}
  APRegionStart := Pointer(NativeUInt(APRegionStart) and (not (CVirtualMemoryPageSize - 1)));

  Winapi.Windows.VirtualQuery(APRegionStart, LMemInfo, SizeOf(LMemInfo));

  AMemoryRegionInfo.RegionStartAddress := LMemInfo.BaseAddress;
  AMemoryRegionInfo.RegionSize := LMemInfo.RegionSize;
  AMemoryRegionInfo.RegionIsFree := LMemInfo.State = MEM_FREE;
  AMemoryRegionInfo.AccessRights := [];
  if (LMemInfo.State = MEM_COMMIT) and (LMemInfo.Protect and PAGE_GUARD = 0) then
  begin
    if (LMemInfo.Protect and (PAGE_READONLY or PAGE_READWRITE or PAGE_EXECUTE or PAGE_EXECUTE_READ or PAGE_EXECUTE_READWRITE or PAGE_EXECUTE_WRITECOPY) <> 0) then
      Include(AMemoryRegionInfo.AccessRights, marRead);
    if (LMemInfo.Protect and (PAGE_READWRITE or PAGE_EXECUTE_READWRITE or PAGE_EXECUTE_WRITECOPY) <> 0) then
      Include(AMemoryRegionInfo.AccessRights, marWrite);
    if (LMemInfo.Protect and (PAGE_EXECUTE or PAGE_EXECUTE_READ or PAGE_EXECUTE_READWRITE or PAGE_EXECUTE_WRITECOPY) <> 0) then
      Include(AMemoryRegionInfo.AccessRights, marExecute);
  end;
end;

{If another thread is ready to run on the current CPU, give it a chance to execute.  This is typically called if the
current thread is unable to make any progress, because it is waiting for locked resources.}
procedure OS_AllowOtherThreadToRun; inline;
begin
  Winapi.Windows.SwitchToThread;
end;

{Returns the thread ID for the calling thread.}
function OS_GetCurrentThreadID: Cardinal; inline;
begin
  Result := Winapi.Windows.GetCurrentThreadID;
end;

{Returns the current system date and time.  The time is in 24 hour format.}
procedure OS_GetCurrentDateTime(var AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliseconds: Word);
var
  LSystemTime: TSystemTime;
begin
  Winapi.Windows.GetLocalTime(LSystemTime);
  AYear := LSystemTime.wYear;
  AMonth := LSystemTime.wMonth;
  ADay := LSystemTime.wDay;
  AHour := LSystemTime.wHour;
  AMinute := LSystemTime.wMinute;
  ASecond := LSystemTime.wSecond;
  AMilliseconds := LSystemTime.wMilliseconds;
end;

{Returns the number of milliseconds that have elapsed since the system was started.  Note that this wraps back to 0
after 49.7 days.}
function OS_GetMillisecondsSinceStartup: Cardinal;
begin
  Result := Winapi.Windows.GetTickCount;
end;

{Fills a buffer with the full path and filename of the application.  If AReturnLibraryFilename = True and this is a
library then the full path and filename of the library is returned instead.}
function OS_GetApplicationFilename(APFilenameBuffer, APBufferEnd: PWideChar; AReturnLibraryFilename: Boolean): PWideChar;
var
  LModuleHandle: HMODULE;
  LNumChars: Cardinal;
begin
  Result := APFilenameBuffer;

  LModuleHandle := 0;
  if AReturnLibraryFilename and IsLibrary then
    LModuleHandle := HInstance;

  LNumChars := Winapi.Windows.GetModuleFileNameW(LModuleHandle, Result, CharCount(APBufferEnd, APFilenameBuffer));
  Inc(Result, LNumChars);
end;

function OS_GetEnvironmentVariableValue(APEnvironmentVariableName, APValueBuffer, APBufferEnd: PWideChar): PWideChar;
var
  LNumChars, LBufferSize: Cardinal;
begin
  Result := APValueBuffer;

  if Result >= APBufferEnd then
    Exit;

  LBufferSize := (NativeInt(APBufferEnd) - NativeInt(Result)) div SizeOf(WideChar);
  LNumChars := Winapi.Windows.GetEnvironmentVariableW(APEnvironmentVariableName, Result, LBufferSize);
  if LNumChars < LBufferSize then
    Inc(Result, LNumChars);
end;

{Returns True if the given file exists.  APFileName must be a #0 terminated.}
function OS_FileExists(APFileName: PWideChar): Boolean;
begin
  {This will return True for folders and False for files that are locked by another process, but is "good enough" for
  the purpose for which it will be used.}
  Result := Winapi.Windows.GetFileAttributesW(APFileName) <> INVALID_FILE_ATTRIBUTES;
end;

{Attempts to delete the file.  Returns True if it was successfully deleted.}
function OS_DeleteFile(APFileName: PWideChar): Boolean;
begin
  Result := Winapi.Windows.DeleteFileW(APFileName);
end;

{Creates the given file if it does not exist yet, and then appends the given data to it.}
function OS_CreateOrAppendFile(APFileName: PWideChar; APData: Pointer; ADataSizeInBytes: Integer): Boolean;
var
  LFileHandle: THandle;
  LBytesWritten: Cardinal;
begin
  if ADataSizeInBytes <= 0 then
    Exit(True);

  {Try to open/create the log file in read/write mode.}
  LFileHandle := Winapi.Windows.CreateFileW(APFileName, GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_ALWAYS,
    FILE_ATTRIBUTE_NORMAL, 0);
  if LFileHandle = INVALID_HANDLE_VALUE then
    Exit(False);

  {Add the data to the end of the file}
  SetFilePointer(LFileHandle, 0, nil, FILE_END);
  Winapi.Windows.WriteFile(LFileHandle, APData^, Cardinal(ADataSizeInBytes), LBytesWritten, nil);
  Result := LBytesWritten = Cardinal(ADataSizeInBytes);

  CloseHandle(LFileHandle);
end;

procedure OS_OutputDebugString(APDebugMessage: PWideChar); inline;
begin
  Winapi.Windows.OutputDebugString(APDebugMessage);
end;

{Shows a message box if the program is not showing one already.}
procedure OS_ShowMessageBox(APText, APCaption: PWideChar);
begin
  Winapi.Windows.MessageBoxW(0, APText, APCaption, MB_OK or MB_ICONERROR or MB_TASKMODAL or MB_DEFAULT_DESKTOP_ONLY);
end;


{------------------------------------------}
{--------Logging support subroutines-------}
{------------------------------------------}

function CharCount(APFirstFreeChar, APBufferStart: PWideChar): Integer; inline;
begin
  Result := (NativeInt(APFirstFreeChar) - NativeInt(APBufferStart)) div SizeOf(WideChar);
end;

{Converts the UTF-16 text pointed to by APWideText to UTF-8 in the buffer provided.  Returns a pointer to the byte
after the last output character.}
function ConvertUTF16toUTF8(APWideText: PWideChar; ANumWideChars: Integer; APOutputBuffer: PByte): PByte;
var
  LPIn, LPEnd: PWord;
  LCode: Cardinal;
begin
  Result := Pointer(APOutputBuffer);

  LPIn := Pointer(APWideText);
  LPEnd := LPIn;
  Inc(LPEnd, ANumWideChars);

  while NativeUInt(LPIn) < NativeUInt(LPEnd) do
  begin
    LCode := PCardinal(LPIn)^;
    if Word(LCode) <= $7f then
    begin
      if LCode <= $7fffff then
      begin
        {Both characters are single byte}
        PWord(Result)^ := Word(LCode or (LCode shr 8));
        Inc(Result, 2);
        Inc(LPIn, 2);
      end
      else
      begin
        {The second character is not single byte}
        Result[0] := Byte(LCode);
        Inc(Result);
        Inc(LPIn);
      end;
    end
    else
    begin
      if Word(LCode) <= $7ff then
      begin
        {Two byte encoding}
        Result[0] := Byte(LCode shr 6) or $c0;
        Result[1] := Byte(LCode and $3f) or $80;
        Inc(Result, 2);
        Inc(LPIn);
      end
      else
      begin
        if (LCode and $fc00fc00) <> $dc00d800 then
        begin
          {Three byte encoding}
          Result[0] := Byte((LCode shr 12) and $0f) or $e0;
          Result[1] := Byte((LCode shr 6) and $3f) or $80;
          Result[2] := Byte(LCode and $3f) or $80;
          Inc(Result, 3);
          Inc(LPIn);
        end
        else
        begin
          {It is a surrogate pair (4 byte) encoding:  Surrogate pairs are encoded in four bytes, with the high word
          first}
          LCode := ((LCode and $3ff) shl 10) + ((LCode shr 16) and $3ff) + $10000;
          Result[0] := Byte((LCode shr 18) and $07) or $e0;
          Result[1] := Byte((LCode shr 12) and $3f) or $80;
          Result[2] := Byte((LCode shr 6) and $3f) or $80;
          Result[3] := Byte(LCode and $3f) or $80;
          Inc(Result, 4);
          Inc(LPIn, 2);
        end;
      end;
    end;
  end;
  {Did we convert past the end?}
  if NativeUInt(LPIn) > NativeUInt(LPEnd) then
    Dec(Result);
end;

function AppendTextFile(APFileName, APText: PWideChar; AWideCharCount: Integer): Boolean;
var
  LBufferSize: Integer;
  LPBufferStart, LPBufferPos: PByte;
begin
  {We need to add either a BOM or a couple of line breaks before the text, so a larger buffer is needed than the
  maximum text size.  If converting to UTF-8 it is also possible for the resulting text to be bigger than the UTF-16
  encoded text.}
  LBufferSize := (AWideCharCount + 4) * 3;

  LPBufferStart := OS_AllocateVirtualMemory(LBufferSize, False, False);
  if LPBufferStart = nil then
    Exit(False);

  try
    LPBufferPos := LPBufferStart;

    if OS_FileExists(APFileName) then
    begin
      {The log file exists:  Add a line break after the previous event.}
      if FastMM_TextFileEncoding in [teUTF8, teUTF8_BOM] then
      begin
        PWord(LPBufferPos)^ := $0A0D;
        Inc(LPBufferPos, 2);
      end
      else
      begin
        PCardinal(LPBufferPos)^ := $000A000D;
        Inc(LPBufferPos, 4);
      end;
    end
    else
    begin
      {The file does not exist, so add the BOM if required.}
      if FastMM_TextFileEncoding = teUTF8_BOM then
      begin
        PCardinal(LPBufferPos)^ := $BFBBEF;
        Inc(LPBufferPos, 3);
      end else if FastMM_TextFileEncoding = teUTF16LE_BOM then
      begin
        PWord(LPBufferPos)^ := $FEFF;
        Inc(LPBufferPos, 2);
      end;
    end;

    {Copy the text across to the buffer, converting it as appropriate.}
    if FastMM_TextFileEncoding in [teUTF8, teUTF8_BOM] then
    begin
      LPBufferPos := ConvertUTF16toUTF8(APText, AWideCharCount, LPBufferPos);
    end
    else
    begin
      System.Move(APText^, LPBufferPos^, AWideCharCount * 2);
      Inc(LPBufferPos, AWideCharCount * 2);
    end;

    Result := OS_CreateOrAppendFile(APFileName, LPBufferStart, NativeInt(LPBufferPos) - NativeInt(LPBufferStart));

  finally
    OS_FreeVirtualMemory(LPBufferStart);
  end;
end;

{Returns the class for a memory block.  Returns nil if it is not a valid class.  Used by the leak detection code.}
function DetectClassInstance(APointer: Pointer): TClass;
var
  LMemoryRegionInfo: TMemoryRegionInfo;

  {Checks whether the given address is a valid address for a VMT entry.}
  function IsValidVMTAddress(APAddress: Pointer): Boolean;
  begin
    {Do some basic pointer checks:  Must be pointer aligned and beyond 64K. (The low 64K is never readable, at least
    under Windows.)}
    if (NativeUInt(APAddress) <= 65535)
      or (NativeUInt(APAddress) and (SizeOf(Pointer) - 1) <> 0) then
    begin
      Exit(False);
    end;

    {Fetch the memory access flags for the region surrounding the pointer, if required.}
    if (NativeUInt(APAddress) < NativeUInt(LMemoryRegionInfo.RegionStartAddress))
      or (NativeUInt(APAddress) - NativeUInt(LMemoryRegionInfo.RegionStartAddress) >= LMemoryRegionInfo.RegionSize) then
    begin
      OS_GetVirtualMemoryRegionInfo(APAddress, LMemoryRegionInfo);
    end;

    {The address must be readable.}
    Result := (not LMemoryRegionInfo.RegionIsFree)
      and (marRead in LMemoryRegionInfo.AccessRights);
  end;

  {Returns True if AClassPointer points to a class VMT}
  function InternalIsValidClass(AClassPointer: Pointer; ADepth: Integer = 0): Boolean;
  var
    LParentClassSelfPointer: PPointer;
  begin
    {Check that the self pointer as well as parent class self pointer addresses are valid}
    if (ADepth < 1000)
      and (NativeUInt(AClassPointer) > 65535)
      and IsValidVMTAddress(Pointer(PByte(AClassPointer) + vmtSelfPtr))
      and IsValidVMTAddress(Pointer(PByte(AClassPointer) + vmtParent)) then
    begin
      {Get a pointer to the parent class' self pointer}
      LParentClassSelfPointer := PPointer(PByte(AClassPointer) + vmtParent)^;
      {Is the "Self" pointer valid?}
      if PPointer(PByte(AClassPointer) + vmtSelfPtr)^ <> AClassPointer then
        Exit(False);
      {No more parent classes?}
      if LParentClassSelfPointer = nil then
        Exit(True);
      {Recursively check the parent class for validity.}
      Result := IsValidVMTAddress(LParentClassSelfPointer)
        and InternalIsValidClass(LParentClassSelfPointer^, ADepth + 1);
    end
    else
      Result := False;
  end;

begin
  {Get the class pointer from the (suspected) object}
  Result := TClass(PPointer(APointer)^);
  {No VM info yet}
  LMemoryRegionInfo.RegionSize := 0;
  {Check the block.  Never return TFastMM_FreedObject as the class.}
  if (Result = TFastMM_FreedObject) or (not InternalIsValidClass(Pointer(Result), 0)) then
    Result := nil;
end;

{Detects the probable string data type for a memory block.  Used by the leak classification code when a block cannot be
identified as a known class instance.}
function DetectStringData(APMemoryBlock: Pointer; AAvailableSpaceInBlock: NativeInt): TStringDataType;
type
  {The layout of a string header.}
  PStrRec = ^StrRec;
  StrRec = packed record
{$ifdef 64Bit}
    _Padding: Integer;
{$endif}
    codePage: Word;
    elemSize: Word;
    refCnt: Integer;
    length: Integer;
  end;
const
  {If the string reference count field contains a value greater than this, then it is assumed that the block is not a
  string.}
  CMaxRefCount = 255;
  {The lowest ASCII character code considered valid string data.  If there are any characters below this code point
  then the data is assumed not to be a string.}
  CMinCharCode = #9; //#9 = Tab.
var
  LStringLength, LElementSize, LCharInd: Integer;
  LPAnsiString: PAnsiChar;
  LPUnicodeString: PWideChar;
begin
  {Check that the reference count is within a reasonable range}
  if PStrRec(APMemoryBlock).refCnt > CMaxRefCount then
    Exit(stNotAString);

  {Element size must be either 1 (Ansi) or 2 (Unicode)}
  LElementSize := PStrRec(APMemoryBlock).elemSize;
  if (LElementSize <> 1) and (LElementSize <> 2) then
    Exit(stNotAString);

  {Get the string length and check whether it fits inside the block}
  LStringLength := PStrRec(APMemoryBlock).length;
  if (LStringLength <= 0)
    or (LStringLength >= (AAvailableSpaceInBlock - SizeOf(StrRec)) div LElementSize) then
  begin
    Exit(stNotAString);
  end;

  {Check for no characters outside the expected range.  If there are, then it is probably not a string.}
  if LElementSize = 1 then
  begin
    LPAnsiString := PAnsiChar(PByte(APMemoryBlock) + SizeOf(StrRec));

    {There must be a trailing #0}
    if LPAnsiString[LStringLength] <> #0 then
      Exit(stNotAString);

    {Check that all characters are in the range considered valid.}
    for LCharInd := 0 to LStringLength - 1 do
    begin
      if LPAnsiString[LCharInd] < CMinCharCode then
        Exit(stNotAString);
    end;

    Result := stAnsiString;
  end
  else
  begin
    LPUnicodeString := PWideChar(PByte(APMemoryBlock) + SizeOf(StrRec));

    {There must be a trailing #0}
    if LPUnicodeString[LStringLength] <> #0 then
      Exit(stNotAString);

    {Check that all characters are in the range considered valid.}
    for LCharInd := 0 to LStringLength - 1 do
    begin
      if LPUnicodeString[LCharInd] < CMinCharCode then
        Exit(stNotAString);
    end;

    Result := stUnicodeString;
  end;
end;

{Attempts to detect the class or string type of the given block.  Possible return values are:
  0 = Unknown class
  1 = AnsiString text
  1 = UnicodeString text
  > 1 = TClass Pointer}
function DetectBlockContentType(APMemoryBlock: Pointer; AAvailableSpaceInBlock: NativeInt): NativeUInt;
var
  LLeakedClass: TClass;
  LStringType: TStringDataType;
begin
  {Attempt to determine the class type for the block.}
  LLeakedClass := DetectClassInstance(APMemoryBlock);
  if LLeakedClass <> nil then
    Exit(NativeUInt(LLeakedClass));

  LStringType := DetectStringData(APMemoryBlock, AAvailableSpaceInBlock);
  Result := Ord(LStringType);
end;

{Counts the number of characters up to the trailing #0}
function GetStringLength(APWideText: PWideChar): Integer;
begin
  Result := 0;

  if APWideText = nil then
    Exit;

  while APWideText^ <> #0 do
  begin
    Inc(Result);
    Inc(APWideText);
  end;
end;

{Adds text to a buffer, returning the new buffer position.}
function AppendTextToBuffer(APSource: PWideChar; ACharCount: Integer;
  APTarget, APTargetBufferEnd: PWideChar): PWideChar; overload;
begin
  Result := APTarget;

  if @Result[ACharCount] > APTargetBufferEnd then
    ACharCount := CharCount(APTargetBufferEnd, Result);

  if ACharCount > 0 then
  begin
    System.Move(APSource^, Result^, ACharCount * SizeOf(WideChar));
    Inc(Result, ACharCount);
  end;
end;

{As above, but if APSource is non-nil then it is assumed to be #0 terminated.  The trailing #0 is not copied.}
function AppendTextToBuffer(APSource, APTarget, APTargetBufferEnd: PWideChar): PWideChar; overload;
var
  LChar: WideChar;
begin
  Result := APTarget;

  if APSource = nil then
    Exit;

  while Result < APTargetBufferEnd do
  begin
    LChar := APSource^;
    if LChar = #0 then
      Break;

    Result^ := LChar;
    Inc(APSource);
    Inc(Result);
  end;
end;

{Converts a NativeUInt to hexadecimal text in the given target buffer.}
function NativeUIntToHexadecimalBuffer(AValue: NativeUInt; APTarget, APTargetBufferEnd: PWideChar): PWideChar;
var
  LTempBuffer: array[0..15] of WideChar;
  LDigit: NativeInt;
  LDigitCount: Integer;
  LPPos: PWideChar;
begin
  Result := APTarget;

  LPPos := @LTempBuffer[High(LTempBuffer)];
  LDigitCount := 0;
  while True do
  begin
    LDigit := AValue mod 16;
    LPPos^ := CHexDigits[LDigit];
    Inc(LDigitCount);

    AValue := AValue div 16;
    if AValue = 0 then
      Break;

    Dec(LPPos);
  end;

  Result := AppendTextToBuffer(LPPos, LDigitCount, Result, APTargetBufferEnd);
end;

{Converts a NativeUInt to text in the given target buffer.}
function NativeUIntToTextBuffer(AValue: NativeUInt; APTarget, APTargetBufferEnd: PWideChar): PWideChar;
var
  LTempBuffer: array[0..20] of WideChar;
  LDigit: NativeInt;
  LDigitCount: Integer;
  LPPos: PWideChar;
begin
  Result := APTarget;

  LPPos := @LTempBuffer[High(LTempBuffer)];
  LDigitCount := 0;
  while True do
  begin
    LDigit := AValue mod 10;
    LPPos^ := WideChar(Ord('0') + LDigit);
    Inc(LDigitCount);

    AValue := AValue div 10;
    if AValue = 0 then
      Break;

    Dec(LPPos);
  end;

  Result := AppendTextToBuffer(LPPos, LDigitCount, Result, APTargetBufferEnd);
end;

{Converts a NativeInt to text in the given target buffer.}
function NativeIntToTextBuffer(AValue: NativeInt; APTarget, APTargetBufferEnd: PWideChar): PWideChar;
const
  CMinusSign: PWideChar = '-';
begin
  Result := APTarget;

  if AValue < 0 then
    Result := AppendTextToBuffer(@CMinusSign, 1, Result, APTargetBufferEnd);

  Result := NativeUIntToTextBuffer(Abs(AValue), Result, APTargetBufferEnd);
end;

function BlockContentTypeToTextBuffer(ABlockContentType: NativeUInt; APTarget, APTargetBufferEnd: PWideChar): PWideChar;
type
  PClassData = ^TClassData;
  TClassData = record
    ClassType: TClass;
    ParentInfo: Pointer;
    PropCount: SmallInt;
    UnitName: ShortString;
  end;
const
  CUnknown = 'Unknown';
  CAnsiString = 'AnsiString';
  CUnicodeString = 'UnicodeString';
var
  LClass: TClass;
  LBuffer: array[0..511] of WideChar;
  LPTarget: PWideChar;
  LPSource: PAnsiChar;
  LCharInd, LNumChars: Integer;
  LClassInfo: Pointer;
  LPShortString: PShortString;
begin
  Result := APTarget;

  case ABlockContentType of
    0: Result := AppendTextToBuffer(CUnknown, Length(CUnknown), Result, APTargetBufferEnd);
    1: Result := AppendTextToBuffer(CAnsiString, Length(CAnsiString), Result, APTargetBufferEnd);
    2: Result := AppendTextToBuffer(CUnicodeString, Length(CUnicodeString), Result, APTargetBufferEnd);

    else
    begin
      {All other content types are classes.}
      LClass := TClass(ABlockContentType);

      LPTarget := @LBuffer;

      {Get the name of the unit.}
      LClassInfo := LClass.ClassInfo;
      if LClassInfo <> nil then
      begin
        LPShortString := @PClassData(PByte(LClassInfo) + 2 + PByte(PByte(LClassInfo) + 1)^).UnitName;
        LPSource := @LPShortString^[1];
        LNumChars := Length(LPShortString^);

        while LNumChars > 0 do
        begin
          if LPSource^ = ':' then
            Break;

          if LPSource^ <> '@' then
          begin
            LPTarget^ := WideChar(LPSource^);
            Inc(LPTarget);
          end;

          Inc(LPSource);
          Dec(LNumChars);
        end;
        LPTarget^ := '.';
        Inc(LPTarget);
      end;

      {Append the class name}
      LPShortString := PShortString(PPointer(PByte(LClass) + vmtClassName)^);
      LPSource := @LPShortString^[1];
      LNumChars := Length(LPShortString^);
      for LCharInd := 1 to LNumChars do
      begin
        LPTarget^ := WideChar(LPSource^);
        Inc(LPTarget);
        Inc(LPSource);
      end;

      Result := AppendTextToBuffer(@LBuffer, CharCount(LPTarget, @LBuffer), Result, APTargetBufferEnd);
    end;

  end;
end;

{Copies a token value to the buffer and sets the pointer to the token in the values array.  Copies up to the size of
the target buffer.}
function AddTokenValue(var ATokenValues: TEventLogTokenValues; ATokenID: Integer; APTokenValue: PWideChar;
  ACharCount: Integer; APBuffer, APBufferEnd: PWideChar): PWideChar;
begin
  Result := APBuffer;

  if Cardinal(ATokenID) > High(ATokenValues) then
    Exit;

  if (ACharCount <= 0)
    or (@Result[ACharCount] >= APBufferEnd) then
  begin
    ATokenValues[ATokenID] := nil;
    Exit;
  end;

  ATokenValues[ATokenID] := Result;
  Result := AppendTextToBuffer(APTokenValue, ACharCount, Result, APBufferEnd);

  {Store the trailing #0}
  Result^ := #0;
  Inc(Result);
end;

function AddTokenValue_NativeInt(var ATokenValues: TEventLogTokenValues; ATokenID: Integer; ATokenValue: NativeInt;
  APTokenValueBufferPos, APBufferEnd: PWideChar): PWideChar;
var
  LTempBuffer: array[0..21] of WideChar;
  LPPos: PWideChar;
begin
  Result := APTokenValueBufferPos;

  LPPos := NativeIntToTextBuffer(ATokenValue, @LTempBuffer, @LTempBuffer[High(LTempBuffer)]);

  Result := AddTokenValue(ATokenValues, ATokenID, @LTempBuffer, CharCount(LPPos, @LTempBuffer), Result, APBufferEnd);
end;

function AddTokenValue_NativeUInt(var ATokenValues: TEventLogTokenValues; ATokenID: Integer; ATokenValue: NativeUInt;
  APTokenValueBufferPos, APBufferEnd: PWideChar): PWideChar;
var
  LTempBuffer: array[0..20] of WideChar;
  LPPos: PWideChar;
begin
  Result := APTokenValueBufferPos;

  LPPos := NativeUIntToTextBuffer(ATokenValue, @LTempBuffer, @LTempBuffer[High(LTempBuffer)]);

  Result := AddTokenValue(ATokenValues, ATokenID, @LTempBuffer, CharCount(LPPos, @LTempBuffer), Result, APBufferEnd);
end;

function AddTokenValue_Hexadecimal(var ATokenValues: TEventLogTokenValues; ATokenID: Integer; ATokenValue: NativeUInt;
  APTokenValueBufferPos, APBufferEnd: PWideChar): PWideChar;
var
  LTempBuffer: array[0..15] of WideChar;
  LPPos: PWideChar;
begin
  Result := APTokenValueBufferPos;

  LPPos := NativeUIntToHexadecimalBuffer(ATokenValue, @LTempBuffer, @LTempBuffer[High(LTempBuffer)]);

  Result := AddTokenValue(ATokenValues, ATokenID, @LTempBuffer, CharCount(LPPos, @LTempBuffer), Result, APBufferEnd);
end;

function AddTokenValue_HexDump(var ATokenValues: TEventLogTokenValues; ATokenID: Integer; APBlock: PByte;
  ANumBytes: Integer; APTokenValueBufferPos, APBufferEnd: PWideChar): PWideChar;
var
  LTempBuffer: array[0..CMemoryDumpMaxBytes * 5] of WideChar; //Worst case scenario:  Allow for CRLF after every byte
  LPTarget: PWideChar;
  LBytesLeftInLine: Integer;
  LByteVal: Byte;
begin
  Result := APTokenValueBufferPos;

  if ANumBytes > CMemoryDumpMaxBytes then
    ANumBytes := CMemoryDumpMaxBytes;
  if ANumBytes <= 0 then
    Exit;

  LPTarget := @LTempBuffer;
  LBytesLeftInLine := CMemoryDumpMaxBytesPerLine;
  while True do
  begin
    LByteVal := APBlock^;
    LPTarget^ := CHexDigits[LByteVal div 16];
    Inc(LPTarget);
    LPTarget^ := CHexDigits[LByteVal and 15];
    Inc(LPTarget);
    Inc(APBlock);

    Dec(ANumBytes);
    if ANumBytes = 0 then
      Break;

    {Add the separator:  Either a space or a line break.}
    Dec(LBytesLeftInLine);
    if LBytesLeftInLine <= 0 then
    begin
      {Add a CRLF at the end of the line}
      LPTarget^ := #13;
      Inc(LPTarget);
      LPTarget^ := #10;
      Inc(LPTarget);

      LBytesLeftInLine := CMemoryDumpMaxBytesPerLine;
    end
    else
    begin
      LPTarget^ := ' ';
      Inc(LPTarget);
    end;

  end;

  Result := AddTokenValue(ATokenValues, ATokenID, @LTempBuffer, CharCount(LPTarget, @LTempBuffer), Result, APBufferEnd);
end;

function AddTokenValue_ASCIIDump(var ATokenValues: TEventLogTokenValues; ATokenID: Integer; APBlock: PByte;
  ANumBytes: Integer; APTokenValueBufferPos, APBufferEnd: PWideChar): PWideChar;
var
  LTempBuffer: array[0..CMemoryDumpMaxBytes * 5] of WideChar; //Worst case scenario:  Allow for CRLF after every byte
  LPTarget: PWideChar;
  LBytesLeftInLine: Integer;
  LByteVal: Byte;
begin
  Result := APTokenValueBufferPos;

  if ANumBytes > CMemoryDumpMaxBytes then
    ANumBytes := CMemoryDumpMaxBytes;
  if ANumBytes <= 0 then
    Exit;

  LPTarget := @LTempBuffer;
  LBytesLeftInLine := CMemoryDumpMaxBytesPerLine;
  while True do
  begin
    LByteVal := APBlock^;
    if (LByteVal > Ord(' ')) and (LByteVal < 128) then
      LPTarget^ := Char(LByteVal)
    else
      LPTarget^ := '.';
    Inc(LPTarget);
    Inc(APBlock);

    Dec(ANumBytes);
    if ANumBytes = 0 then
      Break;

    {Add the separator:  Either a space or a line break.}
    Dec(LBytesLeftInLine);
    if LBytesLeftInLine <= 0 then
    begin
      {Add a CRLF at the end of the line}
      LPTarget^ := #13;
      Inc(LPTarget);
      LPTarget^ := #10;
      Inc(LPTarget);

      LBytesLeftInLine := CMemoryDumpMaxBytesPerLine;
    end
    else
    begin
      LPTarget^ := ' ';
      Inc(LPTarget);
      LPTarget^ := ' ';
      Inc(LPTarget);
    end;

  end;

  Result := AddTokenValue(ATokenValues, ATokenID, @LTempBuffer, CharCount(LPTarget, @LTempBuffer), Result, APBufferEnd);
end;

function AddTokenValue_StackTrace(var ATokenValues: TEventLogTokenValues; ATokenID: Integer;
  const AStackTrace: TFastMM_StackTrace; APTokenValueBufferPos, APBufferEnd: PWideChar): PWideChar;
var
  LStackTraceBuffer: array[0..CFastMM_StackTraceEntryCount * 160] of WideChar;
  LPBuffer: PWideChar;
begin
  Result := APTokenValueBufferPos;

  LPBuffer := FastMM_ConvertStackTraceToText(@AStackTrace, CFastMM_StackTraceEntryCount, @LStackTraceBuffer,
    @LStackTraceBuffer[High(LStackTraceBuffer)]);

  Result := AddTokenValue(ATokenValues, ATokenID, LStackTraceBuffer, CharCount(LPBuffer, @LStackTraceBuffer), Result,
    APBufferEnd);
end;

{Adds a date token in ISO 8601 date format, e.g. 2020-01-01}
function AddTokenValue_Date(var ATokenValues: TEventLogTokenValues; ATokenID: Integer; AYear, AMonth, ADay: Word;
  APTokenValueBufferPos, APBufferEnd: PWideChar): PWideChar;
var
  LDateBuffer: array[0..9] of WideChar;
begin
  Result := APTokenValueBufferPos;

  LDateBuffer[3] := WideChar(Ord('0') + AYear mod 10);
  AYear := AYear div 10;
  LDateBuffer[2] := WideChar(Ord('0') + AYear mod 10);
  AYear := AYear div 10;
  LDateBuffer[1] := WideChar(Ord('0') + AYear mod 10);
  AYear := AYear div 10;
  LDateBuffer[0] := WideChar(Ord('0') + AYear mod 10);

  LDateBuffer[4] := '-';
  LDateBuffer[6] := WideChar(Ord('0') + AMonth mod 10);
  AMonth := AMonth div 10;
  LDateBuffer[5] := WideChar(Ord('0') + AMonth mod 10);

  LDateBuffer[7] := '-';
  LDateBuffer[9] := WideChar(Ord('0') + ADay mod 10);
  ADay := ADay div 10;
  LDateBuffer[8] := WideChar(Ord('0') + ADay mod 10);

  Result := AddTokenValue(ATokenValues, ATokenID, @LDateBuffer, Length(LDateBuffer), Result, APBufferEnd);
end;

{Adds a date token in ISO 8601 date format, e.g. 2020-01-01}
function AddTokenValue_Time(var ATokenValues: TEventLogTokenValues; ATokenID: Integer; AHour, AMinute, ASecond: Word;
  APTokenValueBufferPos, APBufferEnd: PWideChar): PWideChar;
var
  LTimeBuffer: array[0..7] of WideChar;
begin
  Result := APTokenValueBufferPos;

  LTimeBuffer[1] := WideChar(Ord('0') + AHour mod 10);
  AHour := AHour div 10;
  LTimeBuffer[0] := WideChar(Ord('0') + AHour mod 10);

  LTimeBuffer[2] := ':';
  LTimeBuffer[4] := WideChar(Ord('0') + AMinute mod 10);
  AMinute := AMinute div 10;
  LTimeBuffer[3] := WideChar(Ord('0') + AMinute mod 10);

  LTimeBuffer[5] := ':';
  LTimeBuffer[7] := WideChar(Ord('0') + ASecond mod 10);
  ASecond := ASecond div 10;
  LTimeBuffer[6] := WideChar(Ord('0') + ASecond mod 10);

  Result := AddTokenValue(ATokenValues, ATokenID, @LTimeBuffer, Length(LTimeBuffer), Result, APBufferEnd);
end;

{Adds the tokens for the current date and time.}
function AddTokenValues_CurrentDateAndTime(var ATokenValues: TEventLogTokenValues;
  APTokenValueBufferPos, APBufferEnd: PWideChar): PWideChar;
var
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMilliseconds: Word;
begin
  Result := APTokenValueBufferPos;

  OS_GetCurrentDateTime(LYear, LMonth, LDay, LHour, LMinute, LSecond, LMilliseconds);

  Result := AddTokenValue_Date(ATokenValues, CEventLogTokenCurrentDate, LYear, LMonth, LDay, Result, APBufferEnd);
  Result := AddTokenValue_Time(ATokenValues, CEventLogTokenCurrentTime, LHour, LMinute, LSecond, Result, APBufferEnd);
end;

function AddTokenValue_BlockContentType(var ATokenValues: TEventLogTokenValues; ATokenID: Integer;
  ABlockContentType: NativeUInt; APTokenValueBufferPos, APBufferEnd: PWideChar): PWideChar;
const
  CContentBufferSize = 512;
var
  LBuffer: array[0..CContentBufferSize] of WideChar;
  LPPos: PWideChar;
begin
  Result := APTokenValueBufferPos;

  LPPos := BlockContentTypeToTextBuffer(ABlockContentType, @LBuffer, @LBuffer[High(LBuffer)]);

  Result := AddTokenValue(ATokenValues, ATokenID, @LBuffer, CharCount(LPPos, @LBuffer), Result, APBufferEnd);
end;

function AddTokenValues_GeneralTokens(var ATokenValues: TEventLogTokenValues;
  APTokenValueBufferPos, APBufferEnd: PWideChar): PWideChar;
begin
  Result := AddTokenValues_CurrentDateAndTime(ATokenValues, APTokenValueBufferPos, APBufferEnd);
  Result := AddTokenValue(ATokenValues, CEventLogTokenEventLogFilename, @EventLogFilename,
    GetStringLength(@EventLogFilename), Result, APBufferEnd);
end;

function AddTokenValues_BlockTokens(var ATokenValues: TEventLogTokenValues; APBlock: Pointer;
  APBuffer, APBufferEnd: PWideChar): PWideChar;
var
  LBlockUserSize: NativeInt;
  LBlockContentType: NativeUInt;
  LMemoryDumpSize, LBlockHeader: Integer;
  LPDebugBlockHeader: PFastMM_DebugBlockHeader;
begin
  Result := APBuffer;

  {Add the token for the block size.}
  LBlockUserSize := FastMM_BlockMaximumUserBytes(APBlock);
  Result := AddTokenValue_NativeInt(ATokenValues, CEventLogTokenBlockSize, LBlockUserSize, Result, APBufferEnd);

  {Add the token for the block content type.}
  LBlockContentType := DetectBlockContentType(APBlock, LBlockUserSize);
  Result := AddTokenValue_BlockContentType(ATokenValues, CEventLogTokenObjectClass, LBlockContentType, Result,
    APBufferEnd);

  {Add the token for the block adddress in hex.}
  Result := AddTokenValue_Hexadecimal(ATokenValues, CEventLogTokenBlockAddress, NativeUInt(APBlock), Result,
    APBufferEnd);

  {Add the block dump tokens.  The maximum dump size is less than the size of a medium block, so it's safe to read
  beyond the end of the block (due to the medium block header that will always follow a small block span).}
  if LBlockUserSize < CMemoryDumpMaxBytes - CMediumBlockHeaderSize then
    LMemoryDumpSize := LBlockUserSize + CMediumBlockHeaderSize
  else
    LMemoryDumpSize := CMemoryDumpMaxBytes;

  Result := AddTokenValue_NativeInt(ATokenValues, CEventLogTokenMemoryDumpSize, LMemoryDumpSize, Result, APBufferEnd);

  Result := AddTokenValue_HexDump(ATokenValues, CEventLogTokenHexDump, APBlock, LMemoryDumpSize, Result, APBufferEnd);

  Result := AddTokenValue_ASCIIDump(ATokenValues, CEventLogTokenASCIIDump, APBlock, LMemoryDumpSize, Result, APBufferEnd);

  {If this is a debug sub-block, log the additional debug information.}
  LBlockHeader := PBlockStatusFlags(APBlock)[-1];
  if LBlockHeader and (CIsSmallBlockFlag or CIsMediumBlockFlag or CIsLargeBlockFlag or CIsDebugBlockFlag) = CIsDebugBlockFlag then
  begin
    LPDebugBlockHeader := @PFastMM_DebugBlockHeader(APBlock)[-1];

    Result := AddTokenValue_Hexadecimal(ATokenValues, CEventLogTokenAllocatedByThread, LPDebugBlockHeader.AllocatedByThread,
      Result, APBufferEnd);

    Result := AddTokenValue_NativeUInt(ATokenValues, CEventLogTokenAllocationNumber, LPDebugBlockHeader.AllocationNumber,
      Result, APBufferEnd);

    Result := AddTokenValue_StackTrace(ATokenValues, CEventLogTokenAllocationStackTrace, LPDebugBlockHeader.AllocationStackTrace,
      Result, APBufferEnd);

    if LBlockHeader and CBlockIsFreeFlag = CBlockIsFreeFlag then
    begin
      Result := AddTokenValue_Hexadecimal(ATokenValues, CEventLogTokenFreedByThread, LPDebugBlockHeader.FreedByThread,
        Result, APBufferEnd);

      Result := AddTokenValue_StackTrace(ATokenValues, CEventLogTokenFreeStackTrace, LPDebugBlockHeader.FreeStackTrace,
        Result, APBufferEnd);

      {If it is a freed debug block then get the prior class from the debug header.}
      LBlockContentType := NativeUInt(DetectClassInstance(@LPDebugBlockHeader.PreviouslyUsedByClass));
      Result := AddTokenValue_BlockContentType(ATokenValues, CEventLogTokenObjectClass, LBlockContentType, Result,
        APBufferEnd);

    end;
  end;

end;

{The template as well as token values must be #0 terminated.}
function SubstituteTokenValues(APTemplate: PWideChar; const ATokenValues: TEventLogTokenValues;
  APBuffer, APBufferEnd: PWideChar): PWideChar;
const
  CTokenStartChar = '{';
  CTokenEndChar = '}';
var
  LInputChar: WideChar;
  LInsideToken: Boolean;
  LTokenNumber: Cardinal;
  LPTokenValue: PWideChar;
begin
  LInsideToken := False;
  LTokenNumber := 0;
  Result := APBuffer;

  while Result < APBufferEnd do
  begin
    LInputChar := APTemplate^;
    if LInputChar = #0 then
      Break;
    Inc(APTemplate);

    if not LInsideToken then
    begin
      if LInputChar <> CTokenStartChar then
      begin
        Result^ := LInputChar;
        Inc(Result);
      end
      else
      begin
        LInsideToken := True;
        LTokenNumber := 0;
      end;
    end
    else
    begin
      if LInputChar <> CTokenEndChar then
      begin
        LTokenNumber := LTokenNumber * 10 + Ord(LInputChar) - Ord('0');
      end
      else
      begin
        if LTokenNumber <= CEventLogMaxTokenID then
        begin
          LPTokenValue := ATokenValues[LTokenNumber];
          if LPTokenValue <> nil then
          begin
            while Result < APBufferEnd do
            begin
              LInputChar := LPTokenValue^;
              if LInputChar = #0 then
                Break;
              Inc(LPTokenValue);

              Result^ := LInputChar;
              Inc(Result);

            end;
          end;

        end;
        LInsideToken := False;
      end;
    end;

  end;
end;

{Logs an event to OutputDebugString, file or the display (or any combination thereof) depending on configuration.}
procedure LogEvent(AEventType: TFastMM_MemoryManagerEventType; const ATokenValues: TEventLogTokenValues);
var
  LPTextTemplate, LPMessageBoxCaption: PWideChar;
  LTextBuffer: array[0..CEventMessageMaxWideChars] of WideChar;
  LPLogHeaderStart, LPBodyStart: PWideChar;
  LPBuffer, LPBufferEnd: PWideChar;
begin
  LPLogHeaderStart := @LTextBuffer;
  LPBufferEnd := @LTextBuffer[CEventMessageMaxWideChars - 1];
  LPBuffer := LPLogHeaderStart;

  {Add the log file header.}
  if AEventType in FastMM_LogToFileEvents then
    LPBuffer := SubstituteTokenValues(FastMM_LogFileEntryHeader, ATokenValues, LPBuffer, LPBufferEnd);
  LPBodyStart := LPBuffer;

  {Add the message itself.}
  case AEventType of

    mmetAnotherThirdPartyMemoryManagerAlreadyInstalled:
    begin
      LPTextTemplate := FastMM_AnotherMemoryManagerAlreadyInstalledMessage;
      LPMessageBoxCaption := FastMM_CannotSwitchMemoryManagerMessageBoxCaption;
    end;

    mmetCannotInstallAfterDefaultMemoryManagerHasBeenUsed:
    begin
      LPTextTemplate := FastMM_CannotInstallAfterDefaultMemoryManagerHasBeenUsedMessage;
      LPMessageBoxCaption := FastMM_CannotSwitchMemoryManagerMessageBoxCaption;
    end;

    mmetCannotSwitchToSharedMemoryManagerWithLivePointers:
    begin
      LPTextTemplate := FastMM_CannotSwitchToSharedMemoryManagerWithLivePointersMessage;
      LPMessageBoxCaption := FastMM_CannotSwitchMemoryManagerMessageBoxCaption;
    end;

    mmetUnexpectedMemoryLeakDetail:
    begin
      {Determine which template to use from the block type:  Only debug blocks have thread information.}
      if ATokenValues[CEventLogTokenAllocatedByThread] <> nil then
        LPTextTemplate := FastMM_MemoryLeakDetailMessage_DebugBlock
      else
        LPTextTemplate := FastMM_MemoryLeakDetailMessage_NormalBlock;
      LPMessageBoxCaption := FastMM_MemoryLeakMessageBoxCaption;
    end;

    mmetUnexpectedMemoryLeakSummary:
    begin
      if mmetUnexpectedMemoryLeakDetail in FastMM_LogToFileEvents then
        LPTextTemplate := FastMM_MemoryLeakSummaryMessage_LeakDetailLoggedToEventLog
      else
        LPTextTemplate := FastMM_MemoryLeakSummaryMessage_LeakDetailNotLogged;
      LPMessageBoxCaption := FastMM_MemoryLeakMessageBoxCaption;
    end;

    mmetDebugBlockDoubleFree:
    begin
      LPTextTemplate := FastMM_DebugBlockDoubleFree;
      LPMessageBoxCaption := FastMM_MemoryCorruptionMessageBoxCaption;
    end;

    mmetDebugBlockReallocOfFreedBlock:
    begin
      LPTextTemplate := FastMM_DebugBlockReallocOfFreedBlock;
      LPMessageBoxCaption := FastMM_MemoryCorruptionMessageBoxCaption;
    end;

    mmetDebugBlockHeaderCorruption:
    begin
      LPTextTemplate := FastMM_BlockHeaderCorruptedMessage;
      LPMessageBoxCaption := FastMM_MemoryCorruptionMessageBoxCaption;
    end;

    mmetDebugBlockFooterCorruption:
    begin
      if ATokenValues[CEventLogTokenFreedByThread] <> nil then
        LPTextTemplate := FastMM_BlockFooterCorruptedMessage_FreedBlock
      else
        LPTextTemplate := FastMM_BlockFooterCorruptedMessage_AllocatedBlock;
      LPMessageBoxCaption := FastMM_MemoryCorruptionMessageBoxCaption;
    end;

    mmetDebugBlockModifiedAfterFree:
    begin
      LPTextTemplate := FastMM_BlockModifiedAfterFreeMessage;
      LPMessageBoxCaption := FastMM_MemoryCorruptionMessageBoxCaption;
    end;

    mmetVirtualMethodCallOnFreedObject:
    begin
      LPTextTemplate := FastMM_VirtualMethodCallOnFreedObjectMessage;
      LPMessageBoxCaption := FastMM_VirtualMethodCallOnFreedObjectMessageBoxCaption;
    end;

  else
    begin
      {All event types should be handled above.}
      LPTextTemplate := nil;
      LPMessageBoxCaption := nil;
    end;
  end;
  LPBuffer := SubstituteTokenValues(LPTextTemplate, ATokenValues, LPBuffer, LPBufferEnd);

  {Store the trailing #0.}
  LPBuffer^ := #0;

  {Log the message to file, if needed.}
  if AEventType in FastMM_LogToFileEvents then
  begin
    AppendTextFile(@EventLogFilename, LPLogHeaderStart, CharCount(LPBuffer, @LTextBuffer));
  end;

  if AEventType in FastMM_OutputDebugStringEvents then
  begin
    OS_OutputDebugString(LPLogHeaderStart);
  end;

  if AEventType in FastMM_MessageBoxEvents then
  begin
    OS_ShowMessageBox(LPBodyStart, LPMessageBoxCaption);
  end;

end;


{--------------------------------------}
{--------Debug support class-----------}
{--------------------------------------}

{TFastMM_FreedObject is used to catch virtual method calls on a freed object.  Whenever a debug block is freed the
first pointer in the block is set to point to TFastMM_FreedObject, so that an attempt to call a virtual method (like
Destroy) will be caught.}

{ TFastMM_FreedObject }

procedure TFastMM_FreedObject.AfterConstruction;
begin
  VirtualMethodOnFreedObject('AfterConstruction');
end;

procedure TFastMM_FreedObject.BeforeDestruction;
begin
  VirtualMethodOnFreedObject('BeforeDestruction');
end;

procedure TFastMM_FreedObject.DefaultHandler(var Message);
begin
  VirtualMethodOnFreedObject('DefaultHandler');
end;

destructor TFastMM_FreedObject.Destroy;
begin
  VirtualMethodOnFreedObject('Destroy');
end;

procedure TFastMM_FreedObject.Dispatch(var Message);
begin
  VirtualMethodOnFreedObject('Dispatch');
end;

function TFastMM_FreedObject.Equals(Obj: TObject): Boolean;
begin
  VirtualMethodOnFreedObject('Equals');
  Result := False; //Suppress compiler warning
end;

procedure TFastMM_FreedObject.FreeInstance;
begin
  VirtualMethodOnFreedObject('FreeInstance');
end;

function TFastMM_FreedObject.GetHashCode: Integer;
begin
  VirtualMethodOnFreedObject('GetHashCode');
  Result := 0; //Suppress compiler warning
end;

function TFastMM_FreedObject.SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HResult;
begin
  VirtualMethodOnFreedObject('SafeCallException');
  Result := 0; //Suppress compiler warning
end;

function TFastMM_FreedObject.ToString: string;
begin
  VirtualMethodOnFreedObject('ToString');
  Result := ''; //Suppress compiler warning
end;

procedure TFastMM_FreedObject.VirtualMethodOnFreedObject(APMethodName: PWideChar);
begin
  {Get the stack trace and then log the event.}
  FastMM_GetStackTrace(@FVirtualMethodStackTrace, CFastMM_StackTraceEntryCount, 0);
  VirtualMethodOnFreedObject_LogEvent(APMethodName);
end;

procedure TFastMM_FreedObject.VirtualMethodOnFreedObject(AIndex: Byte);
var
  LTextBuffer: array[0..4] of WideChar;
  LPEnd: PWideChar;
begin
  LTextBuffer[0] := '#';
  LPEnd := NativeUIntToTextBuffer(AIndex, @LTextBuffer[1], @LTextBuffer[High(LTextBuffer)]);
  LPEnd^ := #0;
  VirtualMethodOnFreedObject(@LTextBuffer);
end;

procedure TFastMM_FreedObject.VirtualMethodOnFreedObject_LogEvent(APMethodName: PWideChar);
var
  LTokenValues: TEventLogTokenValues;
  LTokenValueBuffer: array[0..CTokenBufferMaxWideChars - 1] of WideChar;
  LPBufferPos, LPBufferEnd: PWideChar;
begin
  LTokenValues := Default(TEventLogTokenValues);

  LPBufferEnd := @LTokenValueBuffer[High(LTokenValueBuffer)];
  LPBufferPos := AddTokenValues_GeneralTokens(LTokenValues, @LTokenValueBuffer, LPBufferEnd);
  LPBufferPos := AddTokenValues_BlockTokens(LTokenValues, Pointer(Self), LPBufferPos, LPBufferEnd);
  LPBufferPos := AddTokenValue(LTokenValues, CEventLogTokenVirtualMethodName, APMethodName,
    GetStringLength(APMethodName), LPBufferPos, LPBufferEnd);
  AddTokenValue_StackTrace(LTokenValues, CEventLogTokenVirtualMethodCallOnFreedObject,
    TFastMM_FreedObject.FVirtualMethodStackTrace, LPBufferPos, LPBufferEnd);

  LogEvent(mmetVirtualMethodCallOnFreedObject, LTokenValues);

  System.Error(reInvalidPtr);
end;

procedure TFastMM_FreedObject.VirtualMethod0; begin VirtualMethodOnFreedObject(0); end;
procedure TFastMM_FreedObject.VirtualMethod1; begin VirtualMethodOnFreedObject(1); end;
procedure TFastMM_FreedObject.VirtualMethod2; begin VirtualMethodOnFreedObject(2); end;
procedure TFastMM_FreedObject.VirtualMethod3; begin VirtualMethodOnFreedObject(3); end;
procedure TFastMM_FreedObject.VirtualMethod4; begin VirtualMethodOnFreedObject(4); end;
procedure TFastMM_FreedObject.VirtualMethod5; begin VirtualMethodOnFreedObject(5); end;
procedure TFastMM_FreedObject.VirtualMethod6; begin VirtualMethodOnFreedObject(6); end;
procedure TFastMM_FreedObject.VirtualMethod7; begin VirtualMethodOnFreedObject(7); end;
procedure TFastMM_FreedObject.VirtualMethod8; begin VirtualMethodOnFreedObject(8); end;
procedure TFastMM_FreedObject.VirtualMethod9; begin VirtualMethodOnFreedObject(9); end;
procedure TFastMM_FreedObject.VirtualMethod10; begin VirtualMethodOnFreedObject(10); end;
procedure TFastMM_FreedObject.VirtualMethod11; begin VirtualMethodOnFreedObject(11); end;
procedure TFastMM_FreedObject.VirtualMethod12; begin VirtualMethodOnFreedObject(12); end;
procedure TFastMM_FreedObject.VirtualMethod13; begin VirtualMethodOnFreedObject(13); end;
procedure TFastMM_FreedObject.VirtualMethod14; begin VirtualMethodOnFreedObject(14); end;
procedure TFastMM_FreedObject.VirtualMethod15; begin VirtualMethodOnFreedObject(15); end;
procedure TFastMM_FreedObject.VirtualMethod16; begin VirtualMethodOnFreedObject(16); end;
procedure TFastMM_FreedObject.VirtualMethod17; begin VirtualMethodOnFreedObject(17); end;
procedure TFastMM_FreedObject.VirtualMethod18; begin VirtualMethodOnFreedObject(18); end;
procedure TFastMM_FreedObject.VirtualMethod19; begin VirtualMethodOnFreedObject(19); end;
procedure TFastMM_FreedObject.VirtualMethod20; begin VirtualMethodOnFreedObject(20); end;
procedure TFastMM_FreedObject.VirtualMethod21; begin VirtualMethodOnFreedObject(21); end;
procedure TFastMM_FreedObject.VirtualMethod22; begin VirtualMethodOnFreedObject(22); end;
procedure TFastMM_FreedObject.VirtualMethod23; begin VirtualMethodOnFreedObject(23); end;
procedure TFastMM_FreedObject.VirtualMethod24; begin VirtualMethodOnFreedObject(24); end;
procedure TFastMM_FreedObject.VirtualMethod25; begin VirtualMethodOnFreedObject(25); end;
procedure TFastMM_FreedObject.VirtualMethod26; begin VirtualMethodOnFreedObject(26); end;
procedure TFastMM_FreedObject.VirtualMethod27; begin VirtualMethodOnFreedObject(27); end;
procedure TFastMM_FreedObject.VirtualMethod28; begin VirtualMethodOnFreedObject(28); end;
procedure TFastMM_FreedObject.VirtualMethod29; begin VirtualMethodOnFreedObject(29); end;
procedure TFastMM_FreedObject.VirtualMethod30; begin VirtualMethodOnFreedObject(30); end;
procedure TFastMM_FreedObject.VirtualMethod31; begin VirtualMethodOnFreedObject(31); end;
procedure TFastMM_FreedObject.VirtualMethod32; begin VirtualMethodOnFreedObject(32); end;
procedure TFastMM_FreedObject.VirtualMethod33; begin VirtualMethodOnFreedObject(33); end;
procedure TFastMM_FreedObject.VirtualMethod34; begin VirtualMethodOnFreedObject(34); end;
procedure TFastMM_FreedObject.VirtualMethod35; begin VirtualMethodOnFreedObject(35); end;
procedure TFastMM_FreedObject.VirtualMethod36; begin VirtualMethodOnFreedObject(36); end;
procedure TFastMM_FreedObject.VirtualMethod37; begin VirtualMethodOnFreedObject(37); end;
procedure TFastMM_FreedObject.VirtualMethod38; begin VirtualMethodOnFreedObject(38); end;
procedure TFastMM_FreedObject.VirtualMethod39; begin VirtualMethodOnFreedObject(39); end;
procedure TFastMM_FreedObject.VirtualMethod40; begin VirtualMethodOnFreedObject(40); end;
procedure TFastMM_FreedObject.VirtualMethod41; begin VirtualMethodOnFreedObject(41); end;
procedure TFastMM_FreedObject.VirtualMethod42; begin VirtualMethodOnFreedObject(42); end;
procedure TFastMM_FreedObject.VirtualMethod43; begin VirtualMethodOnFreedObject(43); end;
procedure TFastMM_FreedObject.VirtualMethod44; begin VirtualMethodOnFreedObject(44); end;
procedure TFastMM_FreedObject.VirtualMethod45; begin VirtualMethodOnFreedObject(45); end;
procedure TFastMM_FreedObject.VirtualMethod46; begin VirtualMethodOnFreedObject(46); end;
procedure TFastMM_FreedObject.VirtualMethod47; begin VirtualMethodOnFreedObject(47); end;
procedure TFastMM_FreedObject.VirtualMethod48; begin VirtualMethodOnFreedObject(48); end;
procedure TFastMM_FreedObject.VirtualMethod49; begin VirtualMethodOnFreedObject(49); end;
procedure TFastMM_FreedObject.VirtualMethod50; begin VirtualMethodOnFreedObject(50); end;
procedure TFastMM_FreedObject.VirtualMethod51; begin VirtualMethodOnFreedObject(51); end;
procedure TFastMM_FreedObject.VirtualMethod52; begin VirtualMethodOnFreedObject(52); end;
procedure TFastMM_FreedObject.VirtualMethod53; begin VirtualMethodOnFreedObject(53); end;
procedure TFastMM_FreedObject.VirtualMethod54; begin VirtualMethodOnFreedObject(54); end;
procedure TFastMM_FreedObject.VirtualMethod55; begin VirtualMethodOnFreedObject(55); end;
procedure TFastMM_FreedObject.VirtualMethod56; begin VirtualMethodOnFreedObject(56); end;
procedure TFastMM_FreedObject.VirtualMethod57; begin VirtualMethodOnFreedObject(57); end;
procedure TFastMM_FreedObject.VirtualMethod58; begin VirtualMethodOnFreedObject(58); end;
procedure TFastMM_FreedObject.VirtualMethod59; begin VirtualMethodOnFreedObject(59); end;
procedure TFastMM_FreedObject.VirtualMethod60; begin VirtualMethodOnFreedObject(60); end;
procedure TFastMM_FreedObject.VirtualMethod61; begin VirtualMethodOnFreedObject(61); end;
procedure TFastMM_FreedObject.VirtualMethod62; begin VirtualMethodOnFreedObject(62); end;
procedure TFastMM_FreedObject.VirtualMethod63; begin VirtualMethodOnFreedObject(63); end;
procedure TFastMM_FreedObject.VirtualMethod64; begin VirtualMethodOnFreedObject(64); end;
procedure TFastMM_FreedObject.VirtualMethod65; begin VirtualMethodOnFreedObject(65); end;
procedure TFastMM_FreedObject.VirtualMethod66; begin VirtualMethodOnFreedObject(66); end;
procedure TFastMM_FreedObject.VirtualMethod67; begin VirtualMethodOnFreedObject(67); end;
procedure TFastMM_FreedObject.VirtualMethod68; begin VirtualMethodOnFreedObject(68); end;
procedure TFastMM_FreedObject.VirtualMethod69; begin VirtualMethodOnFreedObject(69); end;
procedure TFastMM_FreedObject.VirtualMethod70; begin VirtualMethodOnFreedObject(70); end;
procedure TFastMM_FreedObject.VirtualMethod71; begin VirtualMethodOnFreedObject(71); end;
procedure TFastMM_FreedObject.VirtualMethod72; begin VirtualMethodOnFreedObject(72); end;
procedure TFastMM_FreedObject.VirtualMethod73; begin VirtualMethodOnFreedObject(73); end;
procedure TFastMM_FreedObject.VirtualMethod74; begin VirtualMethodOnFreedObject(74); end;


{------------------------------------------}
{--------General utility subroutines-------}
{------------------------------------------}

{$if CompilerVersion < 34}
{Returns the lowest set bit index in the 32-bit number}
function CountTrailingZeros32(AInteger: Integer): Integer;
asm
{$ifdef 64Bit}
  .noframe
  mov rax, rcx
{$endif}
  bsf eax, eax
end;
{$endif}

{Returns True if the block is not in use.}
function BlockIsFree(APSmallMediumOrLargeBlock: Pointer): Boolean; inline;
begin
  Result := PBlockStatusFlags(APSmallMediumOrLargeBlock)[-1] and CBlockIsFreeFlag <> 0;
end;

{Tags a block as free, without affecting any other flags.}
procedure SetBlockIsFreeFlag(APSmallMediumOrLargeBlock: Pointer; ABlockIsFree: Boolean); inline;
begin
  if ABlockIsFree then
    PBlockStatusFlags(APSmallMediumOrLargeBlock)[-1] := PBlockStatusFlags(APSmallMediumOrLargeBlock)[-1] or CBlockIsFreeFlag
  else
    PBlockStatusFlags(APSmallMediumOrLargeBlock)[-1] := PBlockStatusFlags(APSmallMediumOrLargeBlock)[-1] and (not CBlockIsFreeFlag);
end;

{Returns True if the block contains a debug sub-block.}
function BlockHasDebugInfo(APSmallMediumOrLargeBlock: Pointer): Boolean; inline;
begin
  Result := PBlockStatusFlags(APSmallMediumOrLargeBlock)[-1] and CHasDebugInfoFlag <> 0;
end;

{Tags a block as having debug info, without affecting any other flags.}
procedure SetBlockHasDebugInfo(APSmallMediumOrLargeBlock: Pointer; ABlockHasDebugInfo: Boolean); inline;
begin
  if ABlockHasDebugInfo then
    PBlockStatusFlags(APSmallMediumOrLargeBlock)[-1] := PBlockStatusFlags(APSmallMediumOrLargeBlock)[-1] or CHasDebugInfoFlag
  else
    PBlockStatusFlags(APSmallMediumOrLargeBlock)[-1] := PBlockStatusFlags(APSmallMediumOrLargeBlock)[-1] and (not CHasDebugInfoFlag);
end;

function CalculateDebugBlockHeaderChecksum(APDebugBlockHeader: PFastMM_DebugBlockHeader): NativeUInt;
var
  LPCurPos: PNativeUInt;
begin
  Result := 0;
  LPCurPos := @APDebugBlockHeader.UserSize;
  while True do
  begin
    Result := Result xor LPCurPos^;
    Inc(LPCurPos);

    if LPCurPos = @APDebugBlockHeader.HeaderCheckSum then
      Break;
  end;

end;

procedure SetDebugBlockHeaderAndFooterChecksums(APDebugBlockHeader: PFastMM_DebugBlockHeader);
var
  LHeaderChecksum: NativeUInt;
  LPFooter: PNativeUInt;
begin
  LHeaderChecksum := CalculateDebugBlockHeaderChecksum(APDebugBlockHeader);
  APDebugBlockHeader.HeaderCheckSum := LHeaderChecksum;
  LPFooter := PNativeUInt(PByte(APDebugBlockHeader) + APDebugBlockHeader.UserSize + SizeOf(TFastMM_DebugBlockHeader));
  LPFooter^ := not LHeaderChecksum;
end;

procedure LogDebugBlockHeaderInvalid(APDebugBlockHeader: PFastMM_DebugBlockHeader);
var
  LTokenValues: TEventLogTokenValues;
  LTokenValueBuffer: array[0..CTokenBufferMaxWideChars] of WideChar;
  LPBufferPos, LPBufferEnd: PWideChar;
begin
  LTokenValues := Default(TEventLogTokenValues);

  LPBufferEnd := @LTokenValueBuffer[High(LTokenValueBuffer)];
  LPBufferPos := AddTokenValues_GeneralTokens(LTokenValues, @LTokenValueBuffer, LPBufferEnd);
  AddTokenValues_BlockTokens(LTokenValues, APDebugBlockHeader, LPBufferPos, LPBufferEnd);

  LogEvent(mmetDebugBlockHeaderCorruption, LTokenValues);
end;

{The debug header is assumed to be valid.}
procedure LogDebugBlockFooterInvalid(APDebugBlockHeader: PFastMM_DebugBlockHeader);
var
  LTokenValues: TEventLogTokenValues;
  LTokenValueBuffer: array[0..CTokenBufferMaxWideChars - 1] of WideChar;
  LPBufferPos, LPBufferEnd: PWideChar;
begin
  LTokenValues := Default(TEventLogTokenValues);

  LPBufferEnd := @LTokenValueBuffer[High(LTokenValueBuffer)];
  LPBufferPos := AddTokenValues_GeneralTokens(LTokenValues, @LTokenValueBuffer, LPBufferEnd);
  AddTokenValues_BlockTokens(LTokenValues, PByte(APDebugBlockHeader) + CDebugBlockHeaderSize, LPBufferPos, LPBufferEnd);

  LogEvent(mmetDebugBlockFooterCorruption, LTokenValues);
end;

{Checks the consistency of a block with embedded debug info.  Returns True if the block is intact, otherwise
(optionally) logs and/or displays the error and returns False.}
function CheckDebugBlockHeaderAndFooterCheckSumsValid(APDebugBlockHeader: PFastMM_DebugBlockHeader): Boolean;
var
  LHeaderChecksum: NativeUInt;
  LPFooter: PNativeUInt;
begin
  LHeaderChecksum := CalculateDebugBlockHeaderChecksum(APDebugBlockHeader);
  if APDebugBlockHeader.HeaderCheckSum <> LHeaderChecksum then
  begin
    LogDebugBlockHeaderInvalid(APDebugBlockHeader);
    Exit(False);
  end;
  LPFooter := PNativeUInt(PByte(APDebugBlockHeader) + APDebugBlockHeader.UserSize + SizeOf(TFastMM_DebugBlockHeader));
  if LPFooter^ <> (not LHeaderChecksum) then
  begin
    LogDebugBlockFooterInvalid(APDebugBlockHeader);
    Exit(False);
  end;

  Result := True;
end;

procedure FillDebugBlockWithDebugPattern(APDebugBlockHeader: PFastMM_DebugBlockHeader);
var
  LByteOffset: NativeInt;
  LPUserArea: PByte;
begin
  LByteOffset := APDebugBlockHeader.UserSize;
  LPUserArea := PByte(APDebugBlockHeader) + SizeOf(TFastMM_DebugBlockHeader);

  {Store a pointer to the freed object class if the block is large enough.}
  if LByteOffset >= CTObjectInstanceSize then
  begin
    PPointerArray(LPUserArea)[0] := TFastMM_FreedObject;
    {$ifdef 32Bit}
    PIntegerArray(LPUserArea)[1] := Integer(CDebugFillPattern4B);
    {$endif}
    Dec(LByteOffset, 8);
    Inc(LPUserArea, 8);
  end;

  if LByteOffset and 1 <> 0 then
  begin
    Dec(LByteOffset);
    LPUserArea[LByteOffset] := CDebugFillPattern1B;
  end;

  if LByteOffset and 2 <> 0 then
  begin
    Dec(LByteOffset, 2);
    PWord(@LPUserArea[LByteOffset])^ := CDebugFillPattern2B;
  end;

  if LByteOffset and 4 <> 0 then
  begin
    Dec(LByteOffset, 4);
    PCardinal(@LPUserArea[LByteOffset])^ := CDebugFillPattern4B;
  end;

  {Loop over the remaining 8 byte chunks using a negative offset.}
  Inc(LPUserArea, LByteOffset);
  LByteOffset := - LByteOffset;
  while LByteOffset < 0 do
  begin
    PUInt64(@LPUserArea[LByteOffset])^ := CDebugFillPattern8B;
    Inc(LByteOffset, 8);
  end;

end;

{The debug header and footer are assumed to be valid.}
procedure LogDebugBlockFillPatternCorrupted(APDebugBlockHeader: PFastMM_DebugBlockHeader);
const
  CMaxLoggedChanges = 32;
var
  LTokenValues: TEventLogTokenValues;
  LTokenValueBuffer: array[0..CTokenBufferMaxWideChars - 1] of WideChar;
  LPBufferPos, LPBufferEnd: PWideChar;
  LPUserArea: PByte;
  LOffset, LChangeStart: NativeInt;
  LLogCount: Integer;
begin

  LTokenValues := Default(TEventLogTokenValues);
  LPBufferPos := @LTokenValueBuffer;
  LPBufferEnd := @LTokenValueBuffer[High(LTokenValueBuffer)];

  {Add the modification detail tokens.}
  LPUserArea := PByte(APDebugBlockHeader) + SizeOf(TFastMM_DebugBlockHeader);
  LLogCount := 0;
  LOffset := 0;
  LTokenValues[CEventLogTokenModifyAfterFreeDetail] := LPBufferPos;
  while LOffset < APDebugBlockHeader.UserSize do
  begin
    if LPUserArea[LOffset] <> CDebugFillPattern1B then
    begin

      {Found the start of a changed block, now find the length}
      LChangeStart := LOffset;
      while True do
      begin
        Inc(LOffset);
        if (LOffset >= APDebugBlockHeader.UserSize)
          or (LPUserArea[LOffset] = CDebugFillPattern1B) then
        begin
          Break;
        end;
      end;

      if LLogCount > 0 then
      begin
        LPBufferPos^ := ',';
        Inc(LPBufferPos);
        LPBufferPos^ := ' ';
        Inc(LPBufferPos);
      end;

      LPBufferPos := NativeIntToTextBuffer(LChangeStart, LPBufferPos, LPBufferEnd);
      LPBufferPos^ := '(';
      Inc(LPBufferPos);
      LPBufferPos := NativeIntToTextBuffer(LOffset - LChangeStart, LPBufferPos, LPBufferEnd);
      LPBufferPos^ := ')';
      Inc(LPBufferPos);

      Inc(LLogCount);
      if LLogCount >= CMaxLoggedChanges then
        Break;

    end;
    Inc(LOffset);
  end;

  LPBufferPos^ := #0;
  Inc(LPBufferPos);

  LPBufferPos := AddTokenValues_GeneralTokens(LTokenValues, LPBufferPos, LPBufferEnd);
  AddTokenValues_BlockTokens(LTokenValues, PByte(APDebugBlockHeader) + CDebugBlockHeaderSize, LPBufferPos, LPBufferEnd);

  LogEvent(mmetDebugBlockModifiedAfterFree, LTokenValues);
end;

{Checks that the debug fill pattern in the debug block is intact.  Returns True if the block is intact, otherwise
(optionally) logs and/or displays the error and returns False.}
function CheckDebugBlockFillPatternIntact(APDebugBlockHeader: PFastMM_DebugBlockHeader): Boolean;
var
  LByteOffset: NativeInt;
  LPUserArea: PByte;
  LFillPatternIntact: Boolean;
begin
  LByteOffset := APDebugBlockHeader.UserSize;
  LPUserArea := PByte(APDebugBlockHeader) + SizeOf(TFastMM_DebugBlockHeader);
  LFillPatternIntact := True;

  {If the block is large enough the first 4/8 bytes should be a pointer to the freed object class.}
  if LByteOffset >= CTObjectInstanceSize then
  begin
    LFillPatternIntact := (PPointer(LPUserArea)^ = TFastMM_FreedObject)
    {$ifdef 32Bit}
      and (PIntegerArray(LPUserArea)[1] = Integer(CDebugFillPattern4B));
    {$endif};
    Dec(LByteOffset, 8);
    Inc(LPUserArea, 8);
  end;


  if LByteOffset and 1 <> 0 then
  begin
    Dec(LByteOffset);
    if LPUserArea[LByteOffset] <> CDebugFillPattern1B then
      LFillPatternIntact := False;
  end;

  if LByteOffset and 2 <> 0 then
  begin
    Dec(LByteOffset, 2);
    if PWord(@LPUserArea[LByteOffset])^ <> CDebugFillPattern2B then
      LFillPatternIntact := False;
  end;

  if LByteOffset and 4 <> 0 then
  begin
    Dec(LByteOffset, 4);
    if PCardinal(@LPUserArea[LByteOffset])^ <> CDebugFillPattern4B then
      LFillPatternIntact := False;
  end;

  {Loop over the remaining 8 byte chunks using a negative offset.}
  Inc(LPUserArea, LByteOffset);
  LByteOffset := - LByteOffset;
  while LByteOffset < 0 do
  begin
    if PUInt64(@LPUserArea[LByteOffset])^ <> CDebugFillPattern8B then
    begin
      LFillPatternIntact := False;
      Break;
    end;

    Inc(LByteOffset, 8);
  end;

  if not LFillPatternIntact then
  begin
    {Log the block error.}
    LogDebugBlockFillPatternCorrupted(APDebugBlockHeader);
    Result := False;
  end
  else
    Result := True;
end;

{Checks a free debug block for oorruption of the header, footer or fill pattern.  Returns True if it is intact.}
function CheckFreeDebugBlockIntact(APDebugBlockHeader: PFastMM_DebugBlockHeader): Boolean;
begin
  Result := CheckDebugBlockHeaderAndFooterCheckSumsValid(APDebugBlockHeader)
    and CheckDebugBlockFillPatternIntact(APDebugBlockHeader);
end;

procedure EnsureEmergencyReserveAddressSpaceAllocated;
begin
{$ifdef 32Bit}
  if EmergencyReserveAddressSpace = nil then
    EmergencyReserveAddressSpace := OS_AllocateVirtualMemory(CEmergencyReserveAddressSpace, False, True);
{$endif}
end;

procedure ReleaseEmergencyReserveAddressSpace;
begin
{$ifdef 32Bit}
  if EmergencyReserveAddressSpace <> nil then
  begin
    OS_FreeVirtualMemory(EmergencyReserveAddressSpace);
    EmergencyReserveAddressSpace := nil;
  end;
{$endif}
end;

{Logs a thread contention and yields execution to another thread that is ready to run.}
procedure LogSmallBlockThreadContentionAndYieldToOtherThread;
begin
  Inc(FastMM_SmallBlockThreadContentionCount);
  OS_AllowOtherThreadToRun;
end;

procedure LogMediumBlockThreadContentionAndYieldToOtherThread;
begin
  Inc(FastMM_MediumBlockThreadContentionCount);
  OS_AllowOtherThreadToRun;
end;

procedure LogLargeBlockThreadContentionAndYieldToOtherThread;
begin
  Inc(FastMM_LargeBlockThreadContentionCount);
  OS_AllowOtherThreadToRun;
end;

{-----------------------------------------}
{--------Debug block management-----------}
{-----------------------------------------}

function FastMM_FreeMem_FreeDebugBlock(APointer: Pointer): Integer;
var
  LPActualBlock: PFastMM_DebugBlockHeader;
begin
  LPActualBlock := @PFastMM_DebugBlockHeader(APointer)[-1];

  {Check that the debug header and footer are intact}
  if not CheckDebugBlockHeaderAndFooterCheckSumsValid(LPActualBlock) then
    System.Error(reInvalidPtr);

  {Update the information in the block header.}
  LPActualBlock.FreedByThread := OS_GetCurrentThreadID;
  FastMM_GetStackTrace(@LPActualBlock.FreeStackTrace, CFastMM_StackTraceEntryCount, CFastMM_StackTrace_SkipFrames_FreeMem);
  LPActualBlock.PreviouslyUsedByClass := PPointer(APointer)^;

  {Fill the user area of the block with the debug pattern.}
  FillDebugBlockWithDebugPattern(LPActualBlock);

  {The block is now free.}
  LPActualBlock.DebugBlockFlags := CIsDebugBlockFlag or CBlockIsFreeFlag;

  {Update the header and footer checksums}
  SetDebugBlockHeaderAndFooterChecksums(LPActualBlock);

  {Return the actual block to the memory pool.}
  Result := FastMM_FreeMem(LPActualBlock);
end;

{Reallocates a block containing debug information.  Any debug information remains intact.}
function FastMM_ReallocMem_ReallocDebugBlock(APointer: Pointer; ANewSize: NativeInt): Pointer;
var
  LPActualBlock: PFastMM_DebugBlockHeader;
  LAvailableSpace: NativeInt;
begin
  LPActualBlock := @PFastMM_DebugBlockHeader(APointer)[-1];

  {Check that the debug header and footer are intact}
  if not CheckDebugBlockHeaderAndFooterCheckSumsValid(LPActualBlock) then
    System.Error(reInvalidPtr);

  {Can the block be resized in-place?}
  LAvailableSpace := FastMM_BlockMaximumUserBytes(LPActualBlock);
  if LAvailableSpace >= ANewSize + (CDebugBlockHeaderSize + CDebugBlockFooterSize) then
  begin
    {Update the user block size and set the new header and footer checksums.}
    LPActualBlock.UserSize := ANewSize;
    SetDebugBlockHeaderAndFooterChecksums(LPActualBlock);

    Result := APointer;
  end
  else
  begin
    {The new size cannot fit in the existing block:  We need to allocate a new block.}
    Result := FastMM_GetMem(ANewSize + (CDebugBlockHeaderSize + CDebugBlockFooterSize));

    if Result <> nil then
    begin
      {Move the old data across and free the old block.}
      System.Move(LPActualBlock^, Result^, LPActualBlock.UserSize + CDebugBlockHeaderSize);
      FastMM_FreeMem_FreeDebugBlock(APointer);

      {Update the user block size and set the new header and footer checksums.}
      PFastMM_DebugBlockHeader(Result).UserSize := ANewSize;
      SetDebugBlockHeaderAndFooterChecksums(PFastMM_DebugBlockHeader(Result));

      {Set the flag in the actual block header to indicate that the block contains debug information.}
      SetBlockHasDebugInfo(Result, True);

      {Return a pointer to the user data}
      Inc(PByte(Result), CDebugBlockHeaderSize);

    end;

  end;
end;


{----------------------------------------------------}
{------------Invalid Free/realloc handling-----------}
{----------------------------------------------------}

{Always returns - 1.}
function HandleInvalidFreeMemOrReallocMem(APointer: Pointer; AIsReallocMemCall: Boolean): Integer;
var
  LPDebugBlockHeader: PFastMM_DebugBlockHeader;
  LHeaderChecksum: NativeUInt;
  LTokenValues: TEventLogTokenValues;
  LTokenValueBuffer: array[0..CTokenBufferMaxWideChars - 1] of WideChar;
  LPBufferPos, LPBufferEnd: PWideChar;
begin
  {Is this a debug block that has already been freed?  If not, it could be a bad pointer value, in which case there's
  not much that can be done to provide additional error information.}
  if PBlockStatusFlags(APointer)[-1] <> (CBlockIsFreeFlag or CIsDebugBlockFlag) then
    Exit(-1);

  {Check that the debug block header is intact.  If it is, then a meaningful error may be returned.}
  LPDebugBlockHeader := @PFastMM_DebugBlockHeader(APointer)[-1];
  LHeaderChecksum := CalculateDebugBlockHeaderChecksum(LPDebugBlockHeader);
  if LPDebugBlockHeader.HeaderCheckSum <> LHeaderChecksum then
    Exit(-1);

  LTokenValues := Default(TEventLogTokenValues);

  LPBufferEnd := @LTokenValueBuffer[High(LTokenValueBuffer)];
  LPBufferPos := AddTokenValues_GeneralTokens(LTokenValues, @LTokenValueBuffer, LPBufferEnd);
  AddTokenValues_BlockTokens(LTokenValues, APointer, LPBufferPos, LPBufferEnd);

  if AIsReallocMemCall then
    LogEvent(mmetDebugBlockReallocOfFreedBlock, LTokenValues)
  else
    LogEvent(mmetDebugBlockDoubleFree, LTokenValues);

  Result := -1;
end;


{-----------------------------------------}
{--------Large block management-----------}
{-----------------------------------------}

function FastMM_FreeMem_FreeLargeBlock_ReleaseVM(APLargeBlockHeader: PLargeBlockHeader): Integer;
var
  LRemainingSize: NativeUInt;
  LPCurrentSegment: Pointer;
  LMemoryRegionInfo: TMemoryRegionInfo;
begin
  if not APLargeBlockHeader.BlockIsSegmented then
  begin
    Result := OS_FreeVirtualMemory(APLargeBlockHeader);
  end
  else
  begin
    {The large block is segmented - free all segments}
    LPCurrentSegment := APLargeBlockHeader;
    LRemainingSize := NativeUInt(APLargeBlockHeader.ActualBlockSize);
{$if CompilerVersion < 31}
    Result := 0; //Workaround for spurious warning with older compilers
{$endif}
    while True do
    begin
      OS_GetVirtualMemoryRegionInfo(LPCurrentSegment, LMemoryRegionInfo);

      Result := OS_FreeVirtualMemory(LPCurrentSegment);
      if Result <> 0 then
        Break;

      {Done?}
      if LMemoryRegionInfo.RegionSize >= LRemainingSize then
        Break;

      {Decrement the remaining size}
      Dec(LRemainingSize, LMemoryRegionInfo.RegionSize);
      Inc(PByte(LPCurrentSegment), LMemoryRegionInfo.RegionSize);
    end;

  end;
end;

{Unlink this block from the circular list of large blocks.  The manager must be locked.}
procedure UnlinkLargeBlock(APLargeBlockHeader: PLargeBlockHeader);
var
  LPreviousLargeBlockHeader: PLargeBlockHeader;
  LNextLargeBlockHeader: PLargeBlockHeader;
begin
  LPreviousLargeBlockHeader := APLargeBlockHeader.PreviousLargeBlockHeader;
  LNextLargeBlockHeader := APLargeBlockHeader.NextLargeBlockHeader;
  LNextLargeBlockHeader.PreviousLargeBlockHeader := LPreviousLargeBlockHeader;
  LPreviousLargeBlockHeader.NextLargeBlockHeader := LNextLargeBlockHeader;
end;

{Processes all the pending frees in the large block arena, and unlocks the arena when done.  Returns 0 on success.}
function ProcessLargeBlockPendingFrees_ArenaAlreadyLocked(APLargeBlockManager: PLargeBlockManager): Integer;
var
  LOldPendingFreeList, LPCurrentLargeBlock, LPNextLargeBlock: Pointer;
  LPLargeBlockHeader: PLargeBlockHeader;
begin
  Result := 0;

  {Get the pending free list}
  LOldPendingFreeList := AtomicExchange(APLargeBlockManager.PendingFreeList, nil);

  {Unlink all the large blocks from the manager}
  LPCurrentLargeBlock := LOldPendingFreeList;
  while LPCurrentLargeBlock <> nil do
  begin
    LPNextLargeBlock := PPointer(LPCurrentLargeBlock)^;

    LPLargeBlockHeader := @PLargeBlockHeader(LPCurrentLargeBlock)[-1];
    UnlinkLargeBlock(LPLargeBlockHeader);

    LPCurrentLargeBlock := LPNextLargeBlock;
  end;

  {The large block manager no longer needs to be locked}
  APLargeBlockManager.LargeBlockManagerLocked := 0;

  {Free all the memory for the large blocks}
  LPCurrentLargeBlock := LOldPendingFreeList;
  while LPCurrentLargeBlock <> nil do
  begin
    LPNextLargeBlock := PPointer(LPCurrentLargeBlock)^;

    LPLargeBlockHeader := @PLargeBlockHeader(LPCurrentLargeBlock)[-1];
    if FastMM_FreeMem_FreeLargeBlock_ReleaseVM(LPLargeBlockHeader) <> 0 then
      Result := -1;

    LPCurrentLargeBlock := LPNextLargeBlock;
  end;

end;

{Process the pending frees list for all unlocked arenas, returning 0 on success or -1 if any error occurs}
function ProcessLargeBlockPendingFrees: Integer;
var
  LPLargeBlockManager: PLargeBlockManager;
  LArenaIndex: Integer;
begin
  Result := 0;

  LPLargeBlockManager := @LargeBlockManagers[0];
  for LArenaIndex := 0 to CFastMM_LargeBlockArenaCount - 1 do
  begin

    if (LPLargeBlockManager.PendingFreeList <> nil)
      and (LPLargeBlockManager.LargeBlockManagerLocked = 0)
      and (AtomicExchange(LPLargeBlockManager.LargeBlockManagerLocked, 1) = 0) then
    begin

      Result := ProcessLargeBlockPendingFrees_ArenaAlreadyLocked(LPLargeBlockManager);

      if Result <> 0 then
        Break;

    end;

    {Do the next arena.}
    Inc(LPLargeBlockManager);
  end;

end;

{Allocates a Large block of at least ASize (actual size may be larger to allow for alignment etc.).  ASize must be the
actual user requested size.  This procedure will pad it to the appropriate page boundary and also add the space
required by the header.}
function FastMM_GetMem_GetLargeBlock(ASize: NativeInt): Pointer;
var
  LLargeBlockActualSize: NativeInt;
  LPLargeBlockManager: PLargeBlockManager;
  LArenaIndex: Integer;
  LOldFirstLargeBlock: PLargeBlockHeader;
begin
  {Process the pending free lists of all arenas.}
  if ProcessLargeBlockPendingFrees <> 0 then
    Exit(nil);

  {Pad the block size to include the header and granularity, checking for overflow.}
  LLargeBlockActualSize := (ASize + CLargeBlockHeaderSize + CLargeBlockGranularity - 1) and -CLargeBlockGranularity;
  if LLargeBlockActualSize <= CMaximumMediumBlockSize then
    Exit(nil);
  {Get the large block.  For segmented large blocks to work in practice without excessive move operations we need to
  allocate top down.}
  Result := OS_AllocateVirtualMemory(LLargeBlockActualSize, True, False);

  {Set the Large block fields}
  if Result <> nil then
  begin
    {Set the large block size and flags}
    PLargeBlockHeader(Result).UserAllocatedSize := ASize;
    PLargeBlockHeader(Result).ActualBlockSize := LLargeBlockActualSize;
    PLargeBlockHeader(Result).BlockIsSegmented := False;
    PLargeBlockHeader(Result).BlockStatusFlags := CIsLargeBlockFlag;

    {Insert the block in the first available arena.}
    while True do
    begin

      LPLargeBlockManager := @LargeBlockManagers[0];
      for LArenaIndex := 0 to CFastMM_LargeBlockArenaCount - 1 do
      begin

        if (LPLargeBlockManager.LargeBlockManagerLocked = 0)
          and (AtomicExchange(LPLargeBlockManager.LargeBlockManagerLocked, 1) = 0) then
        begin
          PLargeBlockHeader(Result).LargeBlockManager := LPLargeBlockManager;

          {Insert the large block into the linked list of large blocks}
          LOldFirstLargeBlock := LPLargeBlockManager.FirstLargeBlockHeader;
          PLargeBlockHeader(Result).PreviousLargeBlockHeader := Pointer(LPLargeBlockManager);
          LPLargeBlockManager.FirstLargeBlockHeader := Result;
          PLargeBlockHeader(Result).NextLargeBlockHeader := LOldFirstLargeBlock;
          LOldFirstLargeBlock.PreviousLargeBlockHeader := Result;

          LPLargeBlockManager.LargeBlockManagerLocked := 0;

          {Add the size of the header}
          Inc(PByte(Result), CLargeBlockHeaderSize);

          Exit;
        end;

        {Try the next arena.}
        Inc(LPLargeBlockManager);
      end;

    end;

    {All large block managers are locked:  Back off and try again.}
    LogLargeBlockThreadContentionAndYieldToOtherThread;

  end;
end;

function FastMM_FreeMem_FreeLargeBlock(APLargeBlock: Pointer): Integer;
var
  LPLargeBlockHeader: PLargeBlockHeader;
  LPLargeBlockManager: PLargeBlockManager;
  LOldPendingFreeList: Pointer;
begin
  LPLargeBlockHeader := @PLargeBlockHeader(APLargeBlock)[-1];
  LPLargeBlockManager := LPLargeBlockHeader.LargeBlockManager;

  {Try to lock the large block manager so that the block may be freed.}
  if AtomicCmpExchange(LPLargeBlockManager.LargeBlockManagerLocked, 1, 0) = 0 then
  begin
    {Unlink the large block from the circular queue for the manager.}
    UnlinkLargeBlock(LPLargeBlockHeader);

    {The large block manager no longer has to be locked, since the large block has been unlinked.}
    LPLargeBlockManager.LargeBlockManagerLocked := 0;

    {Release the memory used by the large block.}
    Result := FastMM_FreeMem_FreeLargeBlock_ReleaseVM(LPLargeBlockHeader);

  end
  else
  begin
    {The large block manager is currently locked, so we need to add this block to its pending free list.}
    while True do
    begin
      LOldPendingFreeList := LPLargeBlockManager.PendingFreeList;
      PPointer(APLargeBlock)^ := LOldPendingFreeList;
      if AtomicCmpExchange(LPLargeBlockManager.PendingFreeList, APLargeBlock, LOldPendingFreeList) = LOldPendingFreeList then
        Break;
    end;

    Result := 0;
  end;

  if Result = 0 then
    Result := ProcessLargeBlockPendingFrees;
end;

function FastMM_ReallocMem_ReallocLargeBlock(APointer: Pointer; ANewSize: NativeInt): Pointer;
var
  LPLargeBlockHeader: PLargeBlockHeader;
  LOldAvailableSize, LNewAllocSize, LNewSegmentSize, LOldUserSize: NativeInt;
  LMemoryRegionInfo: TMemoryRegionInfo;
  LPNextSegment: Pointer;
begin
  {Get the block header}
  LPLargeBlockHeader := @PLargeBlockHeader(APointer)[-1];
  {Large block - size is (16 + 4) less than the allocated size}
  LOldAvailableSize := LPLargeBlockHeader.ActualBlockSize - CLargeBlockHeaderSize;
  {Is it an upsize or a downsize?}
  if ANewSize > LOldAvailableSize then
  begin
    {This pointer is being reallocated to a larger block and therefore it is logical to assume that it may be enlarged
    again.  Since reallocations are expensive, there is a minimum upsize percentage to avoid unnecessary future move
    operations.  This is currently set to 25%.}
    LNewAllocSize := LOldAvailableSize + (LOldAvailableSize shr 2);
    if LNewAllocSize < ANewSize then
      LNewAllocSize := ANewSize;

    {Can another large block segment be allocated directly after this segment, thus negating the need to move the data?}
    LPNextSegment := Pointer(PByte(LPLargeBlockHeader) + LPLargeBlockHeader.ActualBlockSize);
    OS_GetVirtualMemoryRegionInfo(LPNextSegment, LMemoryRegionInfo);
    if LMemoryRegionInfo.RegionIsFree then
    begin
      {Round the region size to the previous 64K}
      LMemoryRegionInfo.RegionSize := LMemoryRegionInfo.RegionSize and -CLargeBlockGranularity;
      {Enough space to grow in place?}
      if LMemoryRegionInfo.RegionSize >= NativeUInt(ANewSize - LOldAvailableSize) then
      begin
        {There is enough space after the block to extend it - determine by how much}
        LNewSegmentSize := (LNewAllocSize - LOldAvailableSize + CLargeBlockGranularity - 1) and -CLargeBlockGranularity;
        if NativeUInt(LNewSegmentSize) > LMemoryRegionInfo.RegionSize then
          LNewSegmentSize := LMemoryRegionInfo.RegionSize;
        {Attempt to reserve the address range (which will fail if another thread has just reserved it) and commit it
        immediately afterwards.}
        if OS_AllocateVirtualMemoryAtAddress(LPNextSegment, LNewSegmentSize, False) then
        begin
          {Update the requested size}
          LPLargeBlockHeader.UserAllocatedSize := ANewSize;
          Inc(LPLargeBlockHeader.ActualBlockSize, LNewSegmentSize);
          LPLargeBlockHeader.BlockIsSegmented := True;
          Exit(APointer);
        end;
      end;
    end;

    {Could not resize in place:  Allocate the new block}
    Result := FastMM_GetMem(LNewAllocSize);
    if Result <> nil then
    begin
      {If it's a large block - store the actual user requested size (it may not be if the block that is being
      reallocated from was previously downsized)}
      if LNewAllocSize > (CMaximumMediumBlockSize - CMediumBlockHeaderSize) then
        PLargeBlockHeader(Result)[-1].UserAllocatedSize := ANewSize;
      {The user allocated size is stored for large blocks}
      LOldUserSize := LPLargeBlockHeader.UserAllocatedSize;
      {The number of bytes to move is the old user size.}
      MoveMultipleOf64_Large(APointer^, Result^, LOldUserSize);
      {Free the old block.}
      FastMM_FreeMem(APointer);
    end;
  end
  else
  begin
    {It's a downsize:  Do we need to reallocate?  Only if the new size is less than half the old size.}
    if ANewSize >= (LOldAvailableSize shr 1) then
    begin
      {No need to reallocate}
      Result := APointer;
      {Update the requested size}
      LPLargeBlockHeader.UserAllocatedSize := ANewSize;
    end
    else
    begin
      {The new size is less than half the old size:  Reallocate}
      Result := FastMM_GetMem(ANewSize);
      if Result <> nil then
      begin
        {Move the data across}
        System.Move(APointer^, Result^, ANewSize);
        {Free the old block.}
        FastMM_FreeMem(APointer);
      end;
    end;
  end;

end;


{------------------------------------------}
{--------Medium block management-----------}
{------------------------------------------}

{Takes a user request size and converts it to a size that fits the size of a medium block bin exactly.}
function RoundUserSizeUpToNextMediumBlockBin(AUserSize: Integer): Integer; inline;
begin
  if AUserSize <= (CMediumBlockMiddleBinsStart - CMediumBlockHeaderSize) then
  begin
    Result := (AUserSize + (CMediumBlockHeaderSize - CMinimumMediumBlockSize + CInitialBinSpacing - 1)) and -CInitialBinSpacing
      + CMinimumMediumBlockSize;
  end
  else
  begin
    if AUserSize <= (CMediumBlockFinalBinsStart - CMediumBlockHeaderSize) then
    begin
      Result := (AUserSize + (CMediumBlockHeaderSize - CMediumBlockMiddleBinsStart + CMiddleBinSpacing - 1)) and -CMiddleBinSpacing
        + CMediumBlockMiddleBinsStart;
    end
    else
    begin
      Result := (AUserSize + (CMediumBlockHeaderSize - CMediumBlockFinalBinsStart + CFinalBinSpacing - 1)) and -CFinalBinSpacing
        + CMediumBlockFinalBinsStart;
    end;
  end;
end;

{Determines the appropriate bin number for blocks of AMediumBlockSize.  If AMediumBlockSize is not exactly aligned to a
bin size then the bin just smaller than AMediumBlockSize will be returned.  It is assumed that AMediumBlockSize <=
CMaximumMediumBlockSize.}
function GetBinNumberForMediumBlockSize(AMediumBlockSize: Integer): Integer; inline;
begin
  if AMediumBlockSize <= CMediumBlockMiddleBinsStart then
  begin
    Result := (AMediumBlockSize - CMinimumMediumBlockSize) shr CInitialBinSpacingBits;
  end
  else
  begin
    if AMediumBlockSize <= CMediumBlockFinalBinsStart then
      Result := (AMediumBlockSize + (CInitialBinCount * CMiddleBinSpacing - CMediumBlockMiddleBinsStart)) shr CMiddleBinSpacingBits
    else
      Result := (AMediumBlockSize + ((CInitialBinCount + CMiddleBinCount) * CFinalBinSpacing - CMediumBlockFinalBinsStart)) shr CFinalBinSpacingBits;
  end;
end;

function GetMediumBlockSpan(APMediumBlock: Pointer): PMediumBlockSpanHeader; inline;
begin
  Result := PMediumBlockSpanHeader(NativeUInt(APMediumBlock)
    - (PMediumBlockHeader(APMediumBlock)[-1].MediumBlockSpanOffsetMultiple shl CMediumBlockAlignmentBits));
end;

function GetMediumBlockSize(APMediumBlock: Pointer): Integer; inline;
begin
  Result := PMediumBlockHeader(APMediumBlock)[-1].MediumBlockSizeMultiple shl CMediumBlockAlignmentBits;
end;

procedure SetMediumBlockHeader_SetIsSmallBlockSpan(APMediumBlock: Pointer; AIsSmallBlockSpan: Boolean); inline;
begin
  PMediumBlockHeader(APMediumBlock)[-1].IsSmallBlockSpan := AIsSmallBlockSpan;
end;

procedure SetMediumBlockHeader_SetMediumBlockSpan(APMediumBlock: Pointer; APMediumBlockSpan: PMediumBlockSpanHeader); inline;
begin
  {Store the offset to the medium block span.}
  PMediumBlockHeader(APMediumBlock)[-1].MediumBlockSpanOffsetMultiple :=
    (NativeUInt(APMediumBlock) - NativeUInt(APMediumBlockSpan)) shr CMediumBlockAlignmentBits;
end;

procedure SetMediumBlockHeader_SetSizeAndFlags(APMediumBlock: Pointer; ABlockSize: Integer; ABlockIsFree: Boolean;
  ABlockHasDebugInfo: Boolean); inline;
var
  LPNextBlock: Pointer;
begin
  if ABlockIsFree then
  begin

    if ABlockHasDebugInfo then
      PMediumBlockHeader(APMediumBlock)[-1].BlockStatusFlags := CHasDebugInfoFlag + CBlockIsFreeFlag + CIsMediumBlockFlag
    else
      PMediumBlockHeader(APMediumBlock)[-1].BlockStatusFlags := CBlockIsFreeFlag + CIsMediumBlockFlag;

    LPNextBlock := @PByte(APMediumBlock)[ABlockSize];
    {If the block is free then the size must also be stored just before the header of the next block.}
    PMediumFreeBlockFooter(LPNextBlock)[-1].MediumFreeBlockSize := ABlockSize;

    {Update the flag in the next block header to indicate that this block is free.}
    PMediumBlockHeader(LPNextBlock)[-1].PreviousBlockIsFree := True;

  end
  else
  begin

    if ABlockHasDebugInfo then
      PMediumBlockHeader(APMediumBlock)[-1].BlockStatusFlags := CHasDebugInfoFlag + CIsMediumBlockFlag
    else
      PMediumBlockHeader(APMediumBlock)[-1].BlockStatusFlags := CIsMediumBlockFlag;

    LPNextBlock := @PByte(APMediumBlock)[ABlockSize];
    {Update the flag in the next block to indicate that this block is in use.  The block size is not stored before
    the header of the next block if it is not free.}
    PMediumBlockHeader(LPNextBlock)[-1].PreviousBlockIsFree := False;

  end;

  {Store the block size.}
  PMediumBlockHeader(APMediumBlock)[-1].MediumBlockSizeMultiple := ABlockSize shr CMediumBlockAlignmentBits;
end;

{Inserts a medium block into the appropriate medium block bin.  The header for APMediumFreeBlock must already be set
correctly.}
procedure InsertMediumBlockIntoBin(APMediumBlockManager: PMediumBlockManager; APMediumFreeBlock: PMediumFreeBlockContent;
  AMediumBlockSize: Integer);
var
  LBinNumber, LBinGroupNumber: Cardinal;
  LPBin, LPInsertAfterBlock, LPInsertBeforeBlock: PMediumFreeBlockContent;
begin
  {Get the bin for blocks of this size.  If the block is not aligned to a bin size, then put it in the closest bin
  smaller than the block size.}
  if AMediumBlockSize < CMaximumMediumBlockSize then
    LBinNumber := GetBinNumberForMediumBlockSize(AMediumBlockSize)
  else
    LBinNumber := CMediumBlockBinCount - 1;
  LPBin := @APMediumBlockManager.FirstFreeBlockInBin[LBinNumber];

  {Bins are LIFO, so we insert this block as the first free block in the bin}
  LPInsertAfterBlock := LPBin;
  LPInsertBeforeBlock := LPBin.NextFreeMediumBlock;

  APMediumFreeBlock.NextFreeMediumBlock := LPInsertBeforeBlock;
  APMediumFreeBlock.PreviousFreeMediumBlock := LPInsertAfterBlock;
  LPInsertAfterBlock.NextFreeMediumBlock := APMediumFreeBlock;

  {Was this bin previously empty?}
  if LPInsertBeforeBlock <> LPInsertAfterBlock then
  begin
    {It's not a fully circular linked list:  Bins have no "previous" pointer.}
    if LPInsertBeforeBlock <> LPBin then
      LPInsertBeforeBlock.PreviousFreeMediumBlock := APMediumFreeBlock;
  end
  else
  begin
    {Get the group number}
    LBinGroupNumber := LBinNumber shr 5; //32 bins per group
    {Flag this bin as used}
    APMediumBlockManager.MediumBlockBinBitmaps[LBinGroupNumber] := APMediumBlockManager.MediumBlockBinBitmaps[LBinGroupNumber]
      or (1 shl (LBinNumber and 31));
    {Flag the group as used}
    APMediumBlockManager.MediumBlockBinGroupBitmap := APMediumBlockManager.MediumBlockBinGroupBitmap
      or (1 shl LBinGroupNumber);
  end;
end;

{Removes a medium block from the circular linked list of free blocks.  Does not change any header flags.  The medium
block manager should be locked before calling this procedure.}
procedure RemoveMediumFreeBlockFromBin(APMediumBlockManager: PMediumBlockManager; APMediumFreeBlock: PMediumFreeBlockContent);
var
  LPPreviousFreeBlock, LPNextFreeBlock: PMediumFreeBlockContent;
  LBinNumber, LBinGroupNumber: Cardinal;
begin
  {Get the current previous and next blocks}
  LPNextFreeBlock := APMediumFreeBlock.NextFreeMediumBlock;
  LPPreviousFreeBlock := APMediumFreeBlock.PreviousFreeMediumBlock;
  {Remove this block from the linked list}
  LPPreviousFreeBlock.NextFreeMediumBlock := LPNextFreeBlock;
  {Is this bin now empty?  If the previous and next free block pointers are equal, they must point to the bin.}
  if LPNextFreeBlock <> LPPreviousFreeBlock then
  begin
    {It's not a fully circular linked list:  Bins have no "previous" pointer.  Therefore we need to check whether
    LPNextFreeBlock points to the bin or not before setting the previous block pointer.}
    if (NativeUInt(LPNextFreeBlock) > NativeUInt(@MediumBlockManagers) + SizeOf(MediumBlockManagers))
      or (NativeUInt(LPNextFreeBlock) < NativeUInt(@MediumBlockManagers)) then
    begin
      LPNextFreeBlock.PreviousFreeMediumBlock := LPPreviousFreeBlock;
    end;
  end
  else
  begin
    {Calculate the bin number from the bin pointer:  LPNextFreeBlock will be a pointer to the bin, since the bin is now
    empty.)}
    LBinNumber := (NativeInt(LPNextFreeBlock) - NativeInt(@APMediumBlockManager.FirstFreeBlockInBin)) shr CPointerSizeBitShift;
    LBinGroupNumber := LBinNumber shr 5; //32 bins per group
    {Flag this bin as empty}
    APMediumBlockManager.MediumBlockBinBitmaps[LBinGroupNumber] := APMediumBlockManager.MediumBlockBinBitmaps[LBinGroupNumber]
      and (not (1 shl (LBinNumber and 31)));
    {Is the group now entirely empty?}
    if APMediumBlockManager.MediumBlockBinBitmaps[LBinGroupNumber] = 0 then
    begin
      {Flag this group as empty}
      APMediumBlockManager.MediumBlockBinGroupBitmap := APMediumBlockManager.MediumBlockBinGroupBitmap
        and (not (1 shl LBinGroupNumber));
    end;
  end;
end;

{Bins what remains in the current sequential feed medium block span.  The medium block manager must be locked.}
procedure BinMediumSequentialFeedRemainder(APMediumBlockManager: PMediumBlockManager);
var
  LPreviousLastSequentialFeedBlockOffset, LNextBlockSize: Integer;
  LSequentialFeedFreeSize: Integer;
  LPRemainderBlock, LPNextMediumBlock: Pointer;
begin
  while True do
  begin

    LPreviousLastSequentialFeedBlockOffset := APMediumBlockManager.LastMediumBlockSequentialFeedOffset.IntegerValue;

    {Is there anything to bin?}
    if LPreviousLastSequentialFeedBlockOffset <= CMediumBlockSpanHeaderSize then
      Break;

    {There's no need to update the ABA counter, since the medium block manager is locked and no other thread can thus
    change the sequential feed span.}
    if AtomicCmpExchange(APMediumBlockManager.LastMediumBlockSequentialFeedOffset.IntegerValue, 0,
      LPreviousLastSequentialFeedBlockOffset) = LPreviousLastSequentialFeedBlockOffset then
    begin
      LSequentialFeedFreeSize := LPreviousLastSequentialFeedBlockOffset - CMediumBlockSpanHeaderSize;

      {Get the block for the remaining space}
      LPNextMediumBlock := PByte(APMediumBlockManager.SequentialFeedMediumBlockSpan) + LPreviousLastSequentialFeedBlockOffset;

      {Point to the remainder}
      LPRemainderBlock := Pointer(PByte(APMediumBlockManager.SequentialFeedMediumBlockSpan) + CMediumBlockSpanHeaderSize);

      {Can the next block be combined with the remainder?}
      if BlockIsFree(LPNextMediumBlock) then
      begin
        LNextBlockSize := GetMediumBlockSize(LPNextMediumBlock);
        {Increase the size of this block}
        Inc(LSequentialFeedFreeSize, LNextBlockSize);
        {Remove the next block from the bins, if it is currently binned.}
        if LNextBlockSize >= CMinimumMediumBlockSize then
          RemoveMediumFreeBlockFromBin(APMediumBlockManager, LPNextMediumBlock);
      end;

      {Store the size of the block as well as the flags.  Also updates the header of the next block to indicate that
      this block is free.}
      SetMediumBlockHeader_SetSizeAndFlags(LPRemainderBlock, LSequentialFeedFreeSize, True, False);
      SetMediumBlockHeader_SetMediumBlockSpan(LPRemainderBlock, APMediumBlockManager.SequentialFeedMediumBlockSpan);

      {Bin this medium block}
      if LSequentialFeedFreeSize >= CMinimumMediumBlockSize then
        InsertMediumBlockIntoBin(APMediumBlockManager, LPRemainderBlock, LSequentialFeedFreeSize);

      Break;
    end;

  end;

end;

{Subroutine for FastMM_FreeMem_FreeMediumBlock.  The medium block manager must already be locked.  Optionally unlocks the
medium block manager before exit.  Returns 0 on success, -1 on failure.}
function FastMM_FreeMem_InternalFreeMediumBlock_ManagerAlreadyLocked(APMediumBlockManager: PMediumBlockManager;
  APMediumBlockSpan: PMediumBlockSpanHeader; APMediumBlock: Pointer; AUnlockMediumBlockManager: Boolean): Integer;
var
  LPPreviousMediumBlockSpan, LPNextMediumBlockSpan: PMediumBlockSpanHeader;
  LBlockSize, LNextBlockSize, LPreviousBlockSize: Integer;
  LPNextMediumBlock: Pointer;
begin
  LBlockSize := GetMediumBlockSize(APMediumBlock);

  if DebugModeCounter <= 0 then
  begin
    {Combine with the next block, if it is free.}
    LPNextMediumBlock := Pointer(PByte(APMediumBlock) + LBlockSize);
    if BlockIsFree(LPNextMediumBlock) then
    begin
      LNextBlockSize := GetMediumBlockSize(LPNextMediumBlock);
      Inc(LBlockSize, LNextBlockSize);
      if LNextBlockSize >= CMinimumMediumBlockSize then
        RemoveMediumFreeBlockFromBin(APMediumBlockManager, LPNextMediumBlock);
    end;

    {Combine with the previous block, if it is free.}
    if PMediumBlockHeader(APMediumBlock)[-1].PreviousBlockIsFree then
    begin
      LPreviousBlockSize := PMediumFreeBlockFooter(APMediumBlock)[-1].MediumFreeBlockSize;
      {This is the new current block}
      APMediumBlock := Pointer(PByte(APMediumBlock) - LPreviousBlockSize);

      Inc(LBlockSize, LPreviousBlockSize);
      if LPreviousBlockSize >= CMinimumMediumBlockSize then
        RemoveMediumFreeBlockFromBin(APMediumBlockManager, APMediumBlock);
    end;

    {Outside of debug mode medium blocks are combined, so debug info will be lost.}
    SetMediumBlockHeader_SetSizeAndFlags(APMediumBlock, LBlockSize, True, False);

  end
  else
  begin
    {Medium blocks are not coalesced in debug mode, so just flag the block as free and leave the debug info flag as-is.}
    SetBlockIsFreeFlag(APMediumBlock, True);
  end;

  {Is the entire medium block span free?  Normally the span will be freed, but if there is not a lot of space left in
  the sequential feed span and the largest free block bin is empty then the block is binned instead (if allowed by the
  optimization strategy).}
  if (LBlockSize <> (APMediumBlockSpan.SpanSize - CMediumBlockSpanHeaderSize))
    or ((OptimizationStrategy <> mmosOptimizeForLowMemoryUsage)
      and (APMediumBlockManager.LastMediumBlockSequentialFeedOffset.IntegerValue < CMaximumMediumBlockSize)
      and (APMediumBlockManager.MediumBlockBinBitmaps[CMediumBlockBinGroupCount - 1] and (1 shl 31) = 0)) then
  begin
    if LBlockSize >= CMinimumMediumBlockSize then
      InsertMediumBlockIntoBin(APMediumBlockManager, APMediumBlock, LBlockSize);

    if AUnlockMediumBlockManager then
      APMediumBlockManager.MediumBlockManagerLocked := 0;

    Result := 0;
  end
  else
  begin
    {Remove this medium block span from the linked list}
    LPPreviousMediumBlockSpan := APMediumBlockSpan.PreviousMediumBlockSpanHeader;
    LPNextMediumBlockSpan := APMediumBlockSpan.NextMediumBlockSpanHeader;
    LPPreviousMediumBlockSpan.NextMediumBlockSpanHeader := LPNextMediumBlockSpan;
    LPNextMediumBlockSpan.PreviousMediumBlockSpanHeader := LPPreviousMediumBlockSpan;

    if AUnlockMediumBlockManager then
      APMediumBlockManager.MediumBlockManagerLocked := 0;

    {Free the entire span.}
    Result := OS_FreeVirtualMemory(APMediumBlockSpan);
  end;
end;

{Frees a chain of blocks belonging to the medium block manager.  The block manager is assumed to be locked.  Optionally
unlocks the block manager when done.  The first pointer inside each free block should be a pointer to the next free
block.}
function FastMM_FreeMem_FreeMediumBlockChain(APMediumBlockManager: PMediumBlockManager; APPendingFreeMediumBlock: Pointer;
  AUnlockMediumBlockManagerWhenDone: Boolean): Integer;
var
  LPNextBlock: Pointer;
  LPMediumBlockSpan: PMediumBlockSpanHeader;
begin
  Result := 0;

  while True do
  begin
    LPNextBlock := PPointer(APPendingFreeMediumBlock)^;

    LPMediumBlockSpan := GetMediumBlockSpan(APPendingFreeMediumBlock);
    Result := Result or FastMM_FreeMem_InternalFreeMediumBlock_ManagerAlreadyLocked(APMediumBlockManager, LPMediumBlockSpan,
      APPendingFreeMediumBlock, AUnlockMediumBlockManagerWhenDone and (LPNextBlock = nil));

    if LPNextBlock = nil then
      Break;

    APPendingFreeMediumBlock := LPNextBlock;
  end;
end;

function FastMM_FreeMem_FreeMediumBlock(APMediumBlock: Pointer): Integer;
var
  LPMediumBlockSpan: PMediumBlockSpanHeader;
  LPMediumBlockManager: PMediumBlockManager;
  LFirstPendingFreeBlock: Pointer;
begin
  LPMediumBlockSpan := GetMediumBlockSpan(APMediumBlock);
  LPMediumBlockManager := LPMediumBlockSpan.MediumBlockManager;

  {Try to lock the medium block manager so that the block may be freed.}
  if AtomicCmpExchange(LPMediumBlockManager.MediumBlockManagerLocked, 1, 0) = 0 then
  begin

    {Memory fence required for ARM here}

    if LPMediumBlockManager.PendingFreeList = nil then
    begin
      Result := FastMM_FreeMem_InternalFreeMediumBlock_ManagerAlreadyLocked(LPMediumBlockManager, LPMediumBlockSpan,
        APMediumBlock, True);
    end
    else
    begin
      Result := FastMM_FreeMem_InternalFreeMediumBlock_ManagerAlreadyLocked(LPMediumBlockManager, LPMediumBlockSpan,
        APMediumBlock, False);

      {Process the pending frees list.}
      LFirstPendingFreeBlock := AtomicExchange(LPMediumBlockManager.PendingFreeList, nil);
      Result := Result or FastMM_FreeMem_FreeMediumBlockChain(LPMediumBlockManager, LFirstPendingFreeBlock, True);
    end;

  end
  else
  begin
    {The medium block manager is currently locked, so we need to add this block to its pending free list.}
    while True do
    begin
      LFirstPendingFreeBlock := LPMediumBlockManager.PendingFreeList;
      PPointer(APMediumBlock)^ := LFirstPendingFreeBlock;
      if AtomicCmpExchange(LPMediumBlockManager.PendingFreeList, APMediumBlock, LFirstPendingFreeBlock) = LFirstPendingFreeBlock then
        Break;
    end;

    Result := 0;
  end;

end;

{Allocates a new sequential feed medium block span and immediately splits off a block of the requested size.  The block
size must be a multiple of 64 and medium blocks must be locked.  Returns a pointer to the first block.  The block
manager must be locked.}
function FastMM_GetMem_GetMediumBlock_AllocateNewSequentialFeedSpan(APMediumBlockManager: PMediumBlockManager;
  AFirstBlockSize: Integer): Pointer;
var
  LNewSpanSize: Integer;
  LOldFirstMediumBlockSpan, LPNewSpan: PMediumBlockSpanHeader;
begin
  {Bin the current sequential feed remainder}
  BinMediumSequentialFeedRemainder(APMediumBlockManager);
  {Allocate a new sequential feed block pool.  The block is assumed to be zero initialized.}
  LNewSpanSize := DefaultMediumBlockSpanSize;
  LPNewSpan := OS_AllocateVirtualMemory(LNewSpanSize, False, False);
  if LPNewSpan <> nil then
  begin
    LPNewSpan.SpanSize := LNewSpanSize;
    LPNewSpan.MediumBlockManager := APMediumBlockManager;

    {Insert this span into the circular linked list of medium block spans}
    LOldFirstMediumBlockSpan := APMediumBlockManager.FirstMediumBlockSpanHeader;
    LPNewSpan.PreviousMediumBlockSpanHeader := PMediumBlockSpanHeader(APMediumBlockManager);
    APMediumBlockManager.FirstMediumBlockSpanHeader := LPNewSpan;
    LPNewSpan.NextMediumBlockSpanHeader := LOldFirstMediumBlockSpan;
    LOldFirstMediumBlockSpan.PreviousMediumBlockSpanHeader := LPNewSpan;

    {Store the sequential feed span trailer.  Technically, this should not be necessary since the span is
    zero-initialized and the only flag that really matters is the "is free block" flag.}
    PMediumBlockHeader(PByte(LPNewSpan) + LNewSpanSize)[-1].BlockStatusFlags := CIsMediumBlockFlag;

    {Get the result and set its header.}
    Result := Pointer(PByte(LPNewSpan) + LNewSpanSize - AFirstBlockSize);
    SetMediumBlockHeader_SetSizeAndFlags(Result, AFirstBlockSize, False, False);
    SetMediumBlockHeader_SetMediumBlockSpan(Result, LPNewSpan);

    {Install this is the new sequential feed span.  The new offset must be set after the new span and ABA counter,
    since other threads may immediately split off blocks the moment the new offset is set.}
    Inc(APMediumBlockManager.LastMediumBlockSequentialFeedOffset.ABACounter);
    APMediumBlockManager.SequentialFeedMediumBlockSpan := LPNewSpan;

    {May need a memory fence here for ARM.}

    APMediumBlockManager.LastMediumBlockSequentialFeedOffset.IntegerValue := NativeInt(Result) - NativeInt(LPNewSpan);
  end
  else
  begin
    {Out of memory}
    Result := nil;
  end;
end;

{Attempts to split off a medium block from the sequential feed span for the arena.  Returns the block on success, nil if
there is not enough sequential feed space available.  The arena does not have to be locked.}
function FastMM_GetMem_GetMediumBlock_TryGetBlockFromSequentialFeedSpan(APMediumBlockManager: PMediumBlockManager;
  AMinimumBlockSize, AOptimalBlockSize: Integer): Pointer;
{$ifdef X86ASM}
asm
  push ebx
  push esi
  push edi
  push ebp
  push ecx

  {esi = APMediumBlockManager, ebp = AMinimumBlockSize, [esp] = AOptimalBlockSize}
  mov esi, eax
  mov ebp, edx
@TrySequentialFeedLoop:

  {Get the old ABA counter and offset in edx:eax}
  mov eax, TMediumBlockManager(esi).LastMediumBlockSequentialFeedOffset.IntegerValue
  mov edx, TMediumBlockManager(esi).LastMediumBlockSequentialFeedOffset.ABACounter

  {Get the available size in ecx, and check that it is sufficient.}
  lea ecx, [eax - CMediumBlockSpanHeaderSize]
  cmp ecx, ebp
  jl @NoSequentialFeedAvailable

  {Cap the block size at the optimal size.}
  cmp ecx, [esp]
  jle @BlockNotTooBig
  mov ecx, [esp]
@BlockNotTooBig:

  {Get the new ABA counter and offset in ecx:ebx}
  mov ebx, eax
  sub ebx, ecx
  lea ecx, [edx + 1]

  {Get the current sequential feed span in edi}
  mov edi, TMediumBlockManager(esi).SequentialFeedMediumBlockSpan

  {Try to grab the block.  If it fails, try again from the start.}
  lock cmpxchg8b TMediumBlockManager(esi).LastMediumBlockSequentialFeedOffset
  jne @TrySequentialFeedLoop

  {Current state: eax = next block offset, ebx = this block offset, edi = sequential feed span}

  {Get the block size in ecx}
  mov ecx, eax
  sub ecx, ebx

  {The block address is the span + offset.}
  lea eax, [edi + ebx]

  {Configure the block header.  Medium block spans are always zero initialized, so it is not necessary to set the
  PreviousBlockIsFree or IsSmallBlockSpan fields since they will already be zero.  Similarly it is not necessary to set
  the "previous block is free" flag in the next block.}
  shr ecx, CMediumBlockAlignmentBits
  mov TMediumBlockHeader.MediumBlockSizeMultiple(eax - CMediumBlockHeaderSize), cx
  shr ebx, CMediumBlockAlignmentBits
  mov TMediumBlockHeader.MediumBlockSpanOffsetMultiple(eax - CMediumBlockHeaderSize), bx
  mov TMediumBlockHeader.BlockStatusFlags(eax - CMediumBlockHeaderSize), CIsMediumBlockFlag

  jmp @Done

@NoSequentialFeedAvailable:
  xor eax, eax
@Done:
  pop ecx
  pop ebp
  pop edi
  pop esi
  pop ebx
{$else}
var
  LPSequentialFeedSpan: PMediumBlockSpanHeader;
  LPreviousLastSequentialFeedBlockOffset, LNewLastSequentialFeedBlockOffset: TIntegerWithABACounter;
  LBlockSize: Integer;
begin
  {The arena is not necessarily locked, so we may have to try several times to split off a block.}
  while True do
  begin
    LPreviousLastSequentialFeedBlockOffset := APMediumBlockManager.LastMediumBlockSequentialFeedOffset;

    {Is there space available for at least the minimum size block?}
    LBlockSize := LPreviousLastSequentialFeedBlockOffset.IntegerValue - CMediumBlockSpanHeaderSize;
    if LBlockSize >= AMinimumBlockSize then
    begin
      if LBlockSize > AOptimalBlockSize then
        LBlockSize := AOptimalBlockSize;

      {Calculate the new sequential feed parameters.}
      LNewLastSequentialFeedBlockOffset.IntegerAndABACounter := LPreviousLastSequentialFeedBlockOffset.IntegerAndABACounter
        - LBlockSize + (Int64(1) shl 32);

      LPSequentialFeedSpan := APMediumBlockManager.SequentialFeedMediumBlockSpan;

      if AtomicCmpExchange(APMediumBlockManager.LastMediumBlockSequentialFeedOffset.IntegerAndABACounter,
        LNewLastSequentialFeedBlockOffset.IntegerAndABACounter,
        LPreviousLastSequentialFeedBlockOffset.IntegerAndABACounter) = LPreviousLastSequentialFeedBlockOffset.IntegerAndABACounter then
      begin
        Result := Pointer(PByte(LPSequentialFeedSpan) + LNewLastSequentialFeedBlockOffset.IntegerValue);

        {Set the header for the block.}
        SetMediumBlockHeader_SetSizeAndFlags(Result, LBlockSize, False, False);
        SetMediumBlockHeader_SetMediumBlockSpan(Result, LPSequentialFeedSpan);

        Exit;
      end;

    end
    else
    begin
      {There is either no sequential feed span, or it has insufficient space.}
      Exit(nil);
    end;
  end;
{$endif}
end;

{Clears the list of pending frees while attempting to reuse one of a suitable size.  The arena must be locked.}
function FastMM_GetMem_GetMediumBlock_TryReusePendingFreeBlock(APMediumBlockManager: PMediumBlockManager;
  AMinimumBlockSize, AOptimalBlockSize, AMaximumBlockSize: Integer): Pointer;
var
  LBlockSize, LBestMatchBlockSize, LSecondSplitSize: Integer;
  LPSecondSplit: PMediumFreeBlockContent;
  LPPendingFreeBlock, LPNextPendingFreeBlock: Pointer;
  LPMediumBlockSpan: PMediumBlockSpanHeader;
begin
  {Retrieve the pending free list pointer.}
  LPPendingFreeBlock := AtomicExchange(APMediumBlockManager.PendingFreeList, nil);
  if LPPendingFreeBlock = nil then
    Exit(nil);

  {Process all the pending frees, but keep the smallest block that is at least AMinimumBlockSize in size (if
  there is one).}
  LBestMatchBlockSize := MaxInt;
  Result := nil;

  while True do
  begin
    LPNextPendingFreeBlock := PPointer(LPPendingFreeBlock)^;
    LBlockSize := GetMediumBlockSize(LPPendingFreeBlock);

    if (LBlockSize >= AMinimumBlockSize) and (LBlockSize < LBestMatchBlockSize) then
    begin
      {Free the previous best match block.}
      if Result <> nil then
      begin
        LPMediumBlockSpan := GetMediumBlockSpan(Result);
        if FastMM_FreeMem_InternalFreeMediumBlock_ManagerAlreadyLocked(
          APMediumBlockManager, LPMediumBlockSpan, Result, False) <> 0 then
        begin
          System.Error(reInvalidPtr);
        end;
      end;
      Result := LPPendingFreeBlock;
      LBestMatchBlockSize := LBlockSize;
    end
    else
    begin
      LPMediumBlockSpan := GetMediumBlockSpan(LPPendingFreeBlock);
      if FastMM_FreeMem_InternalFreeMediumBlock_ManagerAlreadyLocked(
        APMediumBlockManager, LPMediumBlockSpan, LPPendingFreeBlock, False) <> 0 then
      begin
        System.Error(reInvalidPtr);
      end;
    end;

    if LPNextPendingFreeBlock = nil then
      Break;

    LPPendingFreeBlock := LPNextPendingFreeBlock;
  end;

  {Was there a suitable block in the pending free list?}
  if Result <> nil then
  begin

    {If the block currently has debug info, check it for consistency.}
    if BlockHasDebugInfo(Result)
      and (not CheckFreeDebugBlockIntact(Result)) then
    begin
      {The arena must be unlocked before the error is raised, otherwise the leak check at shutdown will hang.}
      APMediumBlockManager.MediumBlockManagerLocked := 0;
      System.Error(reInvalidPtr);
    end;

    {Should the block be split?}
    if LBestMatchBlockSize > AMaximumBlockSize then
    begin
      {Get the size of the second split}
      LSecondSplitSize := LBestMatchBlockSize - AOptimalBlockSize;
      {Adjust the block size}
      LBestMatchBlockSize := AOptimalBlockSize;
      {Split the block in two}
      LPSecondSplit := PMediumFreeBlockContent(PByte(Result) + LBestMatchBlockSize);
      LPMediumBlockSpan := GetMediumBlockSpan(Result);
      SetMediumBlockHeader_SetSizeAndFlags(LPSecondSplit, LSecondSplitSize, True, False);
      SetMediumBlockHeader_SetMediumBlockSpan(LPSecondSplit, LPMediumBlockSpan);

      {The second split is an entirely new block so all the header fields must be set.}
      SetMediumBlockHeader_SetIsSmallBlockSpan(LPSecondSplit, False);

      {Bin the second split.}
      if LSecondSplitSize >= CMinimumMediumBlockSize then
        InsertMediumBlockIntoBin(APMediumBlockManager, LPSecondSplit, LSecondSplitSize);

    end;

    {Set the header and trailer for this block, clearing the debug info flag.}
    SetMediumBlockHeader_SetSizeAndFlags(Result, LBestMatchBlockSize, False, False);

  end;
end;

{Allocates a free block of at least the size in AMinimumBlockSizeBinNumber.  The arena must be known to have a suitable
free block, the arena must be locked, and AOptimalBlockSize and AMaximumBlockSize must be aligned to a bin size.
Unlocks the arena before returning.  Returns a pointer to the allocated block.}
function FastMM_GetMem_GetMediumBlock_AllocateFreeBlockAndUnlockArena(APMediumBlockManager: PMediumBlockManager;
  AMinimumBlockSizeBinNumber, AOptimalBlockSize, AMaximumBlockSize: Integer): Pointer;
{$ifndef PurePascal}
const
  {The maximum block size is stored on the stack.}
  CMaximumSizeStackOffset = {$ifdef 32Bit}20{$else}80{$endif};
asm
{$ifdef X86ASM}
  {-------x86 Assembly language codepath--------}
  push ebx
  push esi
  push edi

  {esi = medium block manager, edi = bin number, ebp = optimal block size}
  mov esi, eax
  mov edi, edx
  mov ebp, ecx

  {Get the bin group in edx}
  shr edx, 5

  {Check the group corresponding to the minimum block size bin for available blocks.}
  mov ecx, 31
  and ecx, edi
  or eax, -1
  shl eax, cl
  and eax, dword ptr TMediumBlockManager.MediumBlockBinBitmaps(esi + edx * 4)
  jnz @GotBin
  {There are no suitable free blocks in the group containing AMinimumBlockSizeBinNumber, so get a free block from any
  subsequent group.}
  mov ecx, edx
  mov edx, -2
  shl edx, cl
  and edx, TMediumBlockManager(esi).MediumBlockBinGroupBitmap
  {Get the first group with large enough blocks in edx}
  bsf edx, edx
  {Get the bin bitmap for the next group with free blocks}
  mov eax, dword ptr TMediumBlockManager.MediumBlockBinBitmaps(esi + edx * 4)
@GotBin:

  {Group bitmap is in eax, group number in edx:  Find the first bin with free blocks in the group}
  bsf eax, eax
  {Add the index of the first bin in the group.}
  shl edx, 5
  add eax, edx

  {Get the first free block in the bin}
  mov edi, dword ptr TMediumBlockManager.FirstFreeBlockInBin(esi + eax * 4)

  mov eax, esi
  mov edx, edi
  call RemoveMediumFreeBlockFromBin

  {If the block currently has debug info, check it for consistency before resetting the flag.}
  test byte ptr [edi - CBlockStatusFlagsSize], CHasDebugInfoFlag
  jz @DebugInfoOK
  mov eax, edi
  call CheckFreeDebugBlockIntact
  test al, al
  jnz @DebugInfoOK
  mov byte ptr TMediumBlockManager(esi).MediumBlockManagerLocked, 0
  mov al, reInvalidPtr
  call System.Error
@DebugInfoOK:

  {Get the block size in ebx}
  movzx ebx, TMediumBlockHeader.MediumBlockSizeMultiple(edi - CMediumBlockHeaderSize)
  shl ebx, CMediumBlockAlignmentBits

  {Should the block be split?}
  cmp ebx, [esp + CMaximumSizeStackOffset]
  jbe @SecondSplitDone

  {Use the optimal block size, second split size in ecx}
  mov ecx, ebx
  sub ecx, ebp
  mov ebx, ebp

  {Second split pointer in edx}
  lea edx, [edi + ebx]

  {Get the span offset multiple of the first split in eax.}
  movzx eax, TMediumBlockHeader.MediumBlockSpanOffsetMultiple(edi - CMediumBlockHeaderSize)

  {The second split should already be tagged as a free block in the next block's header, but we need to set the size of
  the second split in its own footer.}
  mov TMediumFreeBlockFooter.MediumFreeBlockSize(edx + ecx - CMediumFreeBlockFooterSize), ecx
  {Set the second split's block size in its header}
  mov ebp, ecx
  shr ebp, CMediumBlockAlignmentBits
  mov TMediumBlockHeader.MediumBlockSizeMultiple(edx - CMediumBlockHeaderSize), bp
  {Set the span offset for the second split.  It is the sum of the offset and size multiples of the first split.}
  mov ebp, ebx
  shr ebp, CMediumBlockAlignmentBits
  add ebp, eax
  mov TMediumBlockHeader.MediumBlockSpanOffsetMultiple(edx - CMediumBlockHeaderSize), bp
  {Set the block flags for the second split}
  mov TMediumBlockHeader.BlockStatusFlags(edx - CMediumBlockHeaderSize), CBlockIsFreeFlag + CIsMediumBlockFlag
  {Ensure the second split is not marked as a small block span.}
  mov TMediumBlockHeader.IsSmallBlockSpan(edx - CMediumBlockHeaderSize), False

  {Bin the second split.}
  cmp ecx, CMinimumMediumBlockSize
  jb @SecondSplitDone
  mov eax, esi
  call InsertMediumBlockIntoBin
@SecondSplitDone:

  {Update the flag in the next block to indicate that this block is now in use.  The block size is not stored before
  the header of the next block if it is not free.}
  mov TMediumBlockHeader.PreviousBlockIsFree(edi + ebx - CMediumBlockHeaderSize), False
  {Set the block flags}
  mov TMediumBlockHeader.BlockStatusFlags(edi - CMediumBlockHeaderSize), CIsMediumBlockFlag
  {Update the block size.}
  shr ebx, CMediumBlockAlignmentBits
  mov TMediumBlockHeader.MediumBlockSizeMultiple(edi - CMediumBlockHeaderSize), bx

  mov byte ptr TMediumBlockManager(esi).MediumBlockManagerLocked, 0

  mov eax, edi

  pop edi
  pop esi
  pop ebx
{$else}
  {-------x64 Assembly language codepath--------}
  .pushnv rbx
  .pushnv rsi
  .pushnv rdi
  .params 3

  {rsi = medium block manager, edi = bin number, on stack = optimal block size, on stack = maximum block size}
  mov rsi, rcx
  mov edi, edx
  mov [rsp + CMaximumSizeStackOffset + 8], r8d //Save the optimal block size in the shadow space.
  mov [rsp + CMaximumSizeStackOffset], r9d //Save the maximum block size in the shadow space.

  {Get the bin group in edx}
  shr edx, 5

  {Check the group corresponding to the minimum block size bin for available blocks.}
  mov ecx, 31
  and ecx, edi
  or eax, -1
  shl eax, cl
  and eax, dword ptr TMediumBlockManager.MediumBlockBinBitmaps(rsi + rdx * 4)
  jnz @GotBin
  {There are no suitable free blocks in the group containing AMinimumBlockSizeBinNumber, so get a free block from any
  subsequent group.}
  mov ecx, edx
  mov edx, -2
  shl edx, cl
  and edx, TMediumBlockManager(rsi).MediumBlockBinGroupBitmap
  {Get the first group with large enough blocks in edx}
  bsf edx, edx
  {Get the bin bitmap for the next group with free blocks}
  mov eax, dword ptr TMediumBlockManager.MediumBlockBinBitmaps(rsi + rdx * 4)
@GotBin:

  {Group bitmap is in eax, group number in edx:  Find the first bin with free blocks in the group}
  bsf eax, eax
  {Add the index of the first bin in the group.}
  shl edx, 5
  add eax, edx

  {Get the first free block in the bin}
  mov rdi, qword ptr TMediumBlockManager.FirstFreeBlockInBin(rsi + rax * 8)

  mov rcx, rsi
  mov rdx, rdi
  call RemoveMediumFreeBlockFromBin

  {If the block currently has debug info, check it for consistency before resetting the flag.}
  test byte ptr [rdi - CBlockStatusFlagsSize], CHasDebugInfoFlag
  jz @DebugInfoOK
  mov rcx, rdi
  call CheckFreeDebugBlockIntact
  test al, al
  jnz @DebugInfoOK
  mov byte ptr TMediumBlockManager(rsi).MediumBlockManagerLocked, 0
  mov cl, reInvalidPtr
  call System.Error
@DebugInfoOK:

  {Get the block size in ebx}
  movzx ebx, TMediumBlockHeader.MediumBlockSizeMultiple(rdi - CMediumBlockHeaderSize)
  shl ebx, CMediumBlockAlignmentBits

  {Should the block be split?}
  cmp ebx, [esp + CMaximumSizeStackOffset]
  jbe @SecondSplitDone

  {Use the optimal block size, second split size in ecx}
  mov ecx, ebx
  mov ebx, [rsp + CMaximumSizeStackOffset + 8]
  sub ecx, ebx

  {Second split pointer in rdx}
  lea rdx, [rdi + rbx]

  {Get the span offset multiple of the first split in r9.}
  movzx r9d, TMediumBlockHeader.MediumBlockSpanOffsetMultiple(rdi - CMediumBlockHeaderSize)

  {The second split should already be tagged as a free block in the next block's header, but we need to set the size of
  the second split in its own footer.}
  mov TMediumFreeBlockFooter.MediumFreeBlockSize(rdx + rcx - CMediumFreeBlockFooterSize), ecx
  {Set the second split's block size in its header}
  mov eax, ecx
  shr eax, CMediumBlockAlignmentBits
  mov TMediumBlockHeader.MediumBlockSizeMultiple(rdx - CMediumBlockHeaderSize), ax
  {Set the span offset for the second split.  It is the sum of the offset and size multiples of the first split.}
  mov eax, ebx
  shr eax, CMediumBlockAlignmentBits
  add eax, r9d
  mov TMediumBlockHeader.MediumBlockSpanOffsetMultiple(rdx - CMediumBlockHeaderSize), ax
  {Set the block flags for the second split}
  mov TMediumBlockHeader.BlockStatusFlags(rdx - CMediumBlockHeaderSize), CBlockIsFreeFlag + CIsMediumBlockFlag
  {Ensure the second split is not marked as a small block span.}
  mov TMediumBlockHeader.IsSmallBlockSpan(rdx - CMediumBlockHeaderSize), False

  {Bin the second split.}
  cmp ecx, CMinimumMediumBlockSize
  jb @SecondSplitDone
  mov r8d, ecx
  mov rcx, rsi
  call InsertMediumBlockIntoBin
@SecondSplitDone:

  {Update the flag in the next block to indicate that this block is now in use.  The block size is not stored before
  the header of the next block if it is not free.}
  mov TMediumBlockHeader.PreviousBlockIsFree(rdi + rbx - CMediumBlockHeaderSize), False
  {Set the block flags}
  mov TMediumBlockHeader.BlockStatusFlags(rdi - CMediumBlockHeaderSize), CIsMediumBlockFlag
  {Update the block size.}
  shr ebx, CMediumBlockAlignmentBits
  mov TMediumBlockHeader.MediumBlockSizeMultiple(rdi - CMediumBlockHeaderSize), bx

  mov byte ptr TMediumBlockManager(rsi).MediumBlockManagerLocked, 0

  mov rax, rdi

{$endif}
{$else}
var
  LBinGroupNumber, LBinNumber, LBinGroupMasked, LBinGroupsMasked, LBlockSize, LSecondSplitSize: Integer;
  LPSecondSplit: PMediumFreeBlockContent;
begin
  LBinGroupNumber := AMinimumBlockSizeBinNumber shr 5; //32 bins per group

  {Is there an available block in the group containing the bin?}
  LBinGroupMasked := APMediumBlockManager.MediumBlockBinBitmaps[LBinGroupNumber] and ((-1) shl (AMinimumBlockSizeBinNumber and 31));
  if LBinGroupMasked <> 0 then
  begin
    {There is a block in the group containing AMinimumBlockSizeBinNumber, get the exact bin number.}
    LBinNumber := CountTrailingZeros32(LBinGroupMasked) + (LBinGroupNumber shl 5);
  end
  else
  begin
    {There are no suitable free blocks in the group containing AMinimumBlockSizeBinNumber, so get a free block from any
    subsequent group.}
    LBinGroupsMasked := APMediumBlockManager.MediumBlockBinGroupBitmap and ((-2) shl LBinGroupNumber);
    {There is a suitable group with space:  Get the bin group number}
    LBinGroupNumber := CountTrailingZeros32(LBinGroupsMasked);
    {Get the first bin with a free block in the group}
    LBinNumber := CountTrailingZeros32(APMediumBlockManager.MediumBlockBinBitmaps[LBinGroupNumber]) + (LBinGroupNumber shl 5);
  end;

  Result := APMediumBlockManager.FirstFreeBlockInBin[LBinNumber];

  RemoveMediumFreeBlockFromBin(APMediumBlockManager, Result);

  {If the block currently has debug info, check it for consistency before resetting the flag.}
  if BlockHasDebugInfo(Result)
    and (not CheckFreeDebugBlockIntact(Result)) then
  begin
    APMediumBlockManager.MediumBlockManagerLocked := 0;
    System.Error(reInvalidPtr);
  end;

  {Get the size of the available medium block}
  LBlockSize := GetMediumBlockSize(Result);

  {Should the block be split?}
  if LBlockSize > AMaximumBlockSize then
  begin
    {Get the size of the second split}
    LSecondSplitSize := LBlockSize - AOptimalBlockSize;
    {Adjust the block size}
    LBlockSize := AOptimalBlockSize;
    {Split the block in two}
    LPSecondSplit := PMediumFreeBlockContent(PByte(Result) + LBlockSize);
    SetMediumBlockHeader_SetSizeAndFlags(LPSecondSplit, LSecondSplitSize, True, False);
    SetMediumBlockHeader_SetMediumBlockSpan(LPSecondSplit, GetMediumBlockSpan(Result));

    {The second split is an entirely new block so all the header fields must be set.}
    SetMediumBlockHeader_SetIsSmallBlockSpan(LPSecondSplit, False);

    {Bin the second split.}
    if LSecondSplitSize >= CMinimumMediumBlockSize then
      InsertMediumBlockIntoBin(APMediumBlockManager, LPSecondSplit, LSecondSplitSize);
  end;

  {Set the header for this block, clearing the debug info flag.}
  SetMediumBlockHeader_SetSizeAndFlags(Result, LBlockSize, False, False);

  APMediumBlockManager.MediumBlockManagerLocked := 0;
{$endif}
end;

{Allocates a medium block within the given size constraints.  Sizes must be properly aligned to a bin size.}
function FastMM_GetMem_GetMediumBlock(AMinimumBlockSize, AOptimalBlockSize, AMaximumBlockSize: Integer): Pointer;
{$ifdef X86ASM}
const
  {The offsets of variables on the stack.}
  CMinimumBlockSizeOffset = 12;
  COptimalBlockSizeOffset = 8;
  CMaximumBlockSizeOffset = 4;
  CBinNumberOffset = 0;
asm
  push ebx
  push esi
  push edi
  push ebp
  push eax
  push edx
  push ecx

  {Calculate the bin number for the minimum block size.}
  cmp eax, CMediumBlockMiddleBinsStart
  jg @MiddleOrLastMediumBlockGroup
  lea edx, [eax - CMinimumMediumBlockSize]
  shr edx, CInitialBinSpacingBits
  jmp @GotBinNumber
@MiddleOrLastMediumBlockGroup:
  cmp eax, CMediumBlockFinalBinsStart
  jg @LastMediumBlockGroup
  lea edx, [eax + CInitialBinCount * CMiddleBinSpacing - CMediumBlockMiddleBinsStart]
  shr edx, CMiddleBinSpacingBits
  jmp @GotBinNumber
@LastMediumBlockGroup:
  lea edx, [eax + (CInitialBinCount + CMiddleBinCount) * CFinalBinSpacing - CMediumBlockFinalBinsStart]
  shr edx, CFinalBinSpacingBits
@GotBinNumber:
  push edx

  {Minimum block size group in edi}
  mov edi, edx
  shr edi, 5

  {Bin mask in ebx}
  mov ecx, 31
  and ecx, edx
  mov ebx, -1
  shl ebx, cl

  {Larger groups mask in ebp}
  mov ecx, edi
  mov ebp, -2
  shl ebp, cl

@OuterLoop:

  {--------------Attempt 1--------------
  Try to get a block from the first arena with an available block.  During the first attempt only memory that has
  already been reserved for medium blocks will be used - no new spans will be allocated.  We also avoid grabbing a
  sequential feed block, because that may touch a new page and cause a page fault.  The sequence of allocation attempts
  is:
    1.1) The pending free list
    1.2) From the medium block free lists}

  mov esi, offset MediumBlockManagers
@Attempt1Loop:
  cmp byte ptr TMediumBlockManager(esi).MediumBlockManagerLocked, 0
  jne @Attempt1NextManager
  cmp TMediumBlockManager(esi).PendingFreeList, 0
  jne @Attempt1TryLock
  test TMediumBlockManager(esi).MediumBlockBinGroupBitmap, ebp
  jnz @Attempt1TryLock
  test dword ptr TMediumBlockManager.MediumBlockBinBitmaps(esi + edi * 4), ebx
  jz @Attempt1NextManager
@Attempt1TryLock:
  mov al, 1
  xchg byte ptr TMediumBlockManager(esi).MediumBlockManagerLocked, al
  test al, al
  jnz @Attempt1NextManager

  {1.1) Process pending free lists:  Scan the pending free lists for all medium block managers first, and reuse
  a block that is of sufficient size if possible.}
  cmp TMediumBlockManager(esi).PendingFreeList, 0
  je @Attempt1NoPendingFrees
  mov eax, esi
  mov edx, [esp + CMinimumBlockSizeOffset]
  mov ecx, [esp + COptimalBlockSizeOffset]
  push [esp + CMaximumBlockSizeOffset]
  call FastMM_GetMem_GetMediumBlock_TryReusePendingFreeBlock
  test eax, eax
  jnz @UnlockManagerAndExit
@Attempt1NoPendingFrees:

  {1.2) Try to find a suitable free block in the free lists}
  test TMediumBlockManager(esi).MediumBlockBinGroupBitmap, ebp
  jnz @Attempt1HasFreeBlock
  test dword ptr TMediumBlockManager.MediumBlockBinBitmaps(esi + edi * 4), ebx
  jz @Attempt1NoFreeBlocks
@Attempt1HasFreeBlock:
  mov eax, esi
  mov edx, [esp + CBinNumberOffset]
  mov ecx, [esp + COptimalBlockSizeOffset]
  push [esp + CMaximumBlockSizeOffset]
  call FastMM_GetMem_GetMediumBlock_AllocateFreeBlockAndUnlockArena
  jmp @Done
@Attempt1NoFreeBlocks:

  {A different thread grabbed the last block, unlock the manager and try the next arena.}
  mov TMediumBlockManager(esi).MediumBlockManagerLocked, 0

@Attempt1NextManager:
  cmp esi, offset MediumBlockManagers + CMediumBlockManagerSize * (CFastMM_MediumBlockArenaCount - 1)
  jnb @Attempt1Failed
  add esi, CMediumBlockManagerSize
  jmp @Attempt1Loop
@Attempt1Failed:

  {--------------Attempt 2--------------
  Try to get a block from a sequential feed span.  Splitting off a sequentisal feed block is very likely to touch a new
  memory page and thus cause an (expensive) page fault.}

  {edx = AMinimumBlockSize, eax = AMinimumBlockSize + CMediumBlockSpanHeaderSize}
  mov edx, [esp + CMinimumBlockSizeOffset]
  lea eax, [edx + CMediumBlockSpanHeaderSize]
  mov esi, offset MediumBlockManagers
@Attempt2Loop:

  {2.1) Try to feed a medium block sequentially from an existing sequential feed span}
  cmp eax, TMediumBlockManager(esi).LastMediumBlockSequentialFeedOffset.IntegerValue
  ja @Attempt2NextManager
  mov eax, esi
  mov ecx, [esp + COptimalBlockSizeOffset]
  call FastMM_GetMem_GetMediumBlock_TryGetBlockFromSequentialFeedSpan
  test eax, eax
  jnz @Done
  {The call failed:  Restore edx and eax to correct values}
  mov edx, [esp + CMinimumBlockSizeOffset]
  lea eax, [edx + CMediumBlockSpanHeaderSize]

@Attempt2NextManager:
  cmp esi, offset MediumBlockManagers + CMediumBlockManagerSize * (CFastMM_MediumBlockArenaCount - 1)
  jnb @Attempt2Failed
  add esi, CMediumBlockManagerSize
  jmp @Attempt2Loop
@Attempt2Failed:

  {--------------Attempt 3--------------
  At this point (a) all arenas are either locked, or (b) there are no pending free blocks, no free blocks, and all
  the sequential feed spans are exhausted.  In this second attempt the first unlocked manager is locked and a block
  will then be obtained from it in this sequence:

    3.1) From the medium block free lists
    3.2) From the existing sequential feed span
    3.3) From the pending free list
    3.4) From a new sequential feed span.}

  mov esi, offset MediumBlockManagers
@Attempt3Loop:

  {Try to lock the manager}
  mov eax, $100
  lock cmpxchg byte ptr TMediumBlockManager(esi).MediumBlockManagerLocked, ah
  jne @Attempt3NextManager

  {3.1) Try to allocate a free block.  Another thread may have freed a block before this arena could be locked.}
  test TMediumBlockManager(esi).MediumBlockBinGroupBitmap, ebp
  jnz @Attempt3HasFreeBlock
  test dword ptr TMediumBlockManager.MediumBlockBinBitmaps(esi + edi * 4), ebx
  jz @Attempt3NoFreeBlocks
@Attempt3HasFreeBlock:
  mov eax, esi
  mov edx, [esp + CBinNumberOffset]
  mov ecx, [esp + COptimalBlockSizeOffset]
  push [esp + CMaximumBlockSizeOffset]
  call FastMM_GetMem_GetMediumBlock_AllocateFreeBlockAndUnlockArena
  jmp @Done
@Attempt3NoFreeBlocks:

  {3.2) Another thread may have allocated a sequential feed span before the arena could be locked, so a second attempt
  at feeding a block sequentially should be made before allocating a new span.}
  mov edx, [esp + CMinimumBlockSizeOffset]
  lea eax, [edx + CMediumBlockSpanHeaderSize]
  cmp eax, TMediumBlockManager(esi).LastMediumBlockSequentialFeedOffset.IntegerValue
  ja @Attempt3NoSequentialFeed
  mov eax, esi
  mov ecx, [esp + COptimalBlockSizeOffset]
  call FastMM_GetMem_GetMediumBlock_TryGetBlockFromSequentialFeedSpan
  test eax, eax
  jnz @UnlockManagerAndExit
@Attempt3NoSequentialFeed:

  {3.3) Another thread may have added blocks to the pending free list in the meantime - again try to reuse a pending
  free block.  Allocating a new span is very expensive, so has to be avoided if at all possible.}
  cmp TMediumBlockManager(esi).PendingFreeList, 0
  je @Attempt3NoPendingFrees
  mov eax, esi
  mov edx, [esp + CMinimumBlockSizeOffset]
  mov ecx, [esp + COptimalBlockSizeOffset]
  push [esp + CMaximumBlockSizeOffset]
  call FastMM_GetMem_GetMediumBlock_TryReusePendingFreeBlock
  test eax, eax
  jnz @UnlockManagerAndExit
@Attempt3NoPendingFrees:

  {3.4) If we get here then there are no suitable free blocks, pending free blocks, and the current sequential feed span
  has no space:  Allocate a new sequential feed span and split off a block of the optimal size.}
  mov eax, esi
  mov edx, [esp + COptimalBlockSizeOffset]
  call FastMM_GetMem_GetMediumBlock_AllocateNewSequentialFeedSpan
  jmp @UnlockManagerAndExit

@Attempt3NextManager:
  cmp esi, offset MediumBlockManagers + CMediumBlockManagerSize * (CFastMM_MediumBlockArenaCount - 1)
  jnb @Attempt3Failed
  add esi, CMediumBlockManagerSize
  jmp @Attempt3Loop
@Attempt3Failed:

  {--------Back off--------}

  {All arenas are currently locked:  Back off and try again.}
  call LogMediumBlockThreadContentionAndYieldToOtherThread
  jmp @OuterLoop

@UnlockManagerAndExit:
  mov TMediumBlockManager(esi).MediumBlockManagerLocked, 0

@Done:
  pop ecx
  pop ecx
  pop ecx
  pop ecx
  pop ebp
  pop edi
  pop esi
  pop ebx

{$else}
var
  LPMediumBlockManager: PMediumBlockManager;
  LMinimumBlockSizeBinNumber, LMinimumBlockSizeBinGroupNumber, LMinimumBlockSizeBinMask, LLargerBinGroupsMask: Integer;
begin
  {Determine the bin for blocks of the minimum size, as well as the masks for the bins and groups that will have blocks
  of at least the requested size.}
  LMinimumBlockSizeBinNumber := GetBinNumberForMediumBlockSize(AMinimumBlockSize);
  LMinimumBlockSizeBinGroupNumber := LMinimumBlockSizeBinNumber shr 5; //32 bins per group
  LMinimumBlockSizeBinMask := ((-1) shl (LMinimumBlockSizeBinNumber and 31));
  LLargerBinGroupsMask := ((-2) shl LMinimumBlockSizeBinGroupNumber);

  while True do
  begin

    {--------------Attempt 1--------------
    Try to get a block from the first arena with an available block.  During the first attempt only memory that has
    already been reserved for medium blocks will be used - no new spans will be allocated.  We also avoid grabbing a
    sequential feed block, because that may touch a new page and cause a page fault (which is expensive).  The sequence
    of allocation attempts is:
      1.1) The pending free list
      1.2) From the medium block free lists}


    LPMediumBlockManager := @MediumBlockManagers[0];
    while True do
    begin

      {In order to process the pending free lists or get a block from the free lists the block manager must be locked.
      Locking is expensive, so first check whether locking the manager is likely to result in successful block
      allocation.}
      if (LPMediumBlockManager.MediumBlockManagerLocked = 0)
        and ((LPMediumBlockManager.PendingFreeList <> nil)
          or ((LPMediumBlockManager.MediumBlockBinGroupBitmap and LLargerBinGroupsMask) <> 0)
          or ((LPMediumBlockManager.MediumBlockBinBitmaps[LMinimumBlockSizeBinGroupNumber] and LMinimumBlockSizeBinMask) <> 0))
        and (AtomicExchange(LPMediumBlockManager.MediumBlockManagerLocked, 1) = 0) then
      begin

        {1.1) Process pending free lists:  Scan the pending free lists for all medium block managers first, and reuse
        a block that is of sufficient size if possible.}
        if LPMediumBlockManager.PendingFreeList <> nil then
        begin
          Result := FastMM_GetMem_GetMediumBlock_TryReusePendingFreeBlock(LPMediumBlockManager, AMinimumBlockSize,
            AOptimalBlockSize, AMaximumBlockSize);
          if Result <> nil then
          begin
            LPMediumBlockManager.MediumBlockManagerLocked := 0;
            Exit;
          end;
        end;

        {1.2) Try to find a suitable free block in the free lists}
        if ((LPMediumBlockManager.MediumBlockBinGroupBitmap and LLargerBinGroupsMask) <> 0)
          or ((LPMediumBlockManager.MediumBlockBinBitmaps[LMinimumBlockSizeBinGroupNumber] and LMinimumBlockSizeBinMask) <> 0) then
        begin
          Exit(FastMM_GetMem_GetMediumBlock_AllocateFreeBlockAndUnlockArena(LPMediumBlockManager,
            LMinimumBlockSizeBinNumber, AOptimalBlockSize, AMaximumBlockSize));
        end;

        {A different thread grabbed the last block, unlock the manager and try the next arena.}
        LPMediumBlockManager.MediumBlockManagerLocked := 0;
      end;

      {Try the next arena.}
      if NativeUInt(LPMediumBlockManager) >= NativeUInt(@MediumBlockManagers[CFastMM_MediumBlockArenaCount - 1]) then
        Break;

      Inc(LPMediumBlockManager);
    end;

    {--------------Attempt 2--------------
    Try to get a block from a sequential feed span.  This is likely to touch a new page and thus cause a page fault,
    which is expensive.}

    LPMediumBlockManager := @MediumBlockManagers[0];
    while True do
    begin

      {2.1) Try to feed a medium block sequentially from an existing sequential feed span}
      if LPMediumBlockManager.LastMediumBlockSequentialFeedOffset.IntegerValue >= (AMinimumBlockSize + CMediumBlockSpanHeaderSize) then
      begin
        Result := FastMM_GetMem_GetMediumBlock_TryGetBlockFromSequentialFeedSpan(LPMediumBlockManager,
          AMinimumBlockSize, AOptimalBlockSize);
        if Result <> nil then
          Exit;
      end;

      {Try the next arena.}
      if NativeUInt(LPMediumBlockManager) >= NativeUInt(@MediumBlockManagers[CFastMM_MediumBlockArenaCount - 1]) then
        Break;

      Inc(LPMediumBlockManager);
    end;

    {--------------Attempt 3--------------
    At this point (a) all arenas are either locked, or (b) there are no pending free blocks, no free blocks, and all
    the sequential feed spans are exhausted.  In this second attempt the first unlocked manager is locked and a block
    will then be obtained from it in this sequence:

      3.1) From the medium block free lists
      3.2) From the existing sequential feed span
      3.3) From the pending free list
      3.4) From a new sequential feed span.}


    LPMediumBlockManager := @MediumBlockManagers[0];
    while True do
    begin

      {The first attempt failed to get a block from any manager, so in this second attempt we are more forceful:  Always
      try to lock the manager, and allocate a new sequential feed span if necessary.}
      if AtomicCmpExchange(LPMediumBlockManager.MediumBlockManagerLocked, 1, 0) = 0 then
      begin

        {3.1) Try to allocate a free block.  Another thread may have freed a block before this arena could be locked.}
        if ((LPMediumBlockManager.MediumBlockBinGroupBitmap and LLargerBinGroupsMask) <> 0)
          or ((LPMediumBlockManager.MediumBlockBinBitmaps[LMinimumBlockSizeBinGroupNumber] and LMinimumBlockSizeBinMask) <> 0) then
        begin
          Exit(FastMM_GetMem_GetMediumBlock_AllocateFreeBlockAndUnlockArena(LPMediumBlockManager,
            LMinimumBlockSizeBinNumber, AOptimalBlockSize, AMaximumBlockSize));
        end;

        {3.2) Another thread may have allocated a sequential feed span before the arena could be locked, so a second
        attempt at feeding a block sequentially should be made before allocating a new span.}
        if LPMediumBlockManager.LastMediumBlockSequentialFeedOffset.IntegerValue >= (AMinimumBlockSize + CMediumBlockSpanHeaderSize) then
        begin
          Result := FastMM_GetMem_GetMediumBlock_TryGetBlockFromSequentialFeedSpan(LPMediumBlockManager,
            AMinimumBlockSize, AOptimalBlockSize);
          if Result <> nil then
          begin
            LPMediumBlockManager.MediumBlockManagerLocked := 0;
            Exit;
          end;
        end;

        {3.3) Another thread may have added blocks to the pending free list in the meantime - again try to reuse a
        pending free block.  Allocating a new span is very expensive, so has to be avoided if at all possible.}
        if LPMediumBlockManager.PendingFreeList <> nil then
        begin
          Result := FastMM_GetMem_GetMediumBlock_TryReusePendingFreeBlock(LPMediumBlockManager, AMinimumBlockSize,
            AOptimalBlockSize, AMaximumBlockSize);
          if Result <> nil then
          begin
            LPMediumBlockManager.MediumBlockManagerLocked := 0;
            Exit;
          end;
        end;

        {3.4) If we get here then there are no suitable free blocks, pending free blocks, and the current sequential
        feed span has no space:  Allocate a new sequential feed span and split off a block of the optimal size.}
        Result := FastMM_GetMem_GetMediumBlock_AllocateNewSequentialFeedSpan(LPMediumBlockManager, AOptimalBlockSize);
        LPMediumBlockManager.MediumBlockManagerLocked := 0;
        Exit;
      end;

      {The arena could not be locked - try the next one.}
      if NativeUInt(LPMediumBlockManager) >= NativeUInt(@MediumBlockManagers[CFastMM_MediumBlockArenaCount - 1]) then
        Break;

      Inc(LPMediumBlockManager);
    end;

    {--------Back off--------}

    {All arenas are currently locked:  Back off and try again.}
    LogMediumBlockThreadContentionAndYieldToOtherThread;

  end;

{$endif}
end;

function FastMM_ReallocMem_ReallocMediumBlock_Upsize(APointer: Pointer; ANewUserSize: NativeInt): Pointer;
var
  LPNextBlock: Pointer;
  LBlockSize, LOldUserSize, LNextBlockSize, LCombinedUserSize, LNewAllocSize, LNewBlockSize, LSecondSplitSize: NativeInt;
  LPMediumBlockSpan: PMediumBlockSpanHeader;
  LPMediumBlockManager: PMediumBlockManager;
begin
  {What is the available size in the block being reallocated?}
  LBlockSize := GetMediumBlockSize(APointer);
  {Get a pointer to the next block}
  LPNextBlock := Pointer(PByte(APointer) + LBlockSize);
  {Subtract the block header size from the old available size}
  LOldUserSize := LBlockSize - CMediumBlockHeaderSize;

  {If the next block is free then we need to check if this block can be upsized in-place.}
  if BlockIsFree(LPNextBlock) then
  begin
    LNextBlockSize := GetMediumBlockSize(LPNextBlock);
    LCombinedUserSize := LOldUserSize + LNextBlockSize;
    if ANewUserSize <= LCombinedUserSize then
    begin

      {The next block is currently free and there is enough space to grow this block in place.  Try to lock the
      medium block manager.  If it can be locked and the next block is still free and large enough then stretch the
      medium block in place.}
      LPMediumBlockSpan := GetMediumBlockSpan(APointer);
      LPMediumBlockManager := LPMediumBlockSpan.MediumBlockManager;
      if (LPMediumBlockManager.MediumBlockManagerLocked = 0)
        and (AtomicExchange(LPMediumBlockManager.MediumBlockManagerLocked, 1) = 0) then
      begin

        {We need to recheck this, since another thread could have grabbed the block before the manager could be
        locked.}
        LNextBlockSize := GetMediumBlockSize(LPNextBlock);
        LCombinedUserSize := LOldUserSize + LNextBlockSize;

        if (ANewUserSize <= LCombinedUserSize)
          and BlockIsFree(LPNextBlock) then
        begin
          if LNextBlockSize >= CMinimumMediumBlockSize then
            RemoveMediumFreeBlockFromBin(LPMediumBlockManager, LPNextBlock);

          {Grow by at least 25% for medium block in-place upsizes}
          LNewAllocSize := LOldUserSize + (LOldUserSize shr 2);
          if LNewAllocSize < ANewUserSize then
            LNewAllocSize := ANewUserSize;
          {Round up to the nearest block size granularity}
          LNewBlockSize := ((LNewAllocSize + (CMediumBlockHeaderSize + CMediumBlockAlignment - 1))
            and -CMediumBlockAlignment);
          {Calculate the size of the second split}
          LSecondSplitSize := LCombinedUserSize + CMediumBlockHeaderSize - LNewBlockSize;
          {Does it fit?}
          if LSecondSplitSize <= 0 then
          begin
            {The block size is the full available size plus header}
            LNewBlockSize := LCombinedUserSize + CMediumBlockHeaderSize;
          end
          else
          begin
            {Split the block in two}
            LPNextBlock := PMediumFreeBlockContent(PByte(APointer) + LNewBlockSize);

            SetMediumBlockHeader_SetSizeAndFlags(LPNextBlock, LSecondSplitSize, True, False);
            SetMediumBlockHeader_SetMediumBlockSpan(LPNextBlock, LPMediumBlockSpan);
            {The second split is an entirely new block so all the header fields must be set.}
            SetMediumBlockHeader_SetIsSmallBlockSpan(LPNextBlock, False);

            {Put the remainder in a bin if it is big enough}
            if LSecondSplitSize >= CMinimumMediumBlockSize then
              InsertMediumBlockIntoBin(LPMediumBlockManager, LPNextBlock, LSecondSplitSize);
          end;

          {Set the size and flags for this block}
          SetMediumBlockHeader_SetSizeAndFlags(APointer, LNewBlockSize, False, False);

          {Unlock the medium blocks}
          LPMediumBlockManager.MediumBlockManagerLocked := 0;

          Exit(APointer);
        end;

        {Couldn't use the next block, because another thread grabbed it:  Unlock the medium blocks}
        LPMediumBlockManager.MediumBlockManagerLocked := 0;
      end;
    end;
  end;

  {Couldn't upsize in place.  Allocate a new block and move the data across:  If we have to reallocate and move
  medium blocks, we grow by at least 25%}
  LNewAllocSize := LOldUserSize + (LOldUserSize shr 2);
  if LNewAllocSize < ANewUserSize then
    LNewAllocSize := ANewUserSize;
  {Allocate the new block}
  Result := FastMM_GetMem(LNewAllocSize);
  if Result <> nil then
  begin
    {If it's a large block - store the actual user requested size}
    if LNewAllocSize > (CMaximumMediumBlockSize - CMediumBlockHeaderSize) then
      PLargeBlockHeader(Result)[-1].UserAllocatedSize := ANewUserSize;
    {Move the data across}
    MoveMultipleOf64_Large(APointer^, Result^, LOldUserSize);
    {Free the old block}
    FastMM_FreeMem(APointer);
  end;
end;

function FastMM_ReallocMem_ReallocMediumBlock_Downsize(APointer: Pointer; ANewUserSize: NativeInt): Pointer;
var
  LPNextBlock: Pointer;
  LBlockSize, LOldUserSize, LNewBlockSize, LSecondSplitSize: NativeInt;
  LPMediumBlockSpan: PMediumBlockSpanHeader;
begin
  {What is the available size in the block being reallocated?}
  LBlockSize := GetMediumBlockSize(APointer);
  {Subtract the block header size from the old available size}
  LOldUserSize := LBlockSize - CMediumBlockHeaderSize;

  {In-place downsize?  Balance the cost of moving the data vs. the cost of fragmenting the address space.}
  if ANewUserSize >= CMediumInPlaceDownsizeLimit then
  begin

    {Medium blocks in use may never be smaller than CMinimumMediumBlockSize.}
    if ANewUserSize < (CMinimumMediumBlockSize - CMediumBlockHeaderSize) then
      ANewUserSize := CMinimumMediumBlockSize - CMediumBlockHeaderSize;

    {Round up to the next medium block size}
    LNewBlockSize := ((ANewUserSize + (CMediumBlockHeaderSize + CMediumBlockAlignment - 1))
      and -CMediumBlockAlignment);

    LSecondSplitSize := (LOldUserSize + CMediumBlockHeaderSize) - LNewBlockSize;
    if LSecondSplitSize > 0 then
    begin

      LPMediumBlockSpan := GetMediumBlockSpan(APointer);

      {Set a proper header for the second split.}
      LPNextBlock := PMediumBlockHeader(PByte(APointer) + LNewBlockSize);
      SetMediumBlockHeader_SetSizeAndFlags(LPNextBlock, LSecondSplitSize, False, False);
      SetMediumBlockHeader_SetMediumBlockSpan(LPNextBlock, LPMediumBlockSpan);
      {The second split is an entirely new block so all the header fields must be set.}
      SetMediumBlockHeader_SetIsSmallBlockSpan(LPNextBlock, False);

      {Adjust the size of this block.}
      SetMediumBlockHeader_SetSizeAndFlags(APointer, LNewBlockSize, False, False);

      {Free the second split.}
      FastMM_FreeMem(LPNextBlock);
    end;

    Result := APointer;
  end
  else
  begin

    {Allocate the new block, move the data across and then free the old block.}
    Result := FastMM_GetMem(ANewUserSize);
    if Result <> nil then
    begin
      System.Move(APointer^, Result^, ANewUserSize);
      FastMM_FreeMem(APointer);
    end;

  end;

end;

function FastMM_ReallocMem_ReallocMediumBlock(APointer: Pointer; ANewUserSize: NativeInt): Pointer;
{$ifdef X86ASM}
asm
  {Get the old user size in ecx}
  movzx ecx, TMediumBlockHeader.MediumBlockSizeMultiple(eax - CMediumBlockHeaderSize)
  shl ecx, CMediumBlockAlignmentBits
  sub ecx, CMediumBlockHeaderSize

  cmp ecx, edx
  jb FastMM_ReallocMem_ReallocMediumBlock_Upsize

  {The requested size must be less than half the current size or we don't bother resizing.}
  shr ecx, 1
  cmp ecx, edx
  ja FastMM_ReallocMem_ReallocMediumBlock_Downsize
{$else}
var
  LOldUserSize: NativeInt;
begin
  LOldUserSize := GetMediumBlockSize(APointer) - CMediumBlockHeaderSize;
  if LOldUserSize < ANewUserSize then
  begin
    Result := FastMM_ReallocMem_ReallocMediumBlock_Upsize(APointer, ANewUserSize);
  end
  else
  begin
    {The requested size must be less than half the current size or we don't bother resizing.}
    if (LOldUserSize shr 1) > ANewUserSize then
      Result := FastMM_ReallocMem_ReallocMediumBlock_Downsize(APointer, ANewUserSize)
    else
      Result := APointer;
  end;
{$endif}
end;

{-----------------------------------------}
{--------Small block management-----------}
{-----------------------------------------}

procedure SetSmallBlockHeader(APSmallBlock: Pointer; APSmallBlockSpan: PSmallBlockSpanHeader; ABlockIsFree: Boolean;
  ABlockHasDebugInfo: Boolean); inline;
begin
  if ABlockIsFree then
  begin

    if ABlockHasDebugInfo then
    begin
      PSmallBlockHeader(APSmallBlock)[-1].BlockStatusFlagsAndSpanOffset :=
        (((NativeInt(APSmallBlock) - NativeInt(APSmallBlockSpan)) and -CMediumBlockAlignment) shr CSmallBlockSpanOffsetBitShift)
        + (CHasDebugInfoFlag + CBlockIsFreeFlag + CIsSmallBlockFlag);
    end
    else
    begin
      PSmallBlockHeader(APSmallBlock)[-1].BlockStatusFlagsAndSpanOffset :=
        (((NativeInt(APSmallBlock) - NativeInt(APSmallBlockSpan)) and -CMediumBlockAlignment) shr CSmallBlockSpanOffsetBitShift)
        + (CBlockIsFreeFlag + CIsSmallBlockFlag);
    end;

  end
  else
  begin

    if ABlockHasDebugInfo then
    begin
      PSmallBlockHeader(APSmallBlock)[-1].BlockStatusFlagsAndSpanOffset :=
        (((NativeInt(APSmallBlock) - NativeInt(APSmallBlockSpan)) and -CMediumBlockAlignment) shr CSmallBlockSpanOffsetBitShift)
        + (CHasDebugInfoFlag + CIsSmallBlockFlag);
    end
    else
    begin
      PSmallBlockHeader(APSmallBlock)[-1].BlockStatusFlagsAndSpanOffset :=
        ((NativeInt(APSmallBlock) - NativeInt(APSmallBlockSpan)) and -CMediumBlockAlignment) shr CSmallBlockSpanOffsetBitShift
        + CIsSmallBlockFlag;
    end;

  end;

end;

function GetSpanForSmallBlock(APSmallBlock: Pointer): PSmallBlockSpanHeader; inline;
begin
  Result := Pointer((NativeInt(APSmallBlock) and -CMediumBlockAlignment)
    - (CDropSmallBlockFlagsMask and PBlockStatusFlags(APSmallBlock)[-1]) shl CSmallBlockSpanOffsetBitShift);
end;

{Subroutine for FastMM_FreeMem_FreeSmallBlock.  The small block manager must already be locked.  Optionally unlocks the
small block manager before exit.  Returns 0 on success.}
function FastMM_FreeMem_FreeSmallBlock_ManagerAlreadyLocked(APSmallBlockSpan: PSmallBlockSpanHeader;
  APSmallBlock: Pointer; AUnlockSmallBlockManager: Boolean): Integer;
{$ifdef X86ASM}
asm
  push ebx

  sub TSmallBlockSpanHeader(eax).BlocksInUse, 1
  jz @SpanIsEmpty
@DoNotFreeSpan:

  {Get the old first free block in ebx}
  mov ebx, TSmallBlockSpanHeader(eax).FirstFreeBlock

  {Mark the block as free, keeping the other flags (e.g. debug info) intact.}
  or TSmallBlockHeader.BlockStatusFlagsAndSpanOffset(edx - CSmallBlockHeaderSize), CBlockIsFreeFlag

  {Store the old first free block}
  mov [edx], ebx

  {Store this as the new first free block}
  mov TSmallBlockSpanHeader(eax).FirstFreeBlock, edx

  {Get the small block manager in edx}
  mov edx, TSmallBlockSpanHeader(eax).SmallBlockManager

  {Was the span previously full?}
  test ebx, ebx
  jnz @SpanWasNotFull

  {Insert this as the first partially free span for the block size}
  mov ebx, TSmallBlockManager(edx).FirstPartiallyFreeSpan

  mov TSmallBlockSpanHeader(eax).PreviousPartiallyFreeSpan, edx
  mov TSmallBlockSpanHeader(eax).NextPartiallyFreeSpan, ebx
  mov TSmallBlockManager(edx).FirstPartiallyFreeSpan, eax
  mov TSmallBlockSpanHeader(ebx).PreviousPartiallyFreeSpan, eax

@SpanWasNotFull:

  pop ebx
  xor eax, eax

  test cl, cl
  jz @DoNotUnlock2
  mov byte ptr TSmallBlockManager(edx).SmallBlockManagerLocked, 0
@DoNotUnlock2:

  ret

@SpanIsEmpty:
  {In debug mode spans are never freed.}
  cmp DebugModeCounter, 0
  jg @DoNotFreeSpan
  {Remove this span from the circular linked list of partially free spans for the block type.}
  mov edx, TSmallBlockSpanHeader(eax).PreviousPartiallyFreeSpan
  mov ebx, TSmallBlockSpanHeader(eax).NextPartiallyFreeSpan
  mov TSmallBlockSpanHeader(edx).NextPartiallyFreeSpan, ebx
  mov TSmallBlockSpanHeader(ebx).PreviousPartiallyFreeSpan, edx

  {Unlock the small block manager if required}
  test cl, cl
  jz @DoNotUnlock1
  mov edx, TSmallBlockSpanHeader(eax).SmallBlockManager
  mov byte ptr TSmallBlockManager(edx).SmallBlockManagerLocked, 0
@DoNotUnlock1:

  {Clear the small block span flag in the header of the medium block.  This is important in case the block is ever
  reused and allocated blocks subsequently enumerated.}
  mov TMediumBlockHeader.IsSmallBlockSpan(eax - CMediumBlockHeaderSize), False

  {Free the span}
  pop ebx
  jmp FastMM_FreeMem_FreeMediumBlock
{$else}
var
  LPSmallBlockManager: PSmallBlockManager;
  LPPreviousSpan, LPNextSpan, LPInsertBeforeSpan: PSmallBlockSpanHeader;
  LOldFirstFreeBlock: Pointer;
begin
  {Decrement the number of allocated blocks}
  Dec(APSmallBlockSpan.BlocksInUse);
  {Is the entire span now free? -> Free it, unless debug mode is active.  BlocksInUse is set to the maximum that will
  fit in the span when the span is added as the sequential feed span, so this can only hit zero once all the blocks have
  been fed sequentially and subsequently freed.}
  if (APSmallBlockSpan.BlocksInUse <> 0) or (DebugModeCounter > 0) then
  begin
    LOldFirstFreeBlock := APSmallBlockSpan.FirstFreeBlock;

    {Mark the block as free, keeping the other flags (e.g. debug info) intact.}
    SetBlockIsFreeFlag(APSmallBlock, True);
    {Store the old first free block}
    PPointer(APSmallBlock)^ := LOldFirstFreeBlock;
    {Store this as the new first free block}
    APSmallBlockSpan.FirstFreeBlock := APSmallBlock;

    {Was the span previously full?}
    if LOldFirstFreeBlock = nil then
    begin
      {Insert this as the first partially free span for the block size}
      LPSmallBlockManager := APSmallBlockSpan.SmallBlockManager;
      LPInsertBeforeSpan := LPSmallBlockManager.FirstPartiallyFreeSpan;

      APSmallBlockSpan.PreviousPartiallyFreeSpan := PSmallBlockSpanHeader(LPSmallBlockManager);
      APSmallBlockSpan.NextPartiallyFreeSpan := LPInsertBeforeSpan;
      LPSmallBlockManager.FirstPartiallyFreeSpan := APSmallBlockSpan;
      LPInsertBeforeSpan.PreviousPartiallyFreeSpan := APSmallBlockSpan;
    end;

    {Unlock the small block manager if required}
    if AUnlockSmallBlockManager then
      APSmallBlockSpan.SmallBlockManager.SmallBlockManagerLocked := 0;
  end
  else
  begin
    {Remove this span from the circular linked list of partially free spans for the block type.}
    LPPreviousSpan := APSmallBlockSpan.PreviousPartiallyFreeSpan;
    LPNextSpan := APSmallBlockSpan.NextPartiallyFreeSpan;
    LPPreviousSpan.NextPartiallyFreeSpan := LPNextSpan;
    LPNextSpan.PreviousPartiallyFreeSpan := LPPreviousSpan;

    {Unlock the small block manager if required}
    if AUnlockSmallBlockManager then
      APSmallBlockSpan.SmallBlockManager.SmallBlockManagerLocked := 0;

    {Clear the small block span flag in the header of the medium block.  This is important in case the block is ever
    reused and allocated blocks subsequently enumerated.}
    SetMediumBlockHeader_SetIsSmallBlockSpan(APSmallBlockSpan, False);

    {It's not necessary to check nor update the sequential feed details, since BlocksInUse can only hit 0 after the
    sequential feed range has been exhausted and all the blocks subsequently freed.}

    {Free the block pool}
    FastMM_FreeMem_FreeMediumBlock(APSmallBlockSpan);
  end;

  Result := 0;
{$endif}
end;

{Frees a chain of blocks belonging to the small block manager.  The block manager is assumed to be locked.  Optionally
unlocks the block manager when done.  The first pointer inside each free block should be a pointer to the next free
block.  Returns 0 on success.}
function FastMM_FreeMem_FreeSmallBlockChain(APPendingFreeSmallBlock: Pointer;
  AUnlockSmallBlockManagerWhenDone: Boolean): Integer;
var
  LPNextBlock: Pointer;
  LPSmallBlockSpan: PSmallBlockSpanHeader;
begin
  while True do
  begin
    LPNextBlock := PPointer(APPendingFreeSmallBlock)^;

    LPSmallBlockSpan := GetSpanForSmallBlock(APPendingFreeSmallBlock);
    FastMM_FreeMem_FreeSmallBlock_ManagerAlreadyLocked(LPSmallBlockSpan, APPendingFreeSmallBlock,
      AUnlockSmallBlockManagerWhenDone and (LPNextBlock = nil));

    if LPNextBlock = nil then
      Break;

    APPendingFreeSmallBlock := LPNextBlock;
  end;

  Result := 0;
end;

{Returns a small block to the memory pool.  Returns 0 on success.}
function FastMM_FreeMem_FreeSmallBlock(APSmallBlock: Pointer): Integer; inline;
var
  LPSmallBlockSpan: PSmallBlockSpanHeader;
  LPSmallBlockManager: PSmallBlockManager;
  LOldFirstFreeBlock, LFirstPendingFreeBlock: Pointer;
begin
  LPSmallBlockSpan := GetSpanForSmallBlock(APSmallBlock);
  LPSmallBlockManager := LPSmallBlockSpan.SmallBlockManager;

  if AtomicCmpExchange(LPSmallBlockManager.SmallBlockManagerLocked, 1, 0) = 0 then
  begin

    {ARM requires a memory fence here.}

    if LPSmallBlockManager.PendingFreeList = nil then
    begin
      Result := FastMM_FreeMem_FreeSmallBlock_ManagerAlreadyLocked(LPSmallBlockSpan, APSmallBlock, True);
      Exit;
    end
    else
    begin
      FastMM_FreeMem_FreeSmallBlock_ManagerAlreadyLocked(LPSmallBlockSpan, APSmallBlock, False);

      {Process the pending frees list.}
      LFirstPendingFreeBlock := AtomicExchange(LPSmallBlockManager.PendingFreeList, nil);
      Result := FastMM_FreeMem_FreeSmallBlockChain(LFirstPendingFreeBlock, True);
    end;

  end
  else
  begin
    {The small block manager is currently locked, so we need to add this block to its pending free list.}
    while True do
    begin
      LOldFirstFreeBlock := LPSmallBlockManager.PendingFreeList;
      PPointer(APSmallBlock)^ := LOldFirstFreeBlock;
      if AtomicCmpExchange(LPSmallBlockManager.PendingFreeList, APSmallBlock, LOldFirstFreeBlock) = LOldFirstFreeBlock then
        Break;
    end;
    Result := 0;
  end;
end;

{Allocates a new sequential feed small block span and splits off the first block, returning it.  The small block
manager for the block size and arena is assumed to be locked, and will be unlocked before exit.  There may not be an
existing sequential feed span with available space.}
function FastMM_GetMem_GetSmallBlock_AllocateNewSequentialFeedSpanAndUnlockArena(APSmallBlockManager: PSmallBlockManager): Pointer;
{$ifdef X86ASM}
asm
  push ebx
  push esi

  {Small block manager in esi}
  mov esi, eax

  mov eax, TSmallBlockManager(esi).MinimumSpanSize
  mov edx, TSmallBlockManager(esi).OptimalSpanSize
  lea ecx, [edx + CSmallBlockSpanMaximumAmountWithWhichOptimalSizeMayBeExceeded]
  call FastMM_GetMem_GetMediumBlock
  test eax, eax
  jz @OutOfMemory

  {Save the span pointer in ebx}
  mov ebx, eax

  {Get the span size in eax}
  movzx eax, TMediumBlockHeader.MediumBlockSizeMultiple(eax - CMediumBlockHeaderSize)
  shl eax, CMediumBlockAlignmentBits

  {Calculate the number of small blocks that will fit inside the span.  We need to account for the span header,
  as well as the difference in the medium and small block header sizes for the last block.  All the sequential
  feed blocks are initially marked as used.  This implies that the sequential feed span can never be freed until
  all blocks have been fed sequentially.}
  sub eax, CSmallBlockSpanHeaderSize + CMediumBlockHeaderSize - CSmallBlockHeaderSize
  xor edx, edx
  movzx ecx, TSmallBlockManager(esi).BlockSize
  div ecx

  {Update the medium block header to indicate that this medium block serves as a small block span.}
  mov TMediumBlockHeader.IsSmallBlockSpan(ebx - CMediumBlockHeaderSize), True

  {Set up the block span.  Blocks that will eventually be fed sequentially are counted as in use.}
  mov TSmallBlockSpanHeader(ebx).SmallBlockManager, esi
  mov TSmallBlockSpanHeader(ebx).FirstFreeBlock, 0
  mov TSmallBlockSpanHeader(ebx).TotalBlocksInSpan, eax
  mov TSmallBlockSpanHeader(ebx).BlocksInUse, eax

  {This is the new sequential feed span.  This must be set before the offset is set.}
  mov TSmallBlockManager(esi).SequentialFeedSmallBlockSpan, ebx

  {Get the offset of the last block in eax}
  dec eax
  mul ecx
  add eax, CSmallBlockSpanHeaderSize

  {Set the span up for sequential block serving}
  mov TSmallBlockManager(esi).LastSmallBlockSequentialFeedOffset.IntegerValue, eax
  mov TSmallBlockManager(esi).SmallBlockManagerLocked, 0

  {Return the last block in the span}
  mov ecx, eax
  add eax, ebx

  {Set the header for the returned block.}
  shr ecx, CMediumBlockAlignmentBits
  lea ecx, [ecx * 8 + CIsSmallBlockFlag] //Low 3 bits are used by flags
  mov TSmallBlockHeader.BlockStatusFlagsAndSpanOffset(eax - CSmallBlockHeaderSize), cx

@Done:
  pop esi
  pop ebx
  ret
@OutOfMemory:
  mov TSmallBlockManager(esi).SmallBlockManagerLocked, 0
  pop esi
  pop ebx
{$else}
var
  LPSmallBlockSpan: PSmallBlockSpanHeader;
  LSpanSize, LLastBlockOffset, LTotalBlocksInSpan: Integer;
begin
  LPSmallBlockSpan := FastMM_GetMem_GetMediumBlock(APSmallBlockManager.MinimumSpanSize,
    APSmallBlockManager.OptimalSpanSize, APSmallBlockManager.OptimalSpanSize + CSmallBlockSpanMaximumAmountWithWhichOptimalSizeMayBeExceeded);

  {Handle "out of memory".}
  if LPSmallBlockSpan = nil then
  begin
    APSmallBlockManager.SmallBlockManagerLocked := 0;
    Exit(nil);
  end;

  {Update the medium block header to indicate that this medium block serves as a small block span.}
  SetMediumBlockHeader_SetIsSmallBlockSpan(LPSmallBlockSpan, True);

  LSpanSize := GetMediumBlockSize(LPSmallBlockSpan);

  {Set up the block span}
  LPSmallBlockSpan.SmallBlockManager := APSmallBlockManager;
  LPSmallBlockSpan.FirstFreeBlock := nil;
  {Set it as the sequential feed span.  This must be done before the sequential feed offset is set.}
  APSmallBlockManager.SequentialFeedSmallBlockSpan := LPSmallBlockSpan;
  {Calculate the number of small blocks that will fit inside the span.  We need to account for the span header, as well
  as the difference in the medium and small block header sizes for the last block.  All the sequential feed blocks are
  initially marked as used.  This implies that the sequential feed span can never be freed until all blocks have been
  fed sequentially.}
  LTotalBlocksInSpan := (LSpanSize - (CSmallBlockSpanHeaderSize + CMediumBlockHeaderSize - CSmallBlockHeaderSize))
    div APSmallBlockManager.BlockSize;
  LPSmallBlockSpan.TotalBlocksInSpan := LTotalBlocksInSpan;
  LPSmallBlockSpan.BlocksInUse := LTotalBlocksInSpan;

  {Memory fence required for ARM here.}

  {Set it up for sequential block serving}
  LLastBlockOffset := CSmallBlockSpanHeaderSize + APSmallBlockManager.BlockSize * (LTotalBlocksInSpan - 1);
  APSmallBlockManager.LastSmallBlockSequentialFeedOffset.IntegerValue := LLastBlockOffset;

  APSmallBlockManager.SmallBlockManagerLocked := 0;

  Result := PByte(LPSmallBlockSpan) + LLastBlockOffset;

  {Set the header for the returned block.}
  SetSmallBlockHeader(Result, LPSmallBlockSpan, False, False);
{$endif}
end;

{Attempts to split off a small block from the sequential feed span for the arena.  Returns the block on success, nil if
there is no available sequential feed block.  The arena does not have to be locked.}
function FastMM_GetMem_GetSmallBlock_TryGetBlockFromSequentialFeedSpan(APSmallBlockManager: PSmallBlockManager): Pointer;
{$ifndef PurePascal}
asm
{$ifdef X86ASM}
  {--------x86 Assembly language codepath---------}
  push ebx
  push esi
  push edi

  mov esi, eax
@TrySequentialFeedLoop:

  {Get the old ABA counter and offset in edx:eax}
  mov eax, TSmallBlockManager(esi).LastSmallBlockSequentialFeedOffset.IntegerValue
  mov edx, TSmallBlockManager(esi).LastSmallBlockSequentialFeedOffset.ABACounter

  {Get the new ABA counter and offset in ecx:ebx}
  movzx edi, TSmallBlockManager(esi).BlockSize
  mov ebx, eax
  sub ebx, edi
  lea ecx, [edx + 1]

  {Get the current sequential feed span in edi}
  mov edi, TSmallBlockManager(esi).SequentialFeedSmallBlockSpan

  cmp eax, CSmallBlockSpanHeaderSize
  jle @NoSequentialFeedAvailable

  {Try to grab the block.  If it fails, try again from the start.}
  lock cmpxchg8b TSmallBlockManager(esi).LastSmallBlockSequentialFeedOffset.IntegerAndABACounter
  jne @TrySequentialFeedLoop

  {The block address is the span + offset.}
  lea eax, [edi + ebx]

  {Set the header for the small block.}
  and ebx, -CMediumBlockAlignment
  shr ebx, CSmallBlockSpanOffsetBitShift
  or ebx, CIsSmallBlockFlag
  mov [eax - CSmallBlockHeaderSize], bx

  jmp @Done

@NoSequentialFeedAvailable:
  xor eax, eax
@Done:
  pop edi
  pop esi
  pop ebx
{$else}
  {--------x64 Assembly language codepath---------}
  .noframe

@TrySequentialFeedLoop:

  {Get the old ABA counter and offset in rax}
  mov rax, TSmallBlockManager(rcx).LastSmallBlockSequentialFeedOffset.IntegerAndABACounter

  {Get the new ABA counter and offset in rdx}
  movzx edx, TSmallBlockManager(rcx).BlockSize
  neg edx
  add rdx, rax

  {Get the current sequential feed span in r8}
  mov r8, TSmallBlockManager(rcx).SequentialFeedSmallBlockSpan

  cmp eax, CSmallBlockSpanHeaderSize
  jle @NoSequentialFeedAvailable

  {Try to grab the block.  If it fails, try again from the start.}
  lock cmpxchg TSmallBlockManager(rcx).LastSmallBlockSequentialFeedOffset.IntegerAndABACounter, rdx
  jne @TrySequentialFeedLoop

  {The block address is the span + offset.}
  mov edx, edx
  lea rax, [r8 + rdx]

  {Set the header for the small block.}
  and edx, -CMediumBlockAlignment
  shr edx, CSmallBlockSpanOffsetBitShift
  or edx, CIsSmallBlockFlag
  mov [rax - CSmallBlockHeaderSize], dx

  ret

@NoSequentialFeedAvailable:
  xor eax, eax
@Done:

{$endif}
{$else}
var
  LPreviousLastSequentialFeedBlockOffset, LNewLastSequentialFeedBlockOffset: TIntegerWithABACounter;
  LPSequentialFeedSpan: PSmallBlockSpanHeader;
begin
  while True do
  begin
    LPreviousLastSequentialFeedBlockOffset := APSmallBlockManager.LastSmallBlockSequentialFeedOffset;

    {Subtract the block size and increment the ABA counter to the new sequential feed offset.}
    LNewLastSequentialFeedBlockOffset.IntegerAndABACounter := LPreviousLastSequentialFeedBlockOffset.IntegerAndABACounter
      - APSmallBlockManager.BlockSize + (Int64(1) shl 32);

    LPSequentialFeedSpan := APSmallBlockManager.SequentialFeedSmallBlockSpan;

    if LPreviousLastSequentialFeedBlockOffset.IntegerValue <= CSmallBlockSpanHeaderSize then
      Exit(nil);

    if AtomicCmpExchange(APSmallBlockManager.LastSmallBlockSequentialFeedOffset.IntegerAndABACounter,
      LNewLastSequentialFeedBlockOffset.IntegerAndABACounter,
      LPreviousLastSequentialFeedBlockOffset.IntegerAndABACounter) = LPreviousLastSequentialFeedBlockOffset.IntegerAndABACounter then
    begin

      Result := @PByte(LPSequentialFeedSpan)[LNewLastSequentialFeedBlockOffset.IntegerValue];
      SetSmallBlockHeader(Result, LPSequentialFeedSpan, False, False);

      Exit;
    end;

  end;
{$endif}
end;

{Reuses a pending free block, freeing all pending free blocks other than the first.  On entry it is assumed that
APSmallBlockManager.PendingFreeList <> nil and that the arena is locked.  The arena will be unlocked before exit.}
function FastMM_GetMem_GetSmallBlock_ReusePendingFreeBlockAndUnlockArena(
  APSmallBlockManager: PSmallBlockManager): Pointer;
{$ifdef X86ASM}
asm
  push esi
  {Get the old pending free list pointer in esi}
  xor esi, esi
  xchg TSmallBlockManager(eax).PendingFreeList, esi
  {Get the next block in the chain in edx}
  mov edx, [esi]

  {Free all subsequent blocks in the chain, if there are any.}
  test edx, edx
  jz @NoNextPendingFree
  mov eax, edx
  mov dl, 1
  call FastMM_FreeMem_FreeSmallBlockChain
  jmp @CheckDebugInfo
@NoNextPendingFree:
  mov byte ptr TSmallBlockManager(eax).SmallBlockManagerLocked, 0

  {Does this block currently contain debug info?  If so, check the header and footer checksums as well as the debug
  fill pattern.}
@CheckDebugInfo:
  test word ptr [esi - CSmallBlockHeaderSize], CHasDebugInfoFlag
  jz @BlockHasNoDebugInfo
  mov eax, esi
  call CheckFreeDebugBlockIntact
  test al, al
  jnz @DebugBlockOK
  mov al, reInvalidPtr
  call System.Error
@DebugBlockOK:
  {Reset the debug info flag in the block.}
  and word ptr [esi - CSmallBlockHeaderSize], not CHasDebugInfoFlag
@BlockHasNoDebugInfo:

  {Return the first block in the pending free list}
  mov eax, esi
  pop esi
{$else}
var
  LPNextFreeBlock: Pointer;
begin
  Result := AtomicExchange(APSmallBlockManager.PendingFreeList, nil);

  LPNextFreeBlock := PPointer(Result)^;
  if LPNextFreeBlock <> nil then
    FastMM_FreeMem_FreeSmallBlockChain(LPNextFreeBlock, True)
  else
    APSmallBlockManager.SmallBlockManagerLocked := 0;

  {Does this block currently contain debug info?  If so, check the header and footer checksums as well as the debug
  fill pattern.}
  if BlockHasDebugInfo(Result) then
  begin
    if not CheckFreeDebugBlockIntact(Result) then
      System.Error(reInvalidPtr);

    {Reset the debug info flag in the block.}
    SetBlockHasDebugInfo(Result, False);
  end;
{$endif}
end;

{Returns the first free block and unlocks the small block manager.  On entry the manager must be locked and must be
known to have at least one free block.}
function FastMM_GetMem_GetSmallBlock_AllocateFreeBlockAndUnlockArena(APSmallBlockManager: PSmallBlockManager): Pointer;
{$ifdef X86ASM}
asm
  push esi
  mov esi, eax

  {ecx = first partially free span}
  mov ecx, TSmallBlockManager(eax).FirstPartiallyFreeSpan

  {Return the first free block in the span.}
  mov eax, TSmallBlockSpanHeader(ecx).FirstFreeBlock

  {Mark the block as in use.}
  and word ptr [eax - CSmallBlockHeaderSize], not CBlockIsFreeFlag

  {The current content of the block will be a pointer to the next free block in the span.}
  mov edx, [eax]
  mov TSmallBlockSpanHeader(ecx).FirstFreeBlock, edx

  {Increment the number of used blocks}
  add TSmallBlockSpanHeader(ecx).BlocksInUse, 1

  {If there are no more free blocks in the small block span then it must be removed from the circular linked list of
  small block spans with available blocks.}
  test edx, edx
  jnz @HasMoreFreeBlocks
  mov edx, TSmallBlockSpanHeader(ecx).NextPartiallyFreeSpan
  mov TSmallBlockManager(esi).FirstPartiallyFreeSpan, edx
  mov TSmallBlockSpanHeader(edx).PreviousPartiallyFreeSpan, esi
@HasMoreFreeBlocks:

  mov byte ptr TSmallBlockManager(esi).SmallBlockManagerLocked, 0

  {Does this block currently contain debug info?  If so, check the header and footer checksums as well as the debug
  fill pattern.}
  test word ptr [eax - CSmallBlockHeaderSize], CHasDebugInfoFlag
  jz @BlockHasNoDebugInfo
  push eax
  call CheckFreeDebugBlockIntact
  test al, al
  pop eax
  jnz @DebugBlockOK
  mov al, reInvalidPtr
  call System.Error
@DebugBlockOK:
  {Reset the debug info flag in the block.}
  and word ptr [eax - CSmallBlockHeaderSize], not CHasDebugInfoFlag
@BlockHasNoDebugInfo:

  pop esi
{$else}
var
  LPFirstPartiallyFreeSpan, LPNewFirstPartiallyFreeSpan: PSmallBlockSpanHeader;
  LPNextFreeBlock: Pointer;
begin
  LPFirstPartiallyFreeSpan := APSmallBlockManager.FirstPartiallyFreeSpan;

  {Return the first free block in the span.}
  Result := LPFirstPartiallyFreeSpan.FirstFreeBlock;

  {Mark the block as in use.}
  SetBlockIsFreeFlag(Result, False);

  {The current content of the first free block will be a pointer to the next free block in the span.}
  LPNextFreeBlock := PPointer(Result)^;
  LPFirstPartiallyFreeSpan.FirstFreeBlock := LPNextFreeBlock;

  {Increment the number of used blocks}
  Inc(LPFirstPartiallyFreeSpan.BlocksInUse);

  {If there are no more free blocks in the small block span then it must be removed from the circular linked list of
  small block spans with available blocks.}
  if LPNextFreeBlock = nil then
  begin
    LPNewFirstPartiallyFreeSpan := LPFirstPartiallyFreeSpan.NextPartiallyFreeSpan;
    APSmallBlockManager.FirstPartiallyFreeSpan := LPNewFirstPartiallyFreeSpan;
    LPNewFirstPartiallyFreeSpan.PreviousPartiallyFreeSpan := PSmallBlockSpanHeader(APSmallBlockManager);
  end;

  {ARM requires a data memory barrier here to ensure that all prior writes have completed before the arena is unlocked.}

  APSmallBlockManager.SmallBlockManagerLocked := 0;

  {Does this block currently contain debug info?  If so, check the header and footer checksums as well as the debug
  fill pattern.}
  if BlockHasDebugInfo(Result) then
  begin
    if not CheckFreeDebugBlockIntact(Result) then
    begin
      APSmallBlockManager.SmallBlockManagerLocked := 0;
      System.Error(reInvalidPtr);
    end;

    {Reset the debug info flag in the block.}
    SetBlockHasDebugInfo(Result, False);
  end;

{$endif}
end;

{Tries to allocate a small block through the given small block manager.  If the manager has no available blocks, or
it is locked, then the corresponding managers in other arenas are also tried.}
function FastMM_GetMem_GetSmallBlock(APSmallBlockManager: PSmallBlockManager): Pointer;
{$ifdef X86ASM}
asm
  {--------------Attempt 1--------------
  Try to get a block from the first arena with an available block.  During the first attempt only memory that has
  already been reserved for use by the block type will be used - no new spans will be allocated.

  Try to obtain a block in this sequence:
    1) The pending free list
    2) From a partially free span
    3) From the sequential feed span}

@Attempt1Loop:
  {Is this manager currently locked?}
  cmp byte ptr TSmallBlockManager(eax).SmallBlockManagerLocked, 0
  jne @Attempt1TrySequentialFeed

  {Is there a pending free block?}
  cmp TSmallBlockManager(eax).PendingFreeList, 0
  jne @Attempt1LockManagerAndTryGetBlock

  {Are there free blocks for this manager?}
  cmp TSmallBlockManager(eax).FirstPartiallyFreeSpan, eax
  je @Attempt1TrySequentialFeed

@Attempt1LockManagerAndTryGetBlock:
  {Try to lock the manager}
  mov cl, 1
  xchg byte ptr TSmallBlockManager(eax).SmallBlockManagerLocked, cl
  test cl, cl
  jnz @Attempt1TrySequentialFeed

  {1.1) Try to get a pending free block.  If there's no pending free block after locking the arena, try reusing a free
  block.}
  cmp TSmallBlockManager(eax).PendingFreeList, 0
  jne FastMM_GetMem_GetSmallBlock_ReusePendingFreeBlockAndUnlockArena

  {1.2) Try to get a block from the first free span.}
  cmp TSmallBlockManager(eax).FirstPartiallyFreeSpan, eax
  jne FastMM_GetMem_GetSmallBlock_AllocateFreeBlockAndUnlockArena

  {Other threads took all the available blocks before the manager could be locked.}
  mov byte ptr TSmallBlockManager(eax).SmallBlockManagerLocked, 0

@Attempt1TrySequentialFeed:
  {1.3) Could not reuse a free block nor a pending free block:  Try sequential feed.}
  cmp TSmallBlockManager(eax).LastSmallBlockSequentialFeedOffset.IntegerValue, CSmallBlockSpanHeaderSize
  jle @Attempt1NoSequentialFeedBlockAvailable
  push eax
  call FastMM_GetMem_GetSmallBlock_TryGetBlockFromSequentialFeedSpan
  pop edx
  test eax, eax
  jz @Attempt1SequentialFeedFailed
  ret
@Attempt1SequentialFeedFailed:
  mov eax, edx
@Attempt1NoSequentialFeedBlockAvailable:

  {Is this the last arena?  If not, try the next one.}
  cmp eax, offset SmallBlockManagers + CSmallBlockManagerSize * CSmallBlockTypeCount * (CFastMM_SmallBlockArenaCount - 1)
  jnb @Attempt1Failed
  add eax, CSmallBlockManagerSize * CSmallBlockTypeCount
  jmp @Attempt1Loop

@Attempt1Failed:
  sub eax, CSmallBlockManagerSize * CSmallBlockTypeCount * (CFastMM_SmallBlockArenaCount - 1)

  {--------------Attempt 2--------------
  Lock the first unlocked arena and try again.  During the second attempt a new sequential feed span will be allocated
  if there are no available blocks in the arena.

  Try to obtain a block in this sequence:
    1) The pending free list
    2) From a partially free span
    3) From the sequential feed span
    4) By allocating a new sequential feed span and splitting off a block from it}

@Attempt2Loop:

  {Try to lock the manager}
  mov edx, eax
  xor eax, eax
  mov cl, 1
  lock cmpxchg byte ptr TSmallBlockManager(edx).SmallBlockManagerLocked, cl
  mov eax, edx
  jne @Attempt2ManagerAlreadyLocked

  {2.1) Try to get a pending free block.  If there's no pending free block after locking the arena, try reusing a free
  block.}
  cmp TSmallBlockManager(eax).PendingFreeList, 0
  jne FastMM_GetMem_GetSmallBlock_ReusePendingFreeBlockAndUnlockArena

  {2.2) Try to get a block from the first free span.}
  cmp TSmallBlockManager(eax).FirstPartiallyFreeSpan, eax
  jne FastMM_GetMem_GetSmallBlock_AllocateFreeBlockAndUnlockArena

  {2.3) Could not reuse a free block nor a pending free block:  Try sequential feed.}
  cmp TSmallBlockManager(eax).LastSmallBlockSequentialFeedOffset.IntegerValue, CSmallBlockSpanHeaderSize
  jle @Attempt2NoSequentialFeedBlockAvailable
  push eax
  call FastMM_GetMem_GetSmallBlock_TryGetBlockFromSequentialFeedSpan
  pop edx
  test eax, eax
  jz @Attempt2SequentialFeedFailed
  mov byte ptr TSmallBlockManager(edx).SmallBlockManagerLocked, 0
  ret
@Attempt2SequentialFeedFailed:
  mov eax, edx
@Attempt2NoSequentialFeedBlockAvailable:

  {2.4) Allocate a new sequential feed span and split off a block}
  jmp FastMM_GetMem_GetSmallBlock_AllocateNewSequentialFeedSpanAndUnlockArena;

@Attempt2ManagerAlreadyLocked:
  {Is this the last arena?  If not, try the next one.}
  cmp eax, offset SmallBlockManagers + CSmallBlockManagerSize * CSmallBlockTypeCount * (CFastMM_SmallBlockArenaCount - 1)
  jnb @Attempt2Failed
  add eax, CSmallBlockManagerSize * CSmallBlockTypeCount
  jmp @Attempt2Loop

@Attempt2Failed:
  sub eax, CSmallBlockManagerSize * CSmallBlockTypeCount * (CFastMM_SmallBlockArenaCount - 1)

  {All arenas are currently locked:  Back off and start again at the first arena}
  push eax
  call LogSmallBlockThreadContentionAndYieldToOtherThread
  pop eax
  jmp @Attempt1Loop
{$else}
begin
  while True do
  begin

    {--------------Attempt 1--------------
    Try to get a block from the first arena with an available block.  During the first attempt only memory that has
    already been reserved for use by the block type will be used - no new spans will be allocated.

    Try to obtain a block in this sequence:
      1) The pending free list
      2) From a partially free span
      3) From the sequential feed span}

    {Walk the arenas for this small block type until we find an unlocked arena that can be used to obtain a block.}
    while True do
    begin

      {Atomic operations are very expensive, so in this first cycle through all the arenas we only try to lock the
      small block manager if there is a very high probability that the lock will be successful, and if it is successful
      that it will most likely have a block available.}
      if APSmallBlockManager.SmallBlockManagerLocked = 0 then
      begin

        {Before trying to lock the manager, first check whether it currently has either a non-empty pending free list or
        it has a partially free span.}
        if ((APSmallBlockManager.PendingFreeList <> nil)
            or (NativeInt(APSmallBlockManager.FirstPartiallyFreeSpan) <> NativeInt(APSmallBlockManager)))
          and (AtomicExchange(APSmallBlockManager.SmallBlockManagerLocked, 1) = 0) then
        begin

          {Try to reuse a pending free block first.}
          if APSmallBlockManager.PendingFreeList <> nil then
            Exit(FastMM_GetMem_GetSmallBlock_ReusePendingFreeBlockAndUnlockArena(APSmallBlockManager));

          {Try to allocate a block from the first partially free span.}
          if NativeInt(APSmallBlockManager.FirstPartiallyFreeSpan) <> NativeInt(APSmallBlockManager) then
            Exit(FastMM_GetMem_GetSmallBlock_AllocateFreeBlockAndUnlockArena(APSmallBlockManager));

          {Other threads must have taken all the available blocks before the manager could be locked.}
          APSmallBlockManager.SmallBlockManagerLocked := 0;
        end;
      end;

      {Try to split off a block from the sequential feed span (if there is one).  Splitting off a sequential feed block
      does not require the manager to be locked.}
      if APSmallBlockManager.LastSmallBlockSequentialFeedOffset.IntegerValue > CSmallBlockSpanHeaderSize then
      begin
        Result := FastMM_GetMem_GetSmallBlock_TryGetBlockFromSequentialFeedSpan(APSmallBlockManager);
        if Result <> nil then
          Exit;
      end;

      {Could not obtain a block from this arena:  Move on to the next arena.}
      if NativeUInt(APSmallBlockManager) < NativeUInt(@SmallBlockManagers[CFastMM_SmallBlockArenaCount - 1]) then
        Inc(APSmallBlockManager, CSmallBlockTypeCount)
      else
        Break;

    end;
    {Go back to the corresponding manager in the first arena}
    Dec(APSmallBlockManager, CSmallBlockTypeCount * (CFastMM_SmallBlockArenaCount - 1));

    {--------------Attempt 2--------------
    Lock the first unlocked arena and try again.  During the second attempt a new sequential feed span will be allocated
    if there are no available blocks in the arena.

    Try to obtain a block in this sequence:
      1) The pending free list
      2) From a partially free span
      3) From the sequential feed span
      4) By allocating a new sequential feed span and splitting off a block from it}

    while True do
    begin

      if AtomicCmpExchange(APSmallBlockManager.SmallBlockManagerLocked, 1, 0) = 0 then
      begin

        {Check if there is a pending free list.  If so the first pending free block is returned and the rest are freed.}
        if APSmallBlockManager.PendingFreeList <> nil then
          Exit(FastMM_GetMem_GetSmallBlock_ReusePendingFreeBlockAndUnlockArena(APSmallBlockManager));

        {Try to get a block from the first partially free span.}
        if NativeInt(APSmallBlockManager.FirstPartiallyFreeSpan) <> NativeInt(APSmallBlockManager) then
          Exit(FastMM_GetMem_GetSmallBlock_AllocateFreeBlockAndUnlockArena(APSmallBlockManager));

        {It's possible another thread could have allocated a new sequential feed span in the meantime, so we need to
        check again before allocating a new one.}
        if APSmallBlockManager.LastSmallBlockSequentialFeedOffset.IntegerValue > CSmallBlockSpanHeaderSize then
        begin
          Result := FastMM_GetMem_GetSmallBlock_TryGetBlockFromSequentialFeedSpan(APSmallBlockManager);
          if Result <> nil then
          begin
            APSmallBlockManager.SmallBlockManagerLocked := 0;
            Exit;
          end;
        end;

        {Allocate a new sequential feed span and split off a block from it}
        Exit(FastMM_GetMem_GetSmallBlock_AllocateNewSequentialFeedSpanAndUnlockArena(APSmallBlockManager));

      end;

      {Try the next small block arena}
      if NativeUInt(APSmallBlockManager) < NativeUInt(@SmallBlockManagers[CFastMM_SmallBlockArenaCount - 1]) then
        Inc(APSmallBlockManager, CSmallBlockTypeCount)
      else
        Break;
    end;
    Dec(APSmallBlockManager, CSmallBlockTypeCount * (CFastMM_SmallBlockArenaCount - 1));

    {--------------Back off--------------
    All arenas are currently locked:  Back off and start again at the first arena}

    LogSmallBlockThreadContentionAndYieldToOtherThread;

  end;
{$endif}
end;

function FastMM_ReallocMem_ReallocSmallBlock(APointer: Pointer; ANewUserSize: NativeInt): Pointer;
{$ifdef X86ASM}
asm
  {Get the span pointer in esi}
  movzx ecx, word ptr [eax - CBlockStatusFlagsSize]
  push esi
  and ecx, CDropSmallBlockFlagsMask
  shl ecx, CSmallBlockSpanOffsetBitShift
  mov esi, eax
  and esi, -CMediumBlockAlignment
  sub esi, ecx

  {Get the small block manager in esi}
  mov esi, TSmallBlockSpanHeader(esi).SmallBlockManager

  {Get the old available size in ecx}
  movzx ecx, TSmallBlockManager(esi).BlockSize
  sub ecx, CSmallBlockHeaderSize

  {Is it an upsize or downsize?}
  cmp ecx, edx
  jb @IsUpSize

  {It's a downsize.  Do we need to allocate a smaller block?  Only if the new block size is less than a quarter of the
  available size less SmallBlockDownsizeCheckAdder bytes.}
  lea esi, [edx * 4 + CSmallBlockDownsizeCheckAdder]
  cmp esi, ecx
  jae @Done2

  {Keep the old pointer in ebx}
  push ebx
  mov ebx, eax

  {Keep the new size in esi}
  mov esi, edx

  {Allocate the new block, move the old data across and then free the old block.}
  mov eax, edx
  call FastMM_GetMem
  test eax, eax
  jz @Done
  mov ecx, esi
  mov esi, eax
  mov edx, eax
  mov eax, ebx
  call System.Move
  mov eax, ebx
  call FastMM_FreeMem
  mov eax, esi
  jmp @Done

@IsUpSize:
  {Keep the old pointer in ebx}
  push ebx
  mov ebx, eax

  {This pointer is being reallocated to a larger block and therefore it is logical to assume that it may be enlarged
  again.  Since reallocations are expensive, there is a minimum upsize percentage to avoid unnecessary future move
  operations.}
  lea eax, [ecx + ecx + CSmallBlockUpsizeAdder]
  cmp eax, edx
  ja @GotNewSize
  mov eax, edx
@GotNewSize:

  {Allocate the new block, move the old data across and then free the old block.}
  call FastMM_GetMem
  test eax, eax
  jz @Done
  movzx ecx, TSmallBlockManager(esi).BlockSize
  sub ecx, CSmallBlockHeaderSize
  push eax
  mov edx, eax
  mov eax, ebx
  call TSmallBlockManager(esi).UpsizeMoveProcedure
  mov eax, ebx
  call FastMM_FreeMem
  pop eax

@Done:
  pop ebx
@Done2:
  pop esi
{$else}
var
  LPSmallBlockSpan: PSmallBlockSpanHeader;
  LPSmallBlockManager: PSmallBlockManager;
  LOldUserSize, LNewUserSize: NativeInt;
begin
  LPSmallBlockSpan := GetSpanForSmallBlock(APointer);

  LPSmallBlockManager := LPSmallBlockSpan.SmallBlockManager;

  {Get the available size inside blocks of this type.}
  LOldUserSize := LPSmallBlockManager.BlockSize - CSmallBlockHeaderSize;
  {Is it an upsize or a downsize?}
  if LOldUserSize >= ANewUserSize then
  begin
    {It's a downsize.  Do we need to allocate a smaller block?  Only if the new block size is less than a quarter of
    the available size less SmallBlockDownsizeCheckAdder bytes}
    if (ANewUserSize shl 2 + CSmallBlockDownsizeCheckAdder) >= LOldUserSize then
    begin
      {In-place downsize - return the pointer}
      Result := APointer;
      Exit;
    end
    else
    begin
      {Allocate a smaller block}
      Result := FastMM_GetMem(ANewUserSize);
      {Allocated OK?}
      if Result <> nil then
      begin
        {Move the data across}
        System.Move(APointer^, Result^, ANewUserSize);
        {Free the old pointer}
        FastMM_FreeMem(APointer);
      end;
    end;
  end
  else
  begin
    {This pointer is being reallocated to a larger block and therefore it is logical to assume that it may be enlarged
    again.  Since reallocations are expensive, there is a minimum upsize percentage to avoid unnecessary future move
    operations.}
    {Must grow with at least 100% + x bytes}
    LNewUserSize := LOldUserSize shl 1 + CSmallBlockUpsizeAdder;

    {Still not large enough?}
    if LNewUserSize < ANewUserSize then
      LNewUserSize := ANewUserSize;

    {Allocate the new block, move the old data across and then free the old block.}
    Result := FastMM_GetMem(LNewUserSize);
    if Result <> nil then
    begin
      LPSmallBlockManager.UpsizeMoveProcedure(APointer^, Result^, LOldUserSize);
      FastMM_FreeMem(APointer);
    end;

  end;
{$endif}
end;


{--------------------------------------------------------}
{-------Core memory manager interface: Normal mode-------}
{--------------------------------------------------------}

function FastMM_GetMem(ASize: NativeInt): Pointer;
{$ifndef PurePascal}
asm
{$ifdef X86ASM}
  {--------x86 Assembly language codepath---------}
  cmp eax, (CMaximumSmallBlockSize - CSmallBlockHeaderSize)
  ja @NotASmallBlock
  {Small block:  Get the small block manager index in eax}
  add eax, 1
  shr eax, CSmallBlockGranularityBits
  movzx eax, byte ptr SmallBlockTypeLookup[eax]
  {Get a pointer to the small block manager for arena 0 in eax}
  shl eax, CSmallBlockManagerSizeBits
  add eax, offset SmallBlockManagers
  jmp FastMM_GetMem_GetSmallBlock
@NotASmallBlock:
  cmp eax, (CMaximumMediumBlockSize - CMediumBlockHeaderSize)
  ja FastMM_GetMem_GetLargeBlock
  {Medium block:  Round the requested size up to the next medium block bin.}
  cmp eax, (CMediumBlockMiddleBinsStart - CMediumBlockHeaderSize)
  ja @NotFirstMediumBlockGroup
  add eax, (CMediumBlockHeaderSize - CMinimumMediumBlockSize + CInitialBinSpacing - 1)
  and eax, -CInitialBinSpacing
  add eax, CMinimumMediumBlockSize
  mov edx, eax
  mov ecx, eax
  jmp FastMM_GetMem_GetMediumBlock
@NotFirstMediumBlockGroup:
  cmp eax, (CMediumBlockFinalBinsStart - CMediumBlockHeaderSize)
  ja @LastMediumBlockGroup
  add eax, (CMediumBlockHeaderSize - CMediumBlockMiddleBinsStart + CMiddleBinSpacing - 1)
  and eax, -CMiddleBinSpacing
  add eax, CMediumBlockMiddleBinsStart
  mov edx, eax
  mov ecx, eax
  jmp FastMM_GetMem_GetMediumBlock
@LastMediumBlockGroup:
  add eax, (CMediumBlockHeaderSize - CMediumBlockFinalBinsStart + CFinalBinSpacing - 1)
  and eax, -CFinalBinSpacing
  add eax, CMediumBlockFinalBinsStart
  mov edx, eax
  mov ecx, eax
  jmp FastMM_GetMem_GetMediumBlock
{$else}
  .noframe
  {--------x64 Assembly language codepath---------}
  cmp rcx, (CMaximumSmallBlockSize - CSmallBlockHeaderSize)
  ja @NotASmallBlock
  {Small block:  Get the small block manager index in ecx}
  add ecx, 1
  shr ecx, CSmallBlockGranularityBits
  lea rdx, SmallBlockTypeLookup
  movzx ecx, byte ptr [rdx + rcx]
  {Get a pointer to the small block manager for arena 0 in rcx}
  shl rcx, CSmallBlockManagerSizeBits
  lea rdx, SmallBlockManagers
  add rcx, rdx
  jmp FastMM_GetMem_GetSmallBlock
@NotASmallBlock:
  cmp rcx, (CMaximumMediumBlockSize - CMediumBlockHeaderSize)
  ja FastMM_GetMem_GetLargeBlock
  {Medium block:  Round the requested size up to the next medium block bin.}
  cmp ecx, (CMediumBlockMiddleBinsStart - CMediumBlockHeaderSize)
  ja @NotFirstMediumBlockGroup
  add ecx, (CMediumBlockHeaderSize - CMinimumMediumBlockSize + CInitialBinSpacing - 1)
  and ecx, -CInitialBinSpacing
  add ecx, CMinimumMediumBlockSize
  mov edx, ecx
  mov r8d, ecx
  jmp FastMM_GetMem_GetMediumBlock
@NotFirstMediumBlockGroup:
  cmp ecx, (CMediumBlockFinalBinsStart - CMediumBlockHeaderSize)
  ja @LastMediumBlockGroup
  add ecx, (CMediumBlockHeaderSize - CMediumBlockMiddleBinsStart + CMiddleBinSpacing - 1)
  and ecx, -CMiddleBinSpacing
  add ecx, CMediumBlockMiddleBinsStart
  mov edx, ecx
  mov r8d, ecx
  jmp FastMM_GetMem_GetMediumBlock
@LastMediumBlockGroup:
  add ecx, (CMediumBlockHeaderSize - CMediumBlockFinalBinsStart + CFinalBinSpacing - 1)
  and ecx, -CFinalBinSpacing
  add ecx, CMediumBlockFinalBinsStart
  mov edx, ecx
  mov r8d, ecx
  jmp FastMM_GetMem_GetMediumBlock
{$endif}
{$else}
var
  LPSmallBlockManager: PSmallBlockManager;
  LSmallBlockTypeIndex: Integer;
begin
  {Is it a small block allocation request?}
  if NativeUInt(ASize) <= (CMaximumSmallBlockSize - CSmallBlockHeaderSize) then
  begin
    {Convert the size to a pointer to the corresponding manager in the first arena.}
    LSmallBlockTypeIndex := SmallBlockTypeLookup[(NativeUInt(ASize) + (CSmallBlockHeaderSize - 1)) shr CSmallBlockGranularityBits];
    LPSmallBlockManager := @SmallBlockManagers[0][LSmallBlockTypeIndex];
    Result := FastMM_GetMem_GetSmallBlock(LPSmallBlockManager);
  end
  else
  begin
    {Medium or large block.}
    if NativeUInt(ASize) <= (CMaximumMediumBlockSize - CMediumBlockHeaderSize) then
    begin
      {Add the size of the block header and round up to an exact bin size}
      ASize := RoundUserSizeUpToNextMediumBlockBin(ASize);
      Result := FastMM_GetMem_GetMediumBlock(ASize, ASize, ASize);
    end
    else
    begin
      Result := FastMM_GetMem_GetLargeBlock(ASize);
    end;
  end;
{$endif}
end;

function FastMM_FreeMem(APointer: Pointer): Integer;
{$ifndef PurePascal}
asm
{$ifdef X86ASM}

  {--------x86 Assembly language codepath---------}

  {Read the flags from the block header.}
  movzx edx, word ptr [eax - CBlockStatusFlagsSize]

  {Is it a small block that is in use?}
  mov ecx, (CBlockIsFreeFlag or CIsSmallBlockFlag)
  and ecx, edx
  cmp ecx, CIsSmallBlockFlag
  jne @NotASmallBlock

  {----Start: Inline of FastMM_FreeMem_FreeSmallBlock----}
  {Get the span pointer in ecx}
  and edx, CDropSmallBlockFlagsMask
  shl edx, CSmallBlockSpanOffsetBitShift
  mov ecx, eax
  and ecx, -CMediumBlockAlignment
  sub ecx, edx

  {Get the small block manager in esi}
  push esi
  mov esi, TSmallBlockSpanHeader(ecx).SmallBlockManager

  {Get the block pointer in edx}
  mov edx, eax

  mov eax, $100
  lock cmpxchg byte ptr TSmallBlockManager(esi).SmallBlockManagerLocked, ah
  jne @ManagerCurrentlyLocked

  {Get the span in eax}
  mov eax, ecx

  cmp TSmallBlockManager(esi).PendingFreeList, 0
  jne @HasPendingFreeList

  {No pending free list:  Just free this block and unlock the block manager.}
  pop esi
  mov cl, 1
  jmp FastMM_FreeMem_FreeSmallBlock_ManagerAlreadyLocked

@HasPendingFreeList:
  xor ecx, ecx
  call FastMM_FreeMem_FreeSmallBlock_ManagerAlreadyLocked

  {Unlink the current pending free list}
  xor eax, eax
  xchg TSmallBlockManager(esi).PendingFreeList, eax

  {Process the pending free list.}
  pop esi
  mov dl, 1
  jmp FastMM_FreeMem_FreeSmallBlockChain

  {The small block manager is currently locked, so we need to add this block to its pending free list.}
@ManagerCurrentlyLocked:
  mov eax, TSmallBlockManager(esi).PendingFreeList
  mov [edx], eax
  lock cmpxchg TSmallBlockManager(esi).PendingFreeList, edx
  jne @ManagerCurrentlyLocked

  xor eax, eax
  pop esi
  ret
  {----End: Inline of FastMM_FreeMem_FreeSmallBlock----}

@NotASmallBlock:
  mov ecx, (not CHasDebugInfoFlag)
  and ecx, edx
  cmp ecx, CIsMediumBlockFlag
  je FastMM_FreeMem_FreeMediumBlock

  cmp ecx, CIsLargeBlockFlag
  je FastMM_FreeMem_FreeLargeBlock

  cmp edx, CIsDebugBlockFlag
  je FastMM_FreeMem_FreeDebugBlock

  xor edx,edx
  jmp HandleInvalidFreeMemOrReallocMem
{$else}

  {--------x64 Assembly language codepath---------}

  .params 3
  .pushnv rsi

  {Read the flags from the block header.}
  movzx eax, word ptr [rcx - CBlockStatusFlagsSize]

  {Is it a small block that is in use?}
  mov edx, (CBlockIsFreeFlag or CIsSmallBlockFlag)
  and edx, eax
  cmp edx, CIsSmallBlockFlag
  jne @NotASmallBlock

  {----Start: Inline of FastMM_FreeMem_FreeSmallBlock----}

  {Get the block pointer in rdx}
  mov rdx, rcx

  {Get the span pointer in rcx}
  and eax, CDropSmallBlockFlagsMask
  shl eax, CSmallBlockSpanOffsetBitShift
  and rcx, -CMediumBlockAlignment
  sub rcx, rax

  {Get the small block manager in rsi}
  mov rsi, TSmallBlockSpanHeader(rcx).SmallBlockManager

  mov eax, $100
  lock cmpxchg byte ptr TSmallBlockManager(rsi).SmallBlockManagerLocked, ah
  jne @ManagerCurrentlyLocked

  cmp TSmallBlockManager(rsi).PendingFreeList, 0
  jne @HasPendingFreeList

  {No pending free list:  Just free this block and unlock the block manager.}
  add rsp, $28
  pop rsi
  pop rbp
  mov r8b, 1
  jmp FastMM_FreeMem_FreeSmallBlock_ManagerAlreadyLocked

@HasPendingFreeList:
  xor r8d, r8d
  call FastMM_FreeMem_FreeSmallBlock_ManagerAlreadyLocked

  {Unlink the current pending free list}
  xor ecx, ecx
  xchg TSmallBlockManager(rsi).PendingFreeList, rcx

  {Process the pending free list.}
  add rsp, $28
  pop rsi
  pop rbp
  mov dl, 1
  jmp FastMM_FreeMem_FreeSmallBlockChain

  {The small block manager is currently locked, so we need to add this block to its pending free list.}
@ManagerCurrentlyLocked:
  mov rax, TSmallBlockManager(rsi).PendingFreeList
  mov [rdx], rax
  lock cmpxchg TSmallBlockManager(rsi).PendingFreeList, rdx
  jne @ManagerCurrentlyLocked

  xor eax, eax
  jmp @Done
  {----End: Inline of FastMM_FreeMem_FreeSmallBlock----}

@NotASmallBlock:
  add rsp, $28
  pop rsi
  pop rbp

  mov edx, (not CHasDebugInfoFlag)
  and edx, eax
  cmp edx, CIsMediumBlockFlag
  je FastMM_FreeMem_FreeMediumBlock

  cmp edx, CIsLargeBlockFlag
  je FastMM_FreeMem_FreeLargeBlock
  cmp eax, CIsDebugBlockFlag
  je FastMM_FreeMem_FreeDebugBlock
  xor edx,edx
  jmp HandleInvalidFreeMemOrReallocMem
@Done:

{$endif}
{$else}
var
  LBlockHeader: Integer;
begin
  {Read the flags from the block header.}
  LBlockHeader := PBlockStatusFlags(APointer)[-1];

  {Is it a small block that is in use?}
  if LBlockHeader and (CBlockIsFreeFlag or CIsSmallBlockFlag) = CIsSmallBlockFlag then
  begin
    Result := FastMM_FreeMem_FreeSmallBlock(APointer);
  end
  else
  begin
    if LBlockHeader and (not CHasDebugInfoFlag) = CIsMediumBlockFlag then
    begin
      Result := FastMM_FreeMem_FreeMediumBlock(APointer);
    end
    else
    begin
      if LBlockHeader and (not CHasDebugInfoFlag) = CIsLargeBlockFlag then
      begin
        Result := FastMM_FreeMem_FreeLargeBlock(APointer);
      end
      else
      begin
        if LBlockHeader = CIsDebugBlockFlag then
        begin
          Result := FastMM_FreeMem_FreeDebugBlock(APointer);
        end
        else
        begin
          Result := HandleInvalidFreeMemOrReallocMem(APointer, False);
        end;
      end;
    end;
  end;
{$endif}
end;

function FastMM_ReallocMem(APointer: Pointer; ANewSize: NativeInt): Pointer;
{$ifndef PurePascal}
asm
{$ifdef X86ASM}
  {--------x86 Assembly language codepath---------}

  {Is it a small block that is in use?}
  movzx ecx, word ptr [eax - CBlockStatusFlagsSize]
  and ecx, (CBlockIsFreeFlag or CIsSmallBlockFlag)
  cmp ecx, CIsSmallBlockFlag
  je FastMM_ReallocMem_ReallocSmallBlock

  movzx ecx, word ptr [eax - CBlockStatusFlagsSize]
  and ecx, (not CHasDebugInfoFlag)
  cmp ecx, CIsMediumBlockFlag
  je FastMM_ReallocMem_ReallocMediumBlock

  cmp ecx, CIsLargeBlockFlag
  je FastMM_ReallocMem_ReallocLargeBlock

  cmp word ptr [eax - CBlockStatusFlagsSize], CIsDebugBlockFlag
  je FastMM_ReallocMem_ReallocDebugBlock

  xor edx,edx
  jmp HandleInvalidFreeMemOrReallocMem
{$else}
  {--------x64 Assembly language codepath---------}
  .noframe
  {Get the block flags in r8}
  movzx r8d, word ptr [rcx - CBlockStatusFlagsSize]

  {Is it a small block that is in use?}
  mov eax, r8d
  and eax, (CBlockIsFreeFlag or CIsSmallBlockFlag)
  cmp eax, CIsSmallBlockFlag
  je FastMM_ReallocMem_ReallocSmallBlock

  mov eax, r8d
  and eax, (not CHasDebugInfoFlag)
  cmp eax, CIsMediumBlockFlag
  je FastMM_ReallocMem_ReallocMediumBlock

  cmp eax, CIsLargeBlockFlag
  je FastMM_ReallocMem_ReallocLargeBlock

  cmp r8d, CIsDebugBlockFlag
  je FastMM_ReallocMem_ReallocDebugBlock

  xor edx,edx
  jmp HandleInvalidFreeMemOrReallocMem

{$endif}
{$else}
var
  LBlockHeader: Integer;
begin
  {Read the flags from the block header.}
  LBlockHeader := PBlockStatusFlags(APointer)[-1];

  {Is it a small block that is in use?}
  if LBlockHeader and (CBlockIsFreeFlag or CIsSmallBlockFlag) = CIsSmallBlockFlag then
  begin
    Result := FastMM_ReallocMem_ReallocSmallBlock(APointer, ANewSize);
  end
  else
  begin
    {Is this a medium block in use?}
    if LBlockHeader and (not CHasDebugInfoFlag) = CIsMediumBlockFlag then
    begin
      Result := FastMM_ReallocMem_ReallocMediumBlock(APointer, ANewSize);
    end
    else
    begin
      if LBlockHeader and (not CHasDebugInfoFlag) = CIsLargeBlockFlag then
      begin
        Result := FastMM_ReallocMem_ReallocLargeBlock(APointer, ANewSize);
      end
      else
      begin
        if LBlockHeader = CIsDebugBlockFlag then
        begin
          Result := FastMM_ReallocMem_ReallocDebugBlock(APointer, ANewSize)
        end
        else
        begin
          HandleInvalidFreeMemOrReallocMem(APointer, True);
          Result := nil;
        end;
      end;

    end;
  end;
{$endif}
end;

function FastMM_AllocMem(ASize: NativeInt): Pointer;
begin
  Result := FastMM_GetMem(ASize);
  {Large blocks are already zero filled}
  if (Result <> nil) and (ASize <= (CMaximumMediumBlockSize - CMediumBlockHeaderSize)) then
    FillChar(Result^, ASize, 0);
end;


{--------------------------------------------------------}
{-------Core memory manager interface: Debug mode--------}
{--------------------------------------------------------}

function FastMM_DebugGetMem_GetDebugBlock(ASize: NativeInt; AFillBlockWithDebugPattern: Boolean): Pointer;
begin
  Result := FastMM_GetMem(ASize + (CDebugBlockHeaderSize + CDebugBlockFooterSize));
  if Result = nil then
    Exit;

  {Populate the debug header and set the header and footer checksums.}
  PFastMM_DebugBlockHeader(Result).UserSize := ASize;
  PFastMM_DebugBlockHeader(Result).PreviouslyUsedByClass := nil;
  FastMM_GetStackTrace(@PFastMM_DebugBlockHeader(Result).AllocationStackTrace, CFastMM_StackTraceEntryCount,
    CFastMM_StackTrace_SkipFrames_GetMem);
  PFastMM_DebugBlockHeader(Result).FreeStackTrace := Default(TFastMM_StackTrace);
  PFastMM_DebugBlockHeader(Result).AllocationGroup := FastMM_CurrentAllocationGroup;
  PFastMM_DebugBlockHeader(Result).AllocationNumber := AtomicIncrement(FastMM_LastAllocationNumber);
  PFastMM_DebugBlockHeader(Result).AllocatedByThread := OS_GetCurrentThreadID;
  PFastMM_DebugBlockHeader(Result).FreedByThread := 0;
  PFastMM_DebugBlockHeader(Result).DebugBlockFlags := CIsDebugBlockFlag;
  SetDebugBlockHeaderAndFooterChecksums(Result);

  {Fill the block with the debug pattern}
  if AFillBlockWithDebugPattern then
    FillDebugBlockWithDebugPattern(Result);

  {Set the flag in the actual block header to indicate that the block contains debug information.}
  SetBlockHasDebugInfo(Result, True);

  {Return a pointer to the user data}
  Inc(PByte(Result), CDebugBlockHeaderSize);
end;

function FastMM_DebugGetMem(ASize: NativeInt): Pointer;
begin
  Result := FastMM_DebugGetMem_GetDebugBlock(ASize, True);
end;

function FastMM_DebugFreeMem(APointer: Pointer): Integer;
begin
  Result := FastMM_FreeMem(APointer);
end;

function FastMM_DebugReallocMem(APointer: Pointer; ANewSize: NativeInt): Pointer;
var
  LBlockHeader: Integer;
  LMoveCount: NativeInt;
begin
  {Read the flags from the block header.}
  LBlockHeader := PBlockStatusFlags(APointer)[-1];

  if LBlockHeader = CIsDebugBlockFlag then
  begin
    Result := FastMM_ReallocMem_ReallocDebugBlock(APointer, ANewSize);
  end
  else
  begin
    {Catch an attempt to reallocate a freed block.}
    if LBlockHeader and CBlockIsFreeFlag <> 0 then
    begin
      HandleInvalidFreeMemOrReallocMem(APointer, True);
      Exit(nil);
    end;

    {The old block is not a debug block, so we need to allocate a new debug block and copy the data across.}
    Result := FastMM_DebugGetMem_GetDebugBlock(ANewSize, False);

    if Result <> nil then
    begin
      {Determine the used user size of the old block and move the lesser of the old and new sizes, and then free the
      old block.}
      LMoveCount := FastMM_BlockCurrentUserBytes(APointer);
      if LMoveCount > ANewSize then
        LMoveCount := ANewSize;
      System.Move(APointer^, Result^, LMoveCount);

      FastMM_FreeMem(APointer);
    end;
  end;

end;

function FastMM_DebugAllocMem(ASize: NativeInt): Pointer;
begin
  Result := FastMM_DebugGetMem_GetDebugBlock(ASize, False);
  {Large blocks are already zero filled}
  if (Result <> nil) and (ASize <= (CMaximumMediumBlockSize - CMediumBlockHeaderSize - CDebugBlockHeaderSize - CDebugBlockFooterSize)) then
    FillChar(Result^, ASize, 0);
end;

procedure FastMM_NoOpGetStackTrace(APReturnAddresses: PNativeUInt; AMaxDepth, ASkipFrames: Cardinal);
var
  i: Integer;
begin
  for i := 1 to AMaxDepth do
  begin
    APReturnAddresses^ := 0;
    Inc(APReturnAddresses);
  end;
end;

function FastMM_NoOpConvertStackTraceToText(APReturnAddresses: PNativeUInt; AMaxDepth: Cardinal;
  APBufferPosition, APBufferEnd: PWideChar): PWideChar;
begin
  {Nothing to do.}
  Result := APBufferPosition;
end;

function FastMM_DebugLibrary_LegacyLogStackTrace_Wrapper(APReturnAddresses: PNativeUInt; AMaxDepth: Cardinal;
  APBufferPosition, APBufferEnd: PWideChar): PWideChar;
var
  LAnsiBuffer: array[0..CFastMM_StackTraceEntryCount * 256] of AnsiChar;
  LPEnd, LPCurPos: PAnsiChar;
begin
  Result := APBufferPosition;

  LPEnd := DebugLibrary_LogStackTrace_Legacy(APReturnAddresses, AMaxDepth, @LAnsiBuffer);
  LPCurPos := @LAnsiBuffer;
  while (LPCurPos < LPEnd)
    and (Result < APBufferEnd) do
  begin
    Result^ := WideChar(LPCurPos^); //Assume it is Latin-1 text
    Inc(Result);
    Inc(LPCurPos);
  end;
end;

{--------------------------------------------------------}
{----------------------Diagnostics-----------------------}
{--------------------------------------------------------}

{Returns the user size of the block, i.e. the number of bytes in use by the application.}
function FastMM_BlockCurrentUserBytes(APointer: Pointer): NativeInt;
var
  LBlockHeader: Integer;
  LPSmallBlockSpan: PSmallBlockSpanHeader;
begin
  {Read the flags from the block header.}
  LBlockHeader := PBlockStatusFlags(APointer)[-1];
  {Is it a small block that is in use?}
  if LBlockHeader and CIsSmallBlockFlag = CIsSmallBlockFlag then
  begin
    LPSmallBlockSpan := GetSpanForSmallBlock(APointer);
    Result := LPSmallBlockSpan.SmallBlockManager.BlockSize - CSmallBlockHeaderSize;
  end
  else
  begin
    if LBlockHeader and CIsMediumBlockFlag = CIsMediumBlockFlag then
    begin
      Result := GetMediumBlockSize(APointer) - CMediumBlockHeaderSize;
    end
    else
    begin
      if LBlockHeader and CIsLargeBlockFlag = CIsLargeBlockFlag then
      begin
        Result := PLargeBlockHeader(APointer)[-1].UserAllocatedSize;
      end
      else
      begin
        if LBlockHeader and CIsDebugBlockFlag = CIsDebugBlockFlag then
        begin
          Result := PFastMM_DebugBlockHeader(APointer)[-1].UserSize;
        end
        else
        begin
          System.Error(reInvalidPtr);
          Result := 0;
        end;
      end;
    end;
  end;

end;

{Returns the available user size of the block, i.e. the block size less any headers and footers.}
function FastMM_BlockMaximumUserBytes(APointer: Pointer): NativeInt;
var
  LBlockHeader: Integer;
  LPSmallBlockSpan: PSmallBlockSpanHeader;
begin
  {Read the flags from the block header.}
  LBlockHeader := PBlockStatusFlags(APointer)[-1];
  {Is it a small block?}
  if LBlockHeader and CIsSmallBlockFlag = CIsSmallBlockFlag then
  begin
    LPSmallBlockSpan := GetSpanForSmallBlock(APointer);

    Result := LPSmallBlockSpan.SmallBlockManager.BlockSize - CSmallBlockHeaderSize;
  end
  else
  begin
    if LBlockHeader and CIsMediumBlockFlag = CIsMediumBlockFlag then
    begin
      Result := GetMediumBlockSize(APointer) - CMediumBlockHeaderSize;
    end
    else
    begin
      if LBlockHeader and CIsLargeBlockFlag = CIsLargeBlockFlag then
      begin
        Result := PLargeBlockHeader(APointer)[-1].ActualBlockSize - CLargeBlockHeaderSize;
      end
      else
      begin
        if LBlockHeader and CIsDebugBlockFlag = CIsDebugBlockFlag then
        begin
          Result := PFastMM_DebugBlockHeader(APointer)[-1].UserSize;
        end
        else
        begin
          System.Error(reInvalidPtr);
          Result := 0;
        end;
      end;
    end;
  end;

end;

function FastMM_ProcessAllPendingFrees: Boolean;
var
  LArenaIndex, LBlockTypeIndex: Integer;
  LPSmallBlockManager: PSmallBlockManager;
  LPPendingFreeBlock: Pointer;
  LPMediumBlockManager: PMediumBlockManager;
  LPLargeBlockManager: PLargeBlockManager;
begin
  {Assume success, until proven otherwise.}
  Result := True;

  {-------Small blocks-------}
  for LArenaIndex := 0 to CFastMM_SmallBlockArenaCount - 1 do
  begin
    LPSmallBlockManager := @SmallBlockManagers[LArenaIndex][0];

    for LBlockTypeIndex := 0 to CSmallBlockTypeCount - 1 do
    begin

      if LPSmallBlockManager.PendingFreeList <> nil then
      begin
        if AtomicCmpExchange(LPSmallBlockManager.SmallBlockManagerLocked, 1, 0) = 0 then
        begin
          {Process the pending frees list.}
          LPPendingFreeBlock := AtomicExchange(LPSmallBlockManager.PendingFreeList, nil);
          if LPPendingFreeBlock <> nil then
            FastMM_FreeMem_FreeSmallBlockChain(LPPendingFreeBlock, True)
          else
            LPSmallBlockManager.SmallBlockManagerLocked := 0;
        end
        else
        begin
          {The small block manager has pending frees, but could not be locked.}
          Result := False;
        end;
      end;

      Inc(LPSmallBlockManager);
    end;
  end;

  {-------Medium blocks-------}
  LPMediumBlockManager := @MediumBlockManagers[0];
  for LArenaIndex := 0 to CFastMM_MediumBlockArenaCount - 1 do
  begin

    if LPMediumBlockManager.PendingFreeList <> nil then
    begin
      if AtomicCmpExchange(LPMediumBlockManager.MediumBlockManagerLocked, 1, 0) = 0 then
      begin
        {Process the pending frees list.}
        LPPendingFreeBlock := AtomicExchange(LPMediumBlockManager.PendingFreeList, nil);
        if LPPendingFreeBlock <> nil then
          FastMM_FreeMem_FreeMediumBlockChain(LPMediumBlockManager, LPPendingFreeBlock, True)
        else
          LPMediumBlockManager.MediumBlockManagerLocked := 0;
      end
      else
      begin
        {The medium block manager has pending frees, but could not be locked.}
        Result := False;
      end;
    end;

    Inc(LPMediumBlockManager);
  end;

  {-------Large blocks-------}
  LPLargeBlockManager := @LargeBlockManagers[0];
  for LArenaIndex := 0 to CFastMM_LargeBlockArenaCount - 1 do
  begin

    if LPLargeBlockManager.PendingFreeList <> nil then
    begin
      if AtomicCmpExchange(LPLargeBlockManager.LargeBlockManagerLocked, 1, 0) = 0 then
      begin
        if ProcessLargeBlockPendingFrees_ArenaAlreadyLocked(LPLargeBlockManager) <> 0 then
          System.Error(reInvalidPtr);
      end
      else
      begin
        {The large block manager has pending frees, but could not be locked.}
        Result := False;
      end;
    end;

    Inc(LPLargeBlockManager);
  end;

end;

{Adjusts the block information for blocks that contain a debug mode sub-block.}
procedure FastMM_WalkBlocks_AdjustForDebugSubBlock(var ABlockInfo: TFastMM_WalkAllocatedBlocks_BlockInfo); inline;
begin
  if BlockHasDebugInfo(ABlockInfo.BlockAddress) then
  begin
    ABlockInfo.DebugInformation := ABlockInfo.BlockAddress;
    ABlockInfo.UsableSize := ABlockInfo.DebugInformation.UserSize;
    Inc(PByte(ABlockInfo.BlockAddress), CDebugBlockHeaderSize);
  end
  else
    ABlockInfo.DebugInformation := nil;
end;

{Checks for timeout while waiting on a locked resource.  Returns False if the timeout has expired.}
function FastMM_WalkBlocks_CheckTimeout(var ALockWaitTimeMilliseconds, APreviousTimestampMilliseconds: Cardinal;
  ALockTimeoutMilliseconds: Cardinal): Boolean;
var
  LCurrentTimestampMilliseconds: Cardinal;
begin
  LCurrentTimestampMilliseconds := OS_GetMillisecondsSinceStartup;

  {On the first pass just record the current timestamp.}
  if ALockWaitTimeMilliseconds = 0 then
  begin
    ALockWaitTimeMilliseconds := 1;
  end
  else
  begin
    {Update the total number of milliseconds that have elapsed.}
    Inc(ALockWaitTimeMilliseconds, LCurrentTimestampMilliseconds - APreviousTimestampMilliseconds);
  end;

  APreviousTimestampMilliseconds := LCurrentTimestampMilliseconds;

  {If the lock timeout has expired, return False.}
  Result := ALockWaitTimeMilliseconds <= ALockTimeoutMilliseconds;
end;

{Walks the block types indicated by the AWalkBlockTypes set, calling ACallBack for each allocated block.}
function FastMM_WalkBlocks(ACallBack: TFastMM_WalkBlocksCallback; AWalkBlockTypes: TFastMM_WalkBlocksBlockTypes;
  AWalkUsedBlocksOnly: Boolean; AUserData: Pointer; ALockTimeoutMilliseconds: Cardinal): Boolean;
var
  LArenaIndex: Integer;
  LLockWaitTimeMilliseconds, LTimestampMilliseconds: Cardinal;
  LBlockInfo: TFastMM_WalkAllocatedBlocks_BlockInfo;
  LPLargeBlockManager: PLargeBlockManager;
  LPLargeBlockHeader: PLargeBlockHeader;
  LPMediumBlockManager: PMediumBlockManager;
  LPMediumBlockSpan: PMediumBlockSpanHeader;
  LPMediumBlock: Pointer;
  LBlockOffsetFromMediumSpanStart, LMediumBlockSize, LSmallBlockOffset, LLastBlockOffset: Integer;
  LPSmallBlockManager: PSmallBlockManager;
begin
  {Assume success, i.e. that all arenas will be walked.  This will be reset to False if a lock timeout occurs.}
  Result := True;

  LTimestampMilliseconds := 0;

  LBlockInfo.UserData := AUserData;

  if AWalkBlockTypes = [] then
    AWalkBlockTypes := [Low(TFastMM_WalkAllocatedBlocksBlockType)..High(TFastMM_WalkAllocatedBlocksBlockType)];

  {Walk the large block managers}
  if btLargeBlock in AWalkBlockTypes then
  begin
    LBlockInfo.BlockType := btLargeBlock;
    LBlockInfo.BlockIsFree := False;

    {Clear the fields that are not applicable to large blocks.}
    LBlockInfo.IsSequentialFeedMediumBlockSpan := False;
    LBlockInfo.MediumBlockSequentialFeedSpanUnusedBytes := 0;
    LBlockInfo.SmallBlockSpanBlockSize := 0;
    LBlockInfo.IsSequentialFeedSmallBlockSpan := False;

    for LArenaIndex := 0 to CFastMM_LargeBlockArenaCount - 1 do
    begin
      LPLargeBlockManager := @LargeBlockManagers[LArenaIndex];

      LBlockInfo.ArenaIndex := LArenaIndex;

      LLockWaitTimeMilliseconds := 0;
      while (AtomicCmpExchange(LPLargeBlockManager.LargeBlockManagerLocked, 1, 0) <> 0)
        and FastMM_WalkBlocks_CheckTimeout(LLockWaitTimeMilliseconds, LTimestampMilliseconds, ALockTimeoutMilliseconds) do
      begin
        OS_AllowOtherThreadToRun;
      end;

      if LLockWaitTimeMilliseconds > ALockTimeoutMilliseconds then
      begin
        Result := False;
        Continue;
      end;

      LPLargeBlockHeader := LPLargeBlockManager.FirstLargeBlockHeader;
      while NativeUInt(LPLargeBlockHeader) <> NativeUInt(LPLargeBlockManager) do
      begin
        LBlockInfo.BlockAddress := @PByte(LPLargeBlockHeader)[CLargeBlockHeaderSize];
        LBlockInfo.BlockSize := LPLargeBlockHeader.ActualBlockSize;
        LBlockInfo.UsableSize := LPLargeBlockHeader.UserAllocatedSize;

        FastMM_WalkBlocks_AdjustForDebugSubBlock(LBlockInfo);
        ACallBack(LBlockInfo);

        LPLargeBlockHeader := LPLargeBlockHeader.NextLargeBlockHeader;
      end;

      LPLargeBlockManager.LargeBlockManagerLocked := 0;
    end;

  end;

  {Walk the medium block managers}
  if AWalkBlockTypes * [btMediumBlockSpan, btMediumBlock, btSmallBlockSpan, btSmallBlock] <> [] then
  begin

    for LArenaIndex := 0 to CFastMM_MediumBlockArenaCount - 1 do
    begin
      LPMediumBlockManager := @MediumBlockManagers[LArenaIndex];

      LBlockInfo.ArenaIndex := LArenaIndex;

      LLockWaitTimeMilliseconds := 0;
      while (AtomicCmpExchange(LPMediumBlockManager.MediumBlockManagerLocked, 1, 0) <> 0)
        and FastMM_WalkBlocks_CheckTimeout(LLockWaitTimeMilliseconds, LTimestampMilliseconds, ALockTimeoutMilliseconds) do
      begin
        OS_AllowOtherThreadToRun;
      end;

      if LLockWaitTimeMilliseconds > ALockTimeoutMilliseconds then
      begin
        Result := False;
        Continue;
      end;

      LPMediumBlockSpan := LPMediumBlockManager.FirstMediumBlockSpanHeader;
      while NativeUInt(LPMediumBlockSpan) <> NativeUInt(LPMediumBlockManager) do
      begin

        if LPMediumBlockManager.SequentialFeedMediumBlockSpan = LPMediumBlockSpan then
        begin
          LBlockOffsetFromMediumSpanStart := LPMediumBlockManager.LastMediumBlockSequentialFeedOffset.IntegerValue;
          if LBlockOffsetFromMediumSpanStart <= CMediumBlockSpanHeaderSize then
            LBlockOffsetFromMediumSpanStart := CMediumBlockSpanHeaderSize;
        end
        else
          LBlockOffsetFromMediumSpanStart := CMediumBlockSpanHeaderSize;

        if btMediumBlockSpan in AWalkBlockTypes then
        begin
          LBlockInfo.BlockAddress := LPMediumBlockSpan;
          LBlockInfo.BlockSize := LPMediumBlockSpan.SpanSize;
          LBlockInfo.UsableSize := LPMediumBlockSpan.SpanSize - CMediumBlockSpanHeaderSize;
          LBlockInfo.BlockType := btMediumBlockSpan;
          LBlockInfo.BlockIsFree := False;
          LBlockInfo.ArenaIndex := LArenaIndex;
          if LBlockOffsetFromMediumSpanStart > CMediumBlockSpanHeaderSize then
          begin
            LBlockInfo.IsSequentialFeedMediumBlockSpan := True;
            LBlockInfo.MediumBlockSequentialFeedSpanUnusedBytes := LBlockOffsetFromMediumSpanStart - CMediumBlockSpanHeaderSize;
          end
          else
          begin
            LBlockInfo.IsSequentialFeedMediumBlockSpan := False;
            LBlockInfo.MediumBlockSequentialFeedSpanUnusedBytes := 0;
          end;
          LBlockInfo.SmallBlockSpanBlockSize := 0;
          LBlockInfo.IsSequentialFeedSmallBlockSpan := False;
          LBlockInfo.DebugInformation := nil;

          ACallBack(LBlockInfo);
        end;

        {Walk all the medium blocks in the medium block span.}
        if AWalkBlockTypes * [btMediumBlock, btSmallBlockSpan, btSmallBlock] <> [] then
        begin
          while LBlockOffsetFromMediumSpanStart < LPMediumBlockSpan.SpanSize do
          begin
            LPMediumBlock := PByte(LPMediumBlockSpan) + LBlockOffsetFromMediumSpanStart;
            LMediumBlockSize := GetMediumBlockSize(LPMediumBlock);

            LBlockInfo.BlockIsFree := BlockIsFree(LPMediumBlock);
            if (not AWalkUsedBlocksOnly) or (not LBlockInfo.BlockIsFree) then
            begin
              {Read the pointer to the small block manager in case this is a small block span.}
              if (AWalkBlockTypes * [btSmallBlockSpan, btSmallBlock] <> [])
                and PMediumBlockHeader(LPMediumBlock)[-1].IsSmallBlockSpan then
              begin
                LPSmallBlockManager := PSmallBlockSpanHeader(LPMediumBlock).SmallBlockManager;

                LLockWaitTimeMilliseconds := 0;
                while (AtomicCmpExchange(LPSmallBlockManager.SmallBlockManagerLocked, 1, 0) <> 0)
                  and FastMM_WalkBlocks_CheckTimeout(LLockWaitTimeMilliseconds, LTimestampMilliseconds, ALockTimeoutMilliseconds) do
                begin
                  OS_AllowOtherThreadToRun;
                end;

                if LLockWaitTimeMilliseconds > ALockTimeoutMilliseconds then
                begin
                  Result := False;
                  LPSmallBlockManager := nil;
                  LSmallBlockOffset := 0;
                end
                else
                begin

                  {Memory fence required for ARM}

                  {The last block may have been released before the manager was locked, so we need to check whether it is
                  still a small block span.}
                  if PMediumBlockHeader(LPMediumBlock)[-1].IsSmallBlockSpan then
                  begin
                    if LPSmallBlockManager.SequentialFeedSmallBlockSpan = LPMediumBlock then
                    begin
                      LSmallBlockOffset := LPSmallBlockManager.LastSmallBlockSequentialFeedOffset.IntegerValue;
                      if LSmallBlockOffset < CSmallBlockSpanHeaderSize then
                        LSmallBlockOffset := CSmallBlockSpanHeaderSize;
                    end
                    else
                      LSmallBlockOffset := CSmallBlockSpanHeaderSize;
                  end
                  else
                  begin
                    LSmallBlockOffset := 0;
                    LPSmallBlockManager.SmallBlockManagerLocked := 0;
                    LPSmallBlockManager := nil;
                  end;
                end;
              end
              else
              begin
                LPSmallBlockManager := nil;
                LSmallBlockOffset := 0;
              end;

              if AWalkBlockTypes * [btMediumBlock, btSmallBlockSpan] <> [] then
              begin
                LBlockInfo.BlockAddress := LPMediumBlock;
                LBlockInfo.BlockSize := LMediumBlockSize;
                LBlockInfo.ArenaIndex := LArenaIndex;
                LBlockInfo.MediumBlockSequentialFeedSpanUnusedBytes := 0;

                if LPSmallBlockManager <> nil then
                begin
                  if btSmallBlockSpan in AWalkBlockTypes then
                  begin
                    LBlockInfo.BlockType := btSmallBlockSpan;
                    LBlockInfo.UsableSize := LPSmallBlockManager.BlockSize * PSmallBlockSpanHeader(LPMediumBlock).TotalBlocksInSpan;
                    LBlockInfo.SmallBlockSpanBlockSize := LPSmallBlockManager.BlockSize;
                    LBlockInfo.IsSequentialFeedSmallBlockSpan := LSmallBlockOffset > CSmallBlockSpanHeaderSize;
                    if LBlockInfo.IsSequentialFeedSmallBlockSpan then
                      LBlockInfo.SmallBlockSequentialFeedSpanUnusedBytes := LSmallBlockOffset - CSmallBlockSpanHeaderSize
                    else
                      LBlockInfo.SmallBlockSequentialFeedSpanUnusedBytes := 0;
                    LBlockInfo.DebugInformation := nil;
                    ACallBack(LBlockInfo);
                  end;
                end
                else
                begin
                  if btMediumBlock in AWalkBlockTypes then
                  begin
                    LBlockInfo.BlockType := btMediumBlock;
                    LBlockInfo.UsableSize := LMediumBlockSize - CMediumBlockHeaderSize;
                    LBlockInfo.SmallBlockSpanBlockSize := 0;
                    LBlockInfo.IsSequentialFeedSmallBlockSpan := False;
                    LBlockInfo.SmallBlockSequentialFeedSpanUnusedBytes := 0;
                    FastMM_WalkBlocks_AdjustForDebugSubBlock(LBlockInfo);
                    ACallBack(LBlockInfo);
                  end;
                end;

              end;

              {If small blocks need to be walked then LPSmallBlockManager will be <> nil.}
              if LPSmallBlockManager <> nil then
              begin

                if btSmallBlock in AWalkBlockTypes then
                begin
                  LLastBlockOffset := CSmallBlockSpanHeaderSize
                    + LPSmallBlockManager.BlockSize * (PSmallBlockSpanHeader(LPMediumBlock).TotalBlocksInSpan - 1);
                  while LSmallBlockOffset <= LLastBlockOffset do
                  begin
                    LBlockInfo.BlockAddress := PByte(LPMediumBlock) + LSmallBlockOffset;

                    LBlockInfo.BlockIsFree := BlockIsFree(LBlockInfo.BlockAddress);
                    if (not AWalkUsedBlocksOnly) or (not LBlockInfo.BlockIsFree) then
                    begin
                      LBlockInfo.BlockSize := LPSmallBlockManager.BlockSize;
                      LBlockInfo.UsableSize := LPSmallBlockManager.BlockSize - CSmallBlockHeaderSize;
                      LBlockInfo.ArenaIndex := (NativeInt(LPSmallBlockManager) - NativeInt(@SmallBlockManagers)) div SizeOf(TSmallBlockArena);
                      LBlockInfo.BlockType := btSmallBlock;
                      LBlockInfo.IsSequentialFeedMediumBlockSpan := False;
                      LBlockInfo.MediumBlockSequentialFeedSpanUnusedBytes := 0;
                      LBlockInfo.IsSequentialFeedSmallBlockSpan := False;
                      LBlockInfo.SmallBlockSpanBlockSize := 0;
                      LBlockInfo.SmallBlockSequentialFeedSpanUnusedBytes := 0;

                      FastMM_WalkBlocks_AdjustForDebugSubBlock(LBlockInfo);
                      ACallBack(LBlockInfo);
                    end;

                    Inc(LSmallBlockOffset, LPSmallBlockManager.BlockSize);
                  end;
                end;

                LPSmallBlockManager.SmallBlockManagerLocked := 0;
              end;

            end;

            Inc(LBlockOffsetFromMediumSpanStart, LMediumBlockSize);
          end;
        end;

        LPMediumBlockSpan := LPMediumBlockSpan.NextMediumBlockSpanHeader;
      end;

      LPMediumBlockManager.MediumBlockManagerLocked := 0;
    end;

  end;

end;

procedure FastMM_ScanDebugBlocksForCorruption_CallBack(const ABlockInfo: TFastMM_WalkAllocatedBlocks_BlockInfo);
begin
  {If it is not a debug mode block then there's nothing to check.}
  if ABlockInfo.DebugInformation = nil then
    Exit;

  {Check the block header and footer for corruption}
  if not CheckDebugBlockHeaderAndFooterCheckSumsValid(ABlockInfo.DebugInformation) then
    System.Error(reInvalidPtr);

  {If it is a free block, check whether it has been modified after being freed.}
  if ABlockInfo.BlockIsFree and (not CheckDebugBlockFillPatternIntact(ABlockInfo.DebugInformation)) then
    System.Error(reInvalidPtr);
end;

procedure FastMM_ScanDebugBlocksForCorruption;
begin
  FastMM_WalkBlocks(FastMM_ScanDebugBlocksForCorruption_CallBack, [btLargeBlock, btMediumBlock, btSmallBlock], False);
end;

procedure FastMM_GetHeapStatus_CallBack(const ABlockInfo: TFastMM_WalkAllocatedBlocks_BlockInfo);
var
  LPHeapStatus: ^THeapStatus;
begin
  LPHeapStatus := ABlockInfo.UserData;

  case ABlockInfo.BlockType of

    btLargeBlock:
    begin
      Inc(LPHeapStatus.TotalCommitted, ABlockInfo.BlockSize);
      Inc(LPHeapStatus.TotalAllocated, ABlockInfo.UsableSize);
    end;

    btMediumBlockSpan:
    begin
      Inc(LPHeapStatus.TotalCommitted, ABlockInfo.BlockSize);
      if ABlockInfo.IsSequentialFeedMediumBlockSpan then
        Inc(LPHeapStatus.Unused, ABlockInfo.MediumBlockSequentialFeedSpanUnusedBytes);
    end;

    btMediumBlock:
    begin
      if ABlockInfo.BlockIsFree then
        Inc(LPHeapStatus.FreeBig, ABlockInfo.UsableSize)
      else
        Inc(LPHeapStatus.TotalAllocated, ABlockInfo.UsableSize);
    end;

    btSmallBlockSpan:
    begin
      if ABlockInfo.IsSequentialFeedSmallBlockSpan then
        Inc(LPHeapStatus.Unused, ABlockInfo.SmallBlockSequentialFeedSpanUnusedBytes);
    end;

    btSmallBlock:
    begin
      if ABlockInfo.BlockIsFree then
        Inc(LPHeapStatus.FreeSmall, ABlockInfo.UsableSize)
      else
        Inc(LPHeapStatus.TotalAllocated, ABlockInfo.UsableSize);
    end;

  end;
end;

{Returns a THeapStatus structure with information about the current memory usage.}
function FastMM_GetHeapStatus: THeapStatus;
begin
  Result := Default(THeapStatus);

  FastMM_WalkBlocks(FastMM_GetHeapStatus_CallBack,
    [btLargeBlock, btMediumBlockSpan, btMediumBlock, btSmallBlockSpan, btSmallBlock], False, @Result);

  Result.TotalFree := Result.FreeSmall + Result.FreeBig + Result.Unused;
  Result.TotalAddrSpace := Result.TotalCommitted;
  Result.Overhead := Result.TotalAddrSpace - Result.TotalAllocated - Result.TotalFree;
end;

function FastMM_GetUsageSummary: TFastMM_UsageSummary;
var
  LHeapStatus: THeapStatus;
begin
  LHeapStatus := FastMM_GetHeapStatus;

  Result.AllocatedBytes := LHeapStatus.TotalAllocated;
  Result.OverheadBytes := LHeapStatus.TotalAddrSpace - LHeapStatus.TotalAllocated;

  if LHeapStatus.TotalAddrSpace > 0 then
    Result.EfficiencyPercentage := Result.AllocatedBytes / LHeapStatus.TotalAddrSpace * 100
  else
    Result.EfficiencyPercentage := 100;
end;

{Returns True if there are live pointers using this memory manager.}
function FastMM_HasLivePointers: Boolean;
var
  i: Integer;
  LPMediumBlockManager: PMediumBlockManager;
  LPLargeBlockManager: PLargeBlockManager;
begin
  for i := 0 to CFastMM_MediumBlockArenaCount - 1 do
  begin
    LPMediumBlockManager := @MediumBlockManagers[i];
    if NativeUInt(LPMediumBlockManager.FirstMediumBlockSpanHeader) <> NativeUInt(LPMediumBlockManager) then
      Exit(True);
  end;

  for i := 0 to CFastMM_LargeBlockArenaCount - 1 do
  begin
    LPLargeBlockManager := @LargeBlockManagers[i];
    if NativeUInt(LPLargeBlockManager.FirstLargeBlockHeader) <> NativeUInt(LPLargeBlockManager) then
      Exit(True);
  end;

  Result := False;
end;

{Returns True if external code has changed the installed memory manager.}
function FastMM_InstalledMemoryManagerChangedExternally: Boolean;
var
  LCurrentMemoryManager: TMemoryManagerEx;
begin
  GetMemoryManager(LCurrentMemoryManager);
  Result := (@LCurrentMemoryManager.GetMem <> @InstalledMemoryManager.GetMem)
    or (@LCurrentMemoryManager.FreeMem <> @InstalledMemoryManager.FreeMem)
    or (@LCurrentMemoryManager.ReallocMem <> @InstalledMemoryManager.ReallocMem)
    or (@LCurrentMemoryManager.AllocMem <> @InstalledMemoryManager.AllocMem)
    or (@LCurrentMemoryManager.RegisterExpectedMemoryLeak <> @InstalledMemoryManager.RegisterExpectedMemoryLeak)
    or (@LCurrentMemoryManager.UnregisterExpectedMemoryLeak <> @InstalledMemoryManager.UnregisterExpectedMemoryLeak);
end;


{--------------------------------------------------------}
{----------FastMM_LogStateToFile Implementation----------}
{--------------------------------------------------------}

const
  CMaxMemoryLogNodes = 100000;
  CQuickSortMinimumItemsInPartition = 8;

type
  {While scanning the memory pool the list of classes is built up in a binary search tree.}
  PMemoryLogNode = ^TMemoryLogNode;
  TMemoryLogNode = record
    {The left and right child nodes}
    LeftAndRightNodePointers: array[Boolean] of PMemoryLogNode;
    {A class reference or a string type enum.}
    BlockContentType: NativeUInt;
    {The number of instances of the class}
    InstanceCount: NativeUInt;
    {The total memory usage for this class}
    TotalMemoryUsage: NativeUInt;
  end;
  TMemoryLogNodes = array[0..CMaxMemoryLogNodes - 1] of TMemoryLogNode;
  PMemoryLogNodes = ^TMemoryLogNodes;

  TMemoryLogInfo = record
    {The number of nodes in "Nodes" that are used.}
    NodeCount: Integer;
    {The root node of the binary search tree.  The content of this node is not actually used, it just simplifies the
    binary search code.}
    RootNode: TMemoryLogNode;
    Nodes: TMemoryLogNodes;
  end;
  PMemoryLogInfo = ^TMemoryLogInfo;

procedure FastMM_LogStateToFile_Callback(const ABlockInfo: TFastMM_WalkAllocatedBlocks_BlockInfo);
var
  LBlockContentType, LBlockContentTypeHashBits: NativeUInt;
  LPLogInfo: PMemoryLogInfo;
  LPParentNode, LPClassNode: PMemoryLogNode;
  LChildNodeDirection: Boolean;
begin
  LPLogInfo := ABlockInfo.UserData;

  {Detecting an object is very expensive (due to the VirtualQuery call), so we do some basic checks and try to find the
  "class" in the tree first.}
  LBlockContentType := PNativeUInt(ABlockInfo.BlockAddress)^;
  if (LBlockContentType > 65535)
    and (LBlockContentType and (SizeOf(Pointer) - 1) = 0) then
  begin
    LPParentNode := @LPLogInfo.RootNode;
    LBlockContentTypeHashBits := LBlockContentType;
    repeat
      LChildNodeDirection := Boolean(LBlockContentTypeHashBits and 1);
      {Split off the next bit of the class pointer and traverse in the appropriate direction.}
      LPClassNode := LPParentNode.LeftAndRightNodePointers[LChildNodeDirection];
      if (LPClassNode = nil) or (LPClassNode.BlockContentType = LBlockContentType) then
        Break;
      {The node was not found:  Keep on traversing the tree.}
      LBlockContentTypeHashBits := LBlockContentTypeHashBits shr 1;
      LPParentNode := LPClassNode;
    until False;
  end
  else
    LPClassNode := nil;

  {Was the "class" found?}
  if LPClassNode = nil then
  begin
    {The "class" is not yet in the tree:  Determine if it is actually a class.}
    LBlockContentType := DetectBlockContentType(ABlockInfo.BlockAddress, ABlockInfo.UsableSize);
    {Is this class already in the tree?}
    LPParentNode := @LPLogInfo.RootNode;
    LBlockContentTypeHashBits := LBlockContentType;
    repeat
      LChildNodeDirection := Boolean(LBlockContentTypeHashBits and 1);
      {Split off the next bit of the class pointer and traverse in the appropriate direction.}
      LPClassNode := LPParentNode.LeftAndRightNodePointers[LChildNodeDirection];
      if LPClassNode = nil then
      begin
        {The end of the tree was reached:  Add a new child node (if possible)}
        if LPLogInfo.NodeCount = CMaxMemoryLogNodes then
          Exit;
        LPClassNode := @LPLogInfo.Nodes[LPLogInfo.NodeCount];
        Inc(LPLogInfo.NodeCount);
        LPParentNode.LeftAndRightNodePointers[LChildNodeDirection] := LPClassNode;
        LPClassNode.BlockContentType := LBlockContentType;
        Break;
      end
      else
      begin
        if LPClassNode.BlockContentType = LBlockContentType then
          Break;
      end;
      {The node was not found:  Keep on traversing the tree.}
      LBlockContentTypeHashBits := LBlockContentTypeHashBits shr 1;
      LPParentNode := LPClassNode;
    until False;
  end;

  {Update the statistics for the class}
  Inc(LPClassNode.InstanceCount);
  Inc(LPClassNode.TotalMemoryUsage, ABlockInfo.UsableSize);
end;

{FastMM_LogStateToFile subroutine:  A median-of-3 quicksort routine for sorting a TMemoryLogNodes array.}
procedure FastMM_LogStateToFile_QuickSortLogNodes(APLeftItem: PMemoryLogNodes; ARightIndex: Integer);
var
  M, I, J: Integer;
  LPivot, LTempItem: TMemoryLogNode;
begin
  while True do
  begin
    {Order the left, middle and right items in descending order}
    M := ARightIndex shr 1;
    if APLeftItem[0].TotalMemoryUsage < APLeftItem[M].TotalMemoryUsage then
    begin
      LTempItem := APLeftItem[0];
      APLeftItem[0] := APLeftItem[M];
      APLeftItem[M] := LTempItem;
    end;
    if APLeftItem[M].TotalMemoryUsage < APLeftItem[ARightIndex].TotalMemoryUsage then
    begin
      LTempItem := APLeftItem[ARightIndex];
      APLeftItem[ARightIndex] := APLeftItem[M];
      APLeftItem[M] := LTempItem;
      if APLeftItem[0].TotalMemoryUsage < APLeftItem[M].TotalMemoryUsage then
      begin
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
      {Find the first item from the left that is not greater than the pivot}
      repeat
        Inc(I);
      until APLeftItem[I].TotalMemoryUsage <= LPivot.TotalMemoryUsage;
      {Find the first item from the right that is not less than the pivot}
      repeat
        Dec(J);
      until APLeftItem[J].TotalMemoryUsage >= LPivot.TotalMemoryUsage;
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
    if J >= (CQuickSortMinimumItemsInPartition - 1) then
      FastMM_LogStateToFile_QuickSortLogNodes(APLeftItem, J);

    {Sort the right-hand partition}
    APLeftItem := @APLeftItem[I + 1];
    ARightIndex := ARightIndex - I - 1;
    if ARightIndex < (CQuickSortMinimumItemsInPartition - 1) then
      Break;
  end;
end;

{FastMM_LogStateToFile subroutine:  An InsertionSort routine for sorting a TMemoryLogNodes array.}
procedure FastMM_LogStateToFile_InsertionSortLogNodes(APLeftItem: PMemoryLogNodes; ARightIndex: Integer);
var
  I, J: Integer;
  LCurNode: TMemoryLogNode;
begin
  for I := 1 to ARightIndex do
  begin
    LCurNode := APLeftItem[I];
    {Scan backwards to find the best insertion spot}
    J := I;
    while (J > 0) and (APLeftItem[J - 1].TotalMemoryUsage < LCurNode.TotalMemoryUsage) do
    begin
      APLeftItem[J] := APLeftItem[J - 1];
      Dec(J);
    end;
    APLeftItem[J] := LCurNode;
  end;
end;

{Writes a log file containing a summary of the memory mananger state and a summary of allocated blocks grouped by class.
The file will be saved in the encoding specified by FastMM_TextFileEncoding.}
function FastMM_LogStateToFile(const AFilename: string; const AAdditionalDetails: string;
  ALockTimeoutMilliseconds: Cardinal): Boolean;
const
  CStateLogMaxChars = 1024 * 1024;
  CRLF: PWideChar = #13#10;
var
  LMemoryManagerUsageSummary: TFastMM_UsageSummary;
  LBufferSize: Integer;
  LPLogInfo: PMemoryLogInfo;
  LPTokenBufferStart, LPStateLogBufferStart, LPBufferEnd, LPTokenPos, LPStateLogPos: PWideChar;
  LTokenValues: TEventLogTokenValues;
  LInd: Integer;
  LPNode: PMemoryLogNode;
begin
  {Get the current memory manager usage summary.}
  LMemoryManagerUsageSummary := FastMM_GetUsageSummary;

  {Allocate the memory required to store the token buffer, log text, as well as the detailed allocation information.}
  LBufferSize := SizeOf(TMemoryLogInfo) + (CTokenBufferMaxWideChars + CStateLogMaxChars) * SizeOf(Char);
  LPLogInfo := OS_AllocateVirtualMemory(LBufferSize, False, False);
  if LPLogInfo <> nil then
  begin
    try
      {Obtain the list of classes, together with the total memory usage and block count for each.}
      FastMM_WalkBlocks(FastMM_LogStateToFile_Callback, [btLargeBlock, btMediumBlock, btSmallBlock], True, LPLogInfo,
        ALockTimeoutMilliseconds);

      {Sort the classes in descending total memory usage order:  Do the initial QuickSort pass over the list to sort the
      list in groups of QuickSortMinimumItemsInPartition size, and then do the final InsertionSort pass.}
      if LPLogInfo.NodeCount >= CQuickSortMinimumItemsInPartition then
        FastMM_LogStateToFile_QuickSortLogNodes(@LPLogInfo.Nodes[0], LPLogInfo.NodeCount - 1);
      FastMM_LogStateToFile_InsertionSortLogNodes(@LPLogInfo.Nodes[0], LPLogInfo.NodeCount - 1);

      LPTokenBufferStart := @LPLogInfo.Nodes[LPLogInfo.NodeCount];
      LPStateLogBufferStart := @LPTokenBufferStart[CTokenBufferMaxWideChars];
      LPBufferEnd := @PByte(LPLogInfo)[LBufferSize];

      {Add the header with the usage summary.}
      LTokenValues := Default(TEventLogTokenValues);
      LPTokenPos := AddTokenValues_GeneralTokens(LTokenValues, LPTokenBufferStart, LPStateLogBufferStart);
      LPTokenPos := AddTokenValue_NativeUInt(LTokenValues, CStateLogTokenAllocatedKB,
        LMemoryManagerUsageSummary.AllocatedBytes div 1024, LPTokenPos, LPStateLogBufferStart);
      LPTokenPos := AddTokenValue_NativeUInt(LTokenValues, CStateLogTokenOverheadKB,
        LMemoryManagerUsageSummary.OverheadBytes div 1024, LPTokenPos, LPStateLogBufferStart);
      AddTokenValue_NativeInt(LTokenValues, CStateLogTokenEfficiencyPercentage,
        Round(LMemoryManagerUsageSummary.EfficiencyPercentage), LPTokenPos, LPStateLogBufferStart);
      LPStateLogPos := SubstituteTokenValues(FastMM_LogStateToFileTemplate, LTokenValues, LPStateLogBufferStart,
        LPBufferEnd);

      {Add the usage information for each class}
      LTokenValues := Default(TEventLogTokenValues);
      for LInd := 0 to LPLogInfo.NodeCount - 1 do
      begin
        LPNode := @LPLogInfo.Nodes[LInd];

        LPTokenPos := AddTokenValue_NativeUInt(LTokenValues, CStateLogTokenClassTotalBytesUsed,
          LPNode.TotalMemoryUsage, LPTokenBufferStart, LPStateLogBufferStart);
        LPTokenPos := AddTokenValue_NativeUInt(LTokenValues, CStateLogTokenClassInstanceCount,
          LPNode.InstanceCount, LPTokenPos, LPStateLogBufferStart);
        LPTokenPos := AddTokenValue_NativeUInt(LTokenValues, CStateLogTokenClassAverageBytesPerInstance,
          Round(LPNode.TotalMemoryUsage / LPNode.InstanceCount), LPTokenPos, LPStateLogBufferStart);
        AddTokenValue_BlockContentType(LTokenValues, CEventLogTokenObjectClass, LPNode.BlockContentType, LPTokenPos,
          LPStateLogBufferStart);
        LPStateLogPos := SubstituteTokenValues(FastMM_LogStateToFileTemplate_UsageDetail, LTokenValues, LPStateLogPos,
          LPBufferEnd);
      end;

      {Append the additional information}
      if AAdditionalDetails <> '' then
      begin
        LPStateLogPos := AppendTextToBuffer(CRLF, 2, LPStateLogPos, LPBufferEnd);
        LPStateLogPos := AppendTextToBuffer(PWideChar(AAdditionalDetails), Length(AAdditionalDetails), LPStateLogPos,
          LPBufferEnd);
      end;

      {Delete the old file and write the new one.}
      OS_DeleteFile(PWideChar(AFilename));
      Result := AppendTextFile(PWideChar(AFilename), LPStateLogBufferStart, CharCount(LPStateLogPos, LPStateLogBufferStart));

    finally
      OS_FreeVirtualMemory(LPLogInfo);
    end;
  end
  else
    Result := False;
end;

{--------------------------------------------------------}
{----------------Memory Manager Sharing------------------}
{--------------------------------------------------------}

{Generates a string identifying the process}
procedure FastMM_BuildFileMappingObjectName;
var
  i, LProcessID: Cardinal;
begin
  LProcessID := GetCurrentProcessId;
  for i := 0 to 7 do
  begin
    SharingFileMappingObjectName[(High(SharingFileMappingObjectName) - 1) - i] :=
      AnsiChar(CHexDigits[((LProcessID shr (i * 4)) and $F)]);
  end;
end;

{Searches the current process for a shared memory manager}
function FastMM_FindSharedMemoryManager: PMemoryManagerEx;
var
  LPMapAddress: Pointer;
  LLocalMappingObjectHandle: NativeUInt;
begin
  {Try to open the shared memory manager file mapping}
  LLocalMappingObjectHandle := OpenFileMappingA(FILE_MAP_READ, False, SharingFileMappingObjectName);
  {Is a memory manager in this process sharing its memory manager?}
  if LLocalMappingObjectHandle = 0 then
  begin
    {There is no shared memory manager in the process.}
    Result := nil;
  end
  else
  begin
    {Map a view of the shared memory and get the address of the shared memory manager}
    LPMapAddress := MapViewOfFile(LLocalMappingObjectHandle, FILE_MAP_READ, 0, 0, 0);
    Result := PPointer(LPMapAddress)^;
    UnmapViewOfFile(LPMapAddress);
    CloseHandle(LLocalMappingObjectHandle);
  end;
end;

{Searches the current process for a shared memory manager.  If no memory has been allocated using this memory manager
it will switch to using the shared memory manager instead.  Returns True if another memory manager was found and it
could be shared.  If this memory manager instance *is* the shared memory manager, it will do nothing and return True.}
function FastMM_AttemptToUseSharedMemoryManager: Boolean;
var
  LTokenValues: TEventLogTokenValues;
  LTokenValueBuffer: array[0..CTokenBufferMaxWideChars - 1] of WideChar;
  LPMemoryManagerEx: PMemoryManagerEx;
begin
  if CurrentInstallationState = mmisInstalled then
  begin
    {Is this MM being shared?  If so, switching to another MM is not allowed}
    if SharingFileMappingObjectHandle = 0 then
    begin
      {May not switch memory manager after memory has been allocated}
      if not FastMM_HasLivePointers then
      begin
        LPMemoryManagerEx := FastMM_FindSharedMemoryManager;
        if LPMemoryManagerEx <> nil then
        begin

          InstalledMemoryManager := LPMemoryManagerEx^;
          SetMemoryManager(InstalledMemoryManager);
          CurrentInstallationState := mmisUsingSharedMemoryManager;

          {Free the address space slack, since it will not be needed.}
          ReleaseEmergencyReserveAddressSpace;

          Result := True;
        end
        else
          Result := False;
      end
      else
      begin
        {Memory has already been allocated using this memory manager.  We cannot rip the memory manager out from under
        live pointers.}

        LTokenValues := Default(TEventLogTokenValues);
        AddTokenValues_GeneralTokens(LTokenValues, @LTokenValueBuffer, @LTokenValueBuffer[High(LTokenValueBuffer)]);
        LogEvent(mmetCannotSwitchToSharedMemoryManagerWithLivePointers, LTokenValues);

        Result := False;
      end;
    end
    else
    begin
      {This memory manager is being shared, and an attempt is being made by the application to use the shared memory
      manager (which is this one):  Don't do anything and return success.  (This situation can occur when using
      SimpleShareMem in a DLL together with runtime packages.)}
      Result := True;
    end;
  end
  else
  begin
    {Another memory manager has already been installed.}
    Result := False;
  end;
end;

{Starts sharing this memory manager with other modules in the current process.  Only one memory manager may be shared
per process, so this function may fail.}
function FastMM_ShareMemoryManager: Boolean;
var
  LPMapAddress: Pointer;
begin
  if (CurrentInstallationState = mmisInstalled)
    and (not FastMM_InstalledMemoryManagerChangedExternally)
    and (SharingFileMappingObjectHandle = 0) then
  begin
    {Is any other module already sharing its MM?}
    if FastMM_FindSharedMemoryManager = nil then
    begin
      {Create the memory mapped file}
      SharingFileMappingObjectHandle := CreateFileMappingA(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE, 0,
        SizeOf(Pointer), SharingFileMappingObjectName);
      {Map a view of the memory}
      LPMapAddress := MapViewOfFile(SharingFileMappingObjectHandle, FILE_MAP_WRITE, 0, 0, 0);
      {Set a pointer to the new memory manager}
      PPointer(LPMapAddress)^ := @InstalledMemoryManager;
      {Unmap the file}
      UnmapViewOfFile(LPMapAddress);
      {Sharing this MM}
      Result := True;
    end
    else
    begin
      {Another module is already sharing its memory manager}
      Result := False;
    end;
  end
  else
  begin
    {Either another memory manager has been set or this memory manager is
     already being shared}
    Result := False;
  end;
end;


{--------------------------------------------------}
{-------------Memory leak registration----------------}
{--------------------------------------------------}

{Adds a leak to the specified list}
function UpdateExpectedLeakList(APLeakList: PPExpectedMemoryLeak; APNewEntry: PExpectedMemoryLeak;
  AExactSizeMatch: Boolean = True): Boolean;
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
    {Find a matching entry.  If an exact size match is not required and the leak is larger than the current entry, use
    it if the expected size of the next entry is too large.}
    if (LPInsertAfter.LeakAddress = APNewEntry.LeakAddress)
      and ((LPInsertAfter.LeakedClass = APNewEntry.LeakedClass))
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

{Locks the expected leaks.  Returns False if the list could not be allocated.}
function LockExpectedMemoryLeaksList: Boolean;
begin
  {Lock the expected leaks list}
  while AtomicCmpExchange(ExpectedMemoryLeaksListLocked, 1, 0) <> 0 do
    OS_AllowOtherThreadToRun;

  {Allocate the list if it does not exist}
  if ExpectedMemoryLeaks = nil then
    ExpectedMemoryLeaks := OS_AllocateVirtualMemory(CExpectedMemoryLeaksListSize, False, False);

  Result := ExpectedMemoryLeaks <> nil;
end;

{Registers expected memory leaks.  Returns True on success.  The list of leaked blocks is limited, so failure is
possible if the list is full.}
function FastMM_RegisterExpectedMemoryLeak(ALeakedPointer: Pointer): Boolean; overload;
var
  LNewEntry: TExpectedMemoryLeak;
begin
  {Fill out the structure}
  LNewEntry.LeakAddress := ALeakedPointer;
  LNewEntry.LeakedClass := nil;
  LNewEntry.LeakSize := 0;
  LNewEntry.LeakCount := 1;
  {Add it to the correct list}
  Result := LockExpectedMemoryLeaksList
    and UpdateExpectedLeakList(@ExpectedMemoryLeaks.FirstEntryByAddress, @LNewEntry);
  ExpectedMemoryLeaksListLocked := 0;
end;

function FastMM_RegisterExpectedMemoryLeak(ALeakedObjectClass: TClass; ACount: Integer = 1): Boolean; overload;
var
  LNewEntry: TExpectedMemoryLeak;
begin
  {Fill out the structure}
  LNewEntry.LeakAddress := nil;
  LNewEntry.LeakedClass := ALeakedObjectClass;
  LNewEntry.LeakSize := ALeakedObjectClass.InstanceSize;
  LNewEntry.LeakCount := ACount;
  {Add it to the correct list}
  Result := LockExpectedMemoryLeaksList
    and UpdateExpectedLeakList(@ExpectedMemoryLeaks.FirstEntryByClass, @LNewEntry);
  ExpectedMemoryLeaksListLocked := 0;
end;

function FastMM_RegisterExpectedMemoryLeak(ALeakedBlockSize: NativeInt; ACount: Integer = 1): Boolean; overload;
var
  LNewEntry: TExpectedMemoryLeak;
begin
  {Fill out the structure}
  LNewEntry.LeakAddress := nil;
  LNewEntry.LeakedClass := nil;
  LNewEntry.LeakSize := ALeakedBlockSize;
  LNewEntry.LeakCount := ACount;
  {Add it to the correct list}
  Result := LockExpectedMemoryLeaksList
    and UpdateExpectedLeakList(@ExpectedMemoryLeaks.FirstEntryBySizeOnly, @LNewEntry);
  ExpectedMemoryLeaksListLocked := 0;
end;

function FastMM_UnregisterExpectedMemoryLeak(ALeakedPointer: Pointer): Boolean; overload;
var
  LNewEntry: TExpectedMemoryLeak;
begin
  {Fill out the structure}
  LNewEntry.LeakAddress := ALeakedPointer;
  LNewEntry.LeakedClass := nil;
  LNewEntry.LeakSize := 0;
  LNewEntry.LeakCount := -1;
  {Remove it from the list}
  Result := LockExpectedMemoryLeaksList
    and UpdateExpectedLeakList(@ExpectedMemoryLeaks.FirstEntryByAddress, @LNewEntry);
  ExpectedMemoryLeaksListLocked := 0;
end;

function FastMM_UnregisterExpectedMemoryLeak(ALeakedObjectClass: TClass; ACount: Integer = 1): Boolean; overload;
begin
  Result := FastMM_RegisterExpectedMemoryLeak(ALeakedObjectClass, -ACount);
end;

function FastMM_UnregisterExpectedMemoryLeak(ALeakedBlockSize: NativeInt; ACount: Integer = 1): Boolean; overload;
begin
  Result := FastMM_RegisterExpectedMemoryLeak(ALeakedBlockSize, -ACount);
end;

{Returns a list of all expected memory leaks}
function FastMM_GetRegisteredMemoryLeaks: TFastMM_RegisteredMemoryLeaks;

  procedure AddEntries(AEntry: PExpectedMemoryLeak);
  var
    LInd: Integer;
  begin
    while AEntry <> nil do
    begin
      LInd := Length(Result);
      SetLength(Result, LInd + 1);
      {Add the entry}
      Result[LInd].LeakAddress := AEntry.LeakAddress;
      Result[LInd].LeakedClass := AEntry.LeakedClass;
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
    ExpectedMemoryLeaksListLocked := 0;
  end;
end;


{--------------------------------------------------}
{-------------Memory leak reporting----------------}
{--------------------------------------------------}

{Tries to account for a memory leak.  If the block is an expected leak then it is removed from the list of leaks and
the leak type is returned.}
function FastMM_PerformMemoryLeakCheck_DetectLeakType(AAddress: Pointer; ASpaceInsideBlock: NativeUInt): TMemoryLeakType;
var
  LLeak: TExpectedMemoryLeak;
begin
  Result := mltUnexpectedLeak;

  if ExpectedMemoryLeaks <> nil then
  begin
    {Check by pointer address}
    LLeak.LeakAddress := AAddress;
    LLeak.LeakedClass := nil;
    LLeak.LeakSize := 0;
    LLeak.LeakCount := -1;
    if UpdateExpectedLeakList(@ExpectedMemoryLeaks.FirstEntryByAddress, @LLeak, False) then
    begin
      Result := mltExpectedLeakRegisteredByPointer;
      Exit;
    end;

    {Check by class}
    LLeak.LeakAddress := nil;
    LLeak.LeakedClass := TClass(PNativeUInt(AAddress)^);
    LLeak.LeakSize := ASpaceInsideBlock;
    if UpdateExpectedLeakList(@ExpectedMemoryLeaks.FirstEntryByClass, @LLeak, False) then
    begin
      Result := mltExpectedLeakRegisteredByClass;
      Exit;
    end;

    {Check by size:  The block must be large enough to hold the leak}
    LLeak.LeakedClass := nil;
    if UpdateExpectedLeakList(@ExpectedMemoryLeaks.FirstEntryBySizeOnly, @LLeak, False) then
      Result := mltExpectedLeakRegisteredBySize;
  end;
end;

procedure FastMM_PerformMemoryLeakCheck_AddBlockToLeakSummary(APLeakSummary: PMemoryLeakSummary;
  ABlockUsableSize: NativeInt; ABlockContentType: NativeUInt);
var
  LPSummaryEntry: PMemoryLeakSummaryEntry;
  LChildDirection: Boolean;
  i: Integer;
begin
  {If there's no space to add another entry then we need to abort in order to avoid a potential buffer overrun.}
  if APLeakSummary.LeakCount >= Length(APLeakSummary.MemoryLeakEntries) then
    Exit;

  {Try to find the block type in the list.}
  i := 0;
  if APLeakSummary.LeakCount > 0 then
  begin
    while True do
    begin
      LPSummaryEntry := @APLeakSummary.MemoryLeakEntries[i];

      {$if CompilerVersion < 31}
      LChildDirection := False; //Workaround for spurious warning with older compilers
      {$endif}
      if ABlockUsableSize <> LPSummaryEntry.BlockUsableSize then
      begin
        LChildDirection := ABlockUsableSize > LPSummaryEntry.BlockUsableSize;
      end
      else if ABlockContentType <> LPSummaryEntry.BlockContentType then
      begin
        LChildDirection := ABlockContentType > LPSummaryEntry.BlockContentType;
      end
      else
      begin
        {Found the leak type:  Bump the count.}
        Inc(LPSummaryEntry.NumLeaks);
        Exit;
      end;

      {Navigate in the correct direction, stopping if the end of the tree has been reached.}
      i := LPSummaryEntry.ChildIndexes[LChildDirection];
      if i = 0 then
      begin
        LPSummaryEntry.ChildIndexes[LChildDirection] := APLeakSummary.LeakCount;
        Break;
      end;
    end;
  end;

  {Need to add the block type.}
  LPSummaryEntry := @APLeakSummary.MemoryLeakEntries[APLeakSummary.LeakCount];
  LPSummaryEntry.BlockUsableSize := ABlockUsableSize;
  LPSummaryEntry.BlockContentType := ABlockContentType;
  LPSummaryEntry.NumLeaks := 1;
  LPSummaryEntry.ChildIndexes[False] := 0;
  LPSummaryEntry.ChildIndexes[True] := 0;

  Inc(APLeakSummary.LeakCount);
end;

procedure FastMM_PerformMemoryLeakCheck_CallBack(const ABlockInfo: TFastMM_WalkAllocatedBlocks_BlockInfo);
var
  LPLeakSummary: PMemoryLeakSummary;
  LBlockContentType: NativeUInt;
  LTokenValues: TEventLogTokenValues;
  LTokenValueBuffer: array[0..CTokenBufferMaxWideChars - 1] of WideChar;
  LPBufferPos, LPBufferEnd: PWideChar;
begin
  LPLeakSummary := ABlockInfo.UserData;

  {Is this an expected memory leak?  If so, ignore it.}
  if FastMM_PerformMemoryLeakCheck_DetectLeakType(ABlockInfo.BlockAddress, ABlockInfo.UsableSize) <> mltUnexpectedLeak then
    Exit;

  {If individual leaks must be reported, report the leak now.}
  if mmetUnexpectedMemoryLeakDetail in (FastMM_OutputDebugStringEvents + FastMM_LogToFileEvents + FastMM_MessageBoxEvents) then
  begin
    LTokenValues := Default(TEventLogTokenValues);

    LPBufferEnd := @LTokenValueBuffer[High(LTokenValueBuffer)];
    LPBufferPos := AddTokenValues_GeneralTokens(LTokenValues, @LTokenValueBuffer, LPBufferEnd);
    AddTokenValues_BlockTokens(LTokenValues, ABlockInfo.BlockAddress, LPBufferPos, LPBufferEnd);

    LogEvent(mmetUnexpectedMemoryLeakDetail, LTokenValues);
  end;

  {Add the block to the memory leak summary.}
  LBlockContentType := DetectBlockContentType(ABlockInfo.BlockAddress, ABlockInfo.UsableSize);
  FastMM_PerformMemoryLeakCheck_AddBlockToLeakSummary(LPLeakSummary, ABlockInfo.UsableSize, LBlockContentType);
end;

procedure FastMM_PerformMemoryLeakCheck_SortNodes(var ALeakSummary: TMemoryLeakSummary);
var
  LCurrentIndex, LInsertionIndex: Integer;
  LCurEntry: TMemoryLeakSummaryEntry;
begin
  {Performs an insertion sort.  After the sort the left and right child indexes will no longer be valid.}
  for LCurrentIndex := 1 to ALeakSummary.LeakCount - 1 do
  begin
    LCurEntry := ALeakSummary.MemoryLeakEntries[LCurrentIndex];

    LInsertionIndex := LCurrentIndex;
    while LInsertionIndex > 0 do
    begin
      if ALeakSummary.MemoryLeakEntries[LInsertionIndex - 1].BlockUsableSize < LCurEntry.BlockUsableSize then
        Break;

      if (ALeakSummary.MemoryLeakEntries[LInsertionIndex - 1].BlockUsableSize = LCurEntry.BlockUsableSize)
        and (ALeakSummary.MemoryLeakEntries[LInsertionIndex - 1].BlockContentType > LCurEntry.BlockContentType) then
      begin
        Break;
      end;

      ALeakSummary.MemoryLeakEntries[LInsertionIndex] := ALeakSummary.MemoryLeakEntries[LInsertionIndex - 1];
      Dec(LInsertionIndex);
    end;

    ALeakSummary.MemoryLeakEntries[LInsertionIndex] := LCurEntry;
  end;
end;

procedure FastMM_PerformMemoryLeakCheck_LogLeakSummary(var ALeakSummary: TMemoryLeakSummary);
const
  CLeakTextMaxSize = 32768;
  CLifeFeed = #13#10;
  CLeakSizeSuffix = ': ';
  CLeakSeparator = ', ';
  CLeakMultiple = ' x ';
var
  LCurrentLeakSize: NativeInt;
  LLeakIndex: Integer;
  LLeakEntriesText, LTokenValueBuffer: array[0..CLeakTextMaxSize] of WideChar;
  LPBufferPos, LPBufferEnd, LPTokenBufferPos: PWideChar;
  LTokenValues: TEventLogTokenValues;
begin
  {Sort the leaks in ascending size and descending type order.}
  FastMM_PerformMemoryLeakCheck_SortNodes(ALeakSummary);

  {Build the leak summary entries text:  Walk the blocks from small to large, grouping leaks of the same size.}
  LCurrentLeakSize := -1;
  LPBufferPos := @LLeakEntriesText;
  LPBufferEnd := @LLeakEntriesText[High(LLeakEntriesText)];
  for LLeakIndex := 0 to ALeakSummary.LeakCount - 1 do
  begin

    {Did the leak size change?  If so, add a new line.}
    if ALeakSummary.MemoryLeakEntries[LLeakIndex].BlockUsableSize <> LCurrentLeakSize then
    begin
      LCurrentLeakSize := ALeakSummary.MemoryLeakEntries[LLeakIndex].BlockUsableSize;

      LPBufferPos := AppendTextToBuffer(CLifeFeed, Length(CLifeFeed), LPBufferPos, LPBufferEnd);
      LPBufferPos := NativeIntToTextBuffer(LCurrentLeakSize, LPBufferPos, LPBufferEnd);
      LPBufferPos := AppendTextToBuffer(CLeakSizeSuffix, Length(CLeakSizeSuffix), LPBufferPos, LPBufferEnd);
    end
    else
    begin
      LPBufferPos := AppendTextToBuffer(CLeakSeparator, Length(CLeakSeparator), LPBufferPos, LPBufferEnd);
    end;

    LPBufferPos := NativeIntToTextBuffer(ALeakSummary.MemoryLeakEntries[LLeakIndex].NumLeaks, LPBufferPos, LPBufferEnd);
    LPBufferPos := AppendTextToBuffer(CLeakMultiple, Length(CLeakMultiple), LPBufferPos, LPBufferEnd);
    LPBufferPos := BlockContentTypeToTextBuffer(ALeakSummary.MemoryLeakEntries[LLeakIndex].BlockContentType, LPBufferPos, LPBufferEnd);
  end;

  {Build the token dictionary for the leak summary.}
  LTokenValues := Default(TEventLogTokenValues);
  LPTokenBufferPos := AddTokenValues_GeneralTokens(LTokenValues, @LTokenValueBuffer,
    @LTokenValueBuffer[High(LTokenValueBuffer)]);
  AddTokenValue(LTokenValues, CEventLogTokenLeakSummaryEntries, @LLeakEntriesText,
    CharCount(LPBufferPos, @LLeakEntriesText), LPTokenBufferPos, @LTokenValueBuffer[High(LTokenValueBuffer)]);

  LogEvent(mmetUnexpectedMemoryLeakSummary, LTokenValues);
end;

procedure FastMM_PerformMemoryLeakCheck;
var
  LLeakSummary: TMemoryLeakSummary;
begin
  LLeakSummary := Default(TMemoryLeakSummary);

  FastMM_WalkBlocks(FastMM_PerformMemoryLeakCheck_CallBack, [btLargeBlock, btMediumBlock, btSmallBlock], True,
    @LLeakSummary);

  {Build the leak summary by walking all the block categories.}
  if (LLeakSummary.LeakCount > 0)
    and (mmetUnexpectedMemoryLeakSummary in (FastMM_OutputDebugStringEvents + FastMM_LogToFileEvents + FastMM_MessageBoxEvents)) then
  begin
    FastMM_PerformMemoryLeakCheck_LogLeakSummary(LLeakSummary);
  end;
end;


{--------------------------------------------------------}
{-------------Initialization/installation----------------}
{--------------------------------------------------------}

procedure FastMM_SetOptimizationStrategy(AStrategy: TFastMM_MemoryManagerOptimizationStrategy);
begin
  OptimizationStrategy := AStrategy;

  case AStrategy of

    mmosOptimizeForSpeed:
    begin
      DefaultMediumBlockSpanSize := CMaximumMediumBlockSpanSize;
    end;

    mmosOptimizeForLowMemoryUsage:
    begin
      DefaultMediumBlockSpanSize := 1024 * 1024 * 3 div 2;
    end;

  else
    begin
      DefaultMediumBlockSpanSize := 3 * 1024 * 1024;
    end;

  end;
end;

function FastMM_GetCurrentOptimizationStrategy: TFastMM_MemoryManagerOptimizationStrategy;
begin
  Result := OptimizationStrategy;
end;

{Adjacent small block managers may straddle the same cache line and thus have a false dependency.  Many CPUs also
prefetch adjacent cache lines on a cache miss (e.g. the "Adjacent Cache Line Prefetch" BIOS option), so even if the
small block managers are perfectly aligned on cache line (64-byte) boundaries, these prefetch mechanisms may still
introduce false dependencies.  We do not want the managers for frequently used block sizes to have false dependencies
between them, so the frequently used (small) sizes are interspersed with the less frequently used (larger) sizes.}
function SmallBlockManagerIndexFromSizeIndex(ASizeIndex: Integer): Integer; inline;
begin
  {Fill up the uneven slots first from the front to the back, and then the even slots from the back to the front.}
  Result := ASizeIndex * 2 + 1;
  if Result >= CSmallBlockTypeCount then
    Result := (2 * CSmallBlockTypeCount - 1) - Result;
end;

{Builds the lookup table used for translating a small block allocation request size to a small block type.}
procedure FastMM_BuildSmallBlockTypeLookupTable;
var
  LBlockSizeIndex, LSmallBlockSize, LManagerIndex, LStartIndex, LNextStartIndex, LAndValue: Integer;
begin
  {Determine the allowed small block alignments.  Under 64-bit the minimum alignment is always 16 bytes.}
  if AlignmentRequestCounters[maa64Bytes] > 0 then
    LAndValue := 63
  else if AlignmentRequestCounters[maa32Bytes] > 0 then
    LAndValue := 31
  else if (SizeOf(Pointer) = 8) or (AlignmentRequestCounters[maa16Bytes] > 0) then
    LAndValue := 15
  else
    LAndValue := 0;

  LStartIndex := 0;
  for LBlockSizeIndex := 0 to High(CSmallBlockSizes) do
  begin
    LSmallBlockSize := CSmallBlockSizes[LBlockSizeIndex];
    {Is this a valid block type for the alignment restriction?}
    if LSmallBlockSize and LAndValue = 0 then
    begin
      {Store the block type index in the appropriate slots.}
      LManagerIndex := SmallBlockManagerIndexFromSizeIndex(LBlockSizeIndex);
      LNextStartIndex := LSmallBlockSize div CSmallBlockGranularity;
      while LStartIndex < LNextStartIndex do
      begin
        SmallBlockTypeLookup[LStartIndex] := LManagerIndex;
        Inc(LStartIndex);
      end;
      {Set the start of the next block type}
      LStartIndex := LNextStartIndex;
    end;
  end;
end;

procedure FastMM_EnterMinimumAddressAlignment(AMinimumAddressAlignment: TFastMM_MinimumAddressAlignment);
var
  LOldMinimumAlignment: TFastMM_MinimumAddressAlignment;
begin
  LOldMinimumAlignment := FastMM_GetCurrentMinimumAddressAlignment;
  AtomicIncrement(AlignmentRequestCounters[AMinimumAddressAlignment]);

  {Rebuild the small block type lookup table if the minimum alignment changed.}
  if LOldMinimumAlignment <> FastMM_GetCurrentMinimumAddressAlignment then
    FastMM_BuildSmallBlockTypeLookupTable;
end;

procedure FastMM_ExitMinimumAddressAlignment(AMinimumAddressAlignment: TFastMM_MinimumAddressAlignment);
var
  LOldMinimumAlignment: TFastMM_MinimumAddressAlignment;
begin
  LOldMinimumAlignment := FastMM_GetCurrentMinimumAddressAlignment;
  AtomicDecrement(AlignmentRequestCounters[AMinimumAddressAlignment]);

  {Rebuild the small block type lookup table if the minimum alignment changed.}
  if LOldMinimumAlignment <> FastMM_GetCurrentMinimumAddressAlignment then
    FastMM_BuildSmallBlockTypeLookupTable;
end;

{Returns the current minimum address alignment in effect.}
function FastMM_GetCurrentMinimumAddressAlignment: TFastMM_MinimumAddressAlignment;
begin
  if AlignmentRequestCounters[maa64Bytes] > 0 then
    Result := maa64Bytes
  else if AlignmentRequestCounters[maa32Bytes] > 0 then
    Result := maa32Bytes
  else if (SizeOf(Pointer) = 8) or (AlignmentRequestCounters[maa16Bytes] > 0) then
    Result := maa16Bytes
  else
    Result := maa8Bytes;
end;

{Gets the optimal move procedure for the given small block size.}
function FastMM_InitializeMemoryManager_GetOptimalMoveProc(ASmallBlockSize: Integer): TMoveProc;
begin
  case ASmallBlockSize of

    {64-bit is always 16 byte aligned, so the 8 byte aligned moves are not needed under 64-bit.}
{$ifdef 32Bit}
    8: Result := @Move8;
    24: Result := @Move24;
    40: Result := @Move40;
    56: Result := @Move56;
{$endif}

    16: Result := @Move16;
    32: Result := @Move32;
    48: Result := @Move48;
    64: Result := @Move64;

  else
    begin
      if (ASmallBlockSize and 63) = 0 then
      begin
        if ASmallBlockSize < 1024 then
        begin
{$ifdef X86ASM}
          if System.TestSSE and 4 <> 0 then //Bit 2 = 1 means the CPU supports SSE2
            Result := @MoveMultipleOf64_Small_x86_SSE2
          else
{$endif}
            Result := @MoveMultipleOf64_Small;
        end
        else
          Result := @MoveMultipleOf64_Large;
      end else if (ASmallBlockSize and 31) = 0 then
      begin
{$ifdef X86ASM}
        if System.TestSSE and 4 <> 0 then //Bit 2 = 1 means the CPU supports SSE2
          Result := @MoveMultipleOf32_x86_SSE2
        else
{$endif}
          Result := @MoveMultipleOf32;
      end else if (ASmallBlockSize and 15) = 0 then
      begin
{$ifdef X86ASM}
        if System.TestSSE and 4 <> 0 then //Bit 2 = 1 means the CPU supports SSE2
          Result := @MoveMultipleOf16_x86_SSE2
        else
{$endif}
          Result := @MoveMultipleOf16;
{$ifdef 32Bit}
      {Under 64-bit there are no block sizes that are a multiple of 8.}
      end else if (ASmallBlockSize and 7) = 0 then
      begin
        Result := @MoveMultipleOf8;
{$endif}
      end
      else
      begin
        {Sanity check - should never get here.}
        System.Error(reRangeError);
        Result := nil;
      end;
    end;

  end;
end;

procedure FastMM_InitializeMemoryManager;
var
  LBlockSizeIndex, LSmallBlockSize, LArenaInd, LMinimumSmallBlockSpanSize, LBinInd, LOptimalSmallBlockSpanSize,
    LBlocksPerSpan, LManagerIndex: Integer;
  LPSmallBlockManager: PSmallBlockManager;
  LPMediumBlockManager: PMediumBlockManager;
  LPLargeBlockManager: PLargeBlockManager;
  LPBin: PPointer;
begin
  {---------Bug checks-------}

  {$if CSmallBlockHeaderSize <> 2} {$message error 'Small block header size must be 2 bytes'} {$endif}
  {$if CMediumBlockHeaderSize <> 8} {$message error 'Medium block header size must be 8 bytes'} {$endif}
  {$if CLargeBlockHeaderSize and 63 <> 0} {$message error 'Large block header size must be multiple of 64 bytes'} {$endif}
  {In order to ensure minimum alignment is always honoured the debug block header must be a multiple of 64.}
  {$if CDebugBlockHeaderSize and 63 <> 0} {$message error 'Debug block header must be a multiple of 64 bytes'} {$endif}

  {Span headers have to be a multiple of 64 bytes in order to ensure that 64-byte alignment of user data is possible.}
  {$if CSmallBlockSpanHeaderSize and 63 <> 0} {$message error 'Small block span header size must be multiple of 64 bytes'} {$endif}
  {$if CMediumBlockSpanHeaderSize and 63 <> 0} {$message error 'Medium block span header size must be multiple of 64 bytes'} {$endif}

  {$if CSmallBlockManagerSize and 63 <> 0} {$message error 'Small block manager size must be a multiple of 64 bytes'} {$endif}
  {$if CSmallBlockManagerSize <> (1 shl CSmallBlockManagerSizeBits)} {$message error 'Small block manager size mismatch'} {$endif}

  {$if CLargeBlockManagerSize and 63 <> 0} {$message error 'Large block manager size must be a multiple of 64 bytes'} {$endif}

  {---------General configuration-------}

  FastMM_SetOptimizationStrategy(mmosBalanced);

  GetMemoryManager(PreviousMemoryManager);
  InstalledMemoryManager := PreviousMemoryManager;
  if IsMemoryManagerSet then
    CurrentInstallationState := mmisOtherThirdPartyMemoryManagerInstalled;

  {---------Small blocks-------}

  {Build the request size to small block type lookup table.}
  FastMM_BuildSmallBlockTypeLookupTable;

  {Initialize all the small block arenas}
  for LBlockSizeIndex := 0 to CSmallBlockTypeCount - 1 do
  begin
    LSmallBlockSize := CSmallBlockSizes[LBlockSizeIndex];

    {The minimum useable small block span size.  The first small block's header is inside the span header, so we need
    space for one less small block heaader.}
    LMinimumSmallBlockSpanSize := RoundUserSizeUpToNextMediumBlockBin(
      CMinimumSmallBlocksPerSpan * LSmallBlockSize + (CSmallBlockSpanHeaderSize - CSmallBlockHeaderSize));
    if LMinimumSmallBlockSpanSize < CMinimumMediumBlockSize then
      LMinimumSmallBlockSpanSize := CMinimumMediumBlockSize;

    {The optimal small block span size is rounded so as to minimize wastage due to a partial last block.}
    LOptimalSmallBlockSpanSize := LSmallBlockSize * COptimalSmallBlocksPerSpan;
    if LOptimalSmallBlockSpanSize < COptimalSmallBlockSpanSizeLowerLimit then
      LOptimalSmallBlockSpanSize := COptimalSmallBlockSpanSizeLowerLimit;
    if LOptimalSmallBlockSpanSize > COptimalSmallBlockSpanSizeUpperLimit then
      LOptimalSmallBlockSpanSize := COptimalSmallBlockSpanSizeUpperLimit;
    LBlocksPerSpan := LOptimalSmallBlockSpanSize div LSmallBlockSize;
    {The first small block's header is inside the span header, so we need space for one less small block heaader.}
    LOptimalSmallBlockSpanSize := RoundUserSizeUpToNextMediumBlockBin(LBlocksPerSpan * LSmallBlockSize
      + (CSmallBlockSpanHeaderSize - CSmallBlockHeaderSize));

    {Small block managers are not kept in memory in size order, because they may straddle the same cache lines (or may
    be prefetched together) and we want to avoid false dependencies between frequently used managers (usually similarly
    sized small blocks).}
    LManagerIndex := SmallBlockManagerIndexFromSizeIndex(LBlockSizeIndex);

    for LArenaInd := 0 to CFastMM_SmallBlockArenaCount - 1 do
    begin
      LPSmallBlockManager := @SmallBlockManagers[LArenaInd, LManagerIndex];

      {The circular list is empty initially.}
      LPSmallBlockManager.FirstPartiallyFreeSpan := PSmallBlockSpanHeader(LPSmallBlockManager);
      LPSmallBlockManager.LastPartiallyFreeSpan := PSmallBlockSpanHeader(LPSmallBlockManager);

      LPSmallBlockManager.LastSmallBlockSequentialFeedOffset.IntegerAndABACounter := 0;
      LPSmallBlockManager.BlockSize := LSmallBlockSize;
      LPSmallBlockManager.MinimumSpanSize := LMinimumSmallBlockSpanSize;
      LPSmallBlockManager.OptimalSpanSize := LOptimalSmallBlockSpanSize;

      LPSmallBlockManager.UpsizeMoveProcedure := FastMM_InitializeMemoryManager_GetOptimalMoveProc(
        LPSmallBlockManager.BlockSize);

    end;
  end;

  {---------Medium blocks-------}
  for LArenaInd := 0 to CFastMM_MediumBlockArenaCount - 1 do
  begin
    LPMediumBlockManager := @MediumBlockManagers[LArenaInd];

    {The circular list of spans is empty initially.}
    LPMediumBlockManager.FirstMediumBlockSpanHeader := PMediumBlockSpanHeader(LPMediumBlockManager);
    LPMediumBlockManager.LastMediumBlockSpanHeader := PMediumBlockSpanHeader(LPMediumBlockManager);

    {All the free block bins are empty.}
    for LBinInd := 0 to CMediumBlockBinCount - 1 do
    begin
      LPBin := @LPMediumBlockManager.FirstFreeBlockInBin[LBinInd];
      LPBin^ := LPBin;
    end;

  end;

  {---------Large blocks-------}

  {The circular list is empty initially.}
  for LArenaInd := 0 to CFastMM_LargeBlockArenaCount - 1 do
  begin
    LPLargeBlockManager := @LargeBlockManagers[LArenaInd];

    LPLargeBlockManager.FirstLargeBlockHeader := PLargeBlockHeader(LPLargeBlockManager);
    LPLargeBlockManager.LastLargeBlockHeader := PLargeBlockHeader(LPLargeBlockManager)
  end;

  {---------Debug setup-------}
  {Reserve 64K starting at address $80800000.  $80808080 is the debug fill pattern under 32-bit, so we don't want any
  pointer dereferences at this address to succeed.  This is only necessary under 32-bit, since $8080808000000000 is
  already reserved for the OS under 64-bit.}
{$ifdef 32Bit}
  OS_AllocateVirtualMemoryAtAddress(Pointer($80800000), $10000, True);
{$endif}

  FastMM_GetStackTrace := @FastMM_NoOpGetStackTrace;
  FastMM_ConvertStackTraceToText := FastMM_NoOpConvertStackTraceToText;
  {The first time EnterDebugMode is called an attempt will be made to load the debug support DLL.}
  DebugSupportConfigured := False;

  FastMM_SetDefaultEventLogFilename;

  {---------Sharing setup-------}

  FastMM_BuildFileMappingObjectName;
end;

procedure FastMM_FreeAllMemory;
var
  LArenaIndex, LBinIndex, LBlockTypeIndex: Integer;
  LPMediumBlockManager: PMediumBlockManager;
  LPMediumBlockSpan, LPNextMediumBlockSpan: PMediumBlockSpanHeader;
  LPSmallBlockArena: PSmallBlockArena;
  LPSmallBlockManager: PSmallBlockManager;
  LPLargeBlockManager: PLargeBlockManager;
  LPLargeBlock, LPNextLargeBlock: PLargeBlockHeader;
begin
  {Free all medium block spans.}
  for LArenaIndex := 0 to CFastMM_MediumBlockArenaCount - 1 do
  begin
    LPMediumBlockManager := @MediumBlockManagers[LArenaIndex];
    LPMediumBlockSpan := LPMediumBlockManager.FirstMediumBlockSpanHeader;
    while NativeUInt(LPMediumBlockSpan) <> NativeUInt(LPMediumBlockManager) do
    begin
      LPNextMediumBlockSpan := LPMediumBlockSpan.NextMediumBlockSpanHeader;
      OS_FreeVirtualMemory(LPMediumBlockSpan);
      LPMediumBlockSpan := LPNextMediumBlockSpan;
    end;

    LPMediumBlockManager.FirstMediumBlockSpanHeader := PMediumBlockSpanHeader(LPMediumBlockManager);
    LPMediumBlockManager.LastMediumBlockSpanHeader := PMediumBlockSpanHeader(LPMediumBlockManager);

    LPMediumBlockManager.MediumBlockBinGroupBitmap := 0;
    FilLChar(LPMediumBlockManager.MediumBlockBinBitmaps, SizeOf(LPMediumBlockManager.MediumBlockBinBitmaps), 0);
    for LBinIndex := 0 to CMediumBlockBinCount - 1 do
      LPMediumBlockManager.FirstFreeBlockInBin[LBinIndex] := @LPMediumBlockManager.FirstFreeBlockInBin[LBinIndex];
    LPMediumBlockManager.LastMediumBlockSequentialFeedOffset.IntegerValue := 0;
    LPMediumBlockManager.SequentialFeedMediumBlockSpan := nil;
    LPMediumBlockManager.PendingFreeList := nil;
  end;

  {Clear all small block types}
  for LArenaIndex := 0 to High(SmallBlockManagers) do
  begin
    LPSmallBlockArena := @SmallBlockManagers[LArenaIndex];

    for LBlockTypeIndex := 0 to CSmallBlockTypeCount - 1 do
    begin
      LPSmallBlockManager := @LPSmallBlockArena[LBlockTypeIndex];
      LPSmallBlockManager.FirstPartiallyFreeSpan := PSmallBlockSpanHeader(LPSmallBlockManager);
      LPSmallBlockManager.LastPartiallyFreeSpan := PSmallBlockSpanHeader(LPSmallBlockManager);
      LPSmallBlockManager.LastSmallBlockSequentialFeedOffset.IntegerValue := 0;
      LPSmallBlockManager.SequentialFeedSmallBlockSpan := nil;
      LPSmallBlockManager.PendingFreeList := nil;
    end;
  end;

  {Free all large blocks.}
  for LArenaIndex := 0 to CFastMM_LargeBlockArenaCount - 1 do
  begin
    LPLargeBlockManager := @LargeBlockManagers[LArenaIndex];

    LPLargeBlock := LPLargeBlockManager.FirstLargeBlockHeader;
    while NativeUInt(LPLargeBlock) <> NativeUInt(LPLargeBlockManager) do
    begin
      LPNextLargeBlock := LPLargeBlock.NextLargeBlockHeader;
      FastMM_FreeMem_FreeLargeBlock_ReleaseVM(LPLargeBlock);
      LPLargeBlock := LPNextLargeBlock;
    end;

    LPLargeBlockManager.FirstLargeBlockHeader := PLargeBlockHeader(LPLargeBlockManager);
    LPLargeBlockManager.LastLargeBlockHeader := PLargeBlockHeader(LPLargeBlockManager);
  end;

end;

procedure FastMM_FinalizeMemoryManager;
begin
  ReleaseEmergencyReserveAddressSpace;

  if ExpectedMemoryLeaks <> nil then
  begin
    OS_FreeVirtualMemory(ExpectedMemoryLeaks);
    ExpectedMemoryLeaks := nil;
  end;

  FastMM_FreeDebugSupportLibrary;

  if SharingFileMappingObjectHandle <> 0 then
  begin
    CloseHandle(SharingFileMappingObjectHandle);
    SharingFileMappingObjectHandle := 0;
  end;

end;

{Returns True if FastMM was successfully installed.}
function FastMM_GetInstallationState: TFastMM_MemoryManagerInstallationState;
begin
  Result := CurrentInstallationState;
end;

function FastMM_SetNormalOrDebugMemoryManager: Boolean;
var
  LNewMemoryManager: TMemoryManagerEx;
begin
  {SetMemoryManager is not thread safe.}
  while AtomicCmpExchange(SettingMemoryManager, 1, 0) <> 0 do
    OS_AllowOtherThreadToRun;

  {Check that the memory manager has not been changed since the last time it was set.}
  if FastMM_InstalledMemoryManagerChangedExternally then
  begin
    SettingMemoryManager := 0;
    Exit(False);
  end;

  {Debug mode or normal memory manager?}
  if DebugModeCounter <= 0 then
  begin
    LNewMemoryManager.GetMem := FastMM_GetMem;
    LNewMemoryManager.FreeMem := FastMM_FreeMem;
    LNewMemoryManager.ReallocMem := FastMM_ReallocMem;
    LNewMemoryManager.AllocMem := FastMM_AllocMem;
    LNewMemoryManager.RegisterExpectedMemoryLeak := FastMM_RegisterExpectedMemoryLeak;
    LNewMemoryManager.UnregisterExpectedMemoryLeak := FastMM_UnregisterExpectedMemoryLeak;
  end
  else
  begin
    LNewMemoryManager.GetMem := FastMM_DebugGetMem;
    LNewMemoryManager.FreeMem := FastMM_DebugFreeMem;
    LNewMemoryManager.ReallocMem := FastMM_DebugReallocMem;
    LNewMemoryManager.AllocMem := FastMM_DebugAllocMem;
    LNewMemoryManager.RegisterExpectedMemoryLeak := FastMM_RegisterExpectedMemoryLeak;
    LNewMemoryManager.UnregisterExpectedMemoryLeak := FastMM_UnregisterExpectedMemoryLeak;
  end;

  SetMemoryManager(LNewMemoryManager);
  InstalledMemoryManager := LNewMemoryManager;

  SettingMemoryManager := 0;

  Result := True;
end;

procedure FastMM_InstallMemoryManager;
var
  LTokenValues: TEventLogTokenValues;
  LTokenValueBuffer: array[0..CTokenBufferMaxWideChars - 1] of WideChar;
begin
  {FastMM may only be installed if no other replacement memory manager has already been installed, and no memory has
  been allocated through the default memory manager.}
  if CurrentInstallationState <> mmisDefaultMemoryManagerInUse then
  begin
    LTokenValues := Default(TEventLogTokenValues);
    AddTokenValues_GeneralTokens(LTokenValues, @LTokenValueBuffer, @LTokenValueBuffer[High(LTokenValueBuffer)]);
    LogEvent(mmetAnotherThirdPartyMemoryManagerAlreadyInstalled, LTokenValues);

    Exit;
  end;

  if System.GetHeapStatus.TotalAllocated <> 0 then
  begin
    LTokenValues := Default(TEventLogTokenValues);
    AddTokenValues_GeneralTokens(LTokenValues, @LTokenValueBuffer, @LTokenValueBuffer[High(LTokenValueBuffer)]);
    LogEvent(mmetCannotInstallAfterDefaultMemoryManagerHasBeenUsed, LTokenValues);

    Exit;
  end;

  if FastMM_SetNormalOrDebugMemoryManager then
  begin
    CurrentInstallationState := mmisInstalled;

    EnsureEmergencyReserveAddressSpaceAllocated;
  end;
end;

procedure FastMM_UninstallMemoryManager;
begin
  if CurrentInstallationState in [mmisInstalled, mmisUsingSharedMemoryManager] then
  begin
    {Has another memory manager been installed by external code?  If so, it is not possible to uninstall.}
    if not FastMM_InstalledMemoryManagerChangedExternally then
    begin
      SetMemoryManager(PreviousMemoryManager);
      InstalledMemoryManager := PreviousMemoryManager;
      CurrentInstallationState := mmisDefaultMemoryManagerInUse;
    end;
  end;
end;

function FastMM_LoadDebugSupportLibrary: Boolean;
begin
  {Already loaded?  If so, return success.}
  if DebugSupportLibraryHandle <> 0 then
    Exit(True);

  DebugSupportLibraryHandle := LoadLibrary(FastMM_DebugSupportLibraryName);
  if DebugSupportLibraryHandle <> 0 then
  begin
    DebugLibrary_GetRawStackTrace := GetProcAddress(DebugSupportLibraryHandle, 'GetRawStackTrace');
    DebugLibrary_GetFrameBasedStackTrace := GetProcAddress(DebugSupportLibraryHandle, 'GetFrameBasedStackTrace');
    DebugLibrary_LogStackTrace_Legacy := GetProcAddress(DebugSupportLibraryHandle, 'LogStackTrace');

    {Try to use the stack trace routines from the debug support library, if available.}
    if (@FastMM_GetStackTrace = @FastMM_NoOpGetStackTrace)
      and Assigned(DebugLibrary_GetRawStackTrace) then
    begin
      FastMM_GetStackTrace := DebugLibrary_GetRawStackTrace;
    end;

    if (@FastMM_ConvertStackTraceToText = @FastMM_NoOpConvertStackTraceToText)
      and Assigned(DebugLibrary_LogStackTrace_Legacy) then
    begin
      FastMM_ConvertStackTraceToText := FastMM_DebugLibrary_LegacyLogStackTrace_Wrapper;
    end;

    Result := True;
  end
  else
    Result := False;
end;

function FastMM_FreeDebugSupportLibrary: Boolean;
begin
  if DebugSupportLibraryHandle = 0 then
    Exit(False);

  if (@FastMM_GetStackTrace = @DebugLibrary_GetRawStackTrace)
    or (@FastMM_GetStackTrace = @DebugLibrary_GetFrameBasedStackTrace) then
  begin
    FastMM_GetStackTrace := @FastMM_NoOpGetStackTrace;
  end;

  if @FastMM_ConvertStackTraceToText = @FastMM_DebugLibrary_LegacyLogStackTrace_Wrapper then
  begin
    FastMM_ConvertStackTraceToText := @FastMM_NoOpConvertStackTraceToText;
  end;

  FreeLibrary(DebugSupportLibraryHandle);
  DebugSupportLibraryHandle := 0;

  Result := True;
end;

procedure FastMM_ConfigureDebugMode;
begin
  {If both handlers have been assigned then we do not need to load the support DLL.}
  if (@FastMM_GetStackTrace = @FastMM_NoOpGetStackTrace)
    or (@FastMM_ConvertStackTraceToText = @FastMM_NoOpConvertStackTraceToText) then
  begin
    FastMM_LoadDebugSupportLibrary;
  end;

  DebugSupportConfigured := True;
end;

function FastMM_EnterDebugMode: Boolean;
begin
  if CurrentInstallationState = mmisInstalled then
  begin
    if AtomicIncrement(DebugModeCounter) = 1 then
    begin
      if not DebugSupportConfigured then
        FastMM_ConfigureDebugMode;

      Result := FastMM_SetNormalOrDebugMemoryManager
    end
    else
      Result := True;
  end
  else
    Result := False;
end;

function FastMM_ExitDebugMode: Boolean;
begin
  if CurrentInstallationState = mmisInstalled then
  begin
    if AtomicDecrement(DebugModeCounter) = 0 then
      Result := FastMM_SetNormalOrDebugMemoryManager
    else
      Result := True;
  end
  else
    Result := False;
end;

function FastMM_DebugModeActive: Boolean;
begin
  Result := DebugModeCounter > 0;
end;

procedure FastMM_ApplyLegacyConditionalDefines;
begin
  {This procedure provides backward compatibility with the conditional defines of FastMM4.}

  {$ifdef ClearLogFileOnStartup}
  FastMM_DeleteEventLogFile;
  {$endif}

  {$ifdef Align16Bytes}
  FastMM_EnterMinimumAddressAlignment(maa16Bytes);
  {$endif}

  {$ifdef EnableMemoryLeakReporting}
  {$ifdef RequireDebuggerPresenceForLeakReporting}
  if DebugHook <> 0 then
  {$endif}
  begin
    FastMM_LogToFileEvents := FastMM_LogToFileEvents + [mmetUnexpectedMemoryLeakDetail, mmetUnexpectedMemoryLeakSummary];
    FastMM_MessageBoxEvents := FastMM_MessageBoxEvents + [mmetUnexpectedMemoryLeakSummary];
  end;
  {$endif}

  {$ifdef NoMessageBoxes}
  FastMM_MessageBoxEvents := [];
  {$endif}

  {$ifdef FullDebugModeWhenDLLAvailable}
  {$define FullDebugMode}
  {$endif}

  {$ifdef FullDebugMode}
  if FastMM_LoadDebugSupportLibrary then
  begin
    FastMM_EnterDebugMode;
  end
  else
  begin
    {$ifndef FullDebugModeWhenDLLAvailable}
    {Exception handling is not yet in place, so show an error message and terminate the application.}
    OS_ShowMessageBox(FastMM_DebugSupportLibraryNotAvailableError, FastMM_DebugSupportLibraryNotAvailableError_Caption);
    {Return error code 217 - the same error code that would be returned for an exception in an initialization section.}
    Halt(217);
    {$endif}
  end;
  {$endif}

  {$ifdef ShareMM}
  {$ifndef ShareMMIfLibrary}
  if not IsLibrary then
  {$endif}
    FastMM_ShareMemoryManager;
  {$endif}

  {$ifdef AttemptToUseSharedMM}
  FastMM_AttemptToUseSharedMemoryManager;
  {$endif}
end;

procedure FastMM_SetDefaultEventLogFilename;
const
  CLogFilePathEnvironmentVariable: PWideChar = 'FastMMLogFilePath';
  CLogFileExtension: PWideChar = '_MemoryManager_EventLog.txt';
var
  LModuleFilename: array[0..CFilenameMaxLength] of WideChar;
  LPModuleFilenamePos, LPModuleFilenameStart, LPModuleFilenameEnd, LPBufferPos, LPBufferEnd: PWideChar;
begin
  {Get the module filename into a buffer.}
  LPModuleFilenameEnd := OS_GetApplicationFilename(@LModuleFilename, @LModuleFilename[High(LModuleFilename)], False);

  {Drop the file extension from the module filename.}
  LPModuleFilenamePos := LPModuleFilenameEnd;
  while NativeUInt(LPModuleFilenamePos) > NativeUInt(@LModuleFilename) do
  begin
    if LPModuleFilenamePos^ = '.' then
    begin
      LPModuleFilenameEnd := LPModuleFilenamePos;
      Break;
    end;
    Dec(LPModuleFilenamePos);
  end;
  LPModuleFilenameEnd^ := #0;

  {Try to get the path override from the environment variable.  If there is a path override then that is used instead
  of the application path.}
  LPBufferEnd := @EventLogFilename[High(EventLogFilename)];
  LPBufferPos := OS_GetEnvironmentVariableValue(CLogFilePathEnvironmentVariable, @EventLogFilename, LPBufferEnd);
  if LPBufferPos <> @EventLogFilename then
  begin
    {Strip the trailing path separator from the path override.}
    Dec(LPBufferPos);
    if (LPBufferPos^ <> '\') and (LPBufferPos^ <> '/') then
      Inc(LPBufferPos);

    {Strip the path from the module filename.}
    LPModuleFilenameStart := LPModuleFilenameEnd;
    while NativeUInt(LPModuleFilenameStart) > NativeUInt(@LModuleFilename) do
    begin
      if (LPModuleFilenameStart^ = '\') or (LPModuleFilenameStart^ = '/') then
        Break;
      Dec(LPModuleFilenameStart);
    end;
  end
  else
    LPModuleFilenameStart := @LModuleFilename;

  LPBufferPos := AppendTextToBuffer(LPModuleFilenameStart, LPBufferPos, LPBufferEnd);
  LPBufferPos := AppendTextToBuffer(CLogFileExtension, LPBufferPos, LPBufferEnd);
  LPBufferPos^ := #0;
end;

procedure FastMM_SetEventLogFilename(APEventLogFilename: PWideChar);
var
  LPBufferPos, LPBufferEnd: PWideChar;
begin
  if APEventLogFilename <> nil then
  begin
    LPBufferEnd := @EventLogFilename[High(EventLogFilename)];
    LPBufferPos := AppendTextToBuffer(APEventLogFilename, @EventLogFilename, LPBufferEnd);
    LPBufferPos^ := #0;
  end
  else
    FastMM_SetDefaultEventLogFilename;
end;

function FastMM_GetEventLogFilename: PWideChar;
begin
  Result := @EventLogFilename;
end;

function FastMM_DeleteEventLogFile: Boolean;
begin
  Result := OS_DeleteFile(@EventLogFilename);
end;

initialization
  FastMM_InitializeMemoryManager;
  FastMM_InstallMemoryManager;

  {If installation was successful, check for any legacy FastMM4 conditional defines and adjust the configuration
  accordingly.}
  if CurrentInstallationState = mmisInstalled then
    FastMM_ApplyLegacyConditionalDefines;

finalization

  {Prevent a potential crash when the finalization code in system.pas tries to free PreferredLanguagesOverride after
  FastMM has been uninstalled:  https://quality.embarcadero.com/browse/RSP-16796}
  if CurrentInstallationState = mmisInstalled then
    SetLocaleOverride('');

  {All pending frees must be released before we can do a leak check.}
  FastMM_ProcessAllPendingFrees;

  {Backward compatibility: If ReportMemoryLeaksOnShutdown = True then display the the leak summary.}
  if ReportMemoryLeaksOnShutdown then
    Include(FastMM_MessageBoxEvents, mmetUnexpectedMemoryLeakSummary);

  {Do a memory leak check if required.}
  if [mmetUnexpectedMemoryLeakDetail, mmetUnexpectedMemoryLeakSummary] * (FastMM_OutputDebugStringEvents + FastMM_LogToFileEvents + FastMM_MessageBoxEvents) <> [] then
    FastMM_PerformMemoryLeakCheck;

  FastMM_FinalizeMemoryManager;
  FastMM_UninstallMemoryManager;

  {Free all memory.  If this is a .DLL that owns its own memory manager, then it is necessary to prevent the main
  application from running out of address space.}
  FastMM_FreeAllMemory;

end.
