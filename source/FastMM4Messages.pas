{

Fast Memory Manager: Messages

English translation by Pierre le Riche.

}

unit FastMM4Messages;

interface

{$Include FastMM4Options.inc}

const
  {The name of the debug info support DLL}
  FullDebugModeLibraryName32Bit = 'FastMM_FullDebugMode.dll';
  FullDebugModeLibraryName64Bit = 'FastMM_FullDebugMode64.dll';
  {Event log strings}
  LogFileExtension = '_MemoryManager_EventLog.txt'#0;
  CRLF = #13#10;
  EventSeparator = '--------------------------------';
  {Class name messages}
  UnknownClassNameMsg = 'Unknown';
  {Memory dump message}
  MemoryDumpMsg = #13#10#13#10'Current memory dump of 256 bytes starting at pointer address ';
  {Block Error Messages}
  BlockScanLogHeader = 'Allocated block logged by LogAllocatedBlocksToFile. The size is: ';
  ErrorMsgHeader = 'FastMM has detected an error during a ';
  GetMemMsg = 'GetMem';
  FreeMemMsg = 'FreeMem';
  ReallocMemMsg = 'ReallocMem';
  BlockCheckMsg = 'free block scan';
  OperationMsg = ' operation. ';
  BlockHeaderCorruptedMsg = 'The block header has been corrupted. ';
  BlockFooterCorruptedMsg = 'The block footer has been corrupted. ';
  FreeModifiedErrorMsg = 'FastMM detected that a block has been modified after being freed. ';
  FreeModifiedDetailMsg = #13#10#13#10'Modified byte offsets (and lengths): ';
  DoubleFreeErrorMsg = 'An attempt has been made to free/reallocate an unallocated block.';
  WrongMMFreeErrorMsg = 'An attempt has been made to free/reallocate a block that was allocated through a different FastMM instance. Check your memory manager sharing settings.';
  PreviousBlockSizeMsg = #13#10#13#10'The previous block size was: ';
  CurrentBlockSizeMsg = #13#10#13#10'The block size is: ';
  PreviousObjectClassMsg = #13#10#13#10'The block was previously used for an object of class: ';
  CurrentObjectClassMsg = #13#10#13#10'The block is currently used for an object of class: ';
  PreviousAllocationGroupMsg = #13#10#13#10'The allocation group was: ';
  PreviousAllocationNumberMsg = #13#10#13#10'The allocation number was: ';
  CurrentAllocationGroupMsg = #13#10#13#10'The allocation group is: ';
  CurrentAllocationNumberMsg = #13#10#13#10'The allocation number is: ';
  BlockErrorMsgTitle = 'Memory Error Detected';
  VirtualMethodErrorHeader = 'FastMM has detected an attempt to call a virtual method on a freed object. An access violation will now be raised in order to abort the current operation.';
  InterfaceErrorHeader = 'FastMM has detected an attempt to use an interface of a freed object. An access violation will now be raised in order to abort the current operation.';
  BlockHeaderCorruptedNoHistoryMsg = ' Unfortunately the block header has been corrupted so no history is available.';
  FreedObjectClassMsg = #13#10#13#10'Freed object class: ';
  VirtualMethodName = #13#10#13#10'Virtual method: ';
  VirtualMethodOffset = 'Offset +';
  VirtualMethodAddress = #13#10#13#10'Virtual method address: ';
  {Stack trace messages}
  CurrentThreadIDMsg = #13#10#13#10'The current thread ID is 0x';
  CurrentStackTraceMsg = ', and the stack trace (return addresses) leading to this error is:';
  ThreadIDPrevAllocMsg = #13#10#13#10'This block was previously allocated by thread 0x';
  ThreadIDAtAllocMsg = #13#10#13#10'This block was allocated by thread 0x';
  ThreadIDAtFreeMsg = #13#10#13#10'The block was previously freed by thread 0x';
  ThreadIDAtObjectAllocMsg = #13#10#13#10'The object was allocated by thread 0x';
  ThreadIDAtObjectFreeMsg = #13#10#13#10'The object was subsequently freed by thread 0x';
  StackTraceMsg = ', and the stack trace (return addresses) at the time was:';
  {Installation Messages}
  AlreadyInstalledMsg = 'FastMM4 is already installed.';
  AlreadyInstalledTitle = 'Already installed.';
  OtherMMInstalledMsg = 'FastMM4 cannot be installed since another third party memory '
    + 'manager has already installed itself.'#13#10'If you want to use FastMM4, '
    + 'please make sure that FastMM4.pas is the very first unit in the "uses"'
    + #13#10'section of your project''s .dpr file.';
  OtherMMInstalledTitle = 'Cannot install FastMM4 - Another memory manager is already installed';
  MemoryAllocatedMsg = 'FastMM4 cannot install since memory has already been '
    + 'allocated through the default memory manager.'#13#10'FastMM4.pas MUST '
    + 'be the first unit in your project''s .dpr file, otherwise memory may '
    + 'be allocated'#13#10'through the default memory manager before FastMM4 '
    + 'gains control. '#13#10#13#10'If you are using an exception trapper '
    + 'like MadExcept (or any tool that modifies the unit initialization '
    + 'order),'#13#10'go into its configuration page and ensure that the '
    + 'FastMM4.pas unit is initialized before any other unit.';
  MemoryAllocatedTitle = 'Cannot install FastMM4 - Memory has already been allocated';
  {Leak checking messages}
  LeakLogHeader = 'A memory block has been leaked. The size is: ';
  LeakMessageHeader = 'This application has leaked memory. ';
  SmallLeakDetail = 'The small block leaks are'
{$ifdef HideExpectedLeaksRegisteredByPointer}
    + ' (excluding expected leaks registered by pointer)'
{$endif}
    + ':'#13#10;
  LargeLeakDetail = 'The sizes of leaked medium and large blocks are'
{$ifdef HideExpectedLeaksRegisteredByPointer}
    + ' (excluding expected leaks registered by pointer)'
{$endif}
    + ': ';
  BytesMessage = ' bytes: ';
  AnsiStringBlockMessage = 'AnsiString';
  UnicodeStringBlockMessage = 'UnicodeString';
  LeakMessageFooter = #13#10
{$ifndef HideMemoryLeakHintMessage}
    + #13#10'Note: '
  {$ifdef RequireIDEPresenceForLeakReporting}
    + 'This memory leak check is only performed if Delphi is currently running on the same computer. '
  {$endif}
  {$ifdef FullDebugMode}
    {$ifdef LogMemoryLeakDetailToFile}
    + 'Memory leak detail is logged to a text file in the same folder as this application. '
    {$else}
    + 'Enable the "LogMemoryLeakDetailToFile" to obtain a log file containing detail on memory leaks. '
    {$endif}
  {$else}
    + 'To obtain a log file containing detail on memory leaks, enable the "FullDebugMode" and "LogMemoryLeakDetailToFile" conditional defines. '
  {$endif}
    + 'To disable this memory leak check, undefine "EnableMemoryLeakReporting".'#13#10
{$endif}
    + #0;
  LeakMessageTitle = 'Memory Leak Detected';
{$ifdef UseOutputDebugString}
  FastMMInstallMsg = 'FastMM has been installed.';
  FastMMInstallSharedMsg = 'Sharing an existing instance of FastMM.';
  FastMMUninstallMsg = 'FastMM has been uninstalled.';
  FastMMUninstallSharedMsg = 'Stopped sharing an existing instance of FastMM.';
{$endif}
{$ifdef DetectMMOperationsAfterUninstall}
  InvalidOperationTitle = 'MM Operation after uninstall.';
  InvalidGetMemMsg = 'FastMM has detected a GetMem call after FastMM was uninstalled.';
  InvalidFreeMemMsg = 'FastMM has detected a FreeMem call after FastMM was uninstalled.';
  InvalidReallocMemMsg = 'FastMM has detected a ReallocMem call after FastMM was uninstalled.';
  InvalidAllocMemMsg = 'FastMM has detected an AllocMem call after FastMM was uninstalled.';
{$endif}

implementation

end.

