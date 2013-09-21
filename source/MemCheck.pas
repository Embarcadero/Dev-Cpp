(*
MemCheck: the ultimate memory troubles hunter
Created by: Jean Marc Eber & Vincent Mahon, Société Générale, INFI/SGOP/R&D
Version 2.75	-> Also update OutputFileHeader when changing the version #

Contact...
	Vincent.Mahon@free.fr
	http://v.mahon.free.fr/pro/freeware/memcheck

Mail address:
	Tour Société Générale
	Sgib/Sgop/R&D
	92987 Paris - La Défense cedex
	France

Copyrights...
The authors grant you the right to modify/change the source code as long as the original authors are mentionned.
Please let us know if you make any improvements, so that we can keep an up to date version. We also welcome
all comments, preferably by email.

Portions of this file (all the code dealing with TD32 debug information) where derived from the following work, with permission.
Reuse of this code in a commercial application is not permitted. The portions are identified by a copyright notice.
> DumpFB.C Borland 32-bit Turbo Debugger dumper (FB09 & FB0A)
> Clive Turvey, Electronics Engineer, July 1998
> Copyright (C) Tenth Planet Software Intl., Clive Turvey 1998. All rights reserved.
> Clive Turvey <clive@tbcnet.com> http://www.tbcnet.com/~clive/vcomwinp.html

Disclaimer...
You use MemCheck at your own risks. This means that you cannot hold the authors or Société Générale to be
responsible for any software\hardware problems you may encounter while using this module.

General information...
MemCheck replaces Delphi's memory manager with a home made one. This one logs information each time memory is
allocated, reallocated or freed. When the program ends, information about memory problems is provided in a log file
and exceptions are raised at problematic points.

Basic use...
Set the MemCheckLogFileName option. Call MemChk when you want to start the memory monitoring. Nothing else to do !
When your program terminates and the finalization is executed, MemCheck will report the problems. This is the
behaviour you'll obtain if you change no option in MemCheck.

Features...
- List of memory spaces not deallocated, and raising of EMemoryLeak exception at the exact place in the source code
- Call stack at allocation time. User chooses to see or not to see this call stack at run time (using ShowCallStack),
  when a EMemoryLeak is raised.
- Tracking of virtual method calls after object's destruction (we change the VMT of objects when they are destroyed)
- Tracking of method calls on an interface while the object attached to the interface has been destroyed
- Checking of writes beyond end of allocated blocks (we put a marker at the end of a block on allocation)
- Fill freed block with a byte (this allows for example to set fields of classes to Nil, or buffers to $FF, or whatever)
- Detect writes in deallocated blocks (we do this by not really deallocating block, and checking them on end - this
  can be time consuming)
- Statistics collection about objects allocation (how many objects of a given class are created ?)
- Time stamps can be indicated and will appear in the output

Options and parameters...
- You can specify the log files names (MemCheckLogFileName)
- It is possible to tell MemCheck that you are instanciating an object in a special way - See doc for
  CheckForceAllocatedType
- Clients can specify the depth of the call stack they want to store (StoredCallStackDepth)

Warnings...
- MemCheck is based on a lot of low-level hacks. Some parts of it will not work on other versions of Delphi
without being revisited (as soon as System has been recompiled, MemCheck is very likely to behave strangely,
because for example the address of InitContext will be bad).
- Some debugging tools exploit the map file to return source location information. We chose not to do that, because
we think the way MemCheck raises exceptions at the good places is better. It is still possible to use "find error"
in Delphi.
- Memcheck is not able to report accurate call stack information about a leak of a class which does not redefine
its constructor. For example, if an instance of TStringList is never deallocated, the call stack MemCheck will
report is not very complete. However, the leak is correctly reported by MemCheck.

A word about uses...
Since leaks are reported on end of execution (finalization of this unit), we need as many finalizations to occur
before memcheck's, so that if some memory is freed in these finalizations, it is not erroneously reported as leak. In order to
finalize MemCheck as late as possible, we use a trick to change the order of the list of finalizations.
Other memory managing products which are available (found easily on the internet) do not have this
problem because they just rely on putting the unit first in the DPR; but this is not safe without a build all.
In MemCheck we absolutely need to use two units: SysUtils and Windows.
Then, I decided in MemCheck 2.54 to use the unit Classes because I think it will lead to much simpler code.
We also use two units which we can use without risk since they dont have a finalization: Math and SyncObjs.
An analysis of the uses clauses of these five units shows that in fact MemCheck uses indirectly the following units:
Math, Classes, Typinfo, Consts, Variants, VaRUtils, SysUtils, ActiveX, Messages, SysConst, Windows, SyncObjs, System, SysInit and Types.
Of these, only Classes, Variants, System and SysUtils have a finalization section. I checked and it is not possible to have a leak
reported by MemCheck which is not correct because the memory would have been freed by one of these finalizations.
In the procedure ChangeFinalizationsOrder I make sure that only these four units are finalized after MemCheck (I could have decided for
the fifteen, but this would be more work, and I know it is useless).
*)
unit MemCheck;
{$A+}
{$H+}
{$IFDEF VER170}
  //VER170 = Delphi 2005 for Win32
  //Don't define DELPHI71_OR_LATER for Delphi 2005 for Win32.
  {$UNDEF DELPHI71_OR_LATER}
  {$DEFINE DELPHI6_OR_LATER}
  {$DEFINE DELPHI7_OR_LATER}
{$ENDIF}
{$IFDEF VER150}
  {$IFNDEF DELPHI70_MODE}
    {$DEFINE DELPHI71_OR_LATER}
    //If you are using Delphi 7.0 (not 7.1), then specify DELPHI70_MODE symbol in "Project/Options/Conditional defines" - Delphi 7.1 has build no. 4.453
  {$ENDIF}
  {$DEFINE DELPHI7_OR_LATER}
  {$DEFINE DELPHI6_OR_LATER}
  {$WARNINGS OFF}	//We probably don't want to hear about warnings - Not sure about that
{$ENDIF}
{$IFDEF VER140}
	{$DEFINE DELPHI6_OR_LATER}
{$ENDIF}
{$IFDEF DELPHI6_OR_LATER}
	{$WARN UNIT_PLATFORM OFF}	//NOT certified for Kylix
	{$WARN SYMBOL_PLATFORM OFF}
	{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

interface

procedure MemChk;
{Activates MemCheck and resets the allocated blocks stack.
Warning: the old stack is lost ! - It is the client's duty to commit the
releasable blocks by calling CommitReleases(AllocatedBlocks)}

procedure UnMemChk;
{sets back the memory manager that was installed before MemChk was called
If MemCheck is not active, this does not matter. The default delphi memory manager is set.
You should be very careful about calling this routine and know exactly what it does (see the FAQ on the web site)}

procedure CommitReleases;
{really releases the blocks}

procedure AddTimeStampInformation(const I: string);
{Logs the given information as associated with the current time stamp
Requires that MemCheck is active}

procedure LogSevereExceptions(const WithVersionInfo: string);
{Activates the exception logger}

function MemoryBlockCorrupted(P: Pointer): Boolean;
{Is the given block bad ?
P is a block you may for example have created with GetMem, or P can be an object.
Bad means you have written beyond the block's allocated space or the memory for this object was freed.
If P was allocated before MemCheck was launched, we return False}

function BlockAllocationAddress(P: Pointer): Pointer;
{The address at which P was allocated
If MemCheck was not running when P was allocated (ie we do not find our magic number), we return $00000000}

function IsMemCheckActive: boolean;
{Is MemCheck currently running ?
ie, is the current memory manager memcheck's ?}

function TextualDebugInfoForAddress(const TheAddress: Cardinal): string;

var
	MemCheckLogFileName: string = '';	//The file memcheck will log information to
	DeallocateFreedMemoryWhenBlockBiggerThan: Integer = 0;
	{should blocks be really deallocated when FreeMem is called ? If you want all blocks to be deallocated, set this
	constant to 0. If you want blocks to be never deallocated, set the cstte to MaxInt. When blocks are not deallocated,
	MemCheck can give information about when the second deallocation occured}

	ShowLogFileWhenUseful: Boolean = True;

const
	StoredCallStackDepth = 26;
	{Size of the call stack we store when GetMem is called, must be an EVEN number}

type
	TCallStack = array[0..StoredCallStackDepth] of Pointer;

procedure FillCallStack(var St: TCallStack; const NbLevelsToExclude: integer);
//Fills St with the call stack

function CallStackTextualRepresentation(const S: TCallStack; const LineHeader: string): string;
//Will contain CR/LFs

implementation

uses
	Windows,							{Windows has no finalization, so is OK to use with no care}
	Classes,
	Math,
	SyncObjs,
	{$IFDEF USE_JEDI_JCL}JclDebug,{$ENDIF}
	{$IFDEF DELPHI6_OR_LATER}Variants,{$ENDIF}
	SysUtils;						   {Because of this uses, SysUtils must be finalized after MemCheck - Which is necessary anyway because SysUtils calls DoneExceptions in its finalization}

type
	TKindOfMemory = (MClass, MUser, MReallocedUser);
	{MClass means the block carries an object
	MUser means the block is a buffer of unknown type (in fact we just know this is not an object)
	MReallocedUser means this block was reallocated}

const
	NoDebugInfo = '(no debug info)';
	MemCheckLogFileNameSuffix = '_MemCheck.log';

	(**************** MEMCHECK OPTIONS ********************)
	DanglingInterfacesVerified = False;
	{When an object is destroyed, should we fill the interface VMT with a special value which
	will allow tracking of calls to this interface after the object was destroyed - This incompatible with CheckWipedBlocksOnTermination, so you have to choose}

	WipeOutMemoryOnFreeMem = True;
	{This is about what is done on memory freeing:
	- for objects, this option replaces the VMT with a special one which will raise exceptions if a virtual method is called
	- for other memory kinds, this will fill the memory space with the char below}
	CharToUseToWipeOut: char = #0;
	//I choose #0 because this makes objet fields Nil, which is easier to debug. Tell me if you have a better idea !

	CheckWipedBlocksOnTermination = True and WipeOutMemoryOnFreeMem and not (DanglingInterfacesVerified);
	{When iterating on the blocks (in OutputAllocatedBlocks), we check for every block which has been deallocated that it is still
	filled with CharToUseToWipeOut.
	Warning: this is VERY time-consuming
	This is meaningful only when the blocks are wiped out on free mem
	This is incompatible with dangling interfaces checking}
	DoNotCheckWipedBlocksBiggerThan = 4000;

	CollectStatsAboutObjectAllocation = False;
	{Every time FreeMem is called for allocationg an object, this will register information about the class instanciated:
	class name, number of instances, allocated space for one instance
	Note: this has to be done on FreeMem because when GetMem is called, the VMT is not installed yet and we can not know
	this is an object}

	KeepMaxMemoryUsage = CollectStatsAboutObjectAllocation;
	{Will report the biggest memory usage during the execution}

	ComputeMemoryUsageStats = False;
	{Outputs the memory usage along the life of the execution. This output can be easily graphed, in excel for example}
	MemoryUsageStatsStep = 5;
	{Meaningful only when ComputeMemoryUsageStats
	When this is set to 5, we collect information for the stats every 5 call to GetMem, unless size is bigger than StatCollectionForce}
	StatCollectionForce = 1000;

	BlocksToShow: array[TKindOfMemory] of Boolean = (true, true, true);
	{eg if BlocksToShow[MClass] is True, the blocks allocated for class instances will be shown}

	CheckHeapStatus = False;
	// Checks that the heap has not been corrupted since last call to the memory manager
	// Warning: VERY time-consuming

	IdentifyObjectFields = False;
	IdentifyFieldsOfObjectsConformantTo: TClass = Tobject;

	MaxLeak = 1000;
	{This option tells to MemCheck not to display more than a certain quantity of leaks, so that the finalization
	phase does not take too long}

	UseDebugInfos = True;
	//Should use the debug informations which are in the executable ?

	RaiseExceptionsOnEnd = true;
	//Should we use exceptions to show memory leak information ?

	NotepadApp = 'notepad';
	//The application launched to show the log file

   (**************** END OF MEMCHECK OPTIONS ********************)

var
	ShowCallStack: Boolean;
	{When we show an allocated block, should we show the call stack that went to the allocation ? Set to false
	before each block. The usual way to use this is calling Evaluate/Modify just after an EMemoryLeak was raised}

const
	MaxListSize = MaxInt div 16 - 1;

type
	PObjectsArray = ^TObjectsArray;
	TObjectsArray = array[0..MaxListSize] of TObject;

	PStringsArray = ^TStringsArray;
	TStringsArray = array[0..99999999] of string;
	{Used to simulate string lists}

	PIntegersArray = ^TIntegersArray;
	TIntegersArray = array[0..99999999] of integer;
	{Used to simulate lists of integer}

var
	TimeStamps: PStringsArray = nil;
	{Allows associating a string of information with a time stamp}
	TimeStampsCount: integer = 0;
	{Number of time stamps in the array}
	TimeStampsAllocated: integer = 0;
	{Number of positions available in the array}

const
	DeallocateInstancesConformingTo = False;
	InstancesConformingToForDeallocation: TClass = TObject;
	{used only when BlocksToShow[MClass] is True - eg If InstancesConformingTo = TList, only blocks allocated for instances
	of TList and its heirs will be shown}

	InstancesConformingToForReporting: TClass = TObject;
	{used only when BlocksToShow[MClass] is True - eg If InstancesConformingTo = TList, only blocks allocated for instances
	of TList and its heirs will be shown}

	MaxNbSupportedVMTEntries = 200;
	{Don't change this number, its a Hack! jm}

type
	PMemoryBlocHeader = ^TMemoryBlocHeader;
	TMemoryBlocHeader = record
		{
		This is the header we put in front of a memory block
		For each memory allocation, we allocate "size requested + header size + footer size" because we keep information inside the memory zone.
		Therefore, the address returned by GetMem is: [the address we get from OldMemoryManager.GetMem] + HeaderSize.

		. DestructionAdress: an identifier telling if the bloc is active or not (when FreeMem is called we do not really free the mem).
		  Nil when the block has not been freed yet; otherwise, contains the address of the caller of the destruction. This will be useful
		  for reporting errors such as "this memory has already been freed, at address XXX".
		. PreceedingBlock: link of the linked list of allocated blocs
		. NextBlock: link of the linked list of allocated blocs
		. KindOfBlock: is the data an object or unknown kind of data (such as a buffer)
		. VMT: the classtype of the object
		. CallerAddress: an array containing the call stack at allocation time
		. AllocatedSize: the size allocated for the user (size requested by the user)
		. MagicNumber: an integer we use to recognize a block which was allocated using our own allocator
		}
		DestructionAdress: Pointer;
		PreceedingBlock: Pointer;
		NextBlock: Pointer;
		KindOfBlock: TKindOfMemory;
		VMT: TClass;
		CallerAddress: TCallStack;
		AllocatedSize: integer;		 //this is an integer because the parameter of GetMem is an integer
		LastTimeStamp: integer;		 //-1 means no time stamp
		NotUsed: Cardinal;			  //Because Size of the header must be a multiple 8
		MagicNumber: Cardinal;
	end;

	PMemoryBlockFooter = ^TMemoryBlockFooter;
	TMemoryBlockFooter = Cardinal;
	{This is the end-of-bloc marker we use to check that the user did not write beyond the allowed space}

	EMemoryLeak = class(Exception);
	EStackUnwinding = class(EMemoryLeak);
	EBadInstance = class(Exception);
	{This exception is raised when a virtual method is called on an object which has been freed}
	EFreedBlockDamaged = class(Exception);
	EInterfaceFreedInstance = class(Exception);
	{This exception is raised when a method is called on an interface whom object has been freed}

	VMTTable = array[0..MaxNbSupportedVMTEntries] of pointer;
	pVMTTable = ^VMTTable;
	TMyVMT = record
		A: array[0..19] of byte;
		B: VMTTable;
	end;

	ReleasedInstance = class
		procedure RaiseExcept;
		procedure InterfaceError; stdcall;
		procedure Error; virtual;
	end;

	TFieldInfo = class
		OwnerClass: TClass;
		FieldIndex: integer;

		constructor Create(const TheOwnerClass: TClass; const TheFieldIndex: integer);
	end;

const
	EndOfBlock: Cardinal = $FFFFFFFA;
	Magic: Cardinal = $FFFFFFFF;

var
	FreedInstance: PChar;
	BadObjectVMT: TMyVMT;
	BadInterfaceVMT: VMTTable;
	GIndex: Integer;

	LastBlock: PMemoryBlocHeader;

	MemCheckActive: boolean = False;
	{Is MemCheck currently running ?
	ie, is the current memory manager memcheck's ?}
	MemCheckInitialized: Boolean = False;
	{Has InitializeOnce been called ?
	This variable should ONLY be used by InitializeOnce and the finalization}

   {*** arrays for stats ***}
	AllocatedObjectsClasses: array of TClass;
	NbClasses: integer = 0;

	AllocatedInstances: PIntegersArray = nil; {instances counter}
	AllocStatsCount: integer = 0;
	StatsArraysAllocatedPos: integer = 0;
	{This is used to display some statistics about objects allocated. Each time an object is allocated, we look if its
	class name appears in this list. If it does, we increment the counter of class' instances for this class;
	if it does not appear, we had it with a counter set to one.}

	MemoryUsageStats: PIntegersArray = nil; {instances counter}
	MemoryUsageStatsCount: integer = 0;
	MemoryUsageStatsAllocatedPos: integer = 0;
	MemoryUsageStatsLoop: integer = -1;

	SevereExceptionsLogFile: Text;
	{This is the log file for exceptions}

	OutOfMemory: EOutOfMemory;
	// Because when we have to raise this, we do not want to have to instanciate it (as there is no memory available)

	HeapCorrupted: Exception;

	NotDestroyedFields: PIntegersArray = nil;
	NotDestroyedFieldsInfos: PObjectsArray = nil;
	NotDestroyedFieldsCount: integer = 0;
	NotDestroyedFieldsAllocatedSpace: integer = 0;

	LastHeapStatus: THeapStatus;

	MaxMemoryUsage: Integer = 0;
	// see KeepMaxMemoryUsage

	OldMemoryManager: TMemoryManager;
	//Set by the MemChk routine

type
	TIntegerBinaryTree = class
	protected
		fValue: Cardinal;
		fBigger: TIntegerBinaryTree;
		fSmaller: TIntegerBinaryTree;

		class function StoredValue(const Address: Cardinal): Cardinal;
		constructor _Create(const Address: Cardinal);
		function _Has(const Address: Cardinal): Boolean;
		procedure _Add(const Address: Cardinal);
		procedure _Remove(const Address: Cardinal);

	public
		function Has(const Address: Cardinal): Boolean;
		procedure Add(const Address: Cardinal);
		procedure Remove(const Address: Cardinal);

		property Value: Cardinal read fValue;
	end;

	PCardinal = ^Cardinal;

var
	CurrentlyAllocatedBlocksTree: TIntegerBinaryTree;

type
	TAddressToLine = class
	public
		Address: Cardinal;
		Line: Cardinal;

		constructor Create(const AAddress, ALine: Cardinal);
	end;

	PAddressesArray = ^TAddressesArray;
	TAddressesArray = array[0..MaxInt div 16 - 1] of TAddressToLine;

	TUnitDebugInfos = class
	public
		Name: string;
		Addresses: array of TAddressToLine;

		constructor Create(const AName: string; const NbLines: Cardinal);

		function LineWhichContainsAddress(const Address: Cardinal): string;
	end;

	TRoutineDebugInfos = class
	public
		Name: string;
		StartAddress: Cardinal;
		EndAddress: Cardinal;

		constructor Create(const AName: string; const AStartAddress: Cardinal; const ALength: Cardinal);
	end;

var
	Routines: array of TRoutineDebugInfos;
	RoutinesCount: integer;
	Units: array of TUnitDebugInfos;
	UnitsCount: integer;
	OutputFileHeader: string = 'MemCheck version 2.75'#13#10;
	HeapStatusSynchro : TSynchroObject;

{$IFDEF USE_JEDI_JCL}
function PointerToDebugInfo(Addr: Pointer): String; //!! by ray
var
	_file, _module, _proc: AnsiString;
	_line: Integer;
begin
	JclDebug.MapOfAddr(Addr, _file, _module, _proc, _line);
	if _file <> '' then
		Result := Format('($%p) %s:%s:%d (%s)', [Addr, _module, _proc, _line, _file])
	else
		Result := Format('($%p) %s', [Addr, NoDebugInfo]);
end;
{$ENDIF}

function BlockAllocationAddress(P: Pointer): Pointer;
var
	Block: PMemoryBlocHeader;
begin
	Block := PMemoryBlocHeader(PChar(P) - SizeOf(TMemoryBlocHeader));

	if Block.MagicNumber = Magic then
		Result := Block.CallerAddress[0]
	else
		Result := nil
end;

procedure UpdateLastHeapStatus;
begin
	LastHeapStatus := GetHeapStatus;
end;

function HeapStatusesDifferent(const Old, New: THeapStatus): boolean;
begin
	Result :=
		(Old.TotalAddrSpace <> New.TotalAddrSpace) or
		(Old.TotalUncommitted <> New.TotalUncommitted) or
		(Old.TotalCommitted <> New.TotalCommitted) or
		(Old.TotalAllocated <> New.TotalAllocated) or
		(Old.TotalFree <> New.TotalFree) or
		(Old.FreeSmall <> New.FreeSmall) or
		(Old.FreeBig <> New.FreeBig) or
		(Old.Unused <> New.Unused) or
		(Old.Overhead <> New.Overhead) or
		(Old.HeapErrorCode <> New.HeapErrorCode) or
		(New.TotalUncommitted + New.TotalCommitted <> New.TotalAddrSpace) or
		(New.Unused + New.FreeBig + New.FreeSmall <> New.TotalFree)
end;

class function TIntegerBinaryTree.StoredValue(const Address: Cardinal): Cardinal;
begin
	Result := Address shl 16;
	Result := Result or (Address shr 16);
	Result := Result xor $AAAAAAAA;
end;

constructor TIntegerBinaryTree._Create(const Address: Cardinal);
begin	//We do not call inherited Create for optimization
	fValue := Address
end;

function TIntegerBinaryTree.Has(const Address: Cardinal): Boolean;
begin
	Result := _Has(StoredValue(Address));
end;

procedure TIntegerBinaryTree.Add(const Address: Cardinal);
begin
	_Add(StoredValue(Address));
end;

procedure TIntegerBinaryTree.Remove(const Address: Cardinal);
begin
	_Remove(StoredValue(Address));
end;

function TIntegerBinaryTree._Has(const Address: Cardinal): Boolean;
begin
	if fValue = Address then
		Result := True
	else
		if (Address > fValue) and (fBigger <> nil) then
			Result := fBigger._Has(Address)
		else
			if (Address < fValue) and (fSmaller <> nil) then
				Result := fSmaller._Has(Address)
			else
				Result := False
end;

procedure TIntegerBinaryTree._Add(const Address: Cardinal);
begin
	Assert(Address <> fValue, 'TIntegerBinaryTree._Add: already in !');

	if (Address > fValue) then
		begin
			if fBigger <> nil then
				fBigger._Add(Address)
			else
				fBigger := TIntegerBinaryTree._Create(Address)
		end
	else
		begin
			if fSmaller <> nil then
				fSmaller._Add(Address)
			else
				fSmaller := TIntegerBinaryTree._Create(Address)
		end
end;

procedure TIntegerBinaryTree._Remove(const Address: Cardinal);
var
	Owner, Node: TIntegerBinaryTree;
	NodeIsOwnersBigger: Boolean;
	Middle, MiddleOwner: TIntegerBinaryTree;
begin
	Owner := nil;
	Node := CurrentlyAllocatedBlocksTree;

	while (Node <> nil) and (Node.fValue <> Address) do
		begin
			Owner := Node;

			if Address > Node.Value then
				Node := Node.fBigger
			else
				Node := Node.fSmaller
		end;

	Assert(Node <> nil, 'TIntegerBinaryTree._Remove: not in');

	NodeIsOwnersBigger := Node = Owner.fBigger;

	if Node.fBigger = nil then
		begin
			if NodeIsOwnersBigger then
				Owner.fBigger := Node.fSmaller
			else
				Owner.fSmaller := Node.fSmaller;
		end
	else
		if Node.fSmaller = nil then
			begin
				if NodeIsOwnersBigger then
					Owner.fBigger := Node.fBigger
				else
					Owner.fSmaller := Node.fBigger;
			end
		else
			begin
				Middle := Node.fSmaller;
				MiddleOwner := Node;

				while Middle.fBigger <> nil do
					begin
						MiddleOwner := Middle;
						Middle := Middle.fBigger;
					end;

				if Middle = Node.fSmaller then
					begin
						if NodeIsOwnersBigger then
							Owner.fBigger := Middle
						else
							Owner.fSmaller := Middle;

						Middle.fBigger := Node.fBigger
					end
				else
					begin
						MiddleOwner.fBigger := Middle.fSmaller;

						Middle.fSmaller := Node.fSmaller;
						Middle.fBigger := Node.fBigger;

						if NodeIsOwnersBigger then
							Owner.fBigger := Middle
						else
							Owner.fSmaller := Middle
					end;
			end;

	Node.Destroy;
end;

constructor TFieldInfo.Create(const TheOwnerClass: TClass; const TheFieldIndex: integer);
begin
	inherited Create;

	OwnerClass := TheOwnerClass;
	FieldIndex := TheFieldIndex;
end;

const
	TObjectVirtualMethodNames: array[1..8] of string = ('SafeCallException', 'AfterConstruction', 'BeforeDestruction', 'Dispatch', 'DefaultHandler', 'NewInstance', 'FreeInstance', 'Destroy');
	AddressOfNewInstance: pointer = @TObject.NewInstance;
	AddressOfTObjectCreate: pointer = @TObject.Create;

function CallerOfCaller: pointer;	//with stack frames !
asm
	cmp ebp, 0	//this can happen when there are no stack frames
	je @@EndOfStack
	mov eax, [EBP]
	cmp eax, ebp
	jb @@EndOfStack
	mov eax, [eax + 4]
	sub eax, 4
	ret
	@@EndOfStack:
	mov eax, $FFFF
end;

function Caller: pointer;	//with stack frame !
asm
	cmp ebp, 0	//this can happen when there are no stack frames
	je @@EndOfStack
	mov eax, [ebp + 4]
	sub eax, 4
	ret
	@@EndOfStack:
	mov eax, $FFFF
end;

function CallerOfGetMem: pointer;	//System._GetMem has no stack frame
asm
	cmp ebp, 0	//this can happen when there are no stack frames
	je @@EndOfStack
	{$IFDEF DELPHI6_OR_LATER}
		{$IFNDEF DELPHI71_OR_LATER}
		mov eax, [ebp + 12]
		{$ELSE}
		mov eax, [ebp + 16]
		{$ENDIF}
	{$ELSE}
	mov eax, [ebp + 8]
	{$ENDIF}
	ret
	@@EndOfStack:
	mov eax, $FFFF
end;

function CallerOfReallocMem: pointer;	//System._ReallocMem has no stack frame
asm
	cmp ebp, 0	//this can happen when there are no stack frames
	je @@EndOfStack
	mov eax, [EBP + 12]
	sub eax, 4
	ret
	@@EndOfStack:
	mov eax, $FFFF
end;

{$IFNDEF VER140}
function CallerIsNewAnsiString: boolean;	//NewAnsiString has no stack frame
asm
	cmp ebp, 0	//this can happen when there are no stack frames
	je @@no
	mov eax, [ebp + 8]
	sub eax, 13
	cmp eax, offset System.@NewAnsiString
	je @@yes
	@@no:
	mov eax, 0
	ret
	@@yes:
	mov eax, 1
end;
{$ENDIF}

function CallerIsNewInstance: boolean;	//TObject.NewInstance has no stack frame
asm
	cmp ebp, 0	//this can happen when there are no stack frames
	je @@no
	{$IFNDEF DELPHI6_OR_LATER}
	mov eax, [ebp + 8]
	sub eax, 9
	{$ELSE}
		{$IFNDEF DELPHI71_OR_LATER}
		mov eax, [EBP + 12]
		sub eax, 15
		{$ELSE}
		mov eax, [EBP + 16]
		sub eax, 15
		{$ENDIF}
	{$ENDIF}
	cmp eax, AddressOfNewInstance
	je @@yes
	@@no:
	mov eax, 0
	ret
	@@yes:
	mov eax, 1
end;

{$IFDEF DELPHI6_OR_LATER}
function ltfm_CallerOfFreeInstance: pointer;
	//Tells the address of the caller of FreeInstance from LeakTrackingFreeMem
asm
	cmp ebp, 0	//this can happen when there are no stack frames
	je @@EndOfStack
	mov eax, [EBP + 28]
	sub eax, 4
	ret
	@@EndOfStack:
	mov eax, $FFFF
end;

function ltfm_CallerOf_FreeMem: pointer;
	//Tells the address of the caller of System._FreeMem from LeakTrackingFreeMem
asm
	cmp ebp, 0	//this can happen when there are no stack frames
	je @@EndOfStack
	mov eax, [EBP + 12]
	sub eax, 4
	ret
	@@EndOfStack:
	mov eax, $FFFF
end;

function ltgmCallerOfGetMemIsTObjectCreate: boolean;
	//Tells if the guy who called GetMem is TObject.Create
asm
	cmp ebp, 0	//this can happen when there are no stack frames
	je @@EndOfStack
		{$IFNDEF DELPHI71_OR_LATER}
		mov eax, [ebp + 36]
		{$ELSE}
		mov eax, [ebp + 40]
		{$ENDIF}
	sub eax, 12
	cmp eax, AddressOfTObjectCreate
	jne @@no
	mov eax, 1
	ret
	@@no:
	@@EndOfStack:
	mov eax, 0
end;

function ltgmCallerOfTObjectCreate: pointer;
	//Tells who called TObject.Create
asm
	cmp ebp, 0	//this can happen when there are no stack frames
	je @@EndOfStack
		{$IFNDEF DELPHI71_OR_LATER}
		mov eax, [EBP + 56]
		{$ELSE}
		mov eax, [EBP + 60]
		{$ENDIF}
	ret
	@@EndOfStack:
	mov eax, $FFFF
end;

function ltgmCallerIsNewAnsiString: boolean;
	//Tells if the guy who called GetMem is NewAnsiString
asm
	cmp ebp, 0	//this can happen when there are no stack frames
	je @@no
		{$IFNDEF DELPHI71_OR_LATER}
		mov eax, [EBP + 12]
		{$ELSE}
		mov eax, [EBP + 16]
		{$ENDIF}
	sub eax, 17
	cmp eax, offset System.@NewAnsiString
	je @@yes
	@@no:
	mov eax, 0
	ret
	@@yes:
	mov eax, 1
end;

function CallerIsDynamicArrayAllocation: boolean;
asm
	cmp ebp, 0	//this can happen when there are no stack frames
	je @@no
	mov eax, [EBP + 12]
	{$IFNDEF DELPHI71_OR_LATER}
	add eax, 204
	{$ELSE}
	add eax, 216
	{$ENDIF}
	cmp eax, offset System.@DynArraySetLength
	je @@yes
	@@no:
	mov eax, 0
	ret
	@@yes:
	mov eax, 1
end;
{$ENDIF}

procedure ReleasedInstance.RaiseExcept;
var
	t: TMemoryBlocHeader;
	i: integer;
	FeedBackStr: string;
begin
	t := PMemoryBlocHeader((PChar(Self) - SizeOf(TMemoryBlocHeader)))^;
	try
		i := MaxNbSupportedVMTEntries - GIndex + 1;
		if i in [1..8] then
			FeedBackStr:= 'Call ' + TObjectVirtualMethodNames[i]
		else
			FeedBackStr:= 'Call ' + IntToStr(i) + '° virtual method';
		FeedBackStr:= FeedBackStr + ' on a FREED instance of ' + T.VMT.ClassName + ' (destroyed at ' + TextualDebugInfoForAddress(Cardinal(T.DestructionAdress)) + ' - had been created at ' + TextualDebugInfoForAddress(Cardinal(T.CallerAddress[0])) + ')';
		raise EBadInstance.Create(FeedBackStr) at Caller;
	except
		on EBadInstance do ;
	end;
	if ShowCallStack then
		for i := 1 to StoredCallStackDepth do
			if Integer(T.CallerAddress[i]) > 0 then
				try
					raise EStackUnwinding.Create('Unwinding level ' + chr(ord('0') + i))at T.CallerAddress[i]
				except
					on EStackUnwinding do ;
				end;
	ShowCallStack := False;
end;

function InterfaceErrorCaller: Pointer;
{Returns EBP + 16, which is OK only for InterfaceError !
It would be nice to make this routine local to InterfaceError, but I do not know hot to
implement it in this case - VM}
	asm
		cmp ebp, 0	//this can happen when there are no stack frames
		je @@EndOfStack
		mov	 eax,[EBP+16];
		sub	 eax, 5
		ret
		@@EndOfStack:
		mov eax, $FFFF
	end;

procedure ReleasedInstance.InterfaceError;
begin
	try
		OutputFileHeader := OutputFileHeader + #13#10'Exception: Calling an interface method on an freed Pascal instance @ ' + TextualDebugInfoForAddress(Cardinal(InterfaceErrorCaller)) + #13#10;
		raise EInterfaceFreedInstance.Create('Calling an interface method on an freed Pascal instance')at InterfaceErrorCaller
	except
		on EInterfaceFreedInstance do
			;
	end;
end;

procedure ReleasedInstance.Error;
{Don't change this, its a Hack! jm}
asm
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		JMP ReleasedInstance.RaiseExcept;
	 end;

function MemoryBlockDump(Block: PMemoryBlocHeader): string;
const
	MaxDump = 80;
var
	i,
		count: integer;
	s: string[MaxDump];
begin
	count := Block.AllocatedSize;

	if count > MaxDump then
		Count := MaxDump;

	Byte(s[0]) := count;
	move((PChar(Block) + SizeOf(TMemoryBlocHeader))^, s[1], Count);

	for i := 1 to Length(s) do
		if s[i] = #0 then s[i] := '.' else
			if s[i] < ' ' then
				s[i] := '?';

	Result := '  Dump: [' + s + ']';
end;

procedure FillCallStack(var St: TCallStack; const NbLevelsToExclude: integer);
	{Works only with stack frames - Without, St contains correct info, but is not as deep as it should
	I just don't know a general rule for walking the stack when they are not there}
var
	StackStart: Pointer;
	StackMax: Pointer;	//the stack can never go beyond - http://msdn.microsoft.com/library/periodic/period96/S2CE.htm
	CurrentFrame: Pointer;
	Count, SkipCount: integer;
begin
	FillChar(St, SizeOf(St), 0);
	asm
		mov EAX, FS:[4]
		mov StackMax, EAX
		mov StackStart, EBP
	end;
	CurrentFrame:= StackStart;
	Count:= 0;
	SkipCount:= 0;
	while (longint(CurrentFrame) >= longint(StackStart)) and (longint(CurrentFrame) < longint(StackMax)) and (Count <= StoredCallStackDepth) do
		begin
			if SkipCount >= NbLevelsToExclude then
				begin
					St[Count]:= Pointer(PInteger(longint(CurrentFrame) + 4)^ - 4);
					Count:= Count + 1;
				end;
			CurrentFrame:= Pointer(PInteger(CurrentFrame)^);
			SkipCount:= SkipCount + 1;
		end;
end;

procedure AddAllocatedObjectsClass(const C: TClass);
begin
	if NbClasses >= Length(AllocatedObjectsClasses) then
		begin
			UnMemChk;
			SetLength(AllocatedObjectsClasses, NbClasses * 2);
			MemChk;
		end;

	AllocatedObjectsClasses[NbClasses] := C;
	NbClasses := NbClasses + 1;
end;

procedure CollectNewInstanceOfClassForStats(const TheClass: TClass);
var
	i: integer;
begin
	i := 0;
	while (i < AllocStatsCount) and (AllocatedObjectsClasses[i] <> TheClass) do
		i := i + 1;

	if i = AllocStatsCount then
		begin
			if AllocStatsCount = StatsArraysAllocatedPos then
				begin
					if StatsArraysAllocatedPos = 0 then
						StatsArraysAllocatedPos := 10;
					StatsArraysAllocatedPos := StatsArraysAllocatedPos * 2;
					UnMemChk;
					ReallocMem(AllocatedInstances, StatsArraysAllocatedPos * sizeof(Integer));
					MemChk;
				end;

			AddAllocatedObjectsClass(TheClass);
			AllocatedInstances[AllocStatsCount] := 1;
			AllocStatsCount := AllocStatsCount + 1;
		end
	else
		AllocatedInstances[i] := AllocatedInstances[i] + 1;
end;

var
	LinkedListSynchro: TSynchroObject;

procedure AddBlockAtEndOfLinkedList(const B: PMemoryBlocHeader);
begin
	LinkedListSynchro.Acquire;
	PMemoryBlocHeader(B).PreceedingBlock:= LastBlock;
	PMemoryBlocHeader(B).NextBlock:= nil;
	if LastBlock <> nil then
		LastBlock.NextBlock:= B;
	LastBlock:= B;
	LinkedListSynchro.Release;
end;

procedure RemoveBlockFromLinkedList(const B: PMemoryBlocHeader);
begin
	LinkedListSynchro.Acquire;
	if B.NextBlock <> nil then
		PMemoryBlocHeader(B.NextBlock).PreceedingBlock:= B.PreceedingBlock;
	if B.PreceedingBlock <> nil then
		PMemoryBlocHeader(B.PreceedingBlock).NextBlock:= B.NextBlock;
	if LastBlock = B then
		LastBlock:= B.PreceedingBlock;
	LinkedListSynchro.Release;
end;

function LeakTrackingGetMem(Size: Integer): Pointer;
begin
	{$IFDEF DELPHI6_OR_LATER}
	if ltgmCallerIsNewAnsiString or CallerIsDynamicArrayAllocation then
	{$ELSE}
	if CallerIsNewAnsiString then
	{$ENDIF}
		//We do not log memory allocations for reference counted strings. This would take time and some leaks would be reported	uselessly. However, if you want to know about this, you can just uncomment this part
		//Same for dynamic arrays in Delphi 6 & 7
		begin
			Result := OldMemoryManager.GetMem(Size);
			if Result = nil then
				raise OutOfMemory;
		end
	else
		begin
			if CallerIsNewInstance then
				begin
					Result := OldMemoryManager.GetMem(Size + (SizeOf(TMemoryBlocHeader)));
					if Result = nil then
						raise OutOfMemory;
					PMemoryBlocHeader(Result).KindOfBlock := MClass;
					if StoredCallStackDepth > 0 then
						{$IFDEF DELPHI6_OR_LATER}
						if ltgmCallerOfGetMemIsTObjectCreate then
							begin
								FillCallStack(PMemoryBlocHeader(Result).CallerAddress, 1);
								PMemoryBlocHeader(Result).CallerAddress[0]:= ltgmCallerOfTObjectCreate;
							end
						else
						{$ENDIF}
							FillCallStack(PMemoryBlocHeader(Result).CallerAddress, 2);
				end
			else
				begin	//Neither an object nor a string, this is a MUser
					Result := OldMemoryManager.GetMem(Size + (SizeOf(TMemoryBlocHeader) + SizeOf(TMemoryBlockFooter)));
					if Result = nil then
						raise OutOfMemory;
					PMemoryBlocHeader(Result).KindOfBlock := MUser;
					if StoredCallStackDepth > 0 then
						FillCallStack(PMemoryBlocHeader(Result).CallerAddress, 1);
					PMemoryBlocHeader(Result).CallerAddress[0]:= CallerOfGetMem;
					PMemoryBlockFooter(PChar(Result) + SizeOf(TMemoryBlocHeader) + Size)^ := EndOfBlock;
				end;
			AddBlockAtEndOfLinkedList(Result);
			PMemoryBlocHeader(Result).LastTimeStamp := TimeStampsCount - 1;
			PMemoryBlocHeader(Result).DestructionAdress := nil;
			PMemoryBlocHeader(Result).AllocatedSize := Size;
			PMemoryBlocHeader(Result).MagicNumber := Magic;
			if IdentifyObjectFields then
				begin
					UnMemChk;
					CurrentlyAllocatedBlocksTree.Add(integer(Result));
					MemChk;
				end;
			Inc(integer(Result), SizeOf(TMemoryBlocHeader));
			if ComputeMemoryUsageStats then
				begin
					MemoryUsageStatsLoop := MemoryUsageStatsLoop + 1;
					if MemoryUsageStatsLoop = MemoryUsageStatsStep then
						MemoryUsageStatsLoop := 0;
					if (MemoryUsageStatsLoop = 0) or (Size > StatCollectionForce) then
						begin
							if MemoryUsageStatsCount = MemoryUsageStatsAllocatedPos then
								begin
									if MemoryUsageStatsAllocatedPos = 0 then
										MemoryUsageStatsAllocatedPos := 10;
									MemoryUsageStatsAllocatedPos := MemoryUsageStatsAllocatedPos * 2;
									UnMemChk;
									ReallocMem(MemoryUsageStats, MemoryUsageStatsAllocatedPos * sizeof(Integer));
									MemChk;
								end;
							MemoryUsageStats[MemoryUsageStatsCount] := AllocMemSize;
							MemoryUsageStatsCount := MemoryUsageStatsCount + 1;
						end;
				end;
			if KeepMaxMemoryUsage and (AllocMemSize > MaxMemoryUsage) then
				MaxMemoryUsage := AllocMemSize;
		end;
end;

function HeapCheckingGetMem(Size: Integer): Pointer;
begin
	HeapStatusSynchro.Acquire;
	Result:= nil;	//Note: I don't understand right now why I get a warning if I suppress this line
	try
		if HeapStatusesDifferent(LastHeapStatus, GetHeapStatus) then
			raise HeapCorrupted;
		Result := OldMemoryManager.GetMem(Size);
		UpdateLastHeapStatus;
	finally
		HeapStatusSynchro.Release;
	end;
end;

function MemoryBlockFreed(Block: PMemoryBlocHeader): Boolean;
begin
	Result := Block.DestructionAdress <> nil;
end;

function MemoryBlockOverwritten(Block: PMemoryBlocHeader): Boolean;
begin
	if (block.KindOfBlock = MClass) then
		Result:= false	//We don't put a footer for objects - This could be done if interesting
	else
		Result:= PMemoryBlockFooter(PChar(Block) + SizeOf(TMemoryBlocHeader) + Block.AllocatedSize)^ <> EndOfBlock;
end;

function MemCheckBlockCorrupted(Block: PMemoryBlocHeader): Boolean;
begin
	Result := MemoryBlockFreed(Block) or MemoryBlockOverwritten(Block);
end;

function MemoryBlockCorrupted(P: Pointer): Boolean;
var
	Block: PMemoryBlocHeader;
begin
	if PCardinal(PChar(P) - 4)^ = Magic then
		begin
			Block := PMemoryBlocHeader(PChar(P) - SizeOf(TMemoryBlocHeader));
			Result:= MemCheckBlockCorrupted(Block);
		end
	else
		Result := False
end;

procedure ReplaceInterfacesWithBadInterface(AClass: TClass; Instance: Pointer);
{copied and modified from System.Pas: replaces all INTERFACES in Pascal Objects
with a reference to our dummy INTERFACE VMT}
asm
				PUSH	EBX
				PUSH	ESI
				PUSH	EDI
				MOV	 EBX,EAX
				MOV	 EAX,EDX
				MOV	 EDX,ESP
		@@0:	MOV	 ECX,[EBX].vmtIntfTable
				TEST	ECX,ECX
				JE	  @@1
				PUSH	ECX
		@@1:	MOV	 EBX,[EBX].vmtParent
				TEST	EBX,EBX
				JE	  @@2
				MOV	 EBX,[EBX]
				JMP	 @@0
		@@2:	CMP	 ESP,EDX
				JE	  @@5
		@@3:	POP	 EBX
				MOV	 ECX,[EBX].TInterfaceTable.EntryCount
				ADD	 EBX,4
		@@4:	LEA	 ESI, BadInterfaceVMT // mettre dans ESI l'adresse du début de MyInterfaceVMT: correct ?????
				MOV	 EDI,[EBX].TInterfaceEntry.IOffset
				MOV	 [EAX+EDI],ESI
				ADD	 EBX,TYPE TInterfaceEntry
				DEC	 ECX
				JNE	 @@4
				CMP	 ESP,EDX
				JNE	 @@3
		@@5:	POP	 EDI
				POP	 ESI
				POP	 EBX
	 end;

function FindMem(Base, ToFind: pointer; Nb: integer): integer;
// Base = instance, Nb = nombre de bloc (HORS VMT!)
asm
			// eax=base; edx=Tofind; ecx=Nb
			@loop:
			cmp [eax+ecx*4], edx
			je @found
			dec ecx
			jne  @loop

			@found:
			mov eax,ecx
	 end;

procedure AddFieldInfo(const FieldAddress: Pointer; const OwnerClass: TClass; const FieldPos: integer);
begin
	UnMemChk;

	if NotDestroyedFieldsCount = NotDestroyedFieldsAllocatedSpace then
		begin
			if NotDestroyedFieldsAllocatedSpace = 0 then
				NotDestroyedFieldsAllocatedSpace := 10;
			NotDestroyedFieldsAllocatedSpace := NotDestroyedFieldsAllocatedSpace * 2;
			ReallocMem(NotDestroyedFields, NotDestroyedFieldsAllocatedSpace * sizeof(integer));
			ReallocMem(NotDestroyedFieldsInfos, NotDestroyedFieldsAllocatedSpace * sizeof(integer));
		end;

	NotDestroyedFields[NotDestroyedFieldsCount] := integer(FieldAddress);
	NotDestroyedFieldsInfos[NotDestroyedFieldsCount] := TFieldInfo.Create(OwnerClass, FieldPos);
	NotDestroyedFieldsCount := NotDestroyedFieldsCount + 1;

	MemChk;
end;

function LeakTrackingFreeMem(P: Pointer): Integer;
var
	Block: PMemoryBlocHeader;
	i: integer;
begin
	if PCardinal(PChar(P) - 4)^ = Magic then
		{we recognize a block we marked}
		begin
			Block := PMemoryBlocHeader(PChar(P) - SizeOf(TMemoryBlocHeader));

			if CollectStatsAboutObjectAllocation and (Block.KindOfBlock = MClass) then
				CollectNewInstanceOfClassForStats(TObject(P).ClassType);

			if IdentifyObjectFields then
				begin
					if (Block.KindOfBlock = MClass) and (TObject(P).InheritsFrom(IdentifyFieldsOfObjectsConformantTo)) then
						for i := 1 to (Block.AllocatedSize div 4) - 1 do
							if (PInteger(PChar(P) + i * 4)^ > SizeOf(TMemoryBlocHeader)) and CurrentlyAllocatedBlocksTree.Has(PInteger(PChar(P) + i * 4)^ - SizeOf(TMemoryBlocHeader)) then
								AddFieldInfo(Pointer(PInteger(PChar(P) + i * 4)^ - SizeOf(TMemoryBlocHeader)), TObject(P).ClassType, i);

					UnMemChk;
					if not MemoryBlockFreed(Block) then
						begin
							Assert(CurrentlyAllocatedBlocksTree.Has(integer(Block)), 'freemem: block not among allocated ones');
							CurrentlyAllocatedBlocksTree.Remove(integer(Block));
						end;
					MemChk;
				end;

			if MemoryBlockFreed(Block) then
				begin
					try
						OutputFileHeader := OutputFileHeader + #13#10'Exception: second release of block attempt, allocated at ' + TextualDebugInfoForAddress(Cardinal(Block.CallerAddress[0])) + ' - Already freed at ' + TextualDebugInfoForAddress(Cardinal(Block.DestructionAdress)) + #13#10;
						raise EMemoryLeak.Create('second release of block attempt, already freed') at Block.DestructionAdress;
					except
						on EMemoryLeak do ;
					end;

					if ShowCallStack then
						for i := 1 to StoredCallStackDepth do
							if Integer(Block.CallerAddress[i]) > 0 then
								try
									raise EStackUnwinding.Create('Unwinding level ' + chr(ord('0') + i))at Block.CallerAddress[i]
								except
									on EStackUnwinding do ;
								end;

					ShowCallStack := False;
				end
			else
				begin
					if MemoryBlockOverwritten(Block) then
						begin
							try
								OutputFileHeader := OutputFileHeader + #13#10'Exception: memory damaged beyond block allocated space, allocated at ' + TextualDebugInfoForAddress(Cardinal(BlockAllocationAddress(P))) + #13#10;
								raise EMemoryLeak.Create('memory damaged beyond block allocated space, allocated at ' + TextualDebugInfoForAddress(Cardinal(BlockAllocationAddress(P)))) at CallerOfCaller;
							except
								on EMemoryLeak do ;
							end;
						end;

					if (Block.AllocatedSize > DeallocateFreedMemoryWhenBlockBiggerThan) or (DeallocateInstancesConformingTo and (Block.KindOfBlock = MClass) and (TObject(P) is InstancesConformingToForDeallocation)) then
						{we really deallocate the block}
						begin
							RemoveBlockFromLinkedList(Block);
							OldMemoryManager.FreeMem(Block);
						end
					else
						begin	//Normal case, not an error
							{$IFDEF DELPHI6_OR_LATER}
							if Block.KindOfBlock = MClass then
								Block.DestructionAdress:= ltfm_CallerOfFreeInstance
							else
								Block.DestructionAdress:= ltfm_CallerOf_FreeMem;
							{$ELSE}
							Block.DestructionAdress:= CallerOfCaller;
							{$ENDIF}

							if WipeOutMemoryOnFreeMem then
								if Block.KindOfBlock = MClass then
									begin
										Block.VMT := TObject(P).ClassType;
										FillChar((PChar(P) + 4)^, Block.AllocatedSize - 4, CharToUseToWipeOut);
										PInteger(P)^ := Integer(FreedInstance);
										if DanglingInterfacesVerified then
											ReplaceInterfacesWithBadInterface(Block.VMT, TObject(P))
									end
								else
									FillChar(P^, Block.AllocatedSize, CharToUseToWipeOut);
						end;
					end;

			Result := 0;
		end
	else
		Result := OldMemoryManager.FreeMem(P);
end;

function HeapCheckingFreeMem(P: Pointer): Integer;
begin
	if HeapStatusesDifferent(LastHeapStatus, GetHeapStatus) then
		raise HeapCorrupted;

	Result := OldMemoryManager.FreeMem(P);

	UpdateLastHeapStatus;
end;

function LeakTrackingReallocMem(P: Pointer; Size: Integer): Pointer;
var
	Block: PMemoryBlocHeader;
begin
	if PCardinal(PChar(P) - 4)^ = Magic then
		begin
			GetMem(Result, Size);
			Block:= PMemoryBlocHeader(PChar(Result) - SizeOf(TMemoryBlocHeader));
			if StoredCallStackDepth > 0 then
				FillCallStack(Block.CallerAddress, 1);
			Block.CallerAddress[0]:= CallerOfReallocMem;
			Block.KindOfBlock := MReallocedUser;
			if Size > PMemoryBlocHeader(PChar(P) - SizeOf(TMemoryBlocHeader)).AllocatedSize then
				Move(P^, Result^, PMemoryBlocHeader(PChar(P) - SizeOf(TMemoryBlocHeader)).AllocatedSize)
			else
				Move(P^, Result^, Size);
			LeakTrackingFreeMem(P);
		end
	else
		Result := OldMemoryManager.ReallocMem(P, Size);
end;

function HeapCheckingReallocMem(P: Pointer; Size: Integer): Pointer;
begin
	if HeapStatusesDifferent(LastHeapStatus, GetHeapStatus) then
		raise HeapCorrupted;

	Result := OldMemoryManager.ReallocMem(P, Size);

	UpdateLastHeapStatus;
end;

procedure UnMemChk;
begin
	SetMemoryManager(OldMemoryManager);
	MemCheckActive := False;
end;

function IsMemFilledWithChar(P: Pointer; N: Integer; C: Char): boolean;
	//is the memory at P made of C on N bytes ?
asm
	//EAX: P - EDX: N - CL: C
	@loop:
	cmp   [eax+edx-1],cl
	jne   @diff
	dec   edx
	jne   @loop
	mov   eax,1
	ret
	@diff:
	xor   eax,eax
end;

procedure GoThroughAllocatedBlocks;
{traverses the allocated blocks list and for each one, raises exceptions showing the memory leaks}
var
	Block: PMemoryBlocHeader;
	i: integer;
	S: ShortString;
begin
        if RaiseExceptionsOnEnd then
			begin
				UnMemChk;
				Block := LastBlock;	//note: no thread safety issue here
				ShowCallStack := False;			 {for first block}
				while Block <> nil do
					  begin
							  if BlocksToShow[Block.KindOfBlock] then
									  begin
											  if not MemoryBlockFreed(Block) then
													  {this is a leak}
													  begin
															  case Block.KindOfBlock of
																	  MClass:
																			  S := TObject(PChar(Block) + SizeOf(TMemoryBlocHeader)).ClassName;
																	  MUser:
																			  S := 'User';
																	  MReallocedUser:
																			  S := 'Realloc';
															  end;

															  if (BlocksToShow[Block.KindOfBlock]) and ((Block.KindOfBlock <> MClass) or (TObject(PChar(Block) + SizeOf(TMemoryBlocHeader)) is InstancesConformingToForReporting)) then
																	  try
																			  raise EMemoryLeak.Create(S + ' allocated at ' + TextualDebugInfoForAddress(Cardinal(Block.CallerAddress[0])))at Block.CallerAddress[0];
																	  except
																			  on EMemoryLeak do ;
																	  end;

															  if ShowCallStack then
																	  for i := 1 to StoredCallStackDepth do
																			  if Integer(Block.CallerAddress[i]) > 0 then
																					  try
																							  raise EStackUnwinding.Create(S + ' unwinding level ' + chr(ord('0') + i))at Block.CallerAddress[i]
																					  except
																							  on EStackUnwinding do ;
																					  end;

															  ShowCallStack := False;
													  end			 {Block.DestructionAdress = Nil}
											  else
													  {this is not a leak}
													  if CheckWipedBlocksOnTermination and (Block.AllocatedSize > 5) and (Block.AllocatedSize <= DoNotCheckWipedBlocksBiggerThan) and (not IsMemFilledWithChar(pchar(Block) + SizeOf(TMemoryBlocHeader) + 4, Block.AllocatedSize - 5, CharToUseToWipeOut)) then
															  begin
																	  try
																			  raise EFreedBlockDamaged.Create('Destroyed block damaged - Block allocated at ' + TextualDebugInfoForAddress(Cardinal(Block.CallerAddress[0])) + ' - destroyed at ' + TextualDebugInfoForAddress(Cardinal(Block.DestructionAdress)))at Block.CallerAddress[0]
																	  except
																			  on EFreedBlockDamaged do ;
																	  end;
															  end;
									  end;

							  Block := Block.PreceedingBlock;
					  end;
			end;
end;

procedure dummy; forward;

procedure ChangeFinalizationsOrder;
	//Changes the order in which finalizations will occur. The five last units to be finalized must be MemCheck, Classes, Variants, SysUtils and System (in this order)
	//Warning: this routine is likely to need to be rewritten when upgrading Delphi
type
	PPackageUnitEntry = ^PackageUnitEntry;
var
	UnitsInfo: PackageInfo;
    //This variable will contain the same thing as System.InitContext.InitTable^.UnitInfo, which is unfortunately not public, and changes between versions of Delphi
	NewUnitsInfoOrder: TList;	//of PPackageUnitEntry
	i: integer;
	CurrentUnitInfo: PackageUnitEntry;
	CurrentUnitInfoCopy: PPackageUnitEntry;
	ProcessHandle: THandle;
	BytesWritten: cardinal;
const
	DummyToFinalizationOffset = {$IFOPT I+}356{$ELSE}352{$ENDIF};
begin
  {$IFDEF VER180} // BDS 2006
  UnitsInfo := PInitContext(PChar(@AllocMemSize) + 8728).InitTable;
  {$ELSE}
    {$IFNDEF DELPHI7_OR_LATER}
    UnitsInfo:= PackageInfo(pointer(Pointer(PChar(@AllocMemSize) + 31 * 4 + 8)^));	//Hacky, no ? I learnt to count on my fingers ! (this stuff is not exported by system.pas)
    {$ELSE}
    UnitsInfo := PInitContext(PChar(@AllocMemSize) + 128).InitTable;
    {$ENDIF}
	{$ENDIF}
	NewUnitsInfoOrder:= TList.Create;
	for i:= 0 to UnitsInfo.UnitCount - 1 do
		begin
			CurrentUnitInfo:= UnitsInfo.UnitInfo^[i];
			GetMem(CurrentUnitInfoCopy, SizeOf(PackageUnitEntry));
			CurrentUnitInfoCopy^:= CurrentUnitInfo;
			if {$IFNDEF DELPHI6_OR_LATER}@{$ENDIF}CurrentUnitInfo.Init = @System.InitProc then
				NewUnitsInfoOrder.Insert(0, CurrentUnitInfoCopy)
			else
				{$IFDEF DELPHI6_OR_LATER}
				if CurrentUnitInfo.Init = @Variants.Variants then
					NewUnitsInfoOrder.Insert(2, CurrentUnitInfoCopy)
				else
				{$ENDIF}
					if {$IFNDEF DELPHI6_OR_LATER}@{$ENDIF}CurrentUnitInfo.Init = Pointer(PChar(@Dummy) + DummyToFinalizationOffset) then
						NewUnitsInfoOrder.Insert(4, CurrentUnitInfoCopy)
					else
						NewUnitsInfoOrder.Add(CurrentUnitInfoCopy);
		end;
	ProcessHandle:= openprocess(PROCESS_ALL_ACCESS, True, GetCurrentProcessId);
	for i:= 0 to NewUnitsInfoOrder.Count - 1 do
		begin
			WriteProcessMemory(ProcessHandle, Pointer(PChar(@UnitsInfo^.UnitInfo^[0]) + i * SizeOf(PackageUnitEntry)), NewUnitsInfoOrder[i], SizeOf(PackageUnitEntry), BytesWritten);
			FreeMem(NewUnitsInfoOrder[i]);
		end;
	CloseHandle(ProcessHandle);
	NewUnitsInfoOrder.Destroy;
end;

function UnitWhichContainsAddress(const Address: Cardinal): TUnitDebugInfos;
var
	Start, Finish, Pivot: integer;
begin
	Start := 0;
	Finish := UnitsCount - 1;
	Result := nil;

	while Start <= Finish do
		begin
			Pivot := Start + (Finish - Start) div 2;

			if TUnitDebugInfos(Units[Pivot]).Addresses[0].Address > Address then
				Finish := Pivot - 1
			else
				if TUnitDebugInfos(Units[Pivot]).Addresses[Length(TUnitDebugInfos(Units[Pivot]).Addresses) - 1].Address < Address then
					Start := Pivot + 1
				else
					begin
						Result := Units[Pivot];
						Start := Finish + 1;
					end;
		end;
end;

function RoutineWhichContainsAddress(const Address: Cardinal): string;
var
	Start, Finish, Pivot: integer;
begin
	Start := 0;
	Result := NoDebugInfo;
	Finish := RoutinesCount - 1;

	while Start <= Finish do
		begin
			Pivot := Start + (Finish - Start) div 2;

			if TRoutineDebugInfos(Routines[Pivot]).StartAddress > Address then
				Finish := Pivot - 1
			else
				if TRoutineDebugInfos(Routines[Pivot]).EndAddress < Address then
					Start := Pivot + 1
				else
					begin
						Result := ' Routine ' + TRoutineDebugInfos(Routines[Pivot]).Name;
						Start := Finish + 1;
					end;
		end;
end;

type
	TExceptionProc = procedure(Exc: TObject; Addr: Pointer);

var
	InitialExceptionProc: TExceptionProc;
	VersionInfo: string;

procedure MyExceptProc(Exc: TObject; Addr: Pointer);
var
	S: TCallStack;
begin
	Writeln(SevereExceptionsLogFile, '');
	Writeln(SevereExceptionsLogFile, '********* Severe exception detected - ' + DateTimeToStr(Now) + ' *********');
	Writeln(SevereExceptionsLogFile, VersionInfo);
	Writeln(SevereExceptionsLogFile, 'Exception code: ' + Exc.ClassName);
	Writeln(SevereExceptionsLogFile, 'Exception address: ' + TextualDebugInfoForAddress(Cardinal(Addr)));
	Writeln(SevereExceptionsLogFile, #13#10'Call stack (oldest call at bottom):');
	FillCallStack(S, 0);
	Writeln(SevereExceptionsLogFile, CallStackTextualRepresentation(S, ''));
	Writeln(SevereExceptionsLogFile, '*****************************************************************');
	Writeln(SevereExceptionsLogFile, '');
	InitialExceptionProc(Exc, Addr);
	//The closing of the file is done in the finalization
end;

procedure LogSevereExceptions(const WithVersionInfo: string);
const
	FileNameBufSize = 1000;
var
	LogFileName: string;
begin
	if ExceptProc <> @MyExceptProc then
		{not installed yet ?}
		begin
			try
				SetLength(LogFileName, FileNameBufSize);
				GetModuleFileName(0, PChar(LogFileName), FileNameBufSize);
				LogFileName := copy(LogFileName, 1, pos('.', LogFileName)) + 'log';

				AssignFile(SevereExceptionsLogFile, LogFileName);

				if FileExists(LogFileName) then
					Append(SevereExceptionsLogFile)
				else
					Rewrite(SevereExceptionsLogFile);
			except
			end;

			InitialExceptionProc := ExceptProc;
			ExceptProc := @MyExceptProc;
			VersionInfo := WithVersionInfo;
		end;
end;

function IsMemCheckActive: boolean;
begin
	Result := MemCheckActive
end;

constructor TUnitDebugInfos.Create(const AName: string; const NbLines: Cardinal);
begin
	Name := AName;

	SetLength(Addresses, NbLines);
end;

constructor TRoutineDebugInfos.Create(const AName: string; const AStartAddress: Cardinal; const ALength: Cardinal);
begin
	Name := AName;
	StartAddress := AStartAddress;
	EndAddress := StartAddress + ALength - 1;
end;

constructor TAddressToLine.Create(const AAddress, ALine: Cardinal);
begin
	Address := AAddress;
	Line := ALine
end;

function TUnitDebugInfos.LineWhichContainsAddress(const Address: Cardinal): string;
var
	Start, Finish, Pivot: Cardinal;
begin
	if Addresses[0].Address > Address then
		Result := ''
	else
		begin
			Start := 0;
			Finish := Length(Addresses) - 1;

			while Start < Finish - 1 do
				begin
					Pivot := Start + (Finish - Start) div 2;

					if Addresses[Pivot].Address = Address then
						begin
							Start := Pivot;
							Finish := Start
						end
					else
						if Addresses[Pivot].Address > Address then
							Finish := Pivot
						else
							Start := Pivot
				end;

			Result := ' Line ' + IntToStr(Addresses[Start].Line);
		end;
end;

type
	SRCMODHDR = packed record
		_cFile: Word;
		_cSeg: Word;
		_baseSrcFile: array[0..MaxListSize] of Integer;
	end;

	SRCFILE = packed record
		_cSeg: Word;
		_nName: Integer;
		_baseSrcLn: array[0..MaxListSize] of Integer;
	end;

	SRCLN = packed record
		_Seg: Word;
		_cPair: Word;
		_Offset: array[0..MaxListSize] of Integer;
	end;

	PSRCMODHDR = ^SRCMODHDR;
	PSRCFILE = ^SRCFILE;
	PSRCLN = ^SRCLN;

	TArrayOfByte = array[0..MaxListSize] of Byte;
	TArrayOfWord = array[0..MaxListSize] of Word;
	PArrayOfByte = ^TArrayOfByte;
	PArrayOfWord = ^TArrayOfWord;
	PArrayOfPointer = ^TArrayOfPointer;
	TArrayOfPointer = array[0..MaxListSize] of PArrayOfByte;

procedure AddRoutine(const Name: string; const Start, Len: Cardinal);
begin
	if Length(Routines) <= RoutinesCount then
		SetLength(Routines, Max(RoutinesCount * 2, 1000));

	Routines[RoutinesCount] := TRoutineDebugInfos.Create(Name, Start, Len);
	RoutinesCount := RoutinesCount + 1;
end;

procedure AddUnit(const U: TUnitDebugInfos);
begin
	if Length(Units) <= UnitsCount then
		SetLength(Units, Max(UnitsCount * 2, 1000));

	Units[UnitsCount] := U;
	UnitsCount := UnitsCount + 1;
end;

procedure dumpsymbols(NameTbl: PArrayOfPointer; sstptr: PArrayOfByte; size: integer);
//Copyright (C) Tenth Planet Software Intl., Clive Turvey 1998. All rights reserved. - Reused & modified by SG with permission
var
	len, sym: integer;
begin
	while size > 0 do
		begin
			len := PWord(@sstptr^[0])^;
			sym := PWord(@sstptr^[2])^;

			INC(len, 2);

			if ((sym = $205) or (sym = $204)) and (PInteger(@sstptr^[40])^ > 0) then
				AddRoutine(PChar(NameTbl^[PInteger(@sstptr^[40])^ - 1]), PInteger(@sstptr^[28])^, PInteger(@sstptr^[16])^);

			if (len = 2) then
				size := 0
			else
				begin
					sstptr := PArrayOfByte(@sstptr^[len]);
					DEC(size, len);
				end;
		end;
end;

procedure dumplines(NameTbl: PArrayOfPointer; sstptr: PArrayOfByte; size: integer);
//Copyright (C) Tenth Planet Software Intl., Clive Turvey 1998. All rights reserved. - Reused & modified by SG with permission
var
	srcmodhdr: PSRCMODHDR;
	i: Word;
	srcfile: PSRCFILE;
	srcln: PSRCLN;
	k: Word;
	CurrentUnit: TUnitDebugInfos;
begin
	if size > 0 then
		begin
			srcmodhdr := PSRCMODHDR(sstptr);

			for i := 0 to pred(srcmodhdr^._cFile) do
				begin
					srcfile := PSRCFILE(@sstptr^[srcmodhdr^._baseSrcFile[i]]);

					if srcfile^._nName > 0 then
						//note: I assume that the code is always in segment #1. If this is not the case, Houston !  - VM
						begin
							srcln := PSRCLN(@sstptr^[srcfile^._baseSrcLn[0]]);

							CurrentUnit := TUnitDebugInfos.Create(ExtractFileName(PChar(NameTbl^[srcfile^._nName - 1])), srcln^._cPair);
							AddUnit(CurrentUnit);

							for k := 0 to pred(srcln^._cPair) do
								CurrentUnit.Addresses[k] := TAddressToLine.Create(Integer(PArrayOfPointer(@srcln^._Offset[0])^[k]), Integer(PArrayOfWord(@srcln^._Offset[srcln^._cPair])^[k]));
						end;
				end;
		end;
end;

procedure GetProjectInfos;
//Copyright (C) Tenth Planet Software Intl., Clive Turvey 1998. All rights reserved. - Reused & modified by SG with permission
var
	AHeader: packed record
		Signature: array[0..3] of Char;
		AnInteger: Integer;
	end;
	k: integer;
	j: Word;
	lfodir: Integer;
	SstFrameSize: integer;
	SstFrameElem: PArrayOfByte;
	ssttype, sstsize, sstbase: Integer;
	x, y, z: Integer;
	sstbuf: PArrayOfByte;
	OldFileMode: integer;
	AFileOfByte: file of Byte;
	Names: PArrayOfByte;
	NameTbl: PArrayOfPointer;
	SstFrame: PArrayOfByte;
	ifabase: Integer;
	cdir, cbdirentry: word;
	FileName: string;
begin
	RoutinesCount := 0;
	UnitsCount := 0;
	OldFileMode := FileMode;
	FileMode := 0;
	SetLength(FileName, MAX_PATH + 1);
	SetLength(FileName, GetModuleFileName(HInstance, PChar(FileName), MAX_PATH));
	AssignFile(AFileOfByte, FileName);
	Reset(AFileOfByte);
	Names := nil;
	NameTbl := nil;
	Seek(AFileOfByte, FileSize(AFileOfByte) - SizeOf(AHeader));
	BlockRead(AFileOfByte, AHeader, SizeOf(AHeader));
	if (AHeader.Signature = 'FB09') or (AHeader.Signature = 'FB0A') then
		begin
			ifabase := FilePos(AFileOfByte) - AHeader.AnInteger;
			Seek(AFileOfByte, ifabase);
			BlockRead(AFileOfByte, AHeader, SizeOf(AHeader));
			if (AHeader.Signature = 'FB09') or (AHeader.Signature = 'FB0A') then
				begin
					lfodir := ifabase + AHeader.AnInteger;
					if lfodir >= ifabase then
						begin
							Seek(AFileOfByte, lfodir);
							BlockRead(AFileOfByte, j, SizeOf(Word));
							BlockRead(AFileOfByte, cbdirentry, SizeOf(Word));
							BlockRead(AFileOfByte, cdir, SizeOf(Word));
							Seek(AFileOfByte, lfodir + j);
							SstFrameSize := cdir * cbdirentry;
							getmem(SstFrame, SstFrameSize);
							BlockRead(AFileOfByte, SstFrame^, SstFrameSize);
							for k := 0 to pred(cdir) do
								begin
									SstFrameElem := PArrayOfByte(@SstFrame^[k * cbdirentry]);
									ssttype := PWord(@SstFrameElem^[0])^;
									if (ssttype = $0130) then
										begin
											sstbase := ifabase + PInteger(@SstFrameElem^[4])^;
											sstsize := PInteger(@SstFrameElem^[8])^;
											getmem(Names, sstsize);
											Seek(AFileOfByte, sstbase);
											BlockRead(AFileOfByte, Names^, sstsize);
											y := PInteger(@Names^[0])^;
											getmem(NameTbl, sizeof(Pointer) * y);
											z := 4;
											for x := 0 to pred(y) do
												begin
													NameTbl^[x] := PArrayOfByte(@Names^[z + 1]);
													z := z + Names^[z] + 2;
												end;
										end;
								end;
							for k := 0 to pred(cdir) do
								begin
									SstFrameElem := PArrayOfByte(@SstFrame^[k * cbdirentry]);
									ssttype := PWord(@SstFrameElem^[0])^;
									sstbase := ifabase + PInteger(@SstFrameElem^[4])^;
									sstsize := PInteger(@SstFrameElem^[8])^;
									getmem(sstbuf, sstsize);
									Seek(AFileOfByte, sstbase);
									BlockRead(AFileOfByte, sstbuf^, sstsize);
									if (ssttype = $0125) then
										dumpsymbols(NameTbl, PArrayOfByte(@sstbuf^[4]), sstsize - 4);
									if (ssttype = $0127) then
										dumplines(NameTbl, sstbuf, sstsize);
									FreeMem(sstbuf);
								end;
							FreeMem(Names);
							FreeMem(NameTbl);
							FreeMem(SstFrame);
						end;
				end;
		end;
	CloseFile(AFileOfByte);
	FileMode := OldFileMode;
end;

procedure BadDestroy;
begin
	Writeln('bad destroy');
end;

procedure SetDispl; forward;

procedure InitializeOnce;
var
	i: integer;
begin
	if not MemCheckInitialized then
		{once mechanism}
		begin
			SetDispl;
			OutOfMemory := EOutOfMemory.Create('Memcheck is not able to allocate memory, due to system resource lack');
			HeapCorrupted := Exception.Create('Heap corrupted');
			ChangeFinalizationsOrder;
			MemCheckInitialized := True;
			GIndex := 0;
			LastBlock := nil;

			for I := 0 to MaxNbSupportedVMTEntries do
				begin
					BadObjectVMT.B[I] := PChar(@ReleasedInstance.Error) + 6 * I;
					BadInterfaceVMT[I] := PChar(@ReleasedInstance.InterfaceError);
				end;

			FreedInstance := Pchar(ReleasedInstance) + vmtMethodTable;
			Move(FreedInstance^, BadObjectVMT.A, 20);
			FreedInstance := PChar(@BadObjectVMT.B[8]);

			if IdentifyObjectFields then
				CurrentlyAllocatedBlocksTree := TIntegerBinaryTree.Create;
			if CollectStatsAboutObjectAllocation then
				SetLength(AllocatedObjectsClasses, 100);

			GetProjectInfos;

			GetMemoryManager(OldMemoryManager);

			LinkedListSynchro:= TCriticalSection.Create;

			if CheckHeapStatus then
				HeapStatusSynchro:= TSynchroObject.Create;
		end;
end;

function CallStacksEqual(const CS1, CS2: TCallStack): Boolean;
var
	i: integer;
begin
	Result := True;
	i := 0;
	while (Result) and (i <= StoredCallStackDepth) do
		begin
			Result := Result and (CS1[i] = CS2[i]);
			i := i + 1;
		end;
end;

type
	TLeak = class
	public
		fID: integer;

		fBlock: PMemoryBlocHeader;
		fOccurences: integer;

		fWasFieldOfAnotherObject: Boolean;
		fOwnerClassName: string;
		fOtherFieldIndex: integer;
		fOtherIsDestroyed: Boolean;

		constructor Create(ABlock: PMemoryBlocHeader);
		function IsEqual(const Other: TLeak): Boolean;
		procedure AddOccurence;
		property Occurences: integer read fOccurences;
		property Block: PMemoryBlocHeader read fBlock;
		property WasFieldOfAnotherObject: Boolean read fWasFieldOfAnotherObject;
		property OtherObjectClassName: string read fOwnerClassName;
		property OtherFieldIndex: integer read fOtherFieldIndex;
		property OtherIsDestroyed: Boolean read fOtherIsDestroyed;
		procedure OutputToFile(const F: Text);
		procedure OutputOneLineToFile(const F: Text);
    function TotalSize: integer;
	end;

	TLeakList = class
	public
		fItems: array of TLeak;
		fCapacity: integer;
		fCount: integer;

		procedure Add(const L: TLeak);
		constructor Create;
		function Item(const I: integer): TLeak;
		property Count: integer read fCount;
	end;

	TBlockList = class
	public
		fItems: array of PMemoryBlocHeader;
		fCapacity: integer;
		fCount: integer;

		procedure Add(const B: PMemoryBlocHeader);
		constructor Create;
		function Item(const I: integer): PMemoryBlocHeader;
		property Count: integer read fCount;
	end;

constructor TLeak.Create(ABlock: PMemoryBlocHeader);
begin
	inherited Create;

	fBlock := ABlock;
	fOccurences := 1;
end;

procedure TLeak.OutputToFile(const F: Text);
begin
	Write(F, 'Leak #', fID, ' ');

	case Block.KindOfBlock of
		MClass:
			WriteLn(F, 'Instance of ', TObject(PChar(Block) + SizeOf(TMemoryBlocHeader)).ClassName);
		MUser:
			WriteLn(F, 'User allocated memory (GetMem)');
		MReallocedUser:
			WriteLn(F, 'Reallocated memory (ReallocMem)');
	end;

	WriteLn(F, #9'Size: ', Block.AllocatedSize);
	if fOccurences > 1 then
		WriteLn(F, #9, fOccurences, ' Occurences')
	else
		WriteLn(F, #9, fOccurences, ' Occurence');

	if fWasFieldOfAnotherObject then
		begin
			Write(F, #9'Was field #', fOtherFieldIndex, ' of an instance of ', fOwnerClassName);

			if fOtherIsDestroyed then
				WriteLn(F, ' (destroyed)')
			else
				WriteLn(F, ' (not destroyed)');
		end;

	WriteLn(F, CallStackTextualRepresentation(Block.CallerAddress, #9));
end;

function TLeak.TotalSize: integer;
begin
  Result := 0;
  if Assigned(fBlock) then
    Result := fOccurences*fBlock^.AllocatedSize;
end;

procedure TLeak.OutputOneLineToFile(const F: Text);
begin
	case Block.KindOfBlock of
		MClass:
			Write(F, '* Instance of ', TObject(PChar(Block) + SizeOf(TMemoryBlocHeader)).ClassName);
		MUser:
			Write(F, '* User allocated memory (GetMem)');
		MReallocedUser:
			Write(F, '* Reallocated memory (ReallocMem)');
	end;

	Write(F, ' (Leak #', fID, ') ');

	WriteLn(F, 'Size: ', Block.AllocatedSize);
end;

function TLeak.IsEqual(const Other: TLeak): Boolean;
begin
	Result := (fBlock.KindOfBlock = Other.Block.KindOfBlock) and (fBlock.AllocatedSize = Other.Block.AllocatedSize);

	if fBlock.KindOfBlock = MClass then
		Result := Result and (TObject(PChar(fBlock) + SizeOf(TMemoryBlocHeader)).ClassName = TObject(PChar(Other.Block) + SizeOf(TMemoryBlocHeader)).ClassName);

	Result := Result and (WasFieldOfAnotherObject = Other.WasFieldOfAnotherObject);

	if WasFieldOfAnotherObject then
		Result := Result and (OtherObjectClassName = Other.OtherObjectClassName) and (OtherFieldIndex = Other.OtherFieldIndex) and (OtherIsDestroyed = Other.OtherIsDestroyed);

	Result := Result and CallStacksEqual(fBlock.CallerAddress, Other.Block.CallerAddress)
end;

procedure TLeak.AddOccurence;
begin
	fOccurences := fOccurences + 1
end;

procedure TLeakList.Add(const L: TLeak);
begin
	if Count = fCapacity then
		begin
			fCapacity := fCapacity * 2;
			SetLength(fItems, fCapacity);
		end;

	fItems[fCount] := L;
	fCount := fCount + 1;
end;

constructor TLeakList.Create;
begin
	inherited Create;

	fCapacity := 10;
	fCount := 0;
	SetLength(fItems, fCapacity);
end;

function TLeakList.Item(const I: integer): TLeak;
begin
	Assert((i >= 0) and (i < fCount), Format('TLeakList.Item: out of bounds (%d)', [I]));

	Result := fItems[i]
end;

procedure TBlockList.Add(const B: PMemoryBlocHeader);
begin
	if Count = fCapacity then
		begin
			fCapacity := fCapacity * 2;
			SetLength(fItems, fCapacity);
		end;

	fItems[fCount] := B;
	fCount := fCount + 1;
end;

constructor TBlockList.Create;
begin
	inherited Create;

	fCapacity := 10;
	fCount := 0;
	SetLength(fItems, fCapacity);
end;

function TBlockList.Item(const I: integer): PMemoryBlocHeader;
begin
	Assert((i >= 0) and (i < fCount), 'TBlockList.Item: out of bounds');

	Result := fItems[i]
end;

procedure GetLeaks(const LeaksList, ChronogicalInfo: TLeakList; const MaxNumberOfLeaks: integer; var StoppedDueToMaxLeak: Boolean);
var
	Block: PMemoryBlocHeader;
	CurrentLeak: TLeak;
	i: integer;
	NewLeak: Boolean;
begin
	StoppedDueToMaxLeak := False;
	Block := LastBlock;
	while (Block <> nil) and (LeaksList.Count < MaxNumberOfLeaks) do
		begin
			if not MemoryBlockFreed(Block) then
				{this is a leak}
				begin
					CurrentLeak := TLeak.Create(Block);

					if IdentifyObjectFields then
						for i := 0 to NotDestroyedFieldsCount - 1 do
							if pointer(NotDestroyedFields[i]) = Block then
								begin
									CurrentLeak.fWasFieldOfAnotherObject := True;
									CurrentLeak.fOwnerClassName := TFieldInfo(NotDestroyedFieldsInfos[i]).OwnerClass.ClassName;
									CurrentLeak.fOtherFieldIndex := TFieldInfo(NotDestroyedFieldsInfos[i]).FieldIndex;
									CurrentLeak.fOtherIsDestroyed := True;
								end;

					//A future improvement: identify fields of not destroyed objects

					NewLeak := True;
					i := 0;
					while i < LeaksList.Count do
						begin
							if LeaksList.Item(i).IsEqual(CurrentLeak) then
								begin
									CurrentLeak.Destroy;
									CurrentLeak := LeaksList.Item(i);
									CurrentLeak.AddOccurence;
									i := LeaksList.Count;
									NewLeak := False;
								end;

							i := i + 1;
						end;

					if NewLeak then
						begin
							CurrentLeak.fID := LeaksList.Count;
							LeaksList.Add(CurrentLeak);
						end;

					ChronogicalInfo.Add(CurrentLeak);
				end;

			Block := Block.PreceedingBlock;
		end;

	if LeaksList.Count = MaxNumberOfLeaks then
		StoppedDueToMaxLeak := True;
end;

procedure GetBadBlocks(const B: TBlockList; const MaxNumberOfBlocks, MaxBlockSize: integer; var StoppedDueToMaxBlock: Boolean);
var
	Block: PMemoryBlocHeader;
begin
	StoppedDueToMaxBlock := False;
	Block := LastBlock;
	while (Block <> nil) and (B.Count < MaxNumberOfBlocks) do
		begin
			if MemoryBlockFreed(Block) and (Block.AllocatedSize > 5) and (Block.AllocatedSize <= MaxBlockSize) and (not IsMemFilledWithChar(pchar(Block) + SizeOf(TMemoryBlocHeader) + 4, Block.AllocatedSize - 5, CharToUseToWipeOut)) then
				B.Add(Block);

			Block := Block.PreceedingBlock;
		end;

	if B.Count = MaxNumberOfBlocks then
		StoppedDueToMaxBlock := True;
end;

 function SortTLeaks(Item1, Item2: Pointer): Integer;
 begin
   // sort descending
   Result := TLeak(Item2).TotalSize - TLeak(Item1).TotalSize;
 end;
 
 procedure OutputAllCollectedInformation;
var
	OutputFile: Text;
	LeaksList: TLeakList;			   //Contains all instances of TLeak
	ChronogicalInfo: TLeakList;		 //Contains one ore more instance of each TLeak
	StoppedDueToMax: Boolean;
	TotalLeak: integer;
	i, j: integer;
	LastDisplayedTimeStamp: integer;
	BadBlocks: TBlockList;
  TopLeakRefs: TList;
  MinIdx: integer;
begin
	//Initalize
	InitializeOnce;
	UnMemChk;
	LeaksList := TLeakList.Create;
	ChronogicalInfo := TLeakList.Create;

	//Prepare the output file
	if (IOResult <> 0) then ;	//Clears the internal IO error flag
	AssignFile(OutputFile, MemCheckLogFileName + '.$$$');
	Rewrite(OutputFile);
	WriteLn(OutputFile, OutputFileHeader);

	//We collect the list of allocated blocks
	GetLeaks(LeaksList, ChronogicalInfo, MaxLeak, StoppedDueToMax);

  TopLeakRefs := TList.Create;
  try
	for i := 0 to Min(9, LeaksList.Count - 1) do
	  TopLeakRefs.Add(LeaksList.Item(i));

    for i := 9 + 1 to LeaksList.Count - 1 do
    begin
      MinIdx := 0;
      for j := 0 to TopLeakRefs.Count - 1 do
      begin
        if TLeak(TopLeakRefs[j]).TotalSize < TLeak(TopLeakRefs[MinIdx]).TotalSize then
          MinIdx := j;
	  end;//for

      if LeaksList.Item(i).TotalSize > TLeak(TopLeakRefs[MinIdx]).TotalSize then
        TopLeakRefs[MinIdx] := LeaksList.Item(i);
    end;//for

    TopLeakRefs.Sort(SortTLeaks);

    WriteLn(OutputFile, 'TOP 10 Leaks: begin');
    for i := 0 to TopLeakRefs.Count - 1 do
      TLeak(TopLeakRefs[i]).OutputToFile(OutputFile);
    WriteLn(OutputFile, 'TOP 10 Leaks: end'#13#10);
  finally
    TopLeakRefs.Free;
  end;

	//Improve the header
	TotalLeak := 0;
	for i := 0 to ChronogicalInfo.Count - 1 do
		TotalLeak := TotalLeak + ChronogicalInfo.Item(i).Block.AllocatedSize;
	if StoppedDueToMax then
		WriteLn(OutputFile, 'Total leak not accurate due to MaxLeak constant reached, but at least ', TotalLeak, ' bytes'#13#10)
	else
		WriteLn(OutputFile, 'Total leak: ', TotalLeak, ' bytes'#13#10);

	//We output the memory leaks
	WriteLn(OutputFile, #13#10'*** MEMCHK: Blocks STILL allocated ***'#13#10);
	for i := 0 to LeaksList.Count - 1 do
		LeaksList.Item(i).OutputToFile(OutputFile);
	WriteLn(OutputFile, '*** MEMCHK: End of allocated blocks ***'#13#10);

	//We give chronological info
	WriteLn(OutputFile, #13#10'*** MEMCHK: Chronological leak information ***'#13#10);
	if TimeStampsCount > 0 then
		WriteLn(OutputFile, '  Time stamp: "', TimeStamps[0], '"');
	LastDisplayedTimeStamp := 0;
	for i := ChronogicalInfo.Count - 1 downto 0 do
		begin
			if (TimeStampsCount > 0) and (ChronogicalInfo.Item(i).Block.LastTimeStamp > LastDisplayedTimeStamp) then
				begin
					for j := LastDisplayedTimeStamp + 1 to ChronogicalInfo.Item(i).Block.LastTimeStamp do
						WriteLn(OutputFile, '  Time stamp: "', TimeStamps[j], '"');
					LastDisplayedTimeStamp := ChronogicalInfo.Item(i).Block.LastTimeStamp;
				end;
			ChronogicalInfo.Item(i).OutputOneLineToFile(OutputFile);
		end;
	for j := LastDisplayedTimeStamp + 1 to TimeStampsCount - 1 do
		WriteLn(OutputFile, '  Time stamp: "', TimeStamps[j], '"');
	WriteLn(OutputFile, #13#10'*** MEMCHK: End of chronological leak information ***'#13#10);

	//Output the allocation stats if necessary
	if CollectStatsAboutObjectAllocation then
		begin
			WriteLn(OutputFile, #13#10'*** MEMCHK: Allocation stats ***'#13#10);
			if TotalLeak > 0 then
				WriteLn(OutputFile, #9'The information is not accurate since there are memory leaks'#13#10);
			WriteLn(OutputFile, #9'Nb instances'#9'Instance size'#9'ClassName');
			for i := 0 to AllocStatsCount - 1 do
				WriteLn(OutputFile, #9, AllocatedInstances[i], #9#9, AllocatedObjectsClasses[i].InstanceSize, #9#9, AllocatedObjectsClasses[i].ClassName);
			WriteLn(OutputFile, #13#10'*** MEMCHK: End of allocation stats ***'#13#10);
		end;
	if ComputeMemoryUsageStats then
		begin
			WriteLn(OutputFile, #13#10'*** MEMCHK: Memory usage stats ***'#13#10);
			for i := 0 to MemoryUsageStatsCount - 1 do
				WriteLn(OutputFile, #9, MemoryUsageStats[i]);
			WriteLn(OutputFile, #13#10'*** MEMCHK: End of memory usage stats ***'#13#10);
		end;
	if KeepMaxMemoryUsage then
		WriteLn(OutputFile, #13#10'*** Biggest memory usage was: ', MaxMemoryUsage, ' ***' + #13#10#13#10#13#10);

	//Get and output the damaged blocks if necessary
	BadBlocks := TBlockList.Create;
	if CheckWipedBlocksOnTermination then
		begin
			GetBadBlocks(BadBlocks, MaxLeak, DoNotCheckWipedBlocksBiggerThan, StoppedDueToMax);

			WriteLn(OutputFile, #13#10'*** MEMCHK: Blocks written to after destruction ***'#13#10);
			if StoppedDueToMax then
				WriteLn(OutputFile, #9'Number of bad blocks not accurate  due to MaxLeak constant reached, but at least ', BadBlocks.Count, #13#10)
			else
				WriteLn(OutputFile, #9'Bad blocks count: ', BadBlocks.Count, #13#10);

			for i := 0 to BadBlocks.Count - 1 do
				begin
					WriteLn(OutputFile, #9'* Destroyed block damaged');
					WriteLn(OutputFile, #9#9'Call stack at allocation time:');
					Write(OutputFile, CallStackTextualRepresentation(BadBlocks.Item(i).CallerAddress, #9#9#9));
					WriteLn(OutputFile, #9#9'Destroyed at: ', TextualDebugInfoForAddress(Cardinal(BadBlocks.Item(i).DestructionAdress)));
				end;

			WriteLn(OutputFile, #13#10'*** MEMCHK: End of blocks written to after destruction ***'#13#10);
		end;
	BadBlocks.Destroy;

	//Save and display the output file
	Close(OutputFile);
	if FileExists(MemCheckLogFileName) then
		DeleteFile(MemCheckLogFileName);
	Rename(OutputFile, MemCheckLogFileName);
	if ShowLogFileWhenUseful and (LeaksList.Count > 0) or CollectStatsAboutObjectAllocation or ComputeMemoryUsageStats or KeepMaxMemoryUsage then
		WinExec(PChar(NotepadApp + ' ' + MemCheckLogFileName), sw_Show);

	//Release the memory
	for i := 0 to LeaksList.Count - 1 do
		LeaksList.Item(i).Destroy;
	LeaksList.Destroy;
	ChronogicalInfo.Destroy;
end;

procedure AddTimeStampInformation(const I: string);
begin
	InitializeOnce;

	if TimeStampsCount = TimeStampsAllocated then
		begin
			if TimeStampsAllocated = 0 then
				TimeStampsAllocated := 10;
			TimeStampsAllocated := TimeStampsAllocated * 2;

			UnMemChk;
			ReallocMem(TimeStamps, TimeStampsAllocated * sizeof(WideString));
			ZeroMemory(pointer(integer(TimeStamps) + TimeStampsCount * sizeof(WideString)), (TimeStampsAllocated - TimeStampsCount) * SizeOf(WideString));
			MemChk;
		end;

	TimeStamps[TimeStampsCount] := I + ' (Time stamp: ' + IntToStr(TimeStampsCount) + ')';
	TimeStampsCount := TimeStampsCount + 1;
end;

procedure MemChk;
const
	LeakTrackingMemoryManager: TMemoryManager = (
		GetMem: LeakTrackingGetMem;
		FreeMem: LeakTrackingFreeMem;
		ReallocMem: LeakTrackingReallocMem;
		);
	HeapCheckingMemoryManager: TMemoryManager = (
		GetMem: HeapCheckingGetMem;
		FreeMem: HeapCheckingFreeMem;
		ReallocMem: HeapCheckingReallocMem;
		);
begin
	assert(sizeof(TMemoryBlocHeader) mod 8 = 0, 'SizeOf(TMemoryBlocHeader) in MemCheck should be a multiple of 8');

	if not MemCheckActive then
		begin
			InitializeOnce;
			if CheckHeapStatus then
				begin
					SetMemoryManager(HeapCheckingMemoryManager);
					UpdateLastHeapStatus;
				end
			else
				SetMemoryManager(LeakTrackingMemoryManager);
			MemCheckActive := True;
		end;
end;

procedure CommitReleases;
var
	Block, BlockToFree, previous: PMemoryBlocHeader;
begin
	InitializeOnce;

	Block := LastBlock;
	Previous := nil;

	while Block <> nil do
		begin
			BlockToFree := Block;
			Block := Block.PreceedingBlock;

			if MemoryBlockFreed(BlockToFree) then
				begin
					if LastBlock = BlockToFree then
						LastBlock := Block;

					if previous <> nil then
						previous.PreceedingBlock := Block;

					OldMemoryManager.FreeMem(BlockToFree);
				end
			else
				previous := BlockToFree;
		end;
end;

function CallStackTextualRepresentation(const S: TCallStack; const LineHeader: string): string;
var
	i: integer;
begin
	i := 0;
	Result := '';

	while (i <= StoredCallStackDepth) and (S[i] <> nil) do
		begin
			Result := Result + LineHeader + 'call stack - ' + IntToStr(i) + ' : ' + TextualDebugInfoForAddress(Cardinal(S[i])) + #13#10;
			i := i + 1;
		end;
end;

var
	Displ: Cardinal;
	{Displ is the displacement of the code in the executable file. The code in SetDispl was written by Juan Vidal Pich}

procedure SetDispl;
var
	NTHeader: PImageFileHeader;
	NTOptHeader: PImageOptionalHeader;
begin
	//-> If you have a compilation error in this routine and you are compiling with delphi 4, I'd say that you did not install the Delphi update pack 3

	NTHeader := PImageFileHeader(Cardinal(PImageDosHeader(HInstance)._lfanew) + HInstance + 4); {SizeOf(IMAGE_NT_SIGNATURE) = 4}
	NTOptHeader := PImageOptionalHeader(Cardinal(NTHeader) + IMAGE_SIZEOF_FILE_HEADER);
	Displ := HInstance + NTOptHeader.BaseOfCode;
	//Result := HInstance + PImageNtHeaders(LongInt(HInstance)+PImageDosHeader(HInstance)^._lfanew)^.OptionalHeader.BaseOfCode;
end;

function CardinalToHexa(i: Cardinal): string;
const
	HexChars: array[0..15] of char = ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
var
	J: integer;
begin
	Result := '';
	for j := 1 to 8 do
		begin
			Result := HexChars[i and $0F] + Result;
			I := I shr 4;
		end;
end;

function TextualDebugInfoForAddress(const TheAddress: Cardinal): string;
{$IFNDEF USE_JEDI_JCL}
var
	U: TUnitDebugInfos;
	AddressInDebugInfos: Cardinal;
{$ENDIF}
begin
{$IFNDEF USE_JEDI_JCL}
	InitializeOnce;

	if UseDebugInfos and (TheAddress > Displ) then
		begin
			AddressInDebugInfos := TheAddress - Displ;

			U := UnitWhichContainsAddress(AddressInDebugInfos);

			if U <> nil then
				Result := 'Module ' + U.Name + RoutineWhichContainsAddress(AddressInDebugInfos) + U.LineWhichContainsAddress(AddressInDebugInfos)
			else
				Result := RoutineWhichContainsAddress(AddressInDebugInfos);
		end
	else
		Result := NoDebugInfo;

	Result := Result + ' Find error: ' + CardinalToHexa(TheAddress);
{$ELSE}
	Result := PointerToDebugInfo(Pointer(TheAddress));
{$ENDIF}
end;

procedure dummy;
{This procedure is never called. It is used for computing the address of MemCheck's finalization.
Hence, it MUST be just before the finalization and be empty. If you want to change that, you'll have
to change the way memcheck's finalization is seeked}
begin
end;

initialization
	MemCheckLogFileName := ChangeFileExt(ParamStr(0), MemCheckLogFileNameSuffix);
finalization
	if ExceptProc = @MyExceptProc then
		{Exception logger installed}
		Close(SevereExceptionsLogFile);

	if MemCheckInitialized then
		begin
			if MemCheckActive then
				begin
					UnMemChk;
					OutputAllCollectedInformation;
					GoThroughAllocatedBlocks;
				end;

			if CheckHeapStatus then
				HeapStatusSynchro.Destroy;
			LinkedListSynchro.Destroy;
			FreeMem(TimeStamps);
			FreeMem(AllocatedInstances);
			OutOfMemory.Destroy;
			MemCheckLogFileName := MemCheckLogFileName + '.$$$';
			if FileExists(MemCheckLogFileName) then
				DeleteFile(MemCheckLogFileName);
		end;
end.


