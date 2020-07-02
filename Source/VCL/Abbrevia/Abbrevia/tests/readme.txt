DUnit tests for Abbrevia
========================

This project tests most of the basic functionality of Abbrevia.  It should
catch any obvious errors, but there are plenty of cases that still aren't fully 
covered.

The "testfiles" subdirectory contains all of the data necessary for the tests, 
and care should be taken when modifying it.  Temporary data is written to 
"testfiles/temp".  The tests should clean up after themselves, but if you see 
any unexpected test failures you may need to delete it manually.  Any automated 
processes that access that directory may cause transient failures as well (anti-
virus, dropbox).

The console build does not test VCL/CLX components, but does work with 
FreePascal if it's compiled against DUnit2.

Setup:

- The WavPack data is too large to include in the repository, and is available 
as a separate download from the SourceForge project.  After downloading, 
extract the outer zip into "testfiles/WavPack".  This is relatively slow test, 
and is generally only needed if you're changing the WavPack support or adding 
new compilers/platforms.

- The Zip64 data is also available as a separate download from SourceForge.
Download any files in the "Test Files/Zip64" folder and copy them into the
"testfiles/Zip64" directory without extracting them.

- To test Unicode filename support, open "testfiles/Unicode" and extract one of 
the "OEM & ANSI" zips using WinZip or PowerArchiver into the "source" 
subdirectory.  The readme there includes original filenames for verification.

- If a: is a floppy drive the spanning tests will use it and prompt for disks 
interactively.  If not it relies on the ImDisk Virtual Disk Driver 
(http://www.ltr-data.se/opencode.html/#ImDisk).  When using ImDisk the tests 
are completely automated.


Known Failues:

TAbUnZipperTests/TestGZipDecompress
    This is a failure in the deflate engine (bug 822243).


Known Non-Bugs:

These tests can be disabled and safely ignored.  They show limitations in the library that should be fixed, but aren't necessary for correct operation.

TAbZipArchiveTests/Decompress Canterbury/DCLImpl
    The "DCL Implode" compression algorithm is an optional algorithm available
    in PKZip.  Abbrevia doesn't currently support it.

TAbZipArchiveTests/Decompress Unicode/OSX
    OS X stores accented Unicode characters differently than Windows.  For 
    proper handling the filenames should converted when storing and extracting.

TAbCabArchiveTests/TestAddFromStream
    TAbCabArchive.AddFromStream is not supported.  Cabs are solid archives, so 
    adding a single stream will require decompressing the cab and recreating it.
