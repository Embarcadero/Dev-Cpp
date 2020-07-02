TurboPack Abbrevia


Table of contents

1.  Introduction
2.  Package names
3.  Installation

==============================================


1. Introduction


Abbrevia is a compression toolkit for Delphi and C++Builder.
It supports compressing and decompressing PKZIP, Microsoft CAB, tar, gzip, and 
bzip2 archives, and can create self-extracting executables. On Windows it also 
provides Delphi wrappers for the LZMA, Bzip2, and WavPack SDKs, and PPMd decompression.

Abbrevia also has several visual controls that simplify displaying and manipulating 
archives, including treeview and listview components.

This is a source-only release of TurboPack Abbrevia. It includes
designtime and runtime packages for Delphi and CBuilder and supports 
Win32, Win64, OSX, iOS and Android.

==============================================

2. Package names


TurboPack Abbrevia package names have the following form:

AbbreviaD.bpl          (Delphi Runtime for all platforms)
AbbreviaVCLD.bpl       (Delphi Runtime for the VCL)
AbbreviaVCLDDesign.bpl (Delphi Designtime VCL)
AbbreviaFMXDDesign.bpl (Delphi Designtime FMX)

AbbreviaC.bpl          (C++Builder Runtime for all platforms)
AbbreviaVCLC.bpl       (C++Builder Runtime for the VCL)
AbbreviaVCLCDesign.bpl (C++Builder Designtime VCL)
AbbreviaFMXCDesign.bpl (C++Builder Designtime FMX)


==============================================

3. Installation


To install TurboPack Abbrevia into your IDE, take the following
steps:

  1. Unzip the release files into a directory (e.g., d:\abbrevia).

  2. Start RAD Studio.

  3. Add the source subdirectory (e.g., d:\abbrevia\source) to the
     IDE's library path. For C++Builder, add the hpp subdirectory
     (e.g., d:\abbrevia\source\hpp\Win32\Release) to the IDE's system include path.

  4. Open & install the designtime package specific to the IDE being
     used. The IDE should notify you the components have been
     installed.
