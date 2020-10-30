# TurboPack SynEdit

Updated for **10.4 Sydney** / VER340 / PKG 270

The master branch remains compatible with Delphi 10.1 Berlin or later. You can also access the [10.3 Rio](https://github.com/TurboPack/SynEdit/releases/tag/103RIO), [10.2 Tokyo](https://github.com/TurboPack/SynEdit/releases/tag/102Tokyo) and [10.1 Berlin](https://github.com/TurboPack/SynEdit/releases/tag/101Berlin) releases.

### Table of contents
1.  [Introduction](#Introduction)
2.  [Package names](#Package-names)
3.  [Installation](#Installation)

---

## Introduction

![SynEdit](https://raw.githubusercontent.com/TurboPack/SynEdit/master/Doc/SynEdit-1.3.png "TurboPower SynEdit")


SynEdit is a syntax highlighting edit control, not based on the Windows 
common controls. SynEdit is compatible with both Delphi and C++ Builder.

This is a source-only release of TurboPack SynEdit. It includes
designtime and runtime packages for Delphi and C++Builder and supports Win32 and Win64.

---

## Package names

TurboPack SynEdit package names have the following form:

Delphi
* SynEditDR.bpl (Delphi Runtime)
* SynEditDD.bpl (Delphi Designtime)

C++Builder
* SynEditCR.bpl (C++Builder Runtime)
* SynEditCD.bpl (C++Builder Designtime)

---

## Installation

TurboPack SynEdit is available via the [GetIt Package Manager](http://docwiki.embarcadero.com/RADStudio/en/Installing_a_Package_Using_GetIt_Package_Manager) where you can quickly and easily install and uninstall it.

To manually install TurboPack SynEdit into your IDE, take the following
steps:

1. Unzip the release files into a directory (e.g., d:\SynEdit).

2. Start RAD Studio.

3. Add the source subdirectory (e.g., d:\SynEdit\source) to the IDE's library path. For CBuilder, add the hpp subdirectory
(e.g., d:\SynEdit\source\hpp\Win32\Release) to the IDE's system include path.

4. Open & install the designtime package specific to the IDE being used. The IDE should notify you the components have been
installed.
