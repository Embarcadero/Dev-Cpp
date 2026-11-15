# Building Dev-C++ with DPM

The VCL libraries under `Source\VCL` can be built and consumed as
[DPM](https://github.com/DelphiPackageManager/DPM) packages (DPM 1.0 or later).
This is optional: the vendored sources and the search paths in `devcpp.dproj`
are unchanged, so the build described in `README.txt` still works.

DPM gives you two things the manual build does not:

* the design-time packages are compiled and installed into the IDE for you, so
  opening `main.dfm` no longer stops with *Class TDevFileMonitor not found*;
* the libraries are compiled once per Delphi version into the DPM package cache
  instead of with every build of `devcpp.exe`.

Supported: Delphi 11, 12 and 13, Win32 (`devcpp.exe` is Win32-only).

## Packages

| Package id                 | Spec                                                   | Contents |
|----------------------------|--------------------------------------------------------|----------|
| `DevCpp.SynEdit`           | `VCL\SynEdit\DevCpp.SynEdit.dspec.yaml`                | Dev-C++'s copy of SynEdit (unscoped unit names) |
| `DevCpp.SVGIconImageList`  | `VCL\SVGIconImageList\DevCpp.SVGIconImageList.dspec.yaml` | SVG engine + VCL image list components |
| `DevCpp.DDetours`          | `VCL\DDetours\DevCpp.DDetours.dspec.yaml`              | DDetours 2.2 (runtime only) |
| `DevCpp.VclStylesUtils`    | `VCL\vcl-styles-utils\DevCpp.VclStylesUtils.dspec.yaml` | The vcl-styles-utils units Dev-C++ uses (runtime only, depends on DDetours) |
| `DevCpp.Components`        | `VCL\DevCpp.Components.dspec.yaml`                     | TDevFileMonitor, TDevShortcuts, TCompOptionsList |
| `DevCpp.ClassBrowsing`     | `VCL\ClassBrowsing\DevCpp.ClassBrowsing.dspec.yaml`    | TCppParser, TCppTokenizer, TCppPreprocessor, TClassBrowser, TCodeCompletion |

The third-party libraries carry a `DevCpp.` prefix because they are the copies
vendored (and in places modified) in this repository; they are not
interchangeable with the packages of the same libraries on the public DPM feed.

## Setting up

You need:

* Delphi 11, 12 or 13;
* the VCL styles listed in `README.txt`, from GetIt. They belong to
  Embarcadero and are not distributed as DPM packages;
* [DPM](https://github.com/DelphiPackageManager/DPM/releases) 1.0 or later,
  with its IDE plugin (the DPM installer adds it).

### With the setup script

```bat
Source\dpm-setup.cmd                 rem newest of Delphi 13/12/11 installed
Source\dpm-setup.cmd -Delphi 11.0    rem or choose the version
```

The script checks the prerequisites (DPM, Delphi, the styles `devcpp.dproj`
embeds), registers a folder feed (`%LOCALAPPDATA%\DevCpp\dpm-feed`, DPM source
`DevCppLocal`), packs the six packages into it and installs them into
`devcpp.dproj`. Run it again after changing a spec or a library; add `-Clean`
to make DPM rebuild the packages it has already built. `-CheckOnly` runs just
the checks. Then open `devcpp.dproj` in that Delphi version.

### By hand

The same steps, from `Source\`, with `dpm` on the path (batch-file syntax; at
an interactive prompt use `%s` instead of `%%s`):

```bat
rem 1. A folder feed for the locally built packages (any empty folder will do)
dpm sources add -name=DevCppLocal -source=D:\dpm-feed

rem 2. Pack all six specs into it
for %%s in (
  VCL\DDetours\DevCpp.DDetours.dspec.yaml
  VCL\vcl-styles-utils\DevCpp.VclStylesUtils.dspec.yaml
  VCL\SVGIconImageList\DevCpp.SVGIconImageList.dspec.yaml
  VCL\SynEdit\DevCpp.SynEdit.dspec.yaml
  VCL\DevCpp.Components.dspec.yaml
  VCL\ClassBrowsing\DevCpp.ClassBrowsing.dspec.yaml
) do dpm pack %%s -o=D:\dpm-feed

rem 3. Reference them from the project (use your Delphi version)
dpm install DevCpp.SynEdit,DevCpp.SVGIconImageList,DevCpp.VclStylesUtils,DevCpp.Components,DevCpp.ClassBrowsing devcpp.dproj -compiler=11.0 -s=DevCppLocal
```

Then open `devcpp.dproj` in the IDE. The DPM IDE plugin restores the packages
and installs the design-time packages. (In PowerShell, quote `"-compiler=11.0"`,
or it splits the argument at the dot.)

After packing again (a changed spec or library), refresh the project with
`dpm restore devcpp.dproj -compiler=11.0 -s=DevCppLocal -ihl`, and to rebuild
packages DPM has already built, first remove them from its cache:
`dpm cache remove <package id> -compiler=11.0 -f`.

### Notes

`dpm install` records each package's manifest hash in `devcpp.dproj`. DPM packs
are not byte-reproducible, so every developer who packs locally gets different
hashes. For that reason `devcpp.dproj` in the repository does **not** reference
the packages. Keep that change local (`-ihl` above refreshes the hashes).

`devcpp.dproj` keeps its search paths to the sources under `Source\VCL`, after
the DPM package folders. A full build of `devcpp.exe` therefore compiles the
library units from those sources, so changes to them take effect without
repacking; DPM provides the design-time packages for the IDE. To build against
the package `.dcu` files only, remove the `VCL\...` entries from the search
path in your local `devcpp.dproj`.

Each package carries its licence text in a `licenses` folder (or `LICENSE`).

## Maintaining the specs

Each spec lives next to its library. Its package projects are in
`packages\RAD Studio <ver>\` (or `Packages\` where that folder already
existed). They are ordinary `.dpk`/`.dproj` files, so edit them like any
package. When you change the set of units, update every version folder.

To add a Delphi version, add a `targetPlatforms` entry to each spec and run
`dpm prepare <spec>`. It copies the newest existing pair into the new folder
and applies the per-version changes (ProjectVersion, LIBSUFFIX AUTO).
`prepare` skips existing files, so it will not overwrite hand edits.

Things that are easy to break, and why they are the way they are:

* **Unit scope names.** Dev-C++ uses unscoped VCL/RTL units (`Forms`,
  `SysUtils`, `DB`, `Spin`, ...). Each package `.dproj` sets
  `DCC_Namespace` to `System;Winapi;Vcl;Vcl.Imaging;Vcl.Shell;Vcl.Samples;Data;...`.
  For the same reason the specs do not use DPM's `package definitions` (the
  projects DPM generates have no `Vcl` scope).
* **Register procedures.** The Dev-C++ components (`DevCpp.Components`,
  `DevCpp.ClassBrowsing`) register themselves from the component units, so
  their design packages *contain* the units rather than requiring the runtime
  package. SynEdit and SVGIconImageList have separate registration units, so
  their design packages require the runtime package.
* **Forms and resources.** DPM puts only `lib\<platform>` on a consuming
  project's search path, and package projects may not declare msbuild targets.
  So the specs copy `.dfm`/`.res` files into `lib/Win32` with source entries,
  and `DevCpp.VclStylesUtils` writes `AwesomeFont.res` straight into the `.dcu`
  folder (`BRCC_OutputDir=$(DCC_DcuOutput)\`).
* **cgrc instead of brcc32.** `brcc32` silently writes an empty `.res` when the
  path contains a folder whose name starts with `.`, and the DPM cache lives in
  `%APPDATA%\.dpm`. Projects that compile `.rc` files set
  `BRCC_CompilerToUse=rc`.
* **CharUtils.pas** lives in `VCL\ClassBrowsing` because the parser units use it.
  If it is also compiled from the application folder, a DPM build fails with
  F2051 (*unit compiled with a different version*).

## Verified

Built from a clean package cache with Delphi 11, 12 and 13:

* all six packages, both runtime and design;
* `devcpp.exe` with Delphi 11, both against the DPM packages only (unit search
  path limited to the package `lib` folders) and against the vendored sources
  only;
* the Delphi 11 IDE with the packages installed via DPM: the project opens and
  a Release build of `devcpp.exe` runs;
* `dpm-setup.cmd` on a fresh checkout with Delphi 11 (first run, a repeat run
  and `-Clean`), followed by a successful build of `devcpp.exe`.
