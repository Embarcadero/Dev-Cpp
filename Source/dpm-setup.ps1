<#
.SYNOPSIS
  Sets up Dev-C++ development with DPM: checks prerequisites, packs the
  DevCpp.* packages into a local feed and installs them into devcpp.dproj.

.DESCRIPTION
  Steps (each one is also described in BUILDING-DPM.md, for doing it by hand):
    1. Check that DPM 1.x and the chosen Delphi version are installed, and that
       the VCL styles devcpp.dproj embeds are present (they come from GetIt).
    2. Register a folder feed with DPM (default name DevCppLocal).
    3. Pack the six DevCpp.* specs into that feed.
    4. Install the packages into devcpp.dproj, or refresh them if the project
       already references them.
  Afterwards open devcpp.dproj in that Delphi version; DPM builds the packages
  and installs the design-time packages into the IDE.

  Step 4 edits devcpp.dproj. Keep that change local - do not commit it.

.PARAMETER Delphi
  11.0, 12.0 or 13.0. Default: the newest of these that is installed.
.PARAMETER Feed
  Folder for the local package feed. Default: %LOCALAPPDATA%\DevCpp\dpm-feed
.PARAMETER SourceName
  DPM source name for the feed. Default: DevCppLocal
.PARAMETER Clean
  Remove the DevCpp.* packages for this Delphi version from the DPM package
  cache first, so changes to the specs or sources are rebuilt.
.PARAMETER CheckOnly
  Only run the checks in step 1.

.EXAMPLE
  dpm-setup.cmd
.EXAMPLE
  dpm-setup.cmd -Delphi 12.0 -Clean
#>
[CmdletBinding()]
param(
  [ValidateSet('11.0', '12.0', '13.0')] [string] $Delphi,
  [string] $Feed = (Join-Path $env:LOCALAPPDATA 'DevCpp\dpm-feed'),
  [string] $SourceName = 'DevCppLocal',
  [switch] $Clean,
  [switch] $CheckOnly
)
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version 2

$SourceDir = $PSScriptRoot
$Project   = Join-Path $SourceDir 'devcpp.dproj'
$BdsVersion = @{ '11.0' = '22.0'; '12.0' = '23.0'; '13.0' = '37.0' }
$Specs = @(
  'VCL\DDetours\DevCpp.DDetours.dspec.yaml',
  'VCL\vcl-styles-utils\DevCpp.VclStylesUtils.dspec.yaml',
  'VCL\SVGIconImageList\DevCpp.SVGIconImageList.dspec.yaml',
  'VCL\SynEdit\DevCpp.SynEdit.dspec.yaml',
  'VCL\DevCpp.Components.dspec.yaml',
  'VCL\ClassBrowsing\DevCpp.ClassBrowsing.dspec.yaml'
)
$PackageIds = $Specs | ForEach-Object { [IO.Path]::GetFileName($_) -replace '\.dspec\.yaml$', '' }
# DevCpp.DDetours comes in as a dependency of DevCpp.VclStylesUtils.
$TopLevelIds = $PackageIds | Where-Object { $_ -ne 'DevCpp.DDetours' }

function Step([string] $Text) { Write-Host ''; Write-Host "== $Text" -ForegroundColor Cyan }
function Ok([string] $Text)   { Write-Host "   ok    $Text" -ForegroundColor Green }
function Warn([string] $Text) { Write-Host "   WARN  $Text" -ForegroundColor Yellow }
function Fail([string] $Text) { Write-Host "   FAIL  $Text" -ForegroundColor Red; exit 1 }

function Invoke-Dpm {
  # Runs dpm with each argument passed as one string (PowerShell would split
  # an unquoted -compiler=11.0 at the dot). Returns the output lines; throws
  # on a non-zero exit code unless -AllowFail.
  param([string[]] $Arguments, [switch] $AllowFail, [switch] $Quiet)
  $out = & dpm @Arguments -nb -non-interactive 2>&1 | ForEach-Object { "$_" }
  $code = $LASTEXITCODE
  if (-not $Quiet) { $out | ForEach-Object { Write-Host "         $_" } }
  if ($code -ne 0 -and -not $AllowFail) {
    if ($Quiet) { $out | Select-Object -Last 15 | ForEach-Object { Write-Host "         $_" } }
    Fail "dpm $($Arguments[0]) failed (exit code $code)"
  }
  return $out
}

function Get-DelphiRoot([string] $Bds) {
  foreach ($key in "HKLM:\SOFTWARE\WOW6432Node\Embarcadero\BDS\$Bds", "HKLM:\SOFTWARE\Embarcadero\BDS\$Bds") {
    $p = Get-ItemProperty -Path $key -Name RootDir -ErrorAction SilentlyContinue
    if ($p -and (Test-Path (Join-Path $p.RootDir 'bin\dcc32.exe'))) { return $p.RootDir }
  }
  return $null
}

# --------------------------------------------------------------------------
Step '1. Prerequisites'

if (-not (Test-Path $Project)) { Fail "devcpp.dproj not found next to this script ($SourceDir)" }

$dpmCmd = Get-Command dpm -ErrorAction SilentlyContinue
if (-not $dpmCmd) {
  Fail 'dpm is not on the PATH. Install DPM 1.0 or later: https://github.com/DelphiPackageManager/DPM/releases'
}
$dpmVersion = (Get-Item $dpmCmd.Source).VersionInfo.ProductVersion
if ([int]($dpmVersion -split '\.')[0] -lt 1) { Fail "DPM $dpmVersion found; version 1.0 or later is required (run: dpm upgrade)" }
Ok "DPM $dpmVersion"

if (-not $Delphi) {
  $Delphi = @('13.0', '12.0', '11.0') | Where-Object { Get-DelphiRoot $BdsVersion[$_] } | Select-Object -First 1
  if (-not $Delphi) { Fail 'No supported Delphi found (11, 12 or 13). Use -Delphi to choose one explicitly.' }
}
$bds = $BdsVersion[$Delphi]
$root = Get-DelphiRoot $bds
if (-not $root) { Fail "Delphi $Delphi (BDS $bds) is not installed" }
Ok "Delphi $Delphi at $root"

# The styles devcpp.dproj embeds: <Custom_Styles>Name|VCLSTYLE|path;...</Custom_Styles>
$commonDir = Join-Path $env:PUBLIC "Documents\Embarcadero\Studio\$bds"
$proj = [IO.File]::ReadAllText($Project)
$m = [regex]::Match($proj, '<Custom_Styles>([^<]*)</Custom_Styles>')
$missingStyles = 0
if ($m.Success) {
  foreach ($entry in ($m.Groups[1].Value -replace '&quot;', '' -split ';')) {
    if (-not $entry.Trim()) { continue }
    $parts = $entry.Split('|')
    if ($parts.Count -lt 3) { continue }
    $file = $parts[2].Replace('$(BDSCOMMONDIR)', $commonDir)
    if (Test-Path $file) { Ok "style $($parts[0])" }
    else { Warn "style $($parts[0]) is missing ($file) - install it from GetIt"; $missingStyles++ }
  }
}
if ($missingStyles) { Warn "$missingStyles style(s) missing: the packages will build, but devcpp.exe will not until they are installed" }

if ($CheckOnly) { Write-Host ''; exit 0 }

# --------------------------------------------------------------------------
Step "2. Local feed '$SourceName' at $Feed"

New-Item -ItemType Directory -Force -Path $Feed | Out-Null
$Feed = (Resolve-Path $Feed).Path
$listing = (Invoke-Dpm @('sources', 'list') -Quiet) -join "`n"
$existing = [regex]::Match($listing, "(?m)^\s*\d+\.\s+$([regex]::Escape($SourceName))\s+\[(\w+)\][^\n]*\n\s+(\S[^\n]*)")
if (-not $existing.Success) {
  Invoke-Dpm @('sources', 'add', "-name=$SourceName", "-source=$Feed") -Quiet | Out-Null
  Ok "added DPM source $SourceName"
} elseif ($existing.Groups[2].Value.Trim().TrimEnd('\') -ne $Feed.TrimEnd('\')) {
  Fail "DPM source $SourceName already points at $($existing.Groups[2].Value.Trim()). Run again with -Feed `"$($existing.Groups[2].Value.Trim())`", or pick another -SourceName."
} else {
  if ($existing.Groups[1].Value -ne 'Enabled') { Invoke-Dpm @('sources', 'enable', "-name=$SourceName") -Quiet | Out-Null }
  Ok "DPM source $SourceName already registered"
}

# --------------------------------------------------------------------------
Step '3. Pack the DevCpp.* packages'

foreach ($spec in $Specs) {
  Invoke-Dpm @('pack', (Join-Path $SourceDir $spec), "-o=$Feed") -Quiet | Out-Null
  Ok ([IO.Path]::GetFileName($spec))
}

if ($Clean) {
  foreach ($id in $PackageIds) {
    Invoke-Dpm @('cache', 'remove', $id, "-compiler=$Delphi", '-f') -Quiet -AllowFail | Out-Null
  }
  Ok "removed cached DevCpp.* packages for Delphi $Delphi"
}

# --------------------------------------------------------------------------
Step "4. Reference the packages from devcpp.dproj (Delphi $Delphi)"

$proj = [IO.File]::ReadAllText($Project)
if ($proj -match '<PackageReference id="DevCpp\.') {
  # Already referenced: refresh. -ihl updates the recorded package hashes,
  # which change every time the packages are packed again.
  Invoke-Dpm @('restore', $Project, "-compiler=$Delphi", "-s=$SourceName", '-ihl') -Quiet | Out-Null
  Ok 'restored (package hashes refreshed)'
} else {
  Invoke-Dpm @('install', ($TopLevelIds -join ','), $Project, "-compiler=$Delphi", "-s=$SourceName") -Quiet | Out-Null
  Ok 'installed'
}

Write-Host ''
Write-Host "Done. Open $Project in Delphi $Delphi." -ForegroundColor Green
Write-Host 'DPM builds the packages and installs the design-time packages into the IDE.'
Write-Host 'devcpp.dproj now references the DPM packages: keep that change local, do not commit it.'
if ($missingStyles) { Write-Host "Install the $missingStyles missing style(s) from GetIt before building devcpp.exe." -ForegroundColor Yellow }
