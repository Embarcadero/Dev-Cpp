<#
.SYNOPSIS
  Installs the VCL styles that devcpp.dproj embeds, from GetIt, using GetItCmd.

.DESCRIPTION
  devcpp.dproj embeds a set of VCL styles (Project Options > Application >
  Appearance). Some ship with Delphi; the others come from GetIt. Building
  devcpp.exe fails while any of them is missing.

  This script reads the style files from devcpp.dproj, checks which are
  missing from the chosen Delphi version's Styles folder, looks each one up in
  GetIt (package VCLStyle-<file name>-<version>, newest version) and installs
  them with GetItCmd.exe, the command-line GetIt client in RAD Studio's bin
  folder. Close RAD Studio first.

  Installing accepts the licence agreements (EULAs) of the style packages on
  your behalf, so it needs -AcceptEulas. Without it the script only reports
  what is missing and the command that would install it.

.PARAMETER Delphi
  The Delphi version: 10.4, 11.0, 12.0 or 13.0. Default: the newest installed.
.PARAMETER Project
  The project whose styles to install. Default: devcpp.dproj next to this script.
.PARAMETER AcceptEulas
  Install the missing styles, accepting their licence agreements.
.PARAMETER CheckOnly
  Only report which styles are missing.

.EXAMPLE
  InstallStyles.cmd
.EXAMPLE
  InstallStyles.cmd -Delphi 12.0 -AcceptEulas
#>
[CmdletBinding()]
param(
  [ValidateSet('10.4', '11.0', '12.0', '13.0')] [string] $Delphi,
  [string] $Project,
  [switch] $AcceptEulas,
  [switch] $CheckOnly
)
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version 2

$BdsVersion = @{ '10.4' = '21.0'; '11.0' = '22.0'; '12.0' = '23.0'; '13.0' = '37.0' }

function Ok([string] $Text)   { Write-Host "   ok    $Text" -ForegroundColor Green }
function Warn([string] $Text) { Write-Host "   WARN  $Text" -ForegroundColor Yellow }
function Fail([string] $Text) { Write-Host "   FAIL  $Text" -ForegroundColor Red; exit 1 }

function Get-DelphiRoot([string] $Bds) {
  foreach ($key in "HKLM:\SOFTWARE\WOW6432Node\Embarcadero\BDS\$Bds", "HKLM:\SOFTWARE\Embarcadero\BDS\$Bds") {
    $p = Get-ItemProperty -Path $key -Name RootDir -ErrorAction SilentlyContinue
    if ($p -and (Test-Path (Join-Path $p.RootDir 'bin\dcc32.exe'))) { return $p.RootDir }
  }
  return $null
}

function Get-BdsCommonDir([string] $Root, [string] $Bds) {
  # $(BDSCOMMONDIR) as rsvars.bat sets it; the default if it cannot be read.
  $rsvars = Join-Path $Root 'bin\rsvars.bat'
  if (Test-Path $rsvars) {
    $m = [regex]::Match([IO.File]::ReadAllText($rsvars), '(?im)^\s*@?SET\s+BDSCOMMONDIR=(.+?)\s*$')
    if ($m.Success) { return $m.Groups[1].Value }
  }
  return Join-Path $env:PUBLIC "Documents\Embarcadero\Studio\$Bds"
}

function Get-ProjectStyles([string] $Path, [string] $CommonDir) {
  # <Custom_Styles>Name|VCLSTYLE|path;"Name|VCLSTYLE|path";...</Custom_Styles>
  $text = [IO.File]::ReadAllText($Path)
  $m = [regex]::Match($text, '<Custom_Styles>([^<]*)</Custom_Styles>')
  if (-not $m.Success) { return @() }
  $styles = @()
  foreach ($entry in ($m.Groups[1].Value -replace '&quot;', '' -split ';')) {
    $parts = $entry.Trim().Split('|')
    if ($parts.Count -lt 3) { continue }
    $styles += [pscustomobject]@{
      Name = $parts[0]
      File = $parts[2].Replace('$(BDSCOMMONDIR)', $CommonDir)
    }
  }
  return $styles
}

function Find-GetItStyle([string] $GetItCmd, [string] $FileName) {
  # The GetIt package for a style file X.vsf is VCLStyle-X-<version>. Returns
  # the id of the newest version on offer, or $null.
  $base = [IO.Path]::GetFileNameWithoutExtension($FileName)
  $prefix = "VCLStyle-$base-"
  $out = & $GetItCmd "-l=$base" '-f=all' 2>&1 | ForEach-Object { "$_" }
  $best = $null; $bestVersion = $null
  foreach ($line in $out) {
    $m = [regex]::Match($line, '^\s*(\S+)\s+(\d+(?:\.\d+)*)\s')
    if (-not $m.Success) { continue }
    $id = $m.Groups[1].Value
    if (-not $id.StartsWith($prefix, [StringComparison]::OrdinalIgnoreCase)) { continue }
    $version = [version]($m.Groups[2].Value + $(if ($m.Groups[2].Value -notmatch '\.') { '.0' }))
    if (-not $bestVersion -or $version -gt $bestVersion) { $best = $id; $bestVersion = $version }
  }
  return $best
}

# --------------------------------------------------------------------------
# Windows PowerShell 5.1 has no $PSScriptRoot yet when it evaluates parameter defaults.
if (-not $Project) { $Project = Join-Path (Split-Path -Parent $MyInvocation.MyCommand.Path) 'devcpp.dproj' }
if (-not (Test-Path $Project)) { Fail "Project not found: $Project" }
$Project = (Resolve-Path $Project).Path

if (-not $Delphi) {
  $Delphi = @('13.0', '12.0', '11.0', '10.4') | Where-Object { Get-DelphiRoot $BdsVersion[$_] } | Select-Object -First 1
  if (-not $Delphi) { Fail 'No supported Delphi found (10.4 to 13). Use -Delphi to choose one explicitly.' }
}
$bds = $BdsVersion[$Delphi]
$root = Get-DelphiRoot $bds
if (-not $root) { Fail "Delphi $Delphi (BDS $bds) is not installed" }
$getItCmd = Join-Path $root 'bin\GetItCmd.exe'
$commonDir = Get-BdsCommonDir $root $bds
Write-Host "Delphi $Delphi at $root"
Write-Host "Styles of $Project"

$styles = @(Get-ProjectStyles $Project $commonDir)
if (-not $styles) { Ok 'the project embeds no styles'; exit 0 }

$missing = @()
foreach ($s in $styles) {
  if (Test-Path $s.File) { Ok $s.Name }
  else { $missing += $s }
}
if (-not $missing) { Write-Host ''; Write-Host 'All styles are installed.' -ForegroundColor Green; exit 0 }

if (-not (Test-Path $getItCmd)) {
  foreach ($s in $missing) { Warn "$($s.Name) is missing ($($s.File))" }
  Fail "GetItCmd.exe not found in $root\bin. Install the styles from GetIt in the IDE (Tools > GetIt Package Manager)."
}

$ids = @()
$notOnGetIt = @()
foreach ($s in $missing) {
  $id = Find-GetItStyle $getItCmd $s.File
  if ($id) { Warn "$($s.Name) is missing - GetIt package $id"; $ids += $id }
  else { Warn "$($s.Name) is missing ($($s.File)) and GetIt for Delphi $Delphi does not offer it"; $notOnGetIt += $s }
}
if ($notOnGetIt) {
  Write-Host '         Styles not on GetIt ship with Delphi: check the installation, or copy the .vsf file into'
  Write-Host "         $commonDir\Styles"
}
if (-not $ids) { exit 1 }

$command = "`"$getItCmd`" -i=$($ids -join ';') -ae"
if ($CheckOnly -or -not $AcceptEulas) {
  Write-Host ''
  Write-Host 'To install them, close RAD Studio and run this script again with -AcceptEulas, which accepts'
  Write-Host 'the licence agreements of the style packages for you. Or run:'
  Write-Host "  $command"
  exit 1
}

$ide = Get-Process bds -ErrorAction SilentlyContinue |
  Where-Object { $_.Path -and $_.Path.StartsWith($root, [StringComparison]::OrdinalIgnoreCase) }
if ($ide) { Fail "RAD Studio $Delphi is running. Close it and run this script again." }

Write-Host ''
Write-Host "Installing $($ids -join ', ') (licence agreements accepted with -ae)"
& $getItCmd "-i=$($ids -join ';')" '-ae' 2>&1 | ForEach-Object { Write-Host "         $_" }
if ($LASTEXITCODE -ne 0) { Fail "GetItCmd failed (exit code $LASTEXITCODE)" }

Write-Host ''
$stillMissing = @($missing | Where-Object { -not (Test-Path $_.File) })
foreach ($s in $missing) {
  if (Test-Path $s.File) { Ok "$($s.Name) installed" }
}
if ($stillMissing) {
  foreach ($s in $stillMissing) { Warn "$($s.Name) is still missing ($($s.File))" }
  exit 1
}
Write-Host 'All styles are installed.' -ForegroundColor Green
