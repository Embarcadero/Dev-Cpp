(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower Abbrevia
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Rudy Velthuis
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* Abbrevia: AbResString.pas 3.05                        *}
{*********************************************************}
{* Abbrevia: Resource strings, Dutch localization        *}
{*********************************************************}
unit AbResString;

interface

resourcestring
  AbErrZipInvalidS = 'Ongeldig bestand - geen PKZip bestand';
  AbZipVersionNeededS = 'Kan bestand niet ontpakken - nieuwere versie nodig';
  AbUnknownCompressionMethodS = 'Kan bestand niet ontpakken - niet ondersteunde compressiemethode';
  AbNoExtractionMethodS = 'Kan bestand niet ontpakken - ontpakken wordt niet ondersteund';
  AbInvalidPasswordS = 'Kan bestand niet ontpakken - ongeldig paswoord';
  AbNoInsertionMethodS = 'Kan bestand niet invoegen - invoegen wordt niet ondersteund';
  AbInvalidFactorS = 'Ongeldige reductiefactor';
  AbDuplicateNameS = 'Kan het bestand niet invoegen - dupliceert opgeslagen naam';
  AbUnsupportedCompressionMethodS = 'Kan het bestand niet invoegen - niet ondersteunde compressiemethode';
  AbUserAbortS = 'Proces werd door gebruiker afgebroken';
  AbArchiveBusyS = 'Archief is bezig - kan nieuwe aanvraag niet bewerken';
  AbLastDiskRequestS = 'Plaats laatste diskette van opgesplitst archief';
  AbDiskRequestS = 'Plaats diskette';
  AbImageRequestS = 'Bestandsnaam afbeelding';
  AbBadSpanStreamS = 'Opgesplitste archieven moeten als bestandsstroom geopend worden';
  AbDiskNumRequestS = 'Plaats diskette %d van opgesplitst archief';
  AbImageNumRequestS = 'Plaats segment %d van opgesplitst archief';
  AbNoOverwriteSpanStreamS = 'Kan bestaand opgesplitst archief niet veranderen';
  AbNoSpannedSelfExtractS = 'Kan geen zelfontpakkend opgesplitst archief aanmaken';
  AbBlankDiskS = 'Plaats een lege diskette';
  AbStreamFullS = 'Schrijffout stroom';
  AbNoSuchDirectoryS = 'Directory bestaat niet';
  AbInflateBlockErrorS = 'Kan blok niet ontpakken';
  AbBadStreamTypeS = 'Ongeldige stroom';
  AbTruncateErrorS = 'Fout bij het afknotten van het zip bestand';
  AbZipBadCRCS = 'Mislukte CRC controle';
  AbZipBadStubS = 'Stomp moet uitvoerbaar bestand zijn';
  AbFileNotFoundS = 'Bestand niet gevonden';
  AbInvalidLFHS = 'Ongeldig Local File Header element';
  AbNoArchiveS = 'Archief bestaat niet - lege bestandsnaam';
  AbReadErrorS = 'Fout tijdens lezen van archief';
  AbInvalidIndexS = 'Ongeldige index van archiefelement';
  AbInvalidThresholdS = 'Ongeldige drempel van archiefgrootte';
  AbUnhandledFileTypeS = 'Onbekend archieftype';
  AbSpanningNotSupportedS = 'Opsplitsen wordt voor dit archieftype niet ondersteund';
  AbLogCreateErrorS = 'Fout tijdens aanmaken van protocolbestand';
  AbMoveFileErrorS = 'Fout tijdens het verplaatsen van bestand %s naar %s';
  AbFileSizeTooBigS = 'Bestand is te groot voor dit archieftype';

  AbNoCabinetDllErrorS = 'Kan bestand cabinet.dll niet laden';
  AbFCIFileOpenErrorS = 'FCI kan bestand niet openen';
  AbFCIFileReadErrorS = 'FCI kan bestand niet lezen';
  AbFCIFileWriteErrorS = 'FCI kan bestand niet schrijven';
  AbFCIFileCloseErrorS = 'FCI fout tijdens sluiten van bestand';
  AbFCIFileSeekErrorS = 'FCI fout tijdens positioneren in bestand';
  AbFCIFileDeleteErrorS = 'FCI fout tijdens wissen van bestand';
  AbFCIAddFileErrorS = 'FCI kan bestand niet toevoegen';
  AbFCICreateErrorS = 'FCI kan context niet aanmaken';
  AbFCIFlushCabinetErrorS = 'FCI kan cabinet niet legen';
  AbFCIFlushFolderErrorS = 'FCI kan folder niet legen';
  AbFDICopyErrorS = 'FDI kann bestanden niet opsommen';
  AbFDICreateErrorS = 'FDI kan context niet aanmaken';
  AbInvalidCabTemplateS = 'Ongeldige sjabloon voor cabinetsbestand';
  AbInvalidCabFileS = 'Ongeldig bestand - geen cabinetsbestand';

  AbZipStored = 'Opgeslagen';
  AbZipShrunk = 'Gekrompen';
  AbZipReduced = 'Gereduceerd';
  AbZipImploded = 'Geïmplodeerd';
  AbZipTokenized = 'In symbolen gepakt';
  AbZipDeflated = 'Gedeflationeerd';
  AbZipDeflate64 = 'Uitgebreid gedeflationeerd';
  AbZipDCLImploded = 'DCL geïmplodeerd';
  AbZipBzip2 = 'Bzip2';
  AbZipLZMA = 'LZMA';
  AbZipIBMTerse = 'IBM Terse';
  AbZipLZ77 = 'IBM LZ77';
  AbZipJPEG = 'JPEG';
  AbZipWavPack = 'WavPack';
  AbZipPPMd = 'PPMd';
  AbZipUnknown = 'Onbekend (%d)';
  AbZipBestMethod = 'Beste methode';

  AbVersionFormatS = 'Versie %s';
  AbCompressedSizeFormatS = 'Gecomprimeerde grootte: %d';
  AbUncompressedSizeFormatS = 'Ongecomprimeerde grootte: %d';
  AbCompressionMethodFormatS = 'Compressiemethode: %s';
  AbCompressionRatioFormatS = 'Compressieverhouding: %2.0f%%';
  AbCRCFormatS = 'CRC: %x';
  AbReadOnlyS = 'r';
  AbHiddenS = 'h';
  AbSystemS = 's';
  AbArchivedS = 'a';
  AbEFAFormatS = 'Externe bestandsattributen: %s';
  AbIFAFormatS = 'Bestandstype: %s';
  AbTextS = 'Tekst';
  AbBinaryS = 'Binair';
  AbEncryptionFormatS = 'Versleuteling: %s';
  AbEncryptedS = 'Versleuteld';
  AbNotEncryptedS = 'Niet versleuteld';
  AbUnknownS = 'Onbekend';
  AbTimeStampFormatS = 'Tijdstempel: %s';
  AbMadeByFormatS = 'Gemaakt met versie: %f';
  AbNeededFormatS = 'Versie benodigd voor ontpakken: %f';
  AbCommentFormatS = 'Opmerking: %s';
  AbDefaultExtS = '*.zip';
  AbFilterS = 'PKZip Archieven (*.zip)|*.zip|Zelfontpakkende Archieven (*.exe)|*.exe|Alle Bestanden (*.*)|*.*';
  AbFileNameTitleS = 'Bestandsnaam Kiezen';

  AbOKS = 'OK';
  AbCancelS = 'Verlaten';
  AbSelectDirectoryS = 'Bestand kiezen';

  AbEnterPasswordS = 'Paswoord ingeven';
  AbPasswordS = '&Paswoord';
  AbVerifyS = '&Verificeren';

  AbCabExtS = '*.cab';
  AbCabFilterS = 'Cabinetsarchieven (*.cab)|*.CAB|Alle Bestanden (*.*)|*.*';
  AbLogExtS = '*.txt';
  AbLogFilterS = 'Tekstbestanden (*.txt)|*.TXT|Alle Bestanden (*.*)|*.*';
  AbExeExtS = '*.exe';
  AbExeFilterS = 'Zelfontpakkende Zip Bestanden (*.exe)|*.EXE|Alle Bestanden (*.*)|*.*';

  AbVMSReadTooManyBytesS = 'VMS: Anvraag om te veel byte [%d] te lezen';
  AbVMSInvalidOriginS = 'VMS: Ongeldige oorsprong %d, moet 0, 1 of 2 zijn';
  AbVMSErrorOpenSwapS = 'VMS: Kan wisselbestand %s niet openen';
  AbVMSSeekFailS = 'VMS: Kon niet in wisselbestand %s positioneren';
  AbVMSReadFailS = 'VMS: Kon %d byte in wisselbestand %s niet lezen';
  AbVMSWriteFailS = 'VMS: Kon %d byte niet in wisselbestand %s schrijven';
  AbVMSWriteTooManyBytesS = 'VMS: Anvraag om te veel byte [%d] te schrijven';

  AbBBSReadTooManyBytesS = 'BBS: Anvraag om te veel byte [%d] te lezen';
  AbBBSSeekOutsideBufferS = 'BBS: Nieuwe positie is buiten de buffer';
  AbBBSInvalidOriginS = 'BBS: Ongeldige oorsprongswaarde';
  AbBBSWriteTooManyBytesS = 'BBS: Anvraag om te veel byte [%d] te schrijven';

  AbSWSNotEndofStreamS = 'TabSlidingWindowStream.Write: Niet aan eind van stroom';
  AbSWSSeekFailedS = 'TabSlidingWindowStream.bsWriteChunk: Positioneren mislukt';
  AbSWSWriteFailedS = 'TabSlidingWindowStream.bsWriteChunk: Schrijven mislukt';
  AbSWSInvalidOriginS = 'TabSlidingWindowStream.Seek: Ongeldige oorsprong';
  AbSWSInvalidNewOriginS = 'TabSlidingWindowStream.Seek: Ongeldige nieuwe positie';

  AbItemNameHeadingS = 'Naam';
  AbPackedHeadingS = 'Gepakt';
  AbMethodHeadingS = 'Methode';
  AbRatioHeadingS = 'Besparing (%)';
  AbCRCHeadingS = 'CRC32';
  AbFileAttrHeadingS = 'Attribuut';
  AbFileFormatHeadingS = 'Formaat';
  AbEncryptionHeadingS = 'Versleuteld';
  AbTimeStampHeadingS = 'Tijdstempel';
  AbFileSizeHeadingS = 'Grootte';
  AbVersionMadeHeadingS = 'Gebruikte versie';
  AbVersionNeededHeadingS = 'Benodigde versie';
  AbPathHeadingS = 'Pad';
  AbPartialHeadingS = 'Partieel';
  AbExecutableHeadingS = 'Uitvoerbaar';

  AbCabMethod0S = 'Geen';
  AbCabMethod1S = 'MSZip';

  AbLtAddS = ' toegevoegd ';
  AbLtDeleteS = ' gewist ';
  AbLtExtractS = ' ontpakt ';
  AbLtFreshenS = ' geactualiseerd ';
  AbLtMoveS = ' verplaatst ';
  AbLtReplaceS = ' vervangen ';
  AbLtStartS = ' geprotocolleerd ';

  AbGzipInvalidS                   = 'Ongeldige Gzip';
  AbGzipBadCRCS                    = 'Ongeldige CRC';
  AbGzipBadFileSizeS               = 'Ongeldige bestandsgrootte';

  AbTarInvalidS                    = 'Ongeldige Tar';
  
  AbTarBadFileNameS                = 'Bestandsnaam te lang';
  
  AbTarBadLinkNameS                = 'Link naam te lang';
  
  AbTarBadOpS                      = 'Niet ondersteunde functie';

  

  AbUnhandledEntityS               = 'Niet behandelde entiteit';

  { pre-defined "operating system" (really more FILE system) identifiers for the
    Gzip header }
  AbGzOsFat         = 'FAT Bestandssysteem (MS-DOS, OS/2, NT/Win32)';
  AbGzOsAmiga       = 'Amiga';
  AbGzOsVMS         = 'VMS (oder OpenVMS)';
  AbGzOsUnix        = 'Unix';
  AbGzOsVM_CMS      = 'VM/CMS';
  AbGzOsAtari       = 'Atari TOS';
  AbGzOsHPFS        = 'HPFS Bestandssysteem (OS/2, NT)';
  AbGzOsMacintosh   = 'Macintosh';
  AbGzOsZ_System    = 'Z-System';
  AbGzOsCP_M        = 'CP/M';
  AbGzOsTOPS_20     = 'TOPS-20';
  AbGzOsNTFS        = 'NTFS Bestandssysteem (NT)';
  AbGzOsQDOS        = 'QDOS';
  AbGzOsAcornRISCOS = 'Acorn RISC OS';
  AbGzOsVFAT        = 'VFAT Bestandssysteem (Win95, NT)';
  AbGzOsMVS         = 'MVS';
  AbGzOsBeOS        = 'BeOS (BeBox of PowerMac)';
  AbGzOsTandem      = 'Tandem/NSK';
  AbGzOsTHEOS       = 'THEOS';
  AbGzOsunknown     = 'onbekend';
  AbGzOsUndefined   = 'ID is Gzip niet bekend';

{!!.03 - Moved from AbCompnd.inc }
{ Compound File specific error messages }
resourcestring
  AbCmpndIndexOutOfBounds   = 'Index niet in toegelaten bereik';
  AbCmpndBusyUpdating       = 'Samengesteld bestand wordt geactualiseerd';
  AbCmpndInvalidFile        = 'Ongeldig samengesteld bestand ';
  AbCmpndFileNotFound       = 'Bestand/directory niet gevonden';
  AbCmpndFolderNotEmpty     = 'Directory is niet leeg';
  AbCmpndExceedsMaxFileSize = 'Bestandsgrootte overschrijdt toegelaten maximum';
{!!.03 - End Moved }



implementation

end.
