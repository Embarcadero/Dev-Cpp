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
 * Pavel Koptev, Roman Kassebaum
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* Abbrevia: AbResString.pas                             *}
{*********************************************************}
{* Abbrevia: Resource strings, Russian localization      *}
{*********************************************************}
{* Warning: This file is UTF-8 encoded                   *}
{*********************************************************}
{* You need D2009 or higher to compile this unit         *}
{*********************************************************}
unit AbResString;

interface

resourcestring
  AbErrZipInvalidS = 'Формат архива не соответствует PKZip-формату';
  AbZipVersionNeededS = 'Действие невозможно. Файл запакован более новой версией программы';
  AbUnknownCompressionMethodS = 'Действие невозможно. Нераспознанный метод сжатия';
  AbNoExtractionMethodS = 'Действие невозможно. Не доступен метод распаковки архива';
  AbInvalidPasswordS = 'Действие невозможно. Неверный пароль';
  AbNoInsertionMethodS = 'Действие невозможно. Архивом не поддерживается добавление новых файлов';
  AbInvalidFactorS = 'Недействительный фактор сжатия';
  AbDuplicateNameS = 'Действие невозможно. Файл с таким именем в архиве уже присутствует';
  AbUnsupportedCompressionMethodS = 'Действие невозможно. Неподдерживаемый метод сжатия';
  AbUserAbortS = 'Действие отменено пользователем';
  AbArchiveBusyS = 'Действие невозможно. Архив поврежден';
  AbLastDiskRequestS = 'Вставьте последнюю дискету в дисковод';
  AbDiskRequestS = 'Вставьте дискету в дисковод';
  AbImageRequestS = 'Имя образа';
  AbBadSpanStreamS = 'Многотомные архивы открываются как файловый поток';
  AbDiskNumRequestS = 'Вставьте %d дискету в дисковод';
  AbImageNumRequestS = 'Укажите расположение %d тома архива';
  AbNoOverwriteSpanStreamS = 'Невозможно изменить существующий многотомный архив';
  AbNoSpannedSelfExtractS = 'Невозможно создать многотомный SFX-Архив';
  AbBlankDiskS = 'Вставьте чистую дискету в дисковод';
  AbStreamFullS = 'Ошибка записи в память';
  AbNoSuchDirectoryS = 'Папка не существует';
  AbInflateBlockErrorS = 'Блок данных не может быть распакован';
  AbBadStreamTypeS = 'Недействительный поток';
  AbTruncateErrorS = 'Ошибка при разделении Zip-Файла';
  AbZipBadCRCS = 'Не верная контрольная сумма';
  AbZipBadStubS = 'Корневой элемент архива должен быть исполняемым файлом';
  AbFileNotFoundS = 'Файл не найден';
  AbInvalidLFHS = 'Неверное начало файла';
  AbNoArchiveS = 'Архив не существует';
  AbReadErrorS = 'Ошибка чтения архива';
  AbInvalidIndexS = 'Неверный индекс елемента архива';
  AbInvalidThresholdS = 'Неверный размер частей архива';
  AbUnhandledFileTypeS = 'Неизвестный архив';
  AbSpanningNotSupportedS = 'Многотомность не поддерживается этим типом архивов';
  AbLogCreateErrorS = 'Ошибка при создании файла протокола';
  AbMoveFileErrorS = 'Ошибка при перемещении файла %s в %s';
  AbFileSizeTooBigS = 'Файл слишком велик для выбранного типа архива';

  AbNoCabinetDllErrorS = 'Библиотека cabinet.dll не может быть загружена';
  AbFCIFileOpenErrorS = 'FCI невозможно открыть файл';
  AbFCIFileReadErrorS = 'FCI невозможно прочитать файл';
  AbFCIFileWriteErrorS = 'FCI невозможно записать файл';
  AbFCIFileCloseErrorS = 'FCI ошибка при закрытии файла';
  AbFCIFileSeekErrorS = 'FCI ошибка при поиске в файле';
  AbFCIFileDeleteErrorS = 'FCI ошибка при удалении файла';
  AbFCIAddFileErrorS = 'FCI невозможно добавить файл';
  AbFCICreateErrorS = 'FCI ошибка создания';
  AbFCIFlushCabinetErrorS = 'FCI Cabinet-архив не может быть создан';
  AbFCIFlushFolderErrorS = 'FCI невозможно удалить все файлы из папки';
  AbFDICopyErrorS = 'FDI невозможно пересчитать файлы';
  AbFDICreateErrorS = 'FDI ошибка создания';
  AbInvalidCabTemplateS = 'Неверный шаблон Cabinet-файла';
  AbInvalidCabFileS = ' Неверный Cabinet-файл';

  AbZipStored = 'Сохранено';
  AbZipShrunk = 'Сжато';
  AbZipReduced = 'Сжато';
  AbZipImploded = 'Сжато';
  AbZipTokenized = 'Разделен на части';
  AbZipDeflated = 'Сжато';
  AbZipDeflate64 = 'Лучшее сжатие';
  AbZipDCLImploded = 'DCL Сжато';
  AbZipBzip2 = 'Bzip2';
  AbZipLZMA = 'LZMA';
  AbZipIBMTerse = 'IBM Terse';
  AbZipLZ77 = 'IBM LZ77';
  AbZipJPEG = 'JPEG';
  AbZipWavPack = 'WavPack';
  AbZipPPMd = 'PPMd';
  AbZipUnknown = 'Неизвестно (%d)';
  AbZipBestMethod = 'Лучший метод';

  AbVersionFormatS = 'Версия %s';
  AbCompressedSizeFormatS = 'Размер в архиве: %d';
  AbUncompressedSizeFormatS = 'Размер: %d';
  AbCompressionMethodFormatS = 'Метод сжатия: %s';
  AbCompressionRatioFormatS = 'Степень сжатия: %2.0f%%';
  AbCRCFormatS = 'CRC: %x';
  AbReadOnlyS = 'r';
  AbHiddenS = 'h';
  AbSystemS = 's';
  AbArchivedS = 'a';
  AbEFAFormatS = 'Внешние атрибуты файла: %s';
  AbIFAFormatS = 'Тип файла: %s';
  AbTextS = 'Текст';
  AbBinaryS = 'Двоичный';
  AbEncryptionFormatS = 'Шифрование: %s';
  AbEncryptedS = 'Зашифрован';
  AbNotEncryptedS = 'Не зашифрован';
  AbUnknownS = 'Неизвестно';
  AbTimeStampFormatS = 'Формат времени: %s';
  AbMadeByFormatS = 'Версия программы создания: %f';
  AbNeededFormatS = 'Для распаковки требуется версия: %f';
  AbCommentFormatS = 'Комментарии: %s';
  AbDefaultExtS = '*.zip';
  AbFilterS = 'PKZip-архив (*.zip)|*.zip|SFX-Архив (*.exe)|*.exe|Все файлы (*.*)|*.*';
  AbFileNameTitleS = 'Выберите имя файла';

  AbOKS = 'OK';
  AbCancelS = 'Отмена';
  AbSelectDirectoryS = 'Выберете папку';

  AbEnterPasswordS = 'Введите пароль';
  AbPasswordS = '&Пароль';
  AbVerifyS = '&Проверка';

  AbCabExtS = '*.cab';
  AbCabFilterS = 'Cabinet-архив (*.cab)|*.CAB|Все файлы (*.*)|*.*';
  AbLogExtS = '*.txt';
  AbLogFilterS = 'Текстовые файлы (*.txt)|*.TXT|Все файлы (*.*)|*.*';
  AbExeExtS = '*.exe';
  AbExeFilterS = 'SFX-архивы (*.exe)|*.EXE|Все файлы (*.*)|*.*';

  AbVMSReadTooManyBytesS = 'VMS: попытка чтения слишком большого числа байт [%d]';
  AbVMSInvalidOriginS = 'VMS: недействительный источник %d, разрешены 0, 1, 2';
  AbVMSErrorOpenSwapS = 'VMS: Невозможно открыть файл %s';
  AbVMSSeekFailS = 'VMS: Невозможно осуществить поиск в файле %s';
  AbVMSReadFailS = 'VMS: Невозможно прочитать файл %s';
  AbVMSWriteFailS = 'VMS: Невозможно %d байт записать в файл %s';
  AbVMSWriteTooManyBytesS = 'VMS: попытка записи слишком большого числа байт [%d]';

  AbBBSReadTooManyBytesS = 'BBS: попытка чтения слишком большого числа байт [%d]';
  AbBBSSeekOutsideBufferS = 'BBS: позиция находится вне буфера';
  AbBBSInvalidOriginS = 'BBS: недействительно предыдущее значение';
  AbBBSWriteTooManyBytesS = 'BBS: попытка записи слишком большого числа байт [%d]';

  AbSWSNotEndofStreamS = 'TabSlidingWindowStream.Write: попытка записи данных не в конец потока';
  AbSWSSeekFailedS = 'TabSlidingWindowStream.bsWriteChunk: поиск не удался';
  AbSWSWriteFailedS = 'TabSlidingWindowStream.bsWriteChunk: запись не удалась';
  AbSWSInvalidOriginS = 'TabSlidingWindowStream.Seek: Недействительный источник';
  AbSWSInvalidNewOriginS = 'TabSlidingWindowStream.Seek: недействительная новая позиция';

  AbItemNameHeadingS = 'Имя';
  AbPackedHeadingS = 'Сжато';
  AbMethodHeadingS = 'Метод';
  AbRatioHeadingS = 'Коэффициент сжатия (%)';
  AbCRCHeadingS = 'CRC32';
  AbFileAttrHeadingS = 'Атрибуты';
  AbFileFormatHeadingS = 'Формат';
  AbEncryptionHeadingS = 'Шифрование';
  AbTimeStampHeadingS = 'Время';
  AbFileSizeHeadingS = 'Размер';
  AbVersionMadeHeadingS = 'Использована версия';
  AbVersionNeededHeadingS = 'Необходима версия';
  AbPathHeadingS = 'Путь';
  AbPartialHeadingS = 'Частично';
  AbExecutableHeadingS = 'Выполнимо';

  AbCabMethod0S = 'нет';
  AbCabMethod1S = 'MSZip';

  AbLtAddS = ' вставлен ';
  AbLtDeleteS = ' удален ';
  AbLtExtractS = ' распакован ';
  AbLtFreshenS = ' обновлен ';
  AbLtMoveS = ' перемещен ';
  AbLtReplaceS = ' заменено ';
  AbLtStartS = ' запротоколировано ';

  AbGzipInvalidS                   = 'Недействительный Gzip';
  AbGzipBadCRCS                    = 'Недействительная контрольная сумма';
  AbGzipBadFileSizeS               = 'Недействительный размер файла';

  AbTarInvalidS                    = 'Недествительный Tar-архив'; 
  AbTarBadFileNameS                = 'Слишком длинное имя файла';
  AbTarBadLinkNameS                = 'Слишком длинная ссылка';
  AbTarBadOpS                      = 'Неподдерживаемая операция';
  
  AbUnhandledEntityS             = 'Необрабатываемый объект';

  { pre-defined "operating system" (really more FILE system) identifiers for the
    Gzip header }
  AbGzOsFat         = 'FAT файловая система (MS-DOS, OS/2, NT/Win32)';
  AbGzOsAmiga       = 'Amiga';
  AbGzOsVMS         = 'VMS (или OpenVMS)';
  AbGzOsUnix        = 'Unix';
  AbGzOsVM_CMS      = 'VM/CMS';
  AbGzOsAtari       = 'Atari TOS';
  AbGzOsHPFS        = 'HPFS файловая система (OS/2, NT)';
  AbGzOsMacintosh   = 'Macintosh';
  AbGzOsZ_System    = 'Z-System';
  AbGzOsCP_M        = 'CP/M';
  AbGzOsTOPS_20     = 'TOPS-20';
  AbGzOsNTFS        = 'NTFS файловая система (NT)';
  AbGzOsQDOS        = 'QDOS';
  AbGzOsAcornRISCOS = 'Acorn RISCOS';
  AbGzOsVFAT        = 'VFAT файловая система (Win95, NT)';
  AbGzOsMVS         = 'MVS';
  AbGzOsBeOS        = 'BeOS (BeBox или PowerMac)';
  AbGzOsTandem      = 'Tandem/NSK';
  AbGzOsTHEOS       = 'THEOS';
  AbGzOsunknown     = 'неизвестно';
  AbGzOsUndefined   = 'Идентификационный номер для Gzip не известен';

{!!.03 - Moved from AbCompnd.inc }
{ Compound File specific error messages }
resourcestring
  AbCmpndIndexOutOfBounds   = 'Индекс выходит за пределы допустимого диапазона';
  AbCmpndBusyUpdating       = 'Обновляется файл связок';
  AbCmpndInvalidFile        = 'Недействительный файл связок';
  AbCmpndFileNotFound       = 'Файл или папка не найдены';
  AbCmpndFolderNotEmpty     = 'Папка не пуста';
  AbCmpndExceedsMaxFileSize = 'Допустимый размер файла был превышен';
{!!.03 - End Moved }



implementation

end.
