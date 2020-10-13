{ #----------------------------------------------------------------------------
  Header translation for Microsoft XmlLite
  @author Carl M�nnig

  * XmlLite is a native C++ implementation of .NET XmlReader+Writer for stream-based, forward-only XML parsing and creation.
  * XmlLite.dll is required.  It is included with all new versions of Windows, and service packs for old versions.
  * XmlReader's pull-based interface is cleaner to use than SAX's event-based interface.
  * More info: http://msdn.microsoft.com/en-us/library/ms752838%28v=VS.85%29.aspx

  Note: This is a minimal translation, some parts were not implemented and most are untested.

  UPD: https://github.com/the-Arioch/Delphi-XmlLite
    Updated to Windows SDK 2016, fixed declarations errors, added error-messages, merged forks.
    Win64 compatibility should work but was not tested.
    Many functions and new interfaces should work but were not tested.
    Caveat emptor (well, this is FLOSS, so it is your software, not mine; use responsibly)
  ----------------------------------------------------------------------------- }
unit XmlLite;

interface

uses
  ActiveX,
  Windows,   // LONG_PTR type in Win32/Win64 with different Delphi versions
  Classes,
  SysUtils;

{$MINENUMSIZE 4}
{$ScopedEnums ON}
type
  XmlNodeType = (
    None = 0,
    Element = 1,
    Attribute = 2,
    Text = 3,
    CDATA = 4,
    ProcessingInstruction = 7,
    Comment = 8,
    DocumentType = 10,
    Whitespace = 13,
    EndElement = 15,
    XmlDeclaration = 17
    );

  XmlStandAlone = (
    Omit = 0,
    Yes = 1,
    No = 2
    );

  XmlWriterProperty = (
    MultiLanguage = 0,
    Indent = 1,
    ByteOrderMark = 2,
    OmitXmlDeclaration = 3,
    ConformanceLevel = 4,
    CompactEmptyElement = 5
    );

  XmlReaderProperty = (
    MultiLanguage = 0,
    ConformanceLevel = 1,
    RandomAccess = 2,
    XmlResolver = 3,
    DtdProcessing = 4,
    ReadState = 5,
    MaxElementDepth = 6,
    MaxEntityExpansion = 7
    );

  XmlReadState = (
    Initial	= 0,
    Interactive	= 1,
    Error	= 2,
    EndOfFile	= 3,
    Closed	= 4
    );

  XmlDtdProcessing = (
    Prohibit = 0,
    Parse = 1
    );

  XmlConformanceLevel = (
    Auto	= 0,
    Fragment	= 1,
    Document	= 2
    );

(**  Win32/Win64 properties compatibility, datatypes, enume sizes:

 https://msdn.microsoft.com/en-us/library/ms752842.aspx
 HRESULT GetProperty ([in] UINT nProperty, [out] LONG_PTR ** ppValue);

 The first  parameter in UINT is always 32 bits
 The second parameter in LONG_PTR (pointer-sized signed integer) is 32 or 64 bits

 Here it means - enums are 4-bytes SizeOf.
 Later it means in interface declarations xxxProperty can not have LongWord type
       for their 2nd parameters

 See datatypes declarations at
 https://msdn.microsoft.com/en-us/library/windows/desktop/aa383751.aspx

 Additionally - LPCWSTR stands for "const PWideChar" and
    WideString stands for BSTR ( OLE/COM "Basic String" )
 They are not the same, though kind of worked due to BSTR "implementation details".
 However that incurred redundant datatype casting UnicodeString -> WideString
    if nothing else. Could be a nice hack for pre-Unicode Delphi though.

 Differences example:
   a) PWideChar string can not contain #0 inside.
   b) PWideChar can tell states of "no string" aka nil aka NULL and
            one of empty aka 0-length string. But one can not pass
            nil instead of UnicodeString/WideString into, say, WriteElementString

                  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 The underscored superlong enum values like "XmlNodeType_ProcessingInstruction" are crazy.
       Better to use Scoped Enumerations
       They are supported starting with Delphi 2009 (missing in 2007 and prior)
       They are supported starting with FPC 2.6.0
       Would anyone really care about prior versions? Hardly so.
 http://docs.embarcadero.com/products/rad_studio/delphiAndcpp2009/HelpUpdate2/EN/html/devcommon/compdirsscopedenums_xml.html
 http://docs.embarcadero.com/products/rad_studio/radstudio2007/RS2007_helpupdates/HUpdate4/EN/html/devcommon/delphicompdirectivespart_xml.html
 http://wiki.freepascal.org/FPC_New_Features_2.6.0
 **)
{$MINENUMSIZE 1}

// Enumerations in Delphi are SIGNED integers thus the SDK error constants would trigger
//     W1012 Constant expression violates subrange bounds (Delphi)
//     for older Delphi/FPC try supporessing them with *temporary* {$WARNINGS OFF} instead.
{$WARN BOUNDS_ERROR OFF}
{$IFOPT R+}{$DEFINE RANGEON}{$R-}{$ELSE}{$UNDEF RANGEON}{$ENDIF}
  XmlError = (
    MX_E_MX                         =  $C00CEE00,
    MX_E_INPUTEND,                  // 0xC00CEE01 unexpected end of input
    MX_E_ENCODING,                  // 0xC00CEE02 unrecognized encoding
    MX_E_ENCODINGSWITCH,            // 0xC00CEE03 unable to switch the encoding
    MX_E_ENCODINGSIGNATURE,         // 0xC00CEE04 unrecognized input signature

    WC_E_WC                         =  $C00CEE20,
    WC_E_WHITESPACE,                // 0xC00CEE21 whitespace expected
    WC_E_SEMICOLON,                 // 0xC00CEE22 semicolon expected
    WC_E_GREATERTHAN,               // 0xC00CEE23 '>' expected
    WC_E_QUOTE,                     // 0xC00CEE24 quote expected
    WC_E_EQUAL,                     // 0xC00CEE25 equal expected
    WC_E_LESSTHAN,                  // 0xC00CEE26 wfc: no '<' in attribute value
    WC_E_HEXDIGIT,                  // 0xC00CEE27 hexadecimal digit expected
    WC_E_DIGIT,                     // 0xC00CEE28 decimal digit expected
    WC_E_LEFTBRACKET,               // 0xC00CEE29 '[' expected
    WC_E_LEFTPAREN,                 // 0xC00CEE2A '(' expected
    WC_E_XMLCHARACTER,              // 0xC00CEE2B illegal xml character
    WC_E_NAMECHARACTER,             // 0xC00CEE2C illegal name character
    WC_E_SYNTAX,                    // 0xC00CEE2D incorrect document syntax
    WC_E_CDSECT,                    // 0xC00CEE2E incorrect CDATA section syntax
    WC_E_COMMENT,                   // 0xC00CEE2F incorrect comment syntax
    WC_E_CONDSECT,                  // 0xC00CEE30 incorrect conditional section syntax
    WC_E_DECLATTLIST,               // 0xC00CEE31 incorrect ATTLIST declaration syntax
    WC_E_DECLDOCTYPE,               // 0xC00CEE32 incorrect DOCTYPE declaration syntax
    WC_E_DECLELEMENT,               // 0xC00CEE33 incorrect ELEMENT declaration syntax
    WC_E_DECLENTITY,                // 0xC00CEE34 incorrect ENTITY declaration syntax
    WC_E_DECLNOTATION,              // 0xC00CEE35 incorrect NOTATION declaration syntax
    WC_E_NDATA,                     // 0xC00CEE36 NDATA expected
    WC_E_PUBLIC,                    // 0xC00CEE37 PUBLIC expected
    WC_E_SYSTEM,                    // 0xC00CEE38 SYSTEM expected
    WC_E_NAME,                      // 0xC00CEE39 name expected
    WC_E_ROOTELEMENT,               // 0xC00CEE3A one root element
    WC_E_ELEMENTMATCH,              // 0xC00CEE3B wfc: element type match
    WC_E_UNIQUEATTRIBUTE,           // 0xC00CEE3C wfc: unique attribute spec
    WC_E_TEXTXMLDECL,               // 0xC00CEE3D text/xmldecl not at the beginning of input
    WC_E_LEADINGXML,                // 0xC00CEE3E leading "xml"
    WC_E_TEXTDECL,                  // 0xC00CEE3F incorrect text declaration syntax
    WC_E_XMLDECL,                   // 0xC00CEE40 incorrect xml declaration syntax
    WC_E_ENCNAME,                   // 0xC00CEE41 incorrect encoding name syntax
    WC_E_PUBLICID,                  // 0xC00CEE42 incorrect public identifier syntax
    WC_E_PESINTERNALSUBSET,         // 0xC00CEE43 wfc: pes in internal subset
    WC_E_PESBETWEENDECLS,           // 0xC00CEE44 wfc: pes between declarations
    WC_E_NORECURSION,               // 0xC00CEE45 wfc: no recursion
    WC_E_ENTITYCONTENT,             // 0xC00CEE46 entity content not well formed
    WC_E_UNDECLAREDENTITY,          // 0xC00CEE47 wfc: undeclared entity
    WC_E_PARSEDENTITY,              // 0xC00CEE48 wfc: parsed entity
    WC_E_NOEXTERNALENTITYREF,       // 0xC00CEE49 wfc: no external entity references
    WC_E_PI,                        // 0xC00CEE4A incorrect processing instruction syntax
    WC_E_SYSTEMID,                  // 0xC00CEE4B incorrect system identifier syntax
    WC_E_QUESTIONMARK,              // 0xC00CEE4C '?' expected
    WC_E_CDSECTEND,                 // 0xC00CEE4D no ']]>' in element content
    WC_E_MOREDATA,                  // 0xC00CEE4E not all chunks of value have been read
    WC_E_DTDPROHIBITED,             // 0xC00CEE4F DTD was found but is prohibited
    WC_E_INVALIDXMLSPACE,           // 0xC00CEE50 Invalid xml:space value

    NC_E_NC                         =  $C00CEE60,
    NC_E_QNAMECHARACTER,            // 0xC00CEE61 illegal qualified name character
    NC_E_QNAMECOLON,                // 0xC00CEE62 multiple colons in qualified name
    NC_E_NAMECOLON,                 // 0xC00CEE63 colon in name
    NC_E_DECLAREDPREFIX,            // 0xC00CEE64 declared prefix
    NC_E_UNDECLAREDPREFIX,          // 0xC00CEE65 undeclared prefix
    NC_E_EMPTYURI,                  // 0xC00CEE66 non default namespace with empty uri
    NC_E_XMLPREFIXRESERVED,         // 0xC00CEE67 "xml" prefix is reserved and must have the http://www.w3.org/XML/1998/namespace URI
    NC_E_XMLNSPREFIXRESERVED,       // 0xC00CEE68 "xmlns" prefix is reserved for use by XML
    NC_E_XMLURIRESERVED,            // 0xC00CEE69 xml namespace URI (http://www.w3.org/XML/1998/namespace) must be assigned only to prefix "xml"
    NC_E_XMLNSURIRESERVED,          // 0xC00CEE6A xmlns namespace URI (http://www.w3.org/2000/xmlns/) is reserved and must not be used

    SC_E_SC                         =  $C00CEE80,
    SC_E_MAXELEMENTDEPTH,           // 0xC00CEE81 max element depth was exceeded
    SC_E_MAXENTITYEXPANSION,        // 0xC00CEE82 max number of expanded entities was exceeded

    WR_E_WR                         =  $C00CEF00,
    WR_E_NONWHITESPACE,             // 0xC00CEF01 writer: specified string is not whitespace
    WR_E_NSPREFIXDECLARED,          // 0xC00CEF02 writer: namespace prefix is already declared with a different namespace
    WR_E_NSPREFIXWITHEMPTYNSURI,    // 0xC00CEF03 writer: cannot use prefix with empty namespace URI
    WR_E_DUPLICATEATTRIBUTE,        // 0xC00CEF04 writer: duplicate attribute
    WR_E_XMLNSPREFIXDECLARATION,    // 0xC00CEF05 writer: can not redefine the xmlns prefix
    WR_E_XMLPREFIXDECLARATION,      // 0xC00CEF06 writer: xml prefix must have the http://www.w3.org/XML/1998/namespace URI
    WR_E_XMLURIDECLARATION,         // 0xC00CEF07 writer: xml namespace URI (http://www.w3.org/XML/1998/namespace) must be assigned only to prefix "xml"
    WR_E_XMLNSURIDECLARATION,       // 0xC00CEF08 writer: xmlns namespace URI (http://www.w3.org/2000/xmlns/) is reserved and must not be used
    WR_E_NAMESPACEUNDECLARED,       // 0xC00CEF09 writer: namespace is not declared
    WR_E_INVALIDXMLSPACE,           // 0xC00CEF0A writer: invalid value of xml:space attribute (allowed values are "default" and "preserve")
    WR_E_INVALIDACTION,             // 0xC00CEF0B writer: performing the requested action would result in invalid XML document
    WR_E_INVALIDSURROGATEPAIR,      // 0xC00CEF0C writer: input contains invalid or incomplete surrogate pair

    XML_E_INVALID_DECIMAL           =  $C00CE01D,
    XML_E_INVALID_HEXIDECIMAL       =  $C00CE01E,
    XML_E_INVALID_UNICODE           =  $C00CE01F,
    XML_E_INVALIDENCODING           =  $C00CE06E
  );
{$IFDEF RANGEON}{$R+}{$UNDEF RANGEON}{$ENDIF}
{$WARN BOUNDS_ERROR DEFAULT}

type
  IXMLReader = interface
    ['{7279FC81-709D-4095-B63D-69FE4B0D9030}']
    function SetInput(const _IXMLStream: IUnknown): HRESULT; stdcall;
    function GetProperty(const nProperty: XmlReaderProperty; out ppValue: LONG_PTR): HRESULT; stdcall;
    function SetProperty(const nProperty: XmlReaderProperty; const pValue: LONG_PTR): HRESULT; stdcall;
    function Read(out XmlNodeType: XmlNodeType): HRESULT; stdcall;
    function GetNodeType(out XmlNodeType: XmlNodeType): HRESULT; stdcall;
    function MoveToFirstAttribute: HRESULT; stdcall;
    function MoveToNextAttribute: HRESULT; stdcall;
    function MoveToAttributeByName(const pwszLocalName, pwszNamespaceUri: PWideChar): HRESULT; stdcall;
    function MoveToElement: HRESULT; stdcall;
    function GetQualifiedName(out ppwszQualifiedName: PWideChar; out pcwchQualifiedName: LongWord): HRESULT; stdcall;
    function GetNamespaceUri(out ppwszNamespaceUri: PWideChar; out pcwchNamespaceUri: LongWord): HRESULT; stdcall;
    function GetLocalName(out ppwszLocalName: PWideChar; out pcwchLocalName: LongWord): HRESULT; stdcall;
    function GetPrefix(out ppwszPrefix: PWideChar; out pcwchPrefix: LongWord): HRESULT; stdcall;
    function GetValue(out ppwszValue: PWideChar; out pcwchValue: LongWord): HRESULT; stdcall;
    function ReadValueChunk( const Buffer: PWideChar; const BufferSizeInChars: Cardinal; out ResultLengthInChars: Cardinal )
      : HRESULT; stdcall;
    function GetBaseUri(out ppwszBaseUri: PWideChar; out pcwchBaseUri: LongWord): HRESULT; stdcall;
    function IsDefault: LongBool; stdcall;
    function IsEmptyElement: LongBool; stdcall;
    function GetLineNumber(out pnLineNumber: LongWord): HRESULT; stdcall;
    function GetLinePosition(out pnLinePosition: LongWord): HRESULT; stdcall;
    function GetAttributeCount(out pnAttributeCount: LongWord): HRESULT; stdcall;
    function GetDepth(out pnDepth: LongWord): HRESULT; stdcall;
    function IsEOF: LongBool; stdcall;
  end;

  IXmlReaderInput = Interface(IUnknown)
  end;

  IXMLWriter = interface
    ['{7279FC88-709D-4095-B63D-69FE4B0D9030}']
    function SetOutput(const _IXMLStream: IUnknown): HRESULT; stdcall;
    function GetProperty(const nProperty: XmlWriterProperty; out ppValue: Longint): HRESULT; stdcall;
    function SetProperty(const nProperty: XmlWriterProperty; const pValue: Longint): HRESULT; stdcall;
    function WriteAttributes(const pReader: IXMLReader; const fWriteDefaultAttributes: LongBool): HRESULT; stdcall;
    function WriteAttributeString(const pwszPrefix, pwszLocalName, pwszNamespaceUri, pwszValue: PWideChar): HRESULT; stdcall;
    function WriteCData(const pwszText: PWideChar): HRESULT; stdcall;
    function WriteCharEntity(const wch: WideChar): HRESULT; stdcall;
    function WriteChars( const Chars: PWideChar; const Count: Cardinal ): HRESULT; stdcall;
    function WriteComment(const pwszComment: PWideChar): HRESULT; stdcall;
    function WriteDocType(const pwszName, pwszPublicId, pwszSystemId, pwszSubset: PWideChar): HRESULT; stdcall;
    function WriteElementString(const pwszPrefix, pwszLocalName, pwszNamespaceUri, ContentOrNil: PWideChar): HRESULT; stdcall;
    function WriteEndDocument: HRESULT; stdcall;
    function WriteEndElement: HRESULT; stdcall;
    function WriteEntityRef(const pwszName: PWideChar): HRESULT; stdcall;
    function WriteFullEndElement: HRESULT; stdcall;
    function WriteName(const pwszName: PWideChar): HRESULT; stdcall;
    function WriteNmToken(const pwszNmToken: PWideChar): HRESULT; stdcall;
    function WriteNode(const pReader: IXMLReader; const fWriteDefaultAttributes: LongBool): HRESULT; stdcall;
    function WriteNodeShallow(const pReader: IXMLReader; const fWriteDefaultAttributes: LongBool): HRESULT; stdcall;
    function WriteProcessingInstruction(const pwszName, pwszText: PWideChar): HRESULT; stdcall;
    function WriteQualifiedName(const pwszLocalName, pwszNamespaceUri: PWideChar): HRESULT; stdcall;
    function WriteRaw(const pwszData: PWideChar): HRESULT; stdcall;
    function WriteRawChars( const RawChars: PWideChar; const Count: Cardinal ): HRESULT; stdcall;
    function WriteStartDocument(const standalone: XmlStandAlone): HRESULT; stdcall;
    function WriteStartElement(const pwszPrefix, pwszLocalName, pwszNamespaceUri: PWideChar): HRESULT; stdcall;
    function WriteString(const pwszText: PWideChar): HRESULT; stdcall;
    function WriteSurrogateCharEntity(const wchLow, wchHigh: WideChar): HRESULT; stdcall;
    function WriteWhitespace(const pwszWhitespace: PWideChar): HRESULT; stdcall;
    function Flush: HRESULT; stdcall;
  end;

  IXmlWriterOutput = interface(IUnknown)
  end;

(** MSDN: IXmlWriterLite

This class is a programming interface for writing XML quickly, introduced in Windows 10.
It implements an interface with most of the same methods as IXmlWriter, except for WriteQualifiedName.
Some method signatures are slightly different between IXmlWriter and IXmlWriterLite.
IXmlWriterLite is faster than IXmlWriter because it skips validation of namespaces and attributes, and
  does not maintain information that is required to automatically close tags.
Use IXmlWriterLite when you can maintain complete XML document correctness in your code, and
  output speed is of highest importance. Otherwise, we recommend that you use IXmlWriter. **)

    IXmlWriterLite = interface
      ['{862494C6-1310-4AAD-B3CD-2DBEEBF670D3}']
        function SetOutput(const _IXMLStream: IUnknown): HRESULT; stdcall;

        function GetProperty(const nProperty: XmlWriterProperty; out ppValue: LONG_PTR): HRESULT; stdcall;
        function SetProperty(const nProperty: XmlWriterProperty; const pValue: LONG_PTR): HRESULT; stdcall;

        function WriteAttributes(const pReader: IXMLReader; const fWriteDefaultAttributes: LongBool): HRESULT; stdcall;
        function WriteAttributeString(const QualifiedName: PWideChar; const QualNameLength: Cardinal;
                                      const Value: PWideChar; const ValueLength: Cardinal ): HRESULT; stdcall;

        function WriteCData(const pwszText: PWideChar): HRESULT; stdcall;
        function WriteCharEntity(const wch: WideChar):  HRESULT; stdcall;
        function WriteChars(const Chars: PWideChar; const Count: Cardinal): HRESULT; stdcall;

        function WriteComment(const pwszComment: PWideChar): HRESULT; stdcall;
        function WriteDocType(const pwszName, pwszPublicId, pwszSystemId, pwszSubset: PWideChar): HRESULT; stdcall;
        function WriteElementString(const QualifiedName: PWideChar; const QNameLengthInChars: Cardinal; const ContentOrNil: PWideChar): HRESULT; stdcall;
        function WriteEndDocument: HRESULT; stdcall;
        function WriteEndElement(const QualifiedName: PWideChar; const QNameLengthInChars: Cardinal): HRESULT; stdcall;
        function WriteEntityRef(const pwszName: PWideChar): HRESULT; stdcall;
        function WriteFullEndElement(const QualifiedName: PWideChar; const QNameLengthInChars: Cardinal): HRESULT; stdcall;
        function WriteName(const pwszName: PWideChar): HRESULT; stdcall;
        function WriteNmToken(const pwszNmToken: PWideChar): HRESULT; stdcall;
        function WriteNode(const pReader: IXMLReader; const fWriteDefaultAttributes: LongBool): HRESULT; stdcall;
        function WriteNodeShallow(const pReader: IXMLReader; const fWriteDefaultAttributes: LongBool): HRESULT; stdcall;
        function WriteProcessingInstruction(const pwszName, pwszText: PWideChar): HRESULT; stdcall;
        function WriteRaw(const pwszData: PWideChar): HRESULT; stdcall;
        function WriteRawChars( const RawChars: PWideChar; const Count: Cardinal ): HRESULT; stdcall;
        function WriteStartDocument(const standalone: XmlStandAlone): HRESULT; stdcall;
        function WriteStartElement(const QualifiedName: PWideChar; const QNameLengthInChars: Cardinal): HRESULT; stdcall;
        function WriteString(const pwszText: PWideChar): HRESULT; stdcall;
        function WriteSurrogateCharEntity(const wchLow, wchHigh: WideChar): HRESULT; stdcall;
        function WriteWhitespace(const pwszWhitespace: PWideChar): HRESULT; stdcall;
        function Flush: HRESULT; stdcall;
      end library {'Requires Windows 10'};

(**  MSDN  https://msdn.microsoft.com/en-us/library/ms752841.aspx

     IXmlResolver is an interface for resolving external entities in XML.
     An application can provide an implementation of this interface to allow a reader to resolve external entities.
     For information about implementing the interface, see the description of its member method, ResolveUri.

     See Also: XmlReaderProperty_XmlResolver and iXmlReader.SetProperty
**)

   IXmlResolver = interface
   ['{7279FC82-709D-4095-B63D-69FE4B0D9030}']

   // MSDN: To return a resolved entity, the implementation can return ISequentialStream, IStream, or IXmlReaderInput through the pResolvedInput parameter
   // By default, an IXmlReader has a resolver set to NULL. In this case, external entities are ignored�the reader simply replaces external entities
   //    with an empty string. However, if a resolver is set for a reader, the reader calls the ResolveUri method of that resolver for each external
   //    entity in the input.
   // Note that the reader only accepts the success code S_OK. If the reader receives any other success code, the reader fails with E_FAIL.
        function ResolveUri(
            Const pwszBaseUri, pwszPublicIdentifier, pwszSystemIdentifier: PWideChar;
            Out   ppResolvedInput: IUnknown): HRESULT; stdcall;
   end;


// Low-level, when shaving few msecs really might matter.
// Note: anything involving FileNames is Disk I/O bound, thus does not need those msecs.
function FastCreateXmlReader(): IXMLReader;
function FastCreateXmlWriter(): IXMLWriter;
{$WARN SYMBOL_library OFF}
function FastCreateXmlWriterLite(): IXmlWriterLite; {Windows 10 only; Delphi XE2 does not allow LIBRARY keyword here}
{$WARN SYMBOL_library Default}

type
  TAfterXmlLiteCreationHelper = TProc<iInterface, Pointer>; // re-define for pre-2010 Delphi versions

  iXmlLiteCreationHelper = interface
     function Data(const COMStream: iStream): IXmlLiteCreationHelper; overload;
     function Data(const FileName: TFileName): IXmlLiteCreationHelper; overload;
     function Data(const DataStream: TStream; const OwnStreamObject: boolean = True;
                   const Rewind: boolean = false): IXmlLiteCreationHelper; overload;
     function Data(const S: string): IXmlLiteCreationHelper; overload;

     function Encoding(const Encoding: TEncoding): IXmlLiteCreationHelper; overload;
     function Encoding(const EncodingCodePage: UINT): IXmlLiteCreationHelper; overload;
     function Encoding(const EncodingName: string): IXmlLiteCreationHelper; overload;

     function MemoryManager(const MM: iMalloc): IXmlLiteCreationHelper;
     function ExternalEntities(const EntitiesResolver: iXmlResolver): IXmlLiteCreationHelper;

     function Reader: IXMLReader;
     function Writer: IXMLWriter;
{$WARN SYMBOL_library OFF}
     function LiteWriter: IXmlWriterLite; library {Windows 10 only};
{$WARN SYMBOL_library DEFAULT}

     // one may register a handler that would check that the actual reader or writer was created (the parameter is not nil)
     function OnDestroy(const callback: TAfterXmlLiteCreationHelper; const UserData: Pointer = nil): IXmlLiteCreationHelper;
  end;

function CreateXmlLite: iXmlLiteCreationHelper; // Main entry point :-D

function CreateXmlFileReader(const FileName: string): IXMLReader; overload; deprecated 'Use CreateXmlLite() helper';
function CreateXmlFileReader(const FileName: string;
  const AEncodingCodePage: UINT): IXMLReader; overload;   deprecated 'Use CreateXmlLite() helper';
function CreateXmlFileReader(const FileName: string;
  const AEncodingName: string): IXMLReader; overload;     deprecated 'Use CreateXmlLite() helper';
function CreateXmlFileReader(const FileName: string;
  const Encoding: TEncoding): IXmlReader; overload;       deprecated 'Use CreateXmlLite() helper';

function CreateXmlFileWriter(const FileName: string): IXMLWriter; overload; deprecated 'Use CreateXmlLite() helper';
function CreateXmlFileWriter(const FileName: string;
  const AEncodingCodePage: UINT): IXMLWriter; overload; deprecated 'Use CreateXmlLite() helper';
function CreateXmlFileWriter(const FileName: string;
  const AEncodingName: string): IXMLWriter; overload;   deprecated 'Use CreateXmlLite() helper';
function CreateXmlFileWriter(const FileName: string;
  const Encoding: TEncoding): IXmlWriter; overload;     deprecated 'Use CreateXmlLite() helper';

// All these CreateXmlXXXXX functions are subject to combinatorial explosion
//    with regard to their semantic parameters and different datatypes for each.
// This can be resolved using Advanced Records with class operators Implicit
//    ( approach being christened Magnet Pattern in Scala community, if you
//      want to google some names, though the idea is self evident IMHO ).
// But this approach is problematic with both optional parameters and
//    quite limited Delphi type inference.
// So i prefered to unify them into a fluid-API helper.

// These functions might get helpful for any COM/OLE programming, not just XmlLite
// Though they are nowhere above trivial, so perhaps to be deprecated and removed too?
function OpenFileStreamReader(const FileName: string): IStream;
function OpenFileStreamWriter(const FileName: string): IStream;

procedure CheckHR(const HR: HRESULT); inline; deprecated 'Use EXmlLite.Check';

type
  rErrorDecription = record Code: XmlError; Message: String; end;
  TXMLLiteKnownErrors = array of rErrorDecription;

  EXmlLite = class(Exception)
  private
    FErrCode: Cardinal;
  public
    property ErrorCode: Cardinal read FErrCode; // 0 - unknown
    constructor CreateForErrCode(const FunctionResult: HRESULT);
  public
    Class Var XMLLiteErrorMessages: TXMLLiteKnownErrors; // may be localized by third-party units
    Class Constructor InitMessages;

    Class Function Check(const FunctionResult: HRESULT): HResult; inline;
    Class Function IsOK(const FunctionResult: HRESULT): boolean; inline;
  end;

implementation

const
  XMLReaderGuid:     TGUID = '{7279FC81-709D-4095-B63D-69FE4B0D9030}';
  XMLWriterGuid:     TGUID = '{7279FC88-709D-4095-B63D-69FE4B0D9030}';
  XMLWriterLiteGUID: TGUID = '{862494C6-1310-4AAD-B3CD-2DBEEBF670D3}' library {Windows 10};

// An idea to sleep with: do not load DLL and those functions until we really call them, if ever.
// Implemented starting with Delphi 2010 - http://www.tindex.net/Language/delayed.html

function CreateXmlReader(
  const refiid: TGUID;
  out _IXMLReader: IXMLReader; // actually iUnknown - future versions might have more reading interfaces
  const pMalloc: IMalloc): HRESULT; stdcall; external 'XmlLite.dll' {delayed};

function CreateXmlReaderInputWithEncodingCodePage(
  const pInputStream: IStream;
  const pMalloc: IMalloc;
  const nEncodingCodePage: Cardinal; // TEncoding.XXX.CodePage
  const fEncodingHint: LongBool;
  const pwszBaseUri: PWideChar;
  out ppInput: IXmlReaderInput): HRESULT; stdcall; external 'XmlLite.dll' {delayed};

function CreateXmlReaderInputWithEncodingName(
  const pInputStream: IStream;
  const pMalloc: IMalloc;
  const pwszEncodingName: PWideChar; // TEncoding.XXX.EncodingName
  const fEncodingHint: LongBool;
  const pwszBaseUri: PWideChar;
  out ppInput: IXmlReaderInput): HRESULT; stdcall; external 'XmlLite.dll' {delayed};

function CreateXmlWriter(
  const refiid: TGUID;
  out _IXMLWriter: IUnknown; // can be IXmlWriter or IXmlWriterLite or any future intf
  const pMalloc: IMalloc): HRESULT; stdcall; external 'XmlLite.dll' {delayed};

function CreateXmlWriterOutputWithEncodingCodePage(
  const pOutputStream: IStream;
  const pMalloc: IMalloc;
  const nEncodingCodePage: Cardinal; // TEncoding.XXX.CodePage
  out ppOutput: IXmlWriterOutput): HRESULT; stdcall; external 'XmlLite.dll' {delayed};

function CreateXmlWriterOutputWithEncodingName(
  const pOutputStream: IStream;
  const pMalloc: IMalloc;
  const pwszEncodingName: PWideChar; // TEncoding.XXX.EncodingName
  out ppOutput: IXmlWriterOutput): HRESULT; stdcall; external 'XmlLite.dll' {delayed};




function FastCreateXmlReader(): IXMLReader;
begin
  EXmlLite.Check(CreateXmlReader(XMLReaderGuid, Result, nil));
end;

function FastCreateXmlWriter(): IXMLWriter;
begin
  EXmlLite.Check(CreateXmlWriter(XMLWriterGuid, iUnknown(Result), nil));
end;

{$WARN SYMBOL_library OFF}
function FastCreateXmlWriterLite(): IXmlWriterLite;
begin
  EXmlLite.Check(CreateXmlWriter(XMLWriterLiteGUID, iUnknown(Result), nil));
end;
{$WARN SYMBOL_library Default}

function CreateXmlFileReader(const FileName: string): IXMLReader;
begin
  Result := FastCreateXmlReader;
  if (Result <> nil) and (FileName <> '') then
  begin
    EXmlLite.Check(Result.SetProperty(XmlReaderProperty.DtdProcessing, Ord(XmlDtdProcessing.Parse)));
    EXmlLite.Check(Result.SetInput(OpenFileStreamReader(FileName)));
  end;
end;

function CreateXmlFileReader(const FileName: string; const Encoding: TEncoding): IXmlReader;
begin
  Result := CreateXmlFileReader(FileName, Encoding.CodePage);
  // in Delphi 2009 ( if anyone would ever need ) TEncoding.CodePage was not available
  // see OmniXML's OEncoding.pas to see a class helper hack to pull it out
end;

function CreateXmlFileReader(const FileName: string;
  const AEncodingCodePage: UINT): IXMLReader;
var
  Stream: IStream;
  ReaderInput: IXMLReaderInput;
begin
  Assert(FileName <> '', 'Need XML File name');
  Result := FastCreateXmlReader;
  if Result <> nil then
  begin
    EXmlLite.Check(Result.SetProperty(XmlReaderProperty.DtdProcessing,
      Ord(XmlDtdProcessing.Parse)));
    Stream := OpenFileStreamReader(FileName);
    EXmlLite.Check(CreateXmlReaderInputWithEncodingCodePage(Stream, nil,
      AEncodingCodePage, True, nil, ReaderInput));
    EXmlLite.Check(Result.SetInput(ReaderInput));
  end;
end;

function CreateXmlFileReader(const FileName: string;
  const AEncodingName: string): IXMLReader;
var
  Stream: IStream;
  ReaderInput: IXMLReaderInput;
begin
  Assert(FileName <> '', 'Need XML File name');
  Result := FastCreateXmlReader;
  if Result <> nil then
  begin
    EXmlLite.Check(Result.SetProperty(XmlReaderProperty.DtdProcessing,
      Ord(XmlDtdProcessing.Parse)));
    Stream := OpenFileStreamReader(FileName);
    EXmlLite.Check(CreateXmlReaderInputWithEncodingName(Stream, nil,
      PWideChar(AEncodingName), True, nil, ReaderInput));
    EXmlLite.Check(Result.SetInput(ReaderInput));
  end;
end;


function CreateXmlFileWriter(const FileName: string): IXMLWriter;
begin
  Result := FastCreateXmlWriter;
  if (Result <> nil) and (FileName <> '') then
    EXmlLite.Check(Result.SetOutput(OpenFileStreamWriter(FileName)));
end;

function CreateXmlFileWriter(const FileName: string; const Encoding: TEncoding): IXmlWriter;
begin
  Result := CreateXmlFileWriter( FileName, Encoding.CodePage );
end;

function CreateXmlFileWriter(const FileName: string;
  const AEncodingCodePage: UINT): IXMLWriter;
var
  WriterOutput: IXMLWriterOutput;
  Stream: IStream;
begin
  Assert(FileName <> '', 'Need XML File name');
  Result := FastCreateXmlWriter;
  if (Result <> nil) then
  begin
    Stream := OpenFileStreamWriter(FileName);
    EXmlLite.Check(CreateXmlWriterOutputWithEncodingCodePage(Stream, nil,
      AEncodingCodePage, WriterOutput));
    Assert(WriterOutput <> nil);
    EXmlLite.Check(Result.SetOutput(WriterOutput));
  end;
end;

function CreateXmlFileWriter(const FileName: string;
  const AEncodingName: string): IXMLWriter; overload;
var
  WriterOutput: IXMLWriterOutput;
  Stream: IStream;
begin
  Assert(FileName <> '', 'Need XML File name');
  Result := FastCreateXmlWriter;
  if (Result <> nil) then
  begin
    Stream := OpenFileStreamWriter(FileName);
    EXmlLite.Check(CreateXmlWriterOutputWithEncodingName(Stream, nil,
      PWideChar(AEncodingName), WriterOutput));
    Assert(WriterOutput <> nil);
    EXmlLite.Check(Result.SetOutput(WriterOutput));
  end;
end;

function OpenFileStreamReader(const FileName: string): IStream;
begin
  Assert(FileExists(FileName), 'XML file should exist');
  Result := TStreamAdapter.Create(TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite), soOwned);
end;

function OpenFileStreamWriter(const FileName: string): IStream;
begin
  Result := TStreamAdapter.Create(TFileStream.Create(FileName, fmCreate), soOwned);
end;

type
  xlHelper = class( TInterfacedObject, iXmlLiteCreationHelper)
  private
     DestroyingCallBack: TAfterXmlLiteCreationHelper;
     DestroyingTag: Pointer;
     CreatedReaderWriter: iInterface;

     Resolver:    iXmlResolver;
     HeapManager: iMalloc;

   type
{$ScopedEnums OFF}
     xlEncType =  (etNone, etName, etCP);
     xlDataType = (dtNone, dtCOM, dtFileName, dtDelphiStream);
   var
     EncType:  xlEncType;
     DataType: xlDataType;
     DataDSOwn, DataDSRewind: boolean;
     DataDSObject:  TStream;
     DataCOMStream: iStream;
     DataFileName:  string;
     EncCharset: string;
     EncCodePage: Cardinal;
     procedure CleanFor(const Mode: xlDataType); overload;
     procedure CleanFor(const Mode: xlEncType); overload;
     function InternalCreateReaderStream(): iUnknown;
     function InternalCreateWriterStream(): iUnknown;
     procedure InternalCreateDataCOMStream(const ForWriting: boolean);
  protected
     function Data(const COMStream: iStream): IXmlLiteCreationHelper; overload;
     function Data(const FileName: TFileName): IXmlLiteCreationHelper; overload;
     function Data(const DataStream: TStream; const OwnStreamObject, Rewind: boolean): IXmlLiteCreationHelper; overload;
     function Data(const S: string): IXmlLiteCreationHelper; overload;

     function Encoding(const EncodingObj: TEncoding): IXmlLiteCreationHelper; overload;
     function Encoding(const EncodingCodePage: UINT): IXmlLiteCreationHelper; overload;
     function Encoding(const EncodingName: string): IXmlLiteCreationHelper; overload;

     function MemoryManager(const MM: iMalloc): IXmlLiteCreationHelper;
     function ExternalEntities(const EntitiesResolver: iXmlResolver): IXmlLiteCreationHelper;

     function Reader: IXMLReader;
     function Writer: IXMLWriter;
{$WARN SYMBOL_library Off}
     function LiteWriter: IXmlWriterLite; library {Windows 10 only};
{$WARN SYMBOL_library Default}

     // register a handler that would check that the actual reader or writer was created (the parameter is not nil)
     function OnDestroy(const callback: TAfterXmlLiteCreationHelper; const UserData: Pointer = nil): IXmlLiteCreationHelper;
  public
     destructor Destroy; override;
  end;

{$WARN SYMBOL_library OFF}
function CreateXmlLite: iXmlLiteCreationHelper;
begin
  Result := xlHelper.Create;
end;
{$WARN SYMBOL_library Default}

{ xlHelper }

function xlHelper.Data(const DataStream: TStream; const OwnStreamObject,
  Rewind: boolean): IXmlLiteCreationHelper;
var Mode: xlDataType;
begin
  if Assigned(DataStream)
     then Mode := dtDelphiStream
     else Mode := dtNone;
  CleanFor( Mode );
  if DataType = dtDelphiStream then
  begin
    if Self.DataDSOwn and (Self.DataDSObject <> DataStream) then
       FreeAndNil( Self.DataDSObject );

    Self.DataDSObject := DataStream;
    Self.DataDSRewind := Rewind;
    Self.DataDSOwn    := OwnStreamObject;
  end;

  Result := Self;
end;

function xlHelper.Data(const FileName: TFileName): IXmlLiteCreationHelper;
var Mode: xlDataType;
begin
  if FileName > ''
     then Mode := dtFileName
     else Mode := dtNone;
  CleanFor( Mode );
  if DataType = dtFileName then
     Self.DataFileName := FileName;

  Result := Self;
end;

function xlHelper.Data(const COMStream: iStream): IXmlLiteCreationHelper;
var Mode: xlDataType;
begin
  if Assigned(COMStream)
     then Mode := dtCOM
     else Mode := dtNone;
  CleanFor( Mode );
  if DataType = dtCOM then
     Self.DataCOMStream := COMStream;

  Result := Self;
end;

function xlHelper.Data(const S: string): IXmlLiteCreationHelper;
var
  Stream: TMemoryStream;
  //Bytes: TBytes;
begin
  Stream := TMemoryStream.Create;
  Stream.Write(S[1], Length(S)*2);
  Result := Data(Stream, True, True).Encoding(TEncoding.Unicode);
end;

function xlHelper.Encoding(const EncodingName: string): IXmlLiteCreationHelper;
var Mode: xlEncType;
begin
  if EncodingName > ''
     then Mode := etName
     else Mode := etNone;
  CleanFor( Mode );
  if EncType = etName then
     Self.EncCharset := EncodingName;

  Result := Self;
end;

function xlHelper.Encoding(
  const EncodingCodePage: UINT): IXmlLiteCreationHelper;
var Mode: xlEncType;
begin
  if EncodingCodePage > 0
     then Mode := etCP
     else Mode := etNone;
  CleanFor(Mode);
  if EncType = etCP then
     Self.EncCodePage := EncodingCodePage;

  Result := Self;
end;

function xlHelper.Encoding(const EncodingObj: TEncoding): IXmlLiteCreationHelper;
begin
  Result := Encoding( EncodingObj.CodePage );
  // if .CodePage is not yet published (example: Delphi 2009) see class helper hack in OmniXML's OEncodings
end;

// This can not be easily bound to FastMM4 / Delphi RTL MM because needing two extra methods:
//    is-this-pointer-allocated-in-this-MM and how-much-memory-block-was-REALLY-allocated-for-the-pointer
function xlHelper.MemoryManager(const MM: iMalloc): IXmlLiteCreationHelper;
begin
  Self.HeapManager := MM;

  Result := Self;
end;

function xlHelper.ExternalEntities(
  const EntitiesResolver: iXmlResolver): IXmlLiteCreationHelper;
begin
  Self.Resolver := EntitiesResolver;

  Result := Self;
end;

procedure xlHelper.InternalCreateDataCOMStream(const ForWriting: boolean);
var FileMode: Word;
    Ownership: TStreamOwnership;
begin
  if ForWriting
     then FileMode := fmCreate
     else FileMode := fmOpenRead or fmShareDenyWrite;

  if DataType > dtNone then
  begin
    if DataType = dtFileName then
    begin
      DataDSObject := TFileStream.Create(DataFileName, FileMode);
      DataDSOwn    := True;
      DataDSRewind := False;
      DataType     := dtDelphiStream;
    end;

    if DataType = dtDelphiStream then
    begin
      if DataDSOwn
         then Ownership := soOwned
         else Ownership := soReference;
      if DataDSRewind then
         DataDSObject.Seek(0,soFromBeginning);
      DataCOMStream := TStreamAdapter.Create(DataDSObject, Ownership);
      DataDSObject  := nil;
      DataType := dtCOM;
    end;
  end;

  if DataType <> dtCOM then
     DataCOMStream := nil;
end;

function xlHelper.InternalCreateReaderStream: iUnknown;
var
  ReaderInput: IXMLReaderInput;
begin
  Result := nil;

  InternalCreateDataCOMStream( False );

  if nil <> DataCOMStream then begin
    ReaderInput := nil;
    case EncType of
      etName: EXmlLite.Check(
                 CreateXmlReaderInputWithEncodingName( DataCOMStream, nil,
                      PWideChar(EncCharset), False, nil, ReaderInput));
      etCP: EXmlLite.Check(
               CreateXmlReaderInputWithEncodingCodePage( DataCOMStream, HeapManager,
                   EncCodePage, False, nil, ReaderInput));
      else;
    end;
    if nil <> ReaderInput
       then Result := ReaderInput
       else Result := DataCOMStream;
  end;
end;

function xlHelper.InternalCreateWriterStream(): iUnknown;
var
  WriterOutput: IXMLWriterOutput;
begin
  Result := nil;

  InternalCreateDataCOMStream( True );

  if nil <> DataCOMStream then begin
    WriterOutput := nil;
    case EncType of
      etName: EXmlLite.Check(
                 CreateXmlWriterOutputWithEncodingName(
                     DataCOMStream, HeapManager, PWideChar(EncCharset), WriterOutput));
      etCP:   EXmlLite.Check(
                 CreateXmlWriterOutputWithEncodingCodePage(
                     DataCOMStream, HeapManager, EncCodePage, WriterOutput));
      else;
    end;

    if nil <> WriterOutput
       then Result := WriterOutput
       else Result := DataCOMStream;
  end;
end;

function xlHelper.Reader: IXMLReader;
var
  ReaderInput: IUnknown;
begin
  EXmlLite.Check(CreateXmlReader(XMLReaderGuid, Result, HeapManager));
  if Result <> nil then
  begin
    EXmlLite.Check(Result.SetProperty(XmlReaderProperty.DtdProcessing,
      Ord(XmlDtdProcessing.Parse)));
    if Resolver <> nil then
      EXmlLite.Check(Result.SetProperty(XmlReaderProperty.XmlResolver,
        NativeInt(Pointer(Resolver)))); // did not tested!

    ReaderInput := InternalCreateReaderStream;

    if nil <> ReaderInput then
       EXmlLite.Check(Result.SetInput(ReaderInput));
  end;

  Self.CreatedReaderWriter := Result;
  CleanFor(dtNone);
  CleanFor(etNone);
end;

function xlHelper.Writer: IXMLWriter;
var
  WriterOutput: IUnknown;
begin
  EXmlLite.Check(CreateXmlWriter(XMLWriterGuid, IUnknown(Result), HeapManager));
  if (Result <> nil) then
  begin
    WriterOutput := InternalCreateWriterStream;

    if WriterOutput <> nil then
      EXmlLite.Check(Result.SetOutput(WriterOutput));
  end;

  Self.CreatedReaderWriter := Result;
  CleanFor(dtNone);
  CleanFor(etNone);
end;

{$WARN SYMBOL_library OFF}
function xlHelper.LiteWriter: IXmlWriterLite;
var
  WriterOutput: IUnknown;
begin
  EXmlLite.Check(CreateXmlWriter(XMLWriterLiteGUID, IUnknown(Result), HeapManager));
  if (Result <> nil) then
  begin
    WriterOutput := InternalCreateWriterStream;

    if WriterOutput <> nil then
      EXmlLite.Check(Result.SetOutput(WriterOutput));
  end;

  Self.CreatedReaderWriter := Result;
  CleanFor(dtNone);
  CleanFor(etNone);
end;
{$WARN SYMBOL_library Default}

function xlHelper.OnDestroy(const callback: TAfterXmlLiteCreationHelper;
  const UserData: Pointer): IXmlLiteCreationHelper;
begin
  DestroyingCallBack := callback;
  DestroyingTag := UserData;

  Result := Self;
end;

destructor xlHelper.Destroy;
begin
  if Assigned( DestroyingCallBack ) then
     DestroyingCallBack( CreatedReaderWriter, DestroyingTag);

  CleanFor(dtNone);
  CleanFor(etNone);

  inherited;
end;

procedure xlHelper.CleanFor(const Mode: xlEncType);
begin
  if Mode = EncType then exit;

  if Mode <> etName then
     EncCharset := '';
  if Mode <> etCP then
     EncCodePage := 0;

  EncType := Mode;
end;

procedure xlHelper.CleanFor(const Mode: xlDataType);
begin
  if Mode = DataType then exit;

  if Mode <> dtFileName then
     DataFileName := '';
  if Mode <> dtCOM then
     DataCOMStream := nil;
  if Mode <> dtDelphiStream then
  begin
     if DataDSOwn then
        DataDSObject.Free;
     DataDSObject := nil;
  end;

  DataType := Mode;
end;

{ EXmlLite }

constructor EXmlLite.CreateForErrCode(const FunctionResult: HRESULT);
var s: string; i: integer;
begin
  s := Format('Microsoft XmlLite Error: %d == 0x%x', [FunctionResult, FunctionResult]);

  // Now looking for matching registered human-readable description, if exists.
  // Could use somewhat faster binary search ( TArray<T>.BinarySearch helper), but...
  //   1) if the error messages would get localized, then no warranty they'd still be sorted
  //   2) more dependencies on later Delphi RTL, where speed is most probably no more a goal
  for i := Low(XMLLiteErrorMessages) to High(XMLLiteErrorMessages) do
    if HRESULT(XMLLiteErrorMessages[i].Code) = FunctionResult then begin
      s := s + #13#10+'Description: ' + XMLLiteErrorMessages[i].Message + '.';
      break;
    end;

  FErrCode := FunctionResult;
  Create( s );
end;

// Use example: repeat ... until until S_OK <> EXmlLite.Check( rd.MoveToNextAttribute() );
//              while S_OK = EXmlLite.Check( rd.Read(nodeType) ) do begin ... end;
class function EXmlLite.Check(const FunctionResult: HRESULT): HResult;
begin
  if (FunctionResult < 0) then
    raise CreateForErrCode( FunctionResult );
  Result := FunctionResult;
end;

// Use example: repeat ... until until not EXmlLite.IsOK( rd.MoveToNextAttribute() );
//              while EXmlLite.IsOK( rd.Read(nodeType) ) do begin ... end;
class function EXmlLite.IsOK(const FunctionResult: HRESULT): boolean;
begin
  Result := S_OK = Check(FunctionResult);
end;

procedure CheckHR(const HR: HRESULT);
begin
// inline legacy-compatility stub
  EXmlLite.Check( HR );
end;

class constructor EXmlLite.InitMessages;
  function r(const c: cardinal; m: string): rErrorDecription; inline;
  begin
    Result.Code := XmlError(c);
    Result.Message := m;
  end;
begin
  XMLLiteErrorMessages := TXMLLiteKnownErrors.Create(
    r($C00CEE01, 'unexpected end of input'),
    r($C00CEE02, 'unrecognized encoding'),
    r($C00CEE03, 'unable to switch the encoding'),
    r($C00CEE04, 'unrecognized input signature'),

    r($C00CEE21, 'whitespace expected'),
    r($C00CEE22, 'semicolon expected'),
    r($C00CEE23, '">" expected'),
    r($C00CEE24, 'quote expected'),
    r($C00CEE25, 'equal expected'),
    r($C00CEE26, 'wfc: no "<" in attribute value'),
    r($C00CEE27, 'hexadecimal digit expected'),
    r($C00CEE28, 'decimal digit expected'),
    r($C00CEE29, '"[" expected'),
    r($C00CEE2A, '"(" expected'),
    r($C00CEE2B, 'illegal xml character'),
    r($C00CEE2C, 'illegal name character'),
    r($C00CEE2D, 'incorrect document syntax'),
    r($C00CEE2E, 'incorrect CDATA section syntax'),
    r($C00CEE2F, 'incorrect comment syntax'),
    r($C00CEE30, 'incorrect conditional section syntax'),
    r($C00CEE31, 'incorrect ATTLIST declaration syntax'),
    r($C00CEE32, 'incorrect DOCTYPE declaration syntax'),
    r($C00CEE33, 'incorrect ELEMENT declaration syntax'),
    r($C00CEE34, 'incorrect ENTITY declaration syntax'),
    r($C00CEE35, 'incorrect NOTATION declaration syntax'),
    r($C00CEE36, 'NDATA expected'),
    r($C00CEE37, 'PUBLIC expected'),
    r($C00CEE38, 'SYSTEM expected'),
    r($C00CEE39, 'name expected'),
    r($C00CEE3A, 'one root element'),
    r($C00CEE3B, 'wfc: element type match'),
    r($C00CEE3C, 'wfc: unique attribute spec'),
    r($C00CEE3D, 'text/xmldecl not at the beginning of input'),
    r($C00CEE3E, 'leading "xml"'),
    r($C00CEE3F, 'incorrect text declaration syntax'),
    r($C00CEE40, 'incorrect xml declaration syntax'),
    r($C00CEE41, 'incorrect encoding name syntax'),
    r($C00CEE42, 'incorrect public identifier syntax'),
    r($C00CEE43, 'wfc: pes in internal subset'),
    r($C00CEE44, 'wfc: pes between declarations'),
    r($C00CEE45, 'wfc: no recursion'),
    r($C00CEE46, 'entity content not well formed'),
    r($C00CEE47, 'wfc: undeclared entity'),
    r($C00CEE48, 'wfc: parsed entity'),
    r($C00CEE49, 'wfc: no external entity references'),
    r($C00CEE4A, 'incorrect processing instruction syntax'),
    r($C00CEE4B, 'incorrect system identifier syntax'),
    r($C00CEE4C, '"?" expected'),
    r($C00CEE4D, 'no "]]>" in element content'),
    r($C00CEE4E, 'not all chunks of value have been read'),
    r($C00CEE4F, 'DTD was found but is prohibited'),
    r($C00CEE50, 'Invalid xml:space value'),

    r($C00CEE61, 'illegal qualified name character'),
    r($C00CEE62, 'multiple colons in qualified name'),
    r($C00CEE63, 'colon in name'),
    r($C00CEE64, 'declared prefix'),
    r($C00CEE65, 'undeclared prefix'),
    r($C00CEE66, 'non default namespace with empty uri'),
    r($C00CEE67, '"xml" prefix is reserved and must have the http://www.w3.org/XML/1998/namespace URI'),
    r($C00CEE68, '"xmlns" prefix is reserved for use by XML'),
    r($C00CEE69, 'xml namespace URI ( http://www.w3.org/XML/1998/namespace ) must be assigned only to prefix "xml"'),
    r($C00CEE6A, 'xmlns namespace URI ( http://www.w3.org/2000/xmlns/ ) is reserved and must not be used'),

    r($C00CEE81, 'max element depth was exceeded'),
    r($C00CEE82, 'max number of expanded entities was exceeded'),

    r($C00CEF01, 'writer: specified string is not whitespace'),
    r($C00CEF02, 'writer: namespace prefix is already declared with a different namespace'),
    r($C00CEF03, 'writer: cannot use prefix with empty namespace URI'),
    r($C00CEF04, 'writer: duplicate attribute'),
    r($C00CEF05, 'writer: can not redefine the xmlns prefix'),
    r($C00CEF06, 'writer: xml prefix must have the http://www.w3.org/XML/1998/namespace URI'),
    r($C00CEF07, 'writer: xml namespace URI ( http://www.w3.org/XML/1998/namespace ) must be assigned only to prefix "xml"'),
    r($C00CEF08, 'writer: xmlns namespace URI ( http://www.w3.org/2000/xmlns/ ) is reserved and must not be used'),
    r($C00CEF09, 'writer: namespace is not declared'),
    r($C00CEF0A, 'writer: invalid value of xml:space attribute (allowed values are "default" and "preserve")'),
    r($C00CEF0B, 'writer: performing the requested action would result in invalid XML document'),
    r($C00CEF0C, 'writer: input contains invalid or incomplete surrogate pair'),

    r($C00CE01D, 'XML: Invalid decimal digit'),
    r($C00CE01E, 'XML: Invalid hexadecimal digit'),
    r($C00CE01F, 'XML: Invalid Unicode characters'),
    r($C00CE06E, 'XML: Invalid charset encoding')
  );
end;

end.
