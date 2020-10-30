unit rsvg;

interface
uses rsvglib, Classes, SysUtils;

type
{$IFNDEF UNICODE}
  RawByteString = AnsiString;
{$ENDIF}

  IRSVGObject = interface
    ['{D51E4573-BF97-4F4F-BEFA-19D259696AB0}']
    function GetHandle: PRsvgHandle;
    function GetBaseUri: RawByteString;
    procedure SetBaseUri(const Uri: RawByteString);
    function GetDimensions: TRsvgDimensionData;
    function GetTitle: RawByteString;
    function GetDesc: RawByteString;
    function GetMetadata: RawByteString;

    property BaseUri: RawByteString read GetBaseUri write SetBaseUri;
    property Title: RawByteString read GetTitle;
    property Desc: RawByteString read GetDesc;
    property Metadata: RawByteString read GetMetadata;
    property Dimensions: TRsvgDimensionData read GetDimensions;
    property Handle: PRsvgHandle read GetHandle;

  end;

  TRSVGObject = class(TInterfacedObject, IRSVGObject)
  private
    FHandle: PRsvgHandle;
  protected
    function GetHandle: PRsvgHandle;
    function GetBaseUri: RawByteString;
    procedure SetBaseUri(const Uri: RawByteString);
    function GetDimensions: TRsvgDimensionData;
    function GetTitle: RawByteString;
    function GetDesc: RawByteString;
    function GetMetadata: RawByteString;
  public
    constructor Create; overload;
    constructor Create(const filename: string); overload;
    constructor Create(stream: TStream); overload;
    destructor Destroy; override;
  end;

implementation

{ TRSVGObject }

constructor TRSVGObject.Create;
begin
  FHandle := rsvg_handle_new;
end;

constructor TRSVGObject.Create(const filename: string);
var
  stream: TFileStream;
begin
  stream := TFileStream.Create(filename, fmOpenRead, fmShareDenyWrite);
  try
    Create(stream);
  finally
    stream.Free;
  end;
end;

constructor TRSVGObject.Create(stream: TStream);
var
  buffer: Pointer;
begin
  if stream is TCustomMemoryStream then
    FHandle := rsvg_handle_new_from_data(TCustomMemoryStream(stream).Memory, stream.Size, nil) else
    begin
      GetMem(buffer, stream.Size);
      try
        stream.Read(buffer^, stream.Size);
        FHandle := rsvg_handle_new_from_data(buffer, stream.Size, nil);
      finally
        FreeMem(buffer);
      end;
    end;
  Assert(FHandle <> nil, 'Invalid svg handle');
end;

destructor TRSVGObject.Destroy;
begin
  rsvg_handle_free(FHandle);
  inherited;
end;

function TRSVGObject.GetBaseUri: RawByteString;
begin
  Result := rsvg_handle_get_base_uri(FHandle)
end;

function TRSVGObject.GetDesc: RawByteString;
begin
  Result := rsvg_handle_get_desc(FHandle);
end;

function TRSVGObject.GetDimensions: TRsvgDimensionData;
begin
  rsvg_handle_get_dimensions(FHandle, @Result);
end;

function TRSVGObject.GetHandle: PRsvgHandle;
begin
  Result := FHandle;
end;

function TRSVGObject.GetMetadata: RawByteString;
begin
  Result := rsvg_handle_get_metadata(FHandle);
end;

function TRSVGObject.GetTitle: RawByteString;
begin
  Result := rsvg_handle_get_title(FHandle);
end;

procedure TRSVGObject.SetBaseUri(const Uri: RawByteString);
begin
  rsvg_handle_set_base_uri(FHandle, PAnsiChar(Uri));
end;

initialization
  rsvg_init;
finalization
  rsvg_term;
end.
