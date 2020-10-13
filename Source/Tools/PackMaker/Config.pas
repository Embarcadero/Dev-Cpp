unit Config;

interface

uses System.IniFiles;

type
  TConfig = class
    private
     FIniFile : TMemInifile;
     constructor Create;

    public
     function GetStyle : string;
     procedure SetStyle (StyleName : string);
    destructor Destroy; override;
  end;

var ConfigPackMaker : TConfig;

implementation

uses
  Vcl.Forms, System.IOUtils;

{ TConfig }

constructor TConfig.Create;
var MyInifile : string;
begin
  MyInifile := System.IOUtils.TPath.ChangeExtension(Application.ExeName, '.ini');

  FIniFile := TMemIniFile.Create(MyInifile);
end;

destructor TConfig.Destroy;
begin
  FIniFile.Free;
  inherited;
end;

function TConfig.GetStyle: string;
begin
  Result := FIniFile.ReadString('Config', 'Style', 'Windows10');
end;

procedure TConfig.SetStyle(StyleName: string);
begin
  FIniFile.WriteString('Config', 'Style', StyleName);
  FIniFile.UpdateFile;
end;

initialization
  ConfigPackMaker := TConfig.Create;

finalization
  ConfigPackMaker.Free;
end.
