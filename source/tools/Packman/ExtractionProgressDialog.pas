unit ExtractionProgressDialog;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Variants, Classes, QGraphics, QControls, QForms,
  QDialogs, QComCtrls, QStdCtrls;
{$ENDIF}

type
  TExtractionProgress = class(TForm)
    Label1: TLabel;
    ProgressBar1: TProgressBar;
    Memo1: TMemo;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ExtractionProgress: TExtractionProgress;

implementation

{$R *.dfm}

end.
