unit ClientMainForm;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  WideStrings,
  DbxDatasnap,
  DB,
  SqlExpr,
  ServiceProxy, StdCtrls, ExtCtrls;

type
  TForm5 = class(TForm)
    SQLConnection1: TSQLConnection;
    Button1: TButton;
    Memo1: TMemo;
    Edit1: TEdit;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form5: TForm5;

implementation

uses
  DBXPlatform,
  DBXTransport, CipherFilters;

{$R *.dfm}

procedure TForm5.Button1Click(Sender: TObject);
var
  proxy: TSampleServiceClient;
begin
  TBlowfishFilter.CryptKey := Edit1.Text;
  SQLConnection1.Open;
  proxy := TSampleServiceClient.Create(SQLConnection1.DBXConnection);
  try
    Memo1.Lines.Add(proxy.Echo('Hello World'));
  finally
    proxy.Free;
  end;
  SQLConnection1.Close;
end;

end.
