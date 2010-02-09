unit ServerMainForm;

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
  DSCommonServer,
  DSTCPServerTransport,
  DSServer,
  StdCtrls,
  CheckLst,
  ExtCtrls,
  ActnList;

type
  TForm4 = class(TForm)
    DSServer1: TDSServer;
    DSServerClass1: TDSServerClass;
    DSTCP: TDSTCPServerTransport;
    Button1: TButton;
    RadioGroup1: TRadioGroup;
    Edit1: TEdit;
    Label1: TLabel;
    procedure DSServerClass1GetClass(DSServerClass: TDSServerClass;
      var PersistentClass: TPersistentClass);
    procedure FormCreate(Sender: TObject);
    procedure Action1Execute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;
{$METHODINFO ON}

  TSampleService = class(TPersistent)
  public
    function Echo(Value: String): String;
    function FilterId: String;
  end;

var
  Form4: TForm4;

implementation

uses
  DBXPlatform,
  DBXTransport,
  CipherFilters;
{$R *.dfm}
{ TSampleService }

function TSampleService.Echo(Value: String): String;
begin
  Result := Value + ' (from server)';
end;

procedure TForm4.Action1Execute(Sender: TObject);
begin
  TBlowfishFilter.CryptKey := Edit1.Text;
  T3TDESFilter.CryptKey := Edit1.Text;
  T3DESFilter.CryptKey := Edit1.Text;
  TRijndaelFilter.CryptKey := Edit1.Text;
  TPC1Filter.CryptKey := Edit1.Text;

  DSTCP.Stop;
  DSServer1.Stop;
  DSTCP.Filters.Clear;
  if RadioGroup1.ItemIndex > -1 then
  begin
    DSTCP.Filters.AddFilter(RadioGroup1.Items[RadioGroup1.ItemIndex]);
//    if DSTCP.Filters.GetFilter(0) is TAbstractCipherFilter then
//      TAbstractCipherFilter(DSTCP.Filters.GetFilter(0)).CryptKey := Edit1.Text;
  end;
  DSServer1.Start;
end;

procedure TForm4.DSServerClass1GetClass(DSServerClass: TDSServerClass;
  var PersistentClass: TPersistentClass);
begin
  PersistentClass := TSampleService;
end;

procedure TForm4.FormCreate(Sender: TObject);
var
  Filters: TDBXStringArray;
  filter: string;
begin
  RadioGroup1.Items.Clear;
  Filters := TTransportFilterFactory.Instance.RegisteredFiltersId;
  if Length(Filters) > 0 then
    for filter in Filters do
      RadioGroup1.Items.Add(filter);

  TBlowfishFilter.CryptKey := Edit1.Text;
  T3TDESFilter.CryptKey := Edit1.Text;
  T3DESFilter.CryptKey := Edit1.Text;
  TRijndaelFilter.CryptKey := Edit1.Text;
  TPC1Filter.CryptKey := Edit1.Text;
end;

function TSampleService.FilterId: String;
begin
  if Form4.RadioGroup1.ItemIndex >= 0 then
    Result := Form4.RadioGroup1.Items[Form4.RadioGroup1.ItemIndex]
  else
    Result := '(none)';
end;

end.
