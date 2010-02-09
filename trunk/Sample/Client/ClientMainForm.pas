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
    procedure FormCreate(Sender: TObject);
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

const
  testRepeat = 10;

var
  testData: array of String;
  x: TBytes;
  testDataSize: array [0..6] of integer = (16, 32, 64, 128, 256, 512, 1024);

procedure BuildTestData;
var
  i: integer;
begin
  SetLength(testData, Length(testDataSize));
  for I := 0 to Length(testDataSize) - 1 do
  begin
    testData[i] := 'MyTestData0123456789!@#$%^&*(),.';
    while Length(testData[i]) < testDataSize[i] * 1024 do
      testData[i] := testData[i] + testData[i];
  end;
end;


procedure TForm5.Button1Click(Sender: TObject);
var
  proxy: TSampleServiceClient;
  ticks : int64;
  i, j, pass: integer;
  echoStr: String;
begin
  TBlowfishFilter.CryptKey := Edit1.Text;
  T3TDESFilter.CryptKey := Edit1.Text;
  T3DESFilter.CryptKey := Edit1.Text;
  TRijndaelFilter.CryptKey := Edit1.Text;
  TPC1Filter.CryptKey := Edit1.Text;

  SQLConnection1.Open;
  proxy := TSampleServiceClient.Create(SQLConnection1.DBXConnection);
  try
    Memo1.Lines.Add('Filter: ' + proxy.FilterId);
    Memo1.Lines.Add('===============================');
    for I := 0 to Length(testData) - 1 do
    begin
      pass := 0;
      Memo1.Lines.Add('Test ' + IntToStr(I) + ': ' + IntToStr(Length(testData[i])));
      ticks := GetTickCount;
      for j := 0 to testRepeat - 1 do
      begin
        echoStr := proxy.Echo(testData[i]);
        if AnsiCompareStr(testData[i] + ' (from server)', echoStr) = 0 then
          Inc(pass);
      end;
      ticks := GetTickCount - ticks;
      if pass = testRepeat then
        Memo1.Lines.Add('  passed on ' + IntToStr(ticks) + ' ticks')
      else
        Memo1.Lines.Add('  failed ' + IntToStr(testRepeat - pass) + ' times ');
    end;
    Memo1.Lines.Add('===============================');
  finally
    proxy.Free;
  end;
  SQLConnection1.Close;
end;

procedure TForm5.FormCreate(Sender: TObject);
begin
  BuildTestData
end;

end.
