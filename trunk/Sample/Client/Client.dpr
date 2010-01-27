program Client;

uses
  Forms,
  ClientMainForm in 'ClientMainForm.pas' {Form5},
  ServiceProxy in 'ServiceProxy.pas',
  CipherFilters in '..\..\filters\Cipher\CipherFilters.pas',
  CompressFilters in '..\..\filters\Compress\CompressFilters.pas',
  HashFilters in '..\..\filters\Hash\HashFilters.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm5, Form5);
  Application.Run;
end.
