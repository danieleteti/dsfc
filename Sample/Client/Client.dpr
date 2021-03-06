// JCL_DEBUG_EXPERT_INSERTJDBG OFF
program Client;

uses
  Forms,
  ClientMainForm in 'ClientMainForm.pas' {Form5},
  CipherFilters in '..\..\filters\Cipher\CipherFilters.pas',
  CompressFilters in '..\..\filters\Compress\CompressFilters.pas',
  HashFilters in '..\..\filters\Hash\HashFilters.pas',
  ServiceProxy in 'ServiceProxy.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm5, Form5);
  Application.Run;
end.
