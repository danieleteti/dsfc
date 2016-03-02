// JCL_DEBUG_EXPERT_INSERTJDBG OFF
program Server;

uses
  Forms,
  ServerMainForm in 'ServerMainForm.pas' {Form4},
  CipherFilters in '..\..\filters\Cipher\CipherFilters.pas',
  CompressFilters in '..\..\filters\Compress\CompressFilters.pas',
  HashFilters in '..\..\filters\Hash\HashFilters.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
