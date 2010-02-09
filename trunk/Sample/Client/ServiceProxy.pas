// 
// Created by the DataSnap proxy generator.
// 

unit ServiceProxy;

interface

uses DBXCommon, DBXClient, DBXJSON, Classes, SysUtils, DB, SqlExpr, DBXDBReaders, DBXJSONReflect;

type
  TSampleServiceClient = class
  private
    FDBXConnection: TDBXConnection;
    FInstanceOwner: Boolean;
    FMarshal:   TJSONMarshal;
    FUnMarshal: TJSONUnMarshal;
    FEchoCommand: TDBXCommand;
    FFilterIdCommand: TDBXCommand;
  public
    constructor Create(ADBXConnection: TDBXConnection); overload;
    constructor Create(ADBXConnection: TDBXConnection; AInstanceOwner: Boolean); overload;
    destructor Destroy; override;
    function Echo(Value: string): string;
    function FilterId: string;
  end;

implementation

function TSampleServiceClient.Echo(Value: string): string;
begin
  if FEchoCommand = nil then
  begin
    FEchoCommand := FDBXConnection.CreateCommand;
    FEchoCommand.CommandType := TDBXCommandTypes.DSServerMethod;
    FEchoCommand.Text := 'TSampleService.Echo';
    FEchoCommand.Prepare;
  end;
  FEchoCommand.Parameters[0].Value.SetWideString(Value);
  FEchoCommand.ExecuteUpdate;
  Result := FEchoCommand.Parameters[1].Value.GetWideString;
end;

function TSampleServiceClient.FilterId: string;
begin
  if FFilterIdCommand = nil then
  begin
    FFilterIdCommand := FDBXConnection.CreateCommand;
    FFilterIdCommand.CommandType := TDBXCommandTypes.DSServerMethod;
    FFilterIdCommand.Text := 'TSampleService.FilterId';
    FFilterIdCommand.Prepare;
  end;
  FFilterIdCommand.ExecuteUpdate;
  Result := FFilterIdCommand.Parameters[0].Value.GetWideString;
end;


constructor TSampleServiceClient.Create(ADBXConnection: TDBXConnection);
begin
  inherited Create;
  if ADBXConnection = nil then
    raise EInvalidOperation.Create('Connection cannot be nil.  Make sure the connection has been opened.');
  FDBXConnection := ADBXConnection;
  FInstanceOwner := True;
end;


constructor TSampleServiceClient.Create(ADBXConnection: TDBXConnection; AInstanceOwner: Boolean);
begin
  inherited Create;
  if ADBXConnection = nil then
    raise EInvalidOperation.Create('Connection cannot be nil.  Make sure the connection has been opened.');
  FDBXConnection := ADBXConnection;
  FInstanceOwner := AInstanceOwner;
end;


destructor TSampleServiceClient.Destroy;
begin
  FreeAndNil(FEchoCommand);
  FreeAndNil(FFilterIdCommand);
  inherited;
end;

end.
