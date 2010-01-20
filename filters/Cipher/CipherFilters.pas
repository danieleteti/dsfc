{**************************************************************************************************}
{                                                                                                  }
{ DataSnap Filters Compendium                                                                      }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{                                                                                                  }
{ The Initial Developer of the Original Code is Daniele Teti <http://www.danieleteti.it>           }
{ Portions created by Daniele Teti are Copyright (C) 2009 Daniele Teti                             }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{                                                                                                  }
{**************************************************************************************************}

unit CipherFilters;

interface

uses
  DBXTransport, SysUtils, DBXPlatform, DSFCCommons, DECCipher, DECFmt, Types;


type
  TAbstractCipherFilter = class abstract(TTransportFilter)
  strict protected
    var
      DECCipher: TDECCipher;
      FCryptKey: RawByteString;
    function GetDECCipherClass: TDECCipherClass; virtual; abstract;
    function StrEncode(const Value: RawByteString): RawByteString;
    function StrDecode(const Value: RawByteString): String;
    function GetCryptKey: RawByteString;
  public

    constructor Create; override;
    destructor Destroy; override;
    function ProcessInput(const Data: TBytes): TBytes; override;
    function ProcessOutput(const Data: TBytes): TBytes; override;
    function GetParameterValue(const ParamName: UnicodeString): UnicodeString; override;
    function SetParameterValue(const ParamName: UnicodeString; const ParamValue: UnicodeString): Boolean; override;
    function GetParameters: TDBXStringArray; override;
    function GetUserParameters: TDBXStringArray; override;
  end;

  TBlowfishFilter = class(TAbstractCipherFilter)
  strict protected
    function GetDECCipherClass: TDECCipherClass; override;
  public
    function Id: UnicodeString; override;
  end;

  TRijndaelFilter = class(TAbstractCipherFilter)
  strict protected
    function GetDECCipherClass: TDECCipherClass; override;
  public
    function Id: UnicodeString; override;
  end;

  {Triple DES 24 byte Blocksize, 24 byte Keysize 168 bits relevant}
  T3TDESFilter = class(TAbstractCipherFilter)
  strict protected
    function GetDECCipherClass: TDECCipherClass; override;
  public
    function Id: UnicodeString; override;
  end;

  {Triple DES  8 byte Blocksize, 24 byte Keysize 168 bits relevant}
  T3DESFilter = class(TAbstractCipherFilter)
  strict protected
    function GetDECCipherClass: TDECCipherClass; override;
  public
    function Id: UnicodeString; override;
  end;


const
  DEFAULT_CRYPT_KEY = 'default_crypt_key';
  CRYPT_KEY_PROPERTY_NAME = 'CRYPT KEY';

implementation

uses
  AnsiStrings;

{ TBlowfishFilter }

function TBlowfishFilter.GetDECCipherClass: TDECCipherClass;
begin
  Result := TCipher_Blowfish;
end;

function TBlowfishFilter.Id: UnicodeString;
begin
  Result := 'Blowfish';
end;

constructor TAbstractCipherFilter.Create;
begin
  inherited;
  FCryptKey := DEFAULT_CRYPT_KEY;
  DECCipher := GetDECCipherClass.Create;
  DECCipher.Init(GetCryptKey);
end;

destructor TAbstractCipherFilter.Destroy;
begin
  DECCipher.Free;
  inherited;
end;

function TAbstractCipherFilter.GetCryptKey: RawByteString;
begin
  Result := FCryptKey;
end;

function TAbstractCipherFilter.GetParameters: TDBXStringArray;
begin
  SetLength(Result,1);
//  Result[0] := GetCryptKey;
  Result[0] := CRYPT_KEY_PROPERTY_NAME;
end;

function TAbstractCipherFilter.GetParameterValue(
  const ParamName: UnicodeString): UnicodeString;
begin
  Result := '';
  if ParamName = CRYPT_KEY_PROPERTY_NAME then
    Result := FCryptKey;
end;

function TAbstractCipherFilter.GetUserParameters: TDBXStringArray;
begin
  SetLength(Result, 1);
  Result[0] := CRYPT_KEY_PROPERTY_NAME;
end;

function TAbstractCipherFilter.ProcessInput(const Data: TBytes): TBytes;
begin
  DECCipher.Done;
  Result := BytesOf(StrEncode(stringof(Data)));
end;

function TAbstractCipherFilter.ProcessOutput(const Data: TBytes): TBytes;
begin
  DECCipher.Done;
  Result := BytesOf(StrDecode(StringOf(Data)));
end;

function TAbstractCipherFilter.SetParameterValue(const ParamName,
  ParamValue: UnicodeString): Boolean;
begin
  Result := False;
  if ParamName = CRYPT_KEY_PROPERTY_NAME then
  begin
    FCryptKey := ParamValue;
    DECCipher.Done;
    DECCipher.Init(GetCryptKey);
    Result := true;
  end;
end;

function TAbstractCipherFilter.StrDecode(
  const Value: RawByteString): String;
begin
  Result := DECCipher.DecodeBinary(Value, TFormat_HEXL);
end;

function TAbstractCipherFilter.StrEncode(const Value: RawByteString): RawByteString;
begin
  Result := DECCipher.EncodeBinary(Value,TFormat_HEXL);
end;


{ TRijndaelFilter }

function TRijndaelFilter.GetDECCipherClass: TDECCipherClass;
begin
  Result := TCipher_Rijndael;
end;

function TRijndaelFilter.Id: UnicodeString;
begin
  Result := 'Rijndael';
end;

{ T3TDESFilter }

function T3TDESFilter.GetDECCipherClass: TDECCipherClass;
begin
  Result := TCipher_3TDES;
end;

function T3TDESFilter.Id: UnicodeString;
begin
  Result := '3TDES';
end;

{ T3DESFilter }

function T3DESFilter.GetDECCipherClass: TDECCipherClass;
begin
  Result := TCipher_3DES;
end;

function T3DESFilter.Id: UnicodeString;
begin
  Result := '3DES';
end;

initialization
  TTransportFilterFactory.RegisterFilter(TBlowfishFilter);
  TTransportFilterFactory.RegisterFilter(TRijndaelFilter);
  TTransportFilterFactory.RegisterFilter(T3TDESFilter);
  TTransportFilterFactory.RegisterFilter(T3DESFilter);

finalization
  TTransportFilterFactory.UnregisterFilter(TBlowfishFilter);
  TTransportFilterFactory.UnregisterFilter(TRijndaelFilter);
  TTransportFilterFactory.UnregisterFilter(T3TDESFilter);
  TTransportFilterFactory.UnregisterFilter(T3DESFilter);

end.
