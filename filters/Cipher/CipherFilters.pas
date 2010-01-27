{ ************************************************************************************************** }
{ }
{ DataSnap Filters Compendium }
{ }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the }
{ License at http://www.mozilla.org/MPL/ }
{ }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF }
{ ANY KIND, either express or implied. See the License for the specific language governing rights }
{ and limitations under the License. }
{ }
{ }
{ The Initial Developer of the Original Code is Daniele Teti <http://www.danieleteti.it> }
{ Portions created by Daniele Teti are Copyright (C) 2009 Daniele Teti }
{ All Rights Reserved. }
{ }
{ Contributor(s): }
{ }
{ ************************************************************************************************** }

unit CipherFilters;

interface

uses
  DBXTransport,
  SysUtils,
  DBXPlatform,
  DSFCCommons,
  DECCipher,
  DECFmt,
  Types;

type
  TAbstractCipherFilter = class abstract(TTransportFilter)
  protected
  var
    DECCipher: TDECCipher;
//    FCryptKey: RawByteString;
    function GetDECCipherClass: TDECCipherClass; virtual; abstract;
    function StrEncode(const Value: RawByteString): RawByteString;
    function StrDecode(const Value: RawByteString): String;
    function GetCryptKey: RawByteString; virtual; abstract;
  public
    constructor Create; override;
    destructor Destroy; override;
    function ProcessInput(const Data: TBytes): TBytes; override;
    function ProcessOutput(const Data: TBytes): TBytes; override;
    function SetParameterValue(const ParamName: UnicodeString;
      const ParamValue: UnicodeString): Boolean; override;
    function GetParameters: TDBXStringArray; override;
    function GetUserParameters: TDBXStringArray; override;
  end;

  TBlowfishFilter = class(TAbstractCipherFilter)
  strict protected
    function GetDECCipherClass: TDECCipherClass; override;
    function GetCryptKey: RawByteString; override;
  public
    function Id: UnicodeString; override;
    class var CryptKey: RawByteString;
  end;

  TRijndaelFilter = class(TAbstractCipherFilter)
  strict protected
    function GetDECCipherClass: TDECCipherClass; override;
    function GetCryptKey: RawByteString; override;
  public
    function Id: UnicodeString; override;
    class var CryptKey: RawByteString;
  end;

  { Triple DES 24 byte Blocksize, 24 byte Keysize 168 bits relevant }
  T3TDESFilter = class(TAbstractCipherFilter)
  strict protected
    function GetDECCipherClass: TDECCipherClass; override;
    function GetCryptKey: RawByteString; override;
  public
    function Id: UnicodeString; override;
    class var CryptKey: RawByteString;
  end;

  { Triple DES  8 byte Blocksize, 24 byte Keysize 168 bits relevant }
  T3DESFilter = class(TAbstractCipherFilter)
  strict protected
    function GetDECCipherClass: TDECCipherClass; override;
    function GetCryptKey: RawByteString; override;
  public
    function Id: UnicodeString; override;
    class var CryptKey: RawByteString;
  end;

implementation

uses
  AnsiStrings;

{ TBlowfishFilter }

function TBlowfishFilter.GetCryptKey: RawByteString;
begin
  Result := Self.CryptKey;
end;

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
  DECCipher := GetDECCipherClass.Create;
  DECCipher.Done;
  DECCipher.Init(GetCryptKey);
end;

destructor TAbstractCipherFilter.Destroy;
begin
  DECCipher.Free;
  inherited;
end;

function TAbstractCipherFilter.GetParameters: TDBXStringArray;
begin
  SetLength(Result, 0);
end;

function TAbstractCipherFilter.GetUserParameters: TDBXStringArray;
begin
  SetLength(Result, 0);
end;

function TAbstractCipherFilter.ProcessInput(const Data: TBytes): TBytes;
begin
  DECCipher.Done;
  Result := BytesOf(StrEncode(stringof(Data)));
end;

function TAbstractCipherFilter.ProcessOutput(const Data: TBytes): TBytes;
begin
  DECCipher.Done;
  Result := BytesOf(StrDecode(stringof(Data)));
end;

function TAbstractCipherFilter.SetParameterValue(const ParamName,
  ParamValue: UnicodeString): Boolean;
begin
  Result := False;
end;

function TAbstractCipherFilter.StrDecode(const Value: RawByteString): String;
begin
  Result := DECCipher.DecodeBinary(Value, TFormat_HEXL);
end;

function TAbstractCipherFilter.StrEncode(const Value: RawByteString)
  : RawByteString;
begin
  Result := DECCipher.EncodeBinary(Value, TFormat_HEXL);
end;

{ TRijndaelFilter }

function TRijndaelFilter.GetCryptKey: RawByteString;
begin
  Result := Self.CryptKey;
end;

function TRijndaelFilter.GetDECCipherClass: TDECCipherClass;
begin
  Result := TCipher_Rijndael;
end;

function TRijndaelFilter.Id: UnicodeString;
begin
  Result := 'Rijndael';
end;

{ T3TDESFilter }

function T3TDESFilter.GetCryptKey: RawByteString;
begin
  Result := Self.CryptKey;
end;

function T3TDESFilter.GetDECCipherClass: TDECCipherClass;
begin
  Result := TCipher_3TDES;
end;

function T3TDESFilter.Id: UnicodeString;
begin
  Result := '3TDES';
end;

{ T3DESFilter }

function T3DESFilter.GetCryptKey: RawByteString;
begin
  Result := Self.CryptKey;
end;

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
