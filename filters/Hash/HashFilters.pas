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

unit HashFilters;

interface

uses
  DBXTransport, SysUtils, DBXPlatform, DSFCCommons, DECHash, DECFmt;


type
  TAbstractHashFilter = class abstract(TTransportFilter)
  strict protected
    const
      SALT = '{7BE1B25F-FBEE-42F5-BDA2-30A5B3F42322}';
    var
      DECHash: TDECHash;
    function GetDECHashClass: TDECHashClass; virtual; abstract;
    function GetHashCharLength: Integer; virtual; abstract;
    function StrHash(const Value: RawByteString): RawByteString;
  public
    constructor Create; override;
    destructor Destroy; override;
    function ProcessInput(const Data: TBytes): TBytes; override;
    function ProcessOutput(const Data: TBytes): TBytes; override;
  end;

  TMD5Filter = class(TAbstractHashFilter)
  strict protected
    function GetDECHashClass: TDECHashClass; override;
    function GetHashCharLength: Integer; override;
  public
    function Id: UnicodeString; override;
  end;

  TMD4Filter = class(TAbstractHashFilter)
  strict protected
    function GetDECHashClass: TDECHashClass; override;
    function GetHashCharLength: Integer; override;
  public
    function Id: UnicodeString; override;
  end;

  TSHA1Filter = class(TAbstractHashFilter)
  strict protected
    function GetDECHashClass: TDECHashClass; override;
    function GetHashCharLength: Integer; override;
  public
    function Id: UnicodeString; override;
  end;

  TSHA512Filter = class(TAbstractHashFilter)
  strict protected
    function GetDECHashClass: TDECHashClass; override;
    function GetHashCharLength: Integer; override;
  public
    function Id: UnicodeString; override;
  end;

implementation

uses
  AnsiStrings;

{ TMD5Filter }

function TMD5Filter.GetDECHashClass: TDECHashClass;
begin
  Result := THash_MD5;
end;

function TMD5Filter.GetHashCharLength: Integer;
begin
  Result := 32;
end;

function TMD5Filter.Id: UnicodeString;
begin
  Result := 'MD5';
end;

constructor TAbstractHashFilter.Create;
begin
  inherited;
  DECHash := GetDECHashClass.Create;
end;

destructor TAbstractHashFilter.Destroy;
begin
  DECHash.Free;
  inherited;
end;

function TAbstractHashFilter.ProcessInput(const Data: TBytes): TBytes;
var
  s: String;
begin
  s := stringof(Data);
  Result := BytesOf(StrHash(SALT + s) + s);
end;

function TAbstractHashFilter.ProcessOutput(const Data: TBytes): TBytes;
var
  s: String;
  StrDigest: String;
begin
  s := StringOf(Data);
  StrDigest := Copy(s, 1, GetHashCharLength);
  s := Copy(s, GetHashCharLength + 1, Length(s));
  if AnsiCompareStr(StrHash(SALT + s), StrDigest) <> 0 then
    raise EDSFilterException.Create(ClassName + ' found an invalid packet with Hash: ' + DECHash.ClassName);
  Result := BytesOf(s);
end;

function TAbstractHashFilter.StrHash(const Value: RawByteString): RawByteString;
begin
  Result := DECHash.CalcBinary(Value,TFormat_HEXL);
end;

{ TMD4Filter }

function TMD4Filter.GetDECHashClass: TDECHashClass;
begin
  Result := THash_MD4;
end;

function TMD4Filter.GetHashCharLength: Integer;
begin
  Result := 32;
end;

function TMD4Filter.Id: UnicodeString;
begin
  Result := 'MD4';
end;

{ TSHA1Filter }

function TSHA1Filter.GetDECHashClass: TDECHashClass;
begin
  Result := THash_SHA1;
end;

function TSHA1Filter.GetHashCharLength: Integer;
begin
  Result := 40;
end;

function TSHA1Filter.Id: UnicodeString;
begin
  Result := 'SHA1';
end;

{ TSHA512Filter }

function TSHA512Filter.GetDECHashClass: TDECHashClass;
begin
  Result := THash_SHA512
end;

function TSHA512Filter.GetHashCharLength: Integer;
begin
  Result := 128;
end;

function TSHA512Filter.Id: UnicodeString;
begin
  Result := 'SHA512';
end;

initialization
  TTransportFilterFactory.RegisterFilter(TMD5Filter);
  TTransportFilterFactory.RegisterFilter(TMD4Filter);
  TTransportFilterFactory.RegisterFilter(TSHA1Filter);
  TTransportFilterFactory.RegisterFilter(TSHA512Filter);

finalization
  TTransportFilterFactory.UnregisterFilter(TMD5Filter);
  TTransportFilterFactory.UnregisterFilter(TMD4Filter);
  TTransportFilterFactory.UnregisterFilter(TSHA1Filter);
  TTransportFilterFactory.UnregisterFilter(TSHA512Filter);

end.
