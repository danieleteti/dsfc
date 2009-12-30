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

unit CompressFilters;

interface

uses
  DBXTransport, SysUtils, DBXPlatform, DSFCCommons;

type
  TLZOFilter = class(TTransportFilter)
  public
    function ProcessInput(const Data: TBytes): TBytes; override;
    function ProcessOutput(const Data: TBytes): TBytes; override;
    function Id: UnicodeString; override;
  end;

implementation

uses
  Classes, lzo;

{ TLZOFilter }

function TLZOFilter.Id: UnicodeString;
begin
  Result := 'LZO';
end;

function TLZOFilter.ProcessInput(const Data: TBytes): TBytes;
begin
  Result := BytesOf(TLZO.Compress(StringOf(Data)));
end;

function TLZOFilter.ProcessOutput(const Data: TBytes): TBytes;
begin
  Result :=  BytesOf(TLZO.DeCompress(StringOf(Data)));
end;


initialization
  TTransportFilterFactory.RegisterFilter(TLZOFilter);

finalization
  TTransportFilterFactory.UnregisterFilter(TLZOFilter);

end.
