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
{   Franco Perana: fixed memory allocation bug in TLZO.Decompress and optimized memory allocation  }
{                  in TLZO.Compress                                                                }
{                                                                                                  }
{**************************************************************************************************}

unit lzo;

interface

type
  TLZO = class
    class function Compress(Data: RawByteString): RawByteString;
    class function DeCompress(Data: RawByteString): RawByteString;
  end;

implementation

uses
  Windows,
  SysUtils;

{$I lzo.inc}

{ TLZO }

class function TLZO.Compress(Data: RawByteString): RawByteString;
var
  OutData: PByteArray;
  nI, nO: Integer;
begin
  nI := Length(Data);
  OutData := GetMemory(nI + (((nI div 1024) + 1) * 16)); // MAX 16B/1KB overhead in case of uncompressible data
  try
    nO := lzocompress(Data[1], OutData^, nI);
    SetLength(Result, nO);
    CopyMemory(@Result[1], OutData, nO);
  finally
    FreeMem(OutData);
  end;
end;

class function TLZO.DeCompress(Data: RawByteString): RawByteString;
var
  OutData: PByteArray;
  nO: Integer;
begin
  Assert(Length(Data) > 8);
  nO := PInteger(@Data[1])^;
  OutData := GetMemory(nO);
  try
    lzodecompress(Data[1], OutData^);
    SetLength(Result, nO);
    CopyMemory(@Result[1], OutData, nO);
  finally
    FreeMem(OutData);
  end;
end;

end.
