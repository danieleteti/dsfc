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

program DSFCSpeedTest;
{$APPTYPE CONSOLE}

uses
  SysUtils,
  CompressFilters in '..\filters\Compress\CompressFilters.pas',
  CipherFilters in '..\filters\Cipher\CipherFilters.pas',
  HashFilters in '..\filters\Hash\HashFilters.pas',
  lzo in '..\filters\lib\lzo.pas',
  DSFCCommons in '..\filters\lib\DSFCCommons.pas',
  SpeedTestU in 'SpeedTestU.pas';

var
  st: TDSFCSpeedTest;

procedure Logo;
begin
  WriteLn('DataSnap Filters Compendium - SPEED TEST');
  WriteLn('Copyright 2009 - Daniele Teti - http://www.danieleteti.it');
  WriteLn('---------------------------------------------------------');

end;

begin
  Logo;
  try
    st := TDSFCSpeedTest.Create;
    try
      WriteLn('== HASH FILTERS == ', HOW_MANY_TIMES , ' iterations');
      st.TestHash;
      WriteLn;
      WriteLn('== CIPHER FILTERS == ', HOW_MANY_TIMES , ' iterations');
      st.TestCipher;
      WriteLn;
      WriteLn('== COMPRESS FILTERS == ', HOW_MANY_TIMES , ' iterations');
      st.TestCompress;
      WriteLn;
    finally
      st.Free;
    end;
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
    end;
  end;
  Write('Press return to continue...'); Readln;
end.
