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

// JCL_DEBUG_EXPERT_INSERTJDBG OFF
program DSFCTest;

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  TestFiltersU in 'TestFiltersU.pas',
  HashFilters in '..\filters\Hash\HashFilters.pas',
  DSFCCommons in '..\filters\lib\DSFCCommons.pas',
  CipherFilters in '..\filters\Cipher\CipherFilters.pas',
  CompressFilters in '..\filters\Compress\CompressFilters.pas',
  lzo in '..\filters\lib\lzo.pas';

{$R *.RES}

begin

  Application.Initialize;
  if IsConsole then
    with TextTestRunner.RunRegisteredTests do
      Free
  else
  begin
    ReportMemoryLeaksOnShutdown := True;
    GUITestRunner.RunRegisteredTests;
  end;
end.

