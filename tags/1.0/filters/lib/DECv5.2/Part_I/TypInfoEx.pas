{*****************************************************************************

  Delphi Encryption Compendium (DEC Part I)
  Version 5.2, Part I, for Delphi 7 - 2009

  Remarks:          Freeware, Copyright must be included

  Original Authors: Heiko Behrens (Initiator and Developer), Hagen Reddmann
                    (c) 2006 Heiko Behrens and
                    Hagen Reddmann, HaReddmann [at] T-Online [dot] de
  Modifications:    (c) 2008 Arvid Winkelsdorf, info [at] digivendo [dot] de

  Last change:      02. November 2008

  Description:      TypeInfoEx allows RTTI retrieval of all modules
                    (BPLs, Dlls) in a comfortable and reversed way.

 * THIS SOFTWARE IS PROVIDED BY THE AUTHORS ''AS IS'' AND ANY EXPRESS
 * OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
 * EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

*****************************************************************************}

unit TypInfoEx;

interface

uses TypInfo;

type
  TTypeInfoArray = array of PTypeInfo;
  TTypeInfoEnumCallback = function(AUserData: Pointer; ATypeInfo: PTypeInfo): Boolean; register;
  TTypeInfoEnumMethod = function(ATypeInfo: PTypeInfo): Boolean of object;
  TTypeInfoSortCallback = function(AUserData: Pointer; ATypeInfo1, ATypeInfo2: PTypeInfo): Integer; register;
  TTypeInfoSortMethod = function(ATypeInfo1, ATypeInfo2: PTypeInfo): Integer of object;

const
  allModules = 0;

// enumerates all RTTI records of a given module or all modules
// returns found PTypeInfo if ACallback is returning true
// returns 1st RTTI record if ACallback is nil
function EnumTypeInfo(ACallback: TTypeInfoEnumCallback; AModule: LongWord = allModules; AUserData: Pointer = nil): PTypeInfo; overload;

// enumerates all RTTI records of a given TTypeInfoArray
// returns PTypeInfo is ACallback is true
function EnumTypeInfo(const ATypeInfoArray: TTypeInfoArray; ACallback: TTypeInfoEnumCallback; AUserData: Pointer = nil): PTypeInfo; overload;
function EnumTypeInfo(const ACallback: TTypeInfoEnumMethod; AModule: LongWord = allModules): PTypeInfo; overload;

// creates an array with all PTypeInfo which ACallback is filtering when true
// if ACallback is nil function returns all RTTI records
function CollectTypeInfo(ACallback: TTypeInfoEnumCallback; AModule: LongWord = allModules; AUserData: Pointer = nil): TTypeInfoArray; overload;
function CollectTypeInfo(const ACallback: TTypeInfoEnumMethod; AModule: LongWord = allModules): TTypeInfoArray; overload;
function CollectTypeInfo(const ATypeInfoArray: TTypeInfoArray; ACallback: TTypeInfoEnumCallback; AUserData: Pointer = nil): TTypeInfoArray; overload;
function CollectTypeInfo(const ATypeInfoArray: TTypeInfoArray; const ACallback: TTypeInfoEnumMethod): TTypeInfoArray; overload;

// creates an array of an Interface's PTypeInfo
function CollectInterfaces(AModule: LongWord = allModules): TTypeInfoArray;

// creates an Array of all PTypeInfos an interface is publishing and which are implemented through the class AClass
function CollectInterfaceTypesOfClass(AClass: TClass = nil; AModule: LongWord = 0): TTypeInfoArray; overload;
function CollectInterfaceTypesOfClass(const ATypeInfoArray: TTypeInfoArray; AClass: TClass = nil): TTypeInfoArray; overload;

// search for the TypeInfo of an Interface having the AGUID
function FindTypeInfo(const ATypeInfoArray: TTypeInfoArray; const AGUID: TGUID): PTypeInfo; overload;
function FindTypeInfo(const AGUID: TGUID; AModule: LongWord = allModules): PTypeInfo; overload;

// search for TypeInfo with TypeName
function FindTypeInfo(const ATypeName: ShortString; AModule: LongWord = allModules): PTypeInfo; overload;
function FindTypeInfo(const ATypeInfoArray: TTypeInfoArray; const ATypeName: ShortString): PTypeInfo; overload;

// search for class with AClassName
function FindClassByName(const AClassName: ShortString; AModule: LongWord = allModules): TClass; overload;
function FindClassByName(const ATypeInfoArray: TTypeInfoArray; const AClassName: ShortString): TClass; overload;

// searches all TypeInfo of all classes derivated from AInheritsFrom
function FindClasses(AInheritsFrom: TClass; AModule: LongWord = allModules): TTypeInfoArray; overload;
function FindClasses(const ATypeInfoArray: TTypeInfoArray; AInheritsFrom: TClass): TTypeInfoArray; overload;

// transforms ATypeInfo of a given class into the corresponding classtype
function TypeInfoToClass(ATypeInfo: PTypeInfo): TClass;

// returns the module in which ATypeInfo resides
function FindHInstanceOfTypeInfo(ATypeInfo: PTypeInfo): LongWord;
function ModuleHasType(AModule: LongWord; ATypeInfo: PTypeInfo): Boolean;

// sort ATypeInfoArray by ACallback
function SortTypeInfoArray(var ATypeInfoArray: TTypeInfoArray; ACallback: TTypeInfoSortCallback; AUserData: Pointer = nil): Boolean; overload;
function SortTypeInfoArray(var ATypeInfoArray: TTypeInfoArray; const ACallback: TTypeInfoSortMethod): Boolean; overload;

implementation

uses SysUtils;

{$IFNDEF UNICODE}
function CharInSet(C: Char; const CharSet: TSysCharSet): Boolean;
begin
  Result := C in CharSet;
end;
{$ENDIF}

function CompareGUID(const GUID1, GUID2: TGUID): Integer;
// can be used to sort a list of GUIDs
asm
        MOV   ECX,EAX
        MOV   EAX,[ECX + 0]
        SUB   EAX,[EDX + 0]
        JNZ   @Exit
        MOV   EAX,[ECX + 4]
        SUB   EAX,[EDX + 4]
        JNZ   @Exit
        MOV   EAX,[ECX + 8]
        SUB   EAX,[EDX + 8]
        JNZ   @Exit
        MOV   EAX,[ECX + 12]
        SUB   EAX,[EDX + 12]
@Exit:
end;

function DoEnumTypeInfo(AModule: LongWord; ACallback: TTypeInfoEnumCallback; AUserData: Pointer): PTypeInfo; overload;
// copyright (c) 1998 Hagen Reddmann

  function GetBaseOfCode(AModule: LongWord; var ACodeStart, ACodeEnd: PAnsiChar): Boolean; register;
  // get Codesegment pointers, check if module is a valid PE
  asm
           PUSH  EDI
           PUSH  ESI
           AND   EAX,not 3
           JZ    @@2
           CMP   Word Ptr [EAX],'ZM';
           JNE   @@1
           MOV   ESI,[EAX + 03Ch]
           CMP   Word Ptr [ESI + EAX],'EP'
           JNE   @@1
           MOV   EDI,[EAX + ESI + 014h + 008h]
           ADD   EAX,[EAX + ESI + 014h + 018h]
           ADD   EDI,EAX
           MOV   [EDX],EAX
           MOV   [ECX],EDI
           XOR   EAX,EAX
    @@1:   SETE  AL
    @@2:   POP   ESI
           POP   EDI
  end;

type
  PLongWord = ^LongWord;
  PByte = ^Byte;
var
  P,E,K,N: PAnsiChar;
  L: Integer;
begin
  Result := nil;
  try
    if GetBaseOfCode(AModule, P, E) then
      while P < E do
      begin
        LongWord(P) := LongWord(P) and not 3;
        K := P + 4;
        if (PLongWord(P)^ = LongWord(K)) and (TTypeKind(K^) >= Low(TTypeKind)) and (TTypeKind(K^) <= High(TTypeKind)) then
        begin
          L := PByte(K + 1)^;  // length Info.Name
          N := K + 2;          // @Info.Name[1]
          ;
          if (L > 0) and CharInSet(N^, ['_', 'a'..'z', 'A'..'Z']) then  // valid ident ??
          begin
            repeat
              Inc(N);
              Dec(L);
            until (L = 0) or not CharInSet(N^, ['_', 'a'..'z', 'A'..'Z', '0'..'9']);
            if L = 0 then // length and ident valid
              if not Assigned(ACallback) or ACallback(AUserData, Pointer(K)) then // tell it and if needed abort iteration
              begin
                Result := Pointer(K);
                Exit;
              end
              else
                K := N;
          end;
        end;
        P := K;
      end;
  except
  end;
end;

function EnumTypeInfo(ACallback: TTypeInfoEnumCallback; AModule: LongWord; AUserData: Pointer): PTypeInfo;
type
  PModulesEnumData = ^TModulesEnumData;
  TModulesEnumData = packed record
    ACallback: TTypeInfoEnumCallback;
    AUserData: Pointer;
    AResult: PTypeInfo;
  end;

  function DoEnum(AModule: LongWord; AData: PModulesEnumData): Boolean; register;
  begin
    with AData^ do
    begin
      AResult := DoEnumTypeInfo(AModule, ACallback, AUserData);
      Result := AResult = nil;
    end;
  end;

var
  Data: TModulesEnumData;
begin
  Data.ACallback := ACallback;
  Data.AUserData := AUserData;
  Data.AResult := nil;
  if AModule = allModules then EnumModules(TEnumModuleFuncLW(@DoEnum), @Data)
    else Data.AResult := DoEnumTypeInfo(AModule, ACallback, AUserData);
  Result := Data.AResult;
end;

function EnumTypeInfo(const ATypeInfoArray: TTypeInfoArray; ACallback: TTypeInfoEnumCallback; AUserData: Pointer): PTypeInfo;
var
  I: Integer;
begin
  Result := nil;
  for I := Low(ATypeInfoArray) to High(ATypeInfoArray) do
    if not Assigned(ACallback) or ACallback(AUserData, ATypeInfoArray[I]) then
    begin
      Result := ATypeInfoArray[I];
      Break;
    end;
end;

function EnumTypeInfo(const ACallback: TTypeInfoEnumMethod; AModule: LongWord): PTypeInfo;
begin
  if not Assigned(ACallback) then Result := EnumTypeInfo(nil, AModule)
    else Result := EnumTypeInfo(TMethod(ACallback).Code, AModule, TMethod(ACallback).Data);
end;

type
  PCollectEnumData = ^TCollectEnumData;
  TCollectEnumData =  packed record
    ACallback: TTypeInfoEnumCallback;
    AUserData: Pointer;
    ACount: Cardinal;
    AResult: TTypeInfoArray;
  end;

function DoCollect(AData: PCollectEnumData; ATypeInfo: PTypeInfo): Boolean; register;
begin
  with AData^ do
    if not Assigned(ACallback) or ACallback(AUserData, ATypeInfo) then
    begin
      if ACount mod 256 = 0 then SetLength(AResult, ACount + 256);
      AResult[ACount] := ATypeInfo;
      Inc(ACount);
    end;
  Result := False;
end;

function CollectTypeInfo(ACallback: TTypeInfoEnumCallback; AModule: LongWord; AUserData: Pointer): TTypeInfoArray;
var
  Data: TCollectEnumData;
begin
  Data.ACallback := ACallback;
  Data.AUserData := AUserData;
  Data.ACount := 0;
  Data.AResult := nil;
  EnumTypeInfo(@DoCollect, AModule, @Data);
  SetLength(Data.AResult, Data.ACount);
  Result := Data.AResult;
end;

function CollectTypeInfo(const ACallback: TTypeInfoEnumMethod; AModule: LongWord): TTypeInfoArray;
begin
  if not Assigned(ACallback) then Result := CollectTypeInfo(nil, AModule)
    else Result := CollectTypeInfo(TMethod(ACallback).Code, AModule, TMethod(ACallback).Data);
end;

function CollectTypeInfo(const ATypeInfoArray: TTypeInfoArray; ACallback: TTypeInfoEnumCallback; AUserData: Pointer): TTypeInfoArray;
var
  Data: TCollectEnumData;
  I: Integer;
begin
  Data.ACallback := ACallback;
  Data.AUserData := AUserData;
  Data.ACount := 0;
  Data.AResult := nil;
  for I := Low(ATypeInfoArray) to High(ATypeInfoArray) do
    DoCollect(@Data, ATypeInfoArray[I]);
  SetLength(Data.AResult, Data.ACount);
  Result := Data.AResult;
end;

function CollectTypeInfo(const ATypeInfoArray: TTypeInfoArray; const ACallback: TTypeInfoEnumMethod): TTypeInfoArray;
begin
  if not Assigned(ACallback) then Result := CollectTypeInfo(ATypeInfoArray, nil)
    else Result := CollectTypeInfo(ATypeInfoArray, TMethod(ACallback).Code, TMethod(ACallback).Data);
end;

function FindHInstanceOfTypeInfo(ATypeInfo: PTypeInfo): LongWord;
begin
  Result := FindHInstance(ATypeInfo);
end;

function ModuleHasType(AModule: LongWord; ATypeInfo: PTypeInfo): Boolean;
begin
  Result := AModule = FindHInstanceOfTypeInfo(ATypeInfo);
end;

function CollectInterfaces(AModule: LongWord): TTypeInfoArray;

  function DoCollect(Dummy: Pointer; ATypeInfo: PTypeInfo): Boolean; register;
  begin
    Result := ATypeInfo.Kind = tkInterface;
  end;

begin
  Result := CollectTypeInfo(@DoCollect, AModule);
end;

function DoGUID(AGUID: PGUID; ATypeInfo: PTypeInfo): Boolean; register;
begin
  if ATypeInfo.Kind <> tkInterface then Result := False else
    with GetTypeData(ATypeInfo)^ do
      Result := (ifHasGuid in IntfFlags) and (CompareGUID(GUID, AGUID^) = 0);
end;

function FindTypeInfo(const ATypeInfoArray: TTypeInfoArray; const AGUID: TGUID): PTypeInfo;
begin
  Result := EnumTypeInfo(ATypeInfoArray, @DoGUID, @AGUID);
end;

function FindTypeInfo(const AGUID: TGUID; AModule: LongWord): PTypeInfo;
begin
  Result := EnumTypeInfo(@DoGUID, AModule, @AGUID);
end;

function DoTypeName(AName: PChar; ATypeInfo: PTypeInfo): Boolean; register;
begin
  Result := CompareText(AName, String(ATypeInfo.Name)) = 0;
end;

function FindTypeInfo(const ATypeName: ShortString; AModule: LongWord): PTypeInfo;
begin
  Result := EnumTypeInfo(@DoTypeName, AModule, PChar(@ATypeName));
end;

function FindTypeInfo(const ATypeInfoArray: TTypeInfoArray; const ATypeName: ShortString): PTypeInfo;
begin
  Result := EnumTypeInfo(ATypeInfoArray, @DoTypeName, PChar(@ATypeName));
end;

function TypeInfoToClass(ATypeInfo: PTypeInfo): TClass;
begin
  if not Assigned(ATypeInfo) or (ATypeInfo.Kind <> tkClass) then Result := nil
    else Result := GetTypeData(ATypeInfo).ClassType;
end;

function FindClassByName(const AClassName: ShortString; AModule: LongWord): TClass;
begin
  Result := TypeInfoToClass(FindTypeInfo(AClassName, AModule));
end;

function FindClassByName(const ATypeInfoArray: TTypeInfoArray; const AClassName: ShortString): TClass;
begin
  Result := TypeInfoToClass(FindTypeInfo(ATypeInfoArray, AClassName));
end;

function DoClass(AInheritsFrom: TClass; ATypeInfo: PTypeInfo): Boolean; register;
begin
  Result := (ATypeInfo.Kind = tkClass) and GetTypeData(ATypeInfo).ClassType.InheritsFrom(AInheritsFrom);
end;

function FindClasses(AInheritsFrom: TClass; AModule: LongWord): TTypeInfoArray;
begin
  Result := CollectTypeInfo(@DoClass, AModule, AInheritsFrom);
end;

function FindClasses(const ATypeInfoArray: TTypeInfoArray; AInheritsFrom: TClass): TTypeInfoArray;
begin
  Result := CollectTypeInfo(ATypeInfoArray, @DoClass, AInheritsFrom);
end;

function DoClassGUID(AClass: TClass; ATypeInfo: PTypeInfo): Boolean; register;
begin
  if (AClass = nil) or (ATypeInfo.Kind <> tkInterface) then Result := False else
    with GetTypeData(ATypeInfo)^ do
      Result := (ifHasGuid in IntfFlags) and (AClass.GetInterfaceEntry(GUID) <> nil);
end;

function CollectInterfaceTypesOfClass(AClass: TClass; AModule: LongWord): TTypeInfoArray;
begin
  Result := CollectTypeInfo(@DoClassGUID, AModule, AClass);
end;

function CollectInterfaceTypesOfClass(const ATypeInfoArray: TTypeInfoArray; AClass: TClass): TTypeInfoArray;
begin
  Result := CollectTypeInfo(ATypeInfoArray, @DoClassGUID, AClass);
end;

function SortTypeInfoArray(var ATypeInfoArray: TTypeInfoArray; ACallback: TTypeInfoSortCallback; AUserData: Pointer): Boolean;

  procedure QuickSort(L,R: Integer);
  var
    I,J: Integer;
    M,T: PTypeInfo;
  begin
    I := L;
    repeat
      L := I;
      J := R;
      M := ATypeInfoArray[(L + R) shr 1];
      repeat
        while ACallback(AUserData, ATypeInfoArray[I], M) < 0 do Inc(I);
        while ACallback(AUserData, ATypeInfoArray[J], M) > 0 do Dec(J);
        if I > J then Break;
        T := ATypeInfoArray[I];
        ATypeInfoArray[I] := ATypeInfoArray[J];
        ATypeInfoArray[J] := T;
        Inc(I);
        Dec(J);
      until I > J;
      if L < J then QuickSort(L, J);
    until I >= R;
  end;

begin
  Result := Assigned(ACallback) and (High(ATypeInfoArray) > 0);
  if Result then QuickSort(0, High(ATypeInfoArray));
end;

function SortTypeInfoArray(var ATypeInfoArray: TTypeInfoArray; const ACallback: TTypeInfoSortMethod): Boolean;
begin
  Result := Assigned(ACallback) and SortTypeInfoArray(ATypeInfoArray, TMethod(ACallback).Code, TMethod(ACallback).Data);
end;

{
procedure Test;

  function DoPrint(AUserData: Pointer; ATypeInfo: PTypeInfo): Boolean; register;
  begin
    WriteLn(ATypeInfo.Name);
    Result := False;
  end;

  function DoSort(Dummy: Pointer; ATypeInfo1, ATypeInfo2: PTypeInfo): Integer; register;
  begin
    Result := AnsiCompareText(ATypeInfo1.Name, ATypeInfo2.Name);
  end;

var
  L: TTypeInfoArray;
begin
  L := CollectTypeInfo(nil);
  SortTypeInfoArray(L, @DoSort);
  EnumTypeInfo(L, @DoPrint);
end;
}

end.
