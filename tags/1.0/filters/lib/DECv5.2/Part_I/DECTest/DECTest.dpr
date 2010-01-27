program DECTest;

uses
  Classes,
  Windows,
  SysUtils,
  TypInfo,
  CPU,
  CRC,
  DECUtil,
  DECFmt,
  DECHash,
  DECCipher,
  DECRandom,
  Consts;

{$R *.RES}

procedure RegisterClasses;
begin
  RegisterDECClasses([TFormat_HEX, TFormat_HEXL, TFormat_MIME32, TFormat_MIME64,
                      TFormat_PGP, TFormat_UU, TFormat_XX, TFormat_ESCAPE]);

// or even in single steps
(*
  TFormat_HEX.Register;
  TFormat_HEXL.Register;
  TFormat_MIME32.Register;
  TFormat_MIME64.Register;
  TFormat_PGP.Register;
  TFormat_UU.Register;
  TFormat_XX.Register;
  TFormat_ESCAPE.Register;
*)

// prefered hashs
  THash_MD2.Register;        // 1.5Kb
  THash_MD4.Register;        // 2.0Kb                           // for fast checksums
  THash_MD5.Register;        // 2.5Kb
  THash_SHA.Register;        // 10Kb for SHA,SHA1,SHA256        // strong
  THash_SHA1.Register;
  THash_SHA256.Register;
  THash_SHA384.Register;     // 3.0Kb for SHA384,SHA512
  THash_SHA512.Register;                                        // variable digest
  THash_Sapphire.Register;   // 1.0Kb

  THash_Panama.Register;     // 2.0Kb
  THash_Tiger.Register;      // 12.0kb
  THash_RipeMD128.Register;  // 4.0Kb
  THash_RipeMD160.Register;  // 8.0Kb
  THash_RipeMD256.Register;  // 4.5Kb
  THash_RipeMD320.Register;  // 9.0Kb
  THash_Haval128.Register;   // 6.0Kb for all Haval's
  THash_Haval160.Register;
  THash_Haval192.Register;
  THash_Haval224.Register;
  THash_Haval256.Register;
  THash_Whirlpool.Register;   // 10.0Kb
  THash_Whirlpool1.Register;  // 10.0Kb
  THash_Square.Register;      // 10Kb
  THash_Snefru128.Register;   // 18Kb
  THash_Snefru256.Register;   //

//  TCipher_Null.Register;
  TCipher_Blowfish.Register;
  TCipher_Twofish.Register;
  TCipher_IDEA.Register;
  TCipher_CAST256.Register;
  TCipher_Mars.Register;
  TCipher_RC4.Register;
  TCipher_RC6.Register;
  TCipher_Rijndael.Register;
  TCipher_Square.Register;
  TCipher_SCOP.Register;
  TCipher_Sapphire.Register;
  TCipher_1DES.Register;
  TCipher_2DES.Register;
  TCipher_3DES.Register;
  TCipher_2DDES.Register;
  TCipher_3DDES.Register;
  TCipher_3TDES.Register;
  TCipher_3Way.Register;
  TCipher_Cast128.Register;
  TCipher_Gost.Register;
  TCipher_Misty.Register;
  TCipher_NewDES.Register;
  TCipher_Q128.Register;
  TCipher_RC2.Register;
  TCipher_RC5.Register;
  TCipher_SAFER.Register;
  TCipher_Shark.Register;
  TCipher_Skipjack.Register;
  TCipher_TEA.Register;
  TCipher_TEAN.Register;
end;

procedure PrintRegisteredClasses;

  function DoEnumClasses(Data: Pointer; ClassType: TDECClass): Boolean;
  begin
    Result := False;
    WriteLn(IntToHEX(ClassType.Identity, 8), ' : ', ClassType.ClassName);
  end;

begin
  WriteLn('registered classes');
  WriteLn;
  DECEnumClasses(@DoEnumClasses, nil);
  WriteLn;
end;

procedure Wait;
var
  Msg: TMsg;
begin
  WriteLn('press ESCAPE to terminate');
  while GetAsyncKeyState(vk_Escape) = 0 do
    if PeekMessage(Msg, 0, 0, 0, pm_Remove) then
    begin
      TranslateMessage(Msg);
      DispatchMessage(Msg);
    end;
end;

procedure TestCases;
// process testcases in file DECTest.vec
var
  CurChar: PAnsiChar;
  LineNo: Integer;
  Instance: TObject;
  ClassType: TDECClass;
// Cipher only special properties
  Password: Binary;
  IV: Binary;
  IFiller: Byte;

  procedure InvalidLine;
  begin
    raise Exception.CreateFmt('Invalid line format at %d', [LineNo]);
  end;

  function ExtractClassName: PAnsiChar;
  begin
    while CurChar^ in [' ', '['] do Inc(CurChar);
    Result := CurChar;
    while CurChar^ <> #0 do Inc(CurChar);
    while CurChar^ in [#0, ']', ' ', #13, #10] do Dec(CurChar);
    CurChar[1] := #0;
  end;

  procedure ExtractProperty(Instance: TObject);
  // setup property stored in Testvectors
  // format is .PropName=PropValue
  var
    PropName: PAnsiChar;
  begin
    while CurChar^ in [' ', '.'] do Inc(CurChar);
    PropName := CurChar;
    while not (CurChar^ in [#0, '=']) do Inc(CurChar);
    if CurChar^ <> #0 then
    begin
      CurChar^ := #0;
      Inc(CurChar);
      while CurChar^ in ['=', ' '] do Inc(CurChar);
      if Instance is TDECCipher then
        if AnsiCompareText(PropName, 'Password') = 0 then
        begin
          Password := TFormat_Escape.Decode(CurChar^, StrLen(CurChar));
          with TDECCipher(Instance).Context do
            if Length(Password) > KeySize then SetLength(Password, KeySize);
          Exit;
        end else
          if AnsiCompareText(PropName, 'IV') = 0 then
          begin
            IV := TFormat_Escape.Decode(CurChar^, StrLen(CurChar));
            Exit;
          end else
            if AnsiCompareText(PropName, 'IFiller') = 0 then
            begin
              IFiller := StrToInt(CurChar);
              Exit;
            end;
      try
        SetPropValue(Instance, PropName, AnsiString(CurChar));
      except
        on E: Exception do
        begin
          E.Message := E.Message + ' on ' + Instance.ClassName;
          raise;
        end;
      end;
    end else InvalidLine;
  end;

  function ExtractTestResult: Binary;
  // extract valid test result, and convertion from Escaped string
  // repositionate to testcases
  var
    R,P: PAnsiChar;
  begin
    while CurChar^ in [' ', '<'] do Inc(CurChar);
    R := CurChar;
    while not (CurChar^ in [#0, '>']) do Inc(CurChar);
    if CurChar^ <> '>' then InvalidLine;
    P := CurChar;
    while P^ in ['>', ' '] do Inc(P);
    if P^ <> '=' then InvalidLine;
    CurChar^ := #0;
    while P^ in ['=', ' ', '>'] do Inc(P);
    CurChar := P;
    Result := TFormat_Escape.Decode(R^, StrLen(R));
  end;

  function ExtractTest(var Data: Binary; var Count: Integer): Boolean;
  // extract one testcase and repetition
  var
    L: Boolean;
    T: Binary;
  begin
    Result := CurChar^ <> #0;
    if Result then
    begin
      Count := 0;
      Data := '';
      while CurChar^ = ' ' do Inc(CurChar);
      while CurChar^ in ['0'..'9'] do
      begin
        Count := Count * 10 + Ord(CurChar^) - Ord('0');
        Inc(CurChar);
      end;
      L := CurChar^ = '!';
      while not (CurChar^ in [#0, '<']) do Inc(CurChar);
      if CurChar^ = '<' then
      begin
        Inc(CurChar);
        while not (CurChar^ in [#0, '>']) do
        begin
          Data := Data + CurChar^;
          Inc(CurChar);
        end;
        if CurChar^ <> '>' then InvalidLine;
      end else InvalidLine;
      while CurChar^ in ['>',','] do Inc(CurChar);
      Data := TFormat_Escape.Decode(Data);
      if L then
      begin
        repeat
          T := T + Data;
          Dec(Count);
        until Count <= 0;
        Count := 1;
        Data := T;
      end; 
    end;
  end;

  procedure TestHash;
  // apply testcases to hash function
  var
    Digest: Binary;
    Data: Binary;
    Count: Integer;
    Hash: TDECHash absolute Instance;
  begin
    Digest := ExtractTestResult;
    Hash.Init;
    while ExtractTest(Data, Count) do
    repeat
      Hash.Calc(Data[1], Length(Data));
      Dec(Count);
    until Count <= 0;
    Hash.Done;

    Write(LineNo:5, ': ', Hash.Classname, ' ');
    if AnsiCompareText(Hash.DigestStr(TFormat_HEXL), Digest) <> 0 then WriteLn(Digest, ' != ', Hash.DigestStr(TFormat_HEXL))
      else WriteLn('test ok.');
  end;

  procedure TestCipher;
  var
    CipherText,PlainText,TestResult,PlainResult: Binary;
    Cipher: TDECCipher absolute Instance;
    Count: Integer;
  begin
    CipherText := ExtractTestResult;
    Cipher.Init(Password, IV, IFiller);
    TestResult := '';
    PlainResult := '';
    while ExtractTest(PlainText, Count) do
    begin
      PlainResult := PlainResult + PlainText;
      TestResult := TestResult + Cipher.EncodeBinary(PlainText, TFormat_Copy);
      Dec(Count);
    end;
    Cipher.Done;
    TestResult := TFormat_HEXL.Encode(TestResult);

    Write(LineNo:5, ': ', Cipher.Classname, ' ');
    if CipherText <> TestResult then
    begin
      WriteLn(CipherText, ' != ', TestResult);
      Exit;
    end;
    TestResult := Cipher.DecodeBinary(TestResult, TFormat_HEXL);
    if TestResult <> PlainResult then
    begin
      WriteLn('decode error');
      Exit;
    end;
    WriteLn('test ok.');
  end;

  procedure TestFormat;
  // apply testcases to conversions function
  var
    Test,Output,Data: Binary;
    Count: Integer;
    Format: TDECFormatClass absolute ClassType;
  begin
    Test := ExtractTestResult;
    ExtractTest(Data, Count);
    Output := Format.Encode(Data);

    Write(LineNo:5, ': ', Format.Classname, ' ');
    if Test <> Output then WriteLn(Test, ' != ', Output)
      else WriteLn('test ok.');
  end;

var
  VectorFile: Text;
  Line: AnsiString;
  TestProc: procedure;
begin
  WriteLn('processing test cases');
  WriteLn;

  Instance := nil;
  ClassType := nil;
  TestProc := nil;
  LineNo := 0;
  Assign(VectorFile, ExtractFilePath(ParamStr(0)) + 'DECTest.vec');
  try
    Reset(VectorFile);
    while not EOF(VectorFile) do
    begin
      ReadLn(VectorFile, Line);
      CurChar := PAnsiChar(Line);
      while (CurChar^ <> #0) and (CurChar^ = ' ') do Inc(CurChar);
      Inc(LineNo);
      case CurChar^ of
         #0: ;
        '#': ; // remark
        '[': begin // class
               FreeAndNil(Instance);
               TestProc := nil;
               ClassType := nil;
               if CurChar[1] <> '#' then
               try
                 ClassType := DECClassByName(ExtractClassName, TDECObject);
                 if ClassType.InheritsFrom(TDECHash) then
                 begin
                   Instance := ClassType.Create;
                   TestProc := @TestHash;
                 end else
                   if ClassType.InheritsFrom(TDECFormat) then
                   begin
                     TestProc := @TestFormat;
                   end else
                     if ClassType.InheritsFrom(TDECCipher) then
                     begin
                       Password := '';
                       IV := '';
                       IFiller := $FF;
                       Instance := ClassType.Create;
                       TestProc := @TestCipher;
                     end;
               except
                 on E: Exception do
                 begin
                   WriteLn(E.Message);
                 end;
               end;
             end;
        '.': if Instance <> nil then
               ExtractProperty(Instance);
        '<': if Assigned(TestProc) then // testcase
             begin
               asm
                   PUSH  EBP
                   CALL  TestProc
                   POP   ECX
               end;
             end;
      else
        if ClassType <> nil then
          InvalidLine;
      end;
    end;
  finally
    Close(VectorFile);
    FreeAndNil(Instance);
  end;
  WriteLn;
end;

procedure SpeedTestHashs;
const
  BufferSize = 1024 * 16;

  function DoSpeed(Buffer: PByteArray; HashClass: TDECHashClass): Boolean;
  var
    Start,Stop: Int64;
    ThreadPriority: Cardinal;
    ProcessPriority: Cardinal;
    I: Integer;
  begin
    Result := False;
    ProcessPriority := GetPriorityClass(GetCurrentProcess);
    ThreadPriority := GetThreadPriority(GetCurrentThread);
    with HashClass.Create do
    try
      SetPriorityClass(GetCurrentProcess, REALTIME_PRIORITY_CLASS);
      SetThreadPriority(GetCurrentThread, THREAD_PRIORITY_TIME_CRITICAL);
      Sleep(0);
      Start := CPU.RDTSC;
      for I := 0 to 4 do
      begin
        Init;
        Calc(Buffer[0], BufferSize);
        Done;
      end;
      Stop := (CPU.RDTSC - Start) div 5;
      WriteLn(ClassName, StringOfChar(' ', 20 - Length(ClassName)), ': ',
              Stop/BufferSize:10:1, ' cycles/byte ',
              CPUSpeed/Stop*BufferSize:10:2, ' Mb/sec');
    finally
      Free;
      SetThreadPriority(GetCurrentThread, ThreadPriority);
      SetPriorityClass(GetCurrentProcess, ProcessPriority);
    end;
    Sleep(0);
  end;

var
  Buffer: String;
begin
  WriteLn('compute hash performances');
  WriteLn;
  SetLength(Buffer, BufferSize);
  RandomBuffer(Buffer[1], BufferSize);
  DECEnumClasses(@DoSpeed, Pointer(Buffer), TDECHash);
  WriteLn;
end;

function TestVector: PAnsiChar; assembler; register;
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    030h,044h,0EDh,06Eh,045h,0A4h,096h,0F5h
         DB    0F6h,035h,0A2h,0EBh,03Dh,01Ah,05Dh,0D6h
         DB    0CBh,01Dh,009h,082h,02Dh,0BDh,0F5h,060h
         DB    0C2h,0B8h,058h,0A1h,091h,0F9h,081h,0B1h
         DB    000h,000h,000h,000h,000h,000h,000h,000h
end;

procedure TestCipher;

  function DoTest(Dummy: Pointer; CipherClass: TDECCipherClass): Boolean;
  var
    Buffer: array[0..31] of Byte;
    Key: Binary;
    I: Integer;
  begin
    Result := False;
    Key := CipherClass.ClassName;
    I := Length(Key);
    with CipherClass.Context do
      if I > KeySize then I := KeySize;
    SetLength(Key, I);

    with CipherClass.Create do
    try
      Mode := cmCTSx;
      Init(Key);

      Encode(TestVector^, Buffer, SizeOf(Buffer));
      Done;

      Decode(Buffer, Buffer, SizeOf(Buffer));
      Done;
      if not CompareMem(TestVector, @Buffer, SizeOf(Buffer)) then
        WriteLn(ClassName + StringOfChar(' ', 18 - Length(ClassName)), 'selftest fails');
    finally
      Free;
    end;
  end;

begin
  DECEnumClasses(@DoTest, nil, TDECCipher);
end;

procedure SpeedTestCiphers;
const
  BufferSize = 1024 * 16 * 2;

  function DoSpeed(Buffer: PByteArray; CipherClass: TDECCipherClass): Boolean;
  var
    Start,Stop: Int64;
    ThreadPriority: Cardinal;
    ProcessPriority: Cardinal;
    I,S: Integer;
  begin
    Result := False;
    ProcessPriority := GetPriorityClass(GetCurrentProcess);
    ThreadPriority := GetThreadPriority(GetCurrentThread);
    with CipherClass.Create do
    try
      Mode := cmECBx;
      Init(StringOfChar('x', Context.KeySize));

      S := BufferSize shr 1;
      I := S mod Context.BufferSize;
      Dec(S, I);

      SetPriorityClass(GetCurrentProcess, REALTIME_PRIORITY_CLASS);
      SetThreadPriority(GetCurrentThread, THREAD_PRIORITY_TIME_CRITICAL);
      Sleep(0);
      Start := CPU.RDTSC;
      for I := 0 to 2 do
      begin
        Encode(Buffer[0], Buffer[S], S);
        Done;
        Decode(Buffer[0], Buffer[S], S);
        Done;
      end;
      Stop := (CPU.RDTSC - Start) div 6;
      WriteLn(ClassName, StringOfChar(' ', 20 - Length(ClassName)), ': ',
              Stop/S:10:1, ' cycles/byte ',
              CPUSpeed/Stop*S:10:2, ' Mb/sec');
    finally
      Free;
      SetThreadPriority(GetCurrentThread, ThreadPriority);
      SetPriorityClass(GetCurrentProcess, ProcessPriority);
    end;
    Sleep(0);
  end;

var
  Buffer: Binary;
begin
  WriteLn('compute cipher performances');
  WriteLn;
  SetLength(Buffer, BufferSize);
  RandomBuffer(Buffer[1], BufferSize);
  DECEnumClasses(@DoSpeed, Pointer(Buffer), TDECCipher);
  WriteLn;
end;

procedure DemoCipher(Index: Integer);
// demonstrate en/decryption with cipher Blowfish and use of a
// secure Hash based random KDF -> Key Derivation Function
var
  Seed, Encoded, Decoded: Binary;
begin
  Seed := RandomBinary(16);

  with TCipher_Blowfish.Create do
  try
    Init(THash_SHA1.KDFx('Password here', Seed, Context.KeySize));
    Encoded := EncodeBinary('Secret data here', TFormat_MIME64);
  finally
    Free;
  end;

  with TCipher_Blowfish.Create do
  try
    Init(THash_SHA1.KDFx('Password here', Seed, Context.KeySize));
    Decoded := DecodeBinary(Encoded, TFormat_MIME64);
  finally
    Free;
  end;

  WriteLn(#13#10'Demo Cipher #', Index);
  WriteLn('encoded: ', Encoded);
  WriteLn('decoded: ', Decoded);
end;

procedure DemoCipherFile;
// demonstriert eine sehr sichere Anwendung von Verschlüsselungen, Hashfunktionen,
// Key Derivation Functions und Zufallsdaten.


  procedure EncodeFile(const AFileName: String; const APassword: Binary;
                       ACipher: TDECCipherClass = nil; AMode: TCipherMode = cmCTSx;
                       AHash: TDECHashClass = nil);
  // Die Datei wird verschlüsselt danach überschrieben und gelöscht.
  // Die verschlüsselte Datei wurde mit einem Session Passwort verschlüsselt das mit Hilfe
  // einer KDF = Key Derivation Funktion und einem Zufallswert erzeugt wurde.
  // Der Zufallswert == Seed ist 128 Bits groß und wird in der verschlüsselten Datei gespeichert.
  // Dieser stellt sicher das es unmöglich wird das Passwort zu knacken und randomisiert zusätzlich
  // die Daten der Vershlüsselung. Am Ende der verschlüsselten Datei wird eine Prüfsumme gespeichert
  // mit Hilfe einer CMAC = Cipher Message Authentication Code.
  // Die verschlüsselte Datei enthält am Anfang zusätzlich noch Informationen zum
  // verwendeten Cipher-/Hash Algorithmus, CipherMode usw. Dies ermöglicht bei der Entschlüsselung der
  // Datei die automatische Auswahl der Algorithmen.
  // Werden für Cipher und oder Hash == nil übergeben so wird der Standard Cipher/Hash benutzt.
  // Das benutze Session Passwort hat immer Zufällige Eigenschaften, es verhält sich wie Zufallsdaten.
  // Nur derjenige der den Zufalls-Seed und APassword kennt kann die Daten korrekt entschlüsseln.
  var
    Dest: TStream;

    procedure Write(const Value; Size: Integer);
    begin
      Dest.WriteBuffer(Value, Size);
    end;

    procedure WriteByte(Value: Byte);
    begin
      Write(Value, SizeOf(Value));
    end;

    procedure WriteLong(Value: LongWord);
    begin
      Value := SwapLong(Value);
      Write(Value, SizeOf(Value));
    end;

    procedure WriteBinary(const Value: Binary);
    begin
      WriteByte(Length(Value));
      Write(Value[1], Length(Value));
    end;

  var
    Source: TStream;
    Seed: Binary;
  begin
    ACipher := ValidCipher(ACipher);
    AHash := ValidHash(AHash);

    Seed := RandomBinary(16);

    Source := TFileStream.Create(AFileName, fmOpenReadWrite);
    try
      Dest := TFileStream.Create(AFileName + '.enc', fmCreate);
      try
        with ACipher.Create do
        try
          Mode := AMode;
          Init(AHash.KDFx(APassword, Seed, Context.KeySize));

          WriteLong(Identity);
          WriteByte(Byte(Mode));
          WriteLong(AHash.Identity);
          WriteBinary(Seed);
          WriteLong(Source.Size);
          EncodeStream(Source, Dest, Source.Size);
          WriteBinary(CalcMAC);
        finally
          Free;
        end;
      finally
        Dest.Free;
      end;
      ProtectStream(Source);
    finally
      Source.Free;
    end;
    DeleteFile(AFileName);
  end;

  procedure DecodeFile(const AFileName: String; const APassword: Binary);
  // entschüssele eine Datei die vorher mit EncodeFile() verschlüsselt wurde.
  var
    Source: TStream;

    procedure Read(var Value; Size: Integer);
    begin
      Source.ReadBuffer(Value, Size);
    end;

    function ReadByte: Byte;
    begin
      Read(Result, SizeOf(Result));
    end;

    function ReadLong: LongWord;
    begin
      Read(Result, SizeOf(Result));
      Result := SwapLong(Result);
    end;

    function ReadBinary: Binary;
    begin
      SetLength(Result, ReadByte);
      Read(Result[1], Length(Result));
    end;

  var
    Dest: TStream;
  begin
    Source := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
    try
      try
        Dest := TFileStream.Create(ChangeFileExt(AFileName, ''), fmCreate);
        try
          try
            with CipherByIdentity(ReadLong).Create do
            try
              Mode := TCipherMode(ReadByte);
              Init(HashByIdentity(ReadLong).KDFx(APassword, ReadBinary, Context.KeySize));
              DecodeStream(Source, Dest, ReadLong);
              if ReadBinary <> CalcMAC then
                raise EDECException.Create('Invalid decryption');
            finally
              Free;
            end;
          except
            ProtectStream(Dest);
            raise;
          end;
        finally
          Dest.Free;
        end;
      except
        DeleteFile(ChangeFileExt(AFileName, ''));
        raise;
      end;
    finally
      Source.Free;
    end;
  end;

var
  FileName: String;
begin
  WriteLn(#13#10'File En/Decryption test');

// stelle Standard Cipher/Hash ein.
  SetDefaultCipherClass(TCipher_Rijndael);
  SetDefaultHashClass(THash_SHA1);
// Stelle die Basis-Identität der Cipher/Hash Algorithmen auf einen Anwendungsspezifischen Wert ein.
// Damit ist sichergestellt das nur Dateien die mit dieser Anwendung verschlüsselt wurden auch wieder
// entschlüselbar sind. Dei Verschlüsselungsfunktion oben speichert ja die Identity des benutzen
// Ciphers/Hashs in der verschlüsselten Datei ab. Beim Entschlüsseln mit DecodeFile() werden diese
// Identities geladen und aus den Regstrierten DECClassen geladen.
  IdentityBase := $84485225;
// alle benutzten und ladbaren Cipher/Hash müssen registriert werden.
  RegisterDECClasses([TCipher_Rijndael, THash_SHA1]);
// obige Sourcezeilen sollten normalerweise im Startup der Anwendung erfolgen.

  FileName := ChangeFileExt(ParamStr(0), '.test');
  EncodeFile(FileName, 'Password');
  DecodeFile(FileName + '.enc', 'Password');
end;

begin
  RandomSeed; // randomize DEC's own RNG
  AssignFile(Output, ChangeFileExt(ParamStr(0), '.txt'));
  try
    RegisterClasses;
    PrintRegisteredClasses;
    TestCases;
    SpeedTestHashs;
    TestCipher;
    SpeedTestCiphers;

    DemoCipher(0);
    DemoCipher(1);
    DemoCipher(2);

    DemoCipherFile;
  except
    on E: Exception do WriteLn(E.Message);
  end;
//  Wait;
end.

