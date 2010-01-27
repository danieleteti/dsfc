{*****************************************************************************

  Delphi Encryption Compendium (DEC Part I)
  Version 5.2, Part I, for Delphi 7 - 2009

  Remarks:          Freeware, Copyright must be included

  Original Authors: Heiko Behrens (Initiator and Developer), Hagen Reddmann
                    (c) 2006 Heiko Behrens and
                    Hagen Reddmann, HaReddmann [at] T-Online [dot] de
  Modifications:    (c) 2008 Arvid Winkelsdorf, info [at] digivendo [dot] de

  Last change:      02. November 2008

  Description:      very small and efficient LHSS compression
                    with RC4 like encryption and 32 Bit Checksum

  Remarks:          LHEncodeBuffer() and LHDecodeBuffer() parameter out Data:
                    Pointer MUST be released with FreeMem(Data) by the caller!
                    The interface here works only on one linear chunk of input
                    and process this in one single step. But processing of
                    sequential chunks are possible with LHDeflate() and
                    LHInflate(). Look into LHEncode() and LHDecode() to see
                    some right initialization.
                    Without Encryption the minimal compressable input should
                    be > 10 Bytes. With Encryption the minimal compressable
                    input should be > 13 Bytes. Below these limits the output
                    is larger as the input.

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

unit LHSZ;

{.$D-,L-,Y-,C-,O+}

{$DEFINE LHEncode}  // include compression code
{$DEFINE LHDecode}  // include decompression code
{.$DEFINE LHCrypt}   // include encryption code

interface

const
  LH_ErrProtected   = -9;  // compressed Data is password protected
  LH_ErrPassword    = -8;  // bad Password in Decoding
  LH_ErrCRC         = -7;  // bad CRC or decompressed Data detected (Decode only)
  LH_ErrInflate     = -6;  // error in decode
  LH_ErrWrite       = -5;  // write error in Method WriteProc
  LH_ErrRead        = -4;  // read error in Method RreadProc
  LH_ErrInit        = -3;  // error in initialization phase
  LH_ErrAlloc       = -2;  // can't allocated memory
  LH_ErrGeneric     = -1;  // unspecific error

  LH_Ready          =  0;  // all ok

  // Compression Mode Flags
  LH_TypeMask       = $FF00;
  LH_ModeMask       = $00FF;

  LH_Auto           = $0000;

  // mode flags
  LH_Fastest        = $0001;
  LH_Fast           = $0020;
  LH_Normal         = $0040;
  LH_High           = $0080;
  LH_Max            = $00FF;

  // type flags
  LH_Text           = $0100;
  LH_Binary         = $0200;
  LH_Huffman        = $0400;

type
  TReadProc  = function(var Buffer; Count: Integer): Integer of object;
  TWriteProc = function(const Buffer; Count: Integer): Integer of object;

{$IFDEF LHEncode}
function LHEncode(const Password: AnsiString; ReadProc: TReadProc; WriteProc: TWriteProc; Size, Mode: Integer): Integer;
function LHEncodeBuffer(const Password: AnsiString; const Buffer; BufferSize: Integer; out Data: Pointer): Integer;
{$ENDIF}

{$IFDEF LHDecode}
function LHDecode(const Password: AnsiString; ReadProc: TReadProc; WriteProc: TWriteProc; Size: Integer): Integer;
function LHDecodeBuffer(const Password: AnsiString; const Buffer; BufferSize: Integer; out Data: Pointer): Integer;
{$ENDIF}

function LHCheck(Code: Integer): Integer; // raise exception if code is a error

implementation

uses SysUtils;
{
  generated Codesizes with D3, only LHEncode, LHDecode are used, Bufferprocs ignored

  $DEFINES                         size in bytes
    LHEncode                          3.640
    LHEncode, LHCrypt                 4.256

    LHDecode                          2.404
    LHDecode, LHCrypt                 2.968

    LHEncode, LHDecode                5.148
    LHEncode, LHDecode, LHCrypt       6.104

  Datesizes are always 0
}

{$ALIGN ON}
{$IFOPT O+}
  {.$DEFINE UseASM}
{$ENDIF}

const
  LH_MinCopy        =       2;   { don't modify, except you increase}
  LH_MaxCopy        =     257;   { should be a power of two +1}
  LH_CodesPerRange  =   LH_MaxCopy - LH_MinCopy +1;

  LH_nil            =      -1;           { End of linked list marker}
  LH_HashBits       =      12;           { optimal }
  LH_HashSize       = 1 shl LH_HashBits; { Number of entries in hash table, should be }
  LH_HashMask       = LH_HashSize -1;    { Mask for hash key wrap }

  { Adaptive Huffman variables }

  LH_CodeBits       =      32;

  LH_CopyRanges     =      16;
  // (0, 2, 6, 14, 30, 62, 126, 254, 510, 1022, 2046, 4094, 8190, 16382, 32766, 65534, 131070, 262142, 524286);
  // stored dynamicaly in TLHData.Range, so we need NO Datasegement for LHSZ

  LH_MaxSize        = 131070 + LH_MaxCopy;  // dependend from LH_CopyRange

  LH_Special        =     256;          { Command code, subcommands can be 0-255 }
  LH_SpecialINC     =       0;          { Subcommand, copy Range increment}
  LH_SpecialEOF     =       1;          { Subcommand, Terminate}
  LH_SpecialCRC     =       2;          { Subcommand, Checksum}

  LH_SpecialBITS    =       3;


  LH_FirstCode      =     257;          { First code for COPYING lengths }
  LH_MaxChar        = LH_FirstCode + LH_CopyRanges * LH_CodesPerRange -1;
  LH_MaxChar2       = LH_MaxChar * 2 +1;
  LH_Root           =       1;
  LH_BufSize        =    1024 * 4; { buffer size, must be a multiply of Sizeof(Integer) }

  // LHState
  LH_Init           =  1;
  LH_Working        =  2;
  LH_Finish         =  3;

type
  PInteger = ^Integer;
  PByte    = ^Byte;
  PWord    = ^Word;

  PLHData = ^TLHData;
  TLHData = packed record
    Data: array[0..LH_BufSize -1] of Byte;   // IN for Deflate, OUT for Inflate
    Code: array[0..LH_BufSize -1] of Byte;   // OUT for Deflate, IN for Inflate

    CRCTable: array[0..255] of Integer;
    CRC: Integer;

    // from here
    TextPos: Integer;

    DataPos: Integer;
    DataBytes: Integer;
    DataSize: Integer;

    CodeBits: Integer;
    CodeBitsCount: Integer;
    CodePos: Integer;
    CodeBytes: Integer;
    CodeSize: Integer;

    Flag: Integer;
    Text: array[0..LH_MaxSize + LH_MaxCopy] of Byte;
    // upto here, don't change this order, we fillout these with zero in one step !

    State: Integer; // current State
    InputSize: Integer;
    Read: TReadProc;
    Write: TWriteProc;

    { Huffman tree }
    Range: array[0..LH_CopyRanges] of Integer;
    RangeCopy: Integer;
    RangeMax: Integer;

    FreqCum: Integer;
    FreqReset: Integer;

    Left: array[LH_Root..LH_MaxChar] of Word;
    Right: array[LH_Root..LH_MaxChar] of Word;
    Parent: array[LH_Root..LH_MaxChar2] of Word;
    Freq: array[LH_Root..LH_MaxChar2] of Word;

    Chars: array[Byte] of Integer;

    {encryption, modified RC4 with 8Bit CBC Freedback and Datadependend SBox shuffeling}
    {$IFDEF LHCrypt}
      PC4_T: array[0..255] of Byte;
      PC4_P: Integer;
      PC4_I: Byte;
      PC4_J: Byte;
      PC4_F: Byte;
    {$ENDIF}

    {LZSS data, beginning of deflate only datas}
    Head: array[0..LH_HashSize -1] of Integer;
    Tail: array[0..LH_HashSize -1] of Integer;
    Next: array[0..LH_MaxSize  -1] of Integer;
    Prev: array[0..LH_MaxSize  -1] of Integer;

    Mode: Integer; // LH_Mode Flags
    ResetPos: Integer;
    SearchMax: Integer;
    SearchDepth: Integer;
    TextLen: Integer;
    RangeDist: Integer;
    RangeLimit: Integer;
    //    LastBytes: Integer;
    //    OverBytes: Integer;

    CurPos: Integer;
    NewPos: Integer;
    Distance: Integer;
  end;

// procedures for deflation and inflation

procedure LHFill(Buffer: Pointer; Size: Integer); assembler; register;
asm
         PUSH  EDI
         MOV   EDI,EAX
         MOV   ECX,EDX
         SHR   ECX,2
         XOR   EAX,EAX
         REP   STOSD
         POP   EDI
end;

procedure LHInitCRC(LH: PLHData);
{$IFDEF UseASM}
asm
         PUSH  EBX
         MOV   [EAX].TLHData.CRC,0FFFFFFFFh
         ADD   EAX,OFFSET TLHData.CRCTable
         MOV   ECX,255
@@1:     MOV   EDX,ECX
         MOV   EBX,8
@@2:     SHR   EDX,1
         JNC   @@3
         XOR   EDX,0EDB88320h
@@3:     DEC   EBX
         JNZ   @@2
         MOV   [EAX + ECX * 4],EDX
         DEC   ECX
         JNL   @@1
         POP   EBX
end;
{$ELSE}
var
  I,J,V: Integer;
begin
  for I := 0 to 255 do
  begin
    V := I;
    for J := 0 to 7 do
      if V and 1 <> 0 then V := (V shr 1) xor Integer($EDB88320)
        else V := V shr 1;
    LH.CRCTable[I] := V;
  end;
  LH.CRC := Integer($FFFFFFFF);
end;
{$ENDIF}

function LHUpdateCRC(LH: PLHData; const Buffer; Size: Integer): Integer;
{$IFDEF UseASM}
asm
         PUSH  EBX
         PUSH  EDI
         XOR   EBX,EBX
         LEA   EDI,[EAX].TLHData.CRCTable
         MOV   EAX,[EAX].TLHData.CRC
         DEC   ECX
         JLE   @@2

@@1:     MOV   EBX,[EDX]
         INC   EDX
         XOR   EBX,EAX
         SHR   EAX,8
         MOVZX EBX,BL
         XOR   EAX,[EDI + EBX * 4]
         DEC   ECX
         JNL   @@1

@@2:     POP   EDI
         POP   EBX
end;
{$ELSE}
var
  P: ^Byte;
  C: Integer;
begin
  P := @Buffer;
  C := LH.CRC;
  while Size > 0 do
  begin
    Dec(Size);
    C := C shr 8 xor LH.CRCTable[(C xor P^) and $FF];
    Inc(P);
  end;
  Result := C;
end;
{$ENDIF}

{$IFDEF LHCrypt}
procedure LHInitCrypt(LH: PLHData; const Password: AnsiString);
var
  I,S,J: Integer;
  K: array[0..255] of Byte;
begin
  LH.PC4_P := 0;
  LH.PC4_I := 0;
  LH.PC4_J := 0;
  S := Length(Password);
  if S = 0 then Exit;
  J := 0;
  for I := 0 to 255 do
  begin
    LH.PC4_T[I] := I;
    K[I] := Byte(Password[I mod S +1]);
    J := (J + K[I] * 257) mod MaxInt +1;
  end;
  LH.PC4_P := J;
  LH.PC4_F := J shr 8;
  for I := 0 to 255 do
  begin
    J := (J + LH.PC4_T[I] + K[I]) and $FF;
    S := LH.PC4_T[I];
    LH.PC4_T[I] := LH.PC4_T[J];
    LH.PC4_T[J] := S;
  end;
end;
{$ENDIF}

// Huffman support

procedure LHInitHuffman(LH: PLHData);
var  { Initialize Huffman frequency tree }
  I: Integer;
begin
  LH.Range[0] := 0;
  for I := 1 to High(LH.Range) do
    LH.Range[I] := LH.Range[I -1] * 2 + 2;
  LH.FreqCum := LH_MaxChar2;
  LH.FreqReset := 20000;
  LHFill(@LH.Chars, SizeOf(LH.Chars));
  for I := LH_Root to LH_MaxChar2 do
  begin
    LH.Parent[I] := I shr 1;
    LH.Freq[I] := 1;
  end;
  for I := LH_Root to LH_MaxChar do
  begin
    LH.Left[I] := I * 2;
    LH.Right[I] := I * 2 + 1;
  end;
end;

procedure LHResetFrequency(LH: PLHData);
{$IFDEF UseASM}
asm
         PUSH  EBX
         PUSH  EDI
         PUSH  ESI

         LEA   ESI,[EAX].TLHData.Freq
         MOV   ECX,LH_MaxChar2 shr 1
         XOR   EDI,EDI

@@1:     MOV   EAX,[ESI]

         ADD   EAX,000010001h
         AND   EAX,0FFFEFFFEh
         SHR   EAX,1
         MOV   EDX,EAX
         MOV   [ESI],EAX
         SHR   EDX,16
         MOVZX EAX,AX
         ADD   EDI,EDX
         ADD   EDI,EAX

         DEC   ECX
         LEA   ESI,[ESI + 4]
         JNZ   @@1

// process last Word
         MOVZX EAX,Word Ptr [ESI]
         ADD   EAX,1
         AND   EAX,0FFFEh
         SHR   EAX,1
         MOV   [ESI],AX
         ADD   EDI,EAX
         MOV   [EBX].TLHData.FreqCum,EDI

         POP   ESI
         POP   EDI
         POP   EBX
end;
{$ELSE}
var
  I: Integer;
begin
  LH.FreqCum := 0;
  for I := LH_Root to LH_MaxChar2 do
  begin
    LH.Freq[I] := (LH.Freq[I] + 1) shr 1;
    Inc(LH.FreqCum, LH.Freq[I]);
  end;
end;
{$ENDIF}

procedure LHUpdateModel(LH: PLHData; Code: Integer);

  procedure LHUpdateFrequency(LH: PLHData; A,B: Integer);
  begin { Update frequency counts from leaf to root }
    repeat
      B := LH.Freq[A] + LH.Freq[B];
      A := LH.Parent[A];
      Inc(LH.FreqCum, B - LH.Freq[A]);
      LH.Freq[A] := B;
      if A <> LH_Root then
      begin
        B := LH.Parent[A];
        if LH.Left[B] <> A then B := LH.Left[B] else B := LH.Right[B];
      end else Break;
    until False;
  end;

var { Update Huffman model for each character code }
  A, B, C, X, Y: Integer;
begin
  Inc(LH.Chars[Code mod 256]);
  if LH.FreqCum > LH.FreqReset then
  begin
    C := 0;
    for X := 0 to 255 do
    begin
      if LH.Chars[X] > 0 then Inc(C);
      LH.Chars[X] := 0;//LH.Chars[X] shr 3;
    end;
    if (C < 64) and (LH.FreqReset > 14000) then
      Dec(LH.FreqReset, 1000)
    else
      if (C > 128) and (LH.FreqReset < 20000) then
        Inc(LH.FreqReset, 1000);
    LHResetFrequency(LH);
  end;
  A := Code + LH.RangeMax;
  Inc(LH.Freq[A]);
  Inc(LH.FreqCum);
  X := LH.Parent[A];
  if X <> LH_Root then
  begin
    if LH.Left[X] <> A then LHUpdateFrequency(LH, A, LH.Left[X])
      else LHUpdateFrequency(LH, A, LH.Right[X]);
    repeat
      Y := LH.Parent[X];
      if LH.Left[Y] <> X then B := LH.Left[Y] else B := LH.Right[Y];
      if LH.Freq[A] >= LH.Freq[B] then
      begin
        LH.Parent[A] := Y;
        LH.Parent[B] := X;
        if LH.Left[Y] <> X then LH.Left[Y] := A else LH.Right[Y] := A;
        C := LH.Left[X];
        if C = A then
        begin
          LH.Left[X] := B;
          C := LH.Right[X];
        end else LH.Right[X] := B;
        LHUpdateFrequency(LH, B, C);
        A := B;
      end;
      A := LH.Parent[A];
      X := LH.Parent[A];
    until X = LH_Root;
  end;
end;

// deflation
{$IFDEF LHEncode}
procedure LHDeflate(LH: PLHData);
const
  LH_Found    = 1;
  LH_Full     = 2;
  LH_First    = 4;

  LH_ModeHuff = Integer($80000000);
  LH_ModeBIN  = $40000000;

  function LHHash(LH: PLHData; Index: Integer): Integer;
{$IFDEF UseASM}
  asm
         MOV    EAX,DWord Ptr [EAX].TLHData.Text[EDX]  // Text use overestimated Ringbuffer
         AND    EAX,0FFFFFFh
         MOV    ECX,EAX
         SHR    ECX,9
         XOR    EAX,ECX
         SHR    ECX,5
         XOR    EAX,ECX
         AND    EAX,LH_HashMask
  end;
{$ELSE}
  var
    I: Integer;
  begin
    I := PInteger(@LH.Text[Index])^ and $FFFFFF;
    Result := (I xor (I shr 9) xor (I shr 14)) and LH_HashMask;
  end;
{$ENDIF}

  procedure LHInitLZSS(LH: PLHData);
{$IFDEF UseASM}
  asm
         PUSH   EBX
         PUSH   EDI
         MOV    EBX,EAX

         XOR    EAX,EAX
         LEA    EDI,[EBX].TLHData.DataPos
         MOV    [EBX].TLHData.TextLen,EAX
         MOV    [EBX].TLHData.ResetPos,EAX
         MOV    ECX,10 + LH_MaxSize shr 2
         REP    STOSD

         MOV    EAX,LH_MinCopy
         MOV    [EBX].TLHData.TextPos,EAX
         MOV    [EBX].TLHData.NewPos,EAX

         MOV    [EBX].TLHData.CodeBitsCount,LH_CodeBits

         LEA    EDI,[EBX].TLHData.Head
         MOV    EAX,LH_nil
         MOV    ECX,LH_HashSize
         REP    STOSD

         POP    EDI
         POP    EBX
  end;
{$ELSE}
  var
    I: Integer;
  begin
    with LH^ do
    begin
      LHFill(@LH.DataPos, LH_MaxSize + 10 * 4);
      TextLen := 0;
      ResetPos := 0;
      CodeBitsCount := LH_CodeBits;
      TextPos := LH_MinCopy;
      NewPos := LH_MinCopy;
      for I := Low(Head) to High(Head) do Head[I] := LH_nil;
    end;
  end;
{$ENDIF}

  procedure LHInsertNode(LH: PLHData; N: Integer);
{$IFDEF UseASM}  { insert node to head of list }
  asm
         PUSH   EBX

         MOV    EBX,EAX
         CALL   LHHash               // EAX = Key

         MOV    ECX,DWord Ptr [EBX].TLHData.Head[EAX * 4]  // ECX = T
         MOV    DWord Ptr [EBX].TLHData.Head[EAX * 4],EDX
         MOV    DWord Ptr [EBX].TLHData.Prev[EDX * 4],LH_nil
         CMP    ECX,LH_nil
         JNZ    @@1

         MOV    DWord Ptr [EBX].TLHData.Tail[EAX * 4],EDX
         MOV    DWord Ptr [EBX].TLHdata.Next[EDX * 4],LH_nil
         JMP    @@2

@@1:     MOV    DWord Ptr [EBX].TLHData.Prev[ECX * 4],EDX
         MOV    DWord Ptr [EBX].TLHData.Next[EDX * 4],ECX

@@2:     POP    EBX
  end;
{$ELSE}
  var
    Key,T: Integer;
  begin
    Key := LHHash(LH, N);
    with LH^ do
    begin
      T := Head[Key];
      Head[Key] := N;
      Prev[N] := LH_nil;
      if T = LH_nil then
      begin
        Tail[Key] := N;
        Next[N] := LH_nil;
      end else
      begin
        Next[N] := T;
        Prev[T] := N;
      end;
    end;
  end;
{$ENDIF}

  procedure LHDeleteNode(LH: PLHData; N: Integer);
{$IFDEF UseASM} { Delete node from tail of list }
  asm
         PUSH   EBX
         MOV    EBX,EAX
         CALL   LHHash               // EAX = Key

         MOV    ECX,DWord Ptr [EBX].TLHData.Tail[EAX * 4]
         CMP    ECX,LH_Nil
         JE     @@0
         CMP    ECX,DWord Ptr [EBX].TLHData.Head[EAX * 4]
         JNE    @@1
@@0:     MOV    DWord Ptr [EBX].TLHData.Head[EAX * 4],LH_nil
         JMP    @@2

@@1:     MOV    ECX,DWord Ptr [EBX].TLHData.Prev[ECX * 4]
         MOV    DWord Ptr [EBX].TLHData.Tail[EAX * 4],ECX
         CMP    ECX,LH_nil
         JE     @@2
         MOV    DWord Ptr [EBX].TLHData.Next[ECX * 4],LH_nil

@@2:     POP    EBX
  end;
{$ELSE}
  var
    Key, T: Integer;
  begin
    Key := LHHash(LH, N);
    with LH^ do
    begin
      T := Tail[Key];
      if (T <> LH_nil) and (Head[Key] <> T) then
      begin
        T := Prev[T];
        Tail[Key] := T;
        if T <> LH_nil then Next[T] := LH_nil;
      end else Head[Key] := LH_nil;
    end;
  end;
{$ENDIF}

  procedure LHUpdateRange(LH: PLHData); forward;

  function LHMatch(LH: PLHData; SearchDepth: Boolean): Integer;
{ Find longest string matching lookahead buffer string }

    function LHCompare(LH: PLHData; N, K: Integer): Integer;
    var
      I: Integer;
    begin
      Result := 0;
      I := N;
      while (K <> N) and (I <> LH.TextPos) and (LH.Text[I] = LH.Text[K]) do
      begin
        Inc(I);
        Inc(K);
        Inc(Result);
        if Result >= LH_MaxCopy then Exit;
      end;
    end;

  var
    N,K,L,D,C,Depth: Integer;
  begin
    Result := 0;
    N := LH.NewPos;
    if SearchDepth then
    begin
      Depth := LH.SearchDepth;
      if Depth <= 0 then Exit;
      Inc(N);
      if N >= LH_MaxSize then N := 0;
    end else
    begin
      Depth := LH.SearchMax;
      LH.Distance := 0;
    end;

    K := LH.Head[LHHash(LH, N)];
    if K = LH_nil then Exit;

    C := LH.Text[N];
    repeat
      if C = LH.Text[K + Result] then
      begin
        L := LHCompare(LH, N, K);
        if (L >= LH_MinCopy) and (L > Result) then
        begin
          D := N - K - L;
          if D < 0 then Inc(D, LH_MaxSize);
          if not SearchDepth then LH.Distance := D;
          Result := L;
          if L >= LH_MaxCopy then Exit;
          C := LH.Text[L + N];
        end;
      end;
      Dec(Depth);
      if Depth <= 0 then Exit;
      K := LH.Next[K];
    until K = LH_nil;
  end;

{$IFDEF LHCrypt}
  procedure LHCrypt(LH: PLHData; Size: Integer);
  var
    S: Byte;
    B: PByte;
  begin
    B := @LH.Code;
    if LH.Flag and LH_First = 0 then
    begin
      Inc(B);
      Dec(Size);
      LH.Flag := LH.Flag or LH_First;
    end;
    while Size > 0 do
    begin
      Dec(Size);
      Inc(LH.PC4_I);
      S := LH.PC4_T[LH.PC4_I];
      Inc(LH.PC4_J, S);
      LH.PC4_T[LH.PC4_I] := LH.PC4_T[LH.PC4_J] xor LH.PC4_F;
      LH.PC4_T[LH.PC4_J] := S - LH.PC4_F;
      B^ := (B^ + LH.PC4_F) xor LH.PC4_T[(LH.PC4_T[LH.PC4_I] + S) and $FF];
      LH.PC4_F := B^;
      Inc(B);
    end;
  end;
{$ENDIF}
  function LHWrite(LH: PLHData): Boolean;
  begin
    if LH.State >= LH_Ready then
    begin
      LH.CodeBitsCount := LH_CodeBits;
      PInteger(@LH.Code[LH.CodePos])^ := LH.CodeBits;
      Inc(LH.CodePos, SizeOf(LH.CodeBits));
      Inc(LH.CodeBytes, SizeOf(LH.CodeBits));
      LH.CodeBits := 0;
      if LH.CodePos >= SizeOf(LH.Code) then
      begin
{        if LH.DataBytes - LH.LastBytes < LH.CodePos then
          Inc(LH.OverBytes, LH.CodePos - (LH.DataBytes - LH.LastBytes));
        LH.LastBytes := LH.DataBytes;}
{$IFDEF LHCrypt}
        if LH.PC4_P <> 0 then LHCrypt(LH, LH.CodePos);
{$ENDIF}
        if LH.Write(LH.Code, LH.CodePos) <> LH.CodePos then
          LH.State := LH_ErrWrite;
        LH.CodePos := 0;
      end;
    end;
    Result := LH.State >= LH_Ready;
  end;

  procedure LHWriteCode(LH: PLHData; Value, Bits: Integer);
{$IFDEF UseASM}
  asm
         PUSH  EBX
         PUSH  EDI
         MOV   EBX,EAX
         MOV   EDI,ECX
         MOV   EAX,[EBX].TLHData.CodeBits
         MOV   ECX,[EBX].TLHData.CodeBitsCount

@@1:     SHR   EDX,1
         RCR   EAX,1
         DEC   ECX
         JZ    @@3
@@2:     DEC   EDI
         JNZ   @@1
         MOV   [EBX].TLHData.CodeBits,EAX
         MOV   [EBX].TLHData.CodeBitsCount,ECX

         POP   EDI
         POP   EBX
         RET

@@3:     PUSH  EDX
         MOV   [EBX].TLHData.CodeBits,EAX
         MOV   EAX,EBX
         CALL  LHWrite
         MOV   ECX,[EBX].TLHData.CodeBitsCount
         POP   EDX
         JMP   @@2
  end;
{$ELSE}
  begin
    while Bits > 0 do
    begin
      LH.CodeBits := (LH.CodeBits shr 1) or (Value and 1) shl (LH_CodeBits -1);
      Value := Value shr 1;
      Dec(LH.CodeBitsCount);
      if (LH.CodeBitsCount = 0) and not LHWrite(LH) then Exit;
      Dec(Bits);
    end;
  end;
{$ENDIF}

  procedure LHCompress(LH: PLHData; Code: Integer);
  var
    A,S,T: Integer;
    K: array[0..63] of Boolean;
  begin
    S := 0;
    A := Code + LH.RangeMax;
    repeat
      T := LH.Parent[A];
      K[S] := LH.Right[T] = A;
      A := T;
      Inc(S);
    until A = LH_Root;
    repeat
      Dec(S);
      LH.CodeBits := LH.CodeBits shr 1 or Byte(K[S]) shl (LH_CodeBits -1);
      Dec(LH.CodeBitsCount);
      if (LH.CodeBitsCount = 0) and not LHWrite(LH) then Exit;
    until S = 0;
    LHUpdateModel(LH, Code);
  end;

  procedure LHUpdateRange(LH: PLHData);
  begin
    if LH.RangeCopy < LH_CopyRanges then
    begin
      LHCompress(LH, LH_Special);
      LHWriteCode(LH, LH_SpecialINC, LH_SpecialBITS);
      Inc(LH.RangeCopy);
      LH.RangeMax  := LH_FirstCode + (LH.RangeCopy * LH_CodesPerRange);
      LH.RangeDist := LH.Range[LH.RangeCopy];
    end;
  end;

  procedure LHFlush(LH: PLHData);
  var
    I: Integer;
  begin
    if LH.CodeBitsCount > 0 then
    begin
      PInteger(@LH.Code[LH.CodePos])^ := LH.CodeBits shr LH.CodeBitsCount;
      I := (LH_CodeBits + 7 - LH.CodeBitsCount) div 8;
      Inc(LH.CodePos, I);
      Inc(LH.CodeBytes, I);
      LH.CodeBitsCount := LH_CodeBits;
      LH.CodeBits := 0;
    end;
    if LH.CodePos > 0 then
    begin
{$IFDEF LHCrypt}
      if LH.PC4_P <> 0 then LHCrypt(LH, LH.CodePos);
{$ENDIF}
      if LH.Write(LH.Code, LH.CodePos) <> LH.CodePos then
        LH.State := LH_ErrWrite;
    end;
  end;

  function LHRead(LH: PLHData): Boolean;
  var
    I: Integer;
  begin
    LH.DataPos := 0;
    I := SizeOf(LH.Data);
    if (LH.InputSize >= 0) and (LH.InputSize < LH.DataSize) then I := LH.InputSize;
    if I > 0 then LH.DataSize := LH.Read(LH.Data, I)
      else LH.DataSize := I;
    if LH.DataSize = 0 then LH.State := LH_Finish else
      if LH.DataSize < 0 then LH.State := LH_ErrRead else
      begin
        if LH.InputSize > 0 then Dec(LH.InputSize, LH.DataSize);
        LH.CRC := LHUpdateCRC(LH, LH.Data, LH.DataSize);
      end;
    Result := LH.State >= LH_Ready;
  end;

var
  I, C: Integer;
label
  Skip, Huffman, Finish;
begin
  if LH.State = LH_Init then
  begin
    LHInitCRC(LH);
    LHInitLZSS(LH);
    LHInitHuffman(LH);

    LH.RangeCopy  := 12;
    LH.RangeMax   := LH_FirstCode + (LH.RangeCopy * LH_CodesPerRange);
    LH.RangeDist  := LH.Range[LH.RangeCopy];

    if not LHRead(LH) or (LH.DataSize <= 0) then
    begin
      LH.State := LH_ErrRead;
      Exit;
    end;
    LH.State := LH_Working;

{$IFDEF LHCrypt}
    if LH.PC4_P <> 0 then
    begin
      I := RandSeed; Randomize; C := Random(MaxInt); RandSeed := I;
      for I := 0 to LH.DataSize -1 do
        C := (C + LH.Data[I] * 257) mod MaxInt +1;
      LHWriteCode(LH, 1, 1);
      LHWriteCode(LH, C, 8);
      LHWriteCode(LH, LH.PC4_P xor C, 8);
    end else LHWriteCode(LH, 0, 1);
{$ELSE}
    LHWriteCode(LH, 0, 1);
{$ENDIF}
  { Compress first few characters using Huffman }
    for I := 0 to LH_MinCopy -1 do
    begin
      C := LH.Data[LH.DataPos];
      Inc(LH.DataPos);
      LHCompress(LH, C);
      Inc(LH.DataBytes);
      LH.Text[I] := C;
      if LH.DataPos >= LH.DataSize then
      begin
        if not LHRead(LH) then Exit;
        if LH.State = LH_Finish then goto Finish;
      end;
    end;

  { Preload next few characters into lookahead buffer }
    if LH.State = LH_Working then
      for I := 0 to LH_MaxCopy -1 do
      begin
        C := LH.Data[LH.DataPos];
        Inc(LH.DataPos);
        LH.Text[LH.TextPos] := C;
        if LH.TextPos <= LH_MaxCopy then LH.Text[LH_MaxSize + LH.TextPos] := C;
        Inc(LH.TextPos);
        Inc(LH.DataBytes);
        if LH.DataPos >= LH.DataSize then
        begin
          if not LHRead(LH) then Exit;
          if LH.State = LH_Finish then Break;
        end;
      end;
    if (LH.Mode and LH_Binary <> 0) or (LH.Mode and LH_TypeMask <> LH_Text) then
    begin
      C := 0;
      for I := 0 to LH_MaxCopy + LH_MinCopy do
        if LH.Text[I] > 0 then Inc(C);
      if C > 2 then LH.Mode := LH.Mode or LH_ModeBIN;
    end;
    if LH.Mode and LH_ModeMask = LH_Max then
    begin
      LH.SearchMax   := MaxInt;
      LH.SearchDepth := MaxInt;
    end else
    begin
      if LH.Mode and LH_ModeMask = LH_Auto then
      begin
        LH.SearchMax   := LH_Normal * 4;
        LH.SearchDepth := LH_Normal * 2;
      end else
      begin
        LH.SearchMax   := (LH.Mode and LH_ModeMask -1) * 4 +2;
        LH.SearchDepth := (LH.Mode and LH_ModeMask -1) * 2;
      end;
      if LH.Mode and LH_ModeBIN = 0 then
      begin
        LH.SearchMax   := LH.SearchMax * 3;
        LH.SearchDepth := LH.SearchDepth * 2;
      end;
    end;
    if LH.Mode and LH_Huffman <> 0 then
      LH.Mode := LH.Mode or LH_ModeHuff;
  end else
    if (LH.State = LH_Working) and (LH.DataSize = 0) and not LHRead(LH) then Exit;

  if LH.State < LH_Working then Exit;

  repeat
    { Update nodes in hash table lists }
    if LH.Mode and LH_ModeHuff <> 0 then goto Huffman;

    if LH.Flag and LH_Full <> 0 then LHDeleteNode(LH, LH.TextPos);
    LHInsertNode(LH, LH.NewPos);
    if LH.Flag and LH_Found <> 0 then
    begin
      Dec(LH.TextLen);
      if LH.TextLen = 1 then
        LH.Flag := LH.Flag and not LH_Found;
    end else
    begin
      LH.TextLen := LHMatch(LH, False);
      if LH.TextLen >= LH_MinCopy then
      begin
        C := LHMatch(LH, True);
        if LH.TextLen >= C then
        begin
          if LH.Distance >= LH.RangeDist then
          begin
            LHUpdateRange(LH);
            if LH.Distance >= LH.RangeDist then goto Huffman;
          end;
          for C := 0 to LH.RangeCopy -1 do
            if LH.Distance < LH.Range[C +1] then
            begin
              LH.Flag := LH.Flag or LH_Found;
              LHCompress(LH, LH.TextLen - LH_MinCopy + LH_FirstCode + C * LH_CodesPerRange);
              LHWriteCode(LH, LH.Distance - LH.Range[C], C +1);
              if LH.State < LH_Ready then Exit
                else goto Skip;
            end;
        end;
      end;

Huffman:
      LHCompress(LH, LH.Text[LH.NewPos]);
    end;
Skip:

  { Advance buffer pointers }
    Inc(LH.NewPos); if LH.NewPos = LH_MaxSize then LH.NewPos := 0;
    Inc(LH.CurPos); if LH.CurPos = LH_MaxSize then LH.CurPos := 0;

  { Add next input character to buffer }
    if LH.DataSize > 0 then
    begin
      C := LH.Data[LH.DataPos];
      Inc(LH.DataPos);
      if (LH.DataPos >= LH.DataSize) and not LHRead(LH) then Exit;
      LH.Text[LH.TextPos] := C;
      if LH.TextPos <= LH_MaxCopy then LH.Text[LH_MaxSize + LH.TextPos] := C;
      Inc(LH.TextPos);

      if LH.TextPos = LH_MaxSize then
      begin
        LH.TextPos := 0;
        LH.Flag := LH.Flag or LH_Full;
      end;
      Inc(LH.DataBytes);
    end else
      if LH.State = LH_Finish then
      begin
        if LH.NewPos = LH.TextPos then
        begin
Finish:
          LHCompress(LH, LH_Special);
          LHWriteCode(LH, LH_SpecialCRC, LH_SpecialBITS);
          LHWriteCode(LH, not LH.CRC, 32);
          LHFlush(LH);
          LH.State := LH_Ready;
          Break;
        end;
      end else Break;
  until LH.State < LH_Ready;
end;

function LHEncode(const Password: AnsiString; ReadProc: TReadProc; WriteProc: TWriteProc; Size, Mode: Integer): Integer;
var
  LH: PLHData;
begin
  try
    GetMem(LH, SizeOf(TLHData));
  except
    Result := LH_ErrAlloc;
    Exit;
  end;
  try
    LH.State := LH_Init;
    LH.Mode := Mode;
    LH.Read := ReadProc;
    LH.Write := WriteProc;
    LH.InputSize := Size;
{$IFDEF LHCrypt}
    LHInitCrypt(LH, Password);
{$ENDIF}
    LHDeflate(LH);
  finally
    Result := LH.State;
    if Result >= LH_Ready then Result := LH.CodeBytes;
    LHFill(LH, SizeOf(TLHData));
    ReallocMem(LH, 0);
  end;
end;
{$ENDIF}

{$IFDEF LHDecode}
procedure LHInflate(LH: PLHData);
const
  LH_First = 4;

{$IFDEF LHCrypt}
  procedure LHCrypt(LH: PLHData; Size: Integer);
  var
    S,F: Byte;
    B: PByte;
  begin
    B := @LH.Code;
    if LH.Flag and LH_First = 0 then
    begin
      LH.Flag := LH.Flag or LH_First;
      if B^ and 1 = 0 then // test if data are encryted
      begin
        LH.PC4_P := 0;     // no, deactivate encryption
        LHFill(@LH.PC4_T, SizeOf(LH.PC4_T));
        Exit;
      end;
      Inc(B);
      Dec(Size);
    end;
    while Size > 0 do
    begin
      Dec(Size);
      Inc(LH.PC4_I);
      S := LH.PC4_T[LH.PC4_I];
      Inc(LH.PC4_J, S);
      LH.PC4_T[LH.PC4_I] := LH.PC4_T[LH.PC4_J] xor LH.PC4_F;
      LH.PC4_T[LH.PC4_J] := S - LH.PC4_F;
      F := B^;
      B^ := B^ xor LH.PC4_T[(LH.PC4_T[LH.PC4_I] + S) and $FF] - LH.PC4_F;
      LH.PC4_F := F;
      Inc(B);
    end;
  end;
{$ENDIF}

  function LHRead(LH: PLHData): Integer;
  var
    I: Integer;
  begin
    if LH.CodePos >= LH.CodeSize then
    begin
      LH.CodePos := 0;
      LH.CodeSize := SizeOf(LH.Code);
      if (LH.InputSize > 0) and (LH.CodeSize > LH.InputSize) then
        LH.CodeSize := LH.InputSize;
      if LH.CodeSize > 0 then
        LH.CodeSize := LH.Read(LH.Code, LH.CodeSize);
      if LH.CodeSize = 0 then LH.State := LH_Finish else
        if LH.CodeSize < 0 then
        begin
          LH.State := LH_ErrRead;
          Result := LH.State;
          Exit;
        end else
        begin
          if LH.InputSize > 0 then Dec(LH.InputSize, LH.CodeSize);
          I := LH.CodeSize;
          while I mod 4 <> 0 do
          begin
            LH.Code[I] := 0;
            Inc(I);
          end;
{$IFDEF LHCrypt}
          if LH.PC4_P <> 0 then LHCrypt(LH, LH.CodeSize);
{$ENDIF}
        end;
    end;
    LH.CodeBits := PInteger(@LH.Code[LH.CodePos])^;
    Inc(LH.CodePos, SizeOf(LH.CodeBits));
    Inc(LH.CodeBytes, SizeOf(LH.CodeBits));
    LH.CodeBitsCount := LH_CodeBits;
    Result := LH.State;
  end;

  function LHReadCode(LH: PLHData; Bits: Integer): Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 0 to Bits -1 do
    begin
      if (LH.CodeBitsCount = 0) and (LHRead(LH) < LH_Ready) then Exit;
      Dec(LH.CodeBitsCount);
      Result := Result or (LH.CodeBits and 1) shl I;
      LH.CodeBits := LH.CodeBits shr 1;
    end;
  end;

  function LHUncompress(LH: PLHData): Integer;
  begin
    Result := LH_Root;
    repeat
      if (LH.CodeBitsCount = 0) and (LHRead(LH) < LH_Ready) then Exit;
      Dec(LH.CodeBitsCount);
      if LH.CodeBits and 1 <> 0 then Result := LH.Right[Result]
        else Result := LH.Left[Result];
      LH.CodeBits := LH.CodeBits shr 1;
    until Result >= LH.RangeMax;
    Dec(Result, LH.RangeMax);
    LHUpdateModel(LH, Result);
  end;

  function LHWrite(LH: PLHData): Boolean;
  begin
    LH.DataSize := LH.Write(LH.Data, LH.DataPos);
    if LH.DataSize = LH.DataPos then LH.CRC := LHUpdateCRC(LH, LH.Data, LH.DataSize)
      else LH.State := LH_ErrWrite;
    LH.DataPos := 0;
    Result := LH.State >= LH_Ready;
  end;

var
  C, L, I: Integer;
begin
  if LH.State = LH_Init then
  begin
    LHFill(@LH.TextPos, 10 * 4);
    LH.State := LH_Working;

    LH.RangeCopy  := 12;
    LH.RangeMax  := LH_FirstCode + (LH.RangeCopy * LH_CodesPerRange);

    LHInitCRC(LH);
    LHInitHuffman(LH);
    C := LHReadCode(LH, 1);
    if C <> 0 then
{$IFDEF LHCrypt}
      if LH.PC4_P <> 0 then
      begin
        C := LHReadCode(LH, 16);
        if C shr 8 xor C and $FF <> LH.PC4_P and $FF then
        begin
          LH.State := LH_ErrPassword;
          Exit;
        end;
      end else
      begin
        LH.State := LH_ErrProtected;
        Exit;
      end;
{$ELSE}
      begin
        LH.State := LH_ErrProtected;
        Exit;
      end;
{$ENDIF}
  end;

  if LH.State < LH_Working then Exit else
    if LH.State = LH_Working then C := LHUncompress(LH)
      else C := 0;

  while LH.State = LH_Working do
  begin
    if C < LH_Special then
    begin
      LH.Data[LH.DataPos] := C;
      Inc(LH.DataPos);
      if (LH.DataPos >= SizeOf(LH.Data)) and not LHWrite(LH) then Exit;
      Inc(LH.DataBytes);
      LH.Text[LH.TextPos] := C;
      Inc(LH.TextPos); if LH.TextPos >= LH_MaxSize then LH.TextPos := 0;
    end else
      if C >= LH_FirstCode then
      begin
        Dec(C, LH_FirstCode);

        I := C div LH_CodesPerRange;
        L := C mod LH_CodesPerRange + LH_MinCopy;
        C := LH.TextPos - (LHReadCode(LH, I +1) + L + LH.Range[I]);

        if C < 0 then Inc(C, LH_MaxSize);
        if (C < 0) or (C >= LH_MaxSize) then LH.State := LH_ErrInflate;
        if LH.State < LH_Ready then Exit;

        repeat
          LH.Data[LH.DataPos] := LH.Text[C];
          Inc(LH.DataPos);
          if (LH.DataPos >= SizeOf(LH.Data)) and not LHWrite(LH) then Exit;
          LH.Text[LH.TextPos] := LH.Text[C];
          Inc(LH.TextPos); if LH.TextPos >= LH_MaxSize then LH.TextPos := 0;
          Inc(C); if C >= LH_MaxSize then C := 0;
          Inc(LH.DataBytes);
          Dec(L);
        until L = 0;
      end else
      begin
        C := LHReadCode(LH, LH_SpecialBITS);
        case C of
          LH_SpecialINC:
            if LH.RangeCopy < LH_CopyRanges then
            begin
              Inc(LH.RangeCopy);
              LH.RangeMax := LH_FirstCode + (LH.RangeCopy * LH_CodesPerRange);
            end else
            begin
              LH.State := LH_ErrInflate;
              Exit;
            end;
          LH_SpecialEOF:
            begin
              LH.State := LH_Finish;
              Break;
            end;
          LH_SpecialCRC:
            if not LHReadCode(LH, 32) <> LHUpdateCRC(LH, LH.Data, LH.DataPos) then
            begin
              LH.State := LH_ErrCRC;
              Exit;
            end else
            begin
              LH.State := LH_Finish;
              Break;
            end;
        else
          begin
            LH.State := LH_ErrInflate;
            Exit;
          end;
        end;
      end;
    C := LHUncompress(LH);
  end;

  if LH.State = LH_Finish then
  begin
    if (LH.DataPos > 0) and not LHWrite(LH) then Exit;
    if LH.State > LH_Ready then LH.State := LH_Ready;
  end;
end;

function LHDecode(const Password: AnsiString; ReadProc: TReadProc; WriteProc: TWriteProc; Size: Integer): Integer;
var
  LH: PLHData;
begin
  try
    GetMem(LH, SizeOf(TLHData));
  except
    Result := LH_ErrAlloc;
    Exit;
  end;
  try
    LH.State := LH_Init;
    LH.Read := ReadProc;
    LH.Write := WriteProc;
    LH.InputSize := Size;
{$IFDEF LHCrypt}
    LHInitCrypt(LH, Password);
{$ENDIF}
    LHInflate(LH);
  finally
    Result := LH.State;
    if Result >= LH_Ready then Result := LH.DataBytes;
    LHFill(LH, SizeOf(TLHData));
    ReallocMem(LH, 0);
  end;
end;
{$ENDIF}

// internal used in Buffer En/Decoding
type
  PLHCallbackRec = ^TLHCallbackRec;
  TLHCallbackRec = packed record
    Buffer: PWideChar;
    BufferSize: Integer;
    Data: PWideChar;
    DataSize: Integer;
  end;

  TMethod = record
    Code, Data: Pointer;
  end;

function LHGetRead(R: PLHCallbackRec): TReadProc;

  function DoRead(R: PLHCallbackRec; var Buffer; Count: Integer): Integer; register;
  begin
    if Count > R.BufferSize then
      Count := R.BufferSize;
    Move(R.Buffer^, Buffer, Count);
    Inc(R.Buffer, Count);
    Dec(R.BufferSize, Count);
    Result := Count;
  end;

begin
  TMethod(Result).Data := R;
  TMethod(Result).Code := @DoRead;
end;

function LHGetWrite(R: PLHCallbackRec): TWriteProc;

  function DoWrite(R: PLHCallbackRec; const Buffer; Count: Integer): Integer; register;
  begin
    ReallocMem(R.Data, R.DataSize + Count);
    Move(Buffer, R.Data[R.DataSize], Count);
    Inc(R.DataSize, Count);
    Result := Count;
  end;

begin
  TMethod(Result).Data := R;
  TMethod(Result).Code := @DoWrite;
end;

{$IFDEF LHEncode}
function LHEncodeBuffer(const Password: AnsiString; const Buffer; BufferSize: Integer; out Data: Pointer): Integer;
var
  R: TLHCallbackRec;
begin
  Data := nil;
  R.Buffer := @Buffer;
  R.BufferSize := BufferSize;
  R.Data := nil;
  R.DataSize := 0;
  try
    Result := LHEncode(Password, LHGetRead(@R), LHGetWrite(@R), BufferSize, LH_Max);
    if Result >= LH_Ready then
    begin
      Data := R.Data;
      Result := R.DataSize;
    end;
  except
    Result := LH_ErrGeneric;
    ReallocMem(R.Data, 0);
  end;
end;
{$ENDIF}

{$IFDEF LHDecode}
function LHDecodeBuffer(const Password: AnsiString; const Buffer; BufferSize: Integer; out Data: Pointer): Integer;
var
  R: TLHCallbackRec;
begin
  Data := nil;
  R.Buffer := @Buffer;
  R.BufferSize := BufferSize;
  R.Data := nil;
  R.DataSize := 0;
  try
    Result := LHDecode(Password, LHGetRead(@R), LHGetWrite(@R), BufferSize);
    if Result >= LH_Ready then
    begin
      Data := R.Data;
      Result := R.DataSize;
    end;
  except
    Result := LH_ErrGeneric;
    ReallocMem(R.Data, 0);
  end;
end;
{$ENDIF}

function LHCheck(Code: Integer): Integer;
resourcestring
  sLHSZUnspecific = 'Error in LHSZ library';
  sLHSZAlloc      = 'Error in LHSZ memory allocation';
  sLHSZInit       = 'Error in LHSZ initialization';
  sLHSZRead       = 'Read Error in LHSZ library';
  sLHSZWrite      = 'Write Error in LHSZ library';
  sLHSZInflate    = 'Infalte Error in LHSZ library';
  sLHSZWrongCRC   = 'Checksum Error in LHSZ library';
  sLHSZPassword   = 'Wrong Password in LHSZ library';
  sLHSZProtected  = 'LHSZ data is password protected';

const
  sError: array[-9..-1] of PResStringRec =
    (@sLHSZProtected, @sLHSZPassword, @sLHSZWrongCRC, @sLHSZInflate,
     @sLHSZWrite, @sLHSZRead, @sLHSZInit, @sLHSZAlloc, @sLHSZUnspecific);

begin
  if Code < LH_Ready then
  begin
    if Code < LH_ErrProtected then Code := LH_ErrGeneric;
    raise Exception.Create(LoadResString(sError[Code]));
  end else Result := Code;
end;

end.
