{Copyright:      Hagen Reddmann  HaReddmann at T-Online dot de
 Author:         Hagen Reddmann
 Remarks:        this Copyright must be included
 known Problems: none
 Version:        5.1,  Part II from Delphi Encryption Compendium
                 Delphi 5

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

}
unit NInts;

{$I VER.INC}

interface

uses TypInfo, Classes, SysUtils, CRC, DECUtil, DECHash, DECFmt, NMath;

type
  TBase = type Word;// 0= Default or Parse, 2..64= valid StringFormats, 256= Byte copying

  TIntegerFormat = (ifASN1, ifDEC, ifCRC, ifPGP, ifPlain, ifFast);

  TNAFArray = array of Integer;

  TPiece = (piBit, piByte, piWord, piLong);

  Long96 = array[0..2] of Cardinal;

  IInteger = interface
    ['{126BE010-061D-4067-9E0A-E2A490AF5CEA}']
    function Count: Integer;
    function GetDigit(Index: Integer): Cardinal;

    property Digit[Index: Integer]: Cardinal read GetDigit; default;
  end;

  I2Point = packed record
    X,Y: IInteger;
  end;

  I3Point = packed record
    X,Y,Z: IInteger;
  end;

  IIntegerArray = array of IInteger;

  EInteger = class(EMath);

  TIIntegerPrimeCallback = function(const P: IInteger): Boolean; register;
  TIIntegerSortCompare = function(const A, B: IInteger): Integer; register;
  TIIntegerForEachCallback = function(var A: IInteger): Boolean; register;

  TIIntegerSplitData = packed record
    P,Q,A,B: IInteger;
  end;

  TIIntegerBinarySplittingCallback = procedure(N: Cardinal; var P: TIIntegerSplitData); register;

  TStrFormat = packed record           // String-Convertion structure
    Base: TBase;                       // Numberbase
    Plus: array[0..15] of Char;        // String for positive IInteger (+)
    Minus: array[0..15] of Char;       // String for negative IInteger (-)
    Zero: array[0..15] of Char;        // String for zero              (0)
    Comma: Char;
    DigitsPerBlock: Word;              // Digits on one Block
    BlockSep: array[0..15] of Char;    // separator between two blocks (Space)
    BlockPadding: Char;                // left padding char of first block
    DigitsPerLine: Word;               // count of digits in one line
    LineSep: array[0..15] of Char;     // separator after one line (CR+LF)
    LinePadding: Char;                 // left padding char of first line
    DigitsChars: array[0..63] of Char; // possible Digits of a valid Numberstring
    FormatChars: array[0..63] of Char; // Numberstrings can contain these chars, but should be ignored
    LeftAlign: Boolean;
    Offset: Integer;                   // Offset to first char that contains digits, NSet(string)
    Precision: Integer;
  end;

  TTrialDivision = (IsPrime, NotDivisible, Divisible);

  IPowModPrecomputation = interface
    ['{126BE018-061D-4067-9E0A-E2A490AF5CEA}']
    procedure Precompute(const B,M: IInteger; EMaxBitSize: Integer; EIsNeg: Boolean);
    function  PowMod(var A: IInteger; const B,E,M: IInteger; var Res: Boolean): Boolean;
    function  PowMod2k(var A: IInteger; const B,E: IInteger; K: Cardinal; var Res: Boolean): Boolean;
    procedure Save(Stream: TStream);
    procedure Load(Stream: TStream);
  end;

procedure NSet(var A: IInteger; B: Integer); overload;
procedure NSet(var A: IInteger; B: Int64); overload;
procedure NSet(var A: IInteger; B: Extended); overload;
procedure NSet(var A: IInteger; const B: IInteger = nil; Abs: Boolean = False); overload;
function  NSet(var A: IInteger; const B: String; const Format: TStrFormat): Integer; overload;
procedure NSet(var A: IInteger; const B: String; Base: TBase = 0); overload;
procedure NSet(var A: IInteger; const B; Size: Integer; Bits: Integer = 0); overload;
procedure NSet(var A: IInteger; Stream: TStream; Format: TIntegerFormat = ifASN1); overload;
procedure NSet(var A: IInteger; const B: TVarRec); overload;
procedure NSet(var A: IIntegerArray; const B: array of const); overload;
procedure NSet(var A: IIntegerArray; const B: IIntegerArray); overload;
procedure NRnd(var A: IInteger; Bits: Integer = 0; Sign: Boolean = False); overload;
function  NInt(A: Integer = 0): IInteger; overload;
function  NInt(A: Int64): IInteger; overload;
function  NInt(A: Extended): IInteger; overload;
function  NInt(const A: IInteger; Abs: Boolean = False): IInteger; overload;
function  NInt(const A; Size: Integer; Bits: Integer = 0): IInteger; overload;
function  NInt(const A: String; Base: TBase = 0): IInteger; overload;
function  NInt(Stream: TStream; Format: TIntegerFormat = ifASN1): IInteger; overload;
function  NInt(const A: array of const): IIntegerArray; overload;
function  NSgn(const A: IInteger; Extended: Boolean = False): Integer; overload;
procedure NSgn(var A: IInteger; Sign: Integer); overload;
function  NOdd(const A: IInteger): Boolean; overload;
function  NOdd(var A: IInteger; Odd: Boolean): Boolean; overload;
function  NNeg(var A: IInteger): Boolean; overload;
function  NNeg(var A: IInteger; Negative: Boolean): Boolean; overload;//??
function  NAbs(var A: IInteger): Boolean; overload;
function  NBit(const A: IInteger; Index: Integer): Boolean; overload;
procedure NBit(var A: IInteger; Index: Integer; Value: Boolean); overload;
function  NLow(const A: IInteger; Piece: TPiece = piBit): Integer; overload;
function  NHigh(const A: IInteger; Piece: TPiece = piBit): Integer; overload;
function  NSize(const A: IInteger; Piece: TPiece = piBit): Integer; overload;
function  NCmp(const A,B: IInteger; Abs: Boolean = False): Integer; overload;
function  NCmp(const A: IInteger; B: Integer; Abs: Boolean = False): Integer; overload;
function  NCmp(const A,B: IInteger; Bits: Integer; Abs: Boolean = False): Integer; overload;
function  NCRC(const A: IInteger; CRC: TCRCType = CRC_32CCITT): Cardinal; overload;
function  NCRC(const A: IIntegerArray; CRC: TCRCType = CRC_32CCITT): Cardinal; overload;
function  NParity(const A: IInteger): Boolean; overload;
function  NWeight(const A: IInteger): Integer; overload;
function  NBitPos(const A: IInteger; Bit: Integer): Integer; overload;
//procedure NBitAdd(var A: IInteger; const B: IInteger; Bits: Integer); overload;
procedure NSplit(var A: IInteger; const B: IInteger; Bits: Byte); overload;
procedure NSwp(var A,B: IInteger); overload;
procedure NSwp(var A: IInteger; Piece: TPiece; Normalize: Boolean = True); overload;
procedure NSwp(var A: IInteger; const B: IInteger; Piece: TPiece; Normalize: Boolean = True); overload;
procedure NCpy(var A: IInteger; Count: Integer; Start: Integer = 0); overload;
procedure NCpy(var A: IInteger; const B: IInteger; Count: Integer; Start: Integer = 0); overload;
procedure NShl(var A: IInteger; Shift: Integer); overload;
procedure NShl(var A: IInteger; const B: IInteger; Shift: Integer); overload;
procedure NShr(var A: IInteger; Shift: Integer); overload;
procedure NShr(var A: IInteger; const B: IInteger; Shift: Integer); overload;
function  NShr(var A: IInteger): Integer; overload;
function  NShr(var A: IInteger; const B: IInteger): Integer; overload;
procedure NCut(var A: IInteger; Bits: Integer); overload;
procedure NCut(var A: IInteger; const B: IInteger; Bits: Integer); overload;
procedure NCat(var A: IInteger; const B: IIntegerArray; Bits: Integer = 0);
procedure NNot(var A: IInteger; Bits: Integer = 0; Sign: Boolean = False); overload;
procedure NNot(var A: IInteger; const B: IInteger; Bits: Integer = 0; Sign: Boolean = False); overload;
procedure NXor(var A: IInteger; const B: IInteger; Bits: Integer = 0; Sign: Boolean = False); overload;
procedure NXor(var A: IInteger; const B,C: IInteger; Bits: Integer = 0; Sign: Boolean = False); overload;
procedure NAnd(var A: IInteger; const B: IInteger; Bits: Integer = 0; Sign: Boolean = False); overload;
procedure NAnd(var A: IInteger; const B,C: IInteger; Bits: Integer = 0; Sign: Boolean = False); overload;
procedure NOr (var A: IInteger; const B: IInteger; Bits: Integer = 0; Sign: Boolean = False); overload;
procedure NOr (var A: IInteger; const B,C: IInteger; Bits: Integer = 0; Sign: Boolean = False); overload;
procedure NCpl(var A: IInteger; Bits: Integer = 0; Sign: Boolean = False); overload;
procedure NCpl(var A: IInteger; const B: IInteger; Bits: Integer = 0; Sign: Boolean = False); overload;
procedure NInc(var A: IInteger; B: Cardinal = 1); overload;
procedure NDec(var A: IInteger; B: Cardinal = 1); overload;
procedure NAdd(var A: IInteger; B: Integer); overload;
procedure NAdd(var A: IInteger; const B: IInteger); overload;
procedure NAdd(var A: IInteger; const B,C: IInteger); overload;
procedure NSub(var A: IInteger; B: Integer); overload;
procedure NSub(var A: IInteger; const B: IInteger); overload;
procedure NSub(var A: IInteger; const B,C: IInteger); overload;
procedure NMul(var A: IInteger; B: Integer); overload;
procedure NMul(var A: IInteger; B: Int64); overload;
procedure NMul(var A: IInteger; const B: IInteger; C: Int64); overload;
procedure NMul(var A: IInteger; const B: IInteger; C: Integer); overload;
procedure NMul(var A: IInteger; const B: IInteger); overload;
procedure NMul(var A: IInteger; const B,C: IInteger); overload;
procedure NSqr(var A: IInteger); overload;
procedure NSqr(var A: IInteger; const B: IInteger); overload;
function  NMod(const A: IInteger; M: Integer): Integer; overload;
function  NModU(const A: IInteger; M: Cardinal): Cardinal; overload;
procedure NMod(var A: IInteger; const M: IInteger); overload;
procedure NMod(var A: IInteger; const B,M: IInteger); overload;
function  NRem(const A: IInteger; B: Integer): Integer; overload;
procedure NRem(var A: IInteger; const B: IInteger); overload;
procedure NRem(var A: IInteger; const B,C: IInteger); overload;
function  NDiv(var Q: IInteger; A: Integer): Integer; overload;
function  NDivU(var Q: IInteger; A: Cardinal): Cardinal; overload;
procedure NDiv(var Q: IInteger; const A: IInteger); overload;
function  NDiv(var Q: IInteger; const A: IInteger; B: Integer): Integer; overload;
function  NDivU(var Q: IInteger; const A: IInteger; B: Cardinal): Cardinal; overload;
procedure NDiv(var Q: IInteger; const A,B: IInteger); overload;
procedure NDivRem(var Q,R: IInteger; const A,B: IInteger); overload;
procedure NDivMod(var Q,R: IInteger; const A,B: IInteger); overload;
procedure NAddMod(var A: IInteger; const B,C,M: IInteger); overload;
procedure NAddMod(var A: IInteger; const B,M: IInteger); overload;
procedure NSubMod(var A: IInteger; const B,C,M: IInteger); overload;
procedure NSubMod(var A: IInteger; const B,M: IInteger); overload;
procedure NMulMod(var A: IInteger; const B,C,M: IInteger); overload;
procedure NMulMod(var A: IInteger; const B,M: IInteger); overload;
procedure NMulMod2k(var A: IInteger; const B,C: IInteger; K: Cardinal); overload;
procedure NMulMod2k(var A: IInteger; const B: IInteger; K: Cardinal); overload;
procedure NSqrMod(var A: IInteger; const B,M: IInteger); overload;
procedure NSqrMod(var A: IInteger; const M: IInteger); overload;
procedure NSqrMod2k(var A: IInteger; const B: IInteger; K: Cardinal); overload;
procedure NSqrMod2k(var A: IInteger; K: Cardinal); overload;
function  NInvMod(var A: IInteger; const M: IInteger): Boolean; overload;
function  NInvMod(var A: IInteger; const B,M: IInteger): Boolean; overload;
function  NInvMod(var A: IIntegerArray; const B: IIntegerArray; const M: IInteger; Inv2k: Cardinal = 0): Boolean; overload;
function  NInvMod(var A: IIntegerArray; const M: IInteger; Inv2k: Cardinal = 0): Boolean; overload;
function  NInvMod2k(var A: IInteger; K: Cardinal): Boolean; overload;
function  NInvMod2k(var A: IInteger; const B: IInteger; K: Cardinal): Boolean; overload;
function  NPowMod(var A: IInteger; const E,M: IInteger; const P: IPowModPrecomputation = nil): Boolean; overload;
function  NPowMod(var A: IInteger; const B,E,M: IInteger; const P: IPowModPrecomputation = nil): Boolean; overload;
function  NPowMod(var A: IInteger; const B,E: IIntegerArray; const M: IInteger): Boolean; overload;
function  NPowMod2k(var A: IInteger; const B,E: IInteger; K: Cardinal; const P: IPowModPrecomputation = nil): Boolean; overload;
function  NPowMod2k(var A: IInteger; const E: IInteger; K: Cardinal; const P: IPowModPrecomputation = nil): Boolean; overload;
procedure NPow(var A: IInteger; E: Integer); overload;
procedure NPow(var A: IInteger; B,E: Integer); overload;
procedure NPow(var A: IInteger; const B: IInteger; E: Integer); overload;
function  NGCD1(const A: IIntegerArray): Boolean; overload;
function  NGCD1(const A,B: IInteger): Boolean; overload;
function  NGCD(A, B: Integer): Integer; overload;
function  NGCD(var D: IInteger; const A,B: IInteger): Boolean; overload;
function  NGCD(var D: IInteger; const A: IIntegerArray): Boolean; overload;
function  NGCD(var D,U,V: IInteger; const A,B: IInteger): Boolean; overload;
procedure NLCM(var L: IInteger; const A,B: IInteger); overload;
function  NCRT(var A: IInteger; const R,M: IIntegerArray): Boolean; overload;
function  NCRT(var A: IInteger; const R,M,U: IIntegerArray): Boolean; overload;
function  NCRT(var U: IIntegerArray; const M: IIntegerArray): Boolean; overload;
function  NSqrt(var A: IInteger; const B: IInteger): Boolean; overload;  // 1.)
function  NSqrt(var A: IInteger): Boolean; overload;
function  NSqrt(var A,R: IInteger; const B: IInteger): Boolean; overload;
function  NSqrtMod2k(var A: IInteger; K: Cardinal): Boolean; overload;
function  NSqrtMod2k(var A: IInteger; const B: IInteger; K: Cardinal): Boolean; overload;
function  NSqrtMod(var A: IInteger; const P: IInteger; Check: Boolean = False): Integer; overload;
function  NRoot(var A,R: IInteger; const B: IInteger; E: Integer): Boolean; overload; // 1.)
function  NRoot(var A: IInteger; E: Integer): Boolean; overload;
function  NRoot(var A: IInteger; const B: IInteger; E: Integer): Boolean; overload;
function  NRootMod2k(var A: IInteger; const E: IInteger; K: Cardinal): Boolean; overload;
function  NRootMod2k(var A: IInteger; const B,E: IInteger; K: Cardinal): Boolean; overload;
function  NIsPerfectSqr(const A: IInteger; FullTest: Boolean = True): Boolean; overload;
function  NIsPerfectSqr(const N: Int64): Boolean; overload;
function  NIsPerfectPower(var B: IInteger; const N: IInteger; Bound: Cardinal = 0): Cardinal; overload; // 1.)
function  NIsPerfectPower(const N: IInteger; Bound: Cardinal = 0): Cardinal; overload;
function  NIsPower(const N: IInteger; B,E: Integer): Boolean; overload;
function  NIsPower(const N,B: IInteger; E: Integer): Boolean; overload;
function  NIsPower(const N,B,E: IInteger): Boolean; overload;
function  NIsPowerOf2(const A: IInteger): Integer; overload;
function  NIsDivisible(const A: IInteger; B: Cardinal): Boolean; overload;
function  NTrialDivision(const A: IInteger; Bound: Cardinal = $FFFF): TTrialDivision; overload;
function  NSmallFactor(const A: IInteger; Bound: Cardinal = 0): Cardinal; overload;
function  NSPP(const N: IInteger; const Bases: array of Integer): Boolean;
function  NIsProbablePrime(const A: IInteger): Boolean; overload;
function  NIsProbablePrime(const A: IInteger; const Bases: array of Integer): Boolean; overload;
function  NMakePrime(var P: IInteger; const Bases: array of Integer): Integer; overload;
function  NMakePrime(var P: IInteger; const Bases: array of Integer;
            Residue, Modulus: Integer; Callback: TIIntegerPrimeCallback = nil): Integer; overload;
function  NMakePrime(var P: IInteger; const Bases: array of Integer;
            const Residue, Modulus: IInteger; Callback: TIIntegerPrimeCallback = nil): Integer; overload;
procedure NLimLeePrime(var P: IInteger; var F: IIntegerArray; PBitSize: Integer; QBitSize: Integer = 0); overload;
procedure NLimLeePrime(var P,Q: IInteger; PBitSize: Integer; QBitSize: Integer = 0); overload;
function  NJacobi(A,B: Int64): Integer; overload;
function  NJacobi(A,B: Integer): Integer; overload;
function  NJacobi(A: Integer; const B: IInteger): Integer; overload;
function  NJacobi(const A: IInteger; B: Integer): Integer; overload;
function  NJacobi(const A,B: IInteger): Integer; overload;
procedure NLucas(var V: IInteger; const K: IInteger); overload;
procedure NLucas(var U,V: IInteger; const K: IInteger); overload;
procedure NLucas(var V: IInteger; const K,P,Q: IInteger); overload;
procedure NLucas(var U,V: IInteger; const K,P,Q: IInteger); overload;
procedure NLucasMod(var V: IInteger; const K,P,M: IInteger); overload;
procedure NLucasMod(var V: IInteger; const K,P,Q,M: IInteger); overload;
procedure NLucasMod(var U,V: IInteger; const K,P,Q,M: IInteger); overload;
function  NInvLucasMod(var A: IInteger; const K,N,P,Q: IInteger): Boolean; overload;
function  NInvLucasMod(var A: IInteger; const K,N,P,Q,U: IInteger): Boolean; overload;

procedure NFermat(var A: IInteger; N: Cardinal; const M: IInteger = nil); overload;
procedure NFibonacci(var R: IInteger; N: Cardinal; const M: IInteger = nil); overload;

function  NDigitCount(FromBase, ToBase: TBase; Digits: Cardinal): Cardinal; overload;
function  NDigitCount(const A: IInteger; Base: TBase = 10; Exactly: Boolean = True): Cardinal; overload;
function  NLn(const A: IInteger; Base: Cardinal = 1; ErrorCheck: Boolean = False): Extended; overload;
function  NDigit(const A: IInteger; Index: Integer; Piece: TPiece = piByte): Cardinal; overload;
procedure NDigit(var A: IInteger; Index: Integer; Value: Cardinal; Piece: TPiece = piByte); overload;
procedure NAF(var A: TNAFArray; const B: IInteger; W: Byte = 2); overload;
function  NStr(const A: IInteger; Base: TBase = 0): String; overload;
function  NStr(const A: IInteger; const Format: TStrFormat): String; overload;
function  NInt32(const A: IInteger; RangeCheck: Boolean = True): Integer; overload;
function  NInt64(const A: IInteger; RangeCheck: Boolean = True): Int64; overload;
function  NLong(const A: IInteger; RangeCheck: Boolean = True): Cardinal; overload;
function  NFloat(const A: IInteger; RangeCheck: Boolean = True): Extended; overload;
function  NRange(const A: IInteger; Range: PTypeInfo; RaiseError: Boolean = False): Boolean; overload;
procedure NSave(const A: IInteger; Stream: TStream; Format: TIntegerFormat = ifASN1); overload;
procedure NSave(const A: IInteger; const FileName: String; Format: TIntegerFormat = ifASN1); overload;
procedure NLoad(var R: IInteger; Stream: TStream; Format: TIntegerFormat = ifASN1); overload;
procedure NLoad(var R: IInteger; const FileName: String; Format: TIntegerFormat = ifASN1); overload;
procedure NHash(var A: IInteger; Hash: TDECHashClass = nil; Bits: Integer = 0; Index: Cardinal = 0); overload;
procedure NHash(var A: IInteger; const B: IInteger; Hash: TDECHashClass = nil; Bits: Integer = 0; Index: Cardinal = 0); overload;

function  NMont(const M: IInteger): Cardinal; overload;
procedure NMont(var A: IInteger; const M: IInteger; R: Cardinal); overload;
procedure NMont(var A: IInteger; const B,M: IInteger; R: Cardinal); overload;
procedure NMont(var A: IIntegerArray; const B: IIntegerArray; const M: IInteger; Inv2k: Cardinal = 0); overload;
procedure NMont(var A: IIntegerArray; const M: IInteger; Inv2k: Cardinal = 0); overload;
procedure NRedc(var A: IInteger; const M: IInteger; R: Cardinal); overload;
procedure NRedc(var A: IInteger; const B,M: IInteger; R: Cardinal); overload;
procedure NRedc(var A: IIntegerArray; const B: IIntegerArray; const M: IInteger; Inv2k: Cardinal = 0); overload;
procedure NRedc(var A: IIntegerArray; const M: IInteger; Inv2k: Cardinal = 0); overload;

procedure NSet(var P: IPowModPrecomputation; const B,M: IInteger; EMaxBitSize: Integer; EIsNeg: Boolean = False); overload;
procedure NSave(const P: IPowModPrecomputation; Stream: TStream); overload;
procedure NLoad(var P: IPowModPrecomputation; Stream: TStream); overload;

function  NSum(const A: IInteger): Long96; overload;
function  NMod(const A: Long96; B: Cardinal): Cardinal; overload;
procedure NSumModFactors(var Factors: IInteger; Bit: Integer); overload;
procedure NSumModModulis(var Modulis: IInteger; const Factors: IInteger); overload;
function  NInvMod2k32(A: Cardinal): Cardinal; overload; // A^-1 mod 2^32
function  NInvMod2k32(const A: IInteger; B: Cardinal; BInv: Cardinal = 0): Cardinal; overload;
function  NInvMul2k32(A,B: Cardinal; BInv: Cardinal = 0): Cardinal; overload;

procedure NInsert(var A: IInteger; B: Cardinal; Duplicates: Boolean = False); overload;
function  NFind(const A: IInteger; B: Cardinal; var Index: Integer): Boolean; overload;
function  NFind(const A: IInteger; B: Cardinal): Integer; overload;
procedure NSort(var A: IInteger); overload;
procedure NSort(var A: IIntegerArray; Compare: TIIntegerSortCompare); overload;
procedure NSort(var A: IIntegerArray; Abs: Boolean = False; Descending: Boolean = False); overload;
function  NForEach(const A: IIntegerArray; Callback: TIIntegerForEachCallback): IInteger; overload;
function  NBinarySplitting(var P,Q: IInteger; Count: Integer;
            Callback: TIIntegerBinarySplittingCallback; ImplicitShift: Boolean = True): Cardinal; overload;
function  NConfig(Flag: Cardinal = 3): Cardinal;

procedure NRaise(Msg: PResStringRec; const Param: String); overload;
procedure NRaise(Msg: PResStringRec; const Param: array of const); overload;
procedure NRaise(Msg: PResStringRec = nil); overload;
procedure NRaise_DivByZero; overload;

procedure NParseFormat(var F: TStrFormat; const B: String);
function  NLog2(A: Cardinal): Integer;
function  NBitWeight(A: Cardinal): Integer; overload;
function  NBitSwap(A: Cardinal): Cardinal; overload;
function  NGrayCodeTo(N: Cardinal): Cardinal; overload;
procedure NGrayCodeTo(var A: IInteger; const B: IInteger); overload;
function  NToGrayCode(N: Cardinal): Cardinal; overload;
procedure NToGrayCode(var A: IInteger; const B: IInteger); overload;

function  NGreenCodeTo(N: Cardinal): Cardinal; overload;
procedure NGreenCodeTo(var A: IInteger; const B: IInteger); overload;
function  NToGreenCode(N: Cardinal): Cardinal; overload;
procedure NToGreenCode(var A: IInteger; const B: IInteger); overload;

// predefined and fast constant
function  NNull: IInteger;      //  0
function  NOne: IInteger;       // +1
function  NMinusOne: IInteger;  // -1
function  NTwo: IInteger;       // +2
function  NMinusTwo: IInteger;  // -2

function  NBreakEven(Index: Integer): Cardinal;

// points
procedure NSwp(var A,B: I2Point); overload;
procedure NSwp(var A,B: I3Point); overload;
procedure NSet(var A: I2Point; const B: I2Point); overload;
procedure NSet(var A: I3Point; const B: I3Point); overload;

var
  NStrFormat: TStrFormat = (
    Base: 10;
    Plus: '';
    Minus: '-';
    Zero: '';
    Comma: ',';
    DigitsPerBlock: 5;
    BlockSep: ' ';
    BlockPadding: ' ';
    DigitsPerLine: 0;
    LineSep: #13#10;
    LinePadding: #0;
    DigitsChars: '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz+/';
    FormatChars: ' /\-+;:#~"()[]?_<>!§$%&{}'''#13#10#9;
    LeftAlign: False;
    Offset: 0;
    Precision: 0;
   );

function NNorm(const A: IInteger): Pointer; overload;
function NValid(const A: array of IInteger): Boolean; overload;
function NValid(const A: IIntegerArray): Boolean; overload;




