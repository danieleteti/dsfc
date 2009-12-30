{Copyright:      Hagen Reddmann  HaReddmann at T-Online dot de
 Author:         Hagen Reddmann
 Remarks:        public domain
 known Problems: none
 Version:        5.1, Delphi 5, 9.3.2002
 Description:    different Algorithms to compute the Factorial N! to arbitary value
                 and other usefull combinatoric algorithm

 Comments:       Thanks to Peter Luschny
                 Some factorial functions are based on the JAVA Sources at
                 http://www.luschny.de/math/factorial/FastFactorialFunctions.htm

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
unit NCombi;

interface

uses NInts;

procedure NFactorial_Moessner(var A: IInteger; N: Cardinal);
procedure NFactorial_Naive(var A: IInteger; N: Cardinal);
procedure NFactorial_Recursive(var A: IInteger; N: Cardinal);
procedure NFactorial_DivideAndConquer(var A: IInteger; N: Cardinal);
procedure NFactorial_Binomial(var A: IInteger; N: Cardinal);
procedure NFactorial_Jason_GMP(var A: IInteger; N: Cardinal);

procedure NFactorial(var A: IInteger; N: Cardinal; const M: IInteger = nil); overload;
procedure NComporial(var A: IInteger; N: Cardinal; const M: IInteger = nil); overload;
procedure NBinomial(var A: IInteger; N,K: Cardinal; const M: IInteger = nil); overload;
procedure NProduct(var A: IInteger; N,K: Cardinal; const M: IInteger = nil); overload;
procedure NPermutation(var A: IInteger; N,K: Cardinal; const M: IInteger = nil); overload;
function  NOddFactorial(var A: IInteger; N: Cardinal; const M: IInteger = nil): Cardinal; overload;
procedure NPrimorial(var A: IInteger; K,N: Cardinal; const M: IInteger = nil); overload;

procedure NHalfFactorial(var A: IInteger; N: Cardinal; const M: IInteger = nil); overload;
procedure NHalfComporial(var A: IInteger; N: Cardinal; const M: IInteger = nil); overload;
procedure NHalfPrimorial(var A: IInteger; N: Cardinal; const M: IInteger = nil); overload;
procedure NHalfBinomial(var A: IInteger; N: Cardinal; const M: IInteger = nil); overload;

function  NFactorialTrailingZeros(const N: Int64; Base: Cardinal): Int64;

procedure NTestCombi(N: Cardinal = 200); overload;

// internal most used stuff
type
  TPowerTable = array of record
    B: Cardinal; // base
    E: Cardinal; // exponent
  end;

function NPowerTable(var T: TPowerTable; N: Cardinal; L: Cardinal = 0; K: Cardinal = 0): Cardinal; overload;
function NPowerTable(var A: IInteger; N: Cardinal; L: Cardinal = 0; K: Cardinal = 0;
                       Shift: Boolean = False; const M: IInteger = nil): Cardinal; overload;
function NPrd(var A: IInteger; const T: TPowerTable; E: Cardinal; const M: IInteger = nil): Boolean; overload;
function NPrd(var A: IInteger; const T: TPowerTable; const M: IInteger = nil): Boolean; overload;

implementation

uses SysUtils, Prime, NMath, Math;

// follow four different Factorial procedures, based on Peter Luschny JAVA source
procedure NFactorial_Moessner(var A: IInteger; N: Cardinal);
var
  S: array of IInteger;
  M,K,I: Cardinal;
begin
  SetLength(S, N +1);
  NSet(S[0], 1);
  for M := 1 to N do
    for K := M downto 1 do
      for I := 1 to K do
         NAdd(S[I], S[I -1]);
  NSwp(A, S[N]);
end;

procedure NFactorial_Naive(var A: IInteger; N: Cardinal);
var
  I: Cardinal;
begin
  NSet(A, 1);
  for I := 2 to N do
    NMul(A, I);
end;

procedure NFactorial_Recursive(var A: IInteger; N: Cardinal);

  procedure NSplit(var P: IInteger; var K: Cardinal; N: Cardinal);
  var
    M: Cardinal;
    Q: IInteger;
  begin
    M := N shr 1;
    if M = 0 then
    begin
      NSet(P, K);
      Inc(K);
    end else
      if N = 2 then
      begin
        NSet(P, K);
        NMul(P, K +1);
        Inc(K, 2);
      end else
      begin
        NSplit(P, K, M);
        NSplit(Q, K, N - M);
        NMul(P, Q);
      end;
  end;

var
  K: Cardinal;
begin
  K := 1;
  if N < 2 then NSet(A, 1)
    else NSplit(A, K, N);
end;

procedure NFactorial_DivideAndConquer(var A: IInteger; N: Cardinal);
var
  BreakEven: Cardinal;
  
  procedure NSplit(var P: IInteger; var K: Cardinal; N: Cardinal);
  var
    M: Cardinal;
    Q: IInteger;
  begin
    if N < BreakEven then
    begin
      NSet(P, K);
      Inc(K, 2);
      while N > 1 do
      begin
        NMul(P, K);
        Inc(K, 2);
        Dec(N);
      end;
    end else
    begin
      M := N shr 1;
      NSplit(P, K, M);
      NSplit(Q, K, N - M);
      NMul(P, Q);
    end;
  end;

var
  I,J,K: Cardinal;
  L: Integer;
  Len: array of Cardinal;
  P,Q: IInteger;
begin
  if N >= 2 then
  begin
    BreakEven := NBreakEven(1);   // Karatsuba Multiplication BreakEven
    J := NLog2(N);
    SetLength(Len, J);
    Dec(J);
    K := N;
    for I := J downto 0 do
    begin
      L := K + K and 1 -1;
      K := K shr 1;
      Dec(L, K + K and 1 +1);
      if L >= 0 then
        Len[I] := 1 + L shr 1;
    end;
    K := 3;
    NSet(A, 1);
    NSet(Q, 1);
    for I := 0 to J do
      if Len[I] > 0 then
      begin
        NSplit(P, K, Len[I]);
        NMul(Q, P);
        NMul(A, Q);
      end;
    NShl(A, N - Cardinal(NBitWeight(N)));
  end else NSet(A, 1);
end;

procedure NFactorial_Binomial(var A: IInteger; N: Cardinal);
// my Method, this is equal fast as plain Schönhage and are the same group of algos.
var
  I: Integer;
  K: Cardinal;
  F,B: IInteger;
begin
  NSet(F, 1);
  I := NLog2(N);
  K := 1;
  while I > 0 do
  begin
    Dec(I);
    Inc(K, K + N shr I and 1);
    NPowerTable(B, K, K div 2, K div 2);  // OddBinomial(K, [K/2])
    NSqr(F);
    NMul(F, B);
  end;
  NShl(A, F, N - Cardinal(NBitWeight(N)));
end;

procedure NFactorial_Jason_GMP(var A: IInteger; N: Cardinal);
// Factorial contributed to GMP MP Project by Jason at http://217.35.81.229/

  procedure NOddProduct(Low, High: Cardinal; var S: IIntegerArray);

    procedure NSmallOddProduct(var A: IInteger; Start, Step, Count: Cardinal);
    // this procedure is very simplified compared to Jason's one,
    // because they runningtime is never important for big factorials
    // of course with my underlaying mul.
    begin
      NSet(A, Start);
      while Count > 1 do
      begin
        Dec(Count);
        Inc(Start, Step);
        NMul(A, Start);
      end;
    end;

  var
    A,Z,Y,N,Mask,Stc,Stn: Cardinal;
  begin
    Inc(Low);
    if not Odd(Low) then Inc(Low);
    if High = 0 then Inc(High) else
      if not Odd(High) then Dec(High);
    if High < Low then
    begin
      NSet(S[0], 1);
      Exit;
    end;
    if High = Low then
    begin
      NSet(S[0], Low);
      Exit;
    end;
    High := (High - Low) div 2 +1;
    if High <= 1 shl 5 then
    begin
      NSmallOddProduct(S[0], Low, 2, High);
      Exit;
    end;
    N := NLog2(High) - 5;
    Mask := 1 shl N;
    A := Mask + Mask;
    Dec(Mask);
    stn := 0;
    stc := 1;
    for Z := Mask downto 0 do
    begin
      Y := NBitSwap(Z) shr (32 - N);
      NSmallOddProduct(S[stn], Low + 2 * ((not Y) and Mask), A, (High + Y) shr N);
      Inc(Stn);
      Y := Stc;
      Inc(Stc);
      while not Odd(Y) do
      begin
        NMul(S[stn -2], S[stn -1]);
        Dec(stn);
        Y := Y shr 1;
      end;
    end;
  end;

var
  B: IInteger;
  S: IIntegerArray;
  I,J,Z: Integer;
begin
  SetLength(S, 33);
  Z := NLog2(N div 3) +1;
  NSet(A, 1);

{$IFDEF Old_Jason}
// in fact, because of the clever used asymmetric splitting methods in all
// my multiplication stuff like COMBA, Karatsuba, TOOM-3 and Schönhage FFT
// this old used method is equal fast as below code
  NSet(B, 1);
  for I := Z downto 1 do
  begin
    NOddProduct(N shr I, N shr (I -1), S);
    NMul(B, S[0]);
    NMul(A, B);
  end;
{$ELSE}

// Jason's proposed way with squaring by binary powering
  J := 16;
  while J > 0 do
  begin
    NSet(B, 1);
    I := 32 - J;
    while I >= J do
    begin
      if Z >= I then
      begin
        NOddProduct(N shr I, N shr (I -1), S);
        if I <> J then
          NPow(S[0], S[0], I div J);
        NMul(B, S[0]);
      end;
      Dec(I, 2 * J);
    end;
    if (Z >= J) and (J <> 1) then
    begin
      NMul(A, B);
      NSqr(A);
    end;
    J := J shr 1;
  end;
  NMul(A, B);
{$ENDIF}

  NShl(A, N - Cardinal(NBitWeight(N)));
end;

// follow four procedure are the essential of Schönhages Factorial trick

function NPowerTable(var T: TPowerTable; N: Cardinal; L: Cardinal; K: Cardinal): Cardinal;
// compute primepowers in T[] where ((N! / L! / K!) / 2^Result)

  function FindEP(K,L,N,P: Cardinal): Cardinal;
  // extract exponents to base P, where P is prime
  var
    E: Cardinal;
  begin
    E := 0;
    while K >= P do
    begin
      N := N div P;
      L := L div P;
      K := K div P;
      Inc(E, N - L - K);
    end;
    while L >= P do
    begin
      N := N div P;
      L := L div P;
      Inc(E, N - L);
    end;
    while N >= P do
    begin
      N := N div P;
      Inc(E, N);
    end;
    Result := E;
  end;

resourcestring
  sNPowerTable = 'NPowerTable(), requiere K <= N and L <= N and L + K <= N';
var
  P: Cardinal;
  E,I,C: Cardinal;
  S: TSmallPrimeSieve;
begin
  if K > L then
  begin
    I := K;
    K := L;
    L := I;
  end;
  if (K > N) or (L > N) or (L + K > N) then NRaise(@sNPowerTable);
// extract power to base 2
  Result := (N - L - K) - Cardinal(NBitWeight(N) - NBitWeight(L) - NBitWeight(K));
// extract powers to each prime <= N
  S := Primes;
  NAutoRelease(S);
  C := 0;
  I := 1;  // start with prime 3
  repeat
    Inc(I);
    P := S[I];
    if P > N then Break;
    E := FindEP(K, L, N, P);
    if E <> 0 then
    begin
      if C mod 2048 = 0 then SetLength(T, C + 2048);
      T[C].E := E;
      T[C].B := P;
      Inc(C);
    end;
  until False;
  SetLength(T, C);
end;

function NPowerTable(var A: IInteger; N: Cardinal; L: Cardinal; K: Cardinal; Shift: Boolean; const M: IInteger): Cardinal;
// A = ((N! / L! / K!) / 2^Result) mod M if M <> nil
// "divison" by 2^result only if shift = false
var
  T: TPowerTable;
  R: IInteger;
begin
  Result := NPowerTable(T, N, L, K);
  NPrd(R, T, M);
  if Shift then
  begin
    NShl(R, Result);
    if M <> nil then NMod(R, M);
  end;
  NSwp(R, A);
end;

function NPrd(var A: IInteger; const T: TPowerTable; E: Cardinal; const M: IInteger = nil): Boolean;
// compute products of T[] where only Bases multiplied they Exponents have the right Bit set
// if M <> nil all modulo M
// if result = false then A is normaly zero
// we use an iterative binary splitting
var
  I,J,K,L,N: Integer;
  BreakEven: Integer;
  P: IInteger;
  S: array[0..32] of IInteger; // 32 + 1 IInteger for max. range
begin
  BreakEven := NBreakEven(1) * 2;
  J := 0;
  K := 0;
  N := 0;
  NSet(P, 1);
  for I := Low(T) to High(T) do
    if (T[I].E and E <> 0) and (T[I].B > 0) then
    begin
      NMul(P, T[I].B);
      if M <> nil then NMod(P, M);
      if NSize(P, piLong) >= BreakEven then
      begin
     // now we exide our BreakEven, P is such big that multiplication with
     // Karatsuba, TOOM-COOK or FFT becomes faster
        NSwp(P, S[J]);
        Inc(J);
        Inc(K);
        L := K;
        while L and 1 = 0 do
        begin
          Dec(J);
          NMul(S[J -1], S[J]);
          if M <> nil then NMod(S[J -1], M);
          L := L shr 1;
        end;
        NSet(P, 1);
      end;
      Inc(N);              
    end;
  if M <> nil then NMod(P, M);
  NSwp(S[J], P);
  while J > 0 do
  begin
    NMul(S[J -1], S[J]);
    if M <> nil then NMod(S[J -1], M);
    Dec(J);
  end;
  NSwp(S[0], A);
  Result := N > 0;
end;

function NPrd(var A: IInteger; const T: TPowerTable; const M: IInteger): Boolean;
// A = T[], full multiply the powers
// if M <> nil all modulo M
// if result = false then A is normaly zero

{ we multiply all Bases and they Exponents in T[] by a binary Powering Method
 as Example we have 3^5 * 7^5 * 11^3 * 13^3 * 17^2 * 19 * 23 then we use the
 exponents binary expansion to get

 3^0101 * 7^0101 * 11^0011 * 13^0011 * 17^0010 * 19^0001 * 23^0001

 and use now each bit column from top down to get

  ([3*7]^2 * [11*13*17])^2 * [3*7*11*13*19*23]

 with large sets in T[] this way is one of the fastest, because the partial products
 are best weighted and we indroduce the faster Squaring Algorithm.
 In this Library we use for big integers the Schönhage Strassen modular FFT and
 then Squaring is about 1.5 times faster as Multiplication.
 The partial product like [11*13*17] are multiplied by above NPrd()
 that use an iterative binary splitting approach. 
}

var
  P,Q: IInteger;
  E: Cardinal;
  I: Integer;
begin
// first compute the higest exponent, could be surely more optimized when we
// assume some restrictions (but with no important speedup)
  E := 0;
  for I := Low(T) to High(T) do
    if (T[I].E > E) and (T[I].B <> 0) then
      E := T[I].E;
  Result := E > 0;
  if not Result then
  begin
    NSet(A, 1); // important set to 1 instead zero
    Exit;
  end;
  E := 1 shl NLog2(E);
  NPrd(P, T, E, M);
  while (E > 1) and (NSgn(P) <> 0) do
  begin
    E := E shr 1;
    NPrd(Q, T, E, M);
    NSqr(P);
    if M <> nil then NMod(P, M);
    NMul(P, Q); 
    if M <> nil then NMod(P, M);
  end;
  NSwp(P, A);
end;

procedure NFactorial(var A: IInteger; N: Cardinal; const M: IInteger);
// A = N!, if M <> nil then modulo M
{ as Example 100! =
  (((((3)^2 * 3*5*7)^2 * 5*11)^2 * 13*17*19*23)^2 * 13*29*31*37*41*43*47)^2 *
   11*13*17*19*29*31*53*59*61*67*71*73*79*83*89*97 * 2^97
}
begin
  NPowerTable(A, N, 0, 0, True, M);
end;

procedure NHalfFactorial(var A: IInteger; N: Cardinal; const M: IInteger);
// A = n! / [n/2]!, mod m if M <> nil
begin
  NPowerTable(A, N, N div 2, 0, True, M);
end;

function NOddFactorial(var A: IInteger; N: Cardinal; const M: IInteger): Cardinal;
// A = N! div 2^Result, if M <> nil then modulo M
begin
  Result := NPowerTable(A, N, 0, 0, False, M);
end;

function NFactorialTrailingZeros(const N: Int64; Base: Cardinal): Int64;
// compute count of trailing zeros to base of factorial N!

  function PrimePower(N: Int64; Prime: Cardinal): Int64;
  begin
    Result := 0;
    while N >= Prime do
    begin
      N := N div Prime;
      Inc(Result, N);
    end;
  end;

var
  I,Prime,BaseRoot,Multiple: Cardinal;
  Power: Int64;
begin
  if Base < 2 then
    raise Exception.Create('NFactorialTrailingZeros(), Base must be >= 2');
  if (N < 0) then
    raise Exception.Create('NFactorialTrailingZeros(), N must be >= 0');

  InitSmallPrimes;
  Result := $FFFFFFFF;
  BaseRoot := Trunc(Sqrt(Base));
  I := 0;
  repeat
    Prime := SmallPrimes[I];
    if Prime > BaseRoot then Break;
    Inc(I);
    Multiple := 0;
    while Base mod Prime = 0 do
    begin
      Base := Base div Prime;
      Inc(Multiple);
    end;
    if Multiple > 0 then
    begin
      Power := PrimePower(N, Prime) div Multiple;
      if Result > Power then Result := Power;
    end;
  until Base = 1;
  if Base > 1 then
  begin
    Power := PrimePower(N, Base);
    if Result > Power then Result := Power;
  end;
end;

procedure NBinomial(var A: IInteger; N,K: Cardinal; const M: IInteger);
//                n!
//A = n_C_k = ----------, mod M if M <> nil
//            k!(n - k)!
resourcestring
  sNBinomial = 'NBinomial(), invalid parameter K > N';
begin
  if K > N then NRaise(@sNBinomial) else
    if K or N = 0 then NSet(A, 1)
      else NPowerTable(A, N, N - K, K, True, M);
end;

procedure NHalfBinomial(var A: IInteger; N: Cardinal; const M: IInteger);
// A = n! / ([n/2]! (n-[n/2])!, mod M if M <> nil
begin
  NPowerTable(A, N, N - N div 2, N div 2, True, M);
end;

procedure NProduct(var A: IInteger; N,K: Cardinal; const M: IInteger);
// A = n! div k!, mod M if M <> nil
resourcestring
  sNProduct = 'NProduct(), invalid parameter K > N';
begin
  if K > N then NRaise(@sNProduct)
    else NPowerTable(A, N, K, 0, True, M);
end;

procedure NPermutation(var A: IInteger; N,K: Cardinal; const M: IInteger);
//                n!
//A = n_P_k = --------,  mod m if M <> nil
//            (n - k)!
resourcestring
  sNPermutation = 'NPermutation(), invalid parameter K > N';
begin
  if K > N then NRaise(@sNPermutation) else
    if K = 0 then NSet(A, 1)
      else NPowerTable(A, N, N - K, 0, True, M);
end;

procedure NReducePowers(var T: TPowerTable; K: Cardinal = 0);
// reduce exponents by 1 where they bases > k
var
  I: Integer;
begin
  I := Length(T);
  while I > 0 do
  begin
    Dec(I);
    if T[I].B > K then
    begin
      Dec(T[I].E);
      if T[I].E <> 0 then Break;
    end else Break;
  end;
  SetLength(T, I +1);
  while I > 0 do
  begin
    Dec(I);
    if T[I].B > K then Dec(T[I].E);
  end;
end;

procedure NComporial(var A: IInteger; N: Cardinal; const M: IInteger);
// A = N! div Pr(N), mod M if M <> nil, Pr() denote primorial
// demonstrate the individual use of the powertable
var
  S: Integer;
  T: TPowerTable;
  R: IInteger;
begin
  S := NPowerTable(T, N) -1;
// reduce all prime exponents
  NReducePowers(T);
  NPrd(R, T, M);
  if S > 0 then
  begin
    NShl(R, S);
    if M <> nil then NMod(R, M);
  end;
  NSwp(R, A);
end;

procedure NHalfComporial(var A: IInteger; N: Cardinal; const M: IInteger);
//A = n! / ([n/2]! Pr([n/2],n)), mod M if M <> nil
var
  S: Integer;
  T: TPowerTable;
  R: IInteger;
begin
  S := NPowerTable(T, N, N div 2);
  if N < 4 then Dec(S);
// reduce all prime exponents between [N/2] upto N
  NReducePowers(T, N div 2);
  NPrd(R, T, M);
  if S > 0 then
  begin
    NShl(R, S);
    if M <> nil then NMod(R, M);
  end;
  NSwp(R, A);
end;

procedure NPrimorial(var A: IInteger; K,N: Cardinal; const M: IInteger);
// A = product of all primes between K,N, mod M, if M <> nil
// K,N are any value, all primes between both values would be multiplied
// (N - K) / Ln(B) +- (N - K)^(1/2) ~ digits of P(N) to Base B

var
  BreakEven: Cardinal;
  L: TSmallPrimeSieve;

  procedure NSplit(var P: IInteger; S,E: Cardinal);
  var
    N: Cardinal;
    Q: IInteger;
  begin
    if E - S <= BreakEven then
    begin
      NSet(P, L[S]);
      while S < E do
      begin
        Inc(S);
        NMul(P, L[S]);
      end;
    end else
    begin
      N := (E + S) shr 1;
      NSplit(P, S, N);
      NSplit(Q, N+1, E);
      NMul(P, Q);
    end;
    if M <> nil then NMod(P, M);
  end;

resourcestring
  sNPrimorial = 'NPrimorial(), invalid Parameters N > Primes.MaxPrime';
var
  R: IInteger;
begin
  if N > TSmallPrimeSieve.MaxPrime then
    NRaise(@sNPrimorial);
  BreakEven := NBreakEven(1);    // Karatsuba Multiplication BreakEven
  L := Primes;
  NAutoRelease(L);
  if (N >= K) and (N > 1) then
  begin
    K := L.IndexOf(K);
    if K = 0 then K := 1;
    N := L.IndexOf(N, True);
    if K <= N then
    begin
      NSplit(R, K, N);
      NSwp(R, A);
    end else NSet(A, 1);
  end else NSet(A, 1);
end;

procedure NHalfPrimorial(var A: IInteger; N: Cardinal; const M: IInteger = nil);
begin
  NPrimorial(A, N div 2 +1, N, M);
end;

procedure NTestCombi(N: Cardinal);
resourcestring
  sNTest = 'Selftest failed at';
var
  I,K: Cardinal;
  A,B,C: IInteger;
begin
  for I := 0 to N do
  begin
    NFactorial(A, I);
    NFactorial_Moessner(B, I);
    if NCmp(A, B) <> 0 then NRaise(@sNTest, 'NFactorial_Moessner');
    NFactorial_Naive(A, I);
    if NCmp(A, B) <> 0 then NRaise(@sNTest, 'NFactorial_Naive');
    NFactorial_Recursive(A, I);
    if NCmp(A, B) <> 0 then NRaise(@sNTest, 'NFactorial_Recursive');
    NFactorial_DivideAndConquer(A, I);
    if NCmp(A, B) <> 0 then NRaise(@sNTest, 'NFactorial_DivideAndConquer');
    NFactorial_Jason_GMP(A, I);
    if NCmp(A, B) <> 0 then NRaise(@sNTest, 'NFactorial_Jason_GMP');
    NFactorial_Binomial(A, I);
    if NCmp(A, B) <> 0 then NRaise(@sNTest, 'NFactorial_Binomial');
  end;
  for I := 0 to N do
  begin
    NFactorial(A, I);
    NFactorial(B, I div 2);
    NHalfComporial(C, I);
    NMul(B, C);
    NHalfPrimorial(C, I);
    NMul(B, C);
    if NCmp(A, B) <> 0 then NRaise(@sNTest, 'NHalfComporial(), NHalfPrimorial(), NFactorial()');
  end;
  for I := 0 to N do
  begin
    NFactorial(A, I);
    NFactorial(B, I div 2);
    NHalfFactorial(C, I);
    NMul(B, C);
    if NCmp(A, B) <> 0 then NRaise(@sNTest, 'NHalfFactorial(), NFactorial()');
  end;
  for I := 0 to N do
    for K := 0 to I do
    begin
      NPermutation(A, I, K);
      NBinomial(B, I, K);
      NFactorial(C, K);
      NMul(B, C);
      if NCmp(A, B) <> 0 then NRaise(@sNTest, 'NPermutation(), NBinomial(), NFactorial()');
    end;
end;




end.
