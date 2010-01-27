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
unit NInt_1;

interface

uses SysUtils, NInts, Console, NMath, ConsoleForm;

type
  TCFEFunc = function(Index: Integer): Integer; register;
  TIIntegerPIMethod = (piFastChudnovsky, piIterChudnovsky, piAGM, piFastMachin, piIterMachin);

procedure NCFE(var P,Q: IInteger; CFEFunc: TCFEFunc; Last: Integer);

function  CFE_Euler(Index: Integer): Integer;
function  CFE_GoldenRatio(Index: Integer): Integer;
function  CFE_Tan1(Index: Integer): Integer;

procedure NLn2(var R: IInteger);
procedure NLn10(var R: IInteger);
procedure NArcTan(var R: IInteger; const U,V: IInteger); overload;
procedure NArcTan(var R: IInteger; V: Integer); overload;
procedure NArcTanh(var R: IInteger; const V: IInteger);
procedure NSin(var R: IInteger; const U,V: IInteger);
procedure NSinh(var R: IInteger; const U,V: IInteger);
procedure NCos(var R: IInteger; const U,V: IInteger);
procedure NCosh(var R: IInteger; const U,V: IInteger);
procedure NTan(var R: IInteger; const U,V: IInteger);
procedure NTanh(var R: IInteger; const U,V: IInteger);
procedure NExp(var A: IInteger; U: Integer = 1; V: Integer = 1); overload;
procedure NExp(var A: IInteger; const U,V: IInteger); overload;
function  NPi(var A: IInteger; Decimals: Cardinal; Method: TIIntegerPIMethod = piFastChudnovsky): Cardinal;

procedure NFactorial1(var A: IInteger; N: Cardinal);

implementation

procedure NCFE(var P,Q: IInteger; CFEFunc: TCFEFunc; Last: Integer);
// continued fraction expansion
var
  I: Integer;
  T: IInteger;
begin
  NSet(P, 1);
  NSet(Q, 0);
  for I := Last downto 0 do
  begin
    NMul(T, P, CFEFunc(I));
    NAdd(Q, T);
    NSwp(Q, P);
  end;
end;

function CFE_Euler(Index: Integer): Integer;
// can be checked via NExp(.., 1, 1)
begin
  if Index = 0 then Result := 2 else
    if Index mod 3 <> 2 then Result := 1
      else Result := ((Index + 1) div 3) shl 1;
end;

function CFE_GoldenRatio(Index: Integer): Integer;
begin
  Result := 1;
end;

function CFE_Tan1(Index: Integer): Integer;
begin
  if Odd(Index) then Result := Index
    else Result := 1;
end;
{
procedure NLn2(var R: IInteger);
// R = R * Ln(2), = 2 * arctanh(1/3)
var
  S,T: IInteger;
  K: Cardinal;
begin
  K := 1;
  NShl(S, R, 33);
  NDiv(S, 3);
  NSet(R, 0);
  while NSgn(S) <> 0 do
  begin
    NDiv(T, S, K);
    NAdd(R, T);
    NDiv(S, 9);
    Inc(K, 2);
  end;
  NShr(R, 32);
end; }

procedure NLn2(var R: IInteger);
// R = R * Ln(2)
//   = R * 144 * arctanh(1/251) + 54 * arctanh(1/449) - 38 * arctanh(1/4801) + 62 * arctanh(1/8749)
var
  A,B: IInteger;
begin
  NShl(R, 32); // 1 digit rounding
  NMul(A, R, 144); NArcTanh(A, NInt( 251));
  NMul(B, R,  54); NArcTanh(B, NInt( 449)); NAdd(A, B);
  NMul(B, R,  38); NArcTanh(B, NInt(4801)); NSub(A, B);
  NMul(B, R,  62); NArcTanh(B, NInt(8749)); NAdd(A, B);
  NShr(R, A, 32);
end;

procedure NLn10(var R: IInteger);
// R = R * Ln(10)
//   = R * 478 * arctanh(1/251) + 180 * arctanh(1/449) - 126 * arctanh(1/4801) + 206 * arctanh(1/8749)
var
  A,B: IInteger;
begin
  NShl(R, 32); // 1 digit rounding
  NMul(A, R, 478); NArcTanh(A, NInt( 251));
  NMul(B, R, 180); NArcTanh(B, NInt( 449)); NAdd(A, B);
  NMul(B, R, 126); NArcTanh(B, NInt(4801)); NSub(A, B);
  NMul(B, R, 206); NArcTanh(B, NInt(8749)); NAdd(A, B);
  NShr(R, A, 32);
end;

procedure NArcTanh(var R: IInteger; const V: IInteger);
// R = R * arctanh(1 / V)
// binary splitting adaption of Jörg Arndt's formulas

// method 1: arctanh(1 / V) = sum[n=0..inf] 1 / (2n +1) * 1 / V^(2n +1)
// method 2: arctanh(1 / V) = sum[n=0..inf] (-4)^n*n!^2 / (2n +1)! * V / (V^2 -1)^(n +1)
var
  S: IInteger;

  procedure DoAtanh1(N: Integer; var D: TIIntegerSplitData);
  // 1. method, S = V^2
  begin
    NSet(D.B, N + N +1);
    if N > 0 then D.Q := S       // we assign here instead of use of NSet(Q, S)
      else D.Q := V;             // this save memory and speedup by "copy on demand"
  end;

  procedure DoAtanh2(N: Integer; var D: TIIntegerSplitData);
  // 2. method, S = V^2 -1
  begin
    if N > 0 then
    begin
      NSet(D.P, -(N + N));
      NMul(D.Q, S, N + N +1);
    end else
    begin
      D.P := V;
      D.Q := S;
    end;
  end;

var
  L: Integer;
  P,Q: IInteger;
begin
  if NSgn(R) <> 0 then
  begin
    NSqr(S, V);
    NDec(S, 1); // comment out for method 1
    Assert(NSgn(S) > 0, 'NArcTanh(), require V > 1');
    L := Round(NLn(R) / NLn(S)) +1;
    NBinarySplitting(P, Q, L, @DoAtanh2);
    NMul(R, P);
    NDiv(R, Q);
  end;
end;

(*
procedure NArcTan(var R: IInteger; N: Integer);
// R = R * ArcTan(1 / N)
// iterative version
var
  K,V: Integer;
  S,T: IInteger;
begin
  K := 0;
  V := N * N;
  NDiv(S, R, N);
  NSet(R, 0);
  while NSgn(S) <> 0 do
  begin
    NDiv(T, S, 2 * K + 1);
    NNeg(T, Odd(K));
    NAdd(R, T);
    NDiv(S, V);
    Inc(K);
  end;
end; *)

procedure NArcTan(var R: IInteger; const U,V: IInteger); overload;
// R = R * arctan(U / V)
// binary splitting adaption of Jörg Arndt's formulas

// method 1: arctan(U / V) = sum[n=0..inf] (-1)^n / (2n +1) * U / V^(2n +1)
// method 2: arctan(1 / V) = sum[n=0..inf] 4^n * n!^2 / (2n +1)! * V / (V^2 +1)^(n +1)

var
  sV,sU: IInteger;

  procedure DoAtan1(N: Integer; var D: TIIntegerSplitData);
  // 1. method, S = V^2
  begin
    NSet(D.B, N + N +1);
    NNeg(D.B, Odd(N));
    if N > 0 then
    begin
      D.P := sU;
      D.Q := sV;
    end else
    begin
      D.P := U;
      D.Q := V;
    end;
  end;

  procedure DoAtan2(N: Integer; var D: TIIntegerSplitData);
  // 2. method, S = V^2 +1
  begin
    if N > 0 then
    begin
      NSet(D.P, N + N);
      NMul(D.Q, sV, N + N +1);
    end else
    begin
      D.P := V;
      D.Q := sV;
    end;
  end;

resourcestring
  sNArcTan = 'NArcTan(), require U <> 0 and |U| < |V|';
var
  P,Q: IInteger;
  L: Extended;
begin
  if (NSgn(U) = 0) or (NCmp(U, V, True) >= 0) then NRaise(@sNArcTan);
  if NSgn(R) <> 0 then
  begin
    NSqr(sV, V);
    if Abs(NSgn(U, True)) <> 1 then
    begin
      NSqr(sU, U);
      L := NSize(R) / Abs(NLn(sU) - NLn(sV)) +1;
      NBinarySplitting(P, Q, Round(L), @DoAtan1);
    end else
    begin
      NInc(sV);
      L := NSize(R) / NLn(sV) +1;
      NBinarySplitting(P, Q, Round(L), @DoAtan2);
    end;
    NMul(R, P);
    NDiv(R, Q);
  end;
end;

procedure NArcTan(var R: IInteger; V: Integer); overload;
// R = R * arctan(1 / V)
// binary splitting adaption of Jörg Arndt's formulas

// method: arctan(1 / V) = sum[n=0..inf] 4^n * n!^2 / (2n +1)! * V / (V^2 +1)^(n +1)
var
  S: Int64;

  procedure DoAtan(N: Integer; var D: TIIntegerSplitData);
  begin
    if N > 0 then
    begin
      Inc(N, N);
      NSet(D.P, N);
      Inc(N, 1);
      NSet(D.Q, S);
      NMul(D.Q, N);
    end else
    begin
      NSet(D.P, V);
      NSet(D.Q, S);
    end;
  end;

resourcestring
  sNArcTan = 'NArcTan(), require V <> 0';
var
  P,Q: IInteger;
  L: Extended;
begin
  if V = 0 then NRaise(@sNArcTan);
  if NSgn(R) <> 0 then
  begin
    S := Int64(V) * Int64(V) + 1; // avoid D4 Bug for S := V * V +1 !!
    Assert(S > 0);
    L := S;
    L := NLn(R) / Ln(L) +1;
    NBinarySplitting(P, Q, Round(L), @DoAtan);
    NMul(R, P);
    NDiv(R, Q);
  end;
end;

procedure NSin(var R: IInteger; const U,V: IInteger);
// R = R * sin(U / V)
var
  sU,sV: IInteger;

  procedure DoSIN(N: Integer; var D: TIIntegerSplitData); register;
  begin
    if N > 0 then
    begin
      D.P := sU;                         // -U^2
      NSet(D.Q, Int64(N * (N + N +1)));  // 2V^2 * (2n^2 +n)             n(2n +1)
      NMul(D.Q, sV);
    end else
    begin
      NSet(D.P, U);                    // (-1)^n * x^(2n + 1) / (2n + 1)!
      NSet(D.Q, V);
    end;
  end;

resourcestring
  sNSin = 'NSin(), requiere V <> 0 and U <> 0';
var
  P,Q: IInteger;
  C,D,L: Extended;
begin
  if (NSgn(V) = 0) or (NSgn(U) = 0) then NRaise(@sNSin);
  if NSgn(R) <> 0 then
  begin
    NSqr(sU, U);
    NSqr(sV, V);
    NMul(sV, 2);
  // compute series length
    D := NLn(R);
    C := NLn(sU) - NLn(sV);
    L := 1;
    while D > 0 do
    begin
      D := D + C - Ln(L * (L + L +1));
      L := L + 1;
    end;
    NNeg(sU);
    NBinarySplitting(P, Q, Round(L), @DoSIN);  // P / Q = Sin(U / V)
    NMul(R, P);
    NDiv(R, Q);
  end;
end;

procedure NSinh(var R: IInteger; const U,V: IInteger);
// R = R * sinh(U / V)
var
  sU,sV: IInteger;

  procedure DoSINH(N: Integer; var D: TIIntegerSplitData); register;
  begin
    if N > 0 then
    begin
      D.P := sU;                         // -U^2
      NSet(D.Q, Int64(N * (N + N +1)));  // 2V^2 * (2n^2 +n)
      NMul(D.Q, sV);
    end else
    begin
      NSet(D.P, U);                      //  U
      NSet(D.Q, V);                      //  V
    end;
  end;

resourcestring
  sNSinh = 'NSinh(), requiere V <> 0 and U <> 0';
var
  P,Q: IInteger;
  C,D,L: Extended;
begin
  if (NSgn(V) = 0) or (NSgn(U) = 0) then NRaise(@sNSinh);
  if NSgn(R) <> 0 then
  begin
    NSqr(sU, U);
    NSqr(sV, V);
    NMul(sV, 2);
  // compute series length
    D := NLn(R);
    C := NLn(sU) - NLn(sV);
    L := 1;
    while D > 0 do
    begin
      D := D + C - Ln(L * (L + L +1));
      L := L + 1;
    end;
    NBinarySplitting(P, Q, Round(L), @DoSINH);  // P / Q = Sin(U / V)
    NMul(R, P);
    NDiv(R, Q);
  end;
end;

procedure NCos(var R: IInteger; const U,V: IInteger);
// R = R * cos(U / V)
var
  sU,sV: IInteger;

  procedure DoCOS(N: Integer; var D: TIIntegerSplitData); register;
  begin
    if N > 0 then
    begin
      D.P := sU;                         // -U^2
      NSet(D.Q, Int64(N * (N + N - 1))); //  2V^2 * (2n^2 -n)
      NMul(D.Q, sV);
    end;
  end;

resourcestring
  sNCos = 'NCos(), requiere V <> 0 and U <> 0';
var
  P,Q: IInteger;
  C,D,L: Extended;
begin
  if (NSgn(V) = 0) or (NSgn(U) = 0) then NRaise(@sNCos);
  if NSgn(R) <> 0 then
  begin
    NSqr(sU, U);
    NSqr(sV, V);
    NMul(sV, 2);
  // compute series length
    D := NLn(R);
    C := NLn(sU) - NLn(sV);
    L := 1;
    while D > 0 do
    begin
      D := D + C - Ln(L * (L + L +1));
      L := L + 1;
    end;
    NNeg(sU);
    NBinarySplitting(P, Q, Round(L), @DoCOS);  // P / Q = Sin(U / V)
    NMul(R, P);
    NDiv(R, Q);
  end;
end;

procedure NCosh(var R: IInteger; const U,V: IInteger);
// R = R * cosh(U / V)
var
  sU,sV: IInteger;

  procedure DoCOSH(N: Integer; var D: TIIntegerSplitData); register;
  begin
    if N > 0 then
    begin
      D.P := sU;                         // -U^2
      NSet(D.Q, Int64(N * (N + N - 1))); //  2V^2 * (2n^2 -n)
      NMul(D.Q, sV);
    end;
  end;

resourcestring
  sNCosh = 'NCosh(), requiere V <> 0 and U <> 0';
var
  P,Q: IInteger;
  C,D,L: Extended;
begin
  if (NSgn(V) = 0) or (NSgn(U) = 0) then NRaise(@sNCosh);
  if NSgn(R) <> 0 then
  begin
    NSqr(sU, U);
    NSqr(sV, V);
    NMul(sV, 2);
  // compute series length
    D := NLn(R);
    C := NLn(U) - NLn(V);
    L := 1;
    while D > 0 do
    begin
      D := D + C - Ln(L * (L + L +1));
      L := L + 1;
    end;
    NBinarySplitting(P, Q, Round(L), @DoCOSH);  // P / Q = Sin(U / V)
    NMul(R, P);
    NDiv(R, Q);
  end;
end;

procedure NTan(var R: IInteger; const U,V: IInteger);
// R = R * tan(U / V)
// todo: implement a direct binary splitting
// computation of NTan(R, 1, 1) can be checked with NCFE(.., .., CFETan1, ...);
var
  A: IInteger;
begin
  NShl(R, 32);   // rounding 
  NSqr(A, R);
  NSin(A, U, V);
  NCos(R, U, V);
  NDiv(R, A, R);
  NShr(R, 32);
end;

procedure NTanh(var R: IInteger; const U,V: IInteger);
// R = R * tanh(U / V)
var
  A: IInteger;
begin
  NShl(R, 32);
  NSqr(A, R);
  NSin(R, U, V);
  NCos(A, U, V);
  NDiv(R, A);
  NShr(R, 32);
end;

procedure NExp(var A: IInteger; U,V: Integer);
// A = A * e^(U / V)

  procedure DoExp(N: Integer; var D: TIIntegerSplitData); register;
  // A[n] = 1, B[1] = 1, P[0] = 1, Q[0] = 1, P[n] = U if n > 0, Q[n] = nV if n > 0
  // don't touch A,B because NIL interfaces are here assumed to value +1 and
  // reduce memory, useless interfaces and improve so speed
  begin
    if N > 0 then
    begin
      NSet(D.P, U);
      NSet(D.Q, Int64(N) * Int64(V));  // we need Int64 to avoid overflow and useless compiler warnings
    end;
  end;

  procedure DoExp1(N: Integer; var D: TIIntegerSplitData); register;
  // A[n] = 1, B[1] = 1, P[0] = 1, Q[0] = 1, P[n] = 1 if n > 0, Q[n] = n if n > 0
  begin
    if N > 0 then NSet(D.Q, N);
  end;

resourcestring
  sNExp = 'NExp(), invalid Parameters';
var
  D,C,L: Extended;
  P,Q: IInteger;
  S: Integer;
begin
  if NSgn(A) <> 0 then
  begin
    L := 1;
    try
  // calculate series length
      C := Ln(U) - Ln(V);
      D := NLn(A);
      while D > 0 do
      begin
        D := D + C - Ln(L);
        L := L + 1;
      end;
    except
      on E: Exception do
        NRaise(@sNExp, E.Message);
    end;
  // do binary splitting, exp(U / V) = P / Q
    if (U = 1) and (V = 1) then S := NBinarySplitting(P, Q, Round(L), @DoExp1, False)
      else S := NBinarySplitting(P, Q, Round(L), @DoExp, False);
  // finalization
    NShr(A, S);
    NMul(A, P);
    NDiv(A, Q);             // A = A * exp(U / V)
  end;
end;

procedure NExp(var A: IInteger; const U,V: IInteger);
// A = A * e^(U / V)

  procedure DoExp(N: Integer; var D: TIIntegerSplitData); register;
  // A[n] = 1, B[1] = 1, P[0] = 1, Q[0] = 1, P[n] = U if n > 0, Q[n] = nV if n > 0
  // don't touch A,B because NIL interfaces are here assumed to value +1 and
  // reduce memory, useless interfaces and improve so speed
  begin
    if N > 0 then
    begin
      D.P := U;
      NMul(D.Q, V, N);
    end;
  end;

resourcestring
  sNExp = 'NExp(), invalid Parameters';
var
  D,C,L: Extended;
  P,Q: IInteger;
begin
  if NSgn(A) <> 0 then
  begin
    L := 1;
    try
  // calculate series length
      C := NLn(U) - NLn(V);
      D := NLn(A);
      while D > 0 do
      begin
        D := D + C - Ln(L);
        L := L + 1;
      end;
    except
      on E: Exception do
        NRaise(@sNExp, E.Message);
    end;
  // do binary splitting, exp(U / V) = P / Q
    NBinarySplitting(P, Q, Round(L), @DoExp);
  // finalization
    NMul(A, P);
    NDiv(A, Q);             // A = A * exp(U / V)
  end;
end;

function NPi(var A: IInteger; Decimals: Cardinal; Method: TIIntegerPIMethod): Cardinal;
{ timings on PIV 1500Mhz 512Mb, Win2k in seconds

Decimals :    Chud(fast)   Chud(iter)          AGM Machin(fast) Machin(Iter)    CRC
   10000 :          0.02         0.05         0.06         0.07         0.23    0022C012
   25000 :          0.07         0.33         0.27         0.28         1.44    C95E3B65
   50000 :          0.21         1.35         0.79         0.83         5.70    D4509617
  100000 :          0.57         5.94         2.32         2.20        22.76    364124EB
  250000 :          2.23        44.02         8.08         7.87       145.99    A6211796
  500000 :          5.53       200.37        19.49        19.66       783.36    5BCC3354
 1000000 :         13.77       841.04        54.99        50.40      3805.81    38845D51
 2000000 :         33.26      3384.13       129.67       126.60          -      BE0EE27F
 2500000 :         46.40      5293.82       182.96       176.08          -      64E69944
 4000000 :         82.07          -         354.42       321.70          -      A5A8FE00
 5000000 :        121.13          -         439.85       459.02          -      672A62A5
 8000000 :        198.16          -         825.77       808.90          -      FD180141
16000000 :        483.48          -        2196.44      2041.08          -      7F242222
32000000 :       1151.34          -        5061.61      4775.54          -      742593FD
64000000 :       3288.20          -            -            -            -      D60B96F3

}

  procedure NPi_Iter_Machin(var R: IInteger; Decimals: Cardinal);
  // Machin's Arctan series, iterative method

    procedure AddArcTan(var R,P: IInteger; X,M: Integer);
    var
      N,D: IInteger;
      K: Cardinal;
    begin
      NMul(N, P, X * M);
      X := X * X +1;
      NSet(D, X);
      X := X + X;
      K := 0;
      repeat
        NDiv(N, D);
        if NSgn(N) = 0 then Break;
        Inc(K, 2);
        NAdd(R, N);
        NMul(N, K);
        NAdd(D, X);
      until False;
    end;

  var
    I: Cardinal;
    P: IInteger;
  begin
    I := System.Trunc(Ln(Decimals) / Ln(10)) + 2;
    NPow(P, 10, Decimals + I);
    NSet(R, 0);
//    AddArcTan(Self, P,  10,  8 * 4);
//    AddArcTan(Self, P, 239, -1 * 4);
//    AddArcTan(Self, P, 515, -4 * 4);
    AddArcTan(R, P,  18, 12 * 4);
    AddArcTan(R, P,  57,  8 * 4);
    AddArcTan(R, P, 239, -5 * 4);
    NPow(P, 10, I);
    NDiv(R, P);
  end;

  procedure NPi_Fast_Machin(var R: IInteger; Decimals: Integer);

    procedure NArcTanSeries(var R: IInteger; const S: array of Integer);
    var
      P,T: IInteger;
      I: Integer;
    begin
      NShl(R, 32);
      I := 0;
      repeat
        NSet(P, R);
        NArcTan(P, S[I +1]);
        NMul(P, S[I + 0]);
        NAdd(T, P);
        Inc(I, 2);
      until I >= Length(S);
      NShr(R, T, 30);
    end;

  begin
    NPow(R, 5, Decimals);
    NShl(R, Decimals);
  // pi/4 = 44*arctan(1/57) +7*arctan(1/239) -12*arctan(1/682) +24*arctan(1/12943)
    NArcTanSeries(R, [+44, 57, +7, 239, -12, 682, +24, 12943]);
  //  NArcTanSeries(R, [+1, 2, +1, 3]);
  //  NArcTanSeries(R, [+12, 18, +8, 57, -5, 239]);
  //  NArcTanSeries(R, [+4, 5, -1, 239]);
  //  NArcTanSeries(R, [+6, 8, +2, 57, +1, 239]);
  //  NArcTanSeries(R, [+88, 111, +7, 239, -44, 515, +32, 682, +24, 12943]);
  //  NArcTanSeries(R, [+322, 577, +76, 682, +139, 1393, +156, 12943, +132, 32807, +44, 1049433]);
  end;

  procedure NPi_AGM(var R: IInteger; Decimals: Cardinal);
{AGM start with:
   a = 1, b = 1/Sqrt(2), t = 1/2, k = 1

 iteration:

   s = (a + b) / 2
   d = a - s
   d = d^2
   a = s
   s = s^2
   t = t - d * 2^k
   b = Sqrt(s - d)
   k = k +1

 final:
   pi ~ (a + b)^2 / 2t }
  var
    A,B,D,T: IInteger;
    W: Integer;
  begin
(*
  ---- a formula based on the AGM (Arithmetic-Geometric Mean) ----
    c = sqrt(0.125);
    a = 1 + 3 * c;
    b = sqrt(a);
    e = b - 0.625;
    b = 2 * b;
    c = e - c;
    a = a + e;
    npow = 4;
    do {
        npow = 2 * npow;
        e = (a + b) / 2;
        b = sqrt(a * b);
        e = e - b;
        b = 2 * b;
        c = c - e;
        a = e + b;
    } while (e > SQRT_SQRT_EPSILON);
    e = e * e / 4;
    a = a + b;
    pi = (a * a - e - e / 2) / (a * c - e) / npow;
*)
    Inc(Decimals, 3);           // +3 digits reserve
    NPow(R, 5, Decimals);       // R =  5^Decimals
    NShl(A, R, Decimals);       // A = 10^Decimals
    NShl(D, R, Decimals -1);    // D = 10^(Decimals -1)^2
    NSqr(D);
    NSqr(B, A);                 // B = (10^Decimals^2 * 2)^0.5 div 2
    NShl(B, 1);
    NSqrt(B);
    NShr(B, 1);
    W := 0;
    repeat
      NAdd(T, A, B);            // T = (A + B) div 2, new A
      NShr(T, 1);
      NMul(B, A);               // B = (B * A)^0.5
      NSqrt(B);
      NSub(A, T);               // A = (A - T)^2 * 2^W
      NSqr(A);
      NShl(A, W);
      Inc(W);
      NSub(D, A);               // D = D - A
      NSwp(A, T);
    until NCmp(B, A) = 0;
    NShr(D, Decimals);
    NDiv(D, R);
    NMul(D, 1000);
    NSqr(R, A);
    NDiv(R, D);
  end;

  procedure NPi_Iter_Chudnovsky(var R: IInteger; Decimals: Cardinal);
  // Chudnovsky's brothers Ramanujan iterative computation
  //           12                                      (6n)! * (A + n * B)
  //  1/PI = ------- * sum(n = 0..invinite) * (-1)^n ------------------------
  //         C^(3/2)                                  n!^3 * (3 * n)! * C^3^n
  //
  const
    k_A = 13591409;
    k_B = 545140134;
    k_C = 53360;
    k_1 = 100100025;    // k_1 * k_2 = 32817176580096000 = (12 * C)^3 / 8
    k_2 = 327843840;
  var
    N: Integer;
    P,S,T: IInteger;
  begin
    NPow(T, 10, Decimals + 6);        // T = 10^Deciamls * 10^6  --> +6 Digit correction
    NSqr(R, T);                       // R = 53360 * Sqrt(640320 * T^2)
    NMul(R, k_C * 12);
    NSqrt(R);
    NMul(R, k_C);
    NMul(R, T);

    NMul(T, 1000000);                 // -6 digits correct
    NMul(P, T, k_A);                  // P = T * k_A, P = sum[]

    N := 1;
    while NSgn(T) > 0 do
    begin
      NSet(S, 6 * N -5); // T = T * (6n -5)(6n -3)(6n -1)
      NMul(S, 6 * N -3);
      NMul(S, 6 * N -1);
      NMul(T, S);

      NPow(S, N, 3);     // T = T / n^3 * 32817176580096000
      NMul(S, k_1);
      NMul(S, k_2);
      NDiv(T, S);

      NSet(S, k_B);      // P = P +- T * (k_A + n * k_B)
      NMul(S, N);
      NAdd(S, k_A);
      NMul(S, T);
      NNeg(S, Odd(N));   // (-1)^N --> -1 = odd(N) or 1 = not odd(N)
      NAdd(P, S);

      Inc(N);
    end;
    NDiv(R, P);          // PI == (R * 10^3) div (P * 10^6)
  end;

  procedure NPi_Fast_Chudnovsky(var R: IInteger; Decimals: Cardinal);
  // Chudnovsky's brothers Ramanujan with binary splitting
  // this code is most only 2-3 Times slower as the fastests special PI-programs
  // Some optimizations can be done, but here i wanted a readable code.
  // One way to made it faster (~2 times) could be to avoid NBinarySplitting() and its
  // callback.
  // Use same formula as above.
{
1      12   inf  (-1)^n (6n)! (A+nB)
-- = ------ SUM: -------------------
pi   C^1.5  n=0  (3n)! (n!)^3 C^(3n)


A=13591409 B=545140134 C=640320

a(n) = (A+nB)
p(n) = -1(6n-5)(6n-4)(6n-3)(6n-2)(n6-1)(6n)
b(n) = 1
q(n) = (3n-2)(3n-1)(3n)(n^3)(C^3)
}
  const
    k_A = 163096908;          // 12 *  13591409
    k_B = 6541681608;         // 12 * 545140134
    k_C = 53360;              // 640320 / 12
    k_D = 1728;               // 12^3 = 24 * 72
    k_E = 262537412640768000; // 1727 * k_C^3

    procedure DoPI(N: Integer; var D: TIIntegerSplitData); register;
    // the callback for binary splitting
    // max. n < ~306783379 then 6n << MaxInt
    // B[1] = 1, P[0] = 1, Q[0] = 1
    // don't touch B,P[0],Q[0] because NIL interfaces are here assumed to value +1
    begin
      if N > 0 then
      begin
        NSet(D.A, k_B);                  // a = k_A + n * k_B
        NMul(D.A, N);
        NAdd(D.A, k_A);

        NSet(D.P,   2 * N -1 );
        NMul(D.P,   6 * N -1 );
        NMul(D.P, -(6 * N -5));          // p = - (6*n -5) * (6*n -1)* (2*n -1)

        NSet(D.Q, k_C);                  // q = 72(n * k_C)^3      // 72 * n^3 * k_C^3
        NMul(D.Q, N);
        NPow(D.Q, 3);
        NMul(D.Q, 72);
      end else
        NSet(D.A, k_A); // A[0] = k_A
    end;

  var
    P,Q: IInteger;
    S: Cardinal;
  begin
    S := Trunc(Decimals / 14.181) +2;
    S := NBinarySplitting(P, Q, S, @DoPI, False);  // decimal approximation is very roughly !!
    NPow(R, 100, Decimals);
    NMul(R, NInt(k_E));
    NSqrt(R);           // R = (k_C^3 * 12^3 * 100^Decimals)^(1/2)
    NMul(R, Q);
    NShl(R, S);
    NDiv(R, P);         // R = R * Q / P = R / S
  end;

  procedure NPi_Fast_Chudnovsky2(var R: IInteger; Decimals: Cardinal);
  // another bin. split. chudnovsky
  const
    k_A = 13591409;
    k_B = 545140134;
    k_C = 53360;

    procedure DoPI(N: Integer; var D: TIIntegerSplitData); register;
    begin
      Inc(N);

      NSet(D.A, k_B);                  
      NMul(D.A, N);
      NAdd(D.A, k_A);
      NNeg(D.A, Odd(N));

      NSet(D.Q, N);
      NMul(D.Q, N);
      NMul(D.Q, N);
      NMul(D.Q, (k_C div 2) * (k_C div 2));
      NMul(D.Q, k_C * 12 * 12 * 2);

      NSet(D.P, N + N -1);
      N := N * 6;
      NMul(D.P, N -1);
      NMul(D.P, N -5);
    end;

  var
    P,Q: IInteger;
    S: Cardinal;
  begin
    Inc(Decimals, 3);
    S := NBinarySplitting(P, Q, Trunc(Decimals / 14.181) +2, @DoPI, False);  // decimal approximation is very roughly !!
// R := (10^(Decimals*2) * 12 * k_C)^0.5 * k_C * Q / (P + Q * k_A)
    NMul(R, Q, k_A);
    NShl(R, S);
    NAdd(P, R);
    NPow(R, 5, Decimals + Decimals);
    NMul(R, k_C * 12);
    NShl(R, Decimals + Decimals);
    NSqrt(R);
    NMul(R, k_C);
    NMul(R, Q);
    NShl(R, S);
    NMul(P, 1000);
    NDiv(R, P);
  end;

begin
  Result := 1;
  case Method of
    piFastChudnovsky : NPi_Fast_Chudnovsky(A, Decimals);
    piIterChudnovsky : NPi_Iter_Chudnovsky(A, Decimals);
    piAGM            : NPi_AGM(A, Decimals);
    piIterMachin     : NPi_Iter_Machin(A, Decimals);
    piFastMachin     : NPi_Fast_Machin(A, Decimals);
  end;
end;

procedure NFactorial1(var A: IInteger; N: Cardinal);
// A = N!, demonstrate the use of NBinarySplitting()

  procedure DoFactorial(N: Cardinal; var D: TIIntegerSplitData); register;
  // there exist various combination that can we use here:
  // 1.) A[n] = n, B[n] = n
  // 2.) P[n] = n, Q[n] = n
  // 3.) B[n] = n
  // 4.) Q[n] = n
  // each with and without shifting
  // follow code is the efficent way with NBinarySplitting() because only Q[n] are touched
  // this code is ~2 times slower as the buildin NFactorial(), (due to the slower generic NBinarySplitting())
  // the other variants above are ~3 times slower
  // remark:
  //  NBinarySplitting() and it callback's assumed for P,Q,A,B always 1 if these vars are nil !
  //  That produce readable code such as follow and improve speed and reduce memory usage.
  begin
    if N > 0 then NSet(D.Q, N);
  end;

var
  S: Cardinal;
begin
  S := NBinarySplitting(A, A, N, @DoFactorial, False);
  while not Odd(N) do
  begin
    N := N shr 1;
    Inc(S);
  end;
  NShl(A, S);
  NMul(A, N);
end;

end.
