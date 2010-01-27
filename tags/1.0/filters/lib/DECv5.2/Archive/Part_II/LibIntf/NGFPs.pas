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
{$I VER.INC}
unit NGFPs;

interface

uses Classes, NMath, NInts, IDPrimes;

type
// Elliptic Curve over GF(p)
// Weierstrass form  y^2 = x^3 + ax + b
// where a and b are integers modulo p and  4a^3 + 27b^2 <> 0 mod p
  IGFpEC = packed record
    P,A,B: IInteger;
  end;

  TECState = (Empty, WrongField, WrongCoefficients, WrongBasePoint, WrongOrder,
              WrongCofactor, FailsMOVCondition, PointHaveWrongOrder, PointNotOnCurve,
              WrongPoint, Valid);

  IGFpECC = packed record
    EC: IGFpEC;     // field and curve
    ID: IIDPrime;   // verifiable ID of EC.P
    G: I2Point;     // basepoint
    R: IInteger;    // order, prime  {called n in EC-DSA}
    K: IInteger;    // cofactor      {called h in EC-DSA}
  end;              // FR = field representation
                    // d  = private key IInteger
                    // Q  = public key I2point

// per convention the GF(p)EC point at infinity O = identity is NEmpty(G) = True = (G.X = nil and G.Y = nil)
// all other points are finite points

  IGFpMulPrecomputation = interface
    ['{126BE0A0-061D-4067-9E0A-E2A490AF5CEA}']
    procedure Precompute(const B: I2Point; const E: IGFpEC; CMaxBitSize: Integer);
    function  Mul(var A: I2Point; const B: I2Point; const C: IInteger; const E: IGFpEC; var Res: Boolean): Boolean;
    function  Mul2k(var A: I2Point; const B: I2Point; K: Cardinal; const E: IGFpEC; var Res: Boolean): Boolean;
    procedure Save(Stream: TStream);
    procedure Load(Stream: TStream);
  end;

procedure NECRaise(State: TECState); overload;
function  NECCheck(const E: IGFpEC; RaiseException: Boolean = False): TECState; overload;
function  NECCheck(const E: IGFpECC; RaiseException: Boolean = False): TECState; overload;
function  NECCheck(const P: array of I2Point; const E: IGFpEC; RaiseException: Boolean = False): TECState; overload;
function  NECCheck(const P: array of I2Point; const E: IGFpECC; RaiseException: Boolean = False): TECState; overload;
function  NECCheckMOVCondition(const E: IGFpECC; RaiseException: Boolean = False): TECState; overload;
procedure NSwp(var A,B: IGFpEC); overload;
function  NSet(var A: I2Point; const X: IInteger; YIsOdd: Boolean; const E: IGFpEC): TECState; overload;
procedure NSet(var A: IGFpEC;  const B: IGFpEC); overload;
procedure NSet(var A: IInteger; const B: I2Point); overload;
function  NSet(var A: I2Point; const B: IInteger; const E: IGFpEC): TECState; overload;
function  NAdd(var A: I2Point; const B: I2Point; const E: IGFpEC): Boolean; overload;
function  NSub(var A: I2Point; const B: I2Point; const E: IGFpEC): Boolean; overload;
function  NMul(var A: I2Point; const B: IInteger; const E: IGFpEC; const P: IGFpMulPrecomputation = nil): Boolean; overload;
function  NMul(var A: I2Point; const B: I2Point; const C: IInteger; const E: IGFpEC; const P: IGFpMulPrecomputation = nil): Boolean; overload;
function  NMul2k(var A: I2Point; K: Cardinal; const E: IGFpEC; const P: IGFpMulPrecomputation = nil): Boolean; overload;
function  NMul2k(var A: I2Point; const B: I2Point; K: Cardinal; const E: IGFpEC; const P: IGFpMulPrecomputation = nil): Boolean; overload;
function  NInv(var A: I2Point; const E: IGFpEC): Boolean; overload;
procedure NRnd(var A: I2Point; const E: IGFpEC); overload;
procedure NRnd(var E: IGFpEC; FieldBitSize: Integer = 168); overload;
function  NEmpty(const A: I2Point): Boolean; overload;
function  NEmpty(const A: I3Point): Boolean; overload;
// modified Brickell's precomputation for mul
procedure NSet(var P: IGFpMulPrecomputation; const B: I2Point; const E: IGFpEC; CMaxBitSize: Integer); overload;
procedure NSave(const P: IGFpMulPrecomputation; Stream: TStream); overload;
procedure NLoad(var P: IGFpMulPrecomputation; Stream: TStream); overload;

{$DEFINE Test_Mul}
{$IFDEF Test_Mul}
function NMul_MontPB(var A: I2Point; const B: I2Point; const C: IInteger; const E: IGFpEC): Boolean;
function NMul_Affine(var A: I2Point; const B: I2Point; C: IInteger; const E: IGFpEC): Boolean;
function NMul_Proj(var A: I2Point; const B: I2Point; const C: IInteger; const E: IGFpEC): Boolean;
function NMul2k_Normal(var A: I2Point; const B: I2Point; K: Cardinal; const E: IGFpEC): Boolean;
function NMul2k_Mont(var A: I2Point; const B: I2Point; K: Cardinal; const E: IGFpEC): Boolean;
{$ENDIF}


