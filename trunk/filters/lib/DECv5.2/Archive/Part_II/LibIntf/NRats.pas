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
unit NRats;

interface

uses NInts;

type
  IRational = interface
    ['{126BE020-061D-4067-9E0A-E2A490AF5CEA}']
    function N: IInteger;
    function D: IInteger;
    function Precision(Base: TBase = 10): Cardinal;
  end;

  IRationalArray = array of IRational;

  TIRationalSortCompare = function(const A,B: IRational): Integer; register;
  TIRationalForEachCallback = function(var A: IRational): Boolean; register;

  TIRationalResult = (rrValue, rrNominator, rrDenominator);

procedure NSet(var A: IRational; const N: IInteger; const D: IInteger = nil); overload;
procedure NSet(var A: IRational; const N: Integer; const D: Integer = 1); overload;
procedure NSet(var A: IRational; const N: Int64; const D: Int64 = 1); overload;
procedure NSet(var A: IRational; const B: IRational); overload;
procedure NSet(var A: IRational; const B: Extended); overload;
procedure NSet(var A: IRational; const B: String; const Format: TStrFormat); overload;
procedure NSet(var A: IRational; const B: String); overload;
procedure NSet(var A: IRationalArray; const B: array of IRational); overload;
function  NRat(const N: IInteger): IRational; overload;
function  NRat(const N,D: IInteger): IRational; overload;
function  NRat(const N: Integer): IRational; overload;
function  NRat(const N,D: Integer): IRational; overload;
function  NRat(const N: Int64): IRational; overload;
function  NRat(const N,D: Int64): IRational; overload;
function  NRat(const A: Extended): IRational; overload;
function  NRat(const A: String; const Format: TStrFormat): IRational; overload;
function  NRat(const A: String): IRational; overload;
function  NRat(const A: array of IRational): IRationalArray; overload;
function  NInt(const A: IRational; What: TIRationalResult = rrValue): IInteger; overload;
function  NSgn(const A: IRational): Integer; overload;
procedure NSgn(var A: IRational; Sign: Integer); overload;
function  NNeg(var A: IRational): Boolean; overload;
function  NNeg(var A: IRational; Negative: Boolean): Boolean; overload;
function  NAbs(var A: IRational): Boolean; overload;
procedure NSwp(var A,B: IRational); overload;
function  NCmp(const A,B: IRational; Abs: Boolean = False): Integer; overload;
procedure NInc(var A: IRational; B: Cardinal = 1); overload;
procedure NDec(var A: IRational; B: Cardinal = 1); overload;
procedure NAdd(var A: IRational; B: Integer); overload;
procedure NAdd(var A: IRational; const B: IRational; C: Integer); overload;
procedure NAdd(var A: IRational; const B: IInteger); overload;
procedure NAdd(var A: IRational; const B: IRational; const C: IInteger); overload;
procedure NAdd(var A: IRational; const B: IRational); overload;
procedure NAdd(var A: IRational; const B,C: IRational); overload;
procedure NSub(var A: IRational; B: Integer); overload;
procedure NSub(var A: IRational; const B: IRational; C: Integer); overload;
procedure NSub(var A: IRational; const B: IInteger); overload;
procedure NSub(var A: IRational; const B: IRational; const C: IInteger); overload;
procedure NSub(var A: IRational; const B: IRational); overload;
procedure NSub(var A: IRational; const B,C: IRational); overload;
procedure NShl(var A: IRational; Shift: Integer); overload;
procedure NShl(var A: IRational; const B: IRational; Shift: Integer); overload;
procedure NShr(var A: IRational; Shift: Integer); overload;
procedure NShr(var A: IRational; const B: IRational; Shift: Integer); overload;
procedure NMul(var A: IRational; B: Integer); overload;
procedure NMul(var A: IRational; const B: IRational; C: Integer); overload;
procedure NMul(var A: IRational; const B: IInteger); overload;
procedure NMul(var A: IRational; const B: IRational; const C: IInteger); overload;
procedure NMul(var A: IRational; const B: IRational); overload;
procedure NMul(var A: IRational; const B,C: IRational); overload;
procedure NSqr(var A: IRational); overload;
procedure NSqr(var A: IRational; const B: IRational); overload;
procedure NSqrt(var A: IRational); overload;
procedure NSqrt(var A: IRational; const B: IRational); overload;
procedure NDiv(var A: IRational; B: Integer); overload;
procedure NDiv(var A: IRational; const B: IRational; C: Integer); overload;
procedure NDiv(var A: IRational; const B: IInteger); overload;
procedure NDiv(var A: IRational; const B: IRational; const C: IInteger); overload;
procedure NDiv(var A: IRational; const B: IRational); overload;
procedure NDiv(var A: IRational; const B,C: IRational); overload;
procedure NInv(var A: IRational); overload;
procedure NInv(var A: IRational; const B: IRational); overload;
procedure NPow(var A: IRational; E: Integer); overload;
procedure NPow(var A: IRational; const B: IRational; E: Integer); overload;
procedure NExp(var A: IRational); overload;
procedure NExp(var A: IRational; const B: IRational); overload;

procedure NRnd(var A: IRational; Bits: Integer = 0; Sign: Boolean = False); overload;
function  NStr(const A: IRational; Base: TBase = 10; Precision: Cardinal = 0): String; overload;
function  NStr(const A: IRational; const Format: TStrFormat; Precision: Cardinal = 0): String; overload;
procedure NSort(var A: IRationalArray; Compare: TIRationalSortCompare); overload;
procedure NSort(var A: IRationalArray; Abs: Boolean = False; Descending: Boolean = False); overload;
function  NForEach(const A: IRationalArray; Callback: TIRationalForEachCallback): IRational; overload;

function  NPrc(var A: IRational; Precision: Cardinal = 0; Base: TBase = 10): Cardinal; overload;

var
  DefaultPrecision: Cardinal = 1024;
   

