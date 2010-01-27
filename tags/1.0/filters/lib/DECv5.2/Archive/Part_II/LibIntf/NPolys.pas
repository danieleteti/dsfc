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
unit NPolys;

interface

uses NInts, CRC;

type
  IPoly = type IIntegerArray;

procedure NSet(var A: IPoly; const B: IIntegerArray); overload;
procedure NSet(var A: IPoly; const B: array of const); overload;
procedure NSet(var A: IPoly; const B: IPoly); overload;
procedure NSet(var A: IInteger; const B: IPoly; const X: IInteger; const M: IInteger = nil); overload;
function  NInt(const A: IPoly; const X: IInteger; const M: IInteger = nil): IInteger; overload;
procedure NSwp(var A,B: IPoly); overload;
procedure NSwp(var A: IPoly); overload;
function  NPoly(const B: IIntegerArray): IPoly; overload;
function  NPoly(const B: array of const): IPoly; overload;
function  NDegree(const A: IPoly): Integer; overload;
function  NCmp(const A,B: IPoly): Integer; overload;
procedure NAdd(var A: IPoly; const B: IPoly); overload;
procedure NAdd(var A: IPoly; const B,C: IPoly); overload;
procedure NSub(var A: IPoly; const B: IPoly); overload;
procedure NSub(var A: IPoly; const B,C: IPoly); overload;
procedure NInc(var A: IPoly; const B: Cardinal = 1); overload;
procedure NDec(var A: IPoly; const B: Cardinal = 1); overload;
procedure NMul(var A: IPoly; const B: IPoly); overload;
procedure NMul(var A: IPoly; const B,C: IPoly); overload;
procedure NMul(var A: IPoly; const B: IInteger); overload;
procedure NMul(var A: IPoly; const B: IPoly; const C: IInteger); overload;
procedure NMul(var A: IPoly; B: Integer); overload;
procedure NMul(var A: IPoly; const B: IPoly; C: Integer); overload;
procedure NSqr(var A: IPoly); overload;
procedure NSqr(var A: IPoly; const B: IPoly); overload;
procedure NRem(var A: IPoly; const M: IInteger); overload;
procedure NMod(var A: IPoly; const M: IInteger); overload;
function  NRem(var A: IPoly; const B: IPoly; const M: IInteger): Boolean; overload;
function  NRem(var A: IPoly; const B,C: IPoly; const M: IInteger): Boolean; overload;
function  NDiv(var A: IPoly; const B: IPoly; const M: IInteger): Boolean; overload;
function  NDiv(var A: IPoly; const B,C: IPoly; const M: IInteger): Boolean; overload;
function  NDivRem(var Q,R: IPoly; const A,B: IPoly; const M: IInteger): Boolean; overload;
procedure NDivXk(var A: IPoly; K: Cardinal = 1); overload;
procedure NDivXk(var A: IPoly; const B: IPoly; K: Integer = 1); overload;
procedure NDivRemXk(var Q,R: IPoly; const B: IPoly; K: Integer = 1); overload;
procedure NRemXk(var A: IPoly; K: Integer = 1); overload;
procedure NRemXk(var A: IPoly; const B: IPoly; K: Integer = 1); overload;
procedure NMulXk(var A: IPoly; K: Integer = 1); overload;
procedure NMulXk(var A: IPoly; const B: IPoly; K: Integer = 1); overload;
function  NGCD(var D: IPoly; const A,B: IPoly; const M: IInteger): Boolean; overload;
procedure NPowRem(var A: IPoly; const E: IInteger; const B: IPoly; const M: IInteger); overload;
procedure NPowMod(var A: IPoly; const E: IInteger; const B: IPoly; const M: IInteger); overload;
procedure NRnd(var A: IPoly; Degree: Cardinal; const M: IInteger; Monic: Boolean = False); overload;
procedure NFactor(var A: IPoly; const B: IPoly; K: Integer; const M: IInteger); overload;
function  NStr(const A: IPoly; Base: TBase = 0): String; overload;
function  NStr(const A: IPoly; const Format: TStrFormat): String; overload;
function  NCRC(const A: IPoly; CRC: TCRCType = CRC_32CCITT): Cardinal; overload;
procedure NNorm(var A: IPoly); overload;


