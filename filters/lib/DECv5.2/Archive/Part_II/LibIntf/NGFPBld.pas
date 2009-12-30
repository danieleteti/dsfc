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
unit NGFPBld;
{$I VER.INC}
interface

uses NInts, NGFPs, NPolys, DECHash;

type
  IGFpECPolyTable = interface
    ['{126BE100-061D-4067-9E0A-E2A490AF5CEA}']
    function Get(var A: IPoly; Discriminant: Cardinal): Integer;
    function Coefficients(var A,B: IInteger; Discriminant: Cardinal): Integer;
  end;

  TGFpECGetPoly = function: IGFpECPolyTable;

  TECBuildFlag = (Normal, Coefficient_AM3);

const // result codes for IGFpECPolyTable .Get/.Coefficients
  CM_Found       =  0;
  CM_NotFound    = -1;
  CM_OutOfBounds = -2;

function NECBuild(var E: IGFpECC; FieldBitSize: Integer = 168; CofactorBitSize: Integer = 1;
                   Flag: TECBuildFlag = Normal;
                   SeedBitSize: Word = 31; HashClass: TDECHashClass = nil;
                   MinCM: Cardinal = 0; MaxCM: Cardinal = MaxInt): Cardinal; overload;
function NECRegisterPolyTable(GetPoly: TGFpECGetPoly): Boolean;
function NECUnregisterPolyTable(GetPoly: TGFpECGetPoly): Boolean;
function NECPolyTable: IGFpECPolyTable;


