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
unit NIntM;

interface

uses NInts;

type
  IModulus = packed record
    M: Cardinal; // Modulus
    U: Cardinal; // inverse for Montgomery, if <> 0 M use Montgomery
    C: Cardinal; // value to compute Montgomery
    O: Cardinal; // One in Normal or Montgomery Domain
  end;

procedure NSet(var M: IModulus; Modulus: Cardinal); overload;
function  NSet(A: Cardinal; const M: IModulus): Cardinal; overload;
function  NGet(A: Cardinal; const M: IModulus): Cardinal; overload;
function  NAddMod(A,B: Cardinal; const M: IModulus): Cardinal; overload;
function  NSubMod(A,B: Cardinal; const M: IModulus): Cardinal; overload;
function  NMulMod(A,B: Cardinal; const M: IModulus): Cardinal; overload;
function  NPowMod(A,E: Cardinal; const M: IModulus): Cardinal; overload;
function  NSqrtMod(A: Cardinal; const M: IModulus): Cardinal; overload;
function  NSqrtMod(A,P: Cardinal): Cardinal; overload;
function  NInvMod(A,M: Cardinal): Cardinal; overload;
function  NMulMod(A,B,M: Cardinal): Cardinal; overload;
function  NAddMod(A,B,M: Cardinal): Cardinal; overload;
function  NSubMod(A,B,M: Cardinal): Cardinal; overload;


