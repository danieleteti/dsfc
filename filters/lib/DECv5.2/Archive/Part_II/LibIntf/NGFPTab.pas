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
unit NGFPTab;

interface

var
  SupportsPolyTableFiles: Boolean = False;
  PolyTableFilePath: String = '';
  
// if TRUE scans for *.TAB files in APP path and subdirs. 

{.$R P1003.res}  //  250 Class-Polynoms from Discriminant 0 upto  1003,  8.912 Bytes
{.$R P4984.res}  //  503 Class-Polynoms from Discriminant 0 upto  4984, 22.681 Bytes
{$R P8248.res}   //  316 Class-Polynoms from Discriminant 0 upto  8248, 11.403 Bytes
{.$R P9172.res}  //  380 Class-Polynoms from Discriminant 0 upto  9182, 14.556 Bytes
{.$R P9988.res}  // 1000 Class-Polynoms from Discriminant 0 upto  9988, 69.346 Bytes
{.$R P14008.res} //  507 Class-Polynoms from Discriminant 0 upto 14008, 32.768 Bytes



