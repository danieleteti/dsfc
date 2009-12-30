{Copyright:      Hagen Reddmann HaReddmann at T-Online dor de
 Author:         Hagen Reddmann
 Remarks:        this Copyright must be included
 known Problems: none
 Version:        5.1, Delphi Encryption Compendium II
                 Delphi 5-6
 Description:    ID based Prime system to generate verifyable secure primes
                 Such ID Primes have some important features
                 - can't be forged
                 - verifyable
                 - use a known and common computation way
                 - storage is incomparable shorter as the described prime self
                 - primes are secure random choosen
                 - there exists no known way to produce or forge primes to well
                   choosen weak primes
                 - fast, incomparable faster as the recreation of the prime
                 - the ID Prime datastructure is self checked
                 - supports binary and plain readable formats
                 - use many different secure hash function, eg portable
                 - binary format use Big Endian Notation  
                 - binary dataformat use most only 8 - 16 Bytes, most 13/14 bytes
                   in comparsion a 1024 Bit prime requiere 131 Bytes storage.

 how works:

   NMake() create an random SeedBitSize great seed value. This seed is expanded
   with an indexed repeated Hash computation (MGF) and produce a concatenated Value P of
   BitSize Bits. The indexed hash use internal an Index called HashIndex to ensure
   that with another choosen HashIndex we get absolutly other outputs. Important
   is here the fact that in contrast to the outer seed we change with this HashIndex
   the internal working state of the indexed Hash algorithm.
   Now we can use P as BitSize seed to compute the prime. The Prime is congruent
   to Residue mod Modulus, eg P == R mod M. There exists two exceptions:
   1.) R=+1 and M=1 will produce a Safe Prime so called Sophie Germain Prime into P
       P = 2Q +1 where Q and P is prime.
   2.) R=-1 and M=1 will produce a Strong Lucas Prime a safe prime in modular lucas sequences
       P = 2Q -1 where Q and P is prime
   After creation of the prime, the maincomputation, we store the skipped Residuecount
   eg. ID.Count. This give us on recreation a direct way to compute P based on S directly.
   Basicaly ID.Count * M + R is the Offset that we must add to the seed to get P.
   Now we compute a Hash C over the Prime as secure checksum. This hash is to long
   and need to many space if we want it to store. To avoid this we compute a CRC 16Bit ZModem
   over C and store it into ID.CRC. This CRC requiere now only 2 Bytes and provide us
   with a range of 65535 possible Databits.

   About ID-Prime Stringformats:
   The format looks like this
     342:3:4:SHA1:1:EBBFBBD3:745E:8
   eg.
     Prime Bitsize : Residue : Modulus : Hash Algo : HashIndex : Seed : CRC : Count
       decimal     :   dec   :   dec   :  alpha    :    dec    : hex  : hex : dec
        < 65535    :   > 0   :  >Res.  :           :   < 65535 :  >0  :16Bit: >= 0

   and are a
     342 Bit prime congruent to P == 3 mod 4 created with SHA1 and HashIndex 1
     based on a 32 Bit Seed $EBBFBBD3 and have a hashed CRC 16 Checksum of $745E
     based from the hashed seed we have < 3 + 4 * 8 to add the get the prime.

   If You want to use the binary Dataformat then ensure that SeedBitSize is
   SeedBitSize = SeedBytes * 8 -1. This save always one Byte in the Datasize
   and reduce only 1 bit the seed.

   in binary:
     1024:1:2:MD4:1:79758AA3:02D4:10

    $04 $00 $21 $04 $79 $75 $8A $A3 $04 $01 $02 $D4 $0A

    $0400            = 1024
    $21              = 1 mod 2
    $04 $79758AA3    = ASN1 Length encoded tag of 4 bytes seed in big endian
    $04              = predefined ID for Hash MD4, if custom an ASN1 String are here
    $01              = ASN1 Length encoded tag for HashIndex
    $02D4            = 16 Bit CRC
    $0A              = ASN1 Length encoded tag for Count

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

unit IDPrimes;
{$I VER.INC}
interface

uses Classes, NInts, DECHash;

type
  IIDPrime = interface
    ['{126BE110-061D-4067-9E0A-E2A490AF5CEA}']
    function BitSize: Word;
    function Residue: IInteger;
    function Modulus: IInteger;
    function Seed: IInteger;
    function HashClass: TDECHashClass;
    function HashIndex: Word;
    function CRC: Word;
    function Count: Cardinal;
  end;

// compute the verifyable Prime dependend of ID into P,
// if RaiseError = False then Result contains Errorcode or ZERO for success
function  NSet(var P: IInteger; const ID: IIDPrime; CheckPrimality: Boolean = False;
            RaiseError: Boolean = True): Integer; overload;
// same as above, but as function and raise always an error on bad parameters
function  NInt(const ID: IIDPrime; CheckPrimality: Boolean = False): IInteger; overload;
// setup ID to ID-Prime formated string
function  NSet(var ID: IIDPrime; const S: String;
            RaiseError: Boolean = False): Integer; overload;
// setuo ID, same as above but as fucntion and raise always an error on bad parameters
function  NIDPrime(const ID: String): IIDPrime; overload;
// converts ID into an ID-Prime formatted string
function  NStr(const ID: IIDPrime): String; overload;
// save ID into stream
procedure NSave(const ID: IIDPrime; Stream: TStream); overload;
// load ID from stream
function  NLoad(var ID: IIDPrime; Stream: TStream; RaiseError: Boolean = False): Integer; overload;
// load a ID Prime from Stream
function  NIDPrime(Stream: TStream): IIDPrime; overload;
// created an ID Prime and correspondending Prime as result
function  NMake(var ID: IIDPrime; BitSize: Word; SeedBitSize: Word = 31;
            const Residue: IInteger = nil; const Modulus: IInteger = nil;
            HashIndex: Word = 1; HashClass: TDECHashClass = nil;
            Callback: TIIntegerPrimeCallback = nil): IInteger; overload;


const
// Identities for some Hash Algorithms, intern used in binary Dataformat
// placed here to support custom Identities
  IDPrimeHash: array[0..$11] of Cardinal =
    (        0,$FED1656F,        0,        0,$90762D4A,$E7711DDC,        0,
     $09C97F2E,$637E3218,$A3767AB7,$04962372,$AC6A2448,$C6DD697E,$AF4B149D,
     $A79AD63A,$06D521D1,$CFD609CC,$F625D933);

{ $00 == custom use Identity
  $01 == SHA1-160
  $02 == SHA1-256
  $03 == MD2
  $04 == MD4
  $05 == MD5-128
  $06 == MD5-256
  $07 == RipeMD-128
  $08 == RipeMD-160
  $09 == RipeMD-256
  $0A == RipeMD-320
  $0B == Haval-128
  $0C == Haval-160
  $0D == Haval-192
  $0E == Haval-224
  $0F == Haval-256
  $10 == Tiger
  $11 == Square}


