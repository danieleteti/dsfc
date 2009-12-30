{Copyright:      Hagen Reddmann  HaReddmann at T-Online dot de
 Author:         Hagen Reddmann
 Remarks:        All rights reserved
 Version:        5.1
                 Delphi 3-5, designed and testet under D3-D6
 Description:    Small Primes upto 2^32-1
 Remarks:
   codesize 6415 bytes, datasize 48 bytes if all methods are used

 some usefull primeconstant:

 if SPP(n <= x, [bases]) then n is prime,              HEX(x)
  SPP(           1.373.653, [2, 3]),                   $00000000 0014F5D5
  SPP(           9.080.191, [31, 73]),                 $00000000 008A8D7F
  SPP(          25.326.001  [2, 3, 5]),                $00000000 018271B1
  SPP(       4.759.123.141, [2, 7, 61]),               $00000001 1BAA74C5
  SPP(   1.000.000.000.000, [2, 13, 23, 1662803]),     $000000E8 D4A51000

  SPP(   2.152.302.898.747, [2, 3, 5, 7, 11]),         $000001F5 1F3FEE3B
  SPP(   3.474.749.660.383, [2, 3, 5, 7, 11, 13]),     $00000329 07381CDF
  SPP( 341.550.071.728.321, [2, 3, 5, 7, 11, 13, 17]), $000136A3 52B2C8C1

  http://www.utm.edu/research/primes/glossary/Pseudoprime.html

 a Carmichel, (Bleichenbacher)
 x = 18.215.745.452.589.259.639 * 4.337.082.250.616.490.391 * 867.416.450.123.298.079
   = 68.528.663.395.046.912.244.223.605.902.738.356.719.751.082.784.386.681.071
 is a SPP(x, [2..100]) ->
   X.IsProbablePrime([2, -100]) = true,  a SPP(2 upto  97)
   X.IsProbablePrime([ 101])    = false, a SPP(101)
 but
   X.IsProbablePrime([1, 2])    = false, a SPP(2)-PSW Test is performed


Table of Primecount
                           10                           4
                          100                          25
                        1.000                         168
                       10.000                       1.229
                      100.000                       9.592
                    1.000.000                      78.498
                   10.000.000                     664.579
                  100.000.000                   5.761.455
                1.000.000.000                  50.847.534   
               10.000.000.000                 455.052.511
              100.000.000.000               4.118.054.813
            1.000.000.000.000              37.607.912.018
           10.000.000.000.000             346.065.536.839
          100.000.000.000.000           3.204.941.750.802
        1.000.000.000.000.000          29.844.570.422.669
       10.000.000.000.000.000         279.238.341.033.925
      100.000.000.000.000.000       2.623.557.157.654.233
    1.000.000.000.000.000.000      24.739.954.287.740.860
   10.000.000.000.000.000.000     234.057.667.276.344.607
  100.000.000.000.000.000.000   2.220.819.602.560.918.840
1.000.000.000.000.000.000.000  21.127.269.486.018.731.928
}

unit Prime;

interface
{$I VER.INC}

uses SysUtils, Classes;

type
  TSmallPrimeSieve = class(TInterfacedObject)
  public
    destructor Destroy; override;

    class function MinPrime: Cardinal; // min. Prime, allways 2
    class function MaxPrime: Cardinal; // max. possible Prime, dependend from Compiler Version
    class function MinIndex: Cardinal; // min. Index, allways 1
    class function MaxIndex: Cardinal; // max. Index, see MaxPrime

    function Count(LowerBound, UpperBound: Cardinal): Cardinal; // compute Primecount beetwen Bounds
    function IndexOf(Value: Cardinal; LowerBound: Boolean{$IFDEF VER_D4H} = False{$ENDIF}): Cardinal; // give Primeindex of Value

    procedure LoadCache(const FileName: String); // load a Prime Cache
    procedure BuildCache(const FileName: String; Bound: Cardinal); // create and save a Cache

    property Prime[Index: Cardinal]: Cardinal read GetPrime; default; // return Prime with Index

    property CacheMaxPrime: Cardinal read FCacheMaxPrime; // max. cached Prime
    property CacheMaxIndex: Cardinal read FCacheMaxIndex; // max. cached Index of max. Prime
        // cached min. Values are allways equal to MinPrime, MinIndex
  end;

function Primes: TSmallPrimeSieve;
function IsPrime(Value: Cardinal): Boolean; // fast check if Value is Prime

