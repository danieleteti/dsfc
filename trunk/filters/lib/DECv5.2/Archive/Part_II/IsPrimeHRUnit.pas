unit IsPrimeHRUnit;

interface

function IsPrime(N: Cardinal): Boolean;

implementation

uses SysUtils;

{ Copyright c 2003 by Hagen Reddmann, public domain. }

// Tablesize = 160 bytes
const
  MaxPrime = 137;
  MaxTrial = MaxPrime * MaxPrime; // maximal trial div bounds
  MinPrime = 137;                 // trial div bounds if N >= MaxTrial, must be <= MaxPrime
  Primes: array[0..31] of Byte =
   (   3,   5,   7,  11,  13,  17,  19,  23,  29,  31,  37,  41,  43,  47,  53,  59,
      61,  67,  71,  73,  79,  83,  89,  97, 101, 103, 107, 109, 113, 127, 131, 137);

  InvPrimes: array[0..31] of Cardinal =  // InvPrimes[i] = Primes[i]^-1 mod 2^32
   ($AAAAAAAB,$CCCCCCCD,$B6DB6DB7,$BA2E8BA3,$C4EC4EC5,$F0F0F0F1,$286BCA1B,$E9BD37A7,
    $4F72C235,$BDEF7BDF,$914C1BAD,$C18F9C19,$2FA0BE83,$677D46CF,$8C13521D,$A08AD8F3,
    $C10C9715,$07A44C6B,$E327A977,$C7E3F1F9,$613716AF,$2B2E43DB,$FA3F47E9,$5F02A3A1,
    $7C32B16D,$D3431B57,$8D28AC43,$DA6C0965,$0FDBC091,$EFDFBF7F,$C9484E2B,$077975B9);

function IsPrime(N: Cardinal): Boolean;
// Montgomery Version of IsPrimeHR_IA32_1()
asm
       TEST   AL,1
       JZ     @@0                          // even ??
       CMP    EAX,7
       JA     @@1                          // > 7 ??
       DEC    EAX
       SETNZ  AL
       RET

@@0:   CMP    EAX,2                        // even numbers
       SETZ   AL
       RET

@@1:   PUSH   EBP                          // do trial divsion to small primes
       PUSH   EBX
       PUSH   ESI
       PUSH   EDI

       MOV    EBX,EAX
       CMP    EAX,MaxTrial
       MOV    EBP,MinPrime
       JAE    @@2

       PUSH   EAX
       FILD   DWord Ptr [ESP]
       FSQRT
       FISTP  DWord Ptr [ESP]
       POP    EBP                          // EBP = Sqrt(N)

@@2:   MOV    EDI,Offset Primes
       MOV    ESI,Offset InvPrimes
       XOR    ECX,ECX
@@3:   MOVZX  EDX,Byte Ptr [EDI + ECX]     // take care, InvPrimes[] MUST
                                           // be after Primes[] declared
       MOV    EAX,EBX
       CMP    EDX,EBP
       JA     @@5
       IMUL   EAX,[ESI + ECX * 4]
       INC    ECX
       MUL    EDX
       JC     @@3
       TEST   EDX,EDX
@@4:   POP    EDI
       POP    ESI
       POP    EBX
       POP    EBP
       SETNZ  AL
       RET

@@5:   CMP    EBX,MaxTrial                 // N <= MaxPrime^2 ??
       MOV    EAX,EBX
       JBE    @@4

       IMUL   EAX,EBX                      // compute domain param U = -N^-1 mod 2^32
       SUB    EAX,2                        // Lookuptable can reduce from 72 donwto 32 cycles
       IMUL   EAX,EBX
       MOV    EDX,EAX
       IMUL   EAX,EBX
       ADD    EAX,2
       IMUL   EAX,EDX
       MOV    EDX,EAX
       IMUL   EAX,EBX
       ADD    EAX,2
       IMUL   EAX,EDX
       MOV    EBP,EAX
       IMUL   EBP,EBX
       ADD    EBP,2
       IMUL   EBP,EAX                      // U = -N^-1 mod^2^32 in EBP

       MOV    EDI,EBX
       MOV    EAX,EBX
       DEC    EDI                          // N -1
       NEG    EAX
       BSF    ECX,EDI
       MUL    EAX
       PUSH   ECX                          // bits remain         [ESP + 20]
       MOV    ESI,EAX
       BSR    ECX,EDI
       MOV    EAX,EDX
       XOR    EDX,EDX
       NEG    ECX
       DIV    EBX                          // div, can't be removed
       MOV    EAX,ESI
       ADD    ECX,32
       DIV    EBX                          // div
       SHL    EDI,CL
       MOV    EAX,EDX
       IMUL   EAX,EBP
       MOV    ESI,EDX                      // C = -N^2 mod N, to fast convert into
       MUL    EBX                          // montgomery domain
       PUSH   ESI                          // C                    [ESP + 16]
       ADD    EAX,ESI
       ADC    EDX,0

       PUSH   EDX                          // +1 in montgomery     [ESP + 12]
       PUSH   EDI                          // bit mask exponent    [ESP +  8]
       NEG    EDX
       ADD    EDX,EBX

       CMP    EBX,$08A8D7F                 // N < $08A8D7F, do SPP to bases 31,73
       PUSH   EDX                          // -1 in montgomery     [ESP +  4]
       JAE    @@6
       MOV    EAX,31
       CALL   @@9
       MOV    EAX,73
       PUSH   Offset @@7
       JMP    @@9

@@6:   MOV    EAX,2                        // do SPP to bases 2,7,61
       CALL   @@9
       MOV    EAX,7
       CALL   @@9
       MOV    EAX,61
       CALL   @@9

@@7:   INC    EAX
@@8:   LEA    ESP,[ESP + 4 * 5]            // don't change flags !!
       JMP    @@4

@@9:   MUL    DWord Ptr [ESP + 16]         // convert base in montgomery
       MOV    EDI,EAX                      // Base' = Base * C mod N
       IMUL   EAX,EBP                      // montgomery REDC
       MOV    ESI,EDX
       MUL    EBX
       ADD    EAX,EDI
       ADC    EDX,ESI
       MOV    ECX,[ESP + 8]                // bit mask of exponent N -1
       MOV    EAX,EDX
       PUSH   EDX

@@A:   MUL    EAX                          // X = X^2 mod N
       MOV    EDI,EAX
       IMUL   EAX,EBP
       MOV    ESI,EDX
       MUL    EBX
       ADD    EAX,EDI
       ADC    EDX,ESI
       JNC    @@B
       SUB    EDX,EBX
@@B:   ADD    ECX,ECX
       MOV    EAX,EDX
       JNC    @@D
       MUL    DWord Ptr [ESP]              // X = X * Base mod N
       MOV    EDI,EAX
       IMUL   EAX,EBP
       MOV    ESI,EDX
       MUL    EBX
       ADD    EAX,EDI
       ADC    EDX,ESI
       JNC    @@C
       SUB    EDX,EBX
@@C:   TEST   ECX,ECX
       MOV    EAX,EDX
@@D:   JNZ    @@A
       CMP    EAX,EBX
       JB     @@E
       SUB    EAX,EBX
@@E:   CMP    EAX,[ESP + 16]               // == +1 ??
       MOV    ECX,[ESP + 24]               // bits remain
       JE     @@J
@@F:   CMP    EAX,[ESP + 8]                // == -1 ??
       JE     @@J
       DEC    ECX
       JNG    @@I
       MUL    EAX
       MOV    EDI,EAX
       IMUL   EAX,EBP
       MOV    ESI,EDX
       MUL    EBX
       ADD    EAX,EDI
       ADC    EDX,ESI
       JC     @@G
       CMP    EDX,EBX
       JB     @@H
@@G:   SUB    EDX,EBX
@@H:   CMP    EDX,[ESP + 16]               // <> +1 ??
       MOV    EAX,EDX
       JNE    @@F
@@I:   ADD    ESP,8
       XOR    EAX,EAX
       JMP    @@8
@@J:   POP    EDX
end;

end.
