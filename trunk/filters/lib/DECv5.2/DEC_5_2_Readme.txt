Delphi Encryption Compendium
Version 5.2 - Part I & II
for Delphi 5 - 2009

Copyright (c) 2006 Hagen Reddmann, HaReddmann [at] t-online [dot] de
Modifications (c) 2008 Arvid Winkelsdorf, digivendo GmbH, info [at] digivendo [dot] de

File: DEC_v5.2.zip

Copyright
---------

Part I is freeware shipping with the sources.

Part II (also known as DECMath) is not available as freeware and it's distribution is 
only allowed for noncommercial or academical use. Please check back with the original 
author regarding any commercial use. Part II is only available in the archives for 
compatibility reasons. It is distributed as precompiled Delphi unit binaries
for D5, D6 and D7.

Contents
--------

\Part_I\            - Sources of Part_I
\Part_I\DECTest\    - Demo for Part_I, functional checks and speed tests of symmetric algorithms

\Archive\           - contains Part_II with interface description, precompiled units for
                      D5, D6, D7, some older demos and LHSZ.pas (only working up to D7)
					  
Part I files
------------

DECUtil.pas         - Basic utilities for DEC
DECHash.pas         - Hash algorithms, TDECHash & derivates
DECHash.inc         - Include file for DECHash.pas (includes asm routines for the hash algorithms)
DECCipher.pas       - Symmetric Encryption, TDECCipher & derivates
DECData.pas         - Lookup tables for DECHash & DECCipher
DECFmt.pas          - Data conversion routines
DECRandom.pas       - Yarrow random number generator (cryptographic secure)

ASN1.pas            - some ASN1 utility functions
CPU.pas             - CPU utility function, detects CPU and CPU speed (useful for profiling)
CRC.pas             - Cyclic redundancy check, supports any CRCs in GF(2)
TypeInfoEx.pas      - Dynamically detect TypeInfo (RTTI) of a given module

Ver.inc             - Include file for compiler dependent compilation

Part I contains the following algorithms
----------------------------------------

8 conversions:

TFormat_HEX             - Hex Uppercase
TFormat_HEXL            - Hex Lowercase
TFormat_MIME32          - MIME Base 32
TFormat_MIME64          - MIME Base 64    
TFormat_PGP             - PGP with PGP-Checksum
TFormat_UU              - UNIX UU Format
TFormat_XX              - UNIX XX Format
TFormat_ESCAPE          - Escaped

26 hash algorithms:

THash_MD2               - MD2  
THash_MD4               - MD4
THash_MD5               - MD5
THash_SHA               - SHA
THash_SHA1              - SHA 1
THash_SHA256            - SHA 256 bit 
THash_SHA384            - SHA 384 bit
THash_SHA512            - SHA 512 bit
THash_Sapphire          - Sapphire
THash_Panama            - Panama
THash_Tiger             - Tiger
THash_RipeMD128         - RIPE MD 128 bit 
THash_RipeMD160         - RIPE MD 160 bit
THash_RipeMD256         - RIPE MD 256 bit
THash_RipeMD320         - RIPE MD 320 bit
THash_Haval128          - Haval 128 Bit
THash_Haval160          - Haval 160 Bit (3 Rounds)
THash_Haval192          - Haval 192 Bit (4 Rounds)
THash_Haval224          - Haval 224 Bit (4 Rounds)
THash_Haval256          - Haval 256 Bit (5 Rounds)
THash_Whirlpool         - Whirlpool
THash_Whirlpool1        - Whirlpool 1
THash_Square            - Square
THash_Snefru128         - Snefru 128 Bit 
THash_Snefru256         - Snefru 256 Bit

The hash algorithms support different types of KDFs (Key Derivation Functions) and MGFs (Mask Generation Functions).
Maximum supported data size is 2^192-1 Bits.

30 symmetric ciphers:

TCipher_Blowfish        - Blowfish
TCipher_Twofish         - Twofish
TCipher_IDEA            - IDEA (attention patented!)
TCipher_Cast256         - Cast 256 
TCipher_Mars            - Mars, IBM 
TCipher_RC4             - RC4, Rivest
TCipher_RC6             - RC6, Rivest
TCipher_Rijndael        - Rijndael, AES Winner 
TCipher_Square          - Square
TCipher_SCOP            - SCOP, fast streamcipher 
TCipher_Sapphire        - Sapphire 
TCipher_1DES            - any DES variant
TCipher_2DES
TCipher_3DES
TCipher_2DDES
TCipher_3DDES
TCipher_3TDES
TCipher_3Way            - 3 Way 
TCipher_Cast128         - Cast 128
TCipher_Gost            - Gost, russian GOV
TCipher_Misty           - Misty
TCipher_NewDES          - NewDES, attention no DES dependencies
TCipher_Q128            - Q128  
TCipher_RC2             - RC2, Rivest 
TCipher_RC5             - RC5, Rivest
TCipher_SAFER           - SAFER & SAFER SK any variants
TCipher_Shark           - Shark
TCipher_Skipjack        - Skipjack 
TCipher_TEA             - TEA, very small
TCipher_TEAN            - TEA new

Symmetric ciphers support the following cipher modes:

CTSx = double CBC, with CFS8 padding of truncated final block
CBCx = Cipher Block Chainung, with CFB8 padding of truncated final block
CFB8 = 8bit Cipher Feedback mode
CFBx = CFB on Blocksize of Cipher
OFB8 = 8bit Output Feedback mode
OFBx = OFB on Blocksize bytes
CFS8 = 8Bit CFS, double CFB
CFSx = CFS on Blocksize bytes
ECBx = Electronic Code Book
