# Datasnap Filters Compendium

##What’s Datasnap Filters Compendium
Datasnap Filters Compendium (DSFC) is a compendium of 9 filters for Datasnap 2010 now updated to Delphi XE4 Update1. The filters are divided into 3 groups:

###HASH
- MD5
- MD4
- SHA1
- SHA512

###CIPHER
- Blowfish
- Rijndael
- 3TDES
- 3DES

###COMPRESS
- LZO

##HASH filters
The HASH filters helps avoid to any spiteful person to modify datasnap message through an “Man in the middle” attack (http://en.wikipedia.org/wiki/Man-in-the-middle_attack). Functioning is basing on an easy principle. After sending the message, the filter calculates the hash of the message and tags along it to the message. When the message gets to destination, the filter recovers the hash calculated by the client and recalculates it on the remaining part of the message. If the part of the extrated hash (calculated at the beginning) and the hash recalculated to the end are equal, the message hasn’t change. To avoid someone could modify the message and also recalculates the hash, after calculating the hash, a GUID is tagged along to the message, which just the sender and the receiver know. This kind of filters DOES NOT AVOID THE UNAVOIDED READING OF DATA,it avoids just the modification.

##CIPHER filters
The CIPHER filters are the most interesting filters. Many datasnap users have requested a built-in system to get the data transmission safe. This set of filters colud be the answer. In the actual version I’ve implemented Symmetric-key algorithms. Maybe I’m going to develop of Asymmetric-key algorithms filters. Implemented algorithms features are well known, I list them as follows just to be completed:

Blowfish Blowfish has a 64-bit block size and a variable key length from 32 up to 448 bits. The filter version has a keysize = 56 byte.

Rijndael AES Round 2. AES has a fixed block size of 128 bits and a key size of 128, 192, or 256 bits, whereas Rijndael can be specified with block and key sizes in any multiple of 32 bits, with a minimum of 128 bits and a maximum of 256 bits. The filter version has a keysize = 32 byte.

3TDES Triple DES with 24 byte Blocksize, 24 byte Keysize 168 bits relevant The filter version has a keysize = 24 byte.

3DES Triple DES with 8 byte Blocksize, 24 byte Keysize 168 bits relevant The filter version has a keysize = 24 byte.

##COMPRESS filters
Actually the LZO compression is the only one that exists, and is one of the faster compression algorithms. The compression ratio compared to the ZLib is worse but about 3 times faster.


##What about tests?
DSFC has a huge suite of unit tests and speed tests.
The speed tests show how filters are fast and how the data stream size is affected by their work.



####Read more at http://www.danieleteti.it/?p=168
