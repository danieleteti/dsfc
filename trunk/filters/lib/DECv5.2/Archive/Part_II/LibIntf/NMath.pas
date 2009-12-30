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

unit NMath;           

interface

uses SysUtils;

const                           
// IPool.Flags
  pfSecure   =  $80;      // wipe, e.g. zerofill memoryspace

type
// user defined periodical callback, called from inside of calculation code
  IPoolNotify = interface
    ['{126BE0F8-061D-4067-9E0A-E2A490AF5CEA}']
    function DoIdle: Boolean;
  end;

// stack management, speedup 10% IIntegers
  IPool = interface
    ['{126BE0F0-061D-4067-9E0A-E2A490AF5CEA}']
    function Clear: Boolean;
    function SetLimit(Count: Cardinal = 64; Size: Cardinal = $10000): Boolean;
    function SetFlags(Flags: Cardinal): Cardinal;
    function GetFlags: Cardinal;
    function CurCount: Cardinal;
    function MaxCount: Cardinal;
    function CurSize: Cardinal;
    function MaxSize: Cardinal;
    function ThreadID: Cardinal;

    function SetIdle(const Notify: IPoolNotify = nil; Delay: Cardinal = 100): IPoolNotify;
    function GetIdle(out Notify: IPoolNotify; out Delay: Cardinal): Boolean;
  end;

// direct access to one-dimensional Interfaces such as IIntegers
  IDigitAccess = interface
    ['{126BE000-061D-4067-9E0A-E2A490AF5CEA}']
    function Count: Integer;
    function GetDigit(Index: Integer): Cardinal;
    function Alloc(Count: Integer): Pointer;
    function Digits: Pointer;
    procedure SetDigits(Digits: Pointer; Count: Integer);
    procedure Normalize;
  end;

  EMath        = class(Exception);
  EDigitAccess = class(EMath);
  EMathAbort   = class(EAbort);

function  IsEqualGUID(const GUID1, GUID2: TGUID): Boolean;
function  NPool(ThreadID: Cardinal = 0): IPool;
procedure NCalcCheck;
// make entry unique
procedure NUnique(var Entry);
// add Intf to release it in same time with the internal threaded stack pool
procedure NAutoRelease(const Intf: IUnknown);


{ remarks: How work Stacks/Queue here ?

   The stacks here use "backpropagation" and two linked list of available Entry Types.
   1. TStackEntry
   2. TRefEntry

   TStackEntry manage a Pointer of a long memorychunk with preallocation. Most cases
   this memory chunk is overallocated to avoid to many calls to the Memorymanager
   if we need more space. Such entries are used for a single Interface such as IInteger.
   The TStack manage a linked list of available (freed/unused) TStackEntry's. This
   list use a FILO Method (Queue), means First In Last Out, because the large allocated
   memorychunk ISN'T automatically freed. Instead we preserve it for the next needed
   allocation of a TStackEntry. With a FILO Stack the Entries itself "rotate" into
   this list and so on repeated allocations/deallocations each preserved Entry becomes
   the same probabilty of use. So we avoid efficiently follow situation:

     On the Stack are 4 Enries preserved, each with 1024 Bytes memorychunk. The maximal
     memory space that manage the Stack is 4 * 1024 bytes. The next 5. entry that should
     preserved on this stack must be now free it memoryspace, because the preserved
     space excide 4 * 1024 bytes. Now let us assume a FIFO Stack, then the next allocated
     Stack entry is number 5. The memorychunk is freed. The outer procs must reallocate
     this chunk, play with it and frees again this 5. entry. The Stackhandler must now again
     free this prior allocated Memorychunk and insert 5. entry to stacklist as entry #5.
     We see such a FIFO Stack preserve now 4096 unused bytes and is rater inefficent,
     because it use on repeated reallocation always a bad entry.

     A FILO Stack gets back the first allocated entry no 1. and put this entry after
     deallocation as no 5. The next allocated entry is no 2 and so on. Means the
     allocated memorychunk of each entry is better used.

  TStack supports limit for preserved Stack Entries. One limit of count of entries into
  linked list and one limit of memoryusage of all preserved entries.
  If the current count of StackEntry on deallocation excide the MaxCount then this
  entry is fully freed, include the memorychunk.
  If the current memorysize excide then only the full memorychunk is released but
  the stackentry is inserted to the list.

  TRefEntry are now a Interface with upto 6 Interface members of type TStackEntry,
  and some additional infos into reserved Cardinal fields.
  TRefEntry are only limitated per MaxCount.
  TRefEntry use a FIFO linked list, because these are of fixed sizes and
  contains no additional memorychunk.

  All important operation (insertion, limit checks, stack allocation) are done
  on Entry-deallocation and for each possible thread into a own stack. Each
  such allocated thread stack is linked into a list of all process stacks. So we
  can on termination destroy ALL stacks. If a Thread don't use IIntegers or other
  objects from this library then NO stack exists.

  IMPORTANT ! each Thread that want to use IIntegers must at least
  free the stack by a call to NPool.Clear, otherwise all reallocated Stackentries
  are first on processtermination destroyed.
  That's the ONLY one bad point of the design, but I don't known a realy better
  alternative for this. Some methods of hooking are to inpredictable, unsafe or
  complicated. I think about an additional method of checking if a Thread-Stack
  is valid, and if not we free it. Then we could periodical call this function.
  This should be easily adapted in above code.

  If the limits of a stack are set to 0,0 then only the stack itself are allocated.
  That could be an additional alternative for threads, but then we lost about >10
  percent speed. (ie. for GF(p) we speedup with stacks 20% !!!)

  The third task for the TStack are the Calculation Management. Here we apply a
  asynchrone callback to a userdefined interface. TStack contains now
  all this needed stuff. We can abort any calculation.
  Both features are important if we known how long a computation of PI to 1 million
  decimals digits take, and MUL, DIV and so on...

  This calculation design avoid some disadvantages if we must periodicaly call
  a userdefined procedure, eg. Application.PrcessMessages. Then this would be
  to many times called and reduce dramatical the runtime speed. 

  To use such a callback use the IPoolNotify Interface. For each thread only ONE
  such a callback can be installed, but we can preserve the current installed one.

  as example

  type
    TMyForm = class(TForm, IPoolNotify)
    private
      FMustAbort: Boolean;

      function DoIdle: Boolean;
      procedure DoCalc;
    end;

  function TMyForm.DoIdle: Boolean;
  begin
    Result := not FMustAbort;
    Application.ProcessMessages;
  end;

  procedure TMyForm.DoCalc;
  var
    Notify: IPoolNotify;
  begin
    with NPool do
    try
      Notify := SetIdle(Self);
      SetLimit
      ... compuation

    finally
      Clear;
      SetIdle(Notify);

// or to uninstall all
      SetIdle;
      
    end;
  end;

  .DoCalc can be now recursively called, or intercept another outer calculation
  with its own callback. But only the last one can be active.

  Hm thats all, i hope i could it understandable write with my bad english :)
}

