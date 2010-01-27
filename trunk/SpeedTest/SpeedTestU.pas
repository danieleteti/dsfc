unit SpeedTestU;

interface

uses
  SysUtils,
  DBXTransport,
  DbxCompressionFilter, RTTI;

const
  HOW_MANY_TIMES = 1000;

type
  TDSFCSpeedTest = class sealed
  strict private
    class var FNormalStreamSize: Int64;
    class var HASH_FILTERS, CIPHER_FILTERS, COMPRESS_FILTERS: array of string;
    class var DATASTRING: String;
    class procedure SetupDataString;
    procedure Report(const FilterName: String; const InDataSize, OutDataSize: Int64; Milliseconds: Int64);
    procedure AtomicTest(tf: TTransportFilter; out InDataSize, OutDataSize: Int64);

  public
    procedure TestHash;
    procedure TestCipher;
    procedure TestCompress;
    class constructor Create;
  end;

implementation

uses
  Diagnostics,
  HashFilters,
  CipherFilters,
  CompressFilters,
  IOUtils;

{ TDSFCSpeedTest }

procedure TDSFCSpeedTest.AtomicTest(tf: TTransportFilter; out InDataSize, OutDataSize: Int64);
var
  bytes: TBytes;
begin
  InDataSize := ByteLength(DATASTRING);
  bytes := tf.ProcessInput(BytesOf(DATASTRING));
  OutDataSize := Length(bytes);
  tf.ProcessOutput(bytes);
end;

class constructor TDSFCSpeedTest.Create;
begin
  SetLength(HASH_FILTERS, 4);
  HASH_FILTERS[0] := 'MD5';
  HASH_FILTERS[1] := 'MD4';
  HASH_FILTERS[2] := 'SHA1';
  HASH_FILTERS[3] := 'SHA512';

  SetLength(CIPHER_FILTERS, 4);
  CIPHER_FILTERS[0] := 'Blowfish';
  CIPHER_FILTERS[1] := 'Rijndael';
  CIPHER_FILTERS[2] := '3TDES';
  CIPHER_FILTERS[3] := '3DES';

  SetLength(COMPRESS_FILTERS, 2);
  COMPRESS_FILTERS[0] := 'LZO';
  COMPRESS_FILTERS[1] := 'ZLibCompression';

  SetupDataString;
end;

procedure TDSFCSpeedTest.Report(const FilterName: String; const InDataSize, OutDataSize: Int64; Milliseconds: Int64);
begin
  Writeln(Format('%-16s=%5dms (stream size: %5d byte, filtered stream size: %5d byte)', [FilterName, Milliseconds,
      InDataSize, OutDataSize]));
end;

class procedure TDSFCSpeedTest.SetupDataString;
begin
  DATASTRING := 'Nel mezzo del cammin di nostra vita' + sLineBreak + 'mi ritrovai per una selva oscura' + sLineBreak +
    'ché la diritta via era smarrita.' + sLineBreak + '  Ahi quanto a dir qual era è cosa dura' + sLineBreak +
    'esta selva selvaggia e aspra e forte' + sLineBreak + 'che nel pensier rinova la paura!' + sLineBreak +
    'Tant''è amara che poco è più morte;' + sLineBreak + 'ma per trattar del ben ch''i'' vi trovai,' + sLineBreak +
    'dirò de l''altre cose ch''i'' v''ho scorte.' + sLineBreak + 'Io non so ben ridir com''i'' v''intrai,' +
    sLineBreak + 'tant''era pien di sonno a quel punto' + sLineBreak + 'che la verace via abbandonai.' + sLineBreak +
    '  Ma poi ch''i'' fui al piè d''un colle giunto,' + sLineBreak + 'là dove terminava quella valle' + sLineBreak +
    '	che m''avea di paura il cor compunto,' + sLineBreak + '  guardai in alto, e vidi le sue spalle' + sLineBreak +
    'vestite già de'' raggi del pianeta' + sLineBreak + 'che mena dritto altrui per ogne calle.' + sLineBreak +
    '  Allor fu la paura un poco queta' + sLineBreak + '	che nel lago del cor m''era durata' + sLineBreak +
    'la notte ch''i'' passai con tanta pieta.' + sLineBreak + '  E come quei che con lena affannata' + sLineBreak +
    'uscito fuor del pelago a la riva' + sLineBreak + 'si volge a l''acqua perigliosa e guata,' + sLineBreak +
    '	  così l''animo mio, ch''ancor fuggiva,' + sLineBreak + 'si volse a retro a rimirar lo passo' + sLineBreak +
    'che non lasciò già mai persona viva.' + sLineBreak + '  Poi ch''èi posato un poco il corpo lasso,' + sLineBreak +
    'ripresi via per la piaggia diserta,' + sLineBreak + '	sì che ''l piè fermo sempre era ''l più basso.' +
    sLineBreak + '  Ed ecco, quasi al cominciar de l''erta,' + sLineBreak + 'una lonza leggera e presta molto,' +
    sLineBreak + 'che di pel macolato era coverta;' + sLineBreak + 'e non mi si partia dinanzi al volto,' +
    sLineBreak + 'anzi ''mpediva tanto il mio cammino,' + sLineBreak + 'ch''i'' fui per ritornar più volte vòlto.';
  DATASTRING := DATASTRING + DATASTRING + DATASTRING;
  FNormalStreamSize := ByteLength(DATASTRING);
end;

procedure TDSFCSpeedTest.TestCipher;
var
  tf: TTransportFilter;
  FilterName: string;
  sw: TStopwatch;
  I: Integer;
  InDataSize, OutDataSize: Int64;
  ctx: TRTTIContext;
begin
  sw := TStopwatch.Create;
  for FilterName in CIPHER_FILTERS do
  begin
    tf := TTransportFilterFactory.CreateFilter(FilterName);
    try
      sw.Start;
      for I := 1 to HOW_MANY_TIMES do
        AtomicTest(tf, InDataSize, OutDataSize);
      sw.Stop;
      Report(FilterName, InDataSize, OutDataSize, sw.ElapsedMilliseconds);
      sw.Reset;
    finally
      tf.Free;
    end;
  end;
end;

procedure TDSFCSpeedTest.TestCompress;
var
  tf: TTransportFilter;
  FilterName: string;
  sw: TStopwatch;
  I: Integer;
  InDataSize, OutDataSize: Int64;
begin
  sw := TStopwatch.Create;
  for FilterName in COMPRESS_FILTERS do
  begin
    tf := TTransportFilterFactory.CreateFilter(FilterName);
    try
      sw.Start;
      for I := 1 to HOW_MANY_TIMES do
        AtomicTest(tf, InDataSize, OutDataSize);
      sw.Stop;
      Report(FilterName, InDataSize, OutDataSize, sw.ElapsedMilliseconds);
      sw.Reset;
    finally
      tf.Free;
    end;
  end;
end;

procedure TDSFCSpeedTest.TestHash;
var
  tf: TTransportFilter;
  FilterName: string;
  sw: TStopwatch;
  I: Integer;
  InDataSize, OutDataSize: Int64;
begin
  sw := TStopwatch.Create;
  for FilterName in HASH_FILTERS do
  begin
    tf := TTransportFilterFactory.CreateFilter(FilterName);
    try
      sw.Start;
      for I := 1 to HOW_MANY_TIMES do
        AtomicTest(tf, InDataSize, OutDataSize);
      sw.Stop;
      Report(FilterName, InDataSize, OutDataSize, sw.ElapsedMilliseconds);
      sw.Reset;
    finally
      tf.Free;
    end;
  end;
end;

end.
