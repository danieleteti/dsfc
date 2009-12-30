unit TestFiltersU;

interface

uses
  TestFramework,
  SysUtils;

type
  TFiltersTest = class(TTestCase)
  private
  var
    DATASTRING: string;
  public
    procedure Setup; override;
    procedure TestFilterByName(const FilterName: String; CryptKey: string = '');
  published
    { HASH FILTERS }
    procedure TestMD5;
    procedure TestMD4;
    procedure TestSHA1;
    procedure TestSHA512;
    { CIPHER FILTERS }
    procedure TestBlowfish;
    procedure TestRijndael;
    procedure Test3TDES;
    procedure Test3DES;
    { COMPRESS FILTERS }
    procedure TestLZO;
  end;

implementation

uses
  HashFilters,
  DBXTransport, CipherFilters;
{ TFiltersTest }

procedure TFiltersTest.Setup;
begin
  inherited;
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
    sLineBreak + 'che di pel macolato era coverta;';
end;

procedure TFiltersTest.TestMD5;
begin
  TestFilterByName('MD5');
end;

procedure TFiltersTest.TestRijndael;
begin
  TestFilterByName('Rijndael', 'm');
  TestFilterByName('Rijndael', 'my_crypt_key');
  TestFilterByName('Rijndael', 'my_crypt_keymy_crypt_key');
end;

procedure TFiltersTest.TestSHA1;
begin
  TestFilterByName('SHA1');
end;

procedure TFiltersTest.TestSHA512;
begin
  TestFilterByName('SHA512');
end;

procedure TFiltersTest.Test3DES;
begin
  TestFilterByName('3DES', 'm');
  TestFilterByName('3DES', 'my_crypt_key');
  TestFilterByName('3DES', 'my_crypt_keymy_crypt_key');
end;

procedure TFiltersTest.Test3TDES;
begin
  TestFilterByName('3TDES', 'm');
  TestFilterByName('3TDES', 'my_crypt_key');
  TestFilterByName('3TDES', 'my_crypt_keymy_crypt_key');
end;

procedure TFiltersTest.TestBlowfish;
begin
  TestFilterByName('Blowfish', 'm');
  TestFilterByName('Blowfish', 'my_crypt_key');
  TestFilterByName('Blowfish', 'my_crypt_keymy_crypt_key');
end;

procedure TFiltersTest.TestFilterByName(const FilterName: String; CryptKey: string = '');
var
  tf: TTransportFilter;
begin
  tf := TTransportFilterFactory.CreateFilter(FilterName);
  try
    CheckNotNull(tf, 'TTransportFilterFactory doens''t know filter: ' + FilterName);
    if CryptKey <> '' then
    begin
      CheckEquals(1, length(tf.Parameters));
      CheckEquals('default_crypt_key', tf.GetParameterValue(CRYPT_KEY_PROPERTY_NAME));
      tf.SetParameterValue('crypt_key', CryptKey);
    end
    else
      CheckEquals(0, length(tf.Parameters), FilterName);
    CheckEquals(DATASTRING, StringOf(tf.ProcessOutput(tf.ProcessInput(BytesOf(DATASTRING)))), FilterName);
    CheckEquals(DATASTRING + DATASTRING, StringOf(tf.ProcessOutput(tf.ProcessInput(BytesOf(DATASTRING + DATASTRING)))),
      FilterName);
    CheckEquals(Copy(DATASTRING, 1, 100), StringOf(tf.ProcessOutput(tf.ProcessInput(BytesOf(Copy(DATASTRING, 1, 100)))))
        , FilterName);
    CheckEquals(Copy(DATASTRING, 1, 1), StringOf(tf.ProcessOutput(tf.ProcessInput(BytesOf(Copy(DATASTRING, 1, 1))))),
      FilterName);
    CheckEquals('', StringOf(tf.ProcessOutput(tf.ProcessInput(BytesOf('')))), FilterName);
  finally
    tf.Free;
  end;
end;

procedure TFiltersTest.TestLZO;
begin
  TestFilterByName('LZO');
end;

procedure TFiltersTest.TestMD4;
begin
  TestFilterByName('MD4');
end;

initialization

RegisterTest(TFiltersTest.Suite);

end.
