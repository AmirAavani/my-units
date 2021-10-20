unit TemplateEngineUnit;
{$mode ObjFPC}{$H+}

interface
uses
  classes, GenericCollectionUnit;

type

  { TName2ValueMapper }

  TName2ValueMapper = class(TObject)
  private type
    TNameValueMapping = specialize TMap<AnsiString, AnsiString>;
    TNameValuesMapping = specialize TMapSimpleKeyObjectValue<AnsiString, TStringList>;
    TNameCounter = specialize TMap<AnsiString, Integer>;

  private
    NameValueMapping: TNameValueMapping;
    NameValuesMapping: TNameValuesMapping;
    function GetValueByName(aName: AnsiString): AnsiString;
    function GetValuesByName(aName: AnsiString): TStringList;

  private
    property ValueByName[aName: AnsiString]: AnsiString read GetValueByName;
    property ValuesByName[aName: AnsiString]: TStringList read GetValuesByName;

  public

    constructor Create;
    destructor Destroy; override;

    procedure AddNameValue(const Name, Value: AnsiString);
    procedure AddNameValues(const Name: AnsiString; Values: array of AnsiString);
    procedure AddNameValues(const Name: AnsiString; Values: TStringList);
  end;

  { TTemplateEngine }

  TTemplateEngine = class(TObject)
  private
    Text: AnsiString;
    TextLen: Integer;

  public
    constructor Create(const FileName: AnsiString);
    constructor CreateFromText(const InputText: AnsiString);

    function Map(Mapper: TName2ValueMapper): AnsiString; virtual;

  end;

  function EscapeForJavascript(const InputString: AnsiString): AnsiString;
  function EscapeForHTML(const InputString: AnsiString): AnsiString;
  function EscapeForTextAreas(const InputString: AnsiString): AnsiString;
  function EscapeForIframeSrcDoc(const InputString: AnsiString): AnsiString;

implementation
uses
  sysutils, ALoggerUnit;

function EscapeForJavascript(const InputString: AnsiString): AnsiString;
const
  Str: array [0..4] of AnsiString = ('"', #13#10, #10, '"', Chr(34));
  RepStr: array [0..4] of AnsiString = ('\"', '\n', '\n',
  '\"', '\' + Chr(34));

var
  i: Integer;

begin
  Result := InputString;

  for i := 0 to High(Str) do
    if Pos(Str[i], Result) <> 0 then
    begin
      Result := StringReplace(Result, Str[i], RepStr[i], [rfReplaceAll]);
    end;

end;

function EscapeForHTML(const InputString: AnsiString): AnsiString;
const
  Str: array of AnsiString = ('&', ' ', '<', '>', '"', Chr(34), '¢', '£',
  '¥', '€', '©', '®');
  RepStr: array of AnsiString = ('&amp;', '&nbsp;', '&lt;', '&gt;',
  '&quot;', '&apos;', '&cent;', '&pound;', '&yen;', '&euro;', '&copy;', '@reg;');

var
  i: Integer;

begin
  Result := InputString;
  for i := Low(Str) to High(Str) do
    Result := StringReplace(Result, Str[i], RepStr[i], [rfReplaceAll]);

end;

function EscapeForTextAreas(const InputString: AnsiString): AnsiString;
begin
  Result := InputString;

end;

function EscapeForIframeSrcDoc(const InputString: AnsiString): AnsiString;
const
  Str: array of AnsiString = ('&', '"', #13#10, #10);
  RepStr: array of AnsiString = ('&amp;amp;', '&quot;', '', '');

var
  i: Integer;

begin
  Result := InputString;
  for i := Low(Str) to High(Str) do
    Result := StringReplace(Result, Str[i], RepStr[i], [rfReplaceAll]);

end;

{ TName2ValueMapper }

function TName2ValueMapper.GetValueByName(aName: AnsiString): AnsiString;
begin
  if not NameValueMapping.TryGetData(aName, Result) then
    Result := '';

end;

function TName2ValueMapper.GetValuesByName(aName: AnsiString): TStringList;
begin
  if not NameValuesMapping.TryGetData(aName, Result) then
    Result := nil;

end;

constructor TName2ValueMapper.Create;
begin
  inherited;

  NameValueMapping := TNameValueMapping.Create;
  NameValuesMapping := TNameValuesMapping.Create;

end;

destructor TName2ValueMapper.Destroy;
var
  i: Integer;
begin
  NameValueMapping.Free;

  NameValuesMapping.Free;

  inherited Destroy;
end;

procedure TName2ValueMapper.AddNameValue(const Name, Value: AnsiString);
begin
  NameValueMapping.Add(Name, Value);

end;

procedure TName2ValueMapper.AddNameValues(const Name: AnsiString;
  Values: array of AnsiString);
var
  Value: AnsiString;
  Data: TStringList;

begin
  if NameValuesMapping.Find(Name) = nil then
  begin
     NameValuesMapping.Add(Name, TStringList.Create);
     Self.AddNameValues(Name, Values);
     Exit;
  end;

  Data := NameValuesMapping[Name];
  for Value in Values do
    Data.Add(Value);

end;

procedure TName2ValueMapper.AddNameValues(const Name: AnsiString;
  Values: TStringList);
var
  Value: AnsiString;
  Data: TStringList;

begin
  if NameValuesMapping.Find(Name) = nil then
  begin
     NameValuesMapping.Add(Name, TStringList.Create);
     Self.AddNameValues(Name, Values);
     Exit;

  end;

  Data := NameValuesMapping[Name];
  for Value in Values do
    Data.Add(Value);

end;

{ TTemplateEngine }

constructor TTemplateEngine.Create(const FileName: AnsiString);
var
  Lines: TStringList;

begin
  inherited Create;

  Lines := TStringList.Create;
  Lines.LoadFromFile(FileName);
  Text := Lines.Text;
  TextLen := Length(Lines.Text);

  Lines.Free;

end;

constructor TTemplateEngine.CreateFromText(const InputText: AnsiString);
begin
  inherited Create;

  Self.Text := InputText;
  TextLen := Length(Self.Text);
end;

type
  TNameCounterMap = specialize TMap<AnsiString, Integer>;

function TTemplateEngine.Map(Mapper: TName2ValueMapper): AnsiString;
  function AtStart(PC: PChar; Index: Integer): Boolean;
  begin
    if TextLen <= Index + 1 then
      Exit(False);

    Exit((PC^ = '{') and ((PC+1)^ = '{') and ((PC+2)^ = '@'));
  end;

  function AtEnd(PC: PChar; Index: Integer): Boolean;
  begin
    if TextLen <= Index then
      Exit(False);

    Exit((PC^ = '}') and ((PC+1)^ = '}'));
  end;

var
  NameCounterMap: TNameCounterMap;

  function GetValueForName(var PC: PChar; var Index: Integer): AnsiString;
  var
    aName: AnsiString;
    AllValues: TStringList;
    Count: Integer;

  begin
    aName := '';
    Inc(PC, 3);
    Inc(Index, 3);

    while not AtEnd(PC, Index) do
    begin
      aName += PC^;
      Inc(PC);
      Inc(Index);
      if Index = TextLen then
        Exit('');
    end;
    Inc(PC, 2);
    Inc(Index, 2);

    Result := Mapper.ValueByName[aName];
    if Result <> '' then
      Exit;

    AllValues := Mapper.ValuesByName[aName];
    if AllValues = nil then
      Exit;

    Count := -1;
    NameCounterMap.TryGetData(aName, Count);
    Inc(Count);
    if AllValues.Count < Count then
      Result := AllValues[AllValues.Count - 1];
    NameCounterMap.AddOrUpdateData(aName, Count);

  end;

var
  i: Integer;
  PC: PChar;

begin
  NameCounterMap := TNameCounterMap.Create;

  Result := '';
  i := 1;
  PC := @Text[1];
  while i <= TextLen do
  begin
    if AtStart(PC, i) then
    begin
      Result += GetValueForName(PC, i);
      Continue
    end
    else
      Result += PC^;
    Inc(PC);
    Inc(i);
  end;

  NameCounterMap.Free;
end;

end.
