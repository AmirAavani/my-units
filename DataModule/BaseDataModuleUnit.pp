unit BaseDataModuleUnit;

{$mode objfpc}{$H+}

interface

uses
  DBConnectorUnit, QueryResponeUnit, ValueUnit, Classes, SysUtils, fgl;

type
  { EInvalidColumnName }

   EInvalidColumnName = class(Exception)
   public
     constructor Create(ColumnName: AnsiString);
   end;

   { EInvalidColumnIndex }

   EInvalidColumnIndex = class(Exception)
   public
     constructor Create(ColumnIndex, ColumnCount: Integer);
   end;

  { TBaseDataModule }

  TBaseDataModule = class(TObject)
  protected type
    EUnsupportedDataType = class(Exception);

    { TDMValue }

    TDMValue = class(TValue)
    private
      function GetAsMySQL: AnsiString;
    protected
      function GetAsAnsiString: AnsiString; override;
      function GetAsBoolean: Boolean;  override;
      function GetAsExtended: Extended; override;
      function GetAsInteger: Int64; override;
      function GetAsUInteger: uInt64; override;


    public
      property AsMySQL: AnsiString read GetAsMySQL;

    end;
    TDMValues = specialize TFPGList<TDMValue>;

  protected
    FValues: TDMValues;

    function GetValueByIndex(Index: Integer): TDMValue;
    function FillFromResponse(Row, Column: TStringList): Boolean; virtual;
    procedure SetValueByColumnName(ColumnName: AnsiString; StrValue: AnsiString); virtual;
    procedure SetValueByColumnIndex(ColumnIndex: Integer; StrValue: AnsiString); virtual;
    function GetInsertQuery: AnsiString; virtual;

  public
    property ValueByIndex[Index: Integer]: TDMValue read GetValueByIndex;

    class function TableName: AnsiString; virtual; abstract;
    class function NumFields: Integer; virtual; abstract;
    class function ColumnNameByIndex(Index: Integer): AnsiString; virtual; abstract;
    class function MySQLColumnTypeByIndex(Index: Integer): AnsiString; virtual; abstract;
    class function FPCColumnTypeByIndex(Index: Integer): AnsiString; virtual; abstract;
    class function ColumnIndexByName(const aName: AnsiString): Integer; virtual; abstract;

    function GetDataByIndex(Index: Integer): TDMValue; virtual;

    function Save(DB: TDatabaseConnection): Boolean; virtual;

    function ToString: AnsiString; override;
    constructor Create(_NumFields: Integer);
    destructor Destroy; override;
  end;


  { TBaseDataList }

  generic TBaseDataList<TData> = class(specialize TFPGList<TData>)
  public
    procedure PrintAll; virtual;

    destructor Destroy; override;
  end;

  { TBaseDataModuleManager }

   generic TBaseDataModuleManager<TData> =  class(TObject)
   public type
     TDataList = specialize TBaseDataList<TData>;

   private

   protected
     DB: TDatabaseConnection;
     CS: TRTLCriticalSection;

     procedure Lock; virtual;
     procedure Unlock; virtual;
   public
    // TBaseDataModule will not free DBConnection object.
    constructor Create(aBD: TDatabaseConnection);
    destructor Destroy; override;

    // Returns all the elements in aResponse.
    function ExtractFromResponse(aResponse: TQueryResponse; MaxReturnedResult: Integer = -1): TDataList; virtual;
    // Retruns all Data satisfying the query.
    function GetAllWhere(WhereClause: AnsiString; MaxReturnedResult: Integer = -1): TDataList; virtual;

  end;

procedure ToInt(Source: AnsiString; Target: Pointer);
procedure ToString(Source: AnsiString; Target: Pointer);

implementation

uses
  ALoggerUnit, StringUnit;

procedure ToInt(Source: AnsiString; Target: Pointer);
begin
  PInteger(Target)^ := StrToInt(source);

end;

procedure ToString(Source: AnsiString; Target: Pointer);
begin
  PString(Target)^ := Source;

end;

{ TBaseDataModule.TDMValue }

function TBaseDataModule.TDMValue.GetAsMySQL: AnsiString;
begin
  if Self = nil then
    Exit('NULL');

  case Self.InputType of
    itBoolean:
      Result := BoolToStr(AsBoolean);
    itInteger:
      Result := IntToStr(AsInteger);
    itUInteger:
      Result := IntToStr(AsUInteger);
    itExtended:
      Result := FloatToStr(AsExtended);
    itAnsiString:
      Result := Format('"%s"', [AsAnsiString])
  end;

end;

function TBaseDataModule.TDMValue.GetAsAnsiString: AnsiString;
begin
  if Self = nil then
    Exit('');

  Result:= inherited GetAsAnsiString;
end;

function TBaseDataModule.TDMValue.GetAsBoolean: Boolean;
begin
  if Self = nil then
    Exit(False);

   Result:= inherited GetAsBoolean;
end;

function TBaseDataModule.TDMValue.GetAsExtended: Extended;
begin
  if Self = nil then
    Exit(0.0);

  Result:= inherited GetAsExtended;
end;

function TBaseDataModule.TDMValue.GetAsInteger: Int64;
begin
  if Self = nil then
    Exit(0);

  Result:= inherited GetAsInteger;
end;

function TBaseDataModule.TDMValue.GetAsUInteger: uInt64;
begin
  if Self = nil then
    Exit(0);

  Result:= inherited GetAsUInteger;
end;

{ EInvalidColumnIndex }

constructor EInvalidColumnIndex.Create(ColumnIndex, ColumnCount: Integer);
begin
  inherited Create(Format('ColumnIndex(%d) must be in range [%d, %d)',
    [ColumnIndex, 0, ColumnCount - 1]));
end;

{ TBaseDataList }

procedure TBaseDataList.PrintAll;
var
  Obj: TData;

begin
  for Obj in Self do
    WriteLn(Obj.ToString);

end;

destructor TBaseDataList.Destroy;
var
  Obj: TData;
begin
  for Obj in Self do
    Obj.Free;

  inherited Destroy;
end;

{ EInvalidColumnName }

constructor EInvalidColumnName.Create(ColumnName: AnsiString);
begin
  inherited Create(Format('Column "%s" has not found', [ColumnName]));

end;

{ TBaseDataModuleManager }

function TBaseDataModuleManager.GetAllWhere(WhereClause: AnsiString;
  MaxReturnedResult: Integer): TDataList;
var
  Query: AnsiString;
  Response: TQueryResponse;

begin
  Query := Format('SELECT * FROM %s WHERE %s', [TData.TableName, WhereClause]);
  DebugLn(Format('Q: %s', [Query]));
  Response := DB.RunQuery(Query);

  Result := ExtractFromResponse(Response, MaxReturnedResult);

  Response.Free;

end;

procedure TBaseDataModuleManager.Lock;
begin
  EnterCriticalsection(CS);
end;

procedure TBaseDataModuleManager.Unlock;
begin
  LeaveCriticalsection(CS);
end;

constructor TBaseDataModuleManager.Create(aBD: TDatabaseConnection);
begin
  inherited Create;

  DB := aBD;

  InitCriticalSection(CS);

end;

destructor TBaseDataModuleManager.Destroy;
begin
  DoneCriticalsection(CS);

  inherited Destroy;
end;

function TBaseDataModuleManager.ExtractFromResponse(aResponse: TQueryResponse;
  MaxReturnedResult: Integer): TDataList;
var
  Obj: TData;
  i: Integer;

begin
  Result := TDataList.Create;

  for i := 1 to aResponse.NumRows do
  begin
    Obj := TData.Create;
    Obj.FillFromResponse(aResponse.Row, aResponse.Columns);
    Result.Add(Obj);

  end;
end;

{ TBaseDataModule }

function TBaseDataModule.GetValueByIndex(Index: Integer): TDMValue;
begin
  Result := FValues[Index];
end;

function TBaseDataModule.FillFromResponse(Row, Column: TStringList): Boolean;
var
  i: Integer;

begin
  Result := True;

  for i := 0 to Row.Count - 1 do
    Self.SetValueByColumnName(Column[i], Row[i]);

end;

procedure TBaseDataModule.SetValueByColumnName(ColumnName: AnsiString;
  StrValue: AnsiString);
begin
  SetValueByColumnIndex(ColumnIndexByName(ColumnName), StrValue);

end;

procedure TBaseDataModule.SetValueByColumnIndex(ColumnIndex: Integer;
  StrValue: AnsiString);
begin
  FValues[ColumnIndex].UpdateValue(StrValue);

end;

function TBaseDataModule.GetInsertQuery: AnsiString;
var
  i: Integer;
  Names, Values: TStringList;

begin

  Names := TStringList.Create;
  Values  := TStringList.Create;
  for i := 0 to Self.NumFields - 1 do
  begin
    Names.Add(ColumnNameByIndex(i));
    Values.Add(ValueByIndex[i].AsMySQL);
  end;
  Result := Format('INSERT INTO %s(%s) VALUE(%s)', [Self.TableName,
    JoinStrings(Names, ','), JoinStrings(Values, ',')]);

  Names.Free;
  Values.Free;

end;

function TBaseDataModule.GetDataByIndex(Index: Integer): TDMValue;
begin
  Result := FValues[Index];

end;

function TBaseDataModule.Save(DB: TDatabaseConnection): Boolean;
var
  Query: AnsiString;

begin
  Query := GetInsertQuery;

  DB.RunQuery(Query);


end;

function TBaseDataModule.ToString: AnsiString;
var
  i: Integer;

begin
  Result := '';

  for i := 0 to NumFields - 1 do
    Result += Format('%s: %s' + sLineBreak, [ColumnNameByIndex(i), GetDataByIndex(i)]);

end;

constructor TBaseDataModule.Create(_NumFields: Integer);
begin
  inherited Create;

  FValues := TDMValues.Create;
  FValues.Count := _NumFields;

end;

destructor TBaseDataModule.Destroy;
var
  Value: TDMValue;

begin
  for Value in FValues do
    Value.Free;

  inherited Destroy;
end;

end.

