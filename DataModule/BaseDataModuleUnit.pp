unit BaseDataModuleUnit;

{$mode objfpc}{$H+}

interface

uses
  sysutils, classes, DBConnectorUnit, QueryResponeUnit, ValueUnit, DataModuleUtilUnit,
  GenericCollectionUnit;

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
    TDMValues = specialize TObjectCollection<TDMValue>;

  protected
    FValues: TDMValues;

    function GetValueByIndex(Index: Integer): TDMValue;
    function FillFromResponse(Row, Column: TStringList): Boolean; virtual;
    procedure SetValueByColumnName(ColumnName: AnsiString; StrValue: AnsiString); virtual;
    procedure SetValueByColumnIndex(ColumnIndex: Integer; StrValue: AnsiString); virtual;
    function GetInsertQuery: AnsiString; virtual;
    function GetUpdateQuery: AnsiString; virtual;
    function GetMySQLValuesForKeys: TStringList; virtual; abstract;

  public
    property ValueByIndex[Index: Integer]: TDMValue read GetValueByIndex;

    class function TableName: AnsiString; virtual; abstract;
    class function NumFields: Integer; virtual; abstract;
    class function GetColumnNameByIndex(Index: Integer): AnsiString; virtual; abstract;
    class function GetMySQLColumnTypeByIndex(Index: Integer): AnsiString; virtual; abstract;
    class function GetFPCColumnTypeByIndex(Index: Integer): AnsiString; virtual; abstract;
    class function GetColumnIndexByName(const aName: AnsiString): Integer; virtual; abstract;
    class function GetKeyColumnNames: TStringList; virtual; abstract;

    function GetDataByIndex(Index: Integer): TDMValue; virtual;

    function Save(DB: TDatabaseConnection): Boolean; virtual;
    function Update(DB: TDatabaseConnection): Boolean; virtual;

    function ToString: AnsiString; override;
    constructor Create(_NumFields: Integer);
    destructor Destroy; override;
  end;


  { TBaseDataList }

  generic TBaseDataList<TData> = class(specialize TObjectCollection<TData>)
  public
    procedure PrintAll; virtual;

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
    // Returns all Data satisfying the query.
    function GetAllWhere(WhereClause: AnsiString; Option: TGetWhereAllOptions = nil): TDataList; virtual;
    // Returns the numbre of entries satisfying the query.
    function GetCountWhere(WhereClause: AnsiString): UInt32; virtual;

    procedure DeleteAllWhere(WhereClause: AnsiString); virtual;

  end;

procedure ToInt(Source: AnsiString; Target: Pointer);
procedure ToString(Source: AnsiString; Target: Pointer);
generic function GetTopAndFree<TData>(aDataList: specialize TBaseDataList<TData>): TData;

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

generic function GetTopAndFree<TData>(aDataList: specialize TBaseDataList<TData>): TData;
var
  i: Integer;

begin
  if aDataList = nil then
    Exit(nil);

  if aDataList.Count = 0 then
  begin
    aDataList.Free;
    Exit(nil);

  end;

  Result := aDataList[0];
  for i := 1 to aDataList.Count - 1 do
    aDataList[i].Free;
  aDataList.Clear;
  aDataList.Free;

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
      Result := Format('"%s"', [EscapeForQuery(AsAnsiString)])
  end;

end;

function TBaseDataModule.TDMValue.GetAsAnsiString: AnsiString;
begin
  if Self = nil then
    Exit('');

  Result:= inherited GetAsAnsiString;
  Result := AnsiString(UnEscapeQuery(WideString(Result)));

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

{ EInvalidColumnName }

constructor EInvalidColumnName.Create(ColumnName: AnsiString);
begin
  inherited Create(Format('Column "%s" has not found', [ColumnName]));

end;

{ TBaseDataModuleManager }

function TBaseDataModuleManager.GetAllWhere(WhereClause: AnsiString;
  Option: TGetWhereAllOptions): TDataList;
var
  Query: AnsiString;
  Response: TQueryResponse;
  i: Integer;

begin
  Query := Format('SELECT * FROM %s WHERE %s', [TData.TableName, WhereClause]);
  if Option <> nil then
  begin
    if Option.OrderByColumns <> nil then
    begin
      if Option.OrderByColumns.Count <> 0 then
        Query += ' ORDER BY ';

      for i := 0 to Option.OrderByColumns.Count - 1 do
      begin
        Query += Option.OrderByColumns[i];
        if Option.OrderDesc[i] then
          Query += ' DESC ';

      end;

    end;

    if (Option.StartLimit <> -1) and (Option.CountLimit <> -1) then
      Query += Format(' LIMIT %d, %d', [Option.StartLimit, Option.CountLimit])
    else if Option.CountLimit <> -1 then
      Query += Format(' LIMIT %d', [Option.CountLimit])

  end;

  DebugLn(Format('Q: %s', [Query]));
  Response := DB.RunQuery(Query);

  Result := ExtractFromResponse(Response, -1);

  Option.Free;
  Response.Free;

end;

function TBaseDataModuleManager.GetCountWhere(WhereClause: AnsiString): UInt32;
var
  Query: AnsiString;
  Response: TQueryResponse;

begin
  Query := Format('SELECT COUNT(*) FROM %s WHERE %s', [TData.TableName, WhereClause]);
  DebugLn(Format('Q: %s', [Query]));
  Response := DB.RunQuery(Query);

  Result := StrToInt(Response.Row[0]);

  Response.Free;

end;

procedure TBaseDataModuleManager.DeleteAllWhere(WhereClause: AnsiString);
var
  Query: AnsiString;
  Response: TQueryResponse;
  i: Integer;

begin
  Query := Format('DELETE FROM %s WHERE %s', [TData.TableName, WhereClause]);

  DebugLn(Format('Q: %s', [Query]));
  Response := DB.RunQuery(Query);

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

    aResponse.Next;
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
  begin
    FMTDebugLn('Column[%d]: %s -> %d', [i, Column[i], Length(Row[i])]);
    Self.SetValueByColumnName(Column[i], Row[i]);
  end;

end;

procedure TBaseDataModule.SetValueByColumnName(ColumnName: AnsiString;
  StrValue: AnsiString);
begin
  SetValueByColumnIndex(GetColumnIndexByName(ColumnName), StrValue);

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
    Names.Add(GetColumnNameByIndex(i));
    Values.Add(ValueByIndex[i].AsMySQL);
  end;
  Result := Format('INSERT INTO %s(%s) VALUE(%s)', [Self.TableName,
    JoinStrings(Names, ','), JoinStrings(Values, ',')]);

  Names.Free;
  Values.Free;

end;

function TBaseDataModule.GetUpdateQuery: AnsiString;
var
  i: Integer;
  Values: TStringList;

begin
  Result := Format('UPDATE %s SET ', [Self.TableName]);
  for i := 0 to Self.NumFields - 1 do
    Result += Format('%s = %s, ', [GetColumnNameByIndex(i), ValueByIndex[i].AsMySQL]);
  Delete(Result, Length(Result) - 1, 2);

  Values := GetMySQLValuesForKeys;
  for i := 0 to GetKeyColumnNames.Count - 1 do
    Result += Format(' WHERE %s = %s AND', [GetKeyColumnNames[i], Values[i]]);
  Delete(Result, Length(Result) - 3, 4);

  Values.Free;

end;

function TBaseDataModule.GetDataByIndex(Index: Integer): TDMValue;
begin
  Result := FValues[Index];

end;

function TBaseDataModule.Save(DB: TDatabaseConnection): Boolean;
var
  Response: TQueryResponse;

begin
  Response := DB.RunQuery(GetInsertQuery);

  Result := Response = nil;
end;

function TBaseDataModule.Update(DB: TDatabaseConnection): Boolean;
var
  Response: TQueryResponse;

begin
  Response := DB.RunQuery(GetUpdateQuery);

  Result := Response = nil;

end;

function TBaseDataModule.ToString: AnsiString;
var
  i: Integer;

begin
  Result := '';

  for i := 0 to NumFields - 1 do
    Result += Format('%s: %s' + sLineBreak, [GetColumnNameByIndex(i), GetDataByIndex(i)]);

end;

constructor TBaseDataModule.Create(_NumFields: Integer);
var
  i: Integer;

begin
  inherited Create;

  FValues := TDMValues.Create;
  FValues.Count := _NumFields;

end;

destructor TBaseDataModule.Destroy;
begin
  FValues.Free;

  inherited Destroy;
end;

end.

