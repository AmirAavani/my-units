unit ObjectManagerUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, DBConnectorUnit, QueryResponeUnit;

type

  { EInvalidData }

  EInvalidData = class(Exception);

  TWideStringArray = array of WideString;

  { TDBEntity }

  TDBEntity = class(TObject)
  private
    function GetElement(Index: Integer): WideString;
  protected
    FElements: array of WideString;

    class function ValuesInInsertQuery(data: array of WideString): WideString; virtual; abstract;
    class function GetTable: Ansistring; virtual; abstract;
  public
    property Element[Index: Integer]: WideString read GetElement;

    constructor Create(AllElements: array of WideString);
    destructor Destroy; override;

    class function FillFromRow(ARow: TStringList): TDBEntity; virtual; abstract;
  end;

  TDBEntitiesList = specialize TFPGList<TDBEntity>;
  TDBEntityClass = class of TDBEntity;

  { TDBEntityAccessor }

  TDBEntityAccessor = class(TObject)
  protected
    FTableInfo: TTableInfo;
    class var FdbConnection: TDatabaseConnection;

    class procedure GetAllEntities(EntityClass: TDBEntityClass; List: TDBEntitiesList);
    class function AddNewEntity(
      data: array of WideString;
      EntityClass: TDBEntityClass): Boolean;

    class function ExplainTable(EntityClass: TDBEntityClass): TTableInfo;

  public
    property TableInfo: TTableInfo read FTableInfo;

    constructor Create(dbConnection: TDatabaseConnection; EntityClass: TDBEntityClass);
    destructor Destroy; override;

  end;

implementation

{ TDBEntityTblAccessory }

class procedure TDBEntityAccessor.GetAllEntities(EntityClass: TDBEntityClass;
  List: TDBEntitiesList);
var
  Query: AnsiString;
  Response: TQueryResponse;
  Member: TDBEntity;
  Row: TStringList;

begin
  Query := 'SELECT * FROM ' + EntityClass.GetTable + ';';

  Response := FdbConnection.RunQuery(Query);

  while Response.HasNext do
  begin
    Row := Response.Row;

    Member := EntityClass.FillFromRow(Row);
    if Member <> nil then
      List.Add(Member)
    else
    begin
      raise EInvalidData.Create(Row.Text + ' cannot be converted to ' + EntityClass.ClassName);
    end;
    Response.Next;
  end;

  Response.Free;

end;

class function TDBEntityAccessor.AddNewEntity(data: array of WideString;
  EntityClass: TDBEntityClass): Boolean;
var
  Query: AnsiString;
  Response: TQueryResponse;

begin
  Query := 'INSERT INTO ' + EntityClass.GetTable + ' VALUES(' +
     EntityClass.ValuesInInsertQuery(data) + ')';
  WriteLn('AddNewEntity: ', Query);
  Response := FdbConnection.RunQuery(Query);
  Result := Response = nil;
end;


class function TDBEntityAccessor.ExplainTable(EntityClass: TDBEntityClass
  ): TTableInfo;
var
  Query: AnsiString;
  Response: TQueryResponse;

begin
  Query := 'EXPLAIN ' + EntityClass.GetTable;
  Response := FdbConnection.RunQuery(Query);

  Result := TTableInfo.Create(EntityClass.GetTable, Response);
  Response.Free;

end;

constructor TDBEntityAccessor.Create(dbConnection: TDatabaseConnection;
  EntityClass: TDBEntityClass);
begin
  inherited Create;

  FdbConnection := dbConnection;
  FTableInfo := ExplainTable(EntityClass);

end;

destructor TDBEntityAccessor.Destroy;
begin
  FTableInfo.Free;

  inherited Destroy;
end;

{ TDBEntity }

function TDBEntity.GetElement(Index: Integer): WideString;
begin
  Result := FElements[Index];

end;

constructor TDBEntity.Create(AllElements: array of WideString);
var
  i: Integer;

begin
  inherited Create;

  SetLength(FElements, Length(AllElements));
  for i := 0 to High(AllElements) do
    FElements[i] := AllElements[i];

end;

destructor TDBEntity.Destroy;
begin
  SetLength(FElements, 0);
  inherited Destroy;
end;

end.
