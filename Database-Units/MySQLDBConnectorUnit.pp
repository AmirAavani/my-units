unit MySQLDBConnectorUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DBConnectorUnit, mysql57dyn;

type

  { TMySqlQueryResponse }

  TMySqlQueryResponse =class(TQueryResponse)
  private
    FRes: PMYSQL_RES;
    FCurrentRow : MYSQL_ROW;

  protected
    function GetHasNext: Boolean; override;
    function GetNumColumns: Integer; override;
    function GetNumRows: Integer; override;

    constructor Create(Res: PMYSQL_RES);

  public
    destructor Destroy; override;

    function GetRow: TStringList; override;
    procedure GetRow(Response: TStringList); override;

  end;

  { TMySQLDatabaseConnection }

  TMySQLDatabaseConnection= class (TDatabaseConnection)
  private
    MySQLConnection: PMYSQL;

  protected

    function GetActiveDB: AnsiString; override;
    function GetTables: TStringList; override;

    procedure SetActiveDatabase (DBName: AnsiString); override;

  public
    constructor Create (const Username, Password, Host: AnsiString);
    destructor Destroy; override;

    function Refresh: Boolean; override;
    procedure Disconnect;  override;
    procedure Connect; override;

    function RunQuery(const Query: AnsiString): TQueryResponse; override;

  end;

  { EMySqlError }

  EMySqlError = class (Exception);

implementation

type

  { ENotConnected }

  ENotConnected= class (Exception)
  public
    constructor Create;

  end;

  { ENoActiveDB }

  ENoActiveDB= class (Exception)
  public
    constructor Create;

  end;

{ TMySqlQueryResponse }

function TMySqlQueryResponse.GetHasNext: Boolean;
begin
  Result := FCurrentRow <> nil;
end;

function TMySqlQueryResponse.GetNumColumns: Integer;
begin
  Result := mysql_num_fields(FRes);

end;

function TMySqlQueryResponse.GetNumRows: Integer;
begin
  Result := mysql_num_rows(FRes);

end;

constructor TMySqlQueryResponse.Create(Res: PMYSQL_RES);
begin
  inherited Create;

  FRes := Res;
  FCurrentRow := mysql_fetch_row(FRes);

end;

destructor TMySqlQueryResponse.Destroy;
begin
  mysql_free_result (FRes);

  inherited Destroy;
end;

function TMySqlQueryResponse.GetRow: TStringList;
begin
  Result := TStringList.Create;
  Self.GetRow(Result);

end;

procedure TMySqlQueryResponse.GetRow(Response: TStringList);
var
  i: Integer;
  Field: PMYSQL_FIELD;

begin
  for i := 0 to NumColumns - 1 do
  begin
    Field := mysql_fetch_field_direct(FRes, i);
    Response.Add(FCurrentRow[0]);
  end;
  FCurrentRow := mysql_fetch_row(FRes);
end;

constructor ENoActiveDB.Create;
begin
  inherited Create ('There is no Active Database!');

end;

constructor ENotConnected.Create;
begin
  inherited Create ('Not Connected!');

end;


{ TMySQLDatabaseConnection }


function TMySQLDatabaseConnection.GetTables: TStringList;
begin
end;

function TMySQLDatabaseConnection.GetActiveDB: AnsiString;
begin
  if FActiveDB<> '' then
    Result:= FActiveDB
  else
    raise ENoActiveDB.Create;

end;

constructor TMySQLDatabaseConnection.Create (
  const Username, Password, Host: AnsiString);
begin
  inherited;

end;

destructor TMySQLDatabaseConnection.Destroy;
begin

  inherited Destroy;

end;

function TMySQLDatabaseConnection.Refresh: Boolean;
begin
  mysql_refresh(MySQLConnection, 0);

end;

procedure TMySQLDatabaseConnection.Disconnect;
begin
  mysql_close(MySQLConnection);

end;

procedure TMySQLDatabaseConnection.SetActiveDatabase (DBName: AnsiString);
begin
  if mysql_select_db(MySQLConnection, PAnsiChar(DBName)) <> 0 then
    raise EMySqlError.Create('Failed to connect to Database with name: "'
                              + DBName + '"');
end;

procedure TMySQLDatabaseConnection.Connect;
begin
  MySQLConnection := mysql_init(MySQLConnection);
  if mysql_real_connect(MySQLConnection, PAnsiChar(FHost), PAnsiChar(FUserName),
    PAnsiChar(FPassword), nil, 0, nil, CLIENT_MULTI_RESULTS) = nil then
    begin
     raise EMySqlError.Create('Couldn''t connect to MySQL.');
     Exit;
    end;
end;

function TMySQLDatabaseConnection.RunQuery(const Query: AnsiString
  ): TQueryResponse;
var
  Res: PMYSQL_RES;
begin
  if mysql_query(MySQLConnection, PAnsiChar(Query)) <> 0 then
    raise EMySqlError.Create('Query failed');

  Res := mysql_store_result(MySQLConnection);
  if Res=Nil then
    raise EMySqlError.Create('Query returned nil result.');

  Result := TMySqlQueryResponse.Create(Res);
end;

initialization
  InitialiseMySQL;

finalization
  ReleaseMySQL;

end.

