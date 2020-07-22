unit MySQLDBConnectorUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DBConnectorUnit, mysql50, QueryResponeUnit;

type
  TMySQLDatabaseConnection = class;

  { TMySqlQueryResponse }

  TMySqlQueryResponse = class(TQueryResponse)
  private
    FRes: PMYSQL_RES;
    FCurrentRowRaw : MYSQL_ROW;
    FCurrentRow: TStringList;
    FColumns: TStringList;
    FMySqlConnection: TMySQLDatabaseConnection;

  protected
    function GetHasNext: Boolean; override;
    function GetNumColumns: Integer; override;
    function GetNumRows: Integer; override;


  public
    constructor Create(Res: PMYSQL_RES; Connection: TMySQLDatabaseConnection);
    destructor Destroy; override;

    function GetRow: TStringList; override;
    procedure GetRow(Response: TStringList); override;
    function GetColumns: TStringList; override;
    procedure GetColumns(Response: TStringList); override;

    procedure Next; override;

  end;

  { TMySQLDatabaseConnection }

  TMySQLDatabaseConnection= class (TDatabaseConnection)
  private
    MySQLConnection: PMYSQL;
    QMysql: st_mysql;
    RTLEvent: PRTLEvent;

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
  Result := FCurrentRowRaw <> nil;
end;

function TMySqlQueryResponse.GetNumColumns: Integer;
begin
  Result := mysql_num_fields(FRes);

end;

function TMySqlQueryResponse.GetNumRows: Integer;
begin
  Result := mysql_num_rows(FRes);

end;

constructor TMySqlQueryResponse.Create(Res: PMYSQL_RES;
  Connection: TMySQLDatabaseConnection);
begin
  inherited Create;

  FRes := Res;
  FCurrentRowRaw := mysql_fetch_row(FRes);
  FCurrentRow := TStringList.Create;
  FColumns := TStringList.Create;
  GetColumns(FColumns);
  FMySqlConnection := Connection;

end;

destructor TMySqlQueryResponse.Destroy;
begin
  mysql_free_result(FRes);
  FCurrentRow.Free;
  FColumns.Free;
  RTLeventSetEvent(FMySqlConnection.RTLEvent);

  inherited Destroy;
end;

function TMySqlQueryResponse.GetRow: TStringList;
begin
  if FCurrentRow.Count <> 0 then
    Exit(FCurrentRow);

  FCurrentRow.Clear;
  Self.GetRow(FCurrentRow);

  Result := FCurrentRow;

end;

procedure TMySqlQueryResponse.GetRow(Response: TStringList);
var
  i: Integer;
  Field: PMYSQL_FIELD;

begin
  for i := 0 to NumColumns - 1 do
  begin
    Field := mysql_fetch_field_direct(FRes, i);
    Response.Add(FCurrentRowRaw[i]);
  end;

end;

function TMySqlQueryResponse.GetColumns: TStringList;
begin
  if FColumns.Count <> 0 then
    Exit(FColumns);

  Self.GetColumns(FColumns);

  Result := FColumns;

end;

procedure TMySqlQueryResponse.GetColumns(Response: TStringList);
var
  i: Integer;
  Field: PMYSQL_FIELD;

begin
  Response.Clear;

  for i := 0 to NumColumns - 1 do
  begin
    Field := mysql_fetch_field_direct(FRes, i);
    Response.Add(Field^.name);
  end;

end;

procedure TMySqlQueryResponse.Next;
begin
  FCurrentRow.Clear;
  FCurrentRowRaw := mysql_fetch_row(FRes);

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

  RTLEvent := RTLEventCreate;
  RTLeventSetEvent(RTLEvent);

  mysql_init(PMySQL(@QMysql));
  MySQLConnection := mysql_real_connect(PMysql(@QMysql), PChar(Host), PChar(Username),
    PChar(Password), nil, 3306, nil, 0);

  if MySQLConnection = Nil then
  begin
    Writeln(stderr, 'Couldn''t connect to MySQL.');
    Writeln(stderr, mysql_error(@QMysql));
    halt(1);
  end;

end;

destructor TMySQLDatabaseConnection.Destroy;
begin
  RTLeventdestroy(RTLEvent);
  mysql_close(MySQLConnection);

  inherited Destroy;

end;

function TMySQLDatabaseConnection.Refresh: Boolean;
begin
  RTLeventWaitFor(RTLEvent);

  mysql_refresh(MySQLConnection, 0);

  RTLeventSetEvent(RTLEvent);
end;

procedure TMySQLDatabaseConnection.Disconnect;
begin
  RTLeventWaitFor(RTLEvent);

  if MySQLConnection <> nil then
    mysql_close(MySQLConnection);

  MySQLConnection := nil;
  RTLeventSetEvent(RTLEvent);
end;

procedure TMySQLDatabaseConnection.SetActiveDatabase (DBName: AnsiString);
begin
  RTLeventWaitFor(RTLEvent);

  if mysql_select_db(MySQLConnection, PChar(DBName)) <> 0 then
    raise EMySqlError.Create('Failed to connect to Database with name: "'
                              + DBName + '"');

  RTLeventSetEvent(RTLEvent);
end;

procedure TMySQLDatabaseConnection.Connect;
begin
  RTLeventWaitFor(RTLEvent);

  MySQLConnection := mysql_init(MySQLConnection);
  if mysql_real_connect(MySQLConnection, PAnsiChar(FHost), PAnsiChar(FUserName),
    PAnsiChar(FPassword), nil, 0, nil, CLIENT_MULTI_RESULTS) = nil then
    begin
      RTLeventSetEvent(RTLEvent);
      raise EMySqlError.Create('Couldn''t connect to MySQL.');
      Exit;
    end;
  RTLeventSetEvent(RTLEvent);
end;

function TMySQLDatabaseConnection.RunQuery(const Query: AnsiString
  ): TQueryResponse;
var
  Res: PMYSQL_RES;

begin
  RTLeventWaitFor(RTLEvent);

  if mysql_query(MySQLConnection, PAnsiChar(Query)) <> 0 then
  begin
    WriteLn(StdErr, Format('Mysql_query: %s', [Query]));
    WriteLn(StdErr, mysql_errno(MySQLConnection), ': ', mysql_error(MySQLConnection));
    Flush(Output);

    RTLeventSetEvent(RTLEvent);
    raise EMySqlError.Create('Query failed');
  end;

  Res := mysql_store_result(MySQLConnection);
  if Res = nil then
  begin
    RTLeventSetEvent(RTLEvent);
    Exit(nil);
  end;

  Result := TMySqlQueryResponse.Create(Res, Self);
end;

initialization

finalization

end.
