unit MySQLDBConnectorUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DBConnectorUnit, mysql50, QueryResponeUnit, SyncUnit;

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
    Mutex: TMutex;

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
  private type

    { TRefresherThread }

    TRefresherThread = class(TThread)
    private
      DB: TMySQLDatabaseConnection;
      FDone: Boolean;

    public
      property Done: Boolean read FDone;
      constructor Create(_DB: TMySQLDatabaseConnection);

      procedure Execute; override;

    end;

  private
    MySQLConnection: PMYSQL;
    Mutex: TMutex;
    RefresherThread: TRefresherThread;

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
    function IsDisconnected: Boolean;

    function RunQuery(const Query: AnsiString): TQueryResponse; override;
    function Execute(const ProcName: AnsiString; InputArguments: array of AnsiString;
      OutputArguments: array of Pointer): TQueryResponse; override;

  end;

  { EMySqlError }

  EMySqlError = class (Exception);

  function MysqlEscapeString(const Str: AnsiString): AnsiString;

implementation

uses
  ALoggerUnit, StringUnit, ParameterManagerUnit;

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

function MysqlEscapeString(const Str: AnsiString): AnsiString;
var
  ActualLength: Integer;

begin
  SetLength(Result, 2 * Length(Str));

  ActualLength := mysql_escape_string(PAnsiChar(Result), PAnsiChar(Str), Length(Str));
  SetLength(Result, ActualLength);
end;

{ TMySQLDatabaseConnection.TRefresherThread }

constructor TMySQLDatabaseConnection.TRefresherThread.Create(
  _DB: TMySQLDatabaseConnection);
begin
  inherited Create(True);
  FreeOnTerminate := True;

  DB := _DB;
  FDone := False;
end;

procedure TMySQLDatabaseConnection.TRefresherThread.Execute;
var
  RefreshInterval: Int64;

begin
  RefreshInterval:= GetRunTimeParameterManager.ValueByName['--DBRefreshInterval'].AsInteger;

  while not Done do
  begin
    Sleep(RefreshInterval * 1000);
    FMTDebugLn('Refresh: %s', [BoolToStr(DB.Refresh, 'True', 'False')]);

  end;
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
  Mutex := TMutex.Create;

end;

destructor TMySqlQueryResponse.Destroy;
begin
  mysql_free_result(FRes);
  FCurrentRow.Free;
  FColumns.Free;
  Mutex.Free;

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

begin
  for i := 0 to NumColumns - 1 do
  begin
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
var
  Alloc: PMYSQL;

begin
  inherited;

  Mutex := TMutex.Create;

  Mutex.Lock;

  Alloc := mysql_init(nil);
  MySQLConnection := mysql_real_connect(Alloc, PChar(Host), PChar(Username),
    PChar(Password), nil, 3306, nil, 0);
  if MySQLConnection = Nil then
  begin
    Writeln(stderr, 'Couldn''t connect to MySQL.');
    Writeln(stderr, mysql_error(nil));
    Writeln('Couldn''t connect to MySQL.');
    Writeln(mysql_error(nil));
    halt(12);

  end;
  MySQLConnection^.options.max_allowed_packet:= 1024 * 1024;

  Mutex.Unlock;
  RefresherThread := TRefresherThread.Create(Self);

  RefresherThread.Start;
end;

destructor TMySQLDatabaseConnection.Destroy;
begin
  Disconnect;
  Mutex.Free;
  RefresherThread.Terminate;
  WaitForThreadTerminate(RefresherThread.ThreadID, 0);
  inherited Destroy;

end;

function TMySQLDatabaseConnection.Refresh: Boolean;
begin
  Mutex.Lock;

  Result := mysql_refresh(MySQLConnection, 0) = 0;
  if not Result then
  begin
    FMTDebugLn(mysql_error(MySQLConnection), []);

  end;

  Mutex.Unlock;
end;

procedure TMySQLDatabaseConnection.Disconnect;
begin
  Mutex.Lock;

  if MySQLConnection <> nil then
    mysql_close(MySQLConnection);

  MySQLConnection := nil;
  Mutex.Unlock;
end;

procedure TMySQLDatabaseConnection.SetActiveDatabase (DBName: AnsiString);
begin
  Mutex.Lock;

  if mysql_select_db(MySQLConnection, PChar(DBName)) <> 0 then
  begin
    Mutex.Unlock;
    raise EMySqlError.Create('Failed to connect to Database with name: "'
                              + DBName + '"');
  end;

  Mutex.Unlock;
end;

procedure TMySQLDatabaseConnection.Connect;
begin
  Mutex.Lock;

  MySQLConnection := mysql_init(MySQLConnection);
  if mysql_real_connect(MySQLConnection, PAnsiChar(FHost), PAnsiChar(FUserName),
    PAnsiChar(FPassword), nil, 0, nil, CLIENT_MULTI_RESULTS) = nil then
    begin
      Mutex.Unlock;
      raise EMySqlError.Create('Couldn''t connect to MySQL.');

    end;

  Mutex.Unlock;
end;

const
  DisConnectedStat = 'The client was disconnected by the server because of inactivity. ';

function TMySQLDatabaseConnection.IsDisconnected: Boolean;
begin
  Result := IsPrefix(PChar(DisConnectedStat), mysql_stat(MySQLConnection));

end;

function TMySQLDatabaseConnection.RunQuery(const Query: AnsiString
  ): TQueryResponse;
var
  Res: PMYSQL_RES;

begin
  Mutex.Lock;

  if Self.IsDisconnected then
  begin
    FMTDebugLn('The Client was diconnected! Reconnecting...', []);
    Self.Refresh;
    FMTDebugLn('Reconnected!', []);
  end;

  if mysql_query(MySQLConnection, PAnsiChar(Query)) <> 0 then
  begin
    WriteLn(StdErr, Format('Mysql_query: %s', [PAnsiChar(Query)]));
    WriteLn(StdErr, mysql_errno(MySQLConnection), ': ', mysql_error(MySQLConnection));
    Flush(Output);

    Mutex.Unlock;
    raise EMySqlError.Create('Query failed');
  end;

  Res := mysql_store_result(MySQLConnection);
  if Res = nil then
  begin
    Mutex.Unlock;
    Exit(nil);

  end;

  Mutex.Unlock;

  Result := TMySqlQueryResponse.Create(Res, Self);
end;

function TMySQLDatabaseConnection.Execute(const ProcName: AnsiString;
  InputArguments: array of AnsiString; OutputArguments: array of Pointer
  ): TQueryResponse;
var
  Stmt: PMYSQL_STMT;
  Query, Arg: AnsiString;

begin
  Stmt := mysql_stmt_init(MySQLConnection);

  Query := Format('Call %s(', [ProcName]);
  for Arg in InputArguments do
    Query += Format('''%s''', [Arg]);
  mysql_stmt_prepare(Stmt, PChar(Query), Length(Query));

end;

initialization

finalization

end.
