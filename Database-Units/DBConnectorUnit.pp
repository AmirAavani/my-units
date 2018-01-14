unit DBConnectorUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TQueryResponse }

  TQueryResponse = class(TObject)
  private
  protected
    function GetHasNext: Boolean; virtual; abstract;
    function GetNumColumns: Integer; virtual; abstract;
    function GetNumRows: Integer; virtual; abstract;

  public
    property NumRows: Integer read GetNumRows;
    property NumColumns: Integer read GetNumColumns;
    property HasNext: Boolean read GetHasNext;

    // Caller is responsibe for freeing the result.
    function GetRow: TStringList; virtual; abstract;
    procedure GetRow(Response: TStringList); virtual; abstract;

  end;

  { TDatabaseConnection }

  TDatabaseConnection= class (TObject)
  protected
    FUserName, FPassword, FHost: AnsiString;
    FActiveDB: AnsiString;

    function GetActiveDB: AnsiString; virtual; abstract;
    function GetTables: TStringList; virtual; abstract;

    procedure SetActiveDatabase (DBName: AnsiString); virtual; abstract;

  public
    property ActiveDB: AnsiString read GetActiveDB write SetActiveDatabase;

    constructor Create (const Username, Password, Host: AnsiString);
    destructor Destroy; override;

    function Refresh: Boolean; virtual; abstract;
    procedure Disconnect; virtual; abstract;
    procedure Connect; virtual; abstract;

    function RunQuery(const Query: AnsiString): TQueryResponse; virtual; abstract;
  end;

  EConnectionFailed= class (Exception);

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

{ TQueryResponse }

constructor ENoActiveDB.Create;
begin
  inherited Create ('There is no Active Database!');

end;

constructor ENotConnected.Create;
begin
  inherited Create ('Not Connected!');

end;


{ TDatabaseConnection }

constructor TDatabaseConnection.Create (const Username, Password, Host: AnsiString);
begin
  inherited Create;

  Self.FUserName:= Username;
  Self.FPassword:= Password;
  Self.FHost:= Host;
end;

destructor TDatabaseConnection.Destroy;
begin
  Disconnect;

  inherited Destroy;

end;


end.

