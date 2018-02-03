unit DBConnectorUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, QueryResponeUnit;

type
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

  function EscapeForQuery(const Query: WideString): WideString;

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

function EscapeForQuery(const Query: WideString): WideString;
begin
  {
 \0     An ASCII NUL (0x00) character.
\'     A single quote (“'”) character.
\"     A double quote (“"”) character.
\b     A backspace character.
\n     A newline (linefeed) character.
\r     A carriage return character.
\t     A tab character.
\Z     ASCII 26 (Control-Z). See note following the table.
\\     A backslash (“\”) character.
\%     A “%” character. See note following the table.
\_     A “_” character. See note following the table.
}
  Result := WideString(StringReplace(StringReplace(StringReplace(StringReplace(
     AnsiString(Query), '''', '\''', [rfReplaceAll]),
     '"', '\"', [rfReplaceAll]), '(', '\(', [rfReplaceAll]), ')', '\)', [rfReplaceAll]));
//  Result := WideString(StringReplace(Result, #13, '', [rfReplaceAll]);
end;

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

