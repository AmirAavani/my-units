unit DBConnectorUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, QueryResponeUnit;

type
  { TDatabaseConnection }

  TDatabaseConnection = class (TObject)
  protected
    FUserName, FPassword, FHost: AnsiString;
    FActiveDB: AnsiString;

    function GetActiveDB: AnsiString; virtual; abstract;
    function GetTables: TStringList; virtual; abstract;

    procedure SetActiveDatabase (DBName: AnsiString); virtual; abstract;

  public
    property ActiveDB: AnsiString read GetActiveDB write SetActiveDatabase;

    constructor Create(const Username, Password, Host: AnsiString);
    destructor Destroy; override;

    function Refresh: Boolean; virtual; abstract;
    procedure Disconnect; virtual; abstract;
    procedure Connect; virtual; abstract;

    function RunQuery(const Query: AnsiString): TQueryResponse; virtual; abstract;
    function Execute(const Query: AnsiString; InputArguments: array of AnsiString;
      OutputArguments: array of Pointer): TQueryResponse; virtual; abstract;
  end;

  EConnectionFailed = class(Exception);

  function StringToBlob(const Str: AnsiString): AnsiString;
  function BlobToString(const BlobStr: AnsiString): AnsiString;
  function EscapeForQuery(const Query: WideString): WideString;
  function UnEscapeQuery(const EscapedQuery: WideString): WideString;

implementation
uses
  ALoggerUnit, mysql50;

type

  { ENotConnected }

  ENotConnected = class(Exception)
  public
    constructor Create;

  end;

  { ENoActiveDB }

  ENoActiveDB = class(Exception)
  public
    constructor Create;

  end;

function String2Hex(const Str: AnsiString): AnsiString;
var
  i: Integer;
  c: Char;
  ic: Integer;

begin
  Result := '';

  for i := 1 to Length(Str) do
  begin
    c := Str[i];
    Result += IntToHex(Ord(c), 2);
  end;
end;

function StringToBlob(const Str: AnsiString): AnsiString;
begin
  Result := String2Hex(Str);

end;

function BlobToString(const BlobStr: AnsiString): AnsiString;
var
  i: Integer;
  S: AnsiString;

begin
  Result := '';
  S := BlobStr[1];
  S += BlobStr[2];
  for i := 3 to Length(BlobStr) do
  begin
    if i and 1 = 1 then
      S := '';
    S := S


  end;

end;

function EscapeForQuery(const Query: WideString): WideString;
const
  SingleQuote = Chr(39);
  Tab = Chr(7);
  Source: array of AnsiString = (#$0, '\',   SingleQuote,      '"',   #13#10,  #10,
  Tab, Chr(26));
  Target: array of AnsiString = ('\0', '\\', '\' + SingleQuote, '\"', '\n',    '\n',
  '\t', '\Z');
var
  i: Integer;

begin
{
 \0     An ASCII NUL(0x00) character.
\'     A single quote(“'”) character.
\"     A double quote(“"”) character.
-- \b  A backspace character.
\n     A newline(linefeed) character.
\r     A carriage return character.
\t     A tab character.
\Z     ASCII 26(Control-Z). See note following the table.
\\     A backslash(“\”) character.
\%     A “%” character. See note following the table.
\_     A “_” character. See note following the table.
}

  Result := Query;
  for i := 0 to High(Source) do
  begin
    Result := WideString(StringReplace(AnsiString(Result), Source[i], Target[i],
    [rfReplaceAll]));

  end;


end;

function UnEscapeQuery(const EscapedQuery: WideString): WideString;
begin
  Result := EscapedQuery;

end;

constructor ENoActiveDB.Create;
begin
  inherited Create('There is no Active Database!');

end;

constructor ENotConnected.Create;
begin
  inherited Create('Not Connected!');

end;


{ TDatabaseConnection }

constructor TDatabaseConnection.Create(const Username, Password, Host: AnsiString);
begin
  inherited Create;

  Self.FUserName := Username;
  Self.FPassword := Password;
  Self.FHost := Host;
end;

destructor TDatabaseConnection.Destroy;
begin
  inherited Destroy;

end;

end.

