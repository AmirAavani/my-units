unit QueryResponeUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type
  { TQueryResponse }

  TQueryResponse = class(TObject)
  private
  protected
    function GetHasNext: Boolean; virtual; abstract;
    function GetNumColumns: Integer; virtual; abstract;
    function GetNumRows: Integer; virtual; abstract;

    function GetRow: TStringList; virtual; abstract;
  public
    property NumRows: Integer read GetNumRows;
    property NumColumns: Integer read GetNumColumns;
    property HasNext: Boolean read GetHasNext;
    property Row: TStringList read GetRow;

    procedure GetRow(Response: TStringList); virtual; abstract;
    procedure Next; virtual; abstract;

  end;

  { TColumnInfo }

  TColumnInfo = class(TObject)
  private
    FDefaultValue: AnsiString;
    FExtra: TStringList;
    FField: Ansistring;
    FKey: AnsiString;
    FNull: Boolean;
    FTableType: AnsiString;

    constructor Create(Row: TStringList);
    destructor Destroy; override;
    function GetIsAutoIncrement: Boolean;
  public
    property Field: Ansistring read FField;
    property TableType: AnsiString read FTableType;
    property Null: Boolean read FNull;
    property Key: AnsiString read FKey;
    property DefaultValue: AnsiString read FDefaultValue;
    property Extra: TStringList read FExtra;
    property IsAutoIncrement: Boolean read GetIsAutoIncrement;

    function ToString: AnsiString; override;
  end;

  { TTableInfo }

  TTableInfo = class(specialize TFPGList<TColumnInfo>)
  private
    FTableName: AnsiString;
  public
    property TableName: AnsiString read FTableName;
    constructor Create(const Name: Ansistring; const Response: TQueryResponse);

  end;

implementation
type

  { EExpalinTable }

  EExpalinTable = class(Exception);

{ TTableInfo }

constructor TTableInfo.Create(const Name: Ansistring;
  const Response: TQueryResponse);
var
  i: Integer;
begin
  inherited Create;

  FTableName := Name;

  while Response.HasNext do
  begin
    Self.Add(TColumnInfo.Create(Response.Row));
    Response.Next;
  end;
end;

{ TColumnInfo }

constructor TColumnInfo.Create(Row: TStringList);
begin
  inherited Create;

  if Row.Count <> 6 then
    raise EExpalinTable.Create('Row: "' + Row.Text + '" is an invalid response');

  FField := Row[0];
  FTableType := Row[1];
  FNull := Row[2] = '';
  FKey := Row[3];
  FDefaultValue := Row[4];
  FExtra := TStringList.Create;
  FExtra.Sort;
  FExtra.Add(Row[5]);
end;

destructor TColumnInfo.Destroy;
begin
  FExtra.Free;
  inherited Destroy;
end;

function TColumnInfo.GetIsAutoIncrement: Boolean;
const
  auto_increment_text : AnsiString = 'auto_increment';

begin
  Result := 0 <= FExtra.IndexOf(auto_increment_text);

end;

function TColumnInfo.ToString: AnsiString;
begin
  Result:= 'Field:' + Field + ' Type:' + TableType +
    ' Null:' +  BoolToStr(Null) +
    ' Key:' + Key + ' Default:' + DefaultValue + ' Extra: '+ FExtra.Text;
end;

end.

