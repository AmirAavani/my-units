unit DataModuleUtilUnit;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, fgl;

type

  { TGetWhereAllOptions }

  TGetWhereAllOptions = class(TObject)
  private
    FMaxCount: Integer;
    FOrderByColumns: TStringList;
    FOrderDesc: specialize TFPGList<Boolean>;
    FStartLimit, FCountLimit: Integer;

    function GetOrderByColumns: TStringList;
    function GetCountLimit: Integer;
    function GetStartLimit: Integer;
  public
    property OrderByColumns: TStringList read GetOrderByColumns;
    property OrderDesc: specialize TFPGList<Boolean> read FOrderDesc;
    property StartLimit: Integer read GetStartLimit;
    property CountLimit: Integer read GetCountLimit;

    constructor Create;

    function AddLimit(Start, Count: Integer): TGetWhereAllOptions;
    function AddOrderBy(ColumnName: AnsiString; IsDesc: Boolean): TGetWhereAllOptions;

  end;

function NewGetWhereAllOptions: TGetWhereAllOptions;
function GetUniqID64: Int64;

implementation

function GetWhereAllOptions(MaxCount: Integer;
  OrderByColumns: array of AnsiString; OrderDesc: array of Boolean
  ): TGetWhereAllOptions;
var
  i: Integer;

begin
          {
  Result.FMaxCount := MaxCount;

  SetLength(Result.OrderByColumns, Length(OrderByColumns));
  SetLength(Result.OrderDesc, Length(OrderDesc));
  for i := 0 to High(OrderByColumns) do
    Result.OrderByColumns[i] := OrderByColumns[i];
  SetLength(Result.OrderDesc, Length(OrderDesc));
  for i := 0 to High(OrderDesc) do
    Result.OrderDesc[i] := OrderDesc[i];
          }
end;

function GetWhereAllOptions(MaxCount: Integer;
  OrderByColumns: array of AnsiString): TGetWhereAllOptions;
var
  i: Integer;

begin
  {
  Result.MaxCount := MaxCount;

  SetLength(Result.OrderByColumns, Length(OrderByColumns));
  for i := 0 to High(OrderByColumns) do
    Result.OrderByColumns[i] := OrderByColumns[i];
    }
end;

function GetWhereAllOptions(MaxCount: Integer): TGetWhereAllOptions;
begin
  // Result.MaxCount := MaxCount;

end;

function GetDefaultWhereAllOptions: TGetWhereAllOptions;
begin
  Result := TGetWhereAllOptions.Create.AddLimit(1, 2);

end;

function NewGetWhereAllOptions: TGetWhereAllOptions;
begin
  Result := TGetWhereAllOptions.Create;

end;

function GetUniqID64: Int64;
begin
  Result := DateTimeToTimeStamp(Now).Time;
  Result := Result shl 31;
  Result := Result or (1 + Random(1 shl 31));

end;

{ TGetWhereAllOptions }

function TGetWhereAllOptions.GetOrderByColumns: TStringList;
begin
  if Self = nil then
    Exit(nil);

  Result := FOrderByColumns;

end;

function TGetWhereAllOptions.GetCountLimit: Integer;
begin
  if Self = nil then
    Exit(-1);

  Result := FCountLimit;
end;

function TGetWhereAllOptions.GetStartLimit: Integer;
begin
  if Self = nil then
    Exit(-1);

  Result := FStartLimit;

end;

constructor TGetWhereAllOptions.Create;
begin
  inherited;

  FMaxCount := -1;
  FOrderByColumns := TStringList.Create;
  FOrderDesc := (specialize TFPGList<Boolean>).Create;
  FStartLimit := -1;
  FCountLimit := -1;

end;

function TGetWhereAllOptions.AddLimit(Start, Count: Integer
  ): TGetWhereAllOptions;
begin
  Result := Self;

  Result.FStartLimit := Start;
  Result.FCountLimit := Count;

end;

function TGetWhereAllOptions.AddOrderBy(ColumnName: AnsiString; IsDesc: Boolean
  ): TGetWhereAllOptions;
begin
  Result := Self;

  Result.FOrderByColumns.Add(ColumnName);
  Result.FOrderDesc.Add(IsDesc);
end;

end.

