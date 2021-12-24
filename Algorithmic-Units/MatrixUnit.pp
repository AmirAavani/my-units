unit MatrixUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TMatrix }
  generic TMatrix<TData> = class(TObject)
  private
    type TMatrix_TData = specialize TMatrix<TData>;
  var
    FColumnCount: Integer;
    FRowCount: Integer;
    FMat: array of array of TData;
    function GetCell(r, c: Integer): TData; inline;
    procedure SetCell(r, c: Integer; AValue: TData); inline;

  protected
    function Triangularize: TMatrix_TData;

  public
    property RowCount : Integer read FRowCount;
    property ColumnCount : Integer read FColumnCount;
    property Cell[r, c: Integer] : TData read GetCell write SetCell;

    constructor Create(r, c: Integer);
    constructor Identity(n: Integer);
    destructor Destroy; override;

    procedure Print;

    function Copy: TMatrix; virtual;
    function Inverse(var det: Double; var InvMat: TMatrix_TData): Boolean;
    function Multiply(b: TMatrix_TData): TMatrix_TData;
  end;

  { TVector }

  generic TVector<TData> = class (specialize TMatrix<TData>)
  private
    function GetItem(Index: Integer): TData;
    procedure SetItem(Index: Integer; n : TData);
  public
    property Item[Index: Integer]: TData read GetItem write SetItem;

    constructor Create(n: Integer);

    function Copy: TVector; override;
  end;


implementation

{ TMatrix }

function TMatrix.GetCell(r, c: Integer): TData;
begin
  Result := FMat[r, c];
end;

procedure TMatrix.SetCell(r, c: Integer; AValue: TData);
begin
  FMat[r, c] := AValue;
end;

function TMatrix.Triangularize: TMatrix_TData;
begin

end;

constructor TMatrix.Create(r, c: Integer);
var
  i, j: Integer;

begin
  inherited Create;

  FRowCount := r;
  FColumnCount := c;

  SetLength(FMat, RowCount);
  for i := 0 to RowCount - 1 do
  begin
    SetLength(FMat[i], ColumnCount);
    for j := 0 to ColumnCount - 1 do
      FMat[i][j] := 0;
  end;

end;

constructor TMatrix.Identity(n: Integer);
var
  ir, i, j: Integer;

begin
  inherited Create;

  FRowCount := n;
  FColumnCount := n;

  SetLength(FMat, RowCount);
  for i := 0 to RowCount - 1 do
  begin
    SetLength(FMat[i], ColumnCount);
    for j := 0 to ColumnCount - 1 do
      FMat[i][j] := 0;
  end;

  for ir := 0 to n - 1 do
    Cell[ir, ir] := 1;

end;

destructor TMatrix.Destroy;
var
  r: Integer;

begin
  for r := 0 to RowCount - 1 do
    SetLength(FMat[r], 0);
  SetLength(FMat, 0);

  inherited Destroy;
end;

procedure TMatrix.Print;
var
  r, c: Integer;
begin
  for r := 0 to RowCount - 1 do
  begin
    for c := 0 to ColumnCount - 1 do
      Write(Cell[r, c], ' ');
    WriteLn;
  end;
  WriteLn;
end;

function TMatrix.Copy: TMatrix;
var
  r, c: Integer;
begin
  Result := TMatrix.Create(RowCount, ColumnCount);

  for r := 0 to RowCount - 1 do
    for c := 0 to ColumnCount - 1 do
    Result.FMat[r][c] := FMat[r][c];

end;

function TMatrix.Inverse(var det: Double; var InvMat: TMatrix_TData): Boolean;
var
  me: TMatrix_TData;
  r, c: Integer;
  r1, c1: Integer;
  Coef: Double;
  v: Integer;

begin
  if RowCount <> ColumnCount then
    raise Exception.Create('Invalid Martix Size');

  if InvMat = nil then
    InvMat := TMatrix_TData.Identity(RowCount);
  me := Self.Copy;
  WriteLn('me:');
  me.Print;

  for r := 0 to RowCount - 1  do
    for c := 0 to ColumnCount do
    begin
      v := 0;
      if r = c then
        v := 1;
      if Cell[r, c] <> v then
      begin
        Coef := 0;
        for r1 := r - 1 downto 0 do
          if me.Cell[r1, c] <> 0 then
          begin
            Coef := me.Cell[r, c] / me.Cell[r1, c];

            for c1 := 0 to RowCount - 1 do
              me.Cell[r, c1] := me.Cell[r, c1] - Coef * me.Cell[r1, c1];
            for c1 := 0 to RowCount - 1 do
              InvMat.Cell[r, c1] := InvMat.Cell[r, c1] - Coef * InvMat.Cell[r1, c1];

            Break;
          end;
        WriteLn('me:');
        me.Print;
        WriteLn('InvMat:');
        InvMat.Print;
        WriteLn;
      end;
    end;

  det := 1.0;
  for r := 0 to RowCount - 1 do
    det *= me.Cell[r, r];
  Me.Free;
  Result := det <> 0;
end;

function TMatrix.Multiply(b: TMatrix_TData): TMatrix_TData;
var
  r, c: Integer;
  Sum: TData;
  i: Integer;

begin
  assert(Self.ColumnCount = b.RowCount);
  Result := TMatrix_TData.Create(Self.RowCount, b.ColumnCount);

  for r := 0 to RowCount - 1 do
    for c := 0 to  b.ColumnCount - 1 do
    begin
      Sum := 0;

      for i := 0 to ColumnCount - 1 do
        Sum += Self.Cell[r, i] * b.Cell[i, c];

      Result.Cell[r, c] := Sum;
    end;
end;

constructor TVector.Create(n: Integer);
begin
  inherited Create(n , 1);
end;

function TVector.Copy: TVector;
var
  r: Integer;

begin
  Result := TVector.Create(RowCount);

  for r := 0 to RowCount - 1 do
    Result.Cell[r, 0] := self.Item[r];
end;

function TVector.GetItem(Index: Integer): TData;
begin
  Result := Self.Cell[Index, 0];
end;

procedure TVector.SetItem(Index: Integer; n: TData);
begin
  Self.Cell[Index, 0] := n;
end;

end.

