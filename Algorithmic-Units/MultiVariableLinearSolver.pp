unit MultiVariableLinearSolver;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MatrixUnit;

type
  TmatrixDouble = specialize TMatrix<Double>;
  TDoubleMatrix = specialize TMatrix<Double>;
  TDoubleVector = specialize TVector<Double>;

// Solves ax = b;
function Solve(const a: TDoubleMatrix; b: TDoubleVector): TDoubleVector;

implementation

function Solve(const a: TDoubleMatrix; b: TDoubleVector): TDoubleVector;
var
  ac: TDoubleMatrix;
  bc: TDoubleVector;
  r, c: Integer;
  r1, c1: Integer;
  Coef: Extended;
  Sum: Double;

begin   // r x c  c x 1  = > r x 1
  if a.ColumnCount <> b.RowCount then
    raise Exception.Create('Invalid Matrices!');
  if a.ColumnCount <> a.RowCount then
    raise Exception.Create('Invalid Matrices!');

  ac := a.Copy;
  bc := b.Copy;

  for r := 1 to ac.RowCount - 1  do
    for c := 0 to r - 1 do
      if ac.Cell[r, c] <> 0 then
      begin
        Coef := 0;
        for r1 := r - 1 downto 0 do
          if ac.Cell[r1, c] <> 0 then
          begin
            Coef := ac.Cell[r, c] / ac.Cell[r1, c];

            for c1 := 0 to ac.ColumnCount - 1 do
              ac.Cell[r, c1] := ac.Cell[r, c1] - Coef * ac.Cell[r1, c1];
            bc.Item[r] := bc.Item[r] - Coef * bc.Item[r1];

            Break;
          end;
      end;

  Result := TDoubleVector.Create(ac.RowCount);

  for r := ac.RowCount - 1 downto 0 do
  begin
    Sum := 0;
    for c := r + 1 to ac.ColumnCount - 1 do
      Sum += Result.Item[c] * ac.Cell[r, c];
    Result.Item[r] := (bc.Item[r] - Sum) / ac.Cell[r, r]
  end;


  ac.Free;
  bc.Free;
end;

end.

