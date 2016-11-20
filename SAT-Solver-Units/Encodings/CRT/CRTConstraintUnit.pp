unit CRTProblemUnit;
{$ASSERTIONS ON}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GenericCollectionUnit, BaseProblemUnit, BitVectorUnit;

type
  TIntList = specialize TGenericCollection<Int64>;

  { TCRTProblem }
  {
    States the following problem:
     N mod Mi[i] = Ri[i]
  }
  TCRTProblem = class(TBaseConstraint)
  private
    FN: TBitVector;
    function GetRi(Index: Integer): TBitVector;
    function GetMi(Index: Integer): Integer;
    function GetCount: Integer;
  public
    property Count : Integer read GetCount;
    property Ri[Index: Integer]: TBitVector read GetRi;
    property Mi[Index: Integer]: Integer read GetMi;
    property N: TBitVector read FN;

    // This class owns Mis, Num and Ris.
    constructor Create(Num: TBitVector; Mis: TIntList; Ris: TBitVectorList);
    destructor Destroy; override;

    function ToString: AnsiString; override;

  private
    FModulos: TIntList;
    FRis: TBitVectorList;

  end;

  { TBinaryCRTProblem }

  TBinaryCRTProblem = class(TCRTProblem)
  public
    // This class owns the input parameters
    constructor Create(Num: TBitVector; M1, M2: Int64; R1, R2: TBitVector);
  end;

implementation

{ TBinaryCRTProblem }

constructor TBinaryCRTProblem.Create(Num: TBitVector; M1, M2: Int64; R1,
  R2: TBitVector);
var
  Mis: TIntList;
  Ris: TBitVectorList;

begin
  Mis := TIntList.Create;
  Ris := TBitVectorList.Create;

  Mis.Add(M1);
  Mis.Add(M2);
  Ris.Add(R1);
  Ris.Add(R2);

  inherited Create(Num, Mis, Ris);

end;

{ TCRTProblem }

function TCRTProblem.GetRi(Index: Integer): TBitVector;
begin
  Result := FRis[Index];

end;

function TCRTProblem.GetMi(Index: Integer): Integer;
begin
  Result := FModulos[Index];

end;

function TCRTProblem.GetCount: Integer;
begin
  Result := FRis.Count;

end;

constructor TCRTProblem.Create(Num: TBitVector; Mis: TIntList;
  Ris: TBitVectorList);
begin
  inherited Create;
  Assert(Mis.Count = Ris.Count);

  FModulos := Mis;
  FRis := Ris;
  FN := Num;

end;

destructor TCRTProblem.Destroy;
begin
  FRis.Free;
  FModulos.Free;
  FN.Free;

  inherited Destroy;
end;

function TCRTProblem.ToString: AnsiString;
var
  i: Integer;

begin
  Result := 'N = ' + N.ToString + ' ';
  for i := 0 to Count - 1 do
    Result += '(' + Ri[i].ToString + ':' + IntToStr(Mi[i]) + ')';
end;

end.

