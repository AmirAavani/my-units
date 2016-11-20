unit CRTUnit;
{$ASSERTIONS ON}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GenericCollectionUnit;

type
  TIntList = specialize TGenericCollection<Int64>;

  { TCRTProblem }

  TCRTProblem = class(TObject)
  private
    function GetAi(Index: Integer): Integer;
    function GetMi(Index: Integer): Integer;
    function GetN: Integer;
  public
    property N : Integer read GetN;
    property Ai[Index: Integer]: Integer read GetAi;
    property Mi[Index: Integer]: Integer read GetMi;

    constructor Create(Mis, Ais: TIntList);
    destructor Destroy; override;

    function ToString: AnsiString;

  private
    FModulos: TIntList;
    FAi: TIntList;

  end;

implementation

{ TCRTProblem }

function TCRTProblem.GetAi(Index: Integer): Integer;
begin
  Result := FAi[Index];

end;

function TCRTProblem.GetMi(Index: Integer): Integer;
begin
  Result := FModulos[Index];

end;

function TCRTProblem.GetN: Integer;
begin
  Result := FAI.Count;

end;

constructor TCRTProblem.Create(Mis, Ais: TIntList);
begin
  inherited Create;
  Assert(Mis.Count = Ais.Count);

  FModulos := TIntList.Create(Mis);
  FAi := TIntList.Create(Ais);
end;

destructor TCRTProblem.Destroy;
begin
  FAi.Free;
  FModulos.Free;

  inherited Destroy;
end;

function TCRTProblem.ToString: AnsiString;
var
  i: Integer;

begin
  Result := '';
  for i := 0 to N do
    Result += '(' + IntToStr(Ai[i]) + ':' + IntToStr(Mi[i]) + ')';
end;

end.

