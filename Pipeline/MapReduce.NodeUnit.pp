unit MapReduce.NodeUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, GenericCollectionUnit, MapReduce.MapperUnit,
  MapReduce.UtilsUnits;

type
  TNode = class;

  TNodes = specialize TCollection<TNode>;

  { TNode }

  TNode = class(TObject)
  private
    FShardCount: Integer;

  protected
  type
    TTransition = record
      Target: TNode;
      Mapper: MapReduce.MapperUnit.TMapper;
      ReshardTo: Integer;

    end;
  protected
    FParent: TNode;
    Children: TNodes;
    Transitions: specialize TCollection<TTransition>;
    DestinationPatten: TPattern;

  protected
    property Parent: TNode read FParent;
    property ShardCount: Integer read FShardCount;

  public
    constructor Create(_Parent: TNode);
    destructor Destroy; override;

    function Map(Mapper: MapReduce.MapperUnit.TMapper): TNode;
    function Map(FuncMapper: MapReduce.MapperUnit.TFuncMapper): TNode;
    procedure SetDestination(const Pattern: AnsiString);
    function Reshard(n: Integer): TNode;

  end;


implementation

type
  EIncompatibleSettings = class(Exception)
  end;

{ TNode }

constructor TNode.Create(_Parent: TNode);
begin
  inherited Create;

  Self.Children := TNodes.Create;
  FParent := _Parent;
  if Parent <> nil then
    FParent.Children.Add(Self);
  Transitions := (specialize TCollection<TTransition>).Create;
  FShardCount := 1;

end;

destructor TNode.Destroy;
begin
  Transitions.Free;

  inherited Destroy;
end;

function TNode.Map(Mapper: MapReduce.MapperUnit.TMapper): TNode;
var
  Transition: TTransition;
begin
  Result := TNode.Create(Self);
  Transition.Target := Result;
  Transition.Mapper := Mapper;

  Self.Transitions.Add(Transition);
end;

function TNode.Map(FuncMapper: MapReduce.MapperUnit.TFuncMapper): TNode;
var
  Transition: TTransition;
begin
  Result := TNode.Create(Self);
  Result.FShardCount := Self.ShardCount;
  Transition.Target := Result;
  Transition.Mapper := TMapper.CreateFromFuncMapper(FuncMapper);

  Transitions.Add(Transition);

end;

procedure TNode.SetDestination(const Pattern: AnsiString);
begin
  DestinationPatten := TPattern.Create(Pattern);
  if Self.ShardCount <> DestinationPatten.Count then
    raise EIncompatibleSettings.Create(Format(
      'ShardCount: %d DestinationPatten.Count: %d',
      [Self.ShardCount, Self.DestinationPatten.Count]));
end;

function TNode.Reshard(n: Integer): TNode;
var
  Transition: TTransition;

begin
  if (Self.ShardCount mod n <> 0) and (n mod Self.ShardCount <> 0) then
    raise EIncompatibleSettings.Create(
      Format('Reshard to %d shard(s) is not valid', [n]));
  Result := TNode.Create(Self);
  Result.FShardCount := n;
  Transition.Target := Result;
  Transition.ReshardTo := n;

  Transitions.Add(Transition);

end;

end.

