unit MapReduce.NodeUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, GenericCollectionUnit, MapReduce.KeyValueUnit,
  MapReduce.UtilsUnits, ProtoHelperUnit, ThreadSafeStackUnit;

type
  TNode = class;
  TMapper = class;

  TNodes = specialize TCollection<TNode>;
  TKVStack = specialize TThreadSafeStack<TKeyValue>;

  { TNode }

  TNode = class(TObject)
  private
    class var Index: Integer;

  private
    FShardCount: Integer;
    FNodeIndex: Integer;
    FData: TKVStack;

    function GetData: TKVStack;
    function GetNodeIndex: Integer;

  public
  type
    TTransition = record
      Target: TNode;
      Mapper: TMapper;
      ReshardTo: Integer;

    end;
    //TBaseMessage
  public
    FParent: TNode;
    Children: TNodes;
    Transitions: specialize TCollection<TTransition>;
    DestinationPatten: TPattern;

  public
    property Parent: TNode read FParent;
    property ShardCount: Integer read FShardCount;
    property NodeIndex: Integer read GetNodeIndex;
    property Data: TKVStack read GetData;

  public
    constructor Create(_Parent: TNode);
    destructor Destroy; override;

    function Map(Mapper: TMapper): TNode;
    // function Map(MapperFunc: MapReduce.MapperUnit.TMapperFunc): TNode;
    procedure SetDestination(const Pattern: AnsiString);
    function Reshard(n: Integer): TNode;

    procedure Send(constref Key: AnsiString; Value: TBaseMessage);
  end;

  { TStartingNode }

  TStartingNode = class(TNode)
  public
    constructor Create;

  end;

  { TMapper }

  TMapper = class(TObject)
  public
    constructor Create;
    destructor Destroy; override;

    function Process(constref InputKey: AnsiString; InputValue: TBaseMessage;
      Sender: TNode): Boolean; virtual; abstract;

  end;


implementation
uses
  MapReduce.OutputMappersUnit;

type
  EIncompatibleSettings = class(Exception)
  end;

{ TNode }

function TNode.GetNodeIndex: Integer;
begin
  if Self = nil then
    Exit(-1);

  Result := FNodeIndex;

end;

function TNode.GetData: TKVStack;
begin
  Result := FData;

end;

constructor TNode.Create(_Parent: TNode);
begin
  inherited Create;

  Inc(TNode.Index);
  Self.FNodeIndex := TNode.Index;

  Self.Children := TNodes.Create;
  FParent := _Parent;
  if Parent <> nil then
    FParent.Children.Add(Self);
  Transitions := (specialize TCollection<TTransition>).Create;
  FShardCount := 1;

  FData := TKVStack.Create;
end;

destructor TNode.Destroy;
begin
  Transitions.Free;
  FData.Free;

  inherited Destroy;
end;

function TNode.Map(Mapper: TMapper): TNode;
var
  Transition: TTransition;
begin
  Result := nil;
  if not (Mapper is TBaseOutputMapper) then
    Result := TNode.Create(Self);

  Transition.Target := Result;
  Transition.Mapper := Mapper;

  Self.Transitions.Add(Transition);
end;

{
function TNode.Map(MapperFunc: MapReduce.MapperUnit.TMapperFunc): TNode;
var
  Transition: TTransition;
begin
  Result := TNode.Create(Self);
  Result.FShardCount := Self.ShardCount;
  Transition.Target := Result;
  Transition.Mapper := MapReduce.MapperUnit.CreateMapperFromFunc(MapperFunc, Result);

  Transitions.Add(Transition);

end;
}

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

procedure TNode.Send(constref Key: AnsiString; Value: TBaseMessage);
var
  kv: TKeyValue;

begin
  WriteLn(Format('%d Key: %s', [ThreadID, Key]));
  kv.Init(Key, Value);
  WriteLn(Format('%d Key: %s', [ThreadID, kv.Key]));
  FData.Push(kv);

end;

{ TStartingNode }

constructor TStartingNode.Create;
begin
  inherited Create(nil);

end;

{ TMapper }

constructor TMapper.Create;
begin
  inherited Create;

end;

destructor TMapper.Destroy;
begin

  inherited Destroy;
end;


initialization
  TNode.Index := 0;
end.

