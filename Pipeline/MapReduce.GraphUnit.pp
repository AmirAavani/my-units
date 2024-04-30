unit MapReduce.GraphUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, GenericCollectionUnit, MapReduce.MapperUnit,
  MapReduce.ReaderUnit, MapReduce.UtilsUnits, PipelineUnit;

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

  { TRunConfig }

  TRunConfig = class(TObject)
  public
  type
    TMode = (InMemory = 1);

  private
    FMode: TMode;
    FNumProcesses: Integer;
    FProcessIndex: Integer;

  public
    property Mode: TMode read FMode;
    property NumProcesses: Integer read FNumProcesses;
    property ProcessIndex: Integer read FProcessIndex;

    constructor Create;

    class function NewRunConfg: TRunConfig;
    function SetMode(AValue: TMode): TRunConfig;
    function SetNumProcesses(n: Integer): TRunConfig;
    function SetProcessIndex(n: Integer): TRunConfig;


  end;

  { TGraph }

  TGraph = class(TObject)
  protected
    FName: AnsiString;
    FInputs: TNodes;
    Compiled: Boolean;

  public
    property Inputs: TNodes read FInputs;

    constructor Create(Name: AnsiString);
    destructor Destroy; override;

    function AddInput(constref Pattern: AnsiString; Reader: TReader): TNode;

    procedure MustCompile;

    function ExportAsPipeline(Config: TPipelineConfig): TPipeline;
  end;


implementation

type
  EIncompatibleSettings = class(Exception)
  end;

  { EGraphIsNotCompiled }

  EGraphIsNotCompiled = class(Exception)
  public
    constructor Create;
  end;

  { TThreadPool }

  TThreadPool = class(TObject)

  end;


  { TInputNode }

  TInputNode = class(TNode)
  private
    FPattern: AnsiString;
    FReader: TReader;

  public
    constructor Create(constref p: AnsiString; Reader: TReader);

  end;

{ EGraphIsNotCompiled }

constructor EGraphIsNotCompiled.Create;
begin
  inherited Create('Please call MustCompile function!');

end;

  { TInputNode }

constructor TInputNode.Create(constref p: AnsiString; Reader: TReader);
begin
  inherited Create(nil);

  Self.FPattern := p;
  Self.FReader := Reader;

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

{ TRunConfig }

function TRunConfig.SetMode(AValue: TMode): TRunConfig;
begin
  Result := Self;
  Result.FMode := AValue;
end;

function TRunConfig.SetNumProcesses(n: Integer): TRunConfig;
begin
  Result := Self;
  Result.FNumProcesses := n;

end;

function TRunConfig.SetProcessIndex(n: Integer): TRunConfig;
begin
  Result := Self;
  Result.FProcessIndex := n;

end;

constructor TRunConfig.Create;
begin
  inherited Create;

  Self.FMode := InMemory;
  Self.FNumProcesses := 1;
  Self.FProcessIndex := 0;

end;

class function TRunConfig.NewRunConfg: TRunConfig;
begin
  Result := TRunConfig.Create;

end;

{ TGraph }

constructor TGraph.Create(Name: AnsiString);
begin
  inherited Create;

  FInputs := TNodes.Create;
  Compiled := False;
end;

destructor TGraph.Destroy;
begin
  FInputs.Free;

  inherited Destroy;
end;

function TGraph.AddInput(constref Pattern: AnsiString; Reader: TReader): TNode;
begin
  Result := TInputNode.Create(Pattern, nil);
  FInputs.Add(Result);

end;

procedure TGraph.MustCompile;
begin
  Self.Compiled := True;

end;

function TGraph.ExportAsPipeline(Config: TPipelineConfig): TPipeline;
  function IsVisited(aNode: TNode; VisitedNodes: TNodes): Boolean;
  var
    n: TNode;

  begin
    for n in VisitedNodes do
      if n = aNode then
        Exit(True);

    Result := False;
  end;
var
  Queue: TNodes;
  FoQ: Integer;
  Node: TNode;


begin
  if not Self.Compiled then
    raise EGraphIsNotCompiled.Create;

  Result := TPipeline.Create(Self.FName, Config);
  Foq := 0;
  Queue := TNodes.Create;

  for Node in Self.Inputs do
  begin
    if IsVisited(Node, Queue) then
      Queue.Add(Node);
  end;

  while FoQ < Queue.Count do
  begin
    Node := Queue[FoQ];

  end;
end;


end.
