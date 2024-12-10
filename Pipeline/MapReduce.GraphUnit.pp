unit MapReduce.GraphUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, GenericCollectionUnit,
  MapReduce.ReaderUnit, MapReduce.UtilsUnits, MapReduce.NodeUnit,
  MapReduce.ConfigUnit, MapReduce.InputMappersUnit;

type
  { TGraph }

  TGraph = class(TObject)
  protected
    FName: AnsiString;
    FStartingNode: TStartingNode;
    Compiled: Boolean;

  public
    property StartingNode: TStartingNode read FStartingNode;

    constructor Create(const Name: AnsiString);
    destructor Destroy; override;

    procedure MustCompile;
    const procedure Describe;

    function Run(Config: TRunConfig): Boolean;

    class function CreateGraph(const Name: AnsiString): TGraph;
  end;


implementation
uses
  MapReduce.TypesUnit, ThreadPoolUnit, ThreadSafeStackUnit, ProtoHelperUnit,
  MapReduce.KeyValueUnit, SyncUnit;

type

  { EGraphIsNotCompiled }

  EGraphIsNotCompiled = class(EMapReduce)
  public
    constructor Create;
  end;

{ EGraphIsNotCompiled }

constructor EGraphIsNotCompiled.Create;
begin
  inherited Create('Please call MustCompile function!');

end;

{ TGraph }

constructor TGraph.Create(const Name: AnsiString);
begin
  inherited Create;

  FStartingNode := TStartingNode.Create;
  Compiled := False;
end;

destructor TGraph.Destroy;
begin
  FStartingNode.Free;

  inherited Destroy;
end;

procedure TGraph.MustCompile;
begin
  Self.Compiled := True;

end;

procedure TGraph.Describe;
  procedure DFS(Node: TNode; const Indent: AnsiString);
  var
    t: TNode.TTransition;

  begin
    if Node = nil then
      Exit;

    for t in Node.Transitions do
    begin
       WriteLn(Format('%sS:%d -> T:%d',
         [Indent, Node.NodeIndex, t.Target.NodeIndex]));
       DFS(t.Target, Indent + '  ');

    end;
  end;

begin
  DFS(Self.StartingNode, '');

end;

function RunTransition(Args: TObjectList): Boolean;
var
  Mapper: TMapper;
  Source, Target: TNode;
  kv: TKeyValue;

begin
  Mapper := Args[0] as TMapper;
  Source := Args[1] as TNode;
  Target := Args[2] as TNode;

  Args.Clear;
  Args.Free;

  WriteLn(Format('%d -> (S: %d, T:%d)',
    [ThreadID, Source.NodeIndex, Target.NodeIndex]));
  Result := True;
  if Source = nil then
  begin
    WriteLn(Format('%d: Before Process', [ThreadID]));
    Mapper.Process('', nil, Target);
    WriteLn(Format('%d: Done', [ThreadID]));
    Exit;

  end;

  while True do
  begin
    kv := Source.Data.Pop;
    WriteLn(Format('ThreadID: %d kv.Key: %s', [ThreadID, kv.Key]));
    if (kv.Key = '') and (kv.Value = nil) and Source.Data.Done then
    begin
      if Target <> nil then
        Target.Data.Done := True;
      WriteLn(Format('%d: Done', [ThreadID]));
      Break;

    end;
    if not Mapper.Process(kv.Key, kv.Value, Target) then
    begin
      WriteLn(Format('Failed in mapping Key: %s', [kv.Key]));
      Halt(2);
    end;

  end;
  WriteLn(Format('%d Done with S:%d -> T:%d', [ThreadID, Source.NodeIndex, Target.NodeIndex]));


end;

function TGraph.Run(Config: TRunConfig): Boolean;
type
  TNode2StackMap = specialize TMap<TNode, TKVStack>;

var
  Pool: ThreadPoolUnit.TThreadPool;

  procedure Visit(aNode: TNode);
  var
    t: TNode.TTransition;
    Args: TObjectList;

  begin
    if aNode = nil then
      Exit;

    for t in aNode.Transitions do
    begin
       Args := TObjectList.Create;
       Args.Add(t.Mapper);
       if aNode = Self.StartingNode then
         Args.Add(nil)
       else
         Args.Add(aNode);

       Args.Add(t.Target);
       WriteLn(Format('S: %d T: %d',
       [aNode.NodeIndex, t.Target.NodeIndex]));

       //RunTransition(Args);
      Pool.Run(
        @RunTransition,
        Args,
        nil);

      Visit(t.Target);
    end;

    Result := False;
  end;

begin
  if not Self.Compiled then
    raise EGraphIsNotCompiled.Create;
  Pool := TThreadPool.Create(Config.NumThreads);

  Visit(Self.FStartingNode);

  Pool.Wait;
end;

class function TGraph.CreateGraph(const Name: AnsiString): TGraph;
begin
  Result := TGraph.Create(Name);

end;


end.
