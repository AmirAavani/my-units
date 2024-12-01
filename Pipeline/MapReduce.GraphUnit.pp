unit MapReduce.GraphUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, GenericCollectionUnit, MapReduce.MapperUnit,
  MapReduce.ReaderUnit, MapReduce.UtilsUnits, MapReduce.NodeUnit,
  MapReduce.ConfigUnit;

type
  { TGraph }

  TGraph = class(TObject)
  protected
    FName: AnsiString;
    FInputs: TNodes;
    Compiled: Boolean;

  public
    property Inputs: TNodes read FInputs;

    constructor Create(const Name: AnsiString);
    destructor Destroy; override;

    function AddInput(constref Text: AnsiString): TNode;

    procedure MustCompile;

    function Run(Config: TRunConfig): Boolean;

    class function CreateGraph(const Name: AnsiString): TGraph;
  end;


implementation
type

  { EGraphIsNotCompiled }

  EGraphIsNotCompiled = class(Exception)
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

  FInputs := TNodes.Create;
  Compiled := False;
end;

destructor TGraph.Destroy;
begin
  FInputs.Free;

  inherited Destroy;
end;

function TGraph.AddInput(constref Text: AnsiString): TNode;
begin
  //Result := TInputNode.Create(Pattern, nil);
  FInputs.Add(Result);

end;

procedure TGraph.MustCompile;
begin
  Self.Compiled := True;

end;

function TGraph.Run(Config: TRunConfig): Boolean;
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

  {Result := TPipeline.Create(Self.FName, Config);
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
  }
end;

class function TGraph.CreateGraph(const Name: AnsiString): TGraph;
begin
  Result := TGraph.Create(Name);

end;


end.
