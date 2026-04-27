unit RadixTreeUnit;

{$mode objfpc}
{$COPERATORS ON}
{$modeswitch ADVANCEDRECORDS}
interface

uses
  Classes, SysUtils, Math;

type
  { Base class constraint for TData }
  TSerializable = class(TObject)
  public
    procedure SaveToStream(Stream: TStream); virtual; abstract;
    procedure LoadFromStream(Stream: TStream); virtual; abstract;
  end;

  { TNibbles: Byte array wrapper }
  
  TNibbles = record
  private
    function GetItemPtr: PByte; inline;

  public
    FData: TBytes;
    FLen: Integer;
    function GetItem(Index: Integer): Byte;
    class function Create(const S: AnsiString): TNibbles; static;
    class function Create(const SPtr: PByte; Len: Integer): TNibbles; static;

    function ToString: AnsiString;
    procedure Truncate(NewLength: Integer);
    function Equals(const Other: TNibbles): Boolean;
    
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);

    property Count: Integer read FLen;
    property ItemsPtr: PByte read GetItemPtr;
    property Items[Index: Integer]: Byte read GetItem; default;
  end;

  { TTree }

  generic TTree<TData: TSerializable> = class(TObject)
  public type
    { Forward declarations }
    TNode = class;

    { TEdge }
    TEdge = record
      Path: TNibbles;
      Next: TNode;
    end;

    { TNode }
    TNode = class(TObject)
    private
      FID: Integer;
      FEdges: array[0..255] of TEdge;
      FIsTerminal: Boolean;
      FData: TData;

    public
      property ID: Integer read FID;
      property IsTerminal: Boolean read FIsTerminal write FIsTerminal;
      property Data: TData read FData write FData;

      constructor Create(AID: Integer);
      destructor Destroy; override;
    end;

  private
    type
      TNodeArray = array of TNode;
      
    var
      LastUsedID: Integer;
      FRoot: TNode;
      FSize: Integer;
      FAllNodes: TNodeArray;
      
    function FindLongestCommonPrefix(a, b: PByte; MaxLen: Integer): Integer;
    procedure RegisterNode(Node: TNode);
    procedure FreeAllNodes;

  public
    constructor Create;
    destructor Destroy; override;
    function Insert(const S: string): TNode;
    function Search(const S: string): TNode;
    function Contains(const S: string): Boolean;
    procedure Print;
    
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);
    
    property Size: Integer read FSize;
    property Root: TNode read FRoot;
  end;

implementation

{ TNibbles }

function TNibbles.GetItemPtr: PByte;
begin
  if FLen > 0 then
    Result := PByte(@FData[0])
  else
    Result := nil;
end;

function TNibbles.GetItem(Index: Integer): Byte;
begin
  if (Index < 0) or (Index >= FLen) then
    raise Exception.CreateFmt('Index %d out of bounds [0..%d]', [Index, FLen - 1]);
  
  Result := FData[Index];
end;

class function TNibbles.Create(const S: AnsiString): TNibbles;
begin
  Result.FLen := Length(S);
  SetLength(Result.FData, Length(S));

  if Result.FLen > 0 then
    Move(S[1], Result.FData[0], Result.FLen);
end;

class function TNibbles.Create(const SPtr: PByte; Len: Integer): TNibbles;
begin
  Result.FLen := Len;
  SetLength(Result.FData, Len);

  if Len > 0 then
    Move(SPtr^, Result.FData[0], Len);
end;

procedure TNibbles.Truncate(NewLength: Integer);
begin
  if NewLength < 0 then
    NewLength := 0;
  if NewLength > FLen then
    NewLength := FLen;
    
  Self.FLen := NewLength;
end;

function TNibbles.Equals(const Other: TNibbles): Boolean;
var
  i: Integer;
begin
  if Self.FLen <> Other.FLen then
    Exit(False);
  
  for i := 0 to FLen - 1 do
    if Self.FData[i] <> Other.FData[i] then
      Exit(False);
  
  Result := True;
end;

procedure TNibbles.SaveToStream(Stream: TStream);
begin
  Stream.WriteBuffer(FLen, SizeOf(FLen));
  if FLen > 0 then
    Stream.WriteBuffer(FData[0], FLen);
end;

procedure TNibbles.LoadFromStream(Stream: TStream);
begin
  Stream.ReadBuffer(FLen, SizeOf(FLen));
  if FLen > 0 then
  begin
    SetLength(FData, FLen);
    Stream.ReadBuffer(FData[0], FLen);
  end
  else
    SetLength(FData, 0);
end;

function TNibbles.ToString: AnsiString;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to FLen - 1 do
    Result += Chr(FData[i]);
end;

{ TTree }

constructor TTree.Create;
begin
  inherited Create;
  FRoot := nil;
  LastUsedID := 0;
  FSize := 0;
  SetLength(FAllNodes, 0);
end;

destructor TTree.Destroy;
begin
  FreeAllNodes;
  inherited Destroy;
end;

procedure TTree.FreeAllNodes;
var
  i: Integer;
begin
  for i := 0 to High(FAllNodes) do
    FAllNodes[i].Free;
  SetLength(FAllNodes, 0);
  FRoot := nil;
end;

procedure TTree.RegisterNode(Node: TNode);
var
  OldLen: Integer;
begin
  OldLen := Length(FAllNodes);
  SetLength(FAllNodes, OldLen + 1);
  FAllNodes[OldLen] := Node;
end;

function TTree.FindLongestCommonPrefix(a, b: PByte; MaxLen: Integer): Integer;
begin
  Result := 0;
  while (Result < MaxLen) and (a^ = b^) do
  begin
    Inc(Result);
    Inc(a);
    Inc(b);
  end;
end;

function TTree.Insert(const S: string): TNode;
var
  CurrentNode: TNode;
  QPtr, SuffixPtr, EdgePathPtr: PByte;
  EdgePtr: ^TEdge;
  SplitNode, NewTerminalNode: TNode;
  Query: TNibbles;
  BranchIndex, SuffixIndex, NewQueryIndex: Integer;
  CommonLen, QueryOffset, RemainingQueryLen: Integer;

begin
  Query := TNibbles.Create(S);

  if FRoot = nil then
  begin
    Inc(LastUsedID);
    FRoot := TNode.Create(LastUsedID);
    RegisterNode(FRoot);
  end;

  CurrentNode := FRoot;
  QueryOffset := 0;

  while QueryOffset < Query.Count do
  begin
    QPtr := Query.ItemsPtr + QueryOffset;
    BranchIndex := QPtr^;

    EdgePtr := @CurrentNode.FEdges[BranchIndex];

    // CASE A: NEW EDGE
    if EdgePtr^.Next = nil then
    begin
      Inc(LastUsedID);
      NewTerminalNode := TNode.Create(LastUsedID);
      NewTerminalNode.FIsTerminal := True;
      NewTerminalNode.FData := TData.Create;  // Create data instance
      RegisterNode(NewTerminalNode);
      EdgePtr^.Path := TNibbles.Create(QPtr, Query.Count - QueryOffset);
      EdgePtr^.Next := NewTerminalNode;
      Inc(FSize);
      Exit(NewTerminalNode);
    end;

    // CASE B: MATCHING EDGE
    EdgePathPtr := EdgePtr^.Path.ItemsPtr;
    CommonLen := FindLongestCommonPrefix(QPtr, EdgePathPtr,
                   Min(Query.Count - QueryOffset, EdgePtr^.Path.Count));

    // B.1 FULL MATCH
    if CommonLen = EdgePtr^.Path.Count then
    begin
      QueryOffset += CommonLen;
      CurrentNode := EdgePtr^.Next;
      if QueryOffset = Query.Count then
      begin
        if not CurrentNode.FIsTerminal then
        begin
          CurrentNode.FIsTerminal := True;
          CurrentNode.FData := TData.Create;  // Create data instance
          Inc(FSize);
        end;
        Exit(CurrentNode);
      end;

      Continue;
    end;

    // B.2 PARTIAL MATCH (SPLIT)
    Inc(LastUsedID);
    SplitNode := TNode.Create(LastUsedID);
    RegisterNode(SplitNode);

    // Old suffix becomes a child of SplitNode
    SuffixPtr := EdgePathPtr + CommonLen;
    SuffixIndex := SuffixPtr^;
    SplitNode.FEdges[SuffixIndex].Path := TNibbles.Create(SuffixPtr, EdgePtr^.Path.Count - CommonLen);
    SplitNode.FEdges[SuffixIndex].Next := EdgePtr^.Next;

    // Current edge becomes the prefix pointing to SplitNode
    EdgePtr^.Path.Truncate(CommonLen);
    EdgePtr^.Next := SplitNode;

    // Add the new query branch if needed
    RemainingQueryLen := (Query.Count - QueryOffset) - CommonLen;
    if RemainingQueryLen = 0 then
    begin
      SplitNode.FIsTerminal := True;
      SplitNode.FData := TData.Create;  // Create data instance
      Inc(FSize);
      Result := SplitNode;
    end
    else
    begin
      Inc(LastUsedID);
      NewTerminalNode := TNode.Create(LastUsedID);
      NewTerminalNode.FIsTerminal := True;
      NewTerminalNode.FData := TData.Create;  // Create data instance
      RegisterNode(NewTerminalNode);
      SuffixPtr := QPtr + CommonLen;
      NewQueryIndex := SuffixPtr^;
      SplitNode.FEdges[NewQueryIndex].Path := TNibbles.Create(SuffixPtr, RemainingQueryLen);
      SplitNode.FEdges[NewQueryIndex].Next := NewTerminalNode;
      Inc(FSize);
      Result := NewTerminalNode;
    end;

    Exit;
  end;

  Result := CurrentNode;
end;

function TTree.Search(const S: string): TNode;
var
  CurrentNode: TNode;
  QPtr, EdgePathPtr: PByte;
  EdgePtr: ^TEdge;
  Query: TNibbles;
  BranchIndex: Integer;
  CommonLen, QueryOffset: Integer;

begin
  Result := nil;
  
  if FRoot = nil then
    Exit;

  Query := TNibbles.Create(S);
  CurrentNode := FRoot;
  QueryOffset := 0;

  while QueryOffset < Query.Count do
  begin
    QPtr := Query.ItemsPtr + QueryOffset;
    BranchIndex := QPtr^;

    EdgePtr := @CurrentNode.FEdges[BranchIndex];

    // No edge with this prefix
    if EdgePtr^.Next = nil then
      Exit(nil);

    // Check if query matches the edge path
    EdgePathPtr := EdgePtr^.Path.ItemsPtr;
    CommonLen := FindLongestCommonPrefix(QPtr, EdgePathPtr,
                   Min(Query.Count - QueryOffset, EdgePtr^.Path.Count));

    // Partial match - string not in tree
    if CommonLen < EdgePtr^.Path.Count then
      Exit(nil);

    // Full edge match - continue traversal
    QueryOffset += CommonLen;
    CurrentNode := EdgePtr^.Next;
  end;

  // Found the node - check if it's terminal
  if CurrentNode.FIsTerminal then
    Result := CurrentNode
  else
    Result := nil;
end;

function TTree.Contains(const S: string): Boolean;
begin
  Result := Search(S) <> nil;
end;

procedure TTree.SaveToStream(Stream: TStream);
var
  NumNodes: Integer;
  i, j, NodeIndex: Integer;
  Node: TNode;
  HasEdge: Boolean;
  
  function GetNodeIndex(ANode: TNode): Integer;
  var
    k: Integer;
  begin
    for k := 0 to High(FAllNodes) do
      if FAllNodes[k] = ANode then
        Exit(k);
    Result := -1;
  end;
  
begin
  // Write tree metadata
  Stream.WriteBuffer(FSize, SizeOf(FSize));
  Stream.WriteBuffer(LastUsedID, SizeOf(LastUsedID));
  
  NumNodes := Length(FAllNodes);
  Stream.WriteBuffer(NumNodes, SizeOf(NumNodes));
  
  if NumNodes = 0 then
    Exit;
  
  // Write root index
  NodeIndex := GetNodeIndex(FRoot);
  Stream.WriteBuffer(NodeIndex, SizeOf(NodeIndex));
  
  // Write each node
  for i := 0 to High(FAllNodes) do
  begin
    Node := FAllNodes[i];
    Stream.WriteBuffer(Node.FID, SizeOf(Node.FID));
    Stream.WriteBuffer(Node.FIsTerminal, SizeOf(Node.FIsTerminal));
    
    // Write terminal data if terminal
    if Node.FIsTerminal and (Node.FData <> nil) then
      Node.FData.SaveToStream(Stream);
    
    // Write edges
    for j := 0 to 255 do
    begin
      HasEdge := Node.FEdges[j].Next <> nil;
      Stream.WriteBuffer(HasEdge, SizeOf(HasEdge));
      
      if HasEdge then
      begin
        Node.FEdges[j].Path.SaveToStream(Stream);
        NodeIndex := GetNodeIndex(Node.FEdges[j].Next);
        Stream.WriteBuffer(NodeIndex, SizeOf(NodeIndex));
      end;
    end;
  end;
end;

procedure TTree.LoadFromStream(Stream: TStream);
var
  NumNodes: Integer;
  RootIndex: Integer;
  i, j: Integer;
  NodeID: Integer;
  IsTerminal: Boolean;
  HasEdge: Boolean;
  NextIndex: Integer;
  Node: TNode;
  
  type
    TEdgeInfo = record
      HasEdge: Boolean;
      Path: TNibbles;
      NextIndex: Integer;
    end;
    TNodeEdges = array[0..255] of TEdgeInfo;
    
  var
    AllEdges: array of TNodeEdges;
    
begin
  // Clear existing tree
  FreeAllNodes;
  
  // Read tree metadata
  Stream.ReadBuffer(FSize, SizeOf(FSize));
  Stream.ReadBuffer(LastUsedID, SizeOf(LastUsedID));
  Stream.ReadBuffer(NumNodes, SizeOf(NumNodes));
  
  if NumNodes = 0 then
    Exit;
  
  // Read root index
  Stream.ReadBuffer(RootIndex, SizeOf(RootIndex));
  
  // Allocate arrays
  SetLength(FAllNodes, NumNodes);
  SetLength(AllEdges, NumNodes);
  
  
  // Initialize AllEdges
  for i := 0 to NumNodes - 1 do
    for j := 0 to 255 do
      AllEdges[i][j].HasEdge := False;
  // First pass: Create all nodes and read edge info
  for i := 0 to NumNodes - 1 do
  begin
    Stream.ReadBuffer(NodeID, SizeOf(NodeID));
    Stream.ReadBuffer(IsTerminal, SizeOf(IsTerminal));
    
    Node := TNode.Create(NodeID);
    Node.FIsTerminal := IsTerminal;
    
    // Read terminal data if terminal
    if IsTerminal then
    begin
      Node.FData := TData.Create;
      Node.FData.LoadFromStream(Stream);
    end;
    
    FAllNodes[i] := Node;
    
    // Read edge data
    for j := 0 to 255 do
    begin
      Stream.ReadBuffer(HasEdge, SizeOf(HasEdge));
      AllEdges[i][j].HasEdge := HasEdge;
      
      if HasEdge then
      begin
        AllEdges[i][j].Path.LoadFromStream(Stream);
        Stream.ReadBuffer(NextIndex, SizeOf(NextIndex));
        AllEdges[i][j].NextIndex := NextIndex;
      end;
    end;
  end;
  
  // Second pass: Link nodes
  for i := 0 to NumNodes - 1 do
  begin
    Node := FAllNodes[i];
    for j := 0 to 255 do
    begin
      if AllEdges[i][j].HasEdge then
      begin
        Node.FEdges[j].Path := AllEdges[i][j].Path;
        Node.FEdges[j].Next := FAllNodes[AllEdges[i][j].NextIndex];
      end
      else
        Node.FEdges[j].Next := nil;
    end;
  end;
  // Set root
  FRoot := FAllNodes[RootIndex];
end;

procedure TTree.Print;
  procedure DFS(const Node: TNode; const Prefix: AnsiString);
  var
    i: Integer;
  begin
    WriteLn(Format('%sID: %d', [Prefix, Node.FID]));

    if Node.FIsTerminal then
      WriteLn(Format('%s[TERMINAL]', [Prefix + '  ']));

    for i := 0 to 255 do
    begin
      if Node.FEdges[i].Next <> nil then
        DFS(
          Node.FEdges[i].Next,
          Prefix + '  (' + Node.FEdges[i].Path.ToString + ') -> '
        );
    end;
  end;

begin
  if FRoot = nil then
  begin
    WriteLn('Empty Tree');
    Exit;
  end;

  DFS(FRoot, '');
  WriteLn;
end;

{ TTree.TNode }

constructor TTree.TNode.Create(AID: Integer);
var
  i: Integer;
begin
  inherited Create;
  FID := AID;
  FIsTerminal := False;
  FData := nil;  // Initialize to nil
  
  for i := 0 to 255 do
    FEdges[i].Next := nil;
end;

destructor TTree.TNode.Destroy;
begin
  // Free data if this is a terminal node
  if FData <> nil then
    FData.Free;
    
  // Don't free child nodes - they're managed by the tree's FAllNodes array
  inherited Destroy;
end;

end.
