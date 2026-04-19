unit RadixTreeUnit;

{$mode objfpc}
{$modeswitch ADVANCEDRECORDS}
interface

uses
  Classes, SysUtils, Math, GenericCollectionUnit;

type
  { TTree }

  generic TTree<TTerminalInfo> = class(TObject)
  public type

    { TNibbles: Splitting bytes into 4-bit chunks }

    TNibbles = record
    private
      function GetItemPtr: PByte; inline;

    public
      FData: TBytes; // Reference counted, auto-managed
      FLen: Integer;
      function GetItem(Index: Integer): Byte;
      // Static "constructors" for records
      class function Create(const S: AnsiString): TNibbles; static;
      class function Create(const SPtr: PByte; Len: Integer): TNibbles; static;

      function ToString: AnsiString;
      procedure Truncate(NewLength: Integer);

      property Count: Integer read FLen;
      property ItemsPtr: PByte read GetItemPtr; // Pointer to raw data
      property Items[Index: Integer]: Byte read GetItem; default;
    end;

    { TNode: Forward declaration }

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
      FEdges: array[0..15] of TEdge; // Radix-16 fixed array
      FIsTerminal: Boolean;
      FTerminalInfo: TTerminalInfo;

    public
      property TerminalInfo: TTerminalInfo read FTerminalInfo write FTerminalInfo;

      constructor Create(ID: Integer);
      destructor Destroy; override;

    end;

  private
    LastUsedID: Integer;
    FRoot: TNode;
    FSize: Integer;
    function FindLongestCommonPrefix(a, b: PByte; MaxLen: Integer): Integer;

  public
    constructor Create;
    destructor Destroy; override;
    function Insert(const S: string): TNode;
    procedure Print;
    property Size: Integer read FSize;

  end;

implementation

{ TTree }

constructor TTree.Create;
begin
  inherited Create;

  FRoot := nil;
  LastUsedID := 0;
  FSize := 0;

end;

destructor TTree.Destroy;
begin
  if FRoot <> nil then
    FRoot.Free;

  inherited Destroy;

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
          Inc(FSize);

        end;
        Exit(CurrentNode);

      end;

      Continue;

    end;

    // B.2 PARTIAL MATCH (SPLIT)

    Inc(LastUsedID);
    SplitNode := TNode.Create(LastUsedID);

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
      Inc(FSize);
      Result := SplitNode;

    end
    else
    begin
      Inc(LastUsedID);
      NewTerminalNode := TNode.Create(LastUsedID);
      NewTerminalNode.FIsTerminal := True;
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

procedure TTree.Print;
  procedure DFS(
    const Node: TNode;
    const Prefix: AnsiString;
    LastNibble: Integer;
    HasNibble: Boolean);
  var
    i: Integer;

  begin
    WriteLn(Format('%sID: %d', [Prefix, Node.FID]));

    if Node.FIsTerminal then
      WriteLn(Format('%s[TERMINAL] ID: %d', [Prefix + '  ', Node.FID]));

    for i := 0 to High(Node.FEdges) do
    begin
      if Node.FEdges[i].Next <> nil then
        DFS(
          Node.FEdges[i].Next,
          Prefix + '  (' + Node.FEdges[i].Path.ToString + ') -> ',
          0,
          False
        )

    end;

  end;

begin
  if FRoot = nil then
  begin
    WriteLn('Empty Tree');
    Exit;

  end;

  DFS(FRoot, '', 0, False);
  WriteLn;

end;


{ TTree.TNode }

constructor TTree.TNode.Create(ID: Integer);
begin
  inherited Create;

  FID := ID;
  FIsTerminal := False;

end;

destructor TTree.TNode.Destroy;
var
  i: Integer;

begin
  for i := 0 to 15 do
  begin
    if FEdges[i].Next <> nil then
      FEdges[i].Next.Free;

  end;

  inherited Destroy;

end;

{ TNibbles }

function TTree.TNibbles.GetItemPtr: PByte;
begin
  Result := (PByte(@FData[0]))
end;

function TTree.TNibbles.GetItem(Index: Integer): Byte;
begin

end;

class function TTree.TNibbles.Create(const S: AnsiString): TNibbles;
begin
  Result.FLen := Length(S);
  SetLength(Result.FData, Length(S));

  if Result.FLen > 0 then
    Move(S[1], Result.FData[0], Result.FLen);

end;

class function TTree.TNibbles.Create(const SPtr: PByte; Len: Integer): TNibbles;
begin
  Result.FLen := Len;
  SetLength(Result.FData, Len);

  if Len > 0 then
    Move(SPtr^, Result.FData[0], Len);


end;

procedure TTree.TNibbles.Truncate(NewLength: Integer);
begin
  Self.FLen := NewLength;

end;

function TTree.TNibbles.ToString: AnsiString;
var
  b: Byte;

begin
  Result := '';
  for b in Self.FData do
    Result += IntToHex(b, 1);

end;

end.
