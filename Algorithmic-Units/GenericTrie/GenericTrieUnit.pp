unit GenericTrieUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  TNodeID = Uint32;
  TTokenID = Uint32;
  { TGenericTrie }

  TGenericTrie = class(TObject)
  private
    NextNodeID: TTokenID;
    NextTokenID: TTokenID;

  public type
    TNode = class;
    TNodes = specialize TList<TNode>;

    { TNode }

    TNode = class(TObject)
    private
      FID: TNodeID;
      FTokenID: TTokenID;
      FParent: TNode;
      Children: TNodes;
      FTransitionCount, FLeafCount: Uint32;
      const function GetID: TNodeID; inline;
      const function GetTokenID: TTokenID; inline;

    public
      property ID: TNodeID read GetID;
      property TokenID: TTokenID read GetTokenID;
      property LeafCount: UInt32 read FLeafCount;
      constructor Create(Parent: TNode; NodeID: TTokenID);
      constructor Create(_ID: TTokenID);
      destructor Destroy; override;

      const procedure Print(const Indent: AnsiString);
      const procedure PrintPath;

    end;


  private
    FRoot: TNode;

    function CreateNode(Parent: TNode; ID: TTokenID): TNode;

  public
    constructor Create;
    constructor Load(InputStream: TStream);
    destructor Destroy; override;

    function GetOrAddToken(const Token: AnsiString): TNode;
    function GetOrAddToken(First, Last: PChar): TNode;
    const function GetToken(const Token: AnsiString): TNode;
    const function GetToken(First, Last: PChar): TNode;

    procedure Print;
    procedure Save(OutputStream: TStream);
    function GetLastNodeID: TNodeID;
    function GetLastTokenID: TTokenID;

  end;

implementation
uses
  ALoggerUnit;

{ TGenericTrie }

function TGenericTrie.CreateNode(Parent: TNode; ID: TTokenID): TNode;
begin
  Result := TNode.Create(Parent, ID);

end;

constructor TGenericTrie.Create;
begin
  inherited;

  NextNodeID := 1;
  NextTokenID := 1;
  FRoot := CreateNode(nil, NextNodeID);
  Inc(NextNodeID);

end;

destructor TGenericTrie.Destroy;
var
  Stack: TNodes;
  Current, Child: TGenericTrie.TNode;

begin
  Stack := TNodes.Create;

  Stack.Add(FRoot);

  while Stack.Count <> 0 do
  begin
    Current := Stack.Last;
    Stack.Delete(Stack.Count - 1);

    for Child in Current.Children do
      if Child <> nil then
        Stack.Add(Child);

    Current.Free;

  end;
  Stack.Free;

  inherited Destroy;
end;

function TGenericTrie.GetOrAddToken(const Token: AnsiString): TNode;
begin
  Result := Self.GetOrAddToken(@Token[1], @Token[Length(Token)]);
end;

function TGenericTrie.GetOrAddToken(First, Last: PChar): TNode;
var
  Current, Next: TNode;
  Ch: PChar;
  b: Byte;

begin
  Current := FRoot;
  Inc(Current.FTransitionCount);

  Ch := First;
  while Ch <= Last do
  begin

    b := Ord(Ch^) and $0F;
    Next := Current.Children[b];
    if Next = nil then
    begin
      Next := CreateNode(Current, NextNodeID);
      Inc(NextNodeID);

      Current.Children[b] := Next;
    end;

    Current := Next;
    Inc(Current.FTransitionCount);

    b := Ord(Ch^) shr 4;
    Next := Current.Children[b];
    if Next = nil then
    begin
      Next := TNode.Create(Current, NextNodeID);
      Inc(NextNodeID);

      Current.Children[b] := Next;
    end;

    Current := Next;
    Inc(Current.FTransitionCount);
    Inc(Ch);

  end;
  Result := Current;
  Inc(Result.FLeafCount);
  if Result.FTokenID = 0 then
  begin
    Result.FTokenID := NextTokenID;
    Inc(NextTokenID);

  end;

end;

function TGenericTrie.GetToken(const Token: AnsiString): TNode;
begin
  Result := Self.GetToken(@Token[1], @Token[Length(Token)]);

end;

function TGenericTrie.GetToken(First, Last: PChar): TNode;
var
  Current, Next: TNode;
  Ch: PChar;
  b: Byte;

begin
  Current := FRoot;

  Ch := First;
  while Ch <= Last do
  begin

    b := Ord(Ch^) and $0F;
    Next := Current.Children[b];
    if Next = nil then
    begin
      Exit(nil);

    end;

    Current := Next;

    b := Ord(Ch^) shr 4;
    Next := Current.Children[b];
    if Next = nil then
    begin
      Exit(nil);

    end;

    Current := Next;
    Inc(Ch);

  end;
  Result := Current;
end;

procedure TGenericTrie.Print;
begin
  FRoot.Print('');
end;

procedure TGenericTrie.Save(OutputStream: TStream);
  procedure NodeToByteArray(Node: TNode; BytePtr: PByte);
  var
    i: Integer;
    ChildIndex: Integer;

  begin
    Move(Node.ID, BytePtr^, 4);
    Inc(BytePtr, 4);
    Move(Node.TokenID, BytePtr^, 4);
    Inc(BytePtr, 4);
    Move(Node.FParent.ID, BytePtr^, 4);
    Inc(BytePtr, 4);
    Move(Node.FLeafCount, BytePtr^, 4);
    Inc(BytePtr, 4);
    Move(Node.FTransitionCount, BytePtr^, 4);
    Inc(BytePtr, 4);
    if Node.FParent = nil then
    begin
      Exit;
    end;

    ChildIndex := -1;
    for i := 0 to Node.FParent.Children.Count - 1 do
      if Node.FParent.Children[i] = Node then
      begin
        ChildIndex := i;
        Break;

      end;

    ALoggerUnit.FmtFatalLnIFFalse(ChildIndex >= 0,
        'Node.ID: %d', [Node.ID]);
    Move(ChildIndex, BytePtr^, 1);

  end;
const
  ByteArrayLength = 21;

var
  Stack: TNodes;
  Node, Child: TNode;
  i: Integer;
  ByteArray: array [0..ByteArrayLength - 1] of Byte;

begin
  Stack := TNodes.Create;
  Stack.Add(FRoot);

  i := 0;
  while Stack.Count <> 0 do
  begin
    Node := Stack.Last;
    Inc(i);
    Stack.Delete(Stack.Count - 1);

    {
    WriteLn(Format('S NodeID: %d PID: %d TokenID: %d LCount: %d', [
      Node.ID,
      Node.FParent.ID,
      Node.TokenID,
      Node.LeafCount]));
    }
    NodeToByteArray(Node, @ByteArray[0]);

    OutputStream.WriteBuffer(ByteArray[0], ByteArrayLength);

    for Child in Node.Children do
      if Child  <> nil then
      begin
        Stack.Add(Child);
      end;

  end;
  Stack.Clear;
  Stack.Free;

  //pbNodeCollection.SaveToStream(OutputStream);
  {for pbNode in pbNodeCollection.Nodes do
    pbNode.Free;
    }
  //pbNodeCollection.Clear;
  //pbNodeCollection.Free;
end;

function TGenericTrie.GetLastNodeID: TNodeID;
begin
  Result := NextNodeID - 1;
end;

function TGenericTrie.GetLastTokenID: TTokenID;
begin
  Result := Self.NextTokenID - 1;
end;

constructor TGenericTrie.Load(InputStream: TStream);
const
  ByteArrayLength = 21;
  ByteArrayLast = ByteArrayLength - 1;

var
  AllNodes: TGenericTrie.TNodes;

  function MakeLightNode(ByteArray: array of Byte): TNode;
  var
    uI32: Uint32;

  begin
    uI32 := 0;
    Move(ByteArray[0], uI32, 4);
    Result := TNode.Create(uI32);
    Move(ByteArray[4], uI32, 4);
    Result.FTokenID := uI32;
    Move(ByteArray[8], uI32, 4);
    Result.FParent := AllNodes[uI32];
    Move(ByteArray[12], uI32, 4);
    Result.FLeafCount := uI32;
    Move(ByteArray[16], uI32, 4);
    Result.FTransitionCount := uI32;

    if Result.FParent = nil then
    begin
      if Result.ID <> 1 then
      begin
        WriteLn('Something went wrong');
        Halt(1);
      end;
      Exit;
    end;

    Result.FParent.Children[ByteArray[ByteArrayLast]] := Result;
  end;

var
  ByteArray: array [0..ByteArrayLength - 1] of Byte;
  Node: TNode;

begin
  NextTokenID := 0;
  NextNodeID := 0;
  AllNodes := TGenericTrie.TNodes.Create;
  AllNodes.Add(nil);
  while InputStream.Position < InputStream.Size do
  begin
    InputStream.Read(ByteArray[0], ByteArrayLength);
    Node := MakeLightNode(ByteArray);

    if AllNodes.Count <= Node.ID then
    begin
      AllNodes.Count := Node.ID + 1;
    end;
    if AllNodes[Node.ID] <> nil then
    begin
      WriteLn('Something Went Wrong');
      Halt(1);
    end;

    AllNodes[Node.ID] := Node;
    if NextNodeID <= Node.ID + 1 then
      NextNodeID := Node.ID + 1;

    if NextTokenID <= Node.TokenID + 1 then
      NextTokenID := Node.TokenID + 1;

  end;
  FRoot := AllNodes[1];
  AllNodes.Clear;
  AllNodes.Free;

end;

{ TGenericTrie.TNode }

function TGenericTrie.TNode.GetID: TNodeID;
begin
  if Self = nil then
    Exit(0);

  Result := Self.FID;
end;

function TGenericTrie.TNode.GetTokenID: TTokenID;
begin
  if Self = nil then
    Exit(0);

  Result := FTokenID;
end;

constructor TGenericTrie.TNode.Create(Parent: TNode; NodeID: TTokenID);
begin
  inherited Create;

  FID := NodeID;
  FParent := Parent;
  FTransitionCount := 0;
  FLeafCount := 0;
  Children := TNodes.Create;
  Children.Count := 16;
end;

constructor TGenericTrie.TNode.Create(_ID: TTokenID);
begin
  inherited Create;

  FID := _ID;
  FParent := nil;
  FTransitionCount := 0;
  FLeafCount := 0;
  Children := TNodes.Create;
  Children.Count := 16;
end;

destructor TGenericTrie.TNode.Destroy;
begin
  Children.Free;

  inherited Destroy;
end;

procedure TGenericTrie.TNode.Print(const Indent: AnsiString);
var
  i: Integer;
begin
  WriteLn(Format('%s<Node id="%d" LeafCount="%d">', [Indent, ID, LeafCount]));
  WriteLn(Format('%s<Children>', [Indent]));
  for i := 0 to Children.Count - 1 do
    if Children[i] <> nil then
    begin
      WriteLn(Format('%s<Child%d>', [Indent, i]));
      Children[i].Print(Indent + '  ');
      WriteLn(Format('%s</Child%d>', [Indent, i]));
    end;

  WriteLn(Format('%s</Children>', [Indent]));
  WriteLn(Format('%s</Node>', [Indent]));
end;

procedure TGenericTrie.TNode.PrintPath;
var
  c, p: TNode;
  i: Integer;
  b: Integer;
  Path: AnsiString;

begin
  c := Self;
  p := c.FParent;

  Path := '';
  while p <> nil do
  begin
    b := 0;
    for i := 0 to p.Children.Count - 1 do
      if p.Children[i] = c then
      begin
        b := i shl 4;
        break
      end;
    c := p;
    p := c.FParent;
    if p = nil then
      Break;
    for i := 0 to p.Children.Count - 1 do
      if p.Children[i] = c then
      begin
        b := b or i;
        break
      end;

    Path := String(Chr(b)) + Path;
    c := p;
    p := c.FParent;

  end;

  Write(Path);
end;

initialization
end.

