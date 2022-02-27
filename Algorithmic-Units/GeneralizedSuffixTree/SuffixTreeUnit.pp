unit SuffixTreeUnit;

interface
uses
  GenericCollectionUnit;

// Imported from https://marknelson.us/posts/1996/08/01/suffix-trees.html

const
  MAX_LENGTH: Integer = 1000;
  HashTableSize: Integer = 2179;  //A prime roughly 10% larger

type
  TIntList = specialize TCollection<Int16>;

  { TBaseDoc }

  TBaseDoc = class(TObject)
  protected
    function GetCount: Integer; virtual; abstract;
    function GetCharAt(Index: Integer): Int16; virtual; abstract;

  public
    property CharAt[Index: Integer]: Int16 read GetCharAt;
    property Count: Integer read GetCount;
    constructor Create;

  end;


  TSuffixTree = class;

  { TSuffix }

  TSuffix = class(TObject)
  private
    FFirstCharIndex: Integer;
    FLastCharIndex: Integer;
    FOriginNode: Integer;
    FDocIndex: UInt32;

  public
    property OriginNode: Integer read FOriginNode;
    property FirstCharIndex: Integer read FFirstCharIndex;
    property LastCharIndex: Integer read FLastCharIndex;
    constructor Create(Node, Start, Stop: Integer; DocIndex: Integer);
    destructor Destroy; override;

    function IsExplicit: Boolean;
    function IsImplicit: Boolean;
    procedure Canonize(Doc: TBaseDoc; Tree: TSuffixTree);

    procedure Print(constref Title: AnsiString);
  end;


  { TNode }

  TNode = class(TObject)
  private
    FID: UInt32;
    FSuffixNodeID: Integer;
    FinCounter: Integer;


  public
    property ID: UInt32 read FID;
    property SuffixNodeID: Integer read FSuffixNodeID;

    constructor Create(_ID: UInt32);

  end;

  TNodes = specialize TObjectCollection<TNode>;

  { TEdge }

  TEdge = class(TObject)
  private
    FEndNode: Integer;
    FFirstCharIndex: Integer;
    FLastCharIndex: Integer;
    FStartNode: Integer;
    FText: TIntList;
    FDocIndex: UInt32;

  public
    property FirstCharIndex: Integer read FFirstCharIndex;
    property LastCharIndex: Integer read FLastCharIndex;
    property StartNode: Integer read FStartNode;
    property EndNode: Integer read FEndNode;

    constructor Create;
    constructor Create(InitFirstcharIndex, InitLastCharIndex, ParentNode,
      _EndNode: Integer; DocIndex: UInt32);

    procedure Print(constref Title: AnsiString);

  end;

  TEdges = specialize TMap<Int64, TEdge>;

  { TStringDoc }

  TStringDoc = class(TBaseDoc)
  protected
    FStr: AnsiString;

    function GetCount: Integer; override;
    function GetCharAt(Index: Integer): Int16; override;

  public
    constructor Create(constref Str: AnsiString);
  end;

  TBaseDocs = specialize TObjectCollection<TBaseDoc>;

  { TSuffixTree }

  TSuffixTree = class(TObject)
  private
    FRoot: TNode;
    Nodes: TNodes;
    Edges: TEdges;
    AllDocs: TBaseDocs;

    procedure AddPrefix(Active: TSuffix; LastCharIndex: Integer; Doc: TBaseDoc;
      DocIndex: UInt32);
    procedure DumpEdges(CurrentN: Integer);

    function GetNextNode: Integer;
    function FindEdge(OriginNode, FirstChar: Integer): TEdge;
    procedure RemoveEdge(Edge: TEdge; Doc: TBaseDoc);
    procedure InsertEdge(Edge: TEdge; Doc: TBaseDoc);
    function SplitEdge(Edge: TEdge; Suffix: TSuffix; Doc: TBaseDoc; DocIndex: UInt32): Integer;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Print;
    function Validate: Boolean;
    procedure AddString(constref Str: AnsiString);
    procedure AddDoc(const Doc: TBaseDoc);
    procedure Done;
  end;

implementation
uses
  sysutils, Classes, CollectionUnit, ALoggerUnit;

{ TBaseDoc }

constructor TBaseDoc.Create;
begin
  inherited Create;

end;

{ TStringDoc }

function TStringDoc.GetCount: Integer;
begin
  Result := Length(FStr);

end;

function TStringDoc.GetCharAt(Index: Integer): Int16;
begin
  if Index = Count then
    Result := 0;
  Result := Ord(FStr[Index + 1]);

end;

constructor TStringDoc.Create(constref Str: AnsiString);
begin
  inherited Create;

  FStr := Str;

end;

{ TSuffixTree }

procedure TSuffixTree.AddPrefix(Active: TSuffix; LastCharIndex: Integer;
  Doc: TBaseDoc; DocIndex: UInt32);
var
  ParentNode, LastParentNode: Integer;
  Edge, NewEdge: TEdge;
  Span: Integer;
  Round: Integer;

begin
  LastParentNode := -1;

  Round := 0;
  while True do
  begin
    Inc(Round);
    ParentNode := Active.OriginNode;
    FMTDebugLn('Round: %d', [Round]);
    Active.Print('Active');

    if Active.IsExplicit then
    begin
      Edge := FindEdge(Active.OriginNode, Doc.CharAt[LastCharIndex]);
      if Edge <> nil then
      begin
        Break;

      end;

    end
    else
    begin
      Edge := FindEdge(Active.OriginNode, Doc.CharAt[Active.FirstCharIndex]);
      Span := Active.LastCharIndex - Active.FirstCharIndex;
      if Doc.CharAt[Edge.FirstCharIndex + Span + 1] = Doc.CharAt[LastCharIndex] then
      begin
        Break;

      end;
      ParentNode := SplitEdge(Edge, Active, Doc, DocIndex);

    end;

    NewEdge := TEdge.Create(LastCharIndex, Doc.Count - 1, ParentNode, GetNextNode,
    DocIndex);
    NewEdge.Print('new_edge');
    Self.InsertEdge(NewEdge, Doc);
    if 0 < LastParentNode then
    begin
      Nodes[LastParentNode].FSuffixNodeID := ParentNode;

    end;
    LastParentNode := ParentNode;

    if Active.OriginNode = 0 then
      Active.FFirstCharIndex += 1
    else
      Active.FOriginNode := Nodes[Active.OriginNode].SuffixNodeID;
    Active.Canonize(Doc, self);

  end;

  if 0 < LastParentNode then
    Nodes[LastParentNode].FSuffixNodeID := ParentNode;

  Active.FLastCharIndex += 1;
  Active.Canonize(Doc, Self);

end;

procedure TSuffixTree.DumpEdges(CurrentN: Integer);
var
  j, l: Integer;
  it: TEdges.TPairEnumerator;
  s: TEdge;
  Doc: TBaseDoc;

begin
  it := Edges.GetEnumerator;
  while it.MoveNext do
  begin
    s := it.Current.Value;
    Doc := AllDocs[s.FDocIndex];
    Write(Format('%sSN:%5d EN:%5d SNI:%3d  FCI:%5d LCI:%6d (DI: %4d)->' , [
      specialize IfThen<AnsiString>(Nodes[s.EndNode].FinCounter = 0, '-', '+'),
     s.StartNode,
     s.EndNode,
     Nodes[s.EndNode].SuffixNodeID,
     s.FirstCharIndex,
     S.LastCharIndex,
     S.FDocIndex
     ]));


    for l := s.FirstCharIndex to s.LastCharIndex do
      Write(Chr(Doc.CharAt[l]));
    WriteLn;

  end;

end;

function TSuffixTree.GetNextNode: Integer;
begin
  Result := Nodes.Count;
  Nodes.Add(TNode.Create(Result));

  Exit(Result);

end;

function Hash(Node, c: Int64): Int64;
begin
  Result := (Node shl 32 + c); // mod HashTableSize;

end;

function TSuffixTree.FindEdge(OriginNode, FirstChar: Integer): TEdge;
var
  Index: Int64;

begin
  Index := Hash(OriginNode, FirstChar);
  Result := nil;
  Edges.TryGetData(Index, Result);

end;

procedure TSuffixTree.RemoveEdge(Edge: TEdge; Doc: TBaseDoc);
var
  Index: Int64;

begin
  Edge.Print('Remove');
  Index := Hash(Edge.StartNode, Doc.CharAt[Edge.FirstCharIndex]);
  Edges.Delete(Index, False);

end;

procedure TSuffixTree.InsertEdge(Edge: TEdge; Doc: TBaseDoc);
var
  Index: Int64;

begin
  Edge.Print('Insert');
  Index := Hash(Edge.FStartNode, Doc.CharAt[Edge.FFirstCharIndex]);
  if Edges.Find(Index) <> nil then
    WritelN('Dupes!?');
  Edges.Add(Index, Edge);

end;

function TSuffixTree.SplitEdge(Edge: TEdge; Suffix: TSuffix; Doc: TBaseDoc;
  DocIndex: UInt32): Integer;
var
  NewEdge: TEdge;

begin
  RemoveEdge(Edge, Doc);
  NewEdge := TEdge.Create(
    Edge.FirstCharIndex,
    Edge.FirstCharIndex + Suffix.LastCharIndex - Suffix.FirstCharIndex,
    Suffix.OriginNode,
    GetNextNode,
    DocIndex);
  NewEdge.Print('new_edge');

  InsertEdge(NewEdge, Doc);
  Nodes[NewEdge.EndNode].FSuffixNodeID := Suffix.OriginNode;
  Edge.FFirstCharIndex += Suffix.LastCharIndex - Suffix.FirstCharIndex + 1;
  Edge.FStartNode := NewEdge.EndNode;
  InsertEdge(Edge, Doc);
  Result := NewEdge.EndNode;

end;

constructor TSuffixTree.Create;
begin
  inherited Create;

  Nodes := TNodes.Create;
  Nodes.Add(TNode.Create(0));
  Edges := TEdges.Create;
  AllDocs := TBaseDocs.Create;

end;

destructor TSuffixTree.Destroy;
begin
  Nodes.Free;
  Edges.Free;
  AllDocs.Free;

  inherited Destroy;
end;

procedure TSuffixTree.Print;
begin
  DumpEdges(-1);

end;

function TSuffixTree.Validate: Boolean;
var
  AllStr: TStringList;
  AllChars: TInt64Collection;

  procedure Collect(StartNode: Integer; Current: AnsiString);
  var
    Edge: TEdge;
    Ch: Integer;
    Tmp: AnsiString;
    i: Integer;

  begin
    for Ch in AllChars do
    begin
      Tmp := Current;

      Edge := FindEdge(StartNode, Ch);
      if Edge = nil then
        Continue;

      for i := Edge.FirstCharIndex to Edge.LastCharIndex do
        Tmp += Chr(AllDocs[Edge.FDocIndex].CharAt[i]);
      Collect(Edge.EndNode, Tmp);

    end;
    for i := 1 to Nodes[StartNode].FinCounter do
      AllStr.Add(Tmp);
  end;

var
  i, Index: Integer;
  S: AnsiString;

begin
  AllChars := TInt64Collection.Create;
  for i := 0 to 255 do
    AllChars.Add(i);

  AllChars.Sort;
  i := 0;
  Index := 0;
  while i < AllChars.Count - 1 do
  begin
    inc(i);
    if AllChars[Index] = AllChars[i] then
      Continue;
    Inc(Index);
    AllChars[Index] := AllChars[i];

  end;
  AllChars.Count := Index + 1;

  AllStr := TStringList.Create;

  Collect(0, '');

  AllChars.Free;
  AllStr.Sort;
  WriteLn('Number of Str: ', AllStr.Count);
  WriteLn(AllStr.Text);
  {
  AllSuffixes := TStringList.Create;
  S := '';

  for Index := 0 to AllDocs.Count - 1 do
  begin
    Doc := AllDocs[Index];
    for i := Doc.Count - 1 downto 0 do
    begin
      S := Chr(Doc.CharAt[i]) + S;
      AllSuffixes.Add(S);

    end;
  end;
  AllSuffixes.Sort;

  for i := 0 to AllStr.Count - 1 do
    if AllSuffixes[i] <> AllStr[i] then
    begin
      WriteLn(AllStr.Text);
      WriteLn(AllSuffixes.Text);
      WriteLn(i);
      WriteLn(AllStr[i]);
      WriteLn(AllSuffixes[i]);
      Exit(False);

    end;

  AllStr.Free;
  AllSuffixes.Free;
}
  Result := True;
end;

procedure TSuffixTree.AddString(constref Str: AnsiString);
var
  StrDoc: TStringDoc;

begin
  StrDoc := TStringDoc.Create(Str);
  Self.AddDoc(StrDoc);

end;

procedure TSuffixTree.AddDoc(const Doc: TBaseDoc);
var
  Active: TSuffix;
  i: Integer;
  DocIndex: UInt32;

begin
  DocIndex:= AllDocs.Count;
  AllDocs.Add(Doc);
  Active := TSuffix.Create(0, 0, -1, DocIndex);  // The initial active prefix

  for i := 0 to Doc.Count - 1 do
  begin
    FMTDebugLn('i: %d', [i]);
    AddPrefix(Active, i, Doc, DocIndex);
  end;
  DumpEdges(-1);

  Active.Free;

end;

procedure TSuffixTree.Done;

  function Traverse(Doc: TBaseDoc): TNode;
  var
    Ch: Integer;
    Edge: TEdge;
    RootID: UInt32;
    dIndex: Integer;

  begin
    RootID := 0;
    dIndex := 0;

    while dIndex < Doc.Count do
    begin
      if dIndex = Doc.Count then
        Break;

      Ch := Doc.CharAt[dIndex];
      Edge := FindEdge(RootID, Ch);
      Inc(dIndex, Edge.LastCharIndex - Edge.FirstCharIndex + 1);
      RootID := Edge.EndNode;

    end;

    WriteLn('RootID', RootID);
    Result := Nodes[RootID];

  end;

var
  Doc: TBaseDoc;

begin
  for Doc in AllDocs do
    Inc(Traverse(Doc).FinCounter);

end;

constructor TNode.Create(_ID: UInt32);
begin
  inherited Create;

  FSuffixNodeID := -1;
  FinCounter := 0;
  FID := _ID;

end;


{ TEdge }


constructor TEdge.Create;
begin
  inherited Create;

  FStartNode := -1;
end;

constructor TEdge.Create(InitFirstcharIndex, InitLastCharIndex, ParentNode,
  _EndNode: Integer; DocIndex: UInt32);
begin
  FFirstCharIndex := InitFirstcharIndex;
  FLastCharIndex := InitLastCharIndex;
  FStartNode := ParentNode;
  FEndNode := _EndNode;
  FDocIndex := DocIndex;

end;

procedure TEdge.Print(constref Title: AnsiString);
begin
  Exit;
  FMTDebugLN('%s FirstCharIndex: %d LastCharIndex: %d EndNode: %d StartNode: %d',
  [Title, FirstCharIndex, LastCharIndex, EndNode, StartNode]);

end;

{ TSuffix }

constructor TSuffix.Create(Node, Start, Stop: Integer; DocIndex: Integer);
begin
  inherited Create;

  FOriginNode := Node;
  FFirstCharIndex := Start;
  FLastCharIndex := Stop;
  FDocIndex := DocIndex;

end;

destructor TSuffix.Destroy;
begin
  FMTDebugLn('In Destroy', []);
  inherited Destroy;
end;

function TSuffix.IsExplicit: Boolean;
begin
  Result := LastCharIndex < FirstCharIndex;

end;

function TSuffix.IsImplicit: Boolean;
begin
  Result := FirstCharIndex <= LastCharIndex;

end;

procedure TSuffix.Canonize(Doc: TBaseDoc; Tree: TSuffixTree);
var
  Edge: TEdge;
  EdgeSpan: Integer;

begin
  if IsExplicit then
      Exit;

  Edge := Tree.FindEdge(OriginNode, Doc.CharAt[FFirstCharIndex]);
  EdgeSpan := Edge.LastCharIndex - Edge.FirstCharIndex;
  while EdgeSpan <= LastCharIndex - FirstCharIndex do
  begin
    FFirstCharIndex := FirstCharIndex + EdgeSpan + 1;
    FOriginNode := Edge.EndNode;

    if FirstCharIndex <= LastCharIndex then
    begin
      Edge := Tree.FindEdge(Edge.EndNode, Doc.CharAt[FFirstCharIndex]);
      EdgeSpan := Edge.LastCharIndex - Edge.FirstCharIndex;

    end;

  end;

end;

procedure TSuffix.Print(constref Title: AnsiString);
begin
  FMTDebugLN('%s OriginNode: %d FirstCharIndex: %d LastCharIndex: %d Implicit: %s',
    [Title, OriginNode, FirstCharIndex, LastCharIndex, specialize IfThen<String>(IsImplicit, '1', '0')]);

end;

initialization

finalization

end.

