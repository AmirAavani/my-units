unit GeneralizedSuffixTreeUnit;

{$mode ObjFPC}{$H+}
{$R+}
interface

uses
  Classes, SysUtils, GenericCollectionUnit, DocUnit, TupleUnit, QueryUnit;

const
  EndToken: UInt16 = 0;

type

  { TGeneralizedSuffixTree }

  TGeneralizedSuffixTree = class(TObject)
  public type
    TNode = class;
    TCharType = Int32;
    { TInterval }

    TInterval = record
      Left, Right: Int64;
      DocIndex: UInt32;
    end;

  private
    function ContainEndToken(DocIndex: Integer; sIndex, eIndex: Integer): Boolean;

    class function CreateInterval: TInterval;
    class function CreateInterval(dIndex, Left, Right: Int64): TInterval;
    class function IsEmptyInterval(constref anInterval: TInterval): Boolean;

  private type
    TTransition = specialize TPair<TNode, TInterval>;
    TReferencePoint = specialize TTriplet<TNode, Int64, Int64>;

  private
    function CreateTransition: TTransition;
    function CreateTransition(s: TInterval; t: TNode): TTransition;
    function IsValidTransition(constref aTransition: TTransition): Boolean;
    function CreateReferencePoint(n: TNode; DocIndex, CharIndex: Integer): TReferencePoint;

  private type
    TTransitionMap = specialize TMap<TCharType, TTransition>;

    { TNode }

    TNode = class(TObject)
    private
      FNeighbors: TTransitionMap;
      FSuffixNode: TNode;
      FID: UInt32;

    protected
      function GetIsLeaf: Boolean; virtual;
      function GetNeighbor(Ch: TCharType): TTransition; virtual;

    public
      property Neighbor[Ch: TCharType]: TTransition read GetNeighbor;
      property SuffixNode: TNode read FSuffixNode;
      property IsLeaf: Boolean read GetIsLeaf;
      property ID: UInt32 read FID;

      constructor Create;
      destructor Destroy; override;

      procedure Print(constref Indent: AnsiString);

    end;

    { TSinkNode }

    TSinkNode = class(TNode)
    protected
      function GetNeighbor(Ch: TCharType): TTransition; override;

    public
      constructor Create;

    end;

    { TLeaf }

    TLeaf = class(TNode)
    protected
      function GetIsLeaf: Boolean; override;

    public
      constructor Create;

    end;

    TNodes = specialize TCollection<TNode>;

    { TBase }

    TBase = class(TObject)
    private
      Sink: TSinkNode;
      Root: TNode;

      procedure Clean;

    public
      constructor Create;
      destructor Destroy; override;

    end;
  private type
    THaystack = TBaseDocs;


  private
    Tree: TBase;
    LastIndex: Integer;
    Haystack: THaystack;

    function GetRoot: TNode;
    function ToString(s: TBaseDoc; b, e: Int64): AnsiString;
    function ToString(s: TBaseDoc): AnsiString;
    function ToString(Interval: TInterval): AnsiString;

    function TestAndSplit(n: TNode; kp: TInterval; t: TCharType; d: TBaseDoc;
      var r: TNode): Boolean;
    function Update(n: TNode; ki: TInterval): TReferencePoint;
    function Canonize(n: TNode; kp: TInterval): TReferencePoint;
    function GetStartingNode(s: TBaseDoc; var r: TReferencePoint): Int64;
    function DeploySuffixes(s: TBaseDoc; sIndex: Integer): Integer;
    procedure DumpNode(n: TNode; SameLine: Boolean; Padding: Integer; Orig: TInterval);
    function IsSubstring(Start, Fin: Integer; d: TBaseDoc): Boolean;
    function GetCharAt(DocIndex, Position: Integer): Int32;

  public
    property Root: TNode read GetRoot;

    constructor Create;
    destructor Destroy; override;

    function AddDoc(Doc: TBaseDoc): Integer;
    function IsSuffix(Start, Fin: Integer; d: TBaseDoc): Boolean;

    function CountMatches(Q: TBaseQuery): UInt32;

    procedure DumpTree;
    procedure PrintAll;
    procedure PrintAllTransitions;
  end;

implementation
uses
  Math, MetadataUnit;

const
  Inf = High(Int64);

type

  { TSuffixTreeDoc }

  TSuffixTreeDoc = class(TBaseDoc)
  private
    Doc: TBaseDoc;
    MetaData: TDocMetaData;
    MetaDataBs: TBytes;

  protected
    function GetCount: Integer; override;
    function GetCharAt(Index: Integer): UInt16; override;

  public
    constructor Create(_Doc: TBaseDoc; _Metadata: TDocMetaData);
    destructor Destroy; override;
    function SubStr(b, e: Integer): AnsiString; override;

  end;

function CreateMetadata(DocID: Uint32; Doc: TBaseDoc): TDocMetaData;
begin
  Result := TDocMetaData.Create;

  Result.DocID := DocID;

end;

{ TSuffixTreeDoc }

function TSuffixTreeDoc.GetCount: Integer;
begin
  Result := Doc.Count + 1 + (Length(MetaDataBs) + 1) div 2;


end;

function TSuffixTreeDoc.GetCharAt(Index: Integer): UInt16;
var
  i: Integer;

begin
  if Index < Doc.Count then
    Exit(Doc.CharAt[Index]);

  if Index = Doc.Count then
    Exit(EndToken);

  {
   C+1, -> M[0], M[1]
   C+2  -> M[2], M[3]
   ...
  }
  i := Index - Doc.Count - 1;
  Result := Ord(MetaDataBs[2 * i]) shl 8;
  if 2 * i + 1 < Length(MetaDataBs) then
    Result := Result or UInt16(MetaDataBs[2 * i + 1]);

end;

constructor TSuffixTreeDoc.Create(_Doc: TBaseDoc; _Metadata: TDocMetaData);
var
  S: TStream;

begin
  inherited Create;

  Doc := _Doc;
  MetaData := _Metadata;

  S := TBytesStream.Create;
  Metadata.SaveToStream(S);
  S.Position := 0;
  SetLength(MetaDataBs, S.Size);
  S.ReadData(MetaDataBs, S.Size);
  S.Free;

end;

destructor TSuffixTreeDoc.Destroy;
begin
  Doc.Free;
  MetaData.Free;
  SetLength(MetaDataBs, 0);

  inherited Destroy;
end;

function TSuffixTreeDoc.SubStr(b, e: Integer): AnsiString;
var
  i: Integer;

begin
  Result := Doc.SubStr(b, Min(e, Doc.Count - 1));
  if e < Doc.Count then
    Exit;

  Result += '(EOF)';
  if e = Doc.Count then
    Exit;

  Result += '{';
  for i := Doc.Count + 1 to e do
    Result += IntToStr(Self.CharAt[i]) + ',';
  Delete(Result, Length(Result), 1);
  Result += '}';


end;

{ TGeneralizedSuffixTree.TBase }

procedure TGeneralizedSuffixTree.TBase.Clean;
var
  DelList: TNodes;
  Current: TNode;
  it: TTransitionMap.TPairEnumerator;
  Front: Integer;

begin
  DelList := TNodes.Create;
  DelList.Add(Root);
  Front := 0;

  while Front < DelList.Count do
  begin
    Current := DelList[Front];
    Inc(Front);

    if Current.FNeighbors <> nil then
    begin
      it := Current.FNeighbors.GetEnumerator;

      while it.MoveNext do
      begin
        DelList.Add(it.Current.Value.First);

      end;

      it.Free;

    end;
    if Root <> Current then
      Current.Free;

  end;

  DelList.Clear;
  DelList.Free;

end;

constructor TGeneralizedSuffixTree.TBase.Create;
begin
  inherited Create;

  Sink := TSinkNode.Create;
  Root := TNode.Create;

  Root.FSuffixNode := Sink;
  Sink.FSuffixNode := Root;

end;

destructor TGeneralizedSuffixTree.TBase.Destroy;
begin
  Clean;
  Sink.Free;
  Root.Free;

  inherited Destroy;
end;

{ TGeneralizedSuffixTree.TSinkNode }

function TGeneralizedSuffixTree.TSinkNode.GetNeighbor(Ch: TCharType
  ): TTransition;
begin
  Result.First := Self.SuffixNode;
  Result.Second := CreateInterval;

end;

constructor TGeneralizedSuffixTree.TSinkNode.Create;
begin
  inherited Create;

end;

{ TGeneralizedSuffixTree.TLeaf }

function TGeneralizedSuffixTree.TLeaf.GetIsLeaf: Boolean;
begin
  Result := True;

end;

constructor TGeneralizedSuffixTree.TLeaf.Create;
begin
  inherited Create;

end;

{ TGeneralizedSuffixTree }

class function TGeneralizedSuffixTree.CreateInterval: TInterval;
begin
  Result.Left := 0;
  Result.Right := 0;
  Result.DocIndex := 0;

end;

class function TGeneralizedSuffixTree.CreateInterval(dIndex, Left,
  Right: Int64): TInterval;
begin
  Result.Left := Left;
  Result.Right := Right;
  Result.DocIndex := dIndex;

end;

class function TGeneralizedSuffixTree.IsEmptyInterval(constref
  anInterval: TInterval): Boolean;
begin
  Result := anInterval.Right < anInterval.Left;

end;

function TGeneralizedSuffixTree.CreateTransition: TTransition;
begin
  Result.First := nil;
  Result.Second := CreateInterval;

end;

function TGeneralizedSuffixTree.CreateTransition(s: TInterval; t: TNode
  ): TTransition;
begin
  Result.First := t;
  Result.Second := s;

end;

function TGeneralizedSuffixTree.IsValidTransition(constref
  aTransition: TTransition): Boolean;
begin
  Result := aTransition.First <> nil;

end;

function TGeneralizedSuffixTree.CreateReferencePoint(n: TNode; DocIndex,
  CharIndex: Integer): TReferencePoint;
begin
  Result.First := n;
  Result.Second := DocIndex;
  Result.Third := CharIndex;

end;

function TGeneralizedSuffixTree.ToString(s: TBaseDoc; b, e: Int64): AnsiString;
begin
  Result := '()';
  if (0 <= b) and (e < s.Count) then
    Result := s.SubStr(b, e);

end;

function TGeneralizedSuffixTree.GetRoot: TNode;
begin
  Result := Tree.Root;

end;

function TGeneralizedSuffixTree.ToString(s: TBaseDoc): AnsiString;
begin
  Result := ToString(s, 0, s.Count - 1);

end;

function TGeneralizedSuffixTree.ToString(Interval: TInterval): AnsiString;
var
  s: TBaseDoc;

begin
  Result := '';
  s := Haystack[Interval.DocIndex];
  if Interval.Right = Inf then
    Result := ToString(s, Interval.Left, s.Count - 1)
  else
    Result := ToString(s, Interval.Left, Interval.Right);

end;

function TGeneralizedSuffixTree.TestAndSplit(n: TNode; kp: TInterval;
  t: TCharType; d: TBaseDoc; var r: TNode): Boolean;
var
  tk: TCharType;
  tkTransition: TTransition;
  kpPrime: TInterval;
  Delta: Integer;
  StrPrime: TBaseDoc;
  NewT: TTransition;

begin
  tk := d.CharAt[kp.Left];
  Delta := kp.Right - kp.Left;

  if 0 <= Delta then
  begin
    tkTransition := n.GetNeighbor(tk);
    kpPrime := tkTransition.Second;
    StrPrime := Haystack[kpPrime.DocIndex];

    if StrPrime.CharAt[kpPrime.Left + Delta + 1] = t then
    begin
      r := n;
      Exit(True);

    end;

    r := TNode.Create;
    NewT := tkTransition;
    NewT.Second.Left += Delta + 1;
    r.FNeighbors.AddOrUpdateData(StrPrime.CharAt[NewT.Second.Left], NewT);
    tkTransition.Second.Right := tkTransition.Second.Left + Delta;
    tkTransition.First := r;
    n.FNeighbors[tk] := tkTransition;
    Exit(False);

  end;

  tkTransition := n.GetNeighbor(t);
  r := n;
  Result := IsValidTransition(tkTransition);

end;

function TGeneralizedSuffixTree.Update(n: TNode; ki: TInterval
  ): TReferencePoint;
var
  Oldr: TNode;
  r: TNode;
  IsEndPoint: Boolean;
  ki1: TInterval;
  w: TBaseDoc;
  sk: TReferencePoint;
  rPrime: TLeaf;

begin
  Oldr := Tree.Root;
  r := nil;
  IsEndPoint := False;
  ki1 := ki;
  w := Haystack[ki.DocIndex];
  sk := CreateReferencePoint(n, ki.DocIndex, ki.Left);
  ki1.Right := ki.Right - 1;
  IsEndPoint:= TestAndSplit(n, ki1, w.CharAt[ki.Right], w, r);

  while not IsEndPoint do
  begin
    rPrime := TLeaf.Create;
    r.FNeighbors.AddOrUpdateData(
      w.CharAt[ki.Right],
      CreateTransition(CreateInterval(ki.DocIndex, ki.Right, Inf),
        rPrime));

    if Tree.Root <> Oldr then
      Oldr.FSuffixNode := r;
    Oldr := r;
    sk := Canonize(sk.First.SuffixNode, ki1);
    ki1.Left := sk.Third;
    ki.Left := sk.Third;
    IsEndPoint := TestAndSplit(sk.First, ki1, w.CharAt[ki.Right], w, r);

  end;

  if Tree.Root <> Oldr then
    Oldr.FSuffixNode := sk.First;

  Result := sk;
end;

function TGeneralizedSuffixTree.Canonize(n: TNode; kp: TInterval
  ): TReferencePoint;
var
  KpRefStr: TBaseDoc;
  Delta: Integer;
  tkTrans: TTransition;

begin
  if kp.Right < kp.Left then
    Exit(CreateReferencePoint(n, kp.DocIndex, kp.Left));

  KpRefStr := Haystack[kp.DocIndex];

  tkTrans := n.GetNeighbor(KpRefStr.CharAt[kp.Left]);

  while tkTrans.Second.Right - tkTrans.Second.Left <= kp.Right - kp.Left do
  begin
    Delta := tkTrans.Second.Right - tkTrans.Second.Left;
    kp.Left += 1 + Delta;
    n := tkTrans.First;

    if kp.Left <= kp.Right then
    begin
      tkTrans := n.GetNeighbor(KpRefStr.CharAt[kp.Left]);
    end;

  end;

  Result := CreateReferencePoint(n, kp.DocIndex, kp.Left);

end;

function TGeneralizedSuffixTree.GetStartingNode(s: TBaseDoc;
  var r: TReferencePoint): Int64;
var
  k: Integer;
  sLen: Integer;
  sRunout: Boolean;
  rNode: TNode;
  t: TTransition;
  RefStr: TBaseDoc;
  i: Integer;

begin
  k := r.Third;
  sLen := s.Count;
  sRunout := False;

  while not sRunout do
  begin
    rNode := r.First;
    if sLen <= k then
    begin
      sRunout := True;
      Break;

    end;
    t := rNode.GetNeighbor(s.CharAt[k]);
    if t.First <> nil then
    begin
      RefStr := Haystack[t.Second.DocIndex];

      i := 1;
      while i <= t.Second.Right - t.Second.Left do
      begin
        if sLen <= k + i then
        begin
          sRunout := True;
          Break;

        end;

        if s.CharAt[k + i] <> RefStr.CharAt[t.Second.Left + i] then
        begin
          r.Third := k;
          Exit(k + i);

        end;

        Inc(i);
      end;

      if not sRunout then
      begin
        r.First := t.First;
        k += i;
        r.Third := k;

      end;

    end
    else
    begin
      Exit(k);

    end;
  end;
  r.Third := Inf;
  Result := Inf;

end;

function TGeneralizedSuffixTree.DeploySuffixes(s: TBaseDoc; sIndex: Integer
  ): Integer;
var
  ActivePoint: TReferencePoint;
  i: Int64;
  ki: TInterval;

begin
  ActivePoint := CreateReferencePoint(Tree.Root, sIndex, 0);
  i := GetStartingNode(s, ActivePoint);

  if i = Inf then
    Exit(-1);

  while i < s.Count do
  begin
    ki := CreateInterval(sIndex, ActivePoint.Third, i);
    ActivePoint := Update(ActivePoint.First, ki);
    ki.Left := ActivePoint.Third;
    ActivePoint := Canonize(ActivePoint.First, ki);
    Inc(i);

  end;

  Result := sIndex;

end;

procedure TGeneralizedSuffixTree.DumpNode(n: TNode; SameLine: Boolean;
  Padding: Integer; Orig: TInterval);
var
  Delta: Int64;
  i: Integer;
  s: TBaseDoc;
  tit: TTransitionMap.TPairEnumerator;

begin
  Delta := 0;

  if not SameLine then
  begin
    Write(Format('%d:', [n.ID]));
    for  i := 1 to Padding do
    begin
      Write(Format('..', []));

    end;

  end;

  if not IsEmptyInterval(Orig) then
  begin
    s := Haystack[Orig.DocIndex];
    i := Orig.Left;
    while (i <= Orig.Right) and (i < s.Count) do
    begin
      if s.CharAt[i] <> 0 then
      begin
        if Chr(s.CharAt[i]) in ['A'..'Z', 'a'..'z', '0'..'9'] then
          Write(Chr(s.CharAt[i]), ' ')
        else
          Write('(#', s.CharAt[i], ')')
      end
      else
        Write('$ ');

      Inc(i);

    end;

    Write('- ');
    Delta := Orig.Right - Orig.Left + 2;
    if Orig.Right = Inf then
    begin
      Delta := s.Count - Orig.Left;

    end;

  end;

  SameLine := True;
  tit := n.FNeighbors.GetEnumerator;

  while tit.MoveNext do
  begin
    DumpNode(
      tit.Current.Value.First,
      SameLine,
      Padding + Delta,
      tit.Current.Value.Second);
    SameLine := False;

  end;
  tit.Free;

  if SameLine then
    WriteLn('##');

end;

function TGeneralizedSuffixTree.IsSuffix(Start, Fin: Integer; d: TBaseDoc
  ): Boolean;
var
  s: TBaseDoc;
  RootPoint: TReferencePoint;

begin
  s := TRefDoc.Create(d, Start, Fin);
  WriteLn('s: ', ToString(S));
  RootPoint := CreateReferencePoint(Tree.Root, -1, 0);
  Result := GetStartingNode(s, RootPoint) = Inf;
  s.Free;

end;

function TGeneralizedSuffixTree.CountMatches(Q: TBaseQuery): UInt32;
begin
  Result := 0;

end;

procedure TGeneralizedSuffixTree.PrintAllTransitions;
  procedure DFS(root: TNode);
  var
    it: TTransitionMap.TPairEnumerator;

  begin
    if root = nil then
      Exit;

    if root.FNeighbors = nil then
      Exit;

    it := root.FNeighbors.GetEnumerator;
    while it.MoveNext do
    begin
      WriteLn(Format('id: (%d -> %d) k: %S d: %d l: %d r: %d', [
        root.FID,
        it.Current.Value.First.FID,
        specialize ifthen<AnsiString>(it.Current.Key in [
          Ord('A')..Ord('Z'),
          Ord('a')..Ord('z'),
          Ord('0')..Ord('9')
          ], Chr(it.Current.Key),
          IntToStr(it.Current.Key)),
          it.Current.Value.Second.DocIndex,
          it.Current.Value.Second.Left,
        it.Current.Value.Second.Right]));
      DFS(it.Current.Value.First);

    end;

    it.Free;

  end;

begin
  DFS(Tree.Root);

end;

function TGeneralizedSuffixTree.IsSubstring(Start, Fin: Integer; d: TBaseDoc
  ): Boolean;
var
  s: TBaseDoc;
  RootPoint: TReferencePoint;

begin
  s := TRefDoc.Create(d, Start, Fin);

  RootPoint := CreateReferencePoint(Tree.Root, -1, 0);
  Result := GetStartingNode(s, RootPoint) = Inf;
  s.Free;

end;

function TGeneralizedSuffixTree.GetCharAt(DocIndex, Position: Integer): Int32;
begin
  if Haystack[DocIndex].Count <= Position then
    Position := Haystack[DocIndex].Count - 1;

  Result := Haystack[DocIndex].CharAt[Position];
end;

function TGeneralizedSuffixTree.ContainEndToken(DocIndex: Integer; sIndex, eIndex: Integer
  ): Boolean;
begin
  Result := Haystack[DocIndex].Find(sIndex, eIndex, EndToken);

end;

constructor TGeneralizedSuffixTree.Create;
begin
  inherited Create;

  Tree := TBase.Create;
  Haystack := THaystack.Create;
  LastIndex := 0;
end;

destructor TGeneralizedSuffixTree.Destroy;
begin
  Haystack.Free;
  Tree.Free;

  inherited Destroy;
end;

function TGeneralizedSuffixTree.AddDoc(Doc: TBaseDoc): Integer;
var
  NewDoc: TBaseDoc;

begin
  NewDoc := TSuffixTreeDoc.Create(Doc,
    CreateMetadata(Haystack.Count + 1, Doc));
  Haystack.Add(NewDoc);
  LastIndex := Haystack.Count - 1;

  if DeploySuffixes(NewDoc, LastIndex) < 0 then
  begin
    Haystack.Delete(LastIndex).Free;
    Dec(LastIndex);
    Exit(-1);

  end;

   Result := LastIndex;

end;

procedure TGeneralizedSuffixTree.DumpTree;
begin
  DumpNode(Tree.Root, True, 0, CreateInterval(0, 0, -1));

end;

procedure TGeneralizedSuffixTree.PrintAll;
  procedure DFS(Root: TNode; StrList: TStringList; Current: AnsiString);
  var
    Tmp: AnsiString;
    it: TTransitionMap.TPairEnumerator;
    Transition: TTransition;


  begin
    WriteLn(Format('id: %d Current: "%s"', [Root.ID, Current]));
    if Root.IsLeaf then
    begin
      StrList.Add(Current + ':' + IntToStr(Root.ID));
      Exit;

    end;

    if Root.FNeighbors = nil then
      Exit;

    it := Root.FNeighbors.GetEnumerator;

    while it.MoveNext do
    begin
      Transition := it.Current.Value;
      Tmp := ToString(Transition.Second);

      if Tmp = '(EOF)' then
      begin
        StrList.Add(Current + '[' + Tmp + ']:' + IntToStr(Transition.First.ID));
        Continue;

      end;

      DFS(Transition.First, StrList, Current + '[' + Tmp + ']');

    end;
    it.Free;
  end;

var
  StrList: TStringList;

begin
  StrList := TStringList.Create;

  DFS(Tree.Root, StrList, '');

  WriteLn(StrList.Text);

  StrList.Free;
end;

{ TGeneralizedSuffixTree.TNode }

function TGeneralizedSuffixTree.TNode.GetIsLeaf: Boolean;
begin
  Result := False;

end;

function TGeneralizedSuffixTree.TNode.GetNeighbor(Ch: TCharType): TTransition;
begin
  Result.First := nil;
  Result.Second := CreateInterval(0, 0, -1);
  if Self = nil then
    Exit;
  if FNeighbors <> nil then
    FNeighbors.TryGetData(Ch, Result);

end;

var
  NodeID: Integer;

constructor TGeneralizedSuffixTree.TNode.Create;
begin
  inherited Create;

  FNeighbors := TTransitionMap.Create;
  FSuffixNode := nil;
  Inc(NodeID);
  FID := NodeID;

end;

destructor TGeneralizedSuffixTree.TNode.Destroy;
begin
  FNeighbors.Free;

  inherited Destroy;
end;

procedure TGeneralizedSuffixTree.TNode.Print(constref Indent: AnsiString);
var
  it: TTransitionMap.TPairEnumerator;

begin
  WriteLn(Format('%s(%d):', [Indent, FID]));
  if FNeighbors = nil then
    Exit;

  it := FNeighbors.GetEnumerator;

  while it.MoveNext do
  begin
    WriteLn(Format('%s%d->', [Indent, it.Current.Key]));
    it.Current.Value.First.Print(Indent + '..');

  end;
end;

initialization
  NodeID := 0;

end.

