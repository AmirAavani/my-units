unit TestGenericTrieUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  { TTestTokenTree }

  TTestTokenTree = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLoadAndSave;
    procedure TestMerge;
  end;

implementation

uses
  GenericTrieUnit;

procedure TTestTokenTree.SetUp;
begin
end;

procedure TTestTokenTree.TearDown;
begin
end;

procedure TTestTokenTree.TestLoadAndSave;
type
  TMyTokenTree = specialize TTokenTree<Uint32, UInt32>;
const
  Tokens: array of AnsiString = ('a', 'b', 'c', 'd',
  'a1', 'b', 'c1', '1d', 'a', 'b1', 'cat', 'dad',
  'a2', 'b12', 'c2', 'd', 'e');
var
  Tree, bTree: TMyTokenTree;
  Token: AnsiString;
  Forward: TTokenTree.TNodes;
  i: Integer;
  Node: TTokenTree.TNode;
  Stream: TStream;

begin
  Tree := TTokenTree.Create;

  Forward := TTokenTree.TNodes.Create;
  for Token in Tokens do
  begin
    Forward.Add(Tree.GetOrAddToken(Token));

  end;

  Stream := TFileStream.Create(
  '/tmp/tree.bin',
  fmCreate);

  Tree.Save(Stream);
  Stream.Free;

  WriteLn(Format('LastTokenID: %d LastNodeID: %d',
    [Tree.GetLastTokenID, Tree.GetLastNodeID]));

  Stream := TFileStream.Create(
    '/tmp/tree.bin',
    fmOpenRead);
  bTree := TTokenTree.Load(Stream);
  for i := 0 to High(Tokens) do
  begin
    Token := Tokens[i];
    Node := bTree.GetToken(Token);
    AssertEquals(Forward[i].ID, Node.ID);
    AssertEquals(Forward[i].TokenID, Node.TokenID);
    AssertEquals(Forward[i].LeafCount, Node.LeafCount);

  end;

  AssertEquals(Tree.GetLastNodeID, bTree.GetLastNodeID);
  AssertEquals(Tree.GetLastTokenID, bTree.GetLastTokenID);

  WriteLn(Format('LastTokenID: %d LastNodeID: %d',
    [bTree.GetLastTokenID, bTree.GetLastNodeID]));

  Stream.Free;
  Tree.Free;
  bTree.Free;
end;

procedure TTestTokenTree.TestMerge;
const
  Tokens1: array of AnsiString = ('a', 'b', 'c', 'd',
  'a1', 'b', 'c1', '1d', 'a', 'b1', 'cat', 'dad',
  'a2', 'b12', 'c2', 'd', 'e');
  Tokens2: array of AnsiString = ('a', 'b', 'ac', 'ad',
  'a1', 'b', '1c', 'd1', 'a', 'b1', 'cat1', '2dad',
  'a2', 'b12', 'c2', 'd', 'e');
var
  Tree, bTree: TTokenTree;
  Token: AnsiString;
  Forward: TTokenTree.TNodes;
  i, j: Integer;
  Node: TTokenTree.TNode;
  Stream: TStream;

begin
  Exit;
  Tree := TTokenTree.Create;

  for Token in Tokens1 do
  begin
    Tree.GetOrAddToken(Token);
  end;
  Stream := TFileStream.Create(
    '/tmp/tree.bin',
    fmCreate);
  Tree.Save(Stream);
  Stream.Free;
  Tree := TTokenTree.Create;

  for Token in Tokens1 do
  begin
    Tree.GetOrAddToken(Token);
  end;
  Stream := TFileStream.Create(
    '/tmp/tree.bin',
    fmCreate);
  Tree.Save(Stream);
  Stream.Free;


end;

initialization

  RegisterTest(TTestTokenTree);
end.

