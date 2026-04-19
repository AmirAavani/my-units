unit RadixTreeTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  RadixTreeUnit;

type
  // Define a dummy info type for the generic
  TMyInfo = record
    Data: string;
  end;

  TTestRadixTree = class(TTestCase)
  private
    { We use a specialized version for testing }
    type TStringTree = specialize TTree<TMyInfo>;
    var Tree: TStringTree;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEmptyTree;
    procedure TestSingleInsert;
    procedure TestSimpleSplit;
    procedure TestDeepSplit;
    procedure TestOverlapAndTerminal;
    procedure TestDuplicateInsert;
  end;

implementation

procedure TTestRadixTree.SetUp;
begin
  Tree := TStringTree.Create;
end;

procedure TTestRadixTree.TearDown;
begin
  Tree.Free;
end;

procedure TTestRadixTree.TestEmptyTree;
begin
  AssertEquals('Size should be 0 initially', 0, Tree.Size);
end;

procedure TTestRadixTree.TestSingleInsert;
var
  Node: TTestRadixTree.TStringTree.TNode;
begin
  Node := Tree.Insert('apple');
  AssertNotNull('Node should be returned', Node);
  AssertEquals('Size should be 1', 1, Tree.Size);
end;

procedure TTestRadixTree.TestSimpleSplit;
begin
  { "apple" and "apply" share "appl" }
  Tree.Insert('apple');
  Tree.Insert('apply');

  { Total unique strings = 2 }
  AssertEquals('Size should be 2 after split', 2, Tree.Size);
end;

procedure TTestRadixTree.TestDeepSplit;
begin
  { Constructing:
    romane
    romanus
    romulus
  }
  Tree.Insert('romane');
  Tree.Insert('romanus');
  Tree.Insert('romulus');

  AssertEquals('Size should be 3', 3, Tree.Size);
end;

procedure TTestRadixTree.TestOverlapAndTerminal;
var
  Node: TTestRadixTree.TStringTree.TNode;
begin
  { "intern" is a prefix of "internet" }
  Tree.Insert('internet');
  AssertEquals(1, Tree.Size);

  Node := Tree.Insert('intern');
  AssertEquals('Size should increase when prefix becomes terminal', 2, Tree.Size);

  { Re-inserting the longer one shouldn't change size }
  Tree.Insert('internet');
  AssertEquals('Size should remain 2', 2, Tree.Size);
end;

procedure TTestRadixTree.TestDuplicateInsert;
begin
  Tree.Insert('test');
  Tree.Insert('test');
  Tree.Insert('test');
  AssertEquals('Duplicate inserts should not increase size', 1, Tree.Size);
end;

initialization
  RegisterTest(TTestRadixTree);
end.unit RadixTreeTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  RadixTreeUnit;

type
  // Define a dummy info type for the generic
  TMyInfo = record
    Data: string;
  end;

  TTestRadixTree = class(TTestCase)
  private
    { We use a specialized version for testing }
    type TStringTree = specialize TTree<TMyInfo>;
    var Tree: TStringTree;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEmptyTree;
    procedure TestSingleInsert;
    procedure TestSimpleSplit;
    procedure TestDeepSplit;
    procedure TestOverlapAndTerminal;
    procedure TestDuplicateInsert;
  end;

implementation

procedure TTestRadixTree.SetUp;
begin
  Tree := TStringTree.Create;
end;

procedure TTestRadixTree.TearDown;
begin
  Tree.Free;
end;

procedure TTestRadixTree.TestEmptyTree;
begin
  AssertEquals('Size should be 0 initially', 0, Tree.Size);
end;

procedure TTestRadixTree.TestSingleInsert;
var
  Node: TTestRadixTree.TStringTree.TNode;
begin
  Node := Tree.Insert('apple');
  AssertNotNull('Node should be returned', Node);
  AssertEquals('Size should be 1', 1, Tree.Size);
end;

procedure TTestRadixTree.TestSimpleSplit;
begin
  { "apple" and "apply" share "appl" }
  Tree.Insert('apple');
  Tree.Insert('apply');

  { Total unique strings = 2 }
  AssertEquals('Size should be 2 after split', 2, Tree.Size);
end;

procedure TTestRadixTree.TestDeepSplit;
begin
  { Constructing:
    romane
    romanus
    romulus
  }
  Tree.Insert('romane');
  Tree.Insert('romanus');
  Tree.Insert('romulus');

  AssertEquals('Size should be 3', 3, Tree.Size);
end;

procedure TTestRadixTree.TestOverlapAndTerminal;
var
  Node: TTestRadixTree.TStringTree.TNode;
begin
  { "intern" is a prefix of "internet" }
  Tree.Insert('internet');
  AssertEquals(1, Tree.Size);

  Node := Tree.Insert('intern');
  AssertEquals('Size should increase when prefix becomes terminal', 2, Tree.Size);

  { Re-inserting the longer one shouldn't change size }
  Tree.Insert('internet');
  AssertEquals('Size should remain 2', 2, Tree.Size);
end;

procedure TTestRadixTree.TestDuplicateInsert;
begin
  Tree.Insert('test');
  Tree.Insert('test');
  Tree.Insert('test');
  AssertEquals('Duplicate inserts should not increase size', 1, Tree.Size);
end;

initialization
  RegisterTest(TTestRadixTree);
end.
