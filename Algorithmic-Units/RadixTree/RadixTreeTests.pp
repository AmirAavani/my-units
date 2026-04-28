unit RadixTreeTests;

{$mode objfpc}{$H+}
{$modeswitch ADVANCEDRECORDS}
interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  RadixTreeUnit;

type
  { TTestData - Test data type with serialization }
  TTestData = class(RadixTreeUnit.TSerializable)
  public
    Value: Integer;
    Name: AnsiString;
    
    constructor Create; overload;
    constructor Create(AValue: Integer; const AName: AnsiString); overload;
    
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
  end;

  { TTestRadixTree }
  
  TTestRadixTree = class(TTestCase)
  private
    type 
      TNibbles = RadixTreeUnit.TNibbles;
      TStringTree = specialize TTree<TTestData>;
      TNode = TStringTree.TNode;
    var 
      Tree: TStringTree;
      
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    
  published
    { Basic Operations }
    procedure TestEmptyTree;
    procedure TestSingleInsert;
    procedure TestMultipleInserts;
    procedure TestDuplicateInsert;
    
    { Search Operations }
    procedure TestSearchExisting;
    procedure TestSearchNonExisting;
    procedure TestContains;
    
    { Prefix/Suffix Tests }
    procedure TestPrefixInsert;
    procedure TestSuffixInsert;
    procedure TestCommonPrefix;
    
    { Edge Splitting }
    procedure TestSimpleSplit;
    procedure TestDeepSplit;
    procedure TestMultipleSplits;
    
    { Terminal Nodes }
    procedure TestTerminalData;
    procedure TestPrefixBecomesTerminal;
    
    { TNibbles Tests }
    procedure TestNibblesCreate;
    procedure TestNibblesEquals;
    procedure TestNibblesToString;
    procedure TestNibblesTruncate;
    procedure TestNibblesGetItem;
    
    { Serialization Tests }
    procedure TestSaveLoadEmpty;
    procedure TestSaveLoadSingle;
    procedure TestSaveLoadMultiple;
    procedure TestSaveLoadWithData;
    procedure TestSaveLoadComplex;
    
    { Tree Structure }
    procedure TestTreeSize;
    procedure TestRootNode;
  end;

implementation

{ TTestData }

constructor TTestData.Create;
begin
  inherited Create;
  Value := 0;
  Name := '';
end;

constructor TTestData.Create(AValue: Integer; const AName: AnsiString);
begin
  inherited Create;
  Value := AValue;
  Name := AName;
end;

procedure TTestData.SaveToStream(Stream: TStream);
var
  Len: Integer;
begin
  Stream.WriteBuffer(Value, SizeOf(Value));
  Len := Length(Name);
  Stream.WriteBuffer(Len, SizeOf(Len));
  if Len > 0 then
    Stream.WriteBuffer(Name[1], Len);
end;

procedure TTestData.LoadFromStream(Stream: TStream);
var
  Len: Integer;
begin
  Stream.ReadBuffer(Value, SizeOf(Value));
  Stream.ReadBuffer(Len, SizeOf(Len));
  if Len > 0 then
  begin
    SetLength(Name, Len);
    Stream.ReadBuffer(Name[1], Len);
  end
  else
    Name := '';
end;

{ TTestRadixTree }

procedure TTestRadixTree.SetUp;
begin
  Tree := TStringTree.Create;
end;

procedure TTestRadixTree.TearDown;
begin
  Tree.Free;
end;

{ Basic Operations }

procedure TTestRadixTree.TestEmptyTree;
begin
  AssertEquals('Empty tree size should be 0', 0, Tree.Size);
  AssertNull('Empty tree root should be nil', Tree.Root);
  AssertFalse('Empty tree should not contain anything', Tree.Contains('test'));
end;

procedure TTestRadixTree.TestSingleInsert;
var
  Node: TNode;
begin
  Node := Tree.Insert('apple');
  
  AssertNotNull('Insert should return a node', Node);
  AssertTrue('Node should be terminal', Node.IsTerminal);
  AssertEquals('Tree size should be 1', 1, Tree.Size);
  AssertNotNull('Root should not be nil after insert', Tree.Root);
end;

procedure TTestRadixTree.TestMultipleInserts;
begin
  Tree.Insert('apple');
  Tree.Insert('banana');
  Tree.Insert('cherry');
  
  AssertEquals('Tree should contain 3 items', 3, Tree.Size);
  AssertTrue('Tree should contain "apple"', Tree.Contains('apple'));
  AssertTrue('Tree should contain "banana"', Tree.Contains('banana'));
  AssertTrue('Tree should contain "cherry"', Tree.Contains('cherry'));
end;

procedure TTestRadixTree.TestDuplicateInsert;
var
  Node1, Node2: TNode;
begin
  Node1 := Tree.Insert('test');
  AssertEquals('Size should be 1 after first insert', 1, Tree.Size);
  
  Node2 := Tree.Insert('test');
  AssertEquals('Size should remain 1 after duplicate', 1, Tree.Size);
  AssertEquals('Same node should be returned', PtrUInt(Node1), PtrUInt(Node2));
end;

{ Search Operations }

procedure TTestRadixTree.TestSearchExisting;
var
  InsertNode, SearchNode: TNode;
begin
  InsertNode := Tree.Insert('hello');
  SearchNode := Tree.Search('hello');
  
  AssertNotNull('Search should find the node', SearchNode);
  AssertEquals('Should return the same node', PtrUInt(InsertNode), PtrUInt(SearchNode));
end;

procedure TTestRadixTree.TestSearchNonExisting;
var
  Node: TNode;
begin
  Tree.Insert('hello');
  
  Node := Tree.Search('world');
  AssertNull('Search should return nil for non-existing string', Node);
  
  Node := Tree.Search('hel');
  AssertNull('Search should return nil for prefix only', Node);
  
  Node := Tree.Search('hello world');
  AssertNull('Search should return nil for longer string', Node);
end;

procedure TTestRadixTree.TestContains;
begin
  Tree.Insert('test');
  
  AssertTrue('Contains should return true for existing string', Tree.Contains('test'));
  AssertFalse('Contains should return false for non-existing', Tree.Contains('testing'));
  AssertFalse('Contains should return false for prefix', Tree.Contains('tes'));
end;

{ Prefix/Suffix Tests }

procedure TTestRadixTree.TestPrefixInsert;
begin
  Tree.Insert('internet');
  AssertEquals('Size should be 1', 1, Tree.Size);
  
  Tree.Insert('intern');
  AssertEquals('Size should be 2 after prefix insert', 2, Tree.Size);
  
  AssertTrue('Should contain longer string', Tree.Contains('internet'));
  AssertTrue('Should contain prefix', Tree.Contains('intern'));
end;

procedure TTestRadixTree.TestSuffixInsert;
begin
  Tree.Insert('test');
  AssertEquals('Size should be 1', 1, Tree.Size);
  
  Tree.Insert('testing');
  AssertEquals('Size should be 2 after suffix insert', 2, Tree.Size);
  
  AssertTrue('Should contain shorter string', Tree.Contains('test'));
  AssertTrue('Should contain suffix', Tree.Contains('testing'));
end;

procedure TTestRadixTree.TestCommonPrefix;
begin
  Tree.Insert('apple');
  Tree.Insert('application');
  Tree.Insert('apply');
  
  AssertEquals('Size should be 3', 3, Tree.Size);
  AssertTrue('Should contain "apple"', Tree.Contains('apple'));
  AssertTrue('Should contain "application"', Tree.Contains('application'));
  AssertTrue('Should contain "apply"', Tree.Contains('apply'));
end;

{ Edge Splitting }

procedure TTestRadixTree.TestSimpleSplit;
begin
  Tree.Insert('apple');
  Tree.Insert('apply');
  
  AssertEquals('Size should be 2 after split', 2, Tree.Size);
  AssertTrue('Should contain "apple"', Tree.Contains('apple'));
  AssertTrue('Should contain "apply"', Tree.Contains('apply'));
  AssertFalse('Should not contain common prefix only', Tree.Contains('appl'));
end;

procedure TTestRadixTree.TestDeepSplit;
begin
  Tree.Insert('romane');
  Tree.Insert('romanus');
  Tree.Insert('romulus');
  
  AssertEquals('Size should be 3', 3, Tree.Size);
  AssertTrue('Should contain "romane"', Tree.Contains('romane'));
  AssertTrue('Should contain "romanus"', Tree.Contains('romanus'));
  AssertTrue('Should contain "romulus"', Tree.Contains('romulus'));
end;

procedure TTestRadixTree.TestMultipleSplits;
begin
  Tree.Insert('test');
  Tree.Insert('testing');
  Tree.Insert('tested');
  Tree.Insert('tester');
  Tree.Insert('toast');
  
  AssertEquals('Size should be 5', 5, Tree.Size);
  AssertTrue('Should contain all inserted strings', 
    Tree.Contains('test') and 
    Tree.Contains('testing') and 
    Tree.Contains('tested') and 
    Tree.Contains('tester') and 
    Tree.Contains('toast'));
end;

{ Terminal Nodes }

procedure TTestRadixTree.TestTerminalData;
var
  Node: TNode;
  Data: TTestData;
begin
  Data := TTestData.Create(42, 'FortyTwo');
  
  Node := Tree.Insert('answer');
  Node.Data := Data;
  
  AssertEquals('Data value should be preserved', 42, Node.Data.Value);
  AssertEquals('Data name should be preserved', 'FortyTwo', Node.Data.Name);
end;

procedure TTestRadixTree.TestPrefixBecomesTerminal;
var
  Node1, Node2: TNode;
begin
  Node1 := Tree.Insert('testing');
  Node1.Data := TTestData.Create(1, 'One');
  
  Node2 := Tree.Insert('test');
  Node2.Data := TTestData.Create(2, 'Two');
  
  AssertEquals('Size should be 2', 2, Tree.Size);
  
  Node1 := Tree.Search('testing');
  Node2 := Tree.Search('test');
  
  AssertEquals('testing node should have value 1', 1, Node1.Data.Value);
  AssertEquals('test node should have value 2', 2, Node2.Data.Value);
end;

{ TNibbles Tests }

procedure TTestRadixTree.TestNibblesCreate;
var
  Nib: TNibbles;
begin
  Nib := TNibbles.Create('hello');
  
  // 'hello' = 5 bytes = 10 nibbles
  AssertEquals('Count should be 10 nibbles for 5 bytes', 10, Nib.Count);
  AssertEquals('ToString should return original string', 'hello', Nib.ToString);
end;

procedure TTestRadixTree.TestNibblesEquals;
var
  Nib1, Nib2, Nib3: TNibbles;
begin
  Nib1 := TNibbles.Create('test');
  Nib2 := TNibbles.Create('test');
  Nib3 := TNibbles.Create('different');
  
  AssertTrue('Equal nibbles should return true', Nib1.Equals(Nib2));
  AssertFalse('Different nibbles should return false', Nib1.Equals(Nib3));
end;

procedure TTestRadixTree.TestNibblesToString;
var
  Nib: TNibbles;
begin
  Nib := TNibbles.Create('Hello World');
  AssertEquals('ToString should preserve string', 'Hello World', Nib.ToString);
end;

procedure TTestRadixTree.TestNibblesTruncate;
var
  Nib: TNibbles;
begin
  Nib := TNibbles.Create('testing');
  // 'testing' = 7 bytes = 14 nibbles
  AssertEquals('Initial count should be 14 nibbles', 14, Nib.Count);
  
  // Truncate to 8 nibbles (4 bytes)
  Nib.Truncate(8);
  AssertEquals('Count should be 8 after truncate', 8, Nib.Count);
  AssertEquals('ToString should reflect truncation', 'test', Nib.ToString);
  
  Nib.Truncate(20);
  AssertEquals('Truncate beyond length should not extend', 8, Nib.Count);
  
  Nib.Truncate(-1);
  AssertEquals('Negative truncate should set to 0', 0, Nib.Count);
end;

procedure TTestRadixTree.TestNibblesGetItem;
var
  Nib: TNibbles;
  Failed: Boolean;
begin
  Nib := TNibbles.Create('abc');
  
  // 'a' = 0x61, high nibble = 6, low nibble = 1
  AssertEquals('First nibble (high) of "a" should be 6', 6, Nib[0]);
  AssertEquals('Second nibble (low) of "a" should be 1', 1, Nib[1]);
  // 'b' = 0x62, high nibble = 6, low nibble = 2  
  AssertEquals('Third nibble (high) of "b" should be 6', 6, Nib[2]);
  AssertEquals('Fourth nibble (low) of "b" should be 2', 2, Nib[3]);
  
  Failed := False;
  try
    Nib[10]; // Out of bounds
  except
    on E: Exception do
      Failed := True;
  end;
  AssertTrue('Out of bounds access should raise exception', Failed);
end;

{ Serialization Tests }

procedure TTestRadixTree.TestSaveLoadEmpty;
var
  Stream: TMemoryStream;
  LoadedTree: TStringTree;
begin
  Stream := TMemoryStream.Create;
  LoadedTree := TStringTree.Create;
  try
    Tree.SaveToStream(Stream);
    
    Stream.Position := 0;
    LoadedTree.LoadFromStream(Stream);
    
    AssertEquals('Loaded tree should be empty', 0, LoadedTree.Size);
    AssertNull('Loaded tree root should be nil', LoadedTree.Root);
  finally
    LoadedTree.Free;
    Stream.Free;
  end;
end;

procedure TTestRadixTree.TestSaveLoadSingle;
var
  Stream: TMemoryStream;
  LoadedTree: TStringTree;
begin
  Tree.Insert('test');
  
  Stream := TMemoryStream.Create;
  LoadedTree := TStringTree.Create;
  try
    Tree.SaveToStream(Stream);
    
    Stream.Position := 0;
    LoadedTree.LoadFromStream(Stream);
    
    AssertEquals('Loaded tree size should be 1', 1, LoadedTree.Size);
    AssertTrue('Loaded tree should contain "test"', LoadedTree.Contains('test'));
  finally
    LoadedTree.Free;
    Stream.Free;
  end;
end;

procedure TTestRadixTree.TestSaveLoadMultiple;
var
  Stream: TMemoryStream;
  LoadedTree: TStringTree;
  TestStrings: array[0..4] of string = ('apple', 'banana', 'cherry', 'date', 'elderberry');
  i: Integer;
begin
  for i := 0 to High(TestStrings) do
    Tree.Insert(TestStrings[i]);
  
  Stream := TMemoryStream.Create;
  LoadedTree := TStringTree.Create;
  try
    Tree.SaveToStream(Stream);
    
    Stream.Position := 0;
    LoadedTree.LoadFromStream(Stream);
    
    AssertEquals('Loaded tree size should match', Tree.Size, LoadedTree.Size);
    
    for i := 0 to High(TestStrings) do
      AssertTrue(Format('Loaded tree should contain "%s"', [TestStrings[i]]), 
                 LoadedTree.Contains(TestStrings[i]));
  finally
    LoadedTree.Free;
    Stream.Free;
  end;
end;

procedure TTestRadixTree.TestSaveLoadWithData;
var
  Stream: TMemoryStream;
  LoadedTree: TStringTree;
  Node, LoadedNode: TNode;
  Data: TTestData;
begin
  Data := TTestData.Create(123, 'TestData');
  Node := Tree.Insert('key');
  Node.Data := Data;
  
  Stream := TMemoryStream.Create;
  LoadedTree := TStringTree.Create;
  try
    Tree.SaveToStream(Stream);
    
    Stream.Position := 0;
    LoadedTree.LoadFromStream(Stream);
    
    LoadedNode := LoadedTree.Search('key');
    
    AssertNotNull('Loaded node should exist', LoadedNode);
    AssertEquals('Loaded data value should match', 123, LoadedNode.Data.Value);
    AssertEquals('Loaded data name should match', 'TestData', LoadedNode.Data.Name);
  finally
    LoadedTree.Free;
    Stream.Free;
  end;
end;

procedure TTestRadixTree.TestSaveLoadComplex;
var
  Stream: TMemoryStream;
  LoadedTree: TStringTree;
  Node: TNode;
  i: Integer;
  TestData: array[0..2] of record
    Key: string;
    Value: Integer;
    Name: string;
  end = (
    (Key: 'apple'; Value: 1; Name: 'One'),
    (Key: 'apply'; Value: 2; Name: 'Two'),
    (Key: 'application'; Value: 3; Name: 'Three')
  );
begin
  // Insert with splits and data
  for i := 0 to High(TestData) do
  begin
    Node := Tree.Insert(TestData[i].Key);
    Node.Data := TTestData.Create(TestData[i].Value, TestData[i].Name);
  end;
  
  Stream := TMemoryStream.Create;
  LoadedTree := TStringTree.Create;
  try
    Tree.SaveToStream(Stream);
    
    Stream.Position := 0;
    LoadedTree.LoadFromStream(Stream);
    
    AssertEquals('Loaded tree size should match', Tree.Size, LoadedTree.Size);
    
    for i := 0 to High(TestData) do
    begin
      Node := LoadedTree.Search(TestData[i].Key);
      AssertNotNull(Format('Node for "%s" should exist', [TestData[i].Key]), Node);
      AssertEquals(Format('Value for "%s" should match', [TestData[i].Key]), 
                   TestData[i].Value, Node.Data.Value);
      AssertEquals(Format('Name for "%s" should match', [TestData[i].Key]), 
                   TestData[i].Name, Node.Data.Name);
    end;
  finally
    LoadedTree.Free;
    Stream.Free;
  end;
end;

{ Tree Structure }

procedure TTestRadixTree.TestTreeSize;
begin
  AssertEquals('Initial size should be 0', 0, Tree.Size);
  
  Tree.Insert('a');
  AssertEquals('Size should be 1', 1, Tree.Size);
  
  Tree.Insert('b');
  AssertEquals('Size should be 2', 2, Tree.Size);
  
  Tree.Insert('a'); // Duplicate
  AssertEquals('Size should remain 2 after duplicate', 2, Tree.Size);
end;

procedure TTestRadixTree.TestRootNode;
begin
  AssertNull('Root should be nil for empty tree', Tree.Root);
  
  Tree.Insert('test');
  AssertNotNull('Root should not be nil after insert', Tree.Root);
end;

initialization
  RegisterTest(TTestRadixTree);

end.


{ Main Program }

var
  TestRunner: TTestRunner;
  Results: TTestResult;
begin
  WriteLn('RadixTree Unit Tests');
  WriteLn('===================');
  WriteLn;
  
  TestRunner := TTestRunner.Create(nil);
  try
    TestRunner.TestResult := TTestResult.Create;
    Results := TestRunner.TestResult;
    
    try
      // Run all tests
      TestRunner.Test := GetTestRegistry;
      TestRunner.RunTest(GetTestRegistry);
      
      // Display results
      WriteLn;
      WriteLn('Test Results:');
      WriteLn('-------------');
      WriteLn(Format('Tests run: %d', [Results.RunTests]));
      WriteLn(Format('Passed: %d', [Results.RunTests - Results.NumberOfErrors - Results.NumberOfFailures]));
      WriteLn(Format('Failures: %d', [Results.NumberOfFailures]));
      WriteLn(Format('Errors: %d', [Results.NumberOfErrors]));
      
      // Show failures
      if Results.NumberOfFailures > 0 then
      begin
        WriteLn;
        WriteLn('Failures:');
        WriteLn('---------');
        // Note: Individual failure details would require iterating through Results
      end;
      
      // Show errors
      if Results.NumberOfErrors > 0 then
      begin
        WriteLn;
        WriteLn('Errors:');
        WriteLn('-------');
        // Note: Individual error details would require iterating through Results
      end;
      
      WriteLn;
      if (Results.NumberOfErrors = 0) and (Results.NumberOfFailures = 0) then
        WriteLn('All tests passed!')
      else
        WriteLn('Some tests failed.');
        
    finally
      Results.Free;
    end;
  finally
    TestRunner.Free;
  end;
end.
