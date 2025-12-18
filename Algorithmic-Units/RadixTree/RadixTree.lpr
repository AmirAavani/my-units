program RadixTree;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, RadixTreeUnit, GenericCollectionUnit, ALoggerUnit, SyncUnit
  { you can add units after this };

type
  TIntTree = specialize TTree<Integer>;

var
  Tree: TIntTree;

begin
  Tree := TIntTree.Create;

  Tree.Print;

  Tree.Insert('abcd');
  Tree.Print;
  Tree.Insert('abcdef');
  Tree.Print;

  Tree.Insert('bcde');
  Tree.Print;


end.

