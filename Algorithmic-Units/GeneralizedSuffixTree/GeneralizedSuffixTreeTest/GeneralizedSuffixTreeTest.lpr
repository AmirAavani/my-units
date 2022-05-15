program GeneralizedSuffixTreeTest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, StreamUnit, WideStringUnit, GeneralizedSuffixTreeUnit, DocUnit;

var
  Tree: TGeneralizedSuffixTree;
  Stream: TMyBinStream;

begin

  Tree := TGeneralizedSuffixTree.Create;

  Tree.AddDoc(TStringDoc.Create('Mississipi'));
  Tree.AddDoc(TStringDoc.Create('AMiR'));
  Tree.AddDoc(TStringDoc.Create('R'));
  Tree.AddDoc(TStringDoc.Create('R'));

  WriteLn('PrintAllTransitions');
  Tree.PrintAllTransitions;

  WriteLn('PrintAll');
  Tree.PrintAll;

  WriteLn('DumpTree');
  Stream := TMyBinStream.Create(TFileStream.Create('/tmp/a.bin', fmCreate));
  Tree.DumpTree;
  Stream.Free;

  Tree.Free;

end.

