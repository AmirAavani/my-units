program GeneralizedSuffixTreeTest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, StreamUnit, WideStringUnit, GeneralizedSuffixTreeUnit, DocUnit,
  SuffixTreeDocUnit;

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
  Tree.DumpTree;

  WriteLn('Saving');
  Stream := TMyBinStream.Create(TFileStream.Create('/tmp/a.bin', fmCreate), True);
  Tree.SaveToStream(Stream);
  Stream.Free;

  Tree.Free;

  WriteLn('Loading');
  Stream := TMyBinStream.Create(TFileStream.Create('/tmp/a.bin', fmOpenRead), True);
  Tree := TGeneralizedSuffixTree.LoadFromStream(Stream);
  Stream.Free;

  WriteLn('PrintAllTransitions');
  Tree.PrintAllTransitions;

  WriteLn('PrintAll');
  Tree.PrintAll;

  WriteLn('DumpTree');
  Tree.DumpTree;

  Tree.AddDoc(TStringDoc.Create('R'));
  Tree.AddDoc(TStringDoc.Create('Amin'));

  WriteLn('DumpTree');
  Tree.DumpTree;
  WriteLn('PrintAll');
  Tree.PrintAll;
  WriteLn('PrintAllTransitions');
  Tree.PrintAllTransitions;

  Stream := TMyBinStream.Create(TFileStream.Create('/tmp/a.bin', fmCreate), True);
  Tree.SaveToStream(Stream);
  Stream.Free;

  Tree.Free;

  Tree := TGeneralizedSuffixTree.Create;

  Tree.AddDoc(TStringDoc.Create('Mississipi'));
  Tree.AddDoc(TStringDoc.Create('AMiR'));
  Tree.AddDoc(TStringDoc.Create('R'));
  Tree.AddDoc(TStringDoc.Create('R'));
  Tree.AddDoc(TStringDoc.Create('R'));
  Tree.AddDoc(TStringDoc.Create('Amin'));

  WriteLn('DumpTree');
  Tree.DumpTree;
  WriteLn('PrintAll');
  Tree.PrintAll;
  WriteLn('PrintAllTransitions');
  Tree.PrintAllTransitions;

  Stream := TMyBinStream.Create(TFileStream.Create('/tmp/a.bin', fmCreate), True);
  Tree.SaveToStream(Stream);
  Stream.Free;

  Tree.Free;

end.

