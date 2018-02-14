program TriUnitTest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, TriUnit
  { you can add units after this };
var
  STri: specialize TTrie<Char, Int64>;

begin
  STri := specialize TTrie<Char, Int64>.Create;

  STri.Add('aaa', 1);
  STri.Add('aab', 2);
  WriteLn(STri.ValueOrDefault('aaa', 0));
  WriteLn(STri.ValueOrDefault('aab', 0));
  WriteLn(STri.ValueOrDefault('aca', 0));
  STri.Add('aca', 3);
  WriteLn(STri.ValueOrDefault('aca', 0));
  STri.Add('bca', 4);
  WriteLn(STri.ValueOrDefault('امیر', 0));
  STri.Add('امیر', 5);
  WriteLn(STri.ValueOrDefault('امیر', 0));
  STri.Add('aca امیر', 6);
  WriteLn(STri.ValueOrDefault('aca امیر', 0));

  STri.Free;
end.

