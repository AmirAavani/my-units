program UseHeap;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, HeapUnit, sysutils, Generics.Collections
  { you can add units after this };

{$IFDEF WINDOWS}{$R UseHeap.rc}{$ENDIF}

type

  { TPair }

  TPair = record
    First: Integer;
    Second: AnsiString;

    function ToString: AnsiString;
    class operator <(a,b: TPair) : Boolean;

  end;

  TPairHeap = specialize THeap<TPair>;
  TPairs = specialize TList<TPair>;

function MakePair(n: Integer): TPair;
begin
  Result.First  := n;
  Result.Second := IntToStr(n);

end;


var
  Heap: TPairHeap;
  n: Integer;

{ TPair }

function TPair.ToString: AnsiString;
begin
  Result := Format('(%d:%s)', [Self.First, Self.Second]);

end;

class operator TPair.<(a, b: TPair): Boolean;
begin
  Result := a.First < b.First;
end;

var
  Pairs: TPairs;

begin
  Pairs := TPairs.Create;
  Heap:= TPairHeap.Create(Pairs);

  ReadLn(n);
  while n <> 0 do
  begin
    if n < 0 then
    begin
      WriteLn(Heap.Min.ToString);
      Heap.DeleteMin;

    end;
    if 0 < n then
      Heap.Insert(MakePair(n));

    Heap.Print;
    ReadLn(n);
  end;

  Heap.Free;
  Pairs.Free;
end.

