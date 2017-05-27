unit MatchingUnit;
{$mode Objfpc}
interface
uses
 fgl;
type
  TIntList = specialize TFPGList<Integer>;
  TAdjMartix= array of array of Boolean;

  function MaximumBipartiteMatching(M: TAdjMartix; n: Integer; Left, Right: TIntList): Integer;

implementation

function MaximumBipartiteMatching(M: TAdjMartix; n: Integer; Left, Right: TIntList): Integer;

var
  Visited: array of Boolean;

  function FindNextDFS(l: Integer): Boolean;
  var
    r: Integer;

  begin
    if l = -1 then
      Exit(True);

    if Visited[l] then
      Exit(False);

    Visited[l]:= True;

    for r := 0 to n - 1 do
      if M[l, r] then
        if FindNextDFS(Right[r]) then
        begin
          Right[r] := l;
          Left[l] := r;
          Exit(True);
        end;

    Result := False;

  end;

var
  l: Integer;

begin
  Result := 0;
  Left.Count := n;
  Right.Count := n;
  SetLength(Visited, n);
  for l := 0 to n - 1 do
  begin
    Left[l] := -1;
    Right[l] := -1;
  end;

  for l := 0 to n - 1 do
  begin
    FillChar(Visited[0], SizeOf(Visited), 0);
    if FindNextDFS(l) then
      Inc(Result);
  end;

end;

end.
