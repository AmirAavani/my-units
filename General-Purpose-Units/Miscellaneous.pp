{$mode objfpc}
unit Miscellaneous;
interface
type
  generic TPair<A, B> = record
    First: A;
    Second: B;

  end;

  generic function MakePair<A, B>(f: A; s: B): specialize TPair<A, B>;

implementation

generic function MakePair<A, B>(f: A; s: B): specialize TPair<A, B>;
begin
  Result.First := f;
  Result.Second := s;

end;


end.
