unit TupleUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  generic TPair<TFirst, TSecond> = record
    First: TFirst;
    Second: TSecond;

  end;

  generic TTriplet<TFirst, TSecond, TThird> = record
    First: TFirst;
    Second: TSecond;
    Third: TThird;

  end;

  generic function MakePair<TFirst, TSecond>(First: TFirst; Second: TSecond): specialize TPair<TFirst, TSecond>;
  generic function MakeTriplet<TFirst, TSecond, TThird>(First: TFirst; Second: TSecond; Third: TThird): specialize TTriplet<TFirst, TSecond, TThird>;

implementation

generic function MakePair<TFirst, TSecond>(First: TFirst; Second: TSecond): specialize TPair<TFirst, TSecond>;
begin
  Result.First := First;
  Result.Second := Second;

end;

generic function MakeTriplet<TFirst, TSecond, TThird>(First: TFirst; Second: TSecond; Third: TThird): specialize TTriplet<TFirst, TSecond, TThird>;
begin
  Result.First := First;
  Result.Second := Second;
  Result.Third := Third;

end;

end.

