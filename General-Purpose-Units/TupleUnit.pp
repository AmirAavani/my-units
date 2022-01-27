unit PairUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  generic TPair<TFirst, TSecond> = record
    First: TFirst;
    Second: TSecond;

  end;

  generic function MakePair<TFirst, TSecond>(First: TFirst; Second: TSecond): specialize TPair<TFirst, TSecond>;

implementation

generic function MakePair<TFirst, TSecond>(First: TFirst; Second: TSecond): specialize TPair<TFirst, TSecond>;
begin

end;

end.

