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

implementation

end.

