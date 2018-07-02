unit MapHelperUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

generic function GetOrDefault<KeyType, ValueType>(aMap: specialize TFPGMap<KeyType, ValueType>;
    aKey: KeyType; DefaultValue: ValueType): ValueType;

implementation

generic function GetOrDefault<KeyType, ValueType>(aMap: specialize TFPGMap<KeyType, ValueType>;
    aKey: KeyType; DefaultValue: ValueType): ValueType;
begin

  if not aMap.TryGetData(aKey, Result) then
    Result := DefaultValue;

end;

end.

