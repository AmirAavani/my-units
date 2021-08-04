unit MapperFunctionUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Mapper.ChannelUnit, Mapper.TypesUnit;

function GenerateRandomNumbersMapper(kv: TKeyValue; Output: TDataLine): Boolean;
function SumThemUpMapper(kv: TKeyValue; Output: TDataLine): Boolean;

implementation

function GenerateRandomNumbersMapper(kv: TKeyValue; Output: TDataLine): Boolean;
begin
  Result := True;

end;

function SumThemUpMapper(kv: TKeyValue; Output: TDataLine): Boolean;
begin
  Result := True;
end;

end.

