unit BaseEncoderUnit;

{$mode objfpc}{$H+}

interface

uses
  fgl, Classes, SysUtils, ClauseUnit, BaseConstraintUnit, BitVectorUnit;

type
  TEncoderClass = class of TBaseEncoder;
  TClassMap = specialize TFPGMap<string, TEncoderClass>;

  { EEncoderNotFound }

  EEncoderNotFound = class(Exception)
  end;

  { TBaseEncoder }

  TBaseEncoder = class(TObject)
  private
  protected
     FAllEncoders: TClassMap; static;

  public
    constructor Create;
    destructor Destroy; override;

    {
      Result.Clauses |- (Result.lit <=> Problem).
    }
    function Encode(Problem: TBaseConstraint): TEncoding; virtual; abstract;

    class function GetEncoder(const Name: AnsiString): TBaseEncoder;
    class function GetAllEncoders: TClassMap;
    class procedure RegisterEncoder(Encoder: TEncoderClass);

  end;

implementation

{ TBaseEncoder }

class function TBaseEncoder.GetAllEncoders: TClassMap;
begin
  if FAllEncoders = nil then
  begin
    FAllEncoders := TClassMap.Create;
    FAllEncoders.Sorted := True;
  end;
  Result := FAllEncoders;

end;

class procedure TBaseEncoder.RegisterEncoder(Encoder: TEncoderClass);
begin
  GetAllEncoders.Add(Encoder.ClassName, Encoder);
end;

constructor TBaseEncoder.Create;
begin
  inherited;

end;

destructor TBaseEncoder.Destroy;
begin
  inherited Destroy;
end;

class function TBaseEncoder.GetEncoder(const Name: AnsiString): TBaseEncoder;
var
  Index: Integer;

begin
  Index := GetAllEncoders.IndexOf(Name);
  if Index < 0 then
    raise EEncoderNotFound.Create('No Encoder with Name = ' + Name +
      ' is registered');
  Result := GetAllEncoders.Data[Index].Create;

end;

end.

