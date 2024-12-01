unit MapReduce.ConfigUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;
type
  { TRunConfig }

  TRunConfig = class(TObject)
  public
  type
    TMode = (InMemory = 1);

  private
    FMode: TMode;
    FNumProcesses: Integer;
    FProcessIndex: Integer;

  public
    property Mode: TMode read FMode;
    property NumProcesses: Integer read FNumProcesses;
    property ProcessIndex: Integer read FProcessIndex;

    constructor Create;

    class function NewRunConfg: TRunConfig;
    function SetMode(AValue: TMode): TRunConfig;
    function SetNumProcesses(n: Integer): TRunConfig;
    function SetProcessIndex(n: Integer): TRunConfig;


  end;


implementation

{ TRunConfig }

{ TRunConfig }

function TRunConfig.SetMode(AValue: TMode): TRunConfig;
begin
  Result := Self;
  Result.FMode := AValue;
end;

function TRunConfig.SetNumProcesses(n: Integer): TRunConfig;
begin
  Result := Self;
  Result.FNumProcesses := n;

end;

function TRunConfig.SetProcessIndex(n: Integer): TRunConfig;
begin
  Result := Self;
  Result.FProcessIndex := n;

end;

constructor TRunConfig.Create;
begin
  inherited Create;

  Self.FMode := InMemory;
  Self.FNumProcesses := 1;
  Self.FProcessIndex := 0;

end;

class function TRunConfig.NewRunConfg: TRunConfig;
begin
  Result := TRunConfig.Create;

end;


end.

