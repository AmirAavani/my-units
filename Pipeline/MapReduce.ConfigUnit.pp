unit MapReduce.ConfigUnit;

{$mode ObjFPC}{$H+}
{$modeswitch ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils;
type
  { TRunConfig }

  TRunConfig = record
  public
  type
    TMode = (InMemory = 1);

  private
    FMode: TMode;
    FNumProcesses: Integer;
    FProcessIndex: Integer;
    FNumThreads: Integer;
    FQueueSize: Integer;

  public
    property Mode: TMode read FMode;
    property NumProcesses: Integer read FNumProcesses;
    property NumThreads: Integer read FNumThreads;
    property QueueSize: Integer read FQueueSize;
    property ProcessIndex: Integer read FProcessIndex;

    function Init: TRunConfig;

    function SetMode(AValue: TMode): TRunConfig;
    function SetNumProcesses(n: Integer): TRunConfig;
    function SetNumThreads(n: Integer): TRunConfig;
    function SetProcessIndex(n: Integer): TRunConfig;


  end;


implementation

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

function TRunConfig.SetNumThreads(n: Integer): TRunConfig;
begin
  Self.FNumThreads := n;
  Result := Self;
end;

function TRunConfig.SetProcessIndex(n: Integer): TRunConfig;
begin
  Result := Self;
  Result.FProcessIndex := n;

end;

function TRunConfig.Init: TRunConfig;
begin
  FillChar(Self, SizeOF(Self), 0);

  Self.FMode := InMemory;
  Self.FNumProcesses := 1;
  Self.FProcessIndex := 0;
  Self.FNumThreads := 16;
  Self.FQueueSize := 64 * 1024;

end;


end.

