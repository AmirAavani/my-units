unit ParamUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ParamManagerUnit;

type
  { TParams }

  {$M+}
  TParams = class(TValue)
  public type

  { TPipelineParams }

    TPipelineParams = class(TValue)
    private

    published
      FromStepID: TIntValue;
      StepID: TIntValue;
      TaskID: TIntValue;

    public
      constructor Create;
      destructor Destroy; override;

      function ToString: AnsiString;
    end;


  published
    DebugEnd: TIntValue;
    DebugIndex: TIntValue;
    DebugStart: TIntValue;
    InputFile: TStringValue;
    Mode: TStringValue;
    Pipeline: TPipelineParams;
    WorkingDir: TStringValue;
    Debug: TIntValue;

  public
    constructor Create; override;
    destructor Destroy; override;

    function ToString: AnsiString; override;

  end;

function GetParams: TParams;

implementation

var
  Params: TParams;

function GetParams: TParams;
begin
  Result := Params;

end;

{ TParams }

constructor TParams.Create;
begin
  inherited Create;

end;

destructor TParams.Destroy;
begin
  Pipeline.Free;
  DebugEnd.Free;
  DebugIndex.Free;
  DebugStart.Free;
  InputFile.Free;
  Mode.Free;
  WorkingDir.Free;
  Debug.Free;

  inherited Destroy;
end;

function TParams.ToString: AnsiString;
begin
    Result := Format('DebugEnd: %d DebugIndex: %d DebugStart: %d InputFile: %s' +
      ' Mode: %s Pipeline: (%s) WorkingDir: %s Debug: %d',
      [DebugEnd.Value,
       DebugIndex.Value,
       DebugStart.Value,
       InputFile.Value,
       Mode.Value,
       Pipeline.ToString,
       WorkingDir.Value,
       Debug.Value
       ]);

end;

{ TParams.TPipelineParams }

constructor TParams.TPipelineParams.Create;
begin

end;

destructor TParams.TPipelineParams.Destroy;
begin
  inherited Destroy;

  FromStepID.Free;
  StepID.Free;
  TasKID.Free;

end;

function TParams.TPipelineParams.ToString: AnsiString;
begin
  Result := Format('StepID: %d FromStepID: %d TaskID: %d',
    [StepID.Value, FromStepID.Value, TasKID.Value])

end;

initialization
  Params := TParams.Create;
  ParamManagerUnit.InitAndParse(
    'DebugEnd=-1,DebugIndex=-1,DebugStart=-1,InputFile=,Mode=,WorkingDir=' +
    ',Pipeline.StepID=-1,Pipeline.FromStepID=-1,Pipeline.TaskID=-1',
    Params);
  WriteLn(Params.ToString);
  ParamManagerUnit.InitFromParameters(Params);
  WriteLn(Params.ToString);
  Flush(Output);


finalization
  Params.Free;

end.

