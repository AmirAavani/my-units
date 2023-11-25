unit ParamUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ParameterManagerUnit_V2, ParameterManagerUnit, ValueUnit;

type
{$M+}
  { TParams }

  TParams = class(TBaseParam)
  public type

    { TPipelineParams }

    TPipelineParams = class(Tobject)
    private
      FFromStepID: Integer;
      FStepID: Integer;
      FTasKID: Integer;
    public
      property StepID: Integer read FStepID;
      property FromStepID: Integer read FFromStepID;
      property TasKID: Integer read FTasKID;

      constructor Create(Values: TRunTimeParameterManager.TNameValueMap);
      destructor Destroy; override;
    end;

  private
    FDebugEnd: Integer;
    FDebugIndex: Integer;
    FDebugStart: Integer;
    FInputFile: AnsiString;
    FMode: AnsiString;
    FPipeline: TPipelineParams;
    FWorkingDir: AnsiString;


  public
    property DebugEnd: Integer read FDebugEnd;
    property DebugIndex: Integer read FDebugIndex;
    property DebugStart: Integer read FDebugStart;
    property InputFile: AnsiString read FInputFile;
    property Mode: AnsiString read FMode;
    property Pipeline: TPipelineParams read FPipeline;
    property WorkingDir: AnsiString read FWorkingDir;

  public
    constructor Create(Values: TRunTimeParameterManager.TNameValueMap);
    destructor Destroy; override;

  end;

function GetRunTimeParameterManager: specialize TRunTimeParameterManager<TParams>;

implementation

uses
  StringUnit;

var
  ParamManager: specialize TRunTimeParameterManager<TParams>;

function GetRunTimeParameterManager: specialize TRunTimeParameterManager<TParams
  >;
begin
  Result := ParamManager;

end;

{ TParams }


constructor TParams.Create(Values: TRunTimeParameterManager.TNameValueMap);
var
  Value: ValueUnit.TValue;

begin
  inherited Create(Values);


  Value := nil;
  if Values.TryGetData('DebugEnd', Value) then
    Self.FDebugEnd := Value.AsInteger;
  if Values.TryGetData('DebugIndex', Value) then
    Self.FDebugIndex := Value.AsInteger;
  if Values.TryGetData('DebugStart', Value) then
    Self.FDebugStart := Value.AsInteger;
  if Values.TryGetData('InputFile', Value) then
    Self.FInputFile := Value.AsAnsiString;
  if Values.TryGetData('Mode', Value) then
    Self.FMode := Value.AsAnsiString;
  if Values.TryGetData('WorkingDir', Value) then
    Self.FWorkingDir := Value.AsAnsiString;
  if Values.TryGetData('WorkingDir', Value) then
    Self.FWorkingDir := Value.AsAnsiString;

end;

destructor TParams.Destroy;
begin
  Pipeline.Free;

  inherited Destroy;
end;

{ TParams.TPipelineParams }

constructor TParams.TPipelineParams.Create(
  Values: TRunTimeParameterManager.TNameValueMap);
var
  Value: ValueUnit.TValue;

begin
  inherited Create;

  Value := nil;
  if Values.TryGetData('Pipeline.StepID', Value) then
    Self.FStepID := Value.AsInteger;
  if Values.TryGetData('Pipeline.FromStepID', Value) then
    Self.FFromStepID := Value.AsInteger;
  if Values.TryGetData('Pipeline.TaskID', Value) then
    Self.FTasKID := Value.AsInteger;

end;

destructor TParams.TPipelineParams.Destroy;
begin
  inherited Destroy;
end;

initialization
  ParamManager := specialize TRunTimeParameterManager<TParams>.Create;

finalization
  ParamManager.Free;

end.

