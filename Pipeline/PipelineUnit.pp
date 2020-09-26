unit PipelineUnit;

{$mode objfpc}{$H+}

interface

uses
  TaskUnit, fgl, Classes, SysUtils;

type
  TTask = class;
  TStepHandler = function (Task: TTask): Boolean;

  { TPipeline }

  TPipeline = class(TObject)
  private type

    { TStepInfo }

    TStepInfo = class(TObject)
    private
      NumTasks: Integer;
      StepHandler: TStepHandler;

    public
      constructor Create(_NumTasks: Integer; Handler: TStepHandler);
      destructor Destroy; override;

    end;

    TStepInfoList = specialize TFPGList<TStepInfo>;

  private
    FName: AnsiString;
    Steps: TStepInfoList;

  protected
    function RunStep(Step: TStepInfo): Boolean;

  public
    property Name: AnsiString read FName;

    constructor Create(_Name: AnsiString);
    destructor Destroy; override;

    procedure AddNewStep(InputPattern, OutputPattern: AnsiString;
        Handler: TStepHandler; NumTasks: Integer);

    function Run: Boolean;
    function RunStep(StepIndex: Integer): Boolean;
    function RunFromStep(StepIndex: Integer): Boolean;
  end;


  { TTask }

  TTask = class(TOBject)
  private
    ID: Integer;
    Step: TPipeline.TStepInfo;

  public
    constructor Create(_ID: Integer; _Step: TPipeline.TStepInfo);

  end;


implementation

{ TTask }

constructor TTask.Create(_ID: Integer; _Step: TPipeline.TStepInfo);
begin
  inherited Create;

  ID := _ID;
  Step := _Step;


end;

{ TPipeline.TStepInfo }

constructor TPipeline.TStepInfo.Create(_NumTasks: Integer; Handler: TStepHandler
  );
begin
  inherited Create;

  NumTasks := _NumTasks;
  StepHandler := Handler;

end;

destructor TPipeline.TStepInfo.Destroy;
begin
  inherited Destroy;

end;

{ TPipeline }

function TPipeline.RunStep(Step: TStepInfo): Boolean;
begin

end;

constructor TPipeline.Create(_Name: AnsiString);
begin
  inherited Create;

  FName := _Name;
  Steps := TStepInfoList.Create;

end;

destructor TPipeline.Destroy;
var
  Step: TStepInfo;

begin
  for Step in Steps do
    Step.Free;

  inherited Destroy;
end;

procedure TPipeline.AddNewStep(InputPattern, OutputPattern: AnsiString;
  Handler: TStepHandler; NumTasks: Integer);
begin
  Steps.Add(TStepInfo.Create(NumTasks, Handler));

end;

function TPipeline.Run: Boolean;
begin
  Result := RunFromStep(1);

end;

function TPipeline.RunStep(StepIndex: Integer): Boolean;
begin
  Result := RunStep(Steps[StepIndex]);

end;

function TPipeline.RunFromStep(StepIndex: Integer): Boolean;
var
  Step: Integer;

begin
  for Step := StepIndex to Steps.Count do
    if not RunStep(Step) then
      Exit(False);

  Result := True;


end;

end.

