unit RunInAThreadUnit;

{$mode objfpc}{$H+}

interface

uses
  Pipeline.TypesUnit, Classes, SysUtils;

type
  TThreadFunctionPtr = function (SysArgs, Args: TPointerArray): Boolean;

procedure RunInThread(F: TThreadFunctionPtr; SysArgs, Args: TPointerArray;
  OutputResult: PBoolean);

implementation

type
  { TRunnerThread }

  TRunnerThread = class(TThread)
  private
    SysArguments, Arguments: array of Pointer;
    F: TThreadFunctionPtr;
    Result: PBoolean;

  public
    constructor Create(FToRun: TThreadFunctionPtr; SysArgs, Args: TPointerArray;
       Res: PBoolean);
    destructor Destroy; override;

    procedure Execute; override;
  end;

procedure RunInThread(F: TThreadFunctionPtr; SysArgs, Args: TPointerArray;
  OutputResult: PBoolean);
var
  Thread: TThread;

begin
  Thread := TRunnerThread.Create(F, SysArgs, Args, OutputResult);
  Thread.FreeOnTerminate := True;
  Thread.Suspended := False;

end;

{ TRunnerThread }

constructor TRunnerThread.Create(FToRun: TThreadFunctionPtr; SysArgs,
  Args: TPointerArray; Res: PBoolean);
var
  i: Integer;

begin
  inherited Create(True);

  F := FToRun;
  SetLength(Arguments, Length(Args));
  for i := 0 to High(Args) do
    Arguments[i] := Args[i];
  SetLength(SysArguments, Length(SysArgs));
  for i := 0 to High(SysArgs) do
    SysArguments[i] := SysArgs[i];
  Result := Res;

end;

destructor TRunnerThread.Destroy;
begin
  SetLength(Arguments, 0);

  inherited Destroy;
end;

procedure TRunnerThread.Execute;
begin
  Result^ := F(SysArguments, Arguments);

end;

end.

