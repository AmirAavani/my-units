unit RunInAThreadUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Pipeline.TypesUnit;

type
  TThreadFunctionPtr = function (SysArgs: TPointerList): Boolean;

procedure RunInThread(F: TThreadFunctionPtr; SysArgs: TPointerList;
  OutputResult: PBoolean);

implementation
uses
  ALoggerUnit;

type
  { TRunnerThread }

  TRunnerThread = class(TThread)
  private
    SysArguments: TPointerList;
    F: TThreadFunctionPtr;
    Result: PBoolean;

  public
    // Caller is responsibe for freeing the memory for SysArgs and Args.
    constructor Create(FToRun: TThreadFunctionPtr; SysArgs: TPointerList;
       Res: PBoolean);
    destructor Destroy; override;

    procedure Execute; override;
  end;

procedure RunInThread(F: TThreadFunctionPtr; SysArgs: TPointerList;
  OutputResult: PBoolean);
var
  Thread: TThread;

begin
  Thread := TRunnerThread.Create(F, SysArgs, OutputResult);
  Thread.FreeOnTerminate := True;
  Thread.Suspended := False;

end;

{ TRunnerThread }

constructor TRunnerThread.Create(FToRun: TThreadFunctionPtr;
  SysArgs: TPointerList; Res: PBoolean);
begin
  inherited Create(True);

  F := FToRun;
  SysArguments := SysArgs;
  Result := Res;

end;

destructor TRunnerThread.Destroy;
begin

  inherited Destroy;
end;

procedure TRunnerThread.Execute;
var
  _Result: Boolean;

begin
  _Result := F(SysArguments);

  if Self.Result <> nil then
    Self.Result^ := _Result;

end;

end.

