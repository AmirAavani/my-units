unit RunInAThreadUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, GenericCollectionUnit;

type
  TObjectList = specialize TObjectCollection<TObject>;
  TThreadFunctionPtr = function (Args: TObjectList): Boolean;

procedure RunInThread(F: TThreadFunctionPtr; Args: TObjectList;
  OutputResult: PBoolean);

implementation
uses
  ALoggerUnit;

type
  { TRunnerThread }

  TRunnerThread = class(TThread)
  private
    Arguments: TObjectList;
    F: TThreadFunctionPtr;
    Result: PBoolean;
    FreeArgOnTerminate: Boolean;

  public
    constructor Create(FToRun: TThreadFunctionPtr; Args: TObjectList;
       Res: PBoolean; _FreeArgsOnTerminate: Boolean = False);
    destructor Destroy; override;

    procedure Execute; override;
  end;

procedure RunInThread(F: TThreadFunctionPtr; Args: TObjectList;
  OutputResult: PBoolean);
var
  Thread: TThread;

begin
  Thread := TRunnerThread.Create(F, Args, OutputResult);
  Thread.FreeOnTerminate := True;
  Thread.Suspended := False;

end;

{ TRunnerThread }

constructor TRunnerThread.Create(FToRun: TThreadFunctionPtr; Args: TObjectList;
  Res: PBoolean; _FreeArgsOnTerminate: Boolean);
begin
  inherited Create(True);

  F := FToRun;
  Arguments := Args;
  Result := Res;
  FreeArgOnTerminate := _FreeArgsOnTerminate;
end;

destructor TRunnerThread.Destroy;
var
  Obj: TObject;
begin
  if FreeArgOnTerminate then
  begin
    for Obj in Arguments do
      Obj.Free;

    Arguments.Free;
  end;

  inherited Destroy;
end;

procedure TRunnerThread.Execute;
var
  _Result: Boolean;

begin
  _Result := F(Arguments);

  if Self.Result <> nil then
    Self.Result^ := _Result;

end;

end.

