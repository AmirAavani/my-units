unit PageHandlerUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, HttpServerThreadUnit;

type

  { TBasePageHandler }

  TBasePageHandler = class(TObject)

  private
    FName: AnsiString;
    FServingPath: AnsiString;
  public
    property Name: AnsiString read FName;
    property ServingPath: AnsiString read FServingPath;
    constructor Create(aName: AnsiString; aServingPath: AnsiString);

  end;

implementation

{ TBasePageHandler }

constructor TBasePageHandler.Create(aName: AnsiString; aServingPath: AnsiString
  );
begin
  inherited Create;

  FName := aName;
  FServingPath := aServingPath;
end;

end.

