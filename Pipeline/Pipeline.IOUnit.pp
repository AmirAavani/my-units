unit Pipeline.IOUnit;

{$mode objfpc}{$H+}

interface

uses
  StreamUnit, Classes, SysUtils;

type

  { TPipelineWriter }

  TPipelineWriter = class(TMyBinStream)
  public
    constructor Create(AFilename: AnsiString);
    destructor Destroy; override;

  end;

implementation

{ TPipelineWriter }

constructor TPipelineWriter.Create(AFilename: AnsiString);
begin
  inherited Create(TFileStream.Create(AFilename, fmCreate), True);

end;

destructor TPipelineWriter.Destroy;
begin

  inherited Destroy;
end;

end.

