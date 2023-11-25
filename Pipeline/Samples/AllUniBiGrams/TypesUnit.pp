unit TypesUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, GenericCollectionUnit, LinkedListUnit;

type
  TPositionList = specialize TCollection<UInt64>;

  { TObjectString }

  TObjectString = class(TObject)
  private
    FData: AnsiString;
  public
    property Data: AnsiString read FData;

    constructor Create(_Data: AnsiString);

  end;

  TMemory = specialize TLinkedList<TObjectString>;

  { TInputFileSource }

  TInputFileSource = class(TObject)
  private
    FMemory: TMemory;
    FLength: UInt64;

  public
    property Memory: TMemory        read FMemory;
    property Length: UInt64 read FLength;

    constructor Create(InputFilename: AnsiString);
    destructor Destroy; override;

  end;

implementation

uses
  ALoggerUnit;

{ TObjectString }

constructor TObjectString.Create(_Data: AnsiString);
begin
  inherited Create;

  FData := _Data;

end;

{ TInputFileSource }

constructor TInputFileSource.Create(InputFilename: AnsiString);
var
  Reader: TStream;
  Buffer: AnsiString;
  MemorySize: Int64;
  Len: Integer;

const
  BufferSize = 64 * 1024 * 1024;

begin
  inherited Create;

  ALoggerUnit.GetLogger.FMTDebugLn('InputFile: %s', [InputFilename]);
  Reader := TFileStream.Create(InputFilename, fmOpenRead);
  FMemory := TMemory.Create;

  SetLength(Buffer, BufferSize);
  Len := Reader.Read(Buffer[1], BufferSize);
  MemorySize := 0;
  while 0 < Len do
  begin
    MemorySize += Len;
    if Len = BufferSize then
      FMemory.AddData(TObjectString.Create(Buffer))
    else
      FMemory.AddData(TObjectString.Create(Copy(Buffer, 1, Len)));

    Len := Reader.Read(Buffer[1], BufferSize);

  end;

  Reader.Free;
end;

destructor TInputFileSource.Destroy;
begin
  inherited Destroy;
end;

end.

