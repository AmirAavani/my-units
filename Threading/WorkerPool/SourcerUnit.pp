unit SourcerUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StreamUnit, GenericCollectionUnit, SyncUnit;

type

  { TBaseSourcer }

  generic TBaseSourcer<T> = class(TObject)
  protected
    function GetDone: Boolean; virtual; abstract;
    function GetNext: T; virtual; abstract;

  public
    property Done: Boolean read GetDone;
    property Next: T read GetNext;

    constructor Create;
    destructor Destroy; override;

  end;

  { TSoucrerFromCollection }

  generic TSoucrerFromCollection<T> = class(specialize TBaseSourcer<T>)
  private
    Mutex: TMutex;

  public type
    TCollectionT = specialize TObjectCollection<T>;

  protected
    CurrentIndex: Integer;
    TheCollection: TCollectiont;

    function GetNext: T; override;
    function GetDone: Boolean; override;
  public
    // Take the ownership of aCollection;
    constructor Create(aCollection: TCollectionT);
    destructor Destroy; override;

  end;

  { TLineReaderSourcer }

  TLineReaderSourcer = class(specialize TBaseSourcer<AnsiString>)
  protected
    FTextSteam: TMyTextStream;

    function GetNext: AnsiString; override;
    function GetDone: Boolean; override;
  public
    constructor Create(Stream: TStream);
    destructor Destroy; override;

  end;

implementation

uses
  ALoggerUnit;

{ TSoucrerFromCollection }

function TSoucrerFromCollection.GetNext: T;
var
  Index: Integer;

begin
  Mutex.Lock;

  Index := CurrentIndex;
  Inc(CurrentIndex);
  FMTDebugLn('Count: %d', [Index]);
  Mutex.Unlock;

//  Result := TheCollection[Index];

end;

function TSoucrerFromCollection.GetDone: Boolean;
begin
  Mutex.Lock;

//  Result := CurrentIndex = TheCollection.Count;

  Mutex.Unlock;

end;

constructor TSoucrerFromCollection.Create(aCollection: TCollectionT);
begin
  inherited Create;

  TheCollection := aCollection;
  CurrentIndex := 0;
  Mutex := TMutex.Create;

end;

destructor TSoucrerFromCollection.Destroy;
begin
  TheCollection.Free;
  Mutex.Free;

  inherited Destroy;
end;

{ TBaseSourcer }

constructor TBaseSourcer.Create;
begin
  inherited Create;

end;

destructor TBaseSourcer.Destroy;
begin

  inherited Destroy;
end;

{ TLineReaderSourcer }

constructor TLineReaderSourcer.Create(Stream: TStream);
begin
  inherited Create;

  FTextSteam := TMyTextStream.Create(Stream, True);

end;

destructor TLineReaderSourcer.Destroy;
begin
  FTextSteam.Free;

  inherited Destroy;
end;

function TLineReaderSourcer.GetNext: AnsiString;
begin
  Result := FTextSteam.ReadLine;

end;

function TLineReaderSourcer.GetDone: Boolean;
begin
  Result := FTextSteam.Size <= FTextSteam.Position;

end;

end.

