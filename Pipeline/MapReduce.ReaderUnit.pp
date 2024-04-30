unit MapReduce.ReaderUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, MapReduce.KeyValueUnit, GenericCollectionUnit, StreamUnit,
  TupleUnit, MapReduce.UtilsUnits;

type

  { TReader }

  TReader = class(TObject)
  protected
    FInputPattern: TPattern;

  public
  type
    TPairKeyValueBoolean = specialize TPair<TKeyValue, boolean>;
  public
    constructor Create(constref Pattern: AnsiString);
    destructor Destroy; override;

    function Next: TPairKeyValueBoolean; virtual; abstract;
  end;

  { TMyTextStreams }

  TMyTextStreams = class(specialize TCollection<TMyTextStream>)
  private
    FPattern: TPattern;

  public
    constructor Create(Pattern: TPattern);
    destructor Destroy; override;

  end;

  { TTextReader }

  TTextReader = class(TReader)
  protected
  type
  protected
    TextStreams: TMyTextStreams;

  public
    constructor Create(constref Pattern: AnsiString);

  end;

  { TTextLineReader }

  TTextLineReader = class(TTextReader)
  public
    constructor Create(constref Pattern: AnsiString);
    destructor Destroy; override;

  end;

implementation

{ TReader }

constructor TReader.Create(constref Pattern: AnsiString);
begin
  inherited Create;

  FInputPattern := TPattern.Create(Pattern);
end;

destructor TReader.Destroy;
begin
  inherited Destroy;
end;

{ TMyTextStreams }

constructor TMyTextStreams.Create(Pattern: TPattern);
begin
  inherited Create;

  FPattern := Pattern;
  Self.Count := Pattern.Count;
{  for i := 0 to Pattern.Count - 1 do
    Self.Add(
      TMyTextStream.Create(TFileStream.Create(Pattern.Filename[i], fmOpenRead))
      );
}
end;

destructor TMyTextStreams.Destroy;
begin
  inherited Destroy;
end;

{ TTextReader }

constructor TTextReader.Create(constref Pattern: AnsiString);
begin
  inherited Create(Pattern);
  TextStreams := TMyTextStreams.Create(FInputPattern);

end;

{ TTextLineReader }

constructor TTextLineReader.Create(constref Pattern: AnsiString);
begin
  inherited Create(Pattern);

end;

destructor TTextLineReader.Destroy;
begin
  inherited Destroy;
end;


end.
