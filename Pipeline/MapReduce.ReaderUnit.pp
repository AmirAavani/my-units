unit MapReduce.ReaderUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ProtoHelperUnit, MapReduce.KeyValueUnit, GenericCollectionUnit,
  TupleUnit, MapReduce.UtilsUnits, SimpleTypesUnit;

type

  { TReader }

  generic TReader<TValue: TBaseMessage> = class(TObject)
  protected
    FInputPattern: TPattern;

  public
    constructor Create;
    destructor Destroy; override;

    function Next(var Key: AnsiString; Msg: TValue): Boolean; virtual; abstract;
  end;

  { TTextReader }

  TTextReader = class(specialize TReader<TStringMessage>)
  protected
  type
  protected
    TextStream: TFileStream;

  public
    constructor Create(constref Filename: AnsiString);
    destructor Destroy; override;

    function Next(var Key: AnsiString; Msg: TStringMessage): Boolean; override;
  end;

  { TTextLineReader }

  TTextLineReader = class(TTextReader)
  public
    constructor Create(constref Pattern: AnsiString);
    destructor Destroy; override;

  end;

implementation

{ TReader }

constructor TReader.Create;
begin
  inherited Create;

end;

destructor TReader.Destroy;
begin
  inherited Destroy;
end;


{ TTextReader }

constructor TTextReader.Create(constref Filename: AnsiString);
begin
  inherited Create;
  TextStream := TFileStream.Create(Filename, fmOpenRead);

end;

destructor TTextReader.Destroy;
begin
  TextStream.Free;

  inherited Destroy;
end;

function TTextReader.Next(var Key: AnsiString; Msg: TStringMessage): Boolean;
begin

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
