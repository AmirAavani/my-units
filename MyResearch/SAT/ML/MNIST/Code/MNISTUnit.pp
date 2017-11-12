unit MNISTUnit;

{$mode objfpc}{$H+}
{$Assertions on}
interface

uses
  StreamUnit, Classes, SysUtils, fgl;

type
  { TMNISTImage }

  TMNISTImage = class(TObject)
  private
    FImageLabel: Integer;
    FPixels: array [0..27, 0..27] of Byte;
    function GetPixel(r, c: Integer): Integer;


    constructor Create(const FStream: TMyBinStream; ImgLabel: Integer);

  public
    // Pixel[r, c] = True <=> [r,c] is black.
    property Pixel[r, c: Integer]: Integer read GetPixel;
    property ImageLabel: Integer read FImageLabel;

    function IsBlack(r, c: Integer): Boolean;
    destructor Destroy; override;
    procedure Print;
  end;

  { TImages }

  TMNISTImages = class(specialize TFPGList<TMNISTImage>)
  private
    FMagicNumber: Integer;
  public
    property MagicNumber : Integer read FMagicNumber;

    constructor Create(LabelStream, ImageStream: TStream; ImageCount: Integer = -1);
    destructor Destroy; override;
  end;

const
  MNISTImageHeight: Integer = 28;
  MNISTImageWidth: Integer = 28;

implementation
uses
  math;
{ TMNISTImages }

constructor TMNISTImages.Create(LabelStream, ImageStream: TStream;
  ImageCount: Integer);
var
  i: Integer;
  LabelDataStream, ImageDataStream: TMyBinStream;
  NumImages: Integer;
  Image: TMNISTImage;
  ImageLabel: Integer;

begin
  inherited Create;

  ImageDataStream := TMyBinStream.Create(ImageStream, False);
  LabelDataStream := TMyBinStream.Create(LabelStream, False);

  Assert(ImageDataStream.ReadInt = 2051, 'Invalid Magic number');
  NumImages:= ImageDataStream.ReadInt;
  Assert(ImageDataStream.ReadInt = 28, 'Invalid row number.');
  Assert(ImageDataStream.ReadInt = 28, 'Invalid column number.');

  Assert(LabelDataStream.ReadInt = 2049, 'Invalid Magic number');
  Assert(LabelDataStream.ReadInt = NumImages,
    'Invalid number of images in label file');

  for i := 1 to IfThen(ImageCount = -1, NumImages, ImageCount) do
  begin
    ImageLabel := LabelDataStream.ReadByte;
    Assert((0 <=ImageLabel) and (ImageLabel < 10),
      'Invalid label in label file');
    Image := TMNISTImage.Create(ImageDataStream, ImageLabel);
    Self.Add(Image);
  end;

  ImageDataStream.Free;
  LabelDataStream.Free;
end;

destructor TMNISTImages.Destroy;
var
  i: Integer;

begin

  for i := 0 to Self.Count - 1 do
    Self[i].Free;

  inherited Destroy;
end;

{ TImage }

function TMNISTImage.GetPixel(r, c: Integer): Integer;
begin
  Result := FPixels[r, c];
end;

function TMNISTImage.IsBlack(r, c: Integer): Boolean;
begin
  Result := 127 < Pixel[r, c];
end;

constructor TMNISTImage.Create(const FStream: TMyBinStream; ImgLabel: Integer
  );
var
  r, c: Integer;
begin
  inherited Create;

  for r := 0 to 27 do
    for c := 0 to 27 do
    begin
      FPixels[r, c] := FStream.ReadByte;
      {
        Assert((FPixels[r, c] = 0) or (FPixels[r, c] = 255),
        Format('Invalid Pixel data: Pixel[%d, %d] is %d', [r, c, FPixels[r, c]])
        );
      }
    end;

  FImageLabel:= ImgLabel;

end;

destructor TMNISTImage.Destroy;
begin
  inherited Destroy;
end;

procedure TMNISTImage.Print;
const
  BlackPixel: Char = 'B';
  WhitePixel: Char = 'W';

var
  r, c: Integer;

begin
  for r := 0 to MNISTImageHeight - 1 do
  begin
    for c := 0 to MNISTImageWidth - 1 do
      if IsBlack(r, c) then
        Write(BlackPixel)
      else
        Write(WhitePixel);
    WriteLn;
  end;

end;

end.

