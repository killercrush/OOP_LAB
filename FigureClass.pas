unit FigureClass;

interface

uses Graphics, SysUtils;

type
  PPoint = ^TDot;
  PCircle = ^TCircle;

  TDot = class
  private { приватные поля и методы }
    X: integer; { координата X }
    Y: integer; { координата Y }
    FColor: integer; { цвет }
    FSize: integer; { размер }
    FAngle: integer; { угол }
    Canvas: TCanvas; // на нем рисуем
    FSelected: boolean;
    procedure SetSelection(Value: boolean);
    procedure DrawSelection; virtual;
    procedure SetSize(Value: integer); virtual; { изменить размер }
    procedure SetAngle(Value: integer); virtual;
    procedure SetColor(const Value: integer); { повернуть }
  public
    procedure Draw; virtual; { показать }
    procedure Hide; virtual; { стереть }
    constructor Create(aX, aY, AColor: integer; ACanvas: TCanvas;
      aSize: integer = 1; aAngle: integer = 0); { создать }
    destructor Destroy; override; { удалить }
    procedure Shift(aX, aY: integer); { подвинуть }
    function PointInFugure(aX, aY: integer): boolean; virtual;
    property Selected: boolean read FSelected write SetSelection;
    property Color: integer read FColor write SetColor;
    property Angle: integer read FAngle write SetAngle;
    property Size: integer read FSize write SetSize;
    function Info: string; virtual;
  end;

  TCircle = class(TDot)
  private
    // procedure DrawBorder(AColor: integer); override;
  public
    procedure Draw; override;
    function PointInFugure(aX, aY: integer): boolean; override;
  end;

  TSquare = class(TDot)
  private
    // procedure DrawBorder(AColor: integer); override;
  public
    procedure Draw; override;
    function PointInFugure(aX, aY: integer): boolean; override;
  end;

  TStar = class(TDot)
  private
    // procedure DrawBorder(AColor: integer); override;
  public
    procedure Draw; override;
    function PointInFugure(aX, aY: integer): boolean; override;
  end;

  PListItem = ^ListItem;

  ListItem = record
    Item: TDot;
    NextItem: PListItem;
  end;

  TList = class
  public
    FirstItem: PListItem;
    constructor Create(AFirstItem: PListItem = nil);
    destructor Destroy; override;
    procedure AddItem(AItem: TDot);
    procedure DeleteSelectedItem;
  end;

implementation

procedure TDot.SetSize(Value: integer);
begin
  Hide;
  FSize := Value;
  Draw;
end;

constructor TDot.Create(aX, aY, AColor: integer; ACanvas: TCanvas;
  aSize: integer = 1; aAngle: integer = 0);
begin
  { инициализируем поля экземпляра класса }
  X := aX;
  Y := aY;
  Canvas := ACanvas;
  Canvas.Brush.Style := bsClear;
  FColor := AColor;
  FSize := aSize;
  FAngle := aAngle;
  Draw;
  Selected := true;
end;

destructor TDot.Destroy;
begin
  { сотрем с экрана класс }
  Selected := False;
  Hide;
  inherited;
end;

procedure TDot.Hide;
var
  TmpColor: integer;
begin
  { нарисуем класс с 0-м цветом (стираем) }
  if FSelected then
  begin
    FSelected := false;
    DrawSelection;
    FSelected := true; ;
  end;
  TmpColor := FColor;
  FColor := $FFFFFF;
  Draw;
  FColor := TmpColor;
end;

function TDot.Info: string;
begin
  result := ClassName + ' X:' + IntToStr(X) + ' Y:' + IntToStr(Y);
end;

function TDot.PointInFugure(aX, aY: integer): boolean;
begin
  result := (Abs(aX - X) < 3) and (Abs(aY - Y) < 3);
end;

procedure TDot.SetAngle(Value: integer);
begin
  Hide;
  FAngle := Value;
  Draw;
end;

procedure TDot.SetColor(const Value: integer);
begin
  Hide;
  FColor := Value;
  Draw;
end;

procedure TDot.SetSelection(Value: boolean);
begin
  if Value = FSelected then
    exit;
  FSelected := Value;
  DrawSelection;
end;

procedure TDot.Shift(aX, aY: integer);
var
  TmpSelected: boolean;
begin
  Hide; { сотрем со старыми координатами }
  X := aX;
  Y := aY;
  Draw; { нарисуем с новыми координатами }
  DrawSelection;
end;

procedure TDot.Draw;
begin
  { рисуем класс }
  Canvas.Pen.Color := FColor;
  Canvas.Rectangle(X - 1, Y - 1, X + 2, Y + 2);
  Canvas.Pixels[X, Y] := Color;
end;

procedure TDot.DrawSelection;
var
  TmpColor: integer;
begin
  TmpColor := Color;
  if not FSelected then
    FColor := $FFFFFF;
  Canvas.Pen.Width := 2;
  Draw;
  Canvas.Pen.Width := 1;
  FColor := TmpColor;
  Draw;
end;

{ TCircle }

procedure TCircle.Draw;
begin

  Canvas.Pen.Color := FColor;
  Canvas.Ellipse(X - FSize, Y - FSize, X + FSize, Y + FSize);
end;

// procedure TCircle.DrawBorder(AColor: integer);
// begin
// Canvas.Pen.Color := AColor;
// Canvas.Ellipse(X - FSize - 1, Y - FSize - 1, X + FSize + 1, Y + FSize + 1);
// Canvas.Ellipse(X - FSize + 1, Y - FSize + 1, X + FSize - 1, Y + FSize - 1);
// end;

function TCircle.PointInFugure(aX, aY: integer): boolean;
begin
  result := (((aX - X) * (aX - X) + (aY - Y) * (aY - Y)) <= FSize * FSize);
end;

{ TList }

procedure TList.AddItem(AItem: TDot);
var
  ListItem, Tmp: PListItem;
begin
  New(ListItem);
  ListItem.Item := AItem;
  ListItem.NextItem := nil;

  if FirstItem = nil then
    FirstItem := ListItem
  else
  begin
    Tmp := FirstItem;
    while Tmp.NextItem <> nil do
      Tmp := Tmp.NextItem;
    Tmp.NextItem := ListItem;
  end;
end;

constructor TList.Create(AFirstItem: PListItem);
begin
  FirstItem := AFirstItem;
end;

procedure TList.DeleteSelectedItem;
var
  ListItem, PrevItem, NextItem: PListItem;
begin
  if FirstItem <> nil then
  begin
    ListItem := FirstItem;
    PrevItem := nil;
    repeat
      NextItem := ListItem.NextItem;
      if ListItem.Item.Selected then
      begin
        if PrevItem <> nil then
          PrevItem.NextItem := ListItem.NextItem
        else
          FirstItem := ListItem.NextItem;
        FreeAndNil(ListItem.Item);
        Dispose(ListItem);
      end;
      PrevItem := ListItem;
      ListItem := NextItem;
    until ListItem = nil;
  end;

end;

destructor TList.Destroy;
var
  ListItem, NextItem: PListItem;
begin
  if FirstItem <> nil then
  begin
    ListItem := FirstItem;
    repeat
      NextItem := ListItem.NextItem;
      FreeAndNil(ListItem.Item);
      Dispose(ListItem);
      ListItem := NextItem;
    until ListItem = nil;
  end;
  inherited;
end;

{ TSquare }

procedure TSquare.Draw;
var
  x1, y1, x2, y2, x3, y3, x4, y4: integer;
  RadAngle1, RadAngle2, RadAngle3, RadAngle4: double;
begin
  RadAngle1 := (FAngle + 135) * (pi / 180);
  RadAngle2 := (FAngle + 45) * (pi / 180);
  RadAngle3 := (FAngle + 315) * (pi / 180);
  RadAngle4 := (FAngle + 225) * (pi / 180);
  Canvas.Pen.Color := FColor;

  x1 := X + Round(FSize * (Cos(RadAngle1)));
  y1 := Y - Round(FSize * (Sin(RadAngle1)));
  x2 := X + Round(FSize * (Cos(RadAngle2)));
  y2 := Y - Round(FSize * (Sin(RadAngle2)));
  x3 := X + Round(FSize * (Cos(RadAngle3)));
  y3 := Y - Round(FSize * (Sin(RadAngle3)));
  x4 := X + Round(FSize * (Cos(RadAngle4)));
  y4 := Y - Round(FSize * (Sin(RadAngle4)));

  Canvas.MoveTo(x1, y1);
  Canvas.LineTo(x2, y2);
  Canvas.LineTo(x3, y3);
  Canvas.LineTo(x4, y4);
  Canvas.LineTo(x1, y1);
end;

// procedure TSquare.DrawBorder(AColor: integer);
// var x1, y1, x2, y2: integer;
// begin
/// /  Canvas.Pen.Color := AColor;
/// /  x1 := X - FSize;
/// /  y1 := Y - FSize;
/// /  x2 := X + FSize;
/// /  y2 := Y + FSize;
/// /  x1 := x1 + Round(FSize * (Cos(FAngle) / 180 * pi));
/// /  y1 := y1 - Round(FSize * (Sin(FAngle) / 180 * pi));
/// /  x2 := x2 + Round(FSize * (Cos(FAngle) / 180 * pi));
/// /  y2 := y2 - Round(FSize * (Sin(FAngle) / 180 * pi));
/// /  Canvas.MoveTo(x1, y1);
/// /  Canvas.LineTo(x2, y1);
/// /  Canvas.LineTo(x2, y2);
/// /  Canvas.LineTo(x1, y2);
/// /  Canvas.LineTo(x1, y1);
// end;

function TSquare.PointInFugure(aX, aY: integer): boolean;
begin
  result := not((aX > X + FSize) or (aY > Y + FSize) or (aX < X - FSize) or
      (aY < Y - FSize))
end;

{ TStar }

procedure TStar.Draw;
var
  i, vX, vY, AngleStep, VertexCount, VertexHeight: integer;
begin
  Canvas.Pen.Color := FColor;
  VertexCount := 5;
  VertexHeight := Round(0.6 * FSize);
  vX := X + Round(Cos(FAngle / 180 * pi) * FSize);
  vY := Y - Round(Sin(FAngle / 180 * pi) * FSize);
  Canvas.MoveTo(vX, vY);
  AngleStep := Round(360 / (VertexCount * 2));
  for i := 1 to VertexCount * 2 do
  begin
    if (i mod 2 = 0) then
    begin
      vX := X + Round(Cos((AngleStep * i + FAngle) / 180 * pi) * FSize);
      vY := Y - Round(Sin((AngleStep * i + FAngle) / 180 * pi) * FSize);
    end
    else
    begin
      vX := X + Round(Cos((AngleStep * i + FAngle) / 180 * pi) *
          (FSize - VertexHeight));
      vY := Y - Round(Sin((AngleStep * i + FAngle) / 180 * pi) *
          (FSize - VertexHeight));
    end;
    Canvas.LineTo(vX, vY);
  end;
end;

// procedure TStar.DrawBorder(AColor: integer);
// var
// i, vX, vY, AngleStep, VertexCount, VertexHeight, inSize, outSize: integer;
// begin
// inSize := FSize - 4;
// outSize := FSize + 4;
// Canvas.Pen.Color := AColor;
// VertexCount := 5;
// VertexHeight := Round(0.6 * inSize);
// vX := X + Round(Cos(FAngle / 180 * pi) * inSize);
// vY := Y - Round(Sin(FAngle / 180 * pi) * inSize);
// Canvas.MoveTo(vX, vY);
// AngleStep := Round(360 / (VertexCount * 2));
// for i := 1 to VertexCount * 2 do
// begin
// if (i mod 2 = 0) then
// begin
// vX := X + Round(Cos((AngleStep * i + FAngle) / 180 * pi) * inSize);
// vY := Y - Round(Sin((AngleStep * i + FAngle) / 180 * pi) * inSize);
// end
// else
// begin
// vX := X + Round(Cos((AngleStep * i + FAngle) / 180 * pi) *
// (inSize - VertexHeight));
// vY := Y - Round(Sin((AngleStep * i + FAngle) / 180 * pi) *
// (inSize - VertexHeight));
// end;
// Canvas.LineTo(vX, vY);
// end;
// VertexHeight := Round(0.6 * outSize);
// vX := X + Round(Cos(FAngle / 180 * pi) * outSize);
// vY := Y - Round(Sin(FAngle / 180 * pi) * outSize);
// Canvas.MoveTo(vX, vY);
// AngleStep := Round(360 / (VertexCount * 2));
// for i := 1 to VertexCount * 2 do
// begin
// if (i mod 2 = 0) then
// begin
// vX := X + Round(Cos((AngleStep * i + FAngle) / 180 * pi) * outSize);
// vY := Y - Round(Sin((AngleStep * i + FAngle) / 180 * pi) * outSize);
// end
// else
// begin
// vX := X + Round(Cos((AngleStep * i + FAngle) / 180 * pi) *
// (outSize - VertexHeight));
// vY := Y - Round(Sin((AngleStep * i + FAngle) / 180 * pi) *
// (outSize - VertexHeight));
// end;
// Canvas.LineTo(vX, vY);
// end;
// end;

function Intersection(ax1, ay1, ax2, ay2, bx1, by1, bx2, by2: real): boolean;
var
  v1, v2, v3, v4: real;
begin
  v1 := (bx2 - bx1) * (ay1 - by1) - (by2 - by1) * (ax1 - bx1);
  v2 := (bx2 - bx1) * (ay2 - by1) - (by2 - by1) * (ax2 - bx1);
  v3 := (ax2 - ax1) * (by1 - ay1) - (ay2 - ay1) * (bx1 - ax1);
  v4 := (ax2 - ax1) * (by2 - ay1) - (ay2 - ay1) * (bx2 - ax1);
  Intersection := (v1 * v2 < 0) and (v3 * v4 < 0);
end;

function TStar.PointInFugure(aX, aY: integer): boolean;
var
  i, j, x1, x2, y1, y2, x0, y0: integer;
  vX, vY, AngleStep, VertexCount, VertexHeight, IntersectCount: integer;
begin
  IntersectCount := 0;
  x0 := aX + 3000;
  y0 := aY + 3000;
  VertexCount := 5;
  VertexHeight := Round(0.6 * FSize);
  vX := X + Round(Cos(FAngle / 180 * pi) * FSize);
  vY := Y - Round(Sin(FAngle / 180 * pi) * FSize);
  x1 := vX;
  y1 := vY;
  Canvas.MoveTo(vX, vY);
  AngleStep := Round(360 / (VertexCount * 2));
  for j := 1 to VertexCount * 2 do
  begin
    if (j mod 2 = 0) then
    begin
      vX := X + Round(Cos((AngleStep * j + FAngle) / 180 * pi) * FSize);
      vY := Y - Round(Sin((AngleStep * j + FAngle) / 180 * pi) * FSize);
    end
    else
    begin
      vX := X + Round(Cos((AngleStep * j + FAngle) / 180 * pi) *
          (FSize - VertexHeight));
      vY := Y - Round(Sin((AngleStep * j + FAngle) / 180 * pi) *
          (FSize - VertexHeight));
    end;
    x2 := vX;
    y2 := vY;
    if Intersection(x0, y0, aX, aY, x1, y1, x2, y2) then
      Inc(IntersectCount);
    Canvas.MoveTo(vX, vY);
    x1 := vX;
    y1 := vY;
  end;
  result := (IntersectCount mod 2) <> 0;
end;

end.
