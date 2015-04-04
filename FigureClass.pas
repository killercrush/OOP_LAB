unit FigureClass;

interface

uses Graphics, SysUtils;

type
  PPoint = ^TDot;
  PCircle = ^TCircle;

  TDrawEvent = procedure(Sender: TObject) of object;
  TDotClass = class of TDot;

  TDot = class
  private { приватные поля и методы }
    X: integer; { координата X }
    Y: integer; { координата Y }
    FColor: integer; { цвет }
    FSize: integer; { размер }
    FAngle: integer; { угол }
    Canvas: TCanvas; // на нем рисуем
    FSelected: boolean;
    FOnDraw: TDrawEvent;
    procedure SetSelection(Value: boolean);
    procedure SetSize(const Value: integer); // virtual; { изменить размер }
    procedure SetAngle(const Value: integer); // virtual; { повернуть }
    procedure SetColor(const Value: integer); { повернуть }
  public
    procedure DrawSelection; // virtual;
    procedure Draw; virtual; { показать }
    procedure Hide; // virtual; { стереть }
    constructor Create(aX, aY, AColor: integer; ACanvas: TCanvas;
      aSize: integer = 1; aAngle: integer = 0); { создать }
    destructor Destroy; override; { удалить }
    procedure Shift(aX, aY: integer); { подвинуть }
    function PointInFugure(aX, aY: integer): boolean; virtual;
    function Info: string;
    property Selected: boolean read FSelected write SetSelection;
    property Color: integer read FColor write SetColor;
    property Angle: integer read FAngle write SetAngle;
    property Size: integer read FSize write SetSize;
    property OnDraw: TDrawEvent read FOnDraw write FOnDraw;
  end;

  TCircle = class(TDot)
  public
    procedure Draw; override;
    function PointInFugure(aX, aY: integer): boolean; override;
  end;

  TSquare = class(TDot)
  public
    procedure Draw; override;
    function PointInFugure(aX, aY: integer): boolean; override;
  end;

  TStar = class(TDot)
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
  private
    FirstItem: PListItem;
  public
    CurrentItem: PListItem;
    function IsEmpty: boolean;
    function GetFirstItem: PListItem;
    procedure Reset;
    constructor Create;
    destructor Destroy; override;
    procedure AddItem(AItem: TDot);
    procedure DeleteItem(AItem: PListItem);
  end;

implementation

procedure TDot.SetSize(const Value: integer);
begin
  Hide;
  FSize := Value;
  Draw;
  DrawSelection;
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
    FSelected := False;
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

procedure TDot.SetAngle(const Value: integer);
begin
//  Hide;
  FAngle := Value;
//  Draw;
//  DrawSelection;
end;

procedure TDot.SetColor(const Value: integer);
begin
//  Hide;
  FColor := Value;
//  Draw;
//  DrawSelection;
end;

procedure TDot.SetSelection(Value: boolean);
begin
  if Value = FSelected then
    exit;
  FSelected := Value;
  DrawSelection;
end;

procedure TDot.Shift(aX, aY: integer);
begin
//  Hide; { сотрем со старыми координатами }
  X := aX;
  Y := aY;
//  Draw; { нарисуем с новыми координатами }
//  DrawSelection;
end;

procedure TDot.Draw;
begin
  { рисуем класс }
  Canvas.Pen.Color := FColor;
  Canvas.Rectangle(X - 1, Y - 1, X + 2, Y + 2);
  Canvas.Pixels[X, Y] := Color;
  if Assigned(FOnDraw) then
    FOnDraw(self);
end;

procedure TDot.DrawSelection;
var
  TmpColor: integer;
begin
  TmpColor := Color;
  if not FSelected then
    FColor := $FFFFFF;
  Canvas.Pen.Width := 4;
  Draw;
  Canvas.Pen.Width := 2;
  FColor := TmpColor;
  Draw;
end;

{ TCircle }

procedure TCircle.Draw;
begin
  Canvas.Pen.Color := FColor;
  Canvas.Ellipse(X - FSize, Y - FSize, X + FSize, Y + FSize);
  if Assigned(FOnDraw) then
    FOnDraw(self);
end;

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
  CurrentItem := ListItem;
end;

constructor TList.Create;
begin
  FirstItem := nil;
  CurrentItem := nil;
end;

procedure TList.DeleteItem(AItem: PListItem);
var
  ListItem, NextItem: PListItem;
begin
  if FirstItem = AItem then
  begin
    ListItem := FirstItem.NextItem;
    FreeAndNil(FirstItem.Item);
    Dispose(FirstItem);
    FirstItem := ListItem;
    exit;
  end;
  ListItem := FirstItem;
  repeat
    if ListItem.NextItem = AItem then
    begin
      NextItem := ListItem.NextItem;
      ListItem.NextItem := ListItem.NextItem.NextItem;
      FreeAndNil(NextItem.Item);
      Dispose(NextItem);
      exit;
    end;
    ListItem := ListItem.NextItem;
  until ListItem = nil;
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

function TList.GetFirstItem: PListItem;
begin
  result := FirstItem;
end;

function TList.IsEmpty: boolean;
begin
  result := FirstItem = nil;
end;

procedure TList.Reset;
begin
  CurrentItem := FirstItem;
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
  if Assigned(FOnDraw) then
    FOnDraw(self);
end;

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
  if Assigned(FOnDraw) then
    FOnDraw(self);
end;

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
  i, x1, x2, y1, y2, x0, y0: integer;
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
