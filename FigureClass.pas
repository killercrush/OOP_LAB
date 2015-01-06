unit FigureClass;

interface

uses Graphics, SysUtils, Dialogs;

type
  PPoint = ^TDot;
  PCircle = ^TCircle;

  TDot = class
  private { приватные поля и методы }
    X: integer; { координата X }
    Y: integer; { координата Y }
    Color: integer; { цвет }
    Size: integer; { размер }
    Angle: integer; { угол }
    Canvas: TCanvas; // на нем рисуем
    FSelected: boolean;
    procedure SetSelection(Value: boolean);
    procedure DrawBorder(AColor: integer); virtual;
  public
    procedure Draw; virtual; { показать }
    procedure Hide; virtual; { стереть }
    constructor Create(aX, aY, AColor: integer; ACanvas: TCanvas;
      aSize: integer = 1; aAngle: integer = 0);
    { создать }
    destructor Destroy; override; { удалить }
    procedure Shift(aX, aY: integer); { подвинуть }
    procedure ChangeSize(aSize: integer); virtual; { изменить размер }
    procedure Rotate(aAngle: integer); virtual; abstract; { повернуть }
    function PointInFugure(aX, aY: integer): boolean; virtual;
    property Selected: boolean read FSelected write SetSelection;
    function Info: string; virtual;
  end;

  TCircle = class(TDot)
  private
    procedure DrawBorder(AColor: integer); override;
  public
    procedure Draw; override;
    function PointInFugure(aX, aY: integer): boolean; override;
  end;

  TSquare = class(TDot)
  private
    procedure DrawBorder(AColor: integer); override;
  public
    procedure Draw; override;
    procedure Hide; override;
    procedure ChangeSize(aSize: integer); override;
    destructor Destroy; override;
    function PointInFugure(aX, aY: integer): boolean; override;
  end;

  TStar = class(TDot)
  private
    procedure DrawBorder(AColor: integer); override;
  public
    procedure Draw; override;
    procedure Hide; override;
    procedure ChangeSize(aSize: integer); override;
    destructor Destroy; override;
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

procedure TDot.ChangeSize(aSize: integer);
begin
  Hide;
  Size := aSize;
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
  Color := AColor;
  Size := aSize;
  Angle := aAngle;
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
    DrawBorder($FFFFFF);
  TmpColor := Color;
  Color := $FFFFFF;
  Draw;
  Color := TmpColor;
end;

function TDot.Info: string;
begin
  result := ClassName + ' X:' + IntToStr(X) + ' Y:' + IntToStr(Y);
end;

function TDot.PointInFugure(aX, aY: integer): boolean;
begin
  result := (aX = X) and (aY = Y);
end;

procedure TDot.SetSelection(Value: boolean);
begin
  if Value = FSelected then
    exit;
  FSelected := Value;
  if FSelected then
    DrawBorder($00FF00)
  else
    DrawBorder($FFFFFF);
end;

procedure TDot.Shift(aX, aY: integer);
begin
  Hide; { сотрем со старыми координатами }
  X := aX;
  Y := aY;
  Draw; { нарисуем с новыми координатами }
  if FSelected then
    DrawBorder($00FF00);
end;

procedure TDot.Draw;
begin
  { рисуем класс }
  Canvas.Pixels[X, Y] := Color;
end;

procedure TDot.DrawBorder(AColor: integer);
begin
  Canvas.Pen.Color := AColor;
  Canvas.Rectangle(X - 1, Y - 1, X + 2, Y + 2);
end;

{ TCircle }

procedure TCircle.Draw;
begin

  Canvas.Pen.Color := Color;
  Canvas.Ellipse(X - Size, Y - Size, X + Size, Y + Size);
end;

procedure TCircle.DrawBorder(AColor: integer);
begin
  Canvas.Pen.Color := AColor;
  Canvas.Ellipse(X - Size - 1, Y - Size - 1, X + Size + 1, Y + Size + 1);
  Canvas.Ellipse(X - Size + 1, Y - Size + 1, X + Size - 1, Y + Size - 1);
end;

function TCircle.PointInFugure(aX, aY: integer): boolean;
begin
  result := (((aX - X) * (aX - X) + (aY - Y) * (aY - Y)) <= Size * Size);
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

procedure TSquare.ChangeSize(aSize: integer);
begin
  Hide;
  Size := aSize;
  Draw;
end;

destructor TSquare.Destroy;
begin

  inherited;
end;

procedure TSquare.Draw;
begin
  // inherited;
  Canvas.Pen.Color := Color;
  Canvas.Rectangle(X - Size, Y - Size, X + Size, Y + Size);
end;

procedure TSquare.DrawBorder(AColor: integer);
begin
  Canvas.Pen.Color := AColor;
  Canvas.Rectangle(X - Size - 1, Y - Size - 1, X + Size + 1, Y + Size + 1);
  Canvas.Rectangle(X - Size + 1, Y - Size + 1, X + Size - 1, Y + Size - 1);
end;

procedure TSquare.Hide;
begin
  inherited;

end;

function TSquare.PointInFugure(aX, aY: integer): boolean;
begin
  result := not((aX > X + Size) or (aY > Y + Size) or (aX < X - Size) or
      (aY < Y - Size))
end;

{ TStar }

procedure TStar.ChangeSize(aSize: integer);
begin

end;

destructor TStar.Destroy;
begin

  inherited;
end;

procedure TStar.Draw;
var
  i, vX, vY, AngleStep, VertexCount, VertexHeight: integer;
begin
  Canvas.Pen.Color := Color;
  VertexCount := 5;
  VertexHeight := Round(0.6 * Size);
  vX := X + Round(Cos(Angle / 180 * pi) * Size);
  vY := Y - Round(Sin(Angle / 180 * pi) * Size);
  Canvas.MoveTo(vX, vY);
  AngleStep := Round(360 / (VertexCount * 2));
  for i := 1 to VertexCount * 2 do
  begin
    if (i mod 2 = 0) then
    begin
      vX := X + Round(Cos((AngleStep * i + Angle) / 180 * pi) * Size);
      vY := Y - Round(Sin((AngleStep * i + Angle) / 180 * pi) * Size);
    end
    else
    begin
      vX := X + Round(Cos((AngleStep * i + Angle) / 180 * pi) *
          (Size - VertexHeight));
      vY := Y - Round(Sin((AngleStep * i + Angle) / 180 * pi) *
          (Size - VertexHeight));
    end;
    Canvas.LineTo(vX, vY);
  end;
end;

procedure TStar.DrawBorder(AColor: integer);
var
  i, vX, vY, AngleStep, VertexCount, VertexHeight, inSize, outSize: integer;
begin
  inSize := Size - 4;
  outSize := Size + 4;
  Canvas.Pen.Color := AColor;
  VertexCount := 5;
  VertexHeight := Round(0.6 * inSize);
  vX := X + Round(Cos(Angle / 180 * pi) * inSize);
  vY := Y - Round(Sin(Angle / 180 * pi) * inSize);
  Canvas.MoveTo(vX, vY);
  AngleStep := Round(360 / (VertexCount * 2));
  for i := 1 to VertexCount * 2 do
  begin
    if (i mod 2 = 0) then
    begin
      vX := X + Round(Cos((AngleStep * i + Angle) / 180 * pi) * inSize);
      vY := Y - Round(Sin((AngleStep * i + Angle) / 180 * pi) * inSize);
    end
    else
    begin
      vX := X + Round(Cos((AngleStep * i + Angle) / 180 * pi) *
          (inSize - VertexHeight));
      vY := Y - Round(Sin((AngleStep * i + Angle) / 180 * pi) *
          (inSize - VertexHeight));
    end;
    Canvas.LineTo(vX, vY);
  end;
  VertexHeight := Round(0.6 * outSize);
  vX := X + Round(Cos(Angle / 180 * pi) * outSize);
  vY := Y - Round(Sin(Angle / 180 * pi) * outSize);
  Canvas.MoveTo(vX, vY);
  AngleStep := Round(360 / (VertexCount * 2));
  for i := 1 to VertexCount * 2 do
  begin
    if (i mod 2 = 0) then
    begin
      vX := X + Round(Cos((AngleStep * i + Angle) / 180 * pi) * outSize);
      vY := Y - Round(Sin((AngleStep * i + Angle) / 180 * pi) * outSize);
    end
    else
    begin
      vX := X + Round(Cos((AngleStep * i + Angle) / 180 * pi) *
          (outSize - VertexHeight));
      vY := Y - Round(Sin((AngleStep * i + Angle) / 180 * pi) *
          (outSize - VertexHeight));
    end;
    Canvas.LineTo(vX, vY);
  end;
end;

procedure TStar.Hide;
begin
  inherited;

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
  i, j, x1, x2, y1, y2, x0, y0: integer;
  vX, vY, AngleStep, VertexCount, VertexHeight, IntersectCount: integer;
begin
  IntersectCount := 0;
  x0 := aX + 3000;
  y0 := aY + 3000;
  VertexCount := 5;
  VertexHeight := Round(0.6 * Size);
  vX := X + Round(Cos(Angle / 180 * pi) * Size);
  vY := Y - Round(Sin(Angle / 180 * pi) * Size);
  x1 := vX;
  y1 := vY;
  Canvas.MoveTo(vX, vY);
  AngleStep := Round(360 / (VertexCount * 2));
  for j := 1 to VertexCount * 2 do
  begin
    if (j mod 2 = 0) then
    begin
      vX := X + Round(Cos((AngleStep * j + Angle) / 180 * pi) * Size);
      vY := Y - Round(Sin((AngleStep * j + Angle) / 180 * pi) * Size);
    end
    else
    begin
      vX := X + Round(Cos((AngleStep * j + Angle) / 180 * pi) *
          (Size - VertexHeight));
      vY := Y - Round(Sin((AngleStep * j + Angle) / 180 * pi) *
          (Size - VertexHeight));
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
